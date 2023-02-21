# Author: Brian Derstine
# Date: 2020-04-12
# Just trying to have a little fun...

library(shiny)
library(dplyr)
library(ggplot2)
library(rclipboard)

# download.file("https://raw.githubusercontent.com/jbowens/codenames/master/assets/original.txt","original.txt")
# Thanks for the words file! 

# Info about shared session data on shinyapps.io:  https://shiny.rstudio.com/articles/share-data.html

# Define Global (shared) variables and functions -------------
words_classic <- read.delim("original.txt",header = F, stringsAsFactors = F)
names(words_classic) <- "word"
team_colors <- c("red","blue")
hex_red = "#CC0033"
hex_blue = "#6699FF"
hex_tan = "#FFFFCC"
hex_black = "#000000"
hex_grey = "#CCCCCC"

gv <- reactiveValues()

init_board <- function(key) {
  # create a new board (instance) in the global list of boards. 
  # use a random or user-provided "key" to initialize the board
  # the key can be used for other users to access
  
  if(is.null(gv[[key]])) {
    # create new randomized board
    gv[[key]] <<- list(
      words_teams = get_teams_words(),
      game_num = 1,
      wins_red = 0,
      wins_blue = 0,
      game_over = F
    )
    gv[[key]]$turn = names(which.max(table(isolate(gv[[key]]$words_teams$team))))
  } 
  # otherwise, the board already exists
}


# get 25 (5x5) words and assign teams as red, blue, black (assassin), tan (neutral)
get_teams_words <- function() {
  w_all <- data.frame(word = sample(words_classic$word, size = 25), 
                      stringsAsFactors = F)
  w_all$word <- gsub(" ","\n",w_all$word)
  
  # "randomly" pick red or blue to have 9 cards:
  nine_card_team <- sample(team_colors, 1)
  eight_card_team <- setdiff(team_colors, nine_card_team) 
  
  # 1 black, 7 tan, and 8/9 between red/blue
  teams <- c("black",
             rep("tan",7),
             rep(nine_card_team,9),
             rep(eight_card_team,8))
  
  # "randomly" assign teams words
  w_all = w_all %>%
    mutate(team = sample(x = teams, size = 25, replace = F),
           show = F,
           card_color = case_when(
             team == "blue" ~ hex_blue,
             team == "red"  ~ hex_red,
             team == "tan"  ~ hex_tan,
             team == "black" ~ hex_black),
           card_alpha = 1)
  w_all[,c("x","y")] = expand.grid(0.5:4.5, 0.5:4.5)
  
  return(w_all)
}

change_turn <- function(team_name) {
  setdiff(team_colors, team_name)
  # ifelse(team_name == "blue", "red", "blue")
}

get_remaining = function(x) {
  x %>%
    filter(team %in% team_colors) %>%
    group_by(team, card_color) %>%
    summarise(remaining = sum(!show), 
              .groups = "keep")
}


# TODO: drop boards that have been around too long
# drop_board <- function(key) {
# }


# UI ----
ui <- fluidPage( 
  rclipboardSetup(),
  
  # Application title
  titlePanel(title = "Codenames", windowTitle = "Codenames"),
  
  conditionalPanel("input.load_board <= 0",
    fluidRow(
      actionButton(inputId = "load_board", 
                   label = "Load Gameboard")
    )
  ),
  conditionalPanel("input.load_board > 0", 
                   
    fluidRow(
      # Remaining Cards = # Red vs. # Blue
      column(6, uiOutput("n_red_blue", inline = T)),
      # Current Turn Team 
      column(4, uiOutput("whos_turn", inline = T)),
      # End Turn Button
      column(2, uiOutput("end_turn", inline = T))
    ),
    
    # Use tabs to show Player/Spymaster views
    tabsetPanel(
      tabPanel(title="Gameboard",
               plotOutput("playerboard", 
                          click = "plot_click")
                          # hover = hoverOpts(id = "plot_hover", delay = 1))
      ),
      tabPanel(title="Spymaster",
               plotOutput("spyboard")
      ),
      tabPanel(title="Instructions",
               p(),
               p(strong("Win: "), "Be the first team to uncover ALL of your team's words."),
               p(strong("Lose: "), "Uncover the assassin card or fail to uncover all of your team's words first."),
               p(strong("How to Play:"),
                 "First, pick one Spymaster for each team. 
                 Players use the normal Gameboard tab.
                 Only Spymasters can see the Spymaster tab (using the honor system). 
                 The Spymaster's job is to try to get their agents (teammates) to identify their own team's words 
                 without guessing the assassin word. 
                 Each turn, the Spymaster gives a clue consisting of one word and one number. 
                 The clue cannot be one of the words on the board. 
                 The number is the number of words on the board that are related to the clue word (and the number of guesses allowed).
                 Turn ends when the team members (a) uncover the wrong team's word, (b) uncover a neutral word, 
                 (c) pass, (d) exhaust their number of guesses.")
      ) # ,
      # tabPanel(title = "Debug",
      #          fluidRow(
      #            # verbatimTextOutput("global_board_state"),
      #            verbatimTextOutput("session_info")
      #          )
      # )
    ),
    
    hr(),
    
    fluidRow(
      column(10, tableOutput("games_info")),
      column(2, actionButton(inputId = "new_game", label = "New Game"))
    ),
    
    fluidRow(
      uiOutput(outputId = "gameboard_link")
    )
  )
)


# server ----
server <- function(input, output, session) {
  
  # Reactive key ----
  gameboard_key = eventReactive(input$load_board, {
    
    # first check for URL gameboard key request: 
    query <- parseQueryString(session$clientData$url_search)
    out_key = NULL
    if (length(query) != 0) { 
      out_key = as.character(stringr::str_trim(query[["board_id"]]))
    }
    
    # no request, so grab a "randomized" gameboard key
    if (is.null(out_key)) {
      out_key = session$token
    }
    
    init_board(out_key)

    out_key
  })
  
  gameboard_link_text = reactive({
    if(!is.null(gameboard_key())) {
      # we have a board! 
      
      if (session$clientData$url_hostname == "127.0.0.1") {
        # localhost version:
        paste0(session$clientData$url_protocol,       # http:
               session$clientData$url_pathname,       # /
               session$clientData$url_pathname,       # /
               session$clientData$url_hostname, ":",  # 127.0.0.1:
               session$clientData$url_port,           # 1234
               session$clientData$url_pathname,       # /
               "?board_id=",gameboard_key())
      } else {
        # shinyapps.io version
        paste0(session$clientData$url_protocol, "//", # https://
               session$clientData$url_hostname, # baderstine.shinyapps.io
               # session$clientData$url_port,
               session$clientData$url_pathname, # /codenames_shinyapp/
               "?board_id=",gameboard_key())
      }
    }
  })
                                      
  
  # Game Status  ----
  output$n_red_blue <- renderText({
    # display the number of remaining cards for each team
    req(gameboard_key())
    num_remain <- get_remaining(gv[[gameboard_key()]]$words_teams)
    
    paste("Remaining:",
          paste0("<font color=\"", num_remain$card_color, "\">",
                 num_remain$remaining," ", toupper(num_remain$team),
                 "</font>", collapse=" : "))
  })
  
  output$whos_turn <- renderText({
    req(gameboard_key())
    
    cc = ifelse(gv[[gameboard_key()]]$turn == "blue", hex_blue, hex_red)
    paste0("<font color=\"",cc,"\">",toupper(gv[[gameboard_key()]]$turn),"'s turn</font>")
  })
  
  output$end_turn <- renderUI({
    req(gameboard_key())
    
    actionButton(inputId = "button_end_turn",
                 label = paste0("End ", toupper(gv[[gameboard_key()]]$turn),"'s turn"))
  })
  
  # Link to this board instance ---- 
  output$gameboard_link <- renderUI({
    tagList("Send this URL to your friends to join:", 
            a(gameboard_link_text(), href=gameboard_link_text()))
  })
  

  # Plot Boards ----
  output$playerboard <- renderPlot({
    req(gameboard_key())
    
    # 'show' determines displayed card color
    dispmat = gv[[gameboard_key()]]$words_teams %>%
      mutate(disp_fill = if_else(show, card_color, hex_grey))
    
    ggplot(dispmat, aes(x=x, y=y)) + 
      geom_tile(fill=dispmat$disp_fill, color="black", lty="solid", alpha=dispmat$card_alpha) +
      geom_label(aes(label=word, size=18, fontface = "bold")) +
      theme_void() +
      theme(legend.position="none")
  })
  
  output$spyboard <- renderPlot({
    req(gameboard_key())
    
    dispmat = gv[[gameboard_key()]]$words_teams 
    
    ggplot(dispmat, aes(x=x, y=y)) + 
      geom_tile(fill=dispmat$card_color, color="black", lty="solid") +
      geom_label(aes(label=word, size=18, fontface = "bold")) +
      theme_void() + 
      theme(legend.position="none")
  })
  
  output$games_info <- renderTable({
    req(gameboard_key())
    
    data.frame(labels = c("Game:", "RED:", "BLUE:"),
               values = c(gv[[gameboard_key()]]$game_num, 
                          paste0(gv[[gameboard_key()]]$wins_red, " wins"),
                          paste0(gv[[gameboard_key()]]$wins_blue, " wins")),
               stringsAsFactors = F)
  }, rownames = F, colnames = F, digits = 0)
  
  
  # Session Info (for testing/debugging) ----
  # output$session_info <- renderText({
  #   cdata <- session$clientData
  #   allvalues <- lapply(names(cdata), function(name) {
  #     paste(name, cdata[[name]], sep = " = ")
  #   })
  #   paste(allvalues, collapse = "\n")
  # })
  
  # output$global_board_state <- renderText({
  #   print(paste0("keys:", names(gv)))
  # })
  
  
  # In-Game Event Handlers ----
  observeEvent(input$button_end_turn, {
    # when the user presses the "end turn" button, change the turn variable
    if(!gv[[gameboard_key()]]$game_over) 
      gv[[gameboard_key()]]$turn <<- change_turn(gv[[gameboard_key()]]$turn)
  }, ignoreInit = T)
  
  observeEvent(input$new_game, {
    # start over, resetting the words and turn indicator
    gv[[gameboard_key()]]$words_teams <<- get_teams_words()
    gv[[gameboard_key()]]$turn <<- names(which.max(table(isolate(gv[[gameboard_key()]]$words_teams$team))))
    gv[[gameboard_key()]]$game_num <<- gv[[gameboard_key()]]$game_num+1
    gv[[gameboard_key()]]$game_over <<- F
  }, ignoreInit = T)
  
  observeEvent(input$plot_click, {
    # toggle the clicked tile 'on' 
    
    cx = floor(input$plot_click$x)+.5
    cy = floor(input$plot_click$y)+.5
    
    tile_flipped = gv[[gameboard_key()]]$words_teams %>% 
      filter(x==cx & y==cy) %>%
      select(word, team, show)
    
    # click is not always within a valid tile
    if(nrow(tile_flipped) > 0) {
      
      # check if tile is already flipped:
      tile_shown_already <- gv[[gameboard_key()]]$words_teams %>%
        filter(word == tile_flipped$word) %>%
        pull(show)
      
      if(!tile_shown_already) {
        # turn the clicked tile on
        gv[[gameboard_key()]]$words_teams <<- gv[[gameboard_key()]]$words_teams %>%
          mutate(show = ifelse(word == tile_flipped$word, TRUE, show))
        
        # skip the rest if game is already over
        if(!gv[[gameboard_key()]]$game_over) {
          zero_team = get_remaining(gv[[gameboard_key()]]$words_teams) %>%
            ungroup() %>%
            filter(remaining == 0) %>%
            select(team)
          
          if (nrow(zero_team) == 1) {
            # End game if no more cards for one team
            gv[[gameboard_key()]]$game_over <<- T
            showModal(modalDialog(
              title = "Game Over!",
              paste(zero_team$team, "wins!"),
              easyClose = TRUE
            ))
            # increment wins counter
            if (gv[[gameboard_key()]]$turn == "red") 
              gv[[gameboard_key()]]$wins_red <<- gv[[gameboard_key()]]$wins_red+1
            else 
              gv[[gameboard_key()]]$wins_blue <<- gv[[gameboard_key()]]$wins_blue+1
          }
          
          if (tile_flipped$team == "black" & tile_flipped$show == F) {
            # End game if first time black card is clicked
            gv[[gameboard_key()]]$game_over = T
            showModal(modalDialog(
              title = "Game Over!",
              paste(gv[[gameboard_key()]]$turn, "team assassinated!\n", 
                    change_turn(gv[[gameboard_key()]]$turn), "wins!"),
              easyClose = TRUE
            ))
            
            # increment wins counter
            if (gv[[gameboard_key()]]$turn == "red") 
              gv[[gameboard_key()]]$wins_blue <<- gv[[gameboard_key()]]$wins_blue+1
            else 
              gv[[gameboard_key()]]$wins_red <<- gv[[gameboard_key()]]$wins_red+1
            
          } else if (tile_flipped$team == "tan" | tile_flipped$team != gv[[gameboard_key()]]$turn) {
            # switch team if neutral or wrong team tile flipped
            gv[[gameboard_key()]]$turn <<- change_turn(gv[[gameboard_key()]]$turn)
          }
        }
      }
    }
  })
  
  # can change tile appearance on hover, but its slow and unnecessary 
  # observeEvent(input$plot_hover, {
  #     # figure out which tile is hovered 
  # 
  #     cx = floor(input$plot_hover$x)+.5
  #     cy = floor(input$plot_hover$y)+.5
  # 
  #     # change its alpha
  #     gv[[gameboard_key()]]$words_teams <<- gv[[gameboard_key()]]$words_teams %>%
  #         mutate(card_alpha = ifelse(x==cx & y==cy, .5, 1))
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
