library(shiny)
library(shinyWidgets)
library(shinyfullscreen)

source("stagtracker.R")

ui <- tagList(
  tags$head(tags$style("
    body { 
      overflow: hidden;
    }
    
    .inner.show::-webkit-scrollbar {
      width: 2em;
    }
    
    .inner.show::-webkit-scrollbar-thumb {
      width: 2em;
      height: 2em;
      border-radius: 0.25em;
      border: gray solid 1px;
      background: #333;
    }
    
    [style*='background-color: #F9E300;'] {
      color: black !important;
      text-shadow: 1px 1px 3px #FFFFFF;
    }
    
    .form-group {
      margin-bottom: 0px;
    }
    
    .leaflet-control-zoom a {
      background-color: #445;
      color: #aaa
    }
    
    .comyuter div#commuter {
      width: 690px !important;
    }
      
    #commuter img {
      width: 100% !important;
      height: auto !important;
    }
                       
                       
                       ")),
  fixedPage(
    title = "Stagtracker",
    theme = bslib::bs_theme(
      version = 5L,
      bg = "black",
      fg = "white",
      preset = "flatly"
    ),
    uiOutput("dynamic_css"),
    fullscreen_all(click_id = "timetable", bg_color = "black"),
    fullscreen_all(click_id = "commuter", bg_color = "black"),
    div(
      id = "bottom_bar",
      div(
        div(
          style = "width: 300px !important; float: left;",
          pickerInput(
            "station", "",
            selected = home_station,
            choices = stations,
            options = pickerOptions(
              liveSearch = FALSE
            ),
            choicesOpt =
              list(
                class = "choicePicker"
              )
          )
        ),
        div(
          style = "width: 40px !important; float: left; margin-top: 24px;",
          uiOutput("arrow_button")
        ),
        div(
          style = "display: none;",
          textInput("my_station", "", home_station)
        ),
        div(
          style = "float: right;",
          checkboxGroupButtons(
            inputId = "map_toggle",
            label = "",
            size = "xs",
            choices = "🌎"
          )
        ),
        conditionalPanel(
          condition = "input.map_toggle == ''",
          div(
            style = "float: right;",
            checkboxGroupButtons(
              inputId = "commute_view",
              label = "",
              choices = c("&#x2691;" = "i")
            )
          )
        ),
      )
    ),
    conditionalPanel(
      condition = "input.map_toggle == '' && input.commute_view != 'i'",
      div(id = "tymetable", style = "clear: both;",
        htmlOutput("timetable", width = "90%"))),
    conditionalPanel(
      condition = "input.map_toggle == '' && input.commute_view == 'i'",
      div(id = "comyuter", style = "clear: both;",
          plotOutput("commuter", width = "100%", height = "225px"))),# 
    conditionalPanel(
      condition = "input.map_toggle != ''",
      div(style = "float: center;",
          leafletOutput("mapit", width = "100%", height = "225px"))
    )
  )
)

server <- function(input, output, session) {
  # Load the data ------------------------------------------
  train_times <- reactive({
    invalidateLater(1 * 60 * 1000, session)
    starting_data <- get_trains(station = input$station)
    
    if(direction_order[arrow_state() + 1] == direction_order[3]) {
      result <- starting_data
    } else if(direction_order[arrow_state() + 1] == direction_order[2]){
      result <- starting_data |> 
        limit_trains("north")
    } else {
      result <- starting_data |> 
        limit_trains("south")
    }
    result
  })
  
  # UI Outputs ---------------------------------------------
  output$timetable <- renderUI({
    train_times() |> 
      make_timetable(rows = 5)
  })
  
  output$commuter <- renderPlot({
    train_times() |> 
      plot_trains()
  })
  
  output$mapit <- renderLeaflet({
    train_times() |> 
      map_trains()
  })
  
  # Handle states on the arrow button ----------------------
  arrow_state <- reactiveVal(0) 
  
  output$arrow_button <- renderUI({
    actionButton("arrow_btn", direction_order[arrow_state() + 1])
  })
  
  observeEvent(input$arrow_btn, {
    arrow_state((arrow_state() + 1) %% 3)
  })
  
  observeEvent(input$map_toggle, {
    arrow_state(2 %% 3)
  })
  
  # Optional: Change by day and time -----------------------
  # ## First, use commuter view in mornings
  observe({
    invalidateLater(1 * 30 * 60 * 1000, session) # hr, min, sec, ms
    current_hour <- now() |> hour()
    weekday_morning <- current_hour %in% 6:11 && wday(now()) %in% 2:6
    evening_time <- current_hour %in% 19:23
    if (input$station == home_station && weekday_morning) {
      # weekday morns, switch to commuter view
      updateCheckboxGroupButtons(
        session = session,
        inputId = "commute_view",
        selected = "i"
      )# 
      arrow_state(0) 
    } else if (input$station == home_station && evening_time) {
      # evenings, switch back
      updateCheckboxGroupButtons(
        session = session,
        inputId = "commute_view",
        selected = ""
      )
    }
  })
  
  # ## Next, default to map intelligently
  observe({
    invalidateLater(1 * 30 * 60 * 1000, session)
    current_minutes <- now() |> minute()
    current_hour <- now() |> hour()
    morning_time <- current_hour %in% 6:11
    # don't show map by default in mornings
    if (morning_time) {updateCheckboxGroupButtons(
      session, 
      "map_toggle", 
      selected = "")
    } else if (current_minutes < 30) {
      # otherwise, swap every 30 minutes as a screensaver
      updateCheckboxGroupButtons(
        session, 
        "map_toggle", 
        selected = "🌎")
      } else {
        updateCheckboxGroupButtons(
          session, 
          "map_toggle", 
          selected = "")}
  })
  
  # ## Finally, avoid screen burn-in by jiggling placement
  output$dynamic_css <- renderUI({
    invalidateLater(1 * 60 * 1000, session)
    current_minutes <- now() |> minute()
    if ((current_minutes %% 10 < 5)) {
      # buttons at bottom
      tags$head(
        tags$style(HTML(paste0(
          "#bottom_bar {position: absolute; bottom: ", sample(5:7, size = 1), "px; top: unset; width: 100vw; padding-right: ", sample(23:26, size = 1), "px; margin-top: unset;}
          #mapit {margin-top: 0px;}
          #tymetable, #comyuter {margin-top: -12px; position: absolute; bottom: unset;}"
        )))
      )
    } else {
      # buttons at top
      tags$head(
        tags$style(HTML(paste0(
          "#bottom_bar {position: absolute; bottom: unset; top: ", sample(0:3, size = 1), "px; width: 100vw; padding-right: ", sample(23:26, size = 1), "px; margin-top: -24px;}
          #mapit {margin-top: 41px;}
          #tymetable, #comyuter {position: absolute; bottom: 5px; top: unset;}"
        )))
      )
    }
  })
}

shinyApp(ui, server)