library(shiny)
library(httr2)
library(xml2)
library(purrr)
library(tibble)
library(dplyr)
library(gt)
library(lubridate)
library(stringr)
library(shinyWidgets)
library(shinyfullscreen)

source("app_options.R")

if (file.exists("my_key.R")) {
  source("my_key.R")
}

stations <- readr::read_csv("data/stations.csv") |> 
  {\(x) setNames(x$id, x$station)}()

names(stations) <- names(stations) |> 
  stringr::str_remove_all("Line[s]?")

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
    
    .gt_table {
      color: white !important;
      text-shadow: 1px 1px 3px #000000;
    }
    
    [style*='background-color: #F9E300;'] {
      color: black !important;
      text-shadow: 1px 1px 3px #FFFFFF;
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
  fullscreen_all(click_id = "timetable", bg_color = "black"),
  htmlOutput("timetable"),
  div(
    # style = "width: 470px !important;",
    div(
      style = "width: 80px !important; float: left;",
      radioGroupButtons(
        inputId = "arrow_choice",
        choices = direction_order
      )
    ),
    div(
      style = "width: 300px !important; margin-top: -24px; float: left;",
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
      style = "display: none;",
      textInput("my_station", "", home_station)),
    conditionalPanel(
      condition = "input.station == input.my_station",
      div(
        style = "float: right; margin-top: -24px;",
        checkboxGroupButtons(
          inputId = "limit_line",
          label = "", 
          choices = c(commute_label)
        )
        )
    ),
  )
)
)

server <- function(input, output, session) {
  train_times <- reactive({
    invalidateLater(1 * 60 * 1000, session)
    
    full_query <- url_modify_query(
      "https://lapi.transitchicago.com/api/1.0/ttarrivals.aspx",
      key = api_key,
      max = 20,
      mapid = input$station)
    
    the_df <- full_query |> 
      read_xml() |> 
      xml_find_all("//eta") |> 
      map_dfr(\(tr) {
        kids <- xml_children(tr)
        vals  <- xml_text(kids, trim = TRUE)
        nms   <- xml_name(kids)
        as_tibble(setNames(as.list(replace(vals, vals == "", NA_character_)), nms))
      }) |> 
      select(rn, line = rt, dest = destNm, arriving = arrT, direction = trDr) |> 
      mutate(
        arriving = as_datetime(arriving, tz = "US/Central"),
        est = interval(now(), arriving) / dminutes(1),
        estimated = case_when(
          est == first(est) ~ floor(est),
          .default = round(est)
        ) |> paste("minutes") |> 
          str_replace_all("^0 minutes", "due") |> 
          str_replace_all("-1 minutes", "due") |> 
          str_replace_all("^-.*", "overdue") |> 
          str_replace_all("^1 minutes", "1 minute")) |> 
      arrange(est) |> 
      select(line, dest, estimated, direction)
    
    if (commute_label %in% input$limit_line) {
     the_df <- the_df |> 
       filter(line == commute)
    }
    
    the_df
  })
  
  train_times_south <- reactive({
    train_times() |> 
      filter(direction == 5) |> 
      select(-direction) 
  })
  
  train_times_north <- reactive({
    train_times() |> 
      filter(direction == 1) |> 
      select(-direction)
  })
  
  observeEvent(input$station, {
    updateCheckboxGroupButtons(
      session = session,
      inputId = "limit_line",
      selected = NA
    )
  })
  
  observe({
    invalidateLater(1 * 30 * 60 * 1000, session) # hr, min, sec, ms
    if (input$station == home_station && hour(now(tz = "US/Central")) %in% 6:11 && wday(now()) %in% 2:6) {
      # weekday morns, limit to Red
      updateCheckboxGroupButtons(
        session = session,
        inputId = "limit_line",
        selected = commute_label
      )
    } else if (input$station == home_station && hour(now(tz = "US/Central")) %in% 19:23) {
      # evenings, show all bidirectional
      updateCheckboxGroupButtons(
        session = session,
        inputId = "limit_line",
        selected = NULL
      )
      updateRadioGroupButtons(
        session = session,
        inputId = "arrow_choice",
        selected = character(0))
    } else {
      # generally, show all south
      updateCheckboxGroupButtons(
        session = session,
        inputId = "limit_line",
        selected = NULL
      )
    }
  })
  
  style_timetable_gt <- function(.data){
    
    if (commute_label %in% input$limit_line) {
      .data <- .data |> 
        filter(row_number() <= show_rows * .6)
    } else {
      .data <- .data |> 
        filter(row_number() <= show_rows)
    }
    
    df_gt <- .data |> 
      gt() |> 
      tab_style(
        locations = cells_body(rows = line == "Red"), 
        style = list(cell_fill("#C60C30"))) |>
      tab_style(
        locations = cells_body(rows = line == "Brn"), 
        style = list(cell_fill("#62361b"))) |>
      tab_style(
        locations = cells_body(rows = line == "Blue"), 
        style = list(cell_fill("#00a1de"))) |>
      tab_style(
        locations = cells_body(rows = line == "G"), 
        style = list(cell_fill("#009b3a"))) |>
      tab_style(
        locations = cells_body(rows = line == "Org"), 
        style = list(cell_fill("#f9461c"))) |>
      tab_style(
        locations = cells_body(rows = line == "Pink"), 
        style = list(cell_fill("#e27ea6"))) |>
      tab_style(
        locations = cells_body(rows = line == "Y"), 
        style = list(cell_fill("#F9E300"))) |>
      tab_style(
        locations = cells_body(rows = line == "P"), 
        style = list(cell_fill("#522398"))) |> 
      cols_hide("line") |>
      cols_width(
        estimated ~ px(110)
      ) |> 
      tab_style(
        locations = cells_body(columns = dest),
        style = css(
          padding = paste0(c(0, 0, 0, 20), "px")
        )
      ) |> 
      tab_options(
        column_labels.hidden = TRUE,
        table.border.top.style = "hidden",
        table.border.bottom.style = "hidden",
        table.width = pct(100)) |> 
      tab_style(
        style = list(
          cell_borders(
            sides = "bottom",
            color = "black",
            weight = px(1),
            style = "solid"
          )
        ),
        locations = cells_body()
      )
    
    if (nrow(.data) <= show_rows * .6) {
      df_gt <- df_gt |> 
        tab_options(table.font.size = pct(175))
    }
    
    df_gt |> 
      text_replace("minute[s]?", "min", locations = cells_body(columns = estimated)) |> 
      text_replace("([0-9]+)", "<b>\\1</b>", locations = cells_body(columns = estimated)) |> 
      fmt_markdown(columns = estimated) |> 
      tab_style(
        style = cell_text(
          size = pct(115),
          align = "center"),
        locations = cells_body(columns = estimated))
  }
  
  output$timetable <- shiny::renderUI({
    if(length(input$arrow_choice) > 0 && input$arrow_choice == "▼") {
      train_times_south() |> 
        style_timetable_gt()
    } else if(length(input$arrow_choice) > 0 && input$arrow_choice == "▲"){
      train_times_north() |> 
        style_timetable_gt()
    } else {
      train_times() |> 
        select(-direction) |> 
        style_timetable_gt()
    }
  })
}

shinyApp(ui, server)