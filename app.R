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

if (file.exists("my_key.R")) {
  source("my_key.R")
} else {
  api_key <- "a8456dcbhf8475683cf7818bca81"
}

stations <- readr::read_csv("data/stations.csv") |> 
  {\(x) setNames(x$id, x$station)}()

names(stations) <- names(stations) |> 
  stringr::str_remove_all("Line[s]?")

ui <- fixedPage(
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
    style = "width: 390px !important;",
    div(
      style = "width: 80px !important; float: left;",
      radioGroupButtons(
        inputId = "arrow_choice",
        choices = c("▼", "▲")
      )
    ),
    div(
      style = "width: 300px !important; margin-top: -24px; float: left;",
      pickerInput(
        "station", "",
        selected = 41320,
        choices = stations,
        options = pickerOptions(
          liveSearch = TRUE
        )
      )
    )
  )
  # fixedRow(
  #   column(
  #     width = 2,
  #     align = "center",
  #     # style = css(padding.top = px(6 + 16 + 2)),
  #     radioGroupButtons(
  #       inputId = "arrow_choice",
  #       choices = c("▼", "▲")
  #     )
  #   ),
  #   column(
  #     width = 6,
  #     style = css(margin.top = px(-24), flex.shrink = 2),
  #     pickerInput(
  #       "station", "",
  #       selected = 41320,
  #       choices = stations,
  #       options = pickerOptions(
  #         liveSearch = TRUE
  #       )
  #     )
  #   )
  # )
)

server <- function(input, output, session) {
  train_times <- reactive({
    invalidateLater(1 * 60 * 1000, session)
    
    full_query <- url_modify_query(
      "https://lapi.transitchicago.com/api/1.0/ttarrivals.aspx",
      key = api_key,
      max = 20,
      mapid = input$station)
    
    full_query |> 
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
  
  style_timetable_gt <- function(.data){
    .data |> 
      filter(row_number() <= 7) |> 
      gt() |> 
      tab_style(
        locations = cells_body(rows = line == "Red"), 
        style = list(
          cell_fill("#FF0000"), 
          cell_text(color = "#FFFFFF"))) |>
      tab_style(
        locations = cells_body(rows = line == "Brn"), 
        style = list(
          cell_fill("#662200"), 
          cell_text(color = "#FFFFFF"))) |>
      tab_style(
        locations = cells_body(rows = line == "Blue"), 
        style = list(
          cell_fill("#0000FF"), 
          cell_text(color = "#FFFFFF"))) |>
      tab_style(
        locations = cells_body(rows = line == "G"), 
        style = list(
          cell_fill("#007700"), 
          cell_text(color = "#FFFFFF"))) |>
      tab_style(
        locations = cells_body(rows = line == "Org"), 
        style = list(
          cell_fill("#FF7700"), 
          cell_text(color = "#000000"))) |>
      tab_style(
        locations = cells_body(rows = line == "Pink"), 
        style = list(
          cell_fill("#FF66AA"), 
          cell_text(color = "#000000"))) |>
      tab_style(
        locations = cells_body(rows = line == "Y"), 
        style = list(
          cell_fill("#FFFF00"), 
          cell_text(color = "#000000"))) |>
      tab_style(
        locations = cells_body(rows = line == "P"), 
        style = list(
          cell_fill("#6600AA"), 
          cell_text(color = "#FFFFFF"))) |> 
      cols_hide("line") |>
      cols_width(
        estimated ~ px(110)
      ) |> 
      tab_style(
        locations = cells_body(columns = dest),
        style = css(
          padding = paste0(c(0, 0, 0, 2), "em")
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
    
  }
  
  output$timetable <- shiny::renderUI({
    if(input$arrow_choice == "▼") {
      train_times_south() |> 
        style_timetable_gt()
    } else {
      train_times_north() |> 
        style_timetable_gt()
    }
  })
}

shinyApp(ui, server)