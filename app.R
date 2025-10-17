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
library(leaflet)
library(geojsonsf)

source("app_options.R")

if (file.exists("my_key.R")) {
  source("my_key.R")
}

official_colors <- c(
  Red = "#C60C30",
  Brn = "#62361b",
  P = "#522398",
  Blue = "#00a1de",
  G = "#009b3a",
  Org = "#f9461c",
  Pink = "#e27ea6",
  Y = "#F9E300"
)

line_names <- c(
  Red = "Red",
  Brn = "Brown",
  P = "Purple",
  Blue = "Blue",
  G = "Green",
  Org = "Orange",
  Pink = "Pink",
  Y = "Yellow"
)

stations <- readr::read_csv("data/stations.csv") |> 
  {\(x) setNames(x$id, x$station)}()

names(stations) <- names(stations) |> 
  stringr::str_remove_all("[ ]?Line[s]?") |> 
  stringr::str_squish() |> 
  stringr::str_replace_all(" [)]", ")") |> 
  stringr::str_replace_all("Blue-([a-zA-Z])", "Blue - \\1")

cta_lines <- geojsonsf::geojson_sf("data/CTA_-_L_Rail_Lines_20251016.geojson")

cta_stations <- geojsonsf::geojson_sf("data/CTA_-_L_Rail_Stations_20251016.geojson")

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
    
    .form-group {
      margin-bottom: 0px;
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
    div(
      style = "position: absolute; bottom: 5px; width: 100vw; padding-right: 24px;",
    div(
      # style = "width: 470px !important;",
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
      conditionalPanel(
        condition = "input.map_toggle != '🌎'",
        div(
          style = "width: 40px !important; float: left; margin-top: 24px;",
          uiOutput("arrow_button")
        )
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
          choices = "🌎"#,"\U0001F30E\uFE0E"
        )
      ),
      conditionalPanel(
        condition = "input.station == input.my_station & input.map_toggle != '🌎'",
        div(
          style = "float: right;",
          checkboxGroupButtons(
            inputId = "limit_line",
            label = "",
            choices = c(commute_label)
          )
        )
      ),
    )),
    conditionalPanel(
      condition = "input.map_toggle != '🌎'",
      div(style = "clear: both; margin-top: -12px;",
        htmlOutput("timetable", width = "90%"))),
    conditionalPanel(
      condition = "input.map_toggle == '🌎'",
      div(style = "float: center;",# margin-top: -12px",
          leafletOutput("mapit", width = "100%", height = "225px"))
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
      rename(line = rt, dest = destNm, arriving = arrT, direction = trDr) |> 
      mutate(
        arriving = as_datetime(arriving, tz = "US/Central"),
        est = interval(now(), arriving) / dminutes(1)) |> 
      arrange(est)
    
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
      selected = ""
    )
  })
  
  observeEvent(input$map_toggle, {
    updateCheckboxGroupButtons(
      session = session,
      inputId = "limit_line",
      selected = ""
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
      # evenings, reset the limit
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
      arrange(est) |> 
      mutate(
        estimated = case_when(
          est == first(est) ~ floor(est),
          .default = round(est)
        ) |> paste("minutes") |> 
          str_replace_all("^0 minutes", "due") |> 
          str_replace_all("-1 minutes", "due") |> 
          str_replace_all("^-.*", "late") |> 
          str_replace_all("^1 minutes", "1 minute")) |> 
      select(-est) |> 
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
  
  arrow_state <- reactiveVal(0) 
  
  output$arrow_button <- renderUI({
    actionButton("arrow_btn", direction_order[arrow_state() + 1])
  })
  
  observeEvent(input$arrow_btn, {
    arrow_state((arrow_state() + 1) %% 3)
  })
  
  output$timetable <- shiny::renderUI({
    if(direction_order[arrow_state() + 1] == direction_order[3]) {
      train_times() |> 
        select(line, dest, est) |> 
        style_timetable_gt()
    } else if(direction_order[arrow_state() + 1] == direction_order[2]){
      train_times_north() |> 
        select(line, dest, est) |> 
        style_timetable_gt()
    } else {
      train_times_south() |> 
        select(line, dest, est) |> 
        style_timetable_gt()
    }
  })
  
  trains_map <- reactive({train_times() |> 
      tidyr::drop_na(lon, lat) |> 
      sf::st_as_sf(
        coords = c("lon", "lat"), crs = 4326) |> 
      rowwise() |> 
      mutate(
        hex_color = unname(official_colors[line])
      ) |>
      ungroup()})
  
  get_sf_n <- function(id, n) {
    coords <- cta_stations$geometry[cta_stations$station_id==id] |> 
      sf::st_coordinates()
    
    coords[n]
  }
  
  output$mapit <- renderLeaflet(
    leaflet(options = leafletOptions(
      attributionControl=FALSE)) |> 
      addTiles() |> 
      addPolylines(
        data = cta_lines,
        color = "#884", opacity = 1, weight = 4) |> 
      addCircles(
        get_sf_n(as.numeric(input$station)-40000, 1),
        get_sf_n(as.numeric(input$station)-40000, 2),
        color = "black",
        fill = "white",
        fillOpacity = 0.9,
        radius = 40
      ) |> 
      addCircleMarkers(
        data = trains_map(),
        opacity = 0.8,
        # label = ~ paste(line_names[line], rn),
        popup = ~ paste(line_names[line], "Line", rn, str_replace(stpDe, "Service", "service")),
        color = ~ hex_color) |>
      setView(
        get_sf_n(as.numeric(input$station)-40000, 1),
        get_sf_n(as.numeric(input$station)-40000, 2),
        zoom = 13) |> 
      addProviderTiles(providers$Stadia.StamenToner)
  )
}

shinyApp(ui, server)