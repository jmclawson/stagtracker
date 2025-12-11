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

christmas_train <- 1225# testing: c(1225, 410, 820, 804, 821, 811, 404, 516, 906, 813, 408)

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
    
    .leaflet-control-zoom a {
      background-color: #445;
      color: #aaa
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
    # htmlOutput("bottom_bar"),
    div(
      id = "bottom_bar",
      # style = textOutput("bottom_bar_css"),#"position: absolute; bottom: 5px; width: 100vw; padding-right: 24px;",
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
          condition = "input.station == input.my_station & input.map_toggle == ''",
          div(
            style = "float: right;",
            checkboxGroupButtons(
              inputId = "limit_line",
              label = "",
              choices = c(commute_label)
            )
          )
        ),
      )
    ),
    conditionalPanel(
      condition = "input.map_toggle == ''",
      div(id = "tymetable", style = "clear: both;",
        htmlOutput("timetable", width = "90%"))),
    conditionalPanel(
      condition = "input.map_toggle != ''",
      div(style = "float: center;",
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
    if (input$station == home_station && current_hour() %in% 6:11 && wday(now()) %in% 2:6) {
      # weekday morns, limit to Red
      updateCheckboxGroupButtons(
        session = session,
        inputId = "limit_line",
        selected = commute_label
      )
    } else if (input$station == home_station && current_hour() %in% 19:23) {
      # evenings, reset the limit
      updateCheckboxGroupButtons(
        session = session,
        inputId = "limit_line",
        selected = ""
      )
    }
  })
  
  observe({
    invalidateLater(1 * 30 * 60 * 1000, session)
    if (current_hour() %in% 6:11) {updateCheckboxGroupButtons(
      session, 
      "map_toggle", 
      selected = "")
    } else if (current_minutes() < 30) {
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
  
  observeEvent(input$map_toggle, {
    arrow_state(2 %% 3)
  })
  
  current_minutes <- reactive({
    invalidateLater(1 * 60 * 1000, session)
    now() |> minute()
  })
  
  current_hour <- reactive({
    invalidateLater(30 * 60 * 1000, session)
    now() |> hour()
  })

  output$dynamic_css <- renderUI({
    if ((current_minutes() %% 10 < 5)) {
      # buttons at bottom
      tags$head(
        tags$style(HTML(paste0(
          "#bottom_bar {position: absolute; bottom: ", sample(5:7, size = 1), "px; top: unset; width: 100vw; padding-right: ", sample(23:26, size = 1), "px; margin-top: unset;}
          #mapit {margin-top: 0px;}
          #tymetable {margin-top: -12px; position: absolute; bottom: unset;}"
        )))
      )
    } else {
      # buttons at top
      tags$head(
        tags$style(HTML(paste0(
          "#bottom_bar {position: absolute; bottom: unset; top: ", sample(0:3, size = 1), "px; width: 100vw; padding-right: ", sample(23:26, size = 1), "px; margin-top: -24px;}
          #mapit {margin-top: 41px;}
          #tymetable {position: absolute; bottom: 5px; top: unset;}"
        )))
      )
    }
  })
  
  output$timetable <- renderUI({
    if(direction_order[arrow_state() + 1] == direction_order[3]) {
      input <- train_times()
    } else if(direction_order[arrow_state() + 1] == direction_order[2]){
      input <- train_times_north()
    } else {
      input <- train_times_south()
    }
    input |> 
      mutate(dest = ifelse(rn %in% christmas_train, 
                           paste(dest, 
                                 # html("&#x1f384; &#x1f384;&#xFE0E; &#10052; &#10052;&#xFE0E;"), 
                                 html("<img src='tree.png' width='34px' height='34px'>")
                                 ), dest)) |> 
      select(line, dest, est) |> 
      style_timetable_gt() |> 
      gt::fmt_markdown(columns = "dest")
  })
  
  trains_map <- reactive({
    if(direction_order[arrow_state() + 1] == direction_order[3]) {
      starting_df <- train_times()
    } else if(direction_order[arrow_state() + 1] == direction_order[2]){
      starting_df <- train_times_north()
    } else {
      starting_df <- train_times_south()
    }
    
    starting_df |> 
      tidyr::drop_na(lon, lat) |> 
      sf::st_as_sf(
        coords = c("lon", "lat"), crs = 4326) |> 
      rowwise() |> 
      mutate(
        hex_color = unname(official_colors[line])
      ) |>
      ungroup()})
  
  get_sf_n <- function(id = NULL, n) {
    if (is.null(id)) {
      coords <- cta_stations$geometry |> 
        sf::st_coordinates()
      result <- coords[,n]
    } else {
      coords <- cta_stations$geometry[cta_stations$station_id==id] |> 
        sf::st_coordinates()
      result <- coords[n]
    }
    result
  }
  
  convert_heading <- function(x){
    x <- as.numeric(x) %% 360
    direction <- round(x / 45)
    
    cta_compass <- c(
      "north", 
      "northeast",
      "east",
      "southeast",
      "south",
      "southwest",
      "west",
      "northwest"
    )
    
    cta_compass[(direction + 1) %% 8]
  }
  
  output$mapit <- renderLeaflet(
    leaflet(options = leafletOptions(
      attributionControl=FALSE)) |> 
      addTiles() |> 
      addPolylines(
        data = cta_lines,
        color = "#992", 
        opacity = 0.7, 
        weight = 3.5) |> 
      addCircleMarkers(
        data = trains_map(),
        opacity = 0.9,
        popup = ~ paste(
          line_names[line], "Line", rn,
          "service",
          # convert_heading(heading), # can't trust heading data
          "toward", dest),
        color = "white",
        weight = 1,
        fillColor = ~ hex_color,
        fillOpacity = 0.7) |>
      setView(
        get_sf_n(as.numeric(input$station)-40000, 1) + sample(runif(6, -0.00006, 0.00006), size = 1),
        get_sf_n(as.numeric(input$station)-40000, 2) + sample(runif(6, -0.00006, 0.00006), size = 1),
        zoom = sample(12:14, size = 1)
        ) |> 
      addRectangles(
        lng1 = get_sf_n(n = 1),
        lat1 = get_sf_n(n = 2),
        lng2 = get_sf_n(n = 1),
        lat2 = get_sf_n(n = 2),
        weight = 7,
        color = "#bb3",
        opacity = 0.95,
        popup = paste("<b>", cta_stations$longname, "</b><br>", cta_stations$lines),
      ) |> 
      addMarkers(
        data = train_times() |> mutate(across(c(lat, lon), as.numeric)),
        lat = ~ lat,
        lng = ~ lon,
        icon = icons(
          iconUrl = "www/tree.png",
          iconWidth = ifelse(train_times()$rn %in% christmas_train,
                             20,
                             1), 
          iconHeight = ifelse(train_times()$rn %in% christmas_train,
                              20,
                              1)
        ),
        popup = ~ paste(html("<center><img src='tree.png' width='50px' height='50px'></center><br>"), line_names[line], "Line", html("holiday"), rn, str_replace(stpDe, "Service", "service"))
        ) |> 
      # addProviderTiles(providers$Stadia.StamenToner) |> 
      addProviderTiles(providers$Stadia.AlidadeSmoothDark)
  )
}

shinyApp(ui, server)