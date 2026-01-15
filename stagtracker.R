library(httr2)
library(xml2)
library(purrr)
library(readr)
library(tibble)
library(stringr)
library(lubridate)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)
library(ggimage)
library(leaflet)
library(geojsonsf)
library(sf)
library(fontawesome)

source("app_options.R")

if (file.exists("my_key.R")) {
  source("my_key.R")
}

# christmas <- c(1225, 900:950)
christmas <- c(1225)

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

stations <- read_csv("data/stations.csv") |> 
  {\(x) setNames(x$id, x$station)}()

names(stations) <- names(stations) |> 
  str_remove_all("[ ]?Line[s]?") |> 
  str_squish() |> 
  str_replace_all(" [)]", ")") |> 
  str_replace_all("Blue-([a-zA-Z])", "Blue - \\1")

stations_df <- data.frame(
  station = names(stations),
  id4 = unname(stations),
  id = as.character(as.numeric(unname(stations)) - 40000)
)

cta_lines <- geojson_sf("data/CTA_-_L_Rail_Lines_20251016.geojson")

cta_stations <- geojson_sf("data/CTA_-_L_Rail_Stations_20251016.geojson")

cta_stations_df <- cta_stations |> 
  left_join(stations_df,
            by = join_by(station_id == id)) |> 
  mutate(
    clean_label = station |> 
      str_remove_all(" [(].*[)]$")) #|> 
# mutate(
#   label_dir = case_when(
#     clean_label == "Roosevelt" ~ "left",
#     str_detect(clean_label, "[/]Wabash") ~ "right",
#     .default = "center"
#   ))

get_trains <- function(key = api_key, station = home_station) {
  full_query <- url_modify_query(
    "https://lapi.transitchicago.com/api/1.0/ttarrivals.aspx",
    key = key,
    max = 20,
    mapid = station)
  
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
    # flag predictions older than 2 minutes
    mutate(
      prediction_age = (now() - as_datetime(prdt, tz = "US/Central")) / dminutes(),
      isOld = prediction_age >= 2,
      .after = isFlt
    ) |> 
    mutate(across(starts_with("is"), \(x) as.logical(as.numeric(x)))) |> 
    select(-prediction_age) |> 
    arrange(est)
  
  the_df
}

limit_trains <- function(x, direction = c("northbound", "southbound")) {
  direction <- match.arg(direction)
  if (direction == "southbound") {
    x <- x |> 
      filter(direction == 5)
  } else if (direction == "northbound") {
    x <- x |> 
      filter(direction == 1)
  }
  x
}

make_timetable <- function(x, rows = NULL, christmas_train = christmas, use_img = TRUE) {
  if (!is.null(rows)) {
    x <- x |> 
      filter(row_number() <= rows)
  } else {
    rows <- nrow(x)
  }
  
  if (use_img) {
    tree_64 <- gt:::get_image_uri("tree.png")
    marker <- paste0("<img src='", tree_64, "' width='21px' height='21px' align='center'>")
  } else {
    marker <- as.character(fontawesome::fa("candy-cane"))
  }
  
  x <- x |> 
    mutate(
      dest = ifelse(
        rn %in% christmas_train, 
        paste(dest, html(marker)),
        dest)) |> 
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
    # mark times that have less certainty:
    # - old predictions
    # - delayed trains
    # - predictions based only on schedule
    # - trains indicating a fault
    rowwise() |> 
    mutate(
      estimated = ifelse(any(isOld, isDly, isSch, isFlt), paste0(estimated, "*"), estimated)
    ) |> 
    ungroup()
  
  df_gt <- x |> 
    select(line, dest, estimated) |> 
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
  
  if (nrow(x) <= rows * .6) {
    df_gt <- df_gt |> 
      tab_options(table.font.size = pct(175))
  }
  
  df_gt |> 
    text_replace("minute[s]?", "min", locations = cells_body(columns = estimated)) |> 
    text_replace("([0-9]+)", "<b>\\1</b>", locations = cells_body(columns = estimated)) |> 
    fmt_markdown(columns = estimated) |> 
    fmt_markdown(columns = "dest") |> 
    tab_style(
      style = cell_text(
        size = pct(115),
        align = "center"),
      locations = cells_body(columns = estimated)) |> 
    tab_style(
      locations = cells_body(), 
      style = list(css(
        "color!" = "white", 
        text.shadow = "1px 1px 3px #000000")))
}

plot_trains <- function(x, max = 20, visibility = 1.8, christmas_train = christmas) {
  station <- x$staId[1]
  axis_converter <- function(x, limit = 20) {
    out <- numeric(length(x))
    
    ## Negative numbers
    # 0 to -5 is first half
    condition_n1 <- x < 0 & x >= -(limit/4)
    xn1 <- x[condition_n1]
    out[condition_n1] <- (xn1/(limit/4)) * 0.5
    
    # -6 to -10 is next quarter
    condition_n2 <- x >= -(limit/2) & x < -(limit/4)
    xn2 <- x[condition_n2]
    out[condition_n2] <- (((xn2 - -(limit/4)) / (limit/4)) * 0.25) + -0.5
    
    # -10 to -20 is last quarter
    condition_n3 <- x < -(limit/2)
    xn3 <- x[condition_n3]
    out[condition_n3] <- (((xn3 - -(limit/2)) / (limit/2)) * 0.25) + -0.75  
    
    ## Positive numbers
    # 0 to 5 is first half
    condition_1 <- x > 0 & x <= (limit/4)
    x1 <- x[condition_1]
    out[condition_1] <- (x1/(limit/4)) * 0.5
    
    # 6 to 10 is next quarter
    condition_2 <- x <= (limit/2) & x > (limit/4)
    x2 <- x[condition_2]
    out[condition_2] <- (((x2 - (limit/4)) / (limit/4)) * 0.25) + 0.5
    
    # 10 to 20 is last quarter
    condition_3 <- x > (limit/2)
    x3 <- x[condition_3]
    out[condition_3] <- (((x3 - (limit/2)) / (limit/2)) * 0.25) + 0.75  
    
    out 
  }
  
  size_multiplier <- visibility
  
  if (length(unique(x$direction)) == 1 && unique(x$direction) == 1) {
    the_direction <- "north"
  } else if (length(unique(x$direction)) == 1 && unique(x$direction) == 5) {
    the_direction <- "south"
  } else if (1 %in% x$direction & 5 %in% x$direction) {
    the_direction <- "all"
  } else {
    stop("Something is wrong with direction")
  }
  
  commuting_data <- 
    x |>
    filter(abs(est) <= max) |> 
    arrange(desc(est)) |> 
    mutate(
      adjust_est = ifelse(direction == 5, est, -1 * est) |> 
        axis_converter(limit = max),
      y_value = paste0(direction, "_", dest),
      est_label = round(est) |> 
        str_replace_all("\\b0\\b", "-"))
  
  comm_plot <- commuting_data |> 
    ggplot(aes(adjust_est, y_value)) + 
    geom_vline(
      xintercept = 0,
      color = "white") +
    annotate(
      "label",
      x = 0,
      y = (length(unique(commuting_data$y_value)) + 1) / 2,
      label = cta_stations_df[cta_stations_df$id4 == station,] |> pull(clean_label),
      angle = 90,
      color = "white",
      fill = "black",
      size = 4 * size_multiplier
    ) +
    scale_y_discrete()
  
  if (the_direction != "north") {
    comm_plot <- comm_plot +
      geom_vline(
        xintercept = .5, 
        color = "gray", 
        linetype = "dashed") + 
      annotate(
        "label",
        x = 0.5,
        y = (length(unique(commuting_data$y_value)) + 1) / 2,
        label = paste(round(max/4), "min"),
        angle = 90,
        color = "gray",
        linetype = "dashed",
        fill = "black",
        size = 4 * size_multiplier
      ) +
      geom_vline(
        xintercept = 0.75, 
        color = "#555555", 
        linetype = "dotted") +
      annotate(
        "label",
        x = 0.75,
        y = (length(unique(commuting_data$y_value)) + 1) / 2,
        label = paste(round(max/2), "min"),
        angle = 90,
        color = "#555555",
        linetype = "dotted",
        fill = "black",
        size = 4 * size_multiplier
      )
  } 
  if (the_direction != "south") {
    comm_plot <- comm_plot +
      geom_vline(
        xintercept = -.5, 
        color = "gray", 
        linetype = "dashed") + 
      annotate(
        "label",
        x = -0.5,
        y = (length(unique(commuting_data$y_value)) + 1) / 2,
        label = paste(round(max/4), "min"),
        angle = 90,
        color = "gray",
        linetype = "dashed",
        fill = "black",
        size = 4 * size_multiplier
      ) +
      geom_vline(
        xintercept = -0.75, 
        color = "#555555", 
        linetype = "dotted") +
      annotate(
        "label",
        x = -0.75,
        y = (length(unique(commuting_data$y_value)) + 1) / 2,
        label = paste(round(max/2), "min"),
        angle = 90,
        color = "#555555",
        linetype = "dotted",
        fill = "black",
        size = 4 * size_multiplier
      )
  }
  
  if (the_direction == "south") {
    comm_plot <- comm_plot +
      scale_x_continuous(limits = c(-0.01, 1))
  } else if (the_direction == "north") {
    comm_plot <- comm_plot +
      scale_x_continuous(limits = c(-1, 0.01))
  } else {
    comm_plot <- comm_plot +
      scale_x_continuous(limits = c(-1, 1))
  }
  
  
  comm_plot + 
    geom_point(
      # data = filter(commuting_data, !rn %in% christmas_train),
      aes(fill = line), 
      color = "black",
      shape = 21,
      size = 7 * size_multiplier) +
    geom_image(
      data = filter(commuting_data, rn %in% christmas_train),
      image = "tree.png",
      size = 0.06) +
    geom_point(
      data = filter(commuting_data, est < (1 + 0.375 * max), round(est) > 1), 
      aes(
        fill = line
        # shape = est <= 3,
      ), 
      size = 20 * size_multiplier,
      shape = 21,
      color = "black") + 
    geom_image(
      data = filter(commuting_data, est < (1 + 0.375 * max), round(est) > 1, rn %in% christmas_train), 
      image = "tree.png",
      size = 0.18) +
    geom_point(
      data = filter(commuting_data, round(est) <= 1), 
      aes(
        fill = line,
        # shape = est <= 3,
      ), 
      size = 7 * size_multiplier, 
      shape = 21,
      color = "black") + 
    geom_image(
      data = filter(commuting_data, rn %in% christmas_train, round(est) <= 1),
      image = "tree.png",
      size = 0.06) +
    geom_text(
      data = filter(commuting_data, est < (1 + 0.375 * max), round(est) > 1), 
      aes(label = est_label), 
      color = "white", 
      size = 13 * size_multiplier) + 
    geom_text(
      data = filter(commuting_data, round(est) <= 1), 
      aes(label = est_label), 
      color = "white", 
      size = 5 * size_multiplier) + 
    scale_fill_manual(values = official_colors) + 
    theme_void() + 
    theme(
      legend.position = "none",
      panel.background = element_rect(fill = "black", colour = NA),
      plot.background = element_rect(fill = "black", colour = NA)
    ) 
  
}

plotly_trains <- function(x, max = 20, visibility = 1.8){
  if (inherits(x, "ggplot")) {
    z <- x
  } else {
    z <- x |> 
      plot_trains(max, visibility)
  }
  z |> 
    plotly::ggplotly()
}

map_trains <- function(x, christmas_train = christmas){
  station <- x$staId[1]
  get_sf_n <- function(id = NULL, n) {
    if (is.null(id)) {
      coords <- cta_stations$geometry |> 
        st_coordinates()
      result <- coords[,n]
    } else {
      coords <- cta_stations$geometry[cta_stations$station_id==id] |> 
        st_coordinates()
      result <- coords[n]
    }
    result
  }
  
  map_data <- x |> 
    drop_na(lon, lat) |> 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
    rowwise() |> 
    mutate(hex_color = unname(official_colors[line])) |>
    ungroup()
  
  leaflet(options = leafletOptions(
    attributionControl=FALSE)) |> 
    addTiles() |> 
    addPolylines(
      data = cta_lines,
      color = "#992", 
      opacity = 0.7, 
      weight = 3.5) |> 
    addRectangles(
      # data = cta_stations_df,
      lng1 = get_sf_n(n = 1),
      lat1 = get_sf_n(n = 2),
      lng2 = get_sf_n(n = 1),
      lat2 = get_sf_n(n = 2),
      weight = 7,
      color = "#bb3",
      opacity = 0.75,
      popup = paste("<b>", cta_stations_df$clean_label, "</b><br>", cta_stations_df$lines),
    ) |>
    addLabelOnlyMarkers(
      data = cta_stations_df[cta_stations_df$id4 == station,],
      label = ~ clean_label,
      labelOptions = labelOptions(
        noHide = TRUE, textOnly = TRUE,
        direction = "center", style = list(color = "#ffffff")
      )
    ) |> 
    addCircleMarkers(
      data = map_data,
      opacity = 0.9,
      popup = ~ paste(
        line_names[line], "Line", rn,
        "service",
        "toward", dest),
      color = "white",
      weight = 1,
      fillColor = ~ hex_color,
      fillOpacity = 0.7) |> 
    addMarkers(
      data = x |> mutate(across(c(lat, lon), as.numeric)),
      lat = ~ lat,
      lng = ~ lon,
      icon = icons(
        iconUrl = "www/tree.png",
        iconWidth = ifelse(x$rn %in% christmas_train,
                           20,
                           1), 
        iconHeight = ifelse(x$rn %in% christmas_train,
                            20,
                            1)
      ),
      popup = ~ paste(html("<center><img src='tree.png' width='50px' height='50px'></center><br>"), line_names[line], "Line", rn, "Holiday service toward", dest)
    ) |> 
    setView(
      get_sf_n(as.numeric(station)-40000, 1) + sample(runif(6, -0.00006, 0.00006), size = 1),
      get_sf_n(as.numeric(station)-40000, 2) + sample(runif(6, -0.00006, 0.00006), size = 1),
      zoom = sample(12:14, size = 1)
    ) |> 
    addProviderTiles(providers$CartoDB.DarkMatterNoLabels)
  
}
