# Set API key
api_key <- "a0000aaaaa0000000aa0000aaa00"

# Home Station (41320 is Belmont)
home_station <- 41320

# Primary line for commute
# Options are Red, Org, Y, G, Blue, P, Brn, Pink 
commute <- "Red"
commute_label <- substr(commute, 1, 1)

# Limit number of rows shown
show_rows <- 5

# Adjust the order to favor ▲
# direction_order <- c("▼", "▲")#, c("↓", "↑", "↕"
direction_order <- c("↓", "↑", "↕")
