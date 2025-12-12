# library(rvest)
# 
# the_url <-  "https://www.transitchicago.com/developers/ttdocs/#_Toc296199909"
# 
# the_url |> 
#   read_html() |> 
#   html_elements("table") |> 
#   {\(x) x[[7]]}() |> 
#   html_table() |> 
#   setNames(c("station", "id")) |> 
#   {\(x) x[-1,]}() |> 
#   readr::write_csv("stations.csv")
#   
## Add missing stations
# readr::read_csv("data/stations.csv") |> 
#   rbind(
#     data.frame(
#       station = c("Merchandise Mart", "Roosevelt/Wabash"),
#       id = c("40460", "40410")
#     )
#   ) |> 
#   arrange(station) |> 
#   readr::write_csv("data/stations.csv")
