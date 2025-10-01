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