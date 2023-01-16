library(rvest)
library(jsonlite)
library(plyr)
library(tidyverse)

# geonode.com proxies
# to get the total number of proxies we would have to use RSelenium (requires Javascript to run on the page in order to be present. This doesn't happen with rvest)
# at the moment the website says it has 10.311 proxies, so we scrape 20 pages a 500 entries and get 10,000 proxies
pages = seq(1, 20, 1)

data_geonode <- data.frame()

for (i in pages){
  
  json_file <- paste0("https://proxylist.geonode.com/api/proxy-list?limit=500&page=", i, "&sort_by=lastChecked&sort_type=desc")
  json_to_data_i <- as.data.frame(fromJSON(json_file))
  
  data_geonode <- bind_rows(data_geonode, json_to_data_i)
  
  print(i)
  Sys.sleep(0.5)
}


# free-proxy-list.net proxies
 data_free_prox_list <-  read_html("https://free-proxy-list.net/") %>%
   html_nodes(".fpl-list") %>%
   html_table() %>%
   as.data.frame()