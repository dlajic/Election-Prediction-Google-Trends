## Proxies: Smartproxy ####
# Import proxies (text file)
# data_proxies <- data.frame(readLines("../data.txt")) %>%
# rename("proxy" = "readLines.....data.txt..") %>% #Used direct http out
#   separate(sep = ":",
#            col = "proxy",
#            into = c("ip", "port", "username", "passwort")) %>%
#   mutate(proxy = paste("http://",
#                        username,
#                        ":",
#                        passwort,
#                        "@", ip,
#                        ":",port, sep = ""))

# Test
# proxy_sampled <- sample(data_proxies$proxy, 1)
# Sys.setenv(http_proxy = proxy_sampled,
#            https_proxy = proxy_sampled)  
# Sys.getenv(c("https_proxy", "http_proxy"))# it should be your proxy here now
# 
# # Sys.setenv(no_proxy = "*") # turn off proxies



# Set rotating proxy (smartproxy) ####
# Sys.setenv(http_proxy = "http://user-sp63125425:n8sMsBOH49CP7fEy@us.smartproxy.com:10000",
#            https_proxy = "http://user-sp63125425:n8sMsBOH49CP7fEy@us.smartproxy.com:10000") 
# Sys.setenv(no_proxy = "*") # turn off proxies
# Test if it works: IP should change everytime fromJSON() is called
# fromJSON("https://api.myip.com/")


# library(gtrendsR)
# setHandleParameters(user = "user-sp63125425", 
#                     password = "n8sMsBOH49CP7fEy", 
#                     domain = "77.185.27.222", 
#                     proxyhost = "us.smartproxy.com", 
#                     proxyport = 10000)
# res <- gtrends(c("nhl", "nba"), geo = c("CA", "US"))
# gtrends(keyword= "Merkel", # Ony 1 dataset
#         geo= "DE",
#         category = 19,
#         time = "2005-09-10 2005-09-17",
#         gprop="web",
#         onlyInterest = TRUE)$interest_over_time