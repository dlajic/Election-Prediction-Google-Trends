# Load packages ####
  # install.packages("remotes"); remotes::install_github("Ajfrick/ajfhelpR")

library(pacman)
p_load(gtrendsR,
       ggplot2,
       tidyverse,
       tidyr,
       rvest,
       xml2,
       data.table,
       patchwork,
       lubridate,
       ajfhelpR,
       jsonlite)


# Dataset: Election results ####
# Creates a list
list_electionresults <- NULL
list_electionresults[["2005-09-18"]] <- data.frame(party=c("CDU", "FDP", "Grüne", "Linke", "SPD"),
                                                   share=c(35.2, 9.8, 8.1, 8.7, 34.2))

list_electionresults[["2009-09-27"]] <- data.frame(party=c("CDU", "FDP", "Grüne", "Linke", "SPD"),
                                                   share=c(33.8, 14.6, 10.7, 11.9, 23)) %>% arrange(party)

list_electionresults[["2013-09-22"]] <- data.frame(party=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"),
                                                   share=c(4.7, 41.5, 4.8, 8.4, 8.6, 25.7)) %>% arrange(party)

list_electionresults[["2017-09-24"]] <- data.frame(party=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"),
                                                   share=c(12.6, 32.9, 10.7, 8.9 , 9.2, 20.5)) %>% arrange(party)

list_electionresults[["2021-09-26"]] <- data.frame(party=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"),
                                                   share=c(10.3, 24.1, 11.5, 14.8 , 4.9, 25.7)) %>% arrange(party)





# Scraping survey data from the infratest dimap website


# To be able to weight Google trends data with data from opinion polls,
# polling results of infratest dimap polling institute were scraped from their website on 5th August 2022

#url <- "https://www.infratest-dimap.de/umfragen-analysen/bundesweit/sonntagsfrage/"

#html <- read_html(url)
#tables <- html_table(html, fill=TRUE) # extract the HTML table from the scrapped data with all survey results over time (no other html table on this page of the website)
#infra_dimap_all <- as.data.frame(tables) # construct data frame from scraped htmltable

## delete column for political party "Freie Wähler" and column "Other"
#infra_dimap_all$X8 <- NULL
#infra_dimap_all$X9 <- NULL

## assign names of parties to columns and delete first row of data set (contains names of political parties)
#colnames(infra_dimap_all) <- c("Date", "SPD", "CDU", "Grüne", "FDP", "AFD", "Linke")
#infra_dimap_all <- infra_dimap_all[-1,]


# Load poll data set, scraped on 5th August 2022
load("infratest_dimap_polls.RData")



# Dataset: Models ####

# Create dataframe
data_models <- expand.grid(election_date = as.Date(c("26-09-2021",
                                                     "24-09-2017",
                                                     "22-09-2013",
                                                     "27-09-2009",
                                                     "18-09-2005"), format = "%d-%m-%Y"),
                           datasource_weight = c("GT",
                                                 "GT + election weight",
                                                 "GT + polls weight",
                                                 "GT + weekly polls weight",
                                                 "Only polls"),
                           model_time_interval = duration(seq(7,90, 7)[c(1:4, 6, 8, 10, 11)], "days"),
                           model_time_distance = days(seq(1,90, 1)), # 1 tag vorher, 3 tage, 7 tage, 14 tage
                           model_time_id = "days")




# Convert to tibble
data_models <- as_tibble(data_models)

# Sort dataframe
data_models <- data_models %>% arrange(election_date, datasource_weight, model_time_interval)


## Add model index/number ####
data_models <- data_models %>%
  mutate(model_id = row_number()) %>%
  select(model_id, everything())

## Add GT data collection periods ####
data_models <- data_models %>%
  mutate(GT_end_date = election_date - model_time_distance, # time ends one day before election
         GT_start_date = as.Date(GT_end_date - model_time_interval),
         GT_identifier = paste(GT_start_date, GT_end_date, sep="-")) # time period starts 1 or 3 months earlier

## Add vars for coloring ####
data_models <- data_models %>%
  mutate(election = as.factor(format(election_date, "%d %b, %Y")),
         model_time_period_color = as.factor(as.character(time_length(model_time_interval, unit = "months"))),
         model_time_period_color = paste(model_time_period_color, "month(s)"))


## Add var with parties running in election ####
data_models <- data_models %>%
  mutate(parties = ifelse(election_date <="2009-09-27",
                          list(c("CDU", "SPD", "Grüne", "Linke", "FDP")),
                          list(c("CDU", "SPD", "Grüne", "Linke", "FDP", "AFD"))
  ))


## Add var election results ####

# Write election results to variable
for(i in names(list_electionresults)){

  data_models$data_election[data_models$election_date==i] <-
    list(list_electionresults[[i]])

}

## Add var previous election results ####
for(i in 1:length(list_electionresults)){

  data_models$data_election_previous[data_models$election_date==names(list_electionresults)[i+1]] <-
    list(list_electionresults[[i]])

}



## Add unique model name ####
data_models <- data_models %>%
  mutate(model_name  = paste("M",
          model_id,
          year(election_date),
          "int",
          as.character(time_length(model_time_interval, unit = "days")),
          "days",
          "dist",
          as.character(time_length(model_time_distance, unit = "days")),
          "days",
          gsub("\\s", "_", gsub("\\s+", " ", gsub("\\+", " ", data_models$datasource_weight))),
          sep = "_"))




# Reorder
data_models <- data_models %>%
  select(model_id, model_name, everything())



## Add GT dataset names ####
data_models$name_GT_datasets <- NULL
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("2005", data_models$model_name)] <- list(c("trend_05"))
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("2009", data_models$model_name)] <- list(c("trend_09")) # "trend_05",
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("2013", data_models$model_name)] <- list(c("trend_CDU_13",
                                                                                                                    "trend_AFD_13"))
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("2017", data_models$model_name)] <- list(c("trend_CDU_17",
                                                                                                                    "trend_AFD_17"))
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("2021", data_models$model_name)] <- list(c("trend_CDU_21",
                                                                                                                    "trend_AFD_21"))


## Add GT keywords for each year
data_models$GT_keywords <- NULL
data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("2005", data_models$model_name)] <- list(c('CDU + CSU + Angela Merkel', 'SPD + Gerhard Schröder', 'Grüne + Joschka Fischer', 'PDS + Linkspartei + Gregor Gysi',
                                                                                                               'FDP + Guido Westerwelle'))
data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("2009", data_models$model_name)] <- list(c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Grüne + Jürgen Trittin + Renate Künast', 'Linke + Gregor Gysi',
                                                                                                               'FDP + Guido Westerwelle')) #
data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("2013", data_models$model_name)] <- list(list(c('CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne + ', 'Linke + Sarah Wagenknecht + Gregor Gysi',
                                                                                                               'FDP + Philipp Rösler'),
                                                                                                             c('Bernd Lucke + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne',
                                                                                                               'FDP + Philipp Rösler')))
data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("2017", data_models$model_name)] <- list(list(c('CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne', 'Linke + Sarah Wagenknecht + Dietmar Bartsch',
                                                                                                                    'FDP + Christian Lindner'),
                                                                                                                  c('Alice Weidel + Alexander Gauland + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne',
                                                                                                                    'FDP + Christian Lindner')))
data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("2021", data_models$model_name)] <- list(list(c('CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 'Linke + Janine Wissler + Dietmar Bartsch',
                                                                                                               'FDP + Christian Lindner'),
                                                                                                             c('Alice Weidel + Tino Chrupalla + Afd', 'CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne',
                                                                                                               'FDP + Christian Lindner')))




## Function: Replace search terms ####
replace_searchterms <- function(x){

  # CDU
  x <- gsub('CDU.*', 'CDU', x)

  # SPD
  x <- gsub('SPD.*', 'SPD', x)

  # GREENS
  x <- gsub('Jürgen.*', 'Grüne', x)
  x <- gsub('Annalena.*', 'Grüne', x)
  x <- gsub('Cem.*', 'Grüne', x)
  x <- gsub('Katrin.*', 'Grüne', x)
  x <- gsub('Grüne.*', 'Grüne', x)



  # LINKE
  x <- gsub('Linke.*', 'Linke',  x)
  x <- gsub('PDS.*', 'Linke',  x)

  # FDP
  x <- gsub('FDP.*', 'FDP', x)

  # AFD
  x <- gsub('Bernd.*', 'AFD', x)
  x <- gsub('Alice.*', 'AFD', x)

}



  


  
  
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

## Loop A: Collect GT data ####
  # Idea: Collect GT datasets first then merge with data_model dataframe.
  # 1. Create GT datasets for the different time periods for all 
  # GT data definitions
  # 2. Join with data for all models 
  # Check number of models: length(unique(data_models$GT_identifier))

  

  
  
  # Subset data_models to   
    data_models_GT <- data_models %>%
      filter(datasource_weight=="GT") %>% 
    mutate(data_GT = list(NA)) %>%
  mutate(row_nr = row_number())
  
# Steps
# 1. Choose a new VPN (Proton vpn)
# ". Go to smart proxy website anc copy IP address
# 2. Set domain to own IP Proton VPN, e.g., domain <- "37.120.217.84"
# If you don't you get 407 error.
# 3. Turn of proxies: Sys.setenv(no_proxy = "*")
# 4. Make sure other parameters are up to date.

# Tips: You should experiment on it. Divide on different proxies, 
# divide the queries or try using the code multiple times with different 
# servers (like us.smartproxy.com) if multiple locations are possible
# http code 407: Problem with proxy identification



# Set proxy data
  user <- "user-sp63125425"
  password <- "8ffEGZmupe66Z2Y2"
  domain <- "77.179.81.148"
  proxyhost <- "us.smartproxy.com"
  proxyport <- 10000
  
  # Set rotating proxies: Get IP for "domain" with: fromJSON("https://api.myip.com/")
  setHandleParameters(user = user,
                      password = password,
                      domain = domain,
                      proxyhost = proxyhost,
                      proxyport = proxyport)
  
  # Sys.setenv(http_proxy = "http://user-sp63125425:8ffEGZmupe66Z2Y2@fr.smartproxy.com:40000",
  #            https_proxy = "http://user-sp63125425:8ffEGZmupe66Z2Y2@fr.smartproxy.com:40000") 
  # Show proxies: Sys.getenv(c("https_proxy", "http_proxy"))
  # Sys.setenv(no_proxy = "*")

  # load(file = "data_models_GT.RData")
  # rows_missing_data <- data_models_GT$row_nr[is.na(data_models_GT$data_GT)]
  
  for(i in rows_missing_data){ # 1:nrow(data_models_GT)
    
    cat("\n\n\n\nModel ID: ", data_models_GT$model_id[i], "\n")
    cat("\n\n\n\nrow_nr: ", data_models_GT$row_nr[i], "\n")
    
    # Prepare dataset(s) for model
    year_i <- year(data_models_GT$election_date)[i]
    cat("\n Year:", year_i, "\n")
    

      # cat("\nProxy used:", proxy_i, "\n")

      # Show name of (previous) dataset and keywords
      name_GT_datasets_i <- as.character(data_models_GT$name_GT_datasets[i][[1]])
      cat("\n\nDataset: ", name_GT_datasets_i, "\n")
      GT_keywords_i <- data_models_GT$GT_keywords[i]
      print(GT_keywords_i)
      
      # Detect if 2005/2009 election
      if(length(name_GT_datasets_i)==1){ # THIS PART FOR 2005/2009
        print("\n2005/2009 election\n")
     
        skip_to_next <- FALSE# ERROR HANDLING
        tryCatch({ # ERROR HANDLING
          
          df1 <- gtrends(keyword= GT_keywords_i[[1]], # Ony 1 dataset
                         geo= "DE",
                         category = 19,
                         time = paste(data_models_GT$GT_start_date[i], data_models_GT$GT_end_date[i]),
                         gprop="web",
                         onlyInterest = TRUE)$interest_over_time
        }, 
        error = function(e) { skip_to_next <<- TRUE}) # ERROR HANDLING
        if(skip_to_next) { Sys.sleep(sample(seq(0.5,1,0.01),1)); next } # ERROR HANDLING
        
        data_models_GT$data_GT[[i]] <- df1 %>%
          select(date, hits, keyword) %>%
          mutate(hits = str_replace(hits, "<1", "0"),
                 hits = as.numeric(hits),
                 date = as.Date(date))%>%
          replace(is.na(.), 0) %>%
          mutate(keyword = replace_searchterms(keyword)) # keywords is here party!
        
        print(table(data_models_GT$data_GT[[i]]$keyword))
        
        
        
        # Detect if NO 2005/2009 election
      }else{ # THIS PART 2013-2021
        
        print("\n2013/2021 election\n")
        
        skip_to_next <- FALSE# ERROR HANDLING
        tryCatch({ # ERROR HANDLING

          df1 <- gtrends(keyword= GT_keywords_i[[1]][[1]], # dataset 1
                         geo= "DE",
                         category = 19,
                         time = paste(data_models_GT$GT_start_date[i], data_models_GT$GT_end_date[i]),
                         gprop="web",
                         onlyInterest =  TRUE)$interest_over_time # CDU
        }, 
        error = function(e) { skip_to_next <<- TRUE}) # ERROR HANDLING
        if(skip_to_next) { Sys.sleep(sample(seq(0.5,1,0.01),1)); next } # ERROR HANDLING
        
        
        skip_to_next <- FALSE# ERROR HANDLING
        tryCatch({ # ERROR HANDLING

          df2 <- gtrends(keyword= GT_keywords_i[[1]][[2]], # dataset 1
                         geo= "DE",
                         category = 19,
                         time = paste(data_models_GT$GT_start_date[i], data_models_GT$GT_end_date[i]),
                         gprop="web",
                         onlyInterest =  TRUE)$interest_over_time %>% # AFD
            filter(grepl("Afd.*", keyword) == TRUE)
        }, 
        error = function(e) { skip_to_next <<- TRUE}) # ERROR HANDLING
        if(skip_to_next) { Sys.sleep(sample(seq(0.5,1,0.01),1)); next } # ERROR HANDLING
        
        
        
        data_models_GT$data_GT[[i]] <- bind_rows(df1, df2) %>%
          select(date, hits, keyword) %>%
          mutate(hits = str_replace(hits, "<1", "0"), # HIER WEITER
                 hits = as.numeric(hits),
                 date = as.Date(date))%>%
          replace(is.na(.), 0) %>%
          mutate(keyword = replace_searchterms(keyword))
        
        print(table(data_models_GT$data_GT[[i]]$keyword))
        
      }
      
      
      
      Sys.sleep(sample(seq(1,2,0.001),1)) # Not to overburden gtrends
    }
  
  
  # Check how many datasets were collected 
    table(is.na(data_models_GT$data_GT))
    #save(data_models_GT, file = "data_models_GT.RData")
    #load(file = "data_models_GT_2022_10_31.RData") # Load for several runs
  
    
    
    
    
# START HERE ####
    load(file = "data_models_GT_2022_10_31.RData")
    table(is.na(data_models_GT$data_GT)) # Check whether there are missings
  
# Merge data_models with data_models_GT
  data_models <- left_join(data_models,
                           data_models_GT %>% select(data_GT, GT_identifier),
                           by = "GT_identifier")  
  
  # Check that matching worked (for one model type)
  # data_models %>% 
  #   filter(GT_identifier == "2005-04-04-2005-06-20") %>%
  #   select(model_id, datasource_weight, GT_identifier, data_GT)


  ## Loop B: Create Poll average datasets ####
  # Can we optimize this loop? 
  # For each model: Create poll datasets (averages)
  data_models$data_polls_average <- list(NA)

  for(i in 1:nrow(data_models)){
    cat("\nRow ", i, " out of", nrow(data_models))

    if(grepl("2005", data_models$model_name[i])){
      next
    }

    # Prepare dataset(s) for model
    year_i <- year(data_models$election_date)[i]
    cat("\n\n\n\n", year_i, "\n\n")

      f <- infra_dimap_all %>% # Find polls within GT interval
      mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
      filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i])


      if(nrow(f) == 0){

        if(grepl("2009", data_models$model_name[i])){

        data_models$data_polls_average[[i]] <- infra_dimap_all %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date >= ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"),
                                             data_models$GT_start_date[i], onlypre = T) & Date <= data_models$GT_end_date[i]) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          na.omit(.) %>%
          group_by(party) %>%
          summarize(perc_mean = mean(perc, na.rm=TRUE),
                    SD = sd(perc, na.rm=TRUE), # CHECK THIS DOES NOT WORK IF ONLY ONE POLL IN INTERVAL
                    N = n()) %>% # CHECK
          mutate(lower.ci = perc_mean - 1.96*(SD/sqrt(N)),
                 upper.ci = perc_mean + 1.96*(SD/sqrt(N)))  %>%
          mutate_all(~ifelse(is.nan(.), NA, .)) #%>%
        #filter(!is.na(SD)) # Filter out AFD when no data

      }
  }


      if(nrow(f) == 0){

        if(!grepl("2009", data_models$model_name[i])){

          data_models$data_polls_average[[i]] <- infra_dimap_all %>%
            mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
            filter(Date >= ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"), data_models$GT_start_date[i], onlypre = T) & Date <= data_models$GT_end_date[i]) %>%
            mutate_if(is.character, as.numeric) %>%
            pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
            group_by(party) %>%
            summarize(perc_mean = mean(perc, na.rm=TRUE),
                      SD = sd(perc, na.rm=TRUE),
                      N = n()) %>% # CHECK
            mutate(lower.ci = perc_mean - 1.96*(SD/sqrt(N)),
                   upper.ci = perc_mean + 1.96*(SD/sqrt(N)))  %>%
            mutate_all(~ifelse(is.nan(.), NA, .)) #%>%
          #filter(!is.na(SD)) # Filter out AFD when no data

        }
      }




      if(nrow(f) >= 1){

        if(grepl("2009", data_models$model_name[i])){

          data_models$data_polls_average[[i]] <- infra_dimap_all %>%
            mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
            filter(Date >= ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"), data_models$GT_start_date[i], onlypre = T) & Date <= data_models$GT_end_date[i]) %>%
            mutate_if(is.character, as.numeric) %>%
            pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
            na.omit(.) %>%
            group_by(party) %>%
            summarize(perc_mean = mean(perc, na.rm=TRUE),
                      SD = sd(perc, na.rm=TRUE),
                      N = n()) %>% # CHECK
            mutate(lower.ci = perc_mean - 1.96*(SD/sqrt(N)),
                   upper.ci = perc_mean + 1.96*(SD/sqrt(N)))  %>%
            mutate_all(~ifelse(is.nan(.), NA, .)) #%>%
          #filter(!is.na(SD)) # Filter out AFD when no data


        }
      }


      if(nrow(f) >= 1){

        if(!grepl("2009", data_models$model_name[i])){

          data_models$data_polls_average[[i]] <- infra_dimap_all %>%
            mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
            filter(Date >= ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"), data_models$GT_start_date[i], onlypre = T) & Date <= data_models$GT_end_date[i]) %>%
            mutate_if(is.character, as.numeric) %>%
            pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
            group_by(party) %>%
            summarize(perc_mean = mean(perc, na.rm=TRUE),
                      SD = sd(perc, na.rm=TRUE),
                      N = n()) %>% # CHECK
            mutate(lower.ci = perc_mean - 1.96*(SD/sqrt(N)),
                   upper.ci = perc_mean + 1.96*(SD/sqrt(N)))  %>%
            mutate_all(~ifelse(is.nan(.), NA, .)) #%>%
          #filter(!is.na(SD)) # Filter out AFD when no data


        }
      }

  }
  save(data_models, file = "data_models_B.RData")

  ## Loop C: ADD Predictions (GT) ####
  # Use the GT data, summarize it to create predictions
  # Important: No predictions for AFD for 2009!
  data_models$predictions_GT <- list(NA)

  for(i in 1:nrow(data_models)){
    cat("\nRow ", i, " out of", nrow(data_models))

    if(str_detect(data_models$datasource_weight[i], "GT")){ # Filter GT ONLY

      data_models$predictions_GT[[i]] <- data_models$data_GT[[i]] %>%
        filter(date >= as.Date(data_models$GT_start_date[i], "%d.%m.%y") & date <= as.Date(data_models$GT_end_date[i], "%d.%m.%y")) %>%
        group_by(keyword) %>%
        rename(party=keyword) %>%
        summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
        mutate(prediction = hits_sum/sum(hits_sum)*100) %>%
        select(party, prediction)

    }}
  save(data_models, file = "data_models_C.RData")




  ## Loop D: ADD Predictions (GT + previous election weight) ####
  # Here the GT predictions are simply weighted with the previous election
  # For AFD 2013 we get no prediction because not data for 2009 election
  identifier = sum(str_count(data_models$model_name, "2005"))

  data_models$predictions_GT_election_weight <- list(NA)
  data_models$Weight_Model_2 <- list(NA)

  for(i in 1:nrow(data_models)){
    cat("\nRow ", i, " out of", nrow(data_models))

    if(!grepl("2005", data_models$model_name[[i]]) == TRUE){

      if(grepl("election weight", data_models$datasource_weight[[i]]) == TRUE){ # Filter GT ONLY

        k = i - identifier

        data_models$predictions_GT[[k]][2] <- data_models$predictions_GT[[k]][2] %>%
          mutate(prediction = ifelse(prediction == 0, as.numeric(unlist(data_models$data_election_previous[[i]][2])), prediction))

        t <- (data_models$data_election_previous[[i]][2]/data_models$predictions_GT[[k]][2])

        data_models$Weight_Model_2[[i]] <- cbind(data_models$predictions_GT[[k]][1], t)


        if(grepl("2013", data_models$model_name[[i]]) == TRUE){

          data_models$Weight_Model_2[[i]] <-  data_models$Weight_Model_2[[i]] %>%
            add_row(party = "AFD", share = 1, .before = 1)


        }


        # Apply previously calculated weighting factor on Google Prop. of the interval of interest
        data_models$predictions_GT_election_weight[[i]] <- data_models$data_GT[[i]] %>%
          filter(date >= as.Date(data_models$GT_start_date[i], "%d.%m.%y") & date <= as.Date(data_models$GT_end_date[i], "%d.%m.%y")) %>%
          group_by(keyword) %>%
          rename(party=keyword) %>%
          summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
          mutate(prediction = hits_sum/sum(hits_sum)*100) %>%
          select(party, prediction) %>%
          mutate(prediction = prediction*as.numeric(unlist(data_models$Weight_Model_2[[i]][2])))

      }
    }
  }
  save(data_models, file = "data_models_D.RData")
  # CHANGES: DOES NOT WORK TO TAKE AVERAGE OF PREVIOUS GT DATA (BECAUSE WOULD NEED TO BE RECOLLECTED)


  
  
  
  
  
  ## Loop E: ADD Predictions (GT + polls weight) ####

  #data_models$predictions_GT_before_int <- list(NA)  # OLD
  # data_models$Weight_Model_3 <- list(NA) # OLD
  data_models$predictions_GT_polls <- list(NA)
  data_models$avg_polls_before_int <- list(NA)

  for(i in 1:nrow(data_models)){
    cat("\nRow ", i, " out of", nrow(data_models))

    if(grepl("2005", data_models$model_name[i])){
      next
    }

    if(data_models$datasource_weight[i]=="GT + polls weight"){

      # Get raw data and calculate Google Prop. for the interval before the set interval
        # TAKE THIS OUT!
        # data_models$predictions_GT_before_int[[i]] <- data_models$data_GT[[i]] %>%
        #   filter(date >= as.Date((data_models$GT_start_date[i]-1 - data_models$model_time_interval[[i]]), "%d.%m.%y") & date <= as.Date(data_models$GT_start_date[i]-1, "%d.%m.%y")) %>%
        #   group_by(keyword) %>%
        #   rename(party=keyword) %>%
        #   summarize(hits_sum = sum(hits)) %>%
        #   mutate(prediction = hits_sum/sum(hits_sum)*100) %>%
        #   select(party, prediction)

      # NEW ONLY GET POLL AVERAGES BELOW
      if (grepl("2009", data_models$model_name[i])){

        # Identify polls that lie in the interval before the set interval, take the mean of the identified polls, and delete the AFD row in 2009.
        data_models$avg_polls_before_int[[i]] <- infra_dimap_all %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date >= (data_models$GT_start_date[i]-1 - data_models$model_time_interval[[i]]) &  Date <= data_models$GT_start_date[i]-1) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          group_by(party) %>%
          select(party, perc) %>%
          rename(prediction = perc) %>%
          arrange(party) %>%
          na.omit() %>%
          summarise(prediction = mean(prediction))

      }

      if (!grepl("2009", data_models$model_name[i])){

        # Identify polls that lie in the interval before the set interval, take the mean of the identified polls
        # If AFD NA in 2013, replace NA with Google Proportion to get a weighting factor of 1.
        data_models$avg_polls_before_int[[i]] <- infra_dimap_all %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          #filter(Date >= (data_models$GT_start_date[i]-1 - data_models$model_time_interval[[i]]) &  Date <= data_models$GT_start_date[i]-1) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          group_by(party) %>%
          select(party, perc) %>%
          rename(prediction = perc) %>%
          arrange(party) %>%
          summarise(prediction = mean(prediction)) #%>%
          #replace(is.na(.), as.numeric(unlist(data_models$predictions_GT_before_int[[i]][1,2]))) # If afd NA in 2013, replace NA with Google Proportion to get a weighting factor of 1.

      }

      # In case there are no polls in the defined time span, just use the Google Proportion of the set interval.
      if(nrow(data_models$avg_polls_before_int[[i]]) == 0){

        data_models$predictions_GT_polls[[i]] <- data_models$data_GT[[i]] %>%
          #filter(date >= as.Date(data_models$GT_start_date[i], "%d.%m.%y") & date <= as.Date(data_models$GT_start_date[i], "%d.%m.%y")) %>%
                   group_by(keyword) %>%
                   rename(party=keyword) %>%
                   summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
                   mutate(prediction = hits_sum/sum(hits_sum)*100) %>%
                   select(party, prediction)

                 next
      }


      # Calculate weighting factor using the avg. of the polls before our set interval and the Google Prop. before our set interval
      # t <- (data_models$avg_polls_before_int[[i]][2])/(data_models$predictions_GT_before_int[[i]][2]) # OLD


      # data_models$Weight_Model_3[[i]] <- cbind(data_models$predictions_GT_before_int[[i]][1], t) # OLD

      # Apply previously calculated weighting factor on Google Prop. of the interval of interest

      # data_models$predictions_GT_polls[[i]] <- data_models$data_GT[[i]] %>%
      #   filter(date >= as.Date(data_models$GT_start_date[i], "%d.%m.%y") & date <= as.Date(data_models$GT_end_date[i], "%d.%m.%y")) %>%
      #   group_by(keyword) %>%
      #   rename(party=keyword) %>%
      #   summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
      #   mutate(prediction = hits_sum/sum(hits_sum)*100) %>%
      #   select(party, prediction) %>%
      #   mutate(prediction = prediction*as.numeric(unlist(data_models$Weight_Model_3[[i]][2])))


      # NEW SIMPLY TAKE MEAN OF POLLS BEFORE AND GT PREDICTIONS at INTERVAL
      data_models$predictions_GT_polls[[i]] <-
        left_join(data_models$avg_polls_before_int[[i]],
                  data_models$predictions_GT[[i]],
                  by = "party") %>%
        mutate(prediction = (prediction.x + prediction.y)/2) %>%
        select(party, prediction)

    }
  }
  save(data_models, file = "data_models_E.RData")





  ## Loop G: ADD Predictions (Only polls) ####
  # Here we store average polls result in a new prediction dataframe
  data_models$predictions_only_polls <- list(NA)

  for(i in 1:nrow(data_models)){
    cat("\nRow ", i, " out of", nrow(data_models))

    if(grepl("2005", data_models$model_name[i])){
      next
    }

    # Filter
    if(data_models$datasource_weight[i]=="Only polls"){ # Filter GT ONLY


      data_models$predictions_only_polls[[i]] <-
        data_models$data_polls_average[[i]] %>%
        mutate(prediction = perc_mean) %>%
        select(party, prediction)

    }}
  save(data_models, file = "data_models_G.RData")




  ## Loop H: Merge predictions ####
  # Create column that contains dataframe with all the predictions
  data_models$predictions <- list(NA)

  for(i in 1:nrow(data_models)){
    cat("\nRow ", i, " out of", nrow(data_models))

    if(grepl("2005", data_models$model_name[i])){
      next
    }

    if(data_models$datasource_weight[i]=="GT"){
      data_models$predictions[[i]] <- data_models$predictions_GT[[i]] %>% rename(party_pred=party) # rename
      # because of conflict with names later on
    }


    if(data_models$datasource_weight[i]=="GT + election weight"){
      data_models$predictions[[i]] <- data_models$predictions_GT_election_weight[[i]] %>% rename(party_pred=party)
    }


    if(data_models$datasource_weight[i]=="GT + polls weight"){
      data_models$predictions[[i]] <- data_models$predictions_GT_polls[[i]] %>% rename(party_pred=party)
    }
    # NEW MODEL DOES NOT WORK AT THE MOMENT
    # if(data_models$datasource_weight[i]=="GT + weekly polls weight"){
    #   data_models$predictions[[i]] <- data_models$predictions_GT_weekly_polls_mean[[i]] %>% rename(party_pred=party)
    # }

    if(data_models$datasource_weight[i]=="Only polls"){
      data_models$predictions[[i]] <- data_models$predictions_only_polls[[i]] %>% rename(party_pred=party)
    }
  }
  save(data_models, file = "data_models_H.RData")


  # Dataset: Predictions ####
  # Below we unnest the dataframe to get predictions for single
  # parties across years
  data_predictions <- data_models %>%
    # mutate(df_id = y) %>% # Take out since sampling from google data happens later!
    select(model_id, # df_id,
           model_name, election_date, datasource_weight,
           model_time_interval, GT_end_date, GT_start_date,
           election, model_time_period_color,
           data_election, predictions) %>%
    unnest(cols = c(data_election, predictions))


  ## Add deviations ####
  data_predictions <- data_predictions %>%
    mutate(deviation = prediction - share)

  nrow(data_predictions) # number of predictions (40 models for each party)
  save(data_predictions, file = "data_predictions.RData")
  
 

# CLEAN data_models ####

data_models <- data_models %>%
  filter(grepl("2005", model_name) == FALSE) %>% # delete 2005 rows
    filter(grepl("weekly_polls_weight", model_name) == FALSE) # TAKE OUT WEEKLY POLLS


#And again renaming since first id´s are missing (deleted 2005´s)
## Add model index/number ####
data_models <- data_models %>%
  mutate(model_id = row_number()) %>%
  select(model_id, everything())


# Reorder
data_models <- data_models %>%
  select(model_id, model_name, everything())



# CLEAN data_predictions ####

data_predictions <- data_predictions %>%
  filter(grepl("2005", model_name) == FALSE) %>%
  filter(grepl("weekly_polls_weight", model_name) == FALSE) %>% # filter out models without predictions
  group_by(model_name) %>%
  mutate(model_id = cur_group_id()) %>% # Add new model_id (after filtering)
  ungroup()
 save(data_predictions, file = "data_predictions_cleaned.RData")




# Average over gtrends samples

# data_predictions_final_mean <- data_predictions_final %>%
#   group_by(model_id, model_name, party) %>%
#   summarise(Mean_dev = mean(deviation), SD_dev = sd(deviation), Mean = mean(prediction), SD = sd(prediction), .groups = "keep") %>%
#   mutate(mean_lower.ci = Mean - 1.96*(SD/sqrt(n())),
#          mean_upper.ci = Mean + 1.96*(SD/sqrt(n())),
#          dev_lower.ci = Mean_dev - 1.96*(SD_dev/sqrt(n())),
#          dev_upper.ci = Mean_dev + 1.96*(SD_dev/sqrt(n())))



# Graphs ####

# Prepare data for graph
data_predictions$party <-
  as.factor(ordered(data_predictions$party,
                    levels = c("Grüne", "Linke", "FDP", "AFD", "SPD", "CDU")))
data_predictions$deviation_label <-
  round(data_predictions$deviation, 1)



# data_predictions$datasource_weight <-
#   factor(data_predictions$datasource_weight) %>%
#   mutate()



## Figure X ####
# predictions for different distances #
# BEWARE - NO PREDICTION FOR 2021
data_plot <- data_predictions %>%
  filter(election_date=="2017-09-24"|election_date=="2013-09-22") %>% # filter election - somehow not prediction for 2021
  filter(datasource_weight =="GT",
         # party == "CDU",
         #model_time_interval == "604800s"
         ) %>%# filter out GT models etc.
  mutate(model_time_interval_fac = factor(as.numeric(model_time_interval, "days"))) %>% # convert to days
  mutate(model_time_interval_fac = paste("Interval: ", model_time_interval_fac, " days", sep="")) %>%
  mutate(model_time_distance = election_date - GT_end_date)


# Create average prediction error (across all parties
# THIS DOES NOT WORK...
data_plot2 <- data_plot %>%
  group_by(model_name) %>% 
  summarise(deviation_mean = mean(deviation, na.rm=TRUE)) # STRANGE THAT IT IS CONSTANT.. ERROR?
# Why is the mean of deviations across models exactly the same?


data_plot$model_time_interval_fac <- factor(data_plot$model_time_interval_fac,
                                            levels = c("Interval: 7 days", "Interval: 14 days", "Interval: 21 days", 
                                                       "Interval: 28 days", "Interval: 42 days", "Interval: 56 days", 
                                                       "Interval: 70 days", "Interval: 77 days"),
                                            ordered = TRUE)

x_breaks <- unique(data_plot$GT_end_date)[seq(1, length(unique(data_plot$GT_end_date)), 10)]
x_labels_distance <- unique(data_plot$model_time_distance)[seq(1, length(unique(data_plot$model_time_distance)), 10)]
x_labels_date <- unique(data_plot$GT_end_date)[seq(1, length(unique(data_plot$GT_end_date)), 10)]

cols <- c("SPD" = "red", "CDU" = "black", "AFD" = "blue", 
          "FDP" = "orange", "Link" = "pink", "Grüne" = "green")

p <- ggplot(data_plot,
       aes(x = GT_end_date,
           y = deviation,
           color = party)) +
  geom_point(size = 0.5) +
  geom_line() +
  theme_minimal() +
  facet_grid(vars(model_time_interval_fac),
             vars(election_date), 
             scales = "free_x") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_breaks,
                labels = paste("Distance: ",  x_labels_distance, " day(s)\n",
                               "Date: ", x_labels_date
                               )
               ) +
  scale_color_manual(values = cols) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.Date("2017-09-24"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2013-09-22"),
             linetype="dashed") +
  ylab("Deviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "Party")


p
ggsave(plot = p,
       filename = "plot_predictions.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)  



## Figure 5 ####
ggplot(data = data_predictions,
       aes(x = datasource_weight,
           y = deviation,
           fill = party,
           group = party)) +
  geom_bar(stat="identity", width= 0.6, position=position_dodge(0.9)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"),
                    name = "Political party",
                    breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  labs(y="Percentage", x="") +
  theme_minimal() +
  guides(fill = guide_legend(nrow = 1)) +
  theme(axis.title.x = element_blank(),
        legend.position="top")  +
  facet_wrap(~election_date, ncol = 1) #+

# geom_text(aes(label = deviation_label), # round(deviation, digits = 1)+ifelse(round(deviation, digits = 1) >=0, 3, -3)
#           position=position_dodge(width=.9),
#           angle=90)

