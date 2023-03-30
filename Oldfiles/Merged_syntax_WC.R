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


setwd("/cloud/project")

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







# Load poll data set, scraped on 5th August 2022
load("infratest_dimap_polls.RData")# -> CSV FILE



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
                                                 "Only polls",
                                                 "Last polls"),
                           #model_time_interval = duration(seq(7,14, 7), "days"),
                           #model_time_distance = days(seq(1, 3, 1))) # 1 tag vorher, 3 tage, 7 tage, 14 tage # 1 tag vorher, 3 tage, 7 tage, 14 tage
                           model_time_interval = duration(seq(7,91, 7)[c(1:4, 6, 8, 10, 13)], "days"),
                           model_time_distance = days(seq(1, 150, 1)), # 1 tag vorher, 3 tage, 7 tage, 14 tage # 1 tag vorher, 3 tage, 7 tage, 14 tage
                           model_time_id = "days")



data_models$election_date

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
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2005", data_models$model_name)] <- list(c("trend_05_WK"))
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2009", data_models$model_name)] <- list(c("trend_09_WK")) # "trend_05_WK",
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2013", data_models$model_name)] <- list(c("trend_CDU_13_WK",
                                                                                                                    "trend_AFD_13_WK"))
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2017", data_models$model_name)] <- list(c("trend_CDU_17_WK",
                                                                                                                    "trend_AFD_17_WK"))
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2021", data_models$model_name)] <- list(c("trend_CDU_21_WK",
                                                                                                                    "trend_AFD_21_WK"))


## Add GT keywords for each year
data_models$GT_keywords <- NULL
data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("M_\\d+_2005", data_models$model_name)] <- list(c('CDU + CSU + Angela Merkel', 'SPD + Gerhard Schröder', 'Grüne + Joschka Fischer', 'PDS + Linkspartei + Gregor Gysi',
                                                                                                               'FDP + Guido Westerwelle'))
data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("M_\\d+_2009", data_models$model_name)] <- list(c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Grüne + Jürgen Trittin + Renate Künast', 'Linke + Gregor Gysi',
                                                                                                               'FDP + Guido Westerwelle')) #
data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("M_\\d+_2013", data_models$model_name)] <- list(list(c('CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne + ', 'Linke + Sarah Wagenknecht + Gregor Gysi',
                                                                                                               'FDP + Philipp Rösler'),
                                                                                                             c('Bernd Lucke + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne',
                                                                                                               'FDP + Philipp Rösler')))
data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("M_\\d+_2017", data_models$model_name)] <- list(list(c('CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne', 'Linke + Sarah Wagenknecht + Dietmar Bartsch',
                                                                                                                    'FDP + Christian Lindner'),
                                                                                                                  c('Alice Weidel + Alexander Gauland + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne',
                                                                                                                    'FDP + Christian Lindner')))
data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("M_\\d+_2021", data_models$model_name)] <- list(list(c('CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 'Linke + Janine Wissler + Dietmar Bartsch',
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

 


## Loop A: Collect GT data ####
  # Idea: collect GT datasets first then merge with data_model dataframe.
  # 1. Create GT datasets for the different time periods for all GT data definitions
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
  #user <- "user-sp63125425"
  #password <- "8ffEGZmupe66Z2Y2"
  #domain <- "77.179.81.148"
  #proxyhost <- "us.smartproxy.com"
  #proxyport <- 10000
  
  # Set rotating proxies: Get IP for "domain" with: fromJSON("https://api.myip.com/")
  #setHandleParameters(user = user,
                      #password = password,
                      #domain = domain,
                      #proxyhost = proxyhost,
                      #proxyport = proxyport)
  
  # Sys.setenv(http_proxy = "http://user-sp63125425:8ffEGZmupe66Z2Y2@fr.smartproxy.com:40000",
  #            https_proxy = "http://user-sp63125425:8ffEGZmupe66Z2Y2@fr.smartproxy.com:40000") 
  # Show proxies: Sys.getenv(c("https_proxy", "http_proxy"))
  # Sys.setenv(no_proxy = "*")
  
  # CHECK IF PROXY WORKS
  # gtrends(keyword= "Merkel", # Ony 1 dataset
  #         geo= "DE",
  #         category = 19,
  #         time = "2005-09-10 2005-09-17",
  #         gprop="web",
  #         onlyInterest = TRUE)$interest_over_time

  # load(file = "data_models_GT.RData")
  # rows_missing_data <- data_models_GT$row_nr[is.na(data_models_GT$data_GT)]
  
  #for(i in rows_missing_data){ # 1:nrow(data_models_GT)
  #  
  #  cat("\n\n\n\nModel ID: ", data_models_GT$model_id[i], "\n")
  #  cat("\n\n\n\nrow_nr: ", data_models_GT$row_nr[i], "\n")
  #  
  #  # Prepare dataset(s) for model
  #  year_i <- year(data_models_GT$election_date)[i]
  #  cat("\n Year:", year_i, "\n")
  #  
#
  #    # cat("\nProxy used:", proxy_i, "\n")
#
  #    # Show name of (previous) dataset and keywords
  #    name_GT_datasets_i <- as.character(data_models_GT$name_GT_datasets[i][[1]])
  #    cat("\n\nDataset: ", name_GT_datasets_i, "\n")
  #    GT_keywords_i <- data_models_GT$GT_keywords[i]
  #    print(GT_keywords_i)
  #    
  #    # Detect if 2005/2009 election
  #    if(length(name_GT_datasets_i)==1){ # THIS PART FOR 2005/2009
  #      print("\n2005/2009 election\n")
  #   
  #      skip_to_next <- FALSE# ERROR HANDLING
  #      tryCatch({ # ERROR HANDLING
  #        
  #        df1 <- gtrends(keyword= GT_keywords_i[[1]], # Ony 1 dataset
  #                       geo= "DE",
  #                       category = 19,
  #                       time = paste(data_models_GT$GT_start_date[i], data_models_GT$GT_end_date[i]),
  #                       gprop="web",
  #                       onlyInterest = TRUE)$interest_over_time
  #      }, 
  #      error = function(e) { skip_to_next <<- TRUE}) # ERROR HANDLING
  #      if(skip_to_next) { Sys.sleep(sample(seq(0.5,1,0.01),1)); next } # ERROR HANDLING
  #      
  #      data_models_GT$data_GT[[i]] <- df1 %>%
  #        select(date, hits, keyword) %>%
  #        mutate(hits = str_replace(hits, "<1", "0"),
  #               hits = as.numeric(hits),
  #               date = as.Date(date))%>%
  #        replace(is.na(.), 0) %>%
  #        mutate(keyword = replace_searchterms(keyword)) # keywords is here party!
  #      
  #      print(table(data_models_GT$data_GT[[i]]$keyword))
  #      
  #      
  #      
  #      # Detect if NO 2005/2009 election
  #    }else{ # THIS PART 2013-2021
  #      
  #      print("\n2013/2021 election\n")
  #      
  #      skip_to_next <- FALSE# ERROR HANDLING
  #      tryCatch({ # ERROR HANDLING
#
  #        df1 <- gtrends(keyword= GT_keywords_i[[1]][[1]], # dataset 1
  #                       geo= "DE",
  #                       category = 19,
  #                       time = paste(data_models_GT$GT_start_date[i], data_models_GT$GT_end_date[i]),
  #                       gprop="web",
  #                       onlyInterest =  TRUE)$interest_over_time # CDU
  #      }, 
  #      error = function(e) { skip_to_next <<- TRUE}) # ERROR HANDLING
  #      if(skip_to_next) { Sys.sleep(sample(seq(0.5,1,0.01),1)); next } # ERROR HANDLING
  #      
  #      
  #      skip_to_next <- FALSE# ERROR HANDLING
  #      tryCatch({ # ERROR HANDLING
#
  #        df2 <- gtrends(keyword= GT_keywords_i[[1]][[2]], # dataset 1
  #                       geo= "DE",
  #                       category = 19,
  #                       time = paste(data_models_GT$GT_start_date[i], data_models_GT$GT_end_date[i]),
  #                       gprop="web",
  #                       onlyInterest =  TRUE)$interest_over_time %>% # AFD
  #          filter(grepl("Afd.*", keyword) == TRUE)
  #      }, 
  #      error = function(e) { skip_to_next <<- TRUE}) # ERROR HANDLING
  #      if(skip_to_next) { Sys.sleep(sample(seq(0.5,1,0.01),1)); next } # ERROR HANDLING
  #      
  #      
  #      
  #      data_models_GT$data_GT[[i]] <- bind_rows(df1, df2) %>%
  #        select(date, hits, keyword) %>%
  #        mutate(hits = str_replace(hits, "<1", "0"), # HIER WEITER
  #               hits = as.numeric(hits),
  #               date = as.Date(date))%>%
  #        replace(is.na(.), 0) %>%
  #        mutate(keyword = replace_searchterms(keyword))
  #      
  #      print(table(data_models_GT$data_GT[[i]]$keyword))
  #      
  #    }
  #    
  #    
  #    
  #    Sys.sleep(sample(seq(1,2,0.001),1)) # Not to overburden gtrends
  #  }
  
  
  # Check how many datasets were collected 
    #table(is.na(data_models_GT$data_GT))
    #save(data_models_GT, file = "data_models_GT.RData")
    #load(file = "data_models_GT_2022_10_31.RData") # Load for several runs
  

# START HERE ####
    #load(file = "2022-07-14 10-13-10.RData")
    #table(is.na(data_models_GT$data_GT)) # Check whether there are missings
  
# Merge data_models with data_models_GT
  #data_models <- left_join(data_models,
  #                         data_models_GT %>% select(data_GT, GT_identifier),
  #                         by = "GT_identifier")  
  
  # Check that matching worked (for one model type)
  # data_models %>% 
  #   filter(GT_identifier == "2005-04-04-2005-06-20") %>%
  #   select(model_id, datasource_weight, GT_identifier, data_GT)

  
  
  ## Loop A: Create GT datasets ####
  # Create GT datasets for the different time periods for all models that include GT data (see filter below)
  data_models$data_GT_year <- list(NA)
  
  # Names of Google Trends datasets
setwd("./Data_WC")
dir <- getwd()
names_df <- list.files(dir)  

  data_predictions_final <- data.frame()
  
  start_time <- Sys.time()
  
  
  for(y in names_df){
    
    for(i in 1:nrow(data_models)){
      # Load GT datasets
      name <- y
      load(name)
      
      
      
      # Prepare dataset(s) for model
      year_i <- year(data_models$election_date)[i]
      cat("\n\n\n\n", year_i, "\n\n")
      
      # Detect models using GT data
      if(str_detect(data_models$datasource_weight[i], "GT")){
        
        
        
        # Select GT datasets relevant for this year and merge
        name_GT_datasets_i <- as.character(data_models$name_GT_datasets[i][[1]])
        print(name_GT_datasets_i)
        
        # Detect if 2005/2009 election
        if(length(name_GT_datasets_i)==1){ # THIS PART FOR 2005/2009
          
          df1 <- get(name_GT_datasets_i[1])$interest_over_time # Ony 1 dataset
          
          data_models$data_GT_year[[i]] <- df1 %>%
            select(date, hits, keyword) %>%
            mutate(hits = str_replace(hits, "<1", "0"), 
                   hits = as.numeric(hits), 
                   date = as.Date(date))%>% 
            replace(is.na(.), 0) %>%
            mutate(keyword = replace_searchterms(keyword))
          
          # Detect if NO 2009 election
        }else{ # THIS PART 2013-2021
          
          df1 <- get(name_GT_datasets_i[1])$interest_over_time # CDU
          
          df1 <- df1 %>% select(date, hits, keyword) %>%
            mutate(hits = str_replace(hits, "<1", "0"), # HIER WEITER
                   hits = as.numeric(hits), 
                   date = as.Date(date))%>% 
            replace(is.na(.), 0) %>%
            mutate(keyword = replace_searchterms(keyword))
          
          df2 <- get(name_GT_datasets_i[2])$interest_over_time %>% # AFD
            filter(grepl("Afd.*", keyword) == TRUE)
          
          df2 <- df2 %>% select(date, hits, keyword) %>%
            mutate(hits = str_replace(hits, "<1", "0"), # HIER WEITER
                   hits = as.numeric(hits), 
                   date = as.Date(date))%>% 
            replace(is.na(.), 0) %>%
            mutate(keyword = replace_searchterms(keyword))
          
          
          data_models$data_GT_year[[i]] <- bind_rows(df1, df2) 
        }
        }
      }
    

  ## Loop B: Create Poll average datasets ####
  # Can we optimize this loop? 
  # For each model: Create poll datasets (averages)
  data_models$data_polls_average <- list(NA)

  for(i in 1:nrow(data_models)){
    cat("\nRow ", i, " out of", nrow(data_models))

    if(grepl("M_\\d+_2005", data_models$model_name[i])){
      next
    }

    # Prepare dataset(s) for model
    year_i <- year(data_models$election_date)[i]
    cat("\n\n\n\n", year_i, "\n\n")

      f <- infra_dimap_all %>% # Find polls within GT interval
      mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
      filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i])


      if(nrow(f) == 0){

        if(grepl("M_\\d+_2009", data_models$model_name[i])){

        data_models$data_polls_average[[i]] <- infra_dimap_all %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date == ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"),
                                             data_models$GT_start_date[i], onlypre = T)) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          na.omit(.) %>% # here the AFD gets removed
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

        if(!grepl("M_\\d+_2009", data_models$model_name[i])){

          data_models$data_polls_average[[i]] <- infra_dimap_all %>%
            mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
            filter(Date == ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"),
                                               data_models$GT_start_date[i], onlypre = T)) %>%          
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

        if(grepl("M_\\d+_2009", data_models$model_name[i])){

          data_models$data_polls_average[[i]] <- f %>%
            mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
            mutate_if(is.character, as.numeric) %>%
            pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
            na.omit(.) %>% # here the AFD gets removed
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

        if(!grepl("M_\\d+_2009", data_models$model_name[i])){

          data_models$data_polls_average[[i]] <- f %>%
            mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
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
  #save(data_models, file = "data_models_B.RData")

  
  ##Loop B2: Only last poll ##
  # logic is the same like for polls average, but this time we are using just the last Poll before the time distance
  
  data_models$data_polls_last <- list(NA)
  
  for(i in 1:nrow(data_models)){
    cat("\nRow ", i, " out of", nrow(data_models))
    
    if(grepl("M_\\d+_2005", data_models$model_name[i])){
      next
    }
    
    # Prepare dataset(s) for model
    year_i <- year(data_models$election_date)[i]
    cat("\n\n\n\n", year_i, "\n\n")
    
    f2 <- infra_dimap_all %>% # Find polls within GT interval
      mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
      filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]) %>%
      slice(1)
    
    
    if(nrow(f2) == 0){
      
      if(grepl("M_\\d+_2009", data_models$model_name[i])){
        
        data_models$data_polls_last[[i]] <- infra_dimap_all %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date == ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"),
                                             data_models$GT_start_date[i], onlypre = T)) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          na.omit(.) %>% # here the AFD gets removed
          group_by(party) %>%
          summarize(perc_mean = mean(perc, na.rm=TRUE),
                    SD = sd(perc, na.rm=TRUE),
                    N = n()) %>% # CHECK
          mutate_all(~ifelse(is.nan(.), NA, .))
      }
    }
    
    
    if(nrow(f2) == 0){
      
      if(!grepl("M_\\d+_2009", data_models$model_name[i])){
        
        data_models$data_polls_last[[i]] <- infra_dimap_all %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date == ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"),
                                             data_models$GT_start_date[i], onlypre = T)) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          group_by(party) %>%
          summarize(perc_mean = mean(perc, na.rm=TRUE),
                    SD = sd(perc, na.rm=TRUE),
                    N = n()) %>% # CHECK
          mutate_all(~ifelse(is.nan(.), NA, .))
        
      }
    }
    
    
    
    
    if(nrow(f2) == 1){
      
      if(grepl("M_\\d+_2009", data_models$model_name[i])){
        
        data_models$data_polls_last[[i]] <- f2 %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          na.omit(.) %>% # here the AFD gets removed
          group_by(party) %>%
          summarize(perc_mean = mean(perc, na.rm=TRUE),
                    SD = sd(perc, na.rm=TRUE),
                    N = n()) %>% # CHECK
          mutate_all(~ifelse(is.nan(.), NA, .)) #%>%
        #filter(!is.na(SD)) # Filter out AFD when no data
        
        
      }
    }
    
    
    if(nrow(f2) == 1){
      
      if(!grepl("M_\\d+_2009", data_models$model_name[i])){
        
        data_models$data_polls_last[[i]] <- f2 %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          group_by(party) %>%
          summarize(perc_mean = mean(perc, na.rm=TRUE),
                    SD = sd(perc, na.rm=TRUE),
                    N = n()) %>% # CHECK
          mutate_all(~ifelse(is.nan(.), NA, .)) #%>%
        #filter(!is.na(SD)) # Filter out AFD when no data
      }
    }
  }
  
  
  ## Loop C: ADD Predictions (GT) ####
  # Use the GT data, summarize it to create predictions
  # Important: No predictions for AFD for 2009!
  data_models$predictions_GT <- list(NA)

  for(i in 1:nrow(data_models)){
    cat("\nRow ", i, " out of", nrow(data_models))

    if(str_detect(data_models$datasource_weight[i], "GT")){ # Filter GT ONLY

      data_models$predictions_GT[[i]] <- data_models$data_GT_year[[i]] %>%
        filter(date >= as.Date(data_models$GT_start_date[i], "%d.%m.%y") & date <= as.Date(data_models$GT_end_date[i], "%d.%m.%y")) %>%
        group_by(keyword) %>%
        rename(party=keyword) %>%
        summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
        mutate(prediction = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>% # ifelse is built in for the case that all Google data contain zeros and no NaNs are created but 0 at the end of the calculation.
        select(party, prediction)

    }}
  #save(data_models, file = "data_models_C.RData")


  ## Loop D: ADD Predictions (GT + previous election weight) ####
  # Here the GT predictions are simply weighted with the previous election
  # For AFD 2013 we get no prediction because not data for 2009 election
  identifier = sum(str_count(data_models$model_name, "2005"))

  data_models$predictions_GT_election_weight <- list(NA)
  data_models$Weight_Model_2 <- list(NA)

  for(i in 1:nrow(data_models)){
    cat("\nRow ", i, " out of", nrow(data_models))

    if(!grepl("M_\\d+_2005", data_models$model_name[[i]]) == TRUE){

      if(grepl("election weight", data_models$datasource_weight[[i]]) == TRUE){ # Filter GT ONLY

        k = i - identifier

        data_models$predictions_GT[[k]][2] <- data_models$predictions_GT[[k]][2] %>%
          mutate(prediction = ifelse(prediction == 0, as.numeric(unlist(data_models$data_election_previous[[i]][2])), prediction))

        t <- (data_models$data_election_previous[[i]][2]/data_models$predictions_GT[[k]][2])

        data_models$Weight_Model_2[[i]] <- cbind(data_models$predictions_GT[[k]][1], t)


        if(grepl("M_\\d+_2013", data_models$model_name[[i]]) == TRUE){

          data_models$Weight_Model_2[[i]] <-  data_models$Weight_Model_2[[i]] %>%
            add_row(party = "AFD", share = 1, .before = 1) # In 2009, the AFD did not yet exist, so we weight the data for the AFD in 2013 with a weighting factor of 1 (no weighting)


        }


        # Apply previously calculated weighting factor on Google Prop. of the interval of interest
        data_models$predictions_GT_election_weight[[i]] <- data_models$data_GT_year[[i]] %>%
          filter(date >= as.Date(data_models$GT_start_date[i], "%d.%m.%y") & date <= as.Date(data_models$GT_end_date[i], "%d.%m.%y")) %>%
          group_by(keyword) %>%
          rename(party=keyword) %>%
          summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
          mutate(prediction = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>% # ifelse is built in for the case that all Google data contain zeros and no NaNs are created but 0 at the end of the calculation.
          select(party, prediction) %>%
          mutate(prediction = prediction*as.numeric(unlist(data_models$Weight_Model_2[[i]][2])))

      }
    }
  }
  #save(data_models, file = "data_models_D.RData")
  # CHANGES: DOES NOT WORK TO TAKE AVERAGE OF PREVIOUS GT DATA (BECAUSE WOULD NEED TO BE RECOLLECTED)
  

  
  ## Loop E: ADD Predictions (GT + polls weight) ####
  data_models$avg_polls_before_int <- list(NA)  
  data_models$predictions_GT_before_int <- list(NA)  
  data_models$Weight_Model_3 <- list(NA) 
  data_models$predictions_GT_polls <- list(NA) 
  
  for(i in 1:nrow(data_models)){  
    
    if(grepl("2005", data_models$model_name[i])){
      next
    }
    
    if(data_models$datasource_weight[i]=="GT + polls weight"){
      
      # Get raw data and calculate Google Prop. for the interval before the set interval 
      data_models$predictions_GT_before_int[[i]] <- data_models$data_GT_year[[i]] %>%
        filter(date >= as.Date((data_models$GT_start_date[i]-1 - data_models$model_time_interval[[i]]), "%d.%m.%y") & date <= as.Date(data_models$GT_start_date[i]-1, "%d.%m.%y")) %>%
        group_by(keyword) %>%
        rename(party=keyword) %>%
        summarize(hits_sum = sum(hits)) %>% 
        mutate(prediction = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>% # ifelse is built in for the case that all Google data contain zeros and no NaNs are created but 0 at the end of the calculation.
        select(party, prediction)
      
      
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
          summarise(prediciton = mean(prediction))
        
      }
      
      if (!grepl("2009", data_models$model_name[i])){
        
        # Identify polls that lie in the interval before the set interval, take the mean of the identified polls
        # If AFD NA in 2013, replace NA with Google Proportion to get a weighting factor of 1.
        data_models$avg_polls_before_int[[i]] <- infra_dimap_all %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date >= (data_models$GT_start_date[i]-1 - data_models$model_time_interval[[i]]) &  Date <= data_models$GT_start_date[i]-1) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          group_by(party) %>%
          select(party, perc) %>%
          rename(prediction = perc) %>%
          arrange(party) %>%
          summarise(prediciton = mean(prediction)) %>%
          replace(is.na(.), as.numeric(unlist(data_models$predictions_GT_before_int[[i]][1,2]))) # If afd NA in 2013, replace NA with Google Proportion to get a weighting factor of 1.
        
      }
      
      # In case there are no polls in the defined time span, just use the Google Proportion of the set interval. 
      if(nrow(data_models$avg_polls_before_int[[i]]) == 0){
        
        data_models$predictions_GT_polls[[i]] <- data_models$data_GT_year[[i]] %>%
          filter(date >= as.Date(data_models$GT_start_date[i], "%d.%m.%y") & date <= as.Date(data_models$GT_end_date[i], "%d.%m.%y")) %>%
          group_by(keyword) %>%
          rename(party=keyword) %>%
          summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
          mutate(prediction = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>% # ifelse is built in for the case that all Google data contain zeros and no NaNs are created but 0 at the end of the calculation.
          select(party, prediction)
        
        next
      }
      
      
      # Calculate weighting factor using the avg. of the polls before our set interval and the Google Prop. before our set interval
      t <- (data_models$avg_polls_before_int[[i]][2])/(data_models$predictions_GT_before_int[[i]][2])
      
      data_models$Weight_Model_3[[i]] <- cbind(data_models$predictions_GT_before_int[[i]][1], t)
      
      # Apply previously calculated weighting factor on Google Prop. of the interval of interest
      data_models$predictions_GT_polls[[i]] <- data_models$data_GT_year[[i]] %>%
        filter(date >= as.Date(data_models$GT_start_date[i], "%d.%m.%y") & date <= as.Date(data_models$GT_end_date[i], "%d.%m.%y")) %>%
        group_by(keyword) %>%
        rename(party=keyword) %>%
        summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
        mutate(prediction = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>% # ifelse is built in for the case that all Google data contain zeros and no NaNs are created but 0 at the end of the calculation.
        select(party, prediction) %>%
        mutate(prediction = prediction*as.numeric(unlist(data_models$Weight_Model_3[[i]][2])))
      
    }
  }
  #save(data_models, file = "data_models_E.RData")
  
  
  
  ## Loop F: Weekly Weigthing ####
  data_models$Poll_dates_weekly_weighting <- list(NA)  
  data_models$GT_data_weekly_weighting <- list(NA)
  data_models$predicitons_GT_weekly_polls <- list(NA)
  data_models$predicitons_GT_weekly_polls_mean <- list(NA)
  data_models$predictions
  
  
  for(i in 1:nrow(data_models)){
    
    # Skip year 2005
    if (grepl("M_\\d+_2005", data_models$model_name[i])){
      next
    }
    
    
    # Execute weekly weighting only in the corresponding rows
    if (data_models$datasource_weight[i] == "GT + weekly polls weight"){
      
      
      Model4_4 <- data.frame() # construct an empty data frame into which the results are written
      
      
      # Get all surveys that are within the set interval and write them into a data set.
      # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
      polls_in_interval <- infra_dimap_all %>%
        mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
        filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1)
      
      
      # This if command avoids problems if the interval is set to e.g. one week or less and there are no polls in this interval
      # Logic: If the data set with polls is empty, take the next poll before the interval and weight the Google data starting from the start date to the end date of the interval
      if (nrow(polls_in_interval) == 0){
        
        # The if command ensures that the following code is only executed for the data from the year 2009 (the party AFD did not yet exist in 2009)
        if (grepl("M_\\d+_2009", data_models$model_name[i])){
          
          
          # Search for the two polls that precede our interval (first poll (b) to determine the Google data that will be used to calculate the weighting factor with the second poll (k)).
          b <- ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"), data_models$GT_start_date[i], onlypre = T)
          k <- which(grepl(b, as.Date(infra_dimap_all$Date, "%d.%m.%y")))+1
          infra_dimap_all$Date[k]
          
          
          # Write the found poll dates, which are before our actual interval, into our dataset
          data_models$Poll_dates_weekly_weighting[[i]] <- infra_dimap_all %>%
            mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
            filter(Date >= Date[k]+1  &  Date <= data_models$GT_end_date[i])
          
          
          # Get Google data 1 day after the first poll (b) until the end of our interval 
          data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
            filter(date >= as.Date(infra_dimap_all$Date[k], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
            group_by(keyword) 
          
          
          # Prepare second poll (k) for calculation of weighting factor
          infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
            filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            mutate_if(is.character, as.numeric) %>%
            replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
            summarise(perc = c(mean(CDU), mean(FDP),
                               mean(Grüne), mean(Linke), mean(SPD))) %>%
            mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
            select(party, perc)
          
          
          # Get Google data until second poll outside of interval and calculate weighting factor
          Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
            filter(date >= as.Date(infra_dimap_all$Date[k], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
            mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
          
          
          # Apply weighting factor to the interval starting with the start date and ending with the end date of the set interval
          Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
            filter(date >= data_models$GT_start_date[i] & date <=  data_models$GT_end_date[i]) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(hits_sum2 = sum(hits_sum)) %>%
            mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted. 
            mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
            mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
          
          
          data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning)
          
          print("0 polls successful 2009")
          
          
        }
        
        
        # The if command ensures that the following code is only executed for the data from the years 2013, 2017 and 2021 (accounting for AFD)
        if (!grepl("M_\\d+_2009", data_models$model_name[i])){
          
          
          # Search for the two polls that precede our interval (first poll (b) to determine the Google data that will be used to calculate the weighting factor with the second poll (k)).
          b <- ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"), data_models$GT_start_date[i], onlypre = T)
          k <- which(grepl(b, as.Date(infra_dimap_all$Date, "%d.%m.%y")))+1
          infra_dimap_all$Date[k]
          
          
          # Write the found poll dates, which are before our actual interval, into our dataset
          data_models$Poll_dates_weekly_weighting[[i]] <- infra_dimap_all %>%
            mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
            filter(Date >= Date[k]+1  &  Date <= data_models$GT_end_date[i])
          
          
          # Get Google data 1 day after the first poll (b) until the end of our interval 
          data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
            filter(date >= as.Date(infra_dimap_all$Date[k], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
            group_by(keyword)
          
          
          # Prepare second poll (k) for calculation of weighting factor
          infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
            filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            mutate_if(is.character, as.numeric) %>%
            replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
            summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                               mean(Grüne), mean(Linke), mean(SPD))) %>%
            mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
            select(party, perc)
          
          
          # Get Google data until second poll outside of interval and calculate weighting factor
          Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
            filter(date >= as.Date(infra_dimap_all$Date[k], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
            mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
          
          
          # Apply weighting factor to the interval starting with the start date and ending with the end date of the set interval
          Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
            filter(date >= data_models$GT_start_date[i] & date <=  data_models$GT_end_date[i]) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(hits_sum2 = sum(hits_sum)) %>%
            mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
            mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
            mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
          
          
          data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning)
          
          print("0 polls successful 2013/17/21")
          
          
        }
      }
      
      
      # If only one poll lies in our interval, weighting can be done in one step (no need for loop)
      if (nrow(polls_in_interval) == 1){ 
        
        # The if command ensures that the following code is only executed for the data from the year 2009 (the party AFD did not yet exist in 2009)
        if (grepl("M_\\d+_2009", data_models$model_name[i])){
          
          # Execute if the start day of the interval and the poll fall on the same day (only 1 weighting step required)
          if (data_models$GT_start_date[i] %in% polls_in_interval$Date){
            
            
            # Identify the poll date, which is before the start day of the interval and the poll, to limit the Google data used to calculate the weighting factor
            p <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+1
            infra_dimap_all$Date[p]
            
            
            # Write this poll date and the one that lies in our set interval into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  infra_dimap_all %>%
              mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
              filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Now get Google data one day after the poll, which is before our interval, until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[p], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
            # Prepare poll that falls on same day as the start day of the interval for calculation of weighting factor  
            infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == last(polls_in_interval$Date)) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
              summarise(perc = c(mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            # Get Google data until poll that falls on same day as the start day of the interval and calculate weighting factor
            Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[p], "%d.%m.%y")+1 & date <= last(polls_in_interval$Date)) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
              mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            # Apply weighting factor to the interval starting with the start date until the end date of our set interval
            Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
              filter(date >= data_models$GT_start_date[i]+1 & date <= data_models$GT_end_date[i]) %>% # start_date +1, since the data and poll used for calculating the weighting factor should not overlap with the proportion to be weighted
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
              mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning)
            
            print("1 poll sucessful 2009")
            
            
          }
          
          
          # Execute if the start day of the interval and the poll fall NOT on the same day (two weighting steps required: 1 poll before interval, 1 poll within interval
          if (!(data_models$GT_start_date[i] %in% polls_in_interval$Date)){
            
            
            # Search for the two polls that lie before the first poll in our interval
            a <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+2
            infra_dimap_all$Date[a]
            
            
            # Write these poll dates and the one that lies in our set interval into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  infra_dimap_all %>%
              mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
              filter(Date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the first poll (b) that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
            # Prepare second poll that lies outside of the interval for calculation of weighting factor  
            infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
              summarise(perc = c(mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            # Get Google data until second poll outside the interval and calculate weighting factor
            Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
              mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            # Apply weighting factor to the interval starting with the start date until first poll that lies within our set interval
            Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
              filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>% # start_date NOT +1, because the dates do not overlap in this step (dates and poll are outside the interval in this first step)
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
              mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            # Prepare first poll that lies within the set interval for weighting
            infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
              summarise(perc = c(mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            # Calculate weight from period ranging from start date of the set interval until the first poll within the set interval 
            Model4_3_first <-  data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
              mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            # Get Google proportion to be weighted i.e. one day after the first survey in the set interval to the end date of our set interval and apply the previously calculated weight
            Model4_4_first <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]+1 & date <= data_models$GT_end_date[i]) %>% # poll_dates_weekly_weighting +1, since the dates used for weighting should not overlap with the proportion to be weighted
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc= Model4_3_first$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
              mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
            
            print("1 poll sucessful 2009")
            
            
          }
        }
        
        
        # The if command ensures that the following code is only executed for the data from the years 2013, 2017 and 2021 (accounting for AFD)
        if (!grepl("M_\\d+_2009", data_models$model_name[i])){
          
          # Execute if the start day of the interval and the poll fall on the same day (only 1 weighting step required)
          if (data_models$GT_start_date[i] %in% polls_in_interval$Date){
            
            
            # Identify the poll date, which is before the start day of the interval and the poll, to limit the Google data used to calculate the weighting factor
            p <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+1
            infra_dimap_all$Date[p]
            
            
            # Write this poll date and the one that lies in our set interval into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  infra_dimap_all %>%
              mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
              filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Now get Google data one day after the poll, which is before our interval, until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[p], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
            # Prepare poll that falls on same day as the start day of the interval for calculation of weighting factor  
            infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == last(polls_in_interval$Date)) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
              summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            # Get Google data until poll that falls on same day as the start day of the interval and calculate weighting factor
            Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[p], "%d.%m.%y")+1 & date <= last(polls_in_interval$Date)) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
              mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            # Apply weighting factor to the interval starting with the start date until the end date of our set interval
            Model4_4_beginning <- data_models$GT_data_weekly_weighting[[i]] %>% 
              filter(date >= data_models$GT_start_date[i]+1 & date <= data_models$GT_end_date[i]) %>% # start_date +1, since the data and poll used for calculating the weighting factor should not overlap with the proportion to be weighted
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
              mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning)
            
            print("1 poll successful 2013/17/21")
            
            
          }
          
          
          # Execute if the start day of the interval and the poll fall NOT on the same day (two weighting steps required: 1 poll before interval, 1 poll within interval
          if (!(data_models$GT_start_date[i] %in% polls_in_interval$Date)){
            
            
            # Search for the two polls that lie before the first poll in our interval
            a <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+2
            infra_dimap_all$Date[a]
            
            
            # Write these poll dates and the one that lies in our set interval into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <- infra_dimap_all %>%
              mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
              filter(Date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the first poll (b) that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
            # Prepare second poll that lies outside of the interval for calculation of weighting factor  
            infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
              summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            # Get Google data until second poll outside the interval and calculate weighting factor
            Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
              mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            # Apply weighting factor to the interval starting with the start date until first poll that lies within our set interval
            Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
              filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>% # start_date NOT +1, because the dates do not overlap in this step (dates and poll are outside the interval in this first step)
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
              mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            # Prepare first poll that lies within the set interval for weighting
            infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
              summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            # Calculate weight from period ranging from start date of the set interval until the first poll within the set interval 
            Model4_3_first <-  data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
              mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            # Get google proportion to be weighted i.e. one day after the first survey in the set interval to the end date of our set interval and apply the previously calculated weight
            Model4_4_first <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]+1 & date <= data_models$GT_end_date[i]) %>% # start_date +1, since the data and poll used for calculating the weighting factor should not overlap with the proportion to be weighted
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc= Model4_3_first$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
              mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
            
            print("1 poll sucessful 2013/17/21")  
            
            
          }
        }
      }
      
      
      # Execute the following code for all intervals in which 2 or more polls fall within
      if (nrow(polls_in_interval) >= 2){ 
        
        # The if command ensures that the following code is only executed for the data from the year 2009 (the party AFD did not yet exist in 2009)
        if (grepl("M_\\d+_2009", data_models$model_name[i])){
          
          # Execute if the start day of the interval and the poll fall on the same day (only 1 weighting step required)
          if (data_models$GT_start_date[i] %in% polls_in_interval$Date){
            
            
            # Identify the poll date, which is before the start day of the interval and the poll, to limit the Google data used to calculate the weighting factor
            p <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+1
            infra_dimap_all$Date[p]
            
            
            # Write these poll dates into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  infra_dimap_all %>%
              mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
              filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the poll that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[p], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
          }
          
          
          # Execute if the start day of the interval and the poll fall NOT on the same day (two weighting steps required: 1 poll before interval, 1 poll within interval
          if (!(data_models$GT_start_date[i] %in% polls_in_interval$Date)){
            
            
            # Search for the two polls that lie before the first poll in our previously filtered range of polls
            a <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+2
            infra_dimap_all$Date[a]
            
            
            # Write these poll dates into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <- infra_dimap_all %>%
              mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
              filter(Date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the first poll that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
          }
          
          
          # start with 2:nrow since steps 1 & 2 are done in j == 2 
          for (j in 2:nrow(data_models$Poll_dates_weekly_weighting[[i]])){
            
            if (j == 2){
              
              # Execute if the start day of the interval and the poll fall on the same day (only 1 weighting step required)
              if (data_models$GT_start_date[i] %in% polls_in_interval$Date){
                
                
                # Prepare poll that falls on same day as the start of the interval for calculation of weighting factor  
                infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                  filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
                  mutate_if(is.character, as.numeric) %>%
                  replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                  summarise(perc = c(mean(CDU), mean(FDP),
                                     mean(Grüne), mean(Linke), mean(SPD))) %>%
                  mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                  select(party, perc)
                
                
                # Get Google data until poll that falls on same day as the start day of the interval and calculate weighting factor
                Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
                  filter(date >= as.Date(infra_dimap_all$Date[p], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
                  group_by(keyword) %>%
                  summarize(hits_sum = sum(hits)) %>%
                  mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
                  mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                  rename(party = keyword) %>%
                  select(party, perc) %>%
                  mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
                
                
                # Apply weighting factor to the interval starting with the start date until the next poll in our set interval
                Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
                  filter(date >= data_models$GT_start_date[i]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>% # start_date +1, since the data and poll used for calculating the weighting factor should not overlap with the proportion to be weighted
                  group_by(keyword) %>%
                  summarize(hits_sum = sum(hits)) %>%
                  mutate(hits_sum2 = sum(hits_sum)) %>%
                  mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                  mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
                  rename(party = keyword) %>%
                  select(party, perc) %>%
                  mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
                  mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
                
                
                Model4_4 <- rbind(Model4_4, Model4_4_beginning)
                
                print("Beginning successful 2009")
                
                
              }
              
              
              # Execute if the start day of the interval and the poll fall NOT on the same day (two weighting steps required: 1 poll before interval, 1 poll within interval
              if (!(data_models$GT_start_date[i] %in% polls_in_interval$Date)){
                
                
                # Prepare second poll that lies outside of the interval for calculation of weighting factor  
                infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                  filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
                  mutate_if(is.character, as.numeric) %>%
                  replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                  summarise(perc = c(mean(CDU), mean(FDP),
                                     mean(Grüne), mean(Linke), mean(SPD))) %>%
                  mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                  select(party, perc)
                
                
                # Get Google data until second poll outside the interval and calculate weighting factor
                Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
                  filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
                  group_by(keyword) %>%
                  summarize(hits_sum = sum(hits)) %>%
                  mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
                  mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                  rename(party = keyword) %>%
                  select(party, perc) %>%
                  mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
                
                
                # Apply weighting factor to the interval starting with the start date until first poll that lies within our set interval
                Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
                  filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>% # start_date NOT +1, because the dates do not overlap in this step (dates and poll are outside the interval in this first step)
                  group_by(keyword) %>%
                  summarize(hits_sum = sum(hits)) %>%
                  mutate(hits_sum2 = sum(hits_sum)) %>%
                  mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                  mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
                  rename(party = keyword) %>%
                  select(party, perc) %>%
                  mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
                  mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
                
                
                # Prepare first poll that lies within the set interval for weighting
                infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                  filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                  mutate_if(is.character, as.numeric) %>%
                  replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                  summarise(perc = c(mean(CDU), mean(FDP),
                                     mean(Grüne), mean(Linke), mean(SPD))) %>%
                  mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                  select(party, perc)
                
                
                # Calculate weight from period ranging from start date of the set interval until the first poll within the set interval 
                Model4_3_first <-  data_models$GT_data_weekly_weighting[[i]] %>%
                  filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                  group_by(keyword) %>%
                  summarize(hits_sum = sum(hits)) %>%
                  mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
                  mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                  rename(party = keyword) %>%
                  select(party, perc) %>%
                  mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
                
                
                # Get Google proportion to be weighted i.e. one day after the first survey in the set interval to the next poll in our set interval and apply the previously calculated weight
                Model4_4_first <- data_models$GT_data_weekly_weighting[[i]] %>%
                  filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j+1]) %>% # start_date +1, since the data and poll used for calculating the weighting factor should not overlap with the proportion to be weighted
                  group_by(keyword) %>%
                  summarize(hits_sum = sum(hits)) %>%
                  mutate(hits_sum2 = sum(hits_sum)) %>%
                  mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                  mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
                  rename(party = keyword) %>%
                  select(party, perc) %>%
                  mutate(perc= Model4_3_first$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
                  mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
                
                
                Model4_4 <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
                
                print("Beginning successful 2009")
                
                
              }
            }
            
            
            if (j == max(2:nrow(data_models$Poll_dates_weekly_weighting[[i]]))){
              
              
              # Get the last poll date entry in the current object    
              infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                filter(Date == last(data_models$Poll_dates_weekly_weighting[[i]][1])) %>% 
                mutate_if(is.character, as.numeric) %>%
                replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                summarise(perc = c(mean(CDU), mean(FDP),
                                   mean(Grüne), mean(Linke), mean(SPD))) %>%
                mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                select(party, perc)
              
              
              # Get Google Proportion to calculate weighting factor with the previously prepared poll data
              Model4_2_ending <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j-1]+1 & date <= last(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
                group_by(keyword)%>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
                mutate(perc = ifelse(perc == 0,  infra_dimap_weekly_weighting$perc[keyword ==  infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
              
              
              # Apply weighting factor to the interval starting one day after the last poll to the end of our set interval
              Model4_4_ending <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= last(data_models$Poll_dates_weekly_weighting[[i]][1])+1 & date <= data_models$GT_end_date[i]) %>% # poll_dates_weekly_weighting +1, since the data and poll used for calculating the weighting factor should not overlap with the proportion to be weighted
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(hits_sum2 = sum(hits_sum)) %>%
                mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(perc= Model4_2_ending$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
                mutate(perc = ifelse(perc == 1,  infra_dimap_weekly_weighting$perc[party ==  infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
              
              
              data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_ending)
              
              print("Ending successful 2009")
              
              # break the current iteration of the nested loop if the condition above is fulfilled (last entry reached of object) so that the code below is not executed and a new iteration of the upper loop is started
              break
              
            }
            
            
            if (between(j, 3, max(2:nrow(data_models$Poll_dates_weekly_weighting[[i]])-1))){
              
              
              # Get the survey data of the current iteration (j) 
              infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>% 
                mutate_if(is.character, as.numeric) %>%
                replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                summarise(perc = c(mean(CDU), mean(FDP),
                                   mean(Grüne), mean(Linke), mean(SPD))) %>%
                mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                select(party, perc)
              
              
              # Calculate the weighting factor for the period from one day after the poll of the previous iteration to the poll of the current iteration
              Model4_2 <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j-1]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                group_by(keyword)%>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
                mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
              
              
              # Get Google proportion to be weighted i.e. one day after the poll of the current iteration to the next poll date entry and apply the previously calculated weight
              Model4_3 <- data_models$GT_data_weekly_weighting[[i]]  %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j+1]) %>% # poll_dates_weekly_weighting +1, since the data and poll used for calculating the weighting factor should not overlap with the proportion to be weighted
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(hits_sum2 = sum(hits_sum)) %>%
                mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(perc= Model4_2$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
                mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
              
              
              print(j)
              
              Model4_4 <- rbind(Model4_4, Model4_3)
              
              
            }
          }
        }
        
        
        # The if command ensures that the following code is only executed for the data from the years 2013, 2017 and 2021 (accounting for AFD)
        if (!grepl("M_\\d+_2009", data_models$model_name[i])){
          
          # Execute if the start day of the interval and the poll fall on the same day (only 1 weighting step required)
          if (data_models$GT_start_date[i] %in% polls_in_interval$Date){
            
            
            # Identify the poll date, which is before the start day of the interval and the poll, to limit the Google data used to calculate the weighting factor
            p <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+1
            infra_dimap_all$Date[p]
            
            
            # Write these poll dates into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  infra_dimap_all %>%
              mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
              filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the poll that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[p], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
          }
          
          
          # Execute if the start day of the interval and the poll fall NOT on the same day (two weighting steps required: 1 poll before interval, 1 poll within interval
          if (!(data_models$GT_start_date[i] %in% polls_in_interval$Date)){
            
            
            # Search for the two polls that lie before the first poll in our previously filtered range of polls
            a <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+2
            infra_dimap_all$Date[a]
            
            
            # Write these poll dates into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  infra_dimap_all %>%
              mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
              filter(Date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the first poll that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
          }
          
          
          # start with 2:nrow since steps 1 & 2 are done in j == 2 
          for (j in 2:nrow(data_models$Poll_dates_weekly_weighting[[i]])){
            
            if (j == 2){
              
              # Execute if the start day of the interval and the poll fall on the same day (only 1 weighting step required)
              if (data_models$GT_start_date[i] %in% polls_in_interval$Date){
                
                
                # Prepare poll that falls on same day as the start of the interval for calculation of weighting factor  
                infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                  filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
                  mutate_if(is.character, as.numeric) %>%
                  replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                  summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                                     mean(Grüne), mean(Linke), mean(SPD))) %>%
                  mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                  select(party, perc)
                
                
                # Get Google data until poll that falls on same day as the start day of the interval and calculate weighting factor
                Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
                  filter(date >= as.Date(infra_dimap_all$Date[p], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
                  group_by(keyword) %>%
                  summarize(hits_sum = sum(hits)) %>%
                  mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
                  mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                  rename(party = keyword) %>%
                  select(party, perc) %>%
                  mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
                
                
                # Apply weighting factor to the interval starting with the start date until the next poll in our set interval
                Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
                  filter(date >= data_models$GT_start_date[i]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>% # start_date +1, since the data and poll used for calculating the weighting factor should not overlap with the proportion to be weighted
                  group_by(keyword) %>%
                  summarize(hits_sum = sum(hits)) %>%
                  mutate(hits_sum2 = sum(hits_sum)) %>%
                  mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                  mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
                  rename(party = keyword) %>%
                  select(party, perc) %>%
                  mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
                  mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
                
                
                Model4_4 <- rbind(Model4_4, Model4_4_beginning)
                
                print("Beginning successful 2013/17/21")
                
                
              }
              
              
              # Execute if the start day of the interval and the poll fall NOT on the same day (two weighting steps required: 1 poll before interval, 1 poll within interval
              if (!(data_models$GT_start_date[i] %in% polls_in_interval$Date)){
                
                
                # Prepare second poll that lies outside of the interval for calculation of weighting factor  
                infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                  filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
                  mutate_if(is.character, as.numeric) %>%
                  replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                  summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                                     mean(Grüne), mean(Linke), mean(SPD))) %>%
                  mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                  select(party, perc)
                
                
                # Get Google data until second poll outside the interval and calculate weighting factor
                Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
                  filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
                  group_by(keyword) %>%
                  summarize(hits_sum = sum(hits)) %>%
                  mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
                  mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                  rename(party = keyword) %>%
                  select(party, perc) %>%
                  mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
                
                
                # Apply weighting factor to the interval starting with the start date until first poll that lies within our set interval
                Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
                  filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>% # start_date NOT +1, because the dates do not overlap in this step (dates and poll are outside the interval in this first step)
                  group_by(keyword) %>%
                  summarize(hits_sum = sum(hits)) %>%
                  mutate(hits_sum2 = sum(hits_sum)) %>%
                  mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                  mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
                  rename(party = keyword) %>%
                  select(party, perc) %>%
                  mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
                  mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
                
                
                # Prepare first poll that lies within the set interval for weighting
                infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                  filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                  mutate_if(is.character, as.numeric) %>%
                  replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                  summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                     mean(Grüne), mean(Linke), mean(SPD))) %>%
                  mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                  select(party, perc)
                
                
                # Calculate weight from period ranging from start date of the set interval until the first poll within the set interval 
                Model4_3_first <-  data_models$GT_data_weekly_weighting[[i]] %>%
                  filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                  group_by(keyword) %>%
                  summarize(hits_sum = sum(hits)) %>%
                  mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
                  mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                  rename(party = keyword) %>%
                  select(party, perc) %>%
                  mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
                
                
                # Get Google proportion to be weighted i.e. one day after the first survey in the set interval to the next poll in our set interval and apply the previously calculated weight
                Model4_4_first <- data_models$GT_data_weekly_weighting[[i]] %>%
                  filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j+1]) %>% # poll_dates_weekly_weighting +1, since the data and poll used for calculating the weighting factor should not overlap with the proportion to be weighted
                  group_by(keyword) %>%
                  summarize(hits_sum = sum(hits)) %>%
                  mutate(hits_sum2 = sum(hits_sum)) %>%
                  mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                  mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
                  rename(party = keyword) %>%
                  select(party, perc) %>%
                  mutate(perc= Model4_3_first$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
                  mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
                
                
                Model4_4 <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
                
                
                print("Beginning successful 2013/17/21")
                
                
              }
            }
            
            
            if (j == max(2:nrow(data_models$Poll_dates_weekly_weighting[[i]]))){
              
              
              # Get the last poll date entry in the current object    
              infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                filter(Date == last(data_models$Poll_dates_weekly_weighting[[i]][1])) %>% 
                mutate_if(is.character, as.numeric) %>%
                replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                mutate_if(is.character, as.numeric) %>%
                summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                   mean(Grüne), mean(Linke), mean(SPD))) %>%
                mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                select(party, perc)
              
              
              # Get Google Proportion to calculate weighting factor with the previously prepared poll data
              Model4_2_ending <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j-1]+1 & date <= last(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
                group_by(keyword)%>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
                mutate(perc = ifelse(perc == 0,  infra_dimap_weekly_weighting$perc[keyword ==  infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
              
              
              # Apply weighting factor to the interval starting one day after the last poll to the end of our set interval
              Model4_4_ending <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= last(data_models$Poll_dates_weekly_weighting[[i]][1])+1 & date <= data_models$GT_end_date[i]) %>% # poll_dates_weekly_weighting +1, since the data and poll used for calculating the weighting factor should not overlap with the proportion to be weighted
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(hits_sum2 = sum(hits_sum)) %>%
                mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(perc= Model4_2_ending$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
                mutate(perc = ifelse(perc == 1,  infra_dimap_weekly_weighting$perc[party ==  infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
              
              
              
              data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_ending)
              
              
              print("Ending successful 2013/17/21")
              
              # break the current iteration of the nested loop if the condition above is fulfilled (last entry reached of object) so that the code below is not executed and a new iteration of the upper loop is started
              break
              
            }
            
            if (between(j, 3, max(2:nrow(data_models$Poll_dates_weekly_weighting[[i]])-1))){
              
              
              # Get the survey data of the current iteration (j) 
              infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>% 
                mutate_if(is.character, as.numeric) %>%
                replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                   mean(Grüne), mean(Linke), mean(SPD))) %>%
                mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                select(party, perc)
              
              
              # Calculate the weighting factor for the period from one day after the poll of the previous iteration to the poll of the current iteration
              Model4_2 <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j-1]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                group_by(keyword)%>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(perc = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
                mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
              
              
              # Get Google proportion to be weighted i.e. one day after the poll of the current iteration to the next poll date entry and apply the previously calculated weight
              Model4_3 <- data_models$GT_data_weekly_weighting[[i]]  %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j+1]) %>% # poll_dates_weekly_weighting +1, since the data and poll used for calculating the weighting factor should not overlap with the proportion to be weighted
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(hits_sum2 = sum(hits_sum)) %>%
                mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                mutate(perc = ifelse(hits_sum >= 1, hits_sum/hits_sum2*100, 1)) %>%
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(perc= Model4_2$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
                mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
              
              print(j)
              
              Model4_4 <- rbind(Model4_4, Model4_3)
              
              
            }
            
          }
          
        }
        
      }
      
      
      # take the mean over the weighted intervals 
      mean_df <- as.data.frame(data_models$predicitons_GT_weekly_polls[i])
      
      mean_df2 <- mean_df %>%
        group_by(party) %>%
        summarise(prediction = mean(perc))
      
      # write the mean/results to the predictions column
      data_models$predicitons_GT_weekly_polls_mean[[i]] <- tibble(mean_df2)
      
      print("Nested data set with mean successful")
      
      
    }
  }
  



  ## Loop G: ADD Predictions (Only polls) ####
  # Here we store average polls result in a new prediction dataframe
  data_models$predictions_only_polls <- list(NA)

  for(i in 1:nrow(data_models)){
    cat("\nRow ", i, " out of", nrow(data_models))

    if(grepl("M_\\d+_2005", data_models$model_name[i])){
      next
    }

    # Filter
    if(data_models$datasource_weight[i]=="Only polls"){ # Filter GT ONLY


      data_models$predictions_only_polls[[i]] <-
        data_models$data_polls_average[[i]] %>%
        mutate(prediction = perc_mean) %>%
        select(party, prediction)

    }}
  #save(data_models, file = "data_models_G.RData")


  
  ## Loop G2: ADD Predictions (Last polls) ####
  # Here we store average polls result in a new prediction dataframe
  data_models$predictions_last_polls <- list(NA)
  
  for(i in 1:nrow(data_models)){
    cat("\nRow ", i, " out of", nrow(data_models))
    
    if(grepl("M_\\d+_2005", data_models$model_name[i])){
      next
    }
    
    # Filter
    if(data_models$datasource_weight[i]=="Last polls"){ # Filter GT ONLY
      
      
      data_models$predictions_last_polls[[i]] <-
        data_models$data_polls_last[[i]] %>%
        mutate(prediction = perc_mean) %>%
        select(party, prediction)
      
    }}
  #save(data_models, file = "data_models_G.RData")
  
  


  ## Loop H: Merge predictions ####
  # Create column that contains dataframe with all the predictions
  data_models$predictions <- list(NA)  
  
  for(i in 1:nrow(data_models)){  
    
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
    
    if(data_models$datasource_weight[i]=="GT + weekly polls weight"){
      data_models$predictions[[i]] <- data_models$predicitons_GT_weekly_polls_mean[[i]] %>% rename(party_pred=party)
    }   
    
    if(data_models$datasource_weight[i]=="Only polls"){
      data_models$predictions[[i]] <- data_models$predictions_only_polls[[i]] %>% rename(party_pred=party)
    }
    
    if(data_models$datasource_weight[i]=="Last polls"){
      data_models$predictions[[i]] <- data_models$predictions_last_polls[[i]] %>% rename(party_pred=party)
    }
  }
  
  
  if(grepl("2005", data_models$model_name[i])){
    next
  }
  
  
  
  # Dataset: Predictions ####
  # Below we unnest the dataframe to get predictions for single
  # parties across years
  data_predictions <- data_models %>% 
    mutate(df_id = y) %>%
    select(model_id, df_id, model_name, election_date, datasource_weight,
           model_time_interval, GT_end_date, GT_start_date, 
           election, model_time_period_color, 
           data_election, predictions) %>%
    unnest(cols = c(data_election, predictions))
  
  
  ## Add deviations ####
  data_predictions <- data_predictions %>%
    mutate(deviation = prediction - share)
  
  nrow(data_predictions) # number of predictions (40 models for each party)
  
  data_predictions_final <- rbind(data_predictions_final, data_predictions)
  
  print("Dataset finish")
  
  }
  
  
  end_time <- Sys.time()
  end_time - start_time
  

  
  
  nrow(data_predictions) # number of predictions (40 models for each party)

  #save(data_predictions, file = "data_predictionvs.RData")

  
  #######
  #### delete 2005 rows (just needed for Model2_2009)
  #######
  data_models <- data_models %>% 
    filter(grepl("M_\\d+_2005", data_models$model_name) == FALSE)
  
  #And again renaming since first id´s are missing (deleted 2005ÃÂ´s)
  ## Add model index/number ####
  data_models <- data_models %>% 
    mutate(model_id = row_number()) %>%
    select(model_id, everything())
  
  # Reorder
  data_models <- data_models %>%
    select(model_id, model_name, everything())
  
  ## Add unique model name ####
  #data_models <- data_models %>%
    #mutate(model_name  = paste("M", 
    #                           model_id, 
    #                           year(election_date), 
    #                           "int",
    #                           as.character(time_length(model_time_interval, unit = "days")),
    #                           "days",
    #                           "dist",
    #                           as.character(time_length(model_time_distance, unit = "days")),
    #                           "days",
    #                           gsub("\\s", "_", gsub("\\s+", " ", gsub("\\+", " ", data_models$datasource_weight))),
    #                           sep = "_"))
  
  
  # Reorder
  data_models <- data_models %>%
    select(model_id, model_name, everything())
  
 
  #### delete 2005 rows (just needed for Model2_2009)
  #######
  data_predictions_final <- data_predictions_final %>% 
    filter(grepl("M_\\d+_2005", data_predictions_final$model_name) == FALSE)
  
  #And again renaming since first id´s are missing (deleted 2005ÃÂ´s)
  ## Add model index/number ####
  data_predictions_final <- data_predictions_final %>% 
    mutate(model_id = row_number()) %>%
    select(model_id, everything())
  
  
   
# CLEAN data_predictions ####

data_predictions <- data_predictions %>%
  filter(grepl("M_\\d+_2005", model_name) == FALSE) %>% # filter out models without predictions
  group_by(model_name) %>%
  mutate(model_id = cur_group_id()) %>% # Add new model_id (after filtering)
  ungroup()
 #
  #save(data_predictions_final_mean, file = "data_predictions_final_mean_Dean.RData")

  # Save environment
  save.image(file = paste('/cloud/project/Env_Merged_syntax_WC', 
                          gsub("\\s", "_", gsub(":", "-",Sys.time())), 
                          '.RData'))
