# PART 1: Prediction without "other parties" ####
# 1 Load packages ####
  # install.packages("remotes"); remotes::install_github("Ajfrick/ajfhelpR")

# Possible warnings: 



#options(warn = 1)

library(pacman)
p_load(gtrendsR,
       ggplot2,
       tidyverse,
       tidyr,
       xml2,
       data.table,
       patchwork,
       lubridate,
       ajfhelpR)




# 2 Functions ####

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


# 2 Dataset: Election results ####
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



# load poll datasets
Infratest_Dimap_polls <- read_delim("./Data_polls/data_polls_infratest_dimap.csv", delim = ";")
Forsa_polls           <- read_delim("./Data_polls/data_polls_forsa.csv", delim = ";",
                                    locale = locale(decimal_mark = ","))
Kantar_polls          <- read_delim("./Data_polls/data_polls_kantar.csv", delim = ";")
FGW_polls             <- read_delim("./Data_polls/data_polls_fgw.csv", delim = ";",
                                    locale = locale(decimal_mark = ","))
Allensbach_polls      <- read_delim("./Data_polls/data_polls_allens.csv", delim = ";",
                                    locale = locale(decimal_mark = ","))



# 3 Dataset: Models ####

# Create dataframe
data_models <- expand.grid(election_date = as.Date(c("26-09-2021",
                                                     "24-09-2017",
                                                     "22-09-2013",
                                                     "27-09-2009",
                                                     "18-09-2005"), format = "%d-%m-%Y"),
                           datasource_weight = c("GT",
                                                 "GT + election weight",
                                                 "GT + weekly polls weight",
                                                 "Infratest",
                                                 "Forsa", 
                                                 "Kantar", 
                                                 "FGW", 
                                                 "Allens"#,
                                                 #"Avg_polls_infra",
                                                 ),
                           model_time_interval = duration(seq(7,91, 7)[c(1:4, 6, 8, 10, 13)], "days"),
                           model_time_distance = days(seq(1, 150, 1)), # 1 tag vorher, 3 tage, 7 tage, 14 tage # 1 tag vorher, 3 tage, 7 tage, 14 tage
                           #model_time_interval = duration(c(7,91), "days"), # TEST
                           #model_time_distance = days(seq(1, 150, 50)), # TEST
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
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2005", data_models$model_name)] <- list(c("trend_05"))
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2009", data_models$model_name)] <- list(c("trend_09")) # "trend_05",
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2013", data_models$model_name)] <- list(c("trend_CDU_13",
                                                                                                                    "trend_AFD_13"))
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2017", data_models$model_name)] <- list(c("trend_CDU_17",
                                                                                                                    "trend_AFD_17"))
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2021", data_models$model_name)] <- list(c("trend_CDU_21",
                                                                                                                    "trend_AFD_21"))


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


# Subset data_models to   
data_models_GT <- data_models %>%
      filter(datasource_weight=="GT") %>% 
      mutate(data_GT = list(NA)) %>%
      mutate(row_nr = row_number())










# 4 Loop A: Create GT datasets ####
# Create GT datasets for the different time periods for all models that include GT data (see filter below)
data_models$data_GT_year <- list(NA)
  

# Names of Google Trends datasets
setwd("./Data")
dir <- getwd()
names_df <- list.files(dir)
# names_df <- names_df[c(1,2)]

  data_predictions_final <- data.frame()
  
  start_time <- Sys.time()
 
  # LOOP DATASETS: START ####
  for(y in names_df){
    
    # Load GT datasets
    # y <- names_df[1] # TEST
    name <- y
    load(name)
    
    relevant_rows_a <- which(grepl("GT", data_models$datasource_weight))
    
    
    for(i in relevant_rows_a){
      
      
      # Prepare dataset(s) for model
      year_i <- year(data_models$election_date)[i]
      cat("\n\n\n\n", year_i, "\n\n")
      
      
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


  
# 5 Loop B: newest poll ####
  # write newest poll to rows
  
  data_models$predictions_Infratest <- list(NA)
  data_models$predictions_Forsa <- list(NA)
  data_models$predictions_Kantar <- list(NA)
  data_models$predictions_FGW <- list(NA)
  data_models$predictions_Allens <- list(NA)
  
  
  poll_institutes <- c("Infratest",
                       "Forsa", 
                       "Kantar", 
                       "FGW", 
                       "Allens")
  
  
  for(j in poll_institutes){
  
  
  relevant_rows_b <- which(!grepl("M_\\d+_2005", data_models$model_name) & grepl(j, data_models$datasource_weight))
  col_name <- paste0("predictions_", j)
  
  for(i in relevant_rows_b){
    cat("\nRow ", i, " out of", nrow(data_models))
    
    
    # Prepare dataset(s) for model
    year_i <- year(data_models$election_date)[i]
    cat("\n\n\n\n", year_i, "\n\n")
    
    
    f2 <- get(ls()[grep(j, ls(), ignore.case = T)]) %>% # Find polls within GT interval
      mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
      filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]) %>%
      slice(1)
    
    
    if(nrow(f2) == 0){
      
      if(grepl("M_\\d+_2009", data_models$model_name[i])){
        
        data_models[[col_name]][[i]] <- get(ls()[grep(j, ls(), ignore.case = T)]) %>%
          mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
          filter(Date == ajfhelpR::date_near(as.Date(.$Date, "%Y-%m-%d"),
                                             data_models$GT_start_date[i], onlypre = T)) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          na.omit(.) %>% # here the AFD gets removed
          group_by(party) %>%
          summarize(perc_mean = mean(perc, na.rm=TRUE),
                    SD = sd(perc, na.rm=TRUE),
                    N = n()) %>% # CHECK
          mutate_all(~ifelse(is.nan(.), NA, .)) %>%
          filter(party != "Sonstige" & party != "AFD")
      }
    }
    
    
    if(nrow(f2) == 0){
      
      if(!grepl("M_\\d+_2009", data_models$model_name[i])){
        
        data_models[[col_name]][[i]] <- get(ls()[grep(j, ls(), ignore.case = T)]) %>%
          mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
          filter(Date == ajfhelpR::date_near(as.Date(.$Date, "%Y-%m-%d"),
                                             data_models$GT_start_date[i], onlypre = T)) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          group_by(party) %>%
          summarize(perc_mean = mean(perc, na.rm=TRUE),
                    SD = sd(perc, na.rm=TRUE),
                    N = n()) %>% # CHECK
          mutate_all(~ifelse(is.nan(.), NA, .)) %>%
          filter(party != "Sonstige")
        
      }
    }
    
    
    
    
    if(nrow(f2) == 1){
      
      if(grepl("M_\\d+_2009", data_models$model_name[i])){
        
        data_models[[col_name]][[i]] <- f2 %>%
          mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          na.omit(.) %>% # here the AFD gets removed
          group_by(party) %>%
          summarize(perc_mean = mean(perc, na.rm=TRUE),
                    SD = sd(perc, na.rm=TRUE),
                    N = n()) %>% # CHECK
          mutate_all(~ifelse(is.nan(.), NA, .)) %>%
          filter(party != "Sonstige" & party != "AFD") #%>%
          #filter(!is.na(SD)) # Filter out AFD when no data
        
        
      }
    }
    
    
    if(nrow(f2) == 1){
      
      if(!grepl("M_\\d+_2009", data_models$model_name[i])){
        
        data_models[[col_name]][[i]] <- f2 %>%
          mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          group_by(party) %>%
          summarize(perc_mean = mean(perc, na.rm=TRUE),
                    SD = sd(perc, na.rm=TRUE),
                    N = n()) %>% # CHECK
          mutate_all(~ifelse(is.nan(.), NA, .)) %>%
          filter(party != "Sonstige")#%>%
        #filter(!is.na(SD)) # Filter out AFD when no data
      }
    }
  }
  }
  
  
  
  
# 6 Loop C: ADD Predictions (GT) ####
  # Use the GT data, summarize it to create predictions
  # Important: No predictions for AFD for 2009!
  data_models$predictions_GT <- list(NA)
  
  relevant_rows_c <- which(grepl("^GT$", data_models$datasource_weight) | grepl("election weight", data_models$datasource_weight))

  for(i in relevant_rows_c){
    cat("\nRow ", i, " out of", nrow(data_models))

      data_models$predictions_GT[[i]] <- data_models$data_GT_year[[i]] %>%
        filter(date >= as.Date(data_models$GT_start_date[i], "%Y-%m-%d") & date <= as.Date(data_models$GT_end_date[i], "%Y-%m-%d")) %>%
        group_by(keyword) %>%
        rename(party=keyword) %>%
        summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
        mutate(prediction = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>% # ifelse is built in for the case that all Google data contain zeros and no NaNs are created but 0 at the end of the calculation.
        select(party, prediction)

  }
  #save(data_models, file = "data_models_C.RData")


# 7 Loop D: ADD Predictions (GT + previous election weight) ####
  # Here the GT predictions are simply weighted with the previous election
  # For AFD 2013 we get no prediction because not data for 2009 election
  
  ######### here identify distance between first entry election weight 2005 to first entry election weight 2009
a <-  data_models %>%
    filter(grepl("M_\\d+_2005", data_models$model_name) & grepl("election weight", data_models$datasource_weight)) %>%
    select(model_id) %>%
    slice_head() %>%
    pull()
  
b <-  data_models %>%
    filter(grepl("M_\\d+_2009", data_models$model_name) & grepl("election weight", data_models$datasource_weight)) %>%
    select(model_id) %>%
    slice_head() %>%
    pull()

identifier <- b-a

  
  relevant_rows_d <- which(!grepl("M_\\d+_2005", data_models$model_name) & grepl("election weight", data_models$datasource_weight))
  
  data_models$predictions_GT_election_weight <- list(NA)
  data_models$Weight_Model_2 <- list(NA)

  for(i in relevant_rows_d){
    cat("\nRow ", i, " out of", nrow(data_models))

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
          filter(date >= as.Date(data_models$GT_start_date[i], "%Y-%m-%d") & date <= as.Date(data_models$GT_end_date[i], "%Y-%m-%d")) %>%
          group_by(keyword) %>%
          rename(party=keyword) %>%
          summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
          mutate(prediction = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>% # ifelse is built in for the case that all Google data contain zeros and no NaNs are created but 0 at the end of the calculation.
          select(party, prediction) %>%
          mutate(prediction = prediction*as.numeric(unlist(data_models$Weight_Model_2[[i]][2])))
        

      }
  #save(data_models, file = "data_models_D.RData")
  # CHANGES: DOES NOT WORK TO TAKE AVERAGE OF PREVIOUS GT DATA (BECAUSE WOULD NEED TO BE RECOLLECTED)
  
  
  
  
# 8 Loop E: Weekly Weigthing ####
  data_models$Poll_dates_weekly_weighting <- list(NA)  
  data_models$GT_data_weekly_weighting <- list(NA)
  data_models$predicitons_GT_weekly_polls <- list(NA)
  data_models$predicitons_GT_weekly_polls_mean <- list(NA)
  data_models$predictions
  
  relevant_rows_e <- which(!grepl("M_\\d+_2005", data_models$model_name) & grepl("weekly polls weight", data_models$datasource_weight))
  
  
  for(i in relevant_rows_e){
    
    
      Model4_4 <- data.frame() # construct an empty data frame into which the results are written
      
      
      # Get all surveys that are within the set interval and write them into a data set.
      # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
      polls_in_interval <- Infratest_Dimap_polls %>%
        mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
        filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1)
      
      
      # This if command avoids problems if the interval is set to e.g. one week or less and there are no polls in this interval
      # Logic: If the data set with polls is empty, take the next poll before the interval and weight the Google data starting from the start date to the end date of the interval
      if (nrow(polls_in_interval) == 0){
        
        # The if command ensures that the following code is only executed for the data from the year 2009 (the party AFD did not yet exist in 2009)
        if (grepl("M_\\d+_2009", data_models$model_name[i])){
          
          
          # Search for the two polls that precede our interval (first poll (b) to determine the Google data that will be used to calculate the weighting factor with the second poll (k)).
          b <- ajfhelpR::date_near(as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d"), data_models$GT_start_date[i], onlypre = T)
          k <- which(grepl(b, as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+1
          Infratest_Dimap_polls$Date[k]
          
          
          # Write the found poll dates, which are before our actual interval, into our dataset
          data_models$Poll_dates_weekly_weighting[[i]] <- Infratest_Dimap_polls %>%
            mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
            filter(Date >= Date[k]+1  &  Date <= data_models$GT_end_date[i])
          
          
          # Get Google data 1 day after the first poll (b) until the end of our interval 
          data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
            filter(date >= as.Date(Infratest_Dimap_polls$Date[k], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
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
            filter(date >= as.Date(Infratest_Dimap_polls$Date[k], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
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
          b <- ajfhelpR::date_near(as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d"), data_models$GT_start_date[i], onlypre = T)
          k <- which(grepl(b, as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+1
          Infratest_Dimap_polls$Date[k]
          
          
          # Write the found poll dates, which are before our actual interval, into our dataset
          data_models$Poll_dates_weekly_weighting[[i]] <- Infratest_Dimap_polls %>%
            mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
            filter(Date >= Date[k]+1  &  Date <= data_models$GT_end_date[i])
          
          
          # Get Google data 1 day after the first poll (b) until the end of our interval 
          data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
            filter(date >= as.Date(Infratest_Dimap_polls$Date[k], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
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
            filter(date >= as.Date(Infratest_Dimap_polls$Date[k], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
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
            p <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+1
            Infratest_Dimap_polls$Date[p]
            
            
            # Write this poll date and the one that lies in our set interval into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Now get Google data one day after the poll, which is before our interval, until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
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
              filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= last(polls_in_interval$Date)) %>%
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
            a <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+2
            Infratest_Dimap_polls$Date[a]
            
            
            # Write these poll dates and the one that lies in our set interval into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the first poll (b) that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
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
              filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
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
            p <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+1
            Infratest_Dimap_polls$Date[p]
            
            
            # Write this poll date and the one that lies in our set interval into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Now get Google data one day after the poll, which is before our interval, until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
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
              filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= last(polls_in_interval$Date)) %>%
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
            a <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+2
            Infratest_Dimap_polls$Date[a]
            
            
            # Write these poll dates and the one that lies in our set interval into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <- Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the first poll (b) that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
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
              filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
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
            p <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+1
            Infratest_Dimap_polls$Date[p]
            
            
            # Write these poll dates into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the poll that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
          }
          
          
          # Execute if the start day of the interval and the poll fall NOT on the same day (two weighting steps required: 1 poll before interval, 1 poll within interval
          if (!(data_models$GT_start_date[i] %in% polls_in_interval$Date)){
            
            
            # Search for the two polls that lie before the first poll in our previously filtered range of polls
            a <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+2
            Infratest_Dimap_polls$Date[a]
            
            
            # Write these poll dates into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <- Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the first poll that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
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
                  filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
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
                  filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
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
            p <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+1
            Infratest_Dimap_polls$Date[p]
            
            
            # Write these poll dates into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the poll that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
          }
          
          
          # Execute if the start day of the interval and the poll fall NOT on the same day (two weighting steps required: 1 poll before interval, 1 poll within interval
          if (!(data_models$GT_start_date[i] %in% polls_in_interval$Date)){
            
            
            # Search for the two polls that lie before the first poll in our previously filtered range of polls
            a <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+2
            Infratest_Dimap_polls$Date[a]
            
            
            # Write these poll dates into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the first poll that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
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
                  filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
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
                  filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
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





# 9 Loop F: Merge predictions ####
  # Create column that contains dataframe with all the predictions
  data_models$predictions <- list(NA)  
  
  
  datasource_weight <-  c("^GT$",
                          "election weight",
                          "weekly polls weight",
                          "Infratest",
                          "Forsa", 
                          "Kantar", 
                          "FGW", 
                          "Allens")
  
  for(j in datasource_weight){
    
    relevant_rows_g <- which(!grepl("M_\\d+_2005", data_models$model_name) & grepl(j, data_models$datasource_weight))
  
    for(i in relevant_rows_g){  
      cat("\nRow ", i, " out of", nrow(data_models))
      
      if(j == "^GT$"){
        data_models$predictions[[i]] <- data_models$predictions_GT[[i]] %>% select(party, prediction) %>%
                                                                            rename(party_pred = party) # rename because of conflict with names later on
      }
      
      if(j == "election weight"){
        data_models$predictions[[i]] <- data_models$predictions_GT_election_weight[[i]] %>% select(party, prediction) %>%
                                                                                            rename(party_pred = party)
      }
      
      if(j == "weekly polls weight"){
        data_models$predictions[[i]] <- data_models$predicitons_GT_weekly_polls_mean[[i]] %>% select(party, prediction) %>%
                                                                                              rename(party_pred = party)
      }   
      
      if(j == "Infratest"){
        data_models$predictions[[i]] <- data_models$predictions_Infratest[[i]] %>% select(party, perc_mean) %>%
                                                                                   rename(party_pred = party, prediction = perc_mean)
      }
      
      if(j == "Forsa"){
        data_models$predictions[[i]] <- data_models$predictions_Forsa[[i]] %>% select(party, perc_mean) %>%
                                                                               rename(party_pred = party, prediction = perc_mean)
      }
      
      if(j == "Kantar"){
        data_models$predictions[[i]] <- data_models$predictions_Kantar[[i]] %>% select(party, perc_mean) %>%
                                                                                rename(party_pred = party, prediction = perc_mean)
      }
      
      if(j == "FGW"){
        data_models$predictions[[i]] <- data_models$predictions_FGW[[i]] %>% select(party, perc_mean) %>%
                                                                             rename(party_pred = party, prediction = perc_mean)
      }
      
      if(j == "Allens"){
        data_models$predictions[[i]] <- data_models$predictions_Allens[[i]] %>% select(party, perc_mean) %>%
                                                                                rename(party_pred = party, prediction = perc_mean)
      }
    }
  }
  
  
  
# 10 Dataset: Predictions ####
  # Below we unnest the dataframe data_models to get predictions for single
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
  
  } # LOOP DATASETS: FINISH ####
  
  end_time <- Sys.time()
  end_time - start_time
  
  nrow(data_predictions) # number of predictions
   
  save.image(file=paste0('../Saved_environments/environment_afterloop_',gsub("\\s|:", "-",Sys.time()),'.RData'))

  
  #######
  #### delete 2005 rows (just needed for Model2_2009)
  #######
  data_models <- data_models %>% 
    filter(grepl("M_\\d+_2005", data_models$model_name) == FALSE)
  
  #And again renaming since first id´s are missing (deleted 2005ÃÂ´s)
  ## Add model index/number ####
  data_models <- data_models %>% 
    mutate(model_id = row_number()) %>%
    select(model_id, model_name, everything())
  
  # Prepare for keywords
  data_models <- data_models %>%
    mutate(GT_keywords = map(.x = GT_keywords, ~paste(.x, collapse = "; "))) %>%
    #select(model_name, election_date, GT_keywords) %>%
    unnest(GT_keywords)
  
  
  # Reorder
  #data_models <- data_models %>%
    #select(model_id, model_name, everything())
  
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
  #data_models <- data_models %>%
    #select(model_id, model_name, everything())
  
 
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


  
  # Graphs ####
  
  # Prepare data for graph
  data_predictions$party <-
    as.factor(ordered(data_predictions$party,
                      levels = c("Grüne", "Linke", "FDP", "AFD", "SPD", "CDU")))
  data_predictions$deviation_label <-
    round(data_predictions$deviation, 1)
  
  #### Confidence Intervals calculation and mean over all samples ####
  
  # dataframe data_predictions_final contains all results of all samples (unnested)
  # to calculate confidence intervals and mean over all samples group by model_name & party 
  
  data_predictions_final_mean <- data_predictions_final %>%
    group_by(model_name, party) %>%
    summarise(Mean_dev = mean(deviation), SD_dev = sd(deviation), Mean = mean(prediction), SD = sd(prediction), 
               .groups = "keep") %>%
    mutate(mean_lower.ci = Mean - 1.96*(SD/sqrt(n())),
           mean_upper.ci = Mean + 1.96*(SD/sqrt(n())),
           dev_lower.ci = Mean_dev - 1.96*(SD_dev/sqrt(n())),
           dev_upper.ci = Mean_dev + 1.96*(SD_dev/sqrt(n()))) #%>%
   # replace_na(.,0)
  
  ######### Kann man nuch besser lösen ?  ################
  #Sinn: summarize die oben genannten aber behalte andere Spalten wie datasource_weight etc.
  data_predictions_final_mean <- merge(data_predictions_final_mean, data_predictions, by = c("model_name","party"))
  data_predictions_final_mean <- data_predictions_final_mean %>% select(-c(20,21,22,23,24), -("df_id"))

# Subset datafiles
  data_models <- data_models %>%
    select(model_name, election_date, GT_keywords, model_time_interval)
  data_predictions_final_mean <- data_predictions_final_mean %>%
    select(model_time_interval, 
           datasource_weight,
           model_name, Mean_dev, election,
           election_date,
           party,
           GT_start_date, GT_end_date)
  
# Save files
  setwd("..")
  write_csv(data_models, "data_models.csv")
  #write_csv(data_predictions, "data_predictions.csv")
  #write_csv(data_predictions_final, "data_predictions_final.csv")
  write_csv(data_predictions_final_mean, "data_predictions_final_mean.csv")

# Clean up environment
  rm(list=ls())

  
  
  

  
  # PART 2 - PREDICTIONS: OTHER PARTIES (SONSTIGE) ####
  # Below code to get the predictions including "Other parties" = Sonstige
  
  # 2 Dataset: Election results ####
  # Creates a list
  list_electionresults <- NULL
  list_electionresults[["2005-09-18"]] <- data.frame(party=c("CDU", "FDP", "Grüne", "Linke", "SPD","Sonstige"),
                                                     share=c(35.2, 9.8, 8.1, 8.7, 34.2, 3.9))
  
  list_electionresults[["2009-09-27"]] <- data.frame(party=c("CDU", "FDP", "Grüne", "Linke", "SPD","Sonstige"),
                                                     share=c(33.8, 14.6, 10.7, 11.9, 23,6)) %>% arrange(party)
  
  list_electionresults[["2013-09-22"]] <- data.frame(party=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD","Sonstige"),
                                                     share=c(4.7, 41.5, 4.8, 8.4, 8.6, 25.7, 6.2)) %>% arrange(party)
  
  list_electionresults[["2017-09-24"]] <- data.frame(party=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD","Sonstige"),
                                                     share=c(12.6, 32.9, 10.7, 8.9 , 9.2, 20.5, 5)) %>% arrange(party)
  
  list_electionresults[["2021-09-26"]] <- data.frame(party=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD","Sonstige"),
                                                     share=c(10.3, 24.1, 11.5, 14.8 , 4.9, 25.7, 8.6)) %>% arrange(party)
  
  
  
  
  # load poll datasets
  Infratest_Dimap_polls <- read_delim("./Data_polls/data_polls_infratest_dimap.csv", delim = ";")
  Forsa_polls           <- read_delim("./Data_polls/data_polls_forsa.csv", delim = ";",
                                      locale = locale(decimal_mark = ","))
  Kantar_polls          <- read_delim("./Data_polls/data_polls_kantar.csv", delim = ";")
  FGW_polls             <- read_delim("./Data_polls/data_polls_fgw.csv", delim = ";",
                                      locale = locale(decimal_mark = ","))
  Allensbach_polls      <- read_delim("./Data_polls/data_polls_allens.csv", delim = ";",
                                      locale = locale(decimal_mark = ","))
  
  
  
  # 3 Dataset: Models ####
  
  # Create dataframe
  data_models <- expand.grid(election_date = as.Date(c("26-09-2021",
                                                       "24-09-2017",
                                                       "22-09-2013",
                                                       "27-09-2009",
                                                       "18-09-2005"), format = "%d-%m-%Y"),
                             datasource_weight = c("GT",
                                                   "GT + election weight",
                                                   "GT + weekly polls weight",
                                                   "Infratest",
                                                   "Forsa", 
                                                   "Kantar", 
                                                   "FGW", 
                                                   "Allens"#,
                                                   #"Avg_polls_infra",
                             ),
                             model_time_interval = duration(seq(7,91, 7)[c(1:4, 6, 8, 10, 13)], "days"),
                             model_time_distance = days(seq(1, 150, 1)), # 1 tag vorher, 3 tage, 7 tage, 14 tage # 1 tag vorher, 3 tage, 7 tage, 14 tage
                             #model_time_interval = duration(c(7,91), "days"), # TEST
                             #model_time_distance = days(seq(1, 150, 50)), # TEST
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
                            list(c("CDU", "SPD", "Grüne", "Linke", "FDP", "Sonstige")),
                            list(c("CDU", "SPD", "Grüne", "Linke", "FDP", "AFD", "Sonstige"))
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
  data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2005", data_models$model_name)] <- list(c("trend_05","trend_sonst_05"))
  data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2009", data_models$model_name)] <- list(c("trend_09", "trend_sonst_09")) # "trend_05",
  data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2013", data_models$model_name)] <- list(c("trend_CDU_13",
                                                                                                                             "trend_AFD_13", "trend_sonst_13"))
  data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2017", data_models$model_name)] <- list(c("trend_CDU_17",
                                                                                                                             "trend_AFD_17", "trend_sonst_17"))
  data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("M_\\d+_2021", data_models$model_name)] <- list(c("trend_CDU_21",
                                                                                                                             "trend_AFD_21", "trend_sonst_21"))
  
  
  ## Add GT keywords for each year
  data_models$GT_keywords <- NULL
  data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("M_\\d+_2005", data_models$model_name)] <- list(list(c('CDU + CSU + Angela Merkel', 'SPD + Gerhard Schröder', 'Grüne + Joschka Fischer', 'PDS + Linkspartei + Gregor Gysi',
                                                                                                                             'FDP + Guido Westerwelle'), 
                                                                                                                           c('CDU + CSU + Angela Merkel', 'SPD + Gerhard Schröder', 'Grüne + Joschka Fischer', 'NPD + REP + Graue', 
                                                                                                                             'FDP + Guido Westerwelle')))
  data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("M_\\d+_2009", data_models$model_name)] <- list(list(c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Grüne + Jürgen Trittin + Renate Künast', 'Linke + Gregor Gysi',
                                                                                                                             'FDP + Guido Westerwelle'), 
                                                                                                                           c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Grüne + Jürgen Trittin + Renate Künast', 'REP + Piraten + Tierschutzpartei + NPD', 
                                                                                                                             'FDP + Guido Westerwelle')))
  data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("M_\\d+_2013", data_models$model_name)] <- list(list(c('CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne + ', 'Linke + Sarah Wagenknecht + Gregor Gysi',
                                                                                                                             'FDP + Philipp Rösler'),
                                                                                                                           c('Bernd Lucke + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne',
                                                                                                                             'FDP + Philipp Rösler'), 
                                                                                                                           c('Freie Wähler + Piraten + NPD', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne', 
                                                                                                                             'FDP + Philipp Rösler')))
  data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("M_\\d+_2017", data_models$model_name)] <- list(list(c('CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne', 'Linke + Sarah Wagenknecht + Dietmar Bartsch',
                                                                                                                             'FDP + Christian Lindner'),
                                                                                                                           c('Alice Weidel + Alexander Gauland + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne',
                                                                                                                             'FDP + Christian Lindner'), 
                                                                                                                           c('Freie Wähler + Die Partei + Tierschutzpartei', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne',
                                                                                                                             'FDP + Christian Lindner')))
  data_models$GT_keywords[grepl("GT", data_models$model_name) & grepl("M_\\d+_2021", data_models$model_name)] <- list(list(c('CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 'Linke + Janine Wissler + Dietmar Bartsch',
                                                                                                                             'FDP + Christian Lindner'),
                                                                                                                           c('Alice Weidel + Tino Chrupalla + Afd', 'CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne','FDP + Christian Lindner'),
                                                                                                                           c('Freie Wähler + Tierschutzpartei + dieBasis + Die Partei', 'CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 'FDP + Christian Lindner')))
  
  
  
  
  ### Function: Replace search terms ####
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
    
    #Sontige
    x <- gsub('NPD.*', 'Sonstige', x)
    x <- gsub('REP.*', 'Sonstige', x)
    x <- gsub('Freie.*', 'Sonstige', x)
    
  }
  
  
  
  
  # Subset data_models to 
  data_models_GT <- data_models %>%
    filter(datasource_weight=="GT") %>% 
    mutate(data_GT = list(NA)) %>%
    mutate(row_nr = row_number())
  
  
  
  
  
  ## 4 Loop A: Create GT datasets ####
  # Create GT datasets for the different time periods for all models that include GT data (see filter below)
  
  data_models$data_GT_year <- list(NA)
  
  
  
  
  # Names of Google Trends datasets
  setwd("./Data")
  dir <- getwd()
  names_df <- list.files(dir)  
  
  data_predictions_final <- data.frame()
  
  start_time <- Sys.time()
  
  # LOOP DATASETS: START ####
  for(y in names_df){
    
    
    # Load GT datasets
    name <- y
    load(name)
    
    relevant_rows_a <- which(grepl("GT", data_models$datasource_weight))
    
    
    for(i in relevant_rows_a){
      
      
      # Prepare dataset(s) for model
      year_i <- year(data_models$election_date)[i]
      cat("\n\n\n\n", year_i, "\n\n")
      
      
      # Select GT datasets relevant for this year and merge
      name_GT_datasets_i <- as.character(data_models$name_GT_datasets[i][[1]])
      print(name_GT_datasets_i)
      
      
      # Detect if 2005/2009 election
      if(length(name_GT_datasets_i)==2){ # THIS PART FOR 2005/2009
        
        df1 <- get(name_GT_datasets_i[1])$interest_over_time # CDU
        
        df1 <- df1 %>% select(date, hits, keyword) %>%
          mutate(hits = str_replace(hits, "<1", "0"), # HIER WEITER
                 hits = as.numeric(hits), 
                 date = as.Date(date))%>% 
          replace(is.na(.), 0) %>%
          mutate(keyword = replace_searchterms(keyword))
        
        
        df2 <- get(name_GT_datasets_i[2])$interest_over_time %>% # Sonstige
          filter(grepl("NPD.*", keyword) == TRUE)
        
        df2 <- df2 %>% select(date, hits, keyword) %>%
          mutate(hits = str_replace(hits, "<1", "0"), # HIER WEITER
                 hits = as.numeric(hits), 
                 date = as.Date(date))%>% 
          replace(is.na(.), 0) %>%
          mutate(keyword = replace_searchterms(keyword))
        
        
        data_models$data_GT_year[[i]] <- bind_rows(df1, df2)
        
        
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
        
        df3 <- get(name_GT_datasets_i[3])$interest_over_time %>% # AFD
          filter(grepl("Freie.*", keyword) == TRUE)
        
        df3 <- df3 %>% select(date, hits, keyword) %>%
          mutate(hits = str_replace(hits, "<1", "0"), # HIER WEITER
                 hits = as.numeric(hits), 
                 date = as.Date(date))%>% 
          replace(is.na(.), 0) %>%
          mutate(keyword = replace_searchterms(keyword))
        
        
        data_models$data_GT_year[[i]] <- bind_rows(df1, df2, df3) 
      }
    }
    
    
    # 5 Loop B: newest poll #####
    # write newest poll to rows
    
    data_models$predictions_Infratest <- list(NA)
    data_models$predictions_Forsa <- list(NA)
    data_models$predictions_Kantar <- list(NA)
    data_models$predictions_FGW <- list(NA)
    data_models$predictions_Allens <- list(NA)
    
    
    poll_institutes <- c("Infratest",
                         "Forsa", 
                         "Kantar", 
                         "FGW", 
                         "Allens")
    
    
    for(j in poll_institutes){
      
      
      relevant_rows_b <- which(!grepl("M_\\d+_2005", data_models$model_name) & grepl(j, data_models$datasource_weight))
      col_name <- paste0("predictions_", j)
      
      for(i in relevant_rows_b){
        cat("\nRow ", i, " out of", nrow(data_models))
        
        
        # Prepare dataset(s) for model
        year_i <- year(data_models$election_date)[i]
        cat("\n\n\n\n", year_i, "\n\n")
        
        
        f2 <- get(ls()[grep(j, ls(), ignore.case = T)]) %>% # Find polls within GT interval
          mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
          filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]) %>%
          slice(1)
        
        
        if(nrow(f2) == 0){
          
          if(grepl("M_\\d+_2009", data_models$model_name[i])){
            
            data_models[[col_name]][[i]] <- get(ls()[grep(j, ls(), ignore.case = T)]) %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date == ajfhelpR::date_near(as.Date(.$Date, "%Y-%m-%d"),
                                                 data_models$GT_start_date[i], onlypre = T)) %>%
              mutate_if(is.character, as.numeric) %>%
              pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
              na.omit(.) %>% # here the AFD gets removed
              group_by(party) %>%
              summarize(perc_mean = mean(perc, na.rm=TRUE),
                        SD = sd(perc, na.rm=TRUE),
                        N = n()) %>% # CHECK
              mutate_all(~ifelse(is.nan(.), NA, .))%>%
              filter(party != "AFD")
          }
        }
        
        
        if(nrow(f2) == 0){
          
          if(!grepl("M_\\d+_2009", data_models$model_name[i])){
            
            data_models[[col_name]][[i]] <- get(ls()[grep(j, ls(), ignore.case = T)]) %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date == ajfhelpR::date_near(as.Date(.$Date, "%Y-%m-%d"),
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
            
            data_models[[col_name]][[i]] <- f2 %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              mutate_if(is.character, as.numeric) %>%
              pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
              na.omit(.) %>% # here the AFD gets removed
              group_by(party) %>%
              summarize(perc_mean = mean(perc, na.rm=TRUE),
                        SD = sd(perc, na.rm=TRUE),
                        N = n()) %>% # CHECK
              mutate_all(~ifelse(is.nan(.), NA, .)) %>%
              filter(party != "AFD") #%>%
            #filter(!is.na(SD)) # Filter out AFD when no data
            
            
          }
        }
        
        
        if(nrow(f2) == 1){
          
          if(!grepl("M_\\d+_2009", data_models$model_name[i])){
            
            data_models[[col_name]][[i]] <- f2 %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              mutate_if(is.character, as.numeric) %>%
              pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
              group_by(party) %>%
              summarize(perc_mean = mean(perc, na.rm=TRUE),
                        SD = sd(perc, na.rm=TRUE),
                        N = n()) %>% # CHECK
              mutate_all(~ifelse(is.nan(.), NA, .))
            #filter(!is.na(SD)) # Filter out AFD when no data
          }
        }
      }
    }
    
    
    
    
    # 6 Loop C: ADD Predictions (GT) ####
    # Use the GT data, summarize it to create predictions
    # Important: No predictions for AFD for 2009!
    data_models$predictions_GT <- list(NA)
    
    relevant_rows_c <- which(grepl("^GT$", data_models$datasource_weight) | grepl("election weight", data_models$datasource_weight))
    
    for(i in relevant_rows_c){
      cat("\nRow ", i, " out of", nrow(data_models))
      
      data_models$predictions_GT[[i]] <- data_models$data_GT_year[[i]] %>%
        filter(date >= as.Date(data_models$GT_start_date[i], "%Y-%m-%d") & date <= as.Date(data_models$GT_end_date[i], "%Y-%m-%d")) %>%
        group_by(keyword) %>%
        rename(party=keyword) %>%
        summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
        mutate(prediction = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>% # ifelse is built in for the case that all Google data contain zeros and no NaNs are created but 0 at the end of the calculation.
        select(party, prediction)
      
    }
    #save(data_models, file = "data_models_C.RData")
    
    
    # 7 Loop D: ADD Predictions (GT + previous election weight) ####
    # Here the GT predictions are simply weighted with the previous election
    # For AFD 2013 we get no prediction because not data for 2009 election
    
    ######### here identify distance between first entry election weight 2005 to first entry election weight 2009
    a <-  data_models %>%
      filter(grepl("M_\\d+_2005", data_models$model_name) & grepl("election weight", data_models$datasource_weight)) %>%
      select(model_id) %>%
      slice_head() %>%
      pull()
    
    b <-  data_models %>%
      filter(grepl("M_\\d+_2009", data_models$model_name) & grepl("election weight", data_models$datasource_weight)) %>%
      select(model_id) %>%
      slice_head() %>%
      pull()
    
    identifier <- b-a
    
    
    relevant_rows_d <- which(!grepl("M_\\d+_2005", data_models$model_name) & grepl("election weight", data_models$datasource_weight))
    
    data_models$predictions_GT_election_weight <- list(NA)
    data_models$Weight_Model_2 <- list(NA)
    
    for(i in relevant_rows_d){
      cat("\nRow ", i, " out of", nrow(data_models))
      
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
        filter(date >= as.Date(data_models$GT_start_date[i], "%Y-%m-%d") & date <= as.Date(data_models$GT_end_date[i], "%Y-%m-%d")) %>%
        group_by(keyword) %>%
        rename(party=keyword) %>%
        summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
        mutate(prediction = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>% # ifelse is built in for the case that all Google data contain zeros and no NaNs are created but 0 at the end of the calculation.
        select(party, prediction) %>%
        mutate(prediction = prediction*as.numeric(unlist(data_models$Weight_Model_2[[i]][2])))
      
      
    }
    #save(data_models, file = "data_models_D.RData")
    # CHANGES: DOES NOT WORK TO TAKE AVERAGE OF PREVIOUS GT DATA (BECAUSE WOULD NEED TO BE RECOLLECTED)
    
    
    
    
    # 8 Loop E: Weekly Weigthing ####
    data_models$Poll_dates_weekly_weighting <- list(NA)  
    data_models$GT_data_weekly_weighting <- list(NA)
    data_models$predicitons_GT_weekly_polls <- list(NA)
    data_models$predicitons_GT_weekly_polls_mean <- list(NA)
    data_models$predictions
    
    relevant_rows_e <- which(!grepl("M_\\d+_2005", data_models$model_name) & grepl("weekly polls weight", data_models$datasource_weight))
    
    
    for(i in relevant_rows_e){
      
      
      Model4_4 <- data.frame() # construct an empty data frame into which the results are written
      
      
      # Get all surveys that are within the set interval and write them into a data set.
      # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
      polls_in_interval <- Infratest_Dimap_polls %>%
        mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
        filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1)
      
      
      # This if command avoids problems if the interval is set to e.g. one week or less and there are no polls in this interval
      # Logic: If the data set with polls is empty, take the next poll before the interval and weight the Google data starting from the start date to the end date of the interval
      if (nrow(polls_in_interval) == 0){
        
        # The if command ensures that the following code is only executed for the data from the year 2009 (the party AFD did not yet exist in 2009)
        if (grepl("M_\\d+_2009", data_models$model_name[i])){
          
          
          # Search for the two polls that precede our interval (first poll (b) to determine the Google data that will be used to calculate the weighting factor with the second poll (k)).
          b <- ajfhelpR::date_near(as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d"), data_models$GT_start_date[i], onlypre = T)
          k <- which(grepl(b, as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+1
          Infratest_Dimap_polls$Date[k]
          
          
          # Write the found poll dates, which are before our actual interval, into our dataset
          data_models$Poll_dates_weekly_weighting[[i]] <- Infratest_Dimap_polls %>%
            mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
            filter(Date >= Date[k]+1  &  Date <= data_models$GT_end_date[i])
          
          
          # Get Google data 1 day after the first poll (b) until the end of our interval 
          data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
            filter(date >= as.Date(Infratest_Dimap_polls$Date[k], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
            group_by(keyword) 
          
          
          # Prepare second poll (k) for calculation of weighting factor
          infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
            filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            mutate_if(is.character, as.numeric) %>%
            replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
            summarise(perc = c(mean(CDU), mean(FDP),
                               mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
            mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
            select(party, perc) %>%
            arrange(party)
          
          
          # Get Google data until second poll outside of interval and calculate weighting factor
          Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
            filter(date >= as.Date(Infratest_Dimap_polls$Date[k], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
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
          b <- ajfhelpR::date_near(as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d"), data_models$GT_start_date[i], onlypre = T)
          k <- which(grepl(b, as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+1
          Infratest_Dimap_polls$Date[k]
          
          
          # Write the found poll dates, which are before our actual interval, into our dataset
          data_models$Poll_dates_weekly_weighting[[i]] <- Infratest_Dimap_polls %>%
            mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
            filter(Date >= Date[k]+1  &  Date <= data_models$GT_end_date[i])
          
          
          # Get Google data 1 day after the first poll (b) until the end of our interval 
          data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
            filter(date >= as.Date(Infratest_Dimap_polls$Date[k], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
            group_by(keyword)
          
          
          # Prepare second poll (k) for calculation of weighting factor
          infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
            filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            mutate_if(is.character, as.numeric) %>%
            replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
            summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                               mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
            mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
            select(party, perc) %>%
            arrange(party)
          
          
          # Get Google data until second poll outside of interval and calculate weighting factor
          Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
            filter(date >= as.Date(Infratest_Dimap_polls$Date[k], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
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
            p <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+1
            Infratest_Dimap_polls$Date[p]
            
            
            # Write this poll date and the one that lies in our set interval into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Now get Google data one day after the poll, which is before our interval, until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
            # Prepare poll that falls on same day as the start day of the interval for calculation of weighting factor  
            infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == last(polls_in_interval$Date)) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
              summarise(perc = c(mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
              mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
              select(party, perc) %>%
              arrange(party)
            
            
            # Get Google data until poll that falls on same day as the start day of the interval and calculate weighting factor
            Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= last(polls_in_interval$Date)) %>%
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
            a <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+2
            Infratest_Dimap_polls$Date[a]
            
            
            # Write these poll dates and the one that lies in our set interval into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the first poll (b) that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
            # Prepare second poll that lies outside of the interval for calculation of weighting factor  
            infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
              summarise(perc = c(mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
              mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
              select(party, perc) %>%
              arrange(party)
            
            
            # Get Google data until second poll outside the interval and calculate weighting factor
            Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
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
                                 mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
              mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
              select(party, perc) %>%
              arrange(party)
            
            
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
            p <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+1
            Infratest_Dimap_polls$Date[p]
            
            
            # Write this poll date and the one that lies in our set interval into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Now get Google data one day after the poll, which is before our interval, until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
            # Prepare poll that falls on same day as the start day of the interval for calculation of weighting factor  
            infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == last(polls_in_interval$Date)) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
              summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
              mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
              select(party, perc) %>%
              arrange(party)
            
            
            # Get Google data until poll that falls on same day as the start day of the interval and calculate weighting factor
            Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= last(polls_in_interval$Date)) %>%
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
            a <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+2
            Infratest_Dimap_polls$Date[a]
            
            
            # Write these poll dates and the one that lies in our set interval into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <- Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the first poll (b) that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
            # Prepare second poll that lies outside of the interval for calculation of weighting factor  
            infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
              summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
              mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
              select(party, perc) %>%
              arrange(party)
            
            
            # Get Google data until second poll outside the interval and calculate weighting factor
            Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
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
              summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
              mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
              select(party, perc) %>%
              arrange(party)
            
            
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
            p <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+1
            Infratest_Dimap_polls$Date[p]
            
            
            # Write these poll dates into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the poll that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
            
          }
          
          
          # Execute if the start day of the interval and the poll fall NOT on the same day (two weighting steps required: 1 poll before interval, 1 poll within interval
          if (!(data_models$GT_start_date[i] %in% polls_in_interval$Date)){
            
            
            # Search for the two polls that lie before the first poll in our previously filtered range of polls
            a <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+2
            Infratest_Dimap_polls$Date[a]
            
            
            # Write these poll dates into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <- Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the first poll that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
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
                                     mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
                  mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
                  select(party, perc) %>%
                  arrange(party)
                
                
                # Get Google data until poll that falls on same day as the start day of the interval and calculate weighting factor
                Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
                  filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
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
                                     mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
                  mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
                  select(party, perc) %>%
                  arrange(party)
                
                
                # Get Google data until second poll outside the interval and calculate weighting factor
                Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
                  filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
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
                                     mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
                  mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
                  select(party, perc) %>%
                  arrange(party)
                
                
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
                                   mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
                mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
                select(party, perc) %>%
                arrange(party)
              
              
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
                                   mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
                mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
                select(party, perc) %>%
                arrange(party)
              
              
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
            p <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+1
            Infratest_Dimap_polls$Date[p]
            
            
            # Write these poll dates into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the poll that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword)
            
          }
          
          
          # Execute if the start day of the interval and the poll fall NOT on the same day (two weighting steps required: 1 poll before interval, 1 poll within interval
          if (!(data_models$GT_start_date[i] %in% polls_in_interval$Date)){
            
            
            # Search for the two polls that lie before the first poll in our previously filtered range of polls
            a <- which(grepl(dplyr::last(polls_in_interval$Date), as.Date(Infratest_Dimap_polls$Date, "%Y-%m-%d")))+2
            Infratest_Dimap_polls$Date[a]
            
            
            # Write these poll dates into our data set
            # Subtract -1 from the end date, since no weighting can be applied in the scenario where the end date and survey date fall on the same date
            data_models$Poll_dates_weekly_weighting[[i]] <-  Infratest_Dimap_polls %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d")) %>%
              filter(Date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 &  Date <= data_models$GT_end_date[i]-1) %>%
              arrange(Date) 
            
            
            # Get Google data one day after the first poll that lies before our interval until the end of the set interval
            data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT_year[[i]] %>%
              filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= data_models$GT_end_date[i]) %>%
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
                  summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                     mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
                  mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
                  select(party, perc) %>%
                  arrange(party)
                
                
                # Get Google data until poll that falls on same day as the start day of the interval and calculate weighting factor
                Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
                  filter(date >= as.Date(Infratest_Dimap_polls$Date[p], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
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
                  summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                     mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
                  mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
                  select(party, perc) %>%
                  arrange(party)
                
                
                # Get Google data until second poll outside the interval and calculate weighting factor
                Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
                  filter(date >= as.Date(Infratest_Dimap_polls$Date[a], "%Y-%m-%d")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
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
                                     mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
                  mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
                  select(party, perc) %>%
                  arrange(party)
                
                
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
                                   mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
                mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
                select(party, perc) %>%
                arrange(party)
              
              
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
                                   mean(Grüne), mean(Linke), mean(SPD), mean(Sonstige))) %>%
                mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "Sonstige")) %>%
                select(party, perc) %>%
                arrange(party)
              
              
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
    
    
    
    
    
    # 9 Loop F: Merge predictions ####
    # Create column that contains dataframe with all the predictions
    data_models$predictions <- list(NA)  
    
    
    datasource_weight <-  c("^GT$",
                            "election weight",
                            "weekly polls weight",
                            "Infratest",
                            "Forsa", 
                            "Kantar", 
                            "FGW", 
                            "Allens")
    
    for(j in datasource_weight){
      
      relevant_rows_g <- which(!grepl("M_\\d+_2005", data_models$model_name) & grepl(j, data_models$datasource_weight))
      
      for(i in relevant_rows_g){  
        cat("\nRow ", i, " out of", nrow(data_models))
        
        if(j == "^GT$"){
          data_models$predictions[[i]] <- data_models$predictions_GT[[i]] %>% select(party, prediction) %>%
            rename(party_pred = party) # rename because of conflict with names later on
        }
        
        if(j == "election weight"){
          data_models$predictions[[i]] <- data_models$predictions_GT_election_weight[[i]] %>% select(party, prediction) %>%
            rename(party_pred = party)
        }
        
        if(j == "weekly polls weight"){
          data_models$predictions[[i]] <- data_models$predicitons_GT_weekly_polls_mean[[i]] %>% select(party, prediction) %>%
            rename(party_pred = party)
        }   
        
        if(j == "Infratest"){
          data_models$predictions[[i]] <- data_models$predictions_Infratest[[i]] %>% select(party, perc_mean) %>%
            rename(party_pred = party, prediction = perc_mean)
        }
        
        if(j == "Forsa"){
          data_models$predictions[[i]] <- data_models$predictions_Forsa[[i]] %>% select(party, perc_mean) %>%
            rename(party_pred = party, prediction = perc_mean)
        }
        
        if(j == "Kantar"){
          data_models$predictions[[i]] <- data_models$predictions_Kantar[[i]] %>% select(party, perc_mean) %>%
            rename(party_pred = party, prediction = perc_mean)
        }
        
        if(j == "FGW"){
          data_models$predictions[[i]] <- data_models$predictions_FGW[[i]] %>% select(party, perc_mean) %>%
            rename(party_pred = party, prediction = perc_mean)
        }
        
        if(j == "Allens"){
          data_models$predictions[[i]] <- data_models$predictions_Allens[[i]] %>% select(party, perc_mean) %>%
            rename(party_pred = party, prediction = perc_mean)
        }
      }
    }
    
    
    
    # 10 Dataset: Predictions ####
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
  
  # LOOP DATASETS: FINISH ####
  
  nrow(data_predictions) # number of predictions
  
  save.image(file=paste0('../Saved_environments/environment_sonstige_afterloop_',gsub("\\s|:", "-",Sys.time()),'.RData'))
  
  
  
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
    select(model_id, model_name, everything())
  
  # Prepare for keywords
  data_models <- data_models %>%
    mutate(GT_keywords = map(.x = GT_keywords, ~paste(.x, collapse = "; "))) %>%
    #select(model_name, election_date, GT_keywords) %>%
    unnest(GT_keywords)
  
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
  
  
  
  # Graphs ####
  
  # Prepare data for graph
  data_predictions$party <-
    as.factor(ordered(data_predictions$party,
                      levels = c("Grüne", "Linke", "FDP", "AFD", "SPD", "CDU","Sonstige")))
  data_predictions$deviation_label <-
    round(data_predictions$deviation, 1)
  
  #### Confidence Intervals calculation and mean over all samples ####
  
  # dataframe data_predictions_final contains all results of all samples (unnested)
  # to calculate confidence intervals and mean over all samples group by model_name & party 
  
  data_predictions_final_mean <- data_predictions_final %>%
    group_by( model_name, party) %>%
    summarise(Mean_dev = mean(deviation), SD_dev = sd(deviation), Mean = mean(prediction), SD = sd(prediction), 
              .groups = "keep") %>%
    mutate(mean_lower.ci = Mean - 1.96*(SD/sqrt(n())),
           mean_upper.ci = Mean + 1.96*(SD/sqrt(n())),
           dev_lower.ci = Mean_dev - 1.96*(SD_dev/sqrt(n())),
           dev_upper.ci = Mean_dev + 1.96*(SD_dev/sqrt(n()))) 
  #%>% replace_na(.,0)
  
  ######### Kann man nuch besser lösen ????????  ################
  #Sinn: summarize die oben genannten aber behalte andere Spalten wie datasource_weight etc.
  data_predictions_final_mean <- merge(data_predictions_final_mean, data_predictions, by = c("model_name","party"))
  data_predictions_final_mean <- data_predictions_final_mean %>% select(-c(20,21,22,23,24), -("df_id"))
  
  
  # Subset datafiles
  data_models <- data_models %>%
    select(model_name, election_date, GT_keywords, model_time_interval)
  data_predictions_final_mean <- data_predictions_final_mean %>%
    select(model_time_interval, 
           datasource_weight,
           model_name, Mean_dev, election,
           election_date,
           party,
           GT_start_date, GT_end_date)
  
  # Save files
  setwd("..")
  write_csv(data_models, "data_models_sonstige.csv")
  #write_csv(data_predictions, "data_predictions.csv")
  #write_csv(data_predictions_final, "data_predictions_final.csv")
  write_csv(data_predictions_final_mean, "data_predictions_final_mean_sonstige.csv")
  
  # Clean up environment
  rm(list=ls())
  
  
  
  