# Load packages ####
library(gtrendsR)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(rvest)
library(xml2)
library(data.table)
library(patchwork)
library(lubridate)
library(ajfhelpR)



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
                                                     "27-09-2009"), format = "%d-%m-%Y"),
                           datasource_weight = c("GT",
                                                 "GT + election weight",
                                                 "GT + polls weight",
                                                 "GT + weekly polls weight",
                                                 "Only polls"),
                           model_time_period = duration(c(1,3), "weeks"), # 1 woche, 
                           model_time_distance = days(31)) # 1 tag vorher, 3 tage, 7 tage, 14 tage

# Convert to tibble
data_models <- as_tibble(data_models)

# Sort dataframe
data_models <- data_models %>% arrange(election_date, datasource_weight, model_time_period)


## Add model index/number ####
data_models <- data_models %>% 
  mutate(model_id = row_number()) %>%
  select(model_id, everything())

## Add GT data collection periods ####
data_models <- data_models %>%
  mutate(GT_end_date = election_date - model_time_distance, # time ends one day before election
         GT_start_date = as.Date(GT_end_date - model_time_period)) # time period starts 1 or 3 months earlier

## Add vars for coloring ####
data_models <- data_models %>%
  mutate(election = as.factor(format(election_date, "%d %b, %Y")),
         model_time_period_color = as.factor(as.character(time_length(model_time_period, unit = "months"))),
         model_time_period_color = paste(model_time_period_color, "month(s)"))


## Add var with parties running in election ####
data_models <- data_models %>%
  mutate(parties = ifelse(election_date=="2009-09-27",
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
                             as.character(time_length(model_time_period, unit = "months")),
                             "months",
                             gsub("\\s", "_", gsub("\\s+", " ", gsub("\\+", " ", data_models$datasource_weight))),
                             sep = "_"))


# Reorder
data_models <- data_models %>%
  select(model_id, model_name, everything())



## Add GT dataset names #### 
data_models$name_GT_datasets <- NULL
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("2009", data_models$model_name)] <- list(c("trend_09")) # "trend_05", 
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("2013", data_models$model_name)] <- list(c("trend_CDU_13",
                                                                                                                    "trend_AFD_13"))
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("2017", data_models$model_name)] <- list(c("trend_CDU_17",
                                                                                                                    "trend_AFD_17"))
data_models$name_GT_datasets[grepl("GT", data_models$model_name) & grepl("2021", data_models$model_name)] <- list(c("trend_CDU_21",
                                                                                                                    "trend_AFD_21"))


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
  
  # FDP
  x <- gsub('FDP.*', 'FDP', x)
  
  # AFD
  x <- gsub('Bernd.*', 'AFD', x)
  x <- gsub('Alice.*', 'AFD', x)
  
}




## Loop A: Create GT datasets ####
# Create GT datasets for the different time periods for all models that include GT data (see filter below)
data_models$data_GT <- list(NA)


names_df <- c("2021-09-20 11-21-45", "2021-09-21 15-21-45")
data_predictions_final <- data.frame()


for(y in names_df){

  for(i in 1:nrow(data_models)){
    # Load GT datasets
    name <- paste(y,".RData",sep="") 
    load(name)
    
    
    
    # Prepare dataset(s) for model
    year_i <- year(data_models$election_date)[i]
    cat("\n\n\n\n", year_i, "\n\n")
    
    # Detect models using GT data
    if(str_detect(data_models$datasource_weight[i], "GT")){
      
      
      
      # Select GT datasets relevant for this year and merge
      name_GT_datasets_i <- as.character(data_models$name_GT_datasets[i][[1]])
      print(name_GT_datasets_i)
      
      # Detect if 2009 election
      if(length(name_GT_datasets_i)==1){ # THIS PART FOR 2009
        
        df1 <- get(name_GT_datasets_i[1])$interest_over_time # Ony 1 dataset
        
        data_models$data_GT[[i]] <- df1 %>%
          select(date, hits, keyword) %>%
          mutate(hits = str_replace(hits, "<1", "0"), 
                 hits = as.numeric(hits), 
                 date = as.Date(date))%>% 
          replace(is.na(.), 0) %>%
          mutate(keyword = replace_searchterms(keyword))
        
        print(table(data_models$data_GT[[i]]$keyword))
        
        
        
        # Detect if NO 2009 election
      }else{ # THIS PART 2013-2021
        
        df1 <- get(name_GT_datasets_i[1])$interest_over_time # CDU
        df2 <- get(name_GT_datasets_i[2])$interest_over_time %>% # AFD
          filter(grepl("Afd.*", keyword) == TRUE) 
        
        
        
        data_models$data_GT[[i]] <- bind_rows(df1, df2) %>%
          select(date, hits, keyword) %>%
          mutate(hits = str_replace(hits, "<1", "0"), # HIER WEITER
                 hits = as.numeric(hits), 
                 date = as.Date(date))%>% 
          replace(is.na(.), 0) %>%
          mutate(keyword = replace_searchterms(keyword))
        
        print(table(data_models$data_GT[[i]]$keyword))
        
      }}}
  
  
  
  
  ## Loop B: Create Poll average datasets ####
  # For each model: Create poll datasets (averages)
  data_models$data_polls_average <- list(NA)  
  
  for(i in 1:nrow(data_models)){  
    
    # Prepare dataset(s) for model
    year_i <- year(data_models$election_date)[i]
    cat("\n\n\n\n", year_i, "\n\n")  
    
    
      f <- infra_dimap_all %>%
      mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
      filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]) 
    

      if(nrow(f) == 0){
        
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
  
  
  ## Loop C: ADD Predictions (GT) ####
  # Use the GT data, summarize it to create predictions
  # Important: No predictions for AFD for 2009!
  data_models$predictions_GT <- list(NA)  
  
  for(i in 1:nrow(data_models)){   
    if(str_detect(data_models$datasource_weight[i], "GT")){ # Filter GT ONLY
      
      data_models$predictions_GT[[i]] <- data_models$data_GT[[i]] %>%
        filter(date >= as.Date(data_models$GT_start_date[i], "%d.%m.%y") & date <= as.Date(data_models$GT_end_date[i], "%d.%m.%y")) %>%
        group_by(keyword) %>%
        rename(party=keyword) %>%
        summarize(hits_sum = sum(hits)) %>% # Same as before but diff. code
        mutate(prediction = hits_sum/sum(hits_sum)*100) %>%
        select(party, prediction)
      
    }}
  
  
  ## Loop D: ADD Predictions (GT + previous election weight) ####
  # Here the GT predictions are simply weighted with the previous election
  # For AFD 2013 we get no prediction because not data for 2009 election
  data_models$predictions_GT_election_weight <- list(NA)  
  
  for(i in 1:nrow(data_models)){   
    
    # Filter
    if(data_models$datasource_weight[i]=="GT + election weight"){ # Filter GT ONLY
      
      data_models$predictions_GT_election_weight[[i]] <- 
        left_join(data_models$predictions_GT[[i]], 
                  data_models$data_election_previous[[i]], 
                  by= "party") %>%
        mutate(prediction = rowMeans(select(.,prediction, share))) %>% # Take mean of prediction and election result
        select(party, prediction)
    }} 
  
  
  ## Loop E: ADD Predictions (GT + polls weight) ####
  # Here GT predictions are weighted (take mean) with average polls from this time period
  data_models$predictions_GT_polls <- list(NA)  
  
  for(i in 1:nrow(data_models)){   
    
    # Filter
    if(data_models$datasource_weight[i]=="GT + polls weight"){ # Filter GT ONLY
      
      
      data_models$predictions_GT_polls[[i]] <- 
        left_join(data_models$predictions_GT[[i]], 
                  data_models$data_polls_average[[i]], 
                  by= "party") %>%
        mutate(prediction = rowMeans(select(.,prediction, perc_mean))) %>%
        select(party, prediction)
      
    }} 
  
  
  
  ## Loop F: Weekly Weigthing ####
  data_models$Poll_dates_weekly_weighting <- list(NA)  
  data_models$GT_data_weekly_weighting <- list(NA)
  data_models$predicitons_GT_weekly_polls <- list(NA)
  data_models$predicitons_GT_weekly_polls_mean <- list(NA)
  data_models$predictions
  
  
  for(i in 1:nrow(data_models)){ 
    
    # execute weekly weighting only in the corresponding rows
    if(data_models$datasource_weight[i] == "GT + weekly polls weight"){
      
      Model4_4 <- data.frame() # construct an empty data frame into which the results are written
      
      # get all surveys that are within the set interval and write them into a data set.
      test <- infra_dimap_all %>%
        mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
        filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i])
      
      
      
      # The if command avoids problems if the interval is set to e.g. one week or less and there are no polls in this interval
      # Logic: If data set with polls empty, take next survey before interval and weight the Google data laying in the interval without polls
      if(nrow(test) == 0){
        
        # The if command ensures that the following code is only executed for the data from the year 2009 (the party AFD did not yet exist in 2009)
        if (grepl("2009", data_models$model_name[i])){
          
          # Search for the two polls that lie before our interval (second poll (k) to specify the Google data that will be used to calculate the weighting factor with the first poll (b), which will then be used to weight the Google data in our specified interval).
          b <- ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"), data_models$GT_start_date[i], onlypre = T)
          k <- which(grepl(b, as.Date(infra_dimap_all$Date, "%d.%m.%y")))+1
          infra_dimap_all$Date[k]
          
          
          # write the found poll dates into our data set
          data_models$Poll_dates_weekly_weighting[[i]] <- infra_dimap_all %>%
            mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
            filter(Date >= Date[k]+1  &  Date <= data_models$GT_end_date[i])
          
          
          # now get Google data 1 day before second poll until that lies before our interval until the end of our interval 
          data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT[[i]] %>%
            filter(date >= as.Date(infra_dimap_all$Date[k], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
            group_by(keyword)
          
          
          # prepare first poll for calculation of weighting factor
          infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
            filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            mutate_if(is.character, as.numeric) %>%
            replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
            summarise(perc = c(mean(CDU), mean(FDP),
                               mean(Grüne), mean(Linke), mean(SPD))) %>%
            mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
            select(party, perc)
          
          
          # get google data until first poll outside of interval and calculate weighting factor
          Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
            filter(date >= as.Date(infra_dimap_all$Date[k], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(perc = hits_sum/sum(hits_sum)*100) %>%
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
            mutate(perc = hits_sum/hits_sum2*100) %>%
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
            mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
          
          
          data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning)
          
          print("0 poll data successful_09")
          
          
          
        }
        
        # The if command ensures that the following code is only executed for the data from the years 2013, 2017 and 2021 (accounting for AFD)
        if (!grepl("2009", data_models$model_name[i])){
          
          # Search for the two polls that lie before our interval (second poll (k) to specify the Google data that will be used to calculate the weighting factor with the first poll (b), which will then be used to weight the Google data in our specified interval)
          b <- ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"), data_models$GT_start_date[i], onlypre = T)
          k <- which(grepl(b, as.Date(infra_dimap_all$Date, "%d.%m.%y")))+1
          infra_dimap_all$Date[k]
          
          
          # write the found poll dates into our data set
          data_models$Poll_dates_weekly_weighting[[i]] <- infra_dimap_all %>%
            mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
            filter(Date >= Date[k]+1  &  Date <= data_models$GT_end_date[i])
          
          
          # now get Google data 1 day before second poll until that lies before our interval until the end of our interval 
          data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT[[i]] %>%
            filter(date >= as.Date(infra_dimap_all$Date[k], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
            group_by(keyword)
          
          
          # prepare first poll for calculation of weighting factor
          infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
            filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            mutate_if(is.character, as.numeric) %>%
            replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
            summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                               mean(Grüne), mean(Linke), mean(SPD))) %>%
            mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
            select(party, perc)
          
          
          # get google data until first poll outside of interval and calculate weighting factor
          Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
            filter(date >= as.Date(infra_dimap_all$Date[k], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(perc = hits_sum/sum(hits_sum)*100) %>%
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
            mutate(perc = hits_sum/hits_sum2*100) %>%
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
            mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
          
          
          data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning)
          
          print("0 poll data successful_131721")
          
          
          
        }
      }
      
      
      
      # if only one poll lies in our interval, weighting can be done in one step (no need for loop)
      if(nrow(test) == 1){ 
        
        if (grepl("2009", data_models$model_name[i])){
          
          # Search for the two polls that lie before the first poll in our previously filtered range of polls
          a <- which(grepl(dplyr::last(test$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+2
          infra_dimap_all$Date[a]
          
          
          # write these poll dates and the one that lies in our set interval into our data set
          data_models$Poll_dates_weekly_weighting[[i]] <-  infra_dimap_all %>%
            mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
            filter(Date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 &  Date <= data_models$GT_end_date[i]) %>%
            arrange(Date) 
          
          
          # now get google data one day before the second survey that lies before our interval until the end of the set interval
          data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT[[i]] %>%
            filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
            group_by(keyword)
          
          
          # prepare first poll that lies outside of the interval for calculation of weighting factor  
          infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
            filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            mutate_if(is.character, as.numeric) %>%
            replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
            summarise(perc = c(mean(CDU), mean(FDP),
                               mean(Grüne), mean(Linke), mean(SPD))) %>%
            mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
            select(party, perc)
          
          
          # get Google data until first survey outside the interval and calculate weighting factor
          Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
            filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(perc = hits_sum/sum(hits_sum)*100) %>%
            mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
          
          
          # Apply weighting factor to the interval starting with the start date until first poll that lies within our set interval
          Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
            filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(hits_sum2 = sum(hits_sum)) %>%
            mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
            mutate(perc = hits_sum/hits_sum2*100) %>%
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
            mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
          
          
          # prepare first poll that lies within the set interval for weighting
          infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
            filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
            mutate_if(is.character, as.numeric) %>%
            replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
            summarise(perc = c(mean(CDU), mean(FDP),
                               mean(Grüne), mean(Linke), mean(SPD))) %>%
            mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
            select(party, perc)
          
          
          # calculate weight from period ranging from start date of the set interval until the first poll within the set interval 
          Model4_3_first <-  data_models$GT_data_weekly_weighting[[i]] %>%
            filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(perc = hits_sum/sum(hits_sum)*100) %>%
            mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
          
          
          
          # Get google proportion to be weighted i.e. one day after the first survey in the set interval to the end date of our set interval and apply the previously calculated weight
          Model4_4_first <- data_models$GT_data_weekly_weighting[[i]] %>%
            filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]+1 & date <= data_models$GT_end_date[i]) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(hits_sum2 = sum(hits_sum)) %>%
            mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
            mutate(perc = hits_sum/hits_sum2*100) %>%
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(perc= Model4_3_first$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
            mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
          
          
          data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
          
          print("1 poll sucessful 09")
          
          
          
        }
        
        
        if (!grepl("2009", data_models$model_name[i])){
          
          # Search for the two polls that lie before the first poll in our previously filtered range of polls
          a <- which(grepl(dplyr::last(test$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+2
          infra_dimap_all$Date[a]
          
          
          # write these poll dates and the one that lies in our set interval into our data set
          data_models$Poll_dates_weekly_weighting[[i]] <-  infra_dimap_all %>%
            mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
            filter(Date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 &  Date <= data_models$GT_end_date[i]) %>%
            arrange(Date) 
          
          
          # now get google data one day before the second survey that lies before our interval until the end of the set interval
          data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT[[i]] %>%
            filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
            group_by(keyword)
          
          
          # prepare first poll that lies outside of the interval for calculation of weighting factor  
          infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
            filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            mutate_if(is.character, as.numeric) %>%
            replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
            summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                               mean(Grüne), mean(Linke), mean(SPD))) %>%
            mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
            select(party, perc)
          
          
          # get Google data until first survey outside the interval and calculate weighting factor
          Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
            filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(perc = hits_sum/sum(hits_sum)*100) %>%
            mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
          
          
          # Apply weighting factor to the interval starting with the start date until first poll that lies within our set interval
          Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
            filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(hits_sum2 = sum(hits_sum)) %>%
            mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
            mutate(perc = hits_sum/hits_sum2*100) %>%
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
            mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
          
          
          # prepare first poll that lies within the set interval for weighting
          infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
            filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
            mutate_if(is.character, as.numeric) %>%
            replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
            summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                               mean(Grüne), mean(Linke), mean(SPD))) %>%
            mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
            select(party, perc)
          
          
          # calculate weight from period ranging from start date of the set interval until the first poll within the set interval 
          Model4_3_first <-  data_models$GT_data_weekly_weighting[[i]] %>%
            filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(perc = hits_sum/sum(hits_sum)*100) %>%
            mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
          
          
          # Get google proportion to be weighted i.e. one day after the first survey in the set interval to the end date of our set interval and apply the previously calculated weight
          Model4_4_first <- data_models$GT_data_weekly_weighting[[i]] %>%
            filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]+1 & date <= data_models$GT_end_date[i]) %>%
            group_by(keyword) %>%
            summarize(hits_sum = sum(hits)) %>%
            mutate(hits_sum2 = sum(hits_sum)) %>%
            mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
            mutate(perc = hits_sum/hits_sum2*100) %>%
            rename(party = keyword) %>%
            select(party, perc) %>%
            mutate(perc= Model4_3_first$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
            mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
          
          
          data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
          
          print("1 poll sucessful 131721")  
          
          
          
        }
      }
      
      # Execute the following code for all intervals in which 2 or more polls fall within
      if(nrow(test) >= 2){ 
        
        # Search for the two polls that lie before the first poll in our previously filtered range of polls
        a <- which(grepl(dplyr::last(test$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+2
        infra_dimap_all$Date[a]
        
        # write these poll dates into our data set
        data_models$Poll_dates_weekly_weighting[[i]] <-  infra_dimap_all %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 &  Date <= data_models$GT_end_date[i]) %>%
          arrange(Date) 
        
        
        # now get google data one day before the second survey that lies before our interval until the end of the set interval
        data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT[[i]] %>%
          filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
          group_by(keyword)
        
        
        
        if (grepl("2009", data_models$model_name[i])){
          
          # start with 2:nrow since steps 1 & 2 are done in j == 2 
          for (j in 2:nrow(data_models$Poll_dates_weekly_weighting[[i]])){
            
            if (j == 2){
              
              # calculate weight for interval before first poll
              infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
                mutate_if(is.character, as.numeric) %>%
                replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                summarise(perc = c(mean(CDU), mean(FDP),
                                   mean(Grüne), mean(Linke), mean(SPD))) %>%
                mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                select(party, perc)
              
              
              Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(perc = hits_sum/sum(hits_sum)*100) %>%
                mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
              
              
              Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
                filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(hits_sum2 = sum(hits_sum)) %>%
                mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                mutate(perc = hits_sum/hits_sum2*100) %>%
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
                mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
              
              
              # get the survey data for that is in first place in the objects
              infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                mutate_if(is.character, as.numeric) %>%
                replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                summarise(perc = c(mean(CDU), mean(FDP),
                                   mean(Grüne), mean(Linke), mean(SPD))) %>%
                mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                select(party, perc)
              
              
              # calculate weight for period ranging from one day after survey that is before the first survey in our objects to first survey in objects and calculate weight by dividing the proportion by the survey date pulled in the previous code chunk
              Model4_3_first <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(perc = hits_sum/sum(hits_sum)*100) %>%
                mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
              
              
              # Get google proportion to be weighted i.e. one day after the first survey in our objects to the second survey date entry in our objects and apply the previously calculated weight
              Model4_4_first <- data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j+1]) %>%
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(hits_sum2 = sum(hits_sum)) %>%
                mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                mutate(perc = hits_sum/hits_sum2*100) %>%
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(perc= Model4_3_first$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
                mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
              
              
              Model4_4 <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
              
              
              print("finished_09_beginning")
              
            } 
            
            if (j == max(2:nrow(data_models$Poll_dates_weekly_weighting[[i]]))){
              
              
              # Pull survey data for the last survey date entry in the current object    
              infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                filter(Date == last(data_models$Poll_dates_weekly_weighting[[i]][1])) %>% 
                mutate_if(is.character, as.numeric) %>%
                replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                summarise(perc = c(mean(CDU), mean(FDP),
                                   mean(Grüne), mean(Linke), mean(SPD))) %>%
                mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                select(party, perc)
              
              
              # Get Google Proportion to calculate weighting factor with the previously drawn survey data (go to previous date in the object +1 day to last date in object)
              Model4_2_ending <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j-1]+1 & date <= last(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
                group_by(keyword)%>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(perc = hits_sum/sum(hits_sum)*100) %>%
                mutate(perc = ifelse(perc == 0,  infra_dimap_weekly_weighting$perc[keyword ==  infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
              
              
              # apply weight to data ranging from last date in object + 1 day to entry in list "end_dates" including the end of our setted intervals
              Model4_4_ending <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= last(data_models$Poll_dates_weekly_weighting[[i]][1])+1 & date <= data_models$GT_end_date[i]) %>%
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(hits_sum2 = sum(hits_sum)) %>%
                mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                mutate(perc = hits_sum/hits_sum2*100) %>%
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(perc= Model4_2_ending$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
                mutate(perc = ifelse(perc == 1,  infra_dimap_weekly_weighting$perc[party ==  infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
              
              
              
              data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_ending)
              
              
              print("finished_ending_09")
              
              # break the current iteration of the nested loop if the condition above is fulfilled (last entry reached of object) so that the code below is not executed and a new iteration of the upper loop is started
              break
              
            }
            
            if (between(j, 3, max(2:nrow(data_models$Poll_dates_weekly_weighting[[i]])-1))){
              
              print(j)
              
              # get the survey data of the current survey date (j) 
              infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>% 
                mutate_if(is.character, as.numeric) %>%
                replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                summarise(perc = c(mean(CDU), mean(FDP),
                                   mean(Grüne), mean(Linke), mean(SPD))) %>%
                mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                select(party, perc)
              
              
              # Calculate the weighting for the period from one day after the survey before the survey of the current iteration to the survey of the current iteration in our objects and calculate the weighting by dividing the proportion by the survey date drawn in the previous code chunk      Model4_2 <-  df_final %>%
              Model4_2 <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j-1]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                group_by(keyword)%>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(perc = hits_sum/sum(hits_sum)*100) %>%
                mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
              
              
              # Get google proportion to be weighted i.e. one day after the survey of the current iteration in our objects to the next survey date entry in our objects and apply the previously calculated weight
              Model4_3 <- data_models$GT_data_weekly_weighting[[i]]  %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j+1]) %>%
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(hits_sum2 = sum(hits_sum)) %>%
                mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                mutate(perc = hits_sum/hits_sum2*100) %>%
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(perc= Model4_2$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
                mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
              
              
              Model4_4 <- rbind(Model4_4, Model4_3)
              
              
            }
          }
        }
        
        
        if (!grepl("2009", data_models$model_name[i])){
          
          # start with 2:nrow since steps 1 & 2 are done in j == 2 
          for (j in 2:nrow(data_models$Poll_dates_weekly_weighting[[i]])){
            
            if (j == 2){
              
              # calculate weight for interval before first poll
              
              infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
                mutate_if(is.character, as.numeric) %>%
                replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                                   mean(Grüne), mean(Linke), mean(SPD))) %>%
                mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                select(party, perc)
              
              
              Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(perc = hits_sum/sum(hits_sum)*100) %>%
                mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
              
              
              Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
                filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(hits_sum2 = sum(hits_sum)) %>%
                mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                mutate(perc = hits_sum/hits_sum2*100) %>%
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
                mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
              
              
              # get the survey data for that is in first place in the objects
              infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                mutate_if(is.character, as.numeric) %>%
                replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                   mean(Grüne), mean(Linke), mean(SPD))) %>%
                mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                select(party, perc)
              
              
              # calculate weight for period ranging from one day after survey that is before the first survey in our objects to first survey in objects and calculate weight by dividing the proportion by the survey date pulled in the previous code chunk
              Model4_3_first <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(perc = hits_sum/sum(hits_sum)*100) %>%
                mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
              
              
              # Get google proportion to be weighted i.e. one day after the first survey in our objects to the second survey date entry in our objects and apply the previously calculated weight
              Model4_4_first <- data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j+1]) %>%
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(hits_sum2 = sum(hits_sum)) %>%
                mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                mutate(perc = hits_sum/hits_sum2*100) %>%
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(perc= Model4_3_first$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
                mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
              
              
              Model4_4 <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
              
              
              print("finished_131721_beginning")
              
            } 
            
            if (j == max(2:nrow(data_models$Poll_dates_weekly_weighting[[i]]))){
              
              
              # Pull survey data for the last survey date entry in the current object    
              infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                filter(Date == last(data_models$Poll_dates_weekly_weighting[[i]][1])) %>% 
                mutate_if(is.character, as.numeric) %>%
                replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                mutate_if(is.character, as.numeric) %>%
                summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                   mean(Grüne), mean(Linke), mean(SPD))) %>%
                mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                select(party, perc)
              
              
              # Get Google Proportion to calculate weighting factor with the previously drawn survey data (go to previous date in the object +1 day to last date in object)
              Model4_2_ending <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j-1]+1 & date <= last(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
                group_by(keyword)%>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(perc = hits_sum/sum(hits_sum)*100) %>%
                mutate(perc = ifelse(perc == 0,  infra_dimap_weekly_weighting$perc[keyword ==  infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
              
              
              # apply weight to data ranging from last date in object + 1 day to entry in list "end_dates" including the end of our setted intervals
              Model4_4_ending <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= last(data_models$Poll_dates_weekly_weighting[[i]][1])+1 & date <= data_models$GT_end_date[i]) %>%
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(hits_sum2 = sum(hits_sum)) %>%
                mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                mutate(perc = hits_sum/hits_sum2*100) %>%
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(perc= Model4_2_ending$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
                mutate(perc = ifelse(perc == 1,  infra_dimap_weekly_weighting$perc[party ==  infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
              
              
              
              data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_ending)
              
              
              print("finished_ending_131721")
              
              # break the current iteration of the nested loop if the condition above is fulfilled (last entry reached of object) so that the code below is not executed and a new iteration of the upper loop is started
              break
              
            }
            
            if (between(j, 3, max(2:nrow(data_models$Poll_dates_weekly_weighting[[i]])-1))){
              
              print(j)
              
              # get the survey data of the current survey date (j) 
              infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
                filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>% 
                mutate_if(is.character, as.numeric) %>%
                replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
                summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                   mean(Grüne), mean(Linke), mean(SPD))) %>%
                mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                select(party, perc)
              
              
              # Calculate the weighting for the period from one day after the survey before the survey of the current iteration to the survey of the current iteration in our objects and calculate the weighting by dividing the proportion by the survey date drawn in the previous code chunk      Model4_2 <-  df_final %>%
              Model4_2 <-  data_models$GT_data_weekly_weighting[[i]] %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j-1]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
                group_by(keyword)%>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(perc = hits_sum/sum(hits_sum)*100) %>%
                mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
              
              
              # Get google proportion to be weighted i.e. one day after the survey of the current iteration in our objects to the next survey date entry in our objects and apply the previously calculated weight
              Model4_3 <- data_models$GT_data_weekly_weighting[[i]]  %>%
                filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j+1]) %>%
                group_by(keyword) %>%
                summarize(hits_sum = sum(hits)) %>%
                mutate(hits_sum2 = sum(hits_sum)) %>%
                mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
                mutate(perc = hits_sum/hits_sum2*100) %>%
                rename(party = keyword) %>%
                select(party, perc) %>%
                mutate(perc= Model4_2$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
                mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
              
              
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
    
    # Filter
    if(data_models$datasource_weight[i]=="Only polls"){ # Filter GT ONLY
      
      
      data_models$predictions_only_polls[[i]] <- 
        data_models$data_polls_average[[i]] %>%
        mutate(prediction = perc_mean) %>%
        select(party, prediction)
      
    }} 
  
  
  
  
  
  ## Loop H: Merge predictions ####
  # Create column that contains dataframe with all the predictions
  data_models$predictions <- list(NA)  
  
  for(i in 1:nrow(data_models)){  
    
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
  }
  
  
  
  # Dataset: Predictions ####
  # Below we unnest the dataframe to get predictions for single
  # parties across years
  data_predictions <- data_models %>% 
    mutate(df_id = y) %>%
    select(model_id, df_id, model_name, election_date, datasource_weight,
           model_time_period, GT_end_date, GT_start_date, 
           election, model_time_period_color, 
           data_election, predictions) %>%
    unnest(cols = c(data_election, predictions))
  
  
  ## Add deviations ####
  data_predictions <- data_predictions %>%
    mutate(deviation = prediction - share)
  
  nrow(data_predictions) # number of predictions (40 models for each party)
  
  data_predictions_final <- rbind(data_predictions_final, data_predictions)
  
}




data_predictions_final_mean <- data_predictions_final %>%
  group_by(model_id, model_name, party) %>%
  summarise(Mean = mean(prediction), SD = sd(prediction), .groups = "keep") %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n()))) 



# Graphs ####

## Figure 5 ####
# Prepare data
data_predictions$party <-
  as.factor(ordered(data_predictions$party, 
                    levels = c("Grüne", "Linke", "FDP", "AFD", "SPD", "CDU")))
data_predictions$deviation_label <-
  round(data_predictions$deviation, 1)
data_predictions$datasource_weight <- 
  factor(data_predictions$datasource_weight)


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

