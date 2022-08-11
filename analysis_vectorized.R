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
                           model_time_period = duration(c(1,3), "months"), # 1 woche, 
                           model_time_distance = days(1)) # 1 tag vorher, 3 tage, 7 tage, 14 tage

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

for(i in 1:nrow(data_models)){
  # Load GT datasets
  name <- paste("2021-09-20 11-21-45",".RData",sep="") 
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
  
  # Add polldatasets (average) for each model (i.e., row)
  data_models$data_polls_average[[i]] <-   infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i]) %>%
    mutate_if(is.character, as.numeric) %>% 
    pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
    group_by(party) %>%
    summarize(perc_mean = mean(perc, na.rm=TRUE),
              SD = sd(perc, na.rm=TRUE),
              N = n()) %>% # CHECK
    mutate(lower.ci = perc_mean - 1.96*(SD/sqrt(N)),
           upper.ci = perc_mean + 1.96*(SD/sqrt(N))) %>%
    filter(!is.na(SD)) # Filter out AFD when no data
  
}



## Loop C: ADD Predictions (GT) ####
# Use the GT data, summarize it to create predictions
# Important: No predictions for AFD for 2009!
data_models$predictions_GT <- list(NA)  

for(i in 1:nrow(data_models)){   
  if(str_detect(data_models$datasource_weight[i], "GT")){ # Filter GT ONLY
    
    data_models$predictions_GT[[i]] <- data_models$data_GT[[i]] %>%
      filter(date >= data_models$GT_start_date[i] &  
               date <= data_models$GT_end_date[i]) %>%
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



## Loop F: ADD Predictions (Only polls) ####
# Here we store average polls result in a new prediciton dataframe
data_models$predictions_only_polls <- list(NA)  

for(i in 1:nrow(data_models)){   
  
  # Filter
  if(data_models$datasource_weight[i]=="Only polls"){ # Filter GT ONLY
    
    
    data_models$predictions_only_polls[[i]] <- 
      data_models$data_polls_average[[i]] %>%
      mutate(prediction = perc_mean) %>%
      select(party, prediction)
    
  }} 





## Loop G: Merge predictions ####
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
  
  
  if(data_models$datasource_weight[i]=="Only polls"){
    data_models$predictions[[i]] <- data_models$predictions_only_polls[[i]] %>% rename(party_pred=party)
  }
}






# Dataset: Predictions ####
# Below we unnest the dataframe to get predictions for single
# parties across years
data_predictions <- data_models %>% 
  select(model_id, model_name, election_date, datasource_weight,
         model_time_period, GT_end_date, GT_start_date, 
         election, model_time_period_color, 
         data_election, predictions) %>%
  unnest(cols = c(data_election, predictions))


## Add deviations ####
data_predictions <- data_predictions %>%
  mutate(deviation = prediction - share)

nrow(data_predictions) # number of predictions (40 models for each party)




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


