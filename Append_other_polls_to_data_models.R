
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





# Dataset: Models ####

# Create dataframe
data_models_polls <- expand.grid(election_date = as.Date(c("26-09-2021",
                                                           "24-09-2017",
                                                           "22-09-2013",
                                                           "27-09-2009",
                                                           "18-09-2005"), format = "%d-%m-%Y"),
                                 datasource_weight = c("Forsa",
                                                       "Kantar",
                                                       "FGW",
                                                       "Allens"),
                                 #model_time_interval = duration(seq(7,14, 7), "days"),
                                 #model_time_distance = days(seq(1, 3, 1))) # 1 tag vorher, 3 tage, 7 tage, 14 tage # 1 tag vorher, 3 tage, 7 tage, 14 tage
                                 model_time_interval = duration(seq(7,91, 7)[c(1:4, 6, 8, 10, 13)], "days"),
                                 model_time_distance = days(seq(1, 150, 1)), # 1 tag vorher, 3 tage, 7 tage, 14 tage # 1 tag vorher, 3 tage, 7 tage, 14 tage
                                 model_time_id = "days")


data_models_polls$election_date

# Convert to tibble
data_models_polls <- as_tibble(data_models_polls)

# Sort dataframe
data_models_polls <- data_models_polls %>% arrange(election_date, datasource_weight, model_time_interval)


## Add model index/number ####
data_models_polls <- data_models_polls %>%
  mutate(model_id = row_number()) %>%
  select(model_id, everything())

## Add GT data collection periods ####
data_models_polls <- data_models_polls %>%
  mutate(GT_end_date = election_date - model_time_distance, # time ends one day before election
         GT_start_date = as.Date(GT_end_date - model_time_interval),
         GT_identifier = paste(GT_start_date, GT_end_date, sep="-")) # time period starts 1 or 3 months earlier

## Add vars for coloring ####
data_models_polls <- data_models_polls %>%
  mutate(election = as.factor(format(election_date, "%d %b, %Y")),
         model_time_period_color = as.factor(as.character(time_length(model_time_interval, unit = "months"))),
         model_time_period_color = paste(model_time_period_color, "month(s)"))


## Add var with parties running in election ####
data_models_polls <- data_models_polls %>%
  mutate(parties = ifelse(election_date <="2009-09-27",
                          list(c("CDU", "SPD", "Grüne", "Linke", "FDP")),
                          list(c("CDU", "SPD", "Grüne", "Linke", "FDP", "AFD"))
  ))


## Add var election results ####

# Write election results to variable
for(i in names(list_electionresults)){
  
  data_models_polls$data_election[data_models_polls$election_date==i] <-
    list(list_electionresults[[i]])
  
}

## Add var previous election results ####
for(i in 1:length(list_electionresults)){
  
  data_models_polls$data_election_previous[data_models_polls$election_date==names(list_electionresults)[i+1]] <-
    list(list_electionresults[[i]])
  
}



## Add unique model name ####
data_models_polls <- data_models_polls %>%
  mutate(model_name  = paste("M",
                             model_id,
                             year(election_date),
                             "int",
                             as.character(time_length(model_time_interval, unit = "days")),
                             "days",
                             "dist",
                             as.character(time_length(model_time_distance, unit = "days")),
                             "days",
                             gsub("\\s", "_", gsub("\\s+", " ", gsub("\\+", " ", data_models_polls$datasource_weight))),
                             sep = "_"))




# Reorder
data_models_polls <- data_models_polls %>%
  select(model_id, model_name, everything())





## Add GT dataset names ####
data_models_polls$name_GT_datasets <- NULL
data_models_polls$name_GT_datasets[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2005", data_models_polls$model_name)] <- list(c("trend_05"))
data_models_polls$name_GT_datasets[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2009", data_models_polls$model_name)] <- list(c("trend_09")) # "trend_05",
data_models_polls$name_GT_datasets[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2013", data_models_polls$model_name)] <- list(c("trend_CDU_13",
                                                                                                                                             "trend_AFD_13"))
data_models_polls$name_GT_datasets[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2017", data_models_polls$model_name)] <- list(c("trend_CDU_17",
                                                                                                                                             "trend_AFD_17"))
data_models_polls$name_GT_datasets[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2021", data_models_polls$model_name)] <- list(c("trend_CDU_21",
                                                                                                                                             "trend_AFD_21"))


## Add GT keywords for each year
data_models_polls$GT_keywords <- NULL
data_models_polls$GT_keywords[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2005", data_models_polls$model_name)] <- list(c('CDU + CSU + Angela Merkel', 'SPD + Gerhard Schröder', 'Grüne + Joschka Fischer', 'PDS + Linkspartei + Gregor Gysi',
                                                                                                                                        'FDP + Guido Westerwelle'))
data_models_polls$GT_keywords[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2009", data_models_polls$model_name)] <- list(c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Grüne + Jürgen Trittin + Renate Künast', 'Linke + Gregor Gysi',
                                                                                                                                        'FDP + Guido Westerwelle')) #
data_models_polls$GT_keywords[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2013", data_models_polls$model_name)] <- list(list(c('CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne + ', 'Linke + Sarah Wagenknecht + Gregor Gysi',
                                                                                                                                             'FDP + Philipp Rösler'),
                                                                                                                                           c('Bernd Lucke + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne',
                                                                                                                                             'FDP + Philipp Rösler')))
data_models_polls$GT_keywords[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2017", data_models_polls$model_name)] <- list(list(c('CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne', 'Linke + Sarah Wagenknecht + Dietmar Bartsch',
                                                                                                                                             'FDP + Christian Lindner'),
                                                                                                                                           c('Alice Weidel + Alexander Gauland + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne',
                                                                                                                                             'FDP + Christian Lindner')))
data_models_polls$GT_keywords[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2021", data_models_polls$model_name)] <- list(list(c('CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 'Linke + Janine Wissler + Dietmar Bartsch',
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





# Names of Google Trends datasets
setwd("./Data_polls")
dir <- getwd()
names_df <- list.files(dir)  



data_models_polls$data_polls_last <- list(NA)


for(v in names_df){
  
  
  # more efficient way of looping through rows 
  # write all line numbers containing the name datasource_weight into the limiter and loop through only these lines
  load(v)
  poll_institute <- gsub("_.*", "", v)
  limiter <- which(!grepl("M_\\d+_2005", data_models_polls$model_name) & grepl(poll_institute, data_models_polls$datasource_weight))
  
  
  for (i in limiter){
    cat("\nRow ", i, " out of", nrow(data_models_polls))
    

    
    # Prepare dataset(s) for model
    year_i <- year(data_models_polls$election_date)[i]
    cat("\n\n\n\n", year_i, "\n\n")
    
    f2 <- get(paste0(poll_institute, "_all")) %>% # Find polls within GT interval
      mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
      filter(Date >= data_models_polls$GT_start_date[i] &  Date <= data_models_polls$GT_end_date[i]) %>%
      slice(1)
    
    
    if(nrow(f2) == 0){
      
      if(grepl("M_\\d+_2009", data_models_polls$model_name[i])){
        
        data_models_polls$data_polls_last[[i]] <- get(paste0(poll_institute, "_all")) %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date == ajfhelpR::date_near(as.Date(.$Date, "%d.%m.%y"),
                                             data_models_polls$GT_start_date[i], onlypre = T)) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          na.omit(.) %>% # here the AFD gets removed
          group_by(party) %>%
          summarize(perc_mean = mean(perc, na.rm=TRUE),
                    SD = sd(perc, na.rm=TRUE),
                    N = n()) %>% # CHECK
          mutate_all(~ifelse(is.nan(.), NA, .)) %>%
          filter(party != "Sonstige")
      }
    }
    
    
    if(nrow(f2) == 0){
      
      if(!grepl("M_\\d+_2009", data_models_polls$model_name[i])){
        
        data_models_polls$data_polls_last[[i]] <- get(paste0(poll_institute, "_all")) %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date == ajfhelpR::date_near(as.Date(.$Date, "%d.%m.%y"),
                                             data_models_polls$GT_start_date[i], onlypre = T)) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          group_by(party) %>%
          summarize(perc_mean = mean(perc, na.rm=TRUE),
                    SD = sd(perc, na.rm=TRUE),
                    N = n()) %>% # CHECK
          mutate_all(~ifelse(is.nan(.), NA, .))%>%
          filter(party != "Sonstige")
        
      }
    }
    
    
    
    
    if(nrow(f2) == 1){
      
      if(grepl("M_\\d+_2009", data_models_polls$model_name[i])){
        
        data_models_polls$data_polls_last[[i]] <- f2 %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          mutate_if(is.character, as.numeric) %>%
          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
          na.omit(.) %>% # here the AFD gets removed
          group_by(party) %>%
          summarize(perc_mean = mean(perc, na.rm=TRUE),
                    SD = sd(perc, na.rm=TRUE),
                    N = n()) %>% # CHECK
          mutate_all(~ifelse(is.nan(.), NA, .))%>%
          filter(party != "Sonstige") #%>%
        #filter(!is.na(SD)) # Filter out AFD when no data
        
        
      }
    }
    
    
    if(nrow(f2) == 1){
      
      if(!grepl("M_\\d+_2009", data_models_polls$model_name[i])){
        
        data_models_polls$data_polls_last[[i]] <- f2 %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
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



# append polls to data_models
data_models <- bind_rows(data_models, data_models_polls)



