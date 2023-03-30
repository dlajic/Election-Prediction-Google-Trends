
  #library(pacman)
  #p_load(gtrendsR,
  #       ggplot2,
  #       tidyverse,
  #       tidyr,
  #       rvest,
  #       xml2,
  #       data.table,
  #       patchwork,
  #       lubridate,
  #       ajfhelpR,
#       jsonlite)
#
## Dataset: Election results ####
## Creates a list
#list_electionresults <- NULL
#list_electionresults[["2005-09-18"]] <- data.frame(party=c("CDU", "FDP", "Grüne", "Linke", "SPD"),
#                                                   share=c(35.2, 9.8, 8.1, 8.7, 34.2))
#
#list_electionresults[["2009-09-27"]] <- data.frame(party=c("CDU", "FDP", "Grüne", "Linke", "SPD"),
#                                                   share=c(33.8, 14.6, 10.7, 11.9, 23)) %>% arrange(party)
#
#list_electionresults[["2013-09-22"]] <- data.frame(party=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"),
#                                                   share=c(4.7, 41.5, 4.8, 8.4, 8.6, 25.7)) %>% arrange(party)
#
#list_electionresults[["2017-09-24"]] <- data.frame(party=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"),
#                                                   share=c(12.6, 32.9, 10.7, 8.9 , 9.2, 20.5)) %>% arrange(party)
#
#list_electionresults[["2021-09-26"]] <- data.frame(party=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"),
#                                                   share=c(10.3, 24.1, 11.5, 14.8 , 4.9, 25.7)) %>% arrange(party)
#
#
#
#
#
## Dataset: Models ####
#
## Create dataframe
#data_models_polls <- expand.grid(election_date = as.Date(c("26-09-2021",
#                                                           "24-09-2017",
#                                                           "22-09-2013",
#                                                           "27-09-2009",
#                                                           "18-09-2005"), format = "%d-%m-%Y"),
#                                 datasource_weight = c("Forsa",
#                                                       "Kantar",
#                                                       "FGW",
#                                                       "Allens"),
#                                 #model_time_interval = duration(seq(7,14, 7), "days"),
#                                 #model_time_distance = days(seq(1, 3, 1))) # 1 tag vorher, 3 tage, 7 tage, 14 tage # 1 tag vorher, 3 tage, 7 tage, 14 tage
#                                 model_time_interval = duration(seq(7,91, 7)[c(1:4, 6, 8, 10, 13)], "days"),
#                                 model_time_distance = days(seq(1, 150, 1)), # 1 tag vorher, 3 tage, 7 tage, 14 tage # 1 tag vorher, 3 tage, 7 tage, 14 tage
#                                 model_time_id = "days")
#
#
#data_models_polls$election_date
#
## Convert to tibble
#data_models_polls <- as_tibble(data_models_polls)
#
## Sort dataframe
#data_models_polls <- data_models_polls %>% arrange(election_date, datasource_weight, model_time_interval)
#
#
### Add model index/number ####
#data_models_polls <- data_models_polls %>%
#  mutate(model_id = row_number()) %>%
#  select(model_id, everything())
#
### Add GT data collection periods ####
#data_models_polls <- data_models_polls %>%
#  mutate(GT_end_date = election_date - model_time_distance, # time ends one day before election
#         GT_start_date = as.Date(GT_end_date - model_time_interval),
#         GT_identifier = paste(GT_start_date, GT_end_date, sep="-")) # time period starts 1 or 3 months earlier
#
### Add vars for coloring ####
#data_models_polls <- data_models_polls %>%
#  mutate(election = as.factor(format(election_date, "%d %b, %Y")),
#         model_time_period_color = as.factor(as.character(time_length(model_time_interval, unit = "months"))),
#         model_time_period_color = paste(model_time_period_color, "month(s)"))
#
#
### Add var with parties running in election ####
#data_models_polls <- data_models_polls %>%
#  mutate(parties = ifelse(election_date <="2009-09-27",
#                          list(c("CDU", "SPD", "Grüne", "Linke", "FDP")),
#                          list(c("CDU", "SPD", "Grüne", "Linke", "FDP", "AFD"))
#  ))
#
#
### Add var election results ####
#
## Write election results to variable
#for(i in names(list_electionresults)){
#  
#  data_models_polls$data_election[data_models_polls$election_date==i] <-
#    list(list_electionresults[[i]])
#  
#}
#
### Add var previous election results ####
#for(i in 1:length(list_electionresults)){
#  
#  data_models_polls$data_election_previous[data_models_polls$election_date==names(list_electionresults)[i+1]] <-
#    list(list_electionresults[[i]])
#  
#}
#
#
#
### Add unique model name ####
#data_models_polls <- data_models_polls %>%
#  mutate(model_name  = paste("M",
#                             model_id,
#                             year(election_date),
#                             "int",
#                             as.character(time_length(model_time_interval, unit = "days")),
#                             "days",
#                             "dist",
#                             as.character(time_length(model_time_distance, unit = "days")),
#                             "days",
#                             gsub("\\s", "_", gsub("\\s+", " ", gsub("\\+", " ", data_models_polls$datasource_weight))),
#                             sep = "_"))
#
#
#
#
## Reorder
#data_models_polls <- data_models_polls %>%
#  select(model_id, model_name, everything())
#
#
#
#
#
### Add GT dataset names ####
#data_models_polls$name_GT_datasets <- NULL
#data_models_polls$name_GT_datasets[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2005", data_models_polls$model_name)] <- list(c("trend_05"))
#data_models_polls$name_GT_datasets[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2009", data_models_polls$model_name)] <- list(c("trend_09")) # "trend_05",
#data_models_polls$name_GT_datasets[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2013", data_models_polls$model_name)] <- list(c("trend_CDU_13",
#                                                                                                                                             "trend_AFD_13"))
#data_models_polls$name_GT_datasets[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2017", data_models_polls$model_name)] <- list(c("trend_CDU_17",
#                                                                                                                                             "trend_AFD_17"))
#data_models_polls$name_GT_datasets[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2021", data_models_polls$model_name)] <- list(c("trend_CDU_21",
#                                                                                                                                             "trend_AFD_21"))
#
#
### Add GT keywords for each year
#data_models_polls$GT_keywords <- NULL
#data_models_polls$GT_keywords[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2005", data_models_polls$model_name)] <- list(c('CDU + CSU + Angela Merkel', 'SPD + Gerhard Schröder', 'Grüne + Joschka Fischer', 'PDS + Linkspartei + Gregor Gysi',
#                                                                                                                                        'FDP + Guido Westerwelle'))
#data_models_polls$GT_keywords[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2009", data_models_polls$model_name)] <- list(c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Grüne + Jürgen Trittin + Renate Künast', 'Linke + Gregor Gysi',
#                                                                                                                                        'FDP + Guido Westerwelle')) #
#data_models_polls$GT_keywords[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2013", data_models_polls$model_name)] <- list(list(c('CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne + ', 'Linke + Sarah Wagenknecht + Gregor Gysi',
#                                                                                                                                             'FDP + Philipp Rösler'),
#                                                                                                                                           c('Bernd Lucke + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne',
#                                                                                                                                             'FDP + Philipp Rösler')))
#data_models_polls$GT_keywords[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2017", data_models_polls$model_name)] <- list(list(c('CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne', 'Linke + Sarah Wagenknecht + Dietmar Bartsch',
#                                                                                                                                             'FDP + Christian Lindner'),
#                                                                                                                                           c('Alice Weidel + Alexander Gauland + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne',
#                                                                                                                                             'FDP + Christian Lindner')))
#data_models_polls$GT_keywords[grepl("GT", data_models_polls$model_name) & grepl("M_\\d+_2021", data_models_polls$model_name)] <- list(list(c('CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 'Linke + Janine Wissler + Dietmar Bartsch',
#                                                                                                                                             'FDP + Christian Lindner'),
#                                                                                                                                           c('Alice Weidel + Tino Chrupalla + Afd', 'CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne',
#                                                                                                                                             'FDP + Christian Lindner')))
#
#
#
#
### Function: Replace search terms ####
#replace_searchterms <- function(x){
#  
#  # CDU
#  x <- gsub('CDU.*', 'CDU', x)
#  
#  # SPD
#  x <- gsub('SPD.*', 'SPD', x)
#  
#  # GREENS
#  x <- gsub('Jürgen.*', 'Grüne', x)
#  x <- gsub('Annalena.*', 'Grüne', x)
#  x <- gsub('Cem.*', 'Grüne', x)
#  x <- gsub('Katrin.*', 'Grüne', x)
#  x <- gsub('Grüne.*', 'Grüne', x)
#  
#  
#  
#  # LINKE
#  x <- gsub('Linke.*', 'Linke',  x)
#  x <- gsub('PDS.*', 'Linke',  x)
#  
#  # FDP
#  x <- gsub('FDP.*', 'FDP', x)
#  
#  # AFD
#  x <- gsub('Bernd.*', 'AFD', x)
#  x <- gsub('Alice.*', 'AFD', x)
#  
#}
#
#
#
#
#
## Names of Google Trends datasets
#setwd("./Data_polls")
#dir <- getwd()
#names_df <- list.files(dir)  
#
#
#
#data_models_polls$data_polls_last <- list(NA)
#
#
#for(v in names_df){
#  
#  
#  # more efficient way of looping through rows 
#  # write all line numbers containing the name datasource_weight into the limiter and loop through only these lines
#  load(v)
#  poll_institute <- gsub("_.*", "", v)
#  limiter <- which(!grepl("M_\\d+_2005", data_models_polls$model_name) & grepl(poll_institute, data_models_polls$datasource_weight))
#  
#  
#  for (i in limiter){
#    cat("\nRow ", i, " out of", nrow(data_models_polls))
#    
#
#    
#    # Prepare dataset(s) for model
#    year_i <- year(data_models_polls$election_date)[i]
#    cat("\n\n\n\n", year_i, "\n\n")
#    
#    f2 <- get(paste0(poll_institute, "_all")) %>% # Find polls within GT interval
#      mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
#      filter(Date >= data_models_polls$GT_start_date[i] &  Date <= data_models_polls$GT_end_date[i]) %>%
#      slice(1)
#    
#    
#    if(nrow(f2) == 0){
#      
#      if(grepl("M_\\d+_2009", data_models_polls$model_name[i])){
#        
#        data_models_polls$data_polls_last[[i]] <- get(paste0(poll_institute, "_all")) %>%
#          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
#          filter(Date == ajfhelpR::date_near(as.Date(.$Date, "%d.%m.%y"),
#                                             data_models_polls$GT_start_date[i], onlypre = T)) %>%
#          mutate_if(is.character, as.numeric) %>%
#          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
#          na.omit(.) %>% # here the AFD gets removed
#          group_by(party) %>%
#          summarize(perc_mean = mean(perc, na.rm=TRUE),
#                    SD = sd(perc, na.rm=TRUE),
#                    N = n()) %>% # CHECK
#          mutate_all(~ifelse(is.nan(.), NA, .)) %>%
#          filter(party != "Sonstige" & party != "AFD")
#      }
#    }
#    
#    
#    if(nrow(f2) == 0){
#      
#      if(!grepl("M_\\d+_2009", data_models_polls$model_name[i])){
#        
#        data_models_polls$data_polls_last[[i]] <- get(paste0(poll_institute, "_all")) %>%
#          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
#          filter(Date == ajfhelpR::date_near(as.Date(.$Date, "%d.%m.%y"),
#                                             data_models_polls$GT_start_date[i], onlypre = T)) %>%
#          mutate_if(is.character, as.numeric) %>%
#          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
#          group_by(party) %>%
#          summarize(perc_mean = mean(perc, na.rm=TRUE),
#                    SD = sd(perc, na.rm=TRUE),
#                    N = n()) %>% # CHECK
#          mutate_all(~ifelse(is.nan(.), NA, .))%>%
#          filter(party != "Sonstige")
#        
#      }
#    }
#    
#    
#    
#    
#    if(nrow(f2) == 1){
#      
#      if(grepl("M_\\d+_2009", data_models_polls$model_name[i])){
#        
#        data_models_polls$data_polls_last[[i]] <- f2 %>%
#          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
#          mutate_if(is.character, as.numeric) %>%
#          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
#          na.omit(.) %>% # here the AFD gets removed
#          group_by(party) %>%
#          summarize(perc_mean = mean(perc, na.rm=TRUE),
#                    SD = sd(perc, na.rm=TRUE),
#                    N = n()) %>% # CHECK
#          mutate_all(~ifelse(is.nan(.), NA, .))%>%
#          filter(party != "Sonstige") %>%
#          filter(party != "Sonstige" & party != "AFD") #%>%
#        #filter(!is.na(SD)) # Filter out AFD when no data
#        
#        
#      }
#    }
#    
#    
#    if(nrow(f2) == 1){
#      
#      if(!grepl("M_\\d+_2009", data_models_polls$model_name[i])){
#        
#        data_models_polls$data_polls_last[[i]] <- f2 %>%
#          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
#          mutate_if(is.character, as.numeric) %>%
#          pivot_longer(-Date, names_to = "party", values_to = "perc") %>%
#          group_by(party) %>%
#          summarize(perc_mean = mean(perc, na.rm=TRUE),
#                    SD = sd(perc, na.rm=TRUE),
#                    N = n()) %>% # CHECK
#          mutate_all(~ifelse(is.nan(.), NA, .)) %>%
#          filter(party != "Sonstige")#%>%
#        #filter(!is.na(SD)) # Filter out AFD when no data
#      }
#    }
#  }
#
#}
#
#
#
## append polls to data_models
#data_models <- bind_rows(data_models, data_models_polls)
#
#
#
#
#
#
#
#
#
#data_models$predictions_Allens <- list(NA)
#data_models$predictions_Forsa <- list(NA)
#data_models$predictions_FGW <- list(NA)
#data_models$predictions_Kantar <- list(NA)
#
#
#poll_institute <- c("Allens", "Forsa", "FGW", "Kantar")
#
#for(i in poll_institute){
#  
#  limiter <- which(!grepl("M_\\d+_2005", data_models$model_name) & grepl(i, data_models$datasource_weight))
#  col_name <- paste0("predictions_", i)
#  
#  for (t in limiter){
#  
#    cat("\nRow ", t, " out of", nrow(data_models))
#    
#  # Filter
#  if(data_models$datasource_weight[t]== i){ # Filter GT ONLY
#    
#    
#      data_models[[col_name]][[t]] <- data_models$data_polls_last[[t]] %>%
#                                                mutate(prediction = perc_mean) %>%
#                                                select(party, prediction)
#      
#      data_models$predictions[[t]] <- data_models[[col_name]][[t]] %>%
#                                                rename(party_pred = party)
#    
#      
#  }}}
##save(data_models, file = "data_models_G.RData")
#
#
#
#
## reorder columns 
#data_models <- data_models %>%
#                    relocate(predictions, .after = predictions_Kantar)
#
#
#data_predictions <-  data_models %>% 
#  mutate(df_id = y) %>%
#  select(model_id, df_id, model_name, election_date, datasource_weight,
#         model_time_interval, GT_end_date, GT_start_date, 
#         election, model_time_period_color, 
#         data_election, predictions) %>%
#  unnest(cols = c(data_election, predictions)) 
#
#
#data_predictions <- data_predictions %>%
#  mutate(deviation = prediction - share)
#
#
#data_predictions_final <- data.frame()
#
#
#data_predictions_final <- rbind(data_predictions_final, data_predictions)
#
#
#save.image(file = 'Env_Merged_syntax_NEU_all_Polls.RData')











# START HERE, load Env_Merged_syntax_NEU_all_Polls.RData


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



# Graphs ####

# Prepare data for graph
data_predictions$party <-
  as.factor(ordered(data_predictions$party,
                    levels = c("Grüne", "Linke", "FDP", "AFD", "SPD", "CDU")))
data_predictions$deviation_label <-
  round(data_predictions$deviation, 1)

#for Confidence Intervals

data_predictions_final_mean <- data_predictions_final %>%
  group_by( model_name, party) %>%
  summarise(Mean_dev = mean(deviation), SD_dev = sd(deviation), Mean = mean(prediction), SD = sd(prediction), 
            .groups = "keep") %>%
  mutate(mean_lower.ci = Mean - 1.96*(SD/sqrt(n())),
         mean_upper.ci = Mean + 1.96*(SD/sqrt(n())),
         dev_lower.ci = Mean_dev - 1.96*(SD_dev/sqrt(n())),
         dev_upper.ci = Mean_dev + 1.96*(SD_dev/sqrt(n())),) #%>%
# replace_na(.,0)

######### Kann man nuch besser lösen ????????  ################
#Sinn: summarize die oben genannten aber behalte andere Spalten wie datasource_weight etc.
data_predictions_final_mean <- merge(data_predictions_final_mean, data_predictions, by = c("model_name","party"))
data_predictions_final_mean <- data_predictions_final_mean %>% select(-c(20,21,22,23,24), -("df_id"))



# data_predictions$datasource_weight <-
#   factor(data_predictions$datasource_weight) %>%
#   mutate()



## Figure X ####

############################################################################################
############################## now with more Datasets Confidence Intervals #################

#In Progress

# Graphs ####
# data_predictions$datasource_weight <-
#   factor(data_predictions$datasource_weight) %>%
#   mutate()


## Figure X ####
# predictions for different distances #
data_plot <- data_predictions_final_mean %>%
  #filter(election_date=="2017-09-24"|election_date=="2013-09-22") %>% #can filter for better overview
  mutate(model_time_interval_fac = factor(as.numeric(model_time_interval, "days"))) %>% # convert to days
  mutate(model_time_interval_fac = paste("Interval: ", model_time_interval_fac, " days", sep="")) %>%
  mutate(model_time_distance = election_date - GT_end_date)


data_plot$model_time_interval_fac <- factor(data_plot$model_time_interval_fac,
                                            levels = c("Interval: 7 days", "Interval: 14 days", "Interval: 21 days", 
                                                       "Interval: 28 days", "Interval: 42 days", "Interval: 56 days", 
                                                       "Interval: 70 days", "Interval: 77 days", "Interval: 84 days", "Interval: 91 days"),
                                            ordered = TRUE)

x_breaks <- unique(data_plot$GT_end_date)[seq(1, length(unique(data_plot$GT_end_date)), 10)]
x_labels_distance <- unique(data_plot$model_time_distance)[seq(1, length(unique(data_plot$model_time_distance)), 10)]
x_labels_date <- unique(data_plot$GT_end_date)[seq(1, length(unique(data_plot$GT_end_date)), 10)]

cols <- c("SPD" = "red", "CDU" = "black", "AFD" = "blue", 
          "FDP" = "orange", "Linke" = "purple", "Grüne" = "green")


######### Plot1
data_plot1 <- data_plot %>%
  filter(datasource_weight =="GT") %>%
  group_by(model_name) %>%
  mutate(group_mean_deviation = mean(abs(Mean_dev)))

p <- ggplot(data_plot1,
            aes(x = GT_end_date,
                y = Mean_dev,
                color = party)) +
  geom_vline(xintercept = as.Date("2021-09-26"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2017-09-24"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2013-09-22"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2009-09-27"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid") +  
  #geom_point(size = 0.5) +
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
  ylab("Deviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "Party")
# stat_summary(aes(y = group_mean_deviation, group = 1), fun=mean, colour="purple", geom="line", linetype = "dashed")
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))


p
ggsave(plot = p,
       filename = "plot_predictions_GT_parties.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)  


######################### Create average prediction error (across all parties) ########################
###Plot GT vs. Polls

data_plot2 <- data_plot %>%
  filter(datasource_weight =="GT" | datasource_weight =="Only polls" | datasource_weight =="Last polls" |
           datasource_weight =="Allens" | datasource_weight =="Forsa" | datasource_weight =="Kantar" |
           datasource_weight == "FGW"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE))

cols2 <- c("GT" = "red", "Only polls" = "black", "Last polls" = "blue",
           "Allens" = "darkmagenta", "Forsa" = "darkgoldenrod1", "Kantar" = "cyan", "FGW" = "chartreuse")

#Plot GT vs. Polls
p2 <- ggplot(data_plot2,
             aes(x = GT_end_date,
                 y = deviation_mean,
                 color = datasource_weight)) +
  geom_vline(xintercept = as.Date("2021-09-26"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2017-09-24"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2013-09-22"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2009-09-27"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid") +  
  #geom_point(size = 0.5) +
  geom_line() +
  theme_minimal() +
  facet_grid(vars(model_time_interval_fac),
             vars(election_date), 
             scales = "free") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_breaks,
               labels = paste("Distance: ",  x_labels_distance, " day(s)\n",
                              "Date: ", x_labels_date
               )
  ) +
  scale_color_manual(values = cols2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "datasource_weight")
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))


p2

ggsave(plot = p2,
       filename = "plot_moreSamp_meanDeviations_GT_polls.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)


#### Plot average Deviation all Models
data_plot3 <- data_plot %>%
  filter(datasource_weight =="GT" | datasource_weight == "GT + election weight" | datasource_weight =="Only polls" | datasource_weight =="Last polls" | datasource_weight =="GT + weekly polls weight" |
           datasource_weight =="Allens" | datasource_weight =="Forsa" | datasource_weight =="Kantar" | datasource_weight == "FGW"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev) , na.rm=TRUE))

cols3 <- c("GT" = "red", "Only polls" = "black", "GT + election weight" = "purple",  "GT + weekly polls weight" = "green", 
           "Last polls" = "blue", "Allens" = "peachpuff", "Forsa" = "sienna1", "Kantar" = "yellow3", "FGW" = "violetred")


p3 <- ggplot(data_plot3,
             aes(x = GT_end_date,
                 y = deviation_mean,
                 color = datasource_weight)) +
  geom_vline(xintercept = as.Date("2021-09-26"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2017-09-24"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2013-09-22"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2009-09-27"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid") +  
  #geom_point(size = 0.5) +
  geom_line() +
  theme_minimal() +
  facet_grid(vars(model_time_interval_fac),
             vars(election_date), 
             scales = "free") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_breaks,
               labels = paste("Distance: ",  x_labels_distance, " day(s)\n",
                              "Date: ", x_labels_date
               )
  ) +
  scale_color_manual(values = cols3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "datasource_weight") + 
  ylim(0,10)
#+geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))

p3

ggsave(plot = p3,
       filename = "plot_meanDeviations_allModels.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)



##### comparison weekly weighting
data_plot4 <- data_plot %>%
  group_by(model_name) %>% 
  filter(datasource_weight =="GT" | datasource_weight =="Only polls" | datasource_weight =="Last polls" | datasource_weight =="GT + weekly polls weight" |
           datasource_weight =="Allens" | datasource_weight =="Forsa" | datasource_weight =="Kantar" | datasource_weight == "FGW"
  ) %>%
  mutate(deviation_mean = mean(abs(Mean_dev) , na.rm=TRUE))

cols4 <- c("GT" = "red", "Only polls" = "black", "Last polls" = "blue", "GT + weekly polls weight" = "green",
           "Allens" = "darkmagenta", "Forsa" = "darkgoldenrod1", "Kantar" = "cyan", "FGW" = "springgreen4")


p4 <- ggplot(data_plot4,
             aes(x = GT_end_date,
                 y = deviation_mean,
                 color = datasource_weight)) +
  geom_vline(xintercept = as.Date("2021-09-26"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2017-09-24"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2013-09-22"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2009-09-27"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid") +  
  #geom_point(size = 0.5) +
  geom_line() +
  theme_minimal() +
  facet_grid(vars(model_time_interval_fac),
             vars(election_date), 
             scales = "free") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_breaks,
               labels = paste("Distance: ",  x_labels_distance, " day(s)\n",
                              "Date: ", x_labels_date
               )
  ) +
  scale_color_manual(values = cols4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "datasource_weight") + 
  ylim(0,10)
#+geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))

p4

ggsave(plot = p4,
       filename = "plot_meanDeviations_CompareWeekly.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)






setwd("C:/Users/deanl/Desktop/UniMannheim/ComSocScience/Publikation/Election-Prediction-Google-Trends/Data_plots")

readRDS("data_plot_sonst.RDS")
readRDS("data_plot_WC.RDS")


#### plot GT WC/sonst/normal #########

data_plot2 <- data_plot %>%
  filter(datasource_weight =="GT" | datasource_weight =="Only polls" | datasource_weight =="Last polls"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE))

data_plot_sonst2 <- data_plot_sonst %>%
  filter(datasource_weight =="GT"
  ) %>%
  mutate(datasource_weight = "GTsonst") %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE))

data_plot_WC2 <- data_plot_WC %>%
  filter(datasource_weight =="GT"
  ) %>%
  mutate(datasource_weight = "GTWC") %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE))


cols2 <- c("GT" = "red", "Only polls" = "black", "Last polls" = "blue", "GTsonst" = "Green",  "GTWC" = "Orange")

#Plot GT vs. Polls
p5 <- ggplot() +
  geom_vline(xintercept = as.Date("2021-09-26"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2017-09-24"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2013-09-22"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2009-09-27"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid") +  
  #geom_point(size = 0.5) +
  geom_line(data=data_plot_sonst2, aes(x = GT_end_date, y = deviation_mean, color = datasource_weight)) +
  geom_line(data= data_plot2,aes(x = GT_end_date,y = deviation_mean, color = datasource_weight)) +
  #geom_line(data= data_plot_WC2,aes(x = GT_end_date,y = deviation_mean, color = datasource_weight)) +
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
  scale_color_manual(values = cols2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "datasource_weight")
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))


p5

ggsave(plot = p5,
       filename = "plot_Compare_GT_polls.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)


