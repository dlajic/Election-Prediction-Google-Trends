library(gtrendsR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rvest)
library(xml2)


#election results
##real election results
# constructing an election results 2005 data set to be able to use this data for the weighting factor in the Model 2 for 2009
election_results_05 <- data.frame(keyword=c("CDU", "FDP", "Grüne", "Linke", "SPD"), 
                                  perc=c(35.2, 9.8, 8.1, 8.7, 34.2), mean=0, 
                                  summarized_mean=sum(35.2, 9.8, 8.1, 8.7, 34.2), model=4)

# constructing two election_results_09 data sets (first data set for depiction in results plot & second data set to construct weighting factor in the Model 2 for 2013)
# attention: in election results data set "election_results_09" the absolute percentage column is called "perc_dev" to include it in the final plot

election_results_09 <- data.frame(keyword=c("CDU", "FDP", "Grüne", "Linke", "SPD"), 
                                  perc_dev=c(33.8, 14.6, 10.7, 11.9, 23), mean=0, 
                                  summarized_mean=sum(33.8, 14.6, 10.7, 11.9, 23), model=5, perc=1)

election_results_13 <- data.frame(keyword=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), 
                                  perc_dev=c(4.7, 41.5, 4.8, 8.4, 8.6, 25.7), mean=0, 
                                  summarized_mean=sum(4.7, 41.5, 4.8, 8.4, 8.6, 25.7), model=5, perc=1)

# constructing an election_results_17 data set (for depiction in results plot)
# attention: in election results data set "election_results_17" the absolute percentage column is called "perc_dev" to include it in the final plot

election_results_17 <- data.frame(keyword=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), 
                                  perc_dev=c(12.6, 32.9, 10.7, 8.9 , 9.2, 20.5), mean=0, 
                                  summarized_mean=sum(12.6, 32.9, 10.7, 8.9 , 9.2, 20.5), model=5, perc=1)


####
election_date = as.Date(c("26-09-2021",
                          "24-09-2017",
                          "22-09-2013",
                          "27-09-2009",
                          "18-09-2005"), format = "%d-%m-%Y")




# Create dataframe
data_models <- expand.grid(election_date = as.Date(c("26-09-2021",
                                                     "24-09-2017",
                                                     "22-09-2013",
                                                     "27-09-2009"), format = "%d-%m-%Y"),
                           datasource_weight = c("GT"),
                           model_time_interval = duration(seq(7,42, 7), "days"))

data_models$year <- 0
for(i in 1:nrow(data_models)){

  data_models$year[i] <- year(data_models$election_date[i])
  
}


data_models <- data_models %>% arrange(year)



List09 <- list()
  
####2009
    for(j in 0:6){
      
      if (j ==0){
        trend_09 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Grüne + Jürgen Trittin + Renate Künast', 'Linke + Gregor Gysi', 
                                      'FDP + Guido Westerwelle'), geo= "DE" , category=19, time = "2009-01-01 2009-09-27", gprop="web", onlyInterest =  TRUE)
        
        df_09 <- trend_09$interest_over_time
        
        df_final09 <- df_09 %>%
          select(date, hits, keyword) %>%
          mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
          replace(is.na(.), 0) %>%
          mutate(keyword = gsub('CDU.*', 'CDU', gsub('SPD.*', 'SPD', gsub('Grüne.*', 'Grüne', 
                                                                          gsub('Linke.*', 'Linke', gsub('FDP.*', 'FDP', keyword))))))
        assign(paste0("df_final_09_",j), df_final09)
        
        List09[[1]] <- df_final09
        
        }
      
      if (j>0){
        trend_09 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Grüne + Jürgen Trittin + Renate Künast', 'Linke + Gregor Gysi', 
                                    'FDP + Guido Westerwelle'), geo= "DE" , category=19, time = paste(as.Date(as.Date("2009-09-27") - data_models$model_time_interval[j]), "2009-09-27"), gprop="web", onlyInterest =  TRUE)
        
        df_09 <- trend_09$interest_over_time
        
        df_final_09 <- df_09 %>%
          select(date, hits, keyword) %>%
          mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
          replace(is.na(.), 0) %>%
          mutate(keyword = gsub('CDU.*', 'CDU', gsub('SPD.*', 'SPD', gsub('Grüne.*', 'Grüne', 
                                                                          gsub('Linke.*', 'Linke', gsub('FDP.*', 'FDP', keyword))))))
        
        assign(paste0("df_final_09_",j), df_final_09)
        
        List09[[j+1]] <- df_final_09
  
      }
  
    }
  
  
  
####2013
List13 <- list()

    for(j in 0:6){
      
      if (j ==0){
        trend_CDU_13 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne + ', 'Linke + Sarah Wagenknecht + Gregor Gysi', 
                      'FDP + Philipp Rösler'), geo= "DE" , category=19, time = "2013-01-01 2013-09-21", gprop="web", onlyInterest =  TRUE)
        
        Sys.sleep(1)
                                          
        trend_AFD_13 = gtrends(keyword= c('Bernd Lucke + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne', 
                      'FDP + Philipp Rösler'), geo= "DE" , category=19, time = "2013-01-01 2013-09-21", gprop="web", onlyInterest =  TRUE)
                                          
        df_CDU_13 <- trend_CDU_13$interest_over_time
        df_AFD_13 <- trend_AFD_13$interest_over_time
        
        
        df_AFD_13 <- df_AFD_13 %>%
          filter(grepl("Afd.*", df_AFD_13$keyword) == TRUE)
        
        
        df_final13 <- bind_rows(df_CDU_13, df_AFD_13) %>%
          select(date, hits, keyword) %>%
          mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
          replace(is.na(.), 0) %>%
          mutate(keyword = gsub('CDU.*', 'CDU', gsub('SPD.*', 'SPD', gsub('Jürgen.*', 'Grüne', 
                           gsub('Linke.*', 'Linke',  gsub('FDP.*', 'FDP', gsub('Bernd.*', 'AFD', keyword)))))))
        
        assign(paste0("df_final_13_",j), df_final13)
        
        Sys.sleep(1)
        
        List13[[1]] <- df_final13
        
      }
      
      if (j>0){
        trend_CDU_13 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne + ', 'Linke + Sarah Wagenknecht + Gregor Gysi', 
                                          'FDP + Philipp Rösler'), geo= "DE" , category=19, time = paste(as.Date(as.Date("2013-09-21") - data_models$model_time_interval[j]), as.Date("2013-09-21")), gprop="web", onlyInterest =  TRUE)
        
        Sys.sleep(1)
        
        trend_AFD_13 = gtrends(keyword= c('Bernd Lucke + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne', 
                                          'FDP + Philipp Rösler'), geo= "DE" , category=19, time = paste(as.Date(as.Date("2013-09-21") - data_models$model_time_interval[j]), as.Date("2013-09-21")), gprop="web", onlyInterest =  TRUE)
        
        df_CDU_13 <- trend_CDU_13$interest_over_time
        df_AFD_13 <- trend_AFD_13$interest_over_time
        
        
        df_AFD_13 <- df_AFD_13 %>%
          filter(grepl("Afd.*", df_AFD_13$keyword) == TRUE)
        
        
        df_final13 <- bind_rows(df_CDU_13, df_AFD_13) %>%
          select(date, hits, keyword) %>%
          mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
          replace(is.na(.), 0) %>%
          mutate(keyword = gsub('CDU.*', 'CDU', gsub('SPD.*', 'SPD', gsub('Jürgen.*', 'Grüne', 
                                                                          gsub('Linke.*', 'Linke',  gsub('FDP.*', 'FDP', gsub('Bernd.*', 'AFD', keyword)))))))
        
        assign(paste0("df_final_13_",j), df_final13)
        
        List13[[j+1]] <- df_final13
        
      }
    }
      



####2013
List17 <- list()

for(j in 0:6){
  
  if (j ==0){
    
    trend_CDU_17 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne', 'Linke + Sarah Wagenknecht + Dietmar Bartsch', 
                                'FDP + Christian Lindner'), geo= "DE" , category=19, time = "2017-01-01 2017-09-23", gprop="web", onlyInterest =  TRUE)

    
    trend_AFD_17 = gtrends(keyword= c('Alice Weidel + Alexander Gauland + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne',
                                      'FDP + Christian Lindner'), geo= "DE" , category=19, time = "2017-01-01 2017-09-23", gprop="web", onlyInterest =  TRUE)
    
    
    
    df_CDU_17 <- trend_CDU_17$interest_over_time
    df_AFD_17 <- trend_AFD_17$interest_over_time
   
    df_AFD_17 <- df_AFD_17 %>%
      filter(grepl("Afd.*", df_AFD_17$keyword) == TRUE)

    df_final17 <- bind_rows(df_CDU_17, df_AFD_17) %>%
      select(date, hits, keyword) %>%
      mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
      replace(is.na(.), 0) %>%
      mutate(keyword = gsub('CDU.*', 'CDU', gsub('SPD.*', 'SPD', gsub('Cem.*', 'Grüne', 
                                                                      gsub('Linke.*', 'Linke', gsub('FDP.*', 'FDP', gsub('Alice.*', 'AFD', keyword)))))))
    
    
    assign(paste0("df_final_17_",j), df_final17)
    
    Sys.sleep(1)
    
    List17[[1]] <- df_final17
    
  }
  
  if (j>0){
    trend_CDU_17 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne', 'Linke + Sarah Wagenknecht + Dietmar Bartsch', 
                                      'FDP + Christian Lindner'), geo= "DE" , category=19, time = paste(as.Date(as.Date("2017-09-23") - data_models$model_time_interval[j]), as.Date("2017-09-23")), gprop="web", onlyInterest =  TRUE)

    trend_AFD_17 = gtrends(keyword= c('Alice Weidel + Alexander Gauland + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne',
                                     'FDP + Christian Lindner'), geo= "DE" , category=19, time = paste(as.Date(as.Date("2017-09-23") - data_models$model_time_interval[j]), as.Date("2017-09-23")), gprop="web", onlyInterest =  TRUE)
    
      
    df_CDU_17 <- trend_CDU_17$interest_over_time
    df_AFD_17 <- trend_AFD_17$interest_over_time
    
    df_AFD_17 <- df_AFD_17 %>%
      filter(grepl("Afd.*", df_AFD_17$keyword) == TRUE)
    
    df_final17 <- bind_rows(df_CDU_17, df_AFD_17) %>%
      select(date, hits, keyword) %>%
      mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
      replace(is.na(.), 0) %>%
      mutate(keyword = gsub('CDU.*', 'CDU', gsub('SPD.*', 'SPD', gsub('Cem.*', 'Grüne', 
                                                                      gsub('Linke.*', 'Linke', gsub('FDP.*', 'FDP', gsub('Alice.*', 'AFD', keyword)))))))
    
    
    assign(paste0("df_final_17_",j), df_final17)
    
    Sys.sleep(1)
    
    List17[[j+1]] <- df_final17

    
  }
}




####2021
List21 <- list()

for(j in 0:6){
  
  if (j ==0){
    
    trend_CDU_21 = gtrends(keyword= c('CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 'Linke + Janine Wissler + Dietmar Bartsch', 
                                      'FDP + Christian Lindner'), geo= "DE" , category=19, time = "2021-01-01 2021-09-26", gprop="web", onlyInterest =  TRUE)
    
    Sys.sleep(1)
    
    trend_AFD_21 = gtrends(keyword= c('Alice Weidel + Tino Chrupalla + Afd', 'CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 
                                      'FDP + Christian Lindner'), geo= "DE" , category=19, time = "2021-01-01 2021-09-26", gprop="web", onlyInterest =  TRUE)
    

    df_CDU_21 <- trend_CDU_21$interest_over_time
    df_AFD_21 <- trend_AFD_21$interest_over_time
    
    
    df_AFD_21 <- df_AFD_21 %>%
      filter(grepl("Afd.*", df_AFD_21$keyword) == TRUE)
    
    df_CDU_21$hits <- as.numeric(df_CDU_21$hits)
    df_AFD_21$hits <- as.numeric(df_AFD_21$hits)
    
    df_final21 <- bind_rows(df_CDU_21, df_AFD_21) %>%
      select(date, hits, keyword) %>%
      mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
      replace(is.na(.), 0) %>%
      mutate(keyword = gsub('CDU.*', 'CDU', gsub('SPD.*', 'SPD', gsub('Annalena.*', 'Grüne',
                                                                      gsub('Linke.*','Linke', gsub('FDP.*', 'FDP', gsub('Alice.*', 'AFD', keyword)))))))
    
    
    assign(paste0("df_final_21_",j), df_final21)
    
    Sys.sleep(1)
    
    List21[[1]] <- df_final21
    
  }
  
  if (j>0){
    trend_CDU_21 = gtrends(keyword= c('CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 'Linke + Janine Wissler + Dietmar Bartsch', 
                                      'FDP + Christian Lindner'), geo= "DE" , category=19, time = paste(as.Date(as.Date("2021-09-26") - data_models$model_time_interval[j]), as.Date("2021-09-26")), gprop="web", onlyInterest =  TRUE)
    
    Sys.sleep(1)
    
    trend_AFD_21 = gtrends(keyword= c('Alice Weidel + Tino Chrupalla + Afd', 'CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 
                                      'FDP + Christian Lindner'), geo= "DE" , category=19, time = paste(as.Date(as.Date("2021-09-26") - data_models$model_time_interval[j]), as.Date("2021-09-26")), gprop="web", onlyInterest =  TRUE)
    
    
    df_CDU_21 <- trend_CDU_21$interest_over_time
    df_AFD_21 <- trend_AFD_21$interest_over_time
    
    
    df_AFD_21 <- df_AFD_21 %>%
      filter(grepl("Afd.*", df_AFD_21$keyword) == TRUE)
    
    df_CDU_21$hits <- as.numeric(df_CDU_21$hits)
    df_AFD_21$hits <- as.numeric(df_AFD_21$hits)
    
    df_final21 <- bind_rows(df_CDU_21, df_AFD_21) %>%
      select(date, hits, keyword) %>%
      mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
      replace(is.na(.), 0) %>%
      mutate(keyword = gsub('CDU.*', 'CDU', gsub('SPD.*', 'SPD', gsub('Annalena.*', 'Grüne',
                                                                      gsub('Linke.*','Linke', gsub('FDP.*', 'FDP', gsub('Alice.*', 'AFD', keyword)))))))
    
    
    assign(paste0("df_final_21_",j), df_final21)
    
    Sys.sleep(1)
    
    List21[[j+1]] <- df_final21
    
    
  }
}



################### Comparison

df_interval <- list(df_final_09_1,
                    df_final_09_2,
                    df_final_09_3,
                    df_final_09_4,
                    df_final_09_5,
                    df_final_09_6,
                    df_final_13_1,
                    df_final_13_2,
                    df_final_13_3,
                    df_final_13_4,
                    df_final_13_5,
                    df_final_13_6,
                    df_final_17_1,
                    df_final_17_2,
                    df_final_17_3,
                    df_final_17_4,
                    df_final_17_5,
                    df_final_17_6,
                    df_final_21_1,
                    df_final_21_2,
                    df_final_21_3,
                    df_final_21_4,
                    df_final_21_5,
                    df_final_21_6)

names(df_interval) <- c("df_final_09_1",
                        "df_final_09_2",
                        "df_final_09_3",
                        "df_final_09_4",
                        "df_final_09_5",
                        "df_final_09_6",
                        "df_final_13_1",
                        "df_final_13_2",
                        "df_final_13_3",
                        "df_final_13_4",
                        "df_final_13_5",
                        "df_final_13_6",
                        "df_final_17_1",
                        "df_final_17_2",
                        "df_final_17_3",
                        "df_final_17_4",
                        "df_final_17_5",
                        "df_final_17_6",
                        "df_final_21_1",
                        "df_final_21_2",
                        "df_final_21_3",
                        "df_final_21_4",
                        "df_final_21_5",
                        "df_final_21_6")


df_year <- rbind(df_final_09_0,
                 df_final_13_0,
                 df_final_17_0,
                 df_final_21_0)


for (i in 1:length(df_interval)){
  
  
  if(identical(df_interval[[i]][["hits"]], df_year[df_year$date >= as.Date(first(df_interval[[i]][[1]])) &  df_year$date <= as.Date(last(df_interval[[i]][[1]])),2]) == TRUE){
    print(paste("equal", names(df_interval[i])))
  }
  
  else{
    print(paste("unequal", names(df_interval[i])))
    
    
    a <- df_year %>%
      filter(date >= as.Date(first(df_interval[[i]][[1]])) &  date <= as.Date(last(df_interval[[i]][[1]])))%>%
      group_by(keyword)%>%
      summarize_at(vars(hits),list(mean = mean)) %>%
      mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100)%>%
      print()
    
    b <- df_interval[i][[1]] %>%
      group_by(keyword)%>%
      summarize_at(vars(hits),list(mean = mean)) %>%
      mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100) %>%
      print()
    
    print(a$perc - b$perc)
    
  }
  
  
}
