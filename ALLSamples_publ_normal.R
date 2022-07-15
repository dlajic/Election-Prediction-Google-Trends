library(gtrendsR)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(rvest)
library(xml2)
library(data.table)
library(patchwork)


#### Scraping survey data from the infratest dimap website ####


# To be able to weight Google trends data with data from opinion polls, 
# polling results of infratest dimap polling institute are scraped from their website


url <- "https://www.infratest-dimap.de/umfragen-analysen/bundesweit/sonntagsfrage/"

html <- read_html(url)
tables <- html_table(html, fill=TRUE) # extract the HTML table from the scrapped data with all survey results over time (no other html table on this page of the website)
infra_dimap_all <- as.data.frame(tables) # construct data frame from scraped htmltable

# delete column for political party "Freie Wähler" and column "Other"
infra_dimap_all$X8 <- NULL
infra_dimap_all$X9 <- NULL

# assign names of parties to columns and delete first row of data set (contains names of political parties)
colnames(infra_dimap_all) <- c("Date", "SPD", "CDU", "Grüne", "FDP", "AFD", "Linke")
infra_dimap_all <- infra_dimap_all[-1,] 




#### Create data sets with real election results ####

# constructing an election results 2005 data set to be able to use this data for the weighting factor in the Model 2 for 2009
election_results_05 <- data.frame(keyword=c("CDU", "FDP", "Grüne", "Linke", "SPD"), 
                                  perc=c(35.2, 9.8, 8.1, 8.7, 34.2), mean=0, 
                                  summarized_mean=sum(35.2, 9.8, 8.1, 8.7, 34.2), model=4)


# constructing two election_results_09 data sets (first data set for depiction in results plot & second data set to construct weighting factor in the Model 2 for 2013)
# attention: in election results data set "election_results_09" the absolute percentage column is called "perc_dev" to include it in the final plot


election_results_09 <- data.frame(keyword=c("CDU", "FDP", "Grüne", "Linke", "SPD"), 
                                  perc_dev=c(33.8, 14.6, 10.7, 11.9, 23), mean=0, 
                                  summarized_mean=sum(33.8, 14.6, 10.7, 11.9, 23), model=6, perc=1)

# Political party AFD was not existent in election year 2009. Thus we can not derive a weighting factor for the political party AFD in the Model 2 for 2013.
# Problem: To be able to use the weighting factor in the Model 2 for 2013, we need 6 columns. This is the reason why we assign the political party AFD
# in the data set election_results_09_M2 the value 1 (AFD is weighted with factor 1 in the Model 2 for 2013 = no weight for AFD).

election_results_09_M2 <- data.frame(keyword=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), 
                                     perc=c(1, 33.8, 14.6, 10.7, 11.9, 23), mean=0, 
                                     summarized_mean=sum(1, 33.8, 14.6, 10.7, 11.9, 23), model=4)


#constructing an election_results_13 data set (for depiction in results plot)
# attention: in election results data set "election_results_13" the absolute percentage column is called "perc_dev" to include it in the final plot

election_results_13 <- data.frame(keyword=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), 
                                  perc_dev=c(4.7, 41.5, 4.8, 8.4, 8.6, 25.7), mean=0, 
                                  summarized_mean=sum(4.7, 41.5, 4.8, 8.4, 8.6, 25.7), model=6, perc=1)


# constructing an election_results_17 data set (for depiction in results plot)
# attention: in election results data set "election_results_17" the absolute percentage column is called "perc_dev" to include it in the final plot

election_results_17 <- data.frame(keyword=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), 
                                  perc_dev=c(12.6, 32.9, 10.7, 8.9 , 9.2, 20.5), mean=0, 
                                  summarized_mean=sum(12.6, 32.9, 10.7, 8.9 , 9.2, 20.5), model=6, perc=1)


# constructing an election_results_21 data set (for depiction in results plot)
# attention: in election results data set "election_results_21" the absolute percentage column is called "perc_dev" to include it in the final plot

election_results_21 <- data.frame(keyword=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), 
                                  perc_dev=c(10.3, 24.1, 11.5, 14.8 , 4.9, 25.7), mean=0, 
                                  summarized_mean=sum(12.6, 24.1, 10.7, 8.9 , 9.2, 20.5), model=6, perc=1)



#### Preparation for loop 1####

full_Model1_09_1month <- data.frame()
full_Model2_09_1month <- data.frame()
full_Model3_09_1month <- data.frame()

full_Model1_13_1month <- data.frame()
full_Model2_13_1month <- data.frame()
full_Model3_13_1month <- data.frame()

full_Model1_17_1month <- data.frame()
full_Model2_17_1month <- data.frame()
full_Model3_17_1month <- data.frame()

full_Model1_21_1month <- data.frame()
full_Model2_21_1month <- data.frame()
full_Model3_21_1month <- data.frame()



full_Model1_09_3month <- data.frame()
full_Model2_09_3month <- data.frame()
full_Model3_09_3month <- data.frame()

full_Model1_13_3month <- data.frame()
full_Model2_13_3month <- data.frame()
full_Model3_13_3month <- data.frame()

full_Model1_17_3month <- data.frame()
full_Model2_17_3month <- data.frame()
full_Model3_17_3month <- data.frame()

full_Model1_21_3month <- data.frame()
full_Model2_21_3month <- data.frame()
full_Model3_21_3month <- data.frame()




counter <- c("2021-09-20 11-21-45", "2021-09-21 15-21-45", "2021-09-22 15-21-45")



#### Loop 1 for data prep and calc of Model 1,2,3 for every data set specified in counter ####

# Functioning: 
# One of the data sets specified in counter is read in. 
# Then, the data preparation of the raw Google Trends data takes place and a final data set is created for each year (df_final05, 09, 13, 17, 21). 
# Then the survey data is prepared which is used for weighting and for the presentation in the final overview plots.
# Then the individual models are calculated and finally written into the individual empty data sets created before the loop.


for(i in counter){
  name <- paste(i,".RData",sep="") 
  load(name)
  
  # preparing Google trends data sets (pulled with R Script PublicData.R)
  
  # preparing Google trends data set for the year 2005 (Note: only needed for constructing the weighting factor for Model 2 in 2009)
  
  df_05 <- trend_05$interest_over_time
  
  # construct final data set for the year 2005
  # using gsub (searches for patterns in character variables and replaces a match with a specified word)
  # expression .* in gsub command = everything after the expression/word
  
  df_final05 <- df_05 %>%
    select(date, hits, keyword) %>%
    mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
    replace(is.na(.), 0) %>%
    mutate(keyword = gsub('CDU.*', 'CDU', gsub('SPD.*', 'SPD', gsub('Grüne.*', 'Grüne',
                                                                    gsub('PDS.*','Linke', gsub('FDP.*', 'FDP', keyword))))))
  
  
  
  # preparing Google trends data set for the year 2009 (Note: AFD existed not in 2009 -> thus only 5 political parties are in data set)
  
  df_09 <- trend_09$interest_over_time
  
  
  df_final09 <- df_09 %>%
    select(date, hits, keyword) %>%
    mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
    replace(is.na(.), 0) %>%
    mutate(keyword = gsub('CDU.*', 'CDU', gsub('SPD.*', 'SPD', gsub('Grüne.*', 'Grüne', 
                                                                    gsub('Linke.*', 'Linke', gsub('FDP.*', 'FDP', keyword))))))
  
  
  
  # preparing Google trends data set for the year 2013
  # gtrends/Google Trends only allows to make comparisons of up to five terms --> one extra query has to be executed for political party AFD
  
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
  
  
  
  # preparing Google trends data set for the year 2017
  # gtrends/Google Trends only allows to make comparisons of up to five terms --> one extra query has to be executed for political party AFD
  
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
  
  

  # preparing Google trends data set for the year 2021
  
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
  

  # create a dataset containing the average and confidence intervals of the poll results for each party before the 2009 election.
  # only important for visualization in summary plots
  
  infra_dimap_09_b_1month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2009-08-29" &  Date <= "2009-09-26") %>%
    mutate_if(is.character, as.numeric) %>% 
    summarise(perc = c(mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD)),
              SD = c(sd(CDU), sd(FDP),
                     sd(Grüne), sd(Linke), sd(SPD)),
              N = n()) %>%
    mutate(keyword = c("CDU", "FDP", "Grüne", "Linke", "SPD"), perc_dev=perc-election_results_09$perc_dev,
           lower.ci = perc - 1.96*(SD/sqrt(N)),
           upper.ci = perc + 1.96*(SD/sqrt(N)), 
           dev.low.ci = perc_dev - 1.96*(SD/sqrt(N)), 
           dev.up.ci = perc_dev + 1.96*(SD/sqrt(N)), 
           model=5)
  
  
  infra_dimap_09_b_3month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2009-07-04" &  Date <= "2009-09-26") %>%
    mutate_if(is.character, as.numeric) %>% 
    summarise(perc = c(mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD)),
              SD = c(sd(CDU), sd(FDP),
                     sd(Grüne), sd(Linke), sd(SPD)),
              N = n()) %>%
    mutate(keyword = c("CDU", "FDP", "Grüne", "Linke", "SPD"), perc_dev=perc-election_results_09$perc_dev,
           lower.ci = perc - 1.96*(SD/sqrt(N)),
           upper.ci = perc + 1.96*(SD/sqrt(N)), 
           dev.low.ci = perc_dev - 1.96*(SD/sqrt(N)), 
           dev.up.ci = perc_dev + 1.96*(SD/sqrt(N)), 
           model=5)
  

  
  # calculate average polling results of each political party of interest for weighting factor (1 month period / 3 month period) for the year 2009
  
  # Time span of Google proportion that will be weighted: 29.08.2009 - 26.09.2009
  # Time span of polling data that will be used for weighting: 8 to 4 weeks before election (31.07.2009 - 28.08.2009)
  
  infra_dimap_09_1month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2009-07-31" &  Date <= "2009-08-28") %>%
    mutate_if(is.character, as.numeric) %>%
    summarise(perc = c(mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD))) %>%
    mutate(keyword = c("CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_09$perc_dev, model=4)
  
  
  
  # Time span of Google proportion that will be weighted: 10.04.2009 - 26.09.2009
  # Time span of polling data that will be used for weighting: 24 to 12 weeks before election (3 months)
  
  infra_dimap_09_3month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2009-04-10" &  Date <= "2009-07-03") %>%
    mutate_if(is.character, as.numeric) %>%
    summarise(perc = c(mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD))) %>%
    mutate(keyword = c("CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_09$perc_dev, model=4)
  
  
  
  # create a data set containing the average of the poll results for each party before the 2013 election.
  
  infra_dimap_13_b_1month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2013-08-24" &  Date <= "2013-09-21") %>%
    mutate_if(is.character, as.numeric) %>% 
    summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD)),
              SD = c(sd(AFD, na.rm=TRUE), sd(CDU), sd(FDP),
                     sd(Grüne), sd(Linke), sd(SPD)),
              N = n()) %>%
    mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), perc_dev=perc-election_results_13$perc_dev,
           lower.ci = perc - 1.96*(SD/sqrt(N)),
           upper.ci = perc + 1.96*(SD/sqrt(N)), 
           dev.low.ci = perc_dev - 1.96*(SD/sqrt(N)), 
           dev.up.ci = perc_dev + 1.96*(SD/sqrt(N)), 
           model=5)
  
  
  infra_dimap_13_b_3month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2013-06-29" &  Date <= "2013-09-21") %>%
    mutate_if(is.character, as.numeric) %>% 
    summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD)),
              SD = c(sd(AFD, na.rm=TRUE), sd(CDU), sd(FDP),
                     sd(Grüne), sd(Linke), sd(SPD)),
              N = n()) %>%
    mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), perc_dev=perc-election_results_13$perc_dev,
           lower.ci = perc - 1.96*(SD/sqrt(N)),
           upper.ci = perc + 1.96*(SD/sqrt(N)), 
           dev.low.ci = perc_dev - 1.96*(SD/sqrt(N)), 
           dev.up.ci = perc_dev + 1.96*(SD/sqrt(N)), 
           model=5)
  
  # calculate average polling results of each political party of interest (1 month period / 3 month period) for the year 2013
  
  # Time span of Google proportion that will be weighted: 24.08.2013 - 21.09.2013
  # Time span of polling data that will be used for weighting: 8 to 4 weeks before election (1 month)
  
  infra_dimap_13_1month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2013-07-26" &  Date <= "2013-08-23") %>%
    mutate_if(is.character, as.numeric) %>%
    summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD))) %>%
    mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_13$perc_dev, model=4)
  
  
  
  # Time span of Google proportion that will be weighted: 29.06.2013 - 21.09.2013
  # Time span of polling data that will be used for weighting: 24 to 12 weeks before election (3 months)
  
  infra_dimap_13_3month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2013-04-05" &  Date <= "2013-06-28") %>%
    mutate_if(is.character, as.numeric) %>%
    summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD))) %>%
    mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_13$perc_dev, model=4)
  
  
  
  
  # create a data set containing the average of the poll results for each party before the 2017 election
  
  infra_dimap_17_b_1month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2017-08-26" &  Date <= "2017-09-23") %>%
    mutate_if(is.character, as.numeric) %>% 
    summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD)),
              SD = c(sd(AFD, na.rm=TRUE), sd(CDU), sd(FDP),
                     sd(Grüne), sd(Linke), sd(SPD)),
              N = n()) %>%
    mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), perc_dev=perc-election_results_17$perc_dev,
           lower.ci = perc - 1.96*(SD/sqrt(N)),
           upper.ci = perc + 1.96*(SD/sqrt(N)), 
           dev.low.ci = perc_dev - 1.96*(SD/sqrt(N)), 
           dev.up.ci = perc_dev + 1.96*(SD/sqrt(N)), 
           model=5)
  
  infra_dimap_17_b_3month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2017-07-01" &  Date <= "2017-09-23") %>%
    mutate_if(is.character, as.numeric) %>% 
    summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD)),
              SD = c(sd(AFD, na.rm=TRUE), sd(CDU), sd(FDP),
                     sd(Grüne), sd(Linke), sd(SPD)),
              N = n()) %>%
    mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), perc_dev=perc-election_results_17$perc_dev,
           lower.ci = perc - 1.96*(SD/sqrt(N)),
           upper.ci = perc + 1.96*(SD/sqrt(N)), 
           dev.low.ci = perc_dev - 1.96*(SD/sqrt(N)), 
           dev.up.ci = perc_dev + 1.96*(SD/sqrt(N)), 
           model=5)
  
  # calculate average polling results of each political party of interest (1 month period / 3 month period) for the year 2017
  
  # Time span of Google proportion that will be weighted: 26.08.2017 - 23.09.2017
  # Time span of polling data that will be used for weighting: 8 to 4 weeks before election (1 month)
  
  infra_dimap_17_1month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2017-07-28" &  Date <= "2017-08-25") %>%
    mutate_if(is.character, as.numeric) %>%
    summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD))) %>%
    mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_17$perc_dev, model=4)
  
  # Time span of Google proportion that will be weighted: 01.07.2017 - 23.09.2017
  # Time span of polling data that will be used for weighting: 24 to 12 weeks before election (3 months)
  
  infra_dimap_17_3month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2017-04-07" &  Date <= "2017-06-30") %>%
    mutate_if(is.character, as.numeric) %>%
    summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD))) %>%
    mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_17$perc_dev, model=4)
  
  
  
  # create a data set containing the average of the poll results for each party before the 2021 election.
  
  infra_dimap_21_b_1month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2021-08-23" &  Date <= "2021-09-25") %>%
    mutate_if(is.character, as.numeric) %>% 
    summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD)),
              SD = c(sd(AFD, na.rm=TRUE), sd(CDU), sd(FDP),
                     sd(Grüne), sd(Linke), sd(SPD)),
              N = n()) %>%
    mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), perc_dev=perc-election_results_21$perc_dev,
           lower.ci = perc - 1.96*(SD/sqrt(N)),
           upper.ci = perc + 1.96*(SD/sqrt(N)), 
           dev.low.ci = perc_dev - 1.96*(SD/sqrt(N)), 
           dev.up.ci = perc_dev + 1.96*(SD/sqrt(N)), 
           model=5)
  
  infra_dimap_21_b_3month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2021-07-02" &  Date <= "2021-09-25") %>%
    mutate_if(is.character, as.numeric) %>% 
    summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD)),
              SD = c(sd(AFD, na.rm=TRUE), sd(CDU), sd(FDP),
                     sd(Grüne), sd(Linke), sd(SPD)),
              N = n()) %>%
    mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), perc_dev=perc-election_results_21$perc_dev,
           lower.ci = perc - 1.96*(SD/sqrt(N)),
           upper.ci = perc + 1.96*(SD/sqrt(N)), 
           dev.low.ci = perc_dev - 1.96*(SD/sqrt(N)), 
           dev.up.ci = perc_dev + 1.96*(SD/sqrt(N)), 
           model=5)
  

  
  
  
  # Time span of Google proportion that will be weighted: 23.08.2021 - 25.09.2021
  # Time span of polling data that will be used for weighting: 8 to 4 weeks before election (1 month)
  
  infra_dimap_21_1month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2021-07-21" &  Date <= "2021-08-22") %>%
    mutate_if(is.character, as.numeric) %>%
    summarise(perc = c(mean(AFD), mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD))) %>%
    mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_21$perc_dev, model=4)
  
  
  # Time span of Google proportion that will be weighted: 02.07.2021 - 25.09.2021
  # Time span of polling data that will be used for weighting: 24 to 12 weeks before election (3 months)
  
    infra_dimap_21_3month <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
    filter(Date >= "2021-04-09" &  Date <= "2021-07-02") %>%
    mutate_if(is.character, as.numeric) %>%
    summarise(perc = c(mean(AFD), mean(CDU), mean(FDP),
                       mean(Grüne), mean(Linke), mean(SPD))) %>%
    mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_21$perc_dev, model=4)
  
  


  
  ## constructing for each election year (2009, 2013, 2017, 2021) three models: 
  # Model 1 = Google Trends Proportion is calculated by dividing a party's average Google Trends "hits" score by the sum of Google Trends averages of all parties
  # Model 2 = The Google Trends proportions 1 month or 3 months before the election are weighted. For each party, a weighting factor is calculated by dividing a party's score in the
  #           in the previous election by the Google Trends proportion 1 month or 3 months before that previous election.  
  # Model 3 = The Google Trends proportions 1 month or 3 months before the election are weighted. For each party, a weighting factor is calculated by dividing a party's average score in 
  #           election polls one month or 3 months before the period of Google proportions to be weighted by the Google proportion one month or 3 months before the period of
  #           Google proportions to be weighted.
  ##
  
  
  
  # constructing a Google proportion 2005 data set (Model1_05) to be able to use this data for the weighting factor in the Model 2 for 2009  (only used for this purpose)
  Model1_05_1month <-  df_final05 %>%
    filter(date >= "2005-08-20" &  date <= "2005-09-17")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2), model=1) 
  
  
  Model1_05_3month <-  df_final05 %>%
    filter(date >= "2005-06-25" &  date <= "2005-09-17")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2), model=1) 
  
  
  
  ## 2009
  # Model1_09_M2 only to construct weighting factor in the Model 2 for 2013 (artificial data set with artificial Afd column)
  
  Model1_09_M2_1month <-  df_final09 %>%
    filter(date >= "2009-08-29" &  date <= "2009-09-26")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2)) %>%
    add_row(keyword = "AFD", mean=0, summarized_mean=1, perc=1, .before = 1) %>% # add artificial row for AFD in 2009
    mutate(perc_dev=perc-election_results_09_M2$perc, model=1)
  
  
  Model1_09_M2_3month <-  df_final09 %>%
    filter(date >= "2009-07-04" &  date <= "2009-09-26")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2)) %>%
    add_row(keyword = "AFD", mean=0, summarized_mean=1, perc=1, .before = 1) %>% # add artificial row for AFD in 2009
    mutate(perc_dev=perc-election_results_09_M2$perc, model=1)
  
  
  # Model 1 (calculating Google proportion)
  
  
  Model1_09_1month <-  df_final09 %>%
    filter(date >= "2009-08-29" &  date <= "2009-09-26")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2), 
           perc_dev=perc-election_results_09$perc_dev, model=1)
  
  
  
  Model1_09_3month <-  df_final09 %>%
    filter(date >= "2009-07-04" &  date <= "2009-09-26")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2), 
           perc_dev=perc-election_results_09$perc_dev, model=1)
  
  
  # Model 2 (Using previous election results and the Google proportion before the last election to construct a weighting factor)
  
  
  Model2_09_1month <-  df_final09 %>%
    filter(date >= "2009-08-29" &  date <= "2009-09-26")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_prev_election= election_results_05$perc/Model1_05_1month$perc,  perc= perc_old*weight_prev_election,
           perc_dev=perc-election_results_09$perc_dev, model=2) %>%
    select(keyword, mean, summarized_mean, perc, perc_dev, model) 
  
  
  
  Model2_09_3month <-  df_final09 %>%
    filter(date >= "2009-07-04" &  date <= "2009-09-26")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_prev_election= election_results_05$perc/Model1_05_3month$perc,  perc= perc_old*weight_prev_election,
           perc_dev=perc-election_results_09$perc_dev, model=2) %>%
    select(keyword, mean, summarized_mean, perc, perc_dev, model) 
  
  
  
  # Model 3 (Using average of election polls from "infratest dimap" polling institute one month or 3 months before the period of
  #           Google proportions to be weighted and the Google proportion one month or 3 months before the period of
  #           Google proportions to be weighted as a weighting factor) 
  
  Model3_09_1month <-  df_final09 %>%
    filter(date >= "2009-07-31" &  date <= "2009-08-28") %>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_avg_polls= infra_dimap_09_1month$perc/perc_old, perc=Model1_09_1month$perc*weight_avg_polls, perc=round(perc, digits = 2),
           perc_dev=perc-election_results_09$perc_dev, model=3) %>% 
    select(keyword, mean, summarized_mean, perc, perc_dev, model)
  


  Model3_09_3month <-  df_final09 %>%
    filter(date >= "2009-04-10" &  date <= "2009-07-03") %>%
    group_by(keyword) %>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_avg_polls= infra_dimap_09_3month$perc/perc_old, calc=Model1_09_3month$perc*weight_avg_polls, perc= calc/sum(calc)*100, perc=round(perc, digits = 2),
           perc_dev=perc-election_results_09$perc_dev, model=3) %>% 
    select(keyword, mean, summarized_mean, perc, perc_dev, model)
  
  
  
  
  ## 2013
  
  
  # Model 1 (Google proportion)
  
  Model1_13_1month <-  df_final13 %>%
    filter(date >= "2013-08-31" &  date <= "2013-09-21") %>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2),
           perc_dev=perc-election_results_13$perc_dev, model=1) 
  
  
  Model1_13_3month <-  df_final13 %>%
    filter(date >= "2013-06-29" &  date <= "2013-09-21") %>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2),
           perc_dev=perc-election_results_13$perc_dev, model=1) 
  
  
  # Model 2 (Using previous election results and the Google proportion before the last election to construct a weighting factor)
  
  
  Model2_13_1month <-  df_final13 %>%
    filter(date >= "2013-08-31" &  date <= "2013-09-21") %>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_prev_election= election_results_09_M2$perc/Model1_09_M2_1month$perc,  perc= perc_old*weight_prev_election,
           perc_dev=perc-election_results_13$perc_dev, model=2) %>%
    select(keyword, mean, summarized_mean, perc, perc_dev, model)
  
  
  Model2_13_3month <-  df_final13 %>%
    filter(date >= "2013-06-29" &  date <= "2013-09-21") %>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_prev_election= election_results_09_M2$perc/Model1_09_M2_3month$perc,  perc= perc_old*weight_prev_election,
           perc_dev=perc-election_results_13$perc_dev, model=2) %>%
    select(keyword, mean, summarized_mean, perc, perc_dev, model)
  
  
  # Model 3 (Using average of election polls from "infratest dimap" polling institute one month or 3 months before the period of
  #           Google proportions to be weighted and the Google proportion one month or 3 months before the period of
  #           Google proportions to be weighted as a weighting factor) 
  
  Model3_13_1month <-  df_final13 %>%
    filter(date >= "2013-07-26" &  date <= "2013-08-23") %>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_avg_polls= infra_dimap_13_1month$perc/perc_old, perc= weight_avg_polls*Model1_13_1month$perc , 
           perc_dev=perc-election_results_13$perc_dev, model=3) %>% 
    select(keyword, mean, summarized_mean, perc, perc_dev, model)
  
  
  Model3_13_3month <-  df_final13 %>%
    filter(date >= "2013-04-05" &  date <= "2013-06-28") %>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_avg_polls= infra_dimap_13_3month$perc/perc_old, perc= weight_avg_polls*Model1_13_3month$perc , 
           perc_dev=perc-election_results_13$perc_dev, model=3) %>% 
    select(keyword, mean, summarized_mean, perc, perc_dev, model)
  
  
  
  
  ## 2017
  
  
  # Model 1 (Google proportion)
  
  Model1_17_1month <-  df_final17 %>%
    filter(date >= "2017-08-26" &  date <= "2017-09-23") %>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2), 
           perc_dev=perc-election_results_17$perc_dev, model=1) 
  
  
  Model1_17_3month <-  df_final17 %>%
    filter(date >= "2017-07-01" &  date <= "2017-09-23") %>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2), 
           perc_dev=perc-election_results_17$perc_dev, model=1) 
  
  
  # Model 2 (Using previous election results and the Google proportion before the last election to construct a weighting factor)
  
  Model2_17_1month <-  df_final17 %>%
    filter(date >= "2017-08-26" &  date <= "2017-09-23") %>%
    group_by(keyword) %>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_prev_election= election_results_13$perc_dev/Model1_13_1month$perc, perc= perc_old*weight_prev_election,
           perc_dev=perc-election_results_17$perc_dev, model=2) %>%
    select(keyword, mean, summarized_mean, perc, perc_dev, model) 
  
  
  
  Model2_17_3month <-  df_final17 %>%
    filter(date >= "2017-07-01" &  date <= "2017-09-23") %>%
    group_by(keyword) %>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_prev_election= election_results_13$perc_dev/Model1_13_3month$perc, perc= perc_old*weight_prev_election,
           perc_dev=perc-election_results_17$perc_dev, model=2) %>%
    select(keyword, mean, summarized_mean, perc, perc_dev, model) 
  
  
  
  # Model 3 (Using average of election polls from "infratest dimap" polling institute one month or 3 months before the period of
  #           Google proportions to be weighted and the Google proportion one month or 3 months before the period of
  #           Google proportions to be weighted as a weighting factor) 
  
  
  Model3_17_1month <-  df_final17 %>%
    filter(date >= "2017-07-28" &  date <= "2017-08-25")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_avg_polls= infra_dimap_17_1month$perc/perc_old, perc= weight_avg_polls * Model1_17_1month$perc,
           perc_dev=perc-election_results_17$perc_dev, model=3) %>%
    select(keyword, mean, summarized_mean, perc, perc_dev, model)
  
  
  
  Model3_17_3month <-  df_final17 %>%
    filter(date >= "2017-04-07" & date <= "2017-06-30")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_avg_polls= infra_dimap_17_3month$perc/perc_old, perc= weight_avg_polls * Model1_17_3month$perc,
           perc_dev=perc-election_results_17$perc_dev, model=3) %>%
    select(keyword, mean, summarized_mean, perc, perc_dev, model)
  
  

  ## 2021
  
  
  # Model 1 (Google proportion)
  
  Model1_21_1month <-  df_final21 %>%
    filter(date >= "2021-08-29" &  date <= "2021-09-25")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2),
           perc_dev=perc-election_results_21$perc_dev, model=1) 
  
  Model1_21_3month <-  df_final21 %>%
    filter(date >= "2021-07-04" &  date <= "2021-09-25")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2),
           perc_dev=perc-election_results_21$perc_dev, model=1) 
  

  
  # Model 2 (Using previous election results and the Google proportion before the last election to construct a weighting factor)
  
  Model2_21_1month <-  df_final21 %>%
    filter(date >= "2021-08-29" &  date <= "2021-09-25") %>%
    group_by(keyword) %>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_prev_election= election_results_17$perc_dev/Model1_17_1month$perc,  perc= perc_old*weight_prev_election,
           perc_dev=perc-election_results_21$perc_dev,model=2) %>%
    select(keyword, mean, summarized_mean, perc, perc_dev, model) 
  
  
  Model2_21_3month <-  df_final21 %>%
    filter(date >= "2021-07-04" &  date <= "2021-09-25") %>%
    group_by(keyword) %>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_prev_election= election_results_17$perc_dev/Model1_17_3month$perc,  perc= perc_old*weight_prev_election,
           perc_dev=perc-election_results_21$perc_dev,model=2) %>%
    select(keyword, mean, summarized_mean, perc, perc_dev, model) 
  
  
  # Model 3 (Using average of election polls from "infratest dimap" polling institute one month or 3 months before the period of
  #           Google proportions to be weighted and the Google proportion one month or 3 months before the period of
  #           Google proportions to be weighted as a weighting factor) 
  
  
  Model3_21_1month <-  df_final21 %>%
    filter(date >= "2021-08-03" &  date <= "2021-08-29") %>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_avg_polls= infra_dimap_21_1month$perc/perc_old, perc= weight_avg_polls * Model1_21_1month$perc, perc=round(perc, digits = 2),
           perc_dev=perc-election_results_21$perc_dev, model=3) %>%
    select(keyword, mean, summarized_mean, perc, perc_dev, model)
  
  
  Model3_21_3month <-  df_final21 %>%
    filter(date >= "2021-04-11" &  date <= "2021-07-03") %>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, 
           weight_avg_polls= infra_dimap_21_3month$perc/perc_old, perc= weight_avg_polls * Model1_21_3month$perc, perc=round(perc, digits = 2),
           perc_dev=perc-election_results_21$perc_dev, model=3) %>%
    select(keyword, mean, summarized_mean, perc, perc_dev, model)
  
  

  
# in every loop append data from current data set to final data set
  
  
# for 1 month 
  
  full_Model1_09_1month <- rbind(full_Model1_09_1month, Model1_09_1month[c("keyword","perc","perc_dev")])
  full_Model2_09_1month <- rbind(full_Model2_09_1month, Model2_09_1month[c("keyword","perc","perc_dev")])
  full_Model3_09_1month <- rbind(full_Model3_09_1month, Model3_09_1month[c("keyword","perc","perc_dev")])
  
  full_Model1_13_1month <- rbind(full_Model1_13_1month, Model1_13_1month[c("keyword","perc","perc_dev")])
  full_Model2_13_1month <- rbind(full_Model2_13_1month, Model2_13_1month[c("keyword","perc","perc_dev")])
  full_Model3_13_1month <- rbind(full_Model3_13_1month, Model3_13_1month[c("keyword","perc","perc_dev")])
  
  full_Model1_17_1month <- rbind(full_Model1_17_1month, Model1_17_1month[c("keyword","perc","perc_dev")])
  full_Model2_17_1month <- rbind(full_Model2_17_1month, Model2_17_1month[c("keyword","perc","perc_dev")])
  full_Model3_17_1month <- rbind(full_Model3_17_1month, Model3_17_1month[c("keyword","perc","perc_dev")])
  
  full_Model1_21_1month <- rbind(full_Model1_21_1month, Model1_21_1month[c("keyword","perc","perc_dev")])
  full_Model2_21_1month <- rbind(full_Model2_21_1month, Model2_21_1month[c("keyword","perc","perc_dev")])
  full_Model3_21_1month <- rbind(full_Model3_21_1month, Model3_21_1month[c("keyword","perc","perc_dev")])
  

  
  
# for 3 month   
  
  full_Model1_09_3month <- rbind(full_Model1_09_3month, Model1_09_3month[c("keyword","perc","perc_dev")])
  full_Model2_09_3month <- rbind(full_Model2_09_3month, Model2_09_3month[c("keyword","perc","perc_dev")])
  full_Model3_09_3month <- rbind(full_Model3_09_3month, Model3_09_3month[c("keyword","perc","perc_dev")])
  
  full_Model1_13_3month <- rbind(full_Model1_13_3month, Model1_13_3month[c("keyword","perc","perc_dev")])
  full_Model2_13_3month <- rbind(full_Model2_13_3month, Model2_13_3month[c("keyword","perc","perc_dev")])
  full_Model3_13_3month <- rbind(full_Model3_13_3month, Model3_13_3month[c("keyword","perc","perc_dev")])
  
  full_Model1_17_3month <- rbind(full_Model1_17_3month, Model1_17_3month[c("keyword","perc","perc_dev")])
  full_Model2_17_3month <- rbind(full_Model2_17_3month, Model2_17_3month[c("keyword","perc","perc_dev")])
  full_Model3_17_3month <- rbind(full_Model3_17_3month, Model3_17_3month[c("keyword","perc","perc_dev")])
  
  full_Model1_21_3month <- rbind(full_Model1_21_3month, Model1_21_3month[c("keyword","perc","perc_dev")])
  full_Model2_21_3month <- rbind(full_Model2_21_3month, Model2_21_3month[c("keyword","perc","perc_dev")])
  full_Model3_21_3month <- rbind(full_Model3_21_3month, Model3_21_3month[c("keyword","perc","perc_dev")])
  
  
  
}



#### Preparation for loop 2 ####

# Find all dates of surveys that lie within the time interval (1 month/3 months)

# pull extracts a column from a data frame as a vector

# rename dates_poll_09_1month

dates_09_1month <- infra_dimap_all %>%
  mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
  filter(Date >= "2009-08-29" &  Date <= "2009-09-26") %>%
  mutate_if(is.character, as.numeric) %>%
  arrange(Date) %>% # to start element with earliest date, not latest  
  pull(Date) # creates an object instead of a data set

dates_09_3month <- infra_dimap_all %>%
  mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
  filter(Date >= "2009-07-04" &  Date <= "2009-09-26") %>%
  mutate_if(is.character, as.numeric) %>%
  arrange(Date) %>% # to start element with earliest date, not latest  
  pull(Date) # creates an object instead of a data set



dates_13_1month <- infra_dimap_all %>%
  mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
  filter(Date >= "2013-08-31" &  Date <= "2013-09-21") %>%
  mutate_if(is.character, as.numeric) %>%
  arrange(Date) %>% # to start element with earliest date, not latest  
  pull(Date) # creates an object instead of a data set

dates_13_3month <- infra_dimap_all %>%
  mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
  filter(Date >= "2013-06-29" &  Date <= "2013-09-21") %>%
  mutate_if(is.character, as.numeric) %>%
  arrange(Date) %>% # to start element with earliest date, not latest  
  pull(Date) # creates an object instead of a data set



dates_17_1month <- infra_dimap_all %>%
  mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
  filter(Date >=  "2017-08-26" &  Date <= "2017-09-23") %>%
  mutate_if(is.character, as.numeric) %>%
  arrange(Date) %>% # to start element with earliest date, not latest  
  pull(Date) # creates an object instead of a data set

dates_17_3month <- infra_dimap_all %>%
  mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
  filter(Date >=  "2017-07-01" &  Date <= "2017-09-23") %>%
  mutate_if(is.character, as.numeric) %>%
  arrange(Date) %>% # to start element with earliest date, not latest  
  pull(Date) # creates an object instead of a data set



dates_21_1month <- infra_dimap_all %>%
  mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
  filter(Date >= "2021-08-29" &  Date <= "2021-09-25") %>%
  mutate_if(is.character, as.numeric) %>%
  arrange(Date) %>% # to start element with earliest date, not latest  
  pull(Date) # creates an object instead of a data set

dates_21_3month <- infra_dimap_all %>%
  mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
  filter(Date >= "2021-07-04" &  Date <= "2021-09-25") %>%
  mutate_if(is.character, as.numeric) %>%
  arrange(Date) %>% # to start element with earliest date, not latest  
  pull(Date) # creates an object instead of a data set





# Create a list of previously created objects containing the survey dates to be looped through 
ts <- list(dates_09_1month, dates_09_3month,
           dates_13_1month, dates_13_3month,
           dates_17_1month, dates_17_3month,
           dates_21_1month, dates_21_3month)



# dates that mark the end of our research interval 
a <- c(as.Date("2009-09-26"))
b <- c(as.Date("2009-09-26"))
c <- c(as.Date("2013-09-21"))
d <- c(as.Date("2013-09-21"))

e <- c(as.Date("2017-09-23"))
f <- c(as.Date("2017-09-23"))
g <- c(as.Date("2021-09-25"))
h <- c(as.Date("2021-09-25"))




end_dates <- list(a,b,c,d,e,f,g,h)



# new infra_dimap_all data set needed since the column date in the infra_dimap_all data set is not formatted as.Date (only within dplyr code chunks)
# for missing values in surveys of the AFD in 2013, insert number 1 so that only the Google proportion is used in the weekly weighting model if no survey is available for weighting
infra_dimap_all_2 <- infra_dimap_all %>%
  mutate(Date = as.Date(Date, "%d.%m.%y"), AFD = ifelse(AFD == "-", 1, AFD)) 


# Create object with name for the data sets created in the loop
ww_names <- c("WW_09_1month", "WW_09_3month", 
              "WW_13_1month", "WW_13_3month",
              "WW_17_1month", "WW_17_3month",
              "WW_21_1month", "WW_21_3month")



# construct empty data set where data is saved in loop 
Model4_4 <- data.frame()





#### Loop 2 for data prep and calc of Model 4 ####

# use counter from loop 1

# perc is the relevant result column !!!
for(z in counter){
  name <- paste(z,".RData",sep="") 
  load(name)
  
# data prep as in the beginning of loop 1  

# 2009  
  
  df_09 <- trend_09$interest_over_time
  
  
  df_final09 <- df_09 %>%
    select(date, hits, keyword) %>%
    mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
    replace(is.na(.), 0) %>%
    mutate(keyword = gsub('CDU.*', 'CDU', gsub('SPD.*', 'SPD', gsub('Grüne.*', 'Grüne', 
                                                                    gsub('Linke.*', 'Linke', gsub('FDP.*', 'FDP', keyword))))))

# 2013
  
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

# 2017  
  
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
  
# 2021
  
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
  
  # cannot insert in dplyr code chunks updating data sets (df_final09, 13, 17...) thus all data sets are merged to one
  df_final <- rbind(df_final09, df_final13, df_final17, df_final21)
  
  # e is used to account for the different data structure of the 2009 datasets due to the non-existent AFD
  # if e <= 2 then datasets for 2009 are used (resulting datasets have 5 rows -> AFD doesn't exist), if e >= 3 datasets for 2013 -2021 are used (resulting datasets have 6 rows)
  e <- 0
  
  
  ## Model 4
  for (i in as.list(ts)){ 
    
    
    # update counter for "end_dates" list
    e <- e + 1
    
    
    if(e <= 2){
      
      # Search for the survey that is exactly BEFORE the first survey date from the survey dates that are within our interval (saved in objects of list ts)
      # (+1, since the infra_dimap_all_2 dataset starts with the year 2022 in descending order)
      t <- which(grepl(i[[1]], infra_dimap_all_2$Date))+1
      
      
      # checks whether loop does what we want it to do 
      print(infra_dimap_all_2$Date[t]-14)
      print(infra_dimap_all_2$Date[t])
      print(infra_dimap_all_2$Date[t])
      
      
      # get the Google proportion from 14 days before the survey, i.e. before the first survey in our objects, to the date of this survey (infra_dimap_all_2$Date[t])
      Google_Proportion_outside_beginning <- df_final %>%
        filter(date >= (infra_dimap_all_2$Date[t]-14) & date <= infra_dimap_all_2$Date[t]) %>%
        group_by(keyword) %>%
        summarize_at(vars(hits),list(mean = mean)) %>%
        mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, perc=0, iterator=e)
      
      
      # get poll data for the survey, i.e. before the first survey in our objects
      infra_dimap_first <- infra_dimap_all_2 %>%
        filter(Date == infra_dimap_all_2$Date[t]) %>% 
        mutate_if(is.character, as.numeric) %>%
        summarise(perc = c(mean(CDU), mean(FDP),
                           mean(Grüne), mean(Linke), mean(SPD))) %>%
        mutate(keyword = c("CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_09$perc_dev, iterator=5)
      
      
      # calculate weight
      Weighting_factor_beginning <- Google_Proportion_outside_beginning %>%
        mutate(weight_beginning = infra_dimap_first$perc/perc_old)
      
      
      print("finished_weight_beginning_09")
      
    }
    
    
    if(e >= 3){

      # Search for the survey that is exactly BEFORE the first survey date from the survey dates that are within our interval (saved in objects of list ts)
      # (+1, since the infra_dimap_all_2 dataset starts with the year 2022 in descending order)
      t <- which(grepl(i[[1]], infra_dimap_all_2$Date))+1
      
      
      # checks whether loop does what we want it to do 
      print(infra_dimap_all_2$Date[t]-14)
      print(infra_dimap_all_2$Date[t])
      print(infra_dimap_all_2$Date[t])
      
      
      # get the Google proportion from 14 days before the survey, i.e. before the first survey in our objects, to the date of this survey (infra_dimap_all_2$Date[t])
      Google_Proportion_outside_beginning <- df_final %>%
        filter(date >= (infra_dimap_all_2$Date[t]-14) & date <= infra_dimap_all_2$Date[t]) %>%
        group_by(keyword) %>%
        summarize_at(vars(hits),list(mean = mean)) %>%
        mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100, perc=0, iterator=e)
      
      
      # get poll data for the survey, i.e. before the first survey in our objects
      infra_dimap_first <- infra_dimap_all_2 %>%
        filter(Date == infra_dimap_all_2$Date[t]) %>% 
        mutate_if(is.character, as.numeric) %>%
        summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                           mean(Grüne), mean(Linke), mean(SPD))) %>%
        mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_13$perc_dev, iterator=5)
      
      
      # calculate weight
      Weighting_factor_beginning <- Google_Proportion_outside_beginning %>%
        mutate(weight_beginning = infra_dimap_first$perc/perc_old)
      
      
      print("finished_weight_beginning_13_17_21")  
      
      
    }
    
    
    # start nested loop to iterate through every date included in the objects (objects = dates_09_1month, dates_09_3month, etc.)
    for (j in 1:length(i)){
      
      if (j == 1 & e <= 2){ # needed because in 2009 AfD did not exist yet -> 2013,17,21 data sets have one row per date interval more 
        
        
        # checks whether loop does what we want it to do 
        print(infra_dimap_all_2$Date[t]+1)  
        print(i[[j]])  
        print(i[[j]])  
        print(infra_dimap_all_2$Date[t]+1)  
        print(i[[j]])  
        print(i[[j]]+1)
        print(i[[j+1]])
        
        
        # Apply previously calculated weight (Weighting_factor_beginning) to Google Proportion one day after survey date, i.e. before the first survey in our objects, to first survey date included in the objects
        Model4_4_beginning <-  df_final %>% 
          filter(date >= (infra_dimap_all_2$Date[t]+1) & date <= i[[j]]) %>%
          group_by(keyword)%>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 perc = Weighting_factor_beginning$weight_beginning*perc_old , iterator=0)
        
        
        # get the survey data for that is in first place in the objects
        infra_dimap_weekly_weighting <- infra_dimap_all_2 %>%
          filter(Date == i[[j]]) %>% 
          mutate_if(is.character, as.numeric) %>%
          summarise(perc = c(mean(CDU), mean(FDP),
                             mean(Grüne), mean(Linke), mean(SPD))) %>%
          mutate(keyword = c("CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_09$perc_dev, iterator=0)
        
        
        # calculate weight for period ranging from one day after survey that is before the first survey in our objects to first survey in objects and calculate weight by dividing the proportion by the survey date pulled in the previous code chunk
        Model4_3_first <-  df_final %>%
          filter(date >= (infra_dimap_all_2$Date[t]+1) & date <= i[[j]]) %>%
          group_by(keyword)%>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 weight_weekly = infra_dimap_weekly_weighting$perc/perc_old)
        
        
        # Get google proportion to be weighted i.e. one day after the first survey in our objects to the second survey date entry in our objects and apply the previously calculated weight
        Model4_4_first <- df_final %>%
          filter(date >= i[[j]]+1 & date <= i[[j+1]]) %>%
          group_by(keyword) %>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 perc= Model4_3_first$weight_weekly * perc_old, perc=round(perc, digits = 2), iterator=1)
        
        
        Model4_4 <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
        
        print("finished_applying_weight_beginning_and_weight_first_09")
        
      }
      
      
      if(j == 1 & e >= 3){ # needed because in 2009 AfD did not exist yet -> 2013,17,21 data sets have one row per date interval more 
        
        # checks whether loop does what we want it to do 
        print(infra_dimap_all_2$Date[t]+1)  
        print(i[[j]])  
        print(i[[j]])  
        print(infra_dimap_all_2$Date[t]+1)  
        print(i[[j]])  
        print(i[[j]]+1)
        print(i[[j+1]])
        
        
        # Apply previously calculated weight (Weighting_factor_beginning) to Google Proportion one day after survey date, i.e. before the first survey in our objects, to first survey date included in the objects
        Model4_4_beginning <-  df_final %>% 
          filter(date >= (infra_dimap_all_2$Date[t]+1) & date <= i[[j]]) %>%
          group_by(keyword)%>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 perc =  Weighting_factor_beginning$weight_beginning*perc_old , iterator=0)
        
        
        # get the survey data for that is in first place in the objects
        infra_dimap_weekly_weighting <- infra_dimap_all_2 %>%
          filter(Date == i[[j]]) %>% 
          mutate_if(is.character, as.numeric) %>%
          summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                             mean(Grüne), mean(Linke), mean(SPD))) %>%
          mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_13$perc_dev, iterator=0)
        
        
        # calculate weight for period ranging from one day after survey that is before the first survey in our objects to first survey in objects and calculate weight by dividing the proportion by the survey date pulled in the previous code chunk
        Model4_3_first <-  df_final %>%
          filter(date >= (infra_dimap_all_2$Date[t]+1) & date <= i[[j]]) %>%
          group_by(keyword)%>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 weight_weekly = infra_dimap_weekly_weighting$perc/perc_old)
        
        
        # Get google proportion to be weighted i.e. one day after the first survey in our objects to the second survey date entry in our objects and apply the previously calculated weight
        Model4_4_first <- df_final %>%
          filter(date >= i[[j]]+1 & date <= i[[j+1]]) %>%
          group_by(keyword) %>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 perc= Model4_3_first$weight_weekly * perc_old, perc=round(perc, digits = 2), iterator=1)
        
        
        Model4_4 <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
        
        print("finished_applying_weight_beginning_and_weight_first_13_17_21")
        
      }
      
      
      
      # if nested loop reaches latest iterator j within iterator i, loop comes to an end (list "end_dates" is used)
      if (j == max(1:length(i)) & e <= 2){
        
        
        # checks whether loop does what we want it to do 
        print(i[[j]])
        print(i[[j-1]]+1)
        print(i[[j]])
        print(i[[j]]+1)
        print(end_dates[[e]][[1]])
        
        
        # Pull survey data for the last survey date entry in the current object    
        infra_dimap_weekly_weighting <- infra_dimap_all_2 %>%
          filter(Date == i[[j]]) %>% 
          mutate_if(is.character, as.numeric) %>%
          summarise(perc = c(mean(CDU), mean(FDP),
                             mean(Grüne), mean(Linke), mean(SPD))) %>%
          mutate(keyword = c("CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_09$perc_dev, iterator=0)
        
        
        
        # Get Google Proportion to calculate weighting factor with the previously drawn survey data (go to previous date in the object +1 day to last date in object)
        Model4_2_ending <-  df_final %>%
          filter(date >= i[[j-1]]+1 & date <= i[[j]]) %>%
          group_by(keyword)%>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 weight_weekly = infra_dimap_weekly_weighting$perc/perc_old)
        
        
        # apply weight to data ranging from last date in object + 1 day to entry in list "end_dates" including the end of our setted intervals
        Model4_4_ending <-  df_final %>%
          filter(date >= i[[j]]+1 & date <= end_dates[[e]][[1]]) %>%
          group_by(keyword) %>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 perc= Model4_2_ending$weight_weekly * perc_old, perc=round(perc, digits = 2), iterator=19)
        
        
        Model4_4 <- rbind(Model4_4, Model4_4_ending)
        
        
        print("finished_ending_09")
        
        # break the current iteration of the nested loop if the condition above is fulfilled (last entry reached of object) so that the code below is not executed and a new iteration of the upper loop is started
        break
      }
      
      
      if (j == max(1:length(i)) & e >= 3){
        
        # checks whether loop does what we want it to do 
        print(i[[j]])
        print(i[[j-1]]+1)
        print(i[[j]])
        print(i[[j]]+1)
        print(end_dates[[e]][[1]])
        
        
        # Pull survey data for the last survey date entry in the current object    
        infra_dimap_weekly_weighting <- infra_dimap_all_2 %>%
          filter(Date == i[[j]]) %>% 
          mutate_if(is.character, as.numeric) %>%
          summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                             mean(Grüne), mean(Linke), mean(SPD))) %>%
          mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_13$perc_dev, iterator=0)
        
        
        
        # Get Google Proportion to calculate weighting factor with the previously drawn survey data (go to previous date in the object +1 day to last date in object)
        Model4_2_ending <-  df_final %>%
          filter(date >= i[[j-1]]+1 & date <= i[[j]]) %>%
          group_by(keyword)%>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 weight_weekly = infra_dimap_weekly_weighting$perc/perc_old)
        
        
        # apply weight to data ranging from last date in object + 1 day to entry in list "end_dates" including the end of our setted intervals
        Model4_4_ending <-  df_final %>%
          filter(date >= i[[j]]+1 & date <= end_dates[[e]][[1]]) %>%
          group_by(keyword) %>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 perc= Model4_2_ending$weight_weekly * perc_old, perc=round(perc, digits = 2), iterator=19)
        
        
        Model4_4 <- rbind(Model4_4, Model4_4_ending)
        
        
        print("finished_ending_13_17_21")
        
        # break the current iteration of the nested loop if the condition above is fulfilled (last entry reached of object) so that the code below is not executed and a new iteration of the upper loop is started
        break 
        
        
      }
      
      
      # conduct the same procedure for all iterations that lie between the second and the second last
      if (between(j, 2, max(1:length(i))-1) & e <= 2){
        
        
        # checks whether loop does what we want it to do 
        print(i[[j]])
        print(i[[j-1]]+1)
        print(i[[j]])
        print(i[[j]]+1)
        print(i[[j+1]])
        
        
        # get the survey data of the current survey date (j) 
        infra_dimap_weekly_weighting <- infra_dimap_all_2 %>%
          filter(Date == i[[j]]) %>% 
          mutate_if(is.character, as.numeric) %>%
          summarise(perc = c(mean(CDU), mean(FDP),
                             mean(Grüne), mean(Linke), mean(SPD))) %>%
          mutate(keyword = c("CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_09$perc_dev, iterator=0)
        
        
        # Calculate the weighting for the period from one day after the survey before the survey of the current iteration to the survey of the current iteration in our objects and calculate the weighting by dividing the proportion by the survey date drawn in the previous code chunk      Model4_2 <-  df_final %>%
        Model4_2 <-  df_final %>%
          filter(date >= i[[j-1]]+1 & date <= i[[j]]) %>%
          group_by(keyword)%>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 weight_weekly = infra_dimap_weekly_weighting$perc/perc_old)
        
        
        # Get google proportion to be weighted i.e. one day after the survey of the current iteration in our objects to the next survey date entry in our objects and apply the previously calculated weight
        Model4_3 <- df_final %>%
          filter(date >= i[[j]]+1 & date <= i[[j+1]]) %>%
          group_by(keyword) %>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 perc= Model4_2$weight_weekly * perc_old, perc=round(perc, digits = 2), iterator=j)
        
        Model4_4 <- rbind(Model4_4, Model4_3)
        
        
        
        print("finished_between_09")
        
      }
      
      if (between(j, 2, max(1:length(i))-1) & e >= 3){
        
        
        # checks whether loop does what we want it to do 
        print(i[[j]])
        print(i[[j-1]]+1)
        print(i[[j]])
        print(i[[j]]+1)
        print(i[[j+1]])
        
        
        # get the survey data of the current survey date (j) 
        infra_dimap_weekly_weighting <- infra_dimap_all_2 %>%
          filter(Date == i[[j]]) %>% 
          mutate_if(is.character, as.numeric) %>%
          summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                             mean(Grüne), mean(Linke), mean(SPD))) %>%
          mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, perc_dev=perc-election_results_13$perc_dev, iterator=0)
        
        
        # Calculate the weighting for the period from one day after the survey before the survey of the current iteration to the survey of the current iteration in our objects and calculate the weighting by dividing the proportion by the survey date drawn in the previous code chunk      Model4_2 <-  df_final %>%
        Model4_2 <-  df_final %>%
          filter(date >= i[[j-1]]+1 & date <= i[[j]]) %>%
          group_by(keyword)%>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 weight_weekly = infra_dimap_weekly_weighting$perc/perc_old)
        
        
        # Get google proportion to be weighted i.e. one day after the survey of the current iteration in our objects to the next survey date entry in our objects and apply the previously calculated weight
        Model4_3 <- df_final %>%
          filter(date >= i[[j]]+1 & date <= i[[j+1]]) %>%
          group_by(keyword) %>%
          summarize_at(vars(hits),list(mean = mean)) %>%
          mutate(summarized_mean = sum(mean), perc_old = (mean/summarized_mean)*100,
                 perc= Model4_2$weight_weekly * perc_old, perc=round(perc, digits = 2), iterator=j)
        
        Model4_4 <- rbind(Model4_4, Model4_3)
        
        
        
        print("finished_between_13_17_21")  
        
        
        
        
      }
      
    }
    
    # create for each i (= each object) in list "ts" a data set where all the data that was generated within the loop is written to
    assign(paste("Model_",ww_names[e], z, sep=""), Model4_4) 
    
    
    
    Model4_4 <- Model4_4 %>%
      mutate(perc = ifelse(perc == "Inf", 0, perc))
    
    if(e >= 1 & e <= 2){
      
      Model4_4 <- Model4_4 %>%
        group_by(keyword) %>%
        summarize_at(vars(perc), list(perc = mean)) %>%
        mutate(perc_dev = perc - election_results_09$perc_dev)
    }
    
    
    if(e >= 3 & e <= 4){
      
      Model4_4 <- Model4_4 %>%
        group_by(keyword) %>%
        summarize_at(vars(perc), list(perc = mean)) %>%
        mutate(perc_dev = perc - election_results_13$perc_dev)
    }
    
    
    if(e >= 5 & e <= 6){
      
      Model4_4 <- Model4_4 %>%
        group_by(keyword) %>%
        summarize_at(vars(perc), list(perc = mean)) %>%
        mutate(perc_dev = perc - election_results_17$perc_dev)
    }
    
    
    if(e >= 7 & e <= 8){
      
      Model4_4 <- Model4_4 %>%
        group_by(keyword) %>%
        summarize_at(vars(perc), list(perc = mean)) %>%
        mutate(perc_dev = perc - election_results_21$perc_dev)
    }
    
    
    assign(paste("full_model4_",ww_names[e], z, sep=""), Model4_4) 
    
    
    #reset data set (otherwise all models are written into one data set)
    Model4_4 <- data.frame()
    
    
  }
}





# combine several datasets to get full models 


combine <- c("full_model4_WW_09_1month", "full_model4_WW_09_3month",
             "full_model4_WW_13_1month", "full_model4_WW_13_3month",
             "full_model4_WW_17_1month", "full_model4_WW_17_3month",
             "full_model4_WW_21_1month", "full_model4_WW_21_3month")


names <- c("09_1month", "09_3month",
           "13_1month", "13_3month",
           "17_1month", "17_3month",
           "21_1month", "21_3month")


q = 0

for (v in combine){
  
  df <- data.frame()
  
  q = q+1
  
  lst <- mget(ls(pattern=paste(v)))
  
  df <- rbindlist(lst, fill = TRUE)
  
  assign(paste0("full_Model4_", names[q]), df)
  
  df <- df %>%
    group_by(keyword) %>%
    summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
    mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
           upper.ci = Mean + 1.96*(SD/sqrt(n())), 
           dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
           dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())), 
           model=4)
  
  
  
  assign(paste0("Plot_Model4_", names[q]), df)
  
}








#### Data sets containing mean over all used data sets in counter and confidence intervals (1 month) ####

# 2009

Plot_Model1_09_1month <- full_Model1_09_1month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 1)


Plot_Model2_09_1month <- full_Model2_09_1month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 2)


Plot_Model3_09_1month <- full_Model3_09_1month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 3)

Plot_Model4_09_1month

# 2013

Plot_Model1_13_1month <- full_Model1_13_1month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 1)


Plot_Model2_13_1month <- full_Model2_13_1month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 2)


Plot_Model3_13_1month <- full_Model3_13_1month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 3)

Plot_Model4_13_1month

# 2017

Plot_Model1_17_1month <- full_Model1_17_1month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 1)


Plot_Model2_17_1month <- full_Model2_17_1month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 2)

Plot_Model3_17_1month <- full_Model3_17_1month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 3)

Plot_Model4_17_1month


#2021

Plot_Model1_21_1month <- full_Model1_21_1month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 1)

Plot_Model2_21_1month <- full_Model2_21_1month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 2)

Plot_Model3_21_1month <- full_Model3_21_1month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 3)

Plot_Model4_21_1month


#### Data sets containing mean over all used data sets in counter and confidence intervals (3 month) ####

# 2009

Plot_Model1_09_3month <- full_Model1_09_3month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())), 
         model=1)


Plot_Model2_09_3month <- full_Model2_09_3month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())), 
         model=2)


Plot_Model3_09_3month <- full_Model3_09_3month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())), 
         model=3)

Plot_Model4_09_3month


# 2013

Plot_Model1_13_3month <- full_Model1_13_3month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())), 
         model=1)


Plot_Model2_13_3month <- full_Model2_13_3month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())), 
         model=2)


Plot_Model3_13_3month <- full_Model3_13_3month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())), 
         model=3)

Plot_Model4_13_3month


# 2017

Plot_Model1_17_3month <- full_Model1_17_3month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())), 
         model=1)


Plot_Model2_17_3month <- full_Model2_17_3month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())), 
         model=2)

Plot_Model3_17_3month <- full_Model3_17_3month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())), 
         model=3)

Plot_Model4_17_3month


# 2021

Plot_Model1_21_3month <- full_Model1_21_3month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 1)

Plot_Model2_21_3month <- full_Model2_21_3month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 2)

Plot_Model3_21_3month <- full_Model3_21_3month %>%
  group_by(keyword) %>%
  summarize(Mean = mean(perc), SD = sd(perc), perc_dev = mean(perc_dev)) %>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n())), 
         dev.low.ci = perc_dev - 1.96*(SD/sqrt(n())), 
         dev.up.ci = perc_dev + 1.96*(SD/sqrt(n())),
         model = 3)

Plot_Model4_21_3month



#### Data sets for plot of deviations of different models (1 month) #### 


# Functioning of code below:
# the point "." is a filler instead of inserting the full data set name
# basis function in mutate row: rep(1:5, each=5) -> assign 5 rows number 1, then the next 5 rows 2,.... stopping when 5 is reached
# logic here in mutate row: divide total number of rows in data set by number of how often one party appears in data set, to get the interval for how high it should be numbered
# total number of one party indicates how many data sets were used



# 2009

All_Model1_09_1month <- full_Model1_09_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model2_09_1month <- full_Model2_09_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model3_09_1month <- full_Model3_09_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model4_09_1month <- full_Model4_09_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))

# 2013

All_Model1_13_1month <- full_Model1_13_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model2_13_1month <- full_Model2_13_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model3_13_1month <- full_Model3_13_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model4_13_1month <- full_Model4_13_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))

#2017

All_Model1_17_1month <- full_Model1_17_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model2_17_1month <- full_Model2_17_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model3_17_1month <- full_Model3_17_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model4_17_1month <- full_Model4_17_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))

# 2021

All_Model1_21_1month <- full_Model1_21_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model2_21_1month <- full_Model2_21_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model3_21_1month <- full_Model3_21_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model4_21_1month <- full_Model4_21_1month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))

#### Data sets for plot of deviations of different models (3 month) #### 

# 2009

All_Model1_09_3month <- full_Model1_09_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>% # ID = Identifier for data set
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model2_09_3month <- full_Model2_09_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model3_09_3month <- full_Model3_09_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model4_09_3month <- full_Model4_09_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))

#2013
All_Model1_13_3month <- full_Model1_13_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model2_13_3month <- full_Model2_13_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model3_13_3month <- full_Model3_13_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model4_13_3month <- full_Model4_13_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))

#2017
All_Model1_17_3month <- full_Model1_17_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model2_17_3month <- full_Model2_17_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model3_17_3month <- full_Model3_17_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model4_17_3month <- full_Model4_17_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))

# 2021

All_Model1_21_3month <- full_Model1_21_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model2_21_3month <- full_Model2_21_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model3_21_3month <- full_Model3_21_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))


All_Model4_21_3month <- full_Model4_21_3month %>%
  mutate(ID = rep(1:(nrow(.[.$keyword == "CDU",])), each = nrow(.)/nrow(.[.$keyword == "CDU",]))) %>%
  group_by(ID) %>%
  summarize(Mean = mean(abs(perc_dev))) %>%
  ungroup() %>%
  mutate(All_Mean = mean(Mean),SD = sd(Mean),
         lower.ci = All_Mean - 1.96*(SD/sqrt(n())),
         upper.ci = All_Mean + 1.96*(SD/sqrt(n())))



#### Plots for every model in every election year ####


##### 1month
#plots 2009

ggplot(data=Plot_Model1_09_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model2_09_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model3_09_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model4_09_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

#plots 2013

ggplot(data=Plot_Model1_13_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model2_13_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model3_13_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model4_13_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

#plots 2017

ggplot(data=Plot_Model1_17_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model2_17_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model3_17_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model4_17_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")


#plots 2021

ggplot(data=Plot_Model1_21_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model2_21_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model3_21_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model4_21_1month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

########### 3month
#plots 2009

ggplot(data=Plot_Model1_09_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model2_09_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model3_09_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model4_09_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

#plots 2013

ggplot(data=Plot_Model1_13_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model2_13_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model3_13_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model4_13_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

#plots 2017

ggplot(data=Plot_Model1_17_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model2_17_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model3_17_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model4_17_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")


#plots 2021

ggplot(data=Plot_Model1_21_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model2_21_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model3_21_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")

ggplot(data=Plot_Model4_21_3month, aes(x=keyword, y=Mean, fill=keyword))+
  geom_bar(stat="identity") +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1)), vjust=-2) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  theme(axis.title.x = element_blank()) +
  labs(y="Percentage")




#### Figure 5 plot ####


data_figure5 <- plyr::rbind.fill(Plot_Model1_13_1month, 
                            Plot_Model2_13_1month, 
                            Plot_Model3_13_1month, 
                            Plot_Model4_13_1month,
                            infra_dimap_13_b_1month,
                            election_results_13)


ggplot(data=data_figure5, aes(x=factor(model), y=perc_dev, fill=keyword, group=as.factor(ordered(keyword, levels = c("Grüne", "Linke", "FDP", "AFD", "SPD", "CDU")))))+
  geom_bar(stat="identity", width= 0.9, position=position_dodge(0.9)) +
  scale_x_discrete(labels = c("Model 1 (Google data)", "Model 2 (weighted elect. results)", "Model 3 (weighted poll)", "Model 4 (weekly weighted)", "Average polls", "Final election results 2013")) +
  scale_y_continuous(breaks= c(-15, -10, -5, 0,5,10,15,20,25,30,35,40,45), limits = c(-15,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(perc_dev, digits = 1), y=round(perc_dev, digits = 1)+ifelse(round(perc_dev, digits = 1) >=0, 3, -3)), position=position_dodge(width=.9)) +
  theme(axis.title.x = element_blank()) +
  geom_errorbar(aes(ymin=dev.low.ci, ymax=dev.up.ci),width=.3, position=position_dodge(.9)) +
  labs(y="Percentage")

# Fig5: Alternative ####
plot1 <- data_figure5 %>% 
      filter(model==6) %>%
  ggplot(aes(x=factor(model), y=perc_dev, fill=keyword, group=as.factor(ordered(keyword, levels = c("Grüne", "Linke", "FDP", "AFD", "SPD", "CDU")))))+
  geom_bar(stat="identity", width= 0.9, position=position_dodge(0.9)) +
  scale_x_discrete(labels = c("Final election results 2013")) +
  scale_y_continuous(limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(perc_dev, digits = 1), y=round(perc_dev, digits = 1)+ifelse(round(perc_dev, digits = 1) >=0, 3, -3)), position=position_dodge(width=.9)) +
  theme(axis.title.x = element_blank()) +
  geom_errorbar(aes(ymin=dev.low.ci, ymax=dev.up.ci),width=.3, position=position_dodge(.9)) +
  labs(y="Percentage", x="") +
  theme_minimal()

plot2 <- data_figure5 %>% 
  filter(model!=6) %>%
  ggplot(aes(x=factor(model), y=perc_dev, fill=keyword, group=as.factor(ordered(keyword, levels = c("Grüne", "Linke", "FDP", "AFD", "SPD", "CDU")))))+
  geom_bar(stat="identity", width= 0.9, position=position_dodge(0.9)) +
  scale_x_discrete(labels = c("Model 1\n(Google data)", "Model 2\n(weighted elect. results)", "Model 3\n(weighted poll)", "Model 4\n(weekly weighted)", "Average polls")) +
  scale_y_continuous(limits = c(-15,15)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(perc_dev, digits = 1), y=round(perc_dev, digits = 1)+ifelse(round(perc_dev, digits = 1) >=0, 3, -3)), position=position_dodge(width=.9)) +
  theme(axis.title.x = element_blank()) +
  geom_errorbar(aes(ymin=dev.low.ci, ymax=dev.up.ci),width=.3, position=position_dodge(.9)) +
  labs(y="Percentage", x="") +
  theme_minimal()



figure5 <- plot1 + plot2 +
  plot_layout(ncol = 2) + plot_layout(widths = c(1, 3),
                                      guides = "collect")

figure5 <- figure5 + plot_annotation(
  title = 'Figure 5: Deviations of the one-month models from the ﬁnal election results in 2013',
  theme = theme(plot.title = element_text(size = 20),
                plot.subtitle = element_text(size = 16))
)

ggsave(filename = "fig5.png",
       plot = figure5,
       width = 9,
       height =4.5,
       units = "in",
       dpi = 300)

figure1



#### Overview plot with all models for 2021, avg of polls before election and poll 1 week before election ####

infra_dimap_21_3month1609 <- infra_dimap_all %>%
  mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
  filter(Date >= "2021-09-16" &  Date <= "2021-09-16") %>%
  mutate_if(is.character, as.numeric) %>%
  summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                     mean(Grüne), mean(Linke), mean(SPD))) %>%
  mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"), mean=0, summarized_mean=0, model=6)

infra_dimap_21_b_3month_plot <- infra_dimap_21_b_3month %>%
                                mutate(Mean = perc)


colnames(infra_dimap_21_3month1609) <-  c("Mean", "keyword","mean","summarized_mean","model")

df_21_3month <- plyr::rbind.fill(Plot_Model1_21_3month, 
                                 Plot_Model2_21_3month, 
                                 Plot_Model3_21_3month, 
                                 Plot_Model4_21_3month, 
                                 infra_dimap_21_b_3month_plot, 
                                 infra_dimap_21_3month1609)



ggplot(data=df_21_3month, aes(x=factor(model), y=Mean, fill=keyword, group=as.factor(ordered(keyword, levels = c("Grüne", "Linke", "FDP", "AFD", "SPD", "CDU")))))+
  geom_bar(stat="identity", width= 0.9, position=position_dodge(0.9)) +
  scale_x_discrete(labels = c("Model 1 (Google data)", "Model 2 (weighted elect. results)", "Model 3 (weighted poll)", "Model 4 (ww)", "Average polls", "Last poll before election")) +
  scale_y_continuous(breaks= c(0,5,10,15,20,25,30,35, 40, 45), limits = c(0,45)) +
  scale_fill_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red"), name = "Political party", breaks = c("CDU","SPD","AFD","FDP", "Linke", "Grüne" )) +
  geom_text(aes(label=round(Mean, digits = 1), y=round(Mean, digits = 1)+ifelse(round(Mean, digits = 1) >=0, 0.8, -0.8)), position=position_dodge(width=.9), vjust = -0.8) +
  theme(axis.title.x = element_blank()) +
  geom_errorbar(aes(ymin=lower.ci, ymax=upper.ci),width=.3, position=position_dodge(.9)) +
  labs(y="Percentage")





#### Deviation tables ####

# abs = Absolute Values !!!!!!!!!!!!!!

# 1 month models

All_Model1_09_1month

labels <- data.frame(Year = c("Mean Deviation 2009", "Mean Deviation 2013", "Mean Deviation 2017", "Mean Deviation 2021"))

dev_09 <- cbind(Model1 = round(All_Model1_09_1month[1,3],2), 
                Model2 = round(All_Model2_09_1month[1,3],2),
                Model3 = round(All_Model3_09_1month[1,3],2), 
                Model4 = round(All_Model4_09_1month[1,3],2), 
                Polling = round(mean(abs(infra_dimap_09_b_1month$perc_dev)),2))

dev_13 <- cbind(Model1 = round(All_Model1_13_1month[1,3],2), 
                Model2 = round(All_Model2_13_1month[1,3],2),
                Model3 = round(All_Model3_13_1month[1,3],2), 
                Model4 = round(All_Model4_13_1month[1,3],2), 
                Polling = round(mean(abs(infra_dimap_13_b_1month$perc_dev)),2))

dev_17 <- cbind(Model1 = round(All_Model1_17_1month[1,3],2), 
                Model2 = round(All_Model2_17_1month[1,3],2),
                Model3 = round(All_Model3_17_1month[1,3],2), 
                Model4 = round(All_Model4_17_1month[1,3],2), 
                Polling = round(mean(abs(infra_dimap_17_b_1month$perc_dev)),2))

dev_21 <- cbind(Model1 = round(All_Model1_21_1month[1,3],2), 
                Model2 = round(All_Model2_21_1month[1,3],2),
                Model3 = round(All_Model3_21_1month[1,3],2), 
                Model4 = round(All_Model4_21_1month[1,3],2), 
                Polling = round(mean(abs(infra_dimap_21_b_1month$perc_dev)),2))



dev_Compl_1month <- cbind(labels,rbind(dev_09,dev_13, dev_17, dev_21))
colnames(dev_Compl_1month) <- c("Year","Model1","Model2","Model3","Model4","Polling")
dev_Compl_1month




# 3 month models

All_Model1_09_3month

labels <- data.frame(Year = c("Mean Deviation 2009", "Mean Deviation 2013", "Mean Deviation 2017", "Mean Deviation 2021"))

dev_09 <- cbind(Model1 = round(All_Model1_09_3month[1,3],2), 
                Model2 = round(All_Model2_09_3month[1,3],2),
                Model3 = round(All_Model3_09_3month[1,3],2),
                Model4 = round(All_Model4_09_3month[1,3],2), 
                Polling = round(mean(abs(infra_dimap_09_b_3month$perc_dev)),2))

dev_13 <- cbind(Model1 = round(All_Model1_13_3month[1,3],2), 
                Model2 = round(All_Model2_13_3month[1,3],2),
                Model3 = round(All_Model3_13_3month[1,3],2), 
                Model4 = round(All_Model4_13_3month[1,3],2), 
                Polling = round(mean(abs(infra_dimap_13_b_3month$perc_dev)),2))

dev_17 <- cbind(Model1 = round(All_Model1_17_3month[1,3],2), 
                Model2 = round(All_Model2_17_3month[1,3],2),
                Model3 = round(All_Model3_17_3month[1,3],2), 
                Model4 = round(All_Model4_17_3month[1,3],2), 
                Polling = round(mean(abs(infra_dimap_17_b_3month$perc_dev)),2))

dev_21 <- cbind(Model1 = round(All_Model1_21_3month[1,3],2), 
                Model2 = round(All_Model2_21_3month[1,3],2),
                Model3 = round(All_Model3_21_3month[1,3],2), 
                Model4 = round(All_Model4_21_3month[1,3],2), 
                Polling = round(mean(abs(infra_dimap_21_b_3month$perc_dev)),2))



dev_Compl_3month <- cbind(labels,rbind(dev_09,dev_13, dev_17, dev_21))

colnames(dev_Compl_3month) <- c("Year","Model1","Model2","Model3","Model4","Polling")

dev_Compl_3month



# combining created deviation tables of 1month and 3 month period models
DevComplPlot <- rbind(dev_Compl_1month, dev_Compl_3month) 

DevComplPlot <- DevComplPlot %>%
  mutate(Year = c(2009,2013,2017,2021,2009,2013,2017,2021), 
         Length = c("1month","1month","1month","1month","3month","3month","3month","3month")) %>% #before:rb21 models included
  pivot_longer(cols = starts_with(c("M","P")), 
               names_to = "Model", 
               values_to = "Deviation") %>%
  mutate(Deviation = as.numeric(Deviation), 
         Year = as.factor(Year), 
         Length = as.factor(Length))



#### Deviation plot Figure 9 ####
pd2 <- position_dodge(0.5)

ggplot(DevComplPlot, aes(x = factor(Year), y=Deviation, group = Length, colour= Length, shape= Model))+
  geom_point(size = 3, stroke =2, position=pd2) + xlab("") + ylab("Absolute average deviation from election results") + # stroke defines border size
  scale_y_continuous(breaks= c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10), limits = c(0,6.5)) +
  scale_shape_manual(values=c(21,23,24,22,16), labels = c("Google Prop.", "Weighted prev. elect. results", "Weighted poll", "Weekly weighted poll" ,"Average polls")) + #manual symbols in plot
  labs(shape="Model", colour="Duration") + 
  guides(shape = guide_legend(override.aes = list(shape=c(21,23,24,22,16), size = 3), order = 1)) + # manual symbols in legend / order = reorder legends
  guides(colour = guide_legend(override.aes = list(shape = 15)), order=2) + #changes first legend
  theme(legend.key = element_rect(fill = "transparent"))





#### New model 3 (Paul) ####

# add avg of Google Proportion and avg of polling results and take mean of it 

# 2009
New_Model_3 <- c(Plot_Model1_09_1month$Mean, infra_dimap_09_b_1month$perc)

New_Model_3_09 <- data.frame(New_Model_3)

colnames(New_Model_3_09) <- "Sum"

New_Model_3_09_1month <- New_Model_3_09 %>%
                            mutate(keyword = c("CDU", "FDP", "Grüne", "Linke", "SPD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
                            group_by(keyword) %>%
                            summarise(Mean = mean(Sum)) %>%
                            mutate(perc_dev=Mean-election_results_09$perc_dev)



New_Model_3 <- c(Plot_Model1_09_3month$Mean, infra_dimap_09_b_3month$perc)

New_Model_3_09 <- data.frame(New_Model_3)

colnames(New_Model_3_09) <- "Sum"

New_Model_3_09_3month <- New_Model_3_09 %>%
  mutate(keyword = c("CDU", "FDP", "Grüne", "Linke", "SPD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
  group_by(keyword) %>%
  summarise(Mean = mean(Sum)) %>%
  mutate(perc_dev=Mean-election_results_09$perc_dev)

# 2013

New_Model_3 <- c(Plot_Model1_13_1month$Mean, infra_dimap_13_b_1month$perc)

New_Model_3_13 <- data.frame(New_Model_3)

colnames(New_Model_3_13) <- "Sum"

New_Model_3_13_1month <- New_Model_3_13 %>%
  mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
  group_by(keyword) %>%
  summarise(Mean = mean(Sum)) %>%
  mutate(perc_dev=Mean-election_results_13$perc_dev)



New_Model_3 <- c(Plot_Model1_13_3month$Mean, infra_dimap_13_b_3month$perc)

New_Model_3_13 <- data.frame(New_Model_3)

colnames(New_Model_3_13) <- "Sum"

New_Model_3_13_3month <- New_Model_3_13 %>%
  mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
  group_by(keyword) %>%
  summarise(Mean = mean(Sum)) %>%
  mutate(perc_dev=Mean-election_results_13$perc_dev)

# 2017

New_Model_3 <- c(Plot_Model1_17_1month$Mean, infra_dimap_17_b_1month$perc)

New_Model_3_17 <- data.frame(New_Model_3)

colnames(New_Model_3_17) <- "Sum"

New_Model_3_17_1month <- New_Model_3_17 %>%
  mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
  group_by(keyword) %>%
  summarise(Mean = mean(Sum)) %>%
  mutate(perc_dev=Mean-election_results_17$perc_dev)



New_Model_3 <- c(Plot_Model1_17_3month$Mean, infra_dimap_17_b_3month$perc)

New_Model_3_17 <- data.frame(New_Model_3)

colnames(New_Model_3_17) <- "Sum"

New_Model_3_17_3month <- New_Model_3_17 %>%
  mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
  group_by(keyword) %>%
  summarise(Mean = mean(Sum)) %>%
  mutate(perc_dev=Mean-election_results_17$perc_dev)

# 2021

New_Model_3 <- c(Plot_Model1_21_1month$Mean, infra_dimap_21_b_1month$perc)

New_Model_3_21 <- data.frame(New_Model_3)

colnames(New_Model_3_21) <- "Sum"

New_Model_3_21_1month <- New_Model_3_21 %>%
  mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
  group_by(keyword) %>%
  summarise(Mean = mean(Sum)) %>%
  mutate(perc_dev=Mean-election_results_21$perc_dev)



New_Model_3 <- c(Plot_Model1_21_3month$Mean, infra_dimap_21_b_3month$perc)

New_Model_3_21 <- data.frame(New_Model_3)

colnames(New_Model_3_21) <- "Sum"

New_Model_3_21_3month <- New_Model_3_21 %>%
  mutate(keyword = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD", "AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
  group_by(keyword) %>%
  summarise(Mean = mean(Sum)) %>%
  mutate(perc_dev=Mean-election_results_21$perc_dev)


