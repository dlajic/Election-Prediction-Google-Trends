library(gtrendsR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rvest)
library(xml2)


#all Datsets
dir <- setwd("C:/Users/deanl/Desktop/UniMannheim/ComSocScience/TermPaper/RMarkdown/Data")
df <- list.files(dir)

#creating a list where the results of Model1_09 of every Dataset is stored, to compare these
List <- list()
i<-1
for (name in df){
  
  load(name)
  
  df_09 <- trend_09$interest_over_time
  
  df_final09 <- df_09 %>%
    select(date, hits, keyword) %>%
    mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
    replace(is.na(.), 0) %>%
    mutate(keyword = gsub('CDU.*', 'CDU', gsub('SPD.*', 'SPD', gsub('Grüne.*', 'Grüne', 
                                                                    gsub('Linke.*', 'Linke', gsub('FDP.*', 'FDP', keyword))))))
  
  Model1_09_1 <-  df_final09 %>%
    filter(date >= "2009-09-19" &  date <= "2009-09-26")%>%
    group_by(keyword)%>%
    summarize_at(vars(hits),list(mean = mean)) %>%
    mutate(summarized_mean = sum(mean), perc = (mean/summarized_mean)*100, perc=round(perc, digits = 2))
  
  List[[name]] <- Model1_09_1
  
  i= i+1
}

#Compare if the results od the Datsets are the same
for(i in 2:length(List)){

if(identical(List[[i]], List[[i-1]]) == TRUE){
  print(paste("equal",names(List[i])))
} else{
  print(paste("unequal",names(List[i])))
  }
}

