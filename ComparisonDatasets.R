library(gtrendsR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rvest)
library(xml2)


dir <- setwd("C:/Users/deanl/Desktop/UniMannheim/ComSocScience/TermPaper/RMarkdown/DatensätzeVergleich2")

df <- list.files(dir)


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

  
  
  assign(name, Model1_09_1)

}



#full_Model1_09_1 <- data.frame()
#full_Model1_09_1 <- rbind(full_Model1_09_1, Model1_09_1[c("keyword","perc","data")])


identical(`2021-09-14 22-28-16.RData`,`2021-09-15 10-28-13.RData`)
identical(`2022-07-14 00-13-08.RData`,`2022-07-14 01-13-10.RData`)


dir <- setwd("C:/Users/deanl/Desktop/UniMannheim/ComSocScience/TermPaper/RMarkdown/DatensätzeVergleich2")

df <- list.files(dir)

#load("2021-09-14 22-28-16.RData")



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
  
  name <- paste0("Model1_09_",i)
  
  assign(name,Model1_09_1)
  
  i= i+1
}




identical(Model1_09_7, Model1_09_6)

a <- noquote(paste0("Model1_09_",i))

identical(noquote(paste0("Model1_09_",i)), noquote(paste0("Model1_09_",i-1)))

#


dir <- setwd("C:/Users/deanl/Desktop/UniMannheim/ComSocScience/TermPaper/RMarkdown/DatensätzeVergleich")

df <- list.files(dir)

#load("2021-09-14 22-28-16.RData")


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

for(i in 2:length(List)){

if(identical(List[[i]], List[[i-1]]) == TRUE){
  print(paste("gleich",names(List[i])))
} else{
  print(paste("ungleich",names(List[i])))
  }
}

