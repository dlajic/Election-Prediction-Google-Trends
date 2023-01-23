# File is used to compare GT datasets and extract subset
# of datasets that are not identical (both with and without category)

library(pacman)

p_load(gtrendsR,
       ggplot2,
       dplyr,
       tidyr,
       rvest,
       xml2,
       stringr,
       tidyverse)


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


# Datasets with category ####
#all Datsets
dir <- setwd("C:/Users/janbe/Desktop/Uni/02_Master Sociology/FSS 2021/Research Methods/Publikation/Election-Prediction-Google-Trends/Data")
df_names <- list.files(dir, full.names = FALSE)
df <- list.files(dir, full.names = TRUE)



years <- c("_05", "_09", "_13", "_17", "_21")
years2 <- c("2005", "2009", "2013", "2017", "2021")


for (y in 1:length(df)){ # 
  #y <- 1

  load(df[y])
  
  cnt <- 0
  
  for (t in years){
  
  objects <- ls()[str_detect(ls(), t)]
  
  length <- 1:length(objects)
  
  cnt <- cnt +1
  
  test <- data.frame()
    
  
  if(max(length) == 2){
    
  test <- rbind(get(objects[1])$interest_over_time, 
                          anti_join(get(objects[2])$interest_over_time, get(objects[1])$interest_over_time, by = "keyword"))
  
  test <- test %>%
    mutate(hits = as.numeric(hits), date = as.Date(date), keyword = replace_searchterms(keyword)) %>%
    replace(is.na(.), 0) %>%
    select(date, hits, keyword)
  
  assign(paste0("df", years2[cnt], "_", filename), test)
  
  }
  
  if(max(length) > 2){
    
    mem <- rbind(get(objects[1])$interest_over_time, 
                     anti_join(get(objects[2])$interest_over_time, get(objects[1])$interest_over_time, by = "keyword"))
    
    test <- rbind(mem, anti_join(get(objects[3])$interest_over_time, get(objects[1])$interest_over_time, by = "keyword"))
    
    test <- test %>%
      mutate(hits = as.numeric(hits), date = as.Date(date), keyword = replace_searchterms(keyword)) %>%
      replace(is.na(.), 0) %>%
      select(date, hits, keyword)
    
    assign(paste0("df", years2[cnt], "_", filename), test)
    
    
    
    }
  }
}
  
  

# pick all datasets for 2021
names <- ls()[str_detect(ls(), "df2021")]

test2 <- rbind(get(names[1]), get(names[2]), get(names[3]), get(names[4]), get(names[5]),
      get(names[6]), get(names[7]), get(names[8]), get(names[9]), get(names[10]))


# compute mean, sd, ci intervals over ten datasets
plot21 <- test2 %>%
  group_by(date, keyword) %>%
  summarize(Mean = mean(hits), SD = sd(hits))%>%
  mutate(lower.ci = Mean - 1.96*(SD/sqrt(n())),
         upper.ci = Mean + 1.96*(SD/sqrt(n()))) %>%
  mutate(keyword = as.factor(keyword))



plot21 %>%
  filter(date > "2021-07-01") %>%
  ggplot(aes(x = date, y = Mean, group = keyword, color = keyword)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci,  fill = keyword), alpha = 0.1) +
  scale_color_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow1","Grüne" = "green3", "Linke" = "purple", "SPD" = "red", "Sonstige" = "grey")) +
  facet_grid(facets = vars(keyword), scales = "free") + coord_cartesian(ylim = c(0,25)) +
  ggtitle("Google Trends Sample Error 2021") +
  labs(y = "Mean of hits")


plot21 %>%
  filter(date > "2021-06-01") %>%
  ggplot(aes(x = date, y = Mean, group = keyword, color = keyword)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci,  fill = keyword), alpha = 0.4) +
  scale_color_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow1","Grüne" = "green3", "Linke" = "purple", "SPD" = "red", "Sonstige" = "grey")) +
  facet_wrap(facets = vars(keyword), scales = "free") + #coord_cartesian(ylim = c(0,25)) +
  ggtitle("Google Trends Sample Error 2021") +
  labs(y = "Mean of hits")
