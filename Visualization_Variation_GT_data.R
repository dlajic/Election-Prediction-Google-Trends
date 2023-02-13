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
dir <- setwd("./Data")
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
  
  mem <- data.frame()
    
  # checks if a dataset for 2009 is loaded (only two objects in it, because AfD did not exist yet)
  if(max(length) == 2){
    
  # bind objects together "trend_CDU_09" and "trend_sonst_09" with anti_join together -> joins only the part in "trend_sonst_09" that is different from "trend_CDU_09"  
  mem <- rbind(get(objects[1])$interest_over_time, 
                          anti_join(get(objects[2])$interest_over_time, get(objects[1])$interest_over_time, by = "keyword"))
  
  mem <- mem %>%
    mutate(hits = as.numeric(hits), date = as.Date(date), keyword = replace_searchterms(keyword)) %>%
    replace(is.na(.), 0) %>%
    select(date, hits, keyword)
  
  assign(paste0("df", years2[cnt], "_", filename), mem)
  
  }
  
  # checks if a dataset for 2013/17/21 is loaded
  if(max(length) > 2){
    
    mem <- rbind(get(objects[1])$interest_over_time, 
                     anti_join(get(objects[2])$interest_over_time, get(objects[1])$interest_over_time, by = "keyword"))
    
    mem <- rbind(mem, anti_join(get(objects[3])$interest_over_time, get(objects[1])$interest_over_time, by = "keyword"))
    
    mem <- mem %>%
      mutate(hits = as.numeric(hits), date = as.Date(date), keyword = replace_searchterms(keyword)) %>%
      replace(is.na(.), 0) %>%
      select(date, hits, keyword)
    
    assign(paste0("df", years2[cnt], "_", filename), mem)
    
    
    
    }
  }
}
  
  

# pick all datasets for 2021
names <- ls()[str_detect(ls(), "df2021")]

mem2 <- rbind(get(names[1]), get(names[2]), get(names[3]), get(names[4]), get(names[5]),
      get(names[6]), get(names[7]), get(names[8]), get(names[9]), get(names[10]))


# compute mean, sd, ci intervals over ten datasets for hits and Gprop
plot21 <- mem2 %>%
  rename(party=keyword) %>%
  group_by(date, party) %>%
  summarize(Mean_hits = mean(hits), hits_sum = sum(hits), SD = sd(hits)) %>% # Same as before but diff. code
  mutate(Gprop = ifelse(hits_sum >= 1, hits_sum/sum(hits_sum)*100, 0)) %>%
  mutate(Mean_hits_lower.ci = Mean_hits - 1.96*(SD/sqrt(n())),
         Mean_hits_upper.ci = Mean_hits + 1.96*(SD/sqrt(n())),
         Gprop_lower.ci =  Gprop - 1.96*(SD/sqrt(n())),
         Gprop_upper.ci =  Gprop + 1.96*(SD/sqrt(n()))) %>%
  mutate(party = as.factor(party)) %>%
  filter(party != "Sonstige")


# hits (grid)
grid_hits <-  plot21 %>%
            filter(date > "2021-08-01") %>%
            ggplot(aes(x = date, y = Mean_hits, group = party, color = party)) +
            geom_line(size = 1) +
            geom_ribbon(aes(ymin = Mean_hits_lower.ci, ymax = Mean_hits_upper.ci,  fill = party), alpha = 0.4) +
            scale_color_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red")) +
            facet_grid(facets = vars(party), scales = "free") + #coord_cartesian(ylim = c(0,25)) +
            ggtitle("Google Trends Sample Error 2021") +
            labs(y = "Mean of hits")

grid_hits

# gprop (grid)
grid_gprop <- plot21 %>%
            filter(date > "2021-08-01") %>%
            ggplot(aes(x = date, y = Gprop, group = party, color = party)) +
            geom_line(size = 1) +
            geom_ribbon(aes(ymin = Gprop_lower.ci, ymax = Gprop_upper.ci,  fill = party), alpha = 0.1) +
            scale_color_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red")) +
            facet_grid(facets = vars(party), scales = "free") + #coord_cartesian(ylim = c(0,25)) +
            ggtitle("Google Trends Sample Error 2021") +
            labs(y = "Mean of Gprop")

grid_gprop

# hits (wrap)
wrap_hits <- plot21 %>% 
            filter(date > "2021-08-01") %>%
            ggplot(aes(x = date, y = Mean_hits, group = party, color = party)) +
            geom_line(size = 1) +
            geom_ribbon(aes(ymin = Mean_hits_lower.ci, ymax = Mean_hits_upper.ci,  fill = party), alpha = 0.4) +
            scale_color_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red")) +
            facet_wrap(facets = vars(party), scales = "free") + #coord_cartesian(ylim = c(0,25)) +
            ggtitle("Google Trends Sample Error 2021") +
            labs(y = "Mean of hits")

wrap_hits

# gprop (wrap)
wrap_gprop <- plot21 %>% 
            filter(date > "2021-08-01") %>%
            ggplot(aes(x = date, y = Gprop, group = party, color = party)) +
            geom_line(size = 1) +
            geom_ribbon(aes(ymin = Gprop_lower.ci, ymax = Gprop_upper.ci,  fill = party), alpha = 0.4) +
            scale_color_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red")) +
            facet_wrap(facets = vars(party), scales = "free") + #coord_cartesian(ylim = c(0,25)) +
            ggtitle("Google Trends Sample Error 2021") +
            labs(y = "Mean of Gprop")

wrap_gprop


# gprop (all together)
all_together_gprop <- plot21 %>% 
  filter(date > "2021-08-01") %>%
  ggplot(aes(x = date, y = Gprop, group = party, color = party)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = Gprop_lower.ci, ymax = Gprop_upper.ci,  fill = party), alpha = 0.4) +
  scale_color_manual(values = c("AFD" = "deepskyblue1", "CDU" = "black", "FDP" = "yellow2","Grüne" = "green3", "Linke" = "purple", "SPD" = "red")) +
  ggtitle("Google Trends Sample Error 2021") +
  labs(y = "Mean of Gprop")

all_together_gprop




ggsave(plot = all_together_gprop,
       filename = "Variation_GT_Data_all_together_gprop.png", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "png", # e.g. change to pdf
       dpi = 300)  