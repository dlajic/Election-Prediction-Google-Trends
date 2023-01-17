library(pacman)

p_load(gtrendsR,
       ggplot2,
       dplyr,
       tidyr,
       rvest,
       xml2,
       stringr)


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


#all Datsets
dir <- setwd("/cloud/project/Data_raw")
df_names <- list.files(dir, full.names = FALSE)
df <- list.files(dir, full.names = TRUE)

# Dataframe
data_comparisons <- data.frame(matrix(NA,    # Create empty data frame
                                      nrow = length(df),
                                      ncol = 27))

names(data_comparisons) <- c("name", "trend_05", "trend_09", "trend_AFD_13", "trend_AFD_17", "trend_AFD_21", 
                             "trend_CDU_13", "trend_CDU_17", "trend_CDU_21", "trend_sonst_05", 
                             "trend_sonst_09", "trend_sonst_13", "trend_sonst_17", "trend_sonst_21",
                             
                             "trend_05_identical", "trend_09_identical", "trend_AFD_13_identical", 
                             "trend_AFD_17_identical", "trend_AFD_21_identical", "trend_CDU_13_identical", 
                             "trend_CDU_17_identical", "trend_CDU_21_identical", "trend_sonst_05_identical", 
                             "trend_sonst_09_identical", "trend_sonst_13_identical", "trend_sonst_17_identical", 
                             "trend_sonst_21_identical")
data_comparisons$name <- df_names

data_comparisons <- as_tibble(data_comparisons)
data_comparisons <- data_comparisons %>% mutate_at(2:14, list)

#creating a list where the results of Model1_09 of every Dataset is stored, to compare these


for (y in 1:length(df)){ # 
  #y <- 1
  
  load(df[y])
  
  objects <- ls()[str_detect(ls(), "trend")]
  
  for (i in objects){
    #i <- "trend_05"
    object_i <- get(i)$interest_over_time %>%
      select(date, hits, keyword) %>%
      mutate(hits = as.numeric(hits), date = as.Date(date))%>% 
      replace(is.na(.), 0) %>%
      mutate(keyword =  replace_searchterms(keyword))
    
    data_comparisons[y, i][i][[1]][[1]] <- as_tibble(object_i)
    
    print(y)
    print(i)
    
  }
  

  
  }
  
x <- names(data_comparisons)[str_detect(names(data_comparisons), "trend")]
x <- x[!str_detect(x, "identical")]

for(y in 2:nrow(data_comparisons)){ # 

  for (i in x){

  data_comparisons[paste0(i, "_identical")][[1]][y] <-
  identical(
    data_comparisons[y-1, i][i][[1]][[1]],
  data_comparisons[y, i][i][[1]][[1]])
  print(y)
  print(i)
  }
}
  
data_comparisons <- data_comparisons %>% select(sort(names(data_comparisons)))

data_comparisons <- data_comparisons %>% 
  unite("check", contains("identical"), 
        na.rm = TRUE, 
        remove = FALSE) %>% 
  mutate(check2 = ifelse(str_detect(check, "TRUE"), "BAD", "GOOD"))%>% 
  mutate(how_bad = str_count(check, "TRUE"))


View(data_comparisons %>% select(name, contains("identical"), check, check2, how_bad))



View(data_comparisons %>% select(name, check, check2, how_bad) %>% filter(check2=="GOOD"))

# FALSE IT UNGLEICH!
# Select one .Rdata from one day (= date) -> go to next day and 
# pick one where all are TRUE (because datasets are all different)
# Keep only the 2 datasets from the two dates move to the 3rd date/day

# Nimm so lange bis Reference bis am nächsten Tag ein FULL TRUE -> 
# KEEP Reference and FULL -> move to the next
# Loopen über Tage und Datensätze innerhalb von Tage

compareDF::compare_df(
  data.frame(data_comparisons[[26]][[2]]),
  data.frame(data_comparisons[[26]][[1]]))



