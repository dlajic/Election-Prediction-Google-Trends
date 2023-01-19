# File is used to compare GT datasets and extract subset
# of datasets that are not identical (both with and without category)

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


# Datasets with category ####
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
    
  }}






# LOGIC 1 (dataset selection)
  # Compare datasets in Rdata with those of Rdata in previous row

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
  
data_comparisons <- data_comparisons %>% 
  select(sort(names(data_comparisons))) # Sort variable names








data_comparisons2 <- data_comparisons %>% 
  unite("check", contains("identical"), 
        na.rm = TRUE, 
        remove = FALSE) %>% 
  mutate(no_dataset_identical = ifelse(str_detect(check, "TRUE"), FALSE, TRUE))%>% 
  mutate(how_many_dataset_identical = str_count(check, "TRUE")) #%>%
  #select(name, check, contains("identical"), no_dataset_identical, how_many_dataset_identical)
# no_dataset_identical = GOOD: Non of the datasets in RData is the same as RData in previous row
# how_many_dataset_identical = Counts how many of the datassets in RData are the same as in the previous row


#View(data_comparisons %>% select(name, contains("identical"), check, no_dataset_identical, how_many_dataset_identical))


# Subset all datasets that are not identical
datasets_good <- data_comparisons2 %>% 
  select(name, check, no_dataset_identical, how_many_dataset_identical) %>% 
  filter(no_dataset_identical==TRUE)
  # GOOD = ALL datasets in .Rdata in this row are different from the .Rdata in the row before

# Filter out one RDate per day

datasets_good$date <- as.Date(str_extract(datasets_good$name, 
                                             pattern = "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"))
dates <- na.omit(unique(datasets_good$date))

datasets_good <- datasets_good %>%
                      group_by(date) %>%
                        filter(row_number()==1) %>%
                  filter(date < "2022-12-11" & date >= "2022-12-01")

datasets_we_can_use <- datasets_good$name
# Copy datasets from "Data_raw" to "Data"

unlink("/cloud/project/Data", recursive=TRUE)
dir.create("/cloud/project/Data")
for(i in datasets_we_can_use){
  #i <- "2022-12-07 04-31-13.RData"
  file.copy(from = paste0("/cloud/project/Data_raw/", i), 
            to = paste0("/cloud/project/Data/", i),
            overwrite = TRUE)
}







# compareDF::compare_df(
#   data.frame(data_comparisons[[26]][[2]]),
#   data.frame(data_comparisons[[26]][[1]]))






# Datasets WITHOUT category ####


#all Datsets
rm(list=ls())


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



dir <- setwd("/cloud/project/Data_raw_WC")
df_names <- list.files(dir, full.names = FALSE)
df <- list.files(dir, full.names = TRUE)

# Dataframe
data_comparisons <- data.frame(matrix(NA,    # Create empty data frame
                                      nrow = length(df),
                                      ncol = 27))

names(data_comparisons) <- c("name", "trend_05_WK", "trend_09_WK", "trend_AFD_13_WK", "trend_AFD_17_WK", "trend_AFD_21_WK", 
                             "trend_CDU_13_WK", "trend_CDU_17_WK", "trend_CDU_21_WK", "trend_sonst_05_WK", 
                             "trend_sonst_09_WK", "trend_sonst_13_WK", "trend_sonst_17_WK", "trend_sonst_21_WK",
                             
                             "trend_05_WK_identical", "trend_09_WK_identical", "trend_AFD_13_WK_identical", 
                             "trend_AFD_17_WK_identical", "trend_AFD_21_WK_identical", "trend_CDU_13_WK_identical", 
                             "trend_CDU_17_WK_identical", "trend_CDU_21_WK_identical", "trend_sonst_05_WK_identical", 
                             "trend_sonst_09_WK_identical", "trend_sonst_13_WK_identical", "trend_sonst_17_WK_identical", 
                             "trend_sonst_21_WK_identical")
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
    
  }}






# LOGIC 1 (dataset selection)
# Compare datasets in Rdata with those of Rdata in previous row

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

data_comparisons <- data_comparisons %>% 
  select(sort(names(data_comparisons))) # Sort variable names








data_comparisons2 <- data_comparisons %>% 
  unite("check", contains("identical"), 
        na.rm = TRUE, 
        remove = FALSE) %>% 
  mutate(no_dataset_identical = ifelse(str_detect(check, "TRUE"), FALSE, TRUE))%>% 
  mutate(how_many_dataset_identical = str_count(check, "TRUE")) #%>%
#select(name, check, contains("identical"), no_dataset_identical, how_many_dataset_identical)
# no_dataset_identical = GOOD: Non of the datasets in RData is the same as RData in previous row
# how_many_dataset_identical = Counts how many of the datassets in RData are the same as in the previous row


#View(data_comparisons %>% select(name, contains("identical"), check, no_dataset_identical, how_many_dataset_identical))


# Subset all datasets that are not identical
datasets_good <- data_comparisons2 %>% 
  select(name, check, no_dataset_identical, how_many_dataset_identical) %>% 
  filter(no_dataset_identical==TRUE)
# GOOD = ALL datasets in .Rdata in this row are different from the .Rdata in the row before

# Filter out one RDate per day

datasets_good$date <- as.Date(str_extract(datasets_good$name, 
                                          pattern = "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"))
dates <- na.omit(unique(datasets_good$date))

datasets_good <- datasets_good %>%
  group_by(date) %>%
  filter(row_number()==1) %>%
  filter(date < "2022-12-11" & date >= "2022-12-01")

datasets_we_can_use <- datasets_good$name
# Copy datasets from "Data_raw_WC" to "Data_WC"

unlink("/cloud/project/Data_WC", recursive=TRUE)
dir.create("/cloud/project/Data_WC")
for(i in datasets_we_can_use){
  #i <- "2022-12-07 04-31-13.RData"
  file.copy(from = paste0("/cloud/project/Data_raw_WC/", i), 
            to = paste0("/cloud/project/Data_WC/", i),
            overwrite = TRUE)
}





