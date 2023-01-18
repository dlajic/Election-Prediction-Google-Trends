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
  
data_comparisons <- data_comparisons %>% select(sort(names(data_comparisons)))

data_comparisons <- data_comparisons %>% 
  unite("check", contains("identical"), 
        na.rm = TRUE, 
        remove = FALSE) %>% 
  mutate(check2 = ifelse(str_detect(check, "TRUE"), "BAD", "GOOD"))%>% 
  mutate(how_bad = str_count(check, "TRUE"))


View(data_comparisons %>% select(name, contains("identical"), check, check2, how_bad))



datasets_good <- data_comparisons %>% select(name, check, check2, how_bad) %>% filter(check2=="GOOD") %>%
       filter(name!="WK")
# GOOD = ALL datasets in .Rdata in this row are different from the .Rdata in the row before

# Filter out one RDate per day

datasets_good$date <- as.Date(str_extract(datasets_good$name, 
                                             pattern = "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"))
dates <- na.omit(unique(datasets_good$date))

datasets_good <- datasets_good %>%
                      group_by(date) %>%
                        filter(row_number()==1) %>%
                  filter(date <= "2022-12-16" & date > "2022-12-06")

datasets_we_can_use <- datasets_good$name
# Copy datasets from "Data_raw" to "Data"

for(i in datasets_we_can_use){
  #i <- "2022-12-07 04-31-13.RData"
  file.copy(from = paste0("/cloud/project/Data_raw/", i), 
            to = paste0("/cloud/project/Data/", i),
            overwrite = TRUE)
}




# JAN: Do the same for DataWC
# IGNORE THE BELOW


# Pick 




# compareDF::compare_df(
#   data.frame(data_comparisons[[26]][[2]]),
#   data.frame(data_comparisons[[26]][[1]]))





# NEW LOGIC 2 (dataset selection)
  # 1. Take one .Rdata from day/date
  # 2. Compare all Rdata of next day/date to this one
  # 3. Keep one Rdata of next day/date that it different across all datasets in Rdata
  # 4. Move to next day/date


  # Extract dates to filter on them

  data_comparisons$date <- as.Date(str_extract(data_comparisons$name, 
                                        pattern = "^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"))
  dates <- unique(data_comparisons$date)
  
  
  
  
  
  x <- names(data_comparisons)[str_detect(names(data_comparisons), "trend")]
  x <- x[!str_detect(x, "identical")]
  
  
  
  data_datasets <- NULL
  
  for(y in dates[-1]){
    
    y <- as.Date("2022-11-28")

    data_1 <- data_comparisons %>% 
      filter(date == y - 1) %>% 
      slice(1)
    
    
    data_2 <- data_comparisons %>% 
      filter(date == y)
    
    for (i in x){
      #i <- "trend_05"
      
      for(z in 1:nrow(data_2)){
      #z <- 1

              result <- identical(
          data_1[,i][[1]][[1]],
          data_2[z,i][[1]][[1]])
      print(result)
      
      data_2[z, paste0(i, "_identical")] <- result
      print(z)
      print(i)
    }}
    
  # NOT FINISHED
  
  
  
  
  


# FALSE IT UNGLEICH!
# Select one .Rdata from one day (= date) -> go to next day and 
# pick one where all are TRUE (because datasets are all different)
# Keep only the 2 datasets from the two dates move to the 3rd date/day

# Nimm so lange bis Reference bis am nächsten Tag ein FULL TRUE -> 
# KEEP Reference and FULL -> move to the next
# Loopen über Tage und Datensätze innerhalb von Tage




