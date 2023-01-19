# Comparison of datasets

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
  
  
  