library(gtrendsR)
library(dplyr)
library(stringr)
library(tidyverse)
library(htmlTable)
library(forcats)


###### Run the following code if you want to draw a sample for each category for all four election years ####
#
## get all categories of Google Trends
#data('categories')
#
## filter the corresponding dataset for all supercategories
#categories <- categories %>%
#  filter(name == "All categories" | 
#           name == "Arts & Entertainment" |
#           name == "Autos & Vehicles" |
#           name == "Beauty & Fitness" |
#           name == "Books & Literature" |
#           name == "Business & Industrial" |
#           name == "Computer & Electronics" |
#           name == "Finance" |
#           name == "Food & Drink" |
#           name == "Games" |
#           name == "Health" |
#           name == "Hobbies & Leisure" |
#           name == "Home & Garden" |
#           name == "Internet & Telecom" |
#           name == "Jobs & Education" |
#           name == "Law & Government" |
#           name == "News" |
#           name == "Online Communities" |
#           name == "People & Society" |
#           name == "Pets & Animals" |
#           name == "Real Estate" |
#           name == "Reference" |
#           name == "Science" |
#           name == "Shopping" |
#           name == "Sports" |
#           name == "Travel") %>%
#  filter(!duplicated(name))         # needed because Pets & Animals appear two times in categories list
#
#
## preparation for loop
## create lists containing the election results 
#list_electionresults <- NULL
#list_electionresults[["2009-09-27"]] <- data.frame(party=c("CDU", "FDP", "Grüne", "Linke", "SPD"),
#                                                   share=c(33.8, 14.6, 10.7, 11.9, 23)) %>% arrange(party)
#
#list_electionresults[["2013-09-22"]] <- data.frame(party=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"),
#                                                   share=c(4.7, 41.5, 4.8, 8.4, 8.6, 25.7)) %>% arrange(party)
#
#list_electionresults[["2017-09-24"]] <- data.frame(party=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"),
#                                                   share=c(12.6, 32.9, 10.7, 8.9 , 9.2, 20.5)) %>% arrange(party)
#
#list_electionresults[["2021-09-26"]] <- data.frame(party=c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD"),
#                                                   share=c(10.3, 24.1, 11.5, 14.8 , 4.9, 25.7)) %>% arrange(party)
#
#
## needed for loop
#cat_name <- as.character(categories$name)
#cat_ids <- as.integer(categories$id)
#years <- c("2021-01-01 2021-09-26", "2017-01-01 2017-09-26", "2013-01-01 2013-09-26", "2009-01-01 2009-09-26")
#
#
## create empty dataset in which the GT data and results are written
#Comp_categories <- data.frame("Category_ID" = cat_ids, "Category_Name" = cat_name, "Data_2021" = NA, 
#                              "Data_2017" = NA, "Data_2013" = NA, "Data_2009" = NA, 
#                              "Elec_results21" = NA, "Dev21" = NA,
#                              "Elec_results17" = NA, "Dev17" = NA,
#                              "Elec_results13" = NA, "Dev13" = NA,
#                              "Elec_results09" = NA, "Dev09" = NA)
#
#
## write previously created election results into dataset 
#for (i in 1:nrow(Comp_categories)){
#  
#  Comp_categories$Elec_results09[i] <- list(list_electionresults[["2009-09-27"]])
#  Comp_categories$Elec_results13[i] <- list(list_electionresults[["2013-09-22"]])
#  Comp_categories$Elec_results17[i] <- list(list_electionresults[["2017-09-24"]])
#  Comp_categories$Elec_results21[i] <- list(list_electionresults[["2021-09-26"]])
#  
#}
#
#
## The following code pulls GT raw data for all categories for the four election years  
## trycatch is used since it possible that http error 429 appears (too many requests) 
## the code is designed in fashion that if you retrieve only an empty dataset that it will retry it 5 times and then proceeds
## In the case that after 5 tries no data was retrieved you can execute the code over and over until all datasets are collected
#
#t = 0
#
#for (o in years){
#  
#  for (i in cat_ids){
#    
#    
#    if (t >= 25){
#      
#      t = 0
#      
#    }
#    
#    l = 0
#    t = t + 1      
#    
#    
#    if (o == "2021-01-01 2021-09-26" & is.na(Comp_categories$Data_2021[t])){
#      
#      repeat{
#      
#      skip_to_next_1 <- FALSE
#      
#      trend_all_cat = tryCatch(gtrends(keyword= c('CDU', 'SPD', 'Grüne', 'Linke', 'FDP'), 
#                                       geo = "DE" , category = i , time = o , gprop = "web", onlyInterest = TRUE), error = function(e) { skip_to_next_1 <<- TRUE})
#      
#      if(skip_to_next_1 == TRUE) { next }     
#      Sys.sleep(5)
#  
#      skip_to_next_2 <- FALSE
#      
#      trend_all_cat_AFD = tryCatch(gtrends(keyword= c('CDU', 'SPD', 'Grüne', 'FDP', "Afd"), 
#                                           geo = "DE" , category = i, time = o , gprop = "web", onlyInterest = TRUE), error = function(e) { skip_to_next_2 <<- TRUE})
#      
#      if(skip_to_next_1 == FALSE & skip_to_next_2 == FALSE & is.null(trend_all_cat[[1]][1]) == FALSE & is.null(trend_all_cat_AFD[[1]][1]) == FALSE) { 
#        
#        break
#      }
#      
#      if(l == 5){
#        
#        
#        break
#        
#      }
#      
#      l = l + 1
#      print("Again")
#      
#      }   
#      
#      
#      Comp_categories$Data_2021[t] <- list(rbind(trend_all_cat$interest_over_time, 
#                                                 anti_join(trend_all_cat_AFD$interest_over_time, trend_all_cat$interest_over_time, by = "keyword")))
#      
#      
#      mem <- as.data.frame(Comp_categories$Data_2021[t]) %>%
#        group_by(keyword) %>%
#        mutate(hits = str_replace(hits, "<1", "0"), # HIER WEITER
#               hits = as.numeric(hits)) %>%
#        dplyr::summarise(mean_hits = mean(hits)) %>%
#        mutate(dev_hits = mean_hits - Comp_categories$Elec_results21[[t]][2],
#               gprop = ifelse(mean_hits >= 1, mean_hits/sum(mean_hits)*100, 0),
#               dev_gprop = gprop -  Comp_categories$Elec_results21[[t]][2],
#               sum_dev_hits_abs = sum(abs(dev_hits$share)),
#               sum_dev_prop_abs = sum(abs(dev_gprop$share)))
#      
#      Comp_categories$Dev21[t] <- list(mem)
#      
#      print(i)
#      print(t)
#      
#    }
#    
#    
#    if (o == "2017-01-01 2017-09-26" & is.na(Comp_categories$Data_2017[t])){
#      
#      repeat{
#      
#      skip_to_next_1 <- FALSE
#      
#      trend_all_cat = tryCatch(gtrends(keyword= c('CDU', 'SPD', 'Grüne', 'Linke', 'FDP'), 
#                                       geo = "DE" , category = i , time = o , gprop = "web", onlyInterest = TRUE), error = function(e) { skip_to_next_1 <<- TRUE})
#      
#      if(skip_to_next_1 == TRUE) { next }     
#      Sys.sleep(5)
#      
#      skip_to_next_2 <- FALSE
#      
#      trend_all_cat_AFD = tryCatch(gtrends(keyword= c('CDU', 'SPD', 'Grüne', 'FDP', "Afd"), 
#                                           geo = "DE" , category = i, time = o , gprop = "web", onlyInterest = TRUE), error = function(e) { skip_to_next_2 <<- TRUE})
#      
#      if(skip_to_next_1 == FALSE & skip_to_next_2 == FALSE & is.null(trend_all_cat[[1]][1]) == FALSE & is.null(trend_all_cat_AFD[[1]][1]) == FALSE) { 
#        
#        break
#      }
#      
#      if(l == 5){
#        
#        
#        break
#        
#      }
#      
#      l = l + 1
#      print("Again")
#      
#      }   
#      
#      
#      Comp_categories$Data_2017[t] <- list(rbind(trend_all_cat$interest_over_time, 
#                                                 anti_join(trend_all_cat_AFD$interest_over_time, trend_all_cat$interest_over_time, by = "keyword")))
#      
#      
#      mem <- as.data.frame(Comp_categories$Data_2017[t]) %>%
#        group_by(keyword) %>%
#        mutate(hits = str_replace(hits, "<1", "0"), # HIER WEITER
#               hits = as.numeric(hits)) %>%
#        dplyr::summarise(mean_hits = mean(hits)) %>%
#        mutate(dev_hits = mean_hits - Comp_categories$Elec_results17[[t]][2],
#               gprop = ifelse(mean_hits >= 1, mean_hits/sum(mean_hits)*100, 0),
#               dev_gprop = gprop -  Comp_categories$Elec_results17[[t]][2],
#               sum_dev_hits_abs = sum(abs(dev_hits$share)),
#               sum_dev_prop_abs = sum(abs(dev_gprop$share)))
#      
#      Comp_categories$Dev17[t] <- list(mem)
#      
#      print(i)
#      print(t)
#     
#    }
#    
#    
#    if (o == "2013-01-01 2013-09-26" & is.na(Comp_categories$Data_2013[t])){
#      
#      repeat{
#      
#      skip_to_next_1 <- FALSE
#      
#      trend_all_cat = tryCatch(gtrends(keyword= c('CDU', 'SPD', 'Grüne', 'Linke', 'FDP'), 
#                                       geo = "DE" , category = i , time = o , gprop = "web", onlyInterest = TRUE), error = function(e) { skip_to_next_1 <<- TRUE})
#      
#      if(skip_to_next_1 == TRUE) { next }     
#      Sys.sleep(5)
#      
#      skip_to_next_2 <- FALSE
#      
#      trend_all_cat_AFD = tryCatch(gtrends(keyword= c('CDU', 'SPD', 'Grüne', 'FDP', "Afd"), 
#                                           geo = "DE" , category = i, time = o , gprop = "web", onlyInterest = TRUE), error = function(e) { skip_to_next_2 <<- TRUE})
#      
#      if(skip_to_next_1 == FALSE & skip_to_next_2 == FALSE & is.null(trend_all_cat[[1]][1]) == FALSE & is.null(trend_all_cat_AFD[[1]][1]) == FALSE) { 
#        
#        break
#      }
#      
#      if(l == 5){
#        
#        
#        break
#        
#      }
#      
#      l = l + 1
#      print("Again")
#      
#      }      
#      
#      Comp_categories$Data_2013[t] <- list(rbind(trend_all_cat$interest_over_time, 
#                                                 anti_join(trend_all_cat_AFD$interest_over_time, trend_all_cat$interest_over_time, by = "keyword")))
#      
#      
#      mem <- as.data.frame(Comp_categories$Data_2013[t]) %>%
#        group_by(keyword) %>%
#        mutate(hits = str_replace(hits, "<1", "0"), # HIER WEITER
#               hits = as.numeric(hits)) %>%
#        dplyr::summarise(mean_hits = mean(hits)) %>%
#        mutate(dev_hits = mean_hits - Comp_categories$Elec_results13[[t]][2],
#               gprop = ifelse(mean_hits >= 1, mean_hits/sum(mean_hits)*100, 0),
#               dev_gprop = gprop -  Comp_categories$Elec_results13[[t]][2],
#               sum_dev_hits_abs = sum(abs(dev_hits$share)),
#               sum_dev_prop_abs = sum(abs(dev_gprop$share)))
#      
#      Comp_categories$Dev13[t] <- list(mem)
#      
#      
#      print(i)
#      print(t)
#      
#    }
#    
#    
#    if (o == "2009-01-01 2009-09-26" & is.na(Comp_categories$Data_2009[t])){
#      
#      repeat{
#      
#      skip_to_next_1 <- FALSE
#      
#      trend_all_cat = tryCatch(gtrends(keyword= c('CDU', 'SPD', 'Grüne', 'Linke', 'FDP'), 
#                                       geo = "DE" , category = i , time = o , gprop = "web", onlyInterest = TRUE), error = function(e) { skip_to_next_1 <<- TRUE})
#      
#      if(skip_to_next_1 == TRUE) { next }     
#      Sys.sleep(5)
#      
#      if(skip_to_next_1 == FALSE & is.null(trend_all_cat[[1]][1]) == FALSE) { 
#        
#        break
#      }
#      
#      if(l == 5){
#        
#        
#        break
#        
#      }
#      
#      l = l + 1
#      print("Again")
#      
#      }
#      
#      Comp_categories$Data_2009[t] <- list(trend_all_cat$interest_over_time)
#      
#      
#      mem <- as.data.frame(Comp_categories$Data_2009[t]) %>%
#        group_by(keyword) %>%
#        mutate(hits = str_replace(hits, "<1", "0"), # HIER WEITER
#               hits = as.numeric(hits)) %>%
#        dplyr::summarise(mean_hits = mean(hits)) %>%
#        mutate(dev_hits = mean_hits - Comp_categories$Elec_results09[[t]][2],
#               gprop = ifelse(mean_hits >= 1, mean_hits/sum(mean_hits)*100, 0),
#               dev_gprop = gprop -  Comp_categories$Elec_results09[[t]][2],
#               sum_dev_hits_abs = sum(abs(dev_hits$share)),
#               sum_dev_prop_abs = sum(abs(dev_gprop$share)))
#      
#      Comp_categories$Dev09[t] <- list(mem)
#      
#      print(i)
#      print(t)
#      
#    }
#  }
#}
#
#
## Calculate the average absolute deviation of each category over all four election years
#for (k in 1:nrow(Comp_categories)){
#
#Comp_categories$Mean_dev_hits[k] <- mean(c(Comp_categories$Dev09[[k]][[6]][1], Comp_categories$Dev13[[k]][[6]][1],
#                                              Comp_categories$Dev17[[k]][[6]][1], Comp_categories$Dev21[[k]][[6]][1]))
#
#Comp_categories$Mean_dev_gprop[k] <- mean(c(Comp_categories$Dev09[[k]][[7]][1], Comp_categories$Dev13[[k]][[7]][1],
#                                         Comp_categories$Dev17[[k]][[7]][1], Comp_categories$Dev21[[k]][[7]][1]))
#
#}
#
#
## Create a formatted table to display the results
#library(htmlTable)
#
#Comp_categories %>%
#  arrange(Mean_dev_gprop) %>%
#  select(Category_ID, Category_Name ,Mean_dev_gprop) %>%
#  mutate(Mean_dev_gprop = round(Mean_dev_gprop, digits =  2)) %>%
#  htmlTable()
#
## Save environment
#filename = paste0("Selection_category_all_", gsub(":", "-", Sys.time()), ".RData",sep="")
#save.image(paste0("./Category_Selection/Samples", (filename)))



library(gtrendsR)
library(dplyr)
library(stringr)
library(tidyverse)
library(htmlTable)
library(forcats)



##### Run the following code if you want to bind all collected datasets to one and calculate the final results #####

setwd("./Category_Selection/Samples")


load("Selection_category_all_2023-02-01 16-28-15.RData")

# prep for loops
names <- dir("./")
names <- names[!names %in% "Selection_category_all_2023-02-01 16-28-15.RData"]
names2 <- c("Data_2021", "Data_2017", "Data_2013", "Data_2009")
names3 <- c("Dev09", "Dev13", "Dev17", "Dev21")


# needed to bind datasets from different datasets together via rbind
final_df <- Comp_categories


# bind the data columns and dev columns of all collected datasets together
for (u in names){
  
  load(u)
  
  for (g in names2){
    
    for (z in 1:nrow(Comp_categories)){
  
  final_df[[g]][[z]] <- rbind(data.frame(final_df[[g]][z]), data.frame(Comp_categories[[g]][z]))

    }
  }
  
  for (s in names3){
    
    for (z in 1:nrow(Comp_categories)){
  
      final_df[[s]][[z]] <- rbind(data.frame(final_df[[s]][z]), data.frame(Comp_categories[[s]][z]))
      
    }
  }
}
  
  
# create new empty columns for the following loop, in which the absolute average deviation from the election results and the respective ci.intervals for the respective election years are written
final_df <- cbind(final_df, Mean_Dev09 = NA, Mean_Dev13 = NA, Mean_Dev17 = NA, Mean_Dev21 = NA, 
                  Mean_gprop_lower.ci = NA, Mean_gprop_upper.ci = NA)



for (z in 1:nrow(Comp_categories)){
        

    mem <- final_df$Dev09[[z]] %>%
                group_by(keyword) %>%
                summarise(Mean_dev_hits = mean(sum_dev_hits_abs), SD_mean_dev_hits = sd(sum_dev_hits_abs),
                          Mean_dev_gprop = mean(sum_dev_prop_abs), SD_mean_dev_gprop = sd(sum_dev_prop_abs), 
                          .groups = "keep")  %>% 
                mutate(mean_hits_lower.ci = Mean_dev_hits - 1.96*(SD_mean_dev_hits/sqrt(n())),
                       mean_hits_upper.ci = Mean_dev_hits + 1.96*(SD_mean_dev_hits/sqrt(n())),
                       mean_gprop_lower.ci = Mean_dev_gprop - 1.96*(SD_mean_dev_gprop/sqrt(n())),
                       mean_gprop_upper.ci = Mean_dev_gprop + 1.96*(SD_mean_dev_gprop/sqrt(n())))
    
    final_df$Mean_Dev09[z] <- list(mem)
  
    
    mem <- final_df$Dev13[[z]] %>%
                group_by(keyword) %>%
                summarise(Mean_dev_hits = mean(sum_dev_hits_abs), SD_mean_dev_hits = sd(sum_dev_hits_abs),
                          Mean_dev_gprop = mean(sum_dev_prop_abs), SD_mean_dev_gprop = sd(sum_dev_prop_abs), 
                          .groups = "keep")  %>% 
                mutate(mean_hits_lower.ci = Mean_dev_hits - 1.96*(SD_mean_dev_hits/sqrt(n())),
                       mean_hits_upper.ci = Mean_dev_hits + 1.96*(SD_mean_dev_hits/sqrt(n())),
                       mean_gprop_lower.ci = Mean_dev_gprop - 1.96*(SD_mean_dev_gprop/sqrt(n())),
                       mean_gprop_upper.ci = Mean_dev_gprop + 1.96*(SD_mean_dev_gprop/sqrt(n())))
    
    final_df$Mean_Dev13[z] <- list(mem)
    
    
    mem <- final_df$Dev17[[z]] %>%
                group_by(keyword) %>%
                summarise(Mean_dev_hits = mean(sum_dev_hits_abs), SD_mean_dev_hits = sd(sum_dev_hits_abs),
                          Mean_dev_gprop = mean(sum_dev_prop_abs), SD_mean_dev_gprop = sd(sum_dev_prop_abs), 
                          .groups = "keep")  %>% 
                mutate(mean_hits_lower.ci = Mean_dev_hits - 1.96*(SD_mean_dev_hits/sqrt(n())),
                       mean_hits_upper.ci = Mean_dev_hits + 1.96*(SD_mean_dev_hits/sqrt(n())),
                       mean_gprop_lower.ci = Mean_dev_gprop - 1.96*(SD_mean_dev_gprop/sqrt(n())),
                       mean_gprop_upper.ci = Mean_dev_gprop + 1.96*(SD_mean_dev_gprop/sqrt(n())))
    
    final_df$Mean_Dev17[z] <- list(mem)
    
    
    mem <- final_df$Dev21[[z]] %>%
      group_by(keyword) %>%
      summarise(Mean_dev_hits = mean(sum_dev_hits_abs), SD_mean_dev_hits = sd(sum_dev_hits_abs),
                Mean_dev_gprop = mean(sum_dev_prop_abs), SD_mean_dev_gprop = sd(sum_dev_prop_abs), 
                .groups = "keep")  %>% 
      mutate(mean_hits_lower.ci = Mean_dev_hits - 1.96*(SD_mean_dev_hits/sqrt(n())),
             mean_hits_upper.ci = Mean_dev_hits + 1.96*(SD_mean_dev_hits/sqrt(n())),
             mean_gprop_lower.ci = Mean_dev_gprop - 1.96*(SD_mean_dev_gprop/sqrt(n())),
             mean_gprop_upper.ci = Mean_dev_gprop + 1.96*(SD_mean_dev_gprop/sqrt(n())))
    
    final_df$Mean_Dev21[z] <- list(mem)
      
}    


# Calculation of the absolute average deviation and the average of the ci.intervalls over all elections
for (z in 1:nrow(Comp_categories)){

  
        final_df$Mean_dev_hits[z] <-  mean(c(unique(final_df$Mean_Dev09[[z]][[2]]), 
                                           unique(final_df$Mean_Dev13[[z]][[2]]),
                                           unique(final_df$Mean_Dev17[[z]][[2]]), 
                                           unique(final_df$Mean_Dev21[[z]][[2]])))
        
        final_df$Mean_dev_gprop[z] <- mean(c(unique(final_df$Mean_Dev09[[z]][[4]]),
                                           unique(final_df$Mean_Dev13[[z]][[4]]),
                                           unique(final_df$Mean_Dev17[[z]][[4]]), 
                                           unique(final_df$Mean_Dev21[[z]][[4]])))
        
        final_df$Mean_gprop_lower.ci[z] <- mean(c(unique(final_df$Mean_Dev09[[z]][[8]]), 
                                                unique(final_df$Mean_Dev13[[z]][[8]]),
                                                unique(final_df$Mean_Dev17[[z]][[8]]), 
                                                unique(final_df$Mean_Dev21[[z]][[8]])))
        
        final_df$Mean_gprop_upper.ci[z] <- mean(c(unique(final_df$Mean_Dev09[[z]][[9]]),
                                                unique(final_df$Mean_Dev13[[z]][[9]]),
                                                unique(final_df$Mean_Dev17[[z]][[9]]), 
                                                unique(final_df$Mean_Dev21[[z]][[9]])))
        
      }



# reorder final dataset
final_df <- final_df %>%
  relocate(Mean_dev_hits, Mean_dev_gprop, .before = Mean_gprop_lower.ci)


# create table with final results containing the absolute average deviation and the average c.intervals over all four election years
final <- final_df %>%
  arrange(Mean_dev_gprop) %>%
  select(Category_ID, Category_Name ,Mean_dev_gprop, Mean_gprop_lower.ci, Mean_gprop_upper.ci) %>%
  mutate(Mean_dev_gprop = round(Mean_dev_gprop, digits =  2),
         Mean_gprop_lower.ci = round(Mean_gprop_lower.ci, digits = 2),
         Mean_gprop_upper.ci= round(Mean_gprop_upper.ci, digits = 2)) %>%
  htmlTable()



# Reorder the data for the plot
final_p <- final_df %>%
  select(Category_Name, Mean_dev_gprop, Mean_gprop_lower.ci, Mean_gprop_upper.ci) %>%
  arrange(Mean_dev_gprop) %>%
  mutate(Category_Name = as.factor(Category_Name))


p <- ggplot(final_p, aes(y = Mean_dev_gprop, x = fct_rev(fct_inorder(Category_Name)))) +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0,135,5)) +
  geom_point(aes(x = fct_rev(fct_inorder(Category_Name)), 
                 y = Mean_dev_gprop),
             color = ifelse(final_p$Category_Name == "Law & Government", "green3", 
                            ifelse(final_p$Category_Name == "All categories", "orange",  "black")), size = 2) + 
  #geom_text(aes(label = round(Mean_dev_gprop, digits = 2)), hjust = -3) +
  geom_linerange(aes(x = Category_Name, 
                     ymin = Mean_gprop_lower.ci,
                     ymax = Mean_gprop_upper.ci),
                 color = ifelse(final_p$Category_Name == "Law & Government", "green3", 
                                ifelse(final_p$Category_Name == "All categories", "orange",  "black")),
                 lwd = 1) + 
  #ggtitle("Avg. absolute deviation of Gprop over all election years") +
  ylab("Avg. absolute percentage deviation of the Google Proportion from\nall four election years (Samples n = 10)") +
  xlab("Google Trends supercategories") +
  coord_flip() +
  theme_minimal(base_size = 22)


q <- p + annotate(geom = "text", 
                  x = final_p$Category_Name[final_p$Category_Name == "Law & Government"], 
                  y = round(final_p$Mean_gprop_upper.ci[which(final_p$Category_Name == "Law & Government")], digits = 2) + 5,
                  label = paste(round(final_p$Mean_dev_gprop[which(final_p$Category_Name == "Law & Government")], digits =2), 
                                "(95% CI:",round(final_p$Mean_gprop_lower.ci[which(final_p$Category_Name == "Law & Government")], digits = 2), 
                                "/" , round(final_p$Mean_gprop_upper.ci[which(final_p$Category_Name == "Law & Government")], digits = 2), ")"),
                  color = "black", size = 6,
                  hjust = 0) +
  annotate(geom = "text", 
           x = final_p$Category_Name[final_p$Category_Name == "All categories"], 
           y = round(final_p$Mean_gprop_upper.ci[which(final_p$Category_Name == "All categories")], digits = 2) + 5,
           label = paste(round(final_p$Mean_dev_gprop[which(final_p$Category_Name == "All categories")], digits =2), 
                         "(95% CI:",round(final_p$Mean_gprop_lower.ci[which(final_p$Category_Name == "All categories")], digits = 2), 
                         "/" , round(final_p$Mean_gprop_upper.ci[which(final_p$Category_Name == "All categories")], digits = 2), ")"),
           color = "black", size = 6,
           hjust = 0)

ggsave(plot = q,
       filename = "Figure_A1_Selection_Category_Plot.png", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "png", # e.g. change to pdf
       dpi = 600)

