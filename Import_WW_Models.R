library(ajfhelpR)


## Loop G: ADD Predictions (Only polls) ####
data_models$Poll_dates_weekly_weighting <- list(NA)  
data_models$GT_data_weekly_weighting <- list(NA)
data_models$predicitons_GT_weekly_polls <- list(NA)
data_models$predicitons_GT_weekly_polls_mean <- list(NA)
data_models$predictions


for(i in 1:nrow(data_models)){ 
  
  # execute weekly weighting only in the corresponding rows
  if(data_models$datasource_weight[i] == "GT + weekly polls weight"){
    
    Model4_4 <- data.frame() # construct an empty data frame into which the results are written
    
    # get all surveys that are within the set interval and write them into a data set.
    test <- infra_dimap_all %>%
      mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
      filter(Date >= data_models$GT_start_date[i] &  Date <= data_models$GT_end_date[i])
    
    
    
    # The if command avoids problems if the interval is set to e.g. one week or less and there are no polls in this interval
    # Logic: If data set with polls empty, take next survey before interval and weight the Google data laying in the interval without polls
    if(nrow(test) == 0){
      
      # The if command ensures that the following code is only executed for the data from the year 2009 (the party AFD did not yet exist in 2009)
      if (grepl("2009", data_models$model_name[i])){
        
        # Search for the two polls that lie before our interval (second poll (k) to specify the Google data that will be used to calculate the weighting factor with the first poll (b), which will then be used to weight the Google data in our specified interval).
        b <- ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"), data_models$GT_start_date[i], onlypre = T)
        k <- which(grepl(b, as.Date(infra_dimap_all$Date, "%d.%m.%y")))+1
        infra_dimap_all$Date[k]
        
        
        # write the found poll dates into our data set
        data_models$Poll_dates_weekly_weighting[[i]] <- infra_dimap_all %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date >= Date[k]+1  &  Date <= data_models$GT_end_date[i])
        
        
        # now get Google data 1 day before second poll until that lies before our interval until the end of our interval 
        data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT[[i]] %>%
          filter(date >= as.Date(infra_dimap_all$Date[k], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
          group_by(keyword)
        
        
        # prepare first poll for calculation of weighting factor
        infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
          filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
          mutate_if(is.character, as.numeric) %>%
          replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
          summarise(perc = c(mean(CDU), mean(FDP),
                             mean(Grüne), mean(Linke), mean(SPD))) %>%
          mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
          select(party, perc)
        
        
        # get google data until first poll outside of interval and calculate weighting factor
        Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
          filter(date >= as.Date(infra_dimap_all$Date[k], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
          group_by(keyword) %>%
          summarize(hits_sum = sum(hits)) %>%
          mutate(perc = hits_sum/sum(hits_sum)*100) %>%
          mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
          rename(party = keyword) %>%
          select(party, perc) %>%
          mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
        
        
        # Apply weighting factor to the interval starting with the start date and ending with the end date of the set interval
        Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
          filter(date >= data_models$GT_start_date[i] & date <=  data_models$GT_end_date[i]) %>%
          group_by(keyword) %>%
          summarize(hits_sum = sum(hits)) %>%
          mutate(hits_sum2 = sum(hits_sum)) %>%
          mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted. 
          mutate(perc = hits_sum/hits_sum2*100) %>%
          rename(party = keyword) %>%
          select(party, perc) %>%
          mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
          mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
        
        
        data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning)
        
        print("0 poll data successful_09")
        
        
        
      }
      
      # The if command ensures that the following code is only executed for the data from the years 2013, 2017 and 2021 (accounting for AFD)
      if (!grepl("2009", data_models$model_name[i])){
        
        # Search for the two polls that lie before our interval (second poll (k) to specify the Google data that will be used to calculate the weighting factor with the first poll (b), which will then be used to weight the Google data in our specified interval)
        b <- ajfhelpR::date_near(as.Date(infra_dimap_all$Date, "%d.%m.%y"), data_models$GT_start_date[i], onlypre = T)
        k <- which(grepl(b, as.Date(infra_dimap_all$Date, "%d.%m.%y")))+1
        infra_dimap_all$Date[k]
        
        
        # write the found poll dates into our data set
        data_models$Poll_dates_weekly_weighting[[i]] <- infra_dimap_all %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date >= Date[k]+1  &  Date <= data_models$GT_end_date[i])
        
        
        # now get Google data 1 day before second poll until that lies before our interval until the end of our interval 
        data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT[[i]] %>%
          filter(date >= as.Date(infra_dimap_all$Date[k], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
          group_by(keyword)
        
        
        # prepare first poll for calculation of weighting factor
        infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
          filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
          mutate_if(is.character, as.numeric) %>%
          replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
          summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                             mean(Grüne), mean(Linke), mean(SPD))) %>%
          mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
          select(party, perc)
        
        
        # get google data until first poll outside of interval and calculate weighting factor
        Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
          filter(date >= as.Date(infra_dimap_all$Date[k], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
          group_by(keyword) %>%
          summarize(hits_sum = sum(hits)) %>%
          mutate(perc = hits_sum/sum(hits_sum)*100) %>%
          mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
          rename(party = keyword) %>%
          select(party, perc) %>%
          mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
        
        
        # Apply weighting factor to the interval starting with the start date and ending with the end date of the set interval
        Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
          filter(date >= data_models$GT_start_date[i] & date <=  data_models$GT_end_date[i]) %>%
          group_by(keyword) %>%
          summarize(hits_sum = sum(hits)) %>%
          mutate(hits_sum2 = sum(hits_sum)) %>%
          mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
          mutate(perc = hits_sum/hits_sum2*100) %>%
          rename(party = keyword) %>%
          select(party, perc) %>%
          mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
          mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
        
        
        data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning)
        
        print("0 poll data successful_131721")
        
        
        
      }
    }
    
    
    
    # if only one poll lies in our interval, weighting can be done in one step (no need for loop)
    if(nrow(test) == 1){ 
      
      if (grepl("2009", data_models$model_name[i])){
        
        # Search for the two polls that lie before the first poll in our previously filtered range of polls
        a <- which(grepl(dplyr::last(test$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+2
        infra_dimap_all$Date[a]
        
        
        # write these poll dates and the one that lies in our set interval into our data set
        data_models$Poll_dates_weekly_weighting[[i]] <-  infra_dimap_all %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 &  Date <= data_models$GT_end_date[i]) %>%
          arrange(Date) 
        
        
        # now get google data one day before the second survey that lies before our interval until the end of the set interval
        data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT[[i]] %>%
          filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
          group_by(keyword)
        
        
        # prepare first poll that lies outside of the interval for calculation of weighting factor  
        infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
          filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
          mutate_if(is.character, as.numeric) %>%
          replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
          summarise(perc = c(mean(CDU), mean(FDP),
                             mean(Grüne), mean(Linke), mean(SPD))) %>%
          mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
          select(party, perc)
        
        
        # get Google data until first survey outside the interval and calculate weighting factor
        Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
          filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
          group_by(keyword) %>%
          summarize(hits_sum = sum(hits)) %>%
          mutate(perc = hits_sum/sum(hits_sum)*100) %>%
          mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
          rename(party = keyword) %>%
          select(party, perc) %>%
          mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
        
        
        # Apply weighting factor to the interval starting with the start date until first poll that lies within our set interval
        Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
          filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
          group_by(keyword) %>%
          summarize(hits_sum = sum(hits)) %>%
          mutate(hits_sum2 = sum(hits_sum)) %>%
          mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
          mutate(perc = hits_sum/hits_sum2*100) %>%
          rename(party = keyword) %>%
          select(party, perc) %>%
          mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
          mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
        
        
        # prepare first poll that lies within the set interval for weighting
        infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
          filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
          mutate_if(is.character, as.numeric) %>%
          replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
          summarise(perc = c(mean(CDU), mean(FDP),
                             mean(Grüne), mean(Linke), mean(SPD))) %>%
          mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
          select(party, perc)
        
        
        # calculate weight from period ranging from start date of the set interval until the first poll within the set interval 
        Model4_3_first <-  data_models$GT_data_weekly_weighting[[i]] %>%
          filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
          group_by(keyword) %>%
          summarize(hits_sum = sum(hits)) %>%
          mutate(perc = hits_sum/sum(hits_sum)*100) %>%
          mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
          rename(party = keyword) %>%
          select(party, perc) %>%
          mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
        
        
        
        # Get google proportion to be weighted i.e. one day after the first survey in the set interval to the end date of our set interval and apply the previously calculated weight
        Model4_4_first <- data_models$GT_data_weekly_weighting[[i]] %>%
          filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]+1 & date <= data_models$GT_end_date[i]) %>%
          group_by(keyword) %>%
          summarize(hits_sum = sum(hits)) %>%
          mutate(hits_sum2 = sum(hits_sum)) %>%
          mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
          mutate(perc = hits_sum/hits_sum2*100) %>%
          rename(party = keyword) %>%
          select(party, perc) %>%
          mutate(perc= Model4_3_first$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
          mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
        
        
        data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
        
        print("1 poll sucessful 09")
        
        
        
      }
      
      
      if (!grepl("2009", data_models$model_name[i])){
        
        # Search for the two polls that lie before the first poll in our previously filtered range of polls
        a <- which(grepl(dplyr::last(test$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+2
        infra_dimap_all$Date[a]
        
        
        # write these poll dates and the one that lies in our set interval into our data set
        data_models$Poll_dates_weekly_weighting[[i]] <-  infra_dimap_all %>%
          mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
          filter(Date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 &  Date <= data_models$GT_end_date[i]) %>%
          arrange(Date) 
        
        
        # now get google data one day before the second survey that lies before our interval until the end of the set interval
        data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT[[i]] %>%
          filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
          group_by(keyword)
        
        
        # prepare first poll that lies outside of the interval for calculation of weighting factor  
        infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
          filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
          mutate_if(is.character, as.numeric) %>%
          replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1.
          summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                             mean(Grüne), mean(Linke), mean(SPD))) %>%
          mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
          select(party, perc)
        
        
        # get Google data until first survey outside the interval and calculate weighting factor
        Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
          filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
          group_by(keyword) %>%
          summarize(hits_sum = sum(hits)) %>%
          mutate(perc = hits_sum/sum(hits_sum)*100) %>%
          mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
          rename(party = keyword) %>%
          select(party, perc) %>%
          mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
        
        
        # Apply weighting factor to the interval starting with the start date until first poll that lies within our set interval
        Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
          filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
          group_by(keyword) %>%
          summarize(hits_sum = sum(hits)) %>%
          mutate(hits_sum2 = sum(hits_sum)) %>%
          mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
          mutate(perc = hits_sum/hits_sum2*100) %>%
          rename(party = keyword) %>%
          select(party, perc) %>%
          mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
          mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
        
        
        # prepare first poll that lies within the set interval for weighting
        infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
          filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
          mutate_if(is.character, as.numeric) %>%
          replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
          summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                             mean(Grüne), mean(Linke), mean(SPD))) %>%
          mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
          select(party, perc)
        
        
        # calculate weight from period ranging from start date of the set interval until the first poll within the set interval 
        Model4_3_first <-  data_models$GT_data_weekly_weighting[[i]] %>%
          filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]) %>%
          group_by(keyword) %>%
          summarize(hits_sum = sum(hits)) %>%
          mutate(perc = hits_sum/sum(hits_sum)*100) %>%
          mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
          rename(party = keyword) %>%
          select(party, perc) %>%
          mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
        
        
        # Get google proportion to be weighted i.e. one day after the first survey in the set interval to the end date of our set interval and apply the previously calculated weight
        Model4_4_first <- data_models$GT_data_weekly_weighting[[i]] %>%
          filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][2]+1 & date <= data_models$GT_end_date[i]) %>%
          group_by(keyword) %>%
          summarize(hits_sum = sum(hits)) %>%
          mutate(hits_sum2 = sum(hits_sum)) %>%
          mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
          mutate(perc = hits_sum/hits_sum2*100) %>%
          rename(party = keyword) %>%
          select(party, perc) %>%
          mutate(perc= Model4_3_first$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
          mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
        
        
        data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
        
        print("1 poll sucessful 131721")  
        
        
        
      }
    }
    
    # Execute the following code for all intervals in which 2 or more polls fall within
    if(nrow(test) >= 2){ 
      
      # Search for the two polls that lie before the first poll in our previously filtered range of polls
      a <- which(grepl(dplyr::last(test$Date), as.Date(infra_dimap_all$Date, "%d.%m.%y")))+2
      infra_dimap_all$Date[a]
      
      # write these poll dates into our data set
      data_models$Poll_dates_weekly_weighting[[i]] <-  infra_dimap_all %>%
        mutate(Date = as.Date(Date, "%d.%m.%y")) %>%
        filter(Date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 &  Date <= data_models$GT_end_date[i]) %>%
        arrange(Date) 
      
      
      # now get google data one day before the second survey that lies before our interval until the end of the set interval
      data_models$GT_data_weekly_weighting[[i]] <- data_models$data_GT[[i]] %>%
        filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= data_models$GT_end_date[i]) %>%
        group_by(keyword)
      
      
      
      if (grepl("2009", data_models$model_name[i])){
        
        # start with 2:nrow since steps 1 & 2 are done in j == 2 
        for (j in 2:nrow(data_models$Poll_dates_weekly_weighting[[i]])){
          
          if (j == 2){
            
            # calculate weight for interval before first poll
            infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
              summarise(perc = c(mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = hits_sum/sum(hits_sum)*100) %>%
              mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
              filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = hits_sum/hits_sum2*100) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
              mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            # get the survey data for that is in first place in the objects
            infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
              summarise(perc = c(mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            # calculate weight for period ranging from one day after survey that is before the first survey in our objects to first survey in objects and calculate weight by dividing the proportion by the survey date pulled in the previous code chunk
            Model4_3_first <-  data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = hits_sum/sum(hits_sum)*100) %>%
              mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            # Get google proportion to be weighted i.e. one day after the first survey in our objects to the second survey date entry in our objects and apply the previously calculated weight
            Model4_4_first <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j+1]) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = hits_sum/hits_sum2*100) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc= Model4_3_first$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
              mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            Model4_4 <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
            
            
            print("finished_09_beginning")
            
          } 
          
          if (j == max(2:nrow(data_models$Poll_dates_weekly_weighting[[i]]))){
            
            
            # Pull survey data for the last survey date entry in the current object    
            infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == last(data_models$Poll_dates_weekly_weighting[[i]][1])) %>% 
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
              summarise(perc = c(mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            # Get Google Proportion to calculate weighting factor with the previously drawn survey data (go to previous date in the object +1 day to last date in object)
            Model4_2_ending <-  data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j-1]+1 & date <= last(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
              group_by(keyword)%>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = hits_sum/sum(hits_sum)*100) %>%
              mutate(perc = ifelse(perc == 0,  infra_dimap_weekly_weighting$perc[keyword ==  infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            # apply weight to data ranging from last date in object + 1 day to entry in list "end_dates" including the end of our setted intervals
            Model4_4_ending <-  data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= last(data_models$Poll_dates_weekly_weighting[[i]][1])+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = hits_sum/hits_sum2*100) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc= Model4_2_ending$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
              mutate(perc = ifelse(perc == 1,  infra_dimap_weekly_weighting$perc[party ==  infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            
            data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_ending)
            
            
            print("finished_ending_09")
            
            # break the current iteration of the nested loop if the condition above is fulfilled (last entry reached of object) so that the code below is not executed and a new iteration of the upper loop is started
            break
            
          }
          
          if (between(j, 3, max(2:nrow(data_models$Poll_dates_weekly_weighting[[i]])-1))){
            
            print(j)
            
            # get the survey data of the current survey date (j) 
            infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>% 
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
              summarise(perc = c(mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            # Calculate the weighting for the period from one day after the survey before the survey of the current iteration to the survey of the current iteration in our objects and calculate the weighting by dividing the proportion by the survey date drawn in the previous code chunk      Model4_2 <-  df_final %>%
            Model4_2 <-  data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j-1]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
              group_by(keyword)%>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = hits_sum/sum(hits_sum)*100) %>%
              mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            # Get google proportion to be weighted i.e. one day after the survey of the current iteration in our objects to the next survey date entry in our objects and apply the previously calculated weight
            Model4_3 <- data_models$GT_data_weekly_weighting[[i]]  %>%
              filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j+1]) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = hits_sum/hits_sum2*100) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc= Model4_2$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
              mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            Model4_4 <- rbind(Model4_4, Model4_3)
            
            
          }
        }
      }
      
      
      if (!grepl("2009", data_models$model_name[i])){
        
        # start with 2:nrow since steps 1 & 2 are done in j == 2 
        for (j in 2:nrow(data_models$Poll_dates_weekly_weighting[[i]])){
          
          if (j == 2){
            
            # calculate weight for interval before first poll
            
            infra_dimap_first <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == first(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
              summarise(perc = c(mean(AFD, na.rm = TRUE), mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            Weighting_factor_beginning <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= as.Date(infra_dimap_all$Date[a], "%d.%m.%y")+1 & date <= first(data_models$Poll_dates_weekly_weighting[[i]][[1]][1])) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = hits_sum/sum(hits_sum)*100) %>%
              mutate(perc = ifelse(perc == 0, infra_dimap_first$perc[keyword == infra_dimap_first$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_beginning = ifelse(infra_dimap_first$perc == 1, 1, infra_dimap_first$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            Model4_4_beginning <-  data_models$GT_data_weekly_weighting[[i]] %>% 
              filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = hits_sum/hits_sum2*100) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc = Weighting_factor_beginning$weight_beginning*perc) %>%
              mutate(perc = ifelse(perc == 1, infra_dimap_first$perc[party == infra_dimap_first$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            # get the survey data for that is in first place in the objects
            infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
              summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            # calculate weight for period ranging from one day after survey that is before the first survey in our objects to first survey in objects and calculate weight by dividing the proportion by the survey date pulled in the previous code chunk
            Model4_3_first <-  data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= data_models$GT_start_date[i] & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = hits_sum/sum(hits_sum)*100) %>%
              mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            # Get google proportion to be weighted i.e. one day after the first survey in our objects to the second survey date entry in our objects and apply the previously calculated weight
            Model4_4_first <- data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j+1]) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = hits_sum/hits_sum2*100) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc= Model4_3_first$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
              mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            Model4_4 <- rbind(Model4_4, Model4_4_beginning, Model4_4_first)
            
            
            print("finished_131721_beginning")
            
          } 
          
          if (j == max(2:nrow(data_models$Poll_dates_weekly_weighting[[i]]))){
            
            
            # Pull survey data for the last survey date entry in the current object    
            infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == last(data_models$Poll_dates_weekly_weighting[[i]][1])) %>% 
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
              mutate_if(is.character, as.numeric) %>%
              summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            # Get Google Proportion to calculate weighting factor with the previously drawn survey data (go to previous date in the object +1 day to last date in object)
            Model4_2_ending <-  data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j-1]+1 & date <= last(data_models$Poll_dates_weekly_weighting[[i]][1])) %>%
              group_by(keyword)%>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = hits_sum/sum(hits_sum)*100) %>%
              mutate(perc = ifelse(perc == 0,  infra_dimap_weekly_weighting$perc[keyword ==  infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            # apply weight to data ranging from last date in object + 1 day to entry in list "end_dates" including the end of our setted intervals
            Model4_4_ending <-  data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= last(data_models$Poll_dates_weekly_weighting[[i]][1])+1 & date <= data_models$GT_end_date[i]) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = hits_sum/hits_sum2*100) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc= Model4_2_ending$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
              mutate(perc = ifelse(perc == 1,  infra_dimap_weekly_weighting$perc[party ==  infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            
            data_models$predicitons_GT_weekly_polls[[i]] <- rbind(Model4_4, Model4_4_ending)
            
            
            print("finished_ending_131721")
            
            # break the current iteration of the nested loop if the condition above is fulfilled (last entry reached of object) so that the code below is not executed and a new iteration of the upper loop is started
            break
            
          }
          
          if (between(j, 3, max(2:nrow(data_models$Poll_dates_weekly_weighting[[i]])-1))){
            
            print(j)
            
            # get the survey data of the current survey date (j) 
            infra_dimap_weekly_weighting <- data_models$Poll_dates_weekly_weighting[[i]] %>%
              filter(Date == data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>% 
              mutate_if(is.character, as.numeric) %>%
              replace(is.na(.), 1) %>% # takes into account the case that data for one of the parties is missing in the polls (occurs only 2013 for the AFD at certain points in time). In the case of missing data, this party is assigned the value 1. 
              summarise(perc = c(mean(AFD, na.rm=TRUE), mean(CDU), mean(FDP),
                                 mean(Grüne), mean(Linke), mean(SPD))) %>%
              mutate(party = c("AFD", "CDU", "FDP", "Grüne", "Linke", "SPD")) %>%
              select(party, perc)
            
            
            # Calculate the weighting for the period from one day after the survey before the survey of the current iteration to the survey of the current iteration in our objects and calculate the weighting by dividing the proportion by the survey date drawn in the previous code chunk      Model4_2 <-  df_final %>%
            Model4_2 <-  data_models$GT_data_weekly_weighting[[i]] %>%
              filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j-1]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]) %>%
              group_by(keyword)%>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(perc = hits_sum/sum(hits_sum)*100) %>%
              mutate(perc = ifelse(perc == 0, infra_dimap_weekly_weighting$perc[keyword == infra_dimap_weekly_weighting$party] , perc)) %>% # If a party has the value 0 in the Google Proportion, then replace the 0 with the value the party has in the poll. If the values of the survey are then divided by the Google Proportion, the weighting factor is 1 (no weighting takes place).
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(weight_weekly = ifelse(infra_dimap_weekly_weighting$perc == 1, 1, infra_dimap_weekly_weighting$perc/perc)) # make sure that if a party had no value in the poll, that party gets a weighting factor of 1 
            
            
            # Get google proportion to be weighted i.e. one day after the survey of the current iteration in our objects to the next survey date entry in our objects and apply the previously calculated weight
            Model4_3 <- data_models$GT_data_weekly_weighting[[i]]  %>%
              filter(date >= data_models$Poll_dates_weekly_weighting[[i]][[1]][j]+1 & date <= data_models$Poll_dates_weekly_weighting[[i]][[1]][j+1]) %>%
              group_by(keyword) %>%
              summarize(hits_sum = sum(hits)) %>%
              mutate(hits_sum2 = sum(hits_sum)) %>%
              mutate(hits_sum = ifelse(hits_sum == 0, hits_sum2/100, hits_sum)) %>% # If a party has only 0 values, then divide the sum of the total hits across all parties by 100, so that this party receives the value 1 in the calculation of the Google Proportion and can be weighted.
              mutate(perc = hits_sum/hits_sum2*100) %>%
              rename(party = keyword) %>%
              select(party, perc) %>%
              mutate(perc= Model4_2$weight_weekly * perc, perc=round(perc, digits = 2)) %>%
              mutate(perc = ifelse(perc == 1, infra_dimap_weekly_weighting$perc[party == infra_dimap_weekly_weighting$party] , perc)) # If the Google proportion for a party was 0 when calculating the weighting factor and the Google proportion is also 0 here when applying the weighting factor (perc = 1), then replace the value 1 with the value that the party has in the poll.
            
            
            Model4_4 <- rbind(Model4_4, Model4_3)
            
            
          }
        }
      }
    }
    
    # take the mean over the weighted intervals 
    mean_df <- as.data.frame(data_models$predicitons_GT_weekly_polls[i])
    
    mean_df2 <- mean_df %>%
      group_by(party) %>%
      summarise(prediction = mean(perc))
    
    # write the mean/results to the predictions column
    data_models$predicitons_GT_weekly_polls_mean[[i]] <- tibble(mean_df2)
    
    print(i)
    print("Nested data set with mean successful")
    
  }  
}












