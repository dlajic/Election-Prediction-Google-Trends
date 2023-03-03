# ADD AVERAGE
# data_plot1_average <- data_plot1 %>%
#   group_by(model_name) %>%
#   summarize(Mean_dev = mean(Mean_dev),
#             GT_end_date = first(GT_end_date),
#             model_time_interval_fac = first(model_time_interval_fac),
#             GT_start_date = first(GT_start_date),
#             model_time_distance = first(model_time_distance)) %>%
#   mutate(party = "Av.",
#          model_time_distance = as.duration(model_time_distance)) %>%
#   ungroup()
# data_plot1 <- data_plot1 %>%
#   select(model_name, GT_start_date, GT_end_date, model_time_distance, model_time_interval_fac, Mean_dev, party) %>%
#   bind_rows(data_plot1_average)


# cols <- c(cols, c("Av." = "darkgray"))
# linetypes <- c(linetypes, c("Av." = "dashed"))




## Add unique model name
#data_models <- data_models %>%
#mutate(model_name  = paste("M", 
#                           model_id, 
#                           year(election_date), 
#                           "int",
#                           as.character(time_length(model_time_interval, unit = "days")),
#                           "days",
#                           "dist",
#                           as.character(time_length(model_time_distance, unit = "days")),
#                           "days",
#                           gsub("\\s", "_", gsub("\\s+", " ", gsub("\\+", " ", data_models$datasource_weight))),
#                           sep = "_"))








# data_predictions$datasource_weight <-
#   factor(data_predictions$datasource_weight) %>%
#   mutate()




# now with more Datasets Confidence Intervals ###


# data_predictions$datasource_weight <-
#   factor(data_predictions$datasource_weight) %>%
#   mutate()
