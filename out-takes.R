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