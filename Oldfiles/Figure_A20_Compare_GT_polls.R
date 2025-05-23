library(pacman)
p_load(gtrendsR,
       ggplot2,
       tidyverse,
       tidyr)

# Load data ####
load(file  = "./Environments/Env_Merged_syntax_NEU_all_Polls.RData")


#######
#### delete 2005 rows (just needed for Model2_2009)
#######
data_models <- data_models %>% 
  filter(grepl("M_\\d+_2005", data_models$model_name) == FALSE)

#And again renaming since first id´s are missing (deleted 2005ÃÂ´s)
## Add model index/number ####
data_models <- data_models %>% 
  mutate(model_id = row_number()) %>%
  select(model_id, everything())

# Reorder
data_models <- data_models %>%
  select(model_id, model_name, everything())

## Add unique model name ####
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


# Reorder
data_models <- data_models %>%
  select(model_id, model_name, everything())


#### delete 2005 rows (just needed for Model2_2009)
#######
data_predictions_final <- data_predictions_final %>% 
  filter(grepl("M_\\d+_2005", data_predictions_final$model_name) == FALSE)

#And again renaming since first id´s are missing (deleted 2005ÃÂ´s)
## Add model index/number ####
data_predictions_final <- data_predictions_final %>% 
  mutate(model_id = row_number()) %>%
  select(model_id, everything())



# CLEAN data_predictions ####

data_predictions <- data_predictions %>%
  filter(grepl("M_\\d+_2005", model_name) == FALSE) %>% # filter out models without predictions
  group_by(model_name) %>%
  mutate(model_id = cur_group_id()) %>% # Add new model_id (after filtering)
  ungroup()
#
#save(data_predictions_final_mean, file = "data_predictions_final_mean_Dean.RData")



# Graphs ####

# Prepare data for graph
data_predictions$party <-
  as.factor(ordered(data_predictions$party,
                    levels = c("Grüne", "Linke", "FDP", "AFD", "SPD", "CDU")))
data_predictions$deviation_label <-
  round(data_predictions$deviation, 1)

#for Confidence Intervals

data_predictions_final_mean <- data_predictions_final %>%
  group_by( model_name, party) %>%
  summarise(Mean_dev = mean(deviation), SD_dev = sd(deviation), Mean = mean(prediction), SD = sd(prediction), 
            .groups = "keep") %>%
  mutate(mean_lower.ci = Mean - 1.96*(SD/sqrt(n())),
         mean_upper.ci = Mean + 1.96*(SD/sqrt(n())),
         dev_lower.ci = Mean_dev - 1.96*(SD_dev/sqrt(n())),
         dev_upper.ci = Mean_dev + 1.96*(SD_dev/sqrt(n())),) #%>%
# replace_na(.,0)

######### Kann man nuch besser lösen ????????  ################
#Sinn: summarize die oben genannten aber behalte andere Spalten wie datasource_weight etc.
data_predictions_final_mean <- merge(data_predictions_final_mean, data_predictions, by = c("model_name","party"))
data_predictions_final_mean <- data_predictions_final_mean %>% select(-c(20,21,22,23,24), -("df_id"))



# data_predictions$datasource_weight <-
#   factor(data_predictions$datasource_weight) %>%
#   mutate()



## Figure X ####

############################################################################################
############################## now with more Datasets Confidence Intervals #################

#In Progress

# Graphs ####
# data_predictions$datasource_weight <-
#   factor(data_predictions$datasource_weight) %>%
#   mutate()


## Figure X ####
# predictions for different distances #
data_plot <- data_predictions_final_mean %>%
  #filter(election_date=="2017-09-24"|election_date=="2013-09-22") %>% #can filter for better overview
  mutate(model_time_interval_fac = factor(as.numeric(model_time_interval, "days"))) %>% # convert to days
  mutate(model_time_interval_fac = paste("Interval: ", model_time_interval_fac, " days", sep="")) %>%
  mutate(model_time_distance = election_date - GT_end_date)


data_plot$model_time_interval_fac <- factor(data_plot$model_time_interval_fac,
                                            levels = c("Interval: 7 days", "Interval: 14 days", "Interval: 21 days", 
                                                       "Interval: 28 days", "Interval: 42 days", "Interval: 56 days", 
                                                       "Interval: 70 days", "Interval: 77 days", "Interval: 84 days", "Interval: 91 days"),
                                            ordered = TRUE)

x_breaks <- unique(data_plot$GT_end_date)[seq(1, length(unique(data_plot$GT_end_date)), 10)]
x_labels_distance <- unique(data_plot$model_time_distance)[seq(1, length(unique(data_plot$model_time_distance)), 10)]
x_labels_date <- unique(data_plot$GT_end_date)[seq(1, length(unique(data_plot$GT_end_date)), 10)]

cols <- c("SPD" = "red", "CDU" = "black", "AFD" = "blue", 
          "FDP" = "orange", "Linke" = "purple", "Grüne" = "green")


######### Plot1
data_plot1 <- data_plot %>%
  filter(datasource_weight =="GT") %>%
  group_by(model_name) %>%
  mutate(group_mean_deviation = mean(abs(Mean_dev)))

p <- ggplot(data_plot1,
            aes(x = GT_end_date,
                y = Mean_dev,
                color = party)) +
  geom_vline(xintercept = as.Date("2021-09-26"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2017-09-24"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2013-09-22"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2009-09-27"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid") +  
  #geom_point(size = 0.5) +
  geom_line() +
  theme_minimal() +
  facet_grid(vars(model_time_interval_fac),
             vars(election_date), 
             scales = "free_x") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_breaks,
               labels = paste("Distance: ",  x_labels_distance, " day(s)\n",
                              "Date: ", x_labels_date
               )
  ) +
  scale_color_manual(values = cols) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Deviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "Party")
# stat_summary(aes(y = group_mean_deviation, group = 1), fun=mean, colour="purple", geom="line", linetype = "dashed")
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))


p
ggsave(plot = p,
       filename = "plot_predictions_GT_parties.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)  


######################### Create average prediction error (across all parties) ########################
###Plot GT vs. Polls

data_plot2 <- data_plot %>%
  filter(datasource_weight =="GT" | datasource_weight =="Last polls" |
         datasource_weight =="Allens" | datasource_weight =="Forsa" | datasource_weight =="Kantar" |
         datasource_weight == "FGW"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE))

cols2 <- c("GT" = "red", "Last polls" = "blue",
           "Allens" = "darkmagenta", "Forsa" = "darkgoldenrod1", "Kantar" = "cyan", "FGW" = "chartreuse")

#Plot GT vs. Polls
p2 <- ggplot(data_plot2,
             aes(x = GT_end_date,
                 y = deviation_mean,
                 color = datasource_weight)) +
  geom_vline(xintercept = as.Date("2021-09-26"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2017-09-24"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2013-09-22"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2009-09-27"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid") +  
  #geom_point(size = 0.5) +
  geom_line() +
  theme_minimal() +
  facet_grid(vars(model_time_interval_fac),
             vars(election_date), 
             scales = "free") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_breaks,
               labels = paste("Distance: ",  x_labels_distance, " day(s)\n",
                              "Date: ", x_labels_date
               )
  ) +
  scale_color_manual(labels = c("GT", "Infratest Dimap", "Forsa", "Kantar", "Forschungsgruppe Wahlen", "IfD Allensbach"), values = cols2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "Datasource") + 
  theme(legend.position="top")
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))


p2

ggsave(plot = p2,
       filename = "plot_moreSamp_meanDeviations_GT_polls.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)


#### Plot average Deviation all Models
data_plot3 <- data_plot %>%
  filter(datasource_weight =="GT" | datasource_weight == "GT + election weight" | datasource_weight =="GT + weekly polls weight" | datasource_weight =="Last polls" |
           datasource_weight =="Forsa" | datasource_weight =="Kantar" | datasource_weight == "FGW" | datasource_weight =="Allens"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev) , na.rm=TRUE))

cols3 <- c("GT" = "red", "GT + election weight" = "purple",  "GT + weekly polls weight" = "green", 
           "Last polls" = "blue", "Forsa" = "sienna1", "Kantar" = "yellow3", "FGW" = "violetred", "Allens" = "peachpuff")


p3 <- ggplot(data_plot3,
             aes(x = GT_end_date,
                 y = deviation_mean,
                 color = datasource_weight)) +
  geom_vline(xintercept = as.Date("2021-09-26"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2017-09-24"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2013-09-22"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2009-09-27"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid") +  
  #geom_point(size = 0.5) +
  geom_line() +
  theme_minimal() +
  facet_grid(vars(model_time_interval_fac),
             vars(election_date), 
             scales = "free") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_breaks,
               labels = paste("Distance: ",  x_labels_distance, " day(s)\n",
                              "Date: ", x_labels_date
               )
  ) +
  scale_color_manual(labels = c("GT", "GT + election weight", "GT + weekly polls weight", "Infratest Dimap", "Forsa", "Kantar", "Forschungsgruppe Wahlen", "IfD Allensbach"),values = cols3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "Datasource") + 
  ylim(0,10) + 
  theme(legend.position="top")
#+geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))

p3

ggsave(plot = p3,
       filename = "plot_meanDeviations_allModels.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)



##### comparison weekly weighting
data_plot4 <- data_plot %>%
  group_by(model_name) %>% 
  filter(datasource_weight =="GT" | datasource_weight =="GT + weekly polls weight" | datasource_weight =="Last polls" |
           datasource_weight =="Forsa" | datasource_weight =="Kantar" | datasource_weight == "FGW" | datasource_weight =="Allens"
  ) %>%
  mutate(deviation_mean = mean(abs(Mean_dev) , na.rm=TRUE))

cols4 <- c("GT" = "red", "GT + weekly polls weight" = "green", 
           "Last polls" = "blue", "Forsa" = "sienna1", "Kantar" = "yellow3", "FGW" = "violetred", "Allens" = "peachpuff")


p4 <- ggplot(data_plot4,
             aes(x = GT_end_date,
                 y = deviation_mean,
                 color = datasource_weight)) +
  geom_vline(xintercept = as.Date("2021-09-26"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2017-09-24"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2013-09-22"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2009-09-27"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid") +  
  #geom_point(size = 0.5) +
  geom_line() +
  theme_minimal() +
  facet_grid(vars(model_time_interval_fac),
             vars(election_date), 
             scales = "free") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_breaks,
               labels = paste("Distance: ",  x_labels_distance, " day(s)\n",
                              "Date: ", x_labels_date
               )
  ) +
  scale_color_manual(labels = c("GT", "GT + weekly polls weight", "Infratest Dimap", "Forsa", "Kantar", "Forschungsgruppe Wahlen", "IfD Allensbach"), values = cols4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "datasource_weight") + 
  ylim(0,10)+ 
  theme(legend.position="top")
#+geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))

p4

ggsave(plot = p4,
       filename = "plot_meanDeviations_CompareWeekly.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)






data_plot_sonst <- readRDS("./Data_plots/data_plot_sonst.RDS") # Is that right?
data_plot_WC <- readRDS("./Data_plots/data_plot_WC.RDS")


#### plot GT WC/sonst/normal #########

data_plot2 <- data_plot %>%
  filter(datasource_weight =="GT" | datasource_weight =="Only polls" | datasource_weight =="Last polls"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE))

data_plot_sonst2 <- data_plot_sonst %>%
  filter(datasource_weight =="GT"
  ) %>%
  mutate(datasource_weight = "GTsonst") %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE))

data_plot_WC2 <- data_plot_WC %>%
  filter(datasource_weight =="GT"
  ) %>%
  mutate(datasource_weight = "GTWC") %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE))


cols2 <- c("GT" = "red", "Only polls" = "black", "Last polls" = "blue", "GTsonst" = "Green",  "GTWC" = "Orange")

#Plot GT vs. Polls
p5 <- ggplot() +
  geom_vline(xintercept = as.Date("2021-09-26"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2017-09-24"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2013-09-22"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2009-09-27"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid") +  
  #geom_point(size = 0.5) +
  geom_line(data=data_plot_sonst2, aes(x = GT_end_date, y = deviation_mean, color = datasource_weight)) +
  geom_line(data= data_plot2,aes(x = GT_end_date,y = deviation_mean, color = datasource_weight)) +
  #geom_line(data= data_plot_WC2,aes(x = GT_end_date,y = deviation_mean, color = datasource_weight)) +
  theme_minimal() +
  facet_grid(vars(model_time_interval_fac),
             vars(election_date), 
             scales = "free_x") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_breaks,
               labels = paste("Distance: ",  x_labels_distance, " day(s)\n",
                              "Date: ", x_labels_date
               )
  ) +
  scale_color_manual(values = cols2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "datasource_weight")
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))


p5

ggsave(plot = p5,
       filename = "Figure_A20_Compare_GT_polls.png", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "png", # e.g. change to pdf
       dpi = 550)



