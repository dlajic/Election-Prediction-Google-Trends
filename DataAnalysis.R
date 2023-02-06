library(pacman)
p_load(gtrendsR,
       ggplot2,
       tidyverse,
       tidyr,
       rvest,
       xml2,
       data.table,
       patchwork,
       lubridate,
       ajfhelpR,
       jsonlite,
       kableExtra,
       gt)


### For normal Data

setwd("Environments")
load("Env_Merged_syntax_NEU 2023-01-22_09-06-42 .RData")


#### delete 2005 rows (just needed for Model2_2009)

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


# TABLES ####


# Table 1: Search terms ####

data_models %>%
  select(model_name, election_date, GT_keywords) %>%
  group_by(election_date) %>%
  filter(row_number()==1) %>%
  select(election_date, GT_keywords) %>%
  mutate(GT_keywords = sapply(GT_keywords, paste, collapse = " + ")) %>%
  mutate(GT_keywords = str_replace_all(str_replace_all(GT_keywords, '"', ''), "c", "")) %>%
  rename("Date of election" = "election_date",
         "Final search queries" = "GT_keywords") %>%
  kable(format = "html",
        caption = "Table 1: Final search queries", 
        table.attr = "style = \"color: black;\"") %>%
  #kable_classic(full_width = F) %>%
  save_kable("../table_1_search_queries.html")





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

# Kann man nuch besser lösen ????????
#Sinn: summarize die oben genannten aber behalte andere Spalten wie datasource_weight etc.
data_predictions_final_mean <- merge(data_predictions_final_mean, data_predictions, by = c("model_name","party"))
data_predictions_final_mean <- data_predictions_final_mean %>% select(-c(20,21,22,23,24), -("df_id"))



# data_predictions$datasource_weight <-
#   factor(data_predictions$datasource_weight) %>%
#   mutate()


# GRAPHS ####



# now with more Datasets Confidence Intervals ###


# data_predictions$datasource_weight <-
#   factor(data_predictions$datasource_weight) %>%
#   mutate()


# Data for plots ####
data_plot <- data_predictions_final_mean %>%
  #filter(election_date=="2017-09-24"|election_date=="2013-09-22") %>% #can filter for better overview
  mutate(model_time_interval_fac = factor(as.numeric(model_time_interval, "days"))) %>% # convert to days
  mutate(model_time_interval_fac = paste(model_time_interval_fac, " days", sep="")) %>%
  mutate(model_time_distance = election_date - GT_end_date) %>%
  mutate(model_time_interval_fac = factor(model_time_interval_fac,
                                          levels = c("7 days", "14 days", "21 days", 
                                                     "28 days", "42 days", "56 days", 
                                                     "70 days", "77 days", "84 days", "91 days"),
                                          ordered = TRUE))


cols <- c("SPD" = "red", 
          "CDU" = "black", 
          "AFD" = "blue", 
          "FDP" = "orange", 
          "Linke" = "purple", 
          "Grüne" = "green")


linetypes <- c("SPD" = "solid", 
               "CDU" = "solid", 
               "AFD" = "solid", 
               "FDP" = "solid", 
               "Linke" = "solid", 
               "Grüne" = "solid")



# COMPARISON: DATA WINDOWS ####
## Figure 2 - less intervals ####
data_plot1 <- data_plot %>%
  filter(datasource_weight =="GT") %>%
  group_by(model_name) %>%
  mutate(group_mean_deviation = mean(abs(Mean_dev))) %>%
  filter(election_date == "2021-09-26") %>%
  filter(model_time_interval_fac == "7 days" |
         model_time_interval_fac == "14 days" |
         model_time_interval_fac == "28 days" |
           model_time_interval_fac == "91 days") %>% 
  ungroup()


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

# Create x-axis tick labels
x_tick_labels <- data_plot1 %>%
  group_by(GT_end_date) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  slice(c(1,25, 50, 75, 100, 125, 150)) %>%# Pick every 30th row
  select(GT_start_date, GT_end_date, model_time_distance)



p <- ggplot(data_plot1,
            aes(x = GT_end_date,
                y = Mean_dev,
                color = party#,
                #linetype = party
                )) +
  geom_vline(xintercept = as.Date("2021-09-26"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid",
             color = "lightgray") +  
  geom_point(size = 0.5) +
  geom_line() +
  theme_minimal(base_size = 22) +
  facet_grid(vars(model_time_interval_fac),
             #vars(election_date), 
             scales = "free_x") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_tick_labels$GT_end_date,
               labels = paste0(x_tick_labels$model_time_distance, " day(s)\n[", x_tick_labels$GT_end_date, "]")
  ) +
  scale_y_continuous(sec.axis = dup_axis(
    name = "Width of data window")) +
  scale_color_manual(values = cols) + 
  scale_linetype_manual(values = linetypes) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position="top",
        axis.text.y.right = element_blank()) +
  ylab("Deviation on % scale\n(prediction error)") +
  xlab("Distance of data window [end date of window]") +
  labs(colour = "Party")
# stat_summary(aes(y = group_mean_deviation, group = 1), fun=mean, colour="purple", geom="line", linetype = "dashed")
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))


p
ggsave(plot = p,
       filename = "../Figure_2_less_intervals.png", # e.g. change to pdf
       width = 14,
       height = 14,
       device = "png", # e.g. change to pdf
       dpi = 300)  



## Figure A1 (Figure 2 for appendix) ####
data_plot1 <- data_plot %>%
  filter(datasource_weight =="GT") %>%
  group_by(model_name) %>%
  mutate(group_mean_deviation = mean(abs(Mean_dev))) %>%
  filter(election_date == "2021-09-26")

# Create x-axis tick labels
x_tick_labels <- data_plot1 %>%
  group_by(GT_end_date) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  slice(c(1,25, 50, 75, 100, 125, 150)) %>%# Pick every 30th row
  select(GT_start_date, GT_end_date, model_time_distance)
  


p <- ggplot(data_plot1,
            aes(x = GT_end_date,
                y = Mean_dev,
                color = party)) +
  geom_vline(xintercept = as.Date("2021-09-26"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid") +  
  geom_point(size = 0.5) +
  geom_line() +
  theme_minimal(base_size = 22) +
  facet_grid(vars(model_time_interval_fac),
             #vars(election_date), 
             scales = "free_x") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_tick_labels$GT_end_date,
               labels = paste0(x_tick_labels$model_time_distance, " day(s)\n[", x_tick_labels$GT_end_date, "]")
  ) +
  scale_y_continuous(sec.axis = dup_axis(
    name = "Width of data window")) +
  scale_color_manual(values = cols) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position="top",
        axis.text.y.right = element_blank()) +
  ylab("Deviation on % scale\n(prediction error)") +
  xlab("Distance of data window [end date of window]") +
  labs(colour = "Party")
# stat_summary(aes(y = group_mean_deviation, group = 1), fun=mean, colour="purple", geom="line", linetype = "dashed")
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))


p
ggsave(plot = p,
       filename = "../Figure_A1_2_all_intervalls.png", # e.g. change to pdf
       width = 14,
       height = 14,
       device = "png", # e.g. change to pdf
       dpi = 300)  





# Figure 2-table: Party variation ####
# Summarize across parties (variation across models)
# Table basically shows for which models + parties there is the highest variation
# of predictions across difference distances
data_plot1_party <- data_plot1 %>% 
  group_by(party, model_time_interval) %>%
  select(party, model_time_interval, Mean_dev) %>%
  summarize(Mean_dev_party = mean(abs(Mean_dev)), # What should we use here? absolute?
            Mean_dev_party_sd = sd(Mean_dev),
            Mean_dev_party_max = max(Mean_dev), 
            Mean_dev_party_min = min(Mean_dev)) %>%
  arrange(desc(Mean_dev_party_sd), party)



## Figure A2 (Figure 2-appendix) ####

data_plot2 <- data_plot %>%
  filter(datasource_weight =="GT") %>%
  group_by(model_name) %>%
  mutate(group_mean_deviation = mean(abs(Mean_dev))) %>%
  filter(election_date != "2021-09-26")

# Create x-axis tick labels

x_tick_labels <- data_plot2 %>%
  #filter(election_date == "2017-09-24") %>% # Just use one election
  group_by(election_date, GT_end_date) %>%
  filter(row_number()==1) %>%
  group_by(election_date) %>%
  slice(c(1,50,  100, 150)) %>%# Pick every 30th row
  ungroup() %>%
  select(GT_start_date, GT_end_date, model_time_distance)



p <- ggplot(data_plot2,
            aes(x = GT_end_date,
                y = Mean_dev,
                color = party)) +
  geom_vline(xintercept = as.Date("2017-09-24"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2013-09-22"),
             linetype="dashed") +
  geom_vline(xintercept = as.Date("2009-09-27"),
             linetype="dashed") +
  geom_hline(yintercept = 0,
             linetype="solid") +  
  geom_point(size = 0.5) +
  geom_line() +
  theme_minimal(base_size = 22) +
  facet_grid(vars(model_time_interval_fac),
             vars(election_date), 
             scales = "free_x") +
  scale_x_date(breaks = x_tick_labels$GT_end_date,
               labels = paste0(x_tick_labels$model_time_distance, " day(s)\n[", x_tick_labels$GT_end_date, "]")
  ) +
  scale_y_continuous(sec.axis = dup_axis(
    name = "Width of data window")) +
  scale_color_manual(values = cols) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="top",
        axis.text.y.right = element_blank()) +
  ylab("Deviation on % scale\n(prediction error)") +
  xlab("Distance of data window [end date of window]") +
  labs(colour = "Party")
# stat_summary(aes(y = group_mean_deviation, group = 1), fun=mean, colour="purple", geom="line", linetype = "dashed")
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))


p
ggsave(plot = p,
       filename = "../Figure_A2_figure2_appendix.png", # e.g. change to pdf
       width = 14,
       height = 14,
       device = "png", # e.g. change to pdf
       dpi = 300)  



# Modelling distance/width/elections ####

# Figure A1-2-table: Trends ####
# LM for all GT models BUT ONLY 14 days

# First model distance
data_modelling <- data_plot %>%
  filter(datasource_weight =="GT") %>%
  mutate(Mean_dev_absolute = abs(Mean_dev),
         model_time_distance_num = abs(as.numeric(model_time_distance)-150))  %>%
  filter(model_time_interval_fac == "14 days")

# model_time_distance_num: Distance reversed so that higher values are lower distance!
# Check: View(data_plot1 %>% select(model_time_distance, model_time_distance_num))# 
# View(data_plot1 %>% select(model_time_interval_fac, model_time_interval_fac_num))

M1 <- lm(Mean_dev_absolute ~ model_time_distance_num, data = data_modelling)
M2 <- lm(Mean_dev_absolute ~ model_time_distance_num, data = data_modelling %>% 
           filter(election_date=="2009-09-27"))
M3 <- lm(Mean_dev_absolute ~ model_time_distance_num, data = data_modelling %>% 
           filter(election_date=="2013-09-22"))
M4 <- lm(Mean_dev_absolute ~ model_time_distance_num, data = data_modelling %>% 
           filter(election_date=="2017-09-24"))
M5 <- lm(Mean_dev_absolute ~ model_time_distance_num, data = data_modelling %>% 
           filter(election_date=="2021-09-26"))

library(modelsummary)
models <- list("M1 (all elections)" = M1, 
               "M2 (2009-09-27)" = M2, 
               "M3 (2013-09-22)" = M3, 
               "M4 (2017-09-24)" = M4, 
               "M5 (2021-09-26)" = M5)


library(gt)
# additionally we want to change the font, font size and spacing
modelsummary(models,
             title = 'Linear regression',
             output = 'gt',
             notes = "Notes: Outcome is the absolute mean deviation across parties.",
             estimate  = "{estimate} [{conf.low}, {conf.high}]",
             statistic = "{std.error} ({p.value}){stars}") %>%
  tab_spanner(label = 'Dependent variable: Deviations (absolute)', columns=2:6) %>%
  tab_options(
    table.font.size = 10,
    data_row.padding = px(1),
    table.border.top.color = "white",
    heading.border.bottom.color = "black",
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "white",
    table.border.bottom.color = "white",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    table_body.hlines.color = "white"
  )  %>% gtsave("../tabA1.docx")














# COMPARISON: Models ####
# Figure 3 ####
# prediction error averaged across all parties for the GT data + other datasources

# Create average prediction error (across all parties) ###
###Plot GT vs. Polls

data_plot2 <- data_plot %>%
  filter(datasource_weight =="GT" | 
           #datasource_weight =="Only polls" | 
           datasource_weight =="Last polls" |
           datasource_weight =="GT + election weight" |
           datasource_weight =="GT + weekly polls weight"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE)) %>%
  filter(election_date == "2021-09-26") %>%
  filter(model_time_interval_fac == "7 days" |
           model_time_interval_fac == "14 days" |
           model_time_interval_fac == "28 days" |
           model_time_interval_fac == "91 days") %>% 
  ungroup() %>%
  mutate(datasource_weight = recode(datasource_weight, 
                                    "GT" = "MC1: GT",
                                    "GT + election weight" = "MC2: GT + election weight",
                                    "GT + weekly polls weight" = "MC3: GT + weekly polls weight"))

#  WHY NOT AGGREGATE DATASET?

# Create x-axis tick labels
x_tick_labels <- data_plot2 %>%
  group_by(GT_end_date) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  slice(c(1,25, 50, 75, 100, 125, 150)) %>%# Pick every 30th row
  select(GT_start_date, GT_end_date, model_time_distance)


# Number of models
# data_plot2 %>% group_by(datasource_weight) %>% summarize(n_models = n())

# Create x-axis tick labels
# 
# x_tick_labels <- data_plot2 %>%
#   group_by(GT_end_date) %>%
#   filter(row_number()==1) %>%
#   ungroup() %>%
#   slice(c(1,25, 50, 75, 100, 125, 150)) %>%# Pick every 30th row
#   select(GT_start_date, GT_end_date, model_time_distance)


cols2 <- c("MC1: GT" = "#e41a1c", 
           #"Only polls" = "black", 
           "Last polls" = "black",
           "MC2: GT + election weight" = "#984ea3",
           "MC3: GT + weekly polls weight" = "#ff7f00")

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
  geom_point(size = 0.5) +
  geom_line() +
  theme_minimal(base_size = 22) +
  facet_grid(vars(model_time_interval_fac),
             #vars(election_date), 
             scales = "free_x") +
  scale_x_date(breaks = x_tick_labels$GT_end_date,
               labels = paste0(x_tick_labels$model_time_distance, " day(s)\n[", x_tick_labels$GT_end_date, "]")
  ) +
  scale_y_continuous(sec.axis = dup_axis(
    name = "Width of data window")) +
  scale_color_manual(values = cols2) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position="top",
        axis.text.y.right = element_blank()) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "Model class")
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))



p2

ggsave(plot = p2,
       filename = "../Figure_3_model_comparison.png", # e.g. change to pdf
       width = 14,
       height = 14,
       device = "png", # e.g. change to pdf
       dpi = 300)  

# WEITER HIER!


# Figure 4 ####
#### Plot average Deviation all Models
data_plot3 <- data_plot %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev) , na.rm=TRUE))

cols3 <- c("GT" = "red", "Only polls" = "black", "GT + election weight" = "purple", 
           "GT + polls weight" = "orange", "GT + weekly polls weight" = "green", "Last polls" = "blue")


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
  theme_minimal(base_size = 18) +
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
  scale_color_manual(values = cols3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "datasource_weight") +
  ylim(0,10)
#+geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))

p3

ggsave(plot = p3,
       filename = "../Figure_3_deviation_all_models.png", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "png", # e.g. change to pdf
       dpi = 300)  




# Figure 5 ####

##### comparison weekly weighting
data_plot4 <- data_plot %>%
  group_by(model_name) %>% 
  filter(datasource_weight =="GT" | datasource_weight =="Only polls" | datasource_weight =="Last polls" | datasource_weight =="GT + weekly polls weight"
  ) %>%
  mutate(deviation_mean = mean(abs(Mean_dev) , na.rm=TRUE))

cols4 <- c("GT" = "red", "Only polls" = "black", "Last polls" = "blue", "GT + weekly polls weight" = "green")


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
  theme_minimal(base_size = 18) +
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
  scale_color_manual(values = cols4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "datasource_weight")
#+geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))

p4


ggsave(plot = p3,
       filename = "../Figure_4_meanDeviations_CompareWeekly.png", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "png", # e.g. change to pdf
       dpi = 300)  






# Without Category ####

setwd("C:/Users/deanl/Desktop/UniMannheim/ComSocScience/Publikation/Election-Prediction-Google-Trends/Environments")
load("Env_Merged_syntax_WC 2023-01-22_14-09-15 .RData")


#### delete 2005 rows (just needed for Model2_2009)

data_models_WC <- data_models

data_models_WC <- data_models_WC %>% 
  filter(grepl("M_\\d+_2005", data_models_WC$model_name) == FALSE)

#And again renaming since first id´s are missing (deleted 2005ÃÂ´s)
## Add model index/number ####
data_models_WC <- data_models_WC %>% 
  mutate(model_id = row_number()) %>%
  select(model_id, everything())

# Reorder
data_models_WC <- data_models_WC %>%
  select(model_id, model_name, everything())

## Add unique model name ####
#data_models_WC <- data_models_WC %>%
#mutate(model_name  = paste("M", 
#                           model_id, 
#                           year(election_date), 
#                           "int",
#                           as.character(time_length(model_time_interval, unit = "days")),
#                           "days",
#                           "dist",
#                           as.character(time_length(model_time_distance, unit = "days")),
#                           "days",
#                           gsub("\\s", "_", gsub("\\s+", " ", gsub("\\+", " ", data_models_WC$datasource_weight))),
#                           sep = "_"))


# Reorder
data_models_WC <- data_models_WC %>%
  select(model_id, model_name, everything())


#### delete 2005 rows (just needed for Model2_2009)

data_predictions_final_WC <- data_predictions_final

data_predictions_final_WC <- data_predictions_final_WC %>% 
  filter(grepl("M_\\d+_2005", data_predictions_final_WC$model_name) == FALSE)

#And again renaming since first id´s are missing (deleted 2005ÃÂ´s)
## Add model index/number ####
data_predictions_final_WC <- data_predictions_final_WC %>% 
  mutate(model_id = row_number()) %>%
  select(model_id, everything())



# CLEAN data_predictions ####
data_predictions_WC <- data_predictions

data_predictions_WC <- data_predictions_WC %>%
  filter(grepl("M_\\d+_2005", model_name) == FALSE) %>% # filter out models without predictions
  group_by(model_name) %>%
  mutate(model_id = cur_group_id()) %>% # Add new model_id (after filtering)
  ungroup()
#
#save(data_predictions_final_mean, file = "data_predictions_final_mean_Dean.RData")



# Graphs ####

# Prepare data for graph
data_predictions_WC$party <-
  as.factor(ordered(data_predictions_WC$party,
                    levels = c("Grüne", "Linke", "FDP", "AFD", "SPD", "CDU")))
data_predictions_WC$deviation_label <-
  round(data_predictions_WC$deviation, 1)

#for Confidence Intervals
data_predictions_final_WC <- data_predictions_final

data_predictions_final_mean_WC <- data_predictions_final_WC %>%
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
data_predictions_final_mean_WC <- merge(data_predictions_final_mean_WC, data_predictions_WC, by = c("model_name","party"))
data_predictions_final_mean_WC <- data_predictions_final_mean_WC %>% select(-c(20,21,22,23,24), -("df_id"))



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
data_plot_WC <- data_predictions_final_mean_WC %>%
  #filter(election_date=="2017-09-24"|election_date=="2013-09-22") %>% #can filter for better overview
  mutate(model_time_interval_fac = factor(as.numeric(model_time_interval, "days"))) %>% # convert to days
  mutate(model_time_interval_fac = paste("Interval: ", model_time_interval_fac, " days", sep="")) %>%
  mutate(model_time_distance = election_date - GT_end_date)


data_plot_WC$model_time_interval_fac <- factor(data_plot_WC$model_time_interval_fac,
                                            levels = c("Interval: 7 days", "Interval: 14 days", "Interval: 21 days", 
                                                       "Interval: 28 days", "Interval: 42 days", "Interval: 56 days", 
                                                       "Interval: 70 days", "Interval: 77 days", "Interval: 84 days", "Interval: 91 days"),
                                            ordered = TRUE)

x_breaks <- unique(data_plot_WC$GT_end_date)[seq(1, length(unique(data_plot_WC$GT_end_date)), 10)]
x_labels_distance <- unique(data_plot_WC$model_time_distance)[seq(1, length(unique(data_plot_WC$model_time_distance)), 10)]
x_labels_date <- unique(data_plot_WC$GT_end_date)[seq(1, length(unique(data_plot_WC$GT_end_date)), 10)]

cols <- c("SPD" = "red", "CDU" = "black", "AFD" = "blue", 
          "FDP" = "orange", "Linke" = "purple", "Grüne" = "green")


######### Plot1
data_plot_WC1 <- data_plot_WC %>%
  filter(datasource_weight =="GT") %>%
  group_by(model_name) %>%
  mutate(group_mean_deviation = mean(abs(Mean_dev)))

p <- ggplot(data_plot_WC1,
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
  # xlim(min(data_plot_WC$GT_end_date) - 1, as.Date("2021-09-26")+1) +
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
       filename = "plot_predictions_GT_parties_WC.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)  


######################### Create average prediction error (across all parties) ########################
###Plot GT vs. Polls

data_plot_WC2 <- data_plot_WC %>%
  filter(datasource_weight =="GT" | datasource_weight =="Only polls" | datasource_weight =="Last polls"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE))

cols2 <- c("GT" = "red", "Only polls" = "black", "Last polls" = "blue")

#Plot GT vs. Polls
p2 <- ggplot(data_plot_WC2,
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
             scales = "free_x") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot_WC$GT_end_date) - 1, as.Date("2021-09-26")+1) +
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


p2

ggsave(plot = p2,
       filename = "plot_moreSamp_meanDeviations_GT_polls_WC.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)


#### Plot average Deviation all Models
data_plot_WC3 <- data_plot_WC %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev) , na.rm=TRUE))

cols3 <- c("GT" = "red", "Only polls" = "black", "GT + election weight" = "purple", 
           "GT + polls weight" = "orange", "GT + weekly polls weight" = "green", "Last polls" = "blue")


p3 <- ggplot(data_plot_WC3,
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
             scales = "free_x") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot_WC$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_breaks,
               labels = paste("Distance: ",  x_labels_distance, " day(s)\n",
                              "Date: ", x_labels_date
               )
  ) +
  scale_color_manual(values = cols3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "datasource_weight") +
  ylim(0,10)
#+geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))

p3

ggsave(plot = p3,
       filename = "plot_meanDeviations_allModels_WC.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)



##### comparison weekly weighting
data_plot_WC4 <- data_plot_WC %>%
  group_by(model_name) %>% 
  filter(datasource_weight =="GT" | datasource_weight =="Only polls" | datasource_weight =="Last polls" | datasource_weight =="GT + weekly polls weight"
  ) %>%
  mutate(deviation_mean = mean(abs(Mean_dev) , na.rm=TRUE))

cols4 <- c("GT" = "red", "Only polls" = "black", "Last polls" = "blue", "GT + weekly polls weight" = "green")


p4 <- ggplot(data_plot_WC4,
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
             scales = "free_x") +
  #facet_wrap(~model_time_interval_fac, ncol = 1) +
  # xlim(min(data_plot_WC$GT_end_date) - 1, as.Date("2021-09-26")+1) +
  scale_x_date(breaks = x_breaks,
               labels = paste("Distance: ",  x_labels_distance, " day(s)\n",
                              "Date: ", x_labels_date
               )
  ) +
  scale_color_manual(values = cols4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("MeanDeviation in %\n(prediction error)") +
  xlab("Enddate of interval\n(= distance)") +
  labs(colour = "datasource_weight")
#+geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))

p4

ggsave(plot = p4,
       filename = "plot_meanDeviations_CompareWeekly_WC.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)






#### plot GT WC/sonst/normal #########

data_plot20 <- data_plot %>%
  filter(datasource_weight =="GT" | datasource_weight =="Only polls" | datasource_weight =="Last polls"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE))

data_plot_sonst20 <- data_plot_sonst %>%
  filter(datasource_weight =="GT"
  ) %>%
  mutate(datasource_weight = "GTsonst") %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE))

data_plot_WC20 <- data_plot_WC %>%
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
  geom_line(data=data_plot_sonst20, aes(x = GT_end_date, y = deviation_mean, color = datasource_weight)) +
  geom_line(data= data_plot20,aes(x = GT_end_date,y = deviation_mean, color = datasource_weight)) +
  #geom_line(data= data_plot_WC20,aes(x = GT_end_date,y = deviation_mean, color = datasource_weight)) +
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
       filename = "plot_Compare_GT_polls.pdf", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "pdf", # e.g. change to pdf
       dpi = 300)







