
# Load packages ####
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
       #ajfhelpR,
       jsonlite,
       kableExtra,
       gt,
       grid,
       ggh4x)






# Figure 1: Search terms ####

# Collect data
# terms_CDU <- gtrends(keyword=c("CDU", "CSU", "Angela Merkel", "Christlich demokratische Union","Armin Laschet"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")
# terms_Linke <- gtrends(keyword=c("Linke", "Die Linke","Die Linken"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")
# save(terms_CDU, file = paste0("termsCDU_", gsub(":|\\s", "_", Sys.time()), ".RData"))
# save(terms_Linke, file = paste0("termsLinke_", gsub(":|\\s", "_", Sys.time()), ".RData"))

# Load data
load(file = "termsCDU_2023-02-27_11_16_30.RData")
load(file = "termsLinke_2023-02-27_11_16_34.RData")



######### Search terms + period
#Bundestagswahlen: 26.09.2021; 24.09.2017; 22.09.2013; 27.09.2009; 18.09.2005
# Specifying vertical X intercepts for election dates
elec_vlines <- as.Date(c("2009-09-27", "2013-09-18", "2017-09-24", "2021-09-26")) # "2005-09-18", 

# CDU

terms_CDU_df <- terms_CDU$interest_over_time
terms_CDU_df <- terms_CDU_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

###Used in Paper
p1 <- ggplot(terms_CDU_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=1)  +
  scale_y_continuous(breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year",
               limits = as.Date(c("2006-01-01", "2021-12-01"))) +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, length(elec_vlines)),
           angle = 90,
           hjust = 1,
           size = 5)





# Linke
terms_Linke_df <- terms_Linke$interest_over_time
terms_Linke_df <- terms_Linke_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

###Used in Paper
p2 <- ggplot(terms_Linke_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=1)  +
  scale_y_continuous(breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year",
               limits = as.Date(c("2006-01-01", "2021-12-01"))) +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, length(elec_vlines)),
           angle = 90,
           hjust = 1,
           size = 5)



p1 + p2 + 
  plot_layout(ncol = 1) + 
  plot_layout(guides="collect")


result <- p1 + p2 + 
  plot_layout(ncol = 1)
gt <- patchwork::patchworkGrob(result)
plot_searchterms <- gridExtra::grid.arrange(gt,
                                            bottom=textGrob("Date", gp=gpar(fontsize=22)), 
                                            left=textGrob("Searches (100 = max. interest in time period/territory)", gp=gpar(fontsize=22), rot=90))
ggsave(plot = plot_searchterms,
       filename = "Figure_1_searchterms.png", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "png", # e.g. change to pdf
       dpi = 600) 




# Figure Graph across all search terms/parties
















# LOAD DATA (WOC) ####
### For normal Data
load(file  = "./Environments/Env_Merged_syntax_NEU 2023-01-22_09-06-42.RData")

# Delete unneccessary objects
rm(list=setdiff(ls(), c("data_models", "data_predictions_final", "data_predictions"))) 


## Data management ####
# delete 2005 rows (just needed for Model2_2009)
data_models <- data_models %>% 
  filter(grepl("M_\\d+_2005", data_models$model_name) == FALSE)

#And again renaming since first id´s are missing (deleted 2005ÃÂ´s)
## Add model index/number
data_models <- data_models %>% 
  mutate(model_id = row_number()) %>%
  select(model_id, everything())

# Reorder
data_models <- data_models %>%
  select(model_id, model_name, everything())


# Reorder
data_models <- data_models %>%
  select(model_id, model_name, everything())


#### delete 2005 rows (just needed for Model2_2009)

data_predictions_final <- data_predictions_final %>% 
  filter(grepl("M_\\d+_2005", data_predictions_final$model_name) == FALSE)

#And again renaming since first id´s are missing (deleted 2005ÃÂ´s)
## Add model index/number
data_predictions_final <- data_predictions_final %>% 
  mutate(model_id = row_number()) %>%
  select(model_id, everything())



# CLEAN data_predictions
data_predictions <- data_predictions %>%
  filter(grepl("M_\\d+_2005", model_name) == FALSE) %>% # filter out models without predictions
  group_by(model_name) %>%
  mutate(model_id = cur_group_id()) %>% # Add new model_id (after filtering)
  ungroup()



## Table 1: Search terms ####

data_models %>%
  select(model_name, election_date, GT_keywords) %>%
  group_by(election_date) %>%
  filter(row_number()==1) %>%
  select(election_date, GT_keywords) %>%
  mutate(GT_keywords = sapply(GT_keywords, paste, collapse = " + ")) %>%
  mutate(GT_keywords = str_replace_all(str_replace_all(GT_keywords, '"', ''), "c\\(", "\\(")) %>%
  mutate(election_date = paste0("Election: ", election_date)) %>%
  rename("Date of election" = "election_date",
         "Final search queries" = "GT_keywords") %>%
  gt() %>% 
  tab_options(table.font.size = px(10),
              column_labels.hidden = TRUE) %>%
  opt_table_font(
    font = list(
      google_font(name = "Helvetica Neue")
    )
  ) %>%
  gtsave("table_1_search_queries.docx")




## Figure 4: Comparison data windows ####

## Prepare data for graph #
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




### Data for plots ####
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




data_plot1 <- data_plot %>%
  filter(datasource_weight =="GT") %>%
  group_by(model_name) %>%
  mutate(group_mean_deviation = mean(abs(Mean_dev))) %>%
  filter(election_date == "2021-09-26") %>%
  filter(model_time_interval_fac == "7 days" |
         model_time_interval_fac == "14 days" |
         model_time_interval_fac == "28 days" |
           model_time_interval_fac == "91 days") %>% 
  ungroup() %>%
  mutate(model_time_interval_fac = recode(model_time_interval_fac,
                                          "7 days" = "Plot 1:\n7 days",
                                          "14 days" = "Plot 2:\n14 days",
                                          "28 days" = "Plot 3:\n28 days",
                                          "91 days" = "Plot 4:\n91 days"))



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
             scales = "free_x") +
  scale_x_date(breaks = x_tick_labels$GT_end_date,
               labels = paste0(x_tick_labels$model_time_distance, " day(s)\n[", x_tick_labels$GT_end_date, "]")
  ) +
  scale_y_continuous(sec.axis = dup_axis(
    name = "Width of data window")) +
  scale_color_manual(values = cols) + 
  scale_linetype_manual(values = linetypes) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position="top",
        axis.text.y.right = element_blank(),
        strip.background =element_rect(fill="white")) +
  ylab("Deviation on % scale\n(prediction error)") +
  xlab("Distance of data window [end date of window]") +
  labs(colour = "Party")
# stat_summary(aes(y = group_mean_deviation, group = 1), fun=mean, colour="purple", geom="line", linetype = "dashed")
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))


p
ggsave(plot = p,
       filename = "Figure_4_less_intervals.png", # e.g. change to pdf
       width = 14,
       height = 14,
       device = "png", # e.g. change to pdf
       dpi = 600)  





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
       filename = "Figure_A1_2_all_intervalls.png", # e.g. change to pdf
       width = 14,
       height = 14,
       device = "png", # e.g. change to pdf
       dpi = 600)  





## Figure 4-table: Party variation ####
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
       filename = "Figure_A2_figure2_appendix.png", # e.g. change to pdf
       width = 14,
       height = 14,
       device = "png", # e.g. change to pdf
       dpi = 600)  




## Table-A1-2 (Figure 2 trends) ####
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
  )  %>% gtsave("tabA1.docx")













## Figure 5: Comparison models ####
# prediction error averaged across all parties for the GT data + other datasources

# Create average prediction error (across all parties) ###
###Plot GT vs. Polls
for(i in as.character(unique(data_plot$election_date))){ # Loop over elections
  print(i)
  data_plot2 <- data_plot %>%
  filter(datasource_weight =="GT" | 
           datasource_weight =="Last polls" |
           datasource_weight =="GT + election weight" |
           datasource_weight =="GT + weekly polls weight"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE)) %>%
  filter(election_date == i) %>%
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


# Count number of models
# data_plot2 %>% group_by(datasource_weight) %>% summarize(n_models = n())




cols2 <- c("MC1: GT" = "#e41a1c", 
           #"Only polls" = "black", 
           "Last polls" = "black",
           "MC2: GT + election weight" = "#984ea3",
           "MC3: GT + weekly polls weight" = "#ff7f00")


# Compare predictions across models
# Table profide the number/percentage of models where MC1-3 beat polls

model_comparison <- data_plot2 %>% 
  select(model_time_interval, model_time_distance, datasource_weight, deviation_mean) %>%
  group_by(datasource_weight) %>% 
  mutate(id = row_number()) %>%
  pivot_wider(names_from = datasource_weight, values_from =  deviation_mean) %>%
  group_by(model_time_interval, model_time_distance) %>%
  slice(1) %>% 
  ungroup() %>%
  mutate(id = row_number()) %>% # Recreate ID (aggregated)
  mutate(polls_vs_GT = ifelse(`Last polls` < `MC1: GT`, TRUE, FALSE),
         polls_vs_GTE = ifelse(`Last polls` < `MC2: GT + election weight`, TRUE, FALSE),
         polls_vs_GTP = ifelse(`Last polls` < `MC3: GT + weekly polls weight`, TRUE, FALSE))

# TRUE = POLLS ARE BETTER, FALSE = MODELCLASS X is BETTER
table(model_comparison$polls_vs_GT)
prop.table(table(model_comparison$polls_vs_GT))
table(model_comparison$polls_vs_GTE)
prop.table(table(model_comparison$polls_vs_GTE))
table(model_comparison$polls_vs_GTP)
prop.table(table(model_comparison$polls_vs_GTP))

# Comparison only for large width
# TRUE = POLLS ARE BETTER, FALSE = MODELCLASS X is BETTER
model_comparison_large_width <- model_comparison %>% 
  filter(model_time_interval == "7862400s (~13 weeks)")
table(model_comparison_large_width$polls_vs_GT)
prop.table(table(model_comparison_large_width$polls_vs_GT))
table(model_comparison_large_width$polls_vs_GTE)
prop.table(table(model_comparison_large_width$polls_vs_GTE))
table(model_comparison_large_width$polls_vs_GTP)
prop.table(table(model_comparison_large_width$polls_vs_GTP))


# Add Plot X labels
data_plot2 <- data_plot2 %>%
  mutate(model_time_interval_fac = recode(model_time_interval_fac,
                                          "7 days" = "Plot 1:\n7 days",
                                          "14 days" = "Plot 2:\n14 days",
                                          "28 days" = "Plot 3:\n28 days",
                                          "91 days" = "Plot 4:\n91 days"))




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
        axis.text.y.right = element_blank(),
        plot.caption=element_text(hjust = 0),
        strip.background =element_rect(fill="white")) +
  labs(x = "Enddate of interval\n(= distance)",
       y = "MeanDeviation in %\n(prediction error)",
       colour = "Model class",
       caption = paste0("Note: Predictive models for ",i," election."),
       title = paste0("Election: ", i))
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))



p2

ggsave(plot = p2,
       filename = paste0("Figure_5_model_comparison_",i,".png"), # e.g. change to pdf
       width = 14,
       height = 14,
       device = "png", # e.g. change to pdf
       dpi = 600)  

}




## Figure 6: Comparison elections ####
data_plot2 <- data_plot %>%
  filter(datasource_weight =="GT" | 
           datasource_weight =="Last polls" |
           datasource_weight =="GT + election weight" |
           datasource_weight =="GT + weekly polls weight"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE)) %>%
  #filter(election_date == i) %>%
  filter(model_time_interval_fac == "91 days") %>% 
  ungroup() %>%
  mutate(datasource_weight = recode(datasource_weight, 
                                    "GT" = "MC1: GT",
                                    "GT + election weight" = "MC2: GT + election weight",
                                    "GT + weekly polls weight" = "MC3: GT + weekly polls weight")) %>%
  mutate(election = factor(election, levels = c("18 Sep, 2005", 
                                                "27 Sep, 2009", "22 Sep, 2013", "24 Sep, 2017", "26 Sep, 2021"),
                           ordered = TRUE)) %>%
  mutate(election = recode(election, 
                           "18 Sep, 2005" = "18 Sep, 2005", 
                           "27 Sep, 2009" = "Plot 1:\nElection\n27 Sep, 2009", 
                           "22 Sep, 2013" = "Plot 2:\nElection\n22 Sep, 2013", 
                           "24 Sep, 2017" = "Plot 3:\nElection\n24 Sep, 2017", 
                           "26 Sep, 2021" = "Plot 4:\nElection\n26 Sep, 2021"))


#  WHY NOT AGGREGATE DATASET?

# Create x-axis tick labels
x_tick_labels <- data_plot2 %>%
  group_by(GT_end_date) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  slice(c(1,50, 100, 150)) %>%# Pick every 30th row
  select(GT_start_date, GT_end_date, model_time_distance)



# Compare predictions across models
# Table profide the number/percentage of models where MC1-3 beat polls

model_comparison <- data_plot2 %>% 
  select(model_time_interval, model_time_distance, election, datasource_weight, deviation_mean) %>%
  group_by(datasource_weight, election) %>% 
  mutate(id = row_number()) %>%
  pivot_wider(names_from = datasource_weight, values_from =  deviation_mean) %>%
  group_by(model_time_interval, model_time_distance, election) %>%
  slice(1) %>% 
  ungroup() %>%
  group_by(election) %>%
  arrange(election, model_time_interval, model_time_distance) %>%
  mutate(id = row_number()) %>% # Recreate ID (aggregated)
  ungroup() %>%
  mutate(polls_vs_GT = ifelse(`Last polls` < `MC1: GT`, TRUE, FALSE),
         polls_vs_GTE = ifelse(`Last polls` < `MC2: GT + election weight`, TRUE, FALSE),
         polls_vs_GTP = ifelse(`Last polls` < `MC3: GT + weekly polls weight`, TRUE, FALSE))

# TRUE = POLLS ARE BETTER, FALSE = MODELCLASS X is BETTER
table(model_comparison$election, model_comparison$polls_vs_GT)
prop.table(table(model_comparison$election, model_comparison$polls_vs_GT), margin = 1)
table(model_comparison$election, model_comparison$polls_vs_GTE)
prop.table(table(model_comparison$election, model_comparison$polls_vs_GTE), margin = 1)
table(model_comparison$election, model_comparison$polls_vs_GTP)
prop.table(table(model_comparison$election, model_comparison$polls_vs_GTP), margin = 1)



cols2 <- c("MC1: GT" = "#e41a1c", 
           #"Only polls" = "black", 
           "Last polls" = "black",
           "MC2: GT + election weight" = "#984ea3",
           "MC3: GT + weekly polls weight" = "#ff7f00")

add_election_word <- function(x){paste0("Election:\n",x)}

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
      facet_grid2(rows = vars(election), 
             scales = "free",
             independent = "x") + # labeller = labeller(election = add_election_word)
  scale_y_continuous(sec.axis = dup_axis(
    name = "Election")) +
  scale_color_manual(values = cols2) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position="top",
        axis.text.y.right = element_blank(),
        plot.caption=element_text(hjust = 0),
        strip.background =element_rect(fill="white")) +
  labs(x = "Enddate of interval\n(= distance)",
       y = "MeanDeviation in %\n(prediction error)",
       colour = "Model class",
       caption = paste0("Note: Predictive models across all four elections holding the data window width constant at 91 days."),
       title = paste0("Election: All elections"))


p2

ggsave(plot = p2,
       filename = paste0("Figure_6_election_comparison.png"), # e.g. change to pdf
       width = 14,
       height = 14,
       device = "png", # e.g. change to pdf
       dpi = 500)  






## Figure 5_appendix: Comparison elections ####
data_plot2 <- data_plot %>%
  filter(datasource_weight =="GT" | 
           datasource_weight =="Last polls" |
           datasource_weight =="GT + election weight" |
           datasource_weight =="GT + weekly polls weight"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE)) %>%
  #filter(election_date == i) %>%
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
  slice(c(1,50, 100, 150)) %>%# Pick every 30th row
  select(GT_start_date, GT_end_date, model_time_distance)



# Count number of models
# data_plot2 %>% group_by(datasource_weight) %>% summarize(n_models = n())




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
  #geom_point(size = 0.5) +
  geom_line() +
  theme_minimal(base_size = 22) +
  # scale_x_date(breaks = x_tick_labels$GT_end_date,
  #              labels = paste0(x_tick_labels$model_time_distance, 
  #                              " day(s)\n[", 
  #                              x_tick_labels$GT_end_date, "]")
  # ) +
  facet_grid(vars(model_time_interval_fac),
             vars(election_date), 
             scales = "free_x") +
  scale_y_continuous(sec.axis = dup_axis(
    name = "Width of data window")) +
  scale_color_manual(values = cols2) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position="top",
        axis.text.y.right = element_blank(),
        plot.caption=element_text(hjust = 0)) +
  labs(x = "Enddate of interval\n(= distance)",
       y = "MeanDeviation in %\n(prediction error)",
       colour = "Model class",
       caption = paste0("Note: Predictive models across all four elections."),
       title = paste0("Election: All elections"))



p2

ggsave(plot = p2,
       filename = paste0("Figure_5_election_comparison_appendix.png"), # e.g. change to pdf
       width = 14,
       height = 14,
       device = "png", # e.g. change to pdf
       dpi = 600)  

























# LOAD DATA (Sonstige) ####
# Remove all files
rm(list=ls())
### For normal Data
load(file  = "./Environments/Env_Merged_syntax_Sonstige 2023-01-26_01-11-02.RData")

# Delete unneccessary objects
rm(list=setdiff(ls(), c("data_models", "data_predictions_final", "data_predictions"))) 


## Data management ####
data_models <- data_models %>% 
  filter(grepl("M_\\d+_2005", data_models$model_name) == FALSE)

#And again renaming since first id´s are missing (deleted 2005ÃÂ´s)
## Add model index/number
data_models <- data_models %>% 
  mutate(model_id = row_number()) %>%
  select(model_id, everything())

# Reorder
data_models <- data_models %>%
  select(model_id, model_name, everything())

# Reorder
data_models <- data_models %>%
  select(model_id, model_name, everything())


#### delete 2005 rows (just needed for Model2_2009)

data_predictions_final <- data_predictions_final %>% 
  filter(grepl("M_\\d+_2005", data_predictions_final$model_name) == FALSE)

#And again renaming since first id´s are missing (deleted 2005ÃÂ´s)
## Add model index/number
data_predictions_final <- data_predictions_final %>% 
  mutate(model_id = row_number()) %>%
  select(model_id, everything())



# CLEAN data_predictions
data_predictions <- data_predictions %>%
  filter(grepl("M_\\d+_2005", model_name) == FALSE) %>% # filter out models without predictions
  group_by(model_name) %>%
  mutate(model_id = cur_group_id()) %>% # Add new model_id (after filtering)
  ungroup()









## Figure 4: Comparison data windows ####

## Prepare data for graph #
data_predictions$party <-
  as.factor(ordered(data_predictions$party,
                    levels = c("Grüne", "Linke", "FDP", "AFD", "SPD", "CDU", "Sonstige"))) # ADAPTED
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
         dev_upper.ci = Mean_dev + 1.96*(SD_dev/sqrt(n()))) #%>%
# replace_na(.,0)

# Kann man nuch besser lösen ????????
#Sinn: summarize die oben genannten aber behalte andere Spalten wie datasource_weight etc.
data_predictions_final_mean <- merge(data_predictions_final_mean, data_predictions, by = c("model_name","party"))
data_predictions_final_mean <- data_predictions_final_mean %>% select(-c(20,21,22,23,24), -("df_id"))




### Data for plots ####
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
          "Grüne" = "green",
          "Sonstige" = "gray")  # ADAPTED


linetypes <- c("SPD" = "solid", 
               "CDU" = "solid", 
               "AFD" = "solid", 
               "FDP" = "solid", 
               "Linke" = "solid", 
               "Grüne" = "solid",
               "Sonstige" = "solid")  # ADAPTED




data_plot1 <- data_plot %>%
  filter(datasource_weight =="GT") %>%
  group_by(model_name) %>%
  mutate(group_mean_deviation = mean(abs(Mean_dev))) %>%
  filter(election_date == "2021-09-26") %>%
  filter(model_time_interval_fac == "7 days" |
           model_time_interval_fac == "14 days" |
           model_time_interval_fac == "28 days" |
           model_time_interval_fac == "91 days") %>% 
  ungroup() %>%
  mutate(model_time_interval_fac = recode(model_time_interval_fac,
                                          "7 days" = "Plot 1:\n7 days",
                                          "14 days" = "Plot 2:\n14 days",
                                          "28 days" = "Plot 3:\n28 days",
                                          "91 days" = "Plot 4:\n91 days"))



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
             scales = "free_x") +
  scale_x_date(breaks = x_tick_labels$GT_end_date,
               labels = paste0(x_tick_labels$model_time_distance, " day(s)\n[", x_tick_labels$GT_end_date, "]")
  ) +
  scale_y_continuous(sec.axis = dup_axis(
    name = "Width of data window")) +
  scale_color_manual(values = cols) + 
  scale_linetype_manual(values = linetypes) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position="top",
        axis.text.y.right = element_blank(),
        strip.background =element_rect(fill="white")) +
  ylab("Deviation on % scale\n(prediction error)") +
  xlab("Distance of data window [end date of window]") +
  labs(colour = "Party")
# stat_summary(aes(y = group_mean_deviation, group = 1), fun=mean, colour="purple", geom="line", linetype = "dashed")
#+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))


p
ggsave(plot = p,
       filename = "Figure_4_less_intervals_sonstige.png", # e.g. change to pdf
       width = 14,
       height = 14,
       device = "png", # e.g. change to pdf
       dpi = 550)  
























## Figure 5: Comparison models ####
# prediction error averaged across all parties for the GT data + other datasources

# Create average prediction error (across all parties) ###
###Plot GT vs. Polls
for(i in as.character(unique(data_plot$election_date))){ # Loop over elections
  print(i)
  data_plot2 <- data_plot %>%
    filter(datasource_weight =="GT" | 
             datasource_weight =="Last polls" |
             datasource_weight =="GT + election weight" |
             datasource_weight =="GT + weekly polls weight"
    ) %>%
    group_by(model_name) %>% 
    mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE)) %>%
    filter(election_date == i) %>%
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
  
  
  # Count number of models
  # data_plot2 %>% group_by(datasource_weight) %>% summarize(n_models = n())
  
  
  
  
  cols2 <- c("MC1: GT" = "#e41a1c", 
             #"Only polls" = "black", 
             "Last polls" = "black",
             "MC2: GT + election weight" = "#984ea3",
             "MC3: GT + weekly polls weight" = "#ff7f00")
  
  
  # Compare predictions across models
  # Table profide the number/percentage of models where MC1-3 beat polls
  
  model_comparison <- data_plot2 %>% 
    select(model_time_interval, model_time_distance, datasource_weight, deviation_mean) %>%
    group_by(datasource_weight) %>% 
    mutate(id = row_number()) %>%
    pivot_wider(names_from = datasource_weight, values_from =  deviation_mean) %>%
    group_by(model_time_interval, model_time_distance) %>%
    slice(1) %>% 
    ungroup() %>%
    mutate(id = row_number()) %>% # Recreate ID (aggregated)
    mutate(polls_vs_GT = ifelse(`Last polls` < `MC1: GT`, TRUE, FALSE),
           polls_vs_GTE = ifelse(`Last polls` < `MC2: GT + election weight`, TRUE, FALSE),
           polls_vs_GTP = ifelse(`Last polls` < `MC3: GT + weekly polls weight`, TRUE, FALSE))
  
  # TRUE = POLLS ARE BETTER, FALSE = MODELCLASS X is BETTER
  table(model_comparison$polls_vs_GT)
  prop.table(table(model_comparison$polls_vs_GT))
  table(model_comparison$polls_vs_GTE)
  prop.table(table(model_comparison$polls_vs_GTE))
  table(model_comparison$polls_vs_GTP)
  prop.table(table(model_comparison$polls_vs_GTP))
  
  # Comparison only for large width
  # TRUE = POLLS ARE BETTER, FALSE = MODELCLASS X is BETTER
  model_comparison_large_width <- model_comparison %>% 
    filter(model_time_interval == "7862400s (~13 weeks)")
  table(model_comparison_large_width$polls_vs_GT)
  prop.table(table(model_comparison_large_width$polls_vs_GT))
  table(model_comparison_large_width$polls_vs_GTE)
  prop.table(table(model_comparison_large_width$polls_vs_GTE))
  table(model_comparison_large_width$polls_vs_GTP)
  prop.table(table(model_comparison_large_width$polls_vs_GTP))
  
  
  # Add Plot X labels
  data_plot2 <- data_plot2 %>%
    mutate(model_time_interval_fac = recode(model_time_interval_fac,
                                            "7 days" = "Plot 1:\n7 days",
                                            "14 days" = "Plot 2:\n14 days",
                                            "28 days" = "Plot 3:\n28 days",
                                            "91 days" = "Plot 4:\n91 days"))
  
  
  
  
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
          axis.text.y.right = element_blank(),
          plot.caption=element_text(hjust = 0),
          strip.background =element_rect(fill="white")) +
    labs(x = "Enddate of interval\n(= distance)",
         y = "MeanDeviation in %\n(prediction error)",
         colour = "Model class",
         caption = paste0("Note: Predictive models for ",i," election."),
         title = paste0("Election: ", i))
  #+ geom_errorbar(aes(ymin=dev_lower.ci, ymax=dev_upper.ci),width=.3, position=position_dodge(.9))
  
  
  
  p2
  
  ggsave(plot = p2,
         filename = paste0("Figure_5_model_comparison_sonstige_",i,".png"), # e.g. change to pdf
         width = 14,
         height = 14,
         device = "png", # e.g. change to pdf
         dpi = 500)  
  
}













## Figure 6: Comparison elections ####
data_plot2 <- data_plot %>%
  filter(datasource_weight =="GT" | 
           datasource_weight =="Last polls" |
           datasource_weight =="GT + election weight" |
           datasource_weight =="GT + weekly polls weight"
  ) %>%
  group_by(model_name) %>% 
  mutate(deviation_mean = mean(abs(Mean_dev), na.rm=TRUE)) %>%
  #filter(election_date == i) %>%
  filter(model_time_interval_fac == "91 days") %>% 
  ungroup() %>%
  mutate(datasource_weight = recode(datasource_weight, 
                                    "GT" = "MC1: GT",
                                    "GT + election weight" = "MC2: GT + election weight",
                                    "GT + weekly polls weight" = "MC3: GT + weekly polls weight")) %>%
  mutate(election = factor(election, levels = c("18 Sep, 2005", 
                                                "27 Sep, 2009", "22 Sep, 2013", "24 Sep, 2017", "26 Sep, 2021"),
                           ordered = TRUE)) %>%
  mutate(election = recode(election, 
                           "18 Sep, 2005" = "18 Sep, 2005", 
                           "27 Sep, 2009" = "Plot 1:\nElection\n27 Sep, 2009", 
                           "22 Sep, 2013" = "Plot 2:\nElection\n22 Sep, 2013", 
                           "24 Sep, 2017" = "Plot 3:\nElection\n24 Sep, 2017", 
                           "26 Sep, 2021" = "Plot 4:\nElection\n26 Sep, 2021"))


#  WHY NOT AGGREGATE DATASET?

# Create x-axis tick labels
x_tick_labels <- data_plot2 %>%
  group_by(GT_end_date) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  slice(c(1,50, 100, 150)) %>%# Pick every 30th row
  select(GT_start_date, GT_end_date, model_time_distance)



# Compare predictions across models
# Table profide the number/percentage of models where MC1-3 beat polls

model_comparison <- data_plot2 %>% 
  select(model_time_interval, model_time_distance, election, datasource_weight, deviation_mean) %>%
  group_by(datasource_weight, election) %>% 
  mutate(id = row_number()) %>%
  pivot_wider(names_from = datasource_weight, values_from =  deviation_mean) %>%
  group_by(model_time_interval, model_time_distance, election) %>%
  slice(1) %>% 
  ungroup() %>%
  group_by(election) %>%
  arrange(election, model_time_interval, model_time_distance) %>%
  mutate(id = row_number()) %>% # Recreate ID (aggregated)
  ungroup() %>%
  mutate(polls_vs_GT = ifelse(`Last polls` < `MC1: GT`, TRUE, FALSE),
         polls_vs_GTE = ifelse(`Last polls` < `MC2: GT + election weight`, TRUE, FALSE),
         polls_vs_GTP = ifelse(`Last polls` < `MC3: GT + weekly polls weight`, TRUE, FALSE))

# TRUE = POLLS ARE BETTER, FALSE = MODELCLASS X is BETTER
table(model_comparison$election, model_comparison$polls_vs_GT)
prop.table(table(model_comparison$election, model_comparison$polls_vs_GT), margin = 1)
table(model_comparison$election, model_comparison$polls_vs_GTE)
prop.table(table(model_comparison$election, model_comparison$polls_vs_GTE), margin = 1)
table(model_comparison$election, model_comparison$polls_vs_GTP)
prop.table(table(model_comparison$election, model_comparison$polls_vs_GTP), margin = 1)



cols2 <- c("MC1: GT" = "#e41a1c", 
           #"Only polls" = "black", 
           "Last polls" = "black",
           "MC2: GT + election weight" = "#984ea3",
           "MC3: GT + weekly polls weight" = "#ff7f00")

add_election_word <- function(x){paste0("Election:\n",x)}

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
  facet_grid2(rows = vars(election), 
              scales = "free",
              independent = "x") + # labeller = labeller(election = add_election_word)
  scale_y_continuous(sec.axis = dup_axis(
    name = "Election")) +
  scale_color_manual(values = cols2) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position="top",
        axis.text.y.right = element_blank(),
        plot.caption=element_text(hjust = 0),
        strip.background =element_rect(fill="white")) +
  labs(x = "Enddate of interval\n(= distance)",
       y = "MeanDeviation in %\n(prediction error)",
       colour = "Model class",
       caption = paste0("Note: Predictive models across all four elections holding the data window width constant at 91 days."),
       title = paste0("Election: All elections"))


p2

ggsave(plot = p2,
       filename = paste0("Figure_6_election_comparison_sonstige.png"), # e.g. change to pdf
       width = 14,
       height = 14,
       device = "png", # e.g. change to pdf
       dpi = 500)  




