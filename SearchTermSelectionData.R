library(gtrendsR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rvest)
library(xml2)


rm(list = ls())

###############################  CDU #############################
#result: CDU + Angela Merkel + CSU
#müssen hier nochmal ziehen für Armin Laschet, bekomme momentan nur Fehlercode

termsCDU <- gtrends(keyword=c("CDU", "CSU", "Angela Merkel", "Christlich demokratische Union","Armin Laschet"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

###############################  CSU #############################
#result: Just use CSU, no Peaks for main candidates

termsCSU <- gtrends(keyword=c("CSU", "christlich soziale union", "Edmund Stoiber", "Horst Seehofer", "Alexander Dobrindt"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

###############################  SPD #############################
#Kanzlerkandidaten: Gerhard Schröder (2005), Frank-Walter Steinmeier (2009), Peer Steinbrück(2013), Martin Schulz(2017)

#result: SPD + Candidates

termsSPD <- gtrends(keyword=c("SPD", "Sozialdemokratische Partei Deutschlands"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

termSPD_Cand <- gtrends(keyword=c("SPD", "Frank Walter Steinmeier", "Peer Steinbrück", "Martin Schulz", "Olaf Scholz"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")


###############################  FDP #############################
#Parteivorsitzende: Guido Westerwelle (-2011), Philipp Rösler (2011-2013), Christian Linder (2013-)
#result: FDP + Candidate

termsFDP <- gtrends(keyword=c("FDP", "Freie demokratische Partei Deutschlands", "Guido Westerwelle", "Philipp Rösler", "Christian Lindner"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")


###############################  Die Grünen #############################
#top candidates: Joschka Fischer(2005), Jürgen Trittin + Renate Künast (2009), Jürgen Trittin + Katrin Göring Eckardt (2013), Katrin Göring-Eckardt + Cem Özdemir (2017)
#result: Grüne (includes "Die Grünen" and "Bündnis 90 die Grünen") + top candidates

termsGrüne <- gtrends(keyword=c('"Grüne"',"Die Grünen", "Bündnis 90 die Grünen"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

termsGrüne_Cand <- gtrends(keyword=c("Grüne","Jürgen Trittin + Renate Künast", "Jürgen Trittin + Katrin Göring Eckardt", "Katrin Göring Eckardt + Cem Özdemir", "Baerbock"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

###############################  Die Linke #############################
#Spitzenkandidaten: 2005: PDS, Gregor Gysi ,Gregor Gysi (2009)  ,Gregor Gysi + sarah Wagenknecht + (Dietmar Bartsch) + (5 weitere) (20013) ,Dietmar Bartsch + Sarah Wagenknecht (2017)
#result: Linke (includes "Die Linke" and "Die Linken")

#Linke vs related querries
termsLinke <- gtrends(keyword=c("Linke", "Die Linke","Die Linken"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")


#Spitzenkandidaten
#for election 2013: Sarah Wagenknecht and Gregor Gysi (Most important ones of the 8 candidates due to Google Trends)
termsLinke_Cand <- gtrends(keyword=c("Gregor Gysi", "Gregor Gysi + Sarah Wagenknecht", "Dietmar Bartsch + Sarah Wagenknecht", "Janine Wissler + Dietmar Bartsch"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")


#Overall 2005
#PDS + Linkspartei
termsLinke2 <- gtrends(keyword=c("Die Linke", "Die Linke PDS","PDS", "Linke", "Linkspartei"), geo= "DE" , category=19, time = "2004-01-01 2005-09-24", gprop="web")

###############################  AFD #############################
#Spitzenkandidaten:  Bernd Lucke (2013),Alice Weidel + Alexander Gauland (2017),Alice Weidel + Tino Chrupalla (2021)
#result: AFD + candidates

termsAFD <- gtrends(keyword=c("AFD", "Alternative für Deutschland", "Bernd Lucke", "Alice Weidel + Alexander Gauland", "Alice Widel + Tino Chrupalla"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")


#filename = paste(gsub(":", "-", Sys.time()),".RData",sep="")
#save.image(paste("./Data_SearchTerms/",(filename)))


#Preperation + Plots
# Load data

load(file = "Data_SearchTerms/ 2023-01-22 14-33-21.RData")

# display all available category numbers
df_cat <- data("categories")
summary(df_cat)

######### Search terms + period
#Bundestagswahlen: 26.09.2021; 24.09.2017; 22.09.2013; 27.09.2009; 18.09.2005
# Specifying vertical X intercepts for election dates
elec_vlines <- as.Date(c("2005-09-18", "2009-09-27", "2013-09-18", "2017-09-24", "2021-09-26"))

#CDU

termsCDU_df <- termsCDU$interest_over_time
termsCDU_df <- termsCDU_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

###Used in Paper
p1 <- ggplot(termsCDU_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)


p1
###############################  CSU #############################
#result: Just use CSU, no Peaks for main candidates

termsCSU_df <- termsCSU$interest_over_time
termsCSU_df <- termsCSU_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

p2 <- ggplot(termsCSU_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)
p2

###############################  SPD #############################
#Kanzlerkandidaten: Gerhard Schröder (2005), Frank-Walter Steinmeier (2009), Peer Steinbrück(2013), Martin Schulz(2017)

#result: SPD + Candidates

termsSPD_df <- termsSPD$interest_over_time
termsSPD_df <- termsSPD_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

p3 <- ggplot(termsSPD_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)

p3

#SPD Candidates
termsSPD_Cand_df <- termsSPD_Cand$interest_over_time
termsSPD_Cand_df <- termsSPD_Cand_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

p4 <- ggplot(termsSPD_Cand_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)

p4

###############################  FDP #############################
#Parteivorsitzende: Guido Westerwelle (-2011), Philipp R?sler (2011-2013), Christian Linder (2013-)
#result: FDP + Candidate

termsFDP_df <- termsFDP$interest_over_time
termsFDP_df <- termsFDP_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

p5 <- ggplot(termsFDP_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)

p5

###############################  Die Grünen #############################
#top candidates: Joschka Fischer(2005), Jürgen Trittin + Renate Künast (2009), Jürgen Trittin + Katrin Göring Eckardt (2013), Katrin Göring-Eckardt + Cem Özdemir (2017)
#result: Grüne (includes "Die Grünen" and "Bündnis 90 die Grünen") + top candidates

termsGrüne_df <- termsGrüne$interest_over_time
termsGrüne_df <- termsGrüne_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

p6 <- ggplot(termsGrüne_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)

p6

termsGrüne_Cand_df <- termsGrüne_Cand$interest_over_time
termsGrüne_Cand_df <- termsGrüne_Cand_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

p7 <- ggplot(termsGrüne_Cand_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)

p7

###############################  Die Linke #############################
#Spitzenkandidaten: 2005: PDS, Gregor Gysi ,Gregor Gysi (2009)  ,Gregor Gysi + sarah Wagenknecht + (Dietmar Bartsch) + (5 weitere) (20013) ,Dietmar Bartsch + Sarah Wagenknecht (2017)
#result: Linke (includes "Die Linke" and "Die Linken")

#Linke vs related querries
termsLinke_df <- termsLinke$interest_over_time
termsLinke_df <- termsLinke_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

###Used in main Paper (Figure2)

p8 <- ggplot(termsLinke_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)

p8

#Candidates
#for election 2013: Sarah Wagenknecht and Gregor Gysi (Most important ones of the 8 candidates due to Google Trends)

termsLinke_Cand_df <- termsLinke_Cand$interest_over_time
termsLinke_Cand_df <- termsLinke_Cand_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

p9 <- ggplot(termsLinke_Cand_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)

p9

#Overall 2005
#PDS + Linkspartei
termsLinke2_df <- termsLinke2$interest_over_time
termsLinke2_df <- termsLinke2_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Plot nur bis 2005  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
p10 <- ggplot(termsLinke2_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)

p10
###############################  AFD #############################
#Spitzenkandidaten:  Bernd Lucke (2013),Alice Weidel + Alexander Gauland (2017),Alice Weidel + Tino Chrupalla (2021)
#result: AFD + candidates

termsAFD_df <- termsAFD$interest_over_time
termsAFD_df <- termsAFD_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

p11 <- ggplot(termsAFD_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_y_continuous( breaks = seq(0,100, 10)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted", size = 1) +
  theme_minimal(base_size = 22) +
  ylab("Searches (100 = max. interest in time period/territory)") +
  xlab("Date") +
  labs(colour = "Search terms (below)") +
  scale_x_date(date_labels = paste0("%y", "'"),
               date_breaks = "1 year") +
  theme(legend.position = "top",
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  annotate(geom = "text", 
           label = paste0("Election: ", elec_vlines), 
           x=elec_vlines+60, 
           y=rep(100, 5),
           angle = 90,
           hjust = 1,
           size = 5)

p11


#Figure2

p1 + p8 + 
  plot_layout(ncol = 1) + 
  plot_layout(guides="collect")


result <- p1 + p8 + 
  plot_layout(ncol = 1)
gt <- patchwork::patchworkGrob(result)
plot_searchterms <- gridExtra::grid.arrange(gt,
                                            bottom=textGrob("Date", gp=gpar(fontsize=22)), 
                                            left=textGrob("Searches (100 = max. interest in time period/territory)", gp=gpar(fontsize=22), rot=90))
ggsave(plot = plot_searchterms,
       filename = "Figure_2_searchterms.png", # e.g. change to pdf
       width = 14,
       height = 10,
       device = "png", # e.g. change to pdf
       dpi = 300) 



