library(gtrendsR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rvest)
library(xml2)

# display all available category numbers
df_cat <- data("categories")
summary(df_cat)

######### Search terms + period
#Bundestagswahlen: 26.09.2021; 24.09.2017; 22.09.2013; 27.09.2009; 18.09.2005
# Specifying vertical X intercepts for election dates
elec_vlines <- as.Date(c("2005-09-18", "2009-09-27", "2013-09-18", "2017-09-24", "2021-09-26"))

#Approach: Look if search Querry has Peak before a election, compare the importance of the different querries
#Attention: Here, we are looking at the impact of the querries between 2004 and 2018. Additionally, we looked for specific candidates of parties in the time intervall they were present. For that purpose Google Trends was used directly, not reported here because the amount of code would be too much.

###############################  CDU #############################
#result: CDU + Angela Merkel + CSU

termsCDU <- gtrends(keyword=c("CDU", "CSU", "Angela Merkel", "Christlich demokratische Union"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

termsCDU_df <- termsCDU$interest_over_time
termsCDU_df <- termsCDU_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

###Used in Paper
ggplot(termsCDU_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_x_date(date_breaks = "years" , date_labels = "%y") +
  scale_y_continuous( breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted")


###############################  CSU #############################
#result: Just use CSU, no Peaks for main candidates

termsCSU <- gtrends(keyword=c("CSU", "christlich soziale union", "Edmund Stoiber", "Horst Seehofer", "Alexander Dobrindt"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

termsCSU_df <- termsCSU$interest_over_time
termsCSU_df <- termsCSU_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

ggplot(termsCSU_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_x_date(date_breaks = "years" , date_labels = "%y") +
  scale_y_continuous( breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted")


###############################  SPD #############################
#Kanzlerkandidaten: Gerhard Schröder (2005), Frank-Walter Steinmeier (2009), Peer Steinbrück(2013), Martin Schulz(2017)

#result: SPD + Candidates

termsSPD <- gtrends(keyword=c("SPD", "Sozialdemokratische Partei Deutschlands", "Frank Walter Steinmeier", "Peer Steinbrück", "Martin Schulz", "Olaf Scholz"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

termsSPD_df <- termsSPD$interest_over_time
termsSPD_df <- termsSPD_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

ggplot(termsSPD_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_x_date(date_breaks = "years" , date_labels = "%y") +
  scale_y_continuous( breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted")


###############################  FDP #############################
#Parteivorsitzende: Guido Westerwelle (-2011), Philipp Rösler (2011-2013), Christian Linder (2013-)
#result: FDP + Candidate

termsFDP <- gtrends(keyword=c("FDP", "Freie demokratische Partei Deutschlands", "Guido Westerwelle", "Philipp Rösler", "Christian Lindner"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

termsFDP_df <- termsFDP$interest_over_time
termsFDP_df <- termsFDP_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

ggplot(termsFDP_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_x_date(date_breaks = "years" , date_labels = "%y") +
  scale_y_continuous( breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted")


###############################  Die Grünen #############################
#top candidates: Joschka Fischer(2005), Jürgen Trittin + Renate Künast (2009), Jürgen Trittin + Katrin Göring Eckardt (2013), Katrin Göring-Eckardt + Cem Özdemir (2017)
#result: Grüne (includes "Die Grünen" and "Bündnis 90 die Grünen") + top candidates

termsGrün <- gtrends(keyword=c('"Grüne"',"Die Grünen", "Bündnis 90 die Grünen"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

termsGrün_df <- termsGrün$interest_over_time
termsGrün_df <- termsGrün_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

ggplot(termsGrün_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_x_date(date_breaks = "years" , date_labels = "%y") +
  scale_y_continuous( breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted")


termsGrün_Cand <- gtrends(keyword=c("Grüne","Jürgen Trittin + Renate Künast", "Jürgen Trittin + Katrin Göring Eckardt", "Katrin Göring Eckardt + Cem Özdemir", "Baerbock"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")
termsGrün_Cand_df <- termsGrün_Cand$interest_over_time
termsGrün_Cand_df <- termsGrün_Cand_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

ggplot(termsGrün_Cand_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_x_date(date_breaks = "years" , date_labels = "%y") +
  scale_y_continuous( breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted")

###############################  Die Linke #############################
#Spitzenkandidaten: 2005: PDS, Gregor Gysi ,Gregor Gysi (2009)  ,Gregor Gysi + sarah Wagenknecht + (Dietmar Bartsch) + (5 weitere) (20013) ,Dietmar Bartsch + Sarah Wagenknecht (2017)
#result: Linke (includes "Die Linke" and "Die Linken")

#Linke vs related querries
termsLinke <- gtrends(keyword=c("Linke", "Die Linke","Die Linken"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

termsLinke_df <- termsLinke$interest_over_time
termsLinke_df <- termsLinke_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

###Used in Paper
ggplot(termsLinke_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_x_date(date_breaks = "years" , date_labels = "%y") +
  scale_y_continuous( breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted")


#Spitzenkandidaten
#for election 2013: Sarah Wagenknecht and Gregor Gysi (Most important ones of the 8 candidates due to Google Trends)
termsLinkeSK <- gtrends(keyword=c("Gregor Gysi", "Gregor Gysi + Sarah Wagenknecht", "Dietmar Bartsch + Sarah Wagenknecht", "Janine Wissler + Dietmar Bartsch"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

termsLinke_dfSK <- termsLinkeSK$interest_over_time
termsLinke_dfSK <- termsLinke_dfSK %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

ggplot(termsLinke_dfSK, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_x_date(date_breaks = "years" , date_labels = "%y") +
  scale_y_continuous( breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted")


#Overall 2005
#PDS + Linkspartei
termsLinke2 <- gtrends(keyword=c("Die Linke", "Die Linke PDS","PDS", "Linke", "Linkspartei"), geo= "DE" , category=19, time = "2004-01-01 2005-09-24", gprop="web")

termsLinke2_df <- termsLinke2$interest_over_time
termsLinke2_df <- termsLinke2_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

ggplot(termsLinke2_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_x_date(date_breaks = "years" , date_labels = "%y") +
  scale_y_continuous( breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted")



###############################  AFD #############################
#Spitzenkandidaten:  Bernd Lucke (2013),Alice Weidel + Alexander Gauland (2017),Alice Weidel + Tino Chrupalla (2021)
#result: AFD + candidates

termsAFD <- gtrends(keyword=c("AFD", "Alternative für Deutschland", "Bernd Lucke", "Alice Weidel + Alexander Gauland", "Alice Widel + Tino Chrupalla"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

termsAFD_df <- termsAFD$interest_over_time
termsAFD_df <- termsAFD_df %>%
  mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
  replace(is.na(.), 0)

ggplot(termsAFD_df, aes(x=date, y=hits, group=keyword, col=keyword)) + 
  geom_line(size=0.5)  +
  scale_x_date(date_breaks = "years" , date_labels = "%y") +
  scale_y_continuous( breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) +
  geom_vline(xintercept = elec_vlines, col= "black", linetype="dotted")




# data frame for table 1 (all used search queries)
sq_final <- data.frame(Party = c("CDU","SPD","FDP","Grüne","Linke","AfD"), FinalSearchQuerries = c("CDU + CSU + top candidate","SPD + top candidate","FDP + top candidate","Grüne + top candidate","PDS + Linkspartei + top candidate",""), 
                       topcandidate05 = c("Angela Merkel","Gerhard Schröder","Guido Westerwelle","Joschka Fischer","Gregor Gysi",""),
                       topcandidate09 = c("Angela Merkel","Frank Walter Steinmeier","Guido Westerwelle","Jürgen Trittin + Renate Künast","Gregor Gysi",""),
                       topcandidate13 = c("Angela Merkel","Peer Steinbrück","Philipp Rösler","Jürgen Trittin + Katrin Göring Eckardt","Sarah Wagenknecht + Gregor Gysi","Bernd Lucke"),
                       topcandidate17 = c("Angela Merkel","Martin Schulz","Christian Lindner","Cem Özdemir + Katrin Göring Eckardt","Sarah Wagenknecht + Dietmar Bartsch","Alice Weidel + Alexander Gauland"),
                       topcandidate21 = c("Armin Laschet","Olaf Scholz","Christian Lindner","Annalena Baerbock","Janine Wissler + Dietmar Bartsch","Alice Weidel + Tino Chrupalla")
)
