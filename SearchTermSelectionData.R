library(gtrendsR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rvest)
library(xml2)


rm(list = ls())

###############################  CDU #############################
#result: CDU + Angela Merkel + CSU

termsCDU <- gtrends(keyword=c("CDU", "CSU", "Angela Merkel", "Christlich demokratische Union"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

###############################  CSU #############################
#result: Just use CSU, no Peaks for main candidates

termsCSU <- gtrends(keyword=c("CSU", "christlich soziale union", "Edmund Stoiber", "Horst Seehofer", "Alexander Dobrindt"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

###############################  SPD #############################
#Kanzlerkandidaten: Gerhard Schröder (2005), Frank-Walter Steinmeier (2009), Peer Steinbrück(2013), Martin Schulz(2017)

#result: SPD + Candidates

termsSPD <- gtrends(keyword=c("SPD", "Sozialdemokratische Partei Deutschlands"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

termSPDSK <- gtrends(keyword=c("SPD", "Frank Walter Steinmeier", "Peer Steinbrück", "Martin Schulz", "Olaf Scholz"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")


###############################  FDP #############################
#Parteivorsitzende: Guido Westerwelle (-2011), Philipp Rösler (2011-2013), Christian Linder (2013-)
#result: FDP + Candidate

termsFDP <- gtrends(keyword=c("FDP", "Freie demokratische Partei Deutschlands", "Guido Westerwelle", "Philipp Rösler", "Christian Lindner"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")


###############################  Die Grünen #############################
#top candidates: Joschka Fischer(2005), Jürgen Trittin + Renate Künast (2009), Jürgen Trittin + Katrin Göring Eckardt (2013), Katrin Göring-Eckardt + Cem Özdemir (2017)
#result: Grüne (includes "Die Grünen" and "Bündnis 90 die Grünen") + top candidates

termsGrün <- gtrends(keyword=c('"Grüne"',"Die Grünen", "Bündnis 90 die Grünen"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

termsGrün_Cand <- gtrends(keyword=c("Grüne","Jürgen Trittin + Renate Künast", "Jürgen Trittin + Katrin Göring Eckardt", "Katrin Göring Eckardt + Cem Özdemir", "Baerbock"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")

###############################  Die Linke #############################
#Spitzenkandidaten: 2005: PDS, Gregor Gysi ,Gregor Gysi (2009)  ,Gregor Gysi + sarah Wagenknecht + (Dietmar Bartsch) + (5 weitere) (20013) ,Dietmar Bartsch + Sarah Wagenknecht (2017)
#result: Linke (includes "Die Linke" and "Die Linken")

#Linke vs related querries
termsLinke <- gtrends(keyword=c("Linke", "Die Linke","Die Linken"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")


#Spitzenkandidaten
#for election 2013: Sarah Wagenknecht and Gregor Gysi (Most important ones of the 8 candidates due to Google Trends)
termsLinkeSK <- gtrends(keyword=c("Gregor Gysi", "Gregor Gysi + Sarah Wagenknecht", "Dietmar Bartsch + Sarah Wagenknecht", "Janine Wissler + Dietmar Bartsch"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")


#Overall 2005
#PDS + Linkspartei
termsLinke2 <- gtrends(keyword=c("Die Linke", "Die Linke PDS","PDS", "Linke", "Linkspartei"), geo= "DE" , category=19, time = "2004-01-01 2005-09-24", gprop="web")

###############################  AFD #############################
#Spitzenkandidaten:  Bernd Lucke (2013),Alice Weidel + Alexander Gauland (2017),Alice Weidel + Tino Chrupalla (2021)
#result: AFD + candidates

termsAFD <- gtrends(keyword=c("AFD", "Alternative für Deutschland", "Bernd Lucke", "Alice Weidel + Alexander Gauland", "Alice Widel + Tino Chrupalla"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")


filename = paste(gsub(":", "-", Sys.time()),".RData",sep="")
save.image(paste("C:/Users/deanl/Desktop/UniMannheim/ComSocScience/Publikation/Election-Prediction-Google-Trends/Data_SearchTerms/",(filename)))
