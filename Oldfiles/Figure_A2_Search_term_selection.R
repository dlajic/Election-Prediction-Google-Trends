
# Load/install packages ####
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
       grid)

# CDU
#result: CDU + Angela Merkel + CSU
#müssen hier nochmal ziehen für Armin Laschet, bekomme momentan nur Fehlercode
#  CSU
#result: Just use CSU, no Peaks for main candidates
# SPD 
#Kanzlerkandidaten: Gerhard Schröder (2005), Frank-Walter Steinmeier (2009), Peer Steinbrück(2013), Martin Schulz(2017)
#result: SPD + Candidates
# FDP
#Parteivorsitzende: Guido Westerwelle (-2011), Philipp Rösler (2011-2013), Christian Linder (2013-)
#result: FDP + Candidate
# Die Grünen
#top candidates: Joschka Fischer(2005), Jürgen Trittin + Renate Künast (2009), Jürgen Trittin + Katrin Göring Eckardt (2013), Katrin Göring-Eckardt + Cem Özdemir (2017)
#result: Grüne (includes "Die Grünen" and "Bündnis 90 die Grünen") + top candidates
# Die Linke
#Spitzenkandidaten: 2005: PDS, Gregor Gysi ,Gregor Gysi (2009)  ,Gregor Gysi + sarah Wagenknecht + (Dietmar Bartsch) + (5 weitere) (20013) ,Dietmar Bartsch + Sarah Wagenknecht (2017)
#result: Linke (includes "Die Linke" and "Die Linken")
#Linke vs related querries
#Spitzenkandidaten
#for election 2013: Sarah Wagenknecht and Gregor Gysi (Most important ones of the 8 candidates due to Google Trends)
#Overall 2005
#PDS + Linkspartei
# AFD
#Spitzenkandidaten:  Bernd Lucke (2013),Alice Weidel + Alexander Gauland (2017),Alice Weidel + Tino Chrupalla (2021)
#result: AFD + candidates

# Collect data ####
terms_CDU <- gtrends(keyword=c("CDU", "CSU", "Angela Merkel", "Christlich demokratische Union","Armin Laschet"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")
terms_CSU <- gtrends(keyword=c("CSU", "christlich soziale union", "Edmund Stoiber", "Horst Seehofer", "Alexander Dobrindt"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")
terms_SPD <- gtrends(keyword=c("SPD", "Sozialdemokratische Partei Deutschlands"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")
terms_SPD_Cand <- gtrends(keyword=c("SPD", "Frank Walter Steinmeier", "Peer Steinbrück", "Martin Schulz", "Olaf Scholz"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")
terms_FDP <- gtrends(keyword=c("FDP", "Freie demokratische Partei Deutschlands", "Guido Westerwelle", "Philipp Rösler", "Christian Lindner"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")
terms_Grüne <- gtrends(keyword=c('"Grüne"',"Die Grünen", "Bündnis 90 die Grünen"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")
terms_Grüne_Cand <- gtrends(keyword=c("Grüne","Jürgen Trittin + Renate Künast", "Jürgen Trittin + Katrin Göring Eckardt", "Katrin Göring Eckardt + Cem Özdemir", "Baerbock"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")
terms_Linke <- gtrends(keyword=c("Linke", "Die Linke","Die Linken"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")
terms_Linke_Cand <- gtrends(keyword=c("Gregor Gysi", "Gregor Gysi + Sarah Wagenknecht", "Dietmar Bartsch + Sarah Wagenknecht", "Janine Wissler + Dietmar Bartsch"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")
terms_Linke2 <- gtrends(keyword=c("Die Linke", "Die Linke PDS","PDS", "Linke", "Linkspartei"), geo= "DE" , category=19, time = "2004-01-01 2005-09-24", gprop="web")
terms_AFD <- gtrends(keyword=c("AFD", "Alternative für Deutschland", "Bernd Lucke", "Alice Weidel + Alexander Gauland", "Alice Widel + Tino Chrupalla"), geo= "DE" , category=19, time = "2004-01-01 2021-12-31", gprop="web")



# Save relevant datasets for reproducibility ####
  # save(terms_SPD, file = paste0("terms_SPD_", gsub(":|\\s", "_", Sys.time()), ".RData"))
  # save(terms_Grüne, file = paste0("terms_Grüne_", gsub(":|\\s", "_", Sys.time()), ".RData"))
  # save(terms_AFD, file = paste0("terms_AFD_", gsub(":|\\s", "_", Sys.time()), ".RData"))
  # save(terms_FDP, file = paste0("terms_FDP_", gsub(":|\\s", "_", Sys.time()), ".RData"))

# Clean environment
rm(list=ls())

# Load data
load(file = "terms_SPD_2023-02-27_11_02_50.RData")
load(file = "terms_Grüne_2023-02-27_11_04_41.RData")
load(file = "terms_AFD_2023-02-27_11_05_16.RData")
load(file = "terms_FDP_2023-02-27_11_06_00.RData")


# Modify data

for(i in c("terms_AFD", "terms_FDP", "terms_Grüne", "terms_SPD")){
  data_i <- get(i)$interest_over_time%>%
    mutate(hits = as.numeric(hits), date = as.Date(date)) %>%
    replace(is.na(.), 0)
  assign(paste0(i, "_df"), data_i)
  
}



# Generate plots ####
elec_vlines <- as.Date(c("2009-09-27", "2013-09-18", "2017-09-24", "2021-09-26")) # "2005-09-18", 



for(i in c("terms_AFD_df", "terms_FDP_df", "terms_Grüne_df", "terms_SPD_df")){
  assign(paste0("plot_", i),
  get(i) %>% ggplot(aes(x=date, y=hits, group=keyword, col=keyword)) + 
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
             size = 5)+
    guides(col=guide_legend(nrow=2,byrow=TRUE)))

}




plot_terms_AFD_df + plot_terms_FDP_df + plot_terms_Grüne_df + plot_terms_SPD_df + 
  plot_layout(ncol = 1) + 
  plot_layout(guides="collect")


result <- plot_terms_AFD_df + plot_terms_FDP_df + plot_terms_Grüne_df + plot_terms_SPD_df + 
  plot_layout(ncol = 1)
gt <- patchwork::patchworkGrob(result)
plot_searchterms <- gridExtra::grid.arrange(gt,
                                            bottom=textGrob("Date", gp=gpar(fontsize=22)), 
                                            left=textGrob("Searches (100 = max. interest in time period/territory)", gp=gpar(fontsize=22), rot=90))
ggsave(plot = plot_searchterms,
       filename = "Figure_A2_searchterms.png", # e.g. change to pdf
       width = 14,
       height = 17,
       device = "png", # e.g. change to pdf
       dpi = 600) 





######### Search terms + period
#Bundestagswahlen: 26.09.2021; 24.09.2017; 22.09.2013; 27.09.2009; 18.09.2005
# Specifying vertical X intercepts for election dates
###############################  CSU #############################
#result: Just use CSU, no Peaks for main candidates
###############################  SPD #############################
#Kanzlerkandidaten: Gerhard Schröder (2005), Frank-Walter Steinmeier (2009), Peer Steinbrück(2013), Martin Schulz(2017)
#result: SPD + Candidates
#SPD Candidates
###############################  FDP #############################
#Parteivorsitzende: Guido Westerwelle (-2011), Philipp R?sler (2011-2013), Christian Linder (2013-)
#result: FDP + Candidate
###############################  Die Grünen #############################
#top candidates: Joschka Fischer(2005), Jürgen Trittin + Renate Künast (2009), Jürgen Trittin + Katrin Göring Eckardt (2013), Katrin Göring-Eckardt + Cem Özdemir (2017)
#result: Grüne (includes "Die Grünen" and "Bündnis 90 die Grünen") + top candidates
###############################  Die Linke #############################
#Spitzenkandidaten: 2005: PDS, Gregor Gysi ,Gregor Gysi (2009)  ,Gregor Gysi + sarah Wagenknecht + (Dietmar Bartsch) + (5 weitere) (20013) ,Dietmar Bartsch + Sarah Wagenknecht (2017)
#result: Linke (includes "Die Linke" and "Die Linken")
###Used in main Paper (Figure2)
#Candidates
#for election 2013: Sarah Wagenknecht and Gregor Gysi (Most important ones of the 8 candidates due to Google Trends)
#Overall 2005
#PDS + Linkspartei


