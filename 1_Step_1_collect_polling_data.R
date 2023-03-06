# Packages ####
library(tidyverse)
library(rvest)
library(stringi)
library(gsubfn)



# 1 Infratest Dimap: 6 parties ####
  # Scraping survey data from the infratest dimap website
  
  # To be able to weight Google trends data with data from opinion polls,
  # polling results of infratest dimap polling institute were scraped from their website on 5th August 2022
  
  url <- "https://www.infratest-dimap.de/umfragen-analysen/bundesweit/sonntagsfrage/"
  
  html <- read_html(url)
  tables <- html_table(html, fill=TRUE) # extract the HTML table from the scrapped data with all survey results over time (no other html table on this page of the website)
  infra_dimap_all <- as.data.frame(tables) # construct data frame from scraped htmltable
  
  # delete column for political party "Freie Wähler" and column "Other"
  infra_dimap_all$X8 <- NULL
  infra_dimap_all$X9 <- NULL
  
  # assign names of parties to columns and delete first row of data set (contains names of political parties)
  colnames(infra_dimap_all) <- c("Date", "SPD", "CDU", "Grüne", "FDP", "AFD", "Linke")
  infra_dimap_all <- infra_dimap_all[-1,]
  write_csv(infra_dimap_all, file = "data_polls_infratest_dimap.csv")

  
  
  
  

# 2 Infratest Dimap: 6 parties with Sonstige ####
  # Scraping survey data from the infratest dimap website
  
  
  # To be able to weight Google trends data with data from opinion polls,
  # polling results of infratest dimap polling institute were scraped from their website on 5th August 2022
  
  url <- "https://www.infratest-dimap.de/umfragen-analysen/bundesweit/sonntagsfrage/"
  
  html <- read_html(url)
  tables <- html_table(html, fill=TRUE) # extract the HTML table from the scrapped data with all survey results over time (no other html table on this page of the website)
  infra_dimap_all <- as.data.frame(tables) # construct data frame from scraped htmltable
  
  ## Sum up mcolums "Freie Wähler" and "Sonstige", delete Freie Wähler afterwards
  infra_dimap_all$X8[infra_dimap_all$X8 == "-"] <- 0
  infra_dimap_all$X9 <- as.numeric(infra_dimap_all$X9) + as.numeric(infra_dimap_all$X8)
  
  infra_dimap_all$X8 <- NULL
  #infra_dimap_all$X9 <- NULL
  
  ## assign names of parties to columns and delete first row of data set (contains names of political parties)
  colnames(infra_dimap_all) <- c("Date", "SPD", "CDU", "Grüne", "FDP", "AFD", "Linke","Sonstige")
  infra_dimap_all <- infra_dimap_all[-1,]
  infra_dimap_all <- infra_dimap_all %>%
    mutate(Date = as.Date(Date, format = "%d.%m.%y")) %>%
    filter(Date <= "2022-08-04")
  write_csv(infra_dimap_all, file = "data_polls_infratest_dimap_sonstige.csv")
  
  
  
  
  
  

  
  
  
  
# 3 Allensbach ####  
  url1 <- "https://www.wahlrecht.de/umfragen/allensbach.htm"
  url2 <- "https://www.wahlrecht.de/umfragen/allensbach/2017.htm"
  url3 <- "https://www.wahlrecht.de/umfragen/allensbach/2013.htm"
  url4 <- "https://www.wahlrecht.de/umfragen/allensbach/2009.htm"
  
  html1 <- read_html(url1)
  tables1 <- html_table(html1, fill=TRUE) 
  Allens_23 <- as.data.frame(tables1) 
  
  html2 <- read_html(url2)
  tables2 <- html_table(html2, fill=TRUE) 
  Allens_17 <- as.data.frame(tables2) 
  
  html3 <- read_html(url3)
  tables3 <- html_table(html3, fill=TRUE) 
  Allens_13 <- as.data.frame(tables3) 
  
  html4 <- read_html(url4)
  tables4 <- html_table(html4, fill=TRUE) 
  Allens_09 <- as.data.frame(tables4) 
  
  # filter out election results 
  Allens_23 <- Allens_23 %>%
    filter(!str_detect(.$Befragte,"Bundestagswahl"))
  
  Allens_17 <- Allens_17 %>%
    filter(!str_detect(.$Befragte,"Bundestagswahl"))
  
  Allens_13 <- Allens_13 %>%
    filter(!str_detect(.$Befragte,"Bundestagswahl"))
  
  Allens_09 <- Allens_09 %>%
    filter(!str_detect(.$Befragte,"Bundestagswahl"))
  
  # filter for needed columns
  Allens_23 <- Allens_23 %>%
    select(Var.3, CDU.CSU, SPD, GRÜNE, FDP, LINKE, AfD, Sonstige)
  
  Allens_17 <- Allens_17 %>%
    select(Var.3, CDU.CSU, SPD, GRÜNE, FDP, LINKE, AfD, Sonstige)
  
  Allens_13 <- Allens_13 %>%
    select(Var.3, CDU.CSU, SPD, GRÜNE, FDP, LINKE, PIRATEN, AfD, Sonstige)
  
  Allens_09 <- Allens_09 %>%
    select(Var.3, CDU.CSU, SPD, GRÜNE, FDP, LINKE, Sonstige)
  
  # delete unnecessary rows
  Allens_23 <- Allens_23[-c(1,2,3),]
  
  Allens_17 <- Allens_17[-c(1,2,3),]
  
  Allens_13 <- Allens_13[-1,]
  
  Allens_09 <- Allens_09[-1,]
  
  
  Allens_23 <- Allens_23 %>%
    rename(Date = Var.3,
           CDU = CDU.CSU)  %>%
    mutate(CDU = gsub("%", "", gsub(",", ".", .$CDU)),
           SPD = gsub("%", "", gsub(",", ".", .$SPD)),
           GRÜNE = gsub("%", "", gsub(",", ".", .$GRÜNE)),
           FDP = gsub("%", "", gsub(",", ".", .$FDP)),
           LINKE = gsub("%", "", gsub(",", ".", .$LINKE)),
           AfD = gsub("%", "", gsub(",", ".", .$AfD)),
           Sonstige = gsub("[[:alpha:]]", "", gsub("%", "", gsub(",", ".", gsub("Sonst.", "", .$Sonstige)))))
  
  
  Allens_23 <- Allens_23 %>%
    mutate(Date = as.Date(Date, format = "%d.%m.%Y"),
           CDU = as.numeric(CDU),
           SPD = as.numeric(SPD),
           GRÜNE = as.numeric(GRÜNE),
           FDP = as.numeric(FDP),
           LINKE = as.numeric(LINKE),
           AfD = as.numeric(AfD),
           AfD = replace_na(AfD, 0),
           Sonstige = as.numeric(Sonstige))
  
  
  Allens_17 <- Allens_17 %>%
    rename(Date = Var.3,
           CDU = CDU.CSU) %>%
    mutate(CDU = gsub("%", "", gsub(",", ".", .$CDU)),
           SPD = gsub("%", "", gsub(",", ".", .$SPD)),
           GRÜNE = gsub("%", "", gsub(",", ".", .$GRÜNE)),
           FDP = gsub("%", "", gsub(",", ".", .$FDP)),
           LINKE = gsub("%", "", gsub(",", ".", .$LINKE)),
           AfD = gsub("%", "", gsub(",", ".", .$AfD)),
           Sonstige = gsub("[[:alpha:]]", "", gsub("%", "", gsub(",", ".", gsub("Sonst.", "", .$Sonstige)))))
  
  Allens_17 <- Allens_17 %>%
    mutate(Date = as.Date(Date, format = "%d.%m.%Y"),
           CDU = as.numeric(CDU),
           SPD = as.numeric(SPD),
           GRÜNE = as.numeric(GRÜNE),
           FDP = as.numeric(FDP),
           FDP = replace_na(FDP, 0),
           LINKE = as.numeric(LINKE),
           AfD = as.numeric(AfD),
           AfD = replace_na(AfD, 0),
           Sonstige = as.numeric(Sonstige))
  
  Allens_13 <- Allens_13 %>%
    rename(Date = Var.3,
           CDU = CDU.CSU) %>%
    mutate(CDU = gsub("%", "", gsub(",", ".", .$CDU)),
           SPD = gsub("%", "", gsub(",", ".", .$SPD)),
           GRÜNE = gsub("%", "", gsub(",", ".", .$GRÜNE)),
           FDP = gsub("%", "", gsub(",", ".", .$FDP)),
           LINKE = gsub("%", "", gsub(",", ".", .$LINKE)),
           AfD = gsub("%", "", gsub(",", ".", .$AfD)),
           PIRATEN = gsub("%", "", gsub(",", ".", .$PIRATEN)),
           Sonstige = gsub("[[:alpha:]]", "", gsub("%", "", gsub(",", ".", gsub("Sonst.", "", .$Sonstige)))),
           PIRATEN = as.numeric(PIRATEN),
           Sonstige = as.numeric(Sonstige),
           PIRATEN = replace_na(PIRATEN, 0),
           Sonstige = Sonstige + PIRATEN,
           Date = as.Date(Date, format = "%d.%m.%Y"),
           CDU = as.numeric(CDU),
           SPD = as.numeric(SPD),
           GRÜNE = as.numeric(GRÜNE),
           FDP = as.numeric(FDP),
           LINKE = as.numeric(LINKE),
           AfD = as.numeric(AfD),
           AfD = replace_na(AfD, 0)) %>%
    select(Date, CDU, SPD, GRÜNE, FDP, LINKE, AfD, Sonstige)
  
  
  
  Allens_09 <- Allens_09 %>%
    rename(Date = Var.3,
           CDU = CDU.CSU) %>%
    mutate(CDU = gsub("%", "", gsub(",", ".", .$CDU)),
           SPD = gsub("%", "", gsub(",", ".", .$SPD)),
           GRÜNE = gsub("%", "", gsub(",", ".", .$GRÜNE)),
           FDP = gsub("%", "", gsub(",", ".", .$FDP)),
           LINKE = gsub("%", "", gsub(",", ".", .$LINKE)),
           Sonstige = gsub("[[:alpha:]]", "", gsub("%", "", gsub(",", ".", gsub("Sonst.", "", .$Sonstige)))),
           Date = as.Date(Date, format = "%d.%m.%Y"),
           CDU = as.numeric(CDU),
           SPD = as.numeric(SPD),
           GRÜNE = as.numeric(GRÜNE),
           FDP = as.numeric(FDP),
           LINKE = as.numeric(LINKE),
           Sonstige = as.numeric(Sonstige)) 
  
  
  Allens_all <- bind_rows(Allens_23, Allens_17, Allens_13, Allens_09)
  
  Allens_all <- Allens_all %>%
    rename(Grüne = GRÜNE, 
           Linke = LINKE,
           AFD = AfD)
  
  
  
  write_csv(Allens_all, file = "data_polls_allens.csv")
  
  

  
  
  
  
# 4 FGW ####  
  
  
  
  
  url1 <- "https://www.wahlrecht.de/umfragen/politbarometer.htm"
  url2 <- "https://www.wahlrecht.de/umfragen/politbarometer/politbarometer-2017.htm"
  url3 <- "https://www.wahlrecht.de/umfragen/politbarometer/politbarometer-2013.htm"
  url4 <- "https://www.wahlrecht.de/umfragen/politbarometer/politbarometer-2009.htm"
  
  html1 <- read_html(url1)
  tables1 <- html_table(html1, fill=TRUE) 
  FGW_23 <- as.data.frame(tables1) 
  
  html2 <- read_html(url2)
  tables2 <- html_table(html2, fill=TRUE) 
  FGW_17 <- as.data.frame(tables2) 
  
  html3 <- read_html(url3)
  tables3 <- html_table(html3, fill=TRUE) 
  FGW_13 <- as.data.frame(tables3) 
  
  html4 <- read_html(url4)
  tables4 <- html_table(html4, fill=TRUE) 
  FGW_09 <- as.data.frame(tables4) 
  
  # filter out election results 
  FGW_23 <- FGW_23 %>%
    filter(!str_detect(.$X11,"Bundestagswahl"))
  
  FGW_17 <- FGW_17 %>%
    filter(!str_detect(.$X11,"Bundestagswahl"))
  
  FGW_13 <- FGW_13 %>%
    filter(!str_detect(.$Befragte,"Bundestagswahl"))
  
  FGW_09 <- FGW_09 %>%
    filter(!str_detect(.$Befragte,"Bundestagswahl"))
  
  # filter for needed columns
  FGW_23 <- FGW_23 %>%
    select(X1.1, X3, X4, X5, X6, X7, X8, X9)
  
  FGW_17 <- FGW_17 %>%
    select(X1.1, X3, X4, X5, X6, X7, X8, X9)
  
  FGW_13 <- FGW_13 %>%
    select(Var.3, CDU.CSU, SPD, GRÜNE, FDP, LINKE, PIRATEN, AfD, Sonstige)
  
  FGW_09 <- FGW_09 %>%
    select(Var.3, CDU.CSU, SPD, GRÜNE, FDP, LINKE, Sonstige)
  
  # delete unnecessary rows
  FGW_23 <- FGW_23[-c(1,2),]
  
  FGW_17 <- FGW_17[-c(1,2),]
  
  FGW_13 <- FGW_13[-1,]
  
  FGW_09 <- FGW_09[-c(1,2),]
  
  
  FGW_23 <- FGW_23 %>%
    rename(Date = X1.1, 
           CDU = X3,
           SPD = X4,
           GRÜNE = X5,
           FDP = X6,
           LINKE = X7,
           AfD = X8,
           Sonstige = X9) %>%
    mutate(CDU = gsub("%", "", gsub(",", ".", .$CDU)),
           SPD = gsub("%", "", gsub(",", ".", .$SPD)),
           GRÜNE = gsub("%", "", gsub(",", ".", .$GRÜNE)),
           FDP = gsub("%", "", gsub(",", ".", .$FDP)),
           LINKE = gsub("%", "", gsub(",", ".", .$LINKE)),
           AfD = gsub("%", "", gsub(",", ".", .$AfD)),
           Sonstige = gsub("[[:alpha:]]", "", gsub("%", "", gsub(",", ".", gsub("Sonst.", "", .$Sonstige)))))
  
  FGW_23$Sonstige <- sapply(strsplit(FGW_23$Sonstige, "  "), function(x) sum(as.integer(x)))
  
  FGW_23 <- FGW_23 %>%
    mutate(Date = as.Date(Date, format = "%d.%m.%Y"),
           CDU = as.numeric(CDU),
           SPD = as.numeric(SPD),
           GRÜNE = as.numeric(GRÜNE),
           FDP = as.numeric(FDP),
           LINKE = as.numeric(LINKE),
           AfD = as.numeric(AfD),
           AfD = replace_na(AfD, 0))
  
  
  FGW_17 <- FGW_17 %>%
    rename(Date = X1.1, 
           CDU = X3,
           SPD = X4,
           GRÜNE = X5,
           FDP = X6,
           LINKE = X7,
           AfD = X8,
           Sonstige = X9) %>%
    mutate(CDU = gsub("%", "", gsub(",", ".", .$CDU)),
           SPD = gsub("%", "", gsub(",", ".", .$SPD)),
           GRÜNE = gsub("%", "", gsub(",", ".", .$GRÜNE)),
           FDP = gsub("%", "", gsub(",", ".", .$FDP)),
           LINKE = gsub("%", "", gsub(",", ".", .$LINKE)),
           AfD = gsub("%", "", gsub(",", ".", .$AfD)),
           Sonstige = gsub("[[:alpha:]]", "", gsub("%", "", gsub(",", ".", gsub("Sonst.", "", .$Sonstige)))))
  
  FGW_17$Sonstige <- sapply(strsplit(FGW_17$Sonstige, "  "), function(x) sum(as.integer(x)))
  
  FGW_17 <- FGW_17 %>%
    mutate(Date = as.Date(Date, format = "%d.%m.%Y"),
           CDU = as.numeric(CDU),
           SPD = as.numeric(SPD),
           GRÜNE = as.numeric(GRÜNE),
           FDP = as.numeric(FDP),
           FDP = replace_na(FDP, 0),
           LINKE = as.numeric(LINKE),
           AfD = as.numeric(AfD),
           AfD = replace_na(AfD, 0))
  
  FGW_13 <- FGW_13 %>%
    rename(Date = Var.3,
           CDU = CDU.CSU) %>%
    mutate(CDU = gsub("%", "", gsub(",", ".", .$CDU)),
           SPD = gsub("%", "", gsub(",", ".", .$SPD)),
           GRÜNE = gsub("%", "", gsub(",", ".", .$GRÜNE)),
           FDP = gsub("%", "", gsub(",", ".", .$FDP)),
           LINKE = gsub("%", "", gsub(",", ".", .$LINKE)),
           AfD = gsub("%", "", gsub(",", ".", .$AfD)),
           PIRATEN = gsub("%", "", gsub(",", ".", .$PIRATEN)),
           Sonstige = gsub("[[:alpha:]]", "", gsub("%", "", gsub(",", ".", gsub("Sonst.", "", .$Sonstige)))),
           PIRATEN = as.numeric(PIRATEN),
           Sonstige = as.numeric(Sonstige),
           PIRATEN = replace_na(PIRATEN, 0),
           Sonstige = Sonstige + PIRATEN,
           Date = as.Date(Date, format = "%d.%m.%Y"),
           CDU = as.numeric(CDU),
           SPD = as.numeric(SPD),
           GRÜNE = as.numeric(GRÜNE),
           FDP = as.numeric(FDP),
           LINKE = as.numeric(LINKE),
           AfD = as.numeric(AfD),
           AfD = replace_na(AfD, 0)) %>%
    select(Date, CDU, SPD, GRÜNE, FDP, LINKE, AfD, Sonstige)
  
  
  
  FGW_09 <- FGW_09 %>%
    rename(Date = Var.3,
           CDU = CDU.CSU) %>%
    mutate(CDU = gsub("%", "", gsub(",", ".", .$CDU)),
           SPD = gsub("%", "", gsub(",", ".", .$SPD)),
           GRÜNE = gsub("%", "", gsub(",", ".", .$GRÜNE)),
           FDP = gsub("%", "", gsub(",", ".", .$FDP)),
           LINKE = gsub("%", "", gsub(",", ".", .$LINKE)),
           Sonstige = gsub("[[:alpha:]]", "", gsub("%", "", gsub(",", ".", gsub("Sonst.", "", .$Sonstige)))),
           Date = as.Date(Date, format = "%d.%m.%Y"),
           CDU = as.numeric(CDU),
           SPD = as.numeric(SPD),
           GRÜNE = as.numeric(GRÜNE),
           FDP = as.numeric(FDP),
           LINKE = as.numeric(LINKE),
           Sonstige = as.numeric(Sonstige)) 
  
  
  FGW_all <- bind_rows(FGW_23, FGW_17, FGW_13, FGW_09)
  
  FGW_all <- FGW_all %>%
    rename(Grüne = GRÜNE, 
           Linke = LINKE,
           AFD = AfD)
  

  write_csv(FGW_all, file = "data_polls_fgw.csv")
  
  
  
# 5 Forsa ####
  url1 <- "https://www.wahlrecht.de/umfragen/forsa.htm"
  url2 <- "https://www.wahlrecht.de/umfragen/forsa/2013.htm"
  
  html1 <- read_html(url1)
  tables1 <- html_table(html1, fill=TRUE) 
  forsa_23 <- as.data.frame(tables1) 
  
  html2 <- read_html(url2)
  tables2 <- html_table(html2, fill=TRUE) 
  forsa_13 <- as.data.frame(tables2) 
  
  
  # filter out election results 
  forsa_23 <- forsa_23 %>%
    filter(!str_detect(.$Zeitraum,"Bundestagswahl"))
  
  forsa_13 <- forsa_13 %>%
    filter(!str_detect(.$Zeitraum,"Bundestagswahl"))
  
  # filter for needed columns
  forsa_23 <- forsa_23 %>%
    select(Var.3, CDU.CSU, SPD, GRÜNE, FDP, LINKE, AfD, Sonstige)
  
  forsa_13 <- forsa_13 %>%
    select(Var.3, CDU.CSU, SPD, GRÜNE, FDP, LINKE, PIRATEN, AfD, Sonstige)
  
  # delete unnecessary rows
  forsa_13 <- forsa_13[-1,]
  
  forsa_23 <- forsa_23[-c(1,2,3),]
  
  
  forsa_13 <- forsa_13 %>%
    rename(Date = Var.3,
           CDU = CDU.CSU) %>%
    mutate(CDU = gsub("%", "", gsub(",", ".", forsa_13$CDU)),
           SPD = gsub("%", "", gsub(",", ".", forsa_13$SPD)),
           GRÜNE = gsub("%", "", gsub(",", ".", forsa_13$GRÜNE)),
           FDP = gsub("%", "", gsub(",", ".", forsa_13$FDP)),
           LINKE = gsub("%", "", gsub(",", ".", forsa_13$LINKE)),
           AfD = gsub("%", "", gsub(",", ".", forsa_13$AfD)),
           PIRATEN = gsub("%", "", gsub(",", ".", forsa_13$PIRATEN)),
           Sonstige = gsub("[[:alpha:]]", "", gsub("%", "", gsub(",", ".", gsub("Sonst.", "", forsa_13$Sonstige)))),
           PIRATEN = as.numeric(PIRATEN),
           Sonstige = as.numeric(Sonstige),
           PIRATEN = replace_na(PIRATEN, 0),
           Sonstige = Sonstige + PIRATEN,
           Date = as.Date(Date, format = "%d.%m.%Y"),
           CDU = as.numeric(CDU),
           SPD = as.numeric(SPD),
           GRÜNE = as.numeric(GRÜNE),
           FDP = as.numeric(FDP),
           LINKE = as.numeric(LINKE),
           AfD = as.numeric(AfD),
           AfD = replace_na(AfD, 0)) %>%
    select(Date, CDU, SPD, GRÜNE, FDP, LINKE, AfD, Sonstige)
  
  
  forsa_23 <- forsa_23 %>%
    rename(Date = Var.3,
           CDU = CDU.CSU) %>%
    mutate(CDU = gsub("%", "", gsub(",", ".", forsa_23$CDU)),
           SPD = gsub("%", "", gsub(",", ".", forsa_23$SPD)),
           GRÜNE = gsub("%", "", gsub(",", ".", forsa_23$GRÜNE)),
           FDP = gsub("%", "", gsub(",", ".", forsa_23$FDP)),
           LINKE = gsub("%", "", gsub(",", ".", forsa_23$LINKE)),
           AfD = gsub("%", "", gsub(",", ".", forsa_23$AfD)),
           Sonstige = gsub("[[:alpha:]]", "", gsub("%", "", gsub(",", ".", gsub("Sonst.", "", forsa_23$Sonstige)))))
  
  forsa_23$Sonstige <- sapply(strsplit(forsa_23$Sonstige, "  "), function(x) sum(as.integer(x)))
  
  forsa_23 <- forsa_23 %>%
    mutate(Date = as.Date(Date, format = "%d.%m.%Y"),
           CDU = as.numeric(CDU),
           SPD = as.numeric(SPD),
           GRÜNE = as.numeric(GRÜNE),
           FDP = as.numeric(FDP),
           LINKE = as.numeric(LINKE),
           AfD = as.numeric(AfD),
           AfD = replace_na(AfD, 0))
  
  
  
  Forsa_all <- bind_rows(forsa_23, forsa_13)
  
  Forsa_all <- Forsa_all %>%
    rename(Grüne = GRÜNE, 
           Linke = LINKE,
           AFD = AfD)
  
  write_csv(Forsa_all, file = "data_polls_forsa.csv")

  
  
# 6 Kantar ####
  url1 <- "https://www.wahlrecht.de/umfragen/emnid.htm"
  url2 <- "https://www.wahlrecht.de/umfragen/emnid/2013.htm"
  
  html1 <- read_html(url1)
  tables1 <- html_table(html1, fill=TRUE) 
  Kantar_23 <- as.data.frame(tables1) 
  
  html2 <- read_html(url2)
  tables2 <- html_table(html2, fill=TRUE) 
  Kantar_13 <- as.data.frame(tables2) 
  
  
  # filter out election results 
  Kantar_23 <- Kantar_23 %>%
    filter(!str_detect(.$Zeitraum,"Bundestagswahl"))
  
  Kantar_13 <- Kantar_13 %>%
    filter(!str_detect(.$X12,"Bundestagswahl"))
  
  # filter for needed columns
  Kantar_23 <- Kantar_23 %>%
    select(Var.3, CDU.CSU, SPD, GRÜNE, FDP, LINKE, AfD, Sonstige)
  
  Kantar_13 <- Kantar_13 %>%
    select(X1.1, X3, X4, X5, X6, X7, X8, X9, X10)
  
  # delete unnecessary rows
  Kantar_13 <- Kantar_13[-c(1,2,3, 4),]
  
  Kantar_23 <- Kantar_23[-c(1,2,3),]
  
  
  Kantar_13 <- Kantar_13 %>%
    rename(Date = X1.1, 
           CDU = X3,
           SPD = X4,
           GRÜNE = X5,
           FDP = X6,
           LINKE = X7,
           PIRATEN = X8,
           AfD = X9,
           Sonstige = X10) %>%
    mutate(CDU = gsub("%", "", gsub(",", ".", .$CDU)),
           SPD = gsub("%", "", gsub(",", ".", .$SPD)),
           GRÜNE = gsub("%", "", gsub(",", ".", .$GRÜNE)),
           FDP = gsub("%", "", gsub(",", ".", .$FDP)),
           LINKE = gsub("%", "", gsub(",", ".", .$LINKE)),
           PIRATEN = gsub("%", "", gsub(",", ".", .$PIRATEN)),
           AfD = gsub("%", "", gsub(",", ".", .$AfD)),
           Sonstige = gsub("[[:alpha:]]", "", gsub("%", "", gsub(",", ".", gsub("Sonst.", "", .$Sonstige)))),
           PIRATEN = as.numeric(PIRATEN),
           Sonstige = as.numeric(Sonstige),
           PIRATEN = replace_na(PIRATEN, 0),
           Sonstige = Sonstige + PIRATEN,
           Date = as.Date(Date, format = "%d.%m.%Y"),
           CDU = as.numeric(CDU),
           SPD = as.numeric(SPD),
           GRÜNE = as.numeric(GRÜNE),
           FDP = as.numeric(FDP),
           LINKE = as.numeric(LINKE),
           AfD = as.numeric(AfD),
           AfD = replace_na(AfD, 0),
           Sonstige = as.numeric(Sonstige)) %>%
    select(Date, CDU, SPD, GRÜNE, FDP, LINKE, AfD, Sonstige)
  
  
  Kantar_23 <- Kantar_23 %>%
    rename(Date = Var.3,
           CDU = CDU.CSU) %>%
    mutate(CDU = gsub("%", "", gsub(",", ".", Kantar_23$CDU)),
           SPD = gsub("%", "", gsub(",", ".", Kantar_23$SPD)),
           GRÜNE = gsub("%", "", gsub(",", ".", Kantar_23$GRÜNE)),
           FDP = gsub("%", "", gsub(",", ".", Kantar_23$FDP)),
           LINKE = gsub("%", "", gsub(",", ".", Kantar_23$LINKE)),
           AfD = gsub("%", "", gsub(",", ".", Kantar_23$AfD)),
           Sonstige = gsub("[[:alpha:]]", "", gsub("%", "", gsub(",", ".", gsub("Sonst.", "", Kantar_23$Sonstige)))))
  
  Kantar_23$Sonstige <- sapply(strsplit(Kantar_23$Sonstige, "  "), function(x) sum(as.integer(x)))
  
  Kantar_23 <- Kantar_23 %>%
    mutate(Date = as.Date(Date, format = "%d.%m.%Y"),
           CDU = as.numeric(CDU),
           SPD = as.numeric(SPD),
           GRÜNE = as.numeric(GRÜNE),
           FDP = as.numeric(FDP),
           LINKE = as.numeric(LINKE),
           AfD = as.numeric(AfD),
           AfD = replace_na(AfD, 0),
           Sonstige = as.numeric(Sonstige))
  
  
  
  Kantar_all <- bind_rows(Kantar_23, Kantar_13)
  
  Kantar_all <- Kantar_all %>%
    rename(Grüne = GRÜNE, 
           Linke = LINKE,
           AFD = AfD)
  
  

  write_csv(Kantar_all, file = "data_polls_kantar.csv")
  
  
  
  
  
  
  
  
