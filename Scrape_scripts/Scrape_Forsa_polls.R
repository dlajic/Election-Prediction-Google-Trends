library(rvest)
library(stringi)
library(gsubfn)
library(tidyr)


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




save(Forsa_all, file = "Forsa_polls.RData")


# Load poll data set, scraped on 5th August 2022
load("infratest_dimap_polls_sonstige.RData")
