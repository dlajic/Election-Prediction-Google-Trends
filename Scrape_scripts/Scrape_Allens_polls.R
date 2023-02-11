library(rvest)
library(stringi)
library(gsubfn)
library(tidyr)


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



save(Allens_all, file = "Allens_polls.RData")



