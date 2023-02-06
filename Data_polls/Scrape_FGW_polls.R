library(rvest)
library(stringi)
library(gsubfn)
library(tidyr)


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




save(FGW_all, file = "FGW_polls.RData")



