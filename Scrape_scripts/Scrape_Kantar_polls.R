library(rvest)
library(stringi)
library(gsubfn)
library(tidyr)


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


save(Kantar_all, file = "Kantar_polls.RData")


