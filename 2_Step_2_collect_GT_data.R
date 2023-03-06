# Below we collect the GT data




# Scheduler ####
  ## Run this code in the console (and the script will run every hour)
  ## Run every day at the same time on 09:10, starting from tomorrow on
  ## Mark: change the format of startdate to your locale if needed (e.g. US: %m/%d/%Y)
    # library(taskscheduleR)
    # taskscheduler_create(taskname = "scheduler_collect_gt_data", 
    #                      rscript = "2_Step_2_collect_GT_data.R",
    #                      schedule = "HOUR", 
    #                      starttime = "14:00", 
    #                      modifier = 1)




# Collect GT data ####
## Packages ####
  
library(gtrendsR)


rm(list = ls())


# keyword=c() is equal to a comparison on the google trends website
# Using within keyword the pattern '""' equals the operator "" on the google trends website

# query for data of election 2017
trend_CDU_17 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne', 'Linke + Sarah Wagenknecht + Dietmar Bartsch', 
                                  'FDP + Christian Lindner'), geo= "DE" , category=19, time = "2017-01-01 2017-12-31", gprop="web", onlyInterest =  TRUE)

trend_AFD_17 = gtrends(keyword= c('Alice Weidel + Alexander Gauland + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne',
                                  'FDP + Christian Lindner'), geo= "DE" , category=19, time = "2017-01-01 2017-12-31", gprop="web", onlyInterest =  TRUE)

trend_sonst_17 = gtrends(keyword= c('Freie Wähler + Die Partei + Tierschutzpartei', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem Özdemir + Katrin Göring Eckardt + Grüne',
                                    'FDP + Christian Lindner'), geo= "DE" , category=19, time = "2017-01-01 2017-12-31", gprop="web", onlyInterest =  TRUE)



# query for data of election 2013
trend_CDU_13 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne + ', 'Linke + Sarah Wagenknecht + Gregor Gysi', 
                                  'FDP + Philipp Rösler'), geo= "DE" , category=19, time = "2013-01-01 2013-12-31", gprop="web", onlyInterest =  TRUE)

trend_AFD_13 = gtrends(keyword= c('Bernd Lucke + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne', 
                                  'FDP + Philipp Rösler'), geo= "DE" , category=19, time = "2013-01-01 2013-12-31", gprop="web", onlyInterest =  TRUE)

trend_sonst_13 = gtrends(keyword= c('Freie Wähler + Piraten + NPD', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbrück', 'Jürgen Trittin + Katrin Göring Eckardt + Grüne', 
                                    'FDP + Philipp Rösler'), geo= "DE" , category=19, time = "2013-01-01 2013-12-31", gprop="web", onlyInterest =  TRUE)



# query for data of election 2009
trend_09 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Grüne + Jürgen Trittin + Renate Künast', 'Linke + Gregor Gysi', 
                              'FDP + Guido Westerwelle'), geo= "DE" , category=19, time = "2009-01-01 2009-12-31", gprop="web", onlyInterest =  TRUE)

trend_sonst_09 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Grüne + Jürgen Trittin + Renate Künast', 'REP + Piraten + Tierschutzpartei + NPD', 
                                    'FDP + Guido Westerwelle'), geo= "DE" , category=19, time = "2009-01-01 2009-12-31", gprop="web", onlyInterest =  TRUE)


# query for data of election 2005
trend_05 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Gerhard Schröder', 'Grüne + Joschka Fischer', 'PDS + Linkspartei + Gregor Gysi', 
                              'FDP + Guido Westerwelle'), geo= "DE" , category=19, time = "2005-01-01 2005-12-31", gprop="web", onlyInterest =  TRUE)


trend_sonst_05 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Gerhard Schröder', 'Grüne + Joschka Fischer', 'NPD + REP + Graue', 
                                    'FDP + Guido Westerwelle'), geo= "DE" , category=19, time = "2005-01-01 2005-09-18", gprop="web", onlyInterest =  TRUE)


# query for prediction of election results 2021
# attention: only possible to get data for up to 36h before you search

trend_CDU_21 = gtrends(keyword= c('CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 'Linke + Janine Wissler + Dietmar Bartsch', 
                                  'FDP + Christian Lindner'), geo= "DE" , category=19, time = ("2021-01-01 2021-12-31"), gprop="web", onlyInterest =  TRUE)

trend_AFD_21 = gtrends(keyword= c('Alice Weidel + Tino Chrupalla + Afd', 'CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 
                                  'FDP + Christian Lindner'), geo= "DE" , category=19, time = ("2021-01-01 2021-12-31"), gprop="web", onlyInterest =  TRUE)

trend_sonst_21 = gtrends(keyword= c('Freie Wähler + Tierschutzpartei + dieBasis + Die Partei', 'CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Grüne', 
                                    'FDP + Christian Lindner'), geo= "DE" , category=19, time = ("2021-01-01 2021-12-31"), gprop="web", onlyInterest =  TRUE)



save.image(paste0("./Data_raw/", 
                  paste(gsub("\\s", "_", gsub(":", "-", Sys.time())),
                        ".RData",sep="")))




