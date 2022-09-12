library(gtrendsR)


rm(list = ls())

######
#below are the queries we used, if they are executed, then our used data is overwritten -> thus use r workspace "22_07_21_0UHR.Rdata"
######

# keyword=c() is equal to a comparison on the google trends website
# Using within keyword the pattern '""' equals the operator "" on the google trends website

# query for data of election 2017
trend_CDU_17 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem ?zdemir + Katrin G?ring Eckardt + Gr?ne', 'Linke + Sarah Wagenknecht + Dietmar Bartsch', 
                                  'FDP + Christian Lindner'), geo= "DE" , category=19, time = "2017-01-01 2017-12-31", gprop="web", onlyInterest =  TRUE)

trend_AFD_17 = gtrends(keyword= c('Alice Weidel + Alexander Gauland + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem ?zdemir + Katrin G?ring Eckardt + Gr?ne',
                                  'FDP + Christian Lindner'), geo= "DE" , category=19, time = "2017-01-01 2017-12-31", gprop="web", onlyInterest =  TRUE)

trend_sonst_17 = gtrends(keyword= c('Freie W?hler + Die Partei + Tierschutzpartei', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem ?zdemir + Katrin G?ring Eckardt + Gr?ne',
                                    'FDP + Christian Lindner'), geo= "DE" , category=19, time = "2017-01-01 2017-12-31", gprop="web", onlyInterest =  TRUE)



# query for data of election 2013
trend_CDU_13 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Peer Steinbr?ck', 'J?rgen Trittin + Katrin G?ring Eckardt + Gr?ne + ', 'Linke + Sarah Wagenknecht + Gregor Gysi', 
                                  'FDP + Philipp R?sler'), geo= "DE" , category=19, time = "2013-01-01 2013-12-31", gprop="web", onlyInterest =  TRUE)

trend_AFD_13 = gtrends(keyword= c('Bernd Lucke + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbr?ck', 'J?rgen Trittin + Katrin G?ring Eckardt + Gr?ne', 
                                  'FDP + Philipp R?sler'), geo= "DE" , category=19, time = "2013-01-01 2013-12-31", gprop="web", onlyInterest =  TRUE)

trend_sonst_13 = gtrends(keyword= c('Freie W?hler + Piraten + NPD', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbr?ck', 'J?rgen Trittin + Katrin G?ring Eckardt + Gr?ne', 
                                  'FDP + Philipp R?sler'), geo= "DE" , category=19, time = "2013-01-01 2013-12-31", gprop="web", onlyInterest =  TRUE)



# query for data of election 2009
trend_09 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Gr?ne + J?rgen Trittin + Renate K?nast', 'Linke + Gregor Gysi', 
                              'FDP + Guido Westerwelle'), geo= "DE" , category=19, time = "2009-01-01 2009-12-31", gprop="web", onlyInterest =  TRUE)

trend_sonst_09 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Gr?ne + J?rgen Trittin + Renate K?nast', 'REP + Piraten + Tierschutzpartei + NPD', 
                              'FDP + Guido Westerwelle'), geo= "DE" , category=19, time = "2009-01-01 2009-12-31", gprop="web", onlyInterest =  TRUE)


# query for data of election 2005
trend_05 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Gerhard Schr?der', 'Gr?ne + Joschka Fischer', 'PDS + Linkspartei + Gregor Gysi', 
                              'FDP + Guido Westerwelle'), geo= "DE" , category=19, time = "2005-01-01 2005-12-31", gprop="web", onlyInterest =  TRUE)


trend_sonst_05 = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Gerhard Schr?der', 'Gr?ne + Joschka Fischer', 'NPD + REP + Graue', 
                              'FDP + Guido Westerwelle'), geo= "DE" , category=19, time = "2005-01-01 2005-09-18", gprop="web", onlyInterest =  TRUE)


# query for prediction of election results 2021
# attention: only possible to get data for up to 36h before you search

trend_CDU_21 = gtrends(keyword= c('CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Gr?ne', 'Linke + Janine Wissler + Dietmar Bartsch', 
                                  'FDP + Christian Lindner'), geo= "DE" , category=19, time = ("2021-01-01 2021-12-31"), gprop="web", onlyInterest =  TRUE)

trend_AFD_21 = gtrends(keyword= c('Alice Weidel + Tino Chrupalla + Afd', 'CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Gr?ne', 
                                  'FDP + Christian Lindner'), geo= "DE" , category=19, time = ("2021-01-01 2021-12-31"), gprop="web", onlyInterest =  TRUE)

trend_sonst_21 = gtrends(keyword= c('Freie W?hler + Tierschutzpartei + dieBasis + Die Partei', 'CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Gr?ne', 
                                  'FDP + Christian Lindner'), geo= "DE" , category=19, time = ("2021-01-01 2021-12-31"), gprop="web", onlyInterest =  TRUE)




##################################
#### Without Category ###########
##############################

trend_CDU_17_WK = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem ?zdemir + Katrin G?ring Eckardt + Gr?ne', 'Linke + Sarah Wagenknecht + Dietmar Bartsch', 
                                  'FDP + Christian Lindner'), geo= "DE", time = "2017-01-01 2017-12-31", gprop="web", onlyInterest =  TRUE)

trend_AFD_17_WK = gtrends(keyword= c('Alice Weidel + Alexander Gauland + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem ?zdemir + Katrin G?ring Eckardt + Gr?ne',
                                  'FDP + Christian Lindner'), geo= "DE", time = "2017-01-01 2017-12-31", gprop="web", onlyInterest =  TRUE)

trend_sonst_17_WK = gtrends(keyword= c('Freie W?hler + Die Partei + Tierschutzpartei', 'CDU + CSU + Angela Merkel', 'SPD + Martin Schulz', 'Cem ?zdemir + Katrin G?ring Eckardt + Gr?ne',
                                    'FDP + Christian Lindner'), geo= "DE", time = "2017-01-01 2017-12-31", gprop="web", onlyInterest =  TRUE)



# query for data of election 2013
trend_CDU_13_WK = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Peer Steinbr?ck', 'J?rgen Trittin + Katrin G?ring Eckardt + Gr?ne + ', 'Linke + Sarah Wagenknecht + Gregor Gysi', 
                                  'FDP + Philipp R?sler'), geo= "DE", time = "2013-01-01 2013-12-31", gprop="web", onlyInterest =  TRUE)

trend_AFD_13_WK = gtrends(keyword= c('Bernd Lucke + Afd', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbr?ck', 'J?rgen Trittin + Katrin G?ring Eckardt + Gr?ne', 
                                  'FDP + Philipp R?sler'), geo= "DE", time = "2013-01-01 2013-12-31", gprop="web", onlyInterest =  TRUE)

trend_sonst_13_WK = gtrends(keyword= c('Freie W?hler + Piraten + NPD', 'CDU + CSU + Angela Merkel', 'SPD + Peer Steinbr?ck', 'J?rgen Trittin + Katrin G?ring Eckardt + Gr?ne', 
                                    'FDP + Philipp R?sler'), geo= "DE", time = "2013-01-01 2013-12-31", gprop="web", onlyInterest =  TRUE)



# query for data of election 2009
trend_09_WK = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Gr?ne + J?rgen Trittin + Renate K?nast', 'Linke + Gregor Gysi', 
                              'FDP + Guido Westerwelle'), geo= "DE", time = "2009-01-01 2009-12-31", gprop="web", onlyInterest =  TRUE)

trend_sonst_09_WK = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Frank Walter Steinmeier', 'Gr?ne + J?rgen Trittin + Renate K?nast', 'REP + Piraten + Tierschutzpartei + NPD', 
                                    'FDP + Guido Westerwelle'), geo= "DE", time = "2009-01-01 2009-12-31", gprop="web", onlyInterest =  TRUE)


# query for data of election 2005
trend_05_WK = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Gerhard Schr?der', 'Gr?ne + Joschka Fischer', 'PDS + Linkspartei + Gregor Gysi', 
                              'FDP + Guido Westerwelle'), geo= "DE", time = "2005-01-01 2005-12-31", gprop="web", onlyInterest =  TRUE)

trend_sonst_05_WK = gtrends(keyword= c('CDU + CSU + Angela Merkel', 'SPD + Gerhard Schr?der', 'Gr?ne + Joschka Fischer', 'NPD + REP + Graue', 
                                    'FDP + Guido Westerwelle'), geo= "DE", time = "2005-01-01 2005-09-18", gprop="web", onlyInterest =  TRUE)


# query for prediction of election results 2021
# attention: only possible to get data for up to 36h before you search

trend_CDU_21_WK = gtrends(keyword= c('CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Gr?ne', 'Linke + Janine Wissler + Dietmar Bartsch', 
                                  'FDP + Christian Lindner'), geo= "DE", time = ("2021-01-01 2021-12-31"), gprop="web", onlyInterest =  TRUE)

trend_AFD_21_WK = gtrends(keyword= c('Alice Weidel + Tino Chrupalla + Afd', 'CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Gr?ne', 
                                  'FDP + Christian Lindner'), geo= "DE", time = ("2021-01-01 2021-12-31"), gprop="web", onlyInterest =  TRUE)

trend_sonst_21_WK = gtrends(keyword= c('Freie W?hler + Tierschutzpartei + dieBasis + Die Partei', 'CDU + CSU + Armin Laschet', 'SPD + Olaf Scholz', 'Annalena Baerbock + Gr?ne', 
                                    'FDP + Christian Lindner'), geo= "DE", time = ("2021-01-01 2021-12-31"), gprop="web", onlyInterest =  TRUE)



filename = paste(gsub(":", "-", Sys.time()),".RData",sep="")
save.image(paste("C:/Users/deanl/Desktop/UniMannheim/ComSocScience/TermPaper/RMarkdown/",(filename)))

