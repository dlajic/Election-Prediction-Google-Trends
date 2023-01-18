library(taskscheduleR)

## Run every day at the same time on 09:10, starting from tomorrow on
## Mark: change the format of startdate to your locale if needed (e.g. US: %m/%d/%Y)

## Run every 3 hours
taskscheduler_create(taskname = "New", rscript = "C:/Users/deanl/Desktop/UniMannheim/ComSocScience/TermPaper/RMarkdown/PublData",
                     schedule = "HOUR", starttime = "14:17", modifier = 1)




#taskscheduler_delete(taskname = "New")
 
## Run every 24 hours
taskscheduler_create(taskname = "New", rscript = "C:/Users/deanl/Desktop/UniMannheim/ComSocScience/TermPaper/RMarkdown/SearchTermSelectionData.R",
                     schedule = "DAILY", starttime = "14:30", modifier = 1)


#taskscheduler_delete(taskname = "New")
