library(taskscheduleR)

## Run every day at the same time on 09:10, starting from tomorrow on
## Mark: change the format of startdate to your locale if needed (e.g. US: %m/%d/%Y)

## Run every 3 hours
taskscheduler_create(taskname = "New", rscript = "C:/Users/deanl/Desktop/UniMannheim/ComSocScience/TermPaper/RMarkdown/PublicData.R",
                     schedule = "HOUR", starttime = "00:13", modifier = 1)




#taskscheduler_delete(taskname = "New")
 
