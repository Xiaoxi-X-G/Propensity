rm(list = ls())


RScriptPath<-"C:/gxx/r/project/USCriminal/"
Column.type <- c("POSIXct",  #Dates
                 "factor",  # Category
                 "character",  # Description
                 "factor", # DayofWeek
                 "factor", #PdDistrict
                 "factor",#Resolution
                 "factor", #Add
                 "numeric",
                 "numeric")

DataRaw <- read.csv(paste(RScriptPath, "train.csv", sep=''),
                    na.strings = c("", "NA"),
                    colClasses = Column.type)

DataRaw$Dates <- format(DataRaw$Dates, "%Y-%m-%d")

##### Visu #####
barplot(table(DataRaw$DayOfWeek))
barplot(table(DataRaw$PdDistrict))
barplot(table(DataRaw$Category))
