##########################      PART 1      ##########################
library(lubridate)
library(reshape2)
library(ggplot2)
library(plyr)
library(zoo)

setwd("~/Desktop/Fall 2016/Tambe/HW 1")

###### PART 1A ######
#Load data Set#
foo = as.data.frame(read.csv("Rodent_Inspection.csv", header = TRUE))
foo = foo[order(foo$INSPECTION_DATE), ]

#Isolate month and year from Inspection date#
Insp_Date = parse_date_time(foo$INSPECTION_DATE, orders="mdy HM")

#Use data after 2011
foo1 = foo[which(year(Insp_Date) > 2011 & year(Insp_Date) <2017 & foo$INSPECTION_TYPE == "INITIAL"), ]
foo1 = foo1[order(foo$INSPECTION_DATE),]
foo2 = parse_date_time(foo1$INSPECTION_DATE, orders="mdy HM")
foo2 = foo2[order(year(foo2))]
Mon_Year = as.yearmon(paste(month(foo2),year(foo2), sep = "-"), "%m-%Y")
count = rename(as.data.frame(table(Mon_Year, foo1$BOROUGH)), c("Mon_Year"="Date", "Var2"="Borough"))
count = count[order(count$Date),]

ggplot(count, aes(Date,Freq, col=Borough, group = Borough)) + geom_point(size = 1) + theme(axis.text.x = element_text(size=8,angle=90,hjust=.5)) + geom_line() + ggtitle("Number of Rodent Inspections in NYC") + labs(y = "Number of Inspections")

###### PART 1B ######
eff_count = which(foo1$RESULT=="Active Rat Signs")
count2 = rename(as.data.frame(table(Mon_Year[eff_count], foo1[eff_count,]$BOROUGH)), c("Var1"="Date", "Var2"="Borough", "Freq" = "Active Count"))
count_active = merge(count, count2, all= T)
count_active$Efficiency = 100*count_active$`Active Count`/count_active$Freq
ggplot(count_active, aes(Date,Efficiency, col=Borough, group = Borough)) + geom_point(size = 1) + theme(axis.text.x = element_text(size=8,angle=90,hjust=.5)) + geom_line() + ggtitle("Efficiency of Rodent Inspections in NYC") +labs(y = "Efficiency (%)")

###### PART 1C ######
count_zip = rename(as.data.frame(table(foo1$ZIPCODE, foo1$RESULT)), c("Var1" = "ZipCode", "Var2" = "Result", "Freq" = "Num. Active Rat Signs"))
count_zipcode = count_zip[which(count_zip$Result=="Active Rat Signs"),]
count_zipcode = count_zipcode[order(count_zipcode$`Num. Active Rat Signs`, decreasing = T), ]
head(count_zipcode, 10)

###### PART 2A ######
foo_presandy = foo[which(year(Insp_Date) < 2012 & foo$INSPECTION_TYPE=="INITIAL"), ]
foo_postsandy = foo[which(year(Insp_Date) > 2012 & foo$INSPECTION_TYPE=="INITIAL"),]

#Pre-Sandy Most Active Rat Sightings by ZipCode
count_zip_Pre1 = rename(as.data.frame(table(foo_presandy$ZIPCODE, foo_presandy$RESULT)), c("Var1" = "ZipCode", "Var2" = "Result", "Freq" = "Num. Active Rat Signs Pre-Sandy"))
count_zip_Pre2 = count_zip_Pre1[which(count_zip_Pre1$Result=="Active Rat Signs"),]
count_zip_Pre2 = count_zip_Pre2[order(count_zip_Pre2$`Num. Active Rat Signs`, decreasing = T), ]
head(count_zip_Pre2, 20)
#Post-Sandy Most Active Rat Sightings by ZipCode
count_zip_Post1 = rename(as.data.frame(table(foo_postsandy$ZIPCODE, foo_postsandy$RESULT)), c("Var1" = "ZipCode", "Var2" = "Result", "Freq" = "Num. Active Rat Signs Post-Sandy"))
count_zip_Post2 = count_zip_Post1[which(count_zip_Post1$Result=="Active Rat Signs"),]
count_zip_Post2 = count_zip_Post2[order(count_zip_Post2$`Num. Active Rat Signs`, decreasing = T), ]
head(count_zip_Post2, 20)
#Most Active Rat Signs During Sandy
foo311 = as.data.frame(read.csv("sandyrelated.csv", header = TRUE))
count_sandy1 = rename(as.data.frame(table(foo311$Incident.Zip, foo311$Complaint.Type)), c("Var1" = "ZipCode", "Var2" = "Result", "Freq" = "Num. Active Rat Signs During Sandy"))
count_sandy2 = count_sandy1[which(count_sandy1$Result=="Rodent"),]
count_sandy2 = count_sandy2[order(count_sandy2$`Num. Active Rat Signs`, decreasing = T), ]
head(count_sandy2, 20)

###### PART 3A ######
#Step1
foo_fin = foo[which(foo$INSPECTION_TYPE == "INITIAL", year(Insp_Date)<2017), ]
foo_fin2 = parse_date_time(foo_fin$INSPECTION_DATE, orders="mdy HM")
foo_fin2 = foo_fin2[order(year(foo_fin2))]
Mon_Year2 = paste(month(foo_fin2, label = T, abbr = T), year(foo_fin2), sep = "-")
count_fin = rename(as.data.frame(table(Mon_Year2, foo_fin$ZIPCODE)), c("Mon_Year2"="Date", "Var2"="ZipCode", "Freq"="Num Inspections"))
count_fin = count_fin[order(count_fin$Date),]

eff_count2 = which(foo_fin$RESULT=="Active Rat Signs")
count_fin2 = rename(as.data.frame(table(Mon_Year2[eff_count2], foo_fin[eff_count2,]$ZIPCODE)), c("Var1"="Date", "Var2"="ZipCode", "Freq" = "Active Count"))
count_active2 = merge(count_fin, count_fin2, all= T)
count_active2$Efficiency = 100*count_active2$`Active Count`/count_active2$`Num Inspections`

#Step2
foo_rest = as.data.frame(read.csv("DOHMH_New_York_City_Restaurant_Inspection_Results.csv", header = TRUE))
foo_rest2 = parse_date_time(foo_rest$INSPECTION.DATE, orders="mdy")
Mon_Year3 = paste(month(foo_rest2, label = T, abbr = T),year(foo_rest2), sep = "-")
foo_rest$Date = Mon_Year3
foo_rest = rename(foo_rest, c("ZIPCODE" = "ZipCode"))
foo_restfin = merge(foo_rest,count_active2, by = c("Date", "ZipCode"))

#Step3
foo_restfin$RatViolation = foo_restfin$VIOLATION.CODE %in% c("O4L", "O4K", "08A") 
foo_restfin$Month = factor(month(parse_date_time(foo_restfin$INSPECTION.DATE, orders="mdy")))
foo_restfin$Year = factor(year(parse_date_time(foo_restfin$INSPECTION.DATE, orders="mdy")))
foo_mod = glm(RatViolation ~ Month+Year+Efficiency, data = foo_restfin, family = binomial)
summary(foo_mod)



