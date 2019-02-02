library(RCurl)
library(RJSONIO)

airline.data=getURL("http://ist.gmu.edu/~hpurohit/courses/ait582-proj-data-spring16.json")
dataset=fromJSON(airline.data)
airline.data=do.call(rbind,dataset)
write.csv(airline.data,"airline.data.csv")

airdata=airline.data[-1,]
airdata=data.frame(airdata)
airdata$DESCRIPTION=as.character(airdata$DESCRIPTION)

library(stringr)

airdata$Name = sapply(strsplit(as.character(airdata$DESCRIPTION), split = ";"), "[", 1)
airdata$LastName = sapply(strsplit(as.character(airdata$Name), split = ","), "[", 1)
FirstName = sapply(strsplit(as.character(airdata$Name), split = ","), "[", 2)
airdata$prefix = sapply(strsplit(as.character(FirstName),split= ". "),"[",1)


lakshmi <- str_split(FirstName, ". ", 2)
(fname <- data.frame(
prefix = sapply(lakshmi, head, n = 1),
firstname  = sapply(lakshmi, tail, n = 1)
))
first=fname[,1]
airdata$FirstName=fname
airdata$FirstName=fname[,2]
Name=FirstName
Gender=str_extract(string = Name,pattern = "(Mr|Miss|Mrs|Master)\\.")
airdata=cbind(airdata,Gender)
airdata=airdata[,-7]

#airdata<- setNames(airdata, c("fare","description","success","seatclass","guests","customerid","lastname","prefix","firstname","Gender"))
airdata$Gender=ifelse(grepl("Mrs",airdata$Gender),"Female",ifelse(grepl("Miss",airdata$Gender),
                                                                 "Female",ifelse(grepl("Master",airdata$Gender),"Male",ifelse(grepl("Mr",airdata$Gender),"Male",""))))

airdata$Age = sapply(strsplit(as.character(airdata$DESCRIPTION), split = ";"), "[", 2)


airdata[[11]] <- as.numeric(as.character(airdata[[11]]))
for(i in 1:nrow(airdata))
  {
  if (is.na(airdata$Age[i])== T)
    {
    airdata$Age[i]=floor(mean(airdata$Age, na.rm =T))
  }
}
mean(airdata$Age)
airdata[airdata$Age == "NA"] <- mean(airdata$Age)
airdata$agecategory[airdata$Age > 60] <- "Senior Citizen"
airdata$agecategory[airdata$Age > 30 & airdata$Age <= 60] <- "Middle Aged"
airdata$agecategory[airdata$Age >18 & airdata$Age <=30 ] = "Young"
airdata$agecategory[airdata$Age >12 & airdata$Age<=18] = "Teenage"

airdata$agecategory[airdata$Age > 3 & airdata$Age <= 12]="minor"
airdata$agecategory[airdata$Age<=3]="Infant"
airdata=airdata[,-2]
#airdata=airdata[,-9]
#airdata=airdata[,-7]
#airdata=airdata[,-2]


#airdata=airdata[,c(6,3,4,5,1,2,8,9,7,10,11,12)]
write.csv(airdata,file="airlinedata2_lakshmi.csv")
getwd()

