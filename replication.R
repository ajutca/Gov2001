###Replication code for Levine, Phillip B., Robin McKnight, and Samantha Heep. 2011. 
###"How Effective Are Public Policies to Increase Health Insurance Coverage among Young Adults?" 
###American Economic Journal: Economic Policy, 3(1): 129-56. https://www.aeaweb.org/articles.php?doi=10.1257/pol.3.1.129

###Aaron Pervin and Alex Jutca
###Gov 2001
###3/10/2014

# initial steps
rm(list=ls())
setwd("C:/Users/ajutca/Documents/HKS/Gov2001")

# input Stata file
library(foreign)
mydata <- read.dta("C:/Users/ajutca/Documents/HKS/Gov2001/Replication/maindata.dta")
# AP - you'll need to change mydata and setwd

colnames(mydata, do.NULL = TRUE) #So we can call columns by name


##############################
##Summary Statistics##########
##############################
###mean(mydata[mydata$a_age>=16 & mydata$a_age<=22 & mydata$year<1997,c("insured","pubhi","privhi", "pub_ng", "privonly", "group","nongroup")])


##Full Sample
mydata.sub<-subset(mydata,mydata$year<1997 & mydata$a_age>=16 & mydata$a_age<=22)
colMeans(subset(mydata.sub, select = c("insured", "pubhi","privhi","pub_ng", "privonly", "group", "nongroup")), na.rm = TRUE)

##Full sample w/ parents
mydata.parents<-subset(mydata,mydata$year<1997 & mydata$a_age>=16 & mydata$a_age<=22 & a_parent>0)
colMeans(subset(mydata.parents, select = c("insured", "pubhi","privhi","pub_ng", "privonly", "group", "nongroup")), na.rm = TRUE)

##Income <= 150% of poverty line
mydata.pov150<-subset(mydata,mydata$year<1997 & mydata$a_age>=16 & mydata$a_age<=22 & a_parent>0 & pov==1.5)
colMeans(subset(mydata.pov150, select = c("insured", "pubhi","privhi","pub_ng", "privonly", "group", "nongroup")), na.rm = TRUE)

##Income 150% - 300% of poverty line
mydata.pov300<-subset(mydata,mydata$year<1997 & mydata$a_age>=16 & mydata$a_age<=22 & a_parent>0 & pov==3)
colMeans(subset(mydata.pov300, select = c("insured", "pubhi","privhi","pub_ng", "privonly", "group", "nongroup")), na.rm = TRUE)

##Income > 300% of poverty line
mydata.pov400<-subset(mydata,mydata$year<1997 & mydata$a_age>=16 & mydata$a_age<=22 & a_parent>0 & pov==4)
colMeans(subset(mydata.pov400, select = c("insured", "pubhi","privhi","pub_ng", "privonly", "group", "nongroup")), na.rm = TRUE)

##Group coverage
mydata.group<-subset(mydata,mydata$year<1997 & mydata$a_age>=16 & mydata$a_age<=22 & a_parent>0 & grouphi_any==1)
colMeans(subset(mydata.group, select = c("insured", "pubhi","privhi","pub_ng", "privonly", "group", "nongroup")), na.rm = TRUE)

##Non-group coverage
mydata.nongroup<-subset(mydata,mydata$year<1997 & mydata$a_age>=16 & mydata$a_age<=22 & a_parent>0 & grouphi_any==0)
colMeans(subset(mydata.nongroup, select = c("insured", "pubhi","privhi","pub_ng", "privonly", "group", "nongroup")), na.rm = TRUE)

################################
##Table 3 of the paper##########
################################

####Full Sample

##Any insurance

##Public insurance

##Private insurance
