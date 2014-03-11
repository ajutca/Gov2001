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
full <- lm(insured ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
           factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22)
summary(full)
##Public insurance
public <- lm(pubhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
             factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22)
summary(public)
##Private insurance
private <- lm(privhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
             factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22)
summary(private)


####Sample with parents 
##Any insurance
full.parent <- lm(insured ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
             factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
             a_parent>0)
summary(full.parent)
##Public insurance
public.parent <- lm(pubhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
               factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                 a_parent>0)
summary(public.parent)
##Private insurance
private.parent <- lm(privhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                  a_parent>0)
summary(private.parent)

####By income
###<150 FPL
##Any insurance
full.150 <- lm(insured ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                    factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                    a_parent>0&mydata$pov==1.5)
summary(full.150)
##Public insurance
public.150 <- lm(pubhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                      factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                      a_parent>0&mydata$pov==1.5)
summary(public.150)
##Private insurance
private.150 <- lm(privhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                       factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                       a_parent>0&mydata$pov==1.5)
summary(private.150)

####By income
###150-300 FPL
##Any insurance
full.300 <- lm(insured ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                 factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                 a_parent>0&mydata$pov==3)
summary(full.300)
##Public insurance
public.300 <- lm(pubhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                   factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                   a_parent>0&mydata$pov==3)
summary(public.300)
##Private insurance
private.300 <- lm(privhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                    factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                    a_parent>0&mydata$pov==3)
summary(private.300)

####By income
###>300 FPL
##Any insurance
full.high <- lm(insured ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                 factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                 a_parent>0&mydata$pov==4)
summary(full.high)
##Public insurance
public.high <- lm(pubhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                   factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                   a_parent>0&mydata$pov==4)
summary(public.high)
##Private insurance
private.high <- lm(privhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                    factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                    a_parent>0&mydata$pov==4)
summary(private.high)
