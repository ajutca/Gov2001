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


##First, code clustered s.e.'s
#######################
###Clustered S.E.'s###
#######################

cl   <- function(dat,fm, cluster){
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) }
require(foreign)

########################
####Regressions#########
########################

####Full Sample 

full.sub<-subset(mydata,mydata$a_age>=16 & mydata$a_age<=22)

##Any insurance
full <- lm(insured ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
           factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22)
cl(full.sub,full,full.sub$a_age)
##Public insurance
public <- lm(pubhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
             factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22)
cl(full.sub,public,full.sub$a_age)
##Private insurance
private <- lm(privhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
             factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22)
cl(wparents.sub,private,wparents.sub$a_age)


####Sample with parents 

wparents.sub<-subset(mydata,mydata$a_age>=16 & mydata$a_age<=22 & a_parent>0)

##Any insurance
full.parent <- lm(insured ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
             factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
             a_parent>0)
cl(wparents.sub,full.parent,wparents.sub$a_age)
##Public insurance
public.parent <- lm(pubhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
               factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                 a_parent>0)
cl(wparents.sub,public.parent,wparents.sub$a_age)
##Private insurance
private.parent <- lm(privhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                  a_parent>0)
cl(wparents.sub,private.parent,wparents.sub$a_age)

####By income
###<150 FPL

fpl150.sub<-subset(mydata,mydata$a_age>=16 & mydata$a_age<=22 & a_parent>0 & mydata$pov==1.5)

##Any insurance
full.150 <- lm(insured ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                    factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                    a_parent>0&mydata$pov==1.5)
cl(fpl150.sub,full.150,fpl150.sub$a_age)
##Public insurance
public.150 <- lm(pubhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                      factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                      a_parent>0&mydata$pov==1.5)
cl(fpl150.sub,public.150,fpl150.sub$a_age)
##Private insurance
private.150 <- lm(privhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                       factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                       a_parent>0&mydata$pov==1.5)
cl(fpl150.sub,private.150,fpl150.sub$a_age)

####By income
###150-300 FPL

fpl300.sub<-subset(mydata,mydata$a_age>=16 & mydata$a_age<=22 & a_parent>0 & mydata$pov==3)

##Any insurance
full.300 <- lm(insured ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                 factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                 a_parent>0&mydata$pov==3)
cl(fpl300.sub,full.300,fpl300.sub$a_age)
##Public insurance
public.300 <- lm(pubhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                   factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                   a_parent>0&mydata$pov==3)
cl(fpl300.sub,public.300,fpl300.sub$a_age)
##Private insurance
private.300 <- lm(privhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                    factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                    a_parent>0&mydata$pov==3)
cl(fpl300.sub,private.300,fpl300.sub$a_age)

####By income
###>300 FPL

fpl400.sub<-subset(mydata,mydata$a_age>=16 & mydata$a_age<=22 & a_parent>0 & mydata$pov==4)

##Any insurance
full.high <- lm(insured ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                 factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                 a_parent>0&mydata$pov==4)
cl(fpl400.sub,full.high,fpl400.sub$a_age)
##Public insurance
public.high <- lm(pubhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                   factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                   a_parent>0&mydata$pov==4)
cl(fpl400.sub,public.high,fpl400.sub$a_age)
##Private insurance
private.high <- lm(privhi ~ elig_schip+ur+povratio+povratio2+withparent+married+student+female+ 
                    factor(stfips)+factor(year)+factor(a_age),data=mydata, subset=mydata$a_age>=16&mydata$a_age<=22&
                    a_parent>0&mydata$pov==4)
cl(fpl400.sub,private.high,fpl400.sub$a_age)
