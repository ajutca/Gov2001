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

# begin summary stats replication
