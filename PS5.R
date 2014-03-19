####################################################################################
########## Alex Jutca     ##########################################################
########## Gov 2001       ##########################################################
########## PS 5           ##########################################################
####################################################################################

######################################################################
### Question 1 #######################################################
######################################################################


###1.a

#Stochastic: Y_i \sim Y_{Bern} (y_i \vert \pi_i) = \pi_i^{y_i} (1-\pi_i) ^{y_i} =  \pi_i \space if \space  y = 1 \space and \space (1-\pi_i) \space if y=0
#\\
#Systematic: Pr(Y_i=1\vert \beta) \equiv \pi_i = \frac{1}{1+e^{-x_i \beta}}
#\\
#Independence \space assumption: Y_i \space and \space Y_j \space are \space independent \space \forall i\neq j \space conditional \space on \space X

###1.b

#Derive the log-likelihood for the model parameters.
# \sum_{i=1}^n y_i X_i \beta - X_i \beta - log(1 + e^{-X_i \beta})

###1.c
#Evaluate one modeling assumption
##independence assumption
##Since the authors' are assuming that their data is drawn from a Bernoulli distribution, they are assuming that any given country's involvement in a civil war is independent of any other country's involvement in a civil war. That might be violated since one country's involvement in a civil war could shift the probability distribution for another country's chances of entering civil war.

##1.D

logit.loglikelihood <- function(par, outcome, covariates){
if(!all(covariates[,1] == 1)){
 covariates <- as.matrix(cbind(1,covariates))
}
xb <- covariates %*% par
lll<-sum(outcome * xb - xb - log(1 + exp(-xb)))
return(lll)
}

##1.E
#Import data
library(foreign)
mydata <- read.dta("C:/Users/ajutca/Downloads/fearondata.dta")

subdata<-subset(mydata, select=c(onset, warl, gdpenl, lpopl1, lmtnest, 
                                 ncontig, Oil, nwstate, instab, polity2l, 
                                 ethfrac, relfrac))
newsub<-na.omit(subdata) #drop N.A.'s
newsub$onset[newsub$onset == 4] <- 1 #replace a 4 with a 1 in onset var

opt <- optim(par = rep(0, ncol(newsub[,2:length(newsub)]) + 1),
             fn = logit.loglikelihood,
             covariates = newsub[,2:length(newsub)],
             outcome = newsub$onset,
             control = list(fnscale = -1),
             hessian = T,
             method = "BFGS")
opt$par #coeffs
# -6.73153634 -0.95254630 -0.34378721  0.26294024  0.21885134  0.44282113  0.85765458  1.70866286  0.61699409  0.02086591  0.16657547  0.28504263

sqrt(diag(-solve(opt$hessian))) #s.e.'s
# 0.73577571 0.31423264 0.07180372 0.07270961 0.08475566 0.27400682 0.27919406 0.33867114 0.23511254 0.01677222 0.37311218 0.50876676

##1.F
#Using your estimates from 1.E what is the mean simulated predicted probability of civil war for oil exporters, with all other covariates are held at their median?
#So, we need to look where Oil == 1

#2. Draw our beta tildes from a multivariate normal distribution
# with mean and variance = our estimates
# to account for estimation uncertainty
# Note: make sure to use the entire variance covariance matrix!

library(mvtnorm)
set.seed(1234)

sim.betas<-rmvnorm(n=10000,
                   mean = opt$par,
                   sigma =  -solve(opt$hessian))


head(sim.betas) #take a look
dim(sim.betas)  # 10,000 x 12 matrix

#3. Set the values of X
X<-newsub[,2:length(newsub)]
Xc <- apply(X = cbind(1,X), MARGIN = 2, FUN = median)
Xc

Xc <-c(1,0,2.0109999,9.0069990,2.4248028,0,1,0,0,-3,0.3354173,0.3508000) #this vector represents every covariate at its med. and Oil = 1
Xc
Y<-newsub[,1]

#4 Calculate the pi. systematic component
# and
#5. Draw our Y tildes from the stochastic component
# to account for fundamental uncertainty

ev.ests <- c() # Create an empty vector
for(i in 1:10000){
  pi.tilde <- pnorm(Xc%*%sim.betas[i,])
  y.ests <- rbinom(10000, 1, pi.tilde)
  ev.ests[i] <- mean(y.ests)
}
head(ev.ests)
# Look at the results
hist(ev.ests,main = "Expected Values",
     xlab = "Predicted Probability of Civil War")
mean(ev.ests)
# 0.02941764

##1.G
#Same thing as 1.F, but with Oil = 0
Xc <-c(1,0,2.0109999,9.0069990,2.4248028,0,0,0,0,-3,0.3354173,0.3508000) #this vector represents every covariate at its med. and Oil = 0
ev.ests <- c() # Create an empty vector
for(i in 1:10000){
  pi.tilde <- pnorm(Xc%*%sim.betas[i,])
  y.ests <- rbinom(10000, 1, pi.tilde)
  ev.ests[i] <- mean(y.ests)
}
head(ev.ests)
# Look at the results
hist(ev.ests,main = "Expected Values",
     xlab = "Predicted Probability of Civil War")
mean(ev.ests)
#0.01219982

##Check results for 1.G & 1.F w/ Zelig 

install.packages("Zelig")
require(Zelig)
model.zelig <- zelig(onset ~ warl + gdpenl + lpopl1 + lmtnest + ncontig + Oil + nwstate + instab + polity2l + ethfrac + relfrac,
                     data = newsub,
                     model = "logit")

values <- setx(model.zelig, warl = 0, gdpenl = 2.0109999, lpopl1 = 9.0069990, lmtnest =  2.4248028, ncontig = 0, Oil = 1, nwstate = 0, instab = 0, polity2l = -3, ethfrac = 0.3354173, relfrac = 0.3508000)  
sims <- sim(model.zelig, values, num = 10000)
plot(sims)
sims$stats

values <- setx(model.zelig, warl = 0, gdpenl = 2.0109999, lpopl1 = 9.0069990, lmtnest =  2.4248028, ncontig = 0, Oil = 0, nwstate = 0, instab = 0, polity2l = -3, ethfrac = 0.3354173, relfrac = 0.3508000)  
sims <- sim(model.zelig, values, num = 10000)
plot(sims)
sims$stats

#Conclusion: My results approximate the Zelig output


##1.H
#Likelihood ratio test: compare full specification w/ one that excludes relfrac & ethfrac

restricted.data<-subset(newsub, select=c(onset, warl, gdpenl, lpopl1, lmtnest, 
                                 ncontig, Oil, nwstate, instab, polity2l))

unrestricted<-optim(par = rep(0, ncol(newsub[,2:length(newsub)]) + 1),
                    fn = logit.loglikelihood,
                    covariates = newsub[,2:length(newsub)],
                    outcome = newsub$onset,
                    control = list(fnscale = -1),
                    hessian = T,
                    method = "BFGS")
unrestricted$value
  
restricted<-optim(par = rep(0, ncol(restricted.data[,2:length(restricted.data)]) + 1),
                  fn = logit.loglikelihood,
                  covariates = restricted.data[,2:length(restricted.data)],
                  outcome = restricted.data$onset,
                  control = list(fnscale = -1),
                  hessian = T,
                  method = "BFGS")
restricted$value

#The test statistic
r <- 2*(unrestricted$value - restricted$value)
r

####C
# Calculate the p-value for this test statistic
1-pchisq(r,df=1)


##1.I
#Y_i  \overset{iid}{\sim} Bernoulli(\pi_i)\\
#\pi_i = \Phi(X_i \beta)

##1.J
#\sum_{i=1}^n y_i ln(\Phi(X_i \beta)) + (1-y_i)ln(1-\Phi(X_i \beta))

##1.K

probit.loglikelihood <- function(par, outcome, covariates){
  if(!all(covariates[,1] == 1)){
    covariates <- as.matrix(cbind(1,covariates))
  }
  phi <- pnorm(covariates %*% par, log=TRUE)
  opp.phi<-pnorm(covariates %*% par, log=TRUE, lower.tail = FALSE)
  llp<-sum(outcome * phi + (1-outcome)*opp.phi)
  return(llp)
}

##1.L
opt <- optim(par = rep(0, ncol(newsub[,2:length(newsub)]) + 1),
             fn = probit.loglikelihood,
             covariates = newsub[,2:length(newsub)],
             outcome = newsub$onset,
             control = list(fnscale = -1),
             hessian = T,
             method = "BFGS")
opt$par #coeffs
#0.351957979 
sqrt(diag(-solve(opt$hessian))) #s.e.'s
#0.123276535

######################################################################
### Question 2 #######################################################
######################################################################

##2.A
load("C:/Users/ajutca/Downloads/robust.RData")
simplereg<-lm(y~X1+X2)
summary(simplereg)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  15.9097     1.1749   13.54   <2e-16 ***
#  X1            6.6519     0.1893   35.15   <2e-16 ***
#  X2           -9.3496     0.2043  -45.76   <2e-16 ***

##2.B
bread <-vcov(simplereg)
library(sandwich)
est.fun<-estfun(simplereg)
meat <- t(est.fun)%*%est.fun
sandwich <- bread%*%meat%*%bread
N<-length(X1)
robust <- sandwich(simplereg, meat=crossprod(est.fun)/N)
library(lmtest)
coeftest(simplereg, robust)

#2.C
coeftest(simplereg,robust)[1,2]+coeftest(simplereg,robust)[2,2]+coeftest(simplereg,robust)[3,2]

#Check the robust s.e.'s
library(car)
hccm(simplereg,type=c("hc0"))
sqrt(diag(hccm(simplereg,type=c("hc0"))))
#OK, everything checks out

#2.D
#Yes, Robust S.E.'s are bigger in this case

#2.E
simplereg.res <-resid(simplereg)

pdf('rplot1.pdf')
plot(X1, simplereg.res, 
          ylab="Residuals", xlab="X1", 
          main="Residuals plot for X1") 
 abline(0, 0)                  # the horizon
graphics.off()

pdf('rplot2.pdf')
plot(X2, simplereg.res, 
     ylab="Residuals", xlab="X2", 
     main="Residuals plot for X2") 
abline(0, 0)    
graphics.off()

#2.F
plot(X1,y)
plot(X2,y)
new.y<-y+102 #ensure that y>0 so I can take logs
log.y<-log(new.y) #linearize
newreg<-lm(log.y~X1+X2)
summary(newreg) #homoskedastic errors

bread <-vcov(newreg)
library(sandwich)
est.fun<-estfun(newreg)
meat <- t(est.fun)%*%est.fun
sandwich <- bread%*%meat%*%bread
N<-length(X1)
robust <- sandwich(newreg, meat=crossprod(est.fun)/N)
library(lmtest)
coeftest(newreg, robust)

newreg.res <-resid(newreg)

pdf('residstransform1.pdf')
plot(X1, newreg.res, 
     ylab="Residuals", xlab="X1", 
     main="Residuals plot for X1") 
abline(0, 0)                  # the horizon
graphics.off()


pdf('residstransform2.pdf')
plot(X2, newreg.res, 
     ylab="Residuals", xlab="X2", 
     main="Residuals plot for X2") 
abline(0, 0)                  # the horizon
graphics.off()
