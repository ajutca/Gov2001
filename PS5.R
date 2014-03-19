##PS5

####################################################################################
########## Alex Jutca     ##########################################################
########## Gov 2001       ##########################################################
########## PS 5           ##########################################################
####################################################################################

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
#
