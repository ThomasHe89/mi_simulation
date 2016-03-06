### MI Assignments 1+2 ##
###                    ##


# settings <- function() {
#   x <- readline("Who are you (answer \"may\" or \"thomas\" (without \"\")? ")
#   if (x == "may") {
#     m_rates <- c(0,.8,0,.4,.5,0,.2,.3,.8,.9,.75)
#     cat("Hey May! You got your crazy missingness rates")
#   } else if (x == "thomas") {
#     m_rates <- c(0,.8,0,.4,.5,0,.2,.3,.8,.9,.75)/2.5
#     cat("Hey Thomas. You're the careful type, huh?")
#   } else {
#     stop("Hey stranger, try again.")    
#   }
#   return(m_rates)
# }
# 
# m_rates <- settings()
# m_rates

#############


########################################
# comment out the line you don't need. #
########################################

### THOMAS ###
# m_rates <- c(0,.8,0,.4,.5,0,.2,.3,.8,.9,.75)/2.5
# setwd("C:/Users/johndoe/Desktop/Dropbox/Multiple Imputation (NYU)/Corrections HW1/data_T")
### MAY ###
m_rates <- c(0,.8,0,.4,.5,0,.2,.3,.8,.9,.75)
setwd("~/Dropbox/Multiple Imputation (NYU)/Corrections HW1/data_M")

##############################################################################
##############################################################################
# Simulate Data Set #1
# From Multivariate Normal

set.seed(2534)

library(foreign)
library(MASS)
library(Matrix)
library(GenOrd)
library(arm)
library(foreign)



draw <- function(S = 10, n=1000, mu=c(5,33000,2,44,.6,100,23,8,76,1111)) { 
  data <- list()
  for (s in 1:S) {
    mvn <- data.frame(mvrnorm(n=n,mu=mu,Sigma=Covariance))
    #create an 11th variable "y" with mean structure that's linear combination of all others and interactions
    B<-matrix(seq(-15,12,3),10,1)
    e<-rnorm(1000,0,10000)
    Y<-as.matrix(mvn)%*% B +e
    mvn<-cbind(mvn,Y)
    data[[s]] <- mvn
  }
  return(data)
}

mu<-c(5,33000,2,44,.6,100,23,8,76,1111)
Corr<-matrix(NA,10,10)
for (i in 1:10){
  for (j in 1:10){
    Corr[i,j]<-runif(1,min=-1,max=1)
    Corr[j,i]<-Corr[i,j]
    Corr[i,i]<-1
  }
}
Corr<-nearPD(Corr, corr=TRUE)
Corr$mat
#But I was actually feeding the Corr matrix to the Sigma! Sorry!!! My very very bad!

#Standard deviations:
s<- c(1.5, 5000,0.4,8,0.1,10,5,2,10,200)
sm<-diag(s)
Covariance=sm %*%Corr$mat %*% sm

mvn_datasets<-draw(S=10,n=1000,mu=mu) #Alternatively, change mu to mu<-c(5,8,2,1,3,10,3.4,4,11,9) and go back to corr=var
str(mvn_datasets)


#summary(lm(formula = mvn[, 1] ~ mvn[, 2] + mvn[, 3] + mvn[, 4] + mvn[,5] + mvn[, 6] + mvn[, 7] + mvn[, 8] + mvn[, 9] + mvn[, 10]))

#write(mvn_datasets, "~/Dropbox/Spring 2016/Imputation study/HW1/mvn_complete.r")

##############################################################################
##############################################################################
# Simulate Data Set #2
# Mix of categorical (ordered and unordered) and continuous 

draw2 <- function(S = 1000) { 
  data <- list()
  for (s in 1:S) {
    
mix<-data.frame(matrix(NA,1000,10))

# 1 binary variable
mix[,1]<-rbinom(nrow(mix),1,.65)

# 3 Ordered Categoricals of which:

    # 2 variables are correlated with each other with rho=0.8 and have 5 categories each

    # Marginal probabilities
    marginal <- list(c(0.2,0.4,0.5,0.7),c(0.4,0.7,0.9,0.96))
    # Check the lower and upper bounds of the correlation coefficients.
    corrcheck(marginal)
    # Set the correlation coefficients
    R <- matrix(c(1,0.8,0.8,1),2,2) # Correlation matrix
    n <- 1000
    ##Selects and ordinal sample with given correlation R and given the marginals.
    cat<- ordsample(n, marginal, R)
    ##compare it with the pre-defined R
    cor(cat)

mix[,2:3]<-cat
  
    # I made the 3rd ordered categorical variable mix[,4] dependent on the binary variable mix[,1]
    # When generating the Y
  for (i in 1:1000){
  mix[i,4]  <-  sample(1:4,1,rep=TRUE,prob=c(0.1*mix[i,1], 0.15+0.05*mix[i,1], 0.3+0.05*(1-mix[i,1]), 0.4+0.1*(1-mix[i,1])))
}

# 2 continuous variables

    # mix[,5] dependent on the ordered categoricals and on the binary variable
  
  mix[,5]<-rnorm(1000,mean=50+mix[,1]*3-0.2*mix[,2]-0.5*mix[,3]+0.4*mix[,4],sd=7)

    # mix[,6] dependent on the binary variable and on the other continuous variable mix[,5]
  mix[,6]<-rnorm(1000,mean=14.5-2.5*mix[,1]-4*mix[,5], sd=4)

# 4 unordered categoricals

    #mix[,7] dependent on mix[,6] and mix[,2]
for (i in 1:1000){
  mix[i,7] <- sample(LETTERS[1:7], 1, replace=TRUE, prob=c(0.05-0.00017*mix[i,6]+0.002*mix[i,2], 0.1-0.00015*mix[i,6]+0.001*mix[i,2], 0.3-0.0001*mix[i,6]+0.0001*mix[i,2], 0.2, 0.05+0.0001*mix[i,6]-0.0001*mix[i,2], 0.2+0.00015*mix[i,6]-0.001*mix[i,2], 0.1+0.00017*mix[i,6]-0.002*mix[i,2]) )
}

    #mix[,8] dependent on mix[,1] and mix[,5]
for (i in 1:1000){
  mix[i,8] <- sample(LETTERS[1:7], 1, replace=TRUE, prob=c(0.1383-0.00017*mix[i,5]+0.02*mix[i,1], 0.1-0.00015*mix[i,5]+0.01*mix[i,1], 0.3-0.0001*mix[i,5]+0.001*mix[i,1], 0.1, 0.05+0.0001*mix[i,5]-0.001*mix[i,1], 0.2+0.00015*mix[i,5]-0.01*mix[i,1], 0.1+0.00017*mix[i,5]-0.002*mix[i,1]) )
}

    #mix[,9] and mix[,10] dependent on mix[,7]
    #If they answered A or B in mix[,7] then A and B are feasible choices in mix[,9]

for (i in 1:1000){
  mix[i,9] <- sample(LETTERS[1:7], 1, replace=TRUE, prob=c(0.2*(mix[i,7]=="A"|mix[i,7]=="B"),0.2*(mix[i,7]=="A"|mix[i,7]=="B"),0.2+0.2*!(mix[i,7]=="A"|mix[i,7]=="B"),0.1+0.2*!(mix[i,7]=="A"|mix[i,7]=="B"),0.1,0.1,0.1))
}

    #If they answered D or C in mix[,7] then A and B are feasible choices in mix[,10]

for (i in 1:1000){
  mix[i,10] <- sample(LETTERS[1:7], 1, replace=TRUE, prob=c(0.2*(mix[i,7]=="D"|mix[i,7]=="C"),0.2*(mix[i,7]=="D"|mix[i,7]=="C"),0.2+0.2*!(mix[i,7]=="D"|mix[i,7]=="C"),0.1+0.2*!(mix[i,7]=="D"|mix[i,7]=="C"),0.1,0.1,0.1))
}

#Augment with a Y 

#stepfun? not the same as 2*isTRUE(mix[,2]==1)+4*isTRUE(mix[,2]==2)+8*isTRUE(mix[,2]==3)+4*isTRUE(mix[,2]==4)+2*isTRUE(mix[,2]==5)
#ordered categorical 2: (x-2)^4-x^3-1
#ordered categorical 3: -x^3
#ordered categorical 4: log(x+5)

#interactions

#1 & 2   -> mix[,1]==1, (1/2*x-1)^3   #1 & 5 -> 7  #1 & 6 -> -.5   #5^2 ->.5 #6^2  -> -.1
#6 & 8 
#7 & 9

#stochastic term
e<-rnorm(nrow(mix),0,20)
#Y
Y<-  6.7*mix[,1]-
1*isTRUE(mix[,2]==1)-9*isTRUE(mix[,2]==2)-27*isTRUE(mix[,2]==3)-49*isTRUE(mix[,2]==4)-45*isTRUE(mix[,2]==5)-
  1*isTRUE(mix[,3]==1)-8*isTRUE(mix[,3]==2)-27*isTRUE(mix[,3]==3)-64*isTRUE(mix[,3]==4)-125*isTRUE(mix[,3]==5)+
  1.791759*isTRUE(mix[,4]==1)+1.945910*isTRUE(mix[,4]==2)+2.079442*isTRUE(mix[,4]==3)+2.197225*isTRUE(mix[,4]==4)+
  2.5*mix[,5]-
  .3*mix[,6]+
  3.5*isTRUE(mix[,7]=="A")+4*isTRUE(mix[,7]=="B")-5*isTRUE(mix[,7]=="C")+3*isTRUE(mix[,7]=="D")-.4*isTRUE(mix[,7]=="E")-3.5*isTRUE(mix[,7]=="F")+8*isTRUE(mix[,7]=="G")-
  12.5*isTRUE(mix[,8]=="A")+4.8*isTRUE(mix[,8]=="B")+15*isTRUE(mix[,8]=="C")+0*isTRUE(mix[,8]=="D")-14*isTRUE(mix[,8]=="E")+6*isTRUE(mix[,8]=="F")+18*isTRUE(mix[,8]=="G")+
  22.5*isTRUE(mix[,9]=="A")-8*isTRUE(mix[,9]=="B")+10*isTRUE(mix[,9]=="C")+5*isTRUE(mix[,9]=="D")+0*isTRUE(mix[,9]=="E")+16*isTRUE(mix[,9]=="F")+28*isTRUE(mix[,9]=="G")+
  0.5*isTRUE(mix[,10]=="A")-7*isTRUE(mix[,10]=="B")-1*isTRUE(mix[,10]=="C")+25*isTRUE(mix[,10]=="D")-10*isTRUE(mix[,10]=="E")+12*isTRUE(mix[,10]=="F")+13*isTRUE(mix[,10]=="G")-
#interactions
  0.125*mix[,1]*isTRUE(mix[,2]==1)+0*mix[,1]*isTRUE(mix[,2]==2)+0.125*mix[,1]*isTRUE(mix[,2]==3)+1*mix[,1]*isTRUE(mix[,2]==4)+3.375*mix[,1]*isTRUE(mix[,2]==5)-
  7*mix[,1]*mix[,5]-
  0.5*mix[,1]*mix[,6]+
  .5*mix[,5]*mix[,5]-
  .1*mix[,6]*mix[,6]-
  # two unordered cat
  .5*mix[,6]*isTRUE(mix[,8]=="A")+.59*mix[,6]*isTRUE(mix[,8]=="B")-9*mix[,6]*isTRUE(mix[,8]=="C")+19*mix[,6]*isTRUE(mix[,8]=="D")+1*mix[,6]*isTRUE(mix[,8]=="E")-10*mix[,6]*isTRUE(mix[,8]=="F")-11*mix[,6]*isTRUE(mix[,8]=="G")+
  1*isTRUE(mix[,7]=="A")*isTRUE(mix[,9]=="A")+3*isTRUE(mix[,7]=="B")*isTRUE(mix[,9]=="B")-1*isTRUE(mix[,7]=="C")*isTRUE(mix[,9]=="C")+7*isTRUE(mix[,7]=="D")*isTRUE(mix[,9]=="D")-3*isTRUE(mix[,7]=="E")*isTRUE(mix[,9]=="E")+4*isTRUE(mix[,7]=="F")*isTRUE(mix[,9]=="F")+14*isTRUE(mix[,7]=="F")*isTRUE(mix[,9]=="F")-
  2*isTRUE(mix[,7]=="A")*isTRUE(mix[,9]=="B")-2*isTRUE(mix[,7]=="B")*isTRUE(mix[,9]=="C")-5*isTRUE(mix[,7]=="C")*isTRUE(mix[,9]=="D")-4*isTRUE(mix[,7]=="D")*isTRUE(mix[,9]=="E")+5*isTRUE(mix[,7]=="E")*isTRUE(mix[,9]=="F")-6*isTRUE(mix[,7]=="F")*isTRUE(mix[,9]=="G")+
  2.5*isTRUE(mix[,7]=="A")*isTRUE(mix[,9]=="C")+4*isTRUE(mix[,7]=="B")*isTRUE(mix[,9]=="D")-2*isTRUE(mix[,7]=="C")*isTRUE(mix[,9]=="E")-1*isTRUE(mix[,7]=="D")*isTRUE(mix[,9]=="F")+6*isTRUE(mix[,7]=="E")*isTRUE(mix[,9]=="G")-
  1.5*isTRUE(mix[,7]=="A")*isTRUE(mix[,9]=="D")+0*isTRUE(mix[,7]=="B")*isTRUE(mix[,9]=="E")+0*isTRUE(mix[,7]=="C")*isTRUE(mix[,9]=="F")+1*isTRUE(mix[,7]=="D")*isTRUE(mix[,9]=="G")+
  0*isTRUE(mix[,7]=="A")*isTRUE(mix[,9]=="E")-2*isTRUE(mix[,7]=="B")*isTRUE(mix[,9]=="F")+0*isTRUE(mix[,7]=="C")*isTRUE(mix[,9]=="G")+
  e
  
mix<-cbind(mix,Y)
data[[s]] <- mix
  }
return(data)
}

mix_datasets<-draw2(S=10) #1000 is taking forever
#write.dta(mix, "~/Dropbox/Spring 2016/Imputation study/HW1/mix_complete.dta")

##############################################################################
##############################################################################

                      ## MISSING DATA MECHANISMS ##
                    #7 VARIABLES WITH MISSING DATA #

################################
# USING HORRIBLE RATES OF MISSINGNESS AS I USUALLY ENCOUNTER
#m_rates <- c(0,.8,0,.4,.5,0,.2,.3,.8,.9,.75) #last slot =missingness for Y
# OR, FOR THOMAS
#m_rates = c(0, .3, 0, .3, .3, 0, .3, .3, .3, .3, .3)

################################
## MCAR ##
################################

## old & wrong (does not give you exact missingness rates)! 
#fun_mcar<-function(datasets){
#  MCAR<-list()
#  for (s in 1:length(datasets)) {
#    MISS_MCAR<-matrix(0,nrow(datasets[[s]]),ncol(datasets[[s]]))
#    for (k in 1:ncol(datasets[[s]])) {
#    MISS_MCAR[,k] = rbinom(nrow(datasets[[s]]), 1, m_rates[k])
#  }
#  MCAR[[s]]<- datasets[[s]]
#  for (k in 1:ncol(datasets[[s]])) {
#    MCAR[[s]][MISS_MCAR[,k]==1,k] = NA}
#  }
#  return(MCAR)
#}

# corrected function:
fun_mcar<-function(datasets, m_rates = c(0, .3, 0, .3, .3, 0, .3, .3, .3, .3, .3)) {
  MCAR <- list()
  for (s in 1:length(datasets)) {
    MCAR[[s]]<- datasets[[s]]
    MISS_MCAR <- matrix(0,nrow(datasets[[s]]),ncol(datasets[[s]]))
    for (k in 1:ncol(datasets[[s]])) {
      random <- runif(nrow(datasets[[1]]), 0, 1)
      MISS_MCAR[,k] <- random < quantile(random, m_rates[k])
      MCAR[[s]][MISS_MCAR[,k]==1,k] = NA
    }
  }
  return(MCAR)
}

MCAR_MVN<-fun_mcar(mvn_datasets, m_rates=m_rates)
MCAR_MIX<-fun_mcar(mix_datasets, m_rates=m_rates)

#write.dta(MCAR_MVN, "~/Dropbox/Spring 2016/Imputation study/HW1/MCAR_MVN.dta")
#write.dta(MCAR_MIX, "~/Dropbox/Spring 2016/Imputation study/HW1/MCAR_MIX.dta")

# check if missingness rates behave as desired: 
# (takes a sample of 5 datasets and checks the proportion of missingness)

checkNArates <- function(datasets, samplesize = 5) {
  sample <- sample(1:length(datasets), samplesize)
  cat("These are the missingness rates for a sample of", samplesize, "datasets\n")
  for (s in sample) {
    print(apply(datasets[[s]], 2, FUN = function(x) sum(is.na(x))/nrow(datasets[[s]])))
  }
  cat("Are you happy with them, my dear May?")
}

checkNArates(MCAR_MVN)
checkNArates(MCAR_MIX)
# -> all good

################################
## MAR ##
################################

### For first Data Set MVN-MAR
fun_marmvn<- function(mvn_datasets, m_rates = c(0, .3, 0, .3, .3, 0, .3, .3, .3, .3, .3)) {
  MISS_MAR<-list()
  MISS_MAR1<-list()
  MAR<-list()
  for(s in 1:length(mvn_datasets)){
    MISS_MAR[[s]]<- matrix(rep(0),nrow(mvn_datasets[[s]]),ncol(mvn_datasets[[s]]))
    MISS_MAR1[[s]]<- matrix(rep(0),nrow(mvn_datasets[[s]]),ncol(mvn_datasets[[s]]))
    ps<-matrix(rep(0),nrow(mvn_datasets[[s]]),ncol(mvn_datasets[[s]]))
    ps1<-matrix(rep(0),nrow(mvn_datasets[[s]]),ncol(mvn_datasets[[s]]))
    w<-matrix(rep(0),nrow(mvn_datasets[[s]]),ncol(mvn_datasets[[s]]))
    qa<-matrix(rep(0),ncol(mvn_datasets[[s]]),1)
    qa1<-matrix(rep(0),ncol(mvn_datasets[[s]]),1)
    #Make mvn[,2] missingness dependent on mvn[,3] 80%
    q = quantile(mvn_datasets[[s]][,3],.8) 
    ps1[mvn_datasets[[s]][,3]<q,2] =1
    #Make mvn[,4] missingness dependent on mvn[,1] 40%
    ps1[,4] = .44*mvn_datasets[[s]][,1] + .3*(1-mvn_datasets[[s]][,1])
    #Make mvn[,5] missingness dependent on mvn[,1] and mvn[,3] 50%
    ps1[,5] <- invlogit(.5-.55*(mvn_datasets[[s]][,1]) - .05*(mvn_datasets[[s]][,3]))
    #Make mvn[,7] missingness dependent on mvn[,6] and mvn[,3] 20%
    ps1[,7] <- invlogit(-1.35-.004*(mvn_datasets[[s]][,6]) + .0001*(mvn_datasets[[s]][,3]))
    #Make mvn[,8] missingness dependent on mvn[,1] and mvn[,6]. aiming for 30%
    ps1[,8] <- invlogit(-1.36+.001*(mvn_datasets[[s]][,6]) + 1*(mvn_datasets[[s]][,1]))
    #Make mvn[,9] missingness dependent on mvn[,1], mvn[,3]. aiming for 80%
    ps1[,9] <- invlogit(1+0.62*mvn_datasets[[s]][,1]+8*mvn_datasets[[s]][,3])
    #Make mvn[,10] missingness dependent on mvn[,3] . aiming for 90%
    ps1[,10] <- invlogit(1+.975*mvn_datasets[[s]][,3])
    #Make Y's missingness dependent on mvn[,1], mvn[,3] and mvn[,6]. aiming for 75%
    ps1[,11] <- invlogit(0.5*mvn_datasets[[s]][,1]+1.58*(mvn_datasets[[s]][,3]-2)^4-0.005*mvn_datasets[[s]][,6])
    
    MAR[[s]]<-mvn_datasets[[s]]
    for (k in 1:ncol(mvn_datasets[[s]])){
      #Add noise
      # Though...I think I was already taking care of this before, by using the invlogit probabilities 
      # to randomly draw from a binomial distribution...
      w[,k]<-rnorm(length(ps1[,k]),mean=0,sd=(sd(ps1[,k])/2))
      ps[,k]<-ps1[,k]+w[,k]
      #Set cut-off to missingness rates
      qa[k]<- quantile(ps[,k],m_rates[k]) 
      qa1[k] <- quantile(ps1[,k],m_rates[k])
      MISS_MAR[[s]][ps[,k]<qa[k],k] =1
      MISS_MAR1[[s]][ps[,k]<qa1[k],k] =1
      #mean(MISS_MAR[[s]][,k])
      MAR[[s]][MISS_MAR[[s]][,k]==1,k] = NA
    }
  cat("Proportion of missingness that was \'predetermined\' in dataset s=", s, "\n", sep="")
  print(colMeans(MISS_MAR[[s]] == MISS_MAR1[[s]]))
  }
  return(MAR)
}

# cf. function output: the success of the SD technique for getting some randomness into the 
# assignment of NAs seems to be highly dependent on the missingness rates...
# MAYbe think of another technique?
# Remember m_rates<-c(0,.8,0,.4,.5,0,.2,.3,.8,.9,.75) #last slot =missingness for Y
MAR_mvn<-fun_marmvn(mvn_datasets, m_rates = m_rates)

# check NA rates
checkNArates(MAR_mvn)


### For second Data Set MIX-MAR

#If two variables are independent their missingness cannot depend on each other either?
#So only use for MAR the variables that are related according to the DGP?
#........................
####Uhmm not sure about this anymore, now I think there could be cases where the values are independent but the missingness isn't..


# Remember m_rates<-c(0,.8,0,.4,.5,0,.2,.3,.8,.9,.75) #last slot =missingness for Y

fun_marmix<- function(mix_datasets, m_rates = c(0, .3, 0, .3, .3, 0, .3, .3, .3, .3, .3)) {
  MISS_MAR <- list()
  MISS_MAR1 <- list()
  MAR <- list()
  for(s in 1:length(mix_datasets)){
    MISS_MAR[[s]]<- matrix(rep(0),nrow(mix_datasets[[s]]),ncol(mix_datasets[[s]]))
    MISS_MAR1[[s]]<- matrix(rep(0),nrow(mix_datasets[[s]]),ncol(mix_datasets[[s]]))
    ps<-matrix(rep(0),nrow(mix_datasets[[s]]),ncol(mix_datasets[[s]]))
    ps1<-matrix(rep(0),nrow(mix_datasets[[s]]),ncol(mix_datasets[[s]]))
    w<-matrix(rep(0),nrow(mix_datasets[[s]]),ncol(mix_datasets[[s]]))
    qa<-matrix(rep(0),ncol(mix_datasets[[s]]),1)
    qa1<-matrix(rep(0),ncol(mix_datasets[[s]]),1)
    #Make mix[,2] missingness dependent on mix[,3] 80%
    q = quantile(mix_datasets[[s]][,3],.8) 
    ps1[mix_datasets[[s]][,3]<q,2] =1
    #Make mix[,4] missingness dependent on mix[,1] 40%
    ps1[,4] = .44*mix_datasets[[s]][,1] + .3*(1-mix_datasets[[s]][,1])
    #Make mix[,5] missingness dependent on mix[,1] and mix[,3] 50%
    ps1[,5] <- invlogit(.5-.55*(mix_datasets[[s]][,1]) - .05*(mix_datasets[[s]][,3]))
    #Make mix[,7] missingness dependent on mix[,6] and mix[,3] 20%
    ps1[,7] <- invlogit(-1.35-.004*(mix_datasets[[s]][,6]) + .0001*(mix_datasets[[s]][,3]))
    #Make mix[,8] missingness dependent on mix[,1] and mix[,6]. aiming for 30%
    ps1[,8] <- invlogit(-1.36+.001*(mix_datasets[[s]][,6]) + 1*(mix_datasets[[s]][,1]))
    #Make mix[,9] missingness dependent on mix[,1], mix[,3]. aiming for 80%
    ps1[,9] <- invlogit(1+0.62*mix_datasets[[s]][,1]+8*(mix_datasets[[s]][,3]==1)+4*(mix_datasets[[s]][,3]=="4"))
    #Make mix[,10] missingness dependent on mix[,3]. aiming for 90%
    ps1[,10] <- .975*(mix_datasets[[s]][,3]==1|mix_datasets[[s]][,3]==2)+.713*!(mix_datasets[[s]][,3]==1|mix_datasets[[s]][,3]==2)
    #Make Y's missingness dependent on mix[,1], mix[,3] and mix[,6]. aiming for 75%
    ps1[,11] <- invlogit(0.5*mix_datasets[[s]][,1]+1.58*(mix_datasets[[s]][,3]-2)^4-0.005*mix_datasets[[s]][,6])
  
    MAR[[s]]<-mix_datasets[[s]]
    for (k in 1:ncol(mix_datasets[[s]])) {
      #Add noise
      #Though...I think I was already taking care of this before, by using the invlogit probabilities 
      # to randomly draw from a binomial distribution...
      w[,k]<-rnorm(length(ps1[,k]),mean=0,sd=(sd(ps1[,k])/2))
      ps[,k]<-ps1[,k]+w[,k]
      #Set cut-off to missingness rates
      qa[k]<- quantile(ps[,k],m_rates[k]) 
      qa1[k]<- quantile(ps1[,k],m_rates[k])
      MISS_MAR[[s]][ps[,k]<qa[k],k] =1
      MISS_MAR1[[s]][ps1[,k]<qa1[k],k] =1
      MAR[[s]][MISS_MAR[[s]][,k]==1,k] = NA
    }
  cat("Proportion of missingness that was \'predetermined\' in dataset s=", s, "\n", sep="")
  print(colMeans(MISS_MAR[[s]] == MISS_MAR1[[s]]))
  }
  return(MAR)
}

MAR_MIX<-fun_marmix(mix_datasets, m_rates = m_rates)
# check out the stats about 'predetermined' missingness in the function output (2 vars ~ .4)!!

#write.dta(MAR_MIX, "~/Dropbox/Spring 2016/Imputation study/HW1/MAR_MIX.dta")

# check NA rates
checkNArates(MAR_MIX)

################################
## MNAR ##
################################


### For MVN sample datasets

fun_MNARmvn<- function(mvn_datasets, m_rates = c(0, .3, 0, .3, .3, 0, .3, .3, .3, .3, .3)) {
  MISS_MNAR <- list()
  MISS_MNAR1 <- list()
  MNAR<-list()
  for(s in 1:length(mvn_datasets)){
    MISS_MNAR[[s]] <- matrix(rep(0),nrow(mvn_datasets[[s]]),ncol(mvn_datasets[[s]]))
    MISS_MNAR1[[s]] <- matrix(rep(0),nrow(mvn_datasets[[s]]),ncol(mvn_datasets[[s]]))
    ps <- matrix(rep(0),nrow(mvn_datasets[[s]]),ncol(mvn_datasets[[s]]))
    ps1 <- matrix(rep(0),nrow(mvn_datasets[[s]]),ncol(mvn_datasets[[s]]))
    w <- matrix(rep(0),nrow(mvn_datasets[[s]]),ncol(mvn_datasets[[s]]))
    qa <- matrix(rep(0),ncol(mvn_datasets[[s]]),1)
    qa1 <- matrix(rep(0),ncol(mvn_datasets[[s]]),1)
    #Make mvn[,2] missingness dependent on itself 80%
    q = quantile(mvn_datasets[[s]][,2],.8) 
    ps1[mvn_datasets[[s]][,2]<q,2] =1
    #Make mvn[,4] missingness dependent on itself 40%
    q = quantile(mvn_datasets[[s]][,4],.4) 
    ps1[mvn_datasets[[s]][,4]<q,4] =1    
    #Make mvn[,5] missingness dependent on mvn[,1], mvn[,2],mvn[,3] and mvn[,4]
    ps1[,5] <- invlogit(-.7*(mvn_datasets[[s]][,1]) + .0001*(mvn_datasets[[s]][,2]) - .8*(mvn_datasets[[s]][,3]) + .04*(mvn_datasets[[s]][,4]))
    #Make mvn[,7] missingness dependent on mvn[,6] and mvn[,2] 20%
    ps1[,7] <- invlogit(-1.45-.0004*(mvn_datasets[[s]][,6]) + .001*(mvn_datasets[[s]][,2])^(1/2))
    #Make mvn[,8] missingness dependent on mvn[,1] and mvn[,5]. aiming for 30%
    ps1[,8] <- invlogit(-1.36+.01*(mvn_datasets[[s]][,5]) + 1*(mvn_datasets[[s]][,1]))
    #Make mvn[,9] missingness dependent on mvn[,7]. aiming for 80%
    ps1[,9] <- invlogit(1.38+.01*log(mvn_datasets[[s]][,7]))
    #Make mvn[,10] missingness dependent on mvn[,7]. aiming for 90%
    ps1[,10] <- invlogit(.095*(mvn_datasets[[s]][,7]))    
    #Make Y's missingness dependent on mvn[,1], mvn[,2], mvn[,6], mvn[,7] and mvn[,10]. aiming for 75%
    ps1[,11] <- invlogit(0.5*mvn_datasets[[s]][,1]+1*((mvn_datasets[[s]][,2]-32990)/7)^4-2*mvn_datasets[[s]][,6]-1.14*(mvn_datasets[[s]][,7])+.2*mvn_datasets[[s]][,10])
    
    MNAR[[s]]<-mvn_datasets[[s]]
    for (k in 1:ncol(mvn_datasets[[s]])){
      #Add noise
      w[,k]<-rnorm(length(ps1[,k]),mean=0,sd=(sd(ps1[,k])/2))
      ps[,k]<-ps1[,k]+w[,k]
      #Set cut-off to missingness rates
      qa[k]<- quantile(ps[,k],m_rates[k])
      qa1[k]<- quantile(ps1[,k],m_rates[k])
      MISS_MNAR[[s]][ps[,k]<qa[k],k] =1
      MISS_MNAR1[[s]][ps1[,k]<qa1[k],k] =1
      MNAR[[s]][MISS_MNAR[[s]][,k]==1,k] = NA
    }
  cat("Proportion of missingness that was \'predetermined\' in dataset s=", s, "\n", sep="")
  print(colMeans(MISS_MNAR[[s]] == MISS_MNAR1[[s]]))
  }
  return(MNAR)
}

# cf. function output: again quite variable degrees of 'predetermined' missingness! 
MNAR_mvn<-fun_MNARmvn(mvn_datasets, m_rates = m_rates) 

checkNArates(MNAR_mvn)
# all good

### For MIXED sample datasets

fun_MNARmix <- function(mix_datasets, m_rates = c(0, .3, 0, .3, .3, 0, .3, .3, .3, .3, .3)) {
  MISS_MNAR <- list()
  MISS_MNAR1 <- list()
  MNAR <- list()
  for(s in 1:length(mix_datasets)){
    MISS_MNAR[[s]]<- matrix(rep(0),nrow(mix_datasets[[s]]),ncol(mix_datasets[[s]]))
    MISS_MNAR1[[s]]<- matrix(rep(0),nrow(mix_datasets[[s]]),ncol(mix_datasets[[s]]))
    ps<-matrix(rep(0),nrow(mix_datasets[[s]]),ncol(mix_datasets[[s]]))
    ps1<-matrix(rep(0),nrow(mix_datasets[[s]]),ncol(mix_datasets[[s]]))
    w<-matrix(rep(0),nrow(mix_datasets[[s]]),ncol(mix_datasets[[s]]))
    qa<-matrix(rep(0),ncol(mix_datasets[[s]]),1)
    qa1<-matrix(rep(0),ncol(mix_datasets[[s]]),1)
    #Make mix[,2] missingness dependent on itself 80%
    q = quantile(mix_datasets[[s]][,2],.8) 
    ps1[mix_datasets[[s]][,2]<q,2] =1
    #Make mix[,4] missingness dependent on itself 40%
    q = quantile(mix_datasets[[s]][,4],.4) 
    ps1[mix_datasets[[s]][,4]<q,4] =1    
    #Make mix[,5] missingness dependent on mix[,1], mix[,2],mix[,3] and mix[,4]
    ps1[,5] <- invlogit(.0005-.4*(mix_datasets[[s]][,1]) + .1*(mix_datasets[[s]][,2]) - .08*(mix_datasets[[s]][,3]) + .04*(mix_datasets[[s]][,4]))
    #Make mix[,7] missingness dependent on mix[,6] and mix[,2] 20%
    ps1[,7] <- invlogit(-1.35-.0004*(mix_datasets[[s]][,6]) + .1*(mix_datasets[[s]][,2]))
    #Make mix[,8] missingness dependent on mix[,1] and mix[,5]. aiming for 30%
    ps1[,8] <- invlogit(-1.36+.01*(mix_datasets[[s]][,5]) + 1*(mix_datasets[[s]][,1]))
    #Make mix[,9] missingness dependent on mix[,7]. aiming for 80%
    ps1[,9] <- invlogit(1.38+.01*(mix_datasets[[s]][,7]=="A"))
    #Make mix[,10] missingness dependent on mix[,7]. aiming for 90%
    ps1[,10] <- invlogit(1+30*(mix_datasets[[s]][,7]=="D"|mix_datasets[[s]][,7]=="C")+10*!(mix_datasets[[s]][,7]=="D"|mix_datasets[[s]][,7]=="C"))    
    #Make Y's missingness dependent on mix[,1], mix[,2], mix[,6], mix[,7] and mix[,10]. aiming for 75%
    ps1[,11] <- invlogit(0.5*mix_datasets[[s]][,1]+1*(mix_datasets[[s]][,2]-2)^4-0.005*mix_datasets[[s]][,6]+8*(mix_datasets[[s]][,7]=="A"|mix_datasets[[s]][,7]=="B")-100*(mix_datasets[[s]][,7]=="C"|mix_datasets[[s]][,7]=="E")+5*(mix_datasets[[s]][,10]=="G"))
    
    MNAR[[s]]<-mix_datasets[[s]]
    for (k in 1:ncol(mix_datasets[[s]])){
      #Add noise
      w[,k]<-rnorm(length(ps1[,k]),mean=0,sd=(sd(ps1[,k])/2))
      ps[,k]<-ps1[,k]+w[,k]
      #Set cut-off to missingness rates
      qa[k]<- quantile(ps[,k],m_rates[k]) 
      qa1[k]<- quantile(ps1[,k],m_rates[k])
      MISS_MNAR[[s]][ps[,k]<qa[k],k] =1
      MISS_MNAR1[[s]][ps1[,k]<qa1[k],k] =1
      MNAR[[s]][MISS_MNAR[[s]][,k]==1,k] = NA
    }
  cat("Proportion of missingness that was \'predetermined\' in dataset s=", s, "\n", sep="")
  print(colMeans(MISS_MNAR[[s]] == MISS_MNAR1[[s]]))
  }
  return(MNAR)
}

# check function output, 'predetermined' missingness rates are looking good (except maybe for X2...?)
MNAR_MIX<-fun_MNARmix(mix_datasets, m_rates = m_rates)

checkNArates(MNAR_MIX)
# all good!

######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################
############################################ SUMMARY #################################################


### FULL DATA SETS
# mvn_datasets
# mix_datasets

### MCAR DATA SETS
# MCAR_MVN
# MCAR_MIX

### MAR DATA SETS
# MAR_mvn
# MAR_MIX

### MNAR DATA SETS
# MNAR_mvn
# MNAR_MIX



save.image(file = "data_all.RData")
datasets <- c("mvn_datasets", "mix_datasets", "MCAR_MVN", "MCAR_MIX",
              "MAR_mvn", "MAR_MIX", "MNAR_mvn", "MNAR_MIX")

# remove all but these objects:
rm(list = setdiff(ls(), datasets))

for (data in ls()) {
  outfileR <- file.path(paste(data, ".RData", sep=""))
  outfileStata <- file.path(paste(data, ".dta", sep=""))
  object <- get(data)
  save(object, file = outfileR)
  write.dta(as.data.frame(object[[1]]), file = outfileStata)
}

# end of R file #
#################