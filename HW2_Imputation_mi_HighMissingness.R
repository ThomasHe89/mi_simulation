### Imputations using MI ###
### High Missingness Rates ###

source("~/Dropbox/Multiple Imputation (NYU)/Corrections HW1/HW1_MI_Mayari.R")
setwd("~/Dropbox/Spring 2016/Imputation study/HW2")


#################################################################################################
#################################################################################################

###############################           MVN               #####################################

#################################################################################################
#################################################################################################

# Remember Means
# mvn_datasets -> c(5,33000,2,44,.6,100,23,8,76,1111)

#Impute using mi package


impute_mvn<-function(datasets_list,M=100,iter=30) {
  library(mi)
  miss       <-list()
  imputations<-list()
  imputed    <-list()
  means      <-matrix(rep(NA),M,ncol(datasets_list[[1]]))
  sds        <-matrix(rep(NA),M,ncol(datasets_list[[1]]))
  imeans     <-matrix(rep(NA),length(datasets_list),ncol(datasets_list[[1]]))
  isds       <-matrix(rep(NA),length(datasets_list),ncol(datasets_list[[1]]))
  i_mean_se  <-matrix(rep(NA),length(datasets_list),ncol(datasets_list[[1]]))
  i_sd_se    <-matrix(rep(NA),length(datasets_list),ncol(datasets_list[[1]]))
  Bias       <-matrix(rep(NA),length(datasets_list),ncol(datasets_list[[1]]))
  i_results  <-list()
  
  #Convert each data set to missing data frame
  for(s in 1:length(datasets_list)) {
    miss[[s]]<-missing_data.frame(datasets_list[[s]])
    # You could check how MI is planning to treat each variable with: show(miss)
    #impute
    imputations[[s]]<-mi(miss[[s]],n.iter=iter,n.chains=4,seed=2)
    #is there a way to tell the fuction to print the warnings
    #i.e. in my case it's warning it's not converging...
    #print(warnings())
    #Create 100 imputed data sets for each original dataset that had missingness
   
    imputed[[s]]<-complete(imputations[[s]],M)
    for (i in 1:M) {
      #Compute the mean for each imputed data set
       means[i,]<-as.matrix(apply(imputed[[s]][[i]],2,mean)[1:ncol(datasets_list[[s]])])
       means<-as.data.frame(means)
       sds[i,]<-as.matrix(apply(imputed[[s]][[i]],2,sd)[1:ncol(datasets_list[[s]])])
       sds<-as.data.frame(sds)
    }
    #Compute the mean imputed-mean for all M data sets and it's standard error
    imeans[s,]   <-apply(means,2,mean)[1:ncol(datasets_list[[s]])]
    i_mean_se[s,]<-apply(means,2,sd)[1:ncol(datasets_list[[s]])]
    
    #Compute the mean imputed-sd for all M data sets and it's standard error
    isds[s,]<-apply(sds,2,mean)[1:ncol(datasets_list[[s]])]
    i_sd_se[s,]<-apply(sds,2,sd)[1:ncol(datasets_list[[s]])] 
    
#    for(k in 1:ncol(datasets_list[[s]])){
      #Basic stats doubt: does it make sense to use the predicted means and the true means to estimate the MSE in this context? :S
#      MSE[s,k]<-(sum((means[,k]-as.matrix(apply(datasets_list[[s]],2,mean)[1:ncol(datasets_list[[s]])])[k])^2)/M
      #sqrt of (MSE minus variance of the estimated means for each sample dataset (but across imputed data sets))
#      Bias[s,k]<-sqrt(MSE[s,k]-(i_mean_se[s,k])^2)
#    }

  #Write the imputed data sets for stata
  #   file[s]<-paste("~/Dropbox/Multiple Imputation (NYU)/Corrections HW1/data_M/imputations_M",s,".dta",sep="")
  #   mi2stata(imputations[[s]], M, file[s])
  }
  i_results$Means<-data.frame(imeans)  #Then average over different sample data sets????
  i_results$MeansSE<-data.frame(i_mean_se)
  i_results$SD<-data.frame(isds)  #Then average over different sample data sets????
  i_results$SDSE<-data.frame(i_sd_se)
#  i_results$Bias<-Bias

  return(i_results)
}

# Remember Means
# mvn_datasets -> c(5,33000,2,44,.6,100,23,8,76,1111)

## MCAR ##
Imputed_MCAR<-impute_mvn(MCAR_MVN,iter=30)
str(Imputed_MCAR)
Imputed_MCAR$Means
# Impute_results$Bias

#At first sight: Looks gooooood!

## MAR ##
Imputed_MAR<-impute_mvn(MAR_mvn,iter=30)
str(Imputed_MAR)
Imputed_MAR$Means
# Impute_results$Bias

#At first sight: Looks gooooood!


## MNAR ##
Imputed_MNAR<-impute_mvn(MNAR_mvn,iter=30)
str(Imputed_MNAR)
Imputed_MNAR$Means
# Impute_results$Bias

#At first sight: Looks surprisingly good D:


#True means
true_means<-c(5,33000,2,44,.6,100,23,8,76,1111)
#True mean for Y
B<-(seq(-15,12,3))
Ymean<-sum(true_means*B )

true_means<-c(5,33000,2,44,.6,100,23,8,76,1111,Ymean)

#deviation from true means
dev_m<-function(imputations,datasets,M=100){
  dev_m     <-list()
  Dev_m     <-data.frame(matrix(rep(NA),nrow(imputations$Means),ncol(imputations$Means)))
  stdz_Dev_m<-data.frame(matrix(rep(NA),nrow(imputations$Means),ncol(imputations$Means)))
  
  for(s in 1:nrow(imputations$Means)){
   for (k in 1:ncol(imputations$Means)){
     for(m in 1:M){
      Dev_m[s,k]<-imputations$Means[s,k]-true_means[k]
      stdz_Dev_m[s,k]<- (imputations$Means[s,k]-true_means[k])/sd(datasets[[s]][,k]) #use true data?
     }
    }
  }
  dev_m$Dev_m<-Dev_m
  dev_m$STDZ<-stdz_Dev_m
return(dev_m)
}

## MCAR ##
dev_MCAR<-dev_m(imputations=Imputed_MCAR,datasets=mvn_datasets,M=100)

dev_MCAR[[2]]

#  Still looks gooooood ^_^

## MAR ##
dev_MAR<-dev_m(Imputed_MAR,mvn_datasets,M=100)

dev_MAR[[2]]

#Ahhmmmm not terrible? 7 and 10 not good?


## MNAR ##
dev_MNAR<-dev_m(Imputed_MNAR,mvn_datasets,M=100)

dev_MNAR[[2]]

# Variable 10 is pretty bad :(


library(knitr)
kable()


#################################################################################################
#################################################################################################

###############################         MIXED               #####################################

#################################################################################################
#################################################################################################


#Impute using mi package

#datasets_list<-MCAR_MIX
#M=100

numeric<-c(1,5,6,11)
categorical<-c(2:4,7:10)
impute_mix<-function(datasets_list,M=100,iter=30) {
  library(mi)
  miss          <-list()
  imputations   <-list()
  imputed       <-list()
  means         <-matrix(rep(NA),M,ncol(datasets_list[[1]]))
  #create empty list with pre-determined length
  probabilities <-as.list(rep(NA,ncol(datasets_list[[1]]))) 
  sds           <-matrix(rep(NA),M,ncol(datasets_list[[1]]))
  imeans        <-matrix(rep(NA),length(datasets_list),ncol(datasets_list[[1]]))
  iprobabilities<-as.list(rep(NA,length(datasets_list))) 
  iProb         <-list()
  isds          <-matrix(rep(NA),length(datasets_list),ncol(datasets_list[[1]]))
  i_mean_se     <-matrix(rep(NA),length(datasets_list),ncol(datasets_list[[1]]))
  i_sd_se       <-matrix(rep(NA),length(datasets_list),ncol(datasets_list[[1]]))
  Bias          <-matrix(rep(NA),length(datasets_list),ncol(datasets_list[[1]]))
  i_results     <-list()
  
  #Convert each data set to missing data frame
  for(s in 1:length(datasets_list)) {
    miss[[s]]<-missing_data.frame(datasets_list[[s]])
    # You could check how MI is planning to treat each variable with: show(miss)
    #impute
    imputations[[s]]<-mi(miss[[s]],n.iter=iter,n.chains=4,seed=2)
    #is there a way to tell the fuction to print the warnings
    #i.e. in my case it's warning it's not converging...
    #print(warnings())
    #Create 100 imputed data sets for each original dataset that had missingness
    
    imputed[[s]]<-complete(imputations[[s]],M)
    
    #Estimated parameters for continuous variables
    for (k in numeric){
      for (i in 1:M) {
      #Compute the mean for each imputed data set
      means[i,k]<-mean(as.numeric(as.character(imputed[[s]][[i]][,k])))
      means<-as.data.frame(means)
      sds[i,k]<-sd(as.numeric(as.character(imputed[[s]][[i]][,k])))
      sds<-as.data.frame(sds)
      }
    }
    #Estimated parameters for categorical variables
    for (p in categorical){
      prob<-matrix(rep(NA),M,length(prop.table(table(imputed[[s]][[1]][,p]))))
      for (i in 1:M) {
      prob[i,]<-prop.table(table(imputed[[s]][[i]][,p]))
      }
      names(probabilities)<-names(imputed[[1]][[1]][,1:ncol(datasets_list[[1]])])
      probabilities[[p]]<-apply(as.data.frame(prob),2,mean)
    }
    #Estimated parameters for categorical variables
    iprobabilities[[s]]<- probabilities
    #Compute the mean imputed-mean for all M data sets and it's standard error
    imeans[s,]   <-apply(means,2,mean)[1:ncol(datasets_list[[s]])]
    i_mean_se[s,]<-apply(means,2,sd)[1:ncol(datasets_list[[s]])]
    #Compute the mean imputed-sd for all M data sets and it's standard error
    isds[s,]<-apply(sds,2,mean)[1:ncol(datasets_list[[s]])]
    i_sd_se[s,]<-apply(sds,2,sd)[1:ncol(datasets_list[[s]])] 
    
    #    for(k in 1:ncol(datasets_list[[s]])){
    #Basic stats doubt: does it make sense to use the predicted means and the true means to estimate the MSE in this context? :S
    #      MSE[s,k]<-(sum((means[,k]-as.matrix(apply(datasets_list[[s]],2,mean)[1:ncol(datasets_list[[s]])])[k])^2))/M
    #sqrt of (MSE minus variance of the estimated means for each sample dataset (but across imputed data sets))
    #      Bias[s,k]<-sqrt(MSE[s,k]-(i_mean_se[s,k])^2)
    #    }
    
    #Write the imputed data sets for stata
    #   file[s]<-paste("~/Dropbox/Multiple Imputation (NYU)/Corrections HW1/data_M/imputations_M",s,".dta",sep="")
    #   mi2stata(imputations[[s]], M, file[s])
  }
  for (P in 1:length(probabilities)){
    bind<-character(length=10)
    for (S in 2:(length(iprobabilities)-1)){
    bind[1]  <-paste("as.matrix(iprobabilities[[",1,"]]","[P]),", sep="")
    bind[S]  <-paste(bind[S-1],"as.matrix(iprobabilities[[",S,"]]","[P]),", sep="")
    bind[S+1]<-paste("data.frame(rbind(",paste(bind[S],"as.matrix(iprobabilities[[",S+1,"]]","[P])", sep=""),"))",sep="")
    }
    iProb[[P]]<-eval(parse(text=bind[length(iprobabilities)])) #blagghhh finally it's working!
  }
  i_results$Means         <-data.frame(imeans)  #Then average over different sample data sets????
  i_results$Categoricals  <-iProb #Resulting probabilities from imputations of categorical variables
  i_results$MeansSE       <-data.frame(i_mean_se)
  i_results$SD            <-data.frame(isds)  #Then average over different sample data sets????
  i_results$SDSE          <-data.frame(i_sd_se)
  #  i_results$Bias<-Bias
  
  return(i_results)
}

################################################################################################
## MCAR ##
Imputed_MCAR_mix<-impute_mix(MCAR_MIX, iter=30,M=100)
#Verify means for continuous variables
Imputed_MCAR_mix$Means
Imputed_MCAR_mix$MeansSE

#Verify frequencies for categorical variables
Imputed_MCAR_mix$Categoricals

#At first sight
#var 2, some categories are a little off
#var 3, seems to be doing fine
#var 4, some categories are a little off

# mix_datasets -> 
variables<-c("1.Binary", "2.O.Categorical","3.O.Categorical","4.O.Categorical","5.Continuous","6.Continuous","7.U.Categorical","8.U.Categorical","9.U.Categorical",,"10.U.Categorical","11. Y")
Means <- c("0.65",
           "0.2,0.2,0.1,0.2,0.3",
           "0.4,0.3,0.2,0.06,0.04",
           "0.065,0.1825,0.3175,0.435",)


c("mvn_datasets", "mix_datasets", "MCAR_MVN", "MCAR_MIX",
  "MAR_mvn", "MAR_MIX", "MNAR_mvn", "MNAR_MIX")))

#Mix True Means
mean_1<-0.65
mean_2<-0.2*1+0.2*2+0.1*3+0.2*4+0.3*5
mean_3<-0.4*1+0.3*2+0.2*3+0.06*4+0.04*5
mean_4<-0.065*1+0.1825*2+0.3175*3+0.435*4
mean_5<-50+mean_1*3-0.2*mean_2-0.5*mean_3+0.4*mean_4
mean_6<-14.5-2.5*mean_1-4*mean_5
#Fixed prob issue on 7 and 8. I think we now have a valid DGP
prob_7<-c(0.05-0.00017*mean_6+0.002*mean_2,0.1-0.00015*mean_6+0.001*mean_2,0.3-0.0001*mean_6+0.0001*mean_2, 0.2, 0.05+0.0001*mean_6-0.0001*mean_2, 0.2+0.00015*mean_6-0.001*mean_2, 0.1+0.00017*mean_6-0.002*mean_2)
prob_8<-c(0.1383-0.00017*mean_5+0.02*mean_1, 0.1-0.00015*mean_5+0.01*mean_1, 0.3-0.0001*mean_5+0.001*mean_1, 0.1, 0.05+0.0001*mean_5-0.001*mean_1, 0.2+0.00015*mean_5-0.01*mean_1, 0.1+0.00017*mean_5-0.002*mean_1)
prob_9<-c(0.2*prob_7[1]+0.2*prob_7[2],0.2*prob_7[1]+0.2*prob_7[2],
0.2+sum(0.2*prob_7[3:7]),0.1+sum(0.2*prob_7[3:7]),0.1,0.1,0.1)
prob_10<-c(sum(0.2*prob_7[3:4]),sum(0.2*prob_7[3:4]),0.2+sum(0.2*prob_7[c(1:2,5:7)]),0.1+sum(0.2*prob_7[c(1:2,5:7)]),0.1,0.1,0.1)


################################################################################################
## MAR ##
Imputed_MAR_mix<-impute_mix(MAR_MIX, iter=30,M=100)
#Verify means for continuous variables
Imputed_MAR_mix$Means
Imputed_MAR_mix$MeansSE

#Verify frequencies for categorical variables
Imputed_MAR_mix$Categoricals

################################################################################################
## MNAR ##
Imputed_MNAR_mix<-impute_mix(MNAR_MIX, iter=30,M=100)
#Verify means for continuous variables
Imputed_MNAR_mix$Means
Imputed_MNAR_mix$MeansSE

#Verify frequencies for categorical variables
Imputed_MNAR_mix$Categoricals