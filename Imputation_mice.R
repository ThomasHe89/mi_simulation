library(mice)

# create all data sets
# source("C:/Users/johndoe/Desktop/Dropbox/Multiple Imputation (NYU)/Corrections HW1/HW1_MI_Mayari.R")
setwd("C:/Users/johndoe/Desktop/Dropbox/Multiple Imputation (NYU)/Corrections HW1")

## Imputation MCAR_MVN
load("data_T/MCAR_MVN.RData")
MCAR_MVN <- object
head(MCAR_MVN[[1]])

# check corrs
cor.mtx <- cor(MCAR_MVN[[1]], use = "pairwise.complete.obs")
# return high corrs
cor.mtx.ind <- ifelse(cor.mtx > .05,0,1)
cor.mtx.ind[c(1,3,6),] <- 0
# look at missingness pattern
md.pattern(MCAR_MVN[[1]])
md.pairs(MCAR_MVN[[1]])
# impute
mice(MCAR_MVN[[1]], m=5, method = "norm", seed = 2527)
  # error!

mice(MCAR_MVN[[1]], m=5, method = "norm", 
     seed = 2527, pred = quickpred(MCAR_MVN[[1]]))
  # does not run (only works other way around)!

mice(MCAR_MVN[[1]], m=5, method = "norm", 
     pred = cor.mtx.ind, seed = 2527) 
  # runs but uses hardly anything for prediction

imp <- mice(MCAR_MVN[[1]], m=5, method = "norm", maxit = 100, 
     seed = 2527, ridge =.001)
  # stable and slightly biased

require(lattice)
densityplot(imp, scales = list(x = list(relation = "free")),
            layout = c(4, 2))
  # doesn't look good!
  # what happened to X2 and X10?
plot(imp, c("X2", "X10", "Y"))

# this is because of the ridiculously high means of X2 and X10!!
# looks like pmm solves this, but does it really?
imp.pmm <- mice(MCAR_MVN[[1]], m=5, method = "pmm", maxit = 100, 
            seed = 2527, ridge =.001)
densityplot(imp.pmm, scales = list(x = list(relation = "free")),
            layout = c(4, 2))

com <- complete(imp.pmm, "long")

for (colname in colnames(com[3:length(colnames(com))])) {
  cat("Var:", colname, "\n")
  for (i in unique(com$.imp)) {
    cat("m = ", i, ": mean: ", mean(com[com[,1] == i, colname]), ", sd: ", 
                               sd(com[com[,1] == i, colname]), "\n", sep = "")
  }
}

## Imputation MCAR_MIX
data <- MCAR_MIX[[1]]
head(data)
class(data)
md.pattern(data)

# fix classes for all variables
  # c(1,7:10) are factor
i_bin <- c(1, 7:10)
data[i_bin] <- lapply(data[i_bin], as.factor)
  # 2:4 are ordered
i_ord <- c(2:4)
data[i_ord] <- lapply(data[i_ord], as.ordered)

lapply(data, class)

# impute
imp <- mice(data, m=5, maxit = 20, seed = 2527,
            method = c("logreg", rep("polr", 3), rep("norm", 2), rep("polyreg", 4), "norm"))


# fit model of interest
ivs <- paste("X", 1:10, sep="")
form <- paste("Y ~", paste(ivs, collapse="+")) 
fit_imp <- with(imp, lm(as.formula(form)))
# summarize pooled coeffs
summary(pool(fit_imp))

# get complete data
data_compl <- mix_datasets[[1]]

# fix classes for all variables
  # c(1,7:10) are factor
i_bin <- c(1, 7:10)
data_compl[i_bin] <- lapply(data_compl[i_bin], as.factor)
  # 2:4 are ordered
i_ord <- c(2:4)
data_compl[i_ord] <- lapply(data_compl[i_ord], as.factor)

lapply(data_compl, class)


#  # set ref cat works like this (not necessary - works automatically!)
#data_compl <- within(data_compl, X1 <- relevel(X1, ref = 1))

# fit model with complete data
fit_compl <- with(data_compl, lm(as.formula(form)))
summary(fit_compl)
summary(fit_imp)

# compare compl vs. imp
assertthat::are_equal(rownames(summary(fit_compl)$coefficients[,1]), rownames(summary(pool(fit_imp))[,1]))
cbind(summary(fit_compl)$coefficients[,1], summary(pool(fit_imp))[,1])

############################################################
############################################################

# note that we are still violating assumptions
# too few iterations