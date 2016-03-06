use "C:\Users\johndoe\Desktop\Dropbox\Multiple Imputation (NYU)\Corrections HW1\data_T\MCAR_MVN.dta" 
mi set wide
mi register imputed X2 X4 X5 X7 X8 X9 X10 Y 
mi register regular X1 X3 X6 
mi impute mvn X2 X4 X5 X7 X8 X9 X10 Y = X1 X3 X6, add(5) 
mi impute chained (pmm) X2 X4 X5 X7 X8 X9 X10 Y = X1 X3 X6, add(5) 

