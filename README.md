# TwoWayCrossedANOVA
This package is for testing the equality of treatment effects against ordered alternatives in two-way ANOVA model with interaction. The hypothses are described in the article "Testing for Trend in Two-Way Crossed Effects Model under Heteroscedasticity".
 
Inside the package, functions made for LRT, Min-Min-T, and Min-Max-T tests are respectively LRT(), MinMinT(), and MinMaxT() with signatures LRT(data, a, b, alpha), MinMinT(data, a, b, alpha), and MinMaxT(data, a, b, alpha), respectively. Here all the arguments are required to get results. The first argument 'data' specifies the given data set, containing the response variables in the model. The given data set is to be provided in a particular manner. To test for increasing simple effects of levels 1,2,...,a of factor A across the levels 1,2,...,b of factor B the data set has to be in a form with ab columns. Put the observations of groups (1,1), (1,2),...,(1,b) in the first consecutive b columns, then put the observations of groups (2,1), (2,2),...,(2,b) in the next consecutive b columns. In this way, put the observations (a,1),(a,2),...,(a,b) in consecutive b columns next to the prior columns. Data set can be in .csv file, which can be imported in 'R' by the function read.csv (file.choose()). For more clarification, see the data-sets named 'WPI_JWPB', 'RPSM_NKP' and 'Grades' in the package `TwoWayCrossedANOVA'. The second and third arguments 'a' and 'b' respectively defines the number of levels of factor A and the number of levels of factor B. Fourth argument 'alpha' indicates the level of significance. 
The functions LRT(), MinMinT() and MinMaxT() return the values of the test statistic, corresponding critical points, and decision (rejected or not).
To install packages from GitHub in R, user needs to install the package "devtools" first.
To use the package in R software run the following codes:
>install.packages("devtools")
>library(devtools)
>install_github("AnjanaStat/TwoWayCrossedANOVA") 
>library(TwoWayCrossedANOVA)
To get the results for the data `WPI_JWPB', use the following codes:
for loading the dataset (the data must be in .csv format)
>WPI_JWPB=read.csv(file.choose())
for likelihood ratio test
>LRT(data=WPI_JWPB,a=4,b=3,alpha=0.05)
for Min-Min-T test
>MinMinT(data=WPI_JWPB,a=4,b=3,alpha=0.05)
for Min-Max-T test 
>MinMaxT(data=WPI_JWPB,a=4,b=3,alpha=0.05)
