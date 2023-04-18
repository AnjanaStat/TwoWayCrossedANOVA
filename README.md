# TwoWayCrossedANOVA
 This package is for testing the equality of treatment effects against ordered alternatives in two-way ANOVA model with interaction. This is for testing the hypotheses described in the article "Testing for Trend in Two-Way Crossed Effects Model under Heteroscedasticity".
 
Inside the package, functions made for LRT, Min-Min-T, and Min-Max-T tests are respectively LRT ( ), MinMinT ( ), and MinMaxT ( ) with signatures LRT (data, a, b, alpha), MinMinT (data, a, b, alpha), and MinMaxT (data, a, b, alpha), respectively. Here all the arguments are mandatory. The first argument 'data' specifies the given data set, containing the response variables in the model. The given data set is to be provided in a particular manner. To test for increasing simple effects of levels 1,2,...,a of factor A across the levels 1,2,...,b of factor B the data set has to be in a form with ab columns. 
Put the observations of groups (1,1), (1,2),...,(1,b) in the first consecutive b columns, then put the observations of groups (2,1), (2,2),...,(2,b) in the next consecutive b columns. In this way, put the observations (a,1),(a,2),...,(a,b) in consecutive b columns next to the prior columns. Data set can be provided in .csv file. For more clarification, see the data-sets named 'WPI_JWPB', 'RPSM_NKP' and 'Grades' inside the package `TwoWayCrossedANOVA'. The second and third arguments 'a' and 'b' respectively defines the number of levels of factor A and the number of levels of factor B. Fourth argument 'alpha' indicates the level of significance.

To use the package in R software follow steps:
install_github("AnjanaStat/TwoWayCrossedANOVA") -> library(TwoWayCrossedANOVA). To use any of the functions, say, LRT, use LRT(data,a,b,alpha). For Min-Min-T and Min-Max-T tests corresponding functions are MinMinT(data,a,b,alpha) and MinMaxT(data,a,b,alpha) respectively.  
