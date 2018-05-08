library(dplyr)
library(data.table)
library(foreach)
library(base)
require(Rmosek)

# Calculate mean of squared errors
meanSquareRoot = function(y_hat, y) {
  meanSquareRoot = mean((y - y_hat)^2)
  return(meanSquareRoot)
}

# Read train data
trainBlogData <- fread("blogData_train.csv")
#take 5000 rows from the dataset
trainBlogData = trainBlogData[1:5000]
train.rows <-sample(nrow(trainBlogData), 3000);
train.set <-trainBlogData[train.rows,]

# Read test data
test.set <-trainBlogData[-train.rows,]


# log-transform target column into a new column
train.set[, V281_new := log(1 + V281)]
test.set[, V281_new := log(1 + V281)]

# Select the basic features from the train data
b_features = subset(train.set, select = c(51:54,56:59))
test_b_features = subset(test.set, select = c(51:54,56:59))

# Defining X and y for the objective function 
y = train.set$V281_new 
y = as.matrix(y)

#y for test
y_test = test.set$V281_new 
y_test = as.matrix(y_test)

# Multiply the coefficients obtained from linear regression for each x variable (V51, V52...)
X = cbind(rep(1, length(y)),b_features$V51*0.00228,b_features$V52*0.0091,
          b_features$V53*0.00062,b_features$V54*-0.00177,
          b_features$V56*-0.0771,b_features$V57*0.0780,
          b_features$V58*0.00021,b_features$V59*0.0473)

# Function to perform rmosek optimization for quadratic equation (OLS)
solve.ols<-function(X,y, A, nrows, verb=1){

  
  # the following corresponds to x^2-xy (the objective function)
  xx<-crossprod(X) # X'X=Q variable in the Quadratic program
  c<--crossprod(X,y) # X'y=c variable in the Quadratic program 
  
  # Multiply X'X gives a matrix that has upper and lower triangle same.
  # to improve the calculation replacing the upper part of the matrix with zero and keeping the lower part.
  
  xx2<-xx
  xx2[upper.tri(xx)]<-0 #mosek needs Q to be triangular for quadratic calculations
  idx <- which(xx2 != 0, arr.ind=TRUE) #storing the index of the nonzero elements of Q
  
  #problem definition in Mosek
  quad1<-list() #empty list that contains the QP problem
  quad1$sense<-"min" #problem sense ---> minimize the squared loss.
  quad1$c<-as.vector(c) #objective coefficients
  quad1$qobj<-list(i = idx[,1],
                 j = idx[,2],
                 v = xx2[idx] ) #the Q matrix is imputed by the row indexes i, the col indexes j and the values v that define the Q matrix
  quad1$A<-A
  
  # Setting bounds for true y values (target column)
  quad1$bc<-rbind(blc=rep(min(cMatrix[,9]),nrows),
                buc=rep(max(cMatrix[,9]),nrows)) #constraint bounds
  
  # Each value is min/max of corresponding columns V51, V52...V60
  quad1$bx<-rbind(blx=c(0,0,0,0,0,0,0,0,0), bux=c(6.018593,1065,491,491,786,11,9,9,9)) #parameter bounds
  r<-mosek(quad1, opts = list(verbose = verb)) #call mosek solver
  return(r)
}


#Constraints for train using the train data cMatrix is created
cMatrix = subset(train.set, select = c(51:54, 56:59, 282))
cons = c()
for (i in 1:nrow(cMatrix)) {cons<-c(cons, cMatrix[i])}
A<-Matrix(as.numeric(cons), nrow=3000, ncol=9,byrow=TRUE, sparse=TRUE)
r<-solve.ols(X,y, A, 3000)

r$sol$itr$xx

# Calculate mean square error for the adhoc solution
y_a_hat = rowSums(X[,-1])#removing the random 1 column
#calculate mean square error for adhoc
meanSquareError1<-meanSquareError(y_a_hat,y)

# Calculate mean square error for the optimized solution using rmosek's output
Xoptimized = cbind(b_features$V51*r$sol$itr$xx[2],b_features$V52*r$sol$itr$xx[3],
             b_features$V53*r$sol$itr$xx[4],b_features$V54*r$sol$itr$xx[5],
             b_features$V56*r$sol$itr$xx[6],b_features$V57*r$sol$itr$xx[7],
             b_features$V58*r$sol$itr$xx[8],b_features$V59*r$sol$itr$xx[9])

y_o_hat = rowSums(Xoptimized)
#calculate mean square error for optimized
meanSquareError2<-meanSquareError(y_o_hat, y)

Xtest = cbind(rep(1, length(y_test)),test_b_features$V51*0.00228,test_b_features$V52*0.0091,
              test_b_features$V53*0.00062,test_b_features$V54*-0.00177,
              test_b_features$V56*-0.0771,test_b_features$V57*0.0780,
              test_b_features$V58*0.00021,test_b_features$V59*0.0473)

#Constraints for test
cMatrix_test = subset(test.set, select = c(51:54, 56:59,282))
cons_test = c()
for (i in 1:nrow(cMatrix_test)) {cons_test<-c(cons_test, cMatrix_test[i])}
A_test<-Matrix(as.numeric(cons_test), nrow=2000, ncol=9,byrow=TRUE, sparse=TRUE)
r_test<-solve.ols(X,y, A_test, 2000)

r_test$sol$itr$xx

# Calculate mean square error for the adhoc solution for test data
y_hat_test_adhoc = rowSums(Xtest[,-1])#removing the random 1 column
#calculate mean square error for adhoc
meanSquareError3<-meanSquareError(y_hat_test_adhoc,y_test)

# Calculate mean square error for the optimized solution using rmosek's output
Xtest_optimized = cbind(test_b_features$V51*r_test$sol$itr$xx[2],test_b_features$V52*r_test$sol$itr$xx[3],
                        test_b_features$V53*r_test$sol$itr$xx[4],test_b_features$V54*r_test$sol$itr$xx[5],
                        test_b_features$V56*r_test$sol$itr$xx[6],test_b_features$V57*r_test$sol$itr$xx[7],
                        test_b_features$V58*r_test$sol$itr$xx[8],test_b_features$V59*r_test$sol$itr$xx[9])

y_hat_test_optimized = rowSums(Xtest_optimized)
#calculate mean square error for optimized test data
meanSquareError4<-meanSquareError(y_hat_test_optimized, y_test)






