#read data into table
x = read.table("patients1.txt", header = TRUE)
#change the nominal attributes to factors
for (i in 7:14) {
  x[, i] = as.factor(x[, i])
}
#build the model (don't forget to install package e1071
mod = naiveBayes(default ~ , x)
#examine the model
mod
