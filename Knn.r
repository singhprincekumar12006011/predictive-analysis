# knn

#importing the dataset
data = read.csv("wisc_bc_data.csv")
View(data)

#the first of the data set is id and it is not usefull so exclude id
data = data[-1]
View(data)

#structure  of dataset
str(data)
table(data$diagnosis)

#the data set of disgnosis column in character
#converting into factor
data$diagnosis = factor(data$diagnosis, 
                        levels = c("B","M"),
                        labels = c("Benign","Malignent"))
class(data$diagnosis)

#create normalize function
normalise = function(x){
  return(((x)-min(x))/(max(x)-min(x)))
}
data_n = as.data.frame(lapply(data[2:31],
                            normalise))
str(data)

#splite the dataset into training and test set
data_train = data_n[1:469,]
data_test = data_n[470:569,]

#we have to store the dependent variable in split dataset
data_train_label = data[1:469,1]
data_test_label = data[470:569,1]

#model implementation
library(class)
y_pred = knn(train = data_train,
             test = data_test,
             cl=data_train_label,
             k=21)
install.packages("gmodels")
library(gmodels)
CrossTable(x=data_test_label,
           y=y_pred,
           prop.chisq = FALSE)




# output is 
   Cell Contents
|-------------------------|
|                       N |
|           N / Row Total |
|           N / Col Total |
|         N / Table Total |
|-------------------------|

 
Total Observations in Table:  100 

 
                | y_pred 
data_test_label |    Benign | Malignent | Row Total | 
----------------|-----------|-----------|-----------|
         Benign |        77 |         0 |        77 | 
                |     1.000 |     0.000 |     0.770 | 
                |     0.975 |     0.000 |           | 
                |     0.770 |     0.000 |           | 
----------------|-----------|-----------|-----------|
      Malignent |         2 |        21 |        23 | 
                |     0.087 |     0.913 |     0.230 | 
                |     0.025 |     1.000 |           | 
                |     0.020 |     0.210 |           | 
----------------|-----------|-----------|-----------|
   Column Total |        79 |        21 |       100 | 
                |     0.790 |     0.210 |           | 
----------------|-----------|-----------|-----------|
