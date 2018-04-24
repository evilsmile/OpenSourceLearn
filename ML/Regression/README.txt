Summary
   Regression is the process of predicting a target value similar to classification. The difference 
between regression and classification is that the variable forecasted in regression is continuous,
whereas it's discrete in classification. Regression is one of the most useful tools in statistics.
Minimizing the sum-of-squares error is used to find the best weights for the input features in a 
regression equation. Regression can be done on any set of data provided that for an input matrix X,
you can compute the inverse of xTx. Just because you can compute a regression equation for a set of 
data doesn't mean that the results are very good. One test of how "good" or significant the results
are is the correlation between the predicted values yHat and the original data y.
   When you have more features than data points, you can't compute the inverse of xTx. If you have 
more data points that features, you still may not be able to compute xTx if the feature are highly 
correlated. Ridge regression is a regression method that allows you to compute regression coefficients
despite being unable to compute the inverse of xTx.
   Ridge regression is an example of a shrinkage method. Shrinkage methods impose a constraint on the 
size of the regression coefficients. Another shrinkage method that's powerful is tha lasso. The lasso
is difficult to compute, but stagewise linear regression is easy to compute and gives results close 
to those of the lasso.
   Shrinkage methods can also be viewed as adding bias to a model and reducing the variance. The 
bias/variance tradeoff is a powerful concept in understanding how altering a model impacts the success
of a model.
