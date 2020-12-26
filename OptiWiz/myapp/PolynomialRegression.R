source("GaussJordan.R")

#@author: Daniel Pete M. Aguilar
#Student Number: 2018-67671
#Section: AB-5L
#Date: December 9, 2019
#Project: Polynomial Regression


PolynomialRegression = function(dependent, independent, degree) {
  
  if (is.na(degree)) return (NA)
  
  polyRow = degree + 1
  polyCol = degree + 2
  sumValues = c()
  RHSValues = c()
  polyMatrix = matrix(nrow = polyRow, ncol = polyCol, TRUE)

  # check if degree is less than 0 or greater than the size of the variable in the data frame
  if (degree < 1 || degree > (length(dependent) - 1)) {
    return(NA)
  }
  
  # loop for computing the summation of the dependent variable
  for (i in 1:((degree * 2) + 1)) {
    sumValues[i] = sum((dependent)^(i - 1))
  }
  print(sumValues)
  
  # loop for computing the right hand side of the matrix
  for (j in 1:polyRow) {
    RHSValues[j] = sum(((dependent)^(j - 1)) * independent)
  }
  
  # loop for putting the computed values into the matrix
  for (k in 1:polyRow) {
    for (s in 1:polyCol) {
      if (s == polyCol) {
        polyMatrix[k,s] = RHSValues[k]
      } else {
        sumIndex = (s - 1) + k
        polyMatrix[k, s] = sumValues[sumIndex]
        
      }
    }
  }
  
  # returns the solution set from Gauss-Jordan Elimination
  GaussJordSolSet = GaussJordanElimination(polyMatrix)
  
  
  polyString = "function(x) "
  
  # loop that forms the solution set into a polynomial string
  for (v in degree:1) {
    
    polyString = paste(polyString, GaussJordSolSet$solutionSet[v+1], " * (x^", v, ") + ", sep = "")
  }
  
  polyString = paste(polyString, GaussJordSolSet$solutionSet[1], sep = "")
  functionMatrix = matrix(dimnames = list(NULL, c("Function")), nrow = 1, ncol = 1, byrow = FALSE)
  functionMatrix[1,1] = polyString
  

  polyFinal = list("function" = functionMatrix, "Estimated Value" = eval(parse(text = polyString)))
  return(polyFinal)
  
}




