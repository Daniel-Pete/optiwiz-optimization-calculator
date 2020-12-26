
GaussJordanElimination = function(matrixG) {
  
  gaussMatrix = matrixG
  eqLength = nrow(matrixG)
  solutionSet = c()
  
  for (i in 1:(eqLength)) {
    if (i != eqLength) {
      
      pivotRow = findPivotRow(gaussMatrix, i)
      
      # checks if the solution does not exist
      if (gaussMatrix[pivotRow, i] == 0) { 
        return (NA)
      }
      #swapping for the rows
      temp = gaussMatrix[i,]
      gaussMatrix[i,] = gaussMatrix[pivotRow,]
      gaussMatrix[pivotRow,] = temp
      
      
    }
    # normalization of the pivot row
    gaussMatrix[i,] = gaussMatrix[i,] / gaussMatrix[i,i]
    
    for (j in 1:(eqLength)) {
      if (i == j) {
        next
      }
      # converts the matrix into an identity matrix
      normalizedRow = gaussMatrix[j,i] * gaussMatrix[i,]
      gaussMatrix[j,] = gaussMatrix[j,] - normalizedRow
      
    }
  }
  GaussElim = list(solutionSet = gaussMatrix[1:nrow(matrixG),ncol(matrixG)])
  return (GaussElim)
}


findPivotRow = function(matrix, simplexCol) {
  
  max = abs(matrix[simplexCol,simplexCol])
  maxRow = simplexCol
  
  for(i in simplexCol:length(matrix[,1])) {
    
    if(abs(matrix[i,simplexCol]) > max) {
      max = abs(matrix[i, simplexCol])
      maxRow = i
    }
  }
  return(maxRow)  
}

