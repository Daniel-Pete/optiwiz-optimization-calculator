#@author: Daniel Pete M. Aguilar
#Student Number: 2018-67671
#Section: AB-5L
#Date: December 9, 2019
#Project: Simplex Method


SimplexMethod = function (matrix) {
  simplexRow = nrow(matrix)
  simplexCol = ncol(matrix)
  simplexMatrix = matrix

  tableau = list()
  basicSolList = list()
  
  # storage for the matrices per iteration
  basicSolList[[1]] = BasicSolution(simplexMatrix)
  tableau[[1]] = simplexMatrix
  matrixCounter = 2
  
  # this while loop will go through until the RHS and the final row has no negative number
  while(min(simplexMatrix[1:(simplexRow - 1),simplexCol]) < 0 || min(simplexMatrix[simplexRow, (1:simplexCol-1)]) < 0) {

    # condition for the RHS
    if (min(simplexMatrix[1:(simplexRow - 1),simplexCol]) < 0) {
      
      negativePivotRow = findNegPivotRow(simplexMatrix, simplexCol)
      
      negativePivotCol = findPivotCol(simplexMatrix, negativePivotRow)
      
      # if the row has no negative number, there is no feasible solution for the problem
      if (min(simplexMatrix[negativePivotRow, 1:(simplexCol - 1)]) >= 0) return(NA)
      
      # normalization of the row
      simplexMatrix[negativePivotRow,] = simplexMatrix[negativePivotRow,] / simplexMatrix[negativePivotRow, negativePivotCol]
      
      # apply the normalized row to the other rows
      for (j in 1:simplexRow) {
        if (j == negativePivotRow) next
        normalizedRow = simplexMatrix[j,negativePivotCol] * simplexMatrix[negativePivotRow,]
        simplexMatrix[j,] = simplexMatrix[j,] - normalizedRow
      }
      
      basicSolList[[matrixCounter]] = BasicSolution(simplexMatrix)
      tableau[[matrixCounter]] = simplexMatrix
      matrixCounter = matrixCounter + 1
      
    } else { # condition for the final row
    
      pivotColumnIndex = findPivotCol(simplexMatrix, simplexRow)
      
      pivotRowIndex = findTestRatio(simplexMatrix, pivotColumnIndex)
      print(pivotColumnIndex)
      print(pivotRowIndex)
      
      # if the row has no negative number, there is no feasible solution for the problem
      if (min(simplexMatrix[1:(simplexRow - 1), pivotColumnIndex]) >= 0) return(NA)
      
      # normalization of the row
      simplexMatrix[pivotRowIndex,] = simplexMatrix[pivotRowIndex,] / simplexMatrix[pivotRowIndex, pivotColumnIndex]
      
      # apply the normalized row to the other rows
      for (j in 1:simplexRow) {
        if (j == pivotRowIndex) next
        normalizedRow = simplexMatrix[j,pivotColumnIndex] * simplexMatrix[pivotRowIndex,]
        simplexMatrix[j,] = simplexMatrix[j,] - normalizedRow
        
      }
      
      basicSolList[[matrixCounter]] = BasicSolution(simplexMatrix)
      tableau[[matrixCounter]] = simplexMatrix
      matrixCounter = matrixCounter + 1
    }
  }

  totalShippingCost = c(basicSolList[[matrixCounter - 1]][1:15,2])
  totalCost = sum(as.numeric(totalShippingCost))
  
  simplexFinal = list(listOfTableau = tableau, solutionSet = basicSolList, simplexResult = basicSolList[[matrixCounter - 1]][(simplexCol - 1),2], numOfShipping = totalCost)
  return(simplexFinal)
}

# determines the test ratio in the particular iteration
findTestRatio = function(matrix, simplexCol) {
  RHS = length(matrix[1,])
  
  minVal = Inf
  minIndex = 1
  
  ratioCounter = 1
  for (i in 1:(length(matrix[, simplexCol]) - 1)) {
    
    if (matrix[i, simplexCol] <= 0) {
      next
    }
    
    if ((matrix[i, RHS] / matrix[i, simplexCol]) < minVal) {
      minVal = matrix[i, RHS] / matrix[i, simplexCol]
      minIndex = i
    }
    
  }
  
  return(minIndex)
}

# determines the pivot row in the particular iteration
findNegPivotRow = function(matrix, simplexCol) {
  
  min = matrix[1,(simplexCol)]
  minRow = 1
  
  
  for(i in 2:(length(matrix[,simplexCol]) - 1)) {
    
    if(matrix[i,simplexCol] < min) {
      min = matrix[i,simplexCol]
      minRow = i
    }
  }
  return(minRow)  
}

# determines the pivot column in the particular iteration
findPivotCol = function(matrix, simplexRow) {
  
  min = matrix[simplexRow, 1]
  minCol = 1
  
  for(j in 2:(length(matrix[simplexRow,]) - 1)) {
    
    if(matrix[simplexRow,j] < min) {
      
      min = matrix[simplexRow, j]
      minCol = j
      
    }
  }
  return(minCol)  
}

# creates the matrix for the input of the user
createMatrix = function(inputDataFrame) {
  
  c1 = c(-1, 0, 0, 0, 0, -1, 0,	0, 0,	0, -1, 0, 0, 0, 0,	1, 0,	0, 0,	0, 0,	0, 0,	0, -(inputDataFrame$Sacramento[4]));
  c2 = c(0, -1, 0, 0, 0, 0, -1, 0, 0,	0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, -(inputDataFrame$SaltLake[4]));
  c3 = c(0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, -(inputDataFrame$Chicago[4]));
  c4 = c(0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, -(inputDataFrame$Albuquerque[4]));
  c5 = c(0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 0, 0, -(inputDataFrame$NewYork[4]));
  c6 = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, inputDataFrame$Supply[1]);
  c7 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, inputDataFrame$Supply[2]);
  c8 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, inputDataFrame$Supply[3]);
  z = c(inputDataFrame$Sacramento[1], inputDataFrame$SaltLake[1], inputDataFrame$Chicago[1], inputDataFrame$Albuquerque[1], inputDataFrame$NewYork[1], inputDataFrame$Sacramento[2], inputDataFrame$SaltLake[2], inputDataFrame$Chicago[2], inputDataFrame$Albuquerque[2], inputDataFrame$NewYork[2], inputDataFrame$Sacramento[3], inputDataFrame$SaltLake[3], inputDataFrame$Chicago[3], inputDataFrame$Albuquerque[3], inputDataFrame$NewYork[3], 0, 0, 0, 0, 0, 0, 0, 0, 1, 0);
  
  system = list(c1, c2, c3, c4, c5, c6, c7, c8, z);
  
  simplexRow = length(system)
  simplexCol = length(system[[1]])
  
  simplexMatrix = matrix(0.0, simplexRow, simplexCol, TRUE)
  
  for (i in 1:simplexRow) {
    simplexMatrix[i, ] = system[[i]] 
  }
  
  return(simplexMatrix)
}

# creates a matrix that contains the basic solutions
BasicSolution = function(simplexMatrix) {
  
  resultMatrix = matrix(0.0, dimnames = list(NULL, c("Destination", "Shipping Costs")), nrow = (ncol(simplexMatrix)) - 1, ncol = 2, byrow = FALSE)
  resultMatrix[1,1] = "Denver to Sacramento"
  resultMatrix[2,1] = "Denver to Salt Lake"
  resultMatrix[3,1] = "Denver to Chicago"
  resultMatrix[4,1] = "Denver to Albuquerque"
  resultMatrix[5,1] = "Denver to New York"
  resultMatrix[6,1] = "Phoenix to Sacramento"
  resultMatrix[7,1] = "Phoenix to Salt Lake"
  resultMatrix[8,1] = "Phoenix to Chicago"
  resultMatrix[9,1] = "Phoenix to Albuquerque"
  resultMatrix[10,1] = "Phoenix to New York"
  resultMatrix[11,1] = "Dallas to Sacramento"
  resultMatrix[12,1] = "Dallas to Salt Lake"
  resultMatrix[13,1] = "Dallas to Chicago"
  resultMatrix[14,1] = "Dallas to Albuquerque"
  resultMatrix[15,1] = "Dallas to New York"
  resultMatrix[16,1] = "s1"
  resultMatrix[17,1] = "s2"
  resultMatrix[18,1] = "s3"
  resultMatrix[19,1] = "s4"
  resultMatrix[20,1] = "s5"
  resultMatrix[21,1] = "s6"
  resultMatrix[22,1] = "s7"
  resultMatrix[23,1] = "s8"
  resultMatrix[24,1] = "Minimum Cost"
  
  solutionCounter = 0
  otherCounter = 0
  rowCount = 0
  
  for (i in 1:(ncol(simplexMatrix) - 1)) {
    for (j in 1:nrow(simplexMatrix)) {
      if (simplexMatrix[j, i] == 0) {
        next
      } else if (simplexMatrix[j, i] == 1) {
        solutionCounter = solutionCounter + 1
      } else {
        otherCounter = otherCounter + 1
      }
      rowCount = j
    }
    
    if (solutionCounter == 1 && otherCounter == 0) {
      if (i == (ncol(simplexMatrix)) - 1) {
        resultMatrix[i, 2] = simplexMatrix[rowCount, ncol(simplexMatrix)] * -1
      } else {
        resultMatrix[i, 2] = simplexMatrix[rowCount, ncol(simplexMatrix)]
      }  
    } else (
      resultMatrix[i, 2] = 0
    )
    solutionCounter = 0
    otherCounter = 0
  }
  
  return (resultMatrix)
}
