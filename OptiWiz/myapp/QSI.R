source("GaussJordan.R")

#@author: Daniel Pete M. Aguilar
#Student Number: 2018-67671
#Section: AB-5L
#Date: December 9, 2019
#Project: Quadratic Spline Interpolation


QSI = function(xPos, yPos, evaluateAt) {
  dataPoints = length(xPos)
  numOfIntervals = dataPoints - 1
  numOfUnknowns = 3 * numOfIntervals
  quadMatrix = matrix(0.0, numOfUnknowns - 1, numOfUnknowns)
 
  
  if (is.na(evaluateAt)) return (NA)
  
  # Condition 1
  rowCounter = 1
  for (i in 2:numOfIntervals) {
    
    colCounter = (3 * (i - 2))
    quadMatrix[rowCounter, colCounter] = xPos[i]^2 
    quadMatrix[rowCounter, colCounter + 1] = xPos[i]
    quadMatrix[rowCounter, colCounter + 2] = 1
    quadMatrix[rowCounter, numOfUnknowns] = yPos[i]
    
    rowCounter = rowCounter + 1
    
    colCounter = (3 * (i - 1))
    quadMatrix[rowCounter, colCounter] = xPos[i]^2 
    quadMatrix[rowCounter, colCounter + 1] = xPos[i]
    quadMatrix[rowCounter, colCounter + 2] = 1
    quadMatrix[rowCounter, numOfUnknowns] = yPos[i]
    
    rowCounter = rowCounter + 1
  }

  # Condition 2
  quadMatrix[rowCounter, 1] = xPos[1]
  quadMatrix[rowCounter, 2] = 1
  quadMatrix[rowCounter, numOfUnknowns] = yPos[1]
  
  rowCounter = rowCounter + 1
 
  quadMatrix[rowCounter, numOfUnknowns - 3] = xPos[dataPoints]^2
  quadMatrix[rowCounter, numOfUnknowns - 2] = xPos[dataPoints]
  quadMatrix[rowCounter, numOfUnknowns - 1] = 1
  quadMatrix[rowCounter, numOfUnknowns] = yPos[dataPoints]

  rowCounter = rowCounter + 1
  
  # Condition 3
  colCounter = 0
  for (j in 2:numOfIntervals) {
    
    quadMatrix[rowCounter, colCounter] = xPos[j] * 2
    quadMatrix[rowCounter, colCounter + 1] = 1
    quadMatrix[rowCounter, colCounter + 3] = -(xPos[j] * 2)
    quadMatrix[rowCounter, colCounter + 4] = -1

    rowCounter = rowCounter + 1
    colCounter = (3 * (j - 1))
  }
  
  # computes for the solution set using Gauss Jordan
  gaussJordSolSet = GaussJordanElimination(quadMatrix)
  quadSolutionSet = gaussJordSolSet$solutionSet
  fxnStorage = c()
  
  countA = 0
  countB = 1
  countC = 2
  intervalToEval = 0
  evalChangeCounter = 0
  
  # string manipulation for the function
  for (k in 1:numOfIntervals) {
    quadString = "function(x) "
    
    if (k == 1) {
      quadString = paste(quadString, 0, "* x^2 +", quadSolutionSet[countB], "* x +",quadSolutionSet[countC],  sep = " ")
    } else {
      quadString = paste(quadString, quadSolutionSet[countA], "* x^2 +", quadSolutionSet[countB], "* x +",quadSolutionSet[countC], sep = " ")
    }
    
    if (evaluateAt >= xPos[k] && evaluateAt <= xPos[k + 1]) {
      intervalToEval = k
      evalChangeCounter = evalChangeCounter + 1
    }
    
    fxnStorage[k] = quadString
    countA = countA + 3
    countB = countB + 3
    countC = countC + 3
  }
  
  if (evalChangeCounter == 0) return (NA)
  
  resultMatrix = matrix(dimnames = list(NULL ,c("Interval", "Function")), nrow = length(fxnStorage), ncol = 2, byrow = FALSE)
  resultMatrix[,2] = fxnStorage
  resultMatrix[,1] = c(1:length(fxnStorage))
  
  outPut = eval(parse(text = fxnStorage[intervalToEval]))
  
  quasiFinal = list(quasiMatrix = resultMatrix, result = outPut(evaluateAt))
  return(quasiFinal)
  
} 





# a = QSI(dfSample$x, dfSample$y, 16)
