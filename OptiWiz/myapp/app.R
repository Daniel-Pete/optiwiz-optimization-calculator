
source("PolynomialRegression.R")
source("QSI.R")
source("simplex.R")
library(shiny)
library(shinydashboard)
library(rhandsontable)
library(shinyWidgets)

# Define UI ----
ui <- dashboardPage(
  dashboardHeader(title = "OptiWiz"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("bolt")),
      menuItem("Polynomial Regression", tabName = "Regression", icon = icon("cube")),
      menuItem("Quadratic Spline Interpolation", tabName = "Interpolation", icon = icon("dice-four")),
      menuItem("Simplex", tabName = "SimplexMethod", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tab for the Home Page
      tabItem(tabName = "Home",
          fluidPage(
            titlePanel(
              h1("Welcome to OptiWiz", align = "center"),
            ), 
            h3("Your Generic Solver and Optimization Calculator", align = "center"), 
            wellPanel(
              img(src = "calcu.jpg", align = "center", width = 800, height = 400)
            )  
          )
      ),
      
      # Tab for the Polynomial Regression
      tabItem(tabName = "Regression",
        fluidRow(
          titlePanel("POLYNOMIAL REGRESSION"),
          box(
              # Input: Select a file
              fileInput("file1", "Choose CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
          ),  
          box(
            numericInput("degreeInput", 
                         h3("Enter Degree"), 
                         value = 1),
            numericInput("valueInput", 
                         h3("Enter the value to be estimated"), 
                         value = 1),
            actionButton("polyInput", "Enter")
          ),
          infoBox(
            title = h3("Function"),
            width = 6
          ),
          infoBox(
            icon = icon("edge"),
            title = h3("Estimated Value"),
            width = 6
          ),
          box(
            wellPanel(
              tableOutput("REGcontents")
            )
          ),
          box(
            verbatimTextOutput("DEGcontents")
          )
        )
      ), 
      
      # Tab for the Quadratic Spline Interpolation
      tabItem(tabName = "Interpolation",
        fluidRow(
          titlePanel("QUADRATIC SPLINE INTERPOLATION"),
          box(
            fileInput("file2", "Choose CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
          ),
          box(
            numericInput("evaluateInput", 
                         h3("Enter the value to be estimated"), 
                         value = 1),
            actionButton("quasiInput", "Enter")
          ),
          infoBox(
            title = h3("Function"),
            width = 6
          ),
          infoBox(
            icon = icon("edge"),
            title = h3("Estimated Value"),
            width = 6
          ),
          box(
            wellPanel(
              tableOutput("FunctionContents")
            )  
          ),
          box(
            tableOutput("EvalContents")
          )
        )
      ),
      # Tab for the Simplex Method
      tabItem(tabName = "SimplexMethod",
        fluidRow(
          titlePanel("OPTIMIZATION"),
          box(
            width = 15,
            h4("Data Input"),
            rHandsontableOutput("simplexInputTable"),
            br(),
            actionButton("simplexValuesUpdate", "Enter")
          ),
          valueBox(
            width = 6,
            span(textOutput("SimplexAnswer"), style = "font-size: 30px"), 
            subtitle = ("Minimum Shipping Cost")
          ),
          valueBox(
            width = 6,
            span(textOutput("totalNumOfShipping"), style = "font-size: 30px"),
            subtitle = ("Total Number of Shipping")
          ),
          box(
            width = 11,
            collapsible = TRUE,
            h4("Initial Tableau"),
            wellPanel(
              rHandsontableOutput("simplexMatrixContent")
            )
          ),
          box(
            width = 11,
            collapsible = TRUE,
            h4("Tableau per Iteration"),
            wellPanel(
              rHandsontableOutput("matrixPerIterationContent")
            )
          ),
          box(
            collapsible = TRUE,
            numericInput("tableauInput",
                          h5("Enter iteration for the Tableau"),
                          value = 1),
            span(textOutput("matrixInputLength"), style = "font-size: 20px"),
            actionButton("simplexInputButton", "Enter")
          ),
          box(
            collapsible = TRUE,
            numericInput("solutionInput",
                          h5("Enter iteration for the Basic Solution"),
                          value = 1),
            span(textOutput("solutionInputLength"), style = "font-size: 20px"),
            actionButton("solutionInputButton", "Enter")
          ),
          box(
            collapsible = TRUE,
            h4("Initial Basic Solution"),
            wellPanel(
              rHandsontableOutput("simplexBasicSolContent")
            )
          ),
          box(
            collapsible = TRUE,
            h4("Basic Solution per Iteration"),
            wellPanel(
              rHandsontableOutput("basicSolPerIteration")
            )
          )

   
        )
      )
    )
  )
)
#############FOR SIMPLEX#############
#Initial values for the table
Supply = c(310,260,280, 0)
Sacramento = c(10,6,3, 180)
SaltLake = c(8,5,4, 80)
Chicago = c(6,4,5, 200)
Albuquerque = c(5,3,5, 160)
NewYork = c(4,6,9, 220)

initialMatValues = data.frame(Supply,Sacramento, SaltLake, Chicago, Albuquerque, NewYork, row.names = c("Denver", "Phoenix", "Dallas", "Demands"))
#####################################

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  # determines the estimated value depending on the numeric input of the user
  polyValue = eventReactive(input$polyInput,{
    if(is.null(input$file1)) return (NULL)
    
    req(input$file1)
    
    df = read.csv(input$file1$datapath, header = FALSE, sep = ",")
    
    degree = input$degreeInput
    evalAt = input$valueInput
    
    if(is.na(evalAt)) return (NULL)
    
    polyResult = PolynomialRegression(df$V1, df$V2, degree)
    if (is.na(polyResult)) return (NULL)
    
    polyAnswer = polyResult$"Estimated Value"(evalAt)
    return(polyAnswer)
    
  })
  
  # computes for the function given the degree
  polyMatrix = eventReactive(input$polyInput,{
    if(is.null(input$file1)) return (NULL)

    req(input$file1)

    df = read.csv(input$file1$datapath, header = FALSE, sep = ",")

    degree = input$degreeInput
    polyResult = PolynomialRegression(df$V1, df$V2, degree)
    if (is.na(polyResult)) return (NULL)
    return(polyResult$"function")

  })
  
  # displays the function 
  output$REGcontents = renderTable({
    polyMatrix()
  })
  # displays the estimated value
  output$DEGcontents <- renderText({
    polyValue()
  })
  
  # determines the function per interval 
  quasiMatrix = eventReactive(input$quasiInput,{
    if(is.null(input$file2)) return(NULL)
    
    req(input$file2)
    
    df = read.csv(input$file2$datapath, header = FALSE, sep = ",")
    
    quadSpline = QSI(df$V1, df$V2, df$V1[1])
    if (is.na(quadSpline)) return (NULL)
    if (is.null(quadSpline)) return (NULL)
    return(quadSpline$quasiMatrix)
    
  })
  
  # determines the estimated value depending on the numeric input of the user
  quasiValue = eventReactive(input$quasiInput,{
    if(is.null(input$file2)) return(NULL)
    
    req(input$file2)
    
    df = read.csv(input$file2$datapath, header = FALSE, sep = ",")
    
    evalAt = input$evaluateInput
    
    quadSpline = QSI(df$V1, df$V2, evalAt)
    
    if (is.na(quadSpline)) return (NULL)
    if (is.null(quadSpline)) return (NULL)
    else {
      quasiAnswer = quadSpline$result
      estimateMatrix = matrix(dimnames = list(NULL, c("Estimated Value")), nrow = 1, ncol = 1, byrow = FALSE)
      estimateMatrix[1,1] = quasiAnswer
      return(estimateMatrix)
    }  
    
  })
  
  # displays the function per interval
  output$FunctionContents = renderTable({
    quasiMatrix()
  })
  
  # displays the estimated value 
  output$EvalContents = renderTable({
    quasiValue()
  })
  
  # displays the input table
  output$simplexInputTable = renderRHandsontable({
    rhandsontable(initialMatValues, width = 1000) %>% hot_cols(colWidths = 100) %>% hot_rows(rowHeights = 40)
  })
  

  # determines the output of the input table (only if the enter button was clicked)
  observeEvent(input$simplexValuesUpdate,{
    inputDataFrame = hot_to_r(input$simplexInputTable)
    
    outPutMatrix = createMatrix(inputDataFrame)
    simplexOutput = SimplexMethod(outPutMatrix)
    chosenIteration = input$tableauInput
    
    # returns the matrix depending on the chosen iteration of the user
    iterationVal = eventReactive(input$simplexInputButton,{
     
      chosenIteration = input$tableauInput
      if(chosenIteration > length(simplexOutput$listOfTableau) || chosenIteration < 1) return (NULL)
      
      return(simplexOutput$listOfTableau[[chosenIteration]])
    })
    
    # returns the matrix of the solution depending on the chosen iteration of the user
    solIterationVal = eventReactive(input$solutionInputButton, {
      
      solutionIteration = input$solutionInput
      if(solutionIteration > length(simplexOutput$solutionSet) || chosenIteration < 1) return (NULL)
      
      return(simplexOutput$solutionSet[[solutionIteration]])
    })
    
    
    # displays the initial tableau
    output$simplexMatrixContent = renderRHandsontable({
      if (is.na(simplexOutput)) return (NULL)
      simplexFrame = data.frame(simplexOutput$listOfTableau[[1]])
      
      rhandsontable(simplexFrame, width = 500, readOnly = TRUE) %>% hot_cols(colWidths = 50) %>% hot_rows(rowHeights = 20)
    })
    
    # displays the initial basic solution
    output$simplexBasicSolContent = renderRHandsontable({
      if (is.na(simplexOutput)) return (NULL)
      simplexFrame = data.frame(simplexOutput$solutionSet[[1]])
      rhandsontable(simplexFrame, width = 500, readOnly = TRUE) %>% hot_cols(colWidths = 100) %>% hot_rows(rowHeights = 20)
    })

    # displays the chosen matrix iteration
    output$matrixPerIterationContent = renderRHandsontable({
      
      mat = iterationVal()
      simpFrame = data.frame(mat)
      rhandsontable(simpFrame, width = 500, readOnly = TRUE) %>% hot_cols(colWidths = 50) %>% hot_rows(rowHeights = 20)
    })
    
    # displays the chosen basic solution iteration
    output$basicSolPerIteration = renderRHandsontable({
      
      solMat = solIterationVal()
      solutionFrame = data.frame(solMat)
      rhandsontable(solutionFrame, width = 500, readOnly = TRUE) %>% hot_cols(colWidths = 100) %>% hot_rows(rowHeights = 20)
    })
    
    # displays the minimum cost of the problem
    output$SimplexAnswer = renderText({
      
      if (is.na(simplexOutput)) return ("No Feasible Solution")
      else return(simplexOutput$simplexResult)
    })
    
    # displays the total shipping of the problem
    output$totalNumOfShipping = renderText({
      if (is.na(simplexOutput)) return ("No Feasible Solution")
      else return(simplexOutput$numOfShipping)
    })
    
    output$matrixInputLength = renderText ({
      matResult = paste(input$tableauInput, "of", length(simplexOutput$listOfTableau), sep = " ")
      return(matResult)
    })
    
    output$solutionInputLength = renderText ({
      solResult = paste(input$solutionInput, "of", length(simplexOutput$solutionSet), sep = " ")
      return(solResult)
    })

  })  
  
}

# Create Shiny app ----
shinyApp(ui, server)