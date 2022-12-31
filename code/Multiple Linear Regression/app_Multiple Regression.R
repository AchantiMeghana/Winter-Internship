
library(shiny)
library(vroom)
library(rmarkdown)
library(shinyWidgets)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  
  #Display title
  titlePanel(strong("Linear Regression")),
  
  #Set Background colour
  setBackgroundColor(color =c( "#ffd700","#fff2cc"),gradient = "radial"),
  
  fluidRow(
    column(3,
          
           #Upload dataset containing X,Y data
           fileInput("upload","Upload your dataset",multiple= FALSE,
                     accept = c(".csv",".tsv",".txt")),
           
           # COnditional UI           
          uiOutput("x_pick"),
           br(),         
           uiOutput("y_pick"),
           br(),
           
           #SLider Widget to choose no. of rows of table to be displayed
           sliderInput("n","Choose the number of rows of data to be displayed",
                       min= 0, max =0 ,step = 1,value =0)
    ),
    
    column(9,
           #Plot multiple regression 
           plotOutput("multiple_regression_plot")
    )),
  
  fluidRow(
    column(2,
           # Generate download button        
           downloadButton("download","Get your report!", icon = shiny::icon("download")) 
    )),
  
  #Display dataset table
  fluidRow(
    column(9,
           tableOutput("table")   
    )))  



# Define server logic 
server <- function(input, output,session) {
  
  #storing dataset in reactive variable
  data <- reactive({
    req(input$upload,cancelOutput = TRUE) #program doesn't run if condition is false
    ext <- tools::file_ext(input$upload$name)
    switch (ext,
            tsv = vroom::vroom(input$upload$datapath,delim ="\t"),
            csv  = vroom::vroom(input$upload$datapath,delim =","),
            txt = vroom::vroom(input$upload$datapath,delim =" "),
            validate("Upload a .csv /.tsv /.txt file"))}) 
  
  #Update slider range
  observeEvent(data(),{
    updateSliderInput(inputId = "n",max = nrow(data()))
  }) 
  
  #Displaying dataset    
  observeEvent(data(),{
    output$table <- renderTable(head(data(),input$n))
  })
  
  #Update choices in widgets for x
  output$x_pick <- renderUI({
    req(data())
    xcol <- colnames(data())
    pickerInput(inputId = "x",label = "Choose x",
                choices = c(xcol[1:length(xcol)]),selected =xcol[1],
                multiple = TRUE,options = list(`actions-box`= TRUE))
    
  })
  
  #Update choices in widgets for x
  output$y_pick <- renderUI({
    req(data())
    ycol <- colnames(data())
    pickerInput(inputId = "y",label = "Choose y",
                choices = c(ycol[1:length(ycol)]),selected =ycol[1],
                multiple = FALSE)
    
  })

 
  # Linear regression formula
  formula_lm <- reactive({
   paste(input$y, " ~ ", paste0(input$x, collapse = " + "))
    })
  
  #Storing regression in a model
  model <- reactive({
    req(input$upload)
    lm(as.formula(formula_lm()),data=data(),na.action = na.exclude)
  })

  #Plot predicted values of Y vs residuals
  output$multiple_regression_plot <- renderPlot({
   plot(fitted(model()),resid(model()),xlab="Fitted values",ylab="Residuals")
    })
  
    #Downloading the report
  output$download <- downloadHandler(
    filename = "report.html",
    content = function(file) { 
      
      #Using a temporary directory
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report_Multiple_Regression.Rmd", tempReport, overwrite = TRUE)
      
      # Setting up parameters to pass to Rmd document
      params <- model()
      
      #Knitting the document
      rmarkdown::render(tempReport, output_file = file,
                        params = params)}
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
