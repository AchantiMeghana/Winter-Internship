
library(shiny)
library(vroom)
library(rmarkdown)
library(shinyWidgets)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  
  #Title
  titlePanel(strong("Linear Regression")),
  
  #Background colour
  setBackgroundColor(color =c( "#ffd700","#fff2cc"),gradient = "radial"),
  
  fluidRow(
    column(3,
           
           #Upload dataset
           fileInput("upload","Upload your dataset",multiple= FALSE,
                     accept = c(".csv",".tsv",".txt")),
           
           # Select independent variable x           
          uiOutput("x_pick"),
           br(),
           # Select independent variable y          
           uiOutput("y_pick"),
           br(),
           #Choose no. of rows of table to be displayed
           sliderInput("n","Choose the number of rows of data to be displayed",
                       min= 0, max =0 ,step = 1,value =0)
    ),
    
    column(9,
           #Plot output
           plotOutput("multiple_regression_plot")
    )),
  
  fluidRow(
    column(2,
           # Download reports        
           downloadButton("download","Get your report!", icon = shiny::icon("download")) 
    )),
  
  #display columns of dataset
  fluidRow(
    column(9,
           tableOutput("table")   
    )))  

# Define server logic required to draw a histogram
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
  
  #Update choices in widgets for x,y
  output$x_pick <- renderUI({
    req(data())
    xcol <- colnames(data())
    pickerInput(inputId = "x",label = "Choose x",
                choices = c(xcol[1:length(xcol)]),selected =xcol[1],
                multiple = TRUE,options = list(`actions-box`= TRUE))
    
  })
  
  output$y_pick <- renderUI({
    req(data())
    ycol <- colnames(data())
    pickerInput(inputId = "y",label = "Choose y",
                choices = c(ycol[1:length(ycol)]),selected =ycol[1],
                multiple = FALSE)
    
  })

    # #Columns of x,y widgets
  # ycol <- reactive({
  #   req(data())
  #   colnames(data())
  # })
  # 
  # xcol <- reactive({
  #   req(data())
  #   colnames(data())
  # })

  # #Multiple linear regression model
  # lmmodel <- reactive({
  #   req(data(),input$x,input$y)
  #   x <- as.numeric(data()[[as.name(input$x)]])
  #   y <- as.numeric(data()[[as.name(input$y)]])
  #   current_formula <- paste0(input$y, " ~ ", paste0(input$x, collapse = " + "))
  #   current_formula <- as.formula(current_formula)
  #   model <- lm(current_formula, data = data(), na.action=na.exclude)
  #   return(model)
  # })
  # 
  # 
  # #Plotting multiple regression
  #   output$multiple_regression_plot <- renderPlot({
  #     req(lmmodel())
  #     plot(lmmodel())
  #     })
  #   
  # # Store summary of model
  #   result <- reactive({
  #     
  #     req(lmmodel())
  #     summary(lmmodel())
  #   })
  
  # y <- reactive(as.numeric(unlist(data()[,input$y])))
  # x_chosen <- reactive(as.numeric(unlist(data()[,input$x])))
  # x_in_formula <- reactive(paste(x_chosen(),collapse = "+"))
  
  formula_lm <- reactive({
   paste(input$y, " ~ ", paste0(input$x, collapse = " + "))
    })
  
  model <- reactive({
    req(input$upload)
    lm(as.formula(formula_lm()),data=data(),na.action = na.exclude)
  })
  
  # output$multiple_regression_plot<- reactive({
  # 
  # })
  
  data_use <- reactive({
    req(input$y,input$x,data())
    res <- data()[,input$y]
    for(i in 1: length(input$x)){
      res <- cbind(res,data()[,input$x[i]])
    }
    colnames(res) <- c(input$y,input$x)
    return(res)
  })
  col_res_x <- reactive({
    colnames(data_use())
  })
  
  colors_chosen <- reactive(colorRampPalette(c("#7c2828","#ffd700")))
  
  output$multiple_regression_plot <- renderPlot({
    req(is.matrix(data_use()))
    heatmap(x= data_use(),
            col = colors_chosen(100),
            main = "Heat map",
            margins = c(5,10),
            xlab = "Independent Variables",
            ylab = "Dependent Variable",
            scale = "column"
            )
    })
  
  
  
   #Downloading the report
  output$download <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      
      #Using a temporary directory
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Setting up parameters to pass to Rmd document
      params <- model()
      
      #Knitting the document
      rmarkdown::render(tempReport, output_file = file,
                        params = params)}
  )

  
}

# Run the application 
shinyApp(ui = ui, server = server)
