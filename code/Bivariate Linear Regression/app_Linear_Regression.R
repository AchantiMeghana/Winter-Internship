library(shiny)
library(vroom)
library(readr)
library(rmarkdown)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  
#Title
  titlePanel(strong("Linear Regression")),
  
  fluidRow(
    column(3,

#Upload dataset
           fileInput("upload","Upload your dataset",multiple= FALSE,
                     accept = c(".csv",".tsv")),
                      
# Widget to select independent variable x           
           selectInput("x","Choose the independent variable",
                       choices = NULL),

# Widget to choose independent variable y          
           selectInput("y","Choose the dependent variable",
                       choices = NULL),
           
#Choose no. of rows of dataset to be displayed
           sliderInput("n","Choose the number of rows of data to be displayed",
                       min= 0, max =0 ,step = 1,value =0)
    ),

     column(9,
#Plot output
           plotOutput("regression_plot")
           )),

  fluidRow(
    column(2,
  # Download reports        
   downloadButton("download","Get your report!", icon = shiny::icon("download")) 
              )),

#display columns of dataset
  fluidRow(
    column(9,
        #Display dataset
        tableOutput("table")   
      )),
)   


# Define server logic required to draw a histogram
server <- function(input, output,session) {
 
#Storing dataset in reactive variable
  data <- reactive({
    req(input$upload,cancelOutput = TRUE) #program doesn't run if condition is false
    ext <- tools::file_ext(input$upload$name)
    switch (ext,
            tsv = vroom::vroom(input$upload$datapath,delim ="\t"),
            csv  = vroom::vroom(input$upload$datapath,delim =","),
            validate("Upload a .csv or .tsv file"))}) 

  #Update widget range od no. of rows to be displayed
    observeEvent(data(),{
      updateSliderInput(inputId = "n",max = nrow(data()))
    }) 

#Displaying dataset    
  observeEvent(data(),{
    output$table <- renderTable(head(data(),input$n))
})
  
  #Update choices in widgets for x
  observeEvent(data(),{
    updateSelectInput(inputId = "x",choices = colnames(data()))
  })
  
  #Update choices in widgets for y
  observeEvent(data(),{
    updateSelectInput(inputId = "y",choices = colnames(data()))
  })
  
  # Store X values 
  x<- reactive(as.numeric(unlist(data()[,input$x])))
  
  #Store Y values
  y <- reactive(as.numeric(unlist(data()[,input$y])))

  
 #Linear regression model
  model <- reactive({
    req(input$upload)
    lm(y()~x(),data())
  })
  
  #Plot linear regression graph
  output$regression_plot <- renderPlot({
    ggplot(data(),
           aes(x(),y()))+ geom_point(alpha = 0.8)+ 
      geom_abline(slope = model()$coefficients[2],
                  intercept = model()$coefficients[1],color = "red")+
      labs(x= input$x,y=input$y)

  })
 
#Downloading the report
output$download <- downloadHandler(
   filename = "report.html",
  content = function(file) { #content of file
   
    #Using a temporary directory
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report_Bivariate_Linear_Regression.Rmd", tempReport, overwrite = TRUE)
    
    # Setting up parameters to pass to Rmd document
    params <- model()
    
  #Knitting the document
    rmarkdown::render(tempReport, output_file = file,
                      params = params)}
)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
