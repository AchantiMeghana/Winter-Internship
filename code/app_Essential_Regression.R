rm(list=ls()) 

library(shiny)
library(shinyWidgets)
library(rmarkdown)
library(readr)
library(tidyverse)
library(shinycssloaders)
library(vroom)
library(zip)


setwd(".../Essential Regression")
source("SupLOVE.R")
source("K-CV.R")
source("Helper.R")
source("Other_algorithms.R")


# Define UI for application
ui <- fluidPage(
  setBackgroundColor(color = c("#c8fffa","#d3ffce"),gradient="radial"),
  
  titlePanel("Essential Regression"),
  
  fluidRow(
    column(6,
           #Generate upload widget for X data
           fileInput("upload_x","Upload X",multiple = F),
           br(),
           
          #Generate upload widget for Y data
           fileInput("upload_y","Upload Y",multiple = F),
           
           #Widget to choose delta
           numericInput("delta",label ="Choose delta",
                        value = NULL),
           br(),
           
            #Widget to choose lambda
           numericInput("lambda",label ="Choose lambda",
                        value = NULL),
           br(),
           
            #Widget to choose method to estimate beta
           pickerInput("beta_est",
                       label ="Choose the method to be used to estimate beta", 
                       choices = NULL,selected= NULL),
           br(),
           
           
           #Action button 
           actionButton("submit","Compute !!! "),
           br()
           
    ),

    column(6,
           #Add an image in the right side of the webpage
           img(src = "Regression_cartoon.jpg",width =500)
    )
  ),
    
  fluidRow(
    column(6,
           br(),
           #Widget to choose files to be downloaded
           pickerInput("data_req",
                       label ="Choose the files to be downloaded", 
                       choices = NULL,selected= NULL,multiple = TRUE),
    ),
    column(6,
            br(),
           
           #widget to download result
           downloadButton("download",label = "Get report!"),
           
           #Display message when computation is done
           h3(textOutput("done"))
    )
  )
)


# Define server logic 
server <- function(input, output,session) {
  
  #Read data from X dataset
  data_x <- reactive({
    req(input$upload_x)
    ext <- tools::file_ext(input$upload_x$name)
    switch (ext,
            tsv = read_delim(input$upload_x$datapath, "\t",
                             escape_double = FALSE, trim_ws = TRUE),
            csv  = read_delim(input$upload_x$datapath, ",",
                              escape_double = FALSE, trim_ws = TRUE),
            txt = read_delim(input$upload_x$datapath, " ",
                             escape_double = FALSE, trim_ws = TRUE),
            #Display error message if the uploaded file is not .csv,.tsv,.txt file
            validate("Upload only .csv / .txt/ .tsv files")
    )
    
  })
  
  
   #Read data from Y dataset
  data_y <- reactive({
    req(input$upload_y)
    ext <- tools::file_ext(input$upload_y$name)
    switch (ext,
            tsv = read_delim(input$upload_y$datapath, "\t",
                             escape_double = FALSE, trim_ws = TRUE),
            csv  = read_delim(input$upload_y$datapath, ",",
                              escape_double = FALSE, trim_ws = TRUE),
            txt = read_delim(input$upload_y$datapath, " ",
                             escape_double = FALSE, trim_ws = TRUE),
            #Display error message if the uploaded file is not .csv,.tsv,.txt file
            validate("Upload only .csv / .txt/ .tsv files")
    )
  })

  #Update choices in the widget to choose beta_estimation_method
  observeEvent(data_y(),{
    updatePickerInput(session=session,inputId ="beta_est",
                      choices = c("Dantzig","LS"))
  })
  
   # Store X data
  x_dat <- reactive({
    req(input$submit)
    as.matrix(data_x())
  })
  
  feature_names <- reactive(names(x_dat()))
  
  #Store Y data
  y_dat <- reactive({
    req(input$submit)
    as.matrix(data_y())
  })
  
  # Perform centering of Y using mean
  Y <- reactive( {y_dat() - colMeans(data_y(),na.rm = TRUE)})
  
  #scale X data
  X <- reactive({scale(x_dat(),center = TRUE,scale = TRUE )})
  
  #Code for ER
  res <- reactive({
    req(input$submit,data_x(),data_y())
    
    ER(as.matrix(Y()), as.matrix(X()), delta =input$delta, pred = F, 
       beta_est = as.character(input$beta_est), 
       rep_CV = 50, verbose = T,
       merge = F, CI = T, correction = NULL, lbd = input$lambda)
    
  })
  
  #Obtain beta_coefficients,res, z_hat
    beta <- reactive({
    res()$beta})
  
  beta_scaled <- reactive({
    diag(sqrt(diag(res()$C))) %*% beta()})
  
  order_coef_beta<- reactive({
    order(abs(beta_scaled()), decreasing = T)
  })
  
  
  #Z_hat
  A_hat <- reactive( res()$A)
  z_hat <- reactive({
    X() %*% A_hat() %*% solve(crossprod(A_hat()))
  })

  #Display message when computation is done
  output$done <- renderText({
    req(res())
    "COMPUTATION DONE"
  })
  
  #Update choices in the download widget
  observeEvent(input$submit,{
    updatePickerInput(session=session,inputId ="data_req",
                      choices = c("Result"= "res",
                                  "Beta" = "beta_coefficients",
                                  "Matrix Z" = "z_hat"))
  })
 
    dat_null <- reactive(as.character("The value is NULL"))
  
  # CSV format
  output$download <- downloadHandler(
    filename = function() {
      # if(input$format== "csv"){paste("Regression", ".zip", sep = "")}
      # if(input$format== "xlsx"){paste("Regression", ".xlsx", sep = "")}
      paste("Regression", ".zip", sep = "")
    },
    
    content = function(file) {
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
      # write.csv(res(), file)
      
      # wb <-createWorkbook()
      
      if("res" %in% input$data_req) {
        for (i in 1:length(res())){
          if(i!= 5){
            if(!is.null(res()[i])){
             
              file_name <- paste("Result_",as.character(names(res())[i]),".csv",sep = "")
              write.csv( res()[i],file_name,row.names = FALSE)
              files <- c(file_name,files) 
            }
            else {
              file_name <- paste("Result_",as.character(names(res())[i]),".csv",sep = "")
              write.csv(x = as.character("NULL"),file_name,row.names = FALSE) 
              files <- c(file_name,files)
            }
          }
          
          if(i == 5) {
            file_name <- paste(as.character(names(res())[i]),".csv",sep = "")
            write.csv(unlist(res()[i]),file_name)
            files <- c(file_name,files)
          }
        }
      }
      
      if("z_hat" %in% input$data_req){
        file_name <- paste(as.character("Z_hat"),".csv",sep = "")
        write.csv( z_hat(),file_name,row.names = FALSE)
        files <- c(file_name,files)         
      }
      
      if("beta_coefficients" %in% input$data_req){
        file_name <- paste(as.character("order_coef_beta"),".csv",sep = "")
        write.csv(order_coef_beta(),file_name,row.names = FALSE)
        files <- c(file_name,files)         
      }
      
      zip::zip(file,files)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
