#Version 2
rm(list=ls())

library(shiny)
library(shinyWidgets)
library(rmarkdown)
library(readr)
library(tidyverse)
library(shinycssloaders)
library(vroom)
library(openxlsx)
library(zip)


setwd("C:/Users/Meghana Achanti/Documents/R/R/Regression/Linear Regression/Essential Regression")
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
           #Uploading data set
           fileInput("upload_x","Upload X",multiple = F),
           br(),
           
           fileInput("upload_y","Upload Y",multiple = F),
           
            #Choose delta
           numericInput("delta",label ="Choose delta",
                        value = NULL),
           br(),
           
           #Choose lambda
           numericInput("lambda",label ="Choose lambda",
                        value = NULL),
           br(),
           
           #Choose beta_est_method
           pickerInput("beta_est",
                       label ="Choose the method to be used to estimate beta", 
                       choices = NULL,selected= NULL),
           
           br(),
           
           #Action button
           actionButton("submit","Compute !!! "),
           
           br()
    ),
    
    column(6,
           #Add an image
           img(src = "Regression_cartoon.jpg",width =500)
    )
  ),
  fluidRow(
    column(6,
           # #Display data set
           # tableOutput("data_table"),
    )
  ),
  
  fluidRow(
    column(6,
           br(),
           #Choose files to be downloaded
           pickerInput("data_req",
                       label ="Choose the files to be downloaded", 
                       choices = NULL,selected= NULL,multiple = TRUE),
    ),
    column(6,
            br(),
           
           #Download result
           downloadButton("download",label = "Get report!"),
           
           #Display done
           h3(textOutput("done"))
    )
  )
  
)



# Define server logic 
server <- function(input, output,session) {
  
  #Read data
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
            validate("Upload only .csv / .txt/ .tsv files")
    )
    
  })
  
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
            validate("Upload only .csv / .txt/ .tsv files")
    )
    
  })

  #Update beta_est_method
  observeEvent(data_y(),{
    updatePickerInput(session=session,inputId ="beta_est",
                      choices = c("Dantzig","LS"))
  })
  
   #Obtain x_data
  x_dat <- reactive({
    req(input$submit)
    as.matrix(data_x())
  })
  
  feature_names <- reactive(names(x_dat()))
  
  y_dat <- reactive({
    req(input$submit)
    as.matrix(data_y())
  })
  
  Y <- reactive( {y_dat() - colMeans(data_y(),na.rm = TRUE)})
  X <- reactive({scale(x_dat(),center = TRUE,scale = TRUE )})
  
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
  
  
  #z_hat
  A_hat <- reactive( res()$A)
  z_hat <- reactive({
    X() %*% A_hat() %*% solve(crossprod(A_hat()))
  })

  output$done <- renderText({
    req(res())
    "COMPUTATION DONE"
  })
  
  #Update download options
  
  observeEvent(input$submit,{
    # req(input$beta_est =="Dantzig")
    updatePickerInput(session=session,inputId ="data_req",
                      choices = c("Result"= "res",
                                  "Beta" = "beta_coefficients",
                                  "Matrix Z" = "z_hat"))
  })
 
  #Update format of files to be downloaded
  observeEvent(input$submit,{
    updatePickerInput(session=session,inputId = "format",choices = c("CSV"="csv",
                                                                     "XLSX"="xlsx") )
  })
  
  #Null data data frame
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
