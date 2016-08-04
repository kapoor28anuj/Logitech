

require("RPostgreSQL")
library(plotly)
library("shinythemes")
library(shiny)
library(dplyr)
library(lubridate)
library(R.utils)
library(rhandsontable)
library(shinyIncubator)



# table to be used for editting( be sure to change formatting of the table to be used as the one used here)
table="pos_project_iteration"

#Retrieving data from server to input the filters
drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname = "****",
                host = "****", port = 5432,
                user = "*****", password = "******")
query <- sprintf("SELECT sku,seller,product_group FROM %s", table)
test <- dbGetQuery(db, query)
dbDisconnect(db)


# function to save data
saveData <- function(data) {
  # Connect to the database
  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv, dbname = "csc_amr",
                  host = "aws11bi01", port = 5432,
                  user = "akapoor", password = "summer2016")
  
  for (i in 1:nrow(data))
  {dbGetQuery(db,paste("update pos_test set sales =",data[i,'sales'],
                       " where sku='",data[i,'sku'],"' and seller='",data[i,'seller'],
                       "' and dates='",data[i,'dates'],"'",sep=""
  )
  )
    
  }
  dbDisconnect(db)
  print("saved")
}


# function to load data
loadData <- function() {
  # Connect to the database
  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv, dbname = "csc_amr",
                  host = "aws11bi01", port = 5432,
                  user = "akapoor", password = "summer2016")
  query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  print("Loaded")
  data
}

# the layout of the app
ui <- navbarPage (theme = shinytheme("cerulean"),
                  title = "POS Data Decomposition",
                  
                  
                  tabPanel(            # each tabPanel makes a seperate tab in the app
                    title = "Anomaly data",
                    sidebarLayout(
                      sidebarPanel(
                        wellPanel(              #it combines the objects inside it in one box for display
                          selectInput(           
                            inputId = "group","Product Group",unique(test$product_group),selected = unique(test$product_group)[1],multiple = FALSE,
                            width = 400,size = NULL ),   # input button for the product group
                          selectInput(
                            inputId = "seller1","Seller","",multiple = FALSE,   #
                            width = 400,size = NULL ),# input button for the seller
                          selectInput(
                            inputId = "sku1", "Sku",  "",multiple = FALSE,width = 400,size = NULL),# input button for the sku
                          actionButton(inputId = "clicks", label = "Display/remove smoothened line"),# Button to display smoothened line
                          actionButton(inputId = "save", label = "Save"),# Button to save data
                          actionButton(inputId = "refresh", label = "Refresh"), # Button to Refresh the data in the app
                          tags$h6("Note:-Press Refresh button to load/refresh data")
                          ),
                        wellPanel(tags$h3("Guidelines"),
                                  tags$h6("1.To start working press Refresh to get your data loaded from server. It may take few seconds. Thank you for patience."),
                                  tags$h6("2.Graph with the original sales is available on the left,you can switch to original and expected sales graph by pressing 'Display/remove smoothened line'"),
                                  tags$h6("3.You can edit sales values in the Anomaly data table"),tags$h6("4.Press Save to upload your changes to server"),              tags$h6("5.Press refresh to reload the editted values from server. You can also move further to another sku without reloading the data(It will save your time)"),
                                  tags$h6("6.For data points not identified by algorithm you can visit the 2nd tab 'All sale' and edit vales there and click save button there to upload your data."),
                                  tags$h6("Happy cleaning!"),width = 3
                        )
                          ),
                      
                      mainPanel(
                        wellPanel(
                          # 
                          # tags$img(
                          #   height = 100,
                          #   width = 150,
                          #   src = "Logo_logitech_black.png"),
                          # 
                          tags$h3("Sales graph"),
                          tags$hr(),
                          # used plotly in order to get interactive plots
                          plotlyOutput("plot"),tags$h6("Note:-Red points denote the anomalies found by the algorithm")
                        ),
                        tags$h3("Anomaly data"),
                        # used Rhandsontable to get an editable table 
                        fluidRow(
                          column(1,offset = 3, rHandsontableOutput("hot",height =500 ,width =700 )))
                        
                        
                        
                      ),position = c("right"),fluid = T
                    )
                  )
                  ,
                  tabPanel(   # This tab contains the table of all the data of selected SKU-Seller combination
                    title = "All sale",
                    fluidRow(column(1,actionButton(inputId = "Save_all", label = "Save"), rHandsontableOutput("table",height =1000 ,width =500 )))
                  ),
                  #promo data
                  tabPanel(
                    title = "Promo Data",
                    fluidRow(
                      column(12,
                             rHandsontableOutput('promo')
                      )
                    )
                  )
)



# The server part of the app including all the reactive and active parts

server <- function(input, output, session) {
  
  data=eventReactive(input$refresh,loadData())  # loading data when refresh is pressed
  
  result_display = reactive(subset(data(),
                                   sku == input$sku1 & seller == input$seller1 )
                            [order(subset(data(),sku == input$sku1 & seller == input$seller1)$dates), ])  # subsetting data based on inputs
  
  
  
  seller1 = reactive(unique(as.character(
    # subsetting seller input options based on the product_group selected
    subset(test,product_group == input$group)$seller
  )))    
  # subsetting sku input options based on the seller and group selected
  sku1 = reactive(unique(as.character(
    subset(test, seller == input$seller1 & product_group == input$group)$sku
  )))  
  # using subset of seller to give input choices in seller input button
  observeEvent(input$group,
               updateSelectInput(session, "seller1", choices = seller1(), select = seller1()[1]))
  
  # using subset of sku to give input choices in sku input button
  observeEvent(input$seller1,
               updateSelectInput(session, "sku1", choices = sku1(), select = sku1()[1]))
  # subsetting the data to get only the anomaly data
  result_display2 = reactive(
    subset(result_display(),anomaly=="1"
    )
  )
  #table on Anomaly data page showing only data points declared anomalies 
  output$table <-renderRHandsontable(rhandsontable(select(result_display(), dates,or_sales,sales,sku,seller)[order(result_display()$dates),] ,rowHeaders = NULL)
  )
  
  #table on "All data" page showing subset of data inclusing all the data points of SKU-seller combination sel
  
  output$hot <-renderRHandsontable(rhandsontable(
    select(result_display2(), dates,or_sales,sales,sku,seller)[order(result_display2()$dates),]
    ,rowHeaders = NULL)
  )
  
  # Graph showing original sales and the predicted sales (switching by the press of "Display/remove smoothened line" button)
  output$plot=renderPlotly(
    {
      if (as.numeric(input$clicks) %% 2 == 0) {
        ggplotly(ggplot(result_display(),aes(dates,or_sales))+geom_point(aes(color=as.character(result_display()$anomaly)),show.legend = F)+ scale_colour_manual(values=c( rgb(0,0,0,0),"red"))+geom_line()+xlab("Dates")+ylab("Sales")+ggtitle("Original sales trend"))
        
        
      } 
      else 
      {
        ggplotly(ggplot(result_display(),aes(dates,or_sales))+geom_line()+geom_line(data=result_display(),aes(dates,sales,color="orange"),show.legend = F)+xlab("Dates")+ylab("Sales")+ggtitle("Original sales trend in black and Expected sales trend in Orange"))
      }
    }
  )
  # Using  Save button on "Anomaly data" page to update the values on the server
  observeEvent(input$save,if(!is.null(input$hot))
    isolate(input$hot) %>%
      hot_to_r() %>%
      data.frame() %>%
      saveData())
  # Using  Save button on "ALl data" page to update the values on the server
   observeEvent(input$save_all,if(!is.null(input$table))
    isolate(input$table) %>%
      hot_to_r() %>%
      data.frame() %>%
      saveData()
    )
   Promo_display=reactive(subset(Promo_data_to_use,
                                 sku == input$sku1 & seller == input$seller1 )
                          )
   output$promo <- renderRHandsontable(rhandsontable(Promo_display(),rowHeaders = NULL))
  
}

shinyApp(ui = ui, server = server)