#library(zoo)
library(shinydashboard)
library(shinyBS)
setwd("/home/amogh/Desktop")
#install.packages("sae")
data<-read.csv("~/Desktop/SCM_TAM.txt", sep=",",na.strings=c("","NA"))

cc=is.na(data$start_date)
m=which(cc==c("TRUE"))
data=data[-m,]
data=data[,-which(names(data) == "sales_rep_data")]


#data$opportunity_account_region <- as.character(data$opportunity_account_region)
#data$opportunity_account_region[data$opportunity_account_region == NA]<-"North America"

#data$opportunity_account_region
#unique(subset(data,opportunity_account_region=="EMEA",select=c(opportunity_account_region, opportunity_account_subregion)))

#pulling out different regions for dropdown in geography
geo_choices<-as.character(unique(data$opportunity_account_region))

#pulling out different regions for dropdown in geography
reg_choices<-as.character(unique(data$opportunity_account_subregion))

#getting the min date and max date for slider range
min_date<-min(as.numeric(format(as.Date(data$start_date),"%m-%Y")))
max_date<-max(as.numeric(format(as.Date(data$start_date),"%m-%Y")))

max_date


#getting the min date and max deal size for slider range
min_deal <- min(as.numeric(data$number_total_usd))
max_deal <- max(as.numeric(data$number_total_usd))

#agrregating the data and getting them in a dataframe to plot the grpahs (num of tag against date)
#Num_tam<-aggregate(x = data$sku_type, by = list(data$opportunity_account_region,data$opportunity_account_subregion,data$sku_type,data$start_date),FUN = length)
#num_tam<-data.frame(Num_tam)
#Num_tam

library(shiny)
library(plotly)
ui <- fluidPage(
#  h1("TAM Data Visualization"),
  tags$head(tags$style(
    HTML('
         #sidebar {
           
                      height:900px;
                    position: relative;
                         top: 20px;
                        left: 20px;
         }


        body, label, input, button, select { 
          font-family: "Arial";
        }')
  )),
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        background-color: #778899;
      }

    "))
  ),
  
  headerPanel("TAM Data Visualization"),
  sidebarPanel(
    tags$head(
      tags$style(type="text/css", "select { max-height: 1540px; }"),
      tags$style(type="text/css", ".span4 { max-height: 1590px; }"),
      tags$style(type="text/css", ".well { max-height: 1580px; }")
    ),
    id="sidebar",
    selectInput("geo", "Please select a geography:", choices = c("All",as.character(unique(data$opportunity_account_region)))),
    
    uiOutput("secondSelection"),
    uiOutput("thirdSelection"),
    #selectInput("reg", "Please select a Region:", choices = reg_choices),
   
    #sliderInput("inSlider", "Opportunity Close Date", min=m, max=ma, value = m),
    dateRangeInput("month",  label = h3("Date input"), min = min_date, max = max_date, start = Sys.Date() , end = Sys.Date() + 1500, format = "m-yyyy" ,startview = "month"),
    fluidRow(class="r1",helpText("Note: while the data view will show only the specified",
             "number of observations because of few empty entries in the date, the Data Table will still be based",
             "on the full dataset.")),
    tags$head(tags$style("
                        .r1{background-color: white;position: relative;top: 120px;	border-bottom: 1px solid #ccc;}"
    ))
  ),
  
  mainPanel(
    br(),
    tabsetPanel(id="tabs",
      tabPanel("Data table", dataTableOutput("myTable")),
      tabPanel("Data chart",
               # fluidRow(...)
               fluidRow(class="myRow1",
                 column(6,plotlyOutput(outputId="plot1", width="500px",height="500px")),  
                 column(6,plotlyOutput(outputId="plot4", width="500px",height="500px"))
               ),
               fluidRow(
                 class="myRow2",
                 div="r1",
                 column(6,plotlyOutput(outputId="plot2", width="500px",height="500px")),
                 column(6,plotlyOutput(outputId="plot3", width="500px",height="500px"))
               ),
               tags$head(tags$style("
                         .myRow1{height:550px;position: relative;top: 20px;	border-bottom: 1px solid #ccc;}
                         .myRow2{height:1750px;position: relative;top: 35px;}
                         r1{border-right: 1px solid #ccc;}"
                        ))
            
      )
    )
  )
  
)

server <- function(input,output,session)    {
  
 
  output$myTable<-renderDataTable({
    data})

  output$secondSelection <- renderUI({
    if(input$geo=="All")
    {selectInput("Reg", "Please select a Region:", choices = c("All",as.character(data$opportunity_account_subregion)))}
    else
    {
    selectInput("Reg", "Please select a Region:", choices = c("All",as.character(data[data$opportunity_account_region==input$geo,"opportunity_account_subregion"])))
    }
  })
  output$thirdSelection <- renderUI({
    selectInput("tam", "Please select a TAM type:", choices = c("All",as.character(data[data$opportunity_account_subregion==input$Reg,"sku_type"])))
  })
  
  
 output$plot1 <- renderPlotly({
   if(input$geo=="All")
   {
     
     #print(as.yearmon(input$month[1]))
     #print(as.yearmon(input$month[2]))
     data<-subset(data , as.yearmon(data$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data$start_date) <= as.yearmon(input$month[2]))
     # print(data$start_date)
     if(input$Reg!="All")
     {
     data_filter1 <- subset(data,opportunity_account_subregion==input$Reg,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     }
     else
     {
       data_filter1 <-data
     }
     Num_tam<-aggregate(x = data_filter1$sku_type, by = list(data_filter1$opportunity_account_region,as.yearmon(data_filter1$start_date)),FUN = length)
     print(Num_tam)
     #print(min(Num_tam$Group.2))
     f <- list(
       family = "Courier New, monospace",
       size = 18,
       color = "#7f7f7f"
     )
     x <- list(
       title = "Start Date",
       titlefont = f
     )
     y <- list(
       title = "Number of TAMs",
       titlefont = f
     )
     plot_ly(Num_tam, x = as.Date(Num_tam$Group.2),y = round(Num_tam$x,0),mode = "markers", marker = list(opacity = 0.6, size = 18))
     layout(title = "TAMs by Start Date",
            xaxis = x, yaxis = y)
     
   }
   else if(input$Reg=="All" && input$tam=="All")
   {
     
     #print(as.yearmon(input$month[1]))
     #print(as.yearmon(input$month[2]))
     data<-subset(data , as.yearmon(data$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data$start_date) <= as.yearmon(input$month[2]))
    # print(data$start_date)
     data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     Num_tam<-aggregate(x = data_filter1$sku_type, by = list(data_filter1$opportunity_account_region,as.yearmon(data_filter1$start_date)),FUN = length)
     print(Num_tam)
     #print(min(Num_tam$Group.2))
     f <- list(
       family = "Courier New, monospace",
       size = 18,
       color = "#7f7f7f"
     )
     x <- list(
       title = "Start Date",
       titlefont = f
     )
     y <- list(
       title = "Number of TAMs",
       titlefont = f
     )
    plot_ly(Num_tam, x = as.Date(Num_tam$Group.2),y = round(Num_tam$x,0),mode = "markers", marker = list(opacity = 0.6, size = 18))
    layout(title = "TAMs by Start Date",
           xaxis = x, yaxis = y)
  
   }
   else if(input$tam=="All"){
    
     data<-subset(data , as.yearmon(data$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data$start_date) <= as.yearmon(input$month[2]))
     data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     data_filter3 <- subset(data_filter1,opportunity_account_subregion==input$Reg,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     
     #data_filter4 <- subset(data_filter3,start_date==input$date,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     Num_tam<-aggregate(x = data_filter3$sku_type, by = list(data_filter3$opportunity_account_region,data_filter3$opportunity_account_subregion,as.yearmon(data_filter3$start_date)),FUN = length)
     #print(Num_tam)
     f <- list(
       family = "Courier New, monospace",
       size = 18,
       color = "#7f7f7f"
     )
     x <- list(
       title = "Start Date",
       titlefont = f
     )
     y <- list(
       title = "Number of TAMs",
       titlefont = f
     )
     
     plot_ly(Num_tam, x = as.Date(Num_tam$Group.3), y = Num_tam$x,mode = "markers")
     layout(title = "TAMs by Start Date",
            xaxis = x, yaxis = y)
   }
   else
   {
     
    data<-subset(data , as.yearmon(data$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data$start_date) <= as.yearmon(input$month[2]))
  data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
   data_filter2 <- subset(data_filter1,opportunity_account_subregion==input$Reg,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
   data_filter3 <- subset(data_filter2,sku_type==input$tam,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
   #data_filter4 <- subset(data_filter3,start_date==input$date,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
   Num_tam<-aggregate(x = data_filter3$sku_type, by = list(data_filter3$opportunity_account_region,data_filter3$opportunity_account_subregion,data_filter3$sku_type,as.yearmon(data_filter3$start_date)),FUN = length)
   #print(Num_tam)
   f <- list(
     family = "Courier New, monospace",
     size = 18,
     color = "#7f7f7f"
   )
   x <- list(
     title = "Start Date",
     titlefont = f
   )
   y <- list(
     title = "Number of TAMs",
     titlefont = f
   )
   
   plot_ly(Num_tam, x = as.Date(Num_tam$Group.4), y = Num_tam$x,mode = "markers")
  #plot(Num_tam$Group.4, Num_tam$x)
   layout(title = "TAMs by Start Date",
          xaxis = x, yaxis = y)
  
   
 }
   
  
})
 
##########################################
 output$plot2 <- renderPlotly({
   if(input$geo=="All")
   {
     
     #print(as.yearmon(input$month[1]))
     #print(as.yearmon(input$month[2]))
     data<-subset(data , as.yearmon(data$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data$start_date) <= as.yearmon(input$month[2]))
     # print(data$start_date)
     if(input$Reg!="All")
     {
       tam_filter <- subset(data,opportunity_account_subregion==input$Reg,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     }
     else
     {
       tam_filter <-data
     }
     tam_type<-aggregate(x = tam_filter$sku_type, by = list(tam_filter$sku_type),FUN = length)
     #print(Num_tam)
     #print(min(Num_tam$Group.2))
     tam_type <- tam_type[order(-tam_type$x),] 
     f <- list(
       family = "Courier New, monospace",
       size = 18,
       color = "#7f7f7f"
     )
     x <- list(
       title = "",
       titlefont = f
     )
     y <- list(
       title = "Number of TAMs",
       titlefont = f
     )
     plot_ly(x = tam_type$Group.1, y = tam_type$x, type = "bar", marker = list(color = toRGB("sky blue")),barmode = "horizontal")
     layout(title = "TAMs by type",
            xaxis = x, yaxis = y)
     
   }
   else if(input$Reg=="All")
   {
     data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     tam_filter <- subset(data_filter1,as.yearmon(data_filter1$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data_filter1$start_date) <= as.yearmon(input$month[2]),
                          select = c(sku_type,start_date))
     
     tam_type <- aggregate(x = tam_filter$sku_type, by = list(tam_filter$sku_type),FUN = length)
     #j<-c(1,2,3,4,5)
     tam_type <- tam_type[order(-tam_type$x),] 
     f <- list(
       family = "Courier New, monospace",
       size = 18,
       color = "#7f7f7f"
     )
     x <- list(
       title = "",
       titlefont = f
     )
     y <- list(
       title = "Number of TAMs",
       titlefont = f
     )
     plot_ly(x = tam_type$Group.1, y = tam_type$x, type = "bar", marker = list(color = toRGB("sky blue")),barmode = "horizontal")
     layout(title = "TAMs by type",
            xaxis = x, yaxis = y)
   }
   else
   {
   data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
   data_filter2 <- subset(data_filter1,opportunity_account_subregion==input$Reg,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
   
   tam_filter <- subset(data_filter2,as.yearmon(data_filter2$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data_filter2$start_date) <= as.yearmon(input$month[2]),
                        select = c(sku_type,start_date))
   # print(tam_filter)
   tam_type <- aggregate(x = tam_filter$sku_type, by = list(tam_filter$sku_type),FUN = length)
   #j<-c(1,2,3,4,5)
   tam_type <- tam_type[order(-tam_type$x),] 
   
   f <- list(
     family = "Courier New, monospace",
     size = 18,
     color = "#7f7f7f"
   )
   x <- list(
     title = "",
     titlefont = f
   )
   y <- list(
     title = "Number of TAMs",
     titlefont = f
   )
   plot_ly(x = tam_type$Group.1, y = tam_type$x, type = "bar", marker = list(color = toRGB("sky blue")))
   layout(title = "TAMs by type",
          xaxis = x, yaxis = y)
 }
 
 }) 
 
 #############################################
 output$plot3 <- renderPlotly({
   if(input$geo=="All")
   {
     
     #print(as.yearmon(input$month[1]))
     #print(as.yearmon(input$month[2]))
     data<-subset(data , as.yearmon(data$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data$start_date) <= as.yearmon(input$month[2]))
     # print(data$start_date)
     if(input$Reg!="All")
     {
       tam_filter <- subset(data,opportunity_account_subregion==input$Reg,select = c(opportunity_account_region,opportunity_account_subregion,opportunity_account_name,start_date))
     }
     else
     {
       tam_filter <-data
     }
     tam_type<-aggregate(x = tam_filter$opportunity_account_name, by = list(tam_filter$opportunity_account_name),FUN = length)
     #print(Num_tam)
     #print(min(Num_tam$Group.2))
     tam_type <- tam_type[order(-tam_type$x),] 
     f <- list(
       family = "Courier New, monospace",
       size = 18,
       color = "#7f7f7f"
     )
     x <- list(
       title = "",
       titlefont = f
     )
     y <- list(
       title = "Number of TAMs",
       titlefont = f
     )
     plot_ly(x = tam_type$Group.1, y = tam_type$x, type = "bar", marker = list(color = toRGB("sky blue")),barmode = "horizontal")
     layout(title = "TAMs by type",
            xaxis = x, yaxis = y)
     
   }
   else if(input$Reg=="All")
   {
     data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,opportunity_account_name,start_date))
     tam_filter <- subset(data_filter1,as.yearmon(data_filter1$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data_filter1$start_date) <= as.yearmon(input$month[2]),
                          select = c(opportunity_account_name,start_date))
     
     tam_type <- aggregate(x = tam_filter$opportunity_account_name, by = list(tam_filter$opportunity_account_name),FUN = length)
     #j<-c(1,2,3,4,5)
     tam_type <- tam_type[order(-tam_type$x),] 
     
     f <- list(
       family = "Courier New, monospace",
       size = 18,
       color = "#7f7f7f"
     )
     x <- list(
       title = "",
       titlefont = f
     )
     y <- list(
       title = "Number of TAMs",
       titlefont = f
     )
     plot_ly(x = tam_type$Group.1, y = tam_type$x, type = "bar", marker = list(color = toRGB("sky blue")),enumerated= "h")
     layout(title = " TAMs by Account",
            xaxis = x, yaxis = y)
     
   }
   else
   {
     data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,opportunity_account_name,start_date))
     data_filter2 <- subset(data_filter1,opportunity_account_subregion==input$Reg,select = c(opportunity_account_region,opportunity_account_subregion,opportunity_account_name,start_date))
     
     tam_filter <- subset(data_filter2,as.yearmon(data_filter2$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data_filter2$start_date) <= as.yearmon(input$month[2]),
                          select = c(opportunity_account_name,start_date))
     # print(tam_filter)
     tam_type <- aggregate(x = tam_filter$opportunity_account_name, by = list(tam_filter$opportunity_account_name),FUN = length)
     #j<-c(1,2,3,4,5)
     tam_type <- tam_type[order(-tam_type$x),] 
     
     f <- list(
       family = "Courier New, monospace",
       size = 18,
       color = "#7f7f7f"
     )
     x <- list(
       title = "",
       titlefont = f
     )
     y <- list(
       title = "Number of TAMs",
       titlefont = f
     )
     plot_ly(x = tam_type$Group.1, y = tam_type$x, type = "bar", marker = list(color = toRGB("sky blue")),enumerated= "h")
     layout(title = " TAMs by Account",
            xaxis = x, yaxis = y)
   }
   
 }) 
 
 #################################################################################
 output$plot4 <- renderPlotly({
   if(input$geo=="All")
   {
     
     #print(as.yearmon(input$month[1]))
     #print(as.yearmon(input$month[2]))
     data<-subset(data , as.yearmon(data$end_date) >= as.yearmon(input$month[1]) & as.yearmon(data$end_date) <= as.yearmon(input$month[2]))
     # print(data$start_date)
     if(input$Reg!="All")
     {
       data_filter1 <- subset(data,opportunity_account_subregion==input$Reg,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,end_date))
     }
     else
     {
       data_filter1 <-data
     }
     Num_tam<-aggregate(x = data_filter1$sku_type, by = list(data_filter1$opportunity_account_region,as.yearmon(data_filter1$end_date)),FUN = length)
     print(Num_tam)
     #print(min(Num_tam$Group.2))
     f <- list(
       family = "Courier New, monospace",
       size = 18,
       color = "#7f7f7f"
     )
     x <- list(
       title = "Start Date",
       titlefont = f
     )
     y <- list(
       title = "Number of TAMs",
       titlefont = f
     )
     plot_ly(Num_tam, x = as.Date(Num_tam$Group.2),y = round(Num_tam$x,0),mode = "markers", marker = list(opacity = 0.6, size = 18))
     layout(title = "TAMs by Start Date",
            xaxis = x, yaxis = y)
     
   }
   else if(input$Reg=="All" && input$tam=="All")
   {
     
     #print(as.yearmon(input$month[1]))
     #print(as.yearmon(input$month[2]))
     data<-subset(data , as.yearmon(data$end_date) >= as.yearmon(input$month[1]) & as.yearmon(data$end_date) <= as.yearmon(input$month[2]))
     # print(data$end_date)
     data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,end_date))
     Num_tam<-aggregate(x = data_filter1$sku_type, by = list(data_filter1$opportunity_account_region,as.yearmon(data_filter1$end_date)),FUN = length)
     print(Num_tam)
     #print(min(Num_tam$Group.2))
     f <- list(
       family = "Courier New, monospace",
       size = 18,
       color = "#7f7f7f"
     )
     x <- list(
       title = "End Date",
       titlefont = f
     )
     y <- list(
       title = "Number of TAMs",
       titlefont = f
     )
     plot_ly(Num_tam, x = as.Date(Num_tam$Group.2),y = Num_tam$x,mode = "markers")
     layout(title = "TAMs by End Date",
            xaxis = x, yaxis = y)
     
   }
   else if(input$tam=="All"){
     data<-subset(data , as.yearmon(data$end_date) >= as.yearmon(input$month[1]) & as.yearmon(data$end_date) <= as.yearmon(input$month[2]))
     data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,end_date))
     data_filter3 <- subset(data_filter1,opportunity_account_subregion==input$Reg,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,end_date))
     
     #data_filter4 <- subset(data_filter3,end_date==input$date,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,end_date))
     Num_tam<-aggregate(x = data_filter3$sku_type, by = list(data_filter3$opportunity_account_region,data_filter3$opportunity_account_subregion,as.yearmon(data_filter3$end_date)),FUN = length)
     #print(Num_tam)
     f <- list(
       family = "Courier New, monospace",
       size = 18,
       color = "#7f7f7f"
     )
     x <- list(
       title = "End Date",
       titlefont = f
     )
     y <- list(
       title = "Number of TAMs",
       titlefont = f
     )
     plot_ly(Num_tam, x = as.Date(Num_tam$Group.3),y = Num_tam$x,mode = "markers")
     
     
     layout(title = "TAMs by End Date",
            xaxis = x, yaxis = y)
   }
   else
   {
     data<-subset(data , as.yearmon(data$end_date) >= as.yearmon(input$month[1]) & as.yearmon(data$end_date) <= as.yearmon(input$month[2]))
     data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,end_date))
     data_filter2 <- subset(data_filter1,opportunity_account_subregion==input$Reg,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,end_date))
     data_filter3 <- subset(data_filter2,sku_type==input$tam,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,end_date))
     #data_filter4 <- subset(data_filter3,end_date==input$date,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,end_date))
     Num_tam<-aggregate(x = data_filter3$sku_type, by = list(data_filter3$opportunity_account_region,data_filter3$opportunity_account_subregion,data_filter3$sku_type,as.yearmon(data_filter3$end_date)),FUN = length)
     #print(Num_tam)
     f <- list(
       family = "Courier New, monospace",
       size = 18,
       color = "#7f7f7f"
     )
     x <- list(
       title = "End Date",
       titlefont = f
     )
     y <- list(
       title = "Number of TAMs",
       titlefont = f
     )
     plot_ly(Num_tam, x = as.Date(Num_tam$Group.4),y = Num_tam$x,mode = "markers")
     
     
     layout(title = "TAMs by End Date",
            xaxis = x, yaxis = y)
   }
   
   
 })
 
}


shinyApp(ui = ui , server = server)

