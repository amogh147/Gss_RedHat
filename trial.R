library(zoo)
library(ggvis)
library(shinyBS)
setwd("/home/amogh/Desktop")

attach(amogh)

data <- amogh

subset(data,opportunity_account_region=="APAC",select = c(opportunity_account_region,opportunity_account_subregion))
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
  h1("TAM Data Visualization"),
  
  
  sidebarPanel(
    selectInput("geo", "Please select a geography:", choices = as.character(unique(data$opportunity_account_region))),
    
    uiOutput("secondSelection"),
    uiOutput("thirdSelection"),
    #selectInput("reg", "Please select a Region:", choices = reg_choices),
   
    #sliderInput("inSlider", "Opportunity Close Date", min=m, max=ma, value = m),
    dateRangeInput("month",  label = h3("Date input"), min = min_date, max = max_date,  start = NULL, end = NULL, format = "m-yyyy" ,startview = "month"),
   # sliderInput("date", label = h3("Date input"), value = min_date,min=min_date,max=max_date,format="####.00"),
    sliderInput("inSlider", "Deal Size", min=min_deal, max=max_deal, value = min_deal)
    #selectInput("tam", "Please select TAM type:", choices = as.character(unique(data$sku_type)))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Data table", dataTableOutput("myTable")),
      tabPanel("Data chart",
               # fluidRow(...)
               fluidRow(
                 column(6,plotlyOutput(outputId="plot1", width="600px",height="300px")),  
                 column(6,plotOutput(outputId="plot2", width="600px",height="300px"))
               )
            
      )
    )
  )
)

server <- function(input,output,session)    {
  
  output$myTable<-renderDataTable({
    data})

  output$secondSelection <- renderUI({
    selectInput("Reg", "Region:", choices = c("All",as.character(data[data$opportunity_account_region==input$geo,"opportunity_account_subregion"])))
  })
  output$thirdSelection <- renderUI({
    selectInput("tam", "Tam:", choices = c("All",as.character(data[data$opportunity_account_subregion==input$Reg,"sku_type"])))
  })
  
  
 output$plot1 <- renderPlotly({
   if(input$Reg=="All" && input$tam=="All")
   {
   
     #print(as.yearmon(input$month[1]))
     #print(as.yearmon(input$month[2]))
     data<-subset(data , as.yearmon(data$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data$start_date) <= as.yearmon(input$month[2]))
    # print(data$start_date)
     data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     Num_tam<-aggregate(x = data_filter1$sku_type, by = list(data_filter1$opportunity_account_region,as.yearmon(data_filter1$start_date)),FUN = length)
     print(Num_tam)
     #print(min(Num_tam$Group.2))
    plot_ly(Num_tam, x = as.Date(Num_tam$Group.2),y = Num_tam$x,mode = "markers")
   
    
   }
   else if(input$tam=="All"){
     data<-subset(data , as.yearmon(data$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data$start_date) <= as.yearmon(input$month[2]))
     data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     data_filter3 <- subset(data_filter1,opportunity_account_subregion==input$Reg,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     
     #data_filter4 <- subset(data_filter3,start_date==input$date,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     Num_tam<-aggregate(x = data_filter3$sku_type, by = list(data_filter3$opportunity_account_region,data_filter3$opportunity_account_subregion,as.yearmon(data_filter3$start_date)),FUN = length)
     #print(Num_tam)
     
     plot_ly(Num_tam, x = as.Date(Num_tam$Group.3), y = Num_tam$x,mode = "markers")
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
  
  
   plot_ly(Num_tam, x = as.Date(Num_tam$Group.4), y = Num_tam$x,mode = "markers")
  #plot(Num_tam$Group.4, Num_tam$x)
   
 }
   
  
})
 

 output$plot2 <- renderPlot({
   if(input$Reg=="All")
   {
     data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     tam_filter <- subset(data_filter1,as.yearmon(data_filter1$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data_filter1$start_date) <= as.yearmon(input$month[2]),
                          select = c(sku_type,start_date))
     
     tam_type <- aggregate(x = tam_filter$sku_type, by = list(tam_filter$sku_type),FUN = length)
     #j<-c(1,2,3,4,5)
     tam_type <- tam_type[order(-tam_type$x),] 
     
     barloc<-barplot(tam_type$x,beside=TRUE,col="Skyblue",ylim=c(0,50+max(tam_type$x)),xaxt="n")
     axis(side = 1, 
          at=barloc, 
          lab=tam_type$Group.1,
          las=3, srt=45)
     
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
   
   barloc<-barplot(tam_type$x,beside=TRUE,col="Skyblue",ylim=c(0,50+max(tam_type$x)),xaxt="n")
   axis(side = 1, 
        at=barloc, 
        lab=tam_type$Group.1,
        las=3, srt=45)
 }
 
 }) 
 
 #############################################
 output$plot3 <- renderPlot({
   if(input$Reg=="All")
   {
     data_filter1 <- subset(data,opportunity_account_region==input$geo,select = c(opportunity_account_region,opportunity_account_subregion,sku_type,start_date))
     tam_filter <- subset(data_filter1,as.yearmon(data_filter1$start_date) >= as.yearmon(input$month[1]) & as.yearmon(data_filter1$start_date) <= as.yearmon(input$month[2]),
                          select = c(sku_type,start_date))
     
     tam_type <- aggregate(x = tam_filter$sku_type, by = list(tam_filter$sku_type),FUN = length)
     #j<-c(1,2,3,4,5)
     tam_type <- tam_type[order(-tam_type$x),] 
     
     barloc<-barplot(tam_type$x,beside=TRUE,col="Skyblue",ylim=c(0,50+max(tam_type$x)),xaxt="n")
     axis(side = 1, 
          at=barloc, 
          lab=tam_type$Group.1,
          las=3, srt=45)
     
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
     
     barloc<-barplot(tam_type$x,beside=TRUE,col="Skyblue",ylim=c(0,50+max(tam_type$x)),xaxt="n")
     axis(side = 1, 
          at=barloc, 
          lab=tam_type$Group.1,
          las=3, srt=45)
   }
   
 }) 

}



shinyApp(ui = ui , server = server)

