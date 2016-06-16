shinyServer(function(input,output,session)    {
  
  
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
)
