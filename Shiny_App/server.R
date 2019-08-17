past_orders_matrix = readRDS("past_orders_matrix.rds")

new_data = readRDS("campaign.rds")
item_list <- readRDS("item_list.rds")
source("Analysis1.R")
source("Analysis2.R")
source("Analysis3.R")

server <-function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  #output$plot1 <- renderPlot({
  #  data <- histdata[seq_len(input$slider)]
  #  hist(data)
  #})
  
  #output$plot1 <-renderHighchart ({
  
  #if(input$country==inf$region)
  #{
  #    df<-inf %>% filter(region==input$country)#making is the dataframe of the country
  
  #    df$inflation<-as.numeric(df$inflation)
  #    df$year<-as.numeric(df$year)
  
  #plotting the data
  #    hchart(df, "line",color="#DC270C",hcaes(x=year,y=inflation))  %>%
  
  #      hc_exporting(enabled = TRUE) %>% 
  #      hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
  #                 shared = TRUE, borderWidth = 2) %>%
  #      hc_title(text="Time series plot of Inflation Rates",align="center") %>%
  #      hc_subtitle(text="Data Source: IMF",align="center") %>%
  #      hc_add_theme(hc_theme_elementary()) 
  #to add 3-d effects
  #hc_chart(type = "column",
  #options3d = list(enabled = TRUE, beta = 15, alpha = 15))
  
  
  # })
  
  #Customer Segment
  output$plot <- renderPlot({
    # plot(x=x(), y=y(), main = "iris dataset plot", xlab = xl(), ylab = yl())
    #ggplot(dataPokemon,aes(x=Attack,y= Defense))+geom_point(color='Red') # ploting through GGPLOT
    ggplot(new_data, aes(Recency_group, Frequency_group)) +
      geom_count() +
      facet_grid(mon_value_group ~ .) +
      labs(x="Recency", y="Frequency", title="RFM analysis since 2017")
    #library("plotly")
    #plotly::ggplotly(plot_code)
    
  })
  
  output$plotm1 <- renderPlot({
    retail %>% 
      group_by(Description) %>% 
      summarize(count = n()) %>% 
      top_n(10, wt = count) %>%
      arrange(desc(count)) %>% 
      ggplot(aes(x = reorder(Description, count), y = count))+
      geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
      labs(x = "", y = "Top 10 Best Sellers", title = "Most Ordered Products") +
      coord_flip() +
      theme_grey(base_size = 12)
  })
  
  
  output$plotm3 <- renderPlot({
    retail %>% 
      mutate(Value = UnitPrice * Quantity) %>% 
      group_by(InvoiceNo) %>% 
      summarise(n = mean(Value)) %>%
      ggplot(aes(x=n)) +
      geom_histogram(bins = 200000, fill="firebrick3", colour = "sandybrown") + 
      coord_cartesian(xlim=c(0,100)) +
      scale_x_continuous(breaks=seq(0,100,10)) +
      labs(x = "Average Value per Purchase", y = "") + 
      theme_grey(base_size = 14)
  })
  
  output$plotimage <- renderImage({
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    ##outfile <- tempfile(fileext='/Users/X Ins/Resource/R/Market_Basket_Analysis/Shiny_App/www/clustering.png')
    
    # Generate a png
    ##png(outfile, width=400, height=400)
    ##hist(rnorm(input$n))
    ##dev.off()
    
    # Return a list
    ##list(src = outfile,
    ##   alt = "This is alternate text")
    ##}, deleteFile = TRUE)
    list(src = "www/clustering.png")
  })
  
  output$item_recom <- renderTable({
    # react to submit button
    
    input$submit
    # gather input in string
    customer_order <- 
      isolate(
        
        unique(c(input$input_item1, input$input_item2, input$input_item3, 
                 input$input_item4, input$input_item5))
      )
    
    
    # put in a matrix format
    new_order <- item_list %>%
      # Add a 'value' column with 1's for customer order items
      mutate(value = as.numeric(Description %in% customer_order)) %>%
      # Spread into sparse matrix format
      spread(key = Description, value = value) %>%
      # Change to a matrix
      as.matrix() %>% 
      # Convert to class "dgCMatrix"
      as("dgCMatrix")
    
    # Add new order to retail matrix - binding 2 matrices
    all_orders_dgc <- t(rbind(new_order,past_orders_matrix))
    
    # Set items to predict range
    items_to_predict <- which(all_orders_dgc[ ,1] == 0)
    # items_to_predict <- 1:nrow(all_orders_dgc)
    # Set user to 1
    users <- c(1)
    # Set prediction indices
    prediction_indices <- as.matrix(expand.grid(items_to_predict, users = users))
    
    # Run IBCF model
    recomm <- predict_cf(all_orders_dgc, prediction_indices, 
                         "ibcf", FALSE, cal_cos, 3, FALSE, 4000, 2000)
    
    # Put recommended products into a dataframe
    recomm[,users] %>% 
      as.data.frame() %>% 
      rownames_to_column('NOTE that not all combinations of products return suggestions') %>% 
      filter(.>0) %>% 
      select('NOTE that not all combinations of products return suggestions')
    
  })
}

