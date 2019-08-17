library(shiny)
library(shinydashboard)
library(tidyverse)
library(Matrix)
library(ggplot2) # data visualization

item_list <- readRDS("item_list.rds")
#new_data <- readRDS("/Users/X Ins/Resource/R/Market_Basket_Analysis/Shiny_App/campaign.rds")



ui1 <- dashboardPage(
  
    dashboardHeader(title = "Team 3"),
    dashboardSidebar(
      sidebarMenu(
        #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Introduction", tabName = "about", icon = icon("th")),
        menuItem("Marketing Analyse", tabName = "market", icon = icon("th")),
        menuItem("Recommendation System",tabName="unions",icon=icon("globe")),
        menuItem("Customer Segment",tabName="world",icon=icon("signal"))
        
      )
    ),
    
    dashboardBody( 
      #0 Tab content
      tabItems(
        #Dashboard: First tab content 
        #tabItem(tabName = "dashboard",
        #  fluidRow(
        #    column(12,
                   
        #           box(selectInput("country",label="Select Country",choices=country),width = 12) 
                   
        #    ),
        #    column(12,
        #  box(plotOutput("plot1", height = 250),width = "12") #,
        
       # box(
      #    title = "Controls",
      #    sliderInput("slider", "Number of observations:", 1, 100, 50)
      #  )
      #)
      #    )
    #),
    
    #3 Tab content
    tabItem(tabName = "about",
            h2("Krafty Marketing Analytics Portal",style="text-align:center"),
            br(),
            br(),
            box(width=12,height="450px",
                p(style="font-size:18px","In the era of marketing, its conscientious to precisely pinpoint the immaculate solution to the clients. Our company thrives to integrate plethora of entities to create a marketplace to provide marketing strategy-based solution to our customers. An aggregator in the marketplace of product marketing is currently not pioneered and to fill this void we want to deploy application to employ product marketing strategies for customers.
Our main goal is to show how personalization will help us achieve higher ROI rate."),
                
                
                p(style="font-size:18px","To Improve cart value ( Indirectly to increase our profit)"),
                p(style="font-size:18px","To improve engagement and delight (Customer Retention)"),
                p(style="font-size:18px","To improve the market response by providing customer specific recommended products during checkout"),
                p(style="font-size:18px"," If we create a more compelling service by offering better personalized recommendations, we induce members who were on the fence to stay longer, and improve retention. The more information we have, the easier it will be to tailor your follow-up strategies.")
            )
                ),
            #,fluidRow(                                    
            #  column(12,
            #         p("For a detailed description of this project, please visit my", 
            #           a("Website.", href="http://127.0.0.1:6059", target="_blank"),
            #           "For the full code, please visit my", 
            #           a("GitHub page", href = "https://github.com/itsxshi", target="_blank"))
            #  )
            #)
            #    ),
    
    #Dashboard: First tab content 
      tabItem(tabName = "market",
              h3("Time series of Marketing Analysis",align="center") ,
              fluidRow(
                box(plotOutput("plotm1", height = "400px",width = 12)),
                
                box(plotOutput("plotm3"))
            )
      ),
    
    
    #5 Tab content
    tabItem(tabName = "unions",
            h3("Recommendation System",align="center") ,
            column(6, 
                   # INPUT
                   h3("Select Items and Complete Transaction for Your Suggestions"),    
                   wellPanel(
                     selectInput("input_item1", "Item #1", choices = c("", item_list)),
                     selectInput("input_item2", "Item #2", choices = c("", item_list)),
                     selectInput("input_item3", "Item #3", choices = c("", item_list)),
                     selectInput("input_item4", "Item #4", choices = c("", item_list)),
                     selectInput("input_item5", "Item #5", choices = c("", item_list)),
                     actionButton("submit", "Complete Your Purchase")
                   )
            ),column(6,
                     h3("Other Items you Might Be Interested in"),     
                     tableOutput("item_recom")
            )
    ),
    
    #4 Tab content
    tabItem(tabName = "world",
            fluidRow(
            box(width=12,height="400px",plotOutput("plot")),
            box(plotOutput("plotimage"))
            #box(img(src = "www.rstudio.com", width = "100px", height = "100px"))
            #box(img(src="/Users/X Ins/Resource/R/Market_Basket_Analysis/Shiny_App/www/clustering.png", height = 350, width = 350))
            #fileInput("myFile", "Choose a file", accept = c('image/png', 'image/jpeg'))
            )
    )
    
    
   
    #
      )
)
)


past_orders_matrix = readRDS("past_orders_matrix.rds")

new_data = readRDS("campaign.rds")


server <- function(input, output) {
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


shinyApp(ui1, server)