# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  navbarPage("Project Dashboard",
             id = "proj",
             
             tabPanel('Transaction',column(6, 
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
             ,fluidRow(                                    
               column(12,
                      p("For a detailed description of this project, please visit my", 
                        a("Website.", href="http://127.0.0.1:6059", target="_blank"),
                        "For the full code, please visit my", 
                        a("GitHub page", href = "https://github.com/itsxshi", target="_blank"))
               )
             )),
             tabPanel('Graph',
                      plotOutput("plot")
                      
             )))

past_orders_matrix = readRDS("past_orders_matrix.rds")


# Define server logic for random distribution app ----
server <- function(input, output) {
  output$plot <- renderPlot({
    # plot(x=x(), y=y(), main = "iris dataset plot", xlab = xl(), ylab = yl())
    #ggplot(dataPokemon,aes(x=Attack,y= Defense))+geom_point(color='Red') # ploting through GGPLOT
    ggplot(new_data, aes(Recency_group, Frequency_group)) +
      geom_count() +
      facet_grid(mon_value_group ~ .) +
      labs(x="Recency", y="Frequency", title="RFM analysis since 2016")  
  })
  #dataPokemon
  
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


# Create Shiny app ----
shinyApp(ui, server)
