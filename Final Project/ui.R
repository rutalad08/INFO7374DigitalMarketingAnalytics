library(shiny)
library(shinydashboard)
library(tidyverse)
library(Matrix)
library(ggplot2) # data visualization

item_list <- readRDS("item_list.rds")
new_data <- readRDS("campaign.rds")



ui <- dashboardPage(
  
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
      
      ,fluidRow(                                    
        column(12,
               p("For a detailed description of this project, please visit my", 
                 a("Website.", href="https://public.tableau.com/profile/anisha.ganguly#!/vizhome/KraftyAnalysisProject/SalesDashboard", target="_blank"),
                 "For the full code, please visit my", 
                 a("GitHub page", href = "https://github.com/itsxshi", target="_blank"))
        )
      )
          ),
      
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
