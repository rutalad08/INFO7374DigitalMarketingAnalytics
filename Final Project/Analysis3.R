### Loading the Packages
library(tidyverse)            
library(knitr)
library(Matrix)
library(recommenderlab)

## Data

# read retail data from part 2
retail <- readRDS("retail.rds")


# past_orders_matrix: containing the history of past orders.  
past_orders_matrix <- retail %>%
  # Select only needed variables
  select(InvoiceNo, Description) %>% 
  # Add a column of 1s
  mutate(value = 1) %>%
  # Spread into user-item format
  spread(Description, value, fill = 0) %>%
  select(-InvoiceNo) %>% 
  # Convert to matrix
  as.matrix() %>% 
  # Convert to class "dgCMatrix"
  as("dgCMatrix")

# I save the file for use in the app
saveRDS(past_orders_matrix, file = "past_orders_matrix.rds")

# item_list:list of all the products available to purchase. 
item_list <- retail %>% 
  select(Description) %>% 
  unique()


saveRDS(item_list, file = "item_list.rds")
#past_orders_matrix <- readRDS("/Users/X Ins/Resource/R/Market_Basket_Analysis/Shiny_App/past_orders_matrix.rds")

## Improved Collaborative Filtering using the same 6 randomply selected products.
customer_order <- c("GREEN REGENCY TEACUP AND SAUCER",
                    "SET OF 3 BUTTERFLY COOKIE CUTTERS",
                    "JAM MAKING SET WITH JARS",
                    "SET OF TEA COFFEE SUGAR TINS PANTRY",
                    "SET OF 4 PANTRY JELLY MOULDS")


new_order <- item_list %>%
  mutate(value = as.numeric(Description %in% customer_order)) %>%
  spread(key = Description, value = value) %>%
  as.matrix() %>% 
  as("dgCMatrix")

all_orders_dgc <- t(rbind(new_order,past_orders_matrix))


# Set range of items to calculate predictions
items_to_predict <- 1:nrow(all_orders_dgc)
users <- c(1)
prediction_indices <- as.matrix(expand.grid(items_to_predict, users = users))


# Load algorithm implementations and similarity calculations
source("cf_algorithm.R")
source("similarity_measures.R")


recomm <- predict_cf(all_orders_dgc, prediction_indices,
                     "ibcf", FALSE, cal_cos, 3, FALSE, 4000, 2000)


# Convert all_orders to class "binaryRatingMatrix"
all_orders_brm <- as(all_orders, "binaryRatingMatrix")

recomm <- Recommender(all_orders_brm, 
                      method = "IBCF",  
                      param = list(k = 5))

