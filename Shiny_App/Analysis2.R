# Loading the Packages

# Importing libraries
library(data.table)
library(tidyverse)            
library(knitr)
library(recommenderlab)


# loading cleaned data from part1
retail <- readRDS("retail.rds")


# duplicate items within same order
retail %>% 
  filter(InvoiceNo == 557886 & StockCode == 22436) %>% 
  select(InvoiceNo, StockCode, Quantity, UnitPrice, CustomerID)

# Removing duplicates 
retail <- retail %>%  # create unique identifier
  mutate(InNo_Desc = paste(InvoiceNo, Description, sep = ' ')) 
retail <- retail[!duplicated(retail$InNo_Desc), ] %>% 
  select(-InNo_Desc) 


# Create the rating matrix 
ratings_matrix <- retail %>%
  select(InvoiceNo, Description) %>% 
  mutate(value = 1) %>%
  spread(Description, value, fill = 0) %>%
  select(-InvoiceNo) %>%
  as.matrix() %>%
  as("binaryRatingMatrix")



# Create evaluation scheme
scheme <- ratings_matrix %>% 
  evaluationScheme(method = "cross",
                   k      = 5, 
                   train  = 0.8,
                   given  = -1)


# Set up List of Algorithms
algorithms <- list(
  "association rules" = list(name  = "AR", param = list(supp = 0.01, conf = 0.01)),
  "random items"      = list(name  = "RANDOM",  param = NULL),
  "popular items"     = list(name  = "POPULAR", param = NULL),
  "item-based CF"     = list(name  = "IBCF", param = list(k = 5)),
  "user-based CF"     = list(name  = "UBCF", param = list(method = "Cosine", nn = 500))
)


# Estimate the Models
results <- recommenderlab::evaluate(scheme, algorithms,  type  = "topNList",  n = c(1, 3, 5, 10, 15, 20))

# Results for each single model can be easily retrieved and inspected. 
results$'popular' %>% 
  getConfusionMatrix() 

# Sort out results
tmp <- results$`user-based CF` %>% 
  getConfusionMatrix()  %>%  
  as.list() 

as.data.frame( Reduce("+",tmp) / length(tmp)) %>% # average value of 5 cross-validation rounds
  mutate(n = c(1, 3, 5, 10, 15, 20)) %>% # Add a column for number of recommendations calculated
  select('n', 'precision', 'recall', 'TPR', 'FPR') 


avg_conf_matr <- function(results) {
  tmp <- results %>%
    getConfusionMatrix()  %>%  
    as.list() 
  as.data.frame( Reduce("+",tmp) / length(tmp)) %>% 
    mutate(n = c(1, 3, 5, 10, 15, 20)) %>%
    select('n', 'precision', 'recall', 'TPR', 'FPR') 
}


# use map() to get all results in a tidy format

results_tbl <- results %>%
  map(avg_conf_matr) %>% # iterate function across all models
  enframe() %>% 
  unnest() 

# ROC curve
results_tbl %>%
  ggplot(aes(FPR, TPR, colour = fct_reorder2(as.factor(name), FPR, TPR))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "ROC curves",
       colour = "Model") +
  theme_grey(base_size = 14)

# Precision-Recall curve
results_tbl %>%
  ggplot(aes(recall, precision, 
             colour = fct_reorder2(as.factor(name),  precision, recall))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "Precision-Recall curves",
       colour = "Model") +
  theme_grey(base_size = 14)

## Predictions for a new user

# create a made-up order with a string containing 6 products selected at random.
customer_order <- c("GREEN REGENCY TEACUP AND SAUCER",
                    "SET OF 3 BUTTERFLY COOKIE CUTTERS",
                    "JAM MAKING SET WITH JARS",
                    "SET OF TEA COFFEE SUGAR TINS PANTRY",
                    "SET OF 4 PANTRY JELLY MOULDS")


# put string in a format that recommenderlab accepts.
new_order_rat_matrx <- retail %>% 
  select(Description) %>% # Select item descriptions from retail dataset
  unique() %>% 
  mutate(value = as.numeric(Description %in% customer_order)) %>% # Add a 'value' column
  spread(key = Description, value = value) %>% # Spread into sparse matrix format
  as.matrix() %>% # Change to a matrix
  as("binaryRatingMatrix") # Convert to recommenderlab class 'binaryRatingsMatrix'


# create a `Recommender`
recomm <- Recommender(getData(scheme, 'train'), 
                      method = "IBCF",   
                      param = list(k = 5))


# pass the `Recommender` and the made-up order to the `predict` function to create 
# a top 10 recommendation list for the new customer.
pred <- predict(recomm, 
                newdata = new_order_rat_matrx, 
                n = 10)


# inspect pediction as a list
as(pred, 'list')

