

#' get_model : A function to generate linear model 
#' 
#' @param sales_data A data frame,must contain the columns Category,Discount,Price and Sales
#' @param y A String.Must be  category name given in the data
#' @return coefficients and adjusted r-square of the linear model generated  
#' for the input category.

get_model <- function(sales_data,cat){
  cat_data <- sales_data %>% .[Category == cat]
  model <- lm(Sales~ Price+Discount,cat_data)
  cfnt1 = round(model$coefficients[1])
  cfnt2 = round(model$coefficients[2])
  cfnt3 = round(model$coefficients[3])
  rsqr = round(summary(model)$adj.r.squared *100)
  return(list(cfnt1,cfnt2,cfnt3, rsqr))
}


