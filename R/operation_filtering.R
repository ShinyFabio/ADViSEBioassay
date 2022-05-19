#' operation_filtering
#'
#' @description \code{operation_filtering} Function for the query
#'
#' @param data The summarized data
#' @param column Character. The column where to apply the filtering (e.g. "Cytotoxicity.average")
#' @param operator Character. The operator to be used. A choice beetween "max", "min", "greater than", "greater than or equal", "equal", "less than or equal" or "less than".
#' @param value Numeric. Value used to compare the data.
#' 
#' @importFrom dplyr filter mutate across select
#'


operation_filtering = function(data, column, operator, value){
  
  if(operator == "max"){
    data = data %>% dplyr::filter(get(column) == max(get(column), na.rm = TRUE))
  }else if(operator == "min"){
    data = data %>% dplyr::filter(get(column) == min(get(column), na.rm = TRUE))
  }else if(operator == "greater than"){
    data = data %>% dplyr::filter(get(column) > value)
  }else if(operator == "greater than or equal"){
    data = data %>% dplyr::filter(get(column) >= value)
  }else if(operator == "less than or equal"){
    data = data %>% dplyr::filter(get(column) <= value)
  }else if(operator == "less than"){
    data = data %>% dplyr::filter(get(column) < value)
  }else if(operator == "equal"){
    data = data %>% dplyr::mutate(dplyr::across(where(is.double), round)) %>% dplyr::filter(get(column) == value)
  }else{
    return(NULL)
  }
  
  data = data %>% dplyr::select(Model_type, Product_Family) %>% unique()
  return(data)
  
  
}
