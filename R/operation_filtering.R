#' operation_filtering
#'
#' @description \code{operation_filtering} Function for the query
#'
#' @param data The summarized data
#' @param column Character. The column where to apply the filtering (e.g. "Cytotoxicity.average")
#' @param operator Character. The operator to be used. A choice beetween "max", "min", "greater than", "greater than or equal", "equal", "less than or equal" or "less than".
#' @param value Numeric. Value used to compare the data.
#' @param n_query Character. Number of queries. Can be "one" or "two".
#' @param column2 Character. The column for the second query where to apply the filtering (e.g. "Cytotoxicity.average")
#' @param operator2 Character. The operator to be used for the second query.
#' @param value2 Numeric. Value used to compare the data for the second query.
#' @param type_output Character. The type of the output. Can be "summ" or "raw".
#' 
#' @importFrom dplyr filter mutate across select
#'


operation_filtering = function(data, column, operator, value, 
                               n_query = "one", column2 = NULL, operator2 = NULL, value2 = NULL,
                               type_output
                               ){
  
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
    digits = ifelse(grepl("average", column), 0, 1)
    data = data %>% dplyr::mutate(dplyr::across(where(is.double), ~round(.x,digits = digits))) %>% dplyr::filter(get(column) == value)
  }else{
    return(NULL)
  }
  
  
  #second query
  if(n_query == "two"){
    if(operator2 == "max"){
      data = data %>% dplyr::filter(get(column2) == max(get(column2), na.rm = TRUE))
    }else if(operator2 == "min"){
      data = data %>% dplyr::filter(get(column2) == min(get(column2), na.rm = TRUE))
    }else if(operator2 == "greater than"){
      data = data %>% dplyr::filter(get(column2) > value2)
    }else if(operator2 == "greater than or equal"){
      data = data %>% dplyr::filter(get(column2) >= value2)
    }else if(operator2 == "less than or equal"){
      data = data %>% dplyr::filter(get(column2) <= value2)
    }else if(operator2 == "less than"){
      data = data %>% dplyr::filter(get(column2) < value2)
    }else if(operator2 == "equal"){
      digits2 = ifelse(grepl("average", column2), 0, 1)
      data = data %>% dplyr::mutate(dplyr::across(where(is.double), ~round(.x,digits = digits2))) %>% dplyr::filter(get(column2) == value2)
    }else{
      return(NULL)
    }
    
    
     
  }
  
  if(type_output == "summ"){
    data = data %>% group_by(Model_type, Product_Family) %>% dplyr::summarise(n_products = n())
  }
  
  return(data)


}
