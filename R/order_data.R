#' order_data
#'
#' @description \code{order_data} arrange your data
#'
#' @param data your data
#' @param as_factor Set to FALSE if you don't need factors (for complexheatmap)
#' @param var_to_order Character. Variable to order. By default is "Model_type"
#'
#' @importFrom dplyr arrange mutate filter
#'


order_data = function(data, as_factor = FALSE, var_to_order = "Model_type"){
  
  if(is.null(var_to_order)) return(data)
  if(var_to_order == "") return(data)
  if(length(var_to_order) > 1){
    message("Too many variable for the arrange function in the heatmap.")
    return(data)
  }
  data = data %>% dplyr::arrange(dplyr::all_of(var_to_order))

  prod = unique(na.omit(dplyr::filter(data, !if_any("Product_Family", ~grepl("CTRL",.)))$Product_Family))
  level_order = c("CTRL", "CTRL+",prod)
  
  temp = data %>% dplyr::mutate(Product_Family =  factor(Product_Family, levels = level_order)) %>%
    dplyr::arrange(Product_Family)
  level_order2 = unique(temp$Product)
  
  data = data %>% dplyr::mutate(Product = factor(Product, levels = level_order2)) %>%
    dplyr::arrange(Product) 
  

  if(as_factor == FALSE){
    data = data %>% dplyr::mutate(Product =  as.character(Product))
  }

  if(!is.null(data)){
    return(data)
  }else{
    stop("Something wrong with the ordering data process.")
  }

}
