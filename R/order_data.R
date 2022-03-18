#' order_data
#'
#' @description \code{order_data} arrange your data
#'
#' @param data your data
#' @param as_factor Set to FALSE if you don't need factors (for complexheatmap)
#'
#' @importFrom dplyr arrange mutate filter
#'


order_data = function(data, as_factor = FALSE){
  

  data = data %>% dplyr::arrange(Model_type)

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
