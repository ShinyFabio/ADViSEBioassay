#' make_check_table
#'
#' @description \code{make_check_table} create a table with the intersection of Product_Family with yes or no
#'
#' @param dt A list containing all database.
#' 
#' @importFrom dplyr mutate filter across case_when
#' @importFrom tibble column_to_rownames rownames_to_column
#'


make_check_table = function(dt){
  
  aux_fun <- function(a,b) {merge(a, b, by = "Product_Family", all.x = TRUE, all.y = TRUE)}
  
  create_dfcheck = function(data_from_int, database_names = database_names){
    result = list()
    for(i in names(data_from_int)){
      name = i %>% strsplit(split = "_") %>% unlist()
      name = database_names[database_names %in% name]
      other = database_names[!database_names %in% name]
      
      full2 = data.frame(Product_Family = data_from_int[[i]])
      data2 = list()
      for(z in name){
        data = data.frame(prod = data_from_int[[i]], c = "yes")
        colnames(data) = c("Product_Family", z)
        data2[[z]] = data
      }
      if(length(other) == 1){
        result[[i]] = base::Reduce(aux_fun, data2) %>% cbind(e = NA) %>% dplyr::rename(!!other := e)
      }else{
        result[[i]] = base::Reduce(aux_fun, data2) %>% cbind(e = NA, f = NA) %>% dplyr::rename(!!other[1] := e, !!other[2] := f)
      }
    }
    return(result)
  }
  
  
  ###### quattro
  Result_4 = Reduce(intersect, dt)
  database_names = c("cyto", "d1", "trem2", "seap")
  
  full_4 = data.frame(prod = Result_4, cyto = Result_4, d1 = Result_4, trem2 = Result_4, seap = Result_4) %>% 
    tibble::column_to_rownames("prod") %>% dplyr::mutate(across(.cols = everything(), ~"yes")) %>% 
    tibble::rownames_to_column("Product_Family")
  
  
  
  ###### tre
  comb3 = combn(names(dt),3)
  
  Result_3 <- apply(comb3,2,function(x){
    Reduce(intersect, list(dt[[x[1]]],dt[[x[2]]], dt[[x[3]]]))
  })
  names(Result_3) = apply(comb3,2, function(x)paste0(x, collapse = "_"))
  

  full_3 = create_dfcheck(data = Result_3, database_names = database_names)
  
  #removing where are present in all 4
  full_3 = lapply(full_3, function(x){
    x %>% dplyr::filter(!Product_Family %in% full_4$Product_Family)
  }) %>% Reduce(rbind, .)
  
  
  
  
  ###### due
  
  comb2 = combn(names(dt),2)
  
  Result_2 <- apply(comb2,2,function(x){
    intersect(dt[[x[1]]],dt[[x[2]]])
  })
  names(Result_2) = apply(comb2,2, function(x)paste0(x, collapse = "_"))
  
  full_2 = create_dfcheck(data = Result_2, database_names = database_names)
  
  #removing where are present in all 4 and in all 3
  full_2 = lapply(full_2, function(x){
    x %>% dplyr::filter(!Product_Family %in% full_4$Product_Family) %>% dplyr::filter(!Product_Family %in% full_3$Product_Family)
  }) %>% Reduce(rbind, .)
  
  
  
  
  ####### merge all results
  full_all = list(full_4,full_3,full_2) %>% Reduce(rbind, .) %>% 
    dplyr::mutate(dplyr::across(2:5, ~dplyr::case_when(. == "yes" ~ "yes", is.na(.) ~ "no")))
  colnames(full_all) = c("Product_Family", "Cytotoxicity", "D1", "TREM2", "SEAP")
  
  return(full_all)
}  
