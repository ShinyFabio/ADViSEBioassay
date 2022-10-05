#' enriched_fractions
#'
#' @description \code{enriched_fractions} Function for a query in Reporter.
#'
#' @param data_reporter The reporter dataframe (data_reporter())
#' @param repo_type Character. The type of the reporter data ("SEAP" or "TREM2".
#' @param prod_trem Dataframe. The output from 4 or 5 query of TREM2.
#' 
#' @importFrom dplyr mutate filter if_any
#' @importFrom stringr str_split str_replace
#'

########### ONLY FOR SEAP 



enriched_fractions = function(prod_trem, #output di productive_fractions o del primo filtraggio di trem2
                              data_reporter,
                              repo_type #SEAP or TREM2
                              ){ 
  
  message("Searching for enriched fractions...")
  if(shiny::isRunning()){
    showNotification(tagList(icon("gears"), HTML("&nbsp;Searching for enriched fractions...")), type = "default")
  }
  
  data_repo = data_reporter %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.)))
  

  prod_trem = prod_trem %>% dplyr::mutate(Extract = stringr::str_replace(stringr::str_replace(Product, Product_Family, ""), "_","")) %>% 
    dplyr::mutate(Extract = stringr::str_replace(Extract,pattern="^$",replacement="EXT")) %>% 
    dplyr::filter(Extract != "EXT")
  
  
  #cl <- parallel::makeCluster(parallel::detectCores(logical = T)-1)
  #parallel::clusterEvalQ(cl,{library(dplyr)
  #  library(stringr)})
  #parallel::clusterExport(cl, c("prod_trem", "data_repo", "repo_type", "filter","mutate","str_replace"),envir=environment())
  
  
  #temp2 = parallel::parLapply(cl, unique(prod_trem$Product_Family), function(x){
  temp2 = lapply(unique(prod_trem$Product_Family), function(x){
    data = dplyr::filter(prod_trem, Product_Family == x)
    ext = dplyr::filter(data_repo, Product_Family == x & Experiment_id %in% unique(data$Experiment_id) & Dose %in% unique(data$Dose)) %>% #anzicheè datarepo2
      dplyr::mutate(Extract = stringr::str_replace(stringr::str_replace(Product, Product_Family, ""), "_","")) %>% 
      dplyr::mutate(Extract = stringr::str_replace(Extract, pattern="^$", replacement="EXT")) %>% 
      dplyr::filter(Extract == "EXT")
    
    
    if(repo_type == "SEAP"){
      lapply(unique(ext$Model_type), function(g){
        ext_filt = ext %>% dplyr::filter(Model_type == g)
        data_fn = data %>% dplyr::filter(Model_type == g)
        
        lapply(unique(data_fn$Dose), function(dos){
          ext_filt = ext_filt %>% dplyr::filter(Dose == dos)
          data_fn %>% dplyr::filter(Dose == dos) %>%
            dplyr::mutate(Enrichment = Concentration.average - ext_filt$Concentration.average) %>% 
            dplyr::filter(Enrichment > 0)
        }) %>% Reduce(rbind, .)
      }) %>% Reduce(rbind, .)
    }else{
      lapply(unique(data$Dose), function(dos){
        ext_filt = ext %>% dplyr::filter(Dose == dos)
        data %>% dplyr::filter(Dose == dos) %>%
          dplyr::mutate(Enrichment = GFP.average - ext_filt$GFP.average) %>% 
          dplyr::filter(Enrichment > 0)
      }) %>% Reduce(rbind, .)
    }

  }) %>% Reduce(rbind, .)
  #parallel::stopCluster(cl)
  
  
  if(!is.null(temp2)){
    message("Completed! Found ", length(temp2$Product), " fractions.")
    if(shiny::isRunning()){
      showNotification(tagList(icon("check"), HTML("&nbsp;Completed! Found ", length(temp2$Product), " enriched fractions.")), type = "message")
    }
  }else{
    message("ERROR. Output is null")
    if(shiny::isRunning()){
      showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;ERROR. Output is null")), type = "error")
    }
  }

  return(temp2)
  
}
