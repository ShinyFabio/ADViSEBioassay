#' enriched_fractions
#'
#' @description \code{enriched_fractions} Function for a query in Reporter.
#'
#' @param data_repo The reporter dataframe (data_reporter())
#' @param repo_type Character. The type of the reporter data ("SEAP" or "TREM2".
#' @param model_type Character. A model type that will be used to filter the SEAP data.
#' @param prod_trem Dataframe. The output from 4 or 5 query of TREM2.
#' 
#' @importFrom dplyr mutate filter if_any
#' @importFrom stringr str_split str_replace
#'


productive_fractions = function(data_reporter, model_type, times_ctrl = 2.5){
  
  message("Searching for fractions with concentration greater than ",times_ctrl," times CTRL.")
  if(shiny::isRunning()){
    showNotification(tagList(icon("gears"), HTML("&nbsp;Searching for fractions with concentration greater than ",times_ctrl, "times CTRL...")), type = "default")
  }
  data_repo = data_reporter %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.)))
  
  temp = lapply(model_type, function(modt){
    message("Searching inside ", modt, " ...")
    if(shiny::isRunning()){
      showNotification(tagList(icon("gears"), HTML("&nbsp;Searching inside ", modt, " ...")), type = "default")
    }
    cnt_seap <- data_reporter %>% dplyr::filter(Product_Family == "CTRL") %>% dplyr::filter(Model_type == modt)
    data_repo2 <- data_repo %>% dplyr::filter(Model_type == modt)
    
    lapply(unique(data_repo2$Product_Family), function(x){
      data = dplyr::filter(data_repo2, Product_Family == x)
      if(length(unique(data$Purification)) >1){
        #if there are multiple purification, we have to check for each purification
        lapply(unique(data$Purification), function(k){
          data2 = data %>% dplyr::filter(Purification == k)
          cnt2 = cnt_seap %>% dplyr::filter(Experiment_id %in% unique(data2$Experiment_id)) %>% as.data.frame()
          #if(selop_query_d1 == "greater than"){
          #  data2 %>% dplyr::filter(Concentration.average > mean(cnt2[,"Concentration.average"])*2.5)
          #}else{
          data2 %>% dplyr::filter(Concentration.average >= mean(cnt2[,"Concentration.average"])*times_ctrl)
          #}
        }) %>% {Reduce(rbind, .)} #dato che %>% assegna come primo posto, uso {} e metto il . per la posizione.
        
      }else{
        cnt = cnt_seap %>% dplyr::filter(Experiment_id %in% unique(data$Experiment_id)) %>% as.data.frame()
        #if(selop_query_d1 == "greater than"){
        #  data %>% dplyr::filter(Concentration.average > mean(cnt[,"Concentration.average"])*2.5)
        # }else{
        data %>% dplyr::filter(Concentration.average >= mean(cnt[,"Concentration.average"])*times_ctrl)
        # }
      }
    }) %>% {Reduce(rbind, .)}
    
  }) %>% {Reduce(rbind, .)}
  
  message("Completed! Found ", length(temp$Product), " fractions.")
  if(shiny::isRunning()){
    showNotification(tagList(icon("check"), HTML("&nbsp;Completed! Found ", length(unique(temp$Product)), " different fractions.")), type = "message")
  }
  temp
}







enriched_fractions = function(prod_trem, #output di productive_fractions o del primo filtraggio di trem2
                              data_reporter,
                              repo_type #SEAP or TREM2
                              ){ 
  
  message("Searching for enriched fractions.")
  if(shiny::isRunning()){
    showNotification(tagList(icon("gears"), HTML("&nbsp;Searching for enriched fractions.")), type = "default")
  }
  
  data_repo = data_reporter %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.)))
  

  prod_trem = prod_trem %>% dplyr::mutate(Extract = stringr::str_replace(stringr::str_replace(Product, Product_Family, ""), "_","")) %>% 
    dplyr::mutate(Extract = stringr::str_replace(Extract,pattern="^$",replacement="EXT")) %>% 
    dplyr::filter(Extract != "EXT")
  
  
  temp2 = lapply(unique(prod_trem$Product_Family), function(x){
    message("Checking ", x,"...")
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
  
  message("Completed! Found ", length(temp2$Product), " fractions.")
  if(shiny::isRunning()){
    showNotification(tagList(icon("check"), HTML("&nbsp;Completed! Found ", length(temp2$Product), " enriched fractions.")), type = "message")
  }
  
  
  return(temp2)
  
}
