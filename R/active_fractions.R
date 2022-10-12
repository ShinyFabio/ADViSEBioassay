#' active_fractions
#'
#' @description \code{active_fractions} Search for active fractions.
#'
#' @param data_reporter The reporter dataframe (data_reporter())
#' @param model_type Character. A model type that will be used to filter the data.
#' @param andor Character. If "AND" the function returns only the fractions contained in every model type, otherwhise every fraction. By default "OR".
#' @param variable Character. In which variable search for active fractions. e.g. Concentration.average or GFP.average...
#' @param type_ctrl Character. The control to use for the comparison. One of "BETA_GLUCO_C20_1_S" (trem2) or "MeOH" (seap).
#' @param thresh_ctrl Numeric. Percentage for the threshold of the CTRL/CTRL+. e.g. 50 is half of the CTRL, 200 is 2 times the CTRL.
#' @param final_msg Logical. If FALSE the final message of completed is not sent.
#' 
#' @importFrom dplyr filter if_any
#' @importFrom shiny isRunning showNotification
#'


active_fractions = function(data_reporter, model_type, andor = "OR",variable, type_ctrl, thresh_ctrl, final_msg = TRUE){
  
  
  message("Searching for fractions with ",variable, " greater than ",thresh_ctrl, "% of ",type_ctrl, "...")
  if(shiny::isRunning()){
    showNotification(tagList(icon("gears"), HTML("&nbsp;Searching for fractions with ", variable," greater than ",thresh_ctrl,"% of ", type_ctrl,"...")), type = "default")
  }
  
  if(!(type_ctrl %in% unique(data_reporter$Product))){
    message("Error. No",type_ctrl,"found in the Product column")
    return(NULL)
  }
  
  data_repo = dplyr::filter(data_reporter, !dplyr::if_any("Product", ~grepl(type_ctrl,.)))
  cnt_seap <- dplyr::filter(data_reporter, Product == type_ctrl)
  

  temp = lapply(model_type, function(modt){
    message("Searching inside ", modt, "...")
    if(shiny::isRunning()){
      showNotification(tagList(icon("gears"), HTML("&nbsp;Searching inside ", modt, "...")), type = "default")
    }
    cnt_seap <- cnt_seap %>% dplyr::filter(Model_type == modt)
    data_repo2 <- data_repo %>% dplyr::filter(Model_type == modt)

    lapply(unique(data_repo2$Product_Family), function(x){
      data = dplyr::filter(data_repo2, Product_Family == x)
      
      if(length(unique(data$Purification)) >1){
        #if there are multiple purification, we have to check for each purification
        lapply(unique(data$Purification), function(k){
          
          data2 = data %>% dplyr::filter(Purification == k)
          lapply(unique(data$Experiment_id), function(eid){
            cnt = cnt_seap %>% dplyr::filter(Experiment_id == eid) %>% as.data.frame()
            data %>% dplyr::filter(Experiment_id == eid) %>% dplyr::filter(get(variable) >= cnt[,variable]*(thresh_ctrl/100))

          }) %>% {do.call(rbind, .)}

        }) %>% {Reduce(rbind, .)} #dato che %>% assegna come primo posto, uso {} e metto il . per la posizione.
        
      }else{
        lapply(unique(data$Experiment_id), function(eid){
          cnt = cnt_seap %>% dplyr::filter(Experiment_id == eid) %>% as.data.frame()
          data %>% dplyr::filter(Experiment_id == eid) %>% dplyr::filter(get(variable) >= cnt[,variable]*(thresh_ctrl/100))

        }) %>% {do.call(rbind, .)} #{Reduce(rbind, .)}
      }
    }) %>% {Reduce(rbind, .)}
  }) %>% {Reduce(rbind, .)}
  
  
  
  #removing fractions not contained in all modeltype selected
  if(andor == "AND"){
    message("Taking only active fractions in ",paste(collapse = ", ",model_type),"...")
    if(shiny::isRunning()){
      showNotification(tagList(icon("gears"), HTML("&nbsp;Taking only active fractions in ", paste(collapse = ", ",model_type), "...")), type = "default")
    }
    
    temp2 <- temp %>% dplyr::mutate(sample = paste0(Product_Family, Product, Dose, Purification))
    products_to_take <- temp2 %>% dplyr::group_by(sample) %>% dplyr::summarize(n=n()) %>% dplyr::filter(n >= length(model_type))
    temp <- temp2 %>% dplyr::filter(sample %in% products_to_take$sample) %>% dplyr::select(-sample)
    
    message((nrow(temp2)-nrow(temp)), " fractions are removed.")
    if(shiny::isRunning()){
      showNotification(tagList(icon("info"), HTML("&nbsp;",(nrow(temp2)-nrow(temp)), " fractions are removed.")), type = "default")
    }
  }
  
  
  if(!is.null(temp)){
    if(final_msg == TRUE){
      message("Completed! Found ", length(temp$Product), " fractions.")
      if(shiny::isRunning()){
        showNotification(tagList(icon("check"), HTML("&nbsp;Completed! Found ", length(unique(temp$Product)), " fractions.")), type = "message")
      }
    }
  }else{
    message("ERROR. Output is null")
    if(shiny::isRunning()){
      showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;ERROR. Output is null")), type = "error")
    }
  }
  return(temp)
  
}


