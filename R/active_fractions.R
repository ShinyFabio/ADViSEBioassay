#' active_fractions
#'
#' @description \code{active_fractions} Search for active fractions.
#'
#' @param data_reporter The reporter dataframe (data_reporter())
#' @param model_type Character. A model type that will be used to filter the data.
#' @param variable Character. In which variable search for active fractions. e.g. Concentration.average or GFP.average...
#' @param type_ctrl Character. The control to use for the comparison. One of "CTRL" or "CTRL+".
#' @param thresh_ctrl Numeric. Percentage for the threshold of the CTRL/CTRL+. e.g. 50 is half of the CTRL, 200 is 2 times the CTRL.
#' @param final_msg Logical. If FALSE the final message of completed is not sent.
#' 
#' @importFrom dplyr filter if_any
#' @importFrom shiny isRunning showNotification
#'


active_fractions = function(data_reporter, model_type, variable, type_ctrl, thresh_ctrl, final_msg = TRUE){
  
  
  message("Searching for fractions with ",variable, " greater than ",thresh_ctrl, "% of ",type_ctrl, "...")
  if(shiny::isRunning()){
    showNotification(tagList(icon("gears"), HTML("&nbsp;Searching for fractions with ", variable," greater than ",thresh_ctrl,"% of ", type_ctrl,"...")), type = "default")
  }
  
  if(!(type_ctrl %in% unique(data_reporter$Product_Family))){
    message("Error. No",type_ctrl,"found in the Product_Family column")
    return(NULL)
  }
  
  data_repo = dplyr::filter(data_reporter, !dplyr::if_any("Product_Family", ~grepl(type_ctrl,.)))
  cnt_seap <- dplyr::filter(data_reporter, Product_Family == type_ctrl)
  

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
          cnt2 = cnt_seap %>% dplyr::filter(Experiment_id %in% unique(data2$Experiment_id)) %>% as.data.frame()
          data2 %>% dplyr::filter(get(variable) >= mean(cnt2[,variable],na.rm = T)*(thresh_ctrl/100))
        }) %>% {Reduce(rbind, .)} #dato che %>% assegna come primo posto, uso {} e metto il . per la posizione.
        
      }else{
        cnt = cnt_seap %>% dplyr::filter(Experiment_id %in% unique(data$Experiment_id)) %>% as.data.frame()
        data %>% dplyr::filter(get(variable) >= mean(cnt[,variable],na.rm = T)*(thresh_ctrl/100))
      }
    }) %>% {Reduce(rbind, .)}
  }) %>% {Reduce(rbind, .)}
  
  
  
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


