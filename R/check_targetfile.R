#' check_targetfile
#'
#' @description \code{check_targetfile} Run a check on the target file. Returns a target file without the faulty experiments.
#'
#' @param target The target file (dataframe).
#' @param explist The experiment list file.
#' @param check_back Boolean. Set to TRUE if you want to check the existence of the BACKGROUND. Default to TRUE (cyto and seap). Set to FALSE for D1 and TREM2.
#' @param check_ctrl Boolean. Set to TRUE if you want to check the existence of the CTRLs. Default to TRUE. Set to FALSE only for target calibration file (SEAP).
#' 
#'
#' @importFrom dplyr filter
#' @importFrom shiny isRunning showNotification
#' @importFrom htmltools HTML
#'


check_targetfile = function(target, explist,check_back = TRUE, check_ctrl = TRUE){
  
  
  #filter target based on exp_list
  target = target %>% dplyr::filter(Experiment_id %in% explist$Experiment_id)
  
  to_rem = NULL
  for(i in unique(target$Experiment_id)){
    expid = target %>% dplyr::filter(Experiment_id == i)
    
    
    if("Support_type" %in% colnames(expid)){
      #check id with support type
      num_supp = unique(expid$Support_type)
      if(length(num_supp) > 1){
        print(paste0("There are more than one Support type (",num_supp, ") for the ", i, " and will be removed. Check the target file."))
        if(shiny::isRunning()){
        showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                               HTML("There are more than one Support type (",num_supp, ") for the", i, "and will be removed. Check the target file.")), type = "warning")
        }
        to_rem = c(to_rem, i)
      }
    }
    
    
    #check well numbers in the plate
    if(length(expid$Well) != 96){
      print(paste0("For ", i, " there are ",length(expid$Well)," while they should be 96. This experiment will be removed. Check the target file."))
      if(shiny::isRunning()){
        showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                               HTML("For ", i, " there are ",length(expid$Well)," while they should be 96. This experiment will be removed. Check the target file.")), type = "warning")
      }
      to_rem = c(to_rem, i)
    }
    
    #check well replicates
    if(length(unique(expid$Well)) != length(expid$Well)){
      dup_wells = expid[duplicated(expid$Well),]$Well
      print(paste0("For ", i, " there are some duplicated wells (",dup_wells,"). This experiment will be removed. Check the target file."))
      if(shiny::isRunning()){
        showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                             HTML("For ", i, " there are some duplicated wells (",dup_wells,"). This experiment will be removed. Check the target file.")), type = "warning")
      }
      to_rem = c(to_rem, i)
    }
    
    
    #check presence of BACKGROUND
    if(check_back == TRUE){
      n_back = nrow(dplyr::filter(expid, Product == "BACKGROUND"))
      if(n_back == 1){
        print(paste0("For ", i, " there is only one BACKGROUND instead of two. Check the target file."))
        if(shiny::isRunning()){
          showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                                 HTML("For ", i, " there is only one BACKGROUND instead of two. Check the target file.")), type = "warning")
        }
      }
      if(n_back == 0){
        print(paste0("For ", i, " there isn't any BACKGROUND. This experiment will be removed. Check the target file."))
        if(shiny::isRunning()){
          showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                                 HTML("For ", i, " there isn't any BACKGROUND. This experiment will be removed. Check the target file.")), type = "warning")
        }
        to_rem = c(to_rem, i)
      }
    }

    
    
    
    #check presence of CTRL
    if(check_ctrl == TRUE){
      n_ctrl = nrow(dplyr::filter(expid, Product == "CTRL"))
      if(n_ctrl == 1){
        print(paste0("For ", i, " there is only one CTRL instead of two. Check the target file."))
        if(shiny::isRunning()){
          showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                                 HTML("For ", i, " there is only one CTRL instead of two. Check the target file.")), type = "warning")
        }
      }
      if(n_ctrl == 0){
        print(paste0("For ", i, " there isn't any CTRL. This experiment will be removed. Check the target file."))
        if(shiny::isRunning()){
          showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                                 HTML("For ", i, " there isn't any CTRL. This experiment will be removed. Check the target file.")), type = "warning")
        }
        to_rem = c(to_rem, i)
      }
    }
    
    
    
  }
  
  if(is.null(to_rem)){
    message("Check target file: OK!")
    if(shiny::isRunning()){
      showNotification(tagList(icon("check"), HTML("&nbsp;Target file loaded.")), type = "message")
      }
    return(target)
  }else{
    message("Check target file: something removed!")
    if(shiny::isRunning()){
      showNotification(tagList(icon("check"), HTML("&nbsp;Target file loaded. Some experiments are removed.")), type = "message")
      }
     return(dplyr::filter(target, !(Experiment_id %in% unique(to_rem))))
  }
}
