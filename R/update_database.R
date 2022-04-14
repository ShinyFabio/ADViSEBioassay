#' update_database
#'
#' @description \code{update_database} Function to update the database.
#'
#' @param old_data List. The internal database data in the form of list(myprocesseddata,mydataset,exp_list).
#' @param new_data List. The new data coming from the mod_update_data module in the form of list(myprocesseddata,mydataset,exp_list).
#'
#' @importFrom dplyr filter
#' @importFrom shiny showNotification isRunning HTML
#' @importFrom shinyWidgets sendSweetAlert
#'

# 
# old_data = database_cyto
# 
# new_data = database_cyto_upd


update_database = function(old_data, new_data = NULL){
  
  if(is.null(new_data)){
    return(old_data)
  }
  
  #check names of lists
  if(!all(names(old_data) %in% names(new_data))){
    message("Something wrong in the new data. Names of the lists are not the same.")
    message(paste0("Old data names:",paste0(names(old_data),collapse=", "),". New data names:",paste0(names(new_data), collapse=", ")))
    if(shiny::isRunning()){
      shinyWidgets::sendSweetAlert(title = "Error!", type = "warning",
                                   text = "Something wrong in the new data. Names of the lists are not the same. Check the console.")
    }
    return(NULL)
  }
  
  data_final = list()
  for(i in names(old_data)){
    message(paste0("Merging ",i,"..."))
    if(isRunning()){
      showNotification(tagList(icon("info"), HTML("&nbsp;Merging ", i,"...")), type = "default")
      }
    
    
    #check number of columns
    if(length(old_data[[i]]) != length(new_data[[i]])){
      if(isRunning()){
        shinyWidgets::sendSweetAlert(title = "Error!", type = "warning",
                                     text = "The new data contains a different number of columns. Please check the columns before updating the database.")
      }
      message("The new data contains a different number of columns. Please check the columns before to update the database.")
      return(NULL)
    }
    
    
    #check same column names
    if(!all(colnames(old_data[[i]]) %in% colnames(new_data[[i]]))){
      message("Some columns in the new data have a different name. Check the column names before update the database.")
      if(isRunning()){
        shinyWidgets::sendSweetAlert(title = "Error!", type = "warning",
                                     text = "Some columns in the new data have a different name. Check the column names before updating the database.")
      }
      return(NULL)
    }
    
    
    #check duplicate experiments
    if(TRUE %in% (unique(new_data[[i]]$Experiment_id) %in% unique(old_data[[i]]$Experiment_id))){
      message("The new data contains one or more Experiments already present in the database. They will be removed.")
      showNotification(tagList(icon("exclamation-circle"), 
                               HTML("The new data contains one or more Experiments already present in the database and they will be removed. 
                                    Check to console to know which experiments.")), type = "warning")
      
      
      id = which(unique(new_data[[i]]$Experiment_id) %in% unique(old_data[[i]]$Experiment_id))
      exp_to_rem = unique(new_data[[i]]$Experiment_id)[id]
      
      message(paste0("Duplicated Experiments removed: ",paste0(exp_to_rem, collapse = ", ")))
      
      new_data[[i]] = new_data[[i]] %>% dplyr::filter(Experiment_id != exp_to_rem)
      
    }
    
    data_final[[i]] = rbind(old_data[[i]], new_data[[i]])
    
  }
  
  if(isRunning()){
    showNotification(tagList(icon("check"), HTML("&nbsp;Merging completed!")), type = "message")
  }
  
  return(data_final)


}

