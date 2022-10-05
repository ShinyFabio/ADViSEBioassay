#' read_TREM2
#'
#' @description \code{read_TREM2} Read D1 data
#'
#' @param file_explist The row of the experiment list of that sample (i.e. the experiment list filtered by EXPID)
#' @param file_target The row of the target file of that sample (i.e. the target file filtered by EXPID)
#' @param filter.na A column where the NA filtering is performed. By default is Product.
#'
#' @importFrom dplyr group_by summarise filter n left_join
#' @importFrom readxl read_excel
#' @importFrom janitor remove_empty
#' @importFrom readr type_convert
#' @importFrom stringr str_replace str_split_fixed
#' @importFrom htmltools HTML
#' @importFrom shiny isRunning showNotification
#' @importFrom tidyr pivot_wider
#' @importFrom tibble add_column
#'



read_TREM2 = function(file_explist, file_target, filter.na = "Product"){
  ifile = file_explist$File
  
  #### "BD_ACCURI_C6" ####
  if(file_explist$Instrument == "BD_ACCURI_C6"){
    
    mydata = readxl::read_excel(paste(file_explist$Path,ifile, sep = "\\"), skip = 1)[-1,] %>% janitor::remove_empty(which = c("rows", "cols")) %>% 
      as.data.frame() %>% readr::type_convert(col_types = "cdd")
    
    
    if(!all(c(file_explist$Section_GFP, file_explist$Section_Vitality) %in% colnames(mydata))){
      message(paste("There is a mismatch between Paths in",ifile, "and Sections columns in the Experiment list"))
      if(shiny::isRunning()){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;There is a mismatch between Paths in",ifile, "and Sections columns in the Experiment list")), type = "error")
      }
      return(NULL)
    }
    
    
    coln = colnames(mydata) %>% stringr::str_replace("\\...1", "Well") %>% stringr::str_replace(file_explist$Section_Vitality, "Vitality") %>% 
      stringr::str_replace(file_explist$Section_GFP, "GFP")
    colnames(mydata) = coln
    
    mydata$Well <- stringr::str_split_fixed(mydata$Well, pattern = " ", n = 2)[,2]


    if(length(unique(mydata$Well)) != length(mydata$Well)){
      message(paste0("In ",ifile," there is a duplicated well. Check the file."))
      if(shiny::isRunning()){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;In ",ifile," there is a duplicated well. Check the file.")), type = "error")
      }
      return(NULL)
    }
    
    
    #### "MACS_QUANT_16" ####
  }else if(file_explist$Instrument == "MACS_QUANT_16"){
    mydata = readxl::read_excel(paste(file_explist$Path,ifile, sep = "\\")) %>% janitor::remove_empty(which = c("rows", "cols")) %>% 
      as.data.frame()

    #check duplicates
    checkdup = mydata %>% dplyr::group_by(WID, Path) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::filter(n > 1L)
    
    if(nrow(checkdup) > 0){
      message(paste0("In ",ifile," there is one or more duplicated wells (", paste0(unique(checkdup$WID), collapse = ","),"). Check the file."))
      if(shiny::isRunning()){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;In ",ifile," there is one or more duplicated wells (",
                                                            paste0(unique(checkdup$WID), collapse = ","), "). Check the file.")), type = "error")
      }
      return(NULL)
    }
    
    
    mydata = mydata %>% tidyr::pivot_wider(names_from = Path, values_from = "%-#")
    
    
    if(!all(c(file_explist$Section_GFP, file_explist$Section_Vitality) %in% colnames(mydata))){
      message(paste("There is a mismatch between Paths in",ifile, "and Sections columns in the Experiment list"))
      if(shiny::isRunning()){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;There is a mismatch between Paths in",ifile, "and Sections columns in the Experiment list")), type = "error")
      }
      return(NULL)
    }
      
    
    coln =  gsub("[[:punct:]]", "", colnames(mydata)) %>% stringr::str_replace(gsub("[[:punct:]]", "", file_explist$Section_GFP), "GFP") %>% 
      stringr::str_replace(gsub("[[:punct:]]", "", file_explist$Section_Vitality), "Vitality") %>% stringr::str_replace("WID", "Well")
    colnames(mydata) = coln


    if(length(unique(mydata$Well)) != length(mydata$Well)){
      message(paste0("In ",ifile," there is a duplicated well. Check the file."))
      if(shiny::isRunning()){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;In ",ifile," there is a duplicated well. Check the file.")), type = "error")
      }
      return(NULL)
    }
    

    
    #### ELSE ####
  }else{
    message("Instrument not supported")
    if(shiny::isRunning()){
      showNotification(tagList(icon("times-circle"), HTML("&nbsp;Instrument not supported")), type = "error")
    }
    return(NULL)
  }
  
  
  #### Checking and calculating cytotoxicity ####
  if (!is.null(file_target)){
    mydata <- dplyr::left_join(mydata,file_target,by="Well")
  }
  
  if(!is.null(filter.na)){
    id.na=is.na(mydata[,filter.na])
    mydata=mydata[!id.na,]
  }
  
  id.quality=which(mydata$Status=="Released")
  mydata=mydata[id.quality,]
  

  
  if(file_explist$Instrument == "BD_ACCURI_C6"){
    mydata$Vitality = mydata$Vitality*100 #because 96% is read as 0.96
    mydata$GFP = mydata$GFP *100
  }
  
  # !!!!!!!!!!!!  rimosso cyto ma testare ovunque !!!!!!!!!!
  #mydata = tibble::add_column(mydata, Cytotoxicity = 100 - mydata$Vitality, .after = "Vitality")
  
  
  return(mydata)
}
