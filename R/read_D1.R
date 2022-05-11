#' read_D1
#'
#' @description \code{read_D1} Read D1 data
#'
#' @param file_explist The row of the experiment list of that sample (i.e. the experiment list filtered by EXPID)
#' @param file_target The row of the target file of that sample (i.e. the target file filtered by EXPID)
#' @param filter.na A column where the NA filtering is performed. By default is Product.
#'
#' @importFrom dplyr left_join
#' @importFrom readxl read_excel
#' @importFrom janitor remove_empty
#' @importFrom shiny showNotification
#' @importFrom stringr str_replace str_split_fixed
#' @importFrom htmltools HTML
#' @importFrom shiny isRunning
#'


read_D1 = function(file_explist, file_target, filter.na = "Product"){
  ifile = file_explist$File

  #### "BD_ACCURI_C6" ####
  if(file_explist$Instrument == "BD_ACCURI_C6"){
    mydata = readxl::read_excel(paste(file_explist$Path,ifile, sep = "\\"), skip = 2) %>% janitor::remove_empty(which = c("rows", "cols")) %>% 
      as.data.frame()
    coln = colnames(mydata) %>% stringr::str_replace("\\...1", "Well") %>% stringr::str_replace(file_explist$Vitality, "Vitality")
    colnames(mydata) = coln
    mydata$Well <- stringr::str_split_fixed(mydata$Well, pattern = " ", n = 2)[,2]
    MFI_column = c(stringr::str_split(file_explist$Mean_MFI, pattern = ",",simplify = T))
    
    if(all(MFI_column %in% colnames(mydata))){
      markers_name = c(stringr::str_split(file_explist$Markers, pattern = ",",simplify = T))
      mydata = mydata %>% dplyr::rename_with(~markers_name, MFI_column)
    }else{
      message(paste("There is a mismatch between colnames in",ifile, "and Mean_MFI column in the Experiment list"))
      if(shiny::isRunning()){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;There is a mismatch between colnames in",ifile, "and Mean_MFI column in the Experiment list")), type = "error")
      }
      return(NULL)
    }
    
    
    #check well number
    if(length(mydata$Well) != 96){
      message(paste("In",ifile, "there are", length(mydata$Well), "wells instead of 96."))
      # if(shiny::isRunning()){
      #   showNotification(tagList(icon("info"), HTML("&nbsp;In",ifile, "there are", length(mydata$Well), "wells instead of 96.")), type = "default")
      # }
    }
    
  #### "MACS_QUANT_16" ####
  }else if(file_explist$Instrument == "MACS_QUANT_16"){
    mydata = readxl::read_excel(paste(file_explist$Path,ifile, sep = "\\")) %>% janitor::remove_empty(which = c("rows", "cols")) %>% 
      as.data.frame()

    coln = colnames(mydata) %>% stringr::str_replace("WID", "Well") %>% stringr::str_replace(file_explist$Vitality, "Vitality")
    colnames(mydata) = coln
    
    MFI_column = c(stringr::str_split(file_explist$Mean_MFI, pattern = ",",simplify = T))
    sections = c(stringr::str_split(file_explist$Sections, pattern = ",",simplify = T))
    
    vit = mydata %>% dplyr::filter(Path == file_explist$Sections_Vitality) %>% dplyr::select(Well, Vitality)
    
    others = list()
    for(i in 1:length(sections)){
      others[[i]] = mydata %>% dplyr::filter(Path == sections[i]) %>% dplyr::select(Well, MFI_column[i]) #%>% tibble::column_to_rownames("Well")
    }
    
    mydata = cbind(vit,others)
    mydata = mydata[,unique(colnames(mydata))]
    if(length(unique(mydata$Well)) != length(mydata$Well)){
      message(paste0("In ",ifile," there is a duplicated well. Check the file."))
      if(shiny::isRunning()){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;In ",ifile," there is a duplicated well. Check the file.")), type = "error")
      }
      return(NULL)
    }
    
    
    if(all(MFI_column %in% colnames(mydata))){
      markers_name = c(stringr::str_split(file_explist$Markers, pattern = ",",simplify = T))
      mydata = mydata %>% dplyr::rename_with(~markers_name, MFI_column)
    }else{
      message(paste("There is a mismatch between colnames in",ifile, "and Mean_MFI column in the Experiment list"))
      if(shiny::isRunning()){
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;There is a mismatch between colnames in",ifile, "and Mean_MFI column in the Experiment list")), type = "error")
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
  
  #reorder and calculate cytotox
  mycols <- colnames(mydata)
  reorder_cols <- c("Well", "Vitality", setdiff(mycols,c("Vitality", "Well")))
  mydata <- mydata[,reorder_cols]
  
  if(file_explist$Instrument == "BD_ACCURI_C6"){
    mydata$Vitality = mydata$Vitality*100 #because 96% is read as 0.96
  }
  mydata = tibble::add_column(mydata, Cytotoxicity = 100 - mydata$Vitality, .after = "Vitality")

  
  return(mydata)
}
