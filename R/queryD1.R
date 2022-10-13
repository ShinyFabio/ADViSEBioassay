#' queryD1
#'
#' @description \code{queryD1} Function for a query in D1
#'
#' @param data_D1 The D1 dataframe (data_D1())
#' @param MFI Character. The MFI column where to search (e.g. "CD80").
#' @param operation Operation to perform in comparing with the control (for now one between "greater than" or "greater than or equal").
#' @param thresh Numeric. Threshold for how many times the control. (e.g. 2 means search for MFI greater than 2 times CTRL)
#' @param andor Character. If muliple MFI the operation to be performed ("AND" or "OR"). "AND" returns only the results in common with all the MFIs.
#' 
#' @importFrom dplyr distinct filter if_any
#'



queryD1 <- function(data_D1,
                    MFI = c("CD80","CD40","MHC-II"),
                    operation = "greater than",
                    thresh = 2.5,
                    andor = "OR",
                    final_msg = TRUE){
  
  mydataset_D1 = dplyr::filter(data_D1, !dplyr::if_any("Product_Family", ~grepl("CTRL",.)))
  cnt_D1 <- dplyr::filter(data_D1, dplyr::if_any("Product_Family", ~grepl("CTRL",.)))
  

  temp = sapply(MFI,simplify = FALSE, USE.NAMES = TRUE, function(i){
    message("Searching inside ", i, "...")
    if(shiny::isRunning()){
      showNotification(tagList(icon("gears"), HTML("&nbsp;Searching inside ", i, "...")), type = "default")
    }
    
    #il parallel qui stranamente non funziona
    #cl <- parallel::makeCluster(parallel::detectCores(logical = T)-1)
    #parallel::clusterEvalQ(cl,{library(dplyr)})
    #parallel::clusterExport(cl, c("i", "mydataset_D1", "cnt_D1","operation", "thresh", "filter"), envir=environment())
    
    #temp2 = parallel::parLapply(cl, unique(mydataset_D1$Product_Family), function(x){
    lapply(unique(mydataset_D1$Product_Family), function(x){
      data = dplyr::filter(mydataset_D1, Product_Family == x)
      if(length(unique(data$Purification)) >1){
        #if there are multiple purification, we have to check for each purification
        lapply(unique(data$Purification), function(k){
          data2 = dplyr::filter(data, Purification == k)
          cnt2 = cnt_D1 %>% dplyr::filter(Experiment_id %in% unique(data2$Experiment_id) & Product == "CTRL") %>% as.data.frame()
          if(operation == "greater than"){
            data2 %>% dplyr::filter(base::get(i) > mean(cnt2[,i])*thresh)
          }else{
            data2 %>% dplyr::filter(base::get(i) >= mean(cnt2[,i])*thresh)
          }
          
        }) %>% {Reduce(rbind, .)} #dato che %>% assegna come primo posto, uso {} e metto il . per la posizione.
        
      }else{
        cnt = cnt_D1 %>% dplyr::filter(Experiment_id %in% unique(data$Experiment_id) & Product == "CTRL") %>% as.data.frame()
        if(operation == "greater than"){
          data %>% dplyr::filter(base::get(i) > mean(cnt[,i])*thresh)
        }else{
          data %>% dplyr::filter(base::get(i) >= mean(cnt)*thresh)
        }
      }
    }) %>% {Reduce(rbind, .)}
    
    #parallel::stopCluster(cl)
    #temp2
    
  })
  
  
  if(length(MFI) > 1){
    if(andor == "AND"){
      raw = Reduce(intersect, temp)
    }else{
      raw = Reduce(rbind, temp) %>% dplyr::distinct()
    }
    
  }else{
    raw = temp[[MFI]]
  }
  
  if(final_msg){
    if(!is.null(raw)){
      message("Completed! Found ", length(raw$Product_Family), " fractions.")
      if(shiny::isRunning()){
        showNotification(tagList(icon("check"), HTML("&nbsp;Completed! Found ", length(unique(raw$Product_Family)), " Product Family.")), type = "message")
      }
    }else{
      message("ERROR. Output is null")
      if(shiny::isRunning()){
        showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;ERROR. Output is null")), type = "error")
      }
    }
  }

  
  return(raw)
  
}
