#' eval_cytoxicity
#'
#' @description \code{eval_cytoxicity} draw multiple break line (br())
#'
#' @param X data
#' @param ref Column that contains the information about background and CTRL. By default "Product"
#' @param background The value inside the Product column that refers to the background. By default BACKGROUND
#' @param CTRL The value inside the Product column that refers to the control. By default CTRL
#' 
#' @importFrom dplyr mutate
#'


eval_cytoxicity = function(X,ref="Product",background="BACKGROUND",CTRL="CTRL"){
  
  if(is.null(X)){
    return(NULL)
  }
  X <- as.data.frame(X)
  id_backgroud <- which(X[,ref]==background)
  mean_background <- mean(X[id_backgroud,"Absorbance"])
  
  X <- X %>% dplyr::mutate(Corrected_value=Absorbance-mean_background)
  X$Corrected_value=ifelse(X$Corrected_value>0,X$Corrected_value, 0)
  
  id_CTRL <-  which(X[,ref]==CTRL)
  mean_CTRL <- mean(X[id_CTRL,"Corrected_value"])
  X<- X %>% dplyr::mutate(Cytotoxicity=((mean_CTRL-Corrected_value)/mean_CTRL)*100 )
  X$Cytotoxicity=ifelse(X$Cytotoxicity>0, X$Cytotoxicity, 0)
  X$Cytotoxicity=ifelse(X$Cytotoxicity>100, 100, X$Cytotoxicity)
  X <- X[X[,ref]!=background,]
  
  X[which(X[,ref]==CTRL),"Cytotoxicity"] = 0
  X$Vitality =  100 - X$Cytotoxicity

  return(X)
}  
