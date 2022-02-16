#' eval_cytoxicity
#'
#' @description \code{read_cytoxicity} draw multiple break line (br())
#'
#' @param X X
#'
#' @importFrom dplyr mutate
#'


eval_cytoxicity = function(X,ref="Product",background="BACKGROUND",CTRL="CTRL"){
  
  X <- as.data.frame(X)
  id_backgroud <- which(X[,ref]==background)
  mean_background <- mean(X[id_backgroud,"Absorbance"])
  
  X <- X %>% dplyr::mutate(Corrected_value=Absorbance-mean_background)
  X$Corrected_value=ifelse(X$Corrected_value>0,X$Corrected_value, 0)
  
  id_CTRL <-  which(X[,ref]==CTRL)
  mean_CTRL <- mean(X[id_CTRL,"Corrected_value"])
  X<- X %>% dplyr::mutate(Cytoxicity=((mean_CTRL-Corrected_value)/mean_CTRL)*100 )
  X$Cytoxicity=ifelse(X$Cytoxicity>0, X$Cytoxicity, 0)
  X$Cytoxicity=ifelse(X$Cytoxicity>100, 100, X$Cytoxicity)
  X <- X[X[,ref]!=background,]
  X$Vitality =  100 - X$Cytoxicity

  return(X)
}  
