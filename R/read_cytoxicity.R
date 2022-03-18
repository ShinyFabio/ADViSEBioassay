#' read_cytoxicity
#'
#' @description \code{read_cytoxicity} draw multiple break line (br())
#'
#' @param X X
#'
#' @importFrom dplyr select mutate left_join
#' @importFrom readxl read_excel
#' @importFrom tidyr gather unite
#' @importFrom janitor remove_empty
#' @importFrom shiny showNotification
#'

read_cytoxicity = function(ifile, path= getwd(),instrument="EZ_READ_2000", scan="Single", wave, sample.anno=NULL,sep=",",filter.na="Product"){
  
  #print("Reading raw data")
  if (scan=="Single") {
    if (instrument=="EZ_READ_2000") {
      
      mydata <- readxl::read_excel(paste0(path,ifile), sheet = "Results") %>% janitor::remove_empty(which = c("rows", "cols")) %>% 
        as.data.frame() %>%dplyr::select(Well,where(is.double))
    } else if (instrument=="4300_CHROMATE_PLATE_READER"){
      my_tibble <- readxl::read_excel(paste0(path,ifile),  col_names = as.character(seq(1,12))) %>% janitor::remove_empty(which = c("rows", "cols")) %>% 
        as.data.frame()
      my_tibble$row <- LETTERS[seq( from = 1, to = 8 )]
      mydata <- my_tibble %>% tidyr::gather(column, value, 1:12) %>% 
        tidyr::unite("Well", c(row,column),sep="")
    }
    else {
      stop("Instrument not supported")
    }
  } else if (scan=="Double") {
    if (instrument=="EZ_READ_2000") {
      mydata <- readxl::read_excel(paste0(path,ifile), sheet = "Results") %>% janitor::remove_empty(which = c("rows", "cols")) %>%
        as.data.frame() %>% dplyr::select(Well,where(is.double))
      wave = stringr::str_split(wave, ",", simplify = T)
      mydata <-  mydata %>% dplyr::mutate(value= !!sym(wave[1])-!!sym(wave[2])) %>% 
        dplyr::select(Well, value)
      
    } else if (instrument=="4300_CHROMATE_PLATE_READER"){
      ifile450=unlist(strsplit(ifile, sep))[1]
      ifile630=unlist(strsplit(ifile, sep))[2]
      my_tibble450 <- readxl::read_excel(paste0(path,ifile450),  col_names = as.character(seq(1,12))) %>% 
        janitor::remove_empty(which = c("rows", "cols")) %>% as.data.frame()
      my_tibble630 <- readxl::read_excel(paste0(path,ifile630),  col_names = as.character(seq(1,12))) %>% 
        janitor::remove_empty(which = c("rows", "cols")) %>% as.data.frame()
      my_tibble=my_tibble450-my_tibble630
      my_tibble$row <- LETTERS[seq( from = 1, to = 8 )]
      mydata <- my_tibble %>% tidyr::gather(column, value, 1:12) %>% 
         tidyr::unite("Well", c(row,column),sep="")  
    }
    else {
      stop("Instrument non supported")
    }
  } 
  
  #check well number
  if(length(mydata$Well) != 96){
    message(paste("In",ifile, "there are", length(mydata$Well), "wells instead of 96. This experiment will be removed. Check the file."))
    showNotification(tagList(icon("exclamation-circle"), HTML(
      "In",ifile, "there are", length(mydata$Well), "wells instead of 96.This experiment will be removed. Check the file.")), type = "warning")
    return(NULL)
  }
  
  colnames(mydata)=c("Well","Absorbance")
  if (!is.null(sample.anno)){
    mydata <- dplyr::left_join(mydata,sample.anno,by="Well")
  }

   if(!is.null(filter.na)){
    id.na=is.na(mydata[,filter.na])
    mydata=mydata[!id.na,]
   }


    id.quality=which(mydata$Status=="Released")
    mydata=mydata[id.quality,]


    mycols <- colnames(mydata)
    reorder_cols <- c(setdiff(mycols,"Absorbance"),"Absorbance")
    mydata <- mydata[,reorder_cols]
    
  #  print("Done")
    return(mydata)
 
}
