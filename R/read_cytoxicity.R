#' read_cytoxicity
#'
#' @description \code{read_cytoxicity} draw multiple break line (br())
#'
#' @param X X
#'
#' @importFrom dplyr select mutate left_join
#' @importFrom readxl read_excel
#' @importFrom tidyr gather unite
#'

read_cytoxicity = function(ifile, path= getwd(),instrument="EZ_READ_2000", scan="Single",sample.anno=NULL,sep=",",filter.na="Product"){
  
  #print("Reading raw data")
  if (scan=="Single") {
    if (instrument=="EZ_READ_2000") {
      mydata <- as.data.frame(readxl::read_excel(paste0(path,ifile), sheet = "Results")) %>%
        dplyr::select(Well,where(is.double))
    } else if (instrument=="4300_CHROMATE_PLATE_READER"){
      my_tibble <- as.data.frame(readxl::read_excel(paste0(path,ifile),  col_names = as.character(seq(1,12))))
      my_tibble$row <- LETTERS[seq( from = 1, to = 8 )]
      mydata <- my_tibble %>% tidyr::gather(column, value, 1:12) %>% 
        tidyr::unite("Well", c(row,column),sep="")
    }
    else {
      stop("Instrument non supported")
    }
  } else if (scan=="Double") {
    if (instrument=="EZ_READ_2000") {
      mydata <- as.data.frame(readxl::read_excel(paste0(path,ifile), sheet = "Results")) %>%
        dplyr::select(Well,where(is.double))
      mydata <-  mydata %>% dplyr::mutate(value=A450-A660) %>% 
        dplyr::select(Well, value)
    } else if (instrument=="4300_CHROMATE_PLATE_READER"){
      ifile450=unlist(strsplit(ifile, sep))[1]
      ifile630=unlist(strsplit(ifile, sep))[2]
      my_tibble450 <- as.data.frame(readxl::read_excel(paste0(path,ifile450),  col_names = as.character(seq(1,12))))
      my_tibble630 <- as.data.frame(readxl::read_excel(paste0(path,ifile630),  col_names = as.character(seq(1,12))))
      my_tibble=my_tibble450-my_tibble630
      my_tibble$row <- LETTERS[seq( from = 1, to = 8 )]
      mydata <- my_tibble %>% tidyr::gather(column, value, 1:12) %>% 
         tidyr::unite("Well", c(row,column),sep="")  
    }
    else {
      stop("Instrument non supported")
    }
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
