library(dplyr)

#load exp list
Experiment_list <- readxl::read_xlsx("/Users/fabio/Desktop/Cytotoxicity/Experiment_list_v2.xlsx") %>% janitor::remove_empty(which = c("rows", "cols"), quiet =F)

## load target file
Target_file <- readxl::read_xlsx("/Users/fabio/Desktop/Cytotoxicity/TARGET_FILE 7.4.22.xlsx",na = "NA") %>% 
  janitor::remove_empty(which = c("rows", "cols"), quiet =F) %>% dplyr::filter(Experiment_id %in% Experiment_list$Experiment_id)



##check target file
to_rem = NULL
for(i in unique(Target_file$Experiment_id)){
  expid = Target_file %>% dplyr::filter(Experiment_id == i)
  
  if("Support_type" %in% colnames(expid)){
    #check id with support type
    num_supp = unique(expid$Support_type)
    if(length(num_supp) > 1){
      print(paste0("There are more than one Support type (",num_supp, ") for the ", i, " and will be removed. Check the target file."))
      to_rem = c(to_rem, i)
    }
  }

  #check well numbers in the plate
  if(length(expid$Well) != 96){
    print(paste0("For ", i, " there are ",length(expid$Well)," while they should be 96. This experiment will be removed. Check the target file."))
    to_rem = c(to_rem, i)
  }
  #check well replicates
  if(length(unique(expid$Well)) != length(expid$Well)){
    dup_wells = expid[duplicated(expid$Well),]$Well
    print(paste0("For ", i, " there are some duplicated wells (",dup_wells,"). This experiment will be removed. Check the target file."))
    to_rem = c(to_rem, i)
  }
}

if(!is.null(to_rem)){
  message("Check target file: something to remove!")
  Target_file = Target_file %>% dplyr::filter(!(Experiment_id %in% unique(to_rem)))
}else{
  message("Check target file: OK!")
}


#### eval cytotox

check_files = paste0(Experiment_list$Path, Experiment_list$File)

file_list <- unlist(strsplit(Experiment_list$File, split = ","))


message(paste0("Number of files to be imported: ", length(file_list)))

if (!all(file_list %in% list.files(unique(Experiment_list$Path)))){
  file_wrong = file_list[!file_list %in% list.files(unique(Experiment_list$Path))]
  message("At least one file is missing or reported with the wrong name! Check",file_wrong)
  return(NULL)
} else {
  
  #check wavelength
  expid_for_test = dplyr::filter(Experiment_list, Instrument == "EZ_READ_2000" & Scan == "Double")$Experiment_id
  temp = list()
  temp = lapply(expid_for_test, function(x){
    exp_list = dplyr::filter(Experiment_list, Experiment_id == x)
    if(exp_list$Scan == "Double" && exp_list$Instrument == "EZ_READ_2000"){
      mydata <- as.data.frame(readxl::read_excel(paste0(exp_list$Path,exp_list$File), sheet = "Results")) %>%
        dplyr::select(where(is.double))
      wave = stringr::str_split(exp_list$Wavelength, ",", simplify = T)
      if(!all(wave %in% colnames(mydata))){
        return(exp_list$File)
      }
    }
  })
  where = Filter(Negate(is.null), temp) %>% unlist()
  if(!is.null(where)){
    return(NULL)
  }
  
  
  processed.experiment = list()
  expid = Experiment_list$Experiment_id
  
  percentage <- 0
  
  processed.experiment = lapply(expid, function(x){
    print(paste("Loading experiment",x,sep =" "))  
    
    file_explist = dplyr::filter(Experiment_list, Experiment_id == x)
    x = read_cytoxicity(ifile = file_explist$File, 
                        path = file_explist$Path,
                        instrument = file_explist$Instrument,
                        scan = file_explist$Scan, 
                        sample.anno=dplyr::filter(Target_file, Experiment_id == x),
                        wave = file_explist$Wavelength) %>%
      eval_cytoxicity()
    
  })

  names(processed.experiment) = expid
  myprocesseddata = tibble::as_tibble(data.table::rbindlist(processed.experiment,use.names=TRUE))
  
  col_to_check = c("Model_type","Model_Family", "Product_Family")
  
  err = 0
  for(i in col_to_check){
    if(TRUE %in% is.na(myprocesseddata[,i])){
      message(paste0("There are some NA values inside",i,". Check the target file"))
      err = err+1
    }
  }
  if(err == 0){
    message("col_to_check: OK!")
  }else{
    message("col_to_check: ERROR!")
  }
  
}


### summarise cyto

mydataset = summarise_cytoxicity(myprocesseddata, group = c("Experiment_id","Model_type", "Model_Family", "Product","Product_Family", "Dose", "Purification"))

database_cyto = list(myprocesseddata = myprocesseddata,mydataset = mydataset, exp_list = Experiment_list)
usethis::use_data(database_cyto, overwrite = TRUE)


