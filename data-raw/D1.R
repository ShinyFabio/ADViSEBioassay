library(dplyr)

#load exp list
Experiment_list <- readxl::read_xlsx("/Users/fabio/Desktop/Cytotoxicity/D1/Experiment_list_D1.xlsx") %>% 
  janitor::remove_empty(which = "rows", quiet =F)

## load target file
Target_file <- readxl::read_xlsx("/Users/fabio/Desktop/Cytotoxicity/D1/Target file D1.xlsx",na = "NA") %>% 
  janitor::remove_empty(which = c("rows", "cols"), quiet =F) %>% dplyr::filter(Experiment_id %in% Experiment_list$Experiment_id)


##check target file
to_rem = NULL
for(i in unique(Target_file$Experiment_id)){
  expid = Target_file %>% dplyr::filter(Experiment_id == i)
  
  #in realtà support_type non è presente
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
  
  processed.experiment = list()
  expid = Experiment_list$Experiment_id
  
  processed.experiment = lapply(expid, function(x){
    print(paste("Loading experiment",x,sep =" "))  
    file_explist = dplyr::filter(Experiment_list, Experiment_id == x)
    file_target = dplyr::filter(Target_file, Experiment_id == x)
    read_D1(file_explist, file_target, filter.na = "Product")
  })

  
  names(processed.experiment) = expid
  myprocesseddata_D1 = tibble::as_tibble(data.table::rbindlist(processed.experiment,use.names=TRUE))
  
  
  col_to_check = c("Model_type","Product_Family")
  
  err = 0
  for(i in col_to_check){
    if(TRUE %in% is.na(myprocesseddata_D1[,i])){
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

mydataset = summarise_cytoxicity(myprocesseddata_D1, 
                                 group = c("Experiment_id","Model_type", "Product", "Product_Family","Dose", "Purification"), 
                                 method = "d1",
                                 markers_name = unique(c(stringr::str_split(unique(Experiment_list$Markers), pattern = ",",simplify = T)))
                                 )

### !!!!!!!!!!! markers name da testare se cambiano i markers !!!!!!!!!!!!!!!!!

database_D1 = list(myprocesseddata = myprocesseddata_D1,mydataset = mydataset, exp_list = Experiment_list)
usethis::use_data(database_D1, overwrite = TRUE)
