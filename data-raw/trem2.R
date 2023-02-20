#### TREM2 ####
library(dplyr)
Experiment_Reporter <- readxl::read_xlsx("data-raw/Reporter/TREM2/Experiment_list_TREM2.xlsx") %>% janitor::remove_empty(which = c("rows", "cols"), quiet =F)
Target_Reporter <- readxl::read_xlsx("data-raw/Reporter/TREM2/Target file TREM2.xlsx",na = "NA") %>% janitor::remove_empty(which = c("rows", "cols"), quiet =F)

Target_Reporter = check_targetfile(target = Target_Reporter, explist = Experiment_Reporter, check_back = FALSE)



file_list <- unlist(strsplit(Experiment_Reporter$File, split = ","))

message(paste0("Number of files to be imported: ", length(file_list)))

if (!all(file_list %in% list.files(unique(Experiment_Reporter$Path)))){
  file_wrong = file_list[!file_list %in% list.files(unique(Experiment_Reporter$Path))]
  message("At least one file is missing or reported with the wrong name! Check",file_wrong)
  return(NULL)
} else {
  
  processed.experiment_repo = list()
  expid_repo = Experiment_Reporter$Experiment_id
  
  
  processed.experiment_repo = lapply(expid_repo, function(x){
    print(paste("Loading experiment",x,sep =" "))  
    
    file_explist = dplyr::filter(Experiment_Reporter, Experiment_id == x)
    file_target = dplyr::filter(Target_Reporter, Experiment_id == x)
    read_TREM2(file_explist, file_target, filter.na = "Product")
    
  })
  names(processed.experiment_repo) = expid_repo
  
  myprocesseddata_repo <- as_tibble(data.table::rbindlist(processed.experiment_repo,use.names=TRUE))
  
  
  col_to_check = c("Model_type","Product_Family", "Product")
  
  err = 0
  for(i in col_to_check){
    if(TRUE %in% is.na(myprocesseddata_repo[,i])){
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

mydataset_repo <- summarise_cytoxicity(myprocesseddata_repo, method = "trem",
                                       group = c("Experiment_id","Model_type", "Product", "Product_Family","Dose", "Purification"))

database_trem2 = list(myprocesseddata = myprocesseddata_repo, mydataset = mydataset_repo, exp_list = Experiment_Reporter)
usethis::use_data(database_trem2, overwrite = TRUE)