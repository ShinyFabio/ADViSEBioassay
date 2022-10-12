library(dplyr)

#### importing calibration lines ####

Experiment_cal <- readxl::read_xlsx("/Users/fabio/Desktop/Cytotoxicity/Reporter/SEAP/CALIBRATION SEAP/Experiment_list_SEAPCAL.xlsx") %>% janitor::remove_empty(which = c("rows", "cols"), quiet =F)
Target_cal <- readxl::read_xlsx("/Users/fabio/Desktop/Cytotoxicity/Reporter/SEAP/CALIBRATION SEAP/target file database CALIBRATION SEAP.xlsx") %>% janitor::remove_empty(which = c("rows", "cols"), quiet =F)


Target_cal = check_targetfile(target = Target_cal, explist = Experiment_cal, check_ctrl = FALSE)


file_list_cal <- unlist(strsplit(Experiment_cal$File, split = ","))


message(paste0("Number of files to be imported: ", length(file_list_cal)))

if (!all(file_list_cal %in% list.files(unique(Experiment_cal$Path)))){
  file_wrong = file_list_cal[!file_list_cal %in% list.files(unique(Experiment_cal$Path))]
  message("At least one file is missing or reported with the wrong name! Check",file_wrong)
  return(NULL)
} else {

  expid_cal = Experiment_cal$Experiment_id
  
  lm_calibration <- list()
  lm_calibration <- lapply(expid_cal, function(x){
    print(paste("Loading experiment",x,sep =" "))  
    
    file_explist = dplyr::filter(Experiment_cal, Experiment_id == x)
    temp2 <- read_cytoxicity(ifile = file_explist$File, 
                             path = file_explist$Path,
                             instrument = file_explist$Instrument,
                             scan = file_explist$Scan, 
                             sample.anno = dplyr::filter(Target_cal, Experiment_id == x),
                             wave = file_explist$Wavelength) %>% dplyr::group_by(Experiment_id, Product, Dose) %>% 
      dplyr::summarise(Absorbance = mean(Absorbance, na.rm=TRUE)) %>% dplyr::mutate(Dose = as.double(Dose))
    
    ####add plot lm
    
    lmfit = lm(data = temp2, Dose~0+Absorbance)
    
    data.frame(Calibration_ID = x, Coefficient = rev(lmfit$coefficients)[1], Intercept = rev(lmfit$coefficients)[2])
  })
  
  
  lm_calibration = as_tibble(data.table::rbindlist(lm_calibration,use.names=TRUE))
  }





##### importing files ####
Experiment_SEAP <- readxl::read_xlsx("/Users/fabio/Desktop/Cytotoxicity/Reporter/SEAP/Experiment_list_REPORTERSEAP.xlsx") %>% janitor::remove_empty(which = c("rows", "cols"), quiet =F)
Target_SEAP <- readxl::read_xlsx("/Users/fabio/Desktop/Cytotoxicity/Reporter/SEAP/target file database REPORTER.xlsx",na = "NA") %>% janitor::remove_empty(which = c("rows", "cols"), quiet =F)

Target_SEAP = check_targetfile(target = Target_SEAP, explist = Experiment_SEAP)


file_list <- unlist(strsplit(Experiment_SEAP$File, split = ","))


message(paste0("Number of files to be imported: ", length(file_list)))

if (!all(file_list %in% list.files(unique(Experiment_SEAP$Path)))){
  file_wrong = file_list[!file_list %in% list.files(unique(Experiment_SEAP$Path))]
  message("At least one file is missing or reported with the wrong name! Check",file_wrong)
  return(NULL)
} else {
  
  processed.experiment_SEAP = list()
  expid_SEAP = Experiment_SEAP$Experiment_id
  
  
  processed.experiment_SEAP = lapply(expid_SEAP, function(x){
    print(paste("Loading experiment",x,sep =" "))  
    
    file_explist = dplyr::filter(Experiment_SEAP, Experiment_id == x)
    temp <- read_cytoxicity(ifile = file_explist$File, 
                            path = file_explist$Path,
                            instrument = file_explist$Instrument,
                            scan = file_explist$Scan, 
                            sample.anno=dplyr::filter(Target_SEAP, Experiment_id == x),
                            wave = file_explist$Wavelength) %>% 
      dplyr::left_join(dplyr::select(file_explist,Experiment_id,Calibration_ID), by = "Experiment_id") %>% 
      dplyr::mutate(Corrected_value = Absorbance - mean(dplyr::filter(., Product == "BACKGROUND")$Absorbance)) %>%
      dplyr::filter(Product != "BACKGROUND")
    temp$Corrected_value=ifelse(temp$Corrected_value>0,temp$Corrected_value, 0)
    temp
  })
  
  
  names(processed.experiment_SEAP) = expid_SEAP
  myprocesseddata_SEAP = as_tibble(data.table::rbindlist(processed.experiment_SEAP,use.names=TRUE)) %>% 
    dplyr::left_join(lm_calibration, by = "Calibration_ID") %>% 
    dplyr::mutate(Concentration = (Corrected_value * Coefficient) + 0)
  
  
  
  col_to_check = c("Model_Family", "Model_type","Product_Family")
  
  err = 0
  for(i in col_to_check){
    if(TRUE %in% is.na(myprocesseddata_SEAP[,i])){
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


#### summarise  SEAP ####

mydataset_SEAP = summarise_cytoxicity(myprocesseddata_SEAP, 
                                 group = c("Experiment_id","Model_type", "Model_Family", 
                                           "Product","Product_Family", "Dose", "Purification", "Calibration_ID"), 
                                 method = "seap")


database_seap = list(myprocesseddata = myprocesseddata_SEAP, mydataset = mydataset_SEAP, exp_list = Experiment_SEAP)
usethis::use_data(database_seap, overwrite = TRUE)

