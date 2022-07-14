#' load_cyto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_load_cyto_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(10, fileInput(ns("exp_list_file"),"Select the Experiment list file (.xlsx)")),
      column(
        2, style="padding-left: 9px; padding-top: 4px;",
        br(),
        mod_edit_data_ui(ns("edit_exp_list")))
    ),
    conditionalPanel(
      condition = "output.check_explist == false", ns = ns,
      fluidRow(
        column(10, fileInput(ns("target_file"),"Select the Target file (.xlsx)")),
        column(
          2, br(), style="padding-left: 9px; padding-top: 4px;",
          mod_edit_data_ui(ns("edit_target")))
      )
    ),
    
    ### Only for SEAP ###
    conditionalPanel(
      condition = "output.data_type_ui == 'SEAP'", ns = ns,
      hr(),
      conditionalPanel(
        condition = "output.check_target == false", ns = ns,
        ### experiment list for calibration
        fluidRow(
          column(10, fileInput(ns("exp_list_calib"),"Select the Experiment list file for Calibration (.xlsx)")),
          column(
            2, br(), style="padding-left: 9px; padding-top: 4px;",
            mod_edit_data_ui(ns("edit_explist_calib")))
        ),
        
        ###target file for calibration
        conditionalPanel(
          condition = "output.check_explist_calib == false", ns = ns,
          fluidRow(
            column(10, fileInput(ns("target_file_calib"),"Select the Target file (.xlsx)")),
            column(
              2, br(), style="padding-left: 9px; padding-top: 4px;",
              mod_edit_data_ui(ns("edit_target_calib")))
          ),
          awesomeCheckbox(ns("intercept"), "Intercept to zero", value = FALSE)
        )
      )
      
    ),
    
    conditionalPanel(
      condition = "output.check_allOK == false", ns = ns,
      div(actionButton(ns("gocyto"), "Evaluate cytotoxicity", icon("cogs")), style = "text-align: center;")
    )
 
  )
}
    
#' load_cyto Server Functions
#'
#' @noRd 
mod_load_cyto_server <- function(id, data_type){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$data_type_ui = reactive({
      return(data_type())
    })
    outputOptions(output, "data_type_ui", suspendWhenHidden = FALSE)
    
    
    ##### EXP list #####
    exp_list_to_edit = reactive({
      req(input$exp_list_file)
      ext <- tools::file_ext(input$exp_list_file$name)
      if(ext != "xlsx"){
        shinyWidgets::show_alert("Invalid file!", "Please upload a .xlsx file", type = "error")
      }
      validate(need(ext == "xlsx", "Invalid file! Please upload a .xlsx file"))
      showNotification(tagList(icon("check"), HTML("&nbsp;Experiment list file loaded.")), type = "message")
      readxl::read_xlsx(input$exp_list_file$datapath) %>% janitor::remove_empty(which = "rows", quiet = FALSE)
    })
    
    
    exp_list = mod_edit_data_server("edit_exp_list", data_input = exp_list_to_edit, maxrows = 200)
    
    

    #check data correctly loaded
    output$check_explist = reactive(
      return(is.null(exp_list()))
    )
    outputOptions(output, "check_explist", suspendWhenHidden = FALSE)
    
    
    #### Target file ####
    
    target_to_edit = reactive({
      req(input$target_file, exp_list())
      ext <- tools::file_ext(input$target_file$name)
      if(ext != "xlsx"){
        shinyWidgets::show_alert("Invalid file!", "Please upload a .xlsx file", type = "error")
      }
      validate(need(ext == "xlsx", "Invalid file! Please upload a .xlsx file"))
      Target_file = readxl::read_xlsx(input$target_file$datapath, na = "NA") %>% janitor::remove_empty(which = c("rows", "cols"), quiet = FALSE)
      
      check_targetfile(target = Target_file, 
                       explist = exp_list(), 
                       check_back = ifelse(data_type() == "D1" || data_type() == "TREM2", FALSE, TRUE))

    })
    
    
    target = mod_edit_data_server("edit_target", data_input = target_to_edit, maxrows = 150)
    
    #check data correctly loaded
    output$check_target = reactive(
      return(is.null(target()))
    )
    outputOptions(output, "check_target", suspendWhenHidden = FALSE)
    
    
    
    ##### EXP list for CALIBRAtION ####
    
    exp_list_calib_to_edit = reactive({
      req(input$exp_list_calib)
      ext <- tools::file_ext(input$exp_list_calib$name)
      if(ext != "xlsx"){
        shinyWidgets::show_alert("Invalid file!", "Please upload a .xlsx file", type = "error")
      }
      validate(need(ext == "xlsx", "Invalid file! Please upload a .xlsx file"))
      showNotification(tagList(icon("check"), HTML("&nbsp;Experiment list file loaded.")), type = "message")
      readxl::read_xlsx(input$exp_list_calib$datapath) %>% janitor::remove_empty(which = "rows", quiet = FALSE)
    })
    
    
    exp_list_calib = mod_edit_data_server("edit_explist_calib", data_input = exp_list_calib_to_edit, maxrows = 200)
    
    
    
    #check data correctly loaded
    output$check_explist_calib = reactive(
      return(is.null(exp_list_calib()))
    )
    outputOptions(output, "check_explist_calib", suspendWhenHidden = FALSE)
    
    
    
    ##### TARGET FILE CALIBRATION ####

    target_to_edit_calib = reactive({
      req(input$target_file_calib, exp_list_calib())
      ext <- tools::file_ext(input$target_file_calib$name)
      if(ext != "xlsx"){
        shinyWidgets::show_alert("Invalid file!", "Please upload a .xlsx file", type = "error")
      }
      validate(need(ext == "xlsx", "Invalid file! Please upload a .xlsx file"))
      Target_file = readxl::read_xlsx(input$target_file_calib$datapath, na = "NA") %>% janitor::remove_empty(which = c("rows", "cols"), quiet = FALSE)
      
      check_targetfile(target = Target_file, 
                       explist = exp_list_calib(), 
                       check_back = ifelse(data_type() == "D1" || data_type() == "TREM2", FALSE, TRUE))
      
    })

    target_calib = mod_edit_data_server("edit_target_calib", data_input = target_to_edit_calib, maxrows = 150)
    


    
    
    ##### check if everything is loaded (depending on the data type)
    output$check_allOK = reactive({
      if(data_type() == "SEAP"){
        return(is.null(target_calib()))
      }else{
        return(is.null(target()))
      }
    })
    outputOptions(output, "check_allOK", suspendWhenHidden = FALSE)
    
    
    
    ###### Calculate calibration lm ####
    lm_calibration = reactive({
      req(exp_list_calib(), target_calib())
      file_list_cal <- unlist(strsplit(exp_list_calib()$File, split = ","))
      
      
      message(paste0("Number of files to be imported (Calibration): ", length(file_list_cal)))
      
      if (!all(file_list_cal %in% list.files(unique(exp_list_calib()$Path)))){
        file_wrong = file_list_cal[!file_list_cal %in% list.files(unique(exp_list_calib()$Path))]
        message("At least one file is missing or reported with the wrong name! Check",file_wrong)
        return(NULL)
      } else {
        
        expid_cal = exp_list_calib()$Experiment_id
        lm_calibration <- list()
        lm_calibration <- lapply(expid_cal, function(x){
          print(paste("Loading experiment ",x))  
          
          file_explist = dplyr::filter(exp_list_calib(), Experiment_id == x)
          temp2 <- read_cytoxicity(ifile = file_explist$File, 
                                   path = file_explist$Path,
                                   instrument = file_explist$Instrument,
                                   scan = file_explist$Scan, 
                                   sample.anno = dplyr::filter(target_calib(), Experiment_id == x),
                                   wave = file_explist$Wavelength) %>% dplyr::group_by(Experiment_id, Product, Dose) %>% 
            dplyr::summarise(Absorbance = mean(Absorbance, na.rm=TRUE)) %>% dplyr::mutate(Dose = as.double(Dose))
          
          ####add plot lm e niente intercetta
          if(input$intercept == FALSE){
            lmfit =  lm(data = temp2, Dose~Absorbance)
            data.frame(Calibration_ID = x, Coefficient = rev(lmfit$coefficients)[1], Intercept = rev(lmfit$coefficients)[2])
          }else{
            lmfit = lm(data = temp2, Dose~0+Absorbance)
            data.frame(Calibration_ID = x, Coefficient = rev(lmfit$coefficients)[1], Intercept = 0)
          }
        })
        
        lm_calibration = as_tibble(data.table::rbindlist(lm_calibration,use.names=TRUE))
        return(lm_calibration)
      }
      
    })
    
    observeEvent(lm_calibration(),{
      print(lm_calibration())
    })
    
    ##### eval cytotox ####
    data_notsumm = eventReactive(input$gocyto,{
      req(target(), exp_list())
      
      #check_files = paste0(exp_list()$Path, exp_list()$File)
      
      file_list <- unlist(strsplit(exp_list()$File, split = ","))
      
      
      showNotification(tagList(icon("info"), HTML("&nbsp;Number of files to be imported: ", length(file_list))), type = "default")
      message(paste0("Number of files to be imported: ", length(file_list)))
      
      message(paste0("Name of files to be imported: ", "\n"))
      for (k in 1:length(file_list)){
        message(paste0(file_list[k]))
      }
      
      if (!all(file_list %in% list.files(unique(exp_list()$Path)))){
        file_wrong = file_list[!file_list %in% list.files(unique(exp_list()$Path))]
        showNotification(tagList(icon("times-circle"), HTML("&nbsp;At least one file is missing or reported with the wrong name! Check",file_wrong)), type = "error")
        message("At least one file is missing or reported with the wrong name! Check",file_wrong)
        return(NULL)
      } else {
        
        processed.experiment = list()
        expid = exp_list()$Experiment_id
        
        
        ##### cyto #####
        if(data_type() == "cyto"){
          #check wavelength
          expid_for_test = dplyr::filter(exp_list(), Instrument == "EZ_READ_2000" & Scan == "Double")$Experiment_id
          temp = list()
          temp = lapply(expid_for_test, function(x){
            exp_list = dplyr::filter(exp_list(), Experiment_id == x)
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
            showNotification(tagList(icon("times-circle"), HTML("&nbsp;There is a discrepancy between wavelengths in Experiment_list and ",where)), type = "error")
            return(NULL)
          }
          

          percentage <- 0
          
          withProgress(message = "Reading data...", value=0, {
            processed.experiment = lapply(expid, function(x){
              exp_list = dplyr::filter(exp_list(), Experiment_id == x)
              percentage <<- percentage + 1/length(expid)*100
              incProgress(1/length(expid), detail = paste0("Progress: ",round(percentage,0), " %"))
              print(paste("Loading experiment",x,sep =" "))  
              file_explist = dplyr::filter(exp_list, Experiment_id == x)
              read_cytoxicity(ifile = file_explist$File, 
                              path = file_explist$Path,
                              instrument = file_explist$Instrument,
                              scan = file_explist$Scan, 
                              sample.anno=dplyr::filter(target(), Experiment_id == x),
                              wave = file_explist$Wavelength) %>%
                eval_cytoxicity()
              
            })
          })
          
          col_to_check = c("Model_type","Model_Family", "Product_Family", "Product")
          
        }else if(data_type() == "D1"){
          ##### D1 ####
          

          percentage <- 0
          
          withProgress(message = "Reading data...", value=0, {
            processed.experiment = lapply(expid, function(x){
              percentage <<- percentage + 1/length(expid)*100
              incProgress(1/length(expid), detail = paste0("Progress: ",round(percentage,0), " %"))
              print(paste("Loading experiment",x,sep =" "))
              
              file_explist = dplyr::filter(exp_list(), Experiment_id == x)
              file_target = dplyr::filter(target(), Experiment_id == x)
              read_D1(file_explist, file_target, filter.na = "Product")
            })
          })
          
          col_to_check = c("Model_type", "Product_Family", "Product")
        }else if(data_type() == "TREM2"){
          

          percentage <- 0
          
          withProgress(message = "Reading data...", value=0, {
            processed.experiment = lapply(expid, function(x){
              percentage <<- percentage + 1/length(expid)*100
              incProgress(1/length(expid), detail = paste0("Progress: ",round(percentage,0), " %"))
              print(paste("Loading experiment",x,sep =" "))
              
              file_explist = dplyr::filter(exp_list(), Experiment_id == x)
              file_target = dplyr::filter(target(), Experiment_id == x)
              read_TREM2(file_explist, file_target, filter.na = "Product")
            })
          })
          
          col_to_check = c("Model_type","Product_Family", "Product")
          
        }else if(data_type() == "SEAP"){
          ##### SEAP ##

          percentage <- 0
          withProgress(message = "Reading data...", value=0, {
            processed.experiment = lapply(expid, function(x){
              #percentage <<- percentage + 1/length(expid)*100
              #incProgress(1/length(expid), detail = paste0("Progress: ",round(percentage,0), " %"))
              print(paste("Loading experiment",x,sep =" "))
              
              file_explist = dplyr::filter(exp_list(), Experiment_id == x)
              temp <- read_cytoxicity(ifile = file_explist$File, 
                                      path = file_explist$Path,
                                      instrument = file_explist$Instrument,
                                      scan = file_explist$Scan, 
                                      sample.anno=dplyr::filter(target(), Experiment_id == x),
                                      wave = file_explist$Wavelength) %>% 
                dplyr::left_join(dplyr::select(file_explist,Experiment_id,Calibration_ID), by = "Experiment_id") %>% 
                dplyr::mutate(Corrected_value = Absorbance - mean(dplyr::filter(., Product == "BACKGROUND")$Absorbance)) %>%
                dplyr::filter(Product != "BACKGROUND")
              temp$Corrected_value=ifelse(temp$Corrected_value>0,temp$Corrected_value, 0)
              temp
            })
          })

          col_to_check = c("Model_Family", "Model_type","Product_Family")
        }else{
          message("data_type different from cyto, D1 and TREM2. Probably a typo inside the code. Check mod_load_cyto.R")
          return(NULL)
        }

        
        names(processed.experiment) = expid
        myprocesseddata = tibble::as_tibble(data.table::rbindlist(processed.experiment,use.names=TRUE))
        
        if(data_type() == "SEAP"){
          myprocesseddata = myprocesseddata %>% dplyr::left_join(lm_calibration(), by = "Calibration_ID") %>% 
            dplyr::mutate(Concentration = (Corrected_value * Coefficient) + Intercept)
        }
        

        err = 0
        for(i in col_to_check){
          if(TRUE %in% is.na(myprocesseddata[,i])){
            message(paste0("There are some NA values inside",i,". Check the target file"))
            showNotification(tagList(icon("times-circle"), HTML("&nbsp;There are some NA values inside",i,". Check the target file")), type = "error")
            err = err+1
          }
        }
        if(err == 0){
          showNotification(tagList(icon("check"), HTML("&nbsp;Analysis completed!")), type = "message")
          return(myprocesseddata)
        }else{return(NULL)}
        
      }
    })
 
    
    
    data = reactive({
      req(data_notsumm())
      if(data_type() == "cyto"){
        summarise_cytoxicity(data_notsumm(), group = c("Experiment_id","Model_type", "Model_Family", "Product","Product_Family", "Dose", "Purification"))
      }else if(data_type() == "D1"){
        summarise_cytoxicity(data_notsumm(), group = c("Experiment_id","Model_type", "Product", "Product_Family","Dose", "Purification"), method = "d1")
      }else if(data_type() == "TREM2"){
        summarise_cytoxicity(data_notsumm(), method = "trem",
                             group = c("Experiment_id","Model_type", "Product", "Product_Family","Dose", "Purification"))
      }else if(data_type() == "SEAP"){
        summarise_cytoxicity(data_notsumm(), method = "seap",
                             group = c("Experiment_id","Model_type", "Model_Family", "Product","Product_Family", "Dose", "Purification", "Calibration_ID")
                             )
      }else{
        message("data_type different from cyto and D1. Probably a typo inside the code. Check mod_load_cyto.R")
        return(NULL)
      }
    })
    
    
    return(reactive({
      list(mydataset = data(),myprocesseddata = data_notsumm(), exp_list = exp_list())
    }))
    
    
    
  })
}
    
## To be copied in the UI
# mod_load_cyto_ui("load_cyto_1")
    
## To be copied in the server
# mod_load_cyto_server("load_cyto_1")
