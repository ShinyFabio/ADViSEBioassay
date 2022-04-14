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
    conditionalPanel(
      condition = "output.check_target == false", ns = ns,
      div(actionButton(ns("gocyto"), "Evaluate cytotoxicity", icon("cogs")), style = "text-align: center;")
    )
 
  )
}
    
#' load_cyto Server Functions
#'
#' @noRd 
mod_load_cyto_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    ##### EXP list #####
    exp_list_to_edit = reactive({
      req(input$exp_list_file)
      ext <- tools::file_ext(input$exp_list_file$name)
      if(ext != "xlsx"){
        shinyWidgets::show_alert("Invalid file!", "Please upload a .xlsx file", type = "error")
      }
      validate(need(ext == "xlsx", "Invalid file! Please upload a .xlsx file"))
      showNotification(tagList(icon("check"), HTML("&nbsp;Experiment list file loaded.")), type = "message")
      readxl::read_xlsx(input$exp_list_file$datapath) %>% janitor::remove_empty(which = c("rows", "cols"), quiet = FALSE)
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
      
      #filter target based on exp_list
      Target_file = Target_file %>% dplyr::filter(Experiment_id %in% exp_list()$Experiment_id)
      
      to_rem = NULL
      for(i in unique(Target_file$Experiment_id)){
        expid = Target_file %>% dplyr::filter(Experiment_id == i)
        
        
        if("Support_type" %in% colnames(expid)){
          #check id with support type
          num_supp = unique(expid$Support_type)
          if(length(num_supp) > 1){
            print(paste0("There are more than one Support type (",num_supp, ") for the ", i, " and will be removed. Check the target file."))
            showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                                   HTML("There are more than one Support type (",num_supp, ") for the", i, "and will be removed. Check the target file.")), type = "warning")
            to_rem = c(to_rem, i)
          }
        }
        
        
        #check well numbers in the plate
        if(length(expid$Well) != 96){
          print(paste0("For ", i, " there are ",length(expid$Well)," while they should be 96. This experiment will be removed. Check the target file."))
          showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                                 HTML("For ", i, " there are ",length(expid$Well)," while they should be 96. This experiment will be removed. Check the target file.")), type = "warning")
          to_rem = c(to_rem, i)
        }
        #check well replicates
        if(length(unique(expid$Well)) != length(expid$Well)){
          dup_wells = expid[duplicated(expid$Well),]$Well
          print(paste0("For ", i, " there are some duplicated wells (",dup_wells,"). This experiment will be removed. Check the target file."))
          showNotification(duration = 8, tagList(icon("exclamation-circle"), 
                                                 HTML("For ", i, " there are some duplicated wells (",dup_wells,"). This experiment will be removed. Check the target file.")), type = "warning")
          to_rem = c(to_rem, i)
        }
      }
      
      if(is.null(to_rem)){
        showNotification(tagList(icon("check"), HTML("&nbsp;Target file loaded.")), type = "message")
        Target_file
      }else{
        showNotification(tagList(icon("check"), HTML("&nbsp;Target file loaded. Some experiments are removed.")), type = "message")
        Target_file %>% dplyr::filter(!(Experiment_id %in% unique(to_rem)))
      }
      
    })
    
    
    target = mod_edit_data_server("edit_target", data_input = target_to_edit, maxrows = 150)
    
    #check data correctly loaded
    output$check_target = reactive(
      return(is.null(target()))
    )
    outputOptions(output, "check_target", suspendWhenHidden = FALSE)
    
    
    
    #### eval cytotox ####
    data_notsumm = eventReactive(input$gocyto,{
      req(target(), exp_list())
      
      check_files = paste0(exp_list()$Path, exp_list()$File)
      
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
        
        
        processed.experiment = list()
        expid = exp_list()$Experiment_id
        
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
        
        names(processed.experiment) = expid
        myprocesseddata = tibble::as_tibble(data.table::rbindlist(processed.experiment,use.names=TRUE))
        
        col_to_check = c("Model_type","Model_Family", "Product_Family")
        
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
      summarise_cytoxicity(data_notsumm(), group = c("Experiment_id","Model_type", "Model_Family", "Product","Product_Family", "Dose", "Purification"))
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
