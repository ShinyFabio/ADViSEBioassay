#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import dplyr
#' @import ggplot2
#' @importFrom tibble as_tibble column_to_rownames
#' @importFrom data.table rbindlist
#' @importFrom DT renderDT
#' @importFrom shinyWidgets show_alert ask_confirmation
#' @importFrom readxl read_xlsx
#' @importFrom openxlsx write.xlsx
#' @importFrom tools file_ext
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom janitor remove_empty
#' @importFrom tidyr unite pivot_wider
#' @importFrom stringr str_detect str_split_fixed
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ComplexHeatmap HeatmapAnnotation Heatmap
#' @importFrom grid unit gpar
#' @importFrom InteractiveComplexHeatmap makeInteractiveComplexHeatmap
#' @importFrom utils combn
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  options(scipen = 999)
  

  observeEvent(input$jumptohome, {
    shinydashboard::updateTabItems(session, "sidebarmenu", "home")
  })
  
  
  # the modal dialog where the user can enter the query details.
  query_modal <- modalDialog(
    h3(strong("Welcome to ADViSELipidomics"), style = "text-align: center"),
    br(),
    h4("Before start, please enter your name and your company."),
    textInput("indata_analyst", list(HTML("&nbsp;"), icon("user"), HTML("&nbsp;Enter your name")), value = "Name"),
    textInput("inlab_analyst", list(HTML("&nbsp;"), icon("building"), HTML("&nbsp;Enter your company")), value = "Company"),
    easyClose = F,
    footer = tagList(
      actionButton("run", "Run")
    )
  )
  
  
  
  # Show the model on start up ...
  observeEvent(input$sidebarmenu,{
    req(input$sidebarmenu == "rawsub")
    showModal(query_modal)
  },ignoreInit = TRUE, once = TRUE)
  
  
  # ... or when user wants to change query
  observeEvent(input$change,{
    showModal(query_modal)
  })
  
  observeEvent(input$run, {
    removeModal()
  })
  
  output$nome = renderText({
    if(is.null(input$indata_analyst)){
      "Name"
    }else{
      input$indata_analyst
    }
  })
  
  
  
  ##### cytotoxicity ######
  
  cyto_data1 = reactiveVal(database_cyto)
  
  observe({
    filepath = paste0(base::system.file(package = "ADViSEBioassay"),"/data/cyto/database_updated_cyto.rds")
    
    if(file.exists(filepath) == TRUE){
      cyto_data1(readRDS(filepath))
    }
  })
  
  output$valbox_cyto = renderUI({
    if(is.null(cyto_data1())){
      box(width = 12, background = "yellow",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Cytotoxicity data: "),style = "color: white"),
                     h5("No Cytotoxicity data present in database!", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )
          )
      )
    }else{
      n_az = cyto_data1()$mydataset$Experiment_id %>% unique() %>% length()
      box(width = 12, background = "green",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("Cytotoxicity data: "),style = "color: white"),
                     h5(strong(n_az), " experiments.", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-check", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )
          )
      )
    }
  })
  
  
  #carica il file
  loaded_database_cyto1 = eventReactive(input$loaddatabase,{
    if(!is.null(cyto_data1())){
      showNotification(tagList(icon("info"), HTML("&nbsp;Cytotoxicity data loading...")), type = "default")
      return(cyto_data1())
    }else{
      showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;Cytotoxicity data not loaded")), type = "error")
      return(NULL)
    }
  })
  
  observeEvent(loaded_database_cyto1(),{
    if(!is.null(loaded_database_cyto1())){
      showNotification(tagList(icon("check"), HTML("&nbsp;Cytotoxicity data loaded!")), type = "message")
    }
  })
  
  loaded_database_cyto = reactiveVal()
  
  observeEvent(loaded_database_cyto1(),{
    loaded_database_cyto(loaded_database_cyto1())
  })

  
  #### update data if present
  cyto_from_mod = mod_load_cyto_server("load_cyto_mod", data_type = reactive("cyto"))
  
  
  #check data correctly loaded
  output$check_data_updated = reactive(
    return(is.null(cyto_from_mod()))
  )
  outputOptions(output, "check_data_updated", suspendWhenHidden = FALSE)
  
  
  output$newdata_cyto_DT = renderDT({
    req(cyto_from_mod())
    if(input$summ_viewtable_updated == TRUE){
      cyto_from_mod()$mydataset %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
    }else{
      cyto_from_mod()$myprocesseddata %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
    }
  },options = list(scrollX = TRUE))
  

  observeEvent(input$update_cyto_bttn,{
    if(!is.null(cyto_from_mod())){
      new_data = update_database(old_data = loaded_database_cyto1(), new_data = cyto_from_mod())
      if(!is.null(new_data)){
        if(new_data == 0){
          shinyWidgets::sendSweetAlert(title = "Nothing to merge!", type = "warning",
                                       text = "The experiments in the new data are alredy present in the database.")
        }else{
          loaded_database_cyto(new_data)
          shinyWidgets::sendSweetAlert(title = "Merging completed!", type = "success",
                                       text = "New data loaded! Click on 'Save database' if you want to store the changes.")
          showNotification(tagList(icon("check"), HTML("&nbsp;New data loaded! Click on 'Save database' if you want to store the changes.")), type = "message")
        }
      }

    }
  })
  
  

  ###### Upload updated
  
  #Upload
  observeEvent(input$upload_updated_cyto,{
    showModal(modalDialog(
      title = "Upload an existing database (.rds)",
      footer = modalButton("Close"),
      fluidRow(
        column(8,fileInput("upload_file_cyto", "Upload a database (.rds)", accept = ".rds")),
        conditionalPanel(
          condition = "output.check_fileuploaded_cyto == true",
          column(3,br(),actionButton("load_upload_file_cyto", "Load!", icon("rocket"),style='padding:10px; font-size:140%; font-weight: bold;'))
        )
      )
    ))
  })
  
  
  output$check_fileuploaded_cyto <- reactive({
    return(!is.null(input$upload_file_cyto))
  }) 
  outputOptions(output, 'check_fileuploaded_cyto', suspendWhenHidden=FALSE)
  
  

  
  #import 
  observeEvent(input$load_upload_file_cyto,{
    req(input$upload_file_cyto)
    ext <- tools::file_ext(input$upload_file_cyto$name)
    if(ext != "rds"){
      shinyWidgets::show_alert("Invalid file!", "Please upload a .rds file", type = "error")
    }
    validate(need(ext == "rds", "Invalid file! Please upload a .rds file"))
    
    file = readRDS(file = input$upload_file_cyto$datapath)
    loaded_database_cyto(file)
    showNotification(tagList(icon("check"), HTML("&nbsp;New database loaded! Click on 'Save database' if you want to store the changes.")), type = "message")
  })
  

  
  
  ##### SAVE Updated #####
  
  #check if something new is loaded otherwhise don't display the save button
  output$check_ifsave_cyto = reactive({
    checkdatabase = tryCatch({cyto_from_mod()
      FALSE
    },shiny.silent.error = function(e) {TRUE})
    
    if(TRUE %in% c(!checkdatabase, input$load_upload_file_cyto > 0)){return(TRUE)}
  })
  outputOptions(output, "check_ifsave_cyto", suspendWhenHidden = FALSE)
  
  
  
  observeEvent(input$save_update,{
    shinyWidgets::ask_confirmation(
      inputId = "confirmsave_cyto",
      type = "warning",
      title = "Do you want to save and update the internal database?",
      text = h4("Be sure that everything works before update.
      If you need to restore the original database, click on", strong("Restore Database."))
    )
  })


  observeEvent(input$confirmsave_cyto,{
    checkdatabase = tryCatch({loaded_database_cyto()
      FALSE
    },shiny.silent.error = function(e) {TRUE})
    
    
    if(input$confirmsave_cyto == TRUE && checkdatabase == FALSE){
      if(dim(loaded_database_cyto1()$exp_list)[1] == dim(loaded_database_cyto()$exp_list)[1]){
        showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;The internal database is already updated.")), type = "error")
      }else{
        
        filepath = paste0(base::system.file(package = "ADViSEBioassay"),"/data/cyto/")
        if(dir.exists(filepath) == FALSE){
          dir.create(filepath)
        }
        
        saveRDS(loaded_database_cyto(), file = paste0(filepath,"database_updated_cyto.rds"))
        
        #versione per cronologia
        filever = paste0(filepath,"ver_",Sys.Date(),".rds")
        saveRDS(loaded_database_cyto(), file = filever)

        show_alert(
          title = "Upload completed!",
          text = "Please restart ADViSEBioassay to save the changes.",
          type = "success"
        ) 
      }
    }
  })
  
  
  
  ##### download updated cyto ######
  
  #check all data correctly loaded. If FALSE -> error.
  output$checkupdated_cyto_fordownload = reactive({
    checkdatabase = tryCatch({cyto_from_mod()
      FALSE
    },shiny.silent.error = function(e) {TRUE})
    check_file = file.exists(paste0(base::system.file(package = "ADViSEBioassay"),"/data/cyto/database_updated_cyto.rds"))
    if(TRUE %in% c(!checkdatabase, check_file)){return(TRUE)}
  })
  outputOptions(output, "checkupdated_cyto_fordownload", suspendWhenHidden = FALSE)
  
  
  
  #### Download handler for the download button
  output$download_updated_cyto <- downloadHandler(
    #put the file name with also the file extension
    filename = function() {
      paste0("updated_cyto_", Sys.Date(), ".rds")
    },
    
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      saveRDS(loaded_database_cyto(), file)
    }
  )
  
  

  
  
  #### remove updated cyto #####
  
  
  #modal restore
  observeEvent(input$remove_update_cyto,{
    showModal(modalDialog(
      title = "Restore database",
      footer = modalButton("Close"),
      size = "l",
      fluidRow(
        column(
          6,style = "text-align:center;",
          box(width = NULL, status = "primary", title = "Restore database to a previous version", solidHeader = TRUE,
              column(10,offset = 1, selectInput("ver_databases", "Select a database", choices = "")),
              conditionalPanel(
                condition = "input.ver_databases != ''",
                actionButton("load_version", HTML("&nbsp;Load version"),icon("rotate-left"), style = "background: #e74c3c; border-color: #e74c3c;padding:10px; font-size:120%; font-weight: bold;"),
              )
              )
          
          ),
        column(
          6,style = "text-align:center;",
          box(width = NULL, status = "primary", title = "Reset database to the original internal database", solidHeader = TRUE,
              br(),
              actionButton("rest_to_original", HTML("&nbsp;Reset database"),icon("trash-can"), style = "background: #e74c3c; border-color: #e74c3c;padding:10px; font-size:120%; font-weight: bold;"),
              br(),br(), br()
              ))
      )
    ))
  })
  
  
  #### LOAD OLD VERSION
  
  observeEvent(input$remove_update_cyto,{
    files = list.files(paste0(base::system.file(package = "ADViSEBioassay"),"/data/cyto/"))
    files = grep("ver_",files, value = TRUE)
    updateSelectInput(session, "ver_databases", choices = files)
  })
  
  
  observeEvent(input$load_version,{
    shinyWidgets::ask_confirmation(
      inputId = "confirmload_ver",
      type = "warning",
      title = "Do you want to load this version?",
      text = h4("By clicking yes, this database version will be restored. Please restart ADViSEBioassay to apply this change.")
    )
  })
  
  
  
  observeEvent(input$confirmload_ver,{
    filepath_ver = paste0(base::system.file(package = "ADViSEBioassay"),"/data/cyto/",input$ver_databases)
    
    if(isTRUE(input$confirmload_ver)){
      if(file.exists(filepath_ver)){    
        file = readRDS(filepath_ver)
        filepath = paste0(base::system.file(package = "ADViSEBioassay"),"/data/cyto/database_updated_cyto.rds")
        saveRDS(file, file = filepath)
        showNotification(tagList(icon("check"), HTML("&nbsp;Previous version restored!")), type = "message")
      }else{
        showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;This file version doesn't exist.")), type = "error")
      }
    }
  })
  
  
  

  
  ##### RESTORE
  observeEvent(input$rest_to_original,{
    shinyWidgets::ask_confirmation(
      inputId = "confirmremove_cyto",
      type = "warning",
      title = "Do you want to restore the internal database?",
      text = h4("By clicking yes, the original database will be restored. Please restart ADViSEBioassay to apply this change.")
    )
  })
  
  
  observeEvent(input$confirmremove_cyto,{
    
    filepath = paste0(base::system.file(package = "ADViSEBioassay"),"/data/cyto/")
    listf = list.files(filepath)
    #listf = listf[!grepl("database_cyto.rda", listf)]
    file_to_rem = sapply(listf, function(x) paste0(filepath,x))
    
    if(isTRUE(input$confirmremove_cyto)){
      if(TRUE %in% file.exists(file_to_rem)){
        file.remove(file_to_rem)
        showNotification(tagList(icon("check"), HTML("&nbsp;Original database restored!")), type = "message")
      }else{
        showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;Updated database already removed.")), type = "error")
      }
    }
    
  })
  
  

  
  
  #load data
  data = reactive({
    req(loaded_database_cyto())
    loaded_database_cyto()$mydataset
  })
  
  data_notsumm = reactive({
    req(loaded_database_cyto())
    loaded_database_cyto()$myprocesseddata
  })
  
  exp_list = reactive({
    req(loaded_database_cyto())
    loaded_database_cyto()$exp_list
  })
  

  
  #check data correctly loaded
  output$check_data = reactive(
    return(is.null(data()))
  )
  outputOptions(output, "check_data", suspendWhenHidden = FALSE)
  
  
  output$dtdata = renderDT({
    req(data())
    if(input$summ_viewtable == TRUE){
      data() %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
    }else{
      data_notsumm() %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
    }
  },options = list(scrollX = TRUE))
  
  
 #### cytotoxicity mod ####
  

  
  
  # data = reactive({
  #   req(cyto_from_mod(), data2())
  #   cyto_from_mod()$data
  # })
  # 
  # data_notsumm = reactive({
  #   req(cyto_from_mod())
  #   cyto_from_mod()$data_notsumm
  # })
  # 
  # exp_list = reactive({
  #   req(cyto_from_mod())
  #   cyto_from_mod()$exp_list
  # })
  # 
  

  
  
  
  ##### informative plots #####
  
  output$countbarplot = plotly::renderPlotly({
    req(data(), exp_list())
    
    count = data() %>% dplyr::select(where(is.character)) %>% dplyr::mutate(across(where(is.character), ~length(unique(.x))))
    count = t(count[1,])
    colnames(count) = "Count"
    count = count %>% as.data.frame() %>% tibble::rownames_to_column("Measure")
    count$Var = count$Measure
    
    inst = as.data.frame(table(exp_list()$Instrument))
    inst$Measure = "Instrument"
    scans = as.data.frame(table(exp_list()$Scan))
    scans$Measure = "Scan"
    

    scanint = rbind(inst,scans)
    colnames(scanint) = c("Var", "Count","Measure")
    
    count = rbind(count,scanint)
    count$Measure = factor(count$Measure, levels = unique(count$Measure))
    count$Var = factor(count$Var, levels = unique(count$Var))
    temp = ggplot(count, aes(x = Measure, y = Count, group = Var)) + geom_col(aes(fill = Var)) + 
      geom_text(aes(label=Count),position = position_stack(vjust = 0.5)) + ggtitle("Data overview") + labs(x = "Measure", fill = "Measure", y = "Total numbers") + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0))
    
    plotly::ggplotly(temp)
    
  })
  
  
  
  
  output$modtype_barplto = plotly::renderPlotly({
    req(data())
    temp = ggplot(data(), aes(x= Model_type)) + geom_bar(aes(fill = Model_Family)) + ggtitle("Sample for each Model type")+ 
      ylab("Number of samples") + theme(axis.text.x = element_text(angle = 315, hjust = 0))
    plotly::ggplotly(temp)
  })
  

  output$prodfam_barplot = plotly::renderPlotly({
    req(data())
    data_filt = data() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.)))
    prod_table =  as.data.frame(table(data_filt$Product_Family))
    colnames(prod_table)[1] = "Product_Family"
    
    prod_table = prod_table[order(-prod_table$Freq),]
    prod_table$Product_Family = factor(prod_table$Product_Family, levels = rev(prod_table$Product_Family))
    
    
    if(input$first50_prodfam == TRUE){
      prod_table = prod_table[1:50,]
      size_plot = 84 + 640
    }else{
      size_plot = 84 + (640*length(prod_table$Product_Family))/(50 + length(prod_table$Product_Family)/10) #640 found with html inspect when 50 prod are shown.
    }
    temp = ggplot(prod_table) + geom_col(aes(y = Product_Family, x = Freq, fill = Product_Family))+ 
      xlab("Number of samples")+ ylab("Product Family") + ggtitle("Sample for each Product Family") + 
      theme(axis.text.y = element_text(size = 7.4))

    
    plotly::ggplotly(temp, tooltip = c("x", "fill"))
  })
  
  output$prodfam_barplotUI = renderUI({
    
    if(input$first50_prodfam == TRUE){
      size_plot = 84 + 640
    }else{
      size_plot = 84 + (640*length(unique(data()$Product_Family)))/(50 + length(unique(data()$Product_Family))/10) #640 found with html inspect when 50 prod are shown.
    }
    
    plotly::plotlyOutput("prodfam_barplot", height = paste0(size_plot,"px"))
  })
  
  ####pie plot 
  
  
  # These reactive values keep track of the drilldown state
  # (NULL means inactive)
  drills <- reactiveValues(category = NULL,
                           sub_category = NULL)
  
  # report sales by category, unless a category is chosen
  filtdata_pie <- reactive({
    if (is.null(drills$category)) {
      return(dplyr::count(data(), Model_Family))
    }
    temp = data() %>%
      dplyr::filter(Model_Family %in% drills$category) %>%
      dplyr::count(Model_type)
    
    if (is.null(drills$sub_category)) {
      return(temp)
    }
    
    data() %>%
      dplyr::filter(Model_Family %in% drills$category) %>%
      dplyr::filter(Model_type %in% drills$sub_category) %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.))) %>% 
      dplyr::count(Product_Family)
    
  })
  
  # Note that pie charts don't currently attach the label/value 
  # with the click data, but we can include as `customdata`
  output$piemodfamilyplot <- renderPlotly({
    req(filtdata_pie())
    d <- setNames(filtdata_pie(), c("labels", "values"))
    if(is.null(drills$sub_category)){
      plotly::plot_ly(d, source = "drillpie") %>%
        plotly::add_pie(labels = ~labels, values = ~values, customdata = ~labels, textinfo = 'label+value') %>%
        plotly::layout(title = if(is.null(drills$category)){"Model_Family distribution"}else{ paste("Distribution for", drills$category)})
    }else{
      
      d = d[order(-d$values),]
      d$labels = factor(d$labels, levels = rev(d$labels))
      
      plot = ggplot(d) + geom_col(aes(y = labels, x = values,fill = labels))+ 
        xlab("Number of samples")+ ylab("Product Family") + ggtitle(paste0("Sample for each Product Family in ",drills$sub_category)) + 
        theme(axis.text.y = element_text(size = 7.4))
      
      size_plot = round(84 + (640*length(d$labels))/(50 + length(d$labels)/10)) #640 found with html inspect when 50 prod are shown.
      plotly::ggplotly(plot, source = "drillpie")
    }
    
  })
  
  output$piemodfamilyplotUI = renderUI({
    req(filtdata_pie())
    d <- setNames(filtdata_pie(), c("labels", "values"))
    size_plot = round(84 + (640*length(d$labels))/(50 + length(d$labels)/10)) #640 found with html inspect when 50 prod are shown.
    if(is.null(drills$sub_category)){
      plotly::plotlyOutput("piemodfamilyplot")
    }else{
      plotly::plotlyOutput("piemodfamilyplot", height = paste0(size_plot,"px"))
    }
    
  })
  

  # update the current category when appropriate
  observeEvent(plotly::event_data("plotly_click", source = "drillpie"),{
    req(plotly::event_data("plotly_click", source = "drillpie"))
    cd <- plotly::event_data("plotly_click", source = "drillpie")$customdata[[1]]
    
    if (isTRUE(cd %in% unique(data()$Model_Family))){
      drills$category <- cd
    }
    
    if (isTRUE(cd %in% unique(data()$Model_type))){
      drills$sub_category <- cd
    }
  })
  
  output$back <- renderUI({
    if (!is.null(drills$category) && is.null(drills$sub_category)) {
      actionButton("clear", "Back", icon("chevron-left"))
    }
  })
  
  output$back1 <- renderUI({
    if (!is.null(drills$sub_category)) {
      actionButton("clear1", "Back", icon("chevron-left"))
    }
  })
  
  observeEvent(input$clear,
               drills$category <- NULL)
  observeEvent(input$clear1,
               drills$sub_category <- NULL)
  

 
  ## heatmap product family vs model type
  
  heat_informative = reactive({
    req(data())
    data_wout_cnt = data() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.))) 

    prod_table2 =  table(data_wout_cnt[,c("Product_Family","Model_type")]) %>% as.matrix() 
    
    
    unit_legend = "N of samples"

    #col annotation
    getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))
    
    annotdata_col = data_wout_cnt %>% dplyr::select(Model_type, Model_Family) %>% dplyr::distinct() %>% 
      dplyr::arrange(Model_type, colnames(prod_table2)) %>% tibble::column_to_rownames("Model_type")
    leng_col = annotdata_col %>% table() %>% length()
    colorannot_col = stats::setNames(getPalette(leng_col), c(row.names(table(annotdata_col))))
    colorannot_col = stats::setNames(list(colorannot_col), paste("Model_Family"))
    col_ha = ComplexHeatmap::HeatmapAnnotation(df = annotdata_col, which = "column", col = colorannot_col, border = TRUE)

    prod_table2 = prod_table2 %>% as.data.frame() %>% tidyr::pivot_wider(names_from = "Model_type",values_from = "Freq") %>%
      dplyr::mutate(dplyr::across(.cols = -1, ~ cut(.x, breaks=c(-Inf,0, 10, 50, 100, Inf), labels=c('0', '0-10', '10-50', '50-100', '>100')))) %>% 
      tibble::column_to_rownames("Product_Family") %>% as.matrix()
    
    ComplexHeatmap::Heatmap(prod_table2, name = unit_legend, rect_gp = grid::gpar(col = "white", lwd = 1), 
                            row_title = "Product Family", column_title = "Model type", 
                            row_names_gp = grid::gpar(fontsize = 6), column_names_gp = grid::gpar(fontsize = 8), #size testo
                            cluster_rows = FALSE, cluster_columns = FALSE, 
                            bottom_annotation = col_ha,
                            row_gap = grid::unit(2, "mm"), column_gap = grid::unit(2, "mm"), #spazio tra le divisioni
                            col = structure(c("red","orange", "blue","lightblue","white"), names = c('>100','50-100', '10-50', '0-10', '0'))
    ) %>% ComplexHeatmap::draw(merge_legend = TRUE, padding = unit(c(2, 2, 2, 5), "mm"))
  })
  
  
  observeEvent(heat_informative(),{
    InteractiveComplexHeatmap::InteractiveComplexHeatmapWidget(input, output, session, heat_informative(), output_id  = "heatmap_inform_output",
                                                               layout = "1|(2-3)", width1 = 1000, height1 = 800)
  })
  
  
  
  
  #### Query cyto ####
  
  
  dataquery_cyto = reactive({
    req(data())
    data() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.)))
  })
  
  
  observeEvent(dataquery_cyto(),{
    updateSelectInput(session, "filtmod_query_cyto", choices = c("All",unique(dataquery_cyto()$Model_type)))
    
    cols = dataquery_cyto() %>% dplyr::select(where(is.double)) %>% dplyr::select(-Dose) %>% colnames()
    updateSelectInput(session, "selcol_query_cyto", choices = cols)
    
    #2 query
    #updateSelectInput(session, "filtmod_query_cyto2", choices = c("All",unique(dataquery_cyto()$Model_type)))
    updateSelectInput(session, "selcol_query_cyto2", choices = cols)
  })
  
  
  
  #add another query
  output$checkadd2query_cyto = reactive({
    if(input$add2query_cyto %%2 == 0){
      "onequery"
    }else{"twoquery"}
  })
  outputOptions(output, "checkadd2query_cyto", suspendWhenHidden = FALSE)
  
  observeEvent(input$add2query_cyto,{
    if(input$add2query_cyto %%2 == 1){
      updateButton(session, "add2query_cyto",label = HTML("&nbsp;Remove"), style = "danger", icon("minus")) 
    }else{
      updateButton(session, "add2query_cyto", label = HTML("&nbsp;Add"), style="success", icon("plus"))
    }
  })
  
  

  
  
  query_cyto = reactive({
    req(dataquery_cyto())
    
    validate(need(input$filtmod_query_cyto, "Select something in the Model_type filtering."))
    if("All" %in% input$filtmod_query_cyto){
      data = dataquery_cyto()
      leng_modtype = length(unique(dataquery_cyto()$Model_type))
    }else{
      data = dataquery_cyto() %>% dplyr::filter(Model_type %in% input$filtmod_query_cyto)
      leng_modtype = length(input$filtmod_query_cyto)
    }
    
    temp = list()
    for(i in c("raw","summ")){
      temp[[i]] = lapply(unique(data$Model_type), function(x){
        operation_filtering(data = dplyr::filter(data, Model_type == x), 
                            column = input$selcol_query_cyto,
                            operator = input$selop_query_cyto,
                            value = input$thresh_query_cyto,
                            n_query = ifelse(input$add2query_cyto %%2 == 1, "two", "one"),
                            column2 = input$selcol_query_cyto2,
                            operator2 = input$selop_query_cyto2,
                            value2 = input$thresh_query_cyto2,
                            type_output = i
        )
      })
      
      #convert to dataframe
      temp[[i]] = data.frame(Reduce(rbind, temp[[i]]))
    }

    
    
    #temp = data.frame(Reduce(rbind, temp$summ))
    
    if(input$andor_query_cyto == "AND"){
      joined = temp$summ %>% dplyr::group_by(Product_Family) %>% dplyr::summarise(n = n()) %>% 
        dplyr::filter(n == leng_modtype) %>% dplyr::pull(Product_Family)
      
      temp = lapply(temp, function(x) x %>% dplyr::filter(Product_Family %in% joined) %>% dplyr::arrange(Product_Family))
    }
    
    return(temp)
  })
  
  output$querydt_cyto = renderDT({
    req(query_cyto())
    query_cyto()$summ
  }, selection = "single", server = FALSE, rownames = FALSE, options = list(lengthMenu = c(10, 15, 25, 50), pageLength = 15))
  
  
  
  output$query2dt_cyto = renderDT({
    req(input$querydt_cyto_rows_selected)
    
    nroww = input$querydt_cyto_rows_selected
    
    query_cyto()$raw %>% dplyr::filter(Product_Family == query_cyto()$summ[nroww,]$Product_Family,
                                       Model_type == query_cyto()$summ[nroww,]$Model_type)

  },options = list(scrollX = TRUE))
  
  ##### bubbleplot ####
  
  mod_bubble_plot_server("bubbleplot_cyto", data = data, type_data = reactive("cyto"))
  
 
  
  
  ##### heatmap ####
  
  
  data_heatmap_cyto = mod_heatmap_cyto_repo_server("heatmap_cyto", data = data, data_type = reactive("cyto"))
  
  observeEvent(data_heatmap_cyto(),{
    InteractiveComplexHeatmap::InteractiveComplexHeatmapWidget(input, output, session, data_heatmap_cyto(), output_id  = "heatmap_output_cyto",
                                                               layout = "1|(2-3)", width1 = 1000, height1 = 600)
  })
  
 
  
  ####barplot ####
  
  output$show_barplot2 = reactive({
    ifelse(input$add_barplot %%2 == 1, TRUE, FALSE)
  })
  outputOptions(output, "show_barplot2", suspendWhenHidden = FALSE)
  
  
  observeEvent(input$add_barplot,{
    if(input$add_barplot %%2 == 1){
      updateActionButton (session, "add_barplot",label = HTML("&nbsp;Remove second barplot"),icon("minus")) 
    }else{
      updateActionButton(session, "add_barplot", label = HTML("&nbsp;Add another barplot"), icon("plus"))
    }
  })
  
  
  observeEvent(data(),{
    updateSelectInput(session, "model_filt_bar", choices = unique(data()$Model_type))
    updateSelectInput(session, "model_filt_bar2", choices = unique(data()$Model_type))
    
  })
  
  observeEvent(input$model_filt_bar,{
    family = data() %>% dplyr::filter(Model_type == input$model_filt_bar) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_bar", choices = unique(family))
  })
  
  #purification
  observeEvent(c(input$model_filt_bar,input$family_filt_bar),{
    purif = dplyr::filter(data(), Model_type == input$model_filt_bar & Product_Family == input$family_filt_bar)
    updateSelectInput(session, "purif_filt_bar", choices = unique(purif$Purification), selected = unique(purif$Purification)[1])
  })
  
  output$check_multID_bar1 = reactive({
    req(data_notsumm())
    data_plot_not1 =  dplyr::filter(data_notsumm(), 
                                    Product_Family == input$family_filt_bar & Model_type == input$model_filt_bar & Purification == input$purif_filt_bar)
    ifelse(length(unique(data_plot_not1$Experiment_id)) >1, TRUE, FALSE)
  })
  outputOptions(output, "check_multID_bar1", suspendWhenHidden = FALSE)
  
  
  output$barplot = plotly::renderPlotly({
    req(data_notsumm())

    data_plot_not1 =  dplyr::filter(data_notsumm(), Product_Family == input$family_filt_bar & Model_type == input$model_filt_bar & Purification == input$purif_filt_bar)
    cnts = data_notsumm() %>% dplyr::filter(stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot_not1$Experiment_id))
    data_plot_not = rbind(data_plot_not1, cnts)
    
    if(all(c("MEKinhibitor", "DOXORUBICIN", "CISPLATIN") %in% cnts$Product)){
      level_order = c("CTRL", "MEKinhibitor", "DOXORUBICIN", "CISPLATIN", sort(unique(data_plot_not1$Product)))
    }else{
      level_order = c("CTRL", sort(unique(cnts[cnts$Product_Family == "CTRL+",]$Product)), sort(unique(data_plot_not1$Product)))
    }
    
    ### Hmisc serve per mean_sdl. è esporato da ggplot2 ma richiede il pacchetto
    plot = ggplot(data_plot_not, aes(x = factor(Product, levels = level_order), y = !!sym(input$typeeval_bar), fill = factor(Dose)))+
      coord_cartesian(ylim=c(0, 100)) + 
      geom_bar( position = position_dodge(), stat = "summary",fun = "mean") +
      stat_summary(fun.data= ggplot2::mean_sdl, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2,position = position_dodge(width = 1))+
      xlab("Product") + ggtitle(input$family_filt_bar) + labs(fill = "Dose") +
      theme(axis.text.x = element_text(angle = 315, hjust = 0, size = 10, margin=margin(t=30)),legend.title = element_blank())
    
    
    if(length(unique(data_plot_not$Experiment_id)) >1){
      showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_bar,". 
                                                  You can summarise them (Faceting expID to FALSE) or you can
                                                  facet (Faceting expID to TRUE)")), type = "default")
      if(input$facet_bar == TRUE){
        plot = plot + facet_grid(~Experiment_id, scales = "free", switch = "x")
      }
    }

    if(input$addpoints_barplot == TRUE){
      plot = plot + geom_point(position = position_dodge(width = 1))
    }
      
    plotly::ggplotly(plot)

  })
  
  
  
  ### second barplot
  observeEvent(input$model_filt_bar2,{
    family = data() %>% dplyr::filter(Model_type == input$model_filt_bar2) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_bar2", choices = unique(family))
  })
  
  
  #purification
  observeEvent(c(input$model_filt_bar2,input$family_filt_bar2),{
    purif = dplyr::filter(data(), Model_type == input$model_filt_bar2 & Product_Family == input$family_filt_bar2)
    updateSelectInput(session, "purif_filt_bar2", choices = unique(purif$Purification), selected = unique(purif$Purification)[1])
  })
  
  
  output$check_multID_bar2 = reactive({
    req(data_notsumm())
    data_plot_not1 =  dplyr::filter(data_notsumm(), 
                                    Product_Family == input$family_filt_bar2 & Model_type == input$model_filt_bar2 & Purification == input$purif_filt_bar2)
    ifelse(length(unique(data_plot_not1$Experiment_id)) >1, TRUE, FALSE)
  })
  outputOptions(output, "check_multID_bar2", suspendWhenHidden = FALSE)
  
  
  output$barplot2 = plotly::renderPlotly({
    req(data_notsumm())
    
    data_plot_not1 =  dplyr::filter(data_notsumm(), 
                                    Product_Family == input$family_filt_bar2 & Model_type == input$model_filt_bar2 & Purification == input$purif_filt_bar2)
    cnts = data_notsumm() %>% dplyr::filter(stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot_not1$Experiment_id))
    data_plot_not = rbind(data_plot_not1, cnts)

    if(all(c("MEKinhibitor", "DOXORUBICIN", "CISPLATIN") %in% cnts$Product_Family)){
      level_order = c("CTRL", "MEKinhibitor", "DOXORUBICIN", "CISPLATIN", sort(unique(data_plot_not1$Product)))
    }else{
      level_order = c("CTRL", sort(unique(cnts[cnts$Product_Family == "CTRL+",]$Product)), sort(unique(data_plot_not1$Product)))
    }
    
    plot = ggplot(data_plot_not, aes(x = factor(Product, level = level_order), y = !!sym(input$typeeval_bar2), fill = factor(Dose)))+
      coord_cartesian(ylim=c(0, 100)) + 
      geom_bar( position = position_dodge(), stat = "summary",fun = "mean") +
      stat_summary(fun.data=ggplot2::mean_sdl, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2,position = position_dodge(width = 1))+
      xlab("Product") + ggtitle(input$family_filt_bar2) + labs(fill = "Dose") +
      theme(axis.text.x = element_text(angle = 315, hjust = 0, size = 10, margin=margin(t=30)),legend.title = element_blank())
    
    
    if(length(unique(data_plot_not$Experiment_id)) >1){
      showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_bar2,". 
                                                  You can summarise them (Faceting expID to FALSE) or you can
                                                  facet (Faceting expID to TRUE)")), type = "default")
      if(input$facet_bar2 == TRUE){
        plot = plot + facet_grid(~Experiment_id, scales = "free", switch = "x")
      }
    }
    
    if(input$addpoints_barplot2 == TRUE){
      plot = plot + geom_point(position = position_dodge(width = 1))
    }
    
    plotly::ggplotly(plot)
    
  })
  

  
  
  #### Spider plot ####
  
  mod_spiderplot_server("spiderplot_cyto", data = data, type_data = reactive("cyto"))

  
######## D1 ########
  
  D1_data1 = reactiveVal(database_D1)
  
  observe({
    filepath = paste0(base::system.file(package = "ADViSEBioassay"),"/data/D1/database_updated_D1.rds")
    
    if(file.exists(filepath) == TRUE){
      D1_data1(readRDS(filepath))
    }
  })
  
  
  output$valbox_D1 = renderUI({
    if(is.null(D1_data1())){
      box(width = 12, background = "yellow",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("D1 data: "),style = "color: white"),
                     h5("No D1 data present in database!", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )
          )
      )
    }else{
      n_az = D1_data1()$mydataset$Experiment_id %>% unique() %>% length()
      box(width = 12, background = "green",
          fluidPage(
            fluidRow(
              column(9, 
                     h4(strong("D1 data: "),style = "color: white"),
                     h5(strong(n_az), " experiments.", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-check", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )
          )
      )
    }
  })
  
  
  
  
  #carica il file
  loaded_database_D1_pre = eventReactive(input$loaddatabase,{
    if(!is.null(D1_data1())){
      showNotification(tagList(icon("info"), HTML("&nbsp;D1 data loading...")), type = "default")
      return(D1_data1())
    }else{
      showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;D1 data not loaded")), type = "error")
      return(NULL)
    }
  })
  
  observeEvent(loaded_database_D1_pre(),{
    if(!is.null(loaded_database_D1_pre())){
      showNotification(tagList(icon("check"), HTML("&nbsp;D1 data loaded!")), type = "message")
    }
  })
  
  
  loaded_database_D1 = reactiveVal()
  
  observeEvent(loaded_database_D1_pre(),{
    loaded_database_D1(loaded_database_D1_pre())
  })
  
  

  
  
  #### update data D1 if present ####
  D1_from_mod = mod_load_cyto_server("load_D1_mod", data_type = reactive("D1"))
  
  
  #check data correctly loaded
  output$check_data_updated_D1 = reactive(
    return(is.null(D1_from_mod()))
  )
  outputOptions(output, "check_data_updated_D1", suspendWhenHidden = FALSE)
  
  
  output$newdata_D1_DT = renderDT({
    req(D1_from_mod())
    if(input$summ_viewtable_updated_D1 == TRUE){
      D1_from_mod()$mydataset %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
    }else{
      D1_from_mod()$myprocesseddata %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
    }
  },options = list(scrollX = TRUE))
  
  
  observeEvent(input$update_D1_bttn,{
    if(!is.null(D1_from_mod())){
      loaded_database_D1(update_database(old_data = loaded_database_D1_pre(), new_data = D1_from_mod()))
      showNotification(tagList(icon("check"), HTML("&nbsp;New data loaded! Click on 'Save database' if you want to store the changes.")), type = "message")
    }
  })
  
  
  
  ##### Upload updated
  
  #Upload
  observeEvent(input$upload_updated_D1,{
    showModal(modalDialog(
      title = "Upload an existing database (.rds)",
      footer = modalButton("Close"),
      fluidRow(
        column(8,fileInput("upload_file_D1", "Upload a database (.rds)", accept = ".rds")),
        conditionalPanel(
          condition = "output.check_fileuploaded_D1 == true",
          column(3,br(),actionButton("load_upload_file_D1", "Load!", icon("rocket"),style='padding:10px; font-size:140%; font-weight: bold;'))
        )
      )
    ))
  })
  
  
  output$check_fileuploaded_D1 <- reactive({
    return(!is.null(input$upload_file_D1))
  }) 
  outputOptions(output, 'check_fileuploaded_D1', suspendWhenHidden=FALSE)
  
  
  
  
  #import 
  observeEvent(input$load_upload_file_D1,{
    req(input$upload_file_D1)
    ext <- tools::file_ext(input$upload_file_D1$name)
    if(ext != "rds"){
      shinyWidgets::show_alert("Invalid file!", "Please upload a .rds file", type = "error")
    }
    validate(need(ext == "rds", "Invalid file! Please upload a .rds file"))
    
    file = readRDS(file = input$upload_file_D1$datapath)
    loaded_database_D1(file)
    showNotification(tagList(icon("check"), HTML("&nbsp;New database loaded! Click on 'Save database' if you want to store the changes.")), type = "message")
  })
  
  
  

  
  ##### SAVE Updated #####
  
  #check if something new is loaded otherwhise don't display the save button
  output$check_ifsave_D1 = reactive({
    checkdatabase = tryCatch({D1_from_mod()
      FALSE
    },shiny.silent.error = function(e) {TRUE})
    
    if(TRUE %in% c(!checkdatabase, input$load_upload_file_D1 > 0)){return(TRUE)}
  })
  outputOptions(output, "check_ifsave_D1", suspendWhenHidden = FALSE)
  
  
  
  observeEvent(input$save_update_D1,{
    shinyWidgets::ask_confirmation(
      inputId = "confirmsave_D1",
      type = "warning",
      title = "Do you want to save and update the internal database?",
      text = h4("Be sure that everything works before update.
      If you need to restore the original database, click on", strong("Restore Database."))
    )
  })
  
  
  observeEvent(input$confirmsave_D1,{
    checkdatabase = tryCatch({loaded_database_D1()
      FALSE
    },shiny.silent.error = function(e) {TRUE})
    
    
    if(input$confirmsave_D1 == TRUE && checkdatabase == FALSE){
      if(dim(loaded_database_D1_pre()$exp_list)[1] == dim(loaded_database_D1()$exp_list)[1]){
        showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;The internal database is already updated.")), type = "error")
      }else{
        filepath = paste0(base::system.file(package = "ADViSEBioassay"),"/data/D1/")
        if(dir.exists(filepath) == FALSE){
          dir.create(filepath)
        }
        
        saveRDS(loaded_database_D1(), file = paste0(filepath,"database_updated_D1.rds"))
        
        #versione per cronologia
        filever = paste0(filepath,"ver_",Sys.Date(),".rds")
        saveRDS(loaded_database_D1(), file = filever)
        
        show_alert(
          title = "Upload completed!",
          text = "Please restart ADViSEBioassay to save the changes.",
          type = "success"
        ) 
      }
    }
  })
  
  
  
  
  ##### download updated D1 ######
  
  #check all data correctly loaded. If FALSE -> error.
  output$checkupdated_D1_fordownload = reactive({
    checkdatabase = tryCatch({D1_from_mod()
      FALSE
    },shiny.silent.error = function(e) {TRUE})
    check_file = file.exists(paste0(base::system.file(package = "ADViSEBioassay"),"/data/D1/database_updated_D1.rds"))
    if(TRUE %in% c(!checkdatabase, check_file)){return(TRUE)}
  })
  outputOptions(output, "checkupdated_D1_fordownload", suspendWhenHidden = FALSE)
  
  
  
  #### Download handler for the download button
  output$download_updated_D1 <- downloadHandler(
    #put the file name with also the file extension
    filename = function() {
      paste0("updated_D1_", Sys.Date(), ".rds")
    },
    
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      saveRDS(loaded_database_D1(), file)
    }
  )
  
  
  
  
  #### remove updated D1 #####
  
  
  #modal restore
  observeEvent(input$remove_update_D1,{
    showModal(modalDialog(
      title = "Restore database",
      footer = modalButton("Close"),
      size = "l",
      fluidRow(
        column(
          6,style = "text-align:center;",
          box(width = NULL, status = "primary", title = "Restore database to a previous version", solidHeader = TRUE,
              column(10,offset = 1, selectInput("ver_databases_D1", "Select a database", choices = "")),
              conditionalPanel(
                condition = "input.ver_databases_D1 != ''",
                actionButton("load_version_D1", HTML("&nbsp;Load version"),icon("rotate-left"), style = "background: #e74c3c; border-color: #e74c3c;padding:10px; font-size:120%; font-weight: bold;"),
              )
          )
          
        ),
        column(
          6,style = "text-align:center;",
          box(width = NULL, status = "primary", title = "Reset database to the original internal database", solidHeader = TRUE,
              br(),
              actionButton("rest_to_original_D1", HTML("&nbsp;Reset database"),icon("trash-can"), style = "background: #e74c3c; border-color: #e74c3c;padding:10px; font-size:120%; font-weight: bold;"),
              br(),br(), br()
          ))
      )
    ))
  })
  
  
  
  
  #### LOAD OLD VERSION
  
  observeEvent(input$remove_update_D1,{
    files = list.files(paste0(base::system.file(package = "ADViSEBioassay"),"/data/D1/"))
    files = grep("ver_",files, value = TRUE)
    updateSelectInput(session, "ver_databases_D1", choices = files)
  })
  
  
  observeEvent(input$load_version_D1,{
    shinyWidgets::ask_confirmation(
      inputId = "confirmload_ver_D1",
      type = "warning",
      title = "Do you want to load this version?",
      text = h4("By clicking yes, this database version will be restored. Please restart ADViSEBioassay to apply this change.")
    )
  })
  
  
  observeEvent(input$confirmload_ver_D1,{
    filepath_ver = paste0(base::system.file(package = "ADViSEBioassay"),"/data/D1/",input$ver_databases_D1)
    
    if(isTRUE(input$confirmload_ver_D1)){
      if(file.exists(filepath_ver)){    
        file = readRDS(filepath_ver)
        filepath = paste0(base::system.file(package = "ADViSEBioassay"),"/data/D1/database_updated_D1.rds")
        saveRDS(file, file = filepath)
        showNotification(tagList(icon("check"), HTML("&nbsp;Previous version restored!")), type = "message")
      }else{
        showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;This file version doesn't exist.")), type = "error")
      }
    }
  })
  
  
  
  
  ##### RESTORE
  observeEvent(input$rest_to_original_D1,{
    shinyWidgets::ask_confirmation(
      inputId = "confirmremove_D1",
      type = "warning",
      title = "Do you want to restore the internal database?",
      text = h4("By clicking yes, the original database will be restored. Please restart ADViSEBioassay to apply this change.")
    )
  })
  
  
  observeEvent(input$confirmremove_D1,{
    
    filepath = paste0(base::system.file(package = "ADViSEBioassay"),"/data/D1/")
    listf = list.files(filepath)
    #listf = listf[!grepl("database_D1.rda", listf)]
    file_to_rem = sapply(listf, function(x) paste0(filepath,x))
    
    if(isTRUE(input$confirmremove_D1)){
      if(TRUE %in% file.exists(file_to_rem)){
        file.remove(file_to_rem)
        showNotification(tagList(icon("check"), HTML("&nbsp;Original database restored!")), type = "message")
      }else{
        showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;Updated database already removed.")), type = "error")
      }
    }
    
  })
  
  
  
  
  
  
  
  
  #load data
  data_notsumm_D1 = reactive({
    req(loaded_database_D1())
    loaded_database_D1()$myprocesseddata
  })
  
  data_D1 = reactive({
    req(loaded_database_D1())
    loaded_database_D1()$mydataset
    #summarise_cytoxicity(data_notsumm_D1(), group = c("Experiment_id","Model_type", "Product", "Product_Family","Dose", "Purification"), method = "d1")
  })
  
  
  exp_list_D1 = reactive({
    req(loaded_database_D1())
    loaded_database_D1()$exp_list
  })
  
  
  #check data correctly loaded
  output$check_data_D1 = reactive(
    return(is.null(data_D1()))
  )
  outputOptions(output, "check_data_D1", suspendWhenHidden = FALSE)
  
  
  output$dtdata_D1 = renderDT({
    req(data_D1())
    if(input$summ_viewtable_D1 == TRUE){
      data_D1() %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
    }else{
      data_notsumm_D1() %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
    }
  },options = list(scrollX = TRUE))
  

  
  ##### informative plots D1######
  
  output$countbarplot_D1 = plotly::renderPlotly({
    req(data_D1(), exp_list_D1())
    
    count = data_D1() %>% dplyr::select(where(is.character)) %>% dplyr::mutate(across(where(is.character), ~length(unique(.x))))
    count = t(count[1,])
    colnames(count) = "Count"
    count = count %>% as.data.frame() %>% tibble::rownames_to_column("Measure")
    count$Var = count$Measure
    
    inst = as.data.frame(table(exp_list_D1()$Instrument))
    inst$Measure = "Instrument"
    scanint = rbind(inst)
    colnames(scanint) = c("Var", "Count","Measure")
    
    count = rbind(count,scanint)
    count$Measure = factor(count$Measure, levels = unique(count$Measure))
    count$Var = factor(count$Var, levels = unique(count$Var))
    temp = ggplot(count, aes(x = Measure, y = Count, group = Var)) + geom_col(aes(fill = Var)) + 
      geom_text(aes(label=Count),position = position_stack(vjust = 0.5)) + ggtitle("Data overview") + labs(x = "Measure", fill = "Measure", y = "Total numbers") + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0))
    
    plotly::ggplotly(temp)
    
  })
  
  
  output$prodfam_barplot_D1 = plotly::renderPlotly({
    req(data_D1())
    
    prod_table =  as.data.frame(table(data_D1()$Product_Family))
    colnames(prod_table)[1] = "Product_Family"
    
    prod_table = prod_table[order(-prod_table$Freq),]
    prod_table$Product_Family = factor(prod_table$Product_Family, levels = rev(prod_table$Product_Family))
    
    
    if(input$first50_prodfam_D1 == TRUE && length(unique(data_D1()$Product_Family)) > 50){
      prod_table = prod_table[1:50,]
    }
    
    temp = ggplot(prod_table) + geom_col(aes(y = Product_Family, x = Freq, fill = Product_Family))+ 
      xlab("Number of samples")+ ylab("Product Family") + ggtitle("Sample for each Product Family") + 
      theme(axis.text.y = element_text(size = 7.4))
    
    plotly::ggplotly(temp, tooltip = c("x", "fill"))
  })
  
  output$prodfam_barplotUI_D1 = renderUI({
    
    if(input$first50_prodfam_D1 == TRUE || length(unique(data_D1()$Product_Family)) < 50){
      size_plot = 84 + 640
    }else{
      size_plot = 84 + (640*length(unique(data_D1()$Product_Family)))/(50 + length(unique(data_D1()$Product_Family))/10) #640 found with html inspect when 50 prod are shown.
    }
    
    plotly::plotlyOutput("prodfam_barplot_D1", height = paste0(size_plot,"px"))
  })
  
  
  
  ##### Query D1 #####
  
  
  dataquery_D1 = reactive({
    req(data_D1())
    data_D1() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.)))
  })
  

  
  observeEvent(dataquery_D1(),{

    cols = dataquery_D1() %>% dplyr::select(where(is.double) & -starts_with(c("Cyto", "Vita")) & -ends_with("CV"),-Dose) %>% colnames()
    updateSelectInput(session, "selcol_query_d1", choices = cols)
    
    #2 query
    cols2 = dataquery_D1() %>% dplyr::select(starts_with(c("Cyto", "Vita")) & -ends_with(c("sd",".nreps"))) %>% colnames()
    updateSelectInput(session, "selcol_query_d12", choices = cols2)
  })
  
  
  #add another query
  output$checkadd2query_D1 = reactive({
    if(input$add2query_d1 %%2 == 0){
      "onequery"
    }else{"twoquery"}
  })
  outputOptions(output, "checkadd2query_D1", suspendWhenHidden = FALSE)
  
  observeEvent(input$add2query_d1,{
    if(input$add2query_d1 %%2 == 1){
      updateButton(session, "add2query_d1",label = HTML("&nbsp;Remove"), style = "danger", icon("minus")) 
    }else{
      updateButton(session, "add2query_d1", label = HTML("&nbsp;Add"), style="success", icon("plus"))
    }
  })
  
  
  query_D1 = reactive({
    req(dataquery_D1())
    validate(need(input$selcol_query_d1, "Select something in the MFI selection."))

    mydataset_D1 = dataquery_D1()
    cnt_D1 <- data_D1() %>% dplyr::filter(if_any("Product_Family", ~grepl("CTRL",.)))
    
    temp = NULL
    for(i in input$selcol_query_d1){
      temp[[i]] = lapply(unique(mydataset_D1$Product_Family), function(x){
        data = dplyr::filter(mydataset_D1, Product_Family == x)
        if(length(unique(data$Purification)) >1){
          #if there are multiple purification, we have to check for each purification
          lapply(unique(data$Purification), function(k){
            data2 = data %>% dplyr::filter(Purification == k)
            cnt2 = cnt_D1 %>% dplyr::filter(Experiment_id %in% unique(data2$Experiment_id) & Product == "CTRL") %>% as.data.frame()
            if(input$selop_query_d1 == "greater than"){
              data2 %>% dplyr::filter(get(i) > mean(cnt2[,i])*input$thresh_query_d1)
            }else{
              data2 %>% dplyr::filter(get(i) >= mean(cnt2[,i])*input$thresh_query_d1)
            }
            
          }) %>% {Reduce(rbind, .)} #dato che %>% assegna come primo posto, uso {} e metto il . per la posizione.
          
        }else{
          cnt = cnt_D1 %>% dplyr::filter(Experiment_id %in% unique(data$Experiment_id) & Product == "CTRL") %>% as.data.frame()
          if(input$selop_query_d1 == "greater than"){
            data %>% dplyr::filter(get(i) > mean(cnt[,i])*input$thresh_query_d1)
          }else{
            data %>% dplyr::filter(get(i) >= mean(cnt)*input$thresh_query_d1)
          }
        }
      })
      
      temp[[i]] = data.frame(Reduce(rbind, temp[[i]]))
    }
    if(length(input$selcol_query_d1) > 1){
      if(input$andor_query_d1 == "AND"){
        raw = Reduce(intersect, temp)
      }else{
        raw = Reduce(rbind, temp) %>% dplyr::distinct()
      }
      
    }else{
      raw = temp[[input$selcol_query_d1]]
    }
    if(input$add2query_d1 %%2 == 1){
      raw = operation_filtering(data = raw,
                                column = input$selcol_query_d12,
                                operator = input$selop_query_d12,
                                value = input$thresh_query_d12,
                                n_query = "one",
                                type_output = "raw")
    }
    
    
    summ = raw %>% group_by(Model_type, Product_Family) %>% dplyr::summarise(n_products = n())
    
    list(raw = raw, summ = summ)
  })
  
  
  
  ### datatables
  output$querydt_D1 = renderDT({
    req(query_D1())
    query_D1()$summ
  }, selection = "single", server = FALSE, rownames = FALSE, options = list(lengthMenu = c(10, 15, 25, 50), pageLength = 15))
  
  
  
  output$query2dt_D1 = renderDT({
    req(input$querydt_D1_rows_selected)
    
    nroww = input$querydt_D1_rows_selected
    
    query_D1()$raw %>% dplyr::filter(Product_Family == query_D1()$summ[nroww,]$Product_Family,
                                       Model_type == query_D1()$summ[nroww,]$Model_type)
    
  },options = list(scrollX = TRUE))
  

  
  
  ####barplot D1 ####
  
  observeEvent(data_notsumm_D1(),{
    updateSelectInput(session, "typeeval_bar_D1", choices = colnames(dplyr::select(data_notsumm_D1(), where(is.double),-Dose)))
    updateSelectInput(session, "typeeval_bar2_D1", choices = colnames(dplyr::select(data_notsumm_D1(), where(is.double),-Dose)))
  })
  
  
  output$show_barplot2_D1 = reactive({
    ifelse(input$add_barplot_D1 %%2 == 1, TRUE, FALSE)
  })
  outputOptions(output, "show_barplot2_D1", suspendWhenHidden = FALSE)
  
  
  observeEvent(input$add_barplot_D1,{
    if(input$add_barplot_D1 %%2 == 1){
      updateActionButton(session, "add_barplot_D1",label = HTML("&nbsp;Remove second barplot"),icon("minus")) 
    }else{
      updateActionButton(session, "add_barplot_D1", label = HTML("&nbsp;Add another barplot"), icon("plus"))
    }
  })
  
  
  observeEvent(data_D1(),{
    updateSelectInput(session, "model_filt_bar_D1", choices = unique(data_D1()$Model_type))
    updateSelectInput(session, "model_filt_bar2_D1", choices = unique(data_D1()$Model_type))
    
  })
  
  observeEvent(input$model_filt_bar_D1,{
    family = data_D1() %>% dplyr::filter(Model_type == input$model_filt_bar_D1) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_bar_D1", choices = unique(family))
  })
  
  #purification
  observeEvent(c(input$model_filt_bar_D1,input$family_filt_bar_D1),{
    purif = dplyr::filter(data_D1(), Model_type == input$model_filt_bar_D1 & Product_Family == input$family_filt_bar_D1)
    updateSelectInput(session, "purif_filt_bar_D1", choices = unique(purif$Purification), selected = unique(purif$Purification)[1])
  })
  
  
  output$check_multID_bar1_D1 = reactive({
    req(data_notsumm_D1())
    data_plot_not1 =  dplyr::filter(data_notsumm_D1(), 
                                    Product_Family == input$family_filt_bar_D1 & Model_type == input$model_filt_bar_D1 & Purification == input$purif_filt_bar_D1)
    ifelse(length(unique(data_plot_not1$Experiment_id)) >1, TRUE, FALSE)
  })
  outputOptions(output, "check_multID_bar1_D1", suspendWhenHidden = FALSE)
  
  
  output$barplot_D1 = plotly::renderPlotly({
    req(data_notsumm_D1())
    
    data_plot_not1 =  dplyr::filter(data_notsumm_D1(), 
                                    Product_Family == input$family_filt_bar_D1 & Model_type == input$model_filt_bar_D1 & Purification == input$purif_filt_bar_D1)
    cnts = data_notsumm_D1() %>% dplyr::filter(stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot_not1$Experiment_id))
    data_plot_not = rbind(data_plot_not1, cnts)
    
    if(all(c("LPS") %in% cnts$Product)){
      level_order = c("CTRL", "LPS", sort(unique(data_plot_not1$Product)))
    }else{
      level_order = c("CTRL", sort(unique(cnts[cnts$Product_Family == "CTRL+",]$Product)), sort(unique(data_plot_not1$Product)))
    }
    

    plot = ggplot(data_plot_not, aes(x = factor(Product, level = level_order), y = !!sym(input$typeeval_bar_D1), fill = factor(Dose)))+
      coord_cartesian(ylim=c(0, 100)) + 
      geom_bar(position = position_dodge(), stat = "summary",fun = "mean") +
      stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2,position = position_dodge(width = 1))+
      xlab("Product") + ggtitle(input$family_filt_bar_D1) + labs(fill = "Dose") +
      theme(axis.text.x = element_text(angle = 315, hjust = 0, size = 10, margin=margin(t=30)),legend.title = element_blank())
    
    
    if(length(unique(data_plot_not$Experiment_id)) >1){
      showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_bar_D1,". 
                                                  You can summarise them (Faceting expID to FALSE) or you can
                                                  facet (Faceting expID to TRUE)")), type = "default")
      if(input$facet_bar_D1 == TRUE){
        plot = plot + facet_grid(~Experiment_id, scales = "free", switch = "x")
      }
    }
    
    if(input$addpoints_barplot_D1 == TRUE){
      plot = plot + geom_point(position = position_dodge(width = 1))
    }
    
    plotly::ggplotly(plot)
    
  })
  
  
  
  ### second barplot
  observeEvent(input$model_filt_bar2_D1,{
    family = data_D1() %>% dplyr::filter(Model_type == input$model_filt_bar2_D1) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_bar2_D1", choices = unique(family))
  })
  
  #purification
  observeEvent(c(input$model_filt_bar2_D1,input$family_filt_bar2_D1),{
    purif = dplyr::filter(data_D1(), Model_type == input$model_filt_bar2_D1 & Product_Family == input$family_filt_bar2_D1)
    updateSelectInput(session, "purif_filt_bar2_D1", choices = unique(purif$Purification), selected = unique(purif$Purification)[1])
  })
  
  output$check_multID_bar2_D1 = reactive({
    req(data_notsumm_D1())
    data_plot_not1 =  dplyr::filter(data_notsumm_D1(), 
                                    Product_Family == input$family_filt_bar2_D1 & Model_type == input$model_filt_bar2_D1 & Purification == input$purif_filt_bar2_D1)
    ifelse(length(unique(data_plot_not1$Experiment_id)) >1, TRUE, FALSE)
  })
  outputOptions(output, "check_multID_bar2_D1", suspendWhenHidden = FALSE)
  
  
  output$barplot2_D1 = plotly::renderPlotly({
    req(data_notsumm_D1())
    
    data_plot_not1 =  dplyr::filter(data_notsumm_D1(), 
                                    Product_Family == input$family_filt_bar2_D1 & Model_type == input$model_filt_bar2_D1 & Purification == input$purif_filt_bar2_D1)
    cnts = data_notsumm_D1() %>% dplyr::filter(stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot_not1$Experiment_id))
    data_plot_not = rbind(data_plot_not1, cnts)

    if(all(c("LPS") %in% cnts$Product_Family)){
      level_order = c("CTRL", "LPS", sort(unique(data_plot_not1$Product)))
    }else{
      level_order = c("CTRL", sort(unique(cnts[cnts$Product_Family == "CTRL+",]$Product)), sort(unique(data_plot_not1$Product)))
    }
    
    plot = ggplot(data_plot_not, aes(x = factor(Product, level = level_order), y = !!sym(input$typeeval_bar2_D1), fill = factor(Dose)))+
      coord_cartesian(ylim=c(0, 100)) + 
      geom_bar(position = position_dodge(), stat = "summary",fun = "mean") +
      stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2,position = position_dodge(width = 1))+
      xlab("Product") + ggtitle(input$family_filt_bar2_D1) + labs(fill = "Dose") +
      theme(axis.text.x = element_text(angle = 315, hjust = 0, size = 10, margin=margin(t=30)),legend.title = element_blank())
    
    
    if(length(unique(data_plot_not$Experiment_id)) >1){
      showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_bar2_D1,". 
                                                  You can summarise them (Faceting expID to FALSE) or you can
                                                  facet (Faceting expID to TRUE)")), type = "default")
      if(input$facet_bar2_D1 == TRUE){
        plot = plot + facet_grid(~Experiment_id, scales = "free", switch = "x")
      }
    }
    
    if(input$addpoints_barplot2_D1 == TRUE){
      plot = plot + geom_point(position = position_dodge(width = 1))
    }
    
    plotly::ggplotly(plot)
    
  })
  
  ##### spiderplot d1 ####
  
  mod_spiderplot_server("spiderplot_D1", data = data_D1, type_data = reactive("D1"))
  
  ##### bubbleplot d1 #####
  mod_bubble_plot_server("bubbleplot_D1", data = data_D1, type_data = reactive("D1"))
  
  
  ##### heatmap D1 ####
  
  
  prod_total_D1 = reactive({
    req(data_notsumm_D1())
    data_notsumm_D1() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.)))
  })
  
  
  observeEvent(prod_total_D1(),{
    updateSelectInput(session, "prodfam_heat_D1", choices = unique(prod_total_D1()$Product_Family))
  })

  
  # DATA STORAGE
  values_comb_heat_D1 <- reactiveValues(
    comb = NULL # original data
  )



  #purification filtering
  observeEvent(input$prodfam_heat_D1,{
    doses = prod_total_D1() %>% dplyr::filter(Product_Family == input$prodfam_heat_D1)
    updateSelectInput(session, "purif_filt_heat_D1", choices = unique(doses$Purification), selected = unique(doses$Purification)[1])
  })



  observeEvent(c(input$prodfam_heat_D1, input$purif_filt_heat_D1),{
    req(input$prodfam_heat_D1)
    req(input$purif_filt_heat_D1)
    filt = dplyr::filter(prod_total_D1(), Product_Family == input$prodfam_heat_D1) %>%
      dplyr::filter(Purification == input$purif_filt_heat_D1)
    values_comb_heat_D1$comb <- rev(unique(filt$Dose))
    updateRadioButtons(session, "filt_dose_D1", choices = c("All",unique(filt$Dose)),inline = TRUE) #c("All",
  })

  observeEvent(input$revdose_heat_D1,{
    values_comb_heat_D1$comb <- rev(values_comb_heat_D1$comb)
  })

  observeEvent(values_comb_heat_D1$comb,{
    req(values_comb_heat_D1$comb)
    if(!is.null(values_comb_heat_D1$comb)){
      combin = utils::combn(values_comb_heat_D1$comb, 2, paste, collapse = '-')
      updateSelectInput(session, "subdose_heatmap_D1", choices = combin)
    }
  })





  dataheat_D1 = reactive({
    req(data_notsumm_D1())

    vitaorcyto = ifelse(input$typeeval_heat_D1 == "Vitality", "Cytotoxicity", "Vitality")


    CBC150_D1 = data_notsumm_D1() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.))) %>%
      dplyr::filter(Product_Family == input$prodfam_heat_D1) %>%
      dplyr::filter(Purification == input$purif_filt_heat_D1)


    filt_cnt = data_notsumm_D1() %>% dplyr::filter(if_any("Product_Family", ~grepl("CTRL",.))) %>%
      dplyr::filter(Experiment_id %in% unique(CBC150_D1$Experiment_id)) %>%
      tidyr::unite("Product", Product, Dose, sep = " ")

    #if filter
    if(input$dose_op_heatmap_D1 == "filter"){


      cbc_filtered = split(CBC150_D1, f = ~Dose) %>%
        lapply( function(x) dplyr::bind_rows(x, filt_cnt) %>%
                  dplyr::group_by(across(-c(where(is.numeric), Well, Tec_Replicate))) %>%
                  dplyr::summarise(across(where(is.double) & !Dose, mean, na.rm = T)) %>%
                  dplyr::ungroup())

      cbc_filtered = lapply(cbc_filtered, function(x){
        if(length(unique(x$Experiment_id)) > length(unique(x$Model_type))){
          showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                                 Duplicated will be averaged.")), type = "default")
          x %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric)))) %>%
            dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup() %>% dplyr::select(Product, where(is.numeric)) %>% tibble::column_to_rownames("Product") %>%
            dplyr::select(-dplyr::all_of(vitaorcyto))
        }else{x %>% dplyr::select(Product, where(is.numeric)) %>% tibble::column_to_rownames("Product") %>%
            dplyr::select(-dplyr::all_of(vitaorcyto))}
      })

    }else if(input$dose_op_heatmap_D1 == "mean"){
      #if mean
      cbc_filtered = CBC150_D1 %>% dplyr::bind_rows(filt_cnt) %>%
        dplyr::group_by(across(-c(where(is.numeric), Well, Tec_Replicate))) %>%
        dplyr::summarise(across(where(is.double) & !Dose, mean, na.rm = T)) %>% dplyr::ungroup()


      if(length(unique(cbc_filtered$Experiment_id)) > length(unique(cbc_filtered$Model_type))){
        showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                                 Duplicated will be averaged.")), type = "default")
        cbc_filtered = cbc_filtered %>% dplyr::group_by(across(Product)) %>%
          dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
      }

      cbc_filtered = cbc_filtered %>% dplyr::select(Product, where(is.numeric)) %>% tibble::column_to_rownames("Product") %>%
        dplyr::select(-dplyr::all_of(vitaorcyto))

    }else{
      #if subtract
      combination = strsplit(input$subdose_heatmap_D1, "-")

      cbc_filtered = CBC150_D1 %>% dplyr::group_by(across(-c(where(is.numeric), Well, Tec_Replicate)), Dose) %>%
        dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>%
        dplyr::ungroup()


      if(length(unique(cbc_filtered$Experiment_id)) > length(unique(cbc_filtered$Model_type))){
        showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for the same Model_type.
                                                    Duplicated will be averaged.")), type = "default")
        cbc_filtered = cbc_filtered %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric))), Dose) %>%
          dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
      }


      cbc_filtered = cbc_filtered %>% dplyr::group_by(across(-c(where(is.numeric)))) %>%
        dplyr::summarise(across(where(is.double), ~ .x[Dose == as.numeric(combination[[1]][1])] - .x[Dose == as.numeric(combination[[1]][2])], na.rm = T)) %>%
        dplyr::ungroup()

      filt_cnt_summ =  filt_cnt %>% dplyr::group_by(across(-c(where(is.numeric), Well, Tec_Replicate))) %>%
        dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>%
        dplyr::ungroup()

      if(length(unique(filt_cnt_summ$Experiment_id)) > length(unique(filt_cnt_summ$Model_type))){
        filt_cnt_summ = filt_cnt_summ %>% dplyr::group_by(across(-c(Experiment_id, where(is.numeric)))) %>%
          dplyr::summarise(across(where(is.double), mean, na.rm = T)) %>% dplyr::ungroup()
      }

      cbc_filtered = cbc_filtered %>% dplyr::bind_rows(filt_cnt_summ) %>% dplyr::select(Product, where(is.numeric),-Dose) %>% tibble::column_to_rownames("Product") %>%
        dplyr::select(-dplyr::all_of(vitaorcyto))

    }

    cbc_filtered
  })

  ##### Custom legend values
  
  markers_heat_D1 = reactive({
    req(dataheat_D1())
    
    if(is.data.frame(dataheat_D1())){
      markers_data = dataheat_D1() %>% dplyr::select(-dplyr::any_of(c("Vitality", "Cytotoxicity")))
    }else{
      markers_data = dataheat_D1()[[1]] %>% dplyr::select(-dplyr::any_of(c("Vitality", "Cytotoxicity")))
    }
  })
  
  
  observeEvent(input$logheat_D1,{
    if(input$logheat_D1 == TRUE){
      if(all(markers_heat_D1() > 0) == FALSE){
        showNotification(type = "warning", duration = 7, tagList(icon("circle-exclamation"), 
                                 HTML("&nbsp;Warning! There are some negative values. The log scale will return some NAs.")))
      }
    }
  })
  
  
  output$uicustom_scale_heat_D1 = renderUI({
    req(markers_heat_D1())
    markers_data = markers_heat_D1()
    
    digitround = 0
    
    if(input$logheat_D1 == TRUE){
      markers_data <- log2(markers_data)
      digitround = 1
    }
    rangeui = list()
    for(i in colnames(markers_data)){
      rangeui[[i]] <- shinyWidgets::numericRangeInput(paste0("range_",i), label = paste0("Range of ", i), 
                                                       value = c(round(min(markers_data[,i], na.rm = TRUE),digits = digitround), 
                                                                 round(max(markers_data[,i], na.rm = TRUE), digits = digitround)))
    }
    return(rangeui)
  })
  
  
  #### Color pickers
  output$colormark_ui_heat_D1 = renderUI({
    req(markers_heat_D1())
    
    markers_data = markers_heat_D1()

    default_colors =  RColorBrewer::brewer.pal(8, "Dark2")[-3]
    index_color = 1
    
    colorsui = list()
    for(i in colnames(markers_data)){
      colorsui[[i]] = colorPickr(paste0("color_",i), label = paste0("Color of ", i),
        theme = "monolith", selected = default_colors[index_color])
      
      index_color = index_color+1
    }
    return(colorsui)
  })
  
  
  
  heatmap_D1 = eventReactive(input$makeheatmap_D1,{
    req(markers_heat_D1())
    cbc_filtered = dataheat_D1()
    

    markers = colnames(markers_heat_D1())
    
    #custom range
    custom_ranges = list()
    if(input$scale_type_heat_D1 == "Custom"){
      for(i in markers){
        custom_ranges[[i]] = data.frame(min = input[[paste0("range_",i)]][1], max = input[[paste0("range_",i)]][2])
      }
    }
    
    #custom colors
    custom_colors = NULL
    if(input$custom_color_heat_D1 == TRUE){
      for(i in markers){
        custom_colors[[i]] = data.frame(color = input[[paste0("color_",i)]][1])
      }
    }

    
    if(input$dose_op_heatmap_D1 == "filter"){
      
      if(input$filt_dose_D1 == "All"){
        cnts = cbc_filtered[[1]] %>% tibble::rownames_to_column("Product") %>% dplyr::filter(!stringr::str_detect(Product, "CBC")) %>% 
          tibble::column_to_rownames("Product")
        
        for(i in names(cbc_filtered)){
          cbc_filtered[[i]] = cbc_filtered[[i]] %>% tibble::rownames_to_column("Product") %>% dplyr::filter(stringr::str_detect(Product, "CBC")) %>% 
            dplyr::mutate(Product = paste0(Product, "-",i)) %>%  tibble::column_to_rownames("Product")
        }
        cbc_filtered = list("All"= rbind(Reduce(rbind,cbc_filtered),cnts))
      }
     
      
      doses = as.character(input$filt_dose_D1)

      ht = make_heatmap_D1(data = cbc_filtered[[doses]],
                           type = input$dose_op_heatmap_D1,
                           row_dend = input$rowdend_D1,
                           row_nclust = input$sliderrowheat_D1,
                           dist_method = input$seldistheat_D1,
                           clust_method = input$selhclustheat_D1,
                           add_values = input$show_valheat_D1,
                           logscale = input$logheat_D1,
                           scale_type = input$scale_type_heat_D1,
                           custom_scale = custom_ranges,
                           custom_colors = custom_colors)
      
      col_tit = ifelse(doses == "All", "All doses (ug/ml)", paste(doses, "ug/ml"))

      ComplexHeatmap::draw(ht, padding = grid::unit(c(2,2,2,15), "mm"), ht_gap = grid::unit(1, "cm"),
        column_title  = col_tit,column_title_gp = grid::gpar(fontsize = 18))

    }else if(input$dose_op_heatmap == "mean"){
      ht = make_heatmap_D1(data = cbc_filtered,
                           type = input$dose_op_heatmap_D1,
                           row_dend = input$rowdend_D1,
                           row_nclust = input$sliderrowheat_D1,
                           dist_method = input$seldistheat_D1,
                           clust_method = input$selhclustheat_D1,
                           add_values= input$show_valheat_D1,
                           logscale = input$logheat_D1,
                           scale_type = input$scale_type_heat_D1,
                           custom_scale = custom_ranges,
                           custom_colors = custom_colors)

      ComplexHeatmap::draw(ht, padding = grid::unit(c(2,2,2,15), "mm"), ht_gap = grid::unit(1, "cm"))
    }else{
      ht = make_heatmap_D1(data = cbc_filtered,
                           type = input$dose_op_heatmap_D1,
                           row_dend = input$rowdend_D1,
                           row_nclust = input$sliderrowheat_D1,
                           dist_method = input$seldistheat_D1,
                           clust_method = input$selhclustheat_D1,
                           add_values= input$show_valheat_D1,
                           logscale = input$logheat_D1,
                           scale_type = input$scale_type_heat_D1,
                           custom_scale = custom_ranges,
                           custom_colors = custom_colors)
      ComplexHeatmap::draw(ht, merge_legend = TRUE, padding = grid::unit(c(2,2,2,15), "mm"), ht_gap = grid::unit(1, "cm"))
    }
  })


  observeEvent(heatmap_D1(),{
    InteractiveComplexHeatmap::InteractiveComplexHeatmapWidget(input, output, session, heatmap_D1(), output_id  = "heatmap_D1_output",
                                                               layout = "1|(2-3)", width1 = 650, height1 = 700)
  })


  
  
  ###### Reporter ########
  
  repo_data1 = reactiveValues(
    trem2 = database_trem2,
    seap = database_seap
    #trem2 = if(file.exists(paste0(base::system.file(package = "ADViSEBioassay"),"/data/database_trem2.rda"))) database_trem2 else{NULL},
    #seap = if(file.exists(paste0(base::system.file(package = "ADViSEBioassay"),"/data/database_seap.rda"))) database_seap else{NULL}
  )

  observe({
    filepath_trem2 = paste0(base::system.file(package = "ADViSEBioassay"),"/data/reporter/TREM2/database_updated_TREM2.rds")
    
    if(file.exists(filepath_trem2) == TRUE){
      repo_data1$trem2 = readRDS(filepath_trem2)
    }
    
    filepath_seap = paste0(base::system.file(package = "ADViSEBioassay"),"/data/reporter/SEAP/database_updated_SEAP.rds")
    if(file.exists(filepath_seap) == TRUE){
      repo_data1$seap = readRDS(filepath_seap)
    }
  })
  

  output$valbox_repo = renderUI({
    if(all(is.null(repo_data1$trem2), is.null(repo_data1$seap))){
      box(width = 12, background = "yellow",
          fluidPage(
            fluidRow(
              column(9,
                     h4(strong("Reporter data: "),style = "color: white"),
                     h5("No Reporter data present in database!", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )
          )
      )
    }else{
      ntrem = repo_data1$trem2$mydataset$Experiment_id %>% unique() %>% length()
      nseap = repo_data1$seap$mydataset$Experiment_id %>% unique() %>% length()
      box(width = 12, background = "green",
          fluidPage(
            fluidRow(
              column(9,
                     h4(strong("Reporter data: "),style = "color: white"),
                     h5(strong("TREM2: ", ntrem), " experiments.", style = "color: white"),
                     h5(strong("SEAP: ", nseap), " experiments.", style = "color: white")),
              column(3, style = "padding-right: 0px; text-align: right;",
                     tags$i(class = "fas fa-check", style="font-size: 50px;padding-top: 5px;padding-right: 15px;"))
            )
          )
      )
    }
  })
  
  

  ######carica il file TREM2
  loaded_database_trem2 = eventReactive(input$loaddatabase,{
    if(!is.null(repo_data1$trem2)){
      showNotification(tagList(icon("info"), HTML("&nbsp;TREM2 data loading...")), type = "default")
      return(repo_data1$trem2)
    }else{
      showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;TREM2 data not loaded")), type = "error")
      return(NULL)
    }
  })

  observeEvent(loaded_database_trem2(),{
    if(!is.null(loaded_database_trem2())){
      showNotification(tagList(icon("check"), HTML("&nbsp;TREM2 data loaded!")), type = "message")
    }
  })



  ##### carica il file SEAP
  loaded_database_seap = eventReactive(input$loaddatabase,{
    if(!is.null(repo_data1$seap)){
      showNotification(tagList(icon("info"), HTML("&nbsp;TREM2 data loading...")), type = "default")
      return(repo_data1$seap)
    }else{
      showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;TREM2 data not loaded")), type = "error")
      return(NULL)
    }
  })
  
  observeEvent(loaded_database_seap(),{
    if(!is.null(loaded_database_seap())){
      showNotification(tagList(icon("check"), HTML("&nbsp;TREM2 data loaded!")), type = "message")
    }
  })
  

  
  loaded_database_reporter_pre = reactiveVal()
  
  observeEvent(c(input$sel_reporter,input$loaddatabase),{
    if(input$sel_reporter == "TREM2"){
      loaded_database_reporter_pre(loaded_database_trem2())
    }else{
      loaded_database_reporter_pre(loaded_database_seap())
    }
  })
  
  
  loaded_database_reporter = reactiveVal()
  
  observeEvent(loaded_database_reporter_pre(),{
    loaded_database_reporter(loaded_database_reporter_pre())
  })
  
  
  
  
  #### update data Reporter if present ####
  reporter_from_mod = mod_load_cyto_server("load_reporter_mod", data_type = reactive(input$sel_reporter))


  #check data correctly loaded
  output$check_data_updated_reporter = reactive(
    return(is.null(reporter_from_mod()))
  )
  outputOptions(output, "check_data_updated_reporter", suspendWhenHidden = FALSE)
  
  output$newdata_reporter_DT = renderDT({
    req(reporter_from_mod())
    if(input$summ_viewtable_updated_reporter == TRUE){
      reporter_from_mod()$mydataset %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
    }else{
      reporter_from_mod()$myprocesseddata %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
    }
  },options = list(scrollX = TRUE))
  
  
  observeEvent(input$update_reporter_bttn,{
    if(!is.null(reporter_from_mod())){
      loaded_database_reporter(update_database(old_data = loaded_database_reporter_pre(), new_data = reporter_from_mod()))
      showNotification(tagList(icon("check"), HTML("&nbsp;New data loaded! Click on 'Save database' if you want to store the changes.")), type = "message")
    }
  })
  
  

  
  
  ##### Upload updated
  
  #Upload
  observeEvent(input$upload_updated_reporter,{
    showModal(modalDialog(
      title = "Upload an existing database (.rds)",
      footer = modalButton("Close"),
      fluidRow(
        column(8,fileInput("upload_file_reporter", "Upload a database (.rds)", accept = ".rds")),
        conditionalPanel(
          condition = "output.check_fileuploaded_reporter == true",
          column(3,br(),actionButton("load_upload_file_reporter", "Load!", icon("rocket"),style='padding:10px; font-size:140%; font-weight: bold;'))
        )
      )
    ))
  })
  
  
  output$check_fileuploaded_reporter <- reactive({
    return(!is.null(input$upload_file_reporter))
  }) 
  outputOptions(output, 'check_fileuploaded_reporter', suspendWhenHidden=FALSE)
  
  
  
  
  #import 
  observeEvent(input$load_upload_file_reporter,{
    req(input$upload_file_reporter)
    ext <- tools::file_ext(input$upload_file_reporter$name)
    if(ext != "rds"){
      shinyWidgets::show_alert("Invalid file!", "Please upload a .rds file", type = "error")
    }
    validate(need(ext == "rds", "Invalid file! Please upload a .rds file"))
    
    file = readRDS(file = input$upload_file_reporter$datapath)
    if(file$type == input$sel_reporter){
      loaded_database_reporter(file)
      showNotification(tagList(icon("check"), HTML("&nbsp;New database loaded! Click on 'Save database' if you want to store the changes.")), type = "message")
    }else{
      shinyWidgets::show_alert("Invalid file!", paste("You uploaded the database for",file$type, "while you should have upoaded the database for",input$sel_reporter), type = "error")
    }
    
  })
  
  
  
  ##### SAVE Updated #####
  
  #check if something new is loaded otherwhise don't display the save button
  output$check_ifsave_reporter = reactive({
    checkdatabase = tryCatch({reporter_from_mod()
      FALSE
    },shiny.silent.error = function(e) {TRUE})
    
    if(TRUE %in% c(!checkdatabase, input$load_upload_file_reporter > 0)){return(TRUE)}
  })
  outputOptions(output, "check_ifsave_reporter", suspendWhenHidden = FALSE)
  
  
  
  observeEvent(input$save_update_reporter,{
    shinyWidgets::ask_confirmation(
      inputId = "confirmsave_reporter",
      type = "warning",
      title = "Do you want to save and update the internal database?",
      text = h4("Be sure that everything works before update.
      If you need to restore the original database, click on", strong("Restore Database."))
    )
  })
  
  
  observeEvent(input$confirmsave_reporter,{
    checkdatabase = tryCatch({loaded_database_reporter()
      FALSE
    },shiny.silent.error = function(e) {TRUE})
    
    
    if(input$confirmsave_reporter == TRUE && checkdatabase == FALSE){
      if(dim(loaded_database_reporter_pre()$exp_list)[1] == dim(loaded_database_reporter()$exp_list)[1]){
        showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;The internal database is already updated.")), type = "error")
      }else{
        filepath = paste0(base::system.file(package = "ADViSEBioassay"),"/data/reporter/",input$sel_reporter,"/")
        if(dir.exists(filepath) == FALSE){
          dir.create(filepath, recursive = TRUE)
        }

        saveRDS(loaded_database_reporter(), file = paste0(filepath,"database_updated_",input$sel_reporter,".rds")) 
        #sarà in /reporter/SEAP/database_updated_SEAP.rds o l'altro
        
        #versione per cronologia
        filever = paste0(filepath,input$sel_reporter,"_ver_",Sys.Date(),".rds") #sarà tipo SEAP_ver_05/04/2022
        saveRDS(loaded_database_reporter(), file = filever)
        
        show_alert(
          title = "Upload completed!",
          text = "Please restart ADViSEBioassay to save the changes.",
          type = "success"
        ) 
      }
    }
  })
  
  
  ##### download updated reporter ######
  
  #check all data correctly loaded. If FALSE -> error.
  output$checkupdated_reporter_fordownload = reactive({
    checkdatabase = tryCatch({reporter_from_mod()
      FALSE
    },shiny.silent.error = function(e) {TRUE})
    check_file = file.exists(paste0(base::system.file(package = "ADViSEBioassay"),"/data/reporter/",input$sel_reporter,"/database_updated_",input$sel_reporter,".rds"))
    if(TRUE %in% c(!checkdatabase, check_file)){return(TRUE)}
  })
  outputOptions(output, "checkupdated_reporter_fordownload", suspendWhenHidden = FALSE)
  
  
  
  #### Download handler for the download button
  output$download_updated_reporter <- downloadHandler(
    #put the file name with also the file extension
    filename = function() {
      paste0("updated_",input$sel_reporter,"_", Sys.Date(), ".rds")
    },
    
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      data = loaded_database_reporter()
      data$type = input$sel_reporter  #add the info of the datatype. will be another element of the list data$type...
      saveRDS(data, file)
    }
  )
  
  
  #### remove updated REPORTER #####
  
  
  #modal restore
  observeEvent(input$remove_update_reporter,{
    showModal(modalDialog(
      title = "Restore database",
      footer = modalButton("Close"),
      size = "l",
      fluidRow(
        column(
          6,style = "text-align:center;",
          box(width = NULL, status = "primary", title = "Restore database to a previous version", solidHeader = TRUE,
              column(10,offset = 1, selectInput("ver_databases_reporter", "Select a database", choices = "")),
              conditionalPanel(
                condition = "input.ver_databases_reporter != ''",
                actionButton("load_version_reporter", HTML("&nbsp;Load version"),icon("rotate-left"), style = "background: #e74c3c; border-color: #e74c3c;padding:10px; font-size:120%; font-weight: bold;"),
              )
          )
          
        ),
        column(
          6,style = "text-align:center;",
          box(width = NULL, status = "primary", title = "Reset database to the original internal database", solidHeader = TRUE,
              br(),
              actionButton("rest_to_original_reporter", HTML("&nbsp;Reset database"),icon("trash-can"), style = "background: #e74c3c; border-color: #e74c3c;padding:10px; font-size:120%; font-weight: bold;"),
              br(),br(), br()
          ))
      )
    ))
  })
  
  
  
  
  #### LOAD OLD VERSION
  
  observeEvent(input$remove_update_reporter,{
    files = list.files(paste0(base::system.file(package = "ADViSEBioassay"),"/data/reporter/",input$sel_reporter,"/"))
    files = grep(paste0(input$sel_reporter,"_ver_"),files, value = TRUE)
    updateSelectInput(session, "ver_databases_reporter", choices = files)
  })
  
  
  observeEvent(input$load_version_reporter,{
    shinyWidgets::ask_confirmation(
      inputId = "confirmload_ver_reporter",
      type = "warning",
      title = "Do you want to load this version?",
      text = h4("By clicking yes, this database version will be restored. Please restart ADViSEBioassay to apply this change.")
    )
  })
  
  
  observeEvent(input$confirmload_ver_reporter,{
    filepath_ver = paste0(base::system.file(package = "ADViSEBioassay"),"/data/reporter/",input$sel_reporter,"/",input$ver_databases_reporter)
    
    if(isTRUE(input$confirmload_ver_reporter)){
      if(file.exists(filepath_ver)){    
        file = readRDS(filepath_ver)
        filepath = paste0(base::system.file(package = "ADViSEBioassay"),"/data/reporter/",input$sel_reporter,"/database_updated_",input$sel_reporter,".rds")
        saveRDS(file, file = filepath)
        showNotification(tagList(icon("check"), HTML("&nbsp;Previous version restored!")), type = "message")
      }else{
        showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;This file version doesn't exist.")), type = "error")
      }
    }
  })
  
  
  
  
  ##### RESTORE
  observeEvent(input$rest_to_original_reporter,{
    shinyWidgets::ask_confirmation(
      inputId = "confirmremove_reporter",
      type = "warning",
      title = "Do you want to restore the internal database?",
      text = h4("By clicking yes, the original database will be restored. Please restart ADViSEBioassay to apply this change.")
    )
  })
  
  
  observeEvent(input$confirmremove_reporter,{
    
    filepath = paste0(base::system.file(package = "ADViSEBioassay"),"/data/reporter/",input$sel_reporter,"/")
    listf = list.files(filepath)
    file_to_rem = sapply(listf, function(x) paste0(filepath,x))
    
    if(isTRUE(input$confirmremove_reporter)){
      if(TRUE %in% file.exists(file_to_rem)){
        file.remove(file_to_rem)
        showNotification(tagList(icon("check"), HTML("&nbsp;Original database restored!")), type = "message")
      }else{
        showNotification(tagList(icon("circle-xmark"), HTML("&nbsp;Updated database already removed.")), type = "error")
      }
    }
    
  })
  
  
   
  
  ####
  #load data
  data_notsumm_reporter = reactive({
    req(loaded_database_reporter())
    loaded_database_reporter()$myprocesseddata
  })
  
  data_reporter = reactive({
    req(loaded_database_reporter())
    loaded_database_reporter()$mydataset
    #summarise_cytoxicity(data_notsumm_D1(), group = c("Experiment_id","Model_type", "Product", "Product_Family","Dose", "Purification"), method = "d1")
  })
  
  
  exp_list_reporter = reactive({
    req(loaded_database_reporter())
    loaded_database_reporter()$exp_list
  })
  
  
  #check data correctly loaded
  output$check_data_reporter = reactive(
    return(is.null(data_reporter()))
  )
  outputOptions(output, "check_data_reporter", suspendWhenHidden = FALSE)
  
  
  output$dtdata_reporter = renderDT({
    req(data_reporter())
    if(input$summ_viewtable_reporter == TRUE){
      data_reporter() %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
    }else{
      data_notsumm_reporter() %>% dplyr::mutate(dplyr::across(where(is.double), round, digits = 3))
    }
  },options = list(scrollX = TRUE))
  
  
  
  
  #### informative plots Reporter ####
  observeEvent(input$sel_reporter,{
    if(input$sel_reporter == "SEAP"){
      updateAwesomeRadio(session, "seltype_infograph_reporter", choices = c("Data overview", "Product family", 
                                                                          "Model types per fraction", "Fractions frequence"))
    }else{
      updateAwesomeRadio(session, "seltype_infograph_reporter", choices = c("Data overview", "Product family"))
    }
  })

  
  
  output$countbarplot_reporter = plotly::renderPlotly({
    req(data_reporter(), exp_list_reporter())
    
    count = data_reporter() %>% dplyr::select(where(is.character)) %>% dplyr::mutate(across(where(is.character), ~length(unique(.x))))
    count = t(count[1,])
    colnames(count) = "Count"
    count = count %>% as.data.frame() %>% tibble::rownames_to_column("Measure")
    count$Var = count$Measure
    
    inst = as.data.frame(table(exp_list_reporter()$Instrument))
    inst$Measure = "Instrument"
    scanint = rbind(inst)
    colnames(scanint) = c("Var", "Count","Measure")
    
    count = rbind(count,scanint)
    count$Measure = factor(count$Measure, levels = unique(count$Measure))
    count$Var = factor(count$Var, levels = unique(count$Var))
    temp = ggplot(count, aes(x = Measure, y = Count, group = Var)) + geom_col(aes(fill = Var)) + 
      geom_text(aes(label=Count),position = position_stack(vjust = 0.5)) + ggtitle("Data overview") + labs(x = "Measure", fill = "Measure", y = "Total numbers") + 
      theme(axis.text.x = element_text(angle = 315, hjust = 0))
    
    plotly::ggplotly(temp)
    
  })
  
  
  output$prodfam_barplot_reporter = plotly::renderPlotly({
    req(data_reporter())
    
    prod_table =  as.data.frame(table(data_reporter()$Product_Family))
    colnames(prod_table)[1] = "Product_Family"
    
    prod_table = prod_table[order(-prod_table$Freq),]
    prod_table$Product_Family = factor(prod_table$Product_Family, levels = rev(prod_table$Product_Family))
    
    
    if(input$first50_prodfam_reporter == TRUE && length(unique(data_reporter()$Product_Family)) > 50){
      prod_table = prod_table[1:50,]
    }
    
    temp = ggplot(prod_table) + geom_col(aes(y = Product_Family, x = Freq, fill = Product_Family))+ 
      xlab("Number of samples")+ ylab("Product Family") + ggtitle("Sample for each Product Family") + 
      theme(axis.text.y = element_text(size = 7.4))
    
    plotly::ggplotly(temp, tooltip = c("x", "fill"))
  })
  
  output$prodfam_barplotUI_reporter = renderUI({
    
    if(input$first50_prodfam_reporter == TRUE || length(unique(data_reporter()$Product_Family)) < 50){
      size_plot = 84 + 640
    }else{
      size_plot = 84 + (640*length(unique(data_reporter()$Product_Family)))/(50 + length(unique(data_reporter()$Product_Family))/10) #640 found with html inspect when 50 prod are shown.
    }
    
    plotly::plotlyOutput("prodfam_barplot_reporter", height = paste0(size_plot,"px"))
  })
  
  

  
  #1 quante e quali linee è presente il product
  # observeEvent(data_reporter(),{
  #   if(input$sel_reporter == "SEAP"){
  #     products = dplyr::filter(data_reporter(), !if_any("Product_Family", ~grepl("CTRL",.)))$Product_Family
  #     updateSelectInput(session, "query1_repo_prodfam", choices = unique(products))
  #   }
  # })
  # 
  # observeEvent(input$query1_repo_prodfam,{
  #   updateSelectInput(session, "query1_repo", 
  #                     choices = unique(dplyr::filter(data_reporter(), Product_Family %in% input$query1_repo_prodfam)$Product))
  # })
  
  observeEvent(data_reporter(),{
    if(input$sel_reporter == "SEAP"){
      products = dplyr::filter(data_reporter(), !if_any("Product_Family", ~grepl("CTRL",.)))$Product
      updateSelectInput(session, "query1_repo",
                        choices = unique(products))
    }

  })
  
  #Model types found in a selected fraction
  output$modtype_perfrac_seap = renderDT({
    req(data_reporter())
    req(input$query1_repo)
    data_reporter() %>% dplyr::filter(Product %in% input$query1_repo) %>% dplyr::select(Product, Model_type) %>% 
      dplyr::distinct() %>% dplyr::mutate(Presence = "yes") %>% 
      tidyr::pivot_wider(names_from = Model_type, values_from = Presence) %>%
      dplyr::mutate(across(2:4, ~case_when(. == "yes" ~  yes_icon, . == "no" ~  no_icon)))

  }, selection = "single", escape = FALSE, server = FALSE, rownames = FALSE, class = 'cell-border stripe',
  options = list(lengthMenu = c(15, 20, 25, 50), pageLength = 20, columnDefs = list(list(className = 'dt-center', targets = 1:3))))
  

  
  
  
  
  #2. frequenza delle frazioni (Product ext,A,B,C,D,E) attive nei saggi, quale Product trovo più frequentemente attivo;
  #voglio vedere quanti A, B, ext, C etc.
  output$fractfreq_seap = renderPlotly({
    req(data_reporter())
    if(input$seltype_infograph_reporter == "Fractions frequence"){
      due = data_reporter() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.))) %>% 
        dplyr::mutate(Extract = stringr::str_replace(stringr::str_replace(Product, Product_Family, ""), "_","")) %>% 
        dplyr::mutate(Extract = stringr::str_replace(Extract,pattern="^$",replacement="EXT")) %>% dplyr::pull(Extract) %>% table() %>%
        as.data.frame() %>% dplyr::rename("Fraction" = ".", "Frequence" = "Freq")
      temp = ggplot(due, aes(x=Fraction , y = Frequence)) + geom_col(aes(fill = Fraction)) + 
        geom_text(aes(label=Frequence),position = position_stack(vjust = 0.5))
      plotly::ggplotly(temp)
    }
  })



  
  
  

  
  #### Barplot reporter ####

  observeEvent(data_notsumm_reporter(),{
    if(input$sel_reporter == "SEAP"){
      updateSelectInput(session, "typeeval_bar_reporter", choices = "Concentration")
      updateSelectInput(session, "typeeval_bar2_reporter", choices = "Concentration")
    }else{
      updateSelectInput(session, "typeeval_bar_reporter", choices = colnames(dplyr::select(data_notsumm_reporter(), where(is.double),-Dose)))
      updateSelectInput(session, "typeeval_bar2_reporter", choices = colnames(dplyr::select(data_notsumm_reporter(), where(is.double),-Dose)))
      
    }
  })
  
  
  output$show_barplot2_reporter = reactive({
    ifelse(input$add_barplot_reporter %%2 == 1, TRUE, FALSE)
  })
  outputOptions(output, "show_barplot2_reporter", suspendWhenHidden = FALSE)
  
  
  observeEvent(input$add_barplot_reporter,{
    if(input$add_barplot_reporter %%2 == 1){
      updateActionButton(session, "add_barplot_reporter",label = HTML("&nbsp;Remove second barplot"),icon("minus")) 
    }else{
      updateActionButton(session, "add_barplot_reporter", label = HTML("&nbsp;Add another barplot"), icon("plus"))
    }
  })
  
  
  observeEvent(data_reporter(),{
    updateSelectInput(session, "model_filt_bar_reporter", choices = unique(data_reporter()$Model_type))
    updateSelectInput(session, "model_filt_bar2_reporter", choices = unique(data_reporter()$Model_type))
    
  })
  
  observeEvent(input$model_filt_bar_reporter,{
    family = data_reporter() %>% dplyr::filter(Model_type == input$model_filt_bar_reporter) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_bar_reporter", choices = unique(family))
  })
  
  #purification
  observeEvent(c(input$model_filt_bar_reporter,input$family_filt_bar_reporter),{
    purif = dplyr::filter(data_reporter(), Model_type == input$model_filt_bar_reporter & Product_Family == input$family_filt_bar_reporter)
    updateSelectInput(session, "purif_filt_bar_reporter", choices = unique(purif$Purification), selected = unique(purif$Purification)[1])
  })
  
  
  output$check_multID_bar1_reporter = reactive({
    req(data_notsumm_reporter())
    data_plot_not1 =  dplyr::filter(data_notsumm_reporter(), 
                                    Product_Family == input$family_filt_bar_reporter & Model_type == input$model_filt_bar_reporter & Purification == input$purif_filt_bar_reporter)
    ifelse(length(unique(data_plot_not1$Experiment_id)) >1, TRUE, FALSE)
  })
  outputOptions(output, "check_multID_bar1_reporter", suspendWhenHidden = FALSE)
  
  
  output$barplot_reporter = plotly::renderPlotly({
    req(data_notsumm_reporter())
    
    data_plot_not1 =  dplyr::filter(data_notsumm_reporter(), 
                                    Product_Family == input$family_filt_bar_reporter & Model_type == input$model_filt_bar_reporter & Purification == input$purif_filt_bar_reporter)
    cnts = data_notsumm_reporter() %>% dplyr::filter(stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot_not1$Experiment_id))
    data_plot_not = rbind(data_plot_not1, cnts)
    
    if(all(c("LPS") %in% cnts$Product)){
      level_order = c("CTRL", "LPS", sort(unique(data_plot_not1$Product)))
    }else{
      level_order = c("CTRL", sort(unique(cnts[cnts$Product_Family == "CTRL+",]$Product)), sort(unique(data_plot_not1$Product)))
    }
    
    
    plot = ggplot(data_plot_not, aes(x = factor(Product, level = level_order), y = !!sym(input$typeeval_bar_reporter), fill = factor(Dose)))+
      coord_cartesian(ylim=c(0, 100)) + 
      geom_bar(position = position_dodge(), stat = "summary",fun = "mean") +
      stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2,position = position_dodge(width = 1))+
      xlab("Product") + ggtitle(input$family_filt_bar_reporter) + labs(fill = "Dose") +
      theme(axis.text.x = element_text(angle = 315, hjust = 0, size = 10, margin=margin(t=30)),legend.title = element_blank())
    
    if(input$sel_reporter == "SEAP") plot = plot + ylab("Concentration (ng/mL)")
    
    if(length(unique(data_plot_not$Experiment_id)) >1){
      showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_bar_reporter,". 
                                                  You can summarise them (Faceting expID to FALSE) or you can
                                                  facet (Faceting expID to TRUE)")), type = "default")
      if(input$facet_bar_reporter == TRUE){
        plot = plot + facet_grid(~Experiment_id, scales = "free", switch = "x")
      }
    }
    
    if(input$addpoints_barplot_reporter == TRUE){
      plot = plot + geom_point(position = position_dodge(width = 1))
    }
    
    plotly::ggplotly(plot)
    
  })
  
  
  
  ### second barplot
  observeEvent(input$model_filt_bar2_reporter,{
    family = data_reporter() %>% dplyr::filter(Model_type == input$model_filt_bar2_reporter) %>%
      dplyr::filter(!stringr::str_detect(Product_Family, "CTRL")) %>% dplyr::select(Product_Family)
    updateSelectInput(session, "family_filt_bar2_reporter", choices = unique(family))
  })
  
  #purification
  observeEvent(c(input$model_filt_bar2_reporter,input$family_filt_bar2_reporter),{
    purif = dplyr::filter(data_reporter(), Model_type == input$model_filt_bar2_reporter & Product_Family == input$family_filt_bar2_reporter)
    updateSelectInput(session, "purif_filt_bar2_reporter", choices = unique(purif$Purification), selected = unique(purif$Purification)[1])
  })
  
  output$check_multID_bar2_reporter = reactive({
    req(data_notsumm_reporter())
    data_plot_not1 =  dplyr::filter(data_notsumm_reporter(), 
                                    Product_Family == input$family_filt_bar2_reporter & Model_type == input$model_filt_bar2_reporter & Purification == input$purif_filt_bar2_reporter)
    ifelse(length(unique(data_plot_not1$Experiment_id)) >1, TRUE, FALSE)
  })
  outputOptions(output, "check_multID_bar2_reporter", suspendWhenHidden = FALSE)
  
  
  output$barplot2_reporter = plotly::renderPlotly({
    req(data_notsumm_reporter())
    
    data_plot_not1 =  dplyr::filter(data_notsumm_reporter(), 
                                    Product_Family == input$family_filt_bar2_reporter & Model_type == input$model_filt_bar2_reporter & Purification == input$purif_filt_bar2_reporter)
    cnts = data_notsumm_reporter() %>% dplyr::filter(stringr::str_detect(Product_Family, "CTRL") & Experiment_id %in% unique(data_plot_not1$Experiment_id))
    data_plot_not = rbind(data_plot_not1, cnts)
    
    if(all(c("LPS") %in% cnts$Product_Family)){
      level_order = c("CTRL", "LPS", sort(unique(data_plot_not1$Product)))
    }else{
      level_order = c("CTRL", sort(unique(cnts[cnts$Product_Family == "CTRL+",]$Product)), sort(unique(data_plot_not1$Product)))
    }
    
    plot = ggplot(data_plot_not, aes(x = factor(Product, level = level_order), y = !!sym(input$typeeval_bar2_reporter), fill = factor(Dose)))+
      coord_cartesian(ylim=c(0, 100)) + 
      geom_bar(position = position_dodge(), stat = "summary",fun = "mean") +
      stat_summary(fun.data = ggplot2::mean_sdl, fun.args = list(mult=1), geom="errorbar", color="black", width=0.2,position = position_dodge(width = 1))+
      xlab("Product") + ggtitle(input$family_filt_bar2_reporter) + labs(fill = "Dose") +
      theme(axis.text.x = element_text(angle = 315, hjust = 0, size = 10, margin=margin(t=30)),legend.title = element_blank())
    
    if(input$sel_reporter == "SEAP") plot = plot + ylab("Concentration (ng/mL)")
    
    if(length(unique(data_plot_not$Experiment_id)) >1){
      showNotification(tagList(icon("info"), HTML("&nbsp;There are multiple Experiment ID for", input$family_filt_bar2_reporter,". 
                                                  You can summarise them (Faceting expID to FALSE) or you can
                                                  facet (Faceting expID to TRUE)")), type = "default")
      if(input$facet_bar2_reporter == TRUE){
        plot = plot + facet_grid(~Experiment_id, scales = "free", switch = "x")
      }
    }
    
    if(input$addpoints_barplot2_reporter == TRUE){
      plot = plot + geom_point(position = position_dodge(width = 1))
    }
    
    plotly::ggplotly(plot)
    
  })
  
  ##### spiderplot reporter ####
  
  mod_spiderplot_server("spiderplot_reporter", data = data_reporter, type_data = reactive(input$sel_reporter))
  
  ##### bubbleplot reporter #####
  mod_bubble_plot_server("bubbleplot_reporter", data = data_reporter, type_data = reactive(input$sel_reporter))
  
  
  ##### heatmap ####
  
  
  data_heatmap_reporter = mod_heatmap_cyto_repo_server("heatmap_reporter", data = data_reporter, data_type = reactive(input$sel_reporter))
  
  observeEvent(data_heatmap_reporter(),{
    InteractiveComplexHeatmap::InteractiveComplexHeatmapWidget(input, output, session, data_heatmap_reporter(), output_id  = "heatmap_output_reporter",
                                                               layout = "1|(2-3)", width1 = 1000, height1 = 600)
  })
  
  
  
  #### query reporter ####
  
  #update type of query based on seap or trem2
  observeEvent(input$sel_reporter,{
    if(input$sel_reporter == "SEAP"){
      updateSelectInput(session, "query_reporter", choices = c("Concentration greater than CTRL" = "1"))
    }else{
      updateSelectInput(session, "query_reporter", choices = c("GFP fractions greater than CTRL+" = "4",
                                                               "Enriched fractions" = "6"))
    }
  })
  
  observeEvent(input$query_reporter,{

    if(input$query_reporter == "1"){
      updateSelectInput(session, "query3_repo_modtype", choices = unique(data_reporter()$Model_type))
      updateSelectInput(session, "query_reporter2", choices = c("Enriched fractions" = "3"))
    }
  })
  
  
  #### Add another query for SEAP

  #output to show the second row
  output$checkadd2query_seap = reactive({
    if(input$add2query_seap %%2 == 0){
      "onequery"
    }else{"twoquery"}
    })
  outputOptions(output, "checkadd2query_seap", suspendWhenHidden = FALSE)
  
  #to update the button
  observeEvent(input$add2query_seap,{
    if(input$add2query_seap %%2 == 1){
      updateButton(session, "add2query_seap",label = HTML("&nbsp;Remove"), style = "danger", icon("minus")) 
    }else{
      updateButton(session, "add2query_seap", label = HTML("&nbsp;Add"), style="success", icon("plus"))
    }
  })
  

  #### Add another query for TREM2
  
  #output to show the second row
  output$checkadd2query_trem = reactive({
    if(input$add2query_trem %%2 == 0){
      "onequery"
    }else{"twoquery"}
  })
  outputOptions(output, "checkadd2query_trem", suspendWhenHidden = FALSE)
  
  #to update the button
  observeEvent(input$add2query_trem,{
    if(input$add2query_trem %%2 == 1){
      updateButton(session, "add2query_trem",label = HTML("&nbsp;Remove"), style = "danger", icon("minus")) 
    }else{
      updateButton(session, "add2query_trem", label = HTML("&nbsp;Add"), style="success", icon("plus"))
    }
  })
  
  
  
  
  query_repo_data2 = eventReactive(input$go_queryrepo,{
    req(data_reporter())
    print(input$query_reporter)
    
    ###SEAP
    
    if(input$query_reporter == "1"){
      validate(need(input$query3_repo_modtype, "Please select at least one Model_type"))
      return(productive_fractions(data_reporter = data_reporter(),
                           model_type = input$query3_repo_modtype,
                           times_ctrl = input$query3_repo_thresh))
    }
    
    
    ###TREM2
    if(input$sel_reporter == "TREM2"){
      cnt_trem2 <- data_reporter() %>% dplyr::filter(Product_Family == "CTRL+")
      my_trem2 <- data_reporter() %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.)))
      
      temp_trem2 = lapply(unique(my_trem2$Product_Family), function(m){
        data = dplyr::filter(my_trem2, Product_Family == m)
        if(length(unique(data$Purification)) >1){
          #if there are multiple purification, we have to check for each purification
          lapply(unique(data$Purification), function(k){
            data2 = data %>% dplyr::filter(Purification == k)
            cnt2 = cnt_trem2 %>% dplyr::filter(Experiment_id %in% unique(data2$Experiment_id)) %>% as.data.frame()
            data %>% dplyr::filter(GFP.average >= mean(cnt[,"GFP.average"])*(input$query4_repo_thresh/100))
          }) %>% {Reduce(rbind, .)}
          
        }else{
          cnt = cnt_trem2 %>% dplyr::filter(Experiment_id %in% unique(data$Experiment_id)) %>% as.data.frame()
          data %>% dplyr::filter(GFP.average >= mean(cnt[,"GFP.average"])*(input$query4_repo_thresh/100))
        }
      }) %>% {Reduce(rbind, .)}
      
      if(input$query_reporter == "4"){
        return(temp_trem2)
      }
      
      if(input$query_reporter == "6"){
        return(enriched_fractions(data_reporter = data_reporter(), repo_type = "TREM2", prod_trem = temp_trem2))
      }
      
    }
    
  })
  

  query_repo_data = eventReactive(input$go_queryrepo,{
    req(query_repo_data2())
    
    nqueryseap = if(input$add2query_seap %%2 == 0) "onequery" else "twoquery"
    nquerytrem = if(input$add2query_trem %%2 == 0) "onequery" else "twoquery"
    
    if(nqueryseap == "onequery" && nquerytrem == "onequery"){
      return(query_repo_data2())
    }else{
      if(input$query_reporter == "1" && input$query_reporter2 == "3"){
        enriched_fractions(prod_trem = query_repo_data2(), data_reporter = data_reporter(), repo_type = "SEAP")
      }
    }
    
  })
  
  
  output$query_repo_dt = renderDT({
    req(query_repo_data())
    if("hTREM2_WT_REPORTER" %in% query_repo_data()$Model_type && input$sel_reporter == "TREM2"){
      query_repo_data()
    }else if(!("hTREM2_WT_REPORTER" %in% query_repo_data()$Model_type) && input$sel_reporter == "SEAP"){
      query_repo_data()
    }else{
      NULL
    }

  }, options = list(scrollX = TRUE))
  
  
  
  #### Integration ####
  
  #### check datatable
  
  all_databases = reactive({
    req(data())
    req(loaded_database_trem2())
    req(loaded_database_seap())
    req(data_D1())
    dt = list(cyto = data(), d1 = data_D1(), trem2 = loaded_database_trem2()$mydataset, seap = loaded_database_seap()$mydataset)
    lapply(dt,  function(x) x %>% dplyr::filter(!if_any("Product_Family", ~grepl("CTRL",.))))
  })
  
  yes_icon = '<i aria-label="ok icon" class="glyphicon glyphicon-ok" role="presentation" style="
    color: lawngreen;"></i>'
  no_icon = '<i aria-label="remove icon" class="glyphicon glyphicon-remove" role="presentation" style="
    color: red;"></i>'
  
  
  data_checktable = reactive({
    req(all_databases())
    
    lapply(all_databases(), function(x) x %>% dplyr::pull(Product_Family) %>% unique()) %>% 
      make_check_table() %>%
      dplyr::mutate(across(2:5, ~case_when(. == "yes" ~  yes_icon, . == "no" ~  no_icon)))
  })
  
  output$checktable <- DT::renderDT({
    req(data_checktable())
    req(info)
    dplyr::left_join(data_checktable(), dplyr::select(info, "Chemical_code", "Info"), by = c("Product_Family" = "Chemical_code"))
    
  }, selection = "single", escape = FALSE, server = FALSE, rownames = FALSE, class = 'cell-border stripe',
  options = list(lengthMenu = c(15, 20, 25, 50), pageLength = 20, columnDefs = list(list(className = 'dt-center', targets = 1:5)))
  )
  
###info

  observeEvent(input[["infoprod_select_button"]], {
    print(input$infoprod_select_button)
    toggleModal(session, "modal_infoprod", toggle = "open")
  })

  output$dt_infoprod = renderDT({
    req(input$infoprod_select_button)
    print(input[["infoprod_select_button"]]) #è equivalente all'$. Viene preso dall'onclick parameter dell'actionbutton (vedi data-raw)
    prod_fam = unlist(strsplit(input[["infoprod_select_button"]], "_"))[2]
    info %>% dplyr::select(-Info) %>% dplyr::filter(Chemical_code == prod_fam)
  },selection = "single", rownames = FALSE, options = list(scrollX = TRUE))

  
  
  ####model
  checktable_models = reactive({
    req(all_databases())
    req(input$checktable_rows_selected)
    nroww = input$checktable_rows_selected

    dt = lapply(all_databases(), function(x) x %>% dplyr::filter(Product_Family == data_checktable()[nroww,]$Product_Family) %>% 
                  dplyr::pull(Model_type) %>% unique())
    
    unique_mod =  lapply(all_databases(), function(x) x %>% dplyr::pull(Model_type) %>% unique())
    
    full_mod = list()
    for(k in names(all_databases())){
      full_mod[[k]] = data.frame(Model_type = unique_mod[[k]]) %>%
        dplyr::mutate(Check = case_when(Model_type %in% dt[[k]]~ yes_icon, TRUE ~ no_icon))
    }
    return(full_mod)
  })
  
  
  output$checktable_models_cyto = renderTable({
    req(checktable_models())
    checktable_models()$cyto
  },width = "100%", bordered = T,align = "lc",sanitize.text.function = function(x) x)
  
  output$checktable_models_d1 = renderTable({
    req(checktable_models())
    checktable_models()$d1
  },width = "100%", bordered = T,align = "lc",sanitize.text.function = function(x) x)
  
  output$checktable_models_trem2 = renderTable({
    req(checktable_models())
    checktable_models()$trem2
  },width = "100%",bordered = T,align = "lc",sanitize.text.function = function(x) x)
  

  output$checktable_models_seap = renderTable({
    req(checktable_models())
    checktable_models()$seap
  },width = "100%",bordered = T,align = "lc", sanitize.text.function = function(x) x)
  
  
}
