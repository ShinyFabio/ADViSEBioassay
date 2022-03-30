#' Make a Heatmap using ComplexHeatmap package.
#' 
#' @description This function render a Heatmap using ComplexHeatmap package.
#' 
#' @param data A summarizedexperiment object. Tested only on sumexp averaged
#' @param add_rowannot A column used for the left (row) annotation (e.g. "Product_Batch")
#' @param add_colannot A column used for the bottom (column) annotation (e.g. "Class")
#' @param scale_data Scaling data option. Data can be scaled (using scale()) by row ("row"), by column ("column") or not scaled ("none"). The result is the z-score.
#' @param row_dend Boolean. Render dendogram on rows (TRUE or FALSE).
#' @param col_dend Boolean. Render dendogram on columns (TRUE or FALSE).
#' @param row_nclust Numeric. Number of cluster in the row dendrogram.
#' @param col_nclust Numeric. Number of cluster in the column dendrogram.
#' @param dist_method Choose a distance method (e.g "euclidean"). See ?stats::dist().
#' @param clust_method Choose a clustering method (e.g "ward.D"). See ?stats::hclust().
#' @param title A label for the column (i.e. "Lipids").
#' @param unit_legend An unit label for the legend. If data are scaled will be "Z-score" otherwise will be the input.
#' @param col_label_size Size of the column labels. By default col_label_size = 10.
#' @param color_scale Color scale for the cells. By default is circlize::colorRamp2(c(0, 100), c("white", "blue")).
#'
#' @importFrom dplyr select arrange across left_join all_of
#' @importFrom stats dist hclust as.dendrogram setNames
#' @importFrom dendextend color_branches
#' @importFrom ComplexHeatmap HeatmapAnnotation Heatmap draw ht_opt
#' @importFrom grid unit
#' @importFrom grDevices colorRampPalette rainbow
#' @importFrom RColorBrewer brewer.pal
#' @importFrom circlize colorRamp2
#' @importFrom tibble column_to_rownames
#' @importFrom tidyr pivot_wider
#'


make_heatmap = function(data,
                        filt_data_col = "All",
                        add_rowannot = "Model_Family",
                        add_colannot = NULL,
                        title = "CBC150",
                        #log_data = FALSE,
                        #scale_data = "column",
                        order_data = TRUE,
                        row_dend = TRUE, 
                        row_nclust = 2, 
                        col_dend = FALSE, 
                        col_nclust = 3, 
                        dist_method = "euclidean", 
                        clust_method = "ward.D2", 
                        unit_legend = "% toxicty",
                        col_label_size = 8,
                        color_scale = circlize::colorRamp2(c(0, 100), c("white", "blue")),
                        add_values = FALSE,
                        thresh_values = 0,
                        typeeval_heat = "Cytotoxicity.average"){
  
#data = dplyr::arrange(data, Product_Family)
#data = cbc_filtered
  if(order_data == TRUE){
    data = order_data(data, as_factor = FALSE)
  }

  
  # #filter column
  # if(!("All" %in% filt_data_col)){
  #   temp = dplyr::filter(data, Product_Family %in% filt_data_col)
  # }else{
    temp = data
  # }
  data_for_annotcol = temp
  
  temp = temp %>% dplyr::select(Product, Model_type, dplyr::all_of(typeeval_heat)) %>%
    tidyr::pivot_wider(names_from = "Product", values_from = typeeval_heat) %>%
    tibble::column_to_rownames("Model_type")


# 
# if(order_data == TRUE){
#   order = data %>% SummarizedExperiment::assay() %>% t() %>% as.data.frame() %>% tibble::rownames_to_column("SampleID")
#   cold  = data %>% SummarizedExperiment::colData() %>% as.data.frame() %>% dplyr::select(SampleID, add_rowannot)
#   joined = dplyr::left_join(order, cold, by = "SampleID")
#   temp = joined %>% dplyr::arrange( dplyr::across(add_rowannot)) %>% dplyr::select(-add_rowannot) %>%
#     tibble::column_to_rownames("SampleID") %>% as.matrix()
# }
# 
# 
# 
# if(log_data == TRUE){
#   temp = log2(temp)
# }
# 
# #scale none, row, column
# if(scale_data == "column"){
#   temp = scale(temp) # scale and center columns
#   #ora se ho una colonna con tutti 0, scale restituisce una colonna con tutti NaN. Qui sostituisco le colonne
#   #con tutti NaN con tutti 0. QUesta parte non dovrebbe servire
#   for(i in seq(1:length(temp[1,]))){
#     if(mean(is.na(temp[,i])) == 1){
#       temp[,i] = 0
#     }
#   }
#   unit_legend = "Z-score"
# } else if(scale_data == "row"){
#   temp = t(scale(t(temp))) # scale and center rows
#   unit_legend = "Z-score"
# }
# 
# if(scale_data == "none"){
#   legend_col = c("#ffffff", "#ff8080", "#ff0000")
# }else{
#   max_legend = max(abs(temp), na.rm = TRUE)
#   legend_col = circlize::colorRamp2(c(-max_legend, 0, max_legend), c("blue", "white", "red"))
# }

#dendrogram 
if(row_dend == TRUE){
  row_dend2 = temp %>% stats::dist(method = dist_method) %>% stats::hclust(method = clust_method) %>% stats::as.dendrogram()
  row_dend2 = dendextend::color_branches(row_dend2, k = row_nclust)
  row_split = row_nclust
} else {
  row_dend2 = FALSE
  row_split = NULL
}

if(col_dend == TRUE){
  col_dend2 = temp %>% t() %>% stats::dist(method = dist_method) %>% stats::hclust(method = clust_method) %>% stats::as.dendrogram()
  col_dend2 = dendextend::color_branches(col_dend2, k = col_nclust)
  col_split = col_nclust
} else {
  col_dend2 = FALSE
  col_split = NULL
}


#creo l'annotazione
getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))





#row annotation
if(!is.null(add_rowannot)){
  
  roww = data_for_annotcol %>% dplyr::select(Model_type, add_rowannot) 
  row_ha = c()
  for(i in add_rowannot){
    
    if(is.character(roww[[i]]) || is.factor(roww[[i]])){
      roww2 = roww %>% dplyr::select(Model_type, i) %>% distinct() %>% tibble::column_to_rownames("Model_type")
      leng_row = roww2[,i] %>% table() %>% length()
      colorannot_row = stats::setNames(grDevices::rainbow(n = leng_row), c(row.names(table(roww2[,i])))) #oppure getPalette
      colorannot_row = stats::setNames(list(colorannot_row), paste(i))
      row_ha = append(row_ha, ComplexHeatmap::rowAnnotation(df = roww2[i], col = c(colorannot_row), border = TRUE))
      

      
    }else if(is.numeric(roww[[i]])) {
      
      col_fun2 = list(circlize::colorRamp2(breaks = c(min(roww[,i]), max(roww[,i])), colors = c("white","orange")))
      #here I have to make a list ad assign a name like this otherwise random color
      names(col_fun2) = i
      row_ha = append(row_ha, ComplexHeatmap::rowAnnotation(df = as.data.frame(roww[i]), col = col_fun2, border = TRUE))
      
    }else{
      message("Something wrong with the row annotation. The class isn't numeric, character or factor.")
    }
  }

}else{
  row_ha = NULL
}


#col annotation
if(!is.null(add_colannot)){
  annotdata_col = data_for_annotcol %>% dplyr::select(Product, add_colannot) %>% dplyr::distinct() %>% tibble::column_to_rownames("Product")
  leng_col = annotdata_col %>% table() %>% length()
  colorannot_col = stats::setNames(getPalette(leng_col), c(row.names(table(annotdata_col))))
  colorannot_col = stats::setNames(list(colorannot_col), paste(add_colannot))
  col_ha = ComplexHeatmap::HeatmapAnnotation(df = annotdata_col, which = "column", col = colorannot_col, border = TRUE)
}else{
  col_ha = NULL
}


#aggiungi spazio tra annotazione e heatmap
ComplexHeatmap::ht_opt(ROW_ANNO_PADDING = grid::unit(4, "mm"), COLUMN_ANNO_PADDING = grid::unit(4, "mm"))

if(add_values == TRUE){
  cell_values = function(j, i, x, y, width, height, fill) {
    if(abs(as.matrix(temp)[i, j]) >= thresh_values){
      grid::grid.text(sprintf("%.0f", as.matrix(temp)[i, j]), x, y, gp = grid::gpar(fontsize = 10))
    }
  }
}else{
  cell_values = NULL
}

ht = ComplexHeatmap::Heatmap(as.matrix(temp), name = unit_legend, rect_gp = grid::gpar(col = "white", lwd = 1), 
                             row_title = "Model type", column_title = title, 
                             row_names_gp = grid::gpar(fontsize = 10), column_names_gp = grid::gpar(fontsize = col_label_size), #size testo
                             cluster_rows = row_dend2, cluster_columns = col_dend2, 
                             left_annotation = row_ha, bottom_annotation = col_ha,
                             column_split = col_split, row_split = row_split,
                             row_gap = grid::unit(2, "mm"), column_gap = grid::unit(2, "mm"), #spazio tra le divisioni
                             col = color_scale,
                             cell_fun = cell_values
)


return(ht)
#ht = ComplexHeatmap::draw(ht, padding = grid::unit(padding, "mm"))
}
