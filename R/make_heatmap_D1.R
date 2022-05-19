
make_heatmap_D1 = function(data,
                           row_dend = TRUE, 
                           row_nclust = 2, 
                           dist_method = "euclidean", 
                           clust_method = "ward.D2",
                           col_label_size = 8,
                           add_values = FALSE,
                           thresh_values = 0,
                           type = "mean",
                           logscale = FALSE,
                           scale_type = "To max",
                           custom_scale = NULL,
                           custom_colors = NULL
                           ){
  
  
  ht_list = NULL
  colors =  RColorBrewer::brewer.pal(8, "Dark2")[-3]
  index_color = 1
  
  absolute_max = max(dplyr::select(data, -dplyr::any_of(c("Vitality", "Cytotoxicity"))), na.rm = TRUE)
  
  
  for(k in colnames(data)){
    
    data_filt = data[k]
    
    #color scale and unit legend
    if(k %in% c("Vitality", "Cytotoxicity")){
      if(type == "subtract"){
        color_scale = circlize::colorRamp2(c(-100, 0, 100), c("green","white", "red"))
      }else{
        color_scale = circlize::colorRamp2(c(0, 100), c("white", "blue"))
      }
      unit_legend = paste0("%",k)
    }else{
      
      #logscale
      if(logscale == TRUE){
        data_filt = log2(data_filt)
        absolute_max = log2(absolute_max)
      }
      
      #colors
      if(is.null(custom_colors)){
        final_color = colors[index_color]
        index_color = index_color+1
      }else{
        final_color = custom_colors[[k]]$color
      }
      
      
      #scale type
      if(scale_type == "To max"){
        color_scale = circlize::colorRamp2(c(0, max(data_filt, na.rm = TRUE)), c("white", final_color))
      }else if(scale_type == "Adaptive"){
        color_scale = circlize::colorRamp2(c(min(data_filt, na.rm = TRUE), max(data_filt, na.rm = TRUE)), c("white", final_color))
      }else if(scale_type == "Absolute"){
        #take the largest value among all columns (except vita and cyto)
        color_scale = circlize::colorRamp2(c(0, absolute_max), c("white", final_color))
      }else if(scale_type == "Custom"){
        color_scale = circlize::colorRamp2(c(custom_scale[[k]]$min, custom_scale[[k]]$max), c("white", final_color))
      }
      
      
      unit_legend = k
     
    }
    
    
    
    #dendrogram 
    if(row_dend == TRUE){
      row_dend2 = data_filt %>% stats::dist(method = dist_method) %>% stats::hclust(method = clust_method) %>% stats::as.dendrogram()
      row_dend2 = dendextend::color_branches(row_dend2, k = row_nclust)
      row_split = row_nclust
    } else {
      row_dend2 = FALSE
      row_split = NULL
    }
    
    
    
    #values inside cells
    if(add_values == TRUE){
      
      cell_values = local({
        data_filt = data_filt
        thresh_values = thresh_values
        function(j, i, x, y, width, height, fill) {
          if(abs(as.matrix(data_filt)[i, j]) >= thresh_values){
            grid::grid.text(sprintf("%.0f", as.matrix(data_filt)[i, j]), x, y, gp = grid::gpar(fontsize = 10))
          }
        }
      })
      # 
      # cell_values = function(j, i, x, y, width, height, fill) {
      #   if(abs(as.matrix(data_filt)[i, j]) >= thresh_values){
      #     grid::grid.text(sprintf("%.0f", as.matrix(data_filt)[i, j]), x, y, gp = grid::gpar(fontsize = 10))
      #   }
      # }
    }else{
      cell_values = NULL
    }

    ht_list = ht_list + ComplexHeatmap::Heatmap(as.matrix(data_filt), rect_gp = grid::gpar(col = "white", lwd = 1),
                                                name = unit_legend, column_title = NULL, 
                                                cluster_rows = row_dend2, row_split = row_split, cluster_columns = FALSE,
                                                col = color_scale, row_names_side = "left",
                                                row_gap = grid::unit(2, "mm"), column_gap = grid::unit(2, "mm"), #spazio tra le divisioni
                                                cell_fun = cell_values
                                                #row_names_gp = grid::gpar(fontsize = 10), column_names_gp = grid::gpar(fontsize = col_label_size), #size testo
                                                )
    
    
  }
  
  return(ht_list)
  
}
