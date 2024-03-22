function(input, output, session) {
  
  output$html <-renderUI({
    return(includeHTML(html1))
  })
  
  output$dimReductionPlot1 <- renderPlot({
    req(input$groupByIdentity1) 
    color_set1 <- switch(input$groupByIdentity1,
                         "orig.sample" = three.colors,
                         "known_subtypes" = five.colors,
                         "novel_subtypes" = ten.colors,
                         "combined_ident" = four.colors)
    

    DimPlot_scCustom(seurat_object = sc,
                     reduction = "umap",
                     pt.size = 2,
                     group.by = input$groupByIdentity1,
                     label = FALSE,
                     colors_use = color_set1,
                     aspect_ratio = 1:1) +
      labs(title = NULL) +
      theme(legend.position="bottom")
  })
  
  output$dimReductionPlot2 <- renderPlot({
    req(input$groupByIdentity2)
    color_set2 <- switch(input$groupByIdentity2,
                         "orig.sample" = three.colors,
                         "known_subtypes" = five.colors,
                         "novel_subtypes" = ten.colors,
                         "combined_ident" = four.colors
    )
    
    DimPlot_scCustom(seurat_object = sc,
                     reduction = "umap",
                     pt.size = 2,
                     group.by = input$groupByIdentity2,
                     label = FALSE,
                     colors_use = color_set2,
                     aspect_ratio = 1:1) +
      labs(title = NULL) +
      theme(legend.position="bottom")
    
  })
  # Gene Expression
  output$dimReductionGeneExp <- renderPlot({
    req(input$geneName) 
    color_set1 <- switch(input$groupByIdentity1,
                         "orig.sample" = three.colors,
                         "known_subtypes" = five.colors,
                         "novel_subtypes" = ten.colors,
                         "combined_ident" = four.colors)
    
    cells <- FetchData(sc, vars = input$geneName, layer = "data")
    cells <- filter(cells, .data[[input$geneName]] >= 1)
    cells <- rownames(cells)
    cells1 <- setNames(list(cells), input$geneName)
    
    Cell_Highlight_Plot(seurat_object = sc,
                        cells_highlight = cells1,
                        highlight_color = "turquoise4",
                        pt.size = 2,
                        aspect_ratio = 1:1,
                        reduction = "umap") +
      theme(legend.position="right")
  })
  output$geneExpViolinPlot <- renderPlot({
    req(input$groupByIdentityGeneExp, input$geneName)
    color_set <- switch(input$groupByIdentityGeneExp,
                        "known_subtypes" = five.colors,
                        "novel_subtypes" = ten.colors)
    
    VlnPlot_scCustom(sc,
                     features = input$geneName,
                     group.by = input$groupByIdentityGeneExp,
                     log = FALSE,
                     add.noise = TRUE,
                     layer = "counts",
                     pt.size = 0,
                     colors_use = color_set) +
      NoLegend()
    
  })
  output$genePositiveCellsTable <- renderDataTable({
    req(input$geneName, input$groupByIdentityGeneExp)
    geneData <- FetchData(object = sc, vars = c(input$geneName, input$groupByIdentityGeneExp), layer = "data")
    
    colnames(geneData) <- c("Gene", "Identity")
    summaryData <- aggregate(geneData$Gene, by=list(Category=geneData$Identity), FUN=sum)
    colnames(summaryData) <- c("Identity", "Number of Detections")
    positiveCells <- geneData %>%
      filter(Gene >= 1) %>%
      group_by(Identity) %>%
      tally()
    summaryData$col3 <- positiveCells$n
    colnames(summaryData) <- c("Identity", "Number of Detections", "Number of Positive Cells")
    
    summaryData
  }, rownames= FALSE,
  extensions = 'Buttons', 
  options = list(dom = 'Blfrtip',
                 buttons = c('copy', 'csv')))
  # Co Expression
  output$coexpfig1 <- renderPlot({
    req(input$geneName2) 
    cells <- FetchData(sc, vars = input$geneName2, layer = "data")
    cells <- filter(cells, .data[[input$geneName2]] >= 1)
    cells <- rownames(cells)
    cells1 <- setNames(list(cells), input$geneName2)
    Cell_Highlight_Plot(seurat_object = sc,
                        cells_highlight = cells1,
                        highlight_color = "turquoise4",
                        pt.size = 2,
                        aspect_ratio = 1:1,
                        reduction = "umap") +
      theme(legend.position="right")
    
  })
  output$coexpfig2 <- renderPlot({
    req(input$geneName3)
    cells2 <- FetchData(sc, vars = input$geneName3, layer = "data")
    cells2 <- filter(cells2, .data[[input$geneName3]] >= 1)
    cells2 <- rownames(cells2)
    cells3 <- setNames(list(cells2), input$geneName3)
    Cell_Highlight_Plot(seurat_object = sc,
                        cells_highlight = cells3,
                        highlight_color = "tomato2",
                        pt.size = 2,
                        aspect_ratio = 1:1,
                        reduction = "umap") +
      theme(legend.position="right")
  })
  output$coexpfig3 <- renderPlot({
    req(input$geneName2, input$geneName3)
    cells4 <- FetchData(sc, vars = c(input$geneName2, input$geneName3), layer = "data")
    cells5 <- filter(cells4, .data[[input$geneName2]] >= 1 & .data[[input$geneName3]] >= 1)
    cells5 <- rownames(cells5)
    cells5 <- setNames(list(cells5), c("Both"))
    
    Cell_Highlight_Plot(seurat_object = sc,
                        cells_highlight = cells5,
                        highlight_color = "purple3",
                        pt.size = 2,
                        aspect_ratio = 1:1,
                        reduction = "umap") +
      theme(legend.position="right")
    
  })
  output$groupcoexptable <- renderDataTable({
    req(input$geneName2, input$geneName3, input$groupcoexp)  # Ensure that a gene name is provided
    geneData <- FetchData(object = sc, vars = c(input$geneName2,input$geneName3,input$groupcoexp), layer = "data")
    colnames(geneData) <- c("Gene1", "Gene2", "Identity")
    summaryData <- aggregate(list(geneData$Gene1, geneData$Gene2), by=list(Category=geneData$Identity), FUN=sum)
    colnames(summaryData) <- c("Identity", "Gene1 Detections", "Gene2 Detections")
    positiveCells.1 <- geneData %>%
      filter(Gene1 >= 1) %>%
      group_by(Identity) %>%
      tally()
    positiveCells.2 <- geneData %>%
      filter(Gene2 >= 1) %>%
      group_by(Identity) %>%
      tally()
    positiveCells.3 <- geneData %>%
      filter(Gene1 >= 1 & Gene2 >= 1) %>%
      group_by(Identity) %>%
      tally()
    unique_categories <- union(unique(positiveCells.1$Identity), unique(positiveCells.2$Identity))
    positiveCells.1 <- merge(x = data.frame(Identity = unique_categories), y = positiveCells.1, by = "Identity", all.x = TRUE)
    positiveCells.2 <- merge(x = data.frame(Identity = unique_categories), y = positiveCells.2, by = "Identity", all.x = TRUE)
    positiveCells.3 <- merge(x = data.frame(Identity = unique_categories), y = positiveCells.3, by = "Identity", all.x = TRUE)
    
    # Replace NA with 0
    positiveCells.1$n[is.na(positiveCells.1$n)] <- 0
    positiveCells.2$n[is.na(positiveCells.2$n)] <- 0
    positiveCells.3$n[is.na(positiveCells.3$n)] <- 0
    # Perform the aggregation
    summarygenData <- aggregate(list(positiveCells.1$n, positiveCells.2$n, positiveCells.3$n), by = list(Category = positiveCells.1$Identity), FUN = sum, na.rm = TRUE)
    colnames(summarygenData) <- c("Identity", "Gene 1 Number of Cells", "Gene 2 Number of Cells", "Cells Positive for both Gene 1 & Gene 2")
    summaryData$col4 <- summarygenData$`Gene 1 Number of Cells`
    summaryData$col5 <- summarygenData$`Gene 2 Number of Cells`
    summaryData$col6 <- summarygenData$`Cells Positive for both Gene 1 & Gene 2`
    colnames(summaryData) <- c("Identity", "Gene 1 Number of Detections", "Gene 2 Number of Detections", "Gene 1 Number of Positive Cells", "Gene 2 Number of Positive Cells", "Cells Positive for both Gene 1 & Gene 2")
    
    summaryData
  }, rownames= FALSE,
  extensions = 'Buttons', 
  options = list(dom = 'Blfrtip',
                 buttons = c('copy', 'csv')))
  
  # Diff Exp
  output$diffexpfig1 <- renderPlot({
    req(input$geneName4)
    cells6 <- FetchData(sc, vars = input$geneName4, layer = "data")
    cells6 <- filter(cells6, .data[[input$geneName4]] >= 1)
    cells6 <- rownames(cells6)
    cells7 <- setNames(list(cells6), input$geneName4)
    
    Cell_Highlight_Plot(seurat_object = sc,
                        cells_highlight = cells7,
                        highlight_color = "turquoise4",
                        pt.size = 2,
                        aspect_ratio = 1:1,
                        reduction = "umap") +
      theme(legend.position="right")
    
  })
  
  
  output$diffexptable <- renderDataTable({
    req(input$geneName4)
    cells8 <- FetchData(sc, vars = input$geneName4, layer = "data")
    cells8 <- filter(cells8, .data[[input$geneName4]] >= 1)
    cells8 <- rownames(cells8)
    remaining_cells <- setdiff(colnames(sc), cells8)
    ident_2_cells <- sample(remaining_cells, length(remaining_cells) / 2)

  
    df <- FindMarkers(sc,
                      ident.1 = cells8,
                      ident.2 = ident_2_cells,
                      slot = "data",
                      only.pos = TRUE,
                      logfc.threshold = 0.25,
                      min.diff.pct = 0.125)
    
    df <- df[!grepl("^Gm", rownames(df)), ]
    df <- df[!grepl("Rik$", rownames(df)), ]
    df <- df[!grepl("^mt-", rownames(df)), ]
    
    df$pct_ratio = df$pct.1 / df$pct.2
    
    df <- filter(df, !(rownames(df) %in% input$geneName4))
    
    df <- df %>%
      filter(p_val_adj <= 0.05) %>%
      arrange(desc(avg_log2FC)) %>%
      select(-p_val) %>%
      relocate(pct_ratio, .after = pct.2)
    
    df$avg_log2FC <- format(round(df$avg_log2FC, 3), nsmall = 3)
    df$pct_ratio <- format(round(df$pct_ratio, 3), nsmall = 3)
    
    colnames(df) <- c("Log2FC", "% Expressed in Targeted Cells", "% Expressed in All Other Cells", "% Ratio", "Adjusted p-value")
    df
  }, rownames= TRUE,
  extensions = 'Buttons', 
  options = list(dom = 'Blfrtip',
                 buttons = c('copy', 'csv')))
  
  
  # Clustering
  
  output$kmarkertab <- DT::renderDataTable({
    req(input$kmarkergroup)
    
    df <- known_markers %>%
      filter(Cluster == input$kmarkergroup) %>%
      arrange(desc(PCT.Ratio))
    
    df
  }, rownames= FALSE,
  extensions = 'Buttons', 
  options = list(pageLength = 10,
                 dom = 'Blfrtip',
                 buttons = c('copy', 'csv')))
  
  output$nmarkertab <- DT::renderDataTable({
    req(input$nmarkergroup)
    
    df <- novel_markers %>%
      filter(Cluster == input$nmarkergroup) %>%
      arrange(desc(PCT.Ratio))
    
    df
  }, rownames= FALSE,
  extensions = 'Buttons', 
  options = list(pageLength = 10,
                 dom = 'Blfrtip',
                 buttons = c('copy', 'csv')))
}
