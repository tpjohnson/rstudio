# server.r alteredData app

# Server ----
shinyServer(function(input, output) {
  # tab 1: bars ----
  output$html.barplot <- renderUI({
    HTML("Our data contain information about computers used at different companies.  There are over 8,000 observations in this dataset, where each observation is a unique computer found at a company.  For each computer, we are given information about which programs they have and characteristics about the company they belong to. <br>
         These data were collected by Microsoft to discover what characteristics are associated with someone using their programs or a competitor's programs. <br>
         I have hid the names of the actual programs for data confidentiality reasons. <br> 
         Use the bar chart below to explore the data, and better understand them.  The counts of the programs are found in table below.")
  })
  # actual bars
  output$bars <- renderPlot({
    if(input$x3 %in% names(programs.list) & input$y3 %in% names(programs.list)){
      bars <- a[,c(unlist(programs.list[input$x3]), unlist(programs.list[input$y3])), with = F ]
      bars <- melt.data.table(bars, measure.vars = unlist(programs.list[input$x3]))
      bars <- bars[value > 0,]
      bars$value <- NULL
      bars <- melt.data.table(bars, id.vars = "variable")
      bars <- bars[value > 0,]
      fish <- bars[,sum(value), by = "variable"]
      bars <- data.table(left_join(bars, fish, by = "variable"))
      bars$percent <- bars$value/bars$V1
      barsPlot <- ggplot(bars, aes(x = variable, y = percent, fill = factor(variable.1))) + geom_col() + scale_fill_manual("Program", values = spectral) + labs(title = "Given that a company has x program, here is their percentage breakdown of y program group", x = input$x3)
      
    }else if(input$y3 %in% names(programs.list)){
      
      bars <- a[,c(input$x3, unlist(programs.list[input$y3])), with = F]
      keys <- setdiff(names(bars), input$x3)
      bars <- bars[,lapply(.SD, sum), by = c(input$x3), .SDcols = keys]
      bars <- melt.data.table(bars, id.vars = c(input$x3))
      fish <- bars[,sum(value), by = c(input$x3)]
      bars <- data.table(left_join(bars, fish, by = input$x3))
      bars$percent <- bars$value/bars$V1
      barsPlot <- ggplot(bars, aes(x = get(input$x3), y = percent, fill = factor(variable))) + geom_col() + scale_fill_manual("Program", values = c(spectral)) + labs(title = "Given that a company is in x grouping, these are the percentage breakdowns of y program group", x = input$x3)
      
    }else if(input$x3 %in% names(programs.list)){
      bars <- a[,c(unlist(programs.list[input$x3]), input$y3), with = F]
      bars <- melt.data.table(bars, id.vars = input$y3)
      bars <- bars[value > 0,]
      fish <- bars[,sum(value), by = variable]
      bars <- data.table(left_join(bars, fish, by = "variable"))
      bars$percent <- bars$value/bars$V1
      barsPlot <- ggplot(bars, aes(x = variable, y = percent, fill = factor(get(input$y3)))) + geom_col() + scale_fill_manual(input$y3, values = spectral) + labs(title = "Given that a company has x program, they are this likely to be in y category", x = input$x3)
      
    }else{
      bars <- a[,c(input$x3, input$y3), with = F]
      bars <- bars[, .N, by = c(input$x3, input$y3)] # total id count for each combination
      myTable <- bars
      fish <- a[, .N, by = c(input$x3)]
      bars <- data.table(left_join(bars, fish, by = input$x3))
      bars$percent <- bars$N.x/bars$N.y
      barsPlot <- ggplot(bars, aes(x = get(input$x3), y = percent, fill = factor(get(input$y3)))) + geom_col() + scale_fill_manual(input$y3, values = spectral) + labs(title = "Given that a company falls under x category, they are this likely to be in y category as well", x = input$x3)
      
    }
    
    barsPlot
    
  })
  # table
  output$plotTable <- DT::renderDataTable({ # EDIT
    if(input$x3 %in% names(programs.list) & input$y3 %in% names(programs.list)){ # x program, y program: works
      myTable <- a[,c(unlist(programs.list[input$y3]), unlist(programs.list[input$x3])), with = F ]
      myTable <- melt.data.table(myTable, id.vars = c(unlist(programs.list[input$x3])))
      myTable <- myTable[value > 0,]
      keys <- setdiff(names(myTable), c("variable", "value"))
      myTable <- myTable[, lapply(.SD, sum), by = variable, .SDcols = keys]
      
    }else if(input$y3 %in% names(programs.list)){ # x other, y program: 
      
      myTable <- a[,c(input$x3, unlist(programs.list[input$y3])), with = F]
      keys <- setdiff(names(myTable), input$x3)
      myTable <- myTable[,lapply(.SD, sum), by = c(input$x3), .SDcols = keys]
      myTable <- data.table(myTable)
      myTable <- myTable[order(get(input$x3)),]
      myTable <- data.frame(myTable)
      r.names <- myTable[,1]
      myTable <- t(myTable[,-1])
      colnames(myTable) <- r.names
      myTable <- cbind("Color" = rownames(myTable), myTable)
      
    }else if(input$x3 %in% names(programs.list)){ # x program, y other: works
      # Of those companies that use x program, what percentage of them are in the breakdown of y categories
      myTable <- a[, c(unlist(programs.list[input$x3]), input$y3), with = F]
      keys <- setdiff(names(myTable), input$y3)
      myTable <- myTable[,lapply(.SD, sum), by = c(input$y3), .SDcols = keys]
      
    }else{ # x other, y other: works
      # of those companies in x category, what is there breakdown in another category
      myTable <- a[,c(input$x3, input$y3), with = F]
      myTable <- myTable[, .N, by = c(input$x3, input$y3)]
      form <- as.formula(paste(c(input$x3, input$y3), collapse = "~"))
      myTable <- dcast.data.table(myTable, form, value.var = "N")
      myTable[is.na(myTable)] <- 0
      myTable <- data.frame(myTable)
      tab.names <- myTable[,1]
      myTable <- t(myTable[,-1])
      colnames(myTable) <- tab.names
      myTable <- cbind("Color" = rownames(myTable), myTable)
    }
    
    DT::datatable(
      myTable, rownames = F, extensions = "FixedColumns", options = list(scrollX = T, fixedColumns = list(LeftColumns = 1))
    )
  })  
  # tab 2: lm ----
  tableLm <- reactive({
    m.lm    <- lm(as.formula(paste("percent_ms_prog ~ ", paste(input$checkboxLm, collapse = "+"))), data = da)
    lmTable <- data.table(var = names(m.lm$coefficients), value = m.lm$coefficients)[-1,]
    lmTable <- lmTable[order(-value),]
    
    input$updateLm
    isolate(lmTable)
  })
  
  output$tableLm <- DT::renderDataTable({
    tableLm()
  })
  
  
  output$plotFmodel <- renderPlot({
    m.glm <- glm(as.formula(paste("percent_ms_prog ~ ", paste(input$checkboxLm, collapse = "+"))), data = da)
    fmodel(m.glm)
  })  
  # tab 3: pca ----
  output$pca.html <- renderUI({
    HTML(("Using PCA to reduce the number of dimensions to better visualize cluster patterns in the data.  <br>  Build your own clustering algorithm with the options below to find relationships between certain devices that got clustered together."))
  })
  
  output$cluster_plot <- renderPlot({
    
    if(input$pcaScaled == "yes"){
      scale <- TRUE
    }else{
      scale <- FALSE
    }
    
    pca <- prcomp(pcaData, scale. = scale)
    set.seed(123)
    
    if(input$pcaAlgorithm == "kmeans"){
      pca_kmeans <- kmeans(pca$x[,1:input$pcaNumPcs], centers = input$pcaNumClusters, nstart = 3)
      pca_plot   <- fviz_pca_ind(pca, habillage = pca_kmeans$cluster, addEllipses = T, ellipse.level = .68) + theme_minimal()
    }else{
      hclust.dist <- dist(pca$x[, 1:input$pcaNumPcs], method = "euclidean")
      pca_hclust  <- hclust(hclust.dist, method = "single")
      hclust.tree <- cutree(pca_hclust, eval(input$pcaNumClusters))
      hclust.data <- data.table(cluster = hclust.tree, da)
      pca_plot    <- fviz_pca_ind(pca, habillage = hclust.data$cluster, addEllipses = T, ellipse.level = .68) + theme_minimal()
    }
    
    pca_plot
    
  })
  
  # pca 3D plot
  options(rgl.useNULL = T)
  save <- options(rgl.inShiny = T)
  open3d()
  output$pca_3d_plot <- renderRglwidget({
    if(input$pcaScaled == "yes"){
      scale <- TRUE
    }else{
      scale <- FALSE
    }
    
    pca <- prcomp(pcaData, scale. = scale)
    set.seed(123)
    
    if(input$pcaAlgorithm == "kmeans"){
      pca_kmeans <- kmeans(pca$x[,1:input$pcaNumPcs], centers = input$pcaNumClusters, nstart = 3)
      palette(c(jtaColors, moreColors))
      pca.3d <- plot3d(x = pca$x[,1], y = pca$x[,2], z = pca$x[,3], xlab = "PC1", ylab = "PC2", zlab = "PC3", col = factor(pca_kmeans$cluster))
      scene <- scene3d()
      rgl.close()
      rglwidget(scene)
      
      
    }else{
      hclust.dist <- dist(pca$x[, 1:input$pcaNumPcs], method = "euclidean")
      pca_hclust  <- hclust(hclust.dist, method = "single")
      hclust.tree <- cutree(pca_hclust, eval(input$pcaNumClusters))
      hclust.data <- data.table(cluster = hclust.tree, da)
      palette(c(jtaColors, moreColors))
      pca.3d <- plot3d(x = pca$x[,1], y = pca$x[,2], z = pca$x[,3], xlab = "PC1", ylab = "PC2", zlab = "PC3", col = factor(hclust.tree))
      scene <- scene3d()
      rgl.close()
      rglwidget(scene)
    }
  })
  
  
  output$cluster_table <- DT::renderDataTable({
    if(input$pcaScaled == "yes"){
      scale <- TRUE
    }else{
      scale <- FALSE
    }
    
    pca <- prcomp(pcaData, scale. = scale)
    set.seed(123)
    
    if(input$pcaAlgorithm == "kmeans"){
      pca_kmeans <- kmeans(pca$x[,1:input$pcaNumPcs], centers = input$pcaNumClusters, nstart = 3)
      DTtable    <- data.table(cluster = pca_kmeans$cluster, da)
    }else{
      hclust.dist <- dist(pca$x[, 1:input$pcaNumPcs], method = "euclidean")
      pca_hclust  <- hclust(hclust.dist, method = "single")
      hclust.tree <- cutree(pca_hclust, eval(input$pcaNumClusters))
      hclust.data <- data.table(cluster = hclust.tree, da)
      DTtable     <- data.table(hclust.data)
    }
    
    DT::datatable(DTtable, extensions = "FixedColumns", options = list(scrollX = T, fixedColumns = list(LeftColumns = 2)))
    
  })
  
  
  # tab 4: arules ----
  ## ANALYSIS ON EACH COMPUTER
  
  # output$arulesPlot <- renderPlot({
  #   itemFrequencyPlot(transactions, topN = input$slider_top_items)
  # })
  # 
  # output$mkt_basket_table <- DT::renderDataTable({
  #   apriori.output <- apriori(transactions, parameter = list(minlen = 2, maxlen = 3, supp = input$slider_support))
  #   apriori.data <- inspect(apriori.output)
  #   apriori.data <- data.table(apriori.data)
  #   apriori.data <- apriori.data[order(-support),]
  #   apriori.data
  # })
  
  ## ANALYSIS ON EACH ORG
  
  output$arulesPlot <- renderPlot({
    itemFrequencyPlot(arules.transactions, topN = input$slider_top_items)
  })
  
  output$mkt_basket_table <- DT::renderDataTable({
    apriori.output <- apriori(arules.transactions, parameter = list(minlen = 2, maxlen = 2, supp = input$slider_support))
    apriori.data <- inspect(apriori.output)
    apriori.data <- data.table(apriori.data)
    apriori.data <- apriori.data[order(-support),]
    apriori.data
  })
  
  output$arules_text <- renderUI({
    HTML(paste("Here we use the 'apriori' association rules algorithm to discover relationships between variables.", "Given that a company has the programs listed on the left side, we want to predict what other programs they might use that are listed on the right side.", "Support = The probability that a random company has both programs in the lhs and rhs columns.", "Confidence = The probability that a company uses the program on the rhs, given that it already uses the program on the lhs.", "Lift = A company is this many times more likely to use the program on the rhs with the program on the lhs, than without.", sep = "<br>"))
  })
  
  # tab 4.5: arules2 ----
  output$apJelly <- renderVisNetwork({
    apOut <- apriori(arules.transactions, parameter = list(minlen = 2, maxlen = 2, supp = input$slider_support2))
    ap.inspect <- data.table(inspect(apOut))
    ap.inspect <- ap.inspect[order(-support)]
    ap.inspect$lhs <- lapply(ap.inspect$lhs, function(x){ gsub('[:{:]', '', gsub('[:}:]', '', x)) })
    ap.inspect$rhs <- lapply(ap.inspect$rhs, function(x){ gsub('[:{:]', '', gsub('[:}:]', '', x)) })
    ap.inspect$V2 <- NULL
    # edges data
    ap.edges <- ap.inspect
    names(ap.edges) <- c("from", "to", "support", "confidence", "lift")
    #ap.edges$value <- (ap.edges$lift)*.4 #useful info, but makes the graphic look bad
    ap.edges$color <- "gray"
    #ap.edges[, color := ifelse(lift >= 50, redGradient[1], ifelse(lift < 50 & lift >= 45, redGradient[2], ifelse(lift < 45 & lift >= 40, redGradient[3], ifelse(lift < 40 & lift >= 35, redGradient[4], ifelse(lift < 35 & lift >= 30, redGradient[5], ifelse(lift < 30 & lift >= 20, redGradient[6], ifelse(lift < 20 & lift >= 5, redGradient[7], ifelse(lift < 5 & lift >= 2, redGradient[8], ifelse(lift < 2 & lift >= 1, redGradient[9], redGradient[10])))))))))]
    ap.edges$arrows <- "middle"
    ap.edges$smooth <- T
    ap.edges$shadow <- FALSE
    # nodes data
    ap.nodes <- data.table(id = unique(c(ap.edges$from, ap.edges$to)))
    ap.nodes[, label := id]
    ap.nodes[, color.background := ifelse(label %in% unlist(programs.list[1]), colorz[1], ifelse(label %in% unlist(programs.list[2]), colorz[2], ifelse(label %in% unlist(programs.list[3]), colorz[3], ifelse(label %in% unlist(programs.list[4]), colorz[4], ifelse(label %in% unlist(programs.list[5]), colorz[5], colorz[6])))))]
    ap.nodes[, program.type := ifelse(label %in% unlist(programs.list[1]), names(programs.list[1]), ifelse(label %in% unlist(programs.list[2]), names(programs.list[2]), ifelse(label %in% unlist(programs.list[3]), names(programs.list[3]), ifelse(label %in% unlist(programs.list[4]), names(programs.list[4]), ifelse(label %in% unlist(programs.list[5]), names(programs.list[5]), names(programs.list[6]))))))]
    ap.nodes$shape <- "dot"
    ap.nodes$shadow <- T
    ap.nodes$color.border <- "black"
    ap.nodes$color.highlight.background <- "white"
    
    visNetwork(nodes = ap.nodes, edges = ap.edges) %>% visOptions(highlightNearest = list(enabled = TRUE), selectedBy = "program.type")
    
  })
  # tab5: lda ----
  
  output$ldaPlotly <- renderPlotly({
    
    chosenVars <- c(input$checkboxLda)
    m.lda      <- lda(as.formula(paste("ms_fan ~ ", paste(chosenVars, collapse = "+"))), data = da)
    ldaTable   <- data.table(var = rownames(m.lda$scaling), coef = as.numeric(m.lda$scaling), y = 0)
    ldaTable   <- ldaTable[order(-coef),]
    ldaPlot    <- plot_ly(data = ldaTable, x = ~coef, y = ~y, type = "scatter", mode = "markers", text = ~var, color = "red")
    ldaPlot %>% layout(yaxis = list(range = c(-.1,.1)))
    
  })
  
  })



