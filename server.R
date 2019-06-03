options(shiny.maxRequestSize = 1000*1024^2) 

function(input, output, session){
  
  
  # Reactive table with Compute button - execute filtering
  # transList <- eventReactive(input$goComputation, {
    
    data<-reactive({
      inFile<-input$file1
      if (is.null(inFile))
        return(NULL)
      read.csv(inFile$datapath, header = input$header,sep = ";")   
      #NB : la fonction reactive permet de creer des variables reutilisables ailleurs dans le code
    })#pour la variable "data", elle est reutilisee ailleurs sous la forme data(), il ne faut pas oublier de mettre les parentheses
    #variable "contents" contenant la premiere ligne du tableau 
    
     g1 <- eventReactive(input$goComputation,{
       # 1. plot visNetwork
      require(data.table)
       setDT(data()[,!(names(data()) %in% c(input$var_explic))])

       order_matrix <- as(as.matrix(data()[,!(names(data()) %in% c(input$var_explic))]),"transactions") # data()[,!(names(data()) %in% c(input$var_explic))]
       jaccard_matrix <- as.matrix(as.data.frame(affinity(order_matrix)))
       
       jaccgraph <- igraph::graph_from_adjacency_matrix(jaccard_matrix, weighted = T,mode = "undirected")
       if (is.null(jaccgraph))
         return(NULL)  
     temp.g <- jaccgraph
      temp.g <- igraph::delete.edges(temp.g, E(temp.g)[weight < input$alpha1])
       temp.g <- igraph::delete.vertices(temp.g, degree(temp.g) == 0)
         
     m2 <- membership(cluster_fast_greedy(temp.g))
       d2 <- igraph::graph.strength(temp.g)
       if (is.null(d2))
         return(NULL)   
      temp.g <- toVisNetworkData(temp.g)
      if (is.null(temp.g))
        return(NULL)   
       temp.g$edges$value <- temp.g$edges$weight
       temp.g$nodes$group <- factor(m2)
       temp.g$nodes$value<- d2
       temp.g$nodes$font.color <- "black"  # to delete in case it's wrong
       temp.g$nodes$font.size <- 20
       
       # 2. Apriori rules
       
       rules <- apriori(order_matrix, parameter = list(supp = input$support, conf = input$confidence, target = "rules",maxlen=input$length))# list(supp = 0.01, conf = 0.7, target = "rules",maxlen=2))
       trans <- data.frame(
         lhs = labels(lhs(rules)),
         rhs = labels(rhs(rules)),
         quality(rules)
       )
       
       plot3=inspectDT(rules)
       plot7=plotly_arules(rules)#plot(rules2,method="graph",control=list(type="items",layout=layout.fruchterman.reingold,))#,interactive=TRUE,shading=NA)

      
       
       # eliminate brackets
       trans$lhs <- gsub("[{}]", "", as.character(trans$lhs))
       trans$rhs <- gsub("[{}]", "", as.character(trans$rhs))
       # add rule character field
       trans$rule <- paste0(trans$lhs," => ", trans$rhs)
       
       #sort table
       trans <- trans[order(-trans$lift),]
       
       # only lift > 1
       trans <- trans[trans$lift >= 1, ]
       topRules <- trans[1:30, "rule"]
       
       # tooltip content
       
       trans$tooltrip <- paste0("<b>Lift =", trans$lift, " Confidence = ", trans$confidence, "</b>")
       
       trans$lift <- round(trans$lift, 2)
       trans$confidence <- round(trans$confidence, 2)
       trans$support <- round(trans$support, 5)
       
       #reshape table for graph
       
       trans <- melt(trans, id = c("lhs", "rhs", "rule"))
       trans <- trans[trans$variable == "lift" | trans$variable == "confidence",]
       trans$variable <- ifelse(trans$variable == "lift", "Lift", "Confidence")
       trans <- trans[order(trans$variable),]
       
       trans <- data.table(trans)
       
       ruleGraph <- trans[rule %in% topRules]
       
       # GRAPHS
       p <-   ggplot(data.table(size(order_matrix))[, .(V2 = ifelse(V1 > 5, "6+", as.character(V1)))
                                                    ][, .(Prop = round(100*(.N)/length(order_matrix),2)), by = .(V2)]
                     , aes(x = factor(V2, levels = c("1", "2", "3", "4", "5", "6+")), y = Prop)) +
         geom_bar(stat = 'identity', alpha = 0.7, fill = "#54BCD4") +
         geom_label(aes(label = paste0(round(Prop, 2),"%"))) +
         ggtitle(paste("Total:", length(order_matrix), "baskets,", sum(size(order_matrix)), "items")) +
         xlab("Number of items per basket") +
         theme_minimal(base_family = "Metric Bold", base_size = 20) +
         coord_flip()
       
       # mesh distribution plot
       t <- data.table(itemName = names(itemFrequency(order_matrix)), freq= itemFrequency(order_matrix))[order(-freq)][1:20]
       g <-   ggplot(data = t,
                     aes(x = factor(itemName, levels = rev(t$itemName)), y = freq)) +
         geom_bar(stat = "identity", alpha = 0.7, fill = "#54BCD4") + 
         geom_label(aes(label = paste0(round(100*freq,2),"%")), size = 3) +
         scale_y_continuous(labels = scales::percent) + 
         xlab("Sector activity") + ylab("Frequency") + 
         theme_minimal(base_family = "Metric Bold", base_size = 20) +
         coord_flip()
       
       # create graphic of cross-sell potential
       d1 <- igraph::graph.strength(jaccgraph)
       
       # In the branch of mathematics called graph theory, the strength of an undirected graph 
       # corresponds to the minimum ratio edges removed/components created in a decomposition 
       # of the graph in question. It is a method to compute partitions of the set of vertices and 
       # detect zones of high concentration of edges
       
       
       # details : http://r-statistics.co/Association-Mining-With-R.html
       
       d1 <- sort(d1, decreasing = T)
       
       d_dt <- data.table(cat = names(d1), stren = d1)
       
       Encoding(d_dt$cat) <- "latin1"
       
       s <- ggplot(d_dt[1:20], aes(x = factor(cat, levels = rev(d_dt$cat[1:20])), y = stren)) +
         geom_bar(stat = "identity", alpha = 0.7, fill = "#54BCD4") +
         coord_flip() +
         xlab("Field") + ylab("Cross-sell potential") + 
         theme_minimal(base_family = "Metric Bold", base_size = 20)
       
      most_frequent <- itemFrequencyPlot(order_matrix, topN=10, type="absolute", main="Item Frequency")
       
         
       return(list("aaa"=temp.g,"cross_potential"=s,"catdistri"=g,
                   "sizeplot" = p,"ruleGraph" = ruleGraph,"trans2" = trans,"mfi"=most_frequent,"plot3"=plot3,"plot7"=plot7))}) 
     
     
     
     ######################################################################################################################################
     
     g2 <- eventReactive(input$goComputation1,{
       require(data.table)
       setDT(data()[,!(names(data()) %in% c(input$var_explic))])
       order_matrix1 <- as(as.matrix(data()[,!(names(data()) %in% c(input$var_explic))]),"transactions") # data()[,!(names(data()) %in% c(input$var_explic))]
       
       
       # 2. Apriori rules
       
       rules1 <- apriori(order_matrix1, parameter = list(supp = input$support, conf = input$confidence, target = "rules",maxlen=input$length),appearance = list (default="lhs",rhs=c(input$var_select)))# list(supp = 0.01, conf = 0.7, target = "rules",maxlen=2))
       trans1 <- data.frame(
         lhs = labels(lhs(rules1)),
         rhs = labels(rhs(rules1)),
         quality(rules1)
       )
       
       # eliminate brackets
       trans1$lhs <- gsub("[{}]", "", as.character(trans1$lhs))
       trans1$rhs <- gsub("[{}]", "", as.character(trans1$rhs))
       # add rule character field
       trans1$rule <- paste0(trans1$lhs," => ", trans1$rhs)
       
       #sort table
       trans1 <- trans1[order(-trans1$lift),]
       
       # only lift > 1
       trans1 <- trans1[trans1$lift >= 1, ]
       topRules <- trans1[1:30, "rule"]
       
       # tooltip content
       
       trans1$tooltrip <- paste0("<b>Lift =", trans1$lift, " Confidence = ", trans1$confidence, "</b>")
       
       trans1$lift <- round(trans1$lift, 2)
       trans1$confidence <- round(trans1$confidence, 2)
       trans1$support <- round(trans1$support, 5)
       
       #reshape table for graph
       
       trans1 <- melt(trans1, id = c("lhs", "rhs", "rule"))
       trans1 <- trans1[trans1$variable == "lift" | trans1$variable == "confidence",]
       trans1$variable <- ifelse(trans1$variable == "lift", "Lift", "Confidence")
       trans1 <- trans1[order(trans1$variable),]
       
       trans1 <- data.table(trans1)
       
       rules2 <- apriori(order_matrix1, parameter = list(supp = input$support, conf = input$confidence, target = "rules",maxlen=input$length),appearance = list(lhs=c(input$var_select),default="rhs"))# list(supp = 0.01, conf = 0.7, target = "rules",maxlen=2))
       
       rules3 <- apriori(order_matrix1, parameter = list(supp = input$support, conf = input$confidence, target = "rules",maxlen=input$length),appearance = list(rhs=c(input$var_select),default="lhs"))# list(supp = 0.01, conf = 0.7, target = "rules",maxlen=2))
       
       
       plot1=plotly_arules(rules2)#plot(rules2,method="graph",control=list(type="items",layout=layout.fruchterman.reingold,))#,interactive=TRUE,shading=NA)
       plot2=inspectDT(rules2)
       
       plot5=plotly_arules(rules3)#plot(rules2,method="graph",control=list(type="items",layout=layout.fruchterman.reingold,))#,interactive=TRUE,shading=NA)
       plot6=inspectDT(rules3)
       
       trans2 <- data.frame(
         lhs = labels(lhs(rules2)),
         rhs = labels(rhs(rules2)),
         quality(rules2)
       )
       
       # eliminate brackets
       trans2$lhs <- gsub("[{}]", "", as.character(trans2$lhs))
       trans2$rhs <- gsub("[{}]", "", as.character(trans2$rhs))
       # add rule character field
       trans2$rule <- paste0(trans2$lhs," => ", trans2$rhs)
       
       #sort table
       trans2 <- trans2[order(-trans2$lift),]
       
       # only lift > 1
       trans2 <- trans2[trans2$lift >= 1, ]
       topRules <- trans2[1:30, "rule"]
       
       # tooltip content
       
       trans2$tooltrip <- paste0("<b>Lift =", trans2$lift, " Confidence = ", trans2$confidence, "</b>")
       
       trans2$lift <- round(trans2$lift, 2)
       trans2$confidence <- round(trans2$confidence, 2)
       trans2$support <- round(trans2$support, 5)
       
       #reshape table for graph
       
       trans2 <- melt(trans2, id = c("lhs", "rhs", "rule"))
       trans2 <- trans2[trans2$variable == "lift" | trans2$variable == "confidence",]
       trans2$variable <- ifelse(trans2$variable == "lift", "Lift", "Confidence")
       trans2 <- trans2[order(trans2$variable),]
       
       trans2 <- data.table(trans2)

       return(list("plot1" = plot1,"plot2"=plot2,"trans3" = trans1,"trans4"=trans2,"plot5"=plot5,"plot6"=plot6))
       })
     
     ###########################################################################################################

     
     ######################################################################################################################################
     
     output$network1 <- renderVisNetwork({ 
      visNetwork(g1()[["aaa"]]$nodes, g1()[["aaa"]]$edges) %>% 
        visInteraction(navigationButtons = TRUE) %>% 
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
     })
     
     output$slider_explic <- renderUI({
       selectInput("var_explic", "quelles colonnes supprimer ?",choices=c(names(data())),selectize = TRUE,multiple = TRUE)
     })
     
     output$slider_select <- renderUI({
       selectInput("var_select", "quelle variable selectionner ?",choices=c(names(data())),selectize = TRUE)
     })
     
     # output$slider_supp <- renderUI({
     #   selectInput("var_support", "quelles colonnes supprimer ?",choices=c(names(data())),selectize = TRUE,multiple = TRUE)
     # })
     # 
     # output$slider_conf <- renderUI({
     #   selectInput("var_explic", "quelles colonnes supprimer ?",choices=c(names(data())),selectize = TRUE,multiple = TRUE)
     # })
     # 
     # output$slider_maxlen <- renderUI({
     #   selectInput("var_explic", "quelles colonnes supprimer ?",choices=c(names(data())),selectize = TRUE,multiple = TRUE)
     # })
     
     
     output$crosspotential <- renderPlot({
       g1()[["cross_potential"]]
     })
     
     output$table <- DT::renderDataTable({
       DT::datatable(data = g1()[["trans2"]], 
                     options = list(pageLength = 20), 
                     rownames = FALSE)})
     
     output$plot1 <- renderPlotly({
       g2()[["plot1"]]
     })
     
     output$plot2 <-renderDataTable(
      g2()[["plot2"]]
     )
     
     output$plot3 <-renderDataTable(
       g1()[["plot3"]]
     )
     
     output$plot5 <-renderPlotly(
       g2()[["plot5"]]
     )
     
     output$plot6 <-renderDataTable(
       g2()[["plot6"]]
     )
     
     output$plot7 <-renderPlotly(
       g1()[["plot7"]]
     )
     
     
     
     output$table1 <- DT::renderDataTable({
       DT::datatable(data = g2()[["trans3"]], 
                     options = list(pageLength = 10), 
                     rownames = FALSE)})
     
     output$table2 <- DT::renderDataTable({
       DT::datatable(data = g2()[["trans4"]], 
                     options = list(pageLength = 10), 
                     rownames = FALSE)})
     
     
     output$sizeplot <- renderPlot({
       g1()[["sizeplot"]]
     })
     
     output$freqplot <- renderPlot({
       g1()[["catdistri"]]
     })
     
     output$mfi <- renderPlot({
       g1()[["mfi"]]
     })
    
     ######################################################
     
      #get position info
      observeEvent(input$store_position, {
      visNetworkProxy("network1") %>% visGetPositions()
      })
     
      # format positions
      nodes_positions <- reactive({
        positions <- input$network_positions
        if(!is.null(positions)){
          nodes_positions <- do.call("rbind", lapply(positions, function(x){ data.frame(x = x$x, y = x$y)}))
         nodes_positions$id <- names(positions)
          nodes_positions
        } else {
          NULL
        }
      })
     
     output$downloadNetwork <- downloadHandler(
       filename = function() {
         paste('network-', Sys.Date(), '.html', sep='')
       },
       content = function(con) {
         nodes_positions <- nodes_positions()
         if(!is.null(nodes_positions)){
           nodes_save <- merge(g1()[["aaa"]]$nodes, nodes_positions, by = "id", all = T)
         } else  {
           nodes_save <-g1()[["aaa"]]$nodes
         }
         
         visNetwork(nodes = nodes_save, g1()[["aaa"]]$edges) %>% 
           visInteraction(navigationButtons = TRUE) %>% 
           visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% visSave(con, background = "white")
       }
     )
     
}
     

    