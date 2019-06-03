
library("arules")
data('Adult')
library(arules)
library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(ggthemes)
library(viridis)
library(extrafont)
library(DT)
library(plotly)
library(Cairo)
library(igraph)
library(visNetwork)
library(dplyr)
library(magrittr)


require(data.table) # v1.9.0+
#setDT(order_matrix)

library(readr)
nmc_code1_tg_vf <- read_delim("N:/60. Données DMS AfterSales/1. France/3. Marketing Strategy/3. AS targeting/5. Mesure de performance/17. basket analysis/6. webApp/1. BDD/nmc_code1_tg_vf.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)


order_matrix <- as(as.matrix(nmc_code1_tg_vf),"transactions")

rules1 <- apriori(order_matrix, parameter = list(supp = 0.001, conf = 0.8, target = "rules",maxlen=2))# list(supp = 0.01, conf = 0.7, target = "rules",maxlen=2))

plot1=plot(rules1,method="graph",shading=NA)
plot1=plotly_arules(rules1)

plot2=inspectDT(rules1)



rules <- apriori (order_matrix, parameter = list(supp = 0.001, conf = 0.5))

subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

trans1 <- data.frame(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)),
  quality(rules)
)

trans <- data.frame(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)),
  quality(rules)
)

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











rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf)) # show the support, lift and confidence for all rules

rules1 <- apriori (data=order_matrix, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="DISC BRAKE PAD"), control = list (verbose=F)) # get rules that lead to buying 'whole milk'
rules <- apriori (data=order_matrix, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs=c()), control = list (verbose=F)) # get rules that lead to buying 'whole milk'


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


rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))






frequentItems <- eclat (order_matrix, parameter = list(supp = 0.07, maxlen = 15))
itemFrequencyPlot(order_matrix, topN=10, type="absolute", main="Item Frequency")
most_frequent <- barplot(sort(itemFrequency(order_matrix), decreasing=FALSE))
#order_matrix <- as(as.matrix(nmc_code1_tg_vf[,!(names(nmc_code1_tg_vf) %in% c("MUFFLER"))]),"transactions")
jaccard_matrix <- as.matrix(as.data.frame(affinity(order_matrix)))
jaccgraph <- igraph::graph_from_adjacency_matrix(jaccard_matrix, weighted = T,mode = "undirected")



###############################
temp.g <- jaccgraph
temp.g <- igraph::delete.edges(temp.g, E(temp.g)[weight < 0.2])
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



###################################

#nmc_code1_tg_vf[,!(names(nmc_code1_tg_vf) %in% c("SCREW,BOLT,CLIP","OIL FILTER ELE."))]
#jaccard_matrix <- as.matrix(as.data.frame(affinity(order_matrix)))

rules <- apriori(order_matrix, parameter = list(supp = 0.01, conf = 0.7, target = "rules",maxlen=2))
summary(rules)

inspect(head(rules, by = "lift",8))

trans <- data.frame(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)),
  quality(rules)
)

# eliminate brackets
trans$lhs <- gsub("[{}]", "", as.character(trans$lhs))
trans$rhs <- gsub("[{}]", "", as.character(trans$rhs))

# add rule character field
trans$rule <- paste0(trans$lhs," => ", trans$rhs)

# trans <- data.table(trans)

#sort table
trans <- trans[order(-trans$lift),]

# only lift > 1
trans2 <- trans[trans$lift >= 1, ]
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


# create jaccard_matrix
jaccard_matrix <- as.matrix(as.data.frame(affinity(order_matrix)))
# create graph network
jaccgraph <- igraph::graph_from_adjacency_matrix(jaccard_matrix, weighted = T,mode = "undirected")

# create graphic of cross-sell potential
d1 <- igraph::graph.strength(jaccgraph)

# In the branch of mathematics called graph theory, the strength of an undirected graph 
# corresponds to the minimum ratio edges removed/components created in a decomposition 
# of the graph in question. It is a method to compute partitions of the set of vertices and 
# detect zones of high concentration of edges

d1 <- sort(d1, decreasing = T)

d_dt <- data.table(cat = names(d1), stren = d1)

Encoding(d_dt$cat) <- "latin1"

s <- ggplot(d_dt[1:20], aes(x = factor(cat, levels = rev(d_dt$cat[1:20])), y = stren)) +
  geom_bar(stat = "identity", alpha = 0.7, fill = "#54BCD4") +
  coord_flip() +
  xlab("Field") + ylab("Cross-sell potential") + 
  theme_minimal(base_family = "Metric Bold", base_size = 20) 

