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
library(arulesViz)

 # increase aes quality of graphs
 options(shiny.usecairo=T)
 # show reactlog with ctrl+F3
 options(shiny.reactlog = T)

 rm(list=ls());gc


 if (!interactive()) {
   options(shiny.error = function() {
     stop("An error has occurred")
   })
 }

