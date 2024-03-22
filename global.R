library(shiny)
library(shinydashboard)
library(Seurat)
library(scCustomize)
library(htmlwidgets)
library(ggplot2)
library(DT)
library(dplyr)

#setwd("C:/Users/Evan/Dropbox/single_cell/pvt-shiny/MaiyaLab-PVT-snRNAseq")
sc <- readRDS("data/maiya_pvt_snrnaseq_diet.rds")
html1 <- "data/intro.html"
known_markers <- read.csv("data/markers_known_subtypes.csv")
novel_markers <- read.csv("data/markers_novel_subtypes.csv")

three.colors <- c("#F8730C", "#4DC5C6", "#1B62C5")
five.colors <- c("#3087D5", "#F45B69", "#43A94C", "#F8730C", "#9E69A2")
ten.colors <- c("#F83812","#EEA115","#B1CC2D","#43A94C","#4DC5C6","#3087D5","#4C478D","#9E69A2","#E183AF","#F45B69")
four.colors <- c("#E4E1E3FF", "#F6222EFF","#66B0FFFF","#782AB6FF")

