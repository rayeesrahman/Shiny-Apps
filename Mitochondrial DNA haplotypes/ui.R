
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(mlbench)
library(plotly)
library(shinythemes)
library(dplyr)
library(heatmaply)
library(shinyHeatmaply)
# Load data

# ui.R definition
ui <- fluidPage(
  # Set theme
  theme = shinytheme("spacelab"),
  
  # Some help text
  h1("Group 3: Sequence Analysis of Human Mitochondrial DNA"),
  h2("Clustering of 1000 Genomes mt-DNA using t-SNE"),
  h3("For this analysis we took presence or absense haplotype data from the 1000 genomes consortium and plotted them into 3 dimensions"),
  h3("This Shiny app allows you to interact with the figures! ", tags$code("Select"), " regions of the 2D plot to see the distribution of populations and to see a heatmap of haplotypes for the cluster!" ),
  h5("May take a few seconds to load"),
  tags$ol(
    tags$li("The first chart showcases a 3D representation of the clusters"),
    tags$li("The second chart showcases two dimentions of figure 1, to investigate a region ", tags$code("select") , " it"),
    tags$li("After selecting a region the frequency of subpopulations are shown as well as a heatmap of what mitochondrial haplotypes define the group")),

  #source("../papg-mt/analysis.R"),
  # Vertical space
  tags$hr(),
  plotlyOutput("Plot4", height = "600px"),
  tags$hr(),
  # First row
  fixedRow(
    column(6, plotlyOutput("Plot1", height = "600px")),
    column(6, plotlyOutput("Plot3", height = "600px")),
    column(6, plotlyOutput("Plot2", height = "1500px")),
    column(6, plotlyOutput("Plot5", height = "900px"))
  )
  
    #column(6, plotlyOutput("Plot3", height = "1500px", width = "1000px")))
  )
  
  #tags$hr(),
  #tags$blockquote("First drag a selection box in the scatter plot to populate the barchart. Then select one of the bars in the barchat                  to populate the boxplot"),
  
  
  # Second row
#  fixedRow(
 #   column(6, plotlyOutput("Plot3", height = "600px")),
  #  column(6, plotlyOutput("Plot4", height = "600px")))
 #)