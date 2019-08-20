# Name: Cheng Chua
# Subject: GEOM90007 Spatial Visualisation
# Assignment 1: R project

# About this plot:
# This is a chord diagram plot, visualising the
# interstate migration in Australia.

# Dataset sourced from: Australian Bureau of Statistics (2018)
# http://stat.data.abs.gov.au/Index.aspx?DataSetCode=ABS_DEM_QIM
# Data was originally downloaded as an Excel table file. 
# Using Excel, I exported the file as a csv, which is the 
# "AUStateMigration.csv" file to be used in this script.


# Chord Diagram tutorial and sources:
# Gu, Z. (2014) circlize implements and enhances circular visualization
# in R. Bioinformatics. DOI: 10.1093/bioinformatics/btu393

# Circular Visualisation in R by Zuguang Gu 
# https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html

# Chord Diagram - From Data to Viz: (https://www.data-to-viz.com/graph/chord.html)



# Make sure that the following libraries are installed prior to running

library(tidyverse)
library(viridis)
library(circlize) 
library(magrittr)
library(chorddiag)

# Load the dataset "AUStateMigration.csv" from zip folder
d <- read.table(file.choose(), header=TRUE)

# Convert data from a table into matrix
data.matrix(d)

# Determine the row and column names of the matrix
# Abbreviated names of states
colnames(d) <- c("NSW", "VIC", "QLD","SA","WA","TAS","NT","ACT")
rownames(d) <- colnames(d)

# Turn matrix data into adjacency list, 
# which is the format we will use to create a chord diagram!
data_long <- d %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)


# Creating a pdf file, to roughly an A4 portrait size,
# with minor size modifications for ease of reading
# Comment this portion out if plot does not appear in RStudio
pdf("ChengChua_764526_SpatialVisualisation_Assignment1R.pdf", 
    height = 11, 
    width =  8.27)

# Setting the parameters for the Chord Diagram
par(mar=c(0,0,0,0)+0.1) 

circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)



# Deisgn rationale for colour palette:
# Colours were considered carefully according to the guidelines taught
# in the lecture. Specifically, I was trying to balance between a colour
# palette that has sufficient contrast (Simultaneous Contrast problem)
# and colours allow colour blind viewers to read the plot easily.

# Colours were picked from from ColorBox by Lyft Design (https://www.colorbox.io/)

# The plot was tested for colour-blindness (Trichromatic view, Anomalous Trichromacy,
# Dichromatic View, and Monochromatic view)
# using Coblis - Colour Blindness Simulator
# https://www.color-blindness.com/coblis-color-blindness-simulator/ 

grid.col = c("NSW" = "#0c2c84","VIC" = "#225ea8","SA" = "#177da6","TAS" = "#41b6c4",
            "WA" = "#ffe4af","NT" = "#FFC0C1","ACT" = "#FC8084","QLD" = "#93003a")


# This code plots the Chord Diagram
chordDiagram(
  data_long, 
  order= c("NSW","VIC","SA","TAS",
           "WA","NT","ACT","QLD"),
  grid.col = grid.col,
  transparency = 0.25,
  directional = 1, 
  direction.type = c("diffHeight", "arrows"),
  diffHeight = -0.04,
  
  # Arrows help indicate directionality better.
  # We also want to sort the arrows so that the 
  # biggest arrows are displayed first. This helps us to see 
  # significant migrations more easily
  
  link.arr.type = "big.arrow",  
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  annotationTrack = c("grid", "axis"), 
  preAllocateTracks = list(track.height = min(strwidth(unlist(dimnames(data_long)))))
  )



# Rotate labels so they point outward, for ease of reading
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(-1, 0))
}, bg.border = NA)


# Adding a title to the plot
par(mar=c(0,0,4,0))
title("Interstate Migration in Australia", cex.main=1.5)

  
# Adding a legend to indicate which colours represent which states
# This also allows us to explain the state abbreviations as well.

legend("bottomleft", 
       inset = .02, 
       bg="transparent", 
       legend = c("New South Wales", "Victoria (VIC)", "South Australia (SA)", "Tasmania (TAS)", 
        "Western Australia (WA)","Northern Territory (NT)","Australian Capital Territory (ACT)", "Queensland (QLD)"),
       fill = c("#0c2c84","#225ea8","#177da6","#41b6c4","#ffe4af","#FFC0C1","#FC8084","#93003a"), 
       cex=0.7, 
       box.lty=0)


# A second legend to state the data source: 
# Australian Bureau of Statistics (December 2018)

legend("bottomright", 
       inset = .02, 
       bg="transparent", 
       legend = c("Source: Australian Bureau of Statistics (2018)"), 
       cex=0.8, 
       box.lty=0)


dev.off()

# End of program. Thank you! :)

