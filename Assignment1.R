# sources
# https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html
# https://www.data-to-viz.com/graph/chord.html
# http://stat.data.abs.gov.au/Index.aspx?DataSetCode=ABS_DEM_QIM

# Gu, Z. (2014) circlize implements and enhances circular visualization in R. Bioinformatics. DOI: 10.1093/bioinformatics/btu393

library(tidyverse)
library(viridis)
library(circlize)
library(magrittr)
library(chorddiag)

# Load dataset from github
d <- read.table(file.choose(), header=TRUE)

# convert data from a table into matrix
data.matrix(d)

# short names
colnames(d) <- c("NSW", "VIC", "QLD","SA","WA","TAS","NT","ACT")
rownames(d) <- colnames(d)

# turn data into adjacency list
data_long <- d %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)

pdf("data_in_pdf.pdf", height = 11.69, width =  8.27)
# parameters

par(mar=c(0,0,0,0)+0.1) 
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)


grid.col = c("NSW" = "#0c2c84","VIC" = "#225ea8","SA" = "#1d91c0","TAS" = "#41b6c4",
            "WA" = "#ffe4af","NT" = "#FFC0C1","ACT" = "#FC8084","QLD" = "#93003a")


# Base plot
chordDiagram(
  data_long, 
  order= c("NSW","VIC","SA","TAS",
           "WA","NT","ACT","QLD"),
  grid.col = grid.col,
  transparency = 0.25,
  directional = 1, 
  direction.type = c("diffHeight", "arrows"),
  diffHeight = -0.04,
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE,
  annotationTrack = c("grid", "axis"), 
  preAllocateTracks = list(track.height = min(strwidth(unlist(dimnames(data_long)))))
  )


circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(-1, 0))
}, bg.border = NA)

# adding on a title
par(mar=c(0,0,4,0))

title("Interstate Migration in Australia")
#title("Interstate Migration in Australia", sub = "sub title",
#      cex.main = 2,   font.main= 3, col.main= "Black",
#      cex.sub = 0.75, font.sub = 3, col.sub = "red")
  
# adding a legend
legend("bottomleft", inset = .02, bg="transparent", legend = c("New South Wales", "Victoria (VIC)", "South Australia (SA)", "Tasmania (TAS)", 
"Western Australia (WA)","Northern Territory (NT)","Australian Capital Territory (ACT)", "Queensland (QLD)"),
       fill = c("#0c2c84","#225ea8","#1d91c0","#41b6c4","#ffe4af","#FFC0C1","#FC8084","#93003a"), cex=0.7, box.lty=0)


dev.off()


