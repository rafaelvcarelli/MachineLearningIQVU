# CARREGAMENTO DOS PACOTES

pacotes <- c("plotly","tidyverse","knitr","kableExtra","car","rgl","gridExtra",
             "PerformanceAnalytics","reshape2","rayshader","psych","ggrepel",
             "factoextra","sp","tmap","magick", "ggcorrplot", "plotly", "DataExplorer",
             "sjPlot", "psych", "rela", "MASS", "parallel")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

options(scipen = 5)

# IMPORTAÇÃO DOS DADOS ----

IQVU2016 <- readxl::read_xlsx("dados2016.xlsx")
IQVU20062016 <- readxl::read_xlsx("dados20062016.xlsx")

IQVUINDICES <- IQVU2016[, c(1:3, 40:51)]
IQVUINDICES <- IQVUINDICES[,-4]
IQVUINDICES <- IQVUINDICES[,-4]
IQVUINDICES <- IQVUINDICES[,-1]
IQVUINDICES <- IQVUINDICES[,-1]

###############CLUSTERS######################

IQVUINDICESCLUS <- IQVUINDICES
IQVUINDICESCLUS <- as.data.frame(IQVUINDICESCLUS)
rownames(IQVUINDICESCLUS) <- IQVUINDICESCLUS[, 1]
IQVUINDICESCLUS <- IQVUINDICESCLUS[, -1]
IQVUINDICESCLUS

### TESTE CLUSTERS HIERÁRQUICOS

IQVUINDICESCLUSpad <- scale(IQVUINDICESCLUS)
IQVUINDICESCLUSpad <- as.data.frame(IQVUINDICESCLUSpad)

distmatrixIQVU <- dist(IQVUINDICESCLUSpad, method = "euclidean")
distmatrixIQVU

hc1 <- hclust(distmatrixIQVU, method = "single" )
hc2 <- hclust(distmatrixIQVU, method = "complete" )
hc3 <- hclust(distmatrixIQVU, method = "average" )

plot(hc1, cex = 0.6, hang = -1)
plot(hc2, cex = 0.6, hang = -1)
plot(hc3, cex = 0.6, hang = -1)

dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
dend3 <- as.dendrogram(hc3)

clusthierq <- hclust(distmatrixIQVU, method = "average")

fviz_nbclust(IQVUINDICESCLUS, FUN = hcut, method = "wss")

