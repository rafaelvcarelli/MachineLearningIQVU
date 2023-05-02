## O script de clusteriação hierárquica dos dados está aqui descrito de forma textual. O script na íntegra é o arquivo [ClustersHierarquico.R](https://github.com/rafaelvcarelli/MachineLearningIQVU/blob/main/Cluster%20hier%C3%A1rquicos/ClustersHierarquico.R).

___

### CARREGAMENTO DOS PACOTES

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

___

### IMPORTAÇÃO DOS DADOS
##### Com os dados brutos em mãos, o primeiro passo é manipular a base de dados para que as colunas da base sejam apenas aquelas que são pertinentes a esse projeto. Portanto, deixamos apenas os 10 indicadores IQVU por bairro.


    IQVU2016 <- readxl::read_xlsx("dados2016.xlsx")
    IQVU20062016 <- readxl::read_xlsx("dados20062016.xlsx")
    
    IQVUINDICES <- IQVU2016[, c(1:3, 40:51)]
    IQVUINDICES <- IQVUINDICES[,-4]
    IQVUINDICES <- IQVUINDICES[,-4]
    IQVUINDICES <- IQVUINDICES[,-1]
    IQVUINDICES <- IQVUINDICES[,-1]

___

### CLUSTERS
#### A segunda manipulação é criar uma base de dados secundária para criar os clusters. A técnica é transformar a primeira coluna no nome dos bairros de Belo Horizonte, no caso desta database.

    IQVUINDICESCLUS <- IQVUINDICES
    IQVUINDICESCLUS <- as.data.frame(IQVUINDICESCLUS)
    rownames(IQVUINDICESCLUS) <- IQVUINDICESCLUS[, 1]
    IQVUINDICESCLUS <- IQVUINDICESCLUS[, -1]
    IQVUINDICESCLUS

___

### TESTE CLUSTERS HIERÁRQUICOS

    IQVUINDICESCLUSpad <- scale(IQVUINDICESCLUS)
    IQVUINDICESCLUSpad <- as.data.frame(IQVUINDICESCLUSpad)
    
    distmatrixIQVU <- dist(IQVUINDICESCLUSpad, method = "euclidean")
    distmatrixIQVU
    
    hc1 <- hclust(distmatrixIQVU, method = "single" )
    hc2 <- hclust(distmatrixIQVU, method = "complete" )
    hc3 <- hclust(distmatrixIQVU, method = "average" )
    
    plot(hc1, cex = 0.6, hang = -1)


![https://i.ibb.co/SJ5520c/hclustsimples.jpg](https://i.ibb.co/SJ5520c/hclustsimples.jpg)

    plot(hc2, cex = 0.6, hang = -1)

![https://i.ibb.co/N3th3n1/hclustcomplete.jpg](https://i.ibb.co/N3th3n1/hclustcomplete.jpg)

    plot(hc3, cex = 0.6, hang = -1)

![https://i.ibb.co/wJ9cNd5/hclustaverage.jpg](https://i.ibb.co/wJ9cNd5/hclustaverage.jpg)
    
    dend1 <- as.dendrogram(hc1)
    dend2 <- as.dendrogram(hc2)
    dend3 <- as.dendrogram(hc3)
    dend4 <- as.dendrogram(hc4)
    
    clusthierq <- hclust(distmatrixIQVU, method = "average")
    
    fviz_nbclust(IQVUINDICESCLUS, FUN = hcut, method = "wss")

![https://i.ibb.co/NS8BtSC/elbowhclust.jpg](https://i.ibb.co/NS8BtSC/elbowhclust.jpg)

___

### - [Cluster não - hierárquico](https://github.com/rafaelvcarelli/MachineLearningIQVU/tree/main/Cluster%20n%C3%A3o-hierarquico)