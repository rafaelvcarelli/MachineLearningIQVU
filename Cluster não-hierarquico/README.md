## O script de clusteriação não-hierárquica dos dados está aqui descrito de forma textual. O script na íntegra é o arquivo [ClustersNaoHierarquico.R](https://github.com/rafaelvcarelli/MachineLearningIQVU/blob/main/Cluster%20n%C3%A3o-hierarquico/ClustersNaoHierarquico.R).
___

### CARREGAMENTO DOS PACOTES (função idealizada por Rafael de Souza)

    pacotes <- c("plotly","tidyverse","knitr","kableExtra","car","rgl","gridExtra",
     "PerformanceAnalytics","reshape2","rayshader","psych","ggrepel",
     "factoextra","sp","tmap","magick", "ggcorrplot", "plotly", "DataExplorer",
     "sjPlot", "psych", "rela", "MASS", "parallel", "RColorBrewer")
    
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

### MANIPULAÇÃO DOS DADOS
#### Criação de um segundo objeto, IQVUINDICESCLUS, para criação dos clusters não-hierárquicos. Primeiro, apenas a tabela dos dados originais sem as regionais. 

####Depois, a transformação da primeira coluna da base de dados nova em uma coluna com os nomes dos bairros.
    
    IQVU2016 <- readxl::read_xlsx("dados2016.xlsx")
    
    IQVUINDICES <- IQVU2016[, c(1:4, 42:51)]
    IQVUINDICES <- IQVUINDICES[,-1]
    IQVUINDICES <- IQVUINDICES[,-1]
    
    IQVUINDICESSEMREGIONAL <- IQVUINDICES[,-2]
    
    IQVUINDICESCLUS <- IQVUINDICESSEMREGIONAL
    IQVUINDICESCLUS <- as.data.frame(IQVUINDICESCLUS)
    rownames(IQVUINDICESCLUS) <- IQVUINDICESCLUS[, 1]
    IQVUINDICESCLUS <- IQVUINDICESCLUS[, -1]
    IQVUINDICESCLUS
    
    IQVUINDICESCLUS <- scale(IQVUINDICESCLUS)
    IQVUINDICESCLUS <- as.data.frame(IQVUINDICESCLUS)


## MÉTODO KMEANS
#### Pelo método kmeans, criamos um cluster com dois centróides agrupandos os bairros de Belo Horizonte.

    K2IQVU <- kmeans(IQVUINDICESCLUS, centers = 2)
    fviz_cluster(K2IQVU, data = IQVUINDICESCLUS, main = "Clusters com dois centróides")
    
![](https://i.ibb.co/DRQ9HVG/cluster2.jpg)

#### Criamos também clusters com 3, 4 e 5 centróides.

    K3IQVU <- kmeans(IQVUINDICESCLUS, centers = 3)
    K4IQVU <- kmeans(IQVUINDICESCLUS, centers = 4)
    K5IQVU <- kmeans(IQVUINDICESCLUS, centers = 5)
    
    fviz_cluster(K3IQVU, data = IQVUINDICESCLUS, main = "Clusters com três centróides")

![](https://i.ibb.co/dQxykww/3cnhc.jpg)

    fviz_cluster(K4IQVU, data = IQVUINDICESCLUS, main = "Clusters com quatro centróides")

![](https://i.ibb.co/F0mpvcQ/cluster4.jpg)

    fviz_cluster(K5IQVU, data = IQVUINDICESCLUS, main = "Clusters com cinco centróides")
    
![](https://i.ibb.co/J5j8Y4g/cluster5.jpg)

    agrupamento1 <- fviz_cluster(K2IQVU, geom = "point", data = IQVUINDICESCLUS) + ggtitle("k = 2")
    agrupamento2 <- fviz_cluster(K3IQVU, geom = "point",  data = IQVUINDICESCLUS) + ggtitle("k = 3")
    agrupamento3 <- fviz_cluster(K4IQVU, geom = "point",  data = IQVUINDICESCLUS) + ggtitle("k = 4")
    agrupamento4 <- fviz_cluster(K5IQVU, geom = "point",  data = IQVUINDICESCLUS) + ggtitle("k = 5")

#### Os quatro clusters agrupados com 2, 3, 4 e 5 centróides são visualizados desta forma.

    grid.arrange(agrupamento1, agrupamento2, agrupamento3, agrupamento4, nrow = 2)

![](https://i.ibb.co/Mf8kdW9/cluster2345.jpg)

## VALIDAÇÃO DO NÚMERO DE CLUSTERS
#### Os métodos Elbow, Silhouette e Gap definem, por meio de estatística, o número ideal de clusters para um determinado grupo de dados.

    fviz_nbclust(IQVUINDICESCLUS, kmeans, method = "wss")
    
![](https://i.ibb.co/CWyr67F/nhcelbow.jpg)

    fviz_nbclust(IQVUINDICESCLUS, kmeans, method = "silhouette")
    
![](https://i.ibb.co/CvQWkkq/nhcsilhouette.jpg)

    fviz_nbclust(IQVUINDICESCLUS, kmeans, method = "gap_stat")
    
![](https://i.ibb.co/mDqvtXT/nhcgap.jpg)
    
    bairros3clusters <- data.frame(K3IQVU$cluster)
    bairros4clusters <- data.frame(K4IQVU$cluster)
    bairros5clusters <- data.frame(K5IQVU$cluster)

### Com as visualizações do método Elbow e Gap, podemos inferir que o agrupamento em três clusters é o ideal para o conjunto de dados do IQVU. Portanto, os bairros estarão agrupados em três subgrupos.

### Unindo o número dos clusters encontrados a tabela original.

    IQVU3CLUSTERS <-  cbind(IQVUINDICES, bairros3clusters)
    IQVU4CLUSTERS <-  cbind(IQVUINDICES, bairros4clusters)
    IQVU5CLUSTERS <-  cbind(IQVUINDICES, bairros5clusters)
    
    table(K4IQVU$cluster)
    
    fviz_cluster(K3IQVU, data = IQVUINDICESCLUS, main = "Clusters com três centróides")

![](https://i.ibb.co/dQxykww/3cnhc.jpg)

    k3cluster1_filtrado <- IQVU3CLUSTERS[IQVU3CLUSTERS[,13] == 1, ]
    k3cluster2_filtrado <- IQVU3CLUSTERS[IQVU3CLUSTERS[,13] == 2, ]
    k3cluster3_filtrado <- IQVU3CLUSTERS[IQVU3CLUSTERS[,13] == 3, ]
    
    summarycluster1 <- as.data.frame(summary(k3cluster1_filtrado))
    summarycluster1
    
    summarycluster2 <- as.data.frame(summary(k3cluster2_filtrado))
    summarycluster2
    
    summarycluster3 <- as.data.frame(summary(k3cluster3_filtrado[,3:12]))
    summarycluster3
    
    bairrosregionalcluster1 <- k3cluster1_filtrado[, 1:2]
    bairrosregionalcluster2 <- k3cluster2_filtrado[, 1:2]
    bairrosregionalcluster3 <- k3cluster3_filtrado[, 1:2]
##VISUALIZAÇÃO
#### Abaixo, uma visualização dos clusters pelo número de bairros em cada regional dos agrupamentos encontrados.

    ggplot(bairrosregionalcluster1, aes(REGIONAL)) +
      geom_bar(fill = "#2E9FDF") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +
      labs(x = "Regional", y = "Número absoluto", title = "Bairros por Regional - Cluster 1") +
      theme_minimal()
   
![](https://i.ibb.co/3FxrHjj/cluster1regionais.jpg)

    ggplot(bairrosregionalcluster2, aes(REGIONAL)) +
      geom_bar(fill = "#2E9FDF") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +
      labs(x = "Regional", y = "Número absoluto", title = "Bairros por Regional - Cluster 2") +
      theme_minimal()
    
![](https://i.ibb.co/GkW9Hff/cluster2regionais.jpg)

    ggplot(bairrosregionalcluster3, aes(REGIONAL)) +
      geom_bar(fill = "#2E9FDF") +
      geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 4) +
      labs(x = "Regional", y = "Número absoluto", title = "Bairros por Regional - Cluster 3") +
      theme_minimal()
       
![](https://i.ibb.co/TBnwpzC/cluster3regionais.jpg)

    colunasclus1 <- k3cluster1_filtrado[, 3:12]
    colunasclus2 <- k3cluster2_filtrado[, 3:12]
    colunasclus3 <- k3cluster3_filtrado[, 3:12]
    
    mediasclus1 <- colMeans(colunasclus1)
    mediasclus2 <- colMeans(colunasclus2)
    mediasclus3 <- colMeans(colunasclus3)
    
    mediasclus1df <- as.data.frame(mediasclus1)
    mediasclus2df <- as.data.frame(mediasclus2)
    mediasclus3df <- as.data.frame(mediasclus3)

### As visualizações das médias dos clusters encontrados ajudam a entender como os bairros foram agrupados e as semelhanças entre eles.
  
    barplot(mediasclus1, col = esquema_cores, main = "Médias das Colunas do Cluster 1", xlab = "Colunas", ylab = "Média",
    names.arg = names(mediasclus1), ylim = c(0, max(mediasclus1)*1.2))

![](https://i.ibb.co/txDGSTd/mediasclus1.jpg)
    
    barplot(mediasclus2, col = esquema_cores, main = "Médias das Colunas do Cluster 2", xlab = "Colunas", ylab = "Média",
    names.arg = names(mediasclus1), ylim = c(0, max(mediasclus1)*1.2))

![](https://i.ibb.co/8BLGwLm/mediasclus2.jpg)
    
    barplot(mediasclus3, col = esquema_cores, main = "Médias das Colunas do Cluster 3", xlab = "Colunas", ylab = "Média",
    names.arg = names(mediasclus1), ylim = c(0, max(mediasclus1)*1.2))

![](https://i.ibb.co/fdjS5Dg/mediasclus3.jpg)
    
    medias1 <- barplot(mediasclus1, col = esquema_cores, main = "Médias das Colunas do Cluster 1", xlab = "Colunas", ylab = "Média")
    medias2 <- barplot(mediasclus2, col = esquema_cores, main = "Médias das Colunas do Cluster 2", xlab = "Colunas", ylab = "Média")
    medias3 <- barplot(mediasclus3, col = esquema_cores, main = "Médias das Colunas do Cluster 3", xlab = "Colunas", ylab = "Média")

### Inserindo os IQVU's consolidados para cada bairro dos clusters encontrados

    k3cluster1_filtrado_com_IQVU <- merge(k3cluster1_filtrado, IQVU2016[, c("NOMEUP", "IQVU")], by = "NOMEUP")
    
    k3cluster2_filtrado_com_IQVU <- merge(k3cluster2_filtrado, IQVU2016[, c("NOMEUP", "IQVU")], by = "NOMEUP")
  
    k3cluster3_filtrado_com_IQVU <- merge(k3cluster3_filtrado, IQVU2016[, c("NOMEUP", "IQVU")], by = "NOMEUP")