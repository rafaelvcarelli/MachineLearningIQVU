## O script de análise exploratória dos dados está aqui descrito de forma textual. O script na íntegra é o arquivo [EDAIQVU.R](https://github.com/rafaelvcarelli/MachineLearningIQVU/blob/main/An%C3%A1lise%20Explorat%C3%B3ria/EDAIQVU.R).


## CARREGAMENTO DOS PACOTES (função criada pelo professor Rafael de Souza) ----

    pacotes <- c("plotly","tidyverse","knitr","kableExtra","car","rgl","gridExtra",
     "PerformanceAnalytics","reshape2","rayshader","psych","ggrepel",
     "factoextra","sp","tmap","magick", "ggcorrplot", "plotly", "DataExplorer",
     "sjPlot", "rela", "MASS", "parallel", "stargazer")

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

## IMPORTAÇÃO DOS DADOS E VISUALIZAÇÃO DE ALGUMAS MÉTRICAS

	IQVU2016 <- readxl::read_xlsx("dados2016.xlsx")

## Criando um novo dataframe apenas com os índices já consolidados, para facilitar as análises. Posteriormente, alterando os nomes das colunas para entendimento.

	IQVUINDICES <- IQVU2016[, c(1:4, 42:51)]
	IQVUINDICES <- IQVUINDICES[,-1]
	IQVUINDICES <- IQVUINDICES[,-1]
	glimpse(IQVUINDICES)
	names(IQVUINDICES)[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)] <- c("Abastecimento", "Cultura", "Educacao", "Esportes", "Habitacao", "Infraestrutura Urbana", "Meio Ambiente", "Saude", "Servicos Urbanos", "Seguranca Urbana")
	
	IQVUINDICES <- scale(IQVUINDICES[, 3:12])
	IQVUINDICES <- as.data.frame(IQVUINDICES)

### Uma análise rápida dos dados presentes na tabela.

	plot_intro(IQVUINDICES, ggtheme = theme_bw())

![](https://i.ibb.co/dk5yb6w/plotintro-IQVU.jpg)

### Visualização do número de bairros por regional

	tabeladefrequenciasregionais <- table(IQVUINDICES$REGIONAL)
	tabeladefrequenciasregionais <- as.data.frame(tabeladefrequenciasregionais)
	
	ggplot(tabeladefrequenciasregionais, aes(x = Var1, y = Freq)) + 
	  geom_bar(stat = "identity", fill = "#1f78b4") +
	  geom_text(aes(label = Freq), vjust = -0.5) +
	  labs(title = "Número de bairros por regional",
	       x = "Regional",
	       y = "Número absoluto") +
	  theme(plot.title = element_text(size = 16),
	        axis.title = element_text(size = 14),
	        axis.text = element_text(size = 12))   
	
![](https://i.ibb.co/f8hYBFQ/bairrospregional.jpg)

### Plotando o histograma das variáveis mais importantes (IQVU's consolidados)

	plot_histogram(IQVUINDICES, nrow = 3, ncol = 4, ggtheme = theme_bw(),)

![](https://i.ibb.co/H7YtRf6/histiqvu.jpg)

### Plotagem de boxplot das variáveis

	boxplot(IQVUINDICES[, 3:12], nrow = 3, ncol = 4)

![](https://i.ibb.co/m8SnMj9/boxplotiqvu.jpg)
	
### Boxplot da variável educação

	boxplot(IQVUINDICES[,5])

![](https://i.ibb.co/9Z1nSvm/boxploteducacao.jpg)

### Em termos de abastecimento, aparentemente, a maioria dos bairros de Belo
### Horizonte está bem atendido, bem como na questão de educação e esportes. Já os níveis de segurança urbana chamam a atenção por se concentrarem perto do zero. A distribuição dos indicadores de infraestrutura urbana também podem indicar uma desigualdade entre os bairros da cidade.

### Abaixo, plotamos a relação entre o IQVU registrado e o IQVU específico da Segurança urbana. Aparentemente, não há relação entre os dois indicadores.

	ggplot(IQVUINDICES, aes(x=IQVUINDICES$Educacao, y=IQVUINDICES$`Seguranca Urbana`)) + 
	  geom_point() + 
	  labs(y="Segurança Urbana", 
	       x="Educação", 
	       title="Infraestrutura vs. Segurança Urbana")+
	  theme_dark()+
	  theme(plot.title = element_text(size=22),axis.text.x= element_text(size=15),
	        axis.text.y= element_text(size=15), axis.title=element_text(size=18))

![](https://i.ibb.co/QMwgDM1/educseguriqvu.jpg)

### Uma das importantes análises é a de correlação. O plot abaixo mostra as relações entre as variáveis do dataset de IQVU's.

	chart.Correlation(IQVUINDICES[,3:12], method = "pearson", histogram = TRUE, pch = "+")

![](https://i.ibb.co/GvvWR41/corrtable.jpg)

### Visualização do número de bairros por regional

	freq_table <- table(IQVU2016$REGIONAL)
	freq_df <- data.frame(REGIONAL = names(freq_table), Freq = as.vector(freq_table))
	ggplot(freq_df, aes(x = REGIONAL, y = Freq, fill = REGIONAL)) +
	  geom_bar(stat = "identity") +
	  ggtitle("Frequência por Regional") +
	  xlab("Regional") +
	  ylab("Frequência") +
	  theme_dark()

![](https://i.ibb.co/z82D3K5/freqreg.jpg)

### Gerando as médias dos índices para visualização

	mediasindicesiqvu <- IQVU2016 %>%
	  group_by(REGIONAL) %>%
	  summarise(across(41:50, mean))
	
	mediasformatolong <- mediasindicesiqvu %>%
	  pivot_longer(cols = -REGIONAL, names_to = "variavel", values_to = "media")
	
	esquema_cores <- brewer.pal(11, "Spectral")
	
	ggplot(mediasformatolong, aes(x = media, y = REGIONAL, fill = variavel)) +
	  geom_bar(stat = "identity", position = "dodge") +
	  ggtitle("Média dos indicadores por Regional") +
	  xlab("Média") +
	  ylab("Regional") +
	  scale_fill_manual(values = esquema_cores) +
	  theme_minimal()
	
![](https://i.ibb.co/1QtBmQM/mediaregional.jpg)

	medias_centrosul <- mediasformatolong %>%
	  filter(REGIONAL == "CENTRO-SUL")
	
	ggplot(medias_centrosul, aes(x = media, y = REGIONAL, fill = variavel)) +
	  geom_bar(stat = "identity", position = "dodge") +
	  ggtitle("Média Colunas 42 a 51 na Região Sul") +
	  xlab("Média") +
	  ylab("Regional") +
	  scale_fill_manual(values = esquema_cores) +
	  theme_minimal()

![](https://i.ibb.co/R2p3b6G/mediacs.jpg)

	medias_vendanova <- mediasformatolong %>%
	  filter(REGIONAL == "VENDA NOVA")
	
	ggplot(medias_vendanova, aes(x = media, y = REGIONAL, fill = variavel)) +
	  geom_bar(stat = "identity", position = "dodge") +
	  ggtitle("Média Colunas 42 a 51 na Região Sul") +
	  xlab("Média") +
	  ylab("Regional") +
	  scale_fill_manual(values = esquema_cores) +
	  theme_minimal()

![](https://i.ibb.co/xjVqYwK/mediasvendan.jpg)

___

### - [Cluster hierárquicos](https://github.com/rafaelvcarelli/MachineLearningIQVU/tree/main/Cluster%20hier%C3%A1rquicos)