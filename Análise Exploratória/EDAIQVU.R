# CARREGAMENTO DOS PACOTES (REFERENCIAR PROFS) ----

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

# IMPORTAÇÃO DOS DADOS E VISUALIZAÇÃO DE ALGUMAS MÉTRICAS ----

IQVU2016 <- readxl::read_xlsx("dados2016.xlsx")

# Criando um novo dataframe apenas com os índices já consolidados, para facilitar
# as análises. Posteriormente, alterando os nomes das colunas para entendimento.

IQVUINDICES <- IQVU2016[, c(1:4, 42:51)]
IQVUINDICES <- IQVUINDICES[,-1]
IQVUINDICES <- IQVUINDICES[,-1]
glimpse(IQVUINDICES)
names(IQVUINDICES)[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)] <- c("Abastecimento", "Cultura", "Educacao", "Esportes", "Habitacao", "Infraestrutura Urbana", "Meio Ambiente", "Saude", "Servicos Urbanos", "Seguranca Urbana")

IQVUINDICES <- scale(IQVUINDICES[, 3:12])
IQVUINDICES <- as.data.frame(IQVUINDICES)

# Uma análise rápida dos dados presentes na tabela.

plot_intro(IQVUINDICES, ggtheme = theme_bw())


# Visualização do número de bairros por regional

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


stargazer(IQVUINDICES[,3:12],p.auto = TRUE, type="text",title="Estatísticas descritivas", out="table1.txt")


# Plotando o histograma das variáveis mais importantes (IQVU's consolidados)

plot_histogram(IQVUINDICES, nrow = 3, ncol = 4, ggtheme = theme_bw(),)

plot_boxplot(IQVUINDICES, by = IQVUINDICES$IQVU_1_Abastecimento)

boxplot(IQVUINDICES[, 3:12], nrow = 3, ncol = 4)

boxplot(IQVUINDICES[,5])

ggplot(IQVUINDICES, aes(y=IQVUINDICES$IQVU_10_Seguranca_urbana))+ 
  geom_boxplot(varwidth=T, fill="lightblue")  +
  labs(x="",
       y="Score", 
       title="Distribution of German Universities Score")+  
  theme_bw()+
  theme(plot.title = element_text(size=22)
        ,axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title=element_text(size=18))

# Em termos de abastecimento, aparentemente, a maioria dos bairros de Belo
# Horizonte está bem atendido, bem como na questão de educação e esportes. Já os
# níveis de segurança urbana chamam a atenção por se concentrarem perto do zero.
# A distribuição dos indicadores de infraestrutura urbana também podem indicar
# uma desigualdade entre os bairros da cidade.

# Abaixo, plotamos a relação entre o IQVU registrado e o IQVU específico da Segu-
# rança urbana. Aparentemente, não há relação entre os dois indicadores.

ggplot(IQVUINDICES, aes(x=IQVUINDICES$Educacao, y=IQVUINDICES$`Seguranca Urbana`)) + 
  geom_point() + 
  labs(y="Segurança Urbana", 
       x="Educação", 
       title="Infraestrutura vs. Segurança Urbana")+
  theme_dark()+
  theme(plot.title = element_text(size=22),axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15), axis.title=element_text(size=18))

# Visualmente, não é possível inferir muita coisa senão os baixos índices de segurança
# urbana dos bairros de Belo Horizonte. Apesar disso, a maioria dos pontos está
# com níveis em educação acima dos 0.75.

ggplot(IQVUINDICES, aes(x=IQVUINDICES$`Seguranca Urbana`, y=Saude)) + 
  geom_point() + 
  labs(y="Saúde", 
       x="Segurança", 
       title="Saúde vs. Segurança")+
  theme_dark()+
  theme(plot.title = element_text(size=22),axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15), axis.title=element_text(size=18))

# Uma das importantes análises é a de correlação. O plot abaixo mostra as relações
# entre as variáveis do dataset de IQVU's.

chart.Correlation(IQVUINDICES[,3:12], method = "pearson", histogram = TRUE, pch = "+")

# Visualização do número de bairros por regional

freq_table <- table(IQVU2016$REGIONAL)
freq_df <- data.frame(REGIONAL = names(freq_table), Freq = as.vector(freq_table))
ggplot(freq_df, aes(x = REGIONAL, y = Freq, fill = REGIONAL)) +
  geom_bar(stat = "identity") +
  ggtitle("Frequência por Regional") +
  xlab("Regional") +
  ylab("Frequência") +
  theme_dark()

# Gerando as médias dos índices para visualização

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

medias_centrosul <- mediasformatolong %>%
  filter(REGIONAL == "CENTRO-SUL")

ggplot(medias_centrosul, aes(x = media, y = REGIONAL, fill = variavel)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Média Colunas 42 a 51 na Região Sul") +
  xlab("Média") +
  ylab("Regional") +
  scale_fill_manual(values = esquema_cores) +
  theme_minimal()

medias_vendanova <- mediasformatolong %>%
  filter(REGIONAL == "VENDA NOVA")

ggplot(medias_vendanova, aes(x = media, y = REGIONAL, fill = variavel)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Média Colunas 42 a 51 na Região Sul") +
  xlab("Média") +
  ylab("Regional") +
  scale_fill_manual(values = esquema_cores) +
  theme_minimal()
