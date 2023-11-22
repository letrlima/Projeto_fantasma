vendas <- read_csv("banco/vendas.csv")
caminho_Leticia <- "resultados"
devolução_atualizado <- read_csv("C:/Users/letic/Desktop/ESTAT/devolução_atualizado.csv")

library(tidyverse)
library(dplyr)
library(lubridate)

library(scales)

### Modificando o banco de dados

vendas$`Product Name` <- as.factor(vendas$`Product Name`)
vendas$Category <- as.factor(vendas$Category)
vendas$Color <- as.factor(vendas$Color)
vendas$Size <- as.factor(vendas$Size)
levels(vendas$`Product Name`) <- c("Vestido", "Calca Jeans", "Sapatos", "Casacos", "Camisetas")
levels(vendas$Category) <- c("Moda Infantil", "Moda Masculina", "Moda Feminina")
levels(vendas$Color) <- c("Preto", "Azul", "Verde", "Vermelho", "Branco", "Amarelo")
levels(vendas$Size) <- c("G", "M", "P", "GG")

colnames(vendas)[4]<-'ID_Usuario'
colnames(vendas)[5]<-'ID_Produto'
colnames(vendas)[6]<-'Nome_Produto'
colnames(vendas)[7]<-'Marca'
colnames(vendas)[8]<-'Categoria'
colnames(vendas)[9]<-'Preco'
colnames(vendas)[10]<-'Nota'
colnames(vendas)[11]<-'Cor'
colnames(vendas)[12]<-'Tamanho'
colnames(vendas)[13]<-'ID_unica'

## Arrumando o layout das datas
vendas <- vendas %>% 
  mutate(`Data Venda` = mdy(vendas$`Data Venda`))

## Salvando a paleta de cores da ESTAT
cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")
theme_estat <- function(...){
  theme <- ggplot2::theme_bw() +
    ggplot2:: theme(
      axis.title.y = ggplot2::element_text( colour = "black ",
                                            size = 12),
      axis.title.x = ggplot2::element_text( colour = "black ",
                                            size = 12),
      axis.text = ggplot2::element_text( colour = " black", size
                                         = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line( colour = " black"),
      legend.position = "top",
      ...
    )
  return (
    list( theme,
          scale_fill_manual( values = cores_estat ),
          scale_colour_manual( values = cores_estat )))}


#### Retirando as linhas com ID unicas que estão duplicadas
duplicated(vendas$ID_unica)
which(duplicated(vendas$ID_unica))
vendas <- vendas[!duplicated(vendas$ID_unica),]

### Faturamento anual por categoria


## gráfico de linhas 
vendas1 <- vendas %>% 
  group_by(mes = month(`Data Venda`))

vendas1$mes <- as.factor(vendas1$mes)
levels(vendas1$mes)  
levels(vendas1$mes) <- c("Jan", "Fev", "Mar", "Abr", "Maio", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

tabela1 <- vendas1 %>% 
  filter(!is.na(mes)) %>% 
  filter(!is.na(Categoria)) %>% 
  filter(!is.na(Preco)) %>% 
  select(Preco, Categoria, mes) %>% 
  group_by(mes, Categoria) %>% 
  summarise(Preco = sum(Preco))

ggplot(tabela1) +
  aes(x = mes, y = Preco, group = Categoria, colour = Categoria) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_colour_manual(name = "Produto", labels = c("A", "B")) +
  labs(x = "Ano", y = "Preço em Reais") +
  theme_estat()
ggsave(filename = file.path(caminho_Leticia, "faturamento-linhas.pdf"), width = 158, height = 93, units = "mm")


### Variação do preço por marca

## Gráfico boxplot
vendas %>% 
  filter(!is.na(Marca)) %>% 
  filter(!is.na(Preco)) %>% 
  ggplot() +
  aes(reorder(Marca, Preco, median), Preco)+
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Marca", y = "Variação do Preço em Reais") +
  theme_estat()
ggsave(filename = file.path(caminho_Leticia, "variação-preco-boxplot.pdf"), width = 158, height = 93, units = "mm")

## Medidas resumo 
quadro_resumo <- vendas%>%
  filter(!is.na(Marca)) %>% 
  filter(!is.na(Preco)) %>%  
  group_by(Marca) %>% 
  summarize( Média = round (mean(Preco),2),
                 `Desvio Padrão ` = round (sd(Preco),2),
                 `Mínimo ` = round (min(Preco),2),
                 `1º Quartil ` = round ( quantile (Preco, probs = .25),2),
                  Mediana = round ( quantile (Preco, probs = .5),2),
                 `3º Quartil ` = round ( quantile (Preco, probs = .75),2),
                 `Máximo ` = round (max(Preco),2)) %>% t() %>% as.data.frame() %>%
  mutate(V1 = str_replace(V1,"\\.",","))
quadro_resumo

### Relação entre categoria (feminino e masculino) e cor 
tab <- vendas %>%
  mutate(Categoria = case_when(
    Categoria %>% str_detect("Moda Feminina") ~ "Moda Feminina",
    Categoria %>% str_detect("Moda Masculina") ~ "Moda Masculina"
  )) %>%
  filter(!is.na(Categoria)) %>% 
  filter(!is.na(Cor)) %>% 
  group_by(Cor, Categoria) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = scales::percent(freq / sum(freq), scale = 100, accuracy = 0.01, labels = percent_format(scale = 100))
  )

porcentagens <- str_c(tab$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(tab$freq, " (", porcentagens, ")"))

# Gráfico de colunas
ggplot(tab) +
  aes(
    x = fct_reorder(Categoria, freq, .desc = T), y = freq,
    fill = Categoria, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Cor", y = "Frequência") +
  scale_y_continuous(limits = c(0, 75))+
  facet_wrap(~Cor)+
  theme_estat()+
  theme(
    axis.text.x = element_text(size = 8)
  )
ggsave(filename = file.path(caminho_Leticia, "Categoria-cor-colunas-bivariado.pdf"), width = 158, height = 93, units = "mm")

## Tabela de contingência 
conting <- as.table(rbind(c(40, 55, 54, 51, 59, 64) , c(52, 55, 49, 46, 65, 45)))
dimnames(conting) <- list(gender = c("Feminino", "Masculino"),
                    party = c("Preto","Azul", "Verde", "Vermelho", "Branco", "Amarelo"))
(tabela2 <- chisq.test(conting))
#qui quadrado
qui <- chisq.test(conting)
#valores observados e esperados 
tabela2$observed
tabela2$expected 
x = 5.479
#coeficiente de contingência corrigido 
c <- sqrt(x/(x+639))
cmax <- sqrt((2-1)/2)
corrigido <- c/cmax
corrigido

### Relação entre preço e avaliação

# Gráfico de dispersão
ggplot(vendas) +
  aes(x = Preco, y = Nota) +
  geom_point(colour = "#A11D21", size = 3) +
  labs(
    x = "Preço dos Produtos em Reais",
    y = "Avaliação dos Produtos"
  ) +
  theme_estat()
ggsave(filename = file.path(caminho_Leticia, "preco-avaliação-dispersão.pdf"), width = 158, height = 93, units = "mm")

# Cálculos de correlação
vendas3 <- vendas %>% 
  filter(!is.na(Preco)) %>% 
  filter(!is.na(Nota)) 

cor(vendas3$Preco, vendas3$Nota, method = "pearson")
cor(vendas3$Preco, vendas3$Nota, method = "spearman")

# junção dos bancos (vendas e devolução)
colnames(devolução_atualizado)[2]<-'ID_unica'
vendas <- left_join(vendas, devolução_atualizado, by = "ID_unica")
colnames(vendas)[17]<-'Motivo de Devolução'
### Frequência de cada tipo de devolução por marca
freq_devolucao <- vendas %>%
  mutate(`Motivo de Devolução` = case_when(
    `Motivo de Devolução` %>% str_detect("Produto com defeito") ~ "Produto com \n defeito",
    `Motivo de Devolução` %>% str_detect("Arrependimento") ~ "Arrependimento",
    `Motivo de Devolução` %>% str_detect("Não informado") ~ "Não \n informado"
  )) %>%
  filter(!is.na(`Motivo de Devolução`)) %>% 
  filter(!is.na(Marca)) %>% 
  group_by( `Motivo de Devolução`, Marca) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = scales::percent(freq / sum(freq), scale = 100, accuracy = 0.01, labels = percent_format(scale = 100))
  )

porcentagens <- str_c(freq_devolucao$freq_relativa) %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(freq_devolucao$freq, " (", porcentagens, ")"))

ggplot(freq_devolucao) +
  aes(
    x = fct_reorder(Marca, freq, .desc = T), y = freq,
    fill = `Motivo de Devolução`, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.3, hjust = - 0.2,
    size = 3
  ) +
  labs(x = "Marca", y = "Frequência") +
  scale_y_continuous(limits = c(0, 45))+
  coord_flip()+
  theme_estat()+
  theme(
    axis.text.x = element_text(size = 6))
ggsave(filename = file.path(caminho_Leticia, "devolução-por-marca-bivariado.pdf"), width = 158, height = 93, units = "mm")
# tabela de contingencia 
conting1 <- as.table(rbind(c(28, 21, 23, 25, 17) , c(15, 21, 27, 19, 29), c(21, 20, 20, 24, 37)))
dimnames(conting1) <- list(gender = c("Defeito", "Não informado", "Arrependimento"),
                           party = c("H&M","Gucci", "Zara", "Adidas", "Nike"))
(tabela89<- chisq.test(conting1))
#qui quadrado
qui1 <- chisq.test(conting1)
#valores observados e esperados 
tabela89$observed
tabela89$expected 
x1 = 12.778
#coeficiente de contingência corrigido 
c1 <- sqrt(x1/(x1+347))
cmax1 <- sqrt((3-1)/3)
corrigido1 <- c1/cmax1
corrigido1


### Avaliação média por marca 

dados <- vendas %>% 
  select(Marca, Nota) %>% 
  filter(!is.na(Marca)) %>% 
  filter(!is.na(Nota))
dados

media_por_marca <- aggregate(Nota ~ Marca, data = dados, mean)

ggplot(media_por_marca, aes(x = Marca, y = Nota, fill = Marca)) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(aes(label = round(Nota, 2)), position = position_dodge(width = .9),
            vjust = -0.5, hjust = 0.5,
            size = 3) + 
  labs(x = "Marcas", y = "Avaliação Média") +
  scale_y_continuous(limits = c(0, 3))+
  guides(fill = "none") +
  theme_estat()
ggsave (filename= file.path(caminho_Leticia,"colunas-bi-avmedia-marca.pdf", width = 158, height = 93, units = "mm"))


