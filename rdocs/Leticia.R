vendas <- read_csv("banco/vendas.csv")
devolução <- read_csv("banco/devolução.csv")
caminho_Leticia <- "resultados"
teste <- "teste"

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
levels(vendas1$mes) <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")

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
  labs(x = "Ano", y = "Preço") +
  theme_estat()
ggsave(filename = file.path(caminho_Leticia, "faturamento-linhas.pdf"), width = 158, height = 93, units = "mm")


