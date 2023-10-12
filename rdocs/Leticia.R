vendas <- read_csv("banco/vendas.csv")
devolução <- read_csv("banco/devolução.csv")

library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)

### Modificando o banco de dados mutate

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


install.packages("DBI")
install.packages("RSQLite")
library(DBI)
library(RSQLite)

#### Retirando as linhas com ID unicas que estão duplicadas
duplicated(vendas$ID_unica)
which(duplicated(vendas$ID_unica))
vendas <- vendas[!duplicated(vendas$ID_unica),]
### Faturamento anual por categoria
# calcular o farturamento bruto (soma)
# calcular as porcentagem em relação ao faturamento total 
# montar a tabela
# montar graficos separados
# montar um gráfico junto 

vendas <- vendas %>% 
  mutate(`Data Venda` = mdy(vendas$`Data Venda`))

tabelax <- vendas %>% 
  filter(!is.na(Preco)) %>% 
  filter(!is.na(Categoria)) %>% 
  select(Preco, Categoria) %>% 
  group_by(Categoria) %>% 
  summarise(Preco = sum(Preco)) %>%
  mutate(Freq = Preco / sum(Preco))
tabelax

# Ordenar as categorias pelo faturamento (Preco) em ordem decrescente
tabelax <- tabelax %>% arrange(desc(Preco))
tabelax

ggplot(tabelax, aes(fct_reorder(Categoria, -Preco), Preco, label = scales::percent(Freq)))  +
  geom_bar(stat = "identity", fill = c("#A11D21", "#003366", "#CC9900")) +
  xlab("Categoria")+
  ylab("Faturamento Anual")+
  ggtitle("Gráfico de barras do faturamento anual por categoria") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, hjust = .5,
            size = 3)  +
  theme_estat() +
  theme(legend.title = element_blank())


