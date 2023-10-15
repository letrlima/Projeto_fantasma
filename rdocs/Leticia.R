vendas <- read_csv("banco/vendas.csv")
devolução <- read_csv("banco/devolução.csv")
caminho_Leticia <- "resultados"

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

vendas <- vendas %>% 
  mutate(`Data Venda` = mdy(vendas$`Data Venda`))

##Tabela porcentagem faturamento por categoria
tabelax <- vendas %>% 
  filter(!is.na(Preco)) %>% 
  filter(!is.na(Categoria)) %>% 
  select(Preco, Categoria) %>% 
  group_by(Categoria) %>% 
  summarise(Preco = sum(Preco)) %>%
  mutate(Freq = Preco / sum(Preco))
tabelax

#Ordenando em ordem decrescente
tabelax <- tabelax %>% arrange(desc(Preco))
tabelax

# Configurar a formatação de porcentagem com duas casas decimais
percent_format <- scales::percent_format(accuracy = 0.01)

## Gráfico de barras do Faturamento anual por Categoria
ggplot(tabelax, aes(fct_reorder(Categoria, -Preco), Preco, label = percent_format(Freq)))  +
  geom_bar(stat = "identity", fill = c("#A11D21", "#003366", "#CC9900")) +
  xlab("Categoria")+
  ylab("Faturamento Anual")+
  ggtitle("Gráfico de barras do faturamento anual por categoria") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, hjust = .5,
            size = 3)  +
  theme_estat() +
  theme(legend.title = element_blank())
ggsave(filename = file.path(caminho_Leticia, "barras_freq_faturamento.pdf"), width = 158, height = 93, units = "mm")

## Gráfico de Setores do Faturamento anual por Categoria
ggplot(tabelax, aes(x = "", y = Preco, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  xlab("") +
  ylab("") +
  ggtitle("Gráfico de Setores do Faturamento Anual por Categoria") +
  scale_fill_manual(values = c("#A11D21", "#003366", "#CC9900")) +
  geom_text(aes(x = 1.8, label = scales::percent(Freq)), position = position_stack(vjust = 0.5))+
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = cores_estat, name = 'Categorias')
ggsave(filename = file.path(caminho_Leticia, "setores_freq_faturamento.pdf"), width = 158, height = 93, units = "mm")

##### DAR UM JEITO DE COLOCAR COM DUAS CASAS DECIMAIS 


## Gráfico de barras do faturamento por categorias (separado)
tabelax %>% 
  filter(Categoria == "Moda Feminina") %>% 
  ggplot(aes(fct_reorder(Categoria, -Preco), Preco, label = percent_format(Freq)))  +
  geom_bar(stat = "identity", fill = "#A11D21") +
  xlab("Categoria")+
  ylab("Faturamento Anual")+
  ggtitle("Gráfico de barras do faturamento anual por categoria") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, hjust = .5,
            size = 3)  +
  theme_estat() +
  theme(legend.title = element_blank())

tabelax %>% 
  filter(Categoria == "Moda Masculina") %>% 
  ggplot(aes(fct_reorder(Categoria, -Preco), Preco, label = percent_format(Freq)))  +
  geom_bar(stat = "identity", fill = "#A11D21") +
  xlab("Categoria")+
  ylab("Faturamento Anual")+
  ggtitle("Gráfico de barras do faturamento anual por categoria") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, hjust = .5,
            size = 3)  +
  theme_estat() +
  theme(legend.title = element_blank())

tabelax %>% 
  filter(Categoria == "Moda Infantil") %>% 
  ggplot(aes(fct_reorder(Categoria, -Preco), Preco, label = percent_format(Freq)))  +
  geom_bar(stat = "identity", fill = "#A11D21") +
  xlab("Categoria")+
  ylab("Faturamento Anual")+
  ggtitle("Gráfico de barras do faturamento anual por categoria") + 
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, hjust = .5,
            size = 3)  +
  theme_estat() +
  theme(legend.title = element_blank())


tabelay <- vendas %>% 
  filter(!is.na(Preco)) %>% 
  filter(!is.na(Categoria)) %>% 
  filter(Categoria == "Moda Feminina") %>% 
  select(Preco, Categoria, Nome_Produto) %>% 
  group_by(Nome_Produto) %>% 
  summarise(Preco = sum(Preco)) %>%
  mutate(Freq = Preco / sum(Preco))