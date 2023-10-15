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

## Gráfico de colunas do faturamento anual por categoria
tabela1 <- vendas %>%
  filter(!is.na(Preco)) %>%
  filter(!is.na(Categoria)) %>%
  select(Preco, Categoria) %>% 
  group_by(Categoria) %>% 
  summarise(Preco = sum(Preco)) %>% 
  mutate(freq = Preco / sum(Preco)) %>%
  mutate(
    freq = scales::percent(freq, scale = 100, accuracy = 0.01),
    freq = gsub("\\.", ",", freq) %>% paste( sep = ""),
    label = str_c(Preco, " (", freq, ")") %>% str_squish()
  )

ggplot(tabela1) +
  aes(x = fct_reorder(Categoria, Preco, .desc = TRUE), y = Preco, label = label) +
  geom_bar(stat = "identity", fill = c("#003366","#A11D21", "#CC9900"), width = 0.7) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) + 
  labs(x = "Categoria", y = "Faturamento anual")
ggsave(filename = file.path(caminho_Leticia, "faturamento-colunas-freq.pdf"), width = 158, height = 93, units = "mm")


## Gráfico de setores do faturamento anual por categoria
tabela2 <- vendas %>% 
  filter(!is.na(Preco)) %>%
  filter(!is.na(Categoria)) %>%
  select(Preco, Categoria) %>% 
  group_by(Categoria) %>% 
  summarise(Preco = sum(Preco)) %>% 
  mutate(freq = round(100*(Preco / sum(Preco)), 2)) %>%
  arrange(desc(Categoria)) %>%
  mutate(posicao = cumsum(freq) - 0.5*freq)

ggplot(tabela2) +
  aes(x = factor(""), y = freq , fill = factor(Categoria)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(freq, "%")),
    color = "black"
  ) +
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = cores_estat, name = 'Categoria')
ggsave(filename = file.path(caminho_Leticia, "faturamento-setores-freq.pdf"), width = 158, height = 93, units = "mm")


