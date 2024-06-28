# Projeto final TCC - MBA USP - Esalq


# Instalando pacotes


pacotes <- c ("tidyverse", "dplyr", "ggmap", "GGally", "cluster",
              "geobr", "ggrepel", "ggcorrplot", "knitr", "reshape2",
              "kableExtra", "psych", "FactoMineR", "factoextra", "repr")


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Carregando o dataset

dados_bairros <- read.csv('C:/Users/Neto/Documents/MBA_project/Projeto-TCC/arquivos/2Dataset_utf_8_IDHM_2000_2010.csv', encoding = "UTF-8")

# analisando todas as variáveis

names(dados_bairros)

#Limpeza de dados

# Serão utilizados apenas os dados de 2010. Assim, os dados do ano 2000 serão excluídos

dados_bairros_2010 <- dados_bairros[dados_bairros$ANO == 2010,] 
head(dados_bairros_2010) 

# Selecionando e criando um novo dataset com as colunas com informações sobre o IDH municipal a serem utilizadas para a análise

dados_bairros_IDHM <- dados_bairros_2010 %>% select("DISTRITO", "CODDIST", "REGIAO8", "IDHM") 

head(dados_bairros_IDHM) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Verificando a existência de observações vazias ou nulas

sum(is.na(dados_bairros_IDHM))
sum(is.null(dados_bairros_IDHM))

dados_bairros_IDHM <- na.omit(dados_bairros_IDHM)

# Tirando a média dos valores de bairros repetidos medidos em diferentes UDHs (Unidades de Desenvolvimento Humano) de São Paulo

dados_bairros_unicos <- dados_bairros_IDHM %>% group_by(Bairro=DISTRITO, Codigo=CODDIST, Regiao=REGIAO8) %>% 
  summarise(IDHM=mean(IDHM),
            .groups = 'drop') %>% 
  mutate(Regiao = recode(Regiao, "Norte 1"="Norte", "Norte 2"="Norte",
                         "Leste 1"="Leste", "Leste 2"="Leste",
                         "Sul 1"="Sul", "Sul 2"="Sul")) %>% 
  mutate(Bairro = recode(Bairro, "São Miguel "="São Miguel"))

# importando os dados dos bairros e o mapa da desigualdade

mapa_desigualdade <- read.csv("C:/Users/Neto/Documents/MBA_project/Projeto-TCC/2mapa-da-Desigualdade-2022-–-planilha-aberta.csv", encoding = "UTF-8")

names(mapa_desigualdade)

mapa_desigualdade <- mapa_desigualdade[-97,]

# selecionando as variáveis

mapa_desigualdade <- mapa_desigualdade %>% 
  select("DISTRITO", "População.preta.e.parda", "Favelas", "Homicídios", "Mortes.por.intervenção.policial", "Agressões.por.intervenção.policial")

# renomeando variáveis

mapa_desigualdade <- mapa_desigualdade %>% rename(Bairro=DISTRITO, 
                                                  populacao_preta_e_parda=População.preta.e.parda,
                                                  favelas=Favelas,
                                                  homicidios=Homicídios,
                                                  mortes_intervencao_policial=Mortes.por.intervenção.policial,
                                                  agressoes_intervencao_policial=Agressões.por.intervenção.policial) 
  

dados_bairros_completo <- full_join(dados_bairros_unicos,mapa_desigualdade)

head(dados_bairros_completo) 
summary(dados_bairros_completo)

# Transformados dados

dados_bairros_completo$populacao_preta_e_parda <- gsub(",", ".", dados_bairros_completo$populacao_preta_e_parda)
dados_bairros_completo$favelas <- gsub(",", ".", dados_bairros_completo$favelas)
dados_bairros_completo$homicidios <- gsub(",", ".", dados_bairros_completo$homicidios)
dados_bairros_completo$mortes_intervencao_policial <- gsub(",", ".", dados_bairros_completo$mortes_intervencao_policial)
dados_bairros_completo$agressoes_intervencao_policial <- gsub(",", ".", dados_bairros_completo$agressoes_intervencao_policial)

dados_bairros_completo[, -c(1:3)] <- lapply(dados_bairros_completo[ , -c(1:3)], function(x) as.numeric(as.character(x)))

# Padronizando os dados com a função "scale"

dados_bairros_completo[,4:9] <- scale(dados_bairros_completo[,4:9])

dados_bairros_completo %>% select(-c(2, 3)) %>% melt() %>% 
  ggplot(aes(label=Bairro)) +
  ggtitle("Variáveis padronizadas") + 
  geom_boxplot(aes(x = variable, y = value, fill = variable)) +
  geom_point(aes(x = variable, y = value), alpha = 0.5) +
  labs(x = "Variável",
       y = "Medida") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Atribuindo a localização aos bairros para plotagem do mapa

maps_bairros <- geobr::read_neighborhood() 

maps_bairros <- maps_bairros %>%  
  filter(str_detect(name_muni, "S.o Paulo$"))

maps_bairros <- maps_bairros %>% rename(Bairro=name_district) %>% 
  mutate(Bairro = recode(Bairro, "Parque Do Carmo"="Parque do Carmo", "Mooca"="Moóca",
                         "Freguesia Do Ó"="Freguesia do Ó", "Cidade Lider"="Cidade Líder",
                         "Cangaiba"="Cangaíba", "Alto De Pinheiros"="Alto de Pinheiros", "São Miguel"="São Miguel"))

dados_bairros_mapeados <- maps_bairros %>% full_join(dados_bairros_completo) %>% 
  select_if(~ !any(is.na(.)))

summary(dados_bairros_mapeados)

head(dados_bairros_mapeados) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Plotando mapas com variáveis

## IDH 

ggplot() +
  geom_sf(data = dados_bairros_mapeados, aes(fill = IDHM), color = 'grey', size = .15) +
  labs(title = 'IDH dos bairros de São Paulo', size = 10) +
  geom_sf_text(data = dados_bairros_mapeados, aes(label = Bairro), size = 2, 
               color = 'black') +
  scale_fill_distiller(palette = 'YlGn', direction = 1, name = 'IDH') 


## População Preta e Parda

ggplot() +
  geom_sf(data = dados_bairros_mapeados, aes(fill = populacao_preta_e_parda), color = 'grey', size = .15) +
  labs(title = 'Percentagem da população preta e parda dos bairros de São Paulo', size = 10) +
  geom_sf_text(data = dados_bairros_mapeados, aes(label = Bairro), size = 2, 
               color = 'black') +
  scale_fill_distiller(palette = 'YlOrBr', direction = 1, name = 'População Preta e Parda (%)') 


## Favelas

ggplot() +
  geom_sf(data = dados_bairros_mapeados, aes(fill = favelas), color = 'grey', size = .15) +
  labs(title = 'Porcentagem de favelas nos bairros de São Paulo', size = 10) +
  geom_sf_text(data = dados_bairros_mapeados, aes(label = Bairro), size = 2, 
               color = 'black') +
  scale_fill_distiller(palette = 'Oranges', direction = 1, name = 'Favelas (%)') 

## Homicídios

ggplot() +
  geom_sf(data = dados_bairros_mapeados, aes(fill = homicidios), color = 'grey', size = .15) +
  labs(title = 'Porcentagem de homicídios nos bairros de São Paulo', size = 10) +
  geom_sf_text(data = dados_bairros_mapeados, aes(label = Bairro), size = 2, 
               color = 'black') +
  scale_fill_distiller(palette = 'Reds', direction = 1, name = 'Homicídios (%)') 

## Mortes por intervenção policial

ggplot() +
  geom_sf(data = dados_bairros_mapeados, aes(fill = mortes_intervencao_policial), color = 'grey', size = .15) +
  labs(title = 'Porcentagem de mortes por intervenção policial nos bairros de São Paulo', size = 10) +
  geom_sf_text(data = dados_bairros_mapeados, aes(label = Bairro), size = 2, 
               color = 'black') +
  scale_fill_distiller(palette = 'YlOrRd', direction = 1, name = 'Mortes por intervenção policial (%)') 

## Agressões por intervenção policial

ggplot() +
  geom_sf(data = dados_bairros_mapeados, aes(fill = mortes_intervencao_policial), color = 'grey', size = .15) +
  labs(title = 'Porcentagem de agressões por intervenção policial nos bairros de São Paulo', size = 10) +
  geom_sf_text(data = dados_bairros_mapeados, aes(label = Bairro), size = 2, 
               color = 'black') +
  scale_fill_distiller(palette = 'RdPu', direction = 1, name = 'Agressões por intervenção policial (%)') 

# Verificando a correlação entre as variáveis

## utilizando a função 'cor_pmat' pra verificar o p-value das correlações a uma significância de 5%

pvalor <- dados_bairros_completo[, -c(1:3)] %>% cor_pmat() %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

correl <- cor(dados_bairros_completo[, -c(1:3)]) 

cor_pmat(dados_bairros_completo[, -c(1:3)]) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

ggcorrplot(correl, hc.order=TRUE,type="lower",
           lab = TRUE,
           lab_size = 6,
           p.mat = cor_pmat(dados_bairros_completo[, -c(1:3)]),
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "Correlações")


## apresentando graficamente as variáveis que apresentaram correlação significativa

dados_bairros_completo %>% 
  select(IDHM, favelas, populacao_preta_e_parda, homicidios) %>% 
  ggpairs()

# Aplicando a técnica de PCA para plotar as variáveis em menos dimensões e entender a relação entre elas

 ## Teste de esfericidade de Bartlett pra verificar a aplicabilidade do PCA

as.data.frame(cortest.bartlett(dados_bairros_completo[,4:7])) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

## Gerando PCA

analise_pca <- PCA(dados_bairros_completo[,4:7], graph = FALSE)

## Proporção de variância dos componentes principais

analise_pca$eig %>% 
  kable(col.names=c("Autovalores", 
                    "Prop. da Variância",
                    "Prop. da Variância Acumulada")) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

## Plotando gráfico para demonstrar a variância de cada variável

fviz_eig(analise_pca, addlabels = TRUE)

## Contribuição de cada variável por componente principal

analise_pca$var$contrib %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

### Apesar do critério de Kaiser sugerir a escolha de dimensões com
### autovalores maiores que 1, verifica-se que a variável "Favelas"
### possui maior representatividade no segundo componente (PC2).
### (demonstrado no gráfico abaixo)
### Assim, utiliza-se as duas primeiras dimensões para análise.

## Verificando a influência de cada variáveil por componente

pca_prcomp <- dados_bairros_completo[,4:7] %>% prcomp()

as.data.frame(pca_prcomp$rotation) %>% 
  kable(caption = "Influência de cada variável por componente") %>% 
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Plotando o gráfico por componentes principais 

var <- get_pca_var(analise_pca)
ind <- get_pca_ind(analise_pca)

fviz_pca_var(analise_pca,
             col.circle = "grey", repel = TRUE, col.var = "steelblue",
             ggtheme = theme_minimal())
              

## Distribuição das regiões no gráfico de PCA

grupo <- as.factor(dados_bairros_completo$Regiao)

fviz_pca_biplot(analise_pca, geom = "point",  repel = TRUE,
                addEllipses=TRUE, ellipse.level=0.95, palette = "Dark2",
                habillage = grupo, title = "PCA - bairros de São Paulo")

# Criando cluster dos Bairros por similaridade 

## Matriz de dissimilaridades

matriz <- dados_bairros_completo[,5:7] %>% 
  dist(method = "euclidean")

## Clusterização com o método "complete linkage"

cluster <- cluster::agnes(x = matriz, method = "complete")

## Plotando o dendograma com a divisão dos clusters

fviz_dend(x = cluster,
          h = 2,
          color_labels_by_k = T,
          rect = T,
          rect_fill = T,
          rect_border = "black",
          lwd = 1,
          show_labels = T,
          repel = T,
          main = "Dendograma dos clusters",
          ggtheme = theme_bw())


# Plotando os cluster no mapa da cidade de São Paulo

dados_bairros_mapeados$clusters <- factor(cutree(tree = cluster, k = 11))

ggplot() +
  geom_sf(data = dados_bairros_mapeados, aes(fill = clusters), color = 'grey', size = .15) +
  labs(title = 'Clusters dos bairros similares', size = 10) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_sf_text(data = dados_bairros_mapeados, aes(label = clusters), size = 2, 
               color = 'black') 

####################################################

cluster_10 <- cutree(as.hclust(cluster), k = 10)
dados_num <- dados_bairros_completo[,5:7]
rownames(dados_num) <- dados_bairros_completo$Bairro
fviz_cluster(list(data = dados_num, cluster = cluster_10), xlab = F, ylab = F, repel = T)

