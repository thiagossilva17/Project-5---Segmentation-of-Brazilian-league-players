
#Transfermarkt
#Valores de mercado
df1 <- get_player_market_values(country_name = "Brazil", start_year = 2021)
#Estatisticas
df2 <- tm_squad_stats(team_url = "https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/1023/saison_id/2021")

df5 <- fotmob_get_season_stats(
  country = "BRA",
  league_name = "Serie A",
  season = "2022",
  stat_name = c("Chances created", "Big chances created","Big chances missed", "Accurate long balls per 90", "Accurate passes per 90","Shots per 90","Shots on target per 90", "Goals per 90", "Assists", "Goals + Assists","Successful dribbles per 90","Successful tackles per 90","Penalties won","Penalties conceded"),
  team_or_player = "player"
)
df5<- df5 %>% select(10,9,7,13)
df5 <- df5 %>% pivot_wider(names_from = stat_name, values_from = stat_value)
#Substituindo valores NA por 0
df5 <- df5 %>%
  replace(is.na(.), 0)

#####################
BDclusters <- df5

#install.packages('caret')
library(caret)

#NORMALIZANDO DADOS E ARMAZENANDO NA VARIAVEL "DADOS"
dados<- scale(BDclusters[,c(3:16)])

#REALIZANDO PREDICAO COM ALGORITMO DE CLUSTERIZACAO E UTILIZANDO METODO DE NORMALIZACAO "SCALE"    
BDclusters <- predict(preProcess(BDclusters, method ="scale") ,BDclusters)
#COMANDO PARA GARANTIR QUE O LEITOR CHEGUE AO MESMO RESULTADO
set.seed(1)

#SEPARANDO DIFERENTES TIPOS DE CLUSTERS
G2 <- kmeans(BDclusters[3:16], centers=2)
G3 <- kmeans(BDclusters[3:16], centers=3)
G4 <- kmeans(BDclusters[3:16], centers=4)
G5 <- kmeans(BDclusters[3:16], centers=5)
G6 <- kmeans(BDclusters[3:16], centers=6)
G7 <- kmeans(BDclusters[3:16], centers=7)
G8 <- kmeans(BDclusters[3:16], centers=8)
G9 <- kmeans(BDclusters[3:16], centers=9)
G10 <- kmeans(BDclusters[3:16], centers=10)

library(factoextra) #fviz_cluster function
viz_G2 <- fviz_cluster(G2, data = BDclusters[,c(3:16)],palette = c("#2E9FDF", "#00AFBB"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G4 <- fviz_cluster(G4, data = BDclusters[,c(3:16)],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G5 <- fviz_cluster(G5, data = BDclusters[,c(3:16)],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G6 <- fviz_cluster(G6, data = BDclusters[3:16],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G7 <- fviz_cluster(G7, data = BDclusters[3:16],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G8 <- fviz_cluster(G8, data = BDclusters[3:16],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G9 <- fviz_cluster(G9, data = BDclusters[3:16],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G10 <- fviz_cluster(G10, data = BDclusters[3:16],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585", "#C71585" ), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())




library(gridExtra)
grid.arrange(viz_G2,viz_G3, viz_G4, viz_G5, nrow=2, ncol=2)

grid.arrange(viz_G2,viz_G3, viz_G4, viz_G5,viz_G6, viz_G7, nrow=2, ncol=5)

grid.arrange(viz_G2,viz_G3, viz_G4, viz_G5,viz_G6, viz_G7, viz_G8, viz_G9, viz_G10, nrow=2, ncol=5)


viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#FFFF00","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "euclid", ggtheme = theme_bw())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "norm", ggtheme = theme_bw())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_gray())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_void())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_classic())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "confidence", ggtheme = theme_pubr())

grid.arrange(viz_G3, nrow=1, ncol=1)

?fviz_cluster

#porcentagens em cada cluster