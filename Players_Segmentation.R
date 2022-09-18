library(worldfootballR)
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(stringi)

#Transfermarkt
#Valores de mercado
df1 <- tm_player_market_values(country_name = "Brazil", start_year = 2021)
df1$concatenado <- paste(df1$player_name,"_",df1$squad)
df1 <- df1 %>% select(7,13,14,18,20)


#Estatisticas
df2 <- tm_squad_stats(team_url = "https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/1023/saison_id/2022")

df5 <- fotmob_get_season_stats(
  country = "BRA",
  league_name = "Serie A",
  season = "2022",
  stat_name = c("Chances created", "Big chances created","Big chances missed", "Accurate long balls per 90", "Accurate passes per 90","Shots per 90","Shots on target per 90", "Goals per 90", "Assists", "Goals + Assists","Successful dribbles per 90","Successful tackles per 90","Penalties won","Penalties conceded","Blocks per 90","Clean sheets","Clearances per 90","Fouls committed per 90","Goals conceded per 90","Interceptions per 90","Possession won final 3rd per 90","Save percentage","Saves per 90","Top scorer","Yellow cards","Red cards"),
  team_or_player = "player"
)

#separando variaveis descritivas para remodelagem da base
df_desc <- df5 %>% select(10,9,11,16,15,20)
df_desc <- unique(df_desc)

df5<- df5 %>% select(10,7,13)
df5 <- df5 %>% pivot_wider(names_from = stat_name, values_from = stat_value)
#Substituindo valores NA por 0
df5 <- df5 %>%
  replace(is.na(.), 0)

#retornando variaveis descritivas a base princiapal, pos remodelagem
df5 <- left_join(df5, df_desc, by="particiant_id") 


###################

#Utilizando a API do transfermarkt para conseguir dados de idade e posicao do jogador
Brazilian_teams <- tm_squad_stats(team_url = c("https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/1023/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/614/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/199/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/6600/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/2462/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/679/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/330/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/2863/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/3197/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/221/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/8793/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/10870/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/537/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/585/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/2029/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/28022/saison_id/2022", "https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/776/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/2035/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/15172/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/10492/saison_id/2022"))
#Renomeando coluna 1
#names(Brazilian_teams)[4] <- "participant_name"
names(Brazilian_teams)[1] <- "team_name_transfermarkt"

#APAGAR
#backup <- Brazilian_teams
#Brazilian_teams <- backup

#Selecionando colunas uteis
Brazilian_teams <- Brazilian_teams %>% select(1,4,6,7,8)


back <- Brazilian_teams
Brazilian_teams <- back

#informacoes de valores de mercado e perna preferida
Brazilian_teams$concatenado <- paste(Brazilian_teams$player_name,"_",Brazilian_teams$team_name_transfermarkt)
Brazilian_teams <- left_join(Brazilian_teams, df1, by="concatenado")
Brazilian_teams <- Brazilian_teams %>% select(-6)

#Aplicando tecnicas de mineracao de texto para padronizar nomes de APIs diferentes
#A fim de unificar as bases extraidas do Transfermarkt e Fotmob

#Minerando textos da base Transfermarkt
#Removendo pontuacao de todos os nomes das bases
Brazilian_teams$player_name <- apply(Brazilian_teams[,2,drop=F],2, removePunctuation)
#Convertendo nomes para letras maiusculas
Brazilian_teams$player_name <- apply(Brazilian_teams[,2,drop=F],2, toupper)
#Convertendo tipo de coluna para caractere
Brazilian_teams$player_name<- as.character(Brazilian_teams$player_name)
#Removendo acentuacoes dos nomes
Brazilian_teams$player_name <- stri_trans_general(Brazilian_teams$player_name,id="Latin-ASCII")

#Minerando textos da base Fotmob
#Removendo pontuacao de todos os nomes das bases
df5$participant_name <- apply(df5[,28,drop=F],2, removePunctuation)
#Convertendo tipo de coluna para caractere
df5$participant_name<- as.character(df5$participant_name)
#Convertendo nomes para letras maiusculas
df5$participant_name <- apply(df5[,28,drop=F],2, toupper)
#Removendo acentuacoes dos nomes
df5$participant_name <- stri_trans_general(df5$participant_name,id="Latin-ASCII")


#DETALHAR RECUPERAÇÃO DE +20% AO TRATAR OS NOMES COM MINERACAO DE TEXTO

#DEPARA_TIMES <- df5 %>% select(28,32)
#DEPARA_TIMES2 <- Brazilian_teams
#DEPARA_TIMES2 <- DEPARA_TIMES2 %>% select(1,2)
#names(DEPARA_TIMES2)[1] <- "team_name_transfermarkt"
#DEPARA_TIMES <- left_join(DEPARA_TIMES, DEPARA_TIMES2, by="participant_name")
#DEPARA_TIMES <- DEPARA_TIMES %>% select(2,3)
#DEPARA_TIMES <- unique(DEPARA_TIMES)


team_name_fotmob <- unique(df5$team_name)
team_name_transfermarkt <- c("Sociedade Esportiva Palmeiras","Ceará Sporting Club","Red Bull Bragantino","Fluminense Football Club","Clube Atlético Mineiro","Sport Club Internacional",
       "Clube de Regatas do Flamengo","São Paulo Futebol Clube","América Futebol Clube (MG)","Cuiabá Esporte Clube (MT)","Atlético Clube Goianiense","Coritiba Foot Ball Club","Fortaleza Esporte Clube","Botafogo de Futebol e Regatas","Avaí Futebol Clube (SC)","Santos FC","Goiás Esporte Clube","Club Athletico Paranaense","Esporte Clube Juventude","Sport Club Corinthians Paulista")
team_names <- data.frame(team_name_fotmob, team_name_transfermarkt)


Brazilian_teams$team_name_DEPARA <- left_join(Brazilian_teams, team_names, by="team_name_transfermarkt")
Brazilian_teams$team_name_DEPARA <- Brazilian_teams$team_name_DEPARA$team_name_fotmob
Brazilian_teams$concatenado <- paste(Brazilian_teams$player_name,"_",Brazilian_teams$team_name_DEPARA)
Brazilian_teams <- Brazilian_teams %>% select(7,3,4)
#Brazilian_teams2 <- Brazilian_teams


teste1 <- Brazilian_teams
teste2 <- df5

df5 <- teste2

df5$concatenado <- paste(df5$participant_name,"_",df5$team_name)
#A GRANDE MAIORIA DOS RESTANTES SAIRAM DOS SEUS TIMES
#ALTERANDO JOGADORES QUE ESTAO CADASTRADOS COM NOMES DIFERENTES, ERRO DE DIGITAÇÃO E QUE NAO FORAM VENDIDOS DE SEUS TIMES
library(stringr)
df5$concatenado <- str_replace(df5$concatenado,"RONI _ Palmeiras","RONY _ Palmeiras")
df5$concatenado <- str_replace(df5$concatenado,"MURILO CERQUEIRA _ Palmeiras","MURILO _ Palmeiras")
df5$concatenado <- str_replace(df5$concatenado, "JOSE LOPEZ _ Palmeiras","JOSE MANUEL LOPEZ _ Palmeiras")
df5$concatenado <- str_replace(df5$concatenado, "LUAN GARCIA _ Palmeiras","LUAN _ Palmeiras")
df5$concatenado <- str_replace(df5$concatenado,"LUCIANO NEVES _ São Paulo","LUCIANO _ São Paulo")
df5$concatenado <- str_replace(df5$concatenado,"DIEGO _ São Paulo","DIEGO COSTA _ São Paulo")
df5$concatenado <- str_replace(df5$concatenado,"SANDRY SANTOS _ Santos","SANDRY _ Santos")
df5$concatenado <- str_replace(df5$concatenado,"PAULO GANSO _ Fluminense","GANSO _ Fluminense")
df5$concatenado <- str_replace(df5$concatenado,"VIKTOR HUGO _ Flamengo","VICTOR HUGO _ Flamengo")
df5$concatenado <- str_replace(df5$concatenado,"RAUL _ Corinthians","RAUL GUSTAVO _ Corinthians")
df5$concatenado <- str_replace(df5$concatenado,"EDUARDO QUEIROZ _ Corinthians","DU QUEIROZ _ Corinthians")
df5$concatenado <- str_replace(df5$concatenado,"RAMIRES _ Bragantino","ERIC RAMIRES _ Bragantino")
df5$concatenado <- str_replace(df5$concatenado,"ARTUR GUIMARAES _ Bragantino","ARTUR _ Bragantino")
df5$concatenado <- str_replace(df5$concatenado,"VICTOR LEANDRO CUESTA _ Botafogo","VICTOR CUESTA _ Botafogo")
df5$concatenado <- str_replace(df5$concatenado,"PEDRO ROCHA _ Atlético Paranaense","PEDRO ROCHA _ Fortaleza EC")
df5$concatenado <- str_replace(df5$concatenado,"RODRIGUINHO _ América Mineiro","RODRIGUINHO _ Cuiabá")
df5$concatenado <- str_replace(df5$concatenado,"JO _ Corinthians","JO _ Ceará")
df5 <- left_join(df5, Brazilian_teams, by="concatenado")


#ENTENDER PQ ALGUNS NOMES FORAM PERDIDOS NA CONCATENACAO
#EXCLUSAO DE NAS DEVERIA SER DE 18%

teste<- df5
teste <- na.omit(teste)
nrow(teste)
nrow(df5)



#nrow(teste)
#unique(teste$concatenado)
write.csv(df5, "C:/Users/thiag/Desktop/df5.csv")

write.csv(teste, "C:/Users/thiag/Desktop/teste.csv")
write.csv(Brazilian_teams, "C:/Users/thiag/Desktop/brazilian_teams.csv")

#rICHARD CEARA NOME IGUAL GOLEIRO E VOLANTE - TRATAR
#DANILO NOME IGUAL SAIU DO BRASIL

#Checando quantidade de linhas da base
nrow(df5)
#Porcentagem de valores NAS em cada coluna
NAS <- round(colSums(is.na(df5))*100/nrow(df5), 2)
#VER TODOS NAS
NAS

#Removendo valores NAs
df5<- na.omit(df5)
#Chencando numero de linhas restantes
nrow(df5)


############# REMODELAR BASE PARA ALGORITMO


#COLUNAR POSICOES
#AGRUPAR FAIXAS DE IDADES

#CHECAR ESTATISTIACAS REPETIDAS PARA MESMOS NOMES
#INCLUIR DEMAIS ESTATIISTICAS INDIVIDIUAS DEFENSIVAS NA CHAMADA DA API










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
G11 <- kmeans(BDclusters[3:16], centers=11)

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
viz_G11 <- fviz_cluster(G11, data = BDclusters[3:16],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585", "#C71585", "#C71585" ), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())



library(gridExtra)
grid.arrange(viz_G2,viz_G3, viz_G4, viz_G5,viz_G6, viz_G7, viz_G8, viz_G9, viz_G10, viz_G11, nrow=2, ncol=5)


viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#FFFF00","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "euclid", ggtheme = theme_bw())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "norm", ggtheme = theme_bw())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_gray())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_void())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(3:16)],palette = c("#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_classic())

grid.arrange(viz_G3, nrow=1, ncol=1)
#porcentagens em cada cluster


BDclusters$G3 <- G3$cluster

library(ggplot2)
par(mfrow=c(1,1))

#comparacao dos modelos por variavel especifica - Gols
boxplot(BDclusters$`Goals per 90` ~ BDclusters$G3, col= c("#000000", "#000000", "#DC143C"), main="Gols")


#CLUSTERS
G3$cluster
#CENTROS DOS CLUSTERS
G3$centers
#TAMANHO DOS CLUSTERS
G3$size

#Proporcao da quantidade de individuos por grupo
round(table(BDclusters$G3)*100/nrow(BDclusters),2)
table(BDclusters$G3)

#Média de Gols por grupos de clusters
BDclusters %>% group_by(G3) %>% summarise_at(vars(`Goals per 90`), list(Media = mean))


