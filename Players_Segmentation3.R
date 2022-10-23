library(worldfootballR)
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(stringi)
library(xlsx)


##### PARTE 1 - DADOS DE DESEMPENHO FUTMOB E REMODELAGEM DE DADOS

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


#VALIDADOR DE REPETICAO - TROCA DE TIME (CASO PEDRO ROCHA)
nrow(df_desc)
unique(df_desc$particiant_id)
#subtracao



##################################

# UTILIZANDO API DO TRANSFERMARKT PARA TRAZER DADOS DE IDADE

#PREPARACAO
Brazilian_teams <- tm_squad_stats(team_url = c("https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/1023/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/614/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/199/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/6600/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/2462/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/679/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/330/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/2863/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/3197/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/221/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/8793/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/10870/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/537/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/585/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/2029/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/28022/saison_id/2022", "https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/776/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/2035/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/15172/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/10492/saison_id/2022"))
names(Brazilian_teams)[1] <- "team_name_transfermarkt"
Brazilian_teams <- Brazilian_teams %>% select(1,4,6,7,8)

back <- Brazilian_teams
back2 <- df5

Brazilian_teams <- back
df5 <- back2

# TRATANDO DADOS DE TEXTO PARA UNIFICAÇÃO DE BASES

#COMENTARIO EXPLICATIVO: IMPORTANTE:

#ANTES DE FAZER A UNIÃO DAS DUAS BASES, VALE RESSALTAR QUE NÃO EXISTE UM CAMPO "ID" QUE RELACIONE OS JOGADORES ENTRE AS DUAS APIs, 
#ASSIM SERA NECESSARIO FAZER ESTE CRUZAMENTO DIRETAMENTE PELO NOME DOS ATLETAS, PORÉM EXISTEM MUITOS ATLETAS DE NOMES IGUAIS
#ENTÃO PARA DIFERENCIÁ-LOS CONCATENAREMOS O NOME DO JOGADOR COM O NOME DO TIME, 
#MAS PARA ISSO SERÁ NECESSÁRIO UTILIZAR TECNICAS DE MINERAÇÃO DE TEXTO PARA PADRONIZARMOS

#OUTRO FATOR IMPORTANTE, SERÁ A PADRONIZAÇÃO DOS NOMES DOS JOGADORES COM O USO DE TÉCNICAS DE MINERAÇÃO DE TEXTO, 
#POIS OS MESMOS JOGADORES TIVERAM SEUS NOMES DESCRITOS DE FORMAS DIFERENTES NAS DUAS APIs, ALGUNS ESTÃO COM NOMES COMPOSTOS, 
#OUTROS ESTÃO COM APELIDOS OU ABREVIAÇÕES E AMBAS AS FONTES DE APIs NÃO POSSUEM O MESMO CRITÉRIO DE NOMENCLATURA DOS ATLETAS. 

#ASSIM A IDÉIA AQUI É REMOVERMOS AS PONTUAÇÕES E ACENTUAÇÕES DE TODOS OS NOMES, E PADRONIZARMOS TODOS ELES COM LETRA MAIUSCULA, 
#PARA QUE TENHAMOS O MINIMO DE DIFERENÇA DE ESCRITA ENTRE AS DUAS BASES

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


#COMENTARIO EXPLICATIVO: IMPORTANTE:
#APÓS PADRONIZACAO DOS NOMES DOS ATLETAS, É NECESSARIO CRIAR O DE/PARA ABAIXO PARA PADRONIZAR TAMBÉM OS NOMES DOS TIMES DAS DUAS APIs.

team_name_fotmob <- c("Palmeiras","Ceará","Bragantino","Fluminense","Atlético Mineiro","Internacional","Flamengo","São Paulo","América Mineiro","Cuiabá","Atlético Goianiense","Coritiba","Fortaleza EC","Botafogo","Avaí","Santos","Goiás","Atlético Paranaense","Juventude","Corinthians")
team_name_transfermarkt <- c("Sociedade Esportiva Palmeiras","Ceará Sporting Club","Red Bull Bragantino","Fluminense Football Club","Clube Atlético Mineiro","Sport Club Internacional",
                             "Clube de Regatas do Flamengo","São Paulo Futebol Clube","América Futebol Clube (MG)","Cuiabá Esporte Clube (MT)","Atlético Clube Goianiense","Coritiba Foot Ball Club","Fortaleza Esporte Clube","Botafogo de Futebol e Regatas","Avaí Futebol Clube (SC)","Santos FC","Goiás Esporte Clube","Club Athletico Paranaense","Esporte Clube Juventude","Sport Club Corinthians Paulista")
team_names <- data.frame(team_name_fotmob, team_name_transfermarkt)
Brazilian_teams$team_name_DEPARA <- left_join(Brazilian_teams, team_names, by="team_name_transfermarkt")
Brazilian_teams$team_name_DEPARA <- Brazilian_teams$team_name_DEPARA$team_name_fotmob


#CONCATENANDO NOME DE JOGADOR COM NOME DE TIME PARA AMBAS AS BASES
Brazilian_teams$concatenado <- paste(Brazilian_teams$player_name,"_",Brazilian_teams$team_name_DEPARA)
df5$concatenado <- paste(df5$participant_name,"_",df5$team_name)

back3 <- Brazilian_teams
back4 <- df5

Brazilian_teams <- back3
df5 <- back4


#COMENTARIO EXPLICATIVO: IMPORTANTE:
#MESMO COM A MINERAÇÃO DE TEXTO, ALGUNS ATLETAS IMPORTANTES NÃO CONSEGUIRIAM SER ENCONTRADOS AO UNIFICAR AS DUAS BASES, 
#DEVIDO A DESPADRONIZAÇÃO NA FORMA DE NOMENCLATURA DAS APIs, COMO POR EXEMPLO O IMPORTANTE JOGADOR DO CORINTHIANS EDUARDO QUEIROZ 
#QUE EM UMA DAS APIs TEVE SEU CADASTRO COMO "DU QUEIROZ" (SEU APELIDO),
#NESTE CONTEXTO, FAREI A CORREÇÃO MANUAL DE ALGUNS JOGADORES QUE CONSIDERO IMPORTANTES 
#E QUE NÃO QUERO DEIXAR DE LEVAR EM CONSIDERAÇÃO NA ANÁLISE, POIS SÃO RELATIVAMENTE IMPORTANTES NO CENÁRIO DO FUTEBOL BRASILEIRO. 

#CORRIGINDO NOMENCLATURA DESTES ATLETAS:
library(stringr)
df5$concatenado <- str_replace(df5$concatenado,"RONI _ Palmeiras","RONY _ Palmeiras")
df5$concatenado <- str_replace(df5$concatenado,"MURILO CERQUEIRA _ Palmeiras","MURILO _ Palmeiras")
df5$concatenado <- str_replace(df5$concatenado,"JOSE LOPEZ _ Palmeiras","JOSE MANUEL LOPEZ _ Palmeiras")
df5$concatenado <- str_replace(df5$concatenado,"LUAN GARCIA _ Palmeiras","LUAN _ Palmeiras")
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
#df5$concatenado <- str_replace(df5$concatenado,"PEDRO ROCHA _ Atlético Paranaense","PEDRO ROCHA _ Fortaleza EC")
df5$concatenado <- str_replace(df5$concatenado,"RODRIGUINHO _ América Mineiro","RODRIGUINHO _ Cuiabá")
#df5$concatenado <- str_replace(df5$concatenado,"JO _ Corinthians","JO _ Ceará")


#VALE LEMBRAR QUE AS APIs SÃO ATUALIZADAS TODAS AS RODADAS, COM ISSO A CADA NOVA REQUISIÇÃO DE CONSULTA, HAVERÁ VARIAÇÕES NOS DADOS
#COMO POR EXEMPLO JOGADORES QUE SE DESVINCULARAM CONTRATUALMENTE DE SEUS TIMES OU FORAM COMPRADOS/VENDIDOS NO MERCADO DE TRANSFERÊNCIAS INTERNACIONAL,
#ASSIM NÃO HAVERÁ UM TRATAMENTO UNIVERSAL QUE RESOLVA TODOS OS CASOS ESPECÍFICOS DA BASE.


#UNINDO BASES
df5 <- left_join(df5, Brazilian_teams, by="concatenado")
#Checando quantidade de linhas da base
nrow(df5)
#Porcentagem de valores NAS em cada coluna
NAS <- round(colSums(is.na(df5))*100/nrow(df5), 2)
#VER TODOS NAS
NAS

#45,32% DE FALTANTES
#27,42% DE FALTANTES - APOS MINERACAO
#diferenca: 17,9%

#COM O USO DESTAS TÉCNICAS CONSEGUIMOS RECUPERAR 113 ATLETAS (17% dos atletas da base) QUE NÃO SERIAM LEVADOS EM CONSIDERAÇÃO NO MODELO CASO NÃO TIVESSEMOS REALIZADO
#ESTE TRATAMENTO TEXTUAL.
#EM CASO DE RÉPLICA DESTE TRABALHO VOCÊ PODE AVALIAR E INCLUIR A ALTERAÇÃO DE OUTROS ATLETAS PARA QUE O MODELO OS LEVE EM CONSIDERAÇÃO TAMBÉM,
#APÓS OS TRATAMENTOS, OS DEMAIS ATLETAS QUE NÃO FORAM ENCONTRADOS EM AMBAS AS BASES SE DEVE AO FATO DE NÃO SEREM MAIS ATUANTES DO FUTEBOL BRASILEIRO,
#A IMENSA MAIORIA DESTES FORAM VENDIDOS PARA O FUTEBOL INTERNACIONAL OU PARA OUTRAS LIGAS DO FUTEBOL BRASILEIRO, POR ISSO ESTES NÃO SERÃO RELEVANTES PARA O ESTUDO
#E SERÃO EXCLUSOS DA BASE.

#Removendo valores NAs
df5<- na.omit(df5)
#Chencando numero de linhas restantes
nrow(df5)

#SEGUIMOS COM 457 JOGADORES





#####################
#df5 <- df5 %>% select(1,28,29,32:35,37,38,30:31,39:50,36,2:27)
Zone <- function(x) {
  if(x == "Right Winger") {return("Attacker")}
  if(x == "Left Winger") {return("Attacker")}
  if(x == "Centre-Forward") {return("Attacker")}
  if(x == "Second Striker") {return("Attacker")}
  if(x == "Attacking Midfield") {return("Midfield")}
  if(x == "Central Midfield") {return("Midfield")}
  if(x == "Defensive Midfield") {return("Midfield")}
  if(x == "Left Midfield") {return("Midfield")}
  if(x == "Right Midfield") {return("Midfield")}  
  if(x == "Right-Back") {return("Defender")}  
  if(x == "Left-Back") {return("Defender")}  
  if(x == "Centre-Back") {return("Defender")}  
}

df5$Zone <- ifelse(df5$player_pos ==  "Right Winger" | df5$player_pos == "Left Winger" | df5$player_pos == "Centre-Forward" | df5$player_pos == "Second Striker", "Attacker",
                   ifelse(df5$player_pos ==  "Attacking Midfield" | df5$player_pos == "Central Midfield" | df5$player_pos == "Defensive Midfield" | df5$player_pos == "Left Midfield" | df5$player_pos == "Right Midfield", "Midfield",
                          ifelse(df5$player_pos ==  "Right-Back" | df5$player_pos == "Left-Back" | df5$player_pos == "Centre-Back", "Defender",
                                 "Goalkeeper")))


df5 <- df5 %>% select(1,28,29,32:35,38,36,39,40,30:31,37,2:27)


#GOLEIROS ESTAO SENDO REPRESENTADOS COMO CATEGORIAS DE CONTROLE, ONDE É A UNICA POSICAO CLASSIFICADA COM 0 EM TODAS AS VARIAVEIS DUMMIES
df5$Dummie_position <- df5$player_pos
#TRANSFORMANDO DUMMIES
df5$Dummie <- 1
df5 <- df5 %>% pivot_wider(names_from = Dummie_position, values_from = Dummie)
df5<- df5 %>%
 mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
df5$Goalkeeper <- NULL

#CONSIDERANDO JOGADORES QUE ATUAM EM MEDIA MAIS DE 45 MIN COMO TITULARES
df5$AVG_time <- round(df5$minutes_played / df5$matches_played,0)
df5$starting_player <- ifelse(df5$AVG_time > 45 ,1,0)
#GOLEIROS ESTAO SENDO REPRESENTADOS COMO CATEGORIAS DE CONTROLE AO DEFINIR AS ZONAS DE POSICAO ABAIXO, ONDE SÃO A UNICA ZONA CLASSIFICADA COM 0 EM TODAS AS VARIAVEIS DUMMIES
df5$Attacking <- ifelse(df5$`Right Winger` == 1 | df5$`Left Winger`  == 1 | df5$`Centre-Forward` == 1 | df5$`Second Striker` == 1, 1, 0)
df5$Midfield <- ifelse(df5$`Attacking Midfield` == 1 | df5$`Central Midfield` == 1 | df5$`Defensive Midfield` == 1 | df5$`Left Midfield` == 1, 1, 0)
df5$Defender <- ifelse(df5$`Right-Back` == 1 | df5$`Left-Back` == 1 | df5$`Centre-Back` == 1 , 1, 0)


#removendo caso especial richard ceara, jogador com exatamente o mesmo nome no mesmo time
df5$Exclude <- ifelse(df5$particiant_id == 934515 & df5$player_pos == 'Defensive Midfield'  | df5$particiant_id == 882625 & df5$player_pos == 'Goalkeeper',1,0)
df5 <- filter(df5, Exclude != 1)
df5$Exclude <- NULL

df5$Matches_90min_played <- round(df5$minutes_played/90,2)
df5$Big_chances_created_per_match <- round(df5$`Big chances created` / df5$matches_played,2)
df5$Chances_created_per_match <- round(df5$`Chances created` / df5$matches_played,2)
df5$Big_chances_missed_per_match <- round(df5$`Big chances missed` / df5$matches_played, 2)
df5$Goals_Assists_per_match <- round(df5$`Goals + Assists` / df5$matches_played,2)
df5$Successful_dribbles <- round(df5$Matches_90min_played * df5$`Successful dribbles per 90`)
df5$Disarms <- round(df5$Matches_90min_played * df5$`Successful tackles per 90`)
df5$Shots <- round(df5$Matches_90min_played * df5$`Shots per 90`)
df5$Shots_on_target <- round(df5$Matches_90min_played * df5$`Shots on target per 90`)
df5$Blocks <- round(df5$Matches_90min_played * df5$`Blocks per 90`)
df5$Fouls_committed <- round(df5$Matches_90min_played * df5$`Fouls committed per 90`)
df5$Goals_conceded <- round(df5$Matches_90min_played * df5$`Goals conceded per 90`)
df5$Clearances <- round(df5$Matches_90min_played * df5$`Clearances per 90`)
df5$Interceptions <- round(df5$Matches_90min_played * df5$`Interceptions per 90`)
df5$Possession_won_final_3rd <- round(df5$Matches_90min_played * df5$`Possession won final 3rd per 90`)
df5$Saves <- round(df5$Matches_90min_played * df5$`Saves per 90`)
df5$Accurate_long_balls <- round(df5$Matches_90min_played * df5$`Accurate long balls per 90`)
df5$Accurate_passes <- round(df5$Matches_90min_played * df5$`Accurate passes per 90`)

#df5$Saves_sub_GoalsConceded <- round(df5$Saves - df5$Goals_conceded)
#df5$Perc_Clean_Sheets <- round(df5$`Clean sheets` / df5$matches_played)





#df5 <- df5 %>% select(-c(30,32,36,37,68,72))
#df5$`Goals conceded per 90` <- NULL


BDclusters <- df5





#install.packages('caret')
library(caret)

#NORMALIZANDO DADOS E ARMAZENANDO NA VARIAVEL "DADOS"
#dados<- scale(BDclusters[,c(6:52)])

#REALIZANDO PREDICAO COM ALGORITMO DE CLUSTERIZACAO E UTILIZANDO METODO DE NORMALIZACAO "SCALE"    
BDclusters <- predict(preProcess(BDclusters[,12:74], method ="scale") ,BDclusters)
#COMANDO PARA GARANTIR QUE O LEITOR CHEGUE AO MESMO RESULTADO
set.seed(1)

#SEPARANDO DIFERENTES TIPOS DE CLUSTERS
G2 <- kmeans(BDclusters[12:74], centers=2)
G3 <- kmeans(BDclusters[12:74], centers=3)
G4 <- kmeans(BDclusters[12:74], centers=4)
G5 <- kmeans(BDclusters[12:74], centers=5)
G6 <- kmeans(BDclusters[12:74], centers=6)
G7 <- kmeans(BDclusters[12:74], centers=7)
G8 <- kmeans(BDclusters[12:74], centers=8)
G9 <- kmeans(BDclusters[12:74], centers=9)
G10 <- kmeans(BDclusters[12:74], centers=10)
G11 <- kmeans(BDclusters[12:74], centers=11)

library(factoextra) #fviz_cluster function
viz_G2 <- fviz_cluster(G2, data = BDclusters[,c(12:74)],palette = c("#2E9FDF", "#00AFBB"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 1")
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(12:74)],palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 2")
viz_G4 <- fviz_cluster(G4, data = BDclusters[,c(12:74)],palette = c("#E7B800", "#FF00FF","#00FFFF", "#FF1493"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 3")
viz_G5 <- fviz_cluster(G5, data = BDclusters[,c(12:74)],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 4")
viz_G6 <- fviz_cluster(G6, data = BDclusters[12:74],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 5")
viz_G7 <- fviz_cluster(G7, data = BDclusters[12:74],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 6")
viz_G8 <- fviz_cluster(G8, data = BDclusters[12:74],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 7")
viz_G9 <- fviz_cluster(G9, data = BDclusters[12:74],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 8")
viz_G10 <- fviz_cluster(G10, data = BDclusters[12:74],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585", "#C71585" ), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 9")
viz_G11 <- fviz_cluster(G11, data = BDclusters[12:74],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585", "#C71585", "#C71585" ), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 10")



library(gridExtra)
grid.arrange(viz_G2,viz_G3, viz_G4, viz_G5,viz_G6, viz_G7, viz_G8, viz_G9, viz_G10, viz_G11, nrow=2, ncol=5)


#viz_G4 <- fviz_cluster(G4, data = BDclusters[,c(12:74)],palette = c("#000000","#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_classic())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(12:74)],palette = c("#000000","#000000", "#DC143C"), geom = "point", ellipse.type = "convex", ggtheme = theme_classic(), main = "3 Grupos")

#grid.arrange(viz_G4, nrow=1, ncol=1)
grid.arrange(viz_G3, nrow=1, ncol=1)

#BDclusters$G4 <- G4$cluster
df5$Cluster <- G3$cluster


#Identificando goleiros
df5$G9 <- G9$cluster
teste <- filter(df5, G9==3)
df5$G9 <- NULL

Goalkeepers <- filter(df5, player_pos == "Goalkeeper")
df5 <- filter(df5, player_pos != "Goalkeeper")
Goalkeepers$Cluster <- 4

backup<- df5

df5<- rbind(df5,Goalkeepers)


library(ggplot2)
par(mfrow=c(2,4))

#comparacao dos modelos por variavel especifica - Gols
#boxplot(BDclusters$player_market_value_euro ~ BDclusters$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Valor de Mercado")
boxplot(df5$matches_played ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Partidas jogadas", xlab = "", ylab = "")
boxplot(df5$minutes_played ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Minutos jogados", xlab = "", ylab = "")
boxplot(df5$player_age ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Idade", xlab = "", ylab = "")
boxplot(df5$`Chances created` ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Chances criadas", xlab = "", ylab = "")
boxplot(df5$`Big chances created` ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Grandes chances criadas", xlab = "", ylab = "")
boxplot(df5$`Big chances missed` ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Grandes chances perdidas", xlab = "", ylab = "")
boxplot(df5$Assists ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Assistências", xlab = "", ylab = "")
boxplot(df5$`Top scorer` ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Gols", xlab = "", ylab = "")

boxplot(df5$Successful_dribbles ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Dribles", xlab = "", ylab = "")
boxplot(df5$Disarms ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Desarmes", xlab = "", ylab = "")
boxplot(df5$Shots ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Chutes", xlab = "", ylab = "")
boxplot(df5$Shots_on_target ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Chutes no gol", xlab = "", ylab = "")
boxplot(df5$Blocks ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Bloqueios", xlab = "", ylab = "")
boxplot(df5$Fouls_committed ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Faltas cometidas", xlab = "", ylab = "")
boxplot(df5$Goals_conceded ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Gols concedidos", xlab = "", ylab = "")
boxplot(df5$Interceptions ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Interceptações", xlab = "", ylab = "")

boxplot(df5$Possession_won_final_3rd ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Posse ganha no campo final", xlab = "", ylab = "")
boxplot(df5$Clearances ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Clearances", xlab = "", ylab = "")
boxplot(df5$Saves ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Defesas", xlab = "", ylab = "")
boxplot(df5$Accurate_long_balls ~ df5$Cluster, col= c("#DC143C","#00bfff", "#66cdaa", "#d3d3d3"), main="Precisão de passes", xlab = "", ylab = "")

df5_Zone_Freq <- df5 %>% count(Zone, player_pos, Cluster,  sort= TRUE)

library(lattice)
barchart(Zone ~ n | Cluster, data = df5_Zone_Freq, groups= Zone, stack = T)

Group1 <- filter(df5, Cluster ==1)
prop.table(table(Group1$Zone))

Group2 <- filter(df5, Cluster ==2)
prop.table(table(Group2$Zone))

Group3 <- filter(df5, Cluster ==3)
prop.table(table(Group3$Zone))

Group4 <- filter(df5, Cluster ==4)
prop.table(table(Group4$Zone))


df5 %>% group_by(Cluster) %>% summarise_at(vars(player_age), list(Age_mean = mean))



#TERMINOU AQUI SAVADO 23/10





###################### outros, valor de mercado


#AQUI VOU TRAZER AS INFORMACOES DE VALOR DE MERCADO
#Transfermarkt
#Valores de mercado
df1 <- tm_player_market_values(country_name = "Brazil", start_year = 2021)
backup <- df1
df1 <- backup

#tratando nomes dos jogadores da base de valores
df1$player_name <- apply(df1[,7,drop=F],2, removePunctuation)
#Convertendo tipo de coluna para caractere
df1$player_name<- as.character(df1$player_name)
#Convertendo nomes para letras maiusculas
df1$player_name <- apply(df1[,7,drop=F],2, toupper)
#Removendo acentuacoes dos nomes
df1$player_name <- stri_trans_general(df1$player_name,id="Latin-ASCII")
df3 <- Brazilian_teams %>% select(2,1)
#df3$concatenado <- paste(df3$player_name,"_",df3$team_name_transfermarkt)
#df3$player_name <- NULL
df1 <- left_join(df1, df3,  by="player_name")

df1$concatenado <- paste(df1$player_name,"_",df1$team_name_transfermarkt)
df1 <- df1 %>% select(21,13,14,18)


back <- df5
#df5 <- back

df5$concatenado <- paste(df5$player_name,"_",df5$team_name_transfermarkt)
df5 <- left_join(df5, df1, by="concatenado")

glimpse(df5)


teste <- df5 %>% select(1:5,55:60)


df5$player_height_mtrs <- as.double(df5$player_height_mtrs)



unique(df5$particiant_id)

466
nrow(df5)





df5<- df5 %>% select(1:4,8,10:54)

df5$ValorDeMercado <- round(df5$player_market_value_euro/1000000, 2) 



BDclusters <- df5 %>% select(1:46,48,53,55:57,52,49:51)

############################
#   NOVO CLUSTER  #
########################

library(caret)
#REALIZANDO PREDICAO COM ALGORITMO DE CLUSTERIZACAO E UTILIZANDO METODO DE NORMALIZACAO "SCALE"    
BDclusters <- predict(preProcess(BDclusters[,6:52], method ="scale") ,BDclusters)
#COMANDO PARA GARANTIR QUE O LEITOR CHEGUE AO MESMO RESULTADO
set.seed(1)

#SEPARANDO DIFERENTES TIPOS DE CLUSTERS
G2 <- kmeans(BDclusters[6:52], centers=2)
G3 <- kmeans(BDclusters[6:52], centers=3)
G4 <- kmeans(BDclusters[6:52], centers=4)
G5 <- kmeans(BDclusters[6:52], centers=5)
G6 <- kmeans(BDclusters[6:52], centers=6)
G7 <- kmeans(BDclusters[6:52], centers=7)
G8 <- kmeans(BDclusters[6:52], centers=8)
G9 <- kmeans(BDclusters[6:52], centers=9)
G10 <- kmeans(BDclusters[6:52], centers=10)
G11 <- kmeans(BDclusters[6:52], centers=11)

library(factoextra) #fviz_cluster function
viz_G2 <- fviz_cluster(G2, data = BDclusters[,c(6:52)],palette = c("#2E9FDF", "#00AFBB"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw(), main = "2 Grupos")
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(6:52)],palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw(), main ="3 Grupos")
viz_G4 <- fviz_cluster(G4, data = BDclusters[,c(6:52)],palette = c("#E7B800", "#FF00FF","#00FFFF", "#FF1493"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G5 <- fviz_cluster(G5, data = BDclusters[,c(6:52)],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G6 <- fviz_cluster(G6, data = BDclusters[6:52],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G7 <- fviz_cluster(G7, data = BDclusters[6:52],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G8 <- fviz_cluster(G8, data = BDclusters[6:52],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G9 <- fviz_cluster(G9, data = BDclusters[6:52],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G10 <- fviz_cluster(G10, data = BDclusters[6:52],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585", "#C71585" ), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G11 <- fviz_cluster(G11, data = BDclusters[6:52],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585", "#C71585", "#C71585" ), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())


?fviz_cluster()


library(gridExtra)
grid.arrange(viz_G2,viz_G3, viz_G4, viz_G5,viz_G6, viz_G7, viz_G8, viz_G9, viz_G10, viz_G11, nrow=2, ncol=5)





#################volta da analise
df5$ValorDeMercado <- round(df5$player_market_value_euro/1000000, 2) 





#Relacoes de variaveis
df_ValXalt <- df5 %>% select(2,48,49,20,51)
df_ValXalt <- na.omit(df_ValXalt)
plot(df_ValXalt$player_height_mtrs, df_ValXalt$ValorDeMercado)
plot(df5$player_age, df5$player_market_value_euro)

#incluir posicao

library(lattice)
xyplot(data = df_ValXalt, groups = player_foot , player_height_mtrs ~ ValorDeMercado)
#jogadores canhotos cusam mais? tracar media de valor dos canhotos e destros
xyplot(data = df_ValXalt, groups = player_foot , player_age ~ ValorDeMercado )

stripplot(data = df_ValXalt, player_height_mtrs ~ ValorDeMercado )


?xyplot


library(ggplot2)
par(mfrow=c(2,4))

#comparacao dos modelos por variavel especifica - Gols
#boxplot(BDclusters$player_market_value_euro ~ BDclusters$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Valor de Mercado")
boxplot(df5$matches_played ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Partidas jogadas", xlab = "", ylab = "")
boxplot(df5$minutes_played ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Minutos jogados", xlab = "", ylab = "")
boxplot(df5$player_age ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Idade", xlab = "", ylab = "")
boxplot(df5$`Chances created` ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Chances criadas", xlab = "", ylab = "")
boxplot(df5$`Big chances created` ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Grandes chances criadas", xlab = "", ylab = "")
boxplot(df5$`Big chances missed` ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Grandes chances perdidas", xlab = "", ylab = "")
boxplot(df5$Assists ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Assistências", xlab = "", ylab = "")
boxplot(df5$`Goals per 90` ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Gols", xlab = "", ylab = "")



?boxplot()

#CLUSTERS
G3$cluster
#CENTROS DOS CLUSTERS
G3$centers
#TAMANHO DOS CLUSTERS
G3$size

#Proporcao da quantidade de individuos por grupo
round(table(BDclusters$G3)*100/nrow(BDclusters),2)
table(BDclusters$G4)

#Média de Gols por grupos de clusters
BDclusters %>% group_by(G3) %>% summarise_at(vars(`Goals per 90`), list(Media = mean))



write.csv(BDclusters, "C:/Users/thiag/Desktop/BDclusters.csv")
write.xlsx(df5, sheetName = "df5", file =  "C:/Users/thiag/Desktop/df5.xlsx")

?write.xlsx
