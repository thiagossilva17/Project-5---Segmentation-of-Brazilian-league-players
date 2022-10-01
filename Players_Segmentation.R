library(worldfootballR)
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(stringi)


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


#TRANSFORMANDO DUMMIES
df5$Dummie <- 1
df5 <- df5 %>% pivot_wider(names_from = player_pos, values_from = Dummie)
df5<- df5 %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))



#####################
df5 <- df5 %>% select(1,28,29,32:35,37,38,30:31,39:50,36,2:27)

BDclusters <- df5

#install.packages('caret')
library(caret)

#NORMALIZANDO DADOS E ARMAZENANDO NA VARIAVEL "DADOS"
dados<- scale(BDclusters[,c(10:50)])

#REALIZANDO PREDICAO COM ALGORITMO DE CLUSTERIZACAO E UTILIZANDO METODO DE NORMALIZACAO "SCALE"    
BDclusters <- predict(preProcess(BDclusters[,10:50], method ="scale") ,BDclusters)
#COMANDO PARA GARANTIR QUE O LEITOR CHEGUE AO MESMO RESULTADO
set.seed(1)

#SEPARANDO DIFERENTES TIPOS DE CLUSTERS
G2 <- kmeans(BDclusters[10:50], centers=2)
G3 <- kmeans(BDclusters[10:50], centers=3)
G4 <- kmeans(BDclusters[10:50], centers=4)
G5 <- kmeans(BDclusters[10:50], centers=5)
G6 <- kmeans(BDclusters[10:50], centers=6)
G7 <- kmeans(BDclusters[10:50], centers=7)
G8 <- kmeans(BDclusters[10:50], centers=8)
G9 <- kmeans(BDclusters[10:50], centers=9)
G10 <- kmeans(BDclusters[10:50], centers=10)
G11 <- kmeans(BDclusters[10:50], centers=11)

library(factoextra) #fviz_cluster function
viz_G2 <- fviz_cluster(G2, data = BDclusters[,c(10:50)],palette = c("#2E9FDF", "#00AFBB"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(10:50)],palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G4 <- fviz_cluster(G4, data = BDclusters[,c(10:50)],palette = c("#E7B800", "#FF00FF","#00FFFF", "#FF1493"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G5 <- fviz_cluster(G5, data = BDclusters[,c(10:50)],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G6 <- fviz_cluster(G6, data = BDclusters[10:50],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G7 <- fviz_cluster(G7, data = BDclusters[10:50],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G8 <- fviz_cluster(G8, data = BDclusters[10:50],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G9 <- fviz_cluster(G9, data = BDclusters[10:50],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585"), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G10 <- fviz_cluster(G10, data = BDclusters[10:50],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585", "#C71585" ), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
viz_G11 <- fviz_cluster(G11, data = BDclusters[10:50],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585", "#C71585", "#C71585" ), geom = "point", ellipse.type = "convex", ggtheme = theme_bw())



library(gridExtra)
grid.arrange(viz_G2,viz_G3, viz_G4, viz_G5,viz_G6, viz_G7, viz_G8, viz_G9, viz_G10, viz_G11, nrow=2, ncol=5)


viz_G4 <- fviz_cluster(G4, data = BDclusters[,c(10:50)],palette = c("#000000","#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_classic())


grid.arrange(viz_G4, nrow=1, ncol=1)

#BDclusters$G4 <- G4$cluster
df5$G4 <- G4$cluster


#AQUI VOU TRAZER AS INFORMACOES DE VALOR DE MERCADO
#Transfermarkt
#Valores de mercado
df1 <- tm_player_market_values(country_name = "Brazil", start_year = 2021)
backup <- df1

#tratando nomes dos jogadores da base de valores
df1$player_name <- apply(df1[,7,drop=F],2, removePunctuation)
#Convertendo tipo de coluna para caractere
df1$player_name<- as.character(df1$player_name)
#Convertendo nomes para letras maiusculas
df1$player_name <- apply(df1[,7,drop=F],2, toupper)
#Removendo acentuacoes dos nomes
df1$player_name <- stri_trans_general(df1$player_name,id="Latin-ASCII")
df1$concatenado <- paste(df1$player_name,"_",df1$squad)
df1 <- df1 %>% select(20,13,14,18)


#back <- df5
df5 <- back

df5$concatenado <- paste(df5$player_name,"_",df5$team_name_transfermarkt)
df5 <- left_join(df5, df1, by="concatenado")
df5$player_height_mtrs <- as.double(df5$player_height_mtrs)
df5<- df5 %>% select(1:4,8,10:54)


#Relacoes de variaveis
df_ValXalt <- df5 %>% select(2,48,50)
df_ValXalt <- na.omit(df_ValXalt)
plot(df_ValXalt$player_height_mtrs, df_ValXalt$player_market_value_euro)
plot(df5$player_age, df5$player_market_value_euro)

library(ggplot2)
par(mfrow=c(2,4))

#comparacao dos modelos por variavel especifica - Gols
#boxplot(BDclusters$player_market_value_euro ~ BDclusters$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Valor de Mercado")
boxplot(df5$matches_played ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Idade")
boxplot(df5$minutes_played ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Idade")
boxplot(df5$player_age ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Idade")
boxplot(df5$`Chances created` ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Idade")
boxplot(df5$`Big chances created` ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Idade")
boxplot(df5$`Big chances missed` ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Idade")
boxplot(df5$Assists ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Idade")
boxplot(df5$`Goals per 90` ~ df5$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Idade")

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



