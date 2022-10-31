library(worldfootballR)
library(dplyr)
library(tidyr)
library(stringr)
library(tm)
library(stringi)
library(xlsx)
library(ggplot2)
library(stringr)
library(caret)
library(factoextra)
library(gridExtra)


##### INICIO - EXTRAÇÃO DE DADOS DE DESEMPENHO DA API FUTMOB E REMODELAGEM DE DADOS
# HOME - EXTRACTION OF PERFORMANCE DATA FROM THE FOTMOB API AND DATA REMODELING

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
#nrow(df_desc)
#unique(df_desc$particiant_id)
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
df5$Sum_yellow_red_cards <- df5$`Yellow cards` + df5$`Red cards`
df5$Sum_Disarms_Blocks_Intercep <- df5$Disarms + df5$Blocks + df5$Interceptions
df5$Big_chances_created_per_assists <- df5$`Big chances created` / df5$Assists
df5$Shots_on_target_per_goals <- df5$Shots_on_target / df5$`Top scorer`


df5$Minutes_for_Goals <- round(df5$minutes_played / df5$`Top scorer`)
df5$Minutes_for_Assists <- round(df5$minutes_played / df5$Assists)
df5$Minutes_for_Goals_Assists <- round(df5$minutes_played / df5$`Goals + Assists`)
df5$Minutes_for_Big_chances_created <- round(df5$minutes_played / df5$`Big chances created`)
df5$Minutes_for_Big_chances_missed <- round(df5$minutes_played / df5$`Big chances missed`)
df5$Minutes_for_Disarms <- round(df5$minutes_played / df5$Disarms)
df5$Minutes_for_Blocks <- round(df5$minutes_played / df5$Blocks)
df5$Minutes_for_Interceptions <- round(df5$minutes_played / df5$Interceptions)
df5$Minutes_for_Saves <- round(df5$minutes_played / df5$Saves)
df5$Minutes_for_YellowCards <- round(df5$minutes_played / df5$`Yellow cards`)
df5$Minutes_for_RedCards <- round(df5$minutes_played / df5$`Red cards`)





#tratando divisores por zero
df5$Minutes_for_Goals <- ifelse(df5$Minutes_for_Goals == "Inf" | df5$Minutes_for_Goals == "NaN" , df5$minutes_played, df5$Minutes_for_Goals)
df5$Minutes_for_Assists <- ifelse(df5$Minutes_for_Assists == "Inf" | df5$Minutes_for_Assists == "NaN", df5$minutes_played, df5$Minutes_for_Assists)
df5$Minutes_for_Goals_Assists <- ifelse(df5$Minutes_for_Goals_Assists == "Inf" | df5$Minutes_for_Goals_Assists == "NaN", df5$minutes_played, df5$Minutes_for_Goals_Assists)
df5$Minutes_for_Big_chances_created <- ifelse(df5$Minutes_for_Big_chances_created == "Inf" | df5$Minutes_for_Big_chances_created == "NaN", df5$minutes_played, df5$Minutes_for_Big_chances_created)
df5$Minutes_for_Big_chances_missed <- ifelse(df5$Minutes_for_Big_chances_missed == "Inf" | df5$Minutes_for_Big_chances_missed == "NaN", df5$minutes_played, df5$Minutes_for_Big_chances_missed)
df5$Minutes_for_Disarms <- ifelse(df5$Minutes_for_Disarms == "Inf" | df5$Minutes_for_Disarms == "NaN", df5$minutes_played, df5$Minutes_for_Disarms)
df5$Minutes_for_Blocks <- ifelse(df5$Minutes_for_Blocks == "Inf" | df5$Minutes_for_Blocks == "NaN", df5$minutes_played, df5$Minutes_for_Blocks)
df5$Minutes_for_Interceptions <- ifelse(df5$Minutes_for_Interceptions == "Inf" | df5$Minutes_for_Interceptions == "NaN", df5$minutes_played, df5$Minutes_for_Interceptions)
df5$Minutes_for_Saves <- ifelse(df5$Minutes_for_Saves == "Inf" | df5$Minutes_for_Saves == "NaN", df5$minutes_played, df5$Minutes_for_Saves)
df5$Minutes_for_YellowCards <- ifelse(df5$Minutes_for_YellowCards == "Inf" | df5$Minutes_for_YellowCards == "NaN", df5$minutes_played, df5$Minutes_for_YellowCards)
df5$Minutes_for_RedCards <- ifelse(df5$Minutes_for_RedCards == "Inf" | df5$Minutes_for_RedCards == "NaN", df5$minutes_played, df5$Minutes_for_RedCards)
df5$Big_chances_created_per_assists <- ifelse(df5$Big_chances_created_per_assists == "Inf" | df5$Big_chances_created_per_assists == "NaN", df5$`Big chances created`, df5$Big_chances_created_per_assists)
df5$Shots_on_target_per_goals <- ifelse(df5$Shots_on_target_per_goals == "Inf" | df5$Shots_on_target_per_goals == "NaN", df5$Shots_on_target, df5$Shots_on_target_per_goals)



df5$Age23 <- ifelse(df5$player_age <= 23, 1,0)
df5$smaller_Age30 <- ifelse(df5$player_age > 23 & df5$player_age < 30 , 1,0)
df5$larger_Age30 <- ifelse(df5$player_age >= 30, 1,0)
df5$None_Goals_Assists <- ifelse(df5$`Goals + Assists` == 0, 1,0)


#df5$Saves_sub_GoalsConceded <- round(df5$Saves - df5$Goals_conceded)
#df5$Perc_Clean_Sheets <- round(df5$`Clean sheets` / df5$matches_played)





#df5 <- df5 %>% select(-c(30,32,36,37,68,72))
#df5$`Goals conceded per 90` <- NULL


BDclusters <- df5


?preProcess


#install.packages('caret')


#NORMALIZANDO DADOS E ARMAZENANDO NA VARIAVEL "DADOS"
#dados<- scale(BDclusters[,c(6:52)])

#REALIZANDO PREDICAO COM ALGORITMO DE CLUSTERIZACAO E UTILIZANDO METODO DE NORMALIZACAO "SCALE"    
BDclusters <- predict(preProcess(BDclusters[,12:94], method ="scale") ,BDclusters)
#COMANDO PARA GARANTIR QUE O LEITOR CHEGUE AO MESMO RESULTADO
set.seed(1)

#SEPARANDO DIFERENTES TIPOS DE CLUSTERS
G2 <- kmeans(BDclusters[12:94], centers=2)
G3 <- kmeans(BDclusters[12:94], centers=3)
G4 <- kmeans(BDclusters[12:94], centers=4)
G5 <- kmeans(BDclusters[12:94], centers=5)
G6 <- kmeans(BDclusters[12:94], centers=6)
G7 <- kmeans(BDclusters[12:94], centers=7)
G8 <- kmeans(BDclusters[12:94], centers=8)
G9 <- kmeans(BDclusters[12:94], centers=9)
G10 <- kmeans(BDclusters[12:94], centers=10)
G11 <- kmeans(BDclusters[12:94], centers=11)

 #fviz_cluster function
viz_G2 <- fviz_cluster(G2, data = BDclusters[,c(12:94)],palette = c("#2E9FDF", "#00AFBB"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 1")
viz_G3 <- fviz_cluster(G3, data = BDclusters[,c(12:94)],palette = c("#2E9FDF", "#00AFBB", "#E7B800"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 2")
viz_G4 <- fviz_cluster(G4, data = BDclusters[,c(12:94)],palette = c("#E7B800", "#FF00FF","#00FFFF", "#FF1493"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 3")
viz_G5 <- fviz_cluster(G5, data = BDclusters[,c(12:94)],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 4")
viz_G6 <- fviz_cluster(G6, data = BDclusters[12:94],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 5")
viz_G7 <- fviz_cluster(G7, data = BDclusters[12:94],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 6")
viz_G8 <- fviz_cluster(G8, data = BDclusters[12:94],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 7")
viz_G9 <- fviz_cluster(G9, data = BDclusters[12:94],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 8")
viz_G10 <- fviz_cluster(G10, data = BDclusters[12:94],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585", "#C71585" ), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 9")
viz_G11 <- fviz_cluster(G11, data = BDclusters[12:94],palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF00FF", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585", "#C71585", "#C71585" ), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 10")



grid.arrange(viz_G2,viz_G3, viz_G4, viz_G5,viz_G6, viz_G7, viz_G8, viz_G9, viz_G10, viz_G11, nrow=2, ncol=5)


#viz_G4 <- fviz_cluster(G4, data = BDclusters[,c(12:94)],palette = c("#000000","#000000", "#DC143C","#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_classic())
viz_G4 <- fviz_cluster(G4, data = BDclusters[,c(12:94)],palette = c("#DC143C","#000000","#000000", "#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_classic(), main = "3 Grupos")

viz_G4 <- fviz_cluster(G4, data = BDclusters[,c(12:94)],palette = c("#000000","#DC143C","#000000", "#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_classic(), main = "3 Grupos")

#grid.arrange(viz_G4, nrow=1, ncol=1)
grid.arrange(viz_G4, nrow=1, ncol=1)

#BDclusters$G4 <- G4$cluster
df5$Cluster <- G4$cluster


# #Identificando goleiros
# df5$G9 <- G9$cluster
# teste <- filter(df5, G9==3)
# df5$G9 <- NULL
# 
# Goalkeepers <- filter(df5, player_pos == "Goalkeeper")
# df5 <- filter(df5, player_pos != "Goalkeeper")
# Goalkeepers$Cluster <- 4
# 
# backup<- df5
# 
# df5<- rbind(df5,Goalkeepers)



#comparacao dos modelos por variavel especifica - Gols
#boxplot(BDclusters$player_market_value_euro ~ BDclusters$G4, col= c("#000000","#000000", "#000000", "#DC143C"), main="Valor de Mercado")

#boxplot(df5$matches_played ~ df5$Cluster, col= c("#FB4B4B","#f3f3f3", "#f3f3f3", "#f3f3f3"), main="Matches played", xlab = "Cluster", ylab = "")
#outras cores c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3")


Group1 <- filter(df5, Cluster ==1)
prop.table(table(Group1$Zone))

Group2 <- filter(df5, Cluster ==2)
prop.table(table(Group2$Zone))

Group3 <- filter(df5, Cluster ==3)
prop.table(table(Group3$Zone))

Group4 <- filter(df5, Cluster ==4)
prop.table(table(Group4$Zone))

################## Part 2

df5 %>% group_by(Cluster) %>% summarise_at(vars(player_age), list(Age_mean = mean))

EDA_GoalsAssists <- df5 %>% group_by(Cluster) %>% summarise_at(vars(`Goals + Assists`), list(Sum_Goals_and_Assists = sum))
EDA_GoalsAssists$Percent <- round((EDA_GoalsAssists$Sum_Goals_and_Assists / sum(EDA_GoalsAssists$Sum_Goals_and_Assists))*100,2)
EDA_GoalsAssists

EDA_Goals <- df5 %>% group_by(Cluster) %>% summarise_at(vars(`Top scorer`), list(Goals = sum))
EDA_Goals$Percent <- round((EDA_Goals$Goals / sum(EDA_Goals$Goals))*100,2)
EDA_Goals

EDA_Assists <- df5 %>% group_by(Cluster) %>% summarise_at(vars(Assists), list(Assists = sum))
EDA_Assists$Percent <- round((EDA_Assists$Assists / sum(EDA_Assists$Assists))*100,2)
EDA_Assists

EDA_Assists <- df5 %>% group_by(Cluster) %>% summarise_at(vars(Assists), list(Assists = sum))
EDA_Assists$Percent <- round((EDA_Assists$Assists / sum(EDA_Assists$Assists))*100,2)
EDA_Assists

EDA_BigChancesCreated <- df5 %>% group_by(Cluster) %>% summarise_at(vars(`Big chances created`), list(BigChancesCreated = sum))
EDA_BigChancesCreated$Percent <- round((EDA_BigChancesCreated$BigChancesCreated / sum(EDA_BigChancesCreated$BigChancesCreated))*100,2)
EDA_BigChancesCreated

EDA_Disarms_Blocks_Intercep <- df5 %>% group_by(Cluster) %>% summarise_at(vars(Sum_Disarms_Blocks_Intercep), list(Disarms_Blocks_Intercep = sum))
EDA_Disarms_Blocks_Intercep$Percent <- round((EDA_Disarms_Blocks_Intercep$Disarms_Blocks_Intercep / sum(EDA_Disarms_Blocks_Intercep$Disarms_Blocks_Intercep))*100,2)
EDA_Disarms_Blocks_Intercep

EDA_Accurate_passes <- df5 %>% group_by(Cluster) %>% summarise_at(vars(Accurate_passes), list(Accurate_passes = sum))
EDA_Accurate_passes$Percent <- round((EDA_Accurate_passes$Accurate_passes / sum(EDA_Accurate_passes$Accurate_passes))*100,2)
EDA_Accurate_passes

EDA_Accurate_long_balls <- df5 %>% group_by(Cluster) %>% summarise_at(vars(Accurate_long_balls), list(Accurate_long_balls = sum))
EDA_Accurate_long_balls$Percent <- round((EDA_Accurate_long_balls$Accurate_long_balls / sum(EDA_Accurate_long_balls$Accurate_long_balls))*100,2)
EDA_Accurate_long_balls



################### Part 3

par(mfrow=c(1,3))
boxplot(df5$matches_played ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Matches played", xlab = "Cluster", ylab = "")
boxplot(df5$minutes_played ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes played", xlab = "Cluster", ylab = "")
boxplot(df5$player_age ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Age", xlab = "Cluster", ylab = "")


par(mfrow=c(2,4))
boxplot(df5$`Chances created` ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Chances created", xlab = "Cluster", ylab = "")
boxplot(df5$`Big chances created` ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Big chances created", xlab = "Cluster", ylab = "")
boxplot(df5$`Big chances missed` ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Big chances missed", xlab = "Cluster", ylab = "")
boxplot(df5$Assists ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Assists", xlab = "Cluster", ylab = "")
boxplot(df5$`Top scorer` ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Goals", xlab = "Cluster", ylab = "")
boxplot(df5$Shots ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Shots", xlab = "Cluster", ylab = "")
boxplot(df5$Shots_on_target ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Shots on target", xlab = "Cluster", ylab = "")
boxplot(df5$Successful_dribbles ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Successful dribbles", xlab = "Cluster", ylab = "")


boxplot(df5$Accurate_passes ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Accurate passes", xlab = "Cluster", ylab = "")
boxplot(df5$Accurate_long_balls ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Accurate long balls", xlab = "Cluster", ylab = "")
boxplot(df5$Disarms ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Disarms", xlab = "Cluster", ylab = "")
boxplot(df5$Blocks ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Blocks", xlab = "Cluster", ylab = "")
boxplot(df5$Interceptions ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Interceptions", xlab = "Cluster", ylab = "")
boxplot(df5$Possession_won_final_3rd ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Possession won final 3rd", xlab = "Cluster", ylab = "")
boxplot(df5$Fouls_committed ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Fouls committed", xlab = "Cluster", ylab = "")
boxplot(df5$Sum_yellow_red_cards ~ df5$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Yellow/Red Cards", xlab = "Cluster", ylab = "")



#Para as visualizações de minutos esta sendo desconsiderado os jogadores que possuem a estatistica igual a zero.
DataMinutes_GoalsAssists <- df5 %>% select(1:4,9,11:14,Minutes_for_Goals_Assists, `Goals + Assists`, `Top scorer`,Assists, Cluster)
DataMinutes_GoalsAssists$Remove <- ifelse(DataMinutes_GoalsAssists$Minutes_for_Goals_Assists == DataMinutes_GoalsAssists$minutes_played,1,0)
DataMinutes_GoalsAssists <- filter(DataMinutes_GoalsAssists, Remove== 0)
DataMinutes_GoalsAssists <- DataMinutes_GoalsAssists[order(DataMinutes_GoalsAssists$Minutes_for_Goals_Assists, decreasing = F),]
DataMinutes_GoalsAssists$Remove <-NULL
DataMinutes_GoalsAssists$Position <- row.names(DataMinutes_GoalsAssists)
boxplot(DataMinutes_GoalsAssists$Minutes_for_Goals_Assists ~ DataMinutes_GoalsAssists$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Goals/Assists", xlab = "Cluster", ylab = "")

DataMinutes_Goals <- df5 %>% select(1:4,9,11:14,Minutes_for_Goals, `Top scorer`, Cluster)
DataMinutes_Goals$Remove <- ifelse(DataMinutes_Goals$Minutes_for_Goals == DataMinutes_Goals$minutes_played,1,0)
DataMinutes_Goals <- filter(DataMinutes_Goals, Remove== 0)
DataMinutes_Goals <- DataMinutes_Goals[order(DataMinutes_Goals$Minutes_for_Goals, decreasing = F),]
DataMinutes_Goals$Remove <-NULL
DataMinutes_Goals$Position <- row.names(DataMinutes_Goals)
boxplot(DataMinutes_Goals$Minutes_for_Goals ~ DataMinutes_Goals$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Goals", xlab = "Cluster", ylab = "")


DataMinutes_Assists <- df5 %>% select(1:4,9,11:14,Minutes_for_Assists, Assists, Cluster)
DataMinutes_Assists$Remove <- ifelse(DataMinutes_Assists$Minutes_for_Assists == DataMinutes_Assists$minutes_played,1,0)
DataMinutes_Assists <- filter(DataMinutes_Assists, Remove== 0)
DataMinutes_Assists <- DataMinutes_Assists[order(DataMinutes_Assists$Minutes_for_Assists, decreasing = F),]
DataMinutes_Assists$Remove <-NULL
DataMinutes_Assists$Position <- row.names(DataMinutes_Assists)
boxplot(DataMinutes_Assists$Minutes_for_Assists ~ DataMinutes_Assists$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Assists", xlab = "Cluster", ylab = "")

DataMinutes_BigChancesCreated <- df5 %>% select(1:4,9,11:14,Minutes_for_Big_chances_created, Cluster)
DataMinutes_BigChancesCreated$Remove <- ifelse(DataMinutes_BigChancesCreated$Minutes_for_Big_chances_created == DataMinutes_BigChancesCreated$minutes_played,1,0)
DataMinutes_BigChancesCreated <- filter(DataMinutes_BigChancesCreated, Remove== 0)
DataMinutes_BigChancesCreated <- DataMinutes_BigChancesCreated[order(DataMinutes_BigChancesCreated$Minutes_for_Big_chances_created, decreasing = F),]
DataMinutes_BigChancesCreated$Remove <-NULL
DataMinutes_BigChancesCreated$Position <- row.names(DataMinutes_BigChancesCreated)
boxplot(DataMinutes_BigChancesCreated$Minutes_for_Big_chances_created ~ DataMinutes_BigChancesCreated$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Big chances created", xlab = "Cluster", ylab = "")

DataMinutes_BigChancesMissed <- df5 %>% select(1:4,9,11:14,Minutes_for_Big_chances_missed, Cluster)
DataMinutes_BigChancesMissed$Remove <- ifelse(DataMinutes_BigChancesMissed$Minutes_for_Big_chances_missed == DataMinutes_BigChancesMissed$minutes_played,1,0)
DataMinutes_BigChancesMissed <- filter(DataMinutes_BigChancesMissed, Remove== 0)
DataMinutes_BigChancesMissed <- DataMinutes_BigChancesMissed[order(DataMinutes_BigChancesMissed$Minutes_for_Big_chances_missed, decreasing = F),]
DataMinutes_BigChancesMissed$Remove <-NULL
DataMinutes_BigChancesMissed$Position <- row.names(DataMinutes_BigChancesMissed)
boxplot(DataMinutes_BigChancesMissed$Minutes_for_Big_chances_missed ~ DataMinutes_BigChancesMissed$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Big chances missed", xlab = "Cluster", ylab = "")

DataMinutes_Disarms <- df5 %>% select(1:4,9,11:14,Minutes_for_Disarms, Cluster)
DataMinutes_Disarms$Remove <- ifelse(DataMinutes_Disarms$Minutes_for_Disarms == DataMinutes_Disarms$minutes_played,1,0)
DataMinutes_Disarms <- filter(DataMinutes_Disarms, Remove== 0)
DataMinutes_Disarms <- DataMinutes_Disarms[order(DataMinutes_Disarms$Minutes_for_Disarms, decreasing = F),]
DataMinutes_Disarms$Remove <-NULL
DataMinutes_Disarms$Position <- row.names(DataMinutes_Disarms)
boxplot(DataMinutes_Disarms$Minutes_for_Disarms ~ DataMinutes_Disarms$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Disarms", xlab = "Cluster", ylab = "")

DataMinutes_Blocks <- df5 %>% select(1:4,9,11:14,Minutes_for_Blocks, Cluster)
DataMinutes_Blocks$Remove <- ifelse(DataMinutes_Blocks$Minutes_for_Blocks == DataMinutes_Blocks$minutes_played,1,0)
DataMinutes_Blocks <- filter(DataMinutes_Blocks, Remove== 0)
DataMinutes_Blocks <- DataMinutes_Blocks[order(DataMinutes_Blocks$Minutes_for_Blocks, decreasing = F),]
DataMinutes_Blocks$Remove <-NULL
DataMinutes_Blocks$Position <- row.names(DataMinutes_Blocks)
boxplot(DataMinutes_Blocks$Minutes_for_Blocks ~ DataMinutes_Blocks$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Blocks", xlab = "Cluster", ylab = "")

DataMinutes_Interceptions <- df5 %>% select(1:4,9,11:14,Minutes_for_Interceptions, Cluster)
DataMinutes_Interceptions$Remove <- ifelse(DataMinutes_Interceptions$Minutes_for_Interceptions == DataMinutes_Interceptions$minutes_played,1,0)
DataMinutes_Interceptions <- filter(DataMinutes_Interceptions, Remove== 0)
DataMinutes_Interceptions <- DataMinutes_Interceptions[order(DataMinutes_Interceptions$Minutes_for_Interceptions, decreasing = F),]
DataMinutes_Interceptions$Remove <-NULL
DataMinutes_Interceptions$Position <- row.names(DataMinutes_Interceptions)
boxplot(DataMinutes_Interceptions$Minutes_for_Interceptions ~ DataMinutes_Interceptions$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Interceptions", xlab = "Cluster", ylab = "")


#Goalkeepers
par(mfrow=c(1,4))
Goalkeepers <- filter(df5, Cluster == 4)
boxplot(Goalkeepers$Saves ~ Goalkeepers$Cluster, col= c("#f3f3f3"), main="Saves", xlab = "Cluster 4 (Goalkeepers)", ylab = "")
DataMinutes_Saves <- df5 %>% select(1:4,9,11:14,Minutes_for_Saves, Cluster)
DataMinutes_Saves$Remove <- ifelse(DataMinutes_Saves$Minutes_for_Saves == DataMinutes_Saves$minutes_played,1,0)
DataMinutes_Saves <- filter(DataMinutes_Saves, Remove== 0)
DataMinutes_Saves <- DataMinutes_Saves[order(DataMinutes_Saves$Minutes_for_Saves, decreasing = F),]
DataMinutes_Saves$Remove <-NULL
DataMinutes_Saves$Position <- row.names(DataMinutes_Saves)
boxplot(DataMinutes_Saves$Minutes_for_Saves ~ Goalkeepers$Cluster, col= c("#f3f3f3"), main="Minutes for saves", xlab = "Cluster 4 (Goalkeepers)", ylab = "")
boxplot(Goalkeepers$Goals_conceded ~ Goalkeepers$Cluster, col= c("#f3f3f3"), main="Goals conceded", xlab = "Cluster 4 (Goalkeepers)", ylab = "")
boxplot(Goalkeepers$Clearances ~ Goalkeepers$Cluster, col= c("#f3f3f3"), main="Clearances", xlab = "Cluster 4 (Goalkeepers)", ylab = "")



Rank_Goals <- df5 %>% select(1:4,9,11:14,`Top scorer`, Cluster)
Rank_Goals <- Rank_Goals[order(Rank_Goals$`Top scorer`, decreasing = T),]
Rank_Goals$Position <- row.names(Rank_Goals)

Rank_ShotsOnTargetForGoals <- df5 %>% select(1:4,9,11:14,Shots_on_target_per_goals, Cluster)
filter(Rank_ShotsOnTargetForGoals, participant_name == "PEDRO", team_name == "Flamengo")



head(Rank_Goals, 15)

Rank_Assists <- df5 %>% select(1:4,9,11:14,Assists, Cluster)
Rank_Assists <- Rank_Assists[order(Rank_Assists$Assists, decreasing = T),]
Rank_Assists$Position <- row.names(Rank_Assists)

Rank_GoalsAssists_age23 <- df5 %>% select(1:4,9,11:14,`Goals + Assists`,Age23, Cluster)
Rank_GoalsAssists_age23 <- filter(Rank_GoalsAssists_age23, Age23 == 1)
Rank_GoalsAssists_age23 <- Rank_GoalsAssists_age23[order(Rank_GoalsAssists_age23$`Goals + Assists`, decreasing = T),]
Rank_GoalsAssists_age23$Position <- row.names(Rank_GoalsAssists_age23)



Rank_Disarms_Blocks_Intercep <- df5 %>% select(1:4,9,11:14,Sum_Disarms_Blocks_Intercep, Cluster)
Rank_Disarms_Blocks_Intercep <- Rank_Disarms_Blocks_Intercep[order(Rank_Disarms_Blocks_Intercep$Sum_Disarms_Blocks_Intercep, decreasing = T),]
Rank_Disarms_Blocks_Intercep$Position <- row.names(Rank_Disarms_Blocks_Intercep)

DataMinutes_Goals


df1 <- tm_player_market_values(country_name = "Brazil", start_year = 2021)
#tratando nomes dos jogadores da base de valores
df1$player_name <- apply(df1[,7,drop=F],2, removePunctuation)
#Convertendo tipo de coluna para caractere
df1$player_name<- as.character(df1$player_name)
#Convertendo nomes para letras maiusculas
df1$player_name <- apply(df1[,7,drop=F],2, toupper)
#Removendo acentuacoes dos nomes
df1$player_name <- stri_trans_general(df1$player_name,id="Latin-ASCII")
df3 <- Brazilian_teams %>% select(2,1)
df1 <- left_join(df1, df3,  by="player_name")
df1$concatenado <- paste(df1$player_name,"_",df1$team_name_transfermarkt)
df1 <- df1 %>% select(21,13,14,18)
df1$MarketValue_MM <- df1$player_market_value_euro / 1000000
df1 <- df1[order(df1$MarketValue_MM, decreasing = T),]
df1$Position <- 1:nrow(df1)
filter(df1, Position == c(1,20))


#df5_Zone_Freq <- df5 %>% count(Zone, player_pos, Cluster,  sort= TRUE)

#library(lattice)
#barchart(Zone ~ n | Cluster, data = df5_Zone_Freq, groups= Zone, stack = T)

#############

teste %>% group_by(Cluster) %>% summarise_at(vars(Minutes_for_Big_chances_created), list(Minutes_BigChances = mean))

#DESCONSIDERAR QUEM TIVER MINUTAGEM IGUAL, PARA ANALISAR MINUTOS
teste <- df5 %>% select(1:4,9,11:14,76:95)
teste$Desconsiderar <- ifelse(teste$Minutes_for_Goals_Assists == teste$minutes_played,0,1)
teste <- filter(teste, Desconsiderar== 1)


EDA_Group1 <- df5 %>% select(1:4,38,23:24,72,95)
EDA_Group1 <- filter(EDA_Group1, EDA_Group1$Cluster == 1)
N <- count(EDA_Group1)
Possession_Won_final3rd <- filter(EDA_Group1, EDA_Group1$Possession_won_final_3rd > 18)
count(Possession_Won_final3rd)/N


#teste <- filter(teste, teste$Age23 == 1)
teste <- Group3 %>% select(1:4,9,11:14,76:95)
teste <- filter(Group3, Group3$Age23 == 1)


colnames(df5)

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
write.xlsx(df1, sheetName = "df1", file =  "C:/Users/thiag/Desktop/df1.xlsx")

?write.xlsx
