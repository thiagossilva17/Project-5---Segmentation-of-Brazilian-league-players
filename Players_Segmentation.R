install.packages("worldfootballR")
library(worldfootballR)
install.packages("dply")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("stringr")
library(stringr)
install.packages("tm")
library(tm)
install.packages("stringi")
library(stringi)
install.packages("xlsx")
library(xlsx)
install.packages("ggplot2")
library(ggplot2)
install.packages("caret")
library(caret)
install.packages("factoextra")
library(factoextra)
install.packages("gridExtra")
library(gridExtra)


# HOME - EXTRACTION OF PERFORMANCE DATA FROM THE FOTMOB API AND DATA REMODELING
PERFORMANCE_DATA <- fotmob_get_season_stats(
  country = "BRA",
  league_name = "Serie A",
  season = "2022",
  stat_name = c("Chances created", "Big chances created","Big chances missed", "Accurate long balls per 90", "Accurate passes per 90","Shots per 90","Shots on target per 90", "Goals per 90", "Assists", "Goals + Assists","Successful dribbles per 90","Successful tackles per 90","Penalties won","Penalties conceded","Blocks per 90","Clean sheets","Clearances per 90","Fouls committed per 90","Goals conceded per 90","Interceptions per 90","Possession won final 3rd per 90","Save percentage","Saves per 90","Top scorer","Yellow cards","Red cards"),
  team_or_player = "player"
)

#separating descriptive variables for base remodeling
df_desc <- PERFORMANCE_DATA %>% select(10,9,11,16,15,20)
df_desc <- unique(df_desc)

PERFORMANCE_DATA<- PERFORMANCE_DATA %>% select(10,7,13)
PERFORMANCE_DATA <- PERFORMANCE_DATA %>% pivot_wider(names_from = stat_name, values_from = stat_value)
#Replacing NA values with 0
PERFORMANCE_DATA <- PERFORMANCE_DATA %>%
  replace(is.na(.), 0)
#returning descriptive variables to the main base, after remodeling
PERFORMANCE_DATA <- left_join(PERFORMANCE_DATA, df_desc, by="particiant_id")


# EXTRACTING AGE AND POSITION DATA FROM TRANSFERMARKT API
#PREPARATION
TRANSFERMARKT_DATA <- tm_squad_stats(team_url = c("https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/1023/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/614/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/199/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/6600/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/2462/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/679/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/330/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/2863/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/3197/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/221/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/8793/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/10870/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/537/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/585/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/2029/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/28022/saison_id/2022", "https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/776/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/2035/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/15172/saison_id/2022","https://www.transfermarkt.com/se-palmeiras-sao-paulo/startseite/verein/10492/saison_id/2022"))
names(TRANSFERMARKT_DATA)[1] <- "team_name_transfermarkt"
TRANSFERMARKT_DATA <- TRANSFERMARKT_DATA %>% select(1,4,6,7,8)

#Mining texts from the Transfermarkt database
#Removing punctuation from all base names
TRANSFERMARKT_DATA$player_name <- apply(TRANSFERMARKT_DATA[,2,drop=F],2, removePunctuation)
#Converting names to capital letters
TRANSFERMARKT_DATA$player_name <- apply(TRANSFERMARKT_DATA[,2,drop=F],2, toupper)
#Converting column type to character
TRANSFERMARKT_DATA$player_name<- as.character(TRANSFERMARKT_DATA$player_name)
# Removing accents from names
TRANSFERMARKT_DATA$player_name <- stri_trans_general(TRANSFERMARKT_DATA$player_name,id="Latin-ASCII")

#Mining texts from Fotmob base
# Removing punctuation from all base names
PERFORMANCE_DATA$participant_name <- apply(PERFORMANCE_DATA[,28,drop=F],2, removePunctuation)
#Converting column type to character
PERFORMANCE_DATA$participant_name<- as.character(PERFORMANCE_DATA$participant_name)
# Converting names to capital letters
PERFORMANCE_DATA$participant_name <- apply(PERFORMANCE_DATA[,28,drop=F],2, toupper)
# Removing accents from names
PERFORMANCE_DATA$participant_name <- stri_trans_general(PERFORMANCE_DATA$participant_name,id="Latin-ASCII")

#CREATING FROM/TO TO STANDARDIZE THE NAMES OF THE TWO APIs' TEAM.
team_name_fotmob <- c("Palmeiras","Ceará","Bragantino","Fluminense","Atlético Mineiro","Internacional","Flamengo","São Paulo","América Mineiro","Cuiabá","Atlético Goianiense","Coritiba","Fortaleza EC","Botafogo","Avaí","Santos","Goiás","Atlético Paranaense","Juventude","Corinthians")
team_name_transfermarkt <- c("Sociedade Esportiva Palmeiras","Ceará Sporting Club","Red Bull Bragantino","Fluminense Football Club","Clube Atlético Mineiro","Sport Club Internacional",
                             "Clube de Regatas do Flamengo","São Paulo Futebol Clube","América Futebol Clube (MG)","Cuiabá Esporte Clube (MT)","Atlético Clube Goianiense","Coritiba Foot Ball Club","Fortaleza Esporte Clube","Botafogo de Futebol e Regatas","Avaí Futebol Clube (SC)","Santos FC","Goiás Esporte Clube","Club Athletico Paranaense","Esporte Clube Juventude","Sport Club Corinthians Paulista")
team_names <- data.frame(team_name_fotmob, team_name_transfermarkt)
TRANSFERMARKT_DATA$team_name_DEPARA <- left_join(TRANSFERMARKT_DATA, team_names, by="team_name_transfermarkt")
TRANSFERMARKT_DATA$team_name_DEPARA <- TRANSFERMARKT_DATA$team_name_DEPARA$team_name_fotmob

#CONCATENTING PLAYER NAME WITH TEAM NAME FOR BOTH BASES
TRANSFERMARKT_DATA$concatenado <- paste(TRANSFERMARKT_DATA$player_name,"_",TRANSFERMARKT_DATA$team_name_DEPARA)
PERFORMANCE_DATA$concatenado <- paste(PERFORMANCE_DATA$participant_name,"_",PERFORMANCE_DATA$team_name)

#CORRECTING NOMENCLATURE OF ATHLETES WITH DIFFERENT REGISTRATIONS
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"RONI _ Palmeiras","RONY _ Palmeiras")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"MURILO CERQUEIRA _ Palmeiras","MURILO _ Palmeiras")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"JOSE LOPEZ _ Palmeiras","JOSE MANUEL LOPEZ _ Palmeiras")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"LUAN GARCIA _ Palmeiras","LUAN _ Palmeiras")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"LUCIANO NEVES _ São Paulo","LUCIANO _ São Paulo")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"DIEGO _ São Paulo","DIEGO COSTA _ São Paulo")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"SANDRY SANTOS _ Santos","SANDRY _ Santos")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"PAULO GANSO _ Fluminense","GANSO _ Fluminense")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"VIKTOR HUGO _ Flamengo","VICTOR HUGO _ Flamengo")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"RAUL _ Corinthians","RAUL GUSTAVO _ Corinthians")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"EDUARDO QUEIROZ _ Corinthians","DU QUEIROZ _ Corinthians")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"RAMIRES _ Bragantino","ERIC RAMIRES _ Bragantino")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"ARTUR GUIMARAES _ Bragantino","ARTUR _ Bragantino")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"VICTOR LEANDRO CUESTA _ Botafogo","VICTOR CUESTA _ Botafogo")
PERFORMANCE_DATA$concatenado <- str_replace(PERFORMANCE_DATA$concatenado,"RODRIGUINHO _ América Mineiro","RODRIGUINHO _ Cuiabá")

#UNITING BASES
PERFORMANCE_DATA <- left_join(PERFORMANCE_DATA, TRANSFERMARKT_DATA, by="concatenado")

#Checking number of baselines
nrow(PERFORMANCE_DATA)
#Percentage of NAS values in each column
NAS <- round(colSums(is.na(PERFORMANCE_DATA))*100/nrow(PERFORMANCE_DATA), 2)
#SEE ALL NAS
NAS

# Removing NA values
PERFORMANCE_DATA<- na.omit(PERFORMANCE_DATA)
# Checking number of lines remaining
nrow(PERFORMANCE_DATA)

#Creating field zone variable
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
PERFORMANCE_DATA$Zone <- ifelse(PERFORMANCE_DATA$player_pos ==  "Right Winger" | PERFORMANCE_DATA$player_pos == "Left Winger" | PERFORMANCE_DATA$player_pos == "Centre-Forward" | PERFORMANCE_DATA$player_pos == "Second Striker", "Attacker",
                   ifelse(PERFORMANCE_DATA$player_pos ==  "Attacking Midfield" | PERFORMANCE_DATA$player_pos == "Central Midfield" | PERFORMANCE_DATA$player_pos == "Defensive Midfield" | PERFORMANCE_DATA$player_pos == "Left Midfield" | PERFORMANCE_DATA$player_pos == "Right Midfield", "Midfield",
                          ifelse(PERFORMANCE_DATA$player_pos ==  "Right-Back" | PERFORMANCE_DATA$player_pos == "Left-Back" | PERFORMANCE_DATA$player_pos == "Centre-Back", "Defender",
                                 "Goalkeeper")))
PERFORMANCE_DATA <- PERFORMANCE_DATA %>% select(1,28,29,32:35,38,36,39,40,30:31,37,2:27)

#GOALKEEPERS ARE BEING REPRESENTED AS CONTROL CATEGORIES, WHERE IT IS THE ONLY POSITION CLASSIFIED WITH 0 IN ALL DUMMIES VARIABLES
PERFORMANCE_DATA$Dummie_position <- PERFORMANCE_DATA$player_pos
#TRANSFORMING DUMMIES
PERFORMANCE_DATA$Dummie <- 1
PERFORMANCE_DATA <- PERFORMANCE_DATA %>% pivot_wider(names_from = Dummie_position, values_from = Dummie)
PERFORMANCE_DATA<- PERFORMANCE_DATA %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
PERFORMANCE_DATA$Goalkeeper <- NULL

#CONSIDERING PLAYERS WHO ACT ON AVERAGE MORE THAN 45 MIN AS OWNERS
PERFORMANCE_DATA$AVG_time <- round(PERFORMANCE_DATA$minutes_played / PERFORMANCE_DATA$matches_played,0)
PERFORMANCE_DATA$starting_player <- ifelse(PERFORMANCE_DATA$AVG_time > 45 ,1,0)
#GOALKEEPERS ARE BEING REPRESENTED AS CONTROL CATEGORIES BY SETTING THE POSITION ZONES BELOW, WHERE THEY ARE THE ONLY ZONE RATED WITH 0 IN ALL DUMMIES VARIABLES
PERFORMANCE_DATA$Attacking <- ifelse(PERFORMANCE_DATA$`Right Winger` == 1 | PERFORMANCE_DATA$`Left Winger`  == 1 | PERFORMANCE_DATA$`Centre-Forward` == 1 | PERFORMANCE_DATA$`Second Striker` == 1, 1, 0)
PERFORMANCE_DATA$Midfield <- ifelse(PERFORMANCE_DATA$`Attacking Midfield` == 1 | PERFORMANCE_DATA$`Central Midfield` == 1 | PERFORMANCE_DATA$`Defensive Midfield` == 1 | PERFORMANCE_DATA$`Left Midfield` == 1, 1, 0)
PERFORMANCE_DATA$Defender <- ifelse(PERFORMANCE_DATA$`Right-Back` == 1 | PERFORMANCE_DATA$`Left-Back` == 1 | PERFORMANCE_DATA$`Centre-Back` == 1 , 1, 0)

#removing special case richard ceara, player with exactly the same name on the same team
PERFORMANCE_DATA$Exclude <- ifelse(PERFORMANCE_DATA$particiant_id == 934515 & PERFORMANCE_DATA$player_pos == 'Defensive Midfield'  | PERFORMANCE_DATA$particiant_id == 882625 & PERFORMANCE_DATA$player_pos == 'Goalkeeper',1,0)
PERFORMANCE_DATA <- filter(PERFORMANCE_DATA, Exclude != 1)
PERFORMANCE_DATA$Exclude <- NULL

#Creating variables for better group segmentation and greater analysis power
PERFORMANCE_DATA$Matches_90min_played <- round(PERFORMANCE_DATA$minutes_played/90,2)
PERFORMANCE_DATA$Big_chances_created_per_match <- round(PERFORMANCE_DATA$`Big chances created` / PERFORMANCE_DATA$matches_played,2)
PERFORMANCE_DATA$Chances_created_per_match <- round(PERFORMANCE_DATA$`Chances created` / PERFORMANCE_DATA$matches_played,2)
PERFORMANCE_DATA$Big_chances_missed_per_match <- round(PERFORMANCE_DATA$`Big chances missed` / PERFORMANCE_DATA$matches_played, 2)
PERFORMANCE_DATA$Goals_Assists_per_match <- round(PERFORMANCE_DATA$`Goals + Assists` / PERFORMANCE_DATA$matches_played,2)
PERFORMANCE_DATA$Successful_dribbles <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Successful dribbles per 90`)
PERFORMANCE_DATA$Disarms <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Successful tackles per 90`)
PERFORMANCE_DATA$Shots <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Shots per 90`)
PERFORMANCE_DATA$Shots_on_target <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Shots on target per 90`)
PERFORMANCE_DATA$Blocks <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Blocks per 90`)
PERFORMANCE_DATA$Fouls_committed <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Fouls committed per 90`)
PERFORMANCE_DATA$Goals_conceded <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Goals conceded per 90`)
PERFORMANCE_DATA$Clearances <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Clearances per 90`)
PERFORMANCE_DATA$Interceptions <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Interceptions per 90`)
PERFORMANCE_DATA$Possession_won_final_3rd <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Possession won final 3rd per 90`)
PERFORMANCE_DATA$Saves <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Saves per 90`)
PERFORMANCE_DATA$Accurate_long_balls <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Accurate long balls per 90`)
PERFORMANCE_DATA$Accurate_passes <- round(PERFORMANCE_DATA$Matches_90min_played * PERFORMANCE_DATA$`Accurate passes per 90`)
PERFORMANCE_DATA$Sum_yellow_red_cards <- PERFORMANCE_DATA$`Yellow cards` + PERFORMANCE_DATA$`Red cards`
PERFORMANCE_DATA$Sum_Disarms_Blocks_Intercep <- PERFORMANCE_DATA$Disarms + PERFORMANCE_DATA$Blocks + PERFORMANCE_DATA$Interceptions
PERFORMANCE_DATA$Big_chances_created_per_assists <- PERFORMANCE_DATA$`Big chances created` / PERFORMANCE_DATA$Assists
PERFORMANCE_DATA$Shots_on_target_per_goals <- PERFORMANCE_DATA$Shots_on_target / PERFORMANCE_DATA$`Top scorer`

#Creating variables to explore timing data
PERFORMANCE_DATA$Minutes_for_Goals <- round(PERFORMANCE_DATA$minutes_played / PERFORMANCE_DATA$`Top scorer`)
PERFORMANCE_DATA$Minutes_for_Assists <- round(PERFORMANCE_DATA$minutes_played / PERFORMANCE_DATA$Assists)
PERFORMANCE_DATA$Minutes_for_Goals_Assists <- round(PERFORMANCE_DATA$minutes_played / PERFORMANCE_DATA$`Goals + Assists`)
PERFORMANCE_DATA$Minutes_for_Big_chances_created <- round(PERFORMANCE_DATA$minutes_played / PERFORMANCE_DATA$`Big chances created`)
PERFORMANCE_DATA$Minutes_for_Big_chances_missed <- round(PERFORMANCE_DATA$minutes_played / PERFORMANCE_DATA$`Big chances missed`)
PERFORMANCE_DATA$Minutes_for_Disarms <- round(PERFORMANCE_DATA$minutes_played / PERFORMANCE_DATA$Disarms)
PERFORMANCE_DATA$Minutes_for_Blocks <- round(PERFORMANCE_DATA$minutes_played / PERFORMANCE_DATA$Blocks)
PERFORMANCE_DATA$Minutes_for_Interceptions <- round(PERFORMANCE_DATA$minutes_played / PERFORMANCE_DATA$Interceptions)
PERFORMANCE_DATA$Minutes_for_Saves <- round(PERFORMANCE_DATA$minutes_played / PERFORMANCE_DATA$Saves)
PERFORMANCE_DATA$Minutes_for_YellowCards <- round(PERFORMANCE_DATA$minutes_played / PERFORMANCE_DATA$`Yellow cards`)
PERFORMANCE_DATA$Minutes_for_RedCards <- round(PERFORMANCE_DATA$minutes_played / PERFORMANCE_DATA$`Red cards`)

#handling divisors by zero
PERFORMANCE_DATA$Minutes_for_Goals <- ifelse(PERFORMANCE_DATA$Minutes_for_Goals == "Inf" | PERFORMANCE_DATA$Minutes_for_Goals == "NaN" , PERFORMANCE_DATA$minutes_played, PERFORMANCE_DATA$Minutes_for_Goals)
PERFORMANCE_DATA$Minutes_for_Assists <- ifelse(PERFORMANCE_DATA$Minutes_for_Assists == "Inf" | PERFORMANCE_DATA$Minutes_for_Assists == "NaN", PERFORMANCE_DATA$minutes_played, PERFORMANCE_DATA$Minutes_for_Assists)
PERFORMANCE_DATA$Minutes_for_Goals_Assists <- ifelse(PERFORMANCE_DATA$Minutes_for_Goals_Assists == "Inf" | PERFORMANCE_DATA$Minutes_for_Goals_Assists == "NaN", PERFORMANCE_DATA$minutes_played, PERFORMANCE_DATA$Minutes_for_Goals_Assists)
PERFORMANCE_DATA$Minutes_for_Big_chances_created <- ifelse(PERFORMANCE_DATA$Minutes_for_Big_chances_created == "Inf" | PERFORMANCE_DATA$Minutes_for_Big_chances_created == "NaN", PERFORMANCE_DATA$minutes_played, PERFORMANCE_DATA$Minutes_for_Big_chances_created)
PERFORMANCE_DATA$Minutes_for_Big_chances_missed <- ifelse(PERFORMANCE_DATA$Minutes_for_Big_chances_missed == "Inf" | PERFORMANCE_DATA$Minutes_for_Big_chances_missed == "NaN", PERFORMANCE_DATA$minutes_played, PERFORMANCE_DATA$Minutes_for_Big_chances_missed)
PERFORMANCE_DATA$Minutes_for_Disarms <- ifelse(PERFORMANCE_DATA$Minutes_for_Disarms == "Inf" | PERFORMANCE_DATA$Minutes_for_Disarms == "NaN", PERFORMANCE_DATA$minutes_played, PERFORMANCE_DATA$Minutes_for_Disarms)
PERFORMANCE_DATA$Minutes_for_Blocks <- ifelse(PERFORMANCE_DATA$Minutes_for_Blocks == "Inf" | PERFORMANCE_DATA$Minutes_for_Blocks == "NaN", PERFORMANCE_DATA$minutes_played, PERFORMANCE_DATA$Minutes_for_Blocks)
PERFORMANCE_DATA$Minutes_for_Interceptions <- ifelse(PERFORMANCE_DATA$Minutes_for_Interceptions == "Inf" | PERFORMANCE_DATA$Minutes_for_Interceptions == "NaN", PERFORMANCE_DATA$minutes_played, PERFORMANCE_DATA$Minutes_for_Interceptions)
PERFORMANCE_DATA$Minutes_for_Saves <- ifelse(PERFORMANCE_DATA$Minutes_for_Saves == "Inf" | PERFORMANCE_DATA$Minutes_for_Saves == "NaN", PERFORMANCE_DATA$minutes_played, PERFORMANCE_DATA$Minutes_for_Saves)
PERFORMANCE_DATA$Minutes_for_YellowCards <- ifelse(PERFORMANCE_DATA$Minutes_for_YellowCards == "Inf" | PERFORMANCE_DATA$Minutes_for_YellowCards == "NaN", PERFORMANCE_DATA$minutes_played, PERFORMANCE_DATA$Minutes_for_YellowCards)
PERFORMANCE_DATA$Minutes_for_RedCards <- ifelse(PERFORMANCE_DATA$Minutes_for_RedCards == "Inf" | PERFORMANCE_DATA$Minutes_for_RedCards == "NaN", PERFORMANCE_DATA$minutes_played, PERFORMANCE_DATA$Minutes_for_RedCards)
PERFORMANCE_DATA$Big_chances_created_per_assists <- ifelse(PERFORMANCE_DATA$Big_chances_created_per_assists == "Inf" | PERFORMANCE_DATA$Big_chances_created_per_assists == "NaN", PERFORMANCE_DATA$`Big chances created`, PERFORMANCE_DATA$Big_chances_created_per_assists)
PERFORMANCE_DATA$Shots_on_target_per_goals <- ifelse(PERFORMANCE_DATA$Shots_on_target_per_goals == "Inf" | PERFORMANCE_DATA$Shots_on_target_per_goals == "NaN", PERFORMANCE_DATA$Shots_on_target, PERFORMANCE_DATA$Shots_on_target_per_goals)

#Creating variables for segmentation and exploration of age data
PERFORMANCE_DATA$Age23 <- ifelse(PERFORMANCE_DATA$player_age <= 23, 1,0)
PERFORMANCE_DATA$smaller_Age30 <- ifelse(PERFORMANCE_DATA$player_age > 23 & PERFORMANCE_DATA$player_age < 30 , 1,0)
PERFORMANCE_DATA$larger_Age30 <- ifelse(PERFORMANCE_DATA$player_age >= 30, 1,0)
PERFORMANCE_DATA$None_Goals_Assists <- ifelse(PERFORMANCE_DATA$`Goals + Assists` == 0, 1,0)


PERFORMANCE_DATA_NORMALIZED <- PERFORMANCE_DATA
# PERFORMING PREDICTION WITH CLUSTERING ALGORITHM AND USING "SCALE" STANDARDIZATION METHOD
PERFORMANCE_DATA_NORMALIZED <- predict(preProcess(PERFORMANCE_DATA_NORMALIZED[,12:94], method ="scale") ,PERFORMANCE_DATA_NORMALIZED)
#COMMAND TO ENSURE THE READER REACHES THE SAME RESULT
set.seed(1)

#SEPARATING DIFFERENT TYPES OF CLUSTERS
G2 <- kmeans(PERFORMANCE_DATA_NORMALIZED[12:94], centers=2)
G3 <- kmeans(PERFORMANCE_DATA_NORMALIZED[12:94], centers=3)
G4 <- kmeans(PERFORMANCE_DATA_NORMALIZED[12:94], centers=4)
G5 <- kmeans(PERFORMANCE_DATA_NORMALIZED[12:94], centers=5)
G6 <- kmeans(PERFORMANCE_DATA_NORMALIZED[12:94], centers=6)
G7 <- kmeans(PERFORMANCE_DATA_NORMALIZED[12:94], centers=7)
G8 <- kmeans(PERFORMANCE_DATA_NORMALIZED[12:94], centers=8)
G9 <- kmeans(PERFORMANCE_DATA_NORMALIZED[12:94], centers=9)
G10 <- kmeans(PERFORMANCE_DATA_NORMALIZED[12:94], centers=10)
G11 <- kmeans(PERFORMANCE_DATA_NORMALIZED[12:94], centers=11)

# Storing model graphs in variables
viz_G2 <- fviz_cluster(G2, data = PERFORMANCE_DATA_NORMALIZED[,c(12:94)],palette = c("#DC143C","#7fffd4"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 1")
viz_G3 <- fviz_cluster(G3, data = PERFORMANCE_DATA_NORMALIZED[,c(12:94)],palette = c("#DC143C","#7fffd4", "#87cefa"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 2")
viz_G4 <- fviz_cluster(G4, data = PERFORMANCE_DATA_NORMALIZED[,c(12:94)],palette = c("#DC143C","#7fffd4", "#87cefa", "#a9a9a9"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 3")
viz_G5 <- fviz_cluster(G5, data = PERFORMANCE_DATA_NORMALIZED[,c(12:94)],palette = c("#DC143C","#7fffd4", "#87cefa", "#a9a9a9", "#FF1493"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 4")
viz_G6 <- fviz_cluster(G6, data = PERFORMANCE_DATA_NORMALIZED[12:94],palette = c("#DC143C","#7fffd4", "#87cefa", "#a9a9a9", "#FF1493","#FF0000"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 5")
viz_G7 <- fviz_cluster(G7, data = PERFORMANCE_DATA_NORMALIZED[12:94],palette = c("#DC143C","#7fffd4", "#87cefa", "#a9a9a9", "#FF1493","#FF0000",	"#C71585"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 6")
viz_G8 <- fviz_cluster(G8, data = PERFORMANCE_DATA_NORMALIZED[12:94],palette = c("#DC143C","#7fffd4", "#87cefa", "#a9a9a9", "#FF1493","#FF0000",	"#C71585",	"#C71585"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 7")
viz_G9 <- fviz_cluster(G9, data = PERFORMANCE_DATA_NORMALIZED[12:94],palette = c("#DC143C","#7fffd4", "#87cefa", "#a9a9a9", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585"), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 8")
viz_G10 <- fviz_cluster(G10, data = PERFORMANCE_DATA_NORMALIZED[12:94],palette = c("#DC143C","#7fffd4", "#87cefa", "#a9a9a9", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585", "#C71585" ), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 9")
viz_G11 <- fviz_cluster(G11, data = PERFORMANCE_DATA_NORMALIZED[12:94],palette = c("#DC143C","#7fffd4", "#87cefa", "#a9a9a9", "#FF1493","#FF0000",	"#C71585",	"#C71585", "#C71585", "#C71585", "#C71585" ), geom = "point", ellipse.type = "convex",  ggtheme = theme_classic(), main = "Modelo 10")

# Visually comparing models
grid.arrange(viz_G2,viz_G3, viz_G4, viz_G5,viz_G6, viz_G7, viz_G8, viz_G9, viz_G10, viz_G11, nrow=2, ncol=5)

#Linkedin post
#viz_G4 <- fviz_cluster(G4, data = PERFORMANCE_DATA_NORMALIZED[,c(12:94)],palette = c("#DC143C","#000000","#000000", "#000000"), geom = "point", ellipse.type = "convex", ggtheme = theme_classic(), main = "Segmentation of soccer players from the Brazilian championship of the 2022 season with the application of the Machine Learning K-means algorithm")

#Viewing chosen model
viz_G4 <- fviz_cluster(G4, data = PERFORMANCE_DATA_NORMALIZED[,c(12:94)],palette = c("#DC143C","#7fffd4", "#87cefa", "#a9a9a9"), geom = "point", ellipse.type = "convex", ggtheme = theme_classic(), main = "Model 3 (4 Clusters)")
grid.arrange(viz_G4, nrow=1, ncol=1)

# Storing clusters in the original dataset
PERFORMANCE_DATA$Cluster <- G4$cluster

#Exploring proportion of the field zone variable
Group1 <- filter(PERFORMANCE_DATA, Cluster ==1)
prop.table(table(Group1$Zone))

Group2 <- filter(PERFORMANCE_DATA, Cluster ==2)
prop.table(table(Group2$Zone))

Group3 <- filter(PERFORMANCE_DATA, Cluster ==3)
prop.table(table(Group3$Zone))

Group4 <- filter(PERFORMANCE_DATA, Cluster ==4)
prop.table(table(Group4$Zone))

# Comparing proportions of variables by clusters
PERFORMANCE_DATA %>% group_by(Cluster) %>% summarise_at(vars(player_age), list(Age_mean = mean))

EDA_GoalsAssists <- PERFORMANCE_DATA %>% group_by(Cluster) %>% summarise_at(vars(`Goals + Assists`), list(Sum_Goals_and_Assists = sum))
EDA_GoalsAssists$Percent <- round((EDA_GoalsAssists$Sum_Goals_and_Assists / sum(EDA_GoalsAssists$Sum_Goals_and_Assists))*100,2)
EDA_GoalsAssists

EDA_Goals <- PERFORMANCE_DATA %>% group_by(Cluster) %>% summarise_at(vars(`Top scorer`), list(Goals = sum))
EDA_Goals$Percent <- round((EDA_Goals$Goals / sum(EDA_Goals$Goals))*100,2)
EDA_Goals

EDA_Assists <- PERFORMANCE_DATA %>% group_by(Cluster) %>% summarise_at(vars(Assists), list(Assists = sum))
EDA_Assists$Percent <- round((EDA_Assists$Assists / sum(EDA_Assists$Assists))*100,2)
EDA_Assists

EDA_BigChancesCreated <- PERFORMANCE_DATA %>% group_by(Cluster) %>% summarise_at(vars(`Big chances created`), list(BigChancesCreated = sum))
EDA_BigChancesCreated$Percent <- round((EDA_BigChancesCreated$BigChancesCreated / sum(EDA_BigChancesCreated$BigChancesCreated))*100,2)
EDA_BigChancesCreated

EDA_Accurate_passes <- PERFORMANCE_DATA %>% group_by(Cluster) %>% summarise_at(vars(Accurate_passes), list(Accurate_passes = sum))
EDA_Accurate_passes$Percent <- round((EDA_Accurate_passes$Accurate_passes / sum(EDA_Accurate_passes$Accurate_passes))*100,2)
EDA_Accurate_passes

EDA_Accurate_long_balls <- PERFORMANCE_DATA %>% group_by(Cluster) %>% summarise_at(vars(Accurate_long_balls), list(Accurate_long_balls = sum))
EDA_Accurate_long_balls$Percent <- round((EDA_Accurate_long_balls$Accurate_long_balls / sum(EDA_Accurate_long_balls$Accurate_long_balls))*100,2)
EDA_Accurate_long_balls

EDA_Disarms_Blocks_Intercep <- PERFORMANCE_DATA %>% group_by(Cluster) %>% summarise_at(vars(Sum_Disarms_Blocks_Intercep), list(Disarms_Blocks_Intercep = sum))
EDA_Disarms_Blocks_Intercep$Percent <- round((EDA_Disarms_Blocks_Intercep$Disarms_Blocks_Intercep / sum(EDA_Disarms_Blocks_Intercep$Disarms_Blocks_Intercep))*100,2)
EDA_Disarms_Blocks_Intercep


#Exploring variables with Boxplots
par(mfrow=c(1,3))
boxplot(PERFORMANCE_DATA$matches_played ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Matches played", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$minutes_played ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes played", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$player_age ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Age", xlab = "Cluster", ylab = "")


par(mfrow=c(2,4))
boxplot(PERFORMANCE_DATA$`Chances created` ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Chances created", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$`Big chances created` ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Big chances created", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$`Big chances missed` ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Big chances missed", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$Assists ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Assists", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$`Top scorer` ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Goals", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$Shots ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Shots", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$Shots_on_target ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Shots on target", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$Successful_dribbles ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Successful dribbles", xlab = "Cluster", ylab = "")


boxplot(PERFORMANCE_DATA$Accurate_passes ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Accurate passes", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$Accurate_long_balls ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Accurate long balls", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$Disarms ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Disarms", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$Blocks ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Blocks", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$Interceptions ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Interceptions", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$Possession_won_final_3rd ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Possession won final 3rd", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$Fouls_committed ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Fouls committed", xlab = "Cluster", ylab = "")
boxplot(PERFORMANCE_DATA$Sum_yellow_red_cards ~ PERFORMANCE_DATA$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Yellow/Red Cards", xlab = "Cluster", ylab = "")



#For minute views, players who have a stat equal to zero are being disregarded.
DataMinutes_GoalsAssists <- PERFORMANCE_DATA %>% select(1:4,9,11:14,Minutes_for_Goals_Assists, `Goals + Assists`, `Top scorer`,Assists, Cluster)
DataMinutes_GoalsAssists$Remove <- ifelse(DataMinutes_GoalsAssists$Minutes_for_Goals_Assists == DataMinutes_GoalsAssists$minutes_played,1,0)
DataMinutes_GoalsAssists <- filter(DataMinutes_GoalsAssists, Remove== 0)
DataMinutes_GoalsAssists <- DataMinutes_GoalsAssists[order(DataMinutes_GoalsAssists$Minutes_for_Goals_Assists, decreasing = F),]
DataMinutes_GoalsAssists$Remove <-NULL
DataMinutes_GoalsAssists$Position <- row.names(DataMinutes_GoalsAssists)
boxplot(DataMinutes_GoalsAssists$Minutes_for_Goals_Assists ~ DataMinutes_GoalsAssists$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Goals/Assists", xlab = "Cluster", ylab = "")

DataMinutes_Goals <- PERFORMANCE_DATA %>% select(1:4,9,11:14,Minutes_for_Goals, `Top scorer`, Cluster)
DataMinutes_Goals$Remove <- ifelse(DataMinutes_Goals$Minutes_for_Goals == DataMinutes_Goals$minutes_played,1,0)
DataMinutes_Goals <- filter(DataMinutes_Goals, Remove== 0)
DataMinutes_Goals <- DataMinutes_Goals[order(DataMinutes_Goals$Minutes_for_Goals, decreasing = F),]
DataMinutes_Goals$Remove <-NULL
DataMinutes_Goals$Position <- row.names(DataMinutes_Goals)
boxplot(DataMinutes_Goals$Minutes_for_Goals ~ DataMinutes_Goals$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Goals", xlab = "Cluster", ylab = "")


DataMinutes_Assists <- PERFORMANCE_DATA %>% select(1:4,9,11:14,Minutes_for_Assists, Assists, Cluster)
DataMinutes_Assists$Remove <- ifelse(DataMinutes_Assists$Minutes_for_Assists == DataMinutes_Assists$minutes_played,1,0)
DataMinutes_Assists <- filter(DataMinutes_Assists, Remove== 0)
DataMinutes_Assists <- DataMinutes_Assists[order(DataMinutes_Assists$Minutes_for_Assists, decreasing = F),]
DataMinutes_Assists$Remove <-NULL
DataMinutes_Assists$Position <- row.names(DataMinutes_Assists)
boxplot(DataMinutes_Assists$Minutes_for_Assists ~ DataMinutes_Assists$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Assists", xlab = "Cluster", ylab = "")

DataMinutes_BigChancesCreated <- PERFORMANCE_DATA %>% select(1:4,9,11:14,Minutes_for_Big_chances_created, Cluster)
DataMinutes_BigChancesCreated$Remove <- ifelse(DataMinutes_BigChancesCreated$Minutes_for_Big_chances_created == DataMinutes_BigChancesCreated$minutes_played,1,0)
DataMinutes_BigChancesCreated <- filter(DataMinutes_BigChancesCreated, Remove== 0)
DataMinutes_BigChancesCreated <- DataMinutes_BigChancesCreated[order(DataMinutes_BigChancesCreated$Minutes_for_Big_chances_created, decreasing = F),]
DataMinutes_BigChancesCreated$Remove <-NULL
DataMinutes_BigChancesCreated$Position <- row.names(DataMinutes_BigChancesCreated)
boxplot(DataMinutes_BigChancesCreated$Minutes_for_Big_chances_created ~ DataMinutes_BigChancesCreated$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Big chances created", xlab = "Cluster", ylab = "")

DataMinutes_BigChancesMissed <- PERFORMANCE_DATA %>% select(1:4,9,11:14,Minutes_for_Big_chances_missed, Cluster)
DataMinutes_BigChancesMissed$Remove <- ifelse(DataMinutes_BigChancesMissed$Minutes_for_Big_chances_missed == DataMinutes_BigChancesMissed$minutes_played,1,0)
DataMinutes_BigChancesMissed <- filter(DataMinutes_BigChancesMissed, Remove== 0)
DataMinutes_BigChancesMissed <- DataMinutes_BigChancesMissed[order(DataMinutes_BigChancesMissed$Minutes_for_Big_chances_missed, decreasing = F),]
DataMinutes_BigChancesMissed$Remove <-NULL
DataMinutes_BigChancesMissed$Position <- row.names(DataMinutes_BigChancesMissed)
boxplot(DataMinutes_BigChancesMissed$Minutes_for_Big_chances_missed ~ DataMinutes_BigChancesMissed$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Big chances missed", xlab = "Cluster", ylab = "")

DataMinutes_Disarms <- PERFORMANCE_DATA %>% select(1:4,9,11:14,Minutes_for_Disarms, Cluster)
DataMinutes_Disarms$Remove <- ifelse(DataMinutes_Disarms$Minutes_for_Disarms == DataMinutes_Disarms$minutes_played,1,0)
DataMinutes_Disarms <- filter(DataMinutes_Disarms, Remove== 0)
DataMinutes_Disarms <- DataMinutes_Disarms[order(DataMinutes_Disarms$Minutes_for_Disarms, decreasing = F),]
DataMinutes_Disarms$Remove <-NULL
DataMinutes_Disarms$Position <- row.names(DataMinutes_Disarms)
boxplot(DataMinutes_Disarms$Minutes_for_Disarms ~ DataMinutes_Disarms$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Disarms", xlab = "Cluster", ylab = "")

DataMinutes_Blocks <- PERFORMANCE_DATA %>% select(1:4,9,11:14,Minutes_for_Blocks, Cluster)
DataMinutes_Blocks$Remove <- ifelse(DataMinutes_Blocks$Minutes_for_Blocks == DataMinutes_Blocks$minutes_played,1,0)
DataMinutes_Blocks <- filter(DataMinutes_Blocks, Remove== 0)
DataMinutes_Blocks <- DataMinutes_Blocks[order(DataMinutes_Blocks$Minutes_for_Blocks, decreasing = F),]
DataMinutes_Blocks$Remove <-NULL
DataMinutes_Blocks$Position <- row.names(DataMinutes_Blocks)
boxplot(DataMinutes_Blocks$Minutes_for_Blocks ~ DataMinutes_Blocks$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Blocks", xlab = "Cluster", ylab = "")

DataMinutes_Interceptions <- PERFORMANCE_DATA %>% select(1:4,9,11:14,Minutes_for_Interceptions, Cluster)
DataMinutes_Interceptions$Remove <- ifelse(DataMinutes_Interceptions$Minutes_for_Interceptions == DataMinutes_Interceptions$minutes_played,1,0)
DataMinutes_Interceptions <- filter(DataMinutes_Interceptions, Remove== 0)
DataMinutes_Interceptions <- DataMinutes_Interceptions[order(DataMinutes_Interceptions$Minutes_for_Interceptions, decreasing = F),]
DataMinutes_Interceptions$Remove <-NULL
DataMinutes_Interceptions$Position <- row.names(DataMinutes_Interceptions)
boxplot(DataMinutes_Interceptions$Minutes_for_Interceptions ~ DataMinutes_Interceptions$Cluster, col= c("#DC143C","#7fffd4", "#87cefa", "#f3f3f3"), main="Minutes for Interceptions", xlab = "Cluster", ylab = "")


#Creating specific views for goalkeepers
par(mfrow=c(1,4))
Goalkeepers <- filter(PERFORMANCE_DATA, Cluster == 4)
boxplot(Goalkeepers$Saves ~ Goalkeepers$Cluster, col= c("#f3f3f3"), main="Saves", xlab = "Cluster 4 (Goalkeepers)", ylab = "")
DataMinutes_Saves <- PERFORMANCE_DATA %>% select(1:4,9,11:14,Minutes_for_Saves, Cluster)
DataMinutes_Saves$Remove <- ifelse(DataMinutes_Saves$Minutes_for_Saves == DataMinutes_Saves$minutes_played,1,0)
DataMinutes_Saves <- filter(DataMinutes_Saves, Remove== 0)
DataMinutes_Saves <- DataMinutes_Saves[order(DataMinutes_Saves$Minutes_for_Saves, decreasing = F),]
DataMinutes_Saves$Remove <-NULL
DataMinutes_Saves$Position <- row.names(DataMinutes_Saves)
boxplot(DataMinutes_Saves$Minutes_for_Saves ~ Goalkeepers$Cluster, col= c("#f3f3f3"), main="Minutes for saves", xlab = "Cluster 4 (Goalkeepers)", ylab = "")
boxplot(Goalkeepers$Goals_conceded ~ Goalkeepers$Cluster, col= c("#f3f3f3"), main="Goals conceded", xlab = "Cluster 4 (Goalkeepers)", ylab = "")
boxplot(Goalkeepers$Clearances ~ Goalkeepers$Cluster, col= c("#f3f3f3"), main="Clearances", xlab = "Cluster 4 (Goalkeepers)", ylab = "")



#Pedro
Rank_ShotsOnTargetForGoals <- PERFORMANCE_DATA %>% select(1:4,9,11:14,Shots_on_target_per_goals, Cluster)
filter(Rank_ShotsOnTargetForGoals, participant_name == "PEDRO", team_name == "Flamengo")
head(DataMinutes_Goals,3)
#Extracting market value data from Transfermarkt API
Transfermarkt_MarketValue <- tm_player_market_values(country_name = "Brazil", start_year = 2021)
#handling player names
Transfermarkt_MarketValue$player_name <- apply(Transfermarkt_MarketValue[,7,drop=F],2, removePunctuation)
Transfermarkt_MarketValue$player_name<- as.character(Transfermarkt_MarketValue$player_name)
Transfermarkt_MarketValue$player_name <- apply(Transfermarkt_MarketValue[,7,drop=F],2, toupper)
Transfermarkt_MarketValue$player_name <- stri_trans_general(Transfermarkt_MarketValue$player_name,id="Latin-ASCII")
Data_temporary <- TRANSFERMARKT_DATA %>% select(2,1)
Transfermarkt_MarketValue <- left_join(Transfermarkt_MarketValue, Data_temporary,  by="player_name")
Transfermarkt_MarketValue$concatenado <- paste(Transfermarkt_MarketValue$player_name,"_",Transfermarkt_MarketValue$team_name_transfermarkt)
Transfermarkt_MarketValue <- Transfermarkt_MarketValue %>% select(21,13,14,18)
Transfermarkt_MarketValue$MarketValue_MM <- Transfermarkt_MarketValue$player_market_value_euro / 1000000
Transfermarkt_MarketValue <- Transfermarkt_MarketValue[order(Transfermarkt_MarketValue$MarketValue_MM, decreasing = T),]
Transfermarkt_MarketValue$Position <- 1:nrow(Transfermarkt_MarketValue)
filter(Transfermarkt_MarketValue, Position == c(1,20))


#Creating performance rankings by variables
#Gustavo Gomez performance
Rank_Goals <- PERFORMANCE_DATA %>% select(1:4,9,11:14,`Top scorer`, Cluster)
Rank_Goals <- Rank_Goals[order(Rank_Goals$`Top scorer`, decreasing = T),]
Rank_Goals$Position <- row.names(Rank_Goals)
head(Rank_Goals, 15)

#Gustavo Gomez
Rank_Disarms_Blocks_Intercep <- PERFORMANCE_DATA %>% select(1:4,9,11:14,Sum_Disarms_Blocks_Intercep, Cluster)
Rank_Disarms_Blocks_Intercep <- Rank_Disarms_Blocks_Intercep[order(Rank_Disarms_Blocks_Intercep$Sum_Disarms_Blocks_Intercep, decreasing = T),]
Rank_Disarms_Blocks_Intercep$Position <- row.names(Rank_Disarms_Blocks_Intercep)
head(Rank_Disarms_Blocks_Intercep,15)

Rank_Assists <- PERFORMANCE_DATA %>% select(1:4,9,11:14,Assists, Cluster)
Rank_Assists <- Rank_Assists[order(Rank_Assists$Assists, decreasing = T),]
Rank_Assists$Position <- row.names(Rank_Assists)


#Youth Highlights:
#Endrick
head(DataMinutes_Goals,3)

#Joao Gomes
head(DataMinutes_Disarms,3)

#Marcos Leonardo
Rank_GoalsAssists_age23 <- PERFORMANCE_DATA %>% select(1:4,9,11:14,`Goals + Assists`,Age23, Cluster)
Rank_GoalsAssists_age23 <- filter(Rank_GoalsAssists_age23, Age23 == 1)
Rank_GoalsAssists_age23 <- Rank_GoalsAssists_age23[order(Rank_GoalsAssists_age23$`Goals + Assists`, decreasing = T),]
Rank_GoalsAssists_age23$Position <- row.names(Rank_GoalsAssists_age23)
head(Rank_GoalsAssists_age23,3)



