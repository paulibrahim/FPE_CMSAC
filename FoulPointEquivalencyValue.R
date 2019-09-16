###NBA_SportVU FUNCTIONS
library(RCurl)
library(dplyr)
library(sp)
library(jsonlite)
library(ggplot2)
library(stringr)
library(tidyr)
library(magicfor)
library(rvest)
library(lubridate)
library(RSelenium)
library(rvest)
library(splashr)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)

game_id <- NA

basketball_reference_pbp_scraper <- function (game_id)
{
  url <- paste0("https://www.basketball-reference.com/boxscores/pbp/", game_id, ".html")
  webpage <- read_html(url)
  col_names <- webpage %>% 
    html_nodes("table#pbp > tr >th") %>% 
    html_attr("data-stat")    
  col_names_new <- c(col_names[2:7])
  
  
  dates <- webpage %>% 
    html_nodes("table#pbp > tr >td") %>% 
    html_text()
  
  datesCharacter_vec <- as.character(dates)
  dates_df <- as.data.frame(datesCharacter_vec)
  endq1_rown <- which(grepl("End of 1st quarter", dates_df$datesCharacter_vec))
  endq2_rown <- which(grepl("End of 2nd quarter", dates_df$datesCharacter_vec))
  endq3_rown <- which(grepl("End of 3rd quarter", dates_df$datesCharacter_vec))
  endq4_rown <- which(grepl("End of 4th quarter", dates_df$datesCharacter_vec))
  jump_ball_rown <- which(grepl("Jump ball", dates_df$datesCharacter_vec))
  dates_df$quarter <- c(rep.int(1,(endq1_rown)),rep.int(2,(endq2_rown-endq1_rown)),rep.int(3,(endq3_rown-endq2_rown)),rep.int(4,(endq4_rown-endq3_rown)))
  
  
  dates_new <- dates[-c((1:2),(jump_ball_rown-1),(jump_ball_rown),((endq1_rown-1):(endq1_rown+2)),((endq2_rown-1):(endq2_rown+2)),((endq3_rown-1):(endq3_rown+2)),((endq4_rown-1):endq4_rown))]
  
  dates_quarter <- dates_df$quarter
  dates_quarter_vec <- dates_quarter[-c((1:2),(jump_ball_rown-1),(jump_ball_rown),((endq1_rown-1):(endq1_rown+2)),((endq2_rown-1):(endq2_rown+2)),((endq3_rown-1):(endq3_rown+2)),((endq4_rown-1):endq4_rown))]
  
  
  m1 <- matrix(dates_new, ncol=6, byrow=TRUE)
  m2 <- matrix(dates_quarter_vec, ncol=6, byrow=TRUE)
  d1 <- as.data.frame(m1, stringsAsFactors=FALSE)
  d2 <- as.data.frame(m2, stringsAsFactors=FALSE)
  colnames(d1) <- col_names_new
  d1$quarter <- d2[,1]
  
  
  
  time_separated_vector <- str_split_fixed(d1$Time, ":", 2)
  time_separated_vector_numeric <- as.numeric(time_separated_vector)
  d1$seconds <- (time_separated_vector_numeric[(nrow(d1)+1):(2*nrow(d1))])+((time_separated_vector_numeric[1:nrow(d1)])*60) ##converting minutes to seconds and compiling them as one column
  d1$rounded_gameClock <- (720*d1$quarter)-d1$seconds
  names(d1)[2]<-"Away"
  names(d1)[6]<-"Home"
  d1$gameId <- game_id
}
basketball_reference_pbp_scraper("201510270GSW")
gsw_pbp_201516 <- rbind(basketball_reference_pbp_scraper("201510270GSW"),basketball_reference_pbp_scraper("201510310NOP"),basketball_reference_pbp_scraper("201511020GSW"),basketball_reference_pbp_scraper("201511040GSW"),basketball_reference_pbp_scraper("201511060GSW"),
                        basketball_reference_pbp_scraper("201511070SAC"),basketball_reference_pbp_scraper("201511090GSW"),basketball_reference_pbp_scraper("201511170GSW"),basketball_reference_pbp_scraper("201511190LAC"),
                        basketball_reference_pbp_scraper("201511200GSW"),basketball_reference_pbp_scraper("201511220DEN"),basketball_reference_pbp_scraper("201511240GSW"),basketball_reference_pbp_scraper("201511270PHO"),basketball_reference_pbp_scraper("201511280GSW"),
                        basketball_reference_pbp_scraper("201512020CHO"),basketball_reference_pbp_scraper("201512060BRK"),basketball_reference_pbp_scraper("201512080IND"),basketball_reference_pbp_scraper("201512120MIL"),
                        basketball_reference_pbp_scraper("201512160GSW"),basketball_reference_pbp_scraper("201512180GSW"),basketball_reference_pbp_scraper("201512230GSW"),basketball_reference_pbp_scraper("201512250GSW"),basketball_reference_pbp_scraper("201512280GSW"),
                        basketball_reference_pbp_scraper("201512300DAL"),basketball_reference_pbp_scraper("201512310HOU"),basketball_reference_pbp_scraper("201601040GSW"),basketball_reference_pbp_scraper("201601050LAL"),
                        basketball_reference_pbp_scraper("201601090SAC"),basketball_reference_pbp_scraper("201601110GSW"),basketball_reference_pbp_scraper("201601130DEN"),basketball_reference_pbp_scraper("201601140GSW"),basketball_reference_pbp_scraper("201601180CLE"),
                        basketball_reference_pbp_scraper("201601200CHI"),basketball_reference_pbp_scraper("201601220GSW"))

file_path1 <- "file path to folder with tracking data in it"
setwd(file_path1)
pbp_df <- gsw_pbp_201516
factorconvert <- function(f){as.numeric(levels(f))[f]}

sportvu_convert_json <- function (filen, game_id)
{
  # Much of the process is from http://tcbanalytics.com/blog/nba-movement-data-R.html#.VnX8d4RiOCQ
  # takes a json and converts it into a dataframe
  the.data.file<-fromJSON(filen)
  ##Get the sports vu data
  moments <- the.data.file$events$moments
  
  ##Function for extracting infomration from JSON
  extractbb <- function (listbb)
  {#df <- unlist(listbb,recursive = FALSE)
    df <- listbb
    # str(df)
    quarters <- unlist(lapply(df, function(x) x[[1]]))
    game.clock <- unlist(lapply(df, function(x) x[[3]]))
    shot.clock <- unlist(lapply(df, function(x) ifelse(is.null(x[[4]]), 'NA', x[[4]])))
    moment.details <- (lapply(df, function(x) x[[6]]))
    x3 <-  mapply(cbind, moment.details, game.clock, shot.clock,quarters, SIMPLIFY=F)
    x4 <- do.call('rbind', x3)
    return (x4)
  }
  
  test2 <- lapply(moments, function (x) {extractbb(x)})
  lengthmm <- the.data.file$events$eventId
  test2 <- mapply(cbind, test2, "event.id"=lengthmm, SIMPLIFY=F)
  
  #Remove events that are NAs
  final <- (lapply(test2, function(x) {
    if ((length(unlist(x)))<=1) {x <- NA} 
    return(x)
  }))
  
  ###Merge the file
  test2 <- do.call('rbind', final)
  test2 <- as.data.frame(test2)
  test2[test2 == "NA" ] = NA
  all.movement <- test2
  #all.movement<-test2[order(test2$game.clock),]
  
  ##Lets join the movement to player id
  headers = c("team_id", "player_id", "x_loc", "y_loc", "radius", "game_clock", "shot_clock", "quarter","event.id")
  colnames(all.movement) <- headers
  all.movement<-data.frame(all.movement)
  all.movement<-all.movement[order(all.movement$game_clock),]
  
  home.players <- the.data.file$events$home$players[[1]]
  away.players <- the.data.file$events$visitor$players[[1]]
  colnames(home.players)[3] <- "player_id"
  colnames(away.players)[3] <- "player_id"
  
  ## Add the player name information to each movement moment
  home.movements<-merge(home.players, all.movement, by="player_id")
  away.movements<-merge(away.players, all.movement, by="player_id")
  ball.movement<-all.movement %>% filter(player_id == -1)
  ball.movement$jersey <- NA
  ball.movement$position <- NA
  ball.movement$team_id <- NA
  ball.movement$lastname <- "ball"
  ball.movement$firstname <- NA
  all.movements <- rbind(home.movements, away.movements,ball.movement)
  all.movements[, 6:13] <- lapply(all.movements[, 6:13], factorconvert)
  all.movements <- as.data.frame(all.movements) %>% dplyr::arrange(quarter,desc(game_clock),x_loc,y_loc)
  all.movements$gameId <- game_id
  return(all.movements)
}
sample_game <- sportvu_convert_json("10.27.2015.NOP.at.GSW.json","201510270GSW")

# expandedSample_df <- rbind(sportvu_convert_json("10.27.2015.NOP.at.GSW.json","201510270GSW"),sportvu_convert_json("10.31.2015.GSW.at.NOP.json","201510310NOP"),sportvu_convert_json("11.02.2015.MEM.at.GSW.json", "201511020GSW"),sportvu_convert_json("11.04.2015.LAC.at.GSW.json","201511040GSW"),sportvu_convert_json("11.06.2015.DEN.at.GSW.json","201511060GSW"),
#                              sportvu_convert_json("11.07.2015.GSW.at.SAC.json", "201511070SAC"),sportvu_convert_json("11.09.2015.DET.at.GSW.json","201511090GSW"),sportvu_convert_json("11.17.2015.TOR.at.GSW.json","201511170GSW"),sportvu_convert_json("11.19.2015.GSW.at.LAC.json","201511190LAC"),
#                              sportvu_convert_json("11.20.2015.CHI.at.GSW.json","201511200GSW"),sportvu_convert_json("11.22.2015.GSW.at.DEN.json","201511220DEN"),sportvu_convert_json("11.24.2015.LAL.at.GSW.json","201511240GSW"),sportvu_convert_json("11.27.2015.GSW.at.PHX.json","201511270PHO"),sportvu_convert_json("11.28.2015.SAC.at.GSW.json","201511280GSW"),
#                              sportvu_convert_json("12.02.2015.GSW.at.CHA.json","201512020CHO"),sportvu_convert_json("12.06.2015.GSW.at.BKN.json","201512060BRK"))
# game_df <- expandedSample_df
game_df <- sample_game
game_df$event.id <- NULL
game_df_distinct <- distinct(game_df) ##getting rid of duplicate rows that occasionally manifest themselves
game_df_distinct$singular_game_clock <- (720*game_df_distinct$quarter)-game_df_distinct$game_clock  ##establishing a comprehensive clock that increases constantly throughout the game

team_id_perserved_df <- game_df_distinct
team_id_volume_df <- as.data.frame(table(game_df_distinct$team_id))
analyzedTeam_id <- as.numeric(as.character(team_id_volume_df[team_id_volume_df$Freq==max(table(game_df_distinct$team_id)),]$Var1))

game_df_distinct$rounded_gameClock <- ceiling(game_df_distinct$singular_game_clock)  #rounding up singular game clock to match the pattern of the game clock in the play-by-play data
game_df_distinct$abbrName <- paste(substring(game_df_distinct$firstname, 1, 1), ".", " ", game_df_distinct$lastname,sep="") #creating abbreviated name index to match naming pattern in play-by-play data
game_df_distinct$numeric_player_id <- as.numeric(game_df_distinct$player_id) 
game_df_distinct <- game_df_distinct[order(game_df_distinct$numeric_player_id,game_df_distinct$gameId,game_df_distinct$singular_game_clock),] 
game_df_distinct$time_diff <- c(0,diff(game_df_distinct$singular_game_clock))

game_df_clock_running <- game_df_distinct[game_df_distinct$time_diff!=0,] #filtering out all instances where the singular game clock is not running (stoppages in gameplay)
game_df_clock_running$speed <- c(0,sqrt(diff(game_df_clock_running$x_loc)^2+diff(game_df_clock_running$y_loc)^2)) #finding player speed at each instant (unit=feet/(second/25))
game_df_clock_running$speed[game_df_clock_running$speed>1.5] <- 0 #filtering out for instances of time jumps as identified by impossible speeds

gameClock_player_distribution <- (aggregate(rounded_gameClock ~ player_id, game_df_clock_running, unique))
gameClock_player_volume <- (aggregate(rounded_gameClock ~ player_id, game_df_clock_running, table))
names(gameClock_player_volume)[2]<-"rounded_gameClock_volume"
player_gameClock_df_nested <- merge(gameClock_player_distribution, gameClock_player_volume, by="player_id")
player_gameClock_df <- unnest(player_gameClock_df_nested)
player_gameClock_df$inGame_freq <- player_gameClock_df$rounded_gameClock_volume/(length(unique(game_df$gameId))*25)
player_gameClock_df[player_gameClock_df$inGame_freq > 1, "inGame_freq"] <- 1.00

inGame_freq_df <- merge(game_df_clock_running, player_gameClock_df, by=c("player_id", "rounded_gameClock"))
inGame_freq_df <- inGame_freq_df[order(inGame_freq_df$player_id, inGame_freq_df$gameId, inGame_freq_df$singular_game_clock),]
inGame_freq_df <- inGame_freq_df[inGame_freq_df$time_diff>0 & inGame_freq_df$time_diff<=0.05,]  ##removing time jumps
inGame_freq_df$time_mult <- inGame_freq_df$time_diff*inGame_freq_df$inGame_freq

allFouls_df <- pbp_df[c(which(grepl("foul by", pbp_df$Home)),which(grepl("foul by", pbp_df$Away))),]
rownames(allFouls_df) <- NULL
personalFouls_df <- allFouls_df[-c((which(grepl("Technical", allFouls_df$Home))),(which(grepl("Def 3 sec", allFouls_df$Home))), 
                                   (which(grepl("Technical", allFouls_df$Away))),(which(grepl("Def 3 sec", allFouls_df$Away)))),] ##filtering for only personal fouls (filtering out technical fouls)
personalFouls_df$foulId <- 1:nrow(personalFouls_df)
personalFouls_df <- personalFouls_df [c((which(grepl("drawn by", personalFouls_df $Home))),(which(grepl("drawn by", personalFouls_df $Away)))),] ##filtering for only personal fouls (filtering out technical fouls)

personal_personalFouls_df <- personalFouls_df[c((which(grepl("Personal foul by ", personalFouls_df$Home))),(which(grepl("Personal foul by", personalFouls_df$Away)))),]
personal_blockFouls_df <- personalFouls_df[c((which(grepl("Personal block foul by ", personalFouls_df$Home))),(which(grepl("Personal block foul by", personalFouls_df$Away)))),]
personal_takeFouls_df <- personalFouls_df[c((which(grepl("Personal take foul by ", personalFouls_df$Home))),(which(grepl("Personal take foul by", personalFouls_df$Away)))),]
personal_looseballFouls_df <- personalFouls_df[c((which(grepl("Loose ball foul by ", personalFouls_df$Home))),(which(grepl("Loose ball foul by", personalFouls_df$Away)))),]
personal_offensiveFouls_df <- personalFouls_df[c((which(grepl("Offensive foul by ", personalFouls_df$Home))),(which(grepl("Offensive foul by", personalFouls_df$Away)))),]
personal_afpFouls_df <- personalFouls_df[c((which(grepl("Away from play foul by ", personalFouls_df$Home))),(which(grepl("Away from play foul by", personalFouls_df$Away)))),]
personal_shootingFouls_df <- personalFouls_df[c((which(grepl("Shooting foul by ", personalFouls_df$Home))),(which(grepl("Shooting foul by", personalFouls_df$Away)))),]
personal_shootingblockFouls_df <- personalFouls_df[c((which(grepl("Shooting block foul by ", personalFouls_df$Home))),(which(grepl("Shooting block foul by", personalFouls_df$Away)))),]


oppNames_personalPersonal_home_vec <-(str_remove(personal_personalFouls_df$Home, ("Personal foul by ")))
oppNames_personalPersonal_away_vec <-(str_remove(personal_personalFouls_df$Away, ("Personal foul by ")))
opp_names_personalPersonal_df <- data.frame(oppNames=c(oppNames_personalPersonal_home_vec, oppNames_personalPersonal_away_vec))
hyphenName_personalPersonal_df <- na.omit(separate(opp_names_personalPersonal_df, oppNames, into=c("def_firstInitial","def_lastName","omitOne","omitTwo","off_firstInitial","off_lastName"), sep=" "))
hyphenName_personalPersonal_df$foulId <- personal_personalFouls_df$foulId
def_abbrName_personalPersonalFoul_vec <- paste(substring(hyphenName_personalPersonal_df$def_firstInitial, 1, 1), ".", " ", hyphenName_personalPersonal_df$def_lastName,sep="")
abbrName_personalPersonalFoul_df <- data.frame(def_abbrName=def_abbrName_personalPersonalFoul_vec, foulId=hyphenName_personalPersonal_df$foulId, gameId=personal_personalFouls_df$gameId)

oppNames_blockPersonal_home_vec <-(str_remove(personal_blockFouls_df$Home, ("Personal block foul by ")))
oppNames_blockPersonal_away_vec <-(str_remove(personal_blockFouls_df$Away, ("Personal block foul by ")))
opp_names_blockPersonal_df <- data.frame(oppNames=c(oppNames_blockPersonal_home_vec, oppNames_blockPersonal_away_vec))
hyphenName_blockPersonal_df <- na.omit(separate(opp_names_blockPersonal_df, oppNames, into=c("def_firstInitial","def_lastName","omitOne","omitTwo","off_firstInitial","off_lastName"), sep=" "))
hyphenName_blockPersonal_df$foulId <- personal_blockFouls_df$foulId
def_abbrName_blockFoul_vec <- paste(substring(hyphenName_blockPersonal_df$def_firstInitial, 1, 1), ".", " ", hyphenName_blockPersonal_df$def_lastName,sep="")
abbrName_blockPersonalFoul_df <- data.frame(def_abbrName=def_abbrName_blockFoul_vec, foulId=hyphenName_blockPersonal_df$foulId, gameId=personal_blockFouls_df$gameId)

oppNames_takePersonal_home_vec <-(str_remove(personal_takeFouls_df$Home, ("Personal take foul by ")))
oppNames_takePersonal_away_vec <-(str_remove(personal_takeFouls_df$Away, ("Personal take foul by ")))
opp_names_takePersonal_df <- data.frame(oppNames=c(oppNames_takePersonal_home_vec, oppNames_takePersonal_away_vec))
hyphenName_takePersonal_df <- na.omit(separate(opp_names_takePersonal_df, oppNames, into=c("def_firstInitial","def_lastName","omitOne","omitTwo","off_firstInitial","off_lastName"), sep=" "))
hyphenName_takePersonal_df$foulId <- personal_takeFouls_df$foulId
def_abbrName_takeFoul_vec <- paste(substring(hyphenName_takePersonal_df$def_firstInitial, 1, 1), ".", " ", hyphenName_takePersonal_df$def_lastName,sep="")
abbrName_takePersonalFoul_df <- data.frame(def_abbrName=def_abbrName_takeFoul_vec, foulId=hyphenName_takePersonal_df$foulId, gameId=personal_takeFouls_df$gameId)

oppNames_looseballPersonal_home_vec <-(str_remove(personal_looseballFouls_df$Home, ("Loose ball foul by ")))
oppNames_looseballPersonal_away_vec <-(str_remove(personal_looseballFouls_df$Away, ("Loose ball foul by ")))
opp_names_looseballPersonal_df <- data.frame(oppNames=c(oppNames_looseballPersonal_home_vec, oppNames_looseballPersonal_away_vec))
hyphenName_looseballPersonal_df <- na.omit(separate(opp_names_looseballPersonal_df, oppNames, into=c("def_firstInitial","def_lastName","omitOne","omitTwo","off_firstInitial","off_lastName"), sep=" "))
hyphenName_looseballPersonal_df$foulId <- personal_looseballFouls_df$foulId
def_abbrName_looseballFoul_vec <- paste(substring(hyphenName_looseballPersonal_df$def_firstInitial, 1, 1), ".", " ", hyphenName_looseballPersonal_df$def_lastName,sep="")
abbrName_looseballPersonalFoul_df <- data.frame(def_abbrName=def_abbrName_looseballFoul_vec, foulId=hyphenName_looseballPersonal_df$foulId, gameId=personal_looseballFouls_df$gameId)

oppNames_offensivePersonal_home_vec <-(str_remove(personal_offensiveFouls_df$Home, ("Offensive foul by ")))
oppNames_offensivePersonal_away_vec <-(str_remove(personal_offensiveFouls_df$Away, ("Offensive foul by ")))
opp_names_offensivePersonal_df <- data.frame(oppNames=c(oppNames_offensivePersonal_home_vec, oppNames_offensivePersonal_away_vec))
hyphenName_offensivePersonal_df <- na.omit(separate(opp_names_offensivePersonal_df, oppNames, into=c("def_firstInitial","def_lastName","omitOne","omitTwo","off_firstInitial","off_lastName"), sep=" "))
hyphenName_offensivePersonal_df$foulId <- personal_offensiveFouls_df$foulId
def_abbrName_offensiveFoul_vec <- paste(substring(hyphenName_offensivePersonal_df$def_firstInitial, 1, 1), ".", " ", hyphenName_offensivePersonal_df$def_lastName,sep="")
abbrName_offensivePersonalFoul_df <- data.frame(def_abbrName=def_abbrName_offensiveFoul_vec, foulId=hyphenName_offensivePersonal_df$foulId, gameId=personal_offensiveFouls_df$gameId)

oppNames_afpPersonal_home_vec <-(str_remove(personal_afpFouls_df$Home, ("Away from play foul by ")))
oppNames_afpPersonal_away_vec <-(str_remove(personal_afpFouls_df$Away, ("Away from play foul by ")))
opp_names_afpPersonal_df <- data.frame(oppNames=c(oppNames_afpPersonal_home_vec, oppNames_afpPersonal_away_vec))
hyphenName_afpPersonal_df <- na.omit(separate(opp_names_afpPersonal_df, oppNames, into=c("def_firstInitial","def_lastName","omitOne","omitTwo","off_firstInitial","off_lastName"), sep=" "))
hyphenName_afpPersonal_df$foulId <- personal_afpFouls_df$foulId
def_abbrName_afpFoul_vec <- paste(substring(hyphenName_afpPersonal_df$def_firstInitial, 1, 1), ".", " ", hyphenName_afpPersonal_df$def_lastName,sep="")
abbrName_afpPersonalFoul_df <- data.frame(def_abbrName=def_abbrName_afpFoul_vec, foulId=hyphenName_afpPersonal_df$foulId,gameId=personal_afpFouls_df$gameId)

oppNames_shootingPersonal_home_vec <-(str_remove(personal_shootingFouls_df$Home, ("Shooting foul by ")))
oppNames_shootingPersonal_away_vec <-(str_remove(personal_shootingFouls_df$Away, ("Shooting foul by ")))
opp_names_shootingPersonal_df <- data.frame(oppNames=c(oppNames_shootingPersonal_home_vec, oppNames_shootingPersonal_away_vec))
hyphenName_shootingPersonal_df <- na.omit(separate(opp_names_shootingPersonal_df, oppNames, into=c("def_firstInitial","def_lastName","omitOne","omitTwo","off_firstInitial","off_lastName"), sep=" "))
hyphenName_shootingPersonal_df$foulId <- personal_shootingFouls_df$foulId
def_abbrName_shootingFoul_vec <- paste(substring(hyphenName_shootingPersonal_df$def_firstInitial, 1, 1), ".", " ", hyphenName_shootingPersonal_df$def_lastName,sep="")
abbrName_shootingPersonalFoul_df <- data.frame(def_abbrName=def_abbrName_shootingFoul_vec, foulId=hyphenName_shootingPersonal_df$foulId, gameId=personal_shootingFouls_df$gameId)

oppNames_shootingblockPersonal_home_vec <-(str_remove(personal_shootingblockFouls_df$Home, ("Shooting block foul by ")))
oppNames_shootingblockPersonal_away_vec <-(str_remove(personal_shootingblockFouls_df$Away, ("Shooting block foul by ")))
opp_names_shootingblockPersonal_df <- data.frame(oppNames=c(oppNames_shootingblockPersonal_home_vec, oppNames_shootingblockPersonal_away_vec))
hyphenName_shootingblockPersonal_df <- na.omit(separate(opp_names_shootingblockPersonal_df, oppNames, into=c("def_firstInitial","def_lastName","omitOne","omitTwo","off_firstInitial","off_lastName"), sep=" "))
hyphenName_shootingblockPersonal_df$foulId <- personal_shootingblockFouls_df$foulId
def_abbrName_shootingblockFoul_vec <- paste(substring(hyphenName_shootingblockPersonal_df$def_firstInitial, 1, 1), ".", " ", hyphenName_shootingblockPersonal_df$def_lastName,sep="")
abbrName_shootingblockPersonalFoul_df <- data.frame(def_abbrName=def_abbrName_shootingblockFoul_vec, foulId=hyphenName_shootingblockPersonal_df$foulId, gameId=personal_shootingblockFouls_df$gameId)


personalFouls_abbrName_df <- rbind(abbrName_personalPersonalFoul_df, abbrName_blockPersonalFoul_df, abbrName_takePersonalFoul_df, abbrName_looseballPersonalFoul_df, abbrName_offensivePersonalFoul_df, abbrName_afpPersonalFoul_df, abbrName_shootingPersonalFoul_df, abbrName_shootingblockPersonalFoul_df)
personalFouls_expanded_df <- merge(personalFouls_df, personalFouls_abbrName_df, by=c("gameId","foulId"))



rownames(personalFouls_df) <- NULL
rownames(personalFouls_abbrName_df) <- NULL
defensiveFouls_df <- personalFouls_df[-c((which(grepl("Loose ball", personalFouls_df$Home))),(which(grepl("Offensive", personalFouls_df$Home))), 
                                         (which(grepl("Loose ball", personalFouls_df$Away))),(which(grepl("Offensive", personalFouls_df$Away)))),] ##filtering for only personal fouls (filtering out technical fouls)
shootingFouls_df <- defensiveFouls_df[c((which(grepl("Shooting foul by ", defensiveFouls_df$Home))),(which(grepl("Shooting foul ", defensiveFouls_df$Away)))),]
oppNames_home_vec <-(str_remove(shootingFouls_df$Home, ("Shooting foul by ")))
oppNames_away_vec <-(str_remove(shootingFouls_df$Away, ("Shooting foul by ")))
opp_names_df <- data.frame(oppNames=c(oppNames_home_vec, oppNames_away_vec))
hyphenName_df <- na.omit(separate(opp_names_df, oppNames, into=c("def_firstInitial","def_lastName","omitOne","omitTwo","off_firstInitial","off_lastName")))
hyphenName_df$foulId <- shootingFouls_df$foulId
off_abbrName_shootingFoul_vec <- paste(substring(hyphenName_df$off_firstInitial, 1, 1), ".", " ", hyphenName_df$off_lastName,sep="")
def_abbrName_shootingFoul_vec <- paste(substring(hyphenName_df$def_firstInitial, 1, 1), ".", " ", hyphenName_df$def_lastName,sep="")
abbrName_shootingFoul_df <- data.frame(abbrName=off_abbrName_shootingFoul_vec, def_abbrName=def_abbrName_shootingFoul_vec,foulId=hyphenName_df$foulId)
shootingFouls_df <- merge(shootingFouls_df, abbrName_shootingFoul_df, by="foulId")

shootingValue_offDribble_single_df <- data.frame(dist1=c(46.5,49.6,42.7,37.8,37.7,32.7,33,30.8,29.5,39.8,45.2,51.3,33.3),dist2=c(49.4,48.5,43.3,38.6,34.4,34.2,34.8,31.2,29.7,31.2,33.9,35.6,38.1),dist3=c(57.2,56.7,50.4,42.1,36.8,35.6,35.3,33.5,35.4,33.4,31.7,31.9,34.1),dist4=c(65.4,66.4,56.7,46.7,40.5,37.7,37,39.5,39.5,37.1,34.8,40.1,44.7),dist5=c(71.4,77.7,62,50.7,43.7,41.7,41.8,43.5,41.5,39.1,38,42.7,45.6),dist6=c(76.2,80.7,71.8,48.6,50,43.1,41.8,46.3,42.5,44.2,37.9,47.9,54.9),dist7=c(79.3,82.9,77.6,54.8,45.2,40.8,49.2,50.6,44.7,42.8,41.7,52.7,52.3),dist8=c(84.6,82.1,77.3,54.4,56.9,35.1,44.8,49.4,49.1,47.6,41.9,54.3,51.6),dist9=c(88,96.7,80,54.5,55.6,55.6,55.3,56.8,43.3,49.1,44.4,52.2,52.7),dist10=c(100,93.8,84,60,54.5,60,40,66.2,42,47.6,44.6,47.6,57.6))
shootingValue_offCatch_single_df <- data.frame(dist1=c(53.8,50.4,52.3,41,35.5,28.8,55,37.5,50,38.1,52.9,57.6,66.7),dist2=c(57.6,57,51.3,43.8,34.1,32.1,33.9,38.7,38.7,36.9,37.3,45.1,44.1),dist3=c(68.5,68,59.4,48.6,40.5,38,41.8,35.6,35.8,36.2,38,47.3,53.1),dist4=c(78.2,77.1,66.2,51,42.5,38.4,39.9,38.7,43.6,38.8,41.9,48.6,48.4),dist5=c(85.2,88.3,68.5,54.6,42.9,45.5,41.3,40.2,41,40.1,40.9,54,54.6),dist6=c(87.9,88.6,76.7,59.3,53.7,46.5,40.7,43.6,45.3,42.2,44.7,55.4,54.6),dist7=c(85.5,94.2,84.2,60.6,51.3,47.4,42.1,47.1,43.4,44.8,43.9,58.4,57.6),dist8=c(95.4,92.4,89.5,57.9,55,52.1,56.4,50.5,45.1,45.5,45.6,62.2,60.7),dist9=c(92.6,97.1,94.9,80,42.1,36,49,48.4,46.8,46,48,65.6,61.7),dist10=c(100,99.5,78.9,63.6,54.5,45.5,45.8,43.9,46.8,45.5,43.9,63.3,61.3))


shootingValue_offDribble_df <- shootingValue_offDribble_single_df[rep(row.names(shootingValue_offDribble_single_df),2), 1:10]
shootingValue_offDribble_df <- shootingValue_offDribble_df[sort(as.numeric(row.names(shootingValue_offDribble_df))), ]
row.names(shootingValue_offDribble_df) <- NULL

shootingValue_offCatch_df <- shootingValue_offCatch_single_df[rep(row.names(shootingValue_offCatch_single_df),2), 1:10]
shootingValue_offCatch_df <- shootingValue_offCatch_df[sort(as.numeric(row.names(shootingValue_offCatch_df))), ]
row.names(shootingValue_offCatch_df) <- NULL

player_values_df <- read.csv("player_values.csv")
names(player_values_df)[1]<-"firstInitial"
player_values_df$abbrName <- paste(substring(player_values_df$firstInitial, 1, 1), ".", " ", player_values_df$lastName,sep="")

library(magicfor)
magic_for(View, silent=TRUE)
for(game_id in unique(inGame_freq_df$gameId))
{
  individualGame_df <- inGame_freq_df[inGame_freq_df$gameId==game_id,]
  individualGame_personalFouls_pbp_df <- personalFouls_expanded_df[personalFouls_expanded_df$gameId==game_id,]
  individualGame_personalFouls_pbp_bonus_df <- individualGame_personalFouls_pbp_df
  
  ###################Projecting Effect on Rotation Minutes
  analyzedTeam_df <- na.omit(individualGame_df[individualGame_df$team_id==analyzedTeam_id,])
  analyzedTeam_playerValues_df <- merge(player_values_df, analyzedTeam_df, by="abbrName")
  
  analyzedTeam_player_id <- unique(analyzedTeam_df$player_id) 
  timeMult_elapsed <- unnest((aggregate(time_mult ~ player_id, analyzedTeam_df, cumsum)))
  secondsPlayed_df <- ((aggregate(time_diff ~ player_id, analyzedTeam_df, sum)))
  timeMult_elapsed_total <- ((aggregate(time_mult ~ player_id, analyzedTeam_df, sum)))
  
  individualGame_personalFouls_pbp_df$foulDenotation <- 1
  colnames(individualGame_personalFouls_pbp_df)[colnames(individualGame_personalFouls_pbp_df)=="def_abbrName"] <- "abbrName"
  individualGame_personalFouls_df <- merge(analyzedTeam_df, individualGame_personalFouls_pbp_df, by=c("rounded_gameClock","abbrName","quarter","gameId"))
  individualGame_personalFouls_df <- individualGame_personalFouls_df[order(individualGame_personalFouls_df$singular_game_clock),]
  
  rowParser <- (aggregate(foulId ~ rounded_gameClock, individualGame_personalFouls_df, length)$foulId)
  cum_rowParser <- cumsum(rowParser)
  rowParser_determinant <- ceiling(((aggregate(foulId ~ rounded_gameClock, individualGame_personalFouls_df, length)$foulId)/2))
  median_rowParser <- cum_rowParser-rowParser_determinant
  median_actualFoulOccurrence <- individualGame_personalFouls_df[c(median_rowParser),]
  row.names(median_actualFoulOccurrence) <- NULL
  
  perPlayer_singular_game_clock <- unnest((aggregate(singular_game_clock ~ player_id, analyzedTeam_df, unique)))
  timeMult_elapsed$singular_game_clock <- perPlayer_singular_game_clock$singular_game_clock
  median_foulOccurrence_timeMult <- merge(timeMult_elapsed, median_actualFoulOccurrence, by=c("singular_game_clock","player_id"))
  
  fouls_perPlayer_df <- ((aggregate(foulDenotation ~ player_id, median_actualFoulOccurrence, sum)))
  foulCounter_df_nested <- ((aggregate(foulDenotation ~ player_id, median_actualFoulOccurrence, cumsum)))
  
  foulCounter_df <- unnest(foulCounter_df_nested)
  foulCounter_df$player_id <- as.numeric(foulCounter_df$player_id)
  foulCounter_df <- foulCounter_df[order(foulCounter_df$player_id, foulCounter_df$foulDenotation),]
  
  
  player_id_PF_df <- ((aggregate(PF ~ player_id, analyzedTeam_playerValues_df, unique)))
  fouls_perPlayer_df$NA_arg <- 6-fouls_perPlayer_df$foulDenotation
  
  foulCounter_df <- merge(foulCounter_df, player_id_PF_df, by="player_id")
  foulCounter_df$PF <- NULL
  
  perPlayer_fouls <- merge(player_id_PF_df, secondsPlayed_df, by="player_id")
  fouls_perSecond_vec <- (perPlayer_fouls$PF/perPlayer_fouls$time_diff) 
  fouls_perSecond_df <- data.frame(fouls_perSecond=fouls_perSecond_vec, player_id=player_id_PF_df$player_id)
  median_foulOccurrence_timeMult_df <- merge(median_foulOccurrence_timeMult, fouls_perSecond_df, by="player_id")

  
  
  player_uniqueFoul_df <- merge((((aggregate(foulId ~ player_id, median_foulOccurrence_timeMult_df, unique)))),(((aggregate(time_mult.x ~ player_id, median_foulOccurrence_timeMult_df, unique)))), by="player_id")
  player_uniqueFoul_df2 <- merge(player_uniqueFoul_df, fouls_perSecond_df, by="player_id")
  player_uniqueFoul_df3 <- merge(player_uniqueFoul_df2, secondsPlayed_df, by="player_id")
  player_foulSorting_df <- unnest(player_uniqueFoul_df3)
  player_foulSorting_df <- merge(player_foulSorting_df, player_id_PF_df, by="player_id")
  player_foulSorting_df$averageFouls <- player_foulSorting_df$PF 
  player_foulSorting_df$player_id <- as.numeric(player_foulSorting_df$player_id)
  player_foulSorting_df <- player_foulSorting_df[order(player_foulSorting_df$player_id, player_foulSorting_df$time_mult.x),]
  player_foulSorting_df$player_id_diff <- c(0,diff(player_foulSorting_df$player_id))
  player_foulSorting_df$expectedFouls <- player_foulSorting_df$time_mult.x/(player_foulSorting_df$time_diff/player_foulSorting_df$averageFouls)
  player_foulSorting_df$expectedFouls_diff <- -1*c((player_foulSorting_df[1,"expectedFouls"]-player_foulSorting_df[1,"averageFouls"]),diff(player_foulSorting_df$expectedFouls))

  player_foulSorting_df2 <- player_foulSorting_df[player_foulSorting_df$player_id_diff>0,]
  player_foulSorting_df3 <- player_foulSorting_df[player_foulSorting_df$player_id_diff==0,]
  player_foulSorting_df2$expectedFouls_diff <- player_foulSorting_df2$averageFouls-player_foulSorting_df2$expectedFouls
  
  player_foulExpectation_diff_df <- rbind(player_foulSorting_df3, player_foulSorting_df2)
  player_foulExpectation_diff_df <- player_foulExpectation_diff_df[order(player_foulExpectation_diff_df$player_id,player_foulExpectation_diff_df$time_mult.x),]
  rownames(player_foulExpectation_diff_df) <- NULL
  player_foulExpectation_diff_df$foulCount <- foulCounter_df$foulDenotation
  player_foulExpectation_diff_df$timeLost <- (1-(player_foulExpectation_diff_df$foulCount/player_foulExpectation_diff_df$expectedFouls))*100
  player_foulExpectation_diff_df$timeLost_diff <- c((player_foulExpectation_diff_df[1,"timeLost"]-0),diff(player_foulExpectation_diff_df$timeLost))
  player_foulExpectation_diff_df2 <- player_foulExpectation_diff_df[player_foulExpectation_diff_df$player_id_diff>0,]
  player_foulExpectation_diff_df2$timeLost_diff <- player_foulExpectation_diff_df2$timeLost-0
  player_foulExpectation_diff_df3 <- player_foulExpectation_diff_df[player_foulExpectation_diff_df$player_id_diff==0,]
  
  final1_rotationImpact_df <- rbind(player_foulExpectation_diff_df2,player_foulExpectation_diff_df3)
  final1_rotationImpact_df <- final1_rotationImpact_df[order(final1_rotationImpact_df$player_id,final1_rotationImpact_df$time_mult.x), ]
  final1_rotationImpact_conv_df <- final1_rotationImpact_df[final1_rotationImpact_df$timeLost>0,]
  final1_rotationImpact_conv_df$timeLost <- 0
  final1_rotationImpact_nonconv_df <- final1_rotationImpact_df[final1_rotationImpact_df$timeLost<=0,]
  final_rotationImpact_df <- rbind(final1_rotationImpact_nonconv_df, final1_rotationImpact_conv_df)
  player_id_BPM_df <- ((aggregate(BPM ~ player_id, analyzedTeam_playerValues_df, unique)))
  
  final_rotationImpact_df <- merge(player_id_BPM_df, final_rotationImpact_df, by="player_id")
  final_rotationImpact_df$finalImpact <- (final_rotationImpact_df$BPM/100)*(2.07*final_rotationImpact_df$timeLost/60)
  View(final_rotationImpact_df$player_id)
  View(final_rotationImpact_df$foulId)
  View(final_rotationImpact_df$timeLost)
  View(final_rotationImpact_df$BPM)
  View(final_rotationImpact_df$finalImpact)
}
final1_df <-magic_result_as_dataframe()
View(final1_df)

magic_for(View, silent=TRUE)
for(game_id in unique(inGame_freq_df$gameId))
{
  individualGame_df <- inGame_freq_df[inGame_freq_df$gameId==game_id,]
  individualGame_personalFouls_pbp_df <- personalFouls_expanded_df[personalFouls_expanded_df$gameId==game_id,]
  individualGame_personalFouls_pbp_bonus_df <- individualGame_personalFouls_pbp_df
  ###################Projecting Bonus Rule Impact
  individualGame_personalFouls_pbp_bonus_df$abbrName <- individualGame_personalFouls_pbp_bonus_df$def_abbrName
  individualGame_teamId <- unique(individualGame_df$team_id)
  analyzedTeam_df <- na.omit(individualGame_df[individualGame_df$team_id==analyzedTeam_id,])
  analyzedTeam_player_id <- unique(analyzedTeam_df$player_id) 
  abbreviatedName_analyzedTeam <- analyzedTeam_df$abbrName
  
  analyzedTeam_foulStatement <- ((paste("foul by", " ", abbreviatedName_analyzedTeam,sep="")))
  analyzedTeam_allFoul_df <- merge(individualGame_personalFouls_pbp_bonus_df, analyzedTeam_df, by=c("rounded_gameClock","abbrName"))
  
  analyzedTeam_personalFoul_df <- individualGame_personalFouls_pbp_df
  analyzedTeam_teamFouls_df <- analyzedTeam_allFoul_df[-c((which(grepl("Offensive", analyzedTeam_allFoul_df$Home))),(which(grepl("Offensive", analyzedTeam_allFoul_df$Away)))),] ##filtering for only personal fouls (filtering out technical fouls)
  rownames(analyzedTeam_teamFouls_df) <- NULL
  analyzedTeam_defensiveFouls_df <- analyzedTeam_allFoul_df[-c((which(grepl("Loose ball", analyzedTeam_allFoul_df$Home))),(which(grepl("Offensive", analyzedTeam_allFoul_df$Home))), 
                                                               (which(grepl("Loose ball", analyzedTeam_allFoul_df$Away))),(which(grepl("Offensive", analyzedTeam_allFoul_df$Away)))),] ##filtering for only personal fouls (filtering out technical fouls)
  
  rowParser2 <- (aggregate(foulId ~ rounded_gameClock, analyzedTeam_teamFouls_df, length)$foulId)
  cum_rowParser2 <- cumsum(rowParser2)
  rowParser_determinant2 <- ceiling(((aggregate(foulId ~ rounded_gameClock, analyzedTeam_teamFouls_df, length)$foulId)/2))
  median_rowParser2 <- cum_rowParser2-rowParser_determinant2
  
  analyzedTeam_teamFouls_medianOccurrence_df <- analyzedTeam_teamFouls_df[c(median_rowParser2),]
  total_analyzedTeam_teamFouls <- nrow(analyzedTeam_teamFouls_medianOccurrence_df)
  total_analyzedTeam_defensiveFoul <- nrow(analyzedTeam_defensiveFouls_df)
  
  
  analyzedTeam_teamFouls_df <- analyzedTeam_teamFouls_medianOccurrence_df[order(analyzedTeam_teamFouls_medianOccurrence_df$rounded_gameClock),]
  analyzedTeam_teamFouls_df$foulDenotation <- 1
  perQuarter_teamFouls_df <- analyzedTeam_teamFouls_df[analyzedTeam_teamFouls_df$seconds>=120,]
  underTwo_teamFouls_df <- analyzedTeam_teamFouls_df[analyzedTeam_teamFouls_df$seconds<=120,]
  
  perQuarter_idAgg <- (aggregate(foulId ~ quarter.x, analyzedTeam_teamFouls_df, unique))
  perQuarter_countAgg <- (aggregate(foulDenotation ~ quarter.x, analyzedTeam_teamFouls_df, cumsum))
  perQuarter_df_nested <- merge(perQuarter_idAgg,perQuarter_countAgg,by="quarter.x")
  perQuarter_df <- unnest(perQuarter_df_nested)
  colnames(perQuarter_df)[colnames(perQuarter_df)=="foulDenotation"] <- "foulsInQuarter"
  fouls_inBonus_df <- perQuarter_df[perQuarter_df$foulsInQuarter>=5,]
  fouls_inBonus_df$inBonus <- TRUE
  fouls_NotinBonus_df <- perQuarter_df[perQuarter_df$foulsInQuarter<5,]
  fouls_NotinBonus_df$inBonus <- FALSE
  perQuarter_df <- rbind(fouls_NotinBonus_df, fouls_inBonus_df)
  perQuarter_df <- perQuarter_df[order(perQuarter_df$quarter.x,perQuarter_df$foulsInQuarter),]
  
  underTwo_idAgg <- (aggregate(foulId ~ quarter.x, underTwo_teamFouls_df, unique))
  underTwo_countAgg <- (aggregate(foulDenotation ~ quarter.x, underTwo_teamFouls_df, cumsum))
  underTwo_df_nested <- merge(underTwo_idAgg,underTwo_countAgg,by="quarter.x")
  underTwo_df <- unnest(underTwo_df_nested)
  colnames(underTwo_df)[colnames(underTwo_df)=="foulDenotation"] <- "foulsInQuarter2"
  fouls_inBonus_df2 <- underTwo_df[underTwo_df$foulsInQuarter2>=2,]
  fouls_inBonus_df2$inBonus <- TRUE
  fouls_NotinBonus_df2 <- underTwo_df[underTwo_df$foulsInQuarter2<2,]
  fouls_NotinBonus_df2$inBonus <- FALSE
  underTwo_df <- rbind(fouls_NotinBonus_df2, fouls_inBonus_df2)
  underTwo_df <- underTwo_df[order(underTwo_df$quarter.x,underTwo_df$foulsInQuarter2),]
  
  underTwo_NotinBonus <- (anti_join(underTwo_df, fouls_inBonus_df, by=c("quarter.x","foulId")))
  perQuarter_overTwo_df <- anti_join(fouls_NotinBonus_df, underTwo_df, by=c("quarter.x","foulId"))
  perQuarter_overTwo_df$foulsInQuarter <- NULL
  underTwo_df$foulsInQuarter2 <- NULL
  
  fouls_inBonus_df$foulsInQuarter <- NULL
  perQuarter_inclusive_df <- rbind(perQuarter_overTwo_df,fouls_inBonus_df)
  perQuarter_merge_df <- data.frame(foulsInQuarter=perQuarter_df$foulsInQuarter,foulId=perQuarter_df$foulId)
  perQuarter_inclusive_df <- merge(perQuarter_inclusive_df, perQuarter_merge_df, by="foulId")
  perQuarter_inclusive_df <- perQuarter_inclusive_df[order(perQuarter_inclusive_df$quarter.x,perQuarter_inclusive_df$foulsInQuarter),]
  
  underTwo_NotinBonus <- merge(underTwo_NotinBonus, perQuarter_merge_df, by="foulId")
  
  perQuarter_expanded <- merge(analyzedTeam_teamFouls_df, perQuarter_inclusive_df, by=c("quarter.x","foulId"))
  underTwo_NotinBonus_expanded <- merge(analyzedTeam_teamFouls_df, underTwo_NotinBonus, by=c("quarter.x","foulId"))
  underTwo_NotinBonus_expanded$bonusProbability <- (underTwo_NotinBonus_expanded$foulsInQuarter2+((underTwo_NotinBonus_expanded$seconds)/160))/3
  underTwo_NotinBonus_expanded$foulsInQuarter2 <- NULL
  perQuarter_expanded$bonusProbability <- (perQuarter_expanded$foulsInQuarter+((perQuarter_expanded$seconds)/160))/9
  perQuarter_expanded <- rbind(perQuarter_expanded, underTwo_NotinBonus_expanded)
  
  perQuarter_expanded_inBonus_df <- perQuarter_expanded[perQuarter_expanded$inBonus==TRUE,]
  perQuarter_expanded_inBonus_df$bonusProbability <- 1
  perQuarter_expanded_NotinBonus_df <- perQuarter_expanded[perQuarter_expanded$inBonus==FALSE,]
  perQuarter_expanded <- rbind(perQuarter_expanded_inBonus_df, perQuarter_expanded_NotinBonus_df)
  perQuarter_expanded$bonus_expectedPoints <- ((perQuarter_expanded$seconds*(3/720))*perQuarter_expanded$bonusProbability)*1.514
  perQuarter_expanded <- perQuarter_expanded[order(perQuarter_expanded$quarter.x,perQuarter_expanded$foulsInQuarter),]
  perQuarter_expanded$quarterDiff <- c(0,diff(perQuarter_expanded$quarter.x))
  perQuarter_expanded$bonus_expectedPoints_diff <- c(perQuarter_expanded[1,"bonus_expectedPoints"],diff(perQuarter_expanded$bonus_expectedPoints))
  
  perQuarter_expanded2 <- perQuarter_expanded[perQuarter_expanded$quarterDiff>0,]
  perQuarter_expanded2$bonus_expectedPoints_diff <- perQuarter_expanded2$bonus_expectedPoints-0.375 ###0.375 is replacement level of bonus_expectedPoints
  perQuarter_expanded3 <- perQuarter_expanded[perQuarter_expanded$quarterDiff==0,]
  perQuarter_expandedFinal <- rbind(perQuarter_expanded2, perQuarter_expanded3)
  perQuarter_expandedFinal <- perQuarter_expandedFinal[order(perQuarter_expandedFinal$quarter.x, perQuarter_expandedFinal$foulsInQuarter),]
  
  View(perQuarter_expandedFinal$player_id)
  View(perQuarter_expandedFinal$foulId)
  View(perQuarter_expandedFinal$bonusProbability)
  View(perQuarter_expandedFinal$bonus_expectedPoints_diff*-1)
}
final2_df <-magic_result_as_dataframe()
View(final2_df) 
magic_for(View, silent=TRUE)
for(game_id in unique(inGame_freq_df$gameId))
{
  individualGame_df <- inGame_freq_df[inGame_freq_df$gameId==game_id,]
  individualGame_personalFouls_pbp_df <- personalFouls_expanded_df[personalFouls_expanded_df$gameId==game_id,]
  individualGame_personalFouls_pbp_bonus_df <- individualGame_personalFouls_pbp_df
  ###################Projecting Shot Value
  individualGame_shootingFoul_df <- shootingFouls_df[shootingFouls_df$gameId==game_id,]
  individualGame_offensive_df <- merge(individualGame_df, individualGame_shootingFoul_df, by=c("rounded_gameClock","abbrName","quarter","gameId"))
  individualGame_df$def_abbrName <- individualGame_df$abbrName
  
  individualGame_defensive_df <- merge(individualGame_df, individualGame_shootingFoul_df, by=c("rounded_gameClock","def_abbrName","quarter","gameId"))
  
  individualGame_offensive_df <- individualGame_offensive_df[order(individualGame_offensive_df$singular_game_clock),]
  individualGame_defensive_df <- individualGame_defensive_df[order(individualGame_defensive_df$singular_game_clock),]
  offensivePlayer_speed_df <- data.frame(offSpeed=individualGame_offensive_df$speed, singular_game_clock=individualGame_offensive_df$singular_game_clock, rounded_gameClock=individualGame_offensive_df$rounded_gameClock)
  
  off_x_loc_df_nested <- ((aggregate(x_loc ~ rounded_gameClock, individualGame_offensive_df, unique)))
  offensivePlayer_coords_df <- unnest(off_x_loc_df_nested)
  names(offensivePlayer_coords_df)[2]<-"off_x_loc"
  off_y_loc_df_nested <- ((aggregate(y_loc ~ rounded_gameClock, individualGame_offensive_df, unique)))
  off_y_loc_df <- unnest(off_y_loc_df_nested)
  offensivePlayer_coords_df$off_y_loc <- off_y_loc_df$y_loc
  
  foulId_df <- data.frame(foulId=unique(individualGame_offensive_df$foulId),rounded_gameClock=unique(individualGame_offensive_df$rounded_gameClock))
  
  offensivePlayer_coords_df$singular_game_clock <- individualGame_offensive_df$singular_game_clock
  offensivePlayer_coords_df$off_abbrName <- individualGame_offensive_df$abbrName
  
  def_x_loc_df_nested <- ((aggregate(x_loc ~ rounded_gameClock, individualGame_defensive_df, unique)))
  defensivePlayer_coords_df <- unnest(def_x_loc_df_nested)
  names(defensivePlayer_coords_df)[2]<-"def_x_loc"
  def_y_loc_df_nested <- ((aggregate(y_loc ~ rounded_gameClock, individualGame_defensive_df, unique)))
  def_y_loc_df <- unnest(def_y_loc_df_nested)
  defensivePlayer_coords_df$def_y_loc <- def_y_loc_df$y_loc
  
  defensivePlayer_coords_df$singular_game_clock <- individualGame_defensive_df$singular_game_clock
  defensivePlayer_coords_df$def_abbrName <- individualGame_defensive_df$def_abbrName
  
  keyPlayer_coords <- merge(offensivePlayer_coords_df, defensivePlayer_coords_df, by=c("singular_game_clock","rounded_gameClock"))
  keyPlayer_coords$dist <- sqrt((keyPlayer_coords$off_x_loc-keyPlayer_coords$def_x_loc)^2+(keyPlayer_coords$off_y_loc-keyPlayer_coords$def_y_loc)^2)
  keyPlayer_coords <- keyPlayer_coords[keyPlayer_coords$dist!=0,]
  keyPlayer_minDist_df <- ((aggregate(dist ~ rounded_gameClock, keyPlayer_coords, min)))
  keyPlayer_minDist_instance_df <- merge(keyPlayer_minDist_df, keyPlayer_coords, by=c("rounded_gameClock", "dist"))
  keyPlayer_minDist_instance_df <- keyPlayer_minDist_instance_df[order(keyPlayer_minDist_instance_df$rounded_gameClock),]
  
  
  leftBasket_dist_vec <- sqrt((keyPlayer_minDist_instance_df$off_x_loc)^2+(keyPlayer_minDist_instance_df$off_y_loc-25)^2)
  rightBasket_dist_vec <- sqrt((keyPlayer_minDist_instance_df$off_x_loc-94)^2+(keyPlayer_minDist_instance_df$off_y_loc-25)^2)
  
  both_basketDist_df <- data.frame(rounded_gameClock=rep(keyPlayer_minDist_instance_df$rounded_gameClock,2),basketDist=c(leftBasket_dist_vec,rightBasket_dist_vec))
  basketDist_df <- ((aggregate(basketDist ~ rounded_gameClock, both_basketDist_df, min)))
  allDist_df <- merge(merge(keyPlayer_minDist_instance_df, basketDist_df, by=c("rounded_gameClock")),offensivePlayer_speed_df, by=c("singular_game_clock","rounded_gameClock"))
  allDist_df$basketDist[allDist_df$basketDist>26] <- 26
  allDist_df$basketDist[allDist_df$dist>10] <- 10
  offCatch_instances_df <- allDist_df[allDist_df$offSpeed<=0.3,]
  offDribble_instances_df <- allDist_df[allDist_df$offSpeed>0.3,]
  
  offCatch_columnParser <- (c(ceiling(offCatch_instances_df$dist)))
  offCatch_rowParser <- (c(ceiling(offCatch_instances_df$basketDist)))
  offDribble_columnParser <- (c(ceiling(offDribble_instances_df$dist)))
  offDribble_rowParser <- (c(ceiling(offDribble_instances_df$basketDist)))
  
  offCatch_expectedPoint_df <- shootingValue_offCatch_df[offCatch_rowParser,offCatch_columnParser]
  offCatch_expectedPoint_vec <- diag(as.matrix(offCatch_expectedPoint_df))
  offDribble_expectedPoint_df <- shootingValue_offDribble_df[offDribble_rowParser,offDribble_columnParser]
  offDribble_expectedPoint_vec <- diag(as.matrix(offDribble_expectedPoint_df))
  
  expectedPoints_df <- data.frame(expectedPoints=c((2*offCatch_expectedPoint_vec/100),(2*offDribble_expectedPoint_vec/100)), rounded_gameClock=c(offCatch_instances_df$rounded_gameClock,offDribble_instances_df$rounded_gameClock),
                                  singular_game_clock=c(offCatch_instances_df$singular_game_clock,offDribble_instances_df$singular_game_clock))
  expectedPoints_df$differential <- expectedPoints_df$expectedPoints-1.514
  foulId_use_df <- merge(keyPlayer_minDist_instance_df, foulId_df, by="rounded_gameClock")
  
  View(foulId_use_df$def_abbrName)
  View(expectedPoints_df$differential)
  View(foulId_use_df$foulId)
}
final3_df <-magic_result_as_dataframe()


names(final1_df)[3]<-"foulId"
names(final2_df)[3]<-"foulId"
names(final3_df)[4]<-"foulId"
final_df_inc <- merge(final1_df, final2_df, by="foulId")
final_df <- merge(final_df_inc, final3_df, by="foulId")
foulId_df <- data.frame(foulId=final_df$foulId)


final1_df_filtered <- merge(final1_df, foulId_df, by="foulId")
names(final1_df_filtered)[2]<-"gameId"
names(final1_df_filtered)[3]<-"player_id"
names(final1_df_filtered)[4]<-"relative_timeLost"
names(final1_df_filtered)[5]<-"BPM"
names(final1_df_filtered)[6]<-"PEV"

final2_df_filtered <- merge(final2_df, foulId_df, by="foulId")
names(final2_df_filtered)[3]<-"player_id"
names(final2_df_filtered)[4]<-"bonusEntrance_Prob"
names(final2_df_filtered)[5]<-"PEV"



final3_df_filtered <- merge(final3_df, foulId_df, by="foulId")
names(final3_df_filtered)[4]<-"PEV"
final3_df_filtered[,3] <- NULL
final3_df_filtered$player_id <- final2_df_filtered$player_id

collective_df <- (data.frame(PEV_rotation=final1_df_filtered$PEV,PEV_bonus=final2_df_filtered$PEV,PEV_shotValue=final3_df_filtered$PEV, summedPEV=final1_df_filtered$PEV+final3_df_filtered$PEV+final2_df_filtered$PEV))

View(collective_df)
