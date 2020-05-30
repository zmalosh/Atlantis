library(tidyverse)

get_competitions <- function(){
	source('src/sports/baseball/api/get_all_competitions.R')

	rawComps <- get_all_competitions()

	comps <- NULL
	for(i in 1:nrow(rawComps)){
		rawComp <- rawComps[i,]
		rawSeasons <- rawComp$seasons[[1]]
		c <- data.frame(
			LeagueId = paste0(rawComp$id, '_', rawSeasons$season),
			LeagueName = rawComp$name,
			LeagueSeason = as.character(rawSeasons$season),
			DisplayName = paste0(rawSeasons$season, ' - ', rawComp$name, ' (', rawComp$id, '_', rawSeasons$season, ')'),
			CountryName = rawComp$country$name,
			CountryCode = rawComp$country$code,
			FlagUrl = rawComp$country$flag,
			LogoUrl = rawComp$logo,
			SeasonStart = rawSeasons$start,
			SeasonEnd = rawSeasons$end,
			LeagueType = rawComp$type,
			IsCurrent = rawSeasons$current,
			stringsAsFactors = FALSE
		)

		if(is.null(comps)){
			comps <- c
		} else {
			comps <- rbind(comps, c)
		}
	}

	filteredComps <- comps %>% filter(IsCurrent & toupper(LeagueType) == 'LEAGUE') %>% select(-LeagueType, -IsCurrent)

	return (filteredComps)
}