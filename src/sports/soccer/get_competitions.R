library(tidyverse)

get_competitions <- function(){
	source('src/sports/soccer/api/get_all_competitions.R')

	rawComps <- get_all_competitions()
	filteredComps <- rawComps %>% filter(is_current == 1 & toupper(type) == 'LEAGUE')

	competitions <- data.frame(
		LeagueId = filteredComps$league_id,
		LeagueName = filteredComps$name,
		LeagueSeason = filteredComps$season,
		DisplayName = paste0(filteredComps$season, ' - ', filteredComps$name, ' (', filteredComps$league_id, ')'),
		CountryName = filteredComps$country,
		CountryCode = ifelse(filteredComps$country == 'World', 'XX', filteredComps$country_code),
		FlagUrl = filteredComps$flag,
		LogoUrl = filteredComps$logo,
		SeasonStart = filteredComps$season_start,
		SeasonEnd = filteredComps$season_end,
		stringsAsFactors = FALSE
	)

	return (competitions)
}