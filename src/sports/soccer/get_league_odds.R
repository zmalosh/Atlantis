source('requirements.R')
source('src/sports/soccer/api/get_odds_by_league_id.R')

get_league_odds <- function(leagueId){
	get_bet_values_from_bet <- function(dfBet, gameId, bookmakerName){
		betLabelName <- dfBet$label_name
		currentBetValues <- as.data.frame(dfBet$values)
		resultBetValues <- data.frame(
			GameId = gameId,
			BookmakerName = bookmakerName,
			BetTypeName = betLabelName,
			BetName = currentBetValues$value,
			DecimalValue = currentBetValues$odd,
			stringsAsFactors = FALSE
		)
		return(resultBetValues)
	}

	get_bet_values_from_bookmaker <- function(dfBookmaker, gameId){
		bookmakerName <- dfBookmaker$bookmaker_name
		currentBets <- as.data.frame(dfBookmaker$bets[[1]])

		result <- purrr::map_df(1:nrow(currentBets),
								function(.x, b = currentBets){
									return(get_bet_values_from_bet(b[.x,], gameId, bookmakerName))
								})

		return(result)
	}

	get_bet_values_from_fixture <- function(rawOdds, idxFixture){
		gameId <- rawOdds$fixture$fixture_id[idxFixture]
		bookmakers <- as.data.frame(rawOdds$bookmakers[idxFixture])
		bookmakerCount <- nrow(bookmakers)
		result <- purrr::map_df(1:bookmakerCount, function(.x, b = bookmakers){
			return(get_bet_values_from_bookmaker(b[.x,], gameId))
		})
		return(result)
	}

	rawOdds <- as.data.frame(get_odds_by_league_id(leagueId))

	result <- purrr::map_df(1:nrow(rawOdds$fixture), function(.x, r = rawOdds){
		return(get_bet_values_from_fixture(r, .x))
	})

	return(result)
}
