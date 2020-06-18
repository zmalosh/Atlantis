require(ggplot2)
require(ggimage)
require(ggthemes)

team_game_excitement_quadrants <- function(homeTeamIds, awayTeamIds, homeScores, awayScores, homeLogoUrls, awayLogoUrls){
	teamGames <- data.frame(Team = c(homeTeamIds, awayTeamIds),
							ScoreFor = c(homeScores, awayScores),
							ScoreAgainst = c(awayScores, homeScores))

	teamLogos <- data.frame(Team = c(homeTeamIds, awayTeamIds), Logo = c(homeLogoUrls, awayLogoUrls)) %>% distinct()

	teamValues <- teamGames %>%
		group_by(Team) %>%
		summarise(ScoreTotal = sum(ScoreFor + ScoreAgainst),
				  AbsScoreDiff = sum(abs(ScoreFor - ScoreAgainst)),
				  Games = n()) %>%
		mutate(AvgScoreTotal = ScoreTotal / Games,
			   AvgScoreDiff = AbsScoreDiff / Games) %>%
		inner_join(teamLogos, by = 'Team') %>%
		select(Team, AvgScoreTotal, AvgScoreDiff, Games, ScoreTotal, AbsScoreDiff, Logo) %>%
		arrange(-AvgScoreTotal, -AvgScoreDiff)

	avgLeagueScoreTotal <- mean(teamValues$AvgScoreTotal)
	avgLeagueScoreDiff <- mean(teamValues$AvgScoreDiff)

	maxDecimalPlaces <- 3
	ql.col <- 'red' # Quadrant Label Color
	ql.sz <- 10 # Quadrant Label (Font) Size
	ql.alpha = 0.3 # Quadrant Label Alpha
	rl.size = 0.25

	avgScoreTotalLabel <- format(round(avgLeagueScoreTotal, digits = maxDecimalPlaces), nsmall = maxDecimalPlaces)
	avgScoreDiffLabel <- format(round(avgLeagueScoreDiff, digits = maxDecimalPlaces), nsmall = maxDecimalPlaces)

	p <- ggplot(teamValues, aes(AvgScoreTotal, AvgScoreDiff)) +
		geom_image(aes(image = Logo), size = 0.075) +
		geom_vline(xintercept = avgLeagueScoreTotal, color = 'blue', size = rl.size) +
		geom_hline(yintercept = avgLeagueScoreDiff, color = 'blue', size = rl.size) +
		coord_fixed(ratio = 1) +
		scale_x_continuous(breaks = seq(from = 0, to = 200, by = 0.25)) +
		scale_y_continuous(breaks = seq(from = 0, to = 200, by = 0.25)) +
		labs(
			title = 'Game Excitment: Game Total vs Game Margin',
			subtitle = paste('Avg Game Total:', avgScoreTotalLabel, '- Avg Game Margin:', avgScoreDiffLabel)
		) +
		xlab('Avg Game Total') +
		ylab('Avg Absolute Game Score Difference') +
		annotation_custom(grobTree(textGrob('Electric', x = 0.95, y = 0.05, gp = gpar(col = ql.col, fontsize = ql.sz, alpha = ql.alpha)))) +
		annotation_custom(grobTree(textGrob('Tactical', x = 0.05, y = 0.95, gp = gpar(col = ql.col, fontsize = ql.sz, alpha = ql.alpha)))) +
		annotation_custom(grobTree(textGrob('Slaughter', x = 0.95, y = 0.95, gp = gpar(col = ql.col, fontsize = ql.sz, alpha = ql.alpha)))) +
		annotation_custom(grobTree(textGrob('Grind', x = 0.05, y = 0.05, gp = gpar(col = ql.col, fontsize = ql.sz, alpha = ql.alpha)))) +
		theme_fivethirtyeight(base_size = 10, base_family = 'sans') +
		scale_color_fivethirtyeight() +
		theme(axis.title = element_text())

	return(p)
}