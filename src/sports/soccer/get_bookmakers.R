get_bookmakers <- function(){
	source('src/sports/soccer/api/get_all_bookmakers.R') # KEEP IN FUNCTION DUE TO POTENTIAL NAMING CONFLICT
	rawBookmakers <- get_all_bookmakers()
	iconPaths <- paste0('images/icon_', rawBookmakers$name %>% stringr::str_replace(' ', '') %>% tolower(), '.jpg')
	bookmakers <- data.frame(BookmakerId = rawBookmakers$id,
							 BookmakerName = rawBookmakers$name,
							 BookmakerIconPath = iconPaths)
	return(bookmakers)
}