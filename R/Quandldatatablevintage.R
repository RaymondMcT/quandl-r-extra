#' Retrieves Data from the Quandl Datatable endpoint and formats with vintaging
#'
#' @importFrom Quandl Quandl.datatable
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by_
#' @importFrom dplyr do
#' @export
Quandl.datatable.vintage <- function(code, qopts.vintage_col, ...) {
	# Helper function to return maximum value based on vintage column
	gen_max_row_fun <- function(qopts.vintage_col) {
		return (function(rows) {
			rows[rows[[qopts.vintage_col]] == max(rows[[qopts.vintage_col]]),]
		})
	}
	orig_data <- Quandl.datatable(code, ...)
	primary_key <- quandl.api(paste('datatables', code, 'metadata', sep='/'))$datatable$primary_key
	max_row <- gen_max_row_fun(qopts.vintage_col)
	symbol_key <- lapply(primary_key[primary_key != qopts.vintage_col], as.symbol)
	# This next row is the bulk of the function
	# The data is grouped by the primary key - the vintaging column, then the maximum is taken over that
	# For example:
	# ticker | date       | obs_date   | value
	# AAPL   | 2016-01-02 | 2016-01-02 | 12
	# AAPL   | 2016-01-02 | 2016-01-30 | 16
	# AAPL   | 2016-01-03 | 2016-01-03 | 14
	# primary_key = c('ticker','date','obs_date')
	# qopts.vintage_col = 'obs_date'
	# The new primary key of the transformed data is c('ticker','date')
	# ticker | date       | obs_date   | value
	# AAPL   | 2016-01-02 | 2016-01-30 | 16
	# AAPL   | 2016-01-03 | 2016-01-03 | 14

	vintaged <- orig_data %>% group_by_(.dots=symbol_key) %>% do(max_row(.))
	return(as.data.frame(vintaged))
}

