#' Retrieves Data from the Quandl Datatable endpoint and handles large list inputs
#'
#' 
#' @importFrom Quandl Quandl.datatable
#' @export
Quandl.datatable.extended <- function(table_code, ...) {
  params <- list(...)
  MAX_REQ_LEN = 300
  long_params <- list()
  out_data <- data.frame()
  # Due to http GET requests having finite length, this function splits the larger k-cell of slicing parameters into 
  # smaller cells of each dimension length less than MAX_REQ_LEN. 
  # So if your parameter request space is 1200x1200x1200 and MAX_REQ_LEN = 300, it will split the cube up into 
  # 64 (4**3) spaces of dimension 300x300x300
  # I do not think such an extreme case will happen. Most likely the parameter space would be something like
  # 2000x2x3 which would split up into 6 300x2x3, and one 200x2x3
  # But it would have been harder to code in the non-general case.
  if(length(params) > 0) {
    for(i in 1:length(params)) {
      if(length(params[[i]]) > MAX_REQ_LEN) {
        for( j in 0:floor(length(params[[i]])/MAX_REQ_LEN) ) {
          sliced_params <- params
          sliced_params[[names(params)[i]]] <- params[[names(params)[i]]][(j*MAX_REQ_LEN + 1):((j+1)*MAX_REQ_LEN)]
          out_data <- rbind(
            out_data, 
            do.call(
              Quandl.datatable.extended, 
              c(table_code=table_code, sliced_params)
            )
          )
        }
      }
    }
  }
  if(length(names(out_data)) > 0) {
    return(out_data)
  }
  else {
    return(do.call(
      Quandl.datatable, 
      c(code=table_code, paginate=T, params)
      ))
  }

}