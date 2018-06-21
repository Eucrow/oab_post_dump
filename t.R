compare_day_month_year_date2 <- function(x, day, month, year){
  
  tryCatch({
    
    x <- as.POSIXlt(x, format="%d/%m/%Y") # Date-times known to be invalid will be returned as NA.
    # x <- as.POSIXct(x, format="%d/%m/%Y") # Date-times known to be invalid will be returned as NA.
    
    #check if the date has the correct format
    if( is.na( x ) ){
      warning( paste0("Error in date format. Be sure the format is day/month/year: ", x), call. = F)
      return(FALSE)
    } 
    
    #check if day, month and year match with x
    if ((!missing(day) && (x$mday != day)) || 
        (!missing(month) && (x$mon+1 != month)) ||
        (!missing(year) && (x$year+1900 != year))) {
      return(FALSE) 
    }
    
    return(TRUE)
  })
}

check_year_in_date2 <- function(df, date_field, year){
  
  if (!date_field %in% colnames(df)){
    stop(deparse(substitute(df)), " don't have a ", deparse(substitute(date_field)), " variable.")
  }
  
  l <- sapply(df[[date_field]],
              function(x) {
                return (compare_day_month_year_date2(x, year=year))
              },
              USE.NAMES = F)
  
  errors <- df[!l,]
  
  errors <- errors[, c("ID_MAREA", "YEAR", date_field)]
  
  # this line add a comment to the errors dataframe wich contain the value of the
  # df variable
  errors.date_field <- deparse(substitute(date_field))
  
  errors <- addTypeOfError(errors, "ERROR: el año del campo ", errors.date_field, " no coincide con el año ", year)
  
  return(errors)
  
}

check_year_in_date2(OAB_trips, "FECHA_INI", YEAR_DISCARD)