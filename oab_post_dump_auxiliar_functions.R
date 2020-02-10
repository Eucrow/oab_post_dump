# Add variable with type of error to a dataframe.
addTypeOfError <- function(df, ...){
  
  arguments <- list(...)
  
  type <- paste(arguments, collapse = '', sep = " ")
  
  if(nrow(df)!=0){
    # df[["TIPO_ERROR"]] <- type
    df <- df %>% mutate(TIPO_ERROR = type)
  }
  return(df)
}

#' Split COD_MAREA field.
#' Split COD_MAREA field in its components: identifier, year, month, day and 
#' special
#' @param df daraframe with COD_MAREA field to split. The dataframe must have a
#' field called COD_MAREA.
#' @return character vector list with the components of the COD_MAREA.
#' @export
split_COD_MAREA <- function(df) {
  
  if (!("COD_MAREA" %in% colnames(df))) {
    stop(paste("COD_MAREA field does not exists in", deparse(substitute(df))))
  }
  
  # to get the date
  date_indexes <- regexec("[0-9]+", df$COD_MAREA)
  
  # NOTE: this function doesn't check the date format...
  date_from_COD_MAREA <- regmatches(df$COD_MAREA, date_indexes)
  year_from_COD_MAREA <- substr(date_from_COD_MAREA, 1, 2)
  month_from_COD_MAREA <- substr(date_from_COD_MAREA, 3, 4)
  day_from_COD_MAREA <-  substr(date_from_COD_MAREA, 5, 6)
  
  # to get the identifier: DESIXAC...
  identifier_indexes <- regexpr("([a-zA-Z]+)(?=[0-9]+)", df$COD_MAREA, perl = T)
  
  # to get the final special character
  special_idexes <- regexec("(?:\\D+\\d+)([a-zA-z])", df$COD_MAREA)
  special_idexes <- regmatches(df$COD_MAREA, special_idexes)
  # OMG: https://stackoverflow.com/questions/23758858/how-can-i-extract-elements-from-lists-of-lists-in-r
  special_idexes <- lapply(special_idexes, '[', c(2))
  special_idexes <- as.character(special_idexes)
  
  result <- list(
    identifier= regmatches(df$COD_MAREA, identifier_indexes),
    year= year_from_COD_MAREA,
    month= month_from_COD_MAREA,
    day= day_from_COD_MAREA,
    special= special_idexes
  )
  
  # df <- list(
  #   identifier= substr(df$COD_MAREA, 1, 7),
  #   year= substr(df$COD_MAREA, 8, 9),
  #   month= substr(df$COD_MAREA, 10, 11),
  #   day= substr(df$COD_MAREA, 12, 13),
  #   special= substr(df$COD_MAREA, 14, 14)
  # )
  
  # convert two digits year to four digits year:
  result[["year"]] <- format(as.Date(result[["year"]], "%y"), "%Y")
  
  return(result)
}


#' Convert date in format 12-ENE-17 from 10/01/2017
#' @param dates: vector with dates.
#' @return vector with dates corrected.
#' @export
dby_to_dmy_date_format <- function (dates){
  
  # to avoid some problems with Spanish_Spain.1252 (or if you are using another
  # locale), change locale to Spanish_United States.1252:  
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME","Spanish_United States.1252")
  
  dates <- format(as.Date(dates, "%d-%b-%y"), "%d/%m/%Y")
  
  # and now the come back to the initial configuration of locale:
  Sys.setlocale("LC_TIME", lct)  
  
  return(dates)
}

#' Check code: NONE, is called by other functions
#' Check whether a date match with a given day, month and year.
#' If dd, mm or yyyy are ommited the function check the variable given.
#' @param x: date to check. Must have this format: dd/mm/yyyy
#' @param dd: day
#' @param mm: month
#' @param yyyy: year
#' @return TRUE if date does match and FALSE if doesn't.
#' @export
compare_day_month_year_date <- function(x, day, month, year){
  
  tryCatch({
    
    if( is.na(x) ){
      
      # detect if x is saved in dataframe as NA
      warning("There is a empty date")
      return (FALSE)
      
    } else {
      
      x <- as.POSIXlt(x, format="%d/%m/%Y") # Date-times known to be invalid will be returned as NA.
      
      #check if the date has the correct format
      if( is.na( x ) ){
        stop( paste0("Error in date format. Be sure the format is day/month/year: ", x), call. = F)      
      } 
      
      #check if day, month and year match with x
      if ((!missing(day) && (x$mday != day)) || 
          (!missing(month) && (x$mon+1 != month)) ||
          (!missing(year) && (x$year+1900 != year))) {
        return(FALSE) 
      }
      
      return(TRUE)
      
    }
    

  })
}

#' Check code: NONE, is called by other function
#' Check year of a date field
#' @param df: dataframe with the date field.
#' @param date_field: name of the field with de date to check.
#' @param year: year to compare with.
#' @return df with errors
#' @export
check_year_in_date <- function(df, date_field, year){
  
  if (!date_field %in% colnames(df)){
    stop(deparse(substitute(df)), " don't have a ", deparse(substitute(date_field)), " variable.")
  }
  
  l <- sapply(df[[date_field]],
              function(x) {
                return (compare_day_month_year_date(x, year=year))
              },
              USE.NAMES = F)
  
  errors <- df[!l,]
  
  errors <- errors[, c("COD_MAREA", "YEAR", date_field)]
  
  # this line add a comment to the errors dataframe wich contain the value of the
  # df variable
  errors.date_field <- deparse(substitute(date_field))
  
  errors <- addTypeOfError(errors, "ERROR: el año del campo ",
                           errors.date_field, " no coincide con el año ", year)
  
  return(errors)
  
}

#' Check code: NONE, is called by other function
#' Check if a date match with the COD_MAREA date
#' @param df: dataframe to check.
#' @param field: name of the field with de date to check.
#' @return df with errors
#' @export
check_date_with_COD_MAREA <- function(df, field){
  
  indexes <- regexec("[0-9]+", df$COD_MAREA)
  
  date_from_COD_MAREA <- split_COD_MAREA(df)
  
  year_from_COD_MAREA <- date_from_COD_MAREA$year
  month_from_COD_MAREA <- date_from_COD_MAREA$month
  day_from_COD_MAREA <-  date_from_COD_MAREA$day
  
  date_form_COD_MAREA <- paste0(day_from_COD_MAREA, "/", month_from_COD_MAREA, "/", year_from_COD_MAREA)
  
  errors <- df[date_form_COD_MAREA != df[[field]],]
  
  if (length(errors$COD_MAREA)>0){
    errors.date_field <- deparse(substitute(field))
    errors <- addTypeOfError(errors, "WARNING: the date of field ", errors.date_field, " doesn't match with COD_MAREA")
    return(errors)
  } else {
    return()
  }
  
}

#' Variable exists in a dataframe.
#' Check if a variable exists in a dataframe
#' @return TRUE if the variable exists. Otherwise return an error.
#' @param variable: variable to check.
#' @param df: dataframe to check
#' @export
variable_exists_in_df <- function (variable, df){
  
  # get all the variables of df with the variable name = variable
  var_in_df <- colnames(df)[colnames(df) %in% variable]
  
  if (length(var_in_df) > 1) {
    stop(paste("Hey hard worker! check the ", variable, 
               "variable. Looks like there are multiple columns with the same variable name.
               Using consistents dataframes we will get a better world. Really :) ", 
               variable, "."))
  } else if (length(var_in_df) == 0) {
    stop(paste(variable, " does not exists in this dataframe."))
  } else return (TRUE)
  
}

#' Multiple variables exists in a dataframe.
#' Check if multiple variables exists in a dataframe
#' @param variables: vector with variables to check.
#' @param df: dataframe to check
#' @return TRUE if all the variables exists. Otherwise return a list with errors.
#' @export
variables_in_df <- function(variables, df){
  
  variables <- as.list(variables)
  
  result <- lapply(
    variables, 
    function(x){
      result <- tryCatch(
        variable_exists_in_df(x, df),
        error = function(e){ #here, instead of stop with the error of variable_exists_in_df, save it in result variable
          return(e) }
      )
    })
  
  return(result)
  
}

#' Get the caracteristics_arte dataframe.
#' function to get the dataframe caracteristicas_arte, which is related to the
#' characteristics for every gear
get_gear_characteristics <- function(){
  
  caracteristicas_arte <- read.table("caracteristicas_arte.csv", 
                                     header = T, 
                                     sep=";",
                                     colClasses = c("factor", "factor"))
  
  return(caracteristicas_arte)
}

#' Variable or variables of a dataframe contain empty values.
#' Check if a variable or variables of a dataframe contain empty values.
#' Some variables are mandatory according to the gear. This information is saved
#' in file caracteristicas_arte.csv. This variables are checked only if they are
#' in this file.
#' TODO: split this in two functions: one for the rest of variables and the other
#' one fot the mandatory variables by gear.
#' @param df: dataframe to check
#' @param variables: vector with variables to check.
#' @return A list with a dataframe of every variable with empty values. Every
#' dataframe contains erroneus rows.
#' @export
check_empty_values_in_variables <- function (df, variables){
  
  try(variables_in_df(df, variables))
  
  gear_characteristics <- get_gear_characteristics()
  characteristic_to_check <- levels(gear_characteristics$CARACTERISTICA)
  
  variables <- as.list(variables)
  
  errors <- lapply(variables, function(x){
    
    # Only some characteristicas must be filled according to its gear:
    if(x %in% characteristic_to_check) {
      gear_code <- gear_characteristics[gear_characteristics[["CARACTERISTICA"]]==x,]
      error <- (df[ (df[[x]]=="" | is.na(df[[x]])) & df[["COD_ARTE"]]%in%gear_code[["COD_ARTE"]],])
      error <- addTypeOfError(error, "ERROR: ", x, " variable is empty.")
    } else {
      error <- (df[df[[x]]=="" | is.na(df[[x]]),])
      if (nrow(error)>0){
        error <- addTypeOfError(error, "ERROR: ", x, " variable is empty.")
      }
    }
    
    if (nrow(error) > 0){
      # select only interested variables
      if ("COD_LANCE" %in% colnames(error)){
        error <- error[, c("COD_MAREA", "COD_LANCE", "TIPO_ERROR")]
      } else {
        error <- error[, c("COD_MAREA", "TIPO_ERROR")]
      }
      #remove duplicated rows
      error <- unique(error)
    }
    
    error
    
  })
  
  errors <- Filter(function(x) nrow(x) > 0, errors)
  
  
  if(length(errors) == 0){
    return(NULL)
  } else{
    return (errors)
  }
  
}


#' Data of a dataframe is empty.
#' Check if a data of a dataframe is empty (is NA or "")
#' @param field Field to check
#' @return TRUE if the field is empty. FALSE if doesn't.
check_empty_field <- function(field){
  if(field =="" | is.na(field)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Format the errors produced.
# This function combine all the dataframes of the errors_list (a list of dataframes)
# and format it:
# - combine all dataframes in one
# - order columns
formatErrorsList <- function(errors_list = ERRORS){
  
  if (length(errors_list) == 0){
    stop("There aren't any errors.")
  }
  
  # Combine all the dataframes of ERRORS list:
  # Reduce uses a binary function to successively combine the elements of a
  # given vector. In this case, merge the dataframes in the ERRORS list
  #errors <- Reduce(function(x, y) merge(x, y, all=TRUE), errors_list)
  
  #better with join_all form plyr package because doesn't change the order of columns:
  errors <- join_all(errors_list, type = "full")
  
  # Order columns
  errors <- errors %>%
    select(-one_of("TIPO_ERROR"), one_of("TIPO_ERROR")) %>% #remove TIPO_ERROR, and add it to the end
    arrange(COD_MAREA, COD_LANCE)
  
  # Add column Comprobado
  errors[["comprobado"]] <- ""
  
  return(errors)
}

#' Get the different survey acronyms
#' All the COD_MAREA of discards samples starts with an acronym. This function
#' get all the acronyms available in a dataframe
#' @param df dataframe with COD_MAREA variable
#' @return character vector with the acronyms stored in df 
get_base_acronyms <- function(df){
  
  base_acronym <- lapply(as.character(df[["COD_MAREA"]]), function(x){
    
    spl <- strsplit(x, "\\d")
    
    spl <- unlist(spl)
    
    return(spl[[1]])
    
  })
  
  base_acronym <- unique(base_acronym)
  base_acronym <- unlist(base_acronym)
  
  return(base_acronym)
}

#' Separate a dataframe by base acronym.
#' All the COD_MAREA of discards samples starts with an acronym. Whit this
#' function, a dataframe is split by the base acronym and stored in a list.
#' @param df dataframe to split. Df must contain variable "COD_MAREA"
#' @return list of dataframes each of them named as its base acronym.
separate_df_by_acronym <- function(df){
  
  try(variable_exists_in_df("COD_MAREA", df))
  
  acronyms <- get_base_acronyms(df)
  
  p <- lapply(acronyms, function(acro, df){
    
    df[["COD_MAREA"]] <- as.character(df[["COD_MAREA"]])
    assign(acro, df[grepl(paste0("^", acro, "\\d"), df[["COD_MAREA"]]), ])
    return(get(acro))
    
    
  }, df)
  
  
  names(p) <- acronyms
  
  return(p)
  
}