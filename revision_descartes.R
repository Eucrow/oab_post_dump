#### ---------------------------------------------------------------------------
#### Check discards from SIRENO
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
####
#### files required: 
####
#### ---------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# #### PACKAGES ################################################################
# ------------------------------------------------------------------------------

library(dplyr)
library(devtools)
# remove.packages("sapmuebase")
# install("F:/misdoc/sap/sapmuebase")
library(sapmuebase)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# YOU HAVE ONLY TO CHANGE THIS VARIABLES 

PATH_FILES <- "F:/misdoc/sap/revision_descartes/data"
trips_file <- "IEODESMAREAMARCO.TXT" 
hauls_file <- "IEODESLANCEMARCO.TXT"
catches_file <- "IEODESCAPTURAMARCO_prueba.TXT"
lengths_file <- "IEODESTALLASMARCO.TXT"

YEAR_DISCARD <- "2017"


# ------------------------------------------------------------------------------
# #### GLOBAL VARIABLES ########################################################
# ------------------------------------------------------------------------------

# list with the common fields used in all tables
BASE_FIELDS <- c("YEAR", "ID_MAREA")

# list with all errors found in dataframes:
ERRORS <- list()

# ------------------------------------------------------------------------------
# #### SET WORKING DIRECTORY ###################################################
# ------------------------------------------------------------------------------

setwd("F:/misdoc/sap/revision descartes/")


# ------------------------------------------------------------------------------
# #### FUNCTIONS ###############################################################
# ------------------------------------------------------------------------------

# function to add variable with type of error to a dataframe -------------------
addTypeOfError <- function(df, ...){
  
  arguments <- list(...)
  
  type <- paste(arguments, collapse = '', sep = " ")
  
  if(nrow(df)!=0){
    # df[["TIPO_ERROR"]] <- type
    df <- df %>% mutate(TIPO_ERROR = type)
  }
  return(df)
}

# ------------------------------------------------------------------------------
#' Check if the YEAR variable of the dataframe match with year to stydy.
#' 
#' @param df daraframe to check the field YEAR. The dataframe must have a field
#' called YEAR.
#' @return df with errors
#' @export
check_field_year <- function(df) {
  errors <- df %>%
    select(YEAR, ID_MAREA) %>%
    filter(YEAR != YEAR_DISCARD) %>%
    addTypeOfError("ERROR: The field YEAR doesn't match with the year to check:",YEAR_DISCARD)
  
  return(errors)
}
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Split ID_MAREA field
#' 
#' Split ID_MAREA field in its components: identifier, year, month, day and 
#' special
#' 
#' @param df daraframe with ID_MAREA field to split. The dataframe must have a
#' field called ID_MAREA.
#' @return character vector list with the components of the ID_MAREA.
#' @export

split_ID_MAREA <- function(df) {
  
  if (!("ID_MAREA" %in% colnames(df))) {
    stop(paste("ID_MAREA field does not exists in", deparse(substitute(df))))
  }
  
  # to get the date
  date_indexes <- regexec("[0-9]+", df$ID_MAREA)
  
  # NOTE: this function doesn't check the date format...
  date_from_id_marea <- regmatches(df$ID_MAREA, date_indexes)
  year_from_id_marea <- substr(date_from_id_marea, 1, 2)
  month_from_id_marea <- substr(date_from_id_marea, 3, 4)
  day_from_id_marea <-  substr(date_from_id_marea, 5, 6)
  
  # to get the identifier: DESIXAC...
  identifier_indexes <- regexpr("([a-zA-Z]+)(?=[0-9]+)", df$ID_MAREA, perl = T)
  
  # to get the final special character
  special_idexes <- regexec("(?:\\D+\\d+)([a-zA-z])", df$ID_MAREA)
  special_idexes <- regmatches(df$ID_MAREA, special_idexes)
  # OMG: https://stackoverflow.com/questions/23758858/how-can-i-extract-elements-from-lists-of-lists-in-r
  special_idexes <- lapply(special_idexes, '[', c(2))
  special_idexes <- as.character(special_idexes)
  
  result <- list(
    identifier= regmatches(df$ID_MAREA, identifier_indexes),
    year= year_from_id_marea,
    month= month_from_id_marea,
    day= day_from_id_marea,
    special= special_idexes
  )

  # df <- list(
  #   identifier= substr(df$ID_MAREA, 1, 7),
  #   year= substr(df$ID_MAREA, 8, 9),
  #   month= substr(df$ID_MAREA, 10, 11),
  #   day= substr(df$ID_MAREA, 12, 13),
  #   special= substr(df$ID_MAREA, 14, 14)
  # )
  
  # convert two digits year to four digits year:
  result[["year"]] <- format(as.Date(result[["year"]], "%y"), "%Y")
  
  return(result)

}


# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Convert date in format 12-ENE-17 to 10/01/2017
#' 
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
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
#' Check year in ID_MAREA in a OAB df
#' 
#' @param df: df to check. Must be a df obtained by importOAB functions.
#' @return df with errors
#' @export
check_year_in_ID_MAREA <- function(df){
  
  ID_MAREA_split <- split_ID_MAREA(df)

  errors <- which(!(ID_MAREA_split[["year"]] %in% YEAR_DISCARD))

  errors <- df[errors,]
  
  # this line add a comment to the errors dataframe wich contain the value of the
  # df variable
  errors.name <- deparse(substitute(df))
  
  errors <- errors %>%
            select(YEAR, ID_MAREA)%>%
            addTypeOfError(paste("ERROR: el año del ID_MAREA en", errors.name, "no coincide con el año a comprobar"))
  
  return(errors)

}
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Check whether a date match with a given day, month and year.
#' 
#' If dd, mm or yyyy are ommited the function check the variable given.
#' 
#' @param x: date to check. Must be with the spanish format: dd/mm/yyyy
#' @param dd: day
#' @param mm: month
#' @param yyyy: year
#' @return TRUE if date does match and FALSE if doesn't.
#' @export

compare_day_month_year_date <- function(x, day, month, year){
  
  tryCatch({
  
    x <- as.POSIXlt(x, format="%d/%m/%Y") # Date-times known to be invalid will be returned as NA.

    #check if the date has the correct format
    if( is.na( x ) ) stop( "Error in date format. Be sure the format is day/month/year", call. = F)
      
    #check if day, month and year match with x
    if ((!missing(day) && (x[["mday"]] != day)) || 
        (!missing(month) && (x[["mon"]]+1 != month)) ||
        (!missing(year) && (x[["year"]]+1900 != year))) {
      return(FALSE) 
    }
    
    return(TRUE)
    
  })
  
}

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Check year of a date field
#' 
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

  # this line add a comment to the errors dataframe wich contain the value of the
  # df variable
  errors.date_field <- deparse(substitute(date_field))
  
  errors <- addTypeOfError(errors, "ERROR: el año del campo ", errors.date_field, " no coincide con el año ", year)
    
  return(errors)
  
}


# ------------------------------------------------------------------------------
#' Check if a date match with the ID_MAREA date
#' 
#' @param df: dataframe to check.
#' @param field: name of the field with de date to check.
#' @return df with errors
#' @export
check_date_with_id_marea <- function(df, field){
  
  indexes <- regexec("[0-9]+", df$ID_MAREA)
  
  date_from_id_marea <- split_ID_MAREA(df)
  
  # date_from_id_marea <- regmatches(df$ID_MAREA, indexes)
  year_from_id_marea <- date_from_id_marea$year
  month_from_id_marea <- date_from_id_marea$month
  day_from_id_marea <-  date_from_id_marea$day
  
  date_form_id_marea <- paste0(day_from_id_marea, "/", month_from_id_marea, "/", year_from_id_marea)
  
  errors <- df[date_form_id_marea != df[[field]],]
  
  if (length(errors$ID_MAREA)>0){
    # this line add a comment to the errors dataframe wich contain the value of the
    # df variable
    errors.date_field <- deparse(substitute(field))
    errors <- addTypeOfError(errors, "WARNING: la fecha del campo ", errors.date_field, " no concuerda con el ID_MAREA")
    return(errors)
  } else {
    return()
  }
  
}
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Check start date of OAB_trips previous to end date
#' #' Use the OAB_trips df
#' @return df with errors
#' @export
trips_check_initial_date_before_final_date <- function(){
  
  start <- as.POSIXlt(OAB_trips$FECHA_INI, format="%d/%m/%Y")
  end <- as.POSIXlt(OAB_trips$FECHA_FIN, format="%d/%m/%Y") 
  
  if(end > start){
    errors <- OAB_trips[start - end,]
    errors <- addTypeOfError(errors, "ERROR: la ficha final de la marea es anterior a la fecha inicial")
    return(errors)
  } else {
    return()
  }
  
}
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Check shooting date previous to hauling date
#' Use the OAB_hauls df
#' @return df with errors
#' @export
hauls_check_sooting_date_before_hauling_date <- function(){
  
  start_date <- dby_to_dmy_date_format(OAB_hauls$FECHA_LAR)
  start <- paste(start_date, OAB_hauls$HORA_LAR)
  start <- as.POSIXlt(start, format="%d/%m/%Y %H:%M")
  
  end_date <- dby_to_dmy_date_format(OAB_hauls$FECHA_VIR)
  end <- paste(end_date, OAB_hauls$HORA_VIR)
  end <- as.POSIXlt(end, format="%d/%m/%Y %H:%M")
  

  errors <- OAB_hauls[(start - end) > 0,]
  
  if (length(errors) >0){
    return(errors)
  } else {
    return()
  }

}
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Check the final date is included in id marea, only in GC samples (CERCO_GC and
#' BACA_GC)
#' @return df with errors
#' @export
trips_check_final_date_in_id_marea_GC <- function(){
  errors <- OAB_trips %>%
    filter(ESTRATO_RIM=="BACA_GC" | ESTRATO_RIM == "CERCO_GC")%>%
    check_date_with_id_marea("FECHA_FIN") %>%
    addTypeOfError("ERROR: el ID_MAREA no se corresponde con el campo FECHA_FIN")
  
  return(errors)
}
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Sow boxplot graphic with speed to check the speed by ESTRATRO_RIM.
#' 
#' @export
view_outliers_speed <-function(){
  
  library(ggplot2)  
  library(ggiraph)
  
  hauls_speed <- OAB_hauls %>%
    select(ID_MAREA, ESTRATO_RIM, ESP_OBJ, VELOCIDAD) %>%
    filter(ESTRATO_RIM %in% c("BACA_CN", "BACA_GC", "JURELERA_CN", "PAREJA_CN", "RAPANTER_AC"))
  
  hauls_speed$str <- as.character(hauls_speed$ESTRATO_RIM)
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ != "CABALLA", "str"] <- "PAREJA_CN.RESTO"
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ == "CABALLA", "str"] <- "PAREJA_CN.CABALLA"
  
  p <- ggplot(hauls_speed, aes(str, VELOCIDAD))+
    geom_boxplot_interactive()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ggiraph(code = print(p))  
}
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
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
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Check if various variables exists in a dataframe
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
        error = function(e){ #here, instead of stop the with the error of variable_exists_in_df, save it in result variable
          return(e) }
      )
    })
  
  return(result)
  
}
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Check if a variable or variables of a dataframe contain empty values
#' @param variables: vector with variables to check.
#' @param df: dataframe to check
#' @return A list with a dataframe of every variable with empty values. Every
#' dataframe contains erroneus rows
#' @export
check_empty_values_in_variables <- function (df, variables){
  
  try(variables_in_df(df, variables))
  
  variables <- as.list(variables)
  
  errors <- lapply(variables, function(x){
    error <- (df[df[[x]]=="",])
    addTypeOfError(error, "ERROR: Variable ", x, " vacía" )
  })
  
  errors <- Filter(function(x) nrow(x) > 0, errors)
  return (errors)
  
}
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Collect all the errors returned by check_empty_values_in_variables of the four discards
#' datasets.
#' Some variables are ignored (allowed emptying): OBSERVACIONES from OAB_hauls and
#' OAB_trips; COD_DESCAR, RAZON_DESCAR, PESO_SUB_MUE_TOT from OAB_catches and 
#' PESO_MUE_RET, NOMBRE_INGLES from OAB_lengths.
#' @details Require a dataframe returned by importOABFiles() function
#' @param df: dataframe returned by importOABFiles() function. By default is called
#' 'discards_samples'
#' @return A dataframe with the ID_MAREA and variable with values missing
#' @export
check_empty_fields_in_variables <- function(df = discards_samples){
  
  vars <- lapply(df,
                 function(x){
                   variables <- colnames(x)
                 })
  
  # remove not mandatory variables:
  vars[["hauls"]]<- vars[["hauls"]][!vars[["hauls"]] %in% "OBSERVACIONES"]
  vars[["trips"]] <- vars[["trips"]][!vars[["trips"]] %in% "OBSERVACIONES"]
  vars[["catches"]] <- vars[["catches"]][!vars[["catches"]] %in% c("COD_DESCAR", "RAZON_DESCAR","PESO_SUB_MUE_TOT")]
  vars[["lengths"]] <- vars[["lengths"]][!vars[["lengths"]] %in% c("PESO_MUE_RET", "NOMBRE_INGLES")]
  
  # check the empty values in every dataframe of discard_samples
  errors <- lapply(
    seq_along(df), #instead of use discard_samples, use seq_along(discard_samples)
    function(i){
      n <- names(df)[[i]] #this is the name of the list
      vars[[n]]
      err <- check_empty_values_in_variables(df[[i]],vars[[n]])
      # check_empty_values return a list with one dataframe by variable, so:
      err <- do.call(rbind, err)
      # only return ID_MAREA and TIPO_ERROR:
      err <- err %>% 
        select(ID_MAREA, TIPO_ERROR) %>%
        unique()
    }
  )
  
  erros <- do.call(rbind, errors)
  
}
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# #### IMPORT DISCARDS FILES ###################################################
# ------------------------------------------------------------------------------

discards_samples <- importOABFiles(trips_file, hauls_file, catches_file, lengths_file,
                                   path = PATH_FILES)

OAB_trips <- discards_samples$trips
OAB_hauls <- discards_samples$hauls
OAB_catches <- discards_samples$catches
OAB_lengths <- discards_samples$lengths



# ------------------------------------------------------------------------------
# #### SEARCHING ERRORS ########################################################
# ------------------------------------------------------------------------------

#create errors in dataframe to check functions
#levels(OAB_catches[["ID_MAREA"]]) <- c(levels(OAB_catches[["ID_MAREA"]]), "DESIXAC150515M", "DESIXAC160329M")
#OAB_catches[1:2, c("ID_MAREA")] <- c("DESIXAC150515M", "DESIXAC160329M")

#create errors in dataframe to check functions
#levels(OAB_trips[["ID_MAREA"]]) <- c(levels(OAB_trips[["ID_MAREA"]]), "DESIXAC150515M", "DESIXAC160329M")
#OAB_trips[1:2, c("ID_MAREA")] <- c("DESIXAC150515M", "DESIXAC160329M")



# ALLTOGETHER
ERRORS$all_empty_fields_in_variable <- check_empty_fields_in_variables()
  # ppp <- check_empty_variables()
  # ppp_number <- as.data.frame(table(ppp$TIPO_ERROR))


# TRIPS
ERRORS$trips_field_year <- check_field_year(OAB_trips)

ERRORS$trips_year_in_ID_MAREA <- check_year_in_ID_MAREA(OAB_trips)

ERRORS$trips_year_in_initial_date <- check_year_in_date(OAB_trips, "FECHA_INI", YEAR_DISCARD)
ERRORS$trips_year_in_final_date <- check_year_in_date(OAB_trips, "FECHA_FIN", YEAR_DISCARD)

ERRORS$trips_check_final_date_in_id_marea_GC <- trips_check_final_date_in_id_marea_GC()

ERRORS$trips_check_initial_date_before_final_date <- trips_check_initial_date_before_final_date


# HAULS
ERRORS$hauls_field_year <- check_field_year(OAB_hauls)

ERRORS$hauls_year_in_ID_MAREA_hauls <- check_year_in_ID_MAREA(OAB_hauls)

OAB_hauls$FECHA_LAR <- dby_to_dmy_date_format(OAB_hauls$FECHA_LAR)
ERRORS$hauls_year_in_shotting_date <- check_year_in_date(OAB_hauls, "FECHA_LAR", YEAR_DISCARD)

OAB_hauls$FECHA_VIR <- dby_to_dmy_date_format(OAB_hauls$FECHA_VIR)
ERRORS$hauls_year_in_hauling_date <- check_year_in_date(OAB_hauls, "FECHA_VIR", YEAR_DISCARD)

ERRORS$hauls_check_sooting_date_before_hauling_date <- hauls_check_sooting_date_before_hauling_date()

# CHECK SPEED:




view_outliers_speed()

# PRUEBAS CON SHINY: AL FINAL PARECE QUE NO SE PUEDEN MOSTRAR CON UN BOXPLOT
# library(shiny)
# runApp("speed", display.mode = "showcase")



# CATCHES
ERRORS$catches_field_year <- check_field_year(OAB_catches)

ERRORS$catches_year_in_ID_MAREA <- check_year_in_ID_MAREA(OAB_catches)


# LENGTHS
ERRORS$lengths_field_year <- check_field_year(OAB_lengths)

ERRORS$lengths_year_in_ID_MAREA <- check_year_in_ID_MAREA(OAB_lengths)

ERRORS$lengths_year_in_sample_date <- check_year_in_date(OAB_trips, "FECHA_MUE", YEAR_DISCARD)
