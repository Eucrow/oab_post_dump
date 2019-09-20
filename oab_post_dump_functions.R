#### Check discards from SIRENO
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
####
#### Functions file 
####
#### Convention style guide: http://r-pkgs.had.co.nz/style.html


# Add variable with type of error to a dataframe -------------------------------
addTypeOfError <- function(df, ...){
  
  arguments <- list(...)
  
  type <- paste(arguments, collapse = '', sep = " ")
  
  if(nrow(df)!=0){
    # df[["TIPO_ERROR"]] <- type
    df <- df %>% mutate(TIPO_ERROR = type)
  }
  return(df)
}


# YEAR variable of the dataframe match with year to study ----------------------
#' Check if the YEAR variable of the dataframe match with year to study.
#' 
#' @param df daraframe to check the field YEAR. The dataframe must have a field
#' called YEAR.
#' @return df with errors
#' @export
check_field_year <- function(df) {
  
  errors <- df %>%
    select(COD_MAREA, YEAR) %>%
    filter(YEAR != YEAR_DISCARD) %>%
    unique()%>%
    addTypeOfError("ERROR: El campo YEAR no coincide con el año a comprobar: ",YEAR_DISCARD)
  
  return(errors)
  
}


# Split COD_MAREA field ---------------------------------------------------------
#' Split COD_MAREA field in its components: identifier, year, month, day and 
#' special
#' 
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


# Convert date in format 12-ENE-17 from 10/01/2017 -----------------------------
#' Convert date in format 12-ENE-17 from 10/01/2017
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



# Check year in COD_MAREA -------------------------------------------------------
#' Check year in COD_MAREA in a OAB df 
#' 
#' @param df: df to check. Must be a df obtained by importOAB functions.
#' @return df with errors
#' @export
check_year_in_COD_MAREA <- function(df){
  
  COD_MAREA_split <- split_COD_MAREA(df)

  errors <- which(!(COD_MAREA_split[["year"]] %in% YEAR_DISCARD))

  errors <- df[errors,]
  
  # this line add a comment to the errors dataframe wich contain the value of the
  # df variable
  errors.name <- deparse(substitute(df))
  
  errors <- errors %>%
            select(COD_MAREA, YEAR)%>%
            addTypeOfError(paste("ERROR: el a?o del COD_MAREA en", errors.name, "no coincide con el a?o a comprobar"))
  
  return(errors)
}


# Check whether a date match with a given day, month and year ------------------
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
    if( is.na( x ) ) stop( paste0("Error in date format. Be sure the format is day/month/year: ", x), call. = F)
      
    #check if day, month and year match with x
    if ((!missing(day) && (x$mday != day)) || 
        (!missing(month) && (x$mon+1 != month)) ||
        (!missing(year) && (x$year+1900 != year))) {
      return(FALSE) 
    }
    
    return(TRUE)
  })
}

# Check year of a date field ---------------------------------------------------
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
  
  errors <- errors[, c("COD_MAREA", "YEAR", date_field)]

  # this line add a comment to the errors dataframe wich contain the value of the
  # df variable
  errors.date_field <- deparse(substitute(date_field))
  
  errors <- addTypeOfError(errors, "ERROR: el a?o del campo ", errors.date_field, " no coincide con el a?o ", year)
    
  return(errors)
  
}


# Check if a date match with the COD_MAREA date ---------------------------------
#' Check if a date match with the COD_MAREA date
#' 
#' @param df: dataframe to check.
#' @param field: name of the field with de date to check.
#' @return df with errors
#' @export
check_date_with_COD_MAREA <- function(df, field){
  
  indexes <- regexec("[0-9]+", df$COD_MAREA)
  
  date_from_COD_MAREA <- split_COD_MAREA(df)
  
  # date_from_COD_MAREA <- regmatches(df$COD_MAREA, indexes)
  year_from_COD_MAREA <- date_from_COD_MAREA$year
  month_from_COD_MAREA <- date_from_COD_MAREA$month
  day_from_COD_MAREA <-  date_from_COD_MAREA$day
  
  date_form_COD_MAREA <- paste0(day_from_COD_MAREA, "/", month_from_COD_MAREA, "/", year_from_COD_MAREA)
  
  errors <- df[date_form_COD_MAREA != df[[field]],]
  
  if (length(errors$COD_MAREA)>0){
    # this line add a comment to the errors dataframe wich contain the value of the
    # df variable
    errors.date_field <- deparse(substitute(field))
    errors <- addTypeOfError(errors, "WARNING: la fecha del campo ", errors.date_field, " no concuerda con el COD_MAREA")
    return(errors)
  } else {
    return()
  }
  
}

# Check start date of OAB_trips previous to end date ---------------------------
#' Check start date of OAB_trips previous to end date
#' #' Use the OAB_trips df
#' @return df with errors
#' @export
trips_check_initial_date_before_final_date <- function(){
  
  start <- as.POSIXlt(OAB_trips$FECHA_INI, format="%d/%m/%Y")
  end <- as.POSIXlt(OAB_trips$FECHA_FIN, format="%d/%m/%Y") 

  errors <- OAB_trips[(start - end)>0,]
  
  if(length(nrow(errors)==0)) {
    return(NULL)
  } else {
    errors <- addTypeOfError(errors, "ERROR: la ficha final de la marea es anterior a la fecha inicial")
    return(errors)
  }
}


# Shooting date previous to hauling date ---------------------------------------
#' Check shooting date previous to hauling date
#' Use the OAB_hauls df
#' @return df with errors
#' @export
hauls_check_hauling_date_before_shooting_date <- function(){
  
  # it is imperative check if any of this fields are empty: FECHA_LAR, FECHA_VIR
  # HORA_LAR and HORA_VIR:
  
  
  
  OAB_hauls <- OAB_hauls %>%
    filter(MUESTREADO=="S")
  
  start_date <- dby_to_dmy_date_format(OAB_hauls$FECHA_LAR)
  start <- paste(start_date, OAB_hauls$HORA_LAR)
  start <- as.POSIXlt(start, format="%d/%m/%Y %H:%M")
  
  end_date <- dby_to_dmy_date_format(OAB_hauls$FECHA_VIR)
  end <- paste(end_date, OAB_hauls$HORA_VIR)
  end <- as.POSIXlt(end, format="%d/%m/%Y %H:%M")
  
  start_end <- start - end

  #errors <- OAB_hauls[(start - end) > 0, c("COD_MAREA", "COD_LANCE", "FECHA_LAR", "FECHA_VIR")]
  errors <- OAB_hauls[(start - end) > 0 | is.na(start - end), c("COD_MAREA", "COD_LANCE", "FECHA_LAR", "HORA_LAR", "FECHA_VIR", "HORA_VIR")]
  
  errors <- addTypeOfError(errors, "ERROR: La fecha y hora de virada es anterior o igual a la fecha y hora de largada.")
  
  if (nrow(errors)==0){
    return(NULL)
  } else {
    return(errors)
  }
}


# Final date included in id marea, only in GC samples --------------------------
#' Check the final date is included in id marea, only in GC samples (CERCO_GC and
#' BACA_GC)
#' @return df with errors
#' @export
trips_check_final_date_in_COD_MAREA_GC <- function(){
  errors <- OAB_trips %>%
    filter(ESTRATO_RIM=="BACA_GC" | ESTRATO_RIM == "CERCO_GC")%>%
    check_date_with_COD_MAREA("FECHA_FIN")
  
  if (!is.null(errors)) {
    errors <- errors %>%
      select(COD_MAREA, FECHA_FIN)
    
    addTypeOfError(errors, "ERROR: Final date contained in IDMAREA doesn't match whith FECHA_FIN variable (view trips)")
    
    return(errors)
  }
}

#' Return a oulier
#' ggplot defines an outlier by default as something that's > 1.5*IQR from the
#' borders of the box. 
#' To use only in get_outliers_speed() and show_outliers_speed() functions
is_outlier <- function(x) {
  return(x < quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T) | x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T))
  }  


# Posible outlier in speed -----------------------------------------------------
#' Get the posible outlier in speed.
#' Using the same criteria to ggplot in a boxplot: an outlier is something that's > 1.5*IQR from the
#' borders of the box.
#' 
#' @export
get_speed_outliers <- function(){
  
  hauls_speed <- OAB_hauls %>%
    select(COD_MAREA, COD_LANCE, ESTRATO_RIM, ESP_OBJ, VELOCIDAD) %>%
    filter(ESTRATO_RIM %in% c("BACA_CN", "BACA_GC", "JURELERA_CN", "PAREJA_CN", "RAPANTER_AC"))
  
  hauls_speed$str <- as.character(hauls_speed$ESTRATO_RIM)
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ != "CABALLA", "str"] <- "PAREJA_CN.RESTO"
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ == "CABALLA", "str"] <- "PAREJA_CN.CABALLA"
 
  hauls_speed <- hauls_speed %>%
    group_by(str)%>%
    mutate(OUTLIER = ifelse(is_outlier(VELOCIDAD), VELOCIDAD, as.numeric(NA)))%>%
    filter(!is.na(OUTLIER))%>%
    addTypeOfError("WARNING: posible outlier en el campo velocidad")

  return(as.data.frame(hauls_speed))  
  
}

# Speed boxplot graphic by ESTRATO_RIM -----------------------------------------
#' Sow boxplot graphic with speed to check the speed by ESTRATRO_RIM.
#' 
#' @export
view_speed_outliers <-function(){
  
  library(ggplot2)  
  library(ggiraph)
  
  hauls_speed <- OAB_hauls %>%
    select(COD_MAREA, ESTRATO_RIM, ESP_OBJ, VELOCIDAD) %>%
    filter(ESTRATO_RIM %in% c("BACA_CN", "BACA_GC", "JURELERA_CN", "PAREJA_CN", "RAPANTER_AC"))
  
  hauls_speed$str <- as.character(hauls_speed$ESTRATO_RIM)
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ != "CABALLA", "str"] <- "PAREJA_CN.RESTO"
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ == "CABALLA", "str"] <- "PAREJA_CN.CABALLA"

  hauls_speed %>%
    group_by(str)%>%
    #in the next mutate line: if in the ifelse use COD_MAREA as the returned value
    #when the condition is True, it doesnt't print the COD_MAREA but a weird number??????????
    mutate(outlier = ifelse(is_outlier(VELOCIDAD), VELOCIDAD, as.numeric(NA)))%>%
      ggplot(., aes(str, VELOCIDAD))+
      # geom_boxplot_interactive()+
      geom_boxplot()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      geom_text(aes(label = outlier), na.rm=T, hjust=1.1)

  # ggiraph(code = print(p))  
  # return (p)
}


# Variable exists in a dataframe -----------------------------------------------
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

# Multiple variables exists in a dataframe -------------------------------------
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

# Get the caracteristics_arte dataframe ----------------------------------------
#' function to get the dataframe caracteristicas_arte, which is related to the
#' characteristics for every gear
get_gear_characteristics <- function(){
  
  caracteristicas_arte <- read.table("caracteristicas_arte.csv", 
                                     header = T, 
                                     sep=";",
                                     colClasses = c("factor", "factor"))
  
  return(caracteristicas_arte)
}

# Variable or variables of a dataframe contain empty values --------------------
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
      error <- addTypeOfError(error, "ERROR: Variable ", x, " vac?a" )
    } else {
      error <- (df[df[[x]]=="" | is.na(df[[x]]),])
      if (nrow(error)>0){
        error <- addTypeOfError(error, "ERROR: Variable ", x, " vac?a" )
      }
    }
    
    if (nrow(error) > 0){
      # select only interested variables
      if ("COD_LANCE" %in% colnames(error)){
        error <- error[, c(BASE_FIELDS, "COD_LANCE", "TIPO_ERROR")]
      } else {
        error <- error[, c(BASE_FIELDS, "TIPO_ERROR")]
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


# Data of a dataframe is empty -------------------------------------------------
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


# Empty fields in variables ----------------------------------------------------
#' Return empty variables of a OAB dataframes from importOAB functions. Only
#' variables saved in formato_variables dataset as mandatory are checked.
#' Some variables are mandatory according to its gear. The file caracteristicas_arte.csv
#' contains this variables.
#' @details Require one of the dataframes returned by importOABFiles functions:
#' importOABFiles(), importOABCatches(), importOABHauls(), importOABTrips() and
#' ipmortOABLengtsh().
#' @param df: dataframe returned by one of the importOAB functions.
#' @param type_file: type of the imported file according to this values: OAB_CATCHES,
#' OAB_HAULS, OAB_TRIPS and OAB_LENGTHS.
#' @return A dataframe with the COD_MAREA and variables with values missing.
#' @export
check_empty_fields_in_variables <- function(df, type_file = c("OAB_TRIPS", "OAB_HAULS", "OAB_CATCHES", "OAB_LENGTHS")){
  
  # Detect if the variable type_file is correct:
  match.arg(type_file)

  
  mandatory_field <- paste0(type_file, "_MANDATORY")
  
  mandatory <- formato_variables[which(!is.na(formato_variables[type_file])
                                       & formato_variables[mandatory_field] == TRUE), c("name_variable")]
  df_mandatory <- df[,mandatory]
  
  err <- check_empty_values_in_variables(df_mandatory, mandatory)
  
  # in case there aren't any errors, check_empty_values returns NULL, so:
  if (!is.null(err)){
    
    # check_empty_values return a list with one dataframe by variable, so:
    err <- do.call(rbind, err)
  
  
    # return different fields according to file type:
    # switch(type_file,
    #        OAB_TRIPS = { err <- err[, c("COD_MAREA", "TIPO_ERROR")]},
    #        OAB_HAULS = { err <- err[, c("COD_MAREA", "COD_LANCE", "TIPO_ERROR")]},
    #        OAB_CATCHES = { err <- err[, c("COD_MAREA", "COD_LANCE", "COD_ESP", "TIPO_ERROR")]},
    #        OAB_LENGTHS = { err <- err[, c("COD_MAREA", "COD_LANCE", "COD_ESP", "TIPO_ERROR")]}
    # )
    
    return(err)
  } else {
    return(NULL)
  }

}


# Check variable with master ---------------------------------------------------
# THIS FUNCTION IS A SHIT!!!! remove and do it individually?????
check_variable_with_master <- function (df, variable){
  
  if(variable != "ESTRATO_RIM" &&
     variable != "COD_PUERTO" &&
     variable != "COD_PUERTO_BASE" &&
     variable != "COD_PUERTO_LLEGADA" &&
     variable != "COD_PUERTO_DESCARGA" &&
     variable != "COD_ORIGEN" &&
     variable != "COD_ARTE" &&
     variable != "PROCEDENCIA" &&
     variable != "COD_TIPO_MUE"){
    stop(paste("This function is not available for ", variable))
  }
  
  # If the variable begin with "COD_", the name of the data source
  # is the name of the variable without "COD_"
  variable_formatted <- variable
  if (grepl("^COD_", variable)){
    variable_formatted <- strsplit(variable, "COD_")
    variable_formatted <- variable_formatted[[1]][2]
  }
  
  
  # *********** CASE OF PUERTO_BASE, PUERTO_LLEGADA OR PUERTO_DESCARGA ***********
  # change the name of PUERTO_BASE, PUERTO_LLEGADA or PUERTO_DESCARGA
  # to PUERTO:
  if(variable_formatted == "PUERTO_LLEGADA" ||
     variable_formatted == "PUERTO_BASE" ||
     variable_formatted == "PUERTO_DESCARGA") {
    variable_puerto_original <- variable_formatted
    variable_formatted <- "PUERTO"
  }
  
  # ******************************************************************************
  name_data_set <- tolower(variable_formatted)
  name_data_set <- paste0(name_data_set, "_OAB")
  
  # *********** CASE OF PUERTO_BASE, PUERTO_LLEGADA OR PUERTO_DESCARGA ***********    
  # it's required  join via the variable COD_PUERTO, so a new variable is required:
  if(variable == "COD_PUERTO_LLEGADA" ||
     # variable == "COD_PUERTO_BASE" ||
     variable == "COD_PUERTO_DESCARGA") {
    variable_to_change <- "COD_PUERTO"
  }
  # ****************************************************************************** 
  
  dataframe_variable <- get(name_data_set)
  
  # *********** CASE OF PUERTO_BASE, PUERTO_LLEGADA OR PUERTO_DESCARGA ***********  
  if (exists("variable_to_change")){
    names(dataframe_variable)[names(dataframe_variable) == variable_to_change] <- variable
  }
  
  #search the errors in variable
  # errors <- anti_join(df, get(name_data_set), by = variable)
  
  # ******************************************************************************
  
  
  #prepare to return
  # fields_to_filter <- c(BASE_FIELDS, variable, variable_formatted)
  if (exists("variable_to_change")){
    errors <- anti_join(df, get(name_data_set), by = setNames(nm=variable, variable_to_change))
    fields_to_filter <- c("COD_MAREA", variable, variable_puerto_original)
  } else {
    errors <- anti_join(df, get(name_data_set), by = setNames(nm=variable, variable))
    fields_to_filter <- c("COD_MAREA", variable, variable_formatted)
  }
  
  
  errors <- errors %>%
    select(one_of(fields_to_filter))%>%
    unique()
  
  
  text_type_of_error <- paste0("ERROR: ", name_data_set, " no concuerda con los maestros de SIRENO")
  errors <- addTypeOfError(errors, text_type_of_error)
  
  #return
  return(errors)
}


# Coherence between ESTRATO_RIM and origin -------------------------------------
#' Check coherence between ESTRATO_RIM and origin.
#' 
#'  @return dataframe with wrong coherence.
#'  
checkCoherenceEstratoRimOrigin <- function(df){
  
  try(variables_in_df(c("ESTRATO_RIM", "COD_ORIGEN"), df))
  
  BASE_FIELDS <- c("COD_MAREA", "COD_LANCE", "ESTRATO_RIM", "COD_ORIGEN", "ORIGEN")
  
  estratorim_origen_OAB <- estratorim_origen[, c("ESTRATO_RIM", "COD_ORIGEN", "OAB")]
  
  errors <- OAB_hauls %>%
    select(one_of(BASE_FIELDS)) %>%
    unique() %>%
    anti_join(y=estratorim_origen_OAB, by=c("ESTRATO_RIM", "COD_ORIGEN")) %>%
    addTypeOfError("ERROR: no concuerda el estrato_rim con el origen")
  
  return(errors)
  
}

# Coherence between 'ESTRATO_RIM' and 'gear' -----------------------------------
#
#' Check coherence between ESTRATO_RIM and origin.
#' 
#'  @return dataframe with wrong coherence.
#'  
checkCoherenceEstratoRimGear <- function(df){
  
  try(variables_in_df(c("ESTRATO_RIM", "COD_ARTE"), df))
  
  BASE_FIELDS <- c("COD_MAREA", "COD_LANCE", "ESTRATO_RIM", "COD_ARTE", "ARTE")
  
  estratorim_arte_OAB <- estratorim_arte[, c("ESTRATO_RIM", "COD_ARTE", "OAB")]
  
  errors <- df %>%
    select(one_of(BASE_FIELDS)) %>%
    unique() %>%
    anti_join(y=estratorim_arte_OAB, by=c("ESTRATO_RIM", "COD_ARTE")) %>%
    addTypeOfError("ERROR: no concuerda el estrato_rim con el arte")
  
  return(errors)
  
}

# Format the errors produced ---------------------------------------------------
# This function combine all the dataframes of the errors_list (a list of dataframes)
# and format it:
# - combine all dataframes in one
# - order columns
# - remove empty columns in every area dataframe
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
  
  # Order the errors and remove columns with only NA values
  # Order columns
  errors <- errors %>%
    select(-one_of("TIPO_ERROR"), one_of("TIPO_ERROR")) %>% #remove TIPO_ERROR, and add it to the end
    # mutate(FECHA_MUE = as.Date(FECHA_MUE, "%d-%m-%y")) %>%
    arrange_("COD_MAREA")
  
  #Remove columns with only NA values
  #Filter extracts the elements of a vector for which a predicate (logical) function gives true
  errors <- Filter(function(errors){!all(is.na(errors))}, errors)
  
  # Add column Comprobado
  errors[["comprobado"]] <- ""
  
  return(errors)
}



# sampled hauls without catches weight -----------------------------------------
#' Check sampled hauls without catches weight
#' 
#'  @return dataframe with COD_MAREA with errors.
#'  
hauls_sampled_with_catch_weights <- function(){

    sampled <- OAB_hauls %>%
      filter(MUESTREADO == "S") %>%
      select(COD_MAREA) %>%
      unique()
    
    catches <- OAB_catches %>%
      group_by(COD_MAREA) %>%
      summarise(P_CAP_TOT = sum(P_CAP)) %>%
      filter(P_CAP_TOT==0)
    
    if (nrow(catches) != 0){
      err <- merge(x=sampled, y=catches, by= "COD_MAREA")
      err <- addTypeOfError(err, "ERROR: Lance muestreado pero sin peso de captura.")
      return(err)
    }
  
}

# species without weight caught neither weight discarded -----------------------
#' Check species without weight caught neither weight discarded
#' 
#'  @return dataframe with COD_MAREA with errors.
#'  
species_without_caught_neither_discarded_weight <- function(){

  err <- OAB_catches %>%
    filter(P_CAP == 0 & P_DESCAR == 0) %>%
    select(COD_MAREA, COD_LANCE, COD_ESP, ESP, P_CAP, P_DESCAR) %>%
    arrange(COD_MAREA)%>%
    unique() %>%
    addTypeOfError("ERROR: Especie sin peso captura ni peso descarte.")
  
  return(err)
  
}


# less catch RETENIDA than sampled RETENIDA ------------------------------------
#' Check less catch RETENIDA than sampled RETENIDA
#' 
#' @return dataframe with errors.
catches_less_retained_catch_than_sampled_retained_catch <- function(){
  
  err <- OAB_catches %>%
    select(COD_MAREA, COD_LANCE, COD_ESP, CATEGORIA, P_RET, P_MUE_RET) %>%
    filter(P_RET < P_MUE_RET) %>%
    addTypeOfError("ERROR: captura retenida menor que captura retenida muestreada.")
  
  return(err)
  
}

# less catch discarded than discard sampled ------------------------------------
#' Check less catch RETENIDA than sampled RETENIDA
#' 
#' @return dataframe with errors.
catches_less_discard_weight_than_sampled_discard_weight <- function(){
  
  OAB_catches[["P_DESCAR"]] <- round(OAB_catches[["P_DESCAR"]], 2)
  OAB_catches[["P_MUE_DESCAR"]] <- round(OAB_catches[["P_MUE_DESCAR"]], 2)
  
  err <- OAB_catches %>%
    select(COD_MAREA, COD_LANCE, COD_ESP, CATEGORIA, P_DESCAR, P_MUE_DESCAR) %>%
    filter(P_DESCAR < P_MUE_DESCAR) %>%
    addTypeOfError("ERROR: peso descartado menor que peso descartado muestreado.")
  
  return(err)
  
}

# Hauls duration ---------------------------------------------------------------
#' Check hauls duration
#' 
#' The duration of the hauls are compared with the information of the file
#' "duracion_mareas.txt"
#' 
#' @return dataframe with errors
hauls_hauls_duration <- function(){
  
  trip_hauls <- read.csv("duracion_mareas.txt", sep=";")
  
  
  OAB_hauls[["FECHA_HORA_LAR"]] <- paste(OAB_hauls[["FECHA_LAR"]], OAB_hauls[["HORA_LAR"]])
  OAB_hauls[["FECHA_HORA_VIR"]] <- paste(OAB_hauls[["FECHA_VIR"]], OAB_hauls[["HORA_VIR"]]) 
  
  # change the column "FECHA_LAR" to a date format
  # to avoid some problems with Spanish_Spain.1252 (or if you are using another
  # locale), change locale to Spanish_United States.1252:
  lct <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME","Spanish_United States.1252")
  
  OAB_hauls[["FECHA_HORA_LAR_CT"]] <- as.POSIXct(OAB_hauls$FECHA_HORA_LAR, format = '%d-%b-%y %H:%M')
  OAB_hauls[["FECHA_HORA_VIR_CT"]] <- as.POSIXct(OAB_hauls$FECHA_HORA_VIR, format = '%d-%b-%y %H:%M')
  
  # and now the come back to the initial configuration of locale:
  Sys.setlocale("LC_TIME", lct)
  
  
  err <- OAB_hauls %>%
    select(COD_MAREA, COD_LANCE, ESTRATO_RIM, FECHA_HORA_LAR_CT, FECHA_HORA_VIR_CT) %>%
    unique() %>%
    group_by(COD_MAREA, ESTRATO_RIM) %>%
    summarise(first_date = min(FECHA_HORA_LAR_CT), last_date = max(FECHA_HORA_VIR_CT)) %>%
    mutate(duration_trip = difftime(last_date, first_date, units = "hours")) %>%
    merge(, y = trip_hauls, all.x = T) %>%
    filter(duration_trip > DURACION_MAX) %>%
    mutate(duration_trip = round(duration_trip, 0)) %>%
    addTypeOfError("WARNING: ?Duraci?n de la marea excesivamente larga? habr?a que comprobar las fechas y horas de largado y de virado de los lances.")
  
  # TO DO: find the haul with the erroneus date and return it 
  # duration_p95 <- OAB_hauls %>%
  #   filter(COD_MAREA%in%err$COD_MAREA) %>%
  #   group_by(COD_MAREA, ESTRATO_RIM) %>%
  #   summarise('p95'=quantile(?????, probs=0.95))
  # 
  # error_final <- OAB_hauls %>%
  #   filter(COD_MAREA%in%err$COD_MAREA) %>%
  #   merge(., duration_p95, by=c("COD_MAREA", "ESTRATO_RIM"))
  
  return(err)
  
}


# Target specie of the hauls is the most catched specie ------------------------
check_target_sp_with_catch <- function(){
  tryCatch({
    COD_MAREA_with_obj <- OAB_hauls %>%
      select(ESTRATO_RIM, COD_MAREA, COD_LANCE, COD_ESP_OBJ, ESP_OBJ) %>%
      unique()
    
    catches_only_greater_catch <- OAB_catches %>%
      select(COD_MAREA, COD_LANCE, COD_ESP, ESP, P_RET) %>%
      #mutate(peso_total = PESO_RET + PESO_DESCAR) %>%
      group_by(COD_MAREA, COD_LANCE)%>%
      filter(P_RET == max(P_RET))
    
    colnames(catches_only_greater_catch)[colnames(catches_only_greater_catch)=="ESP"] <- "ESP_MAYOR_CAPTURA"
    
    catches_with_obj <- merge(catches_only_greater_catch, COD_MAREA_with_obj, by = c("COD_MAREA", "COD_LANCE"))
    
    
    # function to get the possible ESP_OBJ according to COD_ESP in OAB_catches
    # return list with COD_ESP_OBJ
    get_cod_target_specie <- function(sp_code){
      as.character(especies_objetiVo_oab[especies_objetiVo_oab$COD_ESP %in% sp_code,"COD_ESP_OBJ"])
    }
    
    # function to check if a COD_ESP has its COD_ESP_OBJ according to the especies_objetivo master
    coherence_sp_target_sp <- function(cod_esp, cod_esp_obj){
      possible_cod_esp_obj <- get_cod_target_specie(cod_esp)
      if (cod_esp_obj %in% possible_cod_esp_obj){
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    
    catches_with_obj$co_sp_target <- apply(catches_with_obj , 1, function(x){
      coherence_sp_target_sp(x["COD_ESP"], x["COD_ESP_OBJ"])
    })
    
    err <- catches_with_obj[catches_with_obj$co_sp_target==FALSE, ] 
    err <- err[, -which(names(err) %in% "co_sp_target")]
    err <- addTypeOfError(err, "WARNING: la especie objetivo no coincide con la especie de mayor captura del lance")
    
    return(err)
  },
  error = function(err){
    print(err)
  }
  )
  
}

# Cable length is greather than 1000 m. ----------------------------------------
length_cable_1000 <- function(){
  
  tryCatch({
      # in the next line, the use of NA prevent the returned NA rows:
      war <- OAB_hauls[which(OAB_hauls$CABLE>1000),c("ESTRATO_RIM", "COD_MAREA", "COD_LANCE", "CABLE")]
      
      war <- addTypeOfError(war, "WARNING: el cable largado es mayor que 1000m.")
      
      return(war)
    },
    error = function(err){
      print(err)
    }
  )
  
}

# Total discard weight less than subsample discard weight
total_discard_less_subsample_discard <- function(df){
  
  errors <- df[
    df$P_DESCAR < df$P_MUE_DESCAR,
    c(BASE_FIELDS, "COD_ESP", "A3_ESP", "ESP", "P_DESCAR", "P_MUE_DESCAR")
    ]
  
  errors <- addTypeOfError(errors, "ERROR: total discard weight less than subsample discard weight.")
  
}



# Sampled discard weight less than subsample discard weight.
sampled_discard_less_subsample_discard <- function(df){
  
  # usually the PESO_SUB_MUE_TOT is NA, so it is neccesary detect it.
  errors <- df[
    which( !is.na(df$P_SUB_MUE_TOT) & df$P_SUB_MUE_TOT > df$P_MUE_DESCAR),
           c(BASE_FIELDS, "COD_LANCE", "COD_ESP", "A3_ESP", "ESP",
             "P_SUB_MUE_TOT", "P_MUE_DESCAR")
    ]
  
  errors <- addTypeOfError(errors, "ERROR: sampled discard weight less than subsample discard weight.")
  
}

# retained sampled weigth less than zero (or NA) when there are any specimens
# retained
retained_sampled_weight_when_specimens_retained <- function(df){
  
  errors <- df[which(
    df[["EJEM_RET"]] > 0 &
      (df[["P_MUE_RET"]] <= 0 |
         is.na(df[["P_MUE_RET"]]))),
      c(BASE_FIELDS, "COD_ESP", "A3_ESP", "ESP", "EJEM_RET", "P_MUE_RET")
    ]
  
  errors <- addTypeOfError(errors, "ERROR: there are specimens retained without retained sampled weight.")
  
}

# discarded sampled weigth less than zero (or NA) when there are any specimens
# discarded
discarded_sampled_weight_when_specimens_discarded <- function(df){
  
  errors <- df[which(
    df[["EJEM_DESCAR"]] > 0 &
      (df[["P_MUE_DESCAR"]] <= 0 | is.na(df[["P_MUE_DESCAR"]]))
    ),
    c(BASE_FIELDS, "COD_ESP", "A3_ESP", "ESP", "EJEM_DESCAR", "P_MUE_DESCAR")]
  
  errors <- addTypeOfError(errors, "ERROR: there are specimens discarded without
                          discarded sampled weight.")
  
}

# reason discard field filled, when the discarded species belongs to the species
# list saved in especies_a_medir_OAB.csv
# Require the file especies_a_medir_OAB.csv
reason_discard_field_filled <- function(df){
  
  species_to_measure <- importCsvSAPMUE("especies_a_medir_OAB.csv")
  species_to_measure <- species_to_measure[,"COD_ESP"]
  
  errors <- df[which(
    df[["P_DESCAR"]]>0 & df[["COD_DESCAR"]] == ""),
    c(BASE_FIELDS, "COD_LANCE", "COD_ESP", "A3_ESP", "ESP", "P_DESCAR", "RAZON_DESCAR")]
  
  errors <- unique(errors)
  
  errors <- errors[errors[["COD_ESP"]] %in% species_to_measure,]
  
  errors <- addTypeOfError(errors, "ERROR: this species must have the field reason discard filled but it doesn't")
  
  return(errors)
  
}

# Haul sampled with empty discard weight.
# When a haul is sampled, the discard weight must be zero even there aren't any
# species discarded
# THIS CHECK CAN'T BE DONE BECAUSE PESO_DESCAR IS FILLED WITH A ZERO IN THE
# REPORT IF THE FIELD IS EMPTY IN SIRENO.
haul_sampled_with_empty_discard_weight <- function(){
  
  sampled_hauls <- OAB_hauls[which(OAB_hauls["MUESTREADO"]=="S"),
                             c("COD_MAREA", "COD_LANCE")]
  
  clean_catches <- OAB_catches[, c("COD_MAREA", "COD_LANCE", "P_DESCAR")]
  clean_catches <- unique(clean_catches)
  
  sampled_hauls <- merge(sampled_hauls,
                         clean_catches,
                         by = c("COD_MAREA", "COD_LANCE"),
                         all.x = TRUE)
  
  errors <- sampled_hauls[which(sampled_hauls[["P_DESCAR"]] == ""),]
  
  sampled_catches <- merge(OAB_catches, sampled_hauls, by=c("COD_MAREA", "COD"))
  
}

# There are priority species without length sampled
priority_species_without_lengths <- function(){
  
  # get the list of priority species which must be measured
  species_to_measure <- especies_a_medir_OAB[,"COD_ESP"]
  
  # get Species With Catch which must be measured
  swc <- OAB_catches[which(OAB_catches[["COD_ESP"]]%in%species_to_measure),
                     c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "P_CAP")]
  swc <- unique(swc)
  
  # clean lengths
  lengths_clean <- OAB_lengths[, c("COD_MAREA", "COD_LANCE",
                                   "COD_ESP", "ESP", "EJEM_MEDIDOS")]
  
  lengths_clean <- aggregate.data.frame(lengths_clean[, c("EJEM_MEDIDOS")],
                             by=list(lengths_clean$COD_MAREA,
                                     lengths_clean$COD_LANCE,
                                     lengths_clean$COD_ESP,
                                     lengths_clean$ESP),
                             sum, na.rm=TRUE)
  
  colnames(lengths_clean) <- c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "EJEM_MEDIDOS")

  # create errors dataframe
  errors <- merge(swc, 
                  lengths_clean, 
                  by.x = c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP"),
                  all.x = TRUE)
  errors <- errors[which(errors$EJEM_MEDIDOS==0 |
                         is.na(errors$EJEM_MEDIDOS)),]
  
  errors <- addTypeOfError(errors, "ERROR: priority species which hasn't been measured")
  
}

# check target species with metier ieo
# Require the file metier_ieo_especie_objetivo_OAB.txt
target_species_metier_ieo <- function(){
  
  # get dataset with relation between metier ieo and target species
  ms <- importCsvSAPMUE("metier_ieo_especie_objetivo_OAB.csv")
  ms$OK <- "ok"
  
  # hauls cleaned
  hc <- OAB_hauls[, c("COD_MAREA", "COD_LANCE", "METIER_IEO", "COD_ESP_OBJ")]

  # errors
  errors <- merge(hc, ms,
                  all.x = TRUE,
                  by = c("METIER_IEO", "COD_ESP_OBJ"))
  
  errors <- errors[is.na(errors[["OK"]]),]
  
  errors <- addTypeOfError(errors, "ERROR: the target species is not coherent with metier ieo.")
    
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
    assign(acro, df[startsWith(df[["COD_MAREA"]], acro), ])
    
    return(get(acro))
    
    
  }, df)
  
  
  names(p) <- acronyms
  
  return(p)
  
}
