#' Add variable with type of error to a dataframe.
#' The name of the variables is "TIPO_ERROR", containing the text in ....
#' @param df Dataframe in which the error type variable is added.
#' @param ... Text to add as error in the dataframe.
#' @return Dataframe df with a new variable called "TIPO_ERROR" with the text
#' in ...
addTypeOfError <- function(df, ...){

  arguments <- list(...)

  tx <- paste(arguments, collapse = '', sep = " ")
  # escape new lines and tabs: only using \s works, I don't know excactly why :/
  tx <- gsub("\\s+", " ", tx)

  if(nrow(df)!=0){
    df[["TIPO_ERROR"]] <- tx
  }
  return(df)
}


#' Split COD_MAREA field.
#' Split COD_MAREA field in its components: identifier, year, month, day and
#' special
#' @param df dataframe with COD_MAREA field to split. The dataframe must have a
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

  if(nrow(errors)>0){

    errors <- errors[, c("COD_MAREA", "YEAR", date_field)]

    # this line add a comment to the errors dataframe which contain the value of the
    # df variable
    errors.date_field <- deparse(substitute(date_field))

    errors.date_field <- as.POSIXct(errors.date_field, format="%d/%m/%Y")

    errors <- addTypeOfError(errors, "ERROR: el año del campo ",
                             errors.date_field, " no coincide con el año ", year)

    return(errors)


  }

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
#' one for the mandatory variables by gear.
#' @param df: dataframe to check
#' @param variables: vector with variables to check.
#' @return A list with a dataframe of every variable with empty values. Every
#' dataframe contains erroneous rows.
check_empty_values_in_variables <- function (df, variables, helper_text){

  try(variables_in_df(df, variables))

  gear_characteristics <- get_gear_characteristics()
  characteristic_to_check <- levels(gear_characteristics$CARACTERISTICA)

  variables <- as.list(variables)

  if(helper_text!=""){
    helper_text <- paste(" in ", helper_text, " screen")
  }

  errors <- lapply(variables, function(x){

    # Only some characteristicas must be filled according to its gear:
    if(x %in% characteristic_to_check) {
      gear_code <- gear_characteristics[gear_characteristics[["CARACTERISTICA"]]==x,]
      error <- (df[ (df[[x]]=="" | is.na(df[[x]])) & df[["COD_ARTE"]]%in%gear_code[["COD_ARTE"]],])
      error <- addTypeOfError(error, "ERROR: Variable ", x, " empty", helper_text )
    } else {
      error <- (df[df[[x]]=="" | is.na(df[[x]]),])
      if (nrow(error)>0){
        error <- addTypeOfError(error, "ERROR: Variable ", x, " empty", helper_text )
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

#' Check if a species belong to a taxon
#' Search in worms database, via API, if a species belongs to a taxon.
#' @param specie specie to check
#' @param taxon_to_match taxon to match
#' @return TO DO
check_spe_belongs_to_taxon <- function (specie, taxon_to_match){
  # Get the AphiaID of the specie
  # I don't use worms package because I'm interested in catch the response in case
  # of the specie does not exists in WORMS
  url_specie <- sprintf("http://www.marinespecies.org/rest/AphiaIDByName/%s", specie)
  url_specie <- gsub(" ", "%20", url_specie)
  resp <- GET(url_specie)
  if (resp$status_code==204){
    return("this specie does not match in WORMS")
  }
  if (resp$status_code==206){
    return("multiple match in WORMS")
  }

  AphiaID_specie <- fromJSON(url_specie)

  #Build the URL to get the data from
  url <- sprintf("http://www.marinespecies.org/rest/AphiaClassificationByAphiaID/%d", AphiaID_specie);

  #Get the actual data from the URL
  tryCatch({
    classificationTree <- fromJSON(url)

    #Walk the classification tree
    currentTreeItem = classificationTree
    while (!is.null(currentTreeItem )) {
      if (currentTreeItem$scientificname == taxon_to_match){
        return(TRUE)
      } else {
        #Get next item in the tree
        currentTreeItem <- currentTreeItem$child;
      }
    }

    return(FALSE)

  },
  error = function(e){
    return(e$message)
  }
  )

}

#' Copy all the error files generated to a shared folder.
copyErrorsFilesToSharedFolder <- function (){

  # test if PATH_ERRORS exists
  ifelse(!file.exists(PATH_ERRORS), stop(paste("Folder", PATH_ERRORS, "does not exists.")), FALSE)

  # test if PATH_ERRORS have files
  ifelse(length(list.files(PATH_ERRORS))==0, stop(paste("Folder", PATH_ERRORS, "doesn't have files.")), FALSE)

  # if the share errors directory does not exists, create it:
  ifelse(!dir.exists(PATH_SHARE_ERRORS), dir.create(PATH_SHARE_ERRORS), FALSE)

  # test if there are files with the same name in folder. In this case,
  # nothing is saved.
  files_list_to <- list.files(PATH_SHARE_ERRORS)

  files_list_from <- list.files(PATH_ERRORS)

  if(any(files_list_from %in% files_list_to)){
    ae <- which(files_list_from %in% files_list_to)
    ae <- paste(files_list_from[ae], collapse = ", ")
    stop(paste("The file(s)", ae, "already exist(s). Nothing has been saved" ))

  }

  files_list_from <- file.path(PATH_ERRORS, files_list_from)
  file.copy(from=files_list_from, to=PATH_SHARE_ERRORS)

}

#' Export to pdf the graphic returned by a function.
#' @param filename name of the file to export (without extension).
#' @param fun function which return a graphic.
#' @param ... parameters of 'fun' function.
printPdfGraphic <- function(filename, func, ...){

  filename <- paste0(PATH_ERRORS, "/", filename, ".pdf")

  pdf(filename)

  g <- func(...)

  print(g)

  dev.off()
}

#' Create path files from the MONTH, YEAR and suffix_multiple_months.
#' @param month month or months used.
#' @param year year.
#' @param suffix_multiple_month Suffix used when multiple months are used.
createPathFiles <- function (month = MONTH, year = YEAR, suffix_multiple_months = suffix_multiple_months){

  if(length(month) != 1){
    path_text <- paste0("data/", year, "/", year, "_", suffix_multiple_months)
  } else {
    path_text <- paste0("data/", year, "/", year, "_", sprintf("%02d", month))
  }

  return(file.path(getwd(), path_text))

}

#' Create character with month, months, or any other tag to name the months used
#' in the names of files.
#' @param month month or months used.
#' @param suffix_multiple_month Suffix used when multiple months are used.
createMonthAsCharacter <- function(month = MONTH, suffix_multiple_months = suffix_multiple_months){

  if (length(month) == 1 && month %in% seq(1:12)){
    return(sprintf("%02d", month))
  } else if (length(month) > 1 & all(month %in% seq(1:12))) {
    return(suffix_multiple_months)
  } else {
    stop("Is there any error in the MONTH variable?")
  }

}

#' Create suffix with month, months, or any other tag to name the months used
#' in the names of files.
#' @param month month or months used.
#' @param suffix_multiple_month Suffix used when multiple months are used.
createSuffixToExport <- function(month,
                                 year,
                                 month_as_character,
                                 suffix_multiple_months){

  if (length(month) == 1 && month %in% seq(1:12)) {
    return(paste0(year, "_", month_as_character))
  } else if (length(month) > 1 & all(month %in% seq(1:12))) {
    return(paste0(year, "_", suffix_multiple_months))
  }

}


#' Create a xlsx file for each dataframe in a list of dataframes. The name of
#' every created file contains the name of its corresponding dataframe. This
#' function is based in exportListToXlsx from sapmuebase library and adapted to
#' this oab_post_dump script.
#' @param list list of dataframes.
#' @param prefix character string to add at the begining of the file name.
#' @param suffix character string to add at the end of the filename.
#' @param separation character string to separate the terms.
#' @param path_export path to save the file.
exportErrorsListToXlsx2 <- function (list, prefix = "", suffix = "", separation = "",
                               path_export = getwd())
{
  #check if package openxlsx is installed:
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Openxlsx package needed for this function to work. Please install it.",
         call = FALSE)
  }

  lapply(seq_along(list), function(i) {
    if (is.data.frame(list[[i]])) {

      list_name <- names(list)[[i]]
      if (prefix != "")
        prefix <- paste0(prefix, separation)
      if (suffix != "")
        suffix <- paste0(separation, suffix)
      filename <- paste0(path_export, "/", prefix, list_name, suffix, ".xlsx")

      # ---- Create a Workbook
      wb <- openxlsx::createWorkbook()

      # ---- Add worksheets
      # name_worksheet <- paste("0",MONTH,sep="")
      name_worksheet <- paste("0",MONTH_AS_CHARACTER,sep="")
      openxlsx::addWorksheet(wb, name_worksheet)

      # ---- Add data to the workbook
      openxlsx::writeData(wb, name_worksheet, list[[i]])

      # ---- Useful variables
      num_cols_df <- length(list[[i]])

      # ---- Stylize data
      # ---- Create styles
      head_style <- openxlsx::createStyle(fgFill = "#EEEEEE",
                                          fontName="Calibri",
                                          fontSize = "11",
                                          halign = "center",
                                          valign = "center")

      # ---- Apply styles
      openxlsx::addStyle(wb, sheet = name_worksheet, head_style, rows = 1, cols = 1:num_cols_df)

      # ---- Column widths: I don't know why, but it doesn't work in the right way
      openxlsx::setColWidths(wb, name_worksheet, cols = c(1:num_cols_df), widths = "auto")

      # ---- Export to excel
      # source: https://github.com/awalker89/openxlsx/issues/111
      Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") ## path to zip.exe
      openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
    }
    else {
      return(paste("This isn't a dataframe"))
    }
  })
}

# Export to google drive.
# Not in use.
# Export the dataframes contained in a list to google drive
OAB_export_list_google_sheet <- function(list, prefix = "", suffix = "", separation = ""){

  #check if package openxlsx is instaled:
  if (!requireNamespace("googlesheets", quietly = TRUE)) {
    stop("Googlesheets package needed for this function to work. Please install it.",
         call = FALSE)
  }

  # sep_along(list): generate regular sequences. With a list, generates
  # the sequence 1, 2, ..., length(from). Return a integer vector.
  lapply(seq_along(list), function(i){


    if(is.data.frame(list[[i]])){

      list_name <- names(list)[[i]]

      if (prefix != "") prefix <- paste0(prefix, separation)

      if (suffix != "") suffix <- paste0(separation, suffix)

      # Before export to google drive, is mandatory export file to csv in local:
      # When the googlesheet4 packages have the oauth implemented, we can
      # use it instead of googledrive package
      filename <- paste0(PATH_ERRORS, "/", prefix, list_name, suffix, '.csv')

      write.table(
        list[[i]],
        file = filename,
        quote = FALSE,
        sep = ",",
        dec = ".",
        row.names = FALSE,
        na = "")

      # export to google drive
      drive_upload(
        media = filename,
        path = as_dribble(GOOGLE_DRIVE_PATH),
        type = "spreadsheet"
      )

    } else {
      return(paste("This isn't a dataframe"))
    }

  })
}
