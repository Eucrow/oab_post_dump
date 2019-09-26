#### Check discards from SIRENO
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
####
#### files required: 
####

# #### PACKAGES ################################################################

library(plyr) # to use function join_all --> TO DO: change to reduce-merge functions
library(dplyr)
library(devtools)
# remove.packages("sapmuebase")
# .rs.restartR()
# install("F:/misdoc/sap/sapmuebase")
# install_github("Eucrow/sapmuebase")
library(sapmuebase)

library(googledrive)

# #### LOAD DATASETS ###########################################################

# TO DO: add this datasets in sapmuebase
origen_OAB <- read.table("origenes_OAB.csv", header = T, sep = ";",
                         colClasses = c("factor", "character"))
estrato_rim_OAB <- read.table("estrato_rim_OAB.csv", header = T, sep = ";")
puerto_OAB <- read.table("puerto_OAB.csv", header = T, sep = ";",
                         colClasses = c("factor"))
arte_OAB <- read.table("arte_OAB.csv", header = T, sep = ";",
                         colClasses = c("factor", "character"))
especies_a_medir_OAB <- read.table("especies_a_medir_OAB.csv", header = T, sep = ";",
                         colClasses = c("factor"))
especies_objetiVo_oab <- read.table("especies_objetivo_OAB.csv", header = T, sep = ";")


# YOU HAVE ONLY TO CHANGE THIS VARIABLES ---------------------------------------

# PATH_FILES <- "F:/misdoc/sap/oab_post_dump/data/2019/1_checking"
# PATH_FILES <- "C:/Users/Marco IEO/Desktop/oab_post_dump/data/2019/1_checking"
PATH_FILES <- file.path(getwd(), "data/2019/1_checking")
trips_file <- "IEODESMAREAMARCO.TXT"
hauls_file <- "IEODESLANCEMARCO.TXT"
catches_file <- "IEODESCAPTURAMARCO.TXT"
lengths_file <- "IEODESTALLASMARCO.TXT"

MONTH <- FALSE

YEAR_DISCARD <- "2019"

# Suffix_id is a suffix added to filenames when they are exported both xls and
# google drive files
suffix_id <- "1_check"

# Only required if the file will be uploaded to google drive. It is the path
# where in google drive will be saved.
GOOGLE_DRIVE_PATH <- "/equipo muestreos/oab_post_dump/2019/errors/"

# GLOBAL VARIABLES -------------------------------------------------------------

# list with the common fields used in all tables
BASE_FIELDS <- c("YEAR", "COD_MAREA")

# list with all errors found in dataframes:
ERRORS <- list()

# path to the generated errors file
PATH_ERRORS <- paste(PATH_FILES,"/errors", sep="")

# month as character
MONTH_AS_CHARACTER <- ifelse(isFALSE(MONTH), "", sprintf("%02d", MONTH))

# names to export
prefix_to_export <- "OAB"
suffix_to_export <- paste(YEAR_DISCARD, MONTH_AS_CHARACTER, suffix_id, sep = "_")

# #### SET WORKING DIRECTORY ###################################################


# setwd("F:/misdoc/sap/oab_post_dump/")


# #### FUNCTIONS ###############################################################

# All the functions required in this script are located in the next files:
source('oab_post_dump_functions.R')

# #### IMPORT DISCARDS FILES ###################################################


# discards_samples <- importOABFiles(trips_file, hauls_file, catches_file, lengths_file,
                                    # path = PATH_FILES)
# 
# OAB_trips <- discards_samples$trips
# OAB_hauls <- discards_samples$hauls
# OAB_catches <- discards_samples$catches
# OAB_lengths <- discards_samples$lengths
OAB_trips <- importOABTrips(trips_file, path = PATH_FILES)

OAB_hauls <- importOABHauls(hauls_file, path = PATH_FILES)

OAB_catches <- importOABCatches(catches_file, path = PATH_FILES)

OAB_lengths <- importOABLengths(lengths_file, path = PATH_FILES)

subsample <- OAB_catches[which(OAB_catches$P_SUB_MUE_TOT != OAB_catches$P_MUE_DESCAR),]
subsample$dif <- subsample$P_SUB_MUE_TOT - subsample$P_MUE_DESCAR

# #### FILTER BY MONTH #########################################################

# TO DO: change all the dates in the same format

trips_fecha_ini <- as.POSIXct(OAB_trips$FECHA_INI, format = '%d/%m/%Y')
OAB_trips$MONTH <- as.POSIXlt(trips_fecha_ini)$mon + 1
# OAB_trips <- OAB_trips[OAB_trips$MONTH == MONTH,]

OAB_hauls <- OAB_hauls[OAB_hauls$COD_MAREA%in%OAB_trips$COD_MAREA,]
OAB_catches <- OAB_catches[OAB_catches$COD_MAREA%in%OAB_trips$COD_MAREA,]


# #### FILTER BY ACRONYM #######################################################


# DESNOR
# OAB_trips <- OAB_trips[ grep("DESNOR", OAB_trips$COD_MAREA), ]
# OAB_hauls <- OAB_hauls[ grep("DESNOR", OAB_hauls$COD_MAREA), ]
# OAB_catches <- OAB_catches[ grep("DESNOR", OAB_catches$COD_MAREA), ]

# DESSUR
# OAB_trips <- OAB_trips[ grep("DESSUR", OAB_trips$COD_MAREA), ]
# OAB_hauls <- OAB_hauls[ grep("DESSUR", OAB_hauls$COD_MAREA), ]
# OAB_catches <- OAB_catches[ grep("DESSUR", OAB_catches$COD_MAREA), ]

# DESIXA
# OAB_trips <- OAB_trips[ grep("(DESIXA)(?!C)", OAB_trips$COD_MAREA, perl = T), ]
# OAB_hauls <- OAB_hauls[ grep("(DESIXA)(?!C)", OAB_hauls$COD_MAREA, perl = T), ]
# OAB_catches <- OAB_catches[ grep("(DESIXA)(?!C)", OAB_catches$COD_MAREA, perl = T), ]


# #### SEARCHING ERRORS ########################################################


check_them_all <- function(){
  
  ERR <- list()
  
  # ALLTOGETHER
    # ERR$all_empty_fields_in_variable <- check_empty_fields_in_variables()
    # ppp <- check_empty_variables()
    # ppp_number <- as.data.frame(table(ppp$TIPO_ERROR))
  
  # TRIPS
  ERR$trips_origen <- check_variable_with_master(OAB_trips, "COD_ORIGEN")
  ERR$trips_estrato_rim <- check_variable_with_master(OAB_trips, "ESTRATO_RIM")
  ERR$trips_puerto_llegada <- check_variable_with_master(OAB_trips, "COD_PUERTO_LLEGADA")
  ERR$trips_puerto_descarga <- check_variable_with_master(OAB_trips, "COD_PUERTO_DESCARGA")
  
  
  
  ERR$trips_empty_fields <- check_empty_fields_in_variables(OAB_trips, "OAB_TRIPS")
  
  ERR$trips_field_year <- check_field_year(OAB_trips)
  
  ERR$trips_year_in_COD_MAREA <- check_year_in_COD_MAREA(OAB_trips)
  
  ERR$trips_year_in_initial_date <- check_year_in_date(OAB_trips, "FECHA_INI", YEAR_DISCARD)
  ERR$trips_year_in_final_date <- check_year_in_date(OAB_trips, "FECHA_FIN", YEAR_DISCARD)
  
  ERR$trips_check_final_date_in_COD_MAREA_GC <- trips_check_final_date_in_COD_MAREA_GC()
  
  ERR$trips_check_initial_date_before_final_date <- trips_check_initial_date_before_final_date()

  
  ERR$coherencia_estrato_rim_origin <- checkCoherenceEstratoRimOrigin(OAB_trips)
  
  # HAULS
  ERR$hauls_arte <- check_variable_with_master(OAB_hauls, "COD_ARTE")
  ERR$hauls_empty_fields <- check_empty_fields_in_variables(OAB_hauls, "OAB_HAULS")
  
  ERR$hauls_field_year <- check_field_year(OAB_hauls)
  
  ERR$hauls_year_in_COD_MAREA_hauls <- check_year_in_COD_MAREA(OAB_hauls)
  
  OAB_hauls$FECHA_LAR <- dby_to_dmy_date_format(OAB_hauls$FECHA_LAR)
  ERR$hauls_year_in_shotting_date <- check_year_in_date(OAB_hauls, "FECHA_LAR", YEAR_DISCARD)
  
  OAB_hauls$FECHA_VIR <- dby_to_dmy_date_format(OAB_hauls$FECHA_VIR)
  ERR$hauls_year_in_hauling_date <- check_year_in_date(OAB_hauls, "FECHA_VIR", YEAR_DISCARD)
  
  ERR$hauls_check_hauling_date_before_shooting_date <- hauls_check_hauling_date_before_shooting_date()
  
  ERR$hauls_coherence_estrato_rim_origin <- checkCoherenceEstratoRimOrigin(OAB_hauls)

  ERR$hauls_coherence_estrato_rim_gear <- checkCoherenceEstratoRimGear(OAB_hauls)

  ERR$hauls_sampled_with_catch_weights <- hauls_sampled_with_catch_weights()
  
  ERR$hauls_possible_speed_outliers <- get_speed_outliers()
  
  ERR$hauls_hauls_duration <- hauls_hauls_duration()
  
  ERR$hauls_target_sp_with_catch <- check_target_sp_with_catch()
  
  ERR$target_species_metier_ieo <- target_species_metier_ieo()
  
  ERR$length_cable_1000 <- length_cable_1000()
  
  
  # CATCHES
  
  ERR$catches_empty_fields <- check_empty_fields_in_variables(OAB_catches, "OAB_CATCHES")
  
  ERR$catches_field_year <- check_field_year(OAB_catches)
  
  ERR$catches_year_in_COD_MAREA <- check_year_in_COD_MAREA(OAB_catches)
  ERR$species_without_caught_neither_discarded_weight <- species_without_caught_neither_discarded_weight()
  
  #ERR$catches_reason_discard_field_empty <- catches_reason_discard_field_empty()
  
  ERR$catches_less_retained_catch_than_sampled_retained_catch <- catches_less_retained_catch_than_sampled_retained_catch()
  ERR$catches_less_discard_weight_than_sampled_discard_weight <- catches_less_discard_weight_than_sampled_discard_weight()

  
  ERR$total_discard_less_subsample_discard <- total_discard_less_subsample_discard(OAB_catches)
  ERR$sampled_discard_less_subsample_discard <- sampled_discard_less_subsample_discard(OAB_catches)
  
  ERR$retained_sampled_weight_when_specimens_retained <- retained_sampled_weight_when_specimens_retained(OAB_catches)
  ERR$discarded_sampled_weight_when_specimens_discarded <- discarded_sampled_weight_when_specimens_discarded(OAB_catches)
  
  ERR$reason_discard_field_filled <- reason_discard_field_filled(OAB_catches)
  
  # LENGTHS
  # next line: uncomment when remove the not mandatory fields in formato variables (see trello)
  ERR$lengths_empty_fields <- check_empty_fields_in_variables(OAB_lengths, "OAB_LENGTHS")

  ERR$lengths_field_year <- check_field_year(OAB_lengths)

  ERR$lengths_year_in_COD_MAREA <- check_year_in_COD_MAREA(OAB_lengths)
  ERR$priority_species_without_lengths <- priority_species_without_lengths()
  

  
  return(ERR)
}


ERRORS <- check_them_all()


    
# FORMAT ERRORS ----------------------------------------------------------------

combined_errors <- formatErrorsList()

errors <- separate_df_by_acronym(combined_errors)

# EXPORT ERRORS ----------------------------------------------------------------

# Export to xls
original_wd <- getwd()
# TO DO: add the variable PATH_ERRORS to exportListToXlsx function
setwd(PATH_ERRORS)
exportListToXlsx(errors, prefix = prefix_to_export,
                 suffix = suffix_to_export, 
                 separation = "_")
setwd(original_wd)

# Export to google drive 
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
        path = GOOGLE_DRIVE_PATH,
        type = "spreadsheet"
      )
      
    } else {
      return(paste("This isn't a dataframe"))
    }
    
  })
}

OAB_export_list_google_sheet(errors, prefix = prefix_to_export,
                        suffix = suffix_to_export,
                        separation = "_")



# CHECK SPEED ------------------------------------------------------------------
print_pdf_graphic <- function(filename, func, ...){
  
  filename <- paste0(PATH_ERRORS, "/", filename, ".pdf")
  
  pdf(filename)
  
  g <- func(...)
  
  print(g)
  
  dev.off()
}

view_speed_outliers()
filename <- paste("speed_outliers", YEAR_DISCARD,  MONTH_AS_CHARACTER, sep ="_")
print_pdf_graphic(filename, view_speed_outliers)

# PRUEBAS CON SHINY: AL FINAL PARECE QUE NO SE PUEDEN MOSTRAR CON UN BOXPLOT
# library(shiny)
# runApp("speed", display.mode = "showcase")

