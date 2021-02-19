#### Check discards from SIRENO
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
####

# INSTRUCTIONS -----------------------------------------------------------------

# To use this script:

# - This scripts require the files oab_post_dump_functions.R,
# oab_post_dump_hauls_overlapped.R and oab_post_dump_haul_characteristics.R, so
# Make sure they are located in the same directory that this file.
# - Change variables in "YOU HAVE ONLY TO CHANGE THIS VARIABLES" section of this
# script.
# - Make sure report files of discards from SIRENO are in a path like
# /data/YYYY/YYYY_MM.
# - Choose the way to export in the "EXPORT ERRORS" section of this script.
# Uncomment the interested way. It's available by a xlsx file or upload directly
# to google drive. In this case an account and password is required, and a token
# is automatically generated.
# - If xlsx option is choosen, the errors file must be created in the directory
# /data/YYYY/YYYY_MM/errors. If the "errors" directory does not exists, it's
# created automatically.
# - TODO: explain speed graphics
# - A file by acronym type (DESIXA, DESSUR...) is generated in "errors"
# directory.

# YOU HAVE ONLY TO CHANGE THIS VARIABLES ---------------------------------------

trips_file <- "IEODESMAREAMARCO.TXT"
hauls_file <- "IEODESLANCEMARCO.TXT"
catches_file <- "IEODESCAPTURAMARCO.TXT"
# lengths_file <- "IEODESTALLASMARCO.TXT"
lengths_file <- "TALLAS_OAB_2020_ICES.TXT"
litter_file <- "IEODESBASURASMARCO.TXT"
accidentals_file <- "IEODESCAPTACCIDMARCO.TXT"

# MONTH: 1 to 12 or "annual" in case of annual check.
MONTH <- "annual"

YEAR_DISCARD <- 2020

# Suffix_id is a suffix added to filenames when they are exported both xls and
# google drive files
suffix_id <- ""

# Only required if the file will be uploaded to google drive. It is the path
# where in google drive will be saved.
# GOOGLE_DRIVE_PATH <- file.path("/equipo muestreos/review_oab/2020/errors/")

# PACKAGES ---------------------------------------------------------------------

library(plyr) # to use function join_all --> TO DO: change to reduce-merge functions
library(dplyr)
library(devtools)
# remove.packages("sapmuebase")
# .rs.restartR()
# install("C:/Users/ieoma/Desktop/sap/sapmuebase")
# install_github("Eucrow/sapmuebase")
library(sapmuebase)

library(googledrive)

library(tinsel)


# FUNCTIONS --------------------------------------------------------------------

# All the functions required in this script are located in the next files:
source('oab_post_dump_auxiliar_functions.R')
source_decoratees('oab_post_dump_functions.R')
source('oab_post_dump_hauls_overlapped.R')
source('oab_post_dump_haul_characteristics.R')

# LOAD DATASETS ----------------------------------------------------------------

especies_a_medir_OAB <- importCsvSAPMUE("especies_a_medir_OAB.csv")

especies_objetivo_oab <- importCsvSAPMUE("especies_objetivo_OAB.csv")

razon_descarte_OAB <- importCsvSAPMUE("razon_descarte_OAB.csv")

metier_ieo_especie_objetivo_OAB <- importCsvSAPMUE("metier_ieo_especie_objetivo_OAB.csv")

duracion_mareas_OAB <- importCsvSAPMUE("duracion_mareas.txt")

caracteristicas_lances <- importCsvSAPMUE("caracteristicas_lances.csv")

not_allowed_species_measured <- importCsvSAPMUE("not_allowed_species_measured.csv")

origin_statistical_rectangle <- importCsvSAPMUE("origin_statistical_rectangle.csv")
origin_statistical_rectangle$COD_ORIGEN <-  sprintf("%03d", origin_statistical_rectangle$COD_ORIGEN)

# this cephalopods master has been created using the annual OAB_catches
# dataframe of 2019. All the species has been checked via API with WORM webpage
# using the function check_spe_belongs_to_taxon()
cephalopods <- importCsvSAPMUE("cephalopods.csv")


# GLOBAL VARIABLES -------------------------------------------------------------

# list with all errors found in dataframes:
ERRORS <- list()

# path to the work files
# PATH_FILES <- "F:/misdoc/sap/oab_post_dump/data/2019/1_checking"
# PATH_FILES <- "C:/Users/Marco IEO/Desktop/oab_post_dump/data/2019/1_checking"
if (MONTH == "annual"){
  path_text <- paste0("data/", YEAR_DISCARD, "/", YEAR_DISCARD, "_annual")
} else {
  path_text <- paste0("data/", YEAR_DISCARD, "/", YEAR_DISCARD, "_", sprintf("%02d", MONTH))
}

PATH_FILES <- file.path(getwd(), path_text)

# path to the generated errors file
PATH_ERRORS <- file.path(PATH_FILES,"errors")
# if the errors directory does not exists, create it:
ifelse(!dir.exists(PATH_ERRORS), dir.create(PATH_ERRORS), FALSE)

# month as character in case of mensual check
if (MONTH != "annual"){
  MONTH_AS_CHARACTER <- ifelse(isFALSE(MONTH), "", sprintf("%02d", MONTH))
} else {
  MONTH_AS_CHARACTER <- MONTH
}

# names to export
prefix_to_export <- "OAB"

if (MONTH == "annual"){
  suffix_to_export <- paste0(YEAR_DISCARD, "_annual", ifelse(suffix_id!="", paste0("_",suffix_id), ""))
} else {
  suffix_to_export <- ifelse(substr(suffix_to_export, nchar(suffix_to_export), nchar(suffix_to_export))=="_",
                             substr(suffix_to_export, 0, nchar(suffix_to_export)-1),
                             suffix_to_export )
}

# IMPORT DISCARDS FILES --------------------------------------------------------

# discards_samples <- importOABFiles(trips_file, hauls_file, catches_file, lengths_file,
                                   # litter_file, accidentals_file, path = PATH_FILES)

OAB_trips <- importOABTrips(trips_file, path = PATH_FILES)

OAB_hauls <- importOABHauls(hauls_file, path = PATH_FILES)

OAB_catches <- importOABCatches(catches_file, path = PATH_FILES)

OAB_lengths <- importOABLengths(lengths_file, path = PATH_FILES)

OAB_litter <- importOABLitter(litter_file, path = PATH_FILES)

OAB_accidentals <- importOABAccidentals(accidentals_file, path = PATH_FILES)

# FILTER BY MONTH --------------------------------------------------------------

if(MONTH != "annual"){
  OAB_trips <- OAB_trips[as.POSIXlt(OAB_trips$FECHA_INI)$mon +1 == MONTH,]
  OAB_hauls <- OAB_hauls[OAB_hauls$COD_MAREA%in%OAB_trips$COD_MAREA,]
  OAB_catches <- OAB_catches[OAB_catches$COD_MAREA%in%OAB_trips$COD_MAREA,]
  OAB_lengths <- OAB_lengths[OAB_lengths$COD_MAREA%in%OAB_trips$COD_MAREA,]
}


# FILTER BY ACRONYM ------------------------------------------------------------

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

# FILTER BY OBSERVER -----------------------------------------------------------

# ADRIAN
# OAB_trips <- OAB_trips[ which(OAB_trips[["NOMBRE_OBS"]] == "ADRIAN"), ]
# OAB_hauls <- OAB_hauls[OAB_hauls$COD_MAREA%in%OAB_trips$COD_MAREA,]
# OAB_catches <- OAB_catches[OAB_catches$COD_MAREA%in%OAB_trips$COD_MAREA,]
# OAB_lengths <- OAB_lengths[OAB_lengths$COD_MAREA%in%OAB_trips$COD_MAREA,]

# SEARCHING ERRORS -------------------------------------------------------------

check_them_all <- function(){
  
  ERR <- list()
  
  # ALLTOGETHER
    # ERR$all_empty_fields_in_variable <- empty_fields_in_variables()
    # ppp <- check_empty_variables()
    # ppp_number <- as.data.frame(table(ppp$TIPO_ERROR))
  
  # TRIPS
  ERR$trips_origen <- check_variable_with_master(OAB_trips, "COD_ORIGEN")
  
  ERR$trips_estrato_rim <- check_variable_with_master(OAB_trips, "ESTRATO_RIM")
  
  ERR$trips_puerto_llegada <- check_variable_with_master(OAB_trips, "COD_PUERTO_LLEGADA")
  
  ERR$trips_puerto_descarga <- check_variable_with_master(OAB_trips, "COD_PUERTO_DESCARGA")
  
  ERR$trips_empty_fields <- empty_fields_in_variables(OAB_trips, "OAB_TRIPS")
  
  ERR$trips_field_year <- field_year(OAB_trips)
  
  ERR$trips_year_in_COD_MAREA <- check_year_in_COD_MAREA(OAB_trips)
  
  ERR$trips_year_in_initial_date <- check_year_in_date(OAB_trips, "FECHA_INI", YEAR_DISCARD)
  
  ERR$trips_year_in_final_date <- check_year_in_date(OAB_trips, "FECHA_FIN", YEAR_DISCARD)
  
  ERR$trips_final_date_in_COD_MAREA_GC <- trips_final_date_in_COD_MAREA_GC()
  
  ERR$trips_initial_date_before_final_date <- trips_initial_date_before_final_date()

  ERR$coherencia_estrato_rim_origin <- coherence_rim_stratum_origin(OAB_trips)
  
  ERR$trip_duration <- trip_duration()
  
  ERR$checked_trips <- trip_is_checked(OAB_trips)
  
  # HAULS
  ERR$hauls_arte <- check_variable_with_master(OAB_hauls, "COD_ARTE")
  
  ERR$hauls_empty_fields <- empty_fields_in_variables(OAB_hauls, "OAB_HAULS")
  
  ERR$hauls_field_year <- field_year(OAB_hauls)
  
  ERR$hauls_year_in_COD_MAREA_hauls <- check_year_in_COD_MAREA(OAB_hauls)
  
  ERR$hauls_year_in_shotting_date <- check_year_in_date(OAB_hauls, "FECHA_LAR", YEAR_DISCARD)
  
  ERR$hauls_year_in_hauling_date <- check_year_in_date(OAB_hauls, "FECHA_VIR", YEAR_DISCARD)
  
  ERR$hauling_date_before_shooting_date <- hauling_date_before_shooting_date()
  
  ERR$hauls_coherence_estrato_rim_origin <- coherence_rim_stratum_origin(OAB_hauls)

  ERR$hauls_coherence_estrato_rim_gear <- coherence_rim_stratum_gear(OAB_hauls)

  ERR$hauls_sampled_with_catch_weights <- hauls_sampled_with_catch_weights()
  
  ERR$hauls_possible_speed_outliers <- get_speed_outliers()
  
  # Remove this check: maybe delete the master too?
  #ERR$hauls_target_sp_with_catch <- coherence_target_sp_with_catch()
  
  ERR$hauls_coherence_target_species_metier_ieo <- coherence_target_species_metier_ieo()
  
  ERR$hauls_length_cable_1000 <- length_cable_1000()
  
  ERR$hauls_overlapped <- hauls_overlapped()
  
  ERR$total_discarded_weight_zero_with_sampled_discard_weight <- total_discarded_weight_zero_with_sampled_discard_weight()
  
  ERR$hauls_duration <- check_hauls_duration()
  
  ERR$hauls_speed <-  check_hauls_speed()
  
  ERR$hauls_depth <-  check_hauls_depth()
  
  # the next one commented temporally:
  # ERR$coherence_origin_statistical_rectangle <- coherence_origin_statistical_rectangle()
  
  ERR$haul_date_shooting_date <- haul_date_shooting_date()
  
  ERR$positive_longitude_shooting <- positive_longitude("LON_LAR_CGS")
  
  ERR$positive_longitude_hauling <- positive_longitude("LON_VIR_CGS")
  
  ERR$checked_hauls <- haul_is_checked(OAB_hauls)
  
  # CATCHES
  ERR$catches_empty_fields <- empty_fields_in_variables(OAB_catches, "OAB_CATCHES")
  
  ERR$catches_discard_reason <- check_discard_reason_variable_with_master(OAB_catches)
  
  ERR$catches_field_year <- field_year(OAB_catches)
  
  ERR$catches_year_in_COD_MAREA <- check_year_in_COD_MAREA(OAB_catches)
  
  ERR$species_without_caught_neither_discarded_weight <- species_without_caught_neither_discarded_weight()
  
  ERR$reason_discard_field_filled <- reason_discard_field_filled(OAB_catches)
  
  ERR$retained_catch_less_than_sampled_retained_catch <- retained_catch_less_than_sampled_retained_catch()
  
  ERR$discarded_weight_less_than_sampled_discarded_weight <- discarded_weight_less_than_sampled_discarded_weight(OAB_catches)

  ERR$sampled_discard_less_subsample_discard <- sampled_discard_less_subsample_discard(OAB_catches)
  
  ERR$retained_sampled_weight_when_specimens_retained <- retained_sampled_weight_when_specimens_retained(OAB_catches)
  
  ERR$discarded_sampled_weight_when_specimens_discarded <- discarded_sampled_weight_when_specimens_discarded(OAB_catches)
  
  ERR$discarded_species_with_total_discarded_weight <- discarded_species_with_total_discarded_weight(OAB_lengths, OAB_hauls)
  
  ERR$reason_discard_field_filled <- reason_discard_field_filled(OAB_catches)
  
  ERR$doubtfull_sp_number_specimens <- doubtfull_sp_number_specimens()
  
  ERR$species_not_allowed <- species_not_allowed()
  
  ERR$cephalopods_counted <- cephalopods_counted()
  
  # LENGTHS
  ERR$lengths_empty_fields <- empty_fields_in_variables(OAB_lengths, "OAB_LENGTHS")

  ERR$lengths_field_year <- field_year(OAB_lengths)

  ERR$lengths_year_in_COD_MAREA <- check_year_in_COD_MAREA(OAB_lengths)
  
  ERR$priority_species_without_lengths <- priority_species_without_lengths()
  
  ERR$with_historical_size_range <- check_species_in_size_range_historical()
  ERR$size_range <- check_size_range()

  ERR$lenghts_not_allowed_taxons <- lenghts_not_allowed_taxons()
  
  # LITTER
  ERR$litter_sample <- litter_sample()
  
  # MIXED
  ERR$errors_date_hauls_in_date_interval_trips <- date_hauls_in_date_interval_trips(OAB_trips, OAB_hauls)
  ERR$final_date_one_day_before_hauling <- final_date_one_day_before_hauling()
  
  return(ERR)
}

check_them_all_annual <- function(){
  
  ERR <- list()
  
  # ALLTOGETHER
  # ERR$all_empty_fields_in_variable <- empty_fields_in_variables()
  # ppp <- check_empty_variables()
  # ppp_number <- as.data.frame(table(ppp$TIPO_ERROR))
  
  # TRIPS
  ERR$trips_origen <- check_variable_with_master(OAB_trips, "COD_ORIGEN")
  
  ERR$trips_estrato_rim <- check_variable_with_master(OAB_trips, "ESTRATO_RIM")
  
  ERR$trips_puerto_llegada <- check_variable_with_master(OAB_trips, "COD_PUERTO_LLEGADA")
  
  ERR$trips_puerto_descarga <- check_variable_with_master(OAB_trips, "COD_PUERTO_DESCARGA")
  
  ERR$trips_empty_fields <- empty_fields_in_variables(OAB_trips, "OAB_TRIPS")
  
  ERR$trips_field_year <- field_year(OAB_trips)
  
  ERR$trips_year_in_COD_MAREA <- check_year_in_COD_MAREA(OAB_trips)
  
  ERR$trips_year_in_initial_date <- check_year_in_date(OAB_trips, "FECHA_INI", YEAR_DISCARD)
  
  ERR$trips_year_in_final_date <- check_year_in_date(OAB_trips, "FECHA_FIN", YEAR_DISCARD)
  
  ERR$trips_final_date_in_COD_MAREA_GC <- trips_final_date_in_COD_MAREA_GC()
  
  ERR$trips_initial_date_before_final_date <- trips_initial_date_before_final_date()
  
  ERR$coherencia_estrato_rim_origin <- coherence_rim_stratum_origin(OAB_trips)
  
  ERR$trip_duration <- trip_duration()
  
  ERR$checked_trips <- trip_is_checked(OAB_trips)
  
  
  # HAULS
  ERR$hauls_arte <- check_variable_with_master(OAB_hauls, "COD_ARTE")
  
  ERR$hauls_empty_fields <- empty_fields_in_variables(OAB_hauls, "OAB_HAULS")
  
  ERR$hauls_field_year <- field_year(OAB_hauls)
  
  ERR$hauls_year_in_COD_MAREA_hauls <- check_year_in_COD_MAREA(OAB_hauls)
  
  ERR$hauls_year_in_shotting_date <- check_year_in_date(OAB_hauls, "FECHA_LAR", YEAR_DISCARD)
  
  ERR$hauls_year_in_hauling_date <- check_year_in_date(OAB_hauls, "FECHA_VIR", YEAR_DISCARD)
  
  ERR$hauling_date_before_shooting_date <- hauling_date_before_shooting_date()
  
  ERR$hauls_coherence_estrato_rim_origin <- coherence_rim_stratum_origin(OAB_hauls)
  
  ERR$hauls_coherence_estrato_rim_gear <- coherence_rim_stratum_gear(OAB_hauls)
  
  ERR$hauls_sampled_with_catch_weights <- hauls_sampled_with_catch_weights()
  
  ERR$hauls_possible_speed_outliers <- get_speed_outliers()
  
  # Remove this check: maybe delete the master too?
  #ERR$hauls_target_sp_with_catch <- coherence_target_sp_with_catch()
  
  ERR$hauls_coherence_target_species_metier_ieo <- coherence_target_species_metier_ieo()
  
  ERR$hauls_length_cable_1000 <- length_cable_1000()
  
  ERR$hauls_overlapped <- hauls_overlapped()
  
  ERR$total_discarded_weight_zero_with_sampled_discard_weight <- total_discarded_weight_zero_with_sampled_discard_weight()
  
  ERR$hauls_duration <- check_hauls_duration()
  
  ERR$hauls_speed <-  check_hauls_speed()
  
  ERR$hauls_depth <-  check_hauls_depth()
  
  # the next one commented temporally:
  # ERR$coherence_origin_statistical_rectangle <- coherence_origin_statistical_rectangle()
  
  ERR$haul_date_shooting_date <- haul_date_shooting_date()
  
  ERR$positive_longitude_shooting <- positive_longitude("LON_LAR_CGS")
  
  ERR$positive_longitude_hauling <- positive_longitude("LON_VIR_CGS")
  
  ERR$checked_hauls <- haul_is_checked(OAB_hauls)
  
  # CATCHES
  ERR$catches_empty_fields <- empty_fields_in_variables(OAB_catches, "OAB_CATCHES")
  
  ERR$catches_discard_reason <- check_discard_reason_variable_with_master(OAB_catches)
  
  ERR$catches_field_year <- field_year(OAB_catches)
  
  ERR$catches_year_in_COD_MAREA <- check_year_in_COD_MAREA(OAB_catches)
  
  ERR$species_without_caught_neither_discarded_weight <- species_without_caught_neither_discarded_weight()
  
  ERR$reason_discard_field_filled <- reason_discard_field_filled(OAB_catches)
  
  ERR$retained_catch_less_than_sampled_retained_catch <- retained_catch_less_than_sampled_retained_catch()
  
  ERR$discarded_weight_less_than_sampled_discarded_weight <- discarded_weight_less_than_sampled_discarded_weight(OAB_catches)
  
  ERR$sampled_discard_less_subsample_discard <- sampled_discard_less_subsample_discard(OAB_catches)
  
  ERR$retained_sampled_weight_when_specimens_retained <- retained_sampled_weight_when_specimens_retained(OAB_catches)
  
  ERR$discarded_sampled_weight_when_specimens_discarded <- discarded_sampled_weight_when_specimens_discarded(OAB_catches)
  
  ERR$discarded_species_with_total_discarded_weight <- discarded_species_with_total_discarded_weight(OAB_lengths, OAB_hauls)
  
  ERR$reason_discard_field_filled <- reason_discard_field_filled(OAB_catches)
  
  ERR$doubtfull_sp_number_specimens <- doubtfull_sp_number_specimens()
  
  ERR$species_not_allowed <- species_not_allowed()
  
  ERR$cephalopods_counted <- cephalopods_counted()
  
  # LENGTHS
  ERR$lengths_empty_fields <- empty_fields_in_variables(OAB_lengths, "OAB_LENGTHS")
  
  ERR$lengths_field_year <- field_year(OAB_lengths)
  
  ERR$lengths_year_in_COD_MAREA <- check_year_in_COD_MAREA(OAB_lengths)
  
  ERR$priority_species_without_lengths <- priority_species_without_lengths()
  
  # ERR$with_historical_size_range <- check_species_in_size_range_historical()
  # ERR$size_range <- check_size_range()
  
  ERR$lenghts_not_allowed_taxons <- lenghts_not_allowed_taxons()
  
  # LITTER
  ERR$litter_sample <- litter_sample()
  
  # MIXED
  ERR$errors_date_hauls_in_date_interval_trips <- date_hauls_in_date_interval_trips(OAB_trips, OAB_hauls)
  ERR$final_date_one_day_before_hauling <- final_date_one_day_before_hauling()
  
  return(ERR)
}

# ERRORS <- check_them_all()
ERRORS <- check_them_all_annual()

# FORMAT ERRORS ----------------------------------------------------------------

combined_errors <- formatErrorsList()

errors <- separate_df_by_acronym(combined_errors)

# Remove columns with only NA values
# Filter extracts the elements of a vector for which a predicate (logical)
# function gives true
errors <-  lapply(errors, function(x){
  Filter(function(x){!all(is.na(x))}, x)
})

# EXPORT ERRORS ----------------------------------------------------------------

# Export to xls
# exportListToXlsx(errors, prefix = prefix_to_export,
#                  suffix = paste(suffix_to_export,"_checks"), 
#                  separation = "_", path_export = PATH_ERRORS)

# Export to google drive 
# Export the dataframes contained in a list to google drive
# OAB_export_list_google_sheet <- function(list, prefix = "", suffix = "", separation = ""){
#   
#   #check if package openxlsx is instaled:
#   if (!requireNamespace("googlesheets", quietly = TRUE)) {
#     stop("Googlesheets package needed for this function to work. Please install it.",
#          call = FALSE)
#   }
#   
#   # sep_along(list): generate regular sequences. With a list, generates
#   # the sequence 1, 2, ..., length(from). Return a integer vector.
#   lapply(seq_along(list), function(i){
#     
#     
#     if(is.data.frame(list[[i]])){
#       
#       list_name <- names(list)[[i]]
#       
#       if (prefix != "") prefix <- paste0(prefix, separation)
#       
#       if (suffix != "") suffix <- paste0(separation, suffix)
#       
#       # Before export to google drive, is mandatory export file to csv in local:
#       # When the googlesheet4 packages have the oauth implemented, we can
#       # use it instead of googledrive package
#       filename <- paste0(PATH_ERRORS, "/", prefix, list_name, suffix, '.csv')
#       
#       write.table(
#         list[[i]], 
#         file = filename, 
#         quote = FALSE, 
#         sep = ",", 
#         dec = ".", 
#         row.names = FALSE,
#         na = "")
#       
#       # export to google drive
#       drive_upload(
#         media = filename,
#         path = as_dribble(GOOGLE_DRIVE_PATH),
#         type = "spreadsheet"
#       )
#       
#     } else {
#       return(paste("This isn't a dataframe"))
#     }
#     
#   })
# }
# 
# OAB_export_list_google_sheet(errors, prefix = prefix_to_export,
#                         suffix = suffix_to_export,
#                         separation = "_")


# CHECK SPEED ------------------------------------------------------------------
# print_pdf_graphic <- function(filename, func, ...){
# 
#   filename <- paste0(PATH_ERRORS, "/", filename, ".pdf")
# 
#   pdf(filename)
# 
#   g <- func(...)
# 
#   print(g)
# 
#   dev.off()
# }
# 
# view_speed_outliers()
# filename <- paste("speed_outliers", YEAR_DISCARD,  MONTH_AS_CHARACTER, sep ="_")
# print_pdf_graphic(filename, view_speed_outliers)

# PRUEBAS CON SHINY: AL FINAL PARECE QUE NO SE PUEDEN MOSTRAR CON UN BOXPLOT
# library(shiny)
# runApp("speed", display.mode = "showcase")




