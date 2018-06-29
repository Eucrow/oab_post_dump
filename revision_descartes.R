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

# library(plyr)
library(dplyr)
library(devtools)
# remove.packages("sapmuebase")
# install("F:/misdoc/sap/sapmuebase")
library(sapmuebase)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# YOU HAVE ONLY TO CHANGE THIS VARIABLES 

PATH_FILES <- "F:/misdoc/sap/revision_descartes/data/2018/2018_01"
trips_file <- "IEODESMAREAMARCO.TXT" 
hauls_file <- "IEODESLANCEMARCO.TXT"
catches_file <- "IEODESCAPTURAMARCO.TXT"
# lengths_file <- "IEODESTALLASMARCO.TXT"

YEAR_DISCARD <- "2018"


# ------------------------------------------------------------------------------
# #### GLOBAL VARIABLES ########################################################
# ------------------------------------------------------------------------------

# list with the common fields used in all tables
BASE_FIELDS <- c("YEAR", "ID_MAREA")

# list with all errors found in dataframes:
ERRORS <- list()

# path to the generated errors file
PATH_ERRORS <- paste(PATH_FILES,"/errors", sep="")

# ------------------------------------------------------------------------------
# #### SET WORKING DIRECTORY ###################################################
# ------------------------------------------------------------------------------

setwd("F:/misdoc/sap/revision descartes/")

# ------------------------------------------------------------------------------
# #### FUNCTIONS ###############################################################
# ------------------------------------------------------------------------------
# All the functions required in this script are located in
# revision_volcado_functions.R file.
source('revision_descartes_functions.R')


# ------------------------------------------------------------------------------
# #### IMPORT DISCARDS FILES ###################################################
# ------------------------------------------------------------------------------

# discards_samples <- importOABFiles(trips_file, hauls_file, catches_file, lengths_file,
#                                    path = PATH_FILES)
# 
# OAB_trips <- discards_samples$trips
# OAB_hauls <- discards_samples$hauls
# OAB_catches <- discards_samples$catches
# OAB_lengths <- discards_samples$lengths

OAB_trips <- importOABTrips(trips_file, path = PATH_FILES)
OAB_hauls <- importOABHauls(hauls_file, path = PATH_FILES)
OAB_catches <- importOABCatches(catches_file, path = PATH_FILES)

# ------------------------------------------------------------------------------
# #### SEARCHING ERRORS ########################################################
# ------------------------------------------------------------------------------

#create errors in dataframe to check functions
#levels(OAB_catches[["ID_MAREA"]]) <- c(levels(OAB_catches[["ID_MAREA"]]), "DESIXAC150515M", "DESIXAC160329M")
#OAB_catches[1:2, c("ID_MAREA")] <- c("DESIXAC150515M", "DESIXAC160329M")

#create errors in dataframe to check functions
#levels(OAB_trips[["ID_MAREA"]]) <- c(levels(OAB_trips[["ID_MAREA"]]), "DESIXAC150515M", "DESIXAC160329M")
#OAB_trips[1:2, c("ID_MAREA")] <- c("DESIXAC150515M", "DESIXAC160329M")



check_them_all <- function(){
  
  ERR <- list()
  
  # ALLTOGETHER
    # ERR$all_empty_fields_in_variable <- check_empty_fields_in_variables()
    # ppp <- check_empty_variables()
    # ppp_number <- as.data.frame(table(ppp$TIPO_ERROR))
  
  # TRIPS
  #ERR$trips_origen <- check_variable_with_master(OAB_trips, "COD_ORIGEN")
  #ERR$trips_estrato_rim <- check_variable_with_master(OAB_trips, "ESTRATO_RIM")
  #ERR$trips_puerto_llegada <- check_variable_with_master(OAB_trips, "COD_PUERTO_LLEGADA")
  #ERR$trips_puerto_descarga <- check_variable_with_master(OAB_trips, "COD_PUERTO_DESCARGA")
  
  ERR$trips_field_year <- check_field_year(OAB_trips)
  
  ERR$trips_year_in_ID_MAREA <- check_year_in_ID_MAREA(OAB_trips)
  
  ERR$trips_year_in_initial_date <- check_year_in_date(OAB_trips, "FECHA_INI", YEAR_DISCARD)
  ERR$trips_year_in_final_date <- check_year_in_date(OAB_trips, "FECHA_FIN", YEAR_DISCARD)
  
  ERR$trips_check_final_date_in_id_marea_GC <- trips_check_final_date_in_id_marea_GC()
  
  ERR$trips_check_initial_date_before_final_date <- trips_check_initial_date_before_final_date()

  
  ERR$coherencia_estrato_rim_origin <- checkCoherenceEstratoRimOrigin(OAB_trips)
  
  # HAULS
  #ERR$hauls_arte <- check_variable_with_master(OAB_hauls, "COD_ARTE")
  
  ERR$hauls_field_year <- check_field_year(OAB_hauls)
  
  ERR$hauls_year_in_ID_MAREA_hauls <- check_year_in_ID_MAREA(OAB_hauls)
  
  OAB_hauls$FECHA_LAR <- dby_to_dmy_date_format(OAB_hauls$FECHA_LAR)
  ERR$hauls_year_in_shotting_date <- check_year_in_date(OAB_hauls, "FECHA_LAR", YEAR_DISCARD)
  
  OAB_hauls$FECHA_VIR <- dby_to_dmy_date_format(OAB_hauls$FECHA_VIR)
  ERR$hauls_year_in_hauling_date <- check_year_in_date(OAB_hauls, "FECHA_VIR", YEAR_DISCARD)
  
  ERR$hauls_check_hauling_date_before_shooting_date <- hauls_check_hauling_date_before_shooting_date()
  
  ERR$hauls_coherence_estrato_rim_origin <- checkCoherenceEstratoRimOrigin(OAB_hauls)

  ERR$hauls_coherence_estrato_rim_gear <- checkCoherenceEstratoRimGear(OAB_hauls)

  ERR$hauls_hauls_sampled_with_catch_weights <- hauls_hauls_sampled_with_catch_weights()
  
  ERR$hauls_possible_speed_outliers <- get_speed_outliers()
  
  ERR$hauls_hauls_duration <- hauls_hauls_duration()
  
  # CATCHES
  ERR$catches_field_year <- check_field_year(OAB_catches)
  
  ERR$catches_year_in_ID_MAREA <- check_year_in_ID_MAREA(OAB_catches)
  ERR$catches_species_without_caught_neither_discarded_weight <- catches_species_without_caught_neither_discarded_weight()
  
  #ERR$catches_reason_discard_field_empty <- catches_reason_discard_field_empty()
  
  ERR$catches_less_RETENIDA_catch_than_sampled_RETENIDA_catch <- catches_less_RETENIDA_catch_than_sampled_RETENIDA_catch()
  ERR$catches_less_discard_weight_than_sampled_discard_weight <- catches_less_discard_weight_than_sampled_discard_weight()

  # LENGTHS
  #ERR$lengths_field_year <- check_field_year(OAB_lengths)
  
  #ERR$lengths_year_in_ID_MAREA <- check_year_in_ID_MAREA(OAB_lengths)
  

  
  return(ERR)
}


ERRORS <- check_them_all()

# CHECK SPEED:
view_speed_outliers()

# PRUEBAS CON SHINY: AL FINAL PARECE QUE NO SE PUEDEN MOSTRAR CON UN BOXPLOT
# library(shiny)
# runApp("speed", display.mode = "showcase")
# ------------------------------------------------------------------------------    
# #### COMBINE ERRORS ##########################################################
# ------------------------------------------------------------------------------

combined_errors <- formatErrorsList()

# ------------------------------------------------------------------------------
# #### EXPORT ERRORS ###########################################################
# ------------------------------------------------------------------------------

# Uncomment the way to export errors:

# one month

combined_errors <-  list(errores = combined_errors)
MONTH <- "all"

exportListToXlsx(combined_errors, suffix = paste0("descartes", YEAR_DISCARD), separation = "_")



# OAB_exportListToGoogleSheet <- function (list, prefix = "", suffix = "", separation = "") 
# {
#   if (!requireNamespace("googlesheets", quietly = TRUE)) {
#     stop("Googlesheets package needed for this function to work. Please install it.", 
#          call = FALSE)
#   }
#   lapply(seq_along(list), function(i) {
# 
#     if (is.data.frame(list[[i]])) {
#       list_name <- names(list)[[i]]
#       if (prefix != "") 
#         prefix <- paste0(prefix, separation)
#       if (suffix != "") 
#         suffix <- paste0(separation, suffix)
#       filename <- paste0(prefix, list_name, suffix, ".csv")
#       googlesheets::gs_new(filename, ws_title = filename, 
#                            input = list[[i]], trim = TRUE, verbose = FALSE)
#     }
#     else {
#       return(paste("This isn't a dataframe"))
#     }
#   })
# }
# 
# OAB_exportListToGoogleSheet(combined_errors, suffix = paste0("errors", "_", YEAR_DISCARD), separation = "_")


id_marea_with_obj <- OAB_hauls %>%
  select(ID_MAREA, COD_LANCE, COD_ESP_OBJ, ESP_OBJ) %>%
  unique()

catches_only_greater_catch <- OAB_catches %>%
  select(ID_MAREA, COD_LANCE, COD_ESP, ESP, PESO_RET) %>%
  #mutate(peso_total = PESO_RET + PESO_DESCAR) %>%
  group_by(ID_MAREA, COD_LANCE)%>%
  filter(PESO_RET == max(PESO_RET))

catches_with_obj <- merge(catches_only_greater_catch, id_marea_with_obj)

obj_species <- read.csv("especies_objetivo.csv", sep=",")

catches_with_obj_species <- merge(catches_with_obj, obj_species, by = c("COD_ESP_OBJ", "ESP_OBJ"))

err <-catches_with_obj_species %>%
  filter(COD_ESP.x != COD_ESP.y)

#instead of select the rows with the species of maximun catch by id_marea and check
#with the especies_objetivo dataset.wich the object species does not match 
#

catches_teorical_obj_spe <- merge(catches_only_greater_catch, obj_species, all = T)

catches_with_obj_and_teorical <- merge(catches_teorical_obj_spe, id_marea_with_obj, by = c("ID_MAREA", "COD_LANCE"))


