#### Check discards from SIRENO
#### Return xls files with errors and warnings detected by acronym type.
#### author: Marco A. Ámez Fernández
#### email: ieo.marco.a.amez@gmail.com

# INSTRUCTIONS -----------------------------------------------------------------

# To use this script:

# - This scripts require the files oab_post_dump_functions.R,
# oab_post_dump_hauls_overlapped.R and oab_post_dump_haul_characteristics.R, so
# Make sure they are located in the same directory that this file.
# - Change variables in "YOU HAVE ONLY TO CHANGE THIS VARIABLES" section of this
# script.
# - Make sure report files of discards from SIRENO are in path
# /data/YYYY/YYYY_MM.
# - The xlsx errors file will be created in the directory
# /data/YYYY/YYYY_MM/errors. If this directory does not exists, it's
# created automatically.
# - A errors are exported in files with name as its related acronym type
# (DESIXA, DESSUR...).
# - A function to export and upload the errors file in google drive is stored in
# oab_post_dump_auxiliar_functions.R. It is not use in this script right now.
# At the end of the script, a backup of R the files used in the script are save
# in /data/YYYY/YYYY_MM/backup.
# - TODO: explain speed graphics


# YOU HAVE ONLY TO CHANGE THIS VARIABLES ---------------------------------------

trips_file <- "IEODESMAREAMARCO.TXT"
hauls_file <- "IEODESLANCEMARCO.TXT"
catches_file <- "IEODESCAPTURAMARCO.TXT"
lengths_file <- "IEODESTALLASMARCO.TXT"
litter_file <- "IEODESBASURASMARCO.TXT"
accidentals_file <- "IEODESCAPTACCIDMARCO.TXT"

# MONTH: 1 to 12, or vector with month in numbers
MONTH <- 4
# MONTH <- c(1:12)

# Suffix to path folder (useful when the data of the same month is received
# in different files). If it is not necessary, use "".
# FOLDER_SUFFIX <- "b"
FOLDER_SUFFIX <- ""

# Suffix to add to path. Use only in case MONTH is a vector of months. This
# suffix will be added to the end of the path with a "_" as separation.
# suffix_multiple_months <- "annual"

YEAR <- 2025

PATH_SHARED_FOLDER <- "C:/Users/ieoma/Nextcloud/SAP_OAB/OAB_data_review"

# cfpo to use in the script
cfpo_to_use <- "CFPO2024 DEF.xlsx"

# sireno fleet file to use in the script
sireno_fleet_to_use <- "IEOPROBARMARCO_2024_03_12.TXT"

# PACKAGES ---------------------------------------------------------------------

library(plyr) # to use function join_all --> TO DO: change to reduce-merge functions
library(dplyr)
library(devtools)
# install_github("Eucrow/sapmuebase")
library(sapmuebase)
library(openxlsx)
library(ggplot2) # to use in speed_outliers_pdf
library(plotly) # to use in speed_outliers_interactive
library(htmlwidgets) # to use in speed_outliers_interactive


# FUNCTIONS --------------------------------------------------------------------

# All the functions required in this script are located in the next files:
source("oab_post_dump_auxiliar_functions.R")
source("oab_post_dump_functions.R")
source("oab_post_dump_hauls_overlapped.R")
source("oab_post_dump_haul_characteristics.R")
source("create_elasmobranchii_file.R")
source("check_them_all.R")
source("check_them_all_annual.R")
source("oab_post_dump_speed_plot_functions.R")

# GLOBAL VARIABLES -------------------------------------------------------------

# list with all errors found in dataframes:
ERRORS <- list()

# path to the work files
PATH_FILES <- createPathFiles(MONTH, YEAR, suffix_multiple_months, FOLDER_SUFFIX)

# path to the generated errors file
PATH_ERRORS <- file.path(PATH_FILES, "errors")
# if the errors directory does not exists, create it:
ifelse(!dir.exists(PATH_ERRORS), dir.create(PATH_ERRORS), FALSE)

# Name of the folder where are stored private files with sensitive information.
PRIVATE_FOLDER_NAME <- "private"

# Path where private files are stored.
PATH_PRIVATE_FILES <- file.path(getwd(), PRIVATE_FOLDER_NAME)

# path to store files as backup
PATH_BACKUP <- file.path(PATH_FILES, "backup")

# month as character in case of monthly check
MONTH_AS_CHARACTER <- createMonthAsCharacter(month = MONTH, smm = suffix_multiple_months)

# path to shared folder
# PATH_SHARE_ERRORS <- file.path("C:/Users/ieoma/SAP_MUE/SAP_OAB - OAB_data_review - Revisión_datos", YEAR, paste0(YEAR, "_", MONTH_AS_CHARACTER))
# PATH_SHARE_ERRORS <- file.path("C:/Users/ieoma/Nextcloud/SAP_OAB/OAB_data_review", YEAR, paste0(YEAR, "_", MONTH_AS_CHARACTER))

PATH_SHARE_ERRORS <- createPathSharedFiles(PATH_SHARED_FOLDER, MONTH, YEAR, suffix_multiple_months, FOLDER_SUFFIX)
# if the shared errors directory does not exists, create it:
ifelse(!dir.exists(PATH_SHARE_ERRORS), dir.create(PATH_SHARE_ERRORS), FALSE)

# names to export
PREFIX_TO_EXPORT <- "OAB"

SUFFIX_TO_EXPORT <- createSuffixToExport(MONTH, YEAR, MONTH_AS_CHARACTER, suffix_multiple_months)

# files to backup
FILES_TO_BACKUP <- c(
  "oab_post_dump.R",
  "oab_post_dump_auxiliar_functions.R",
  "oab_post_dump_functions.R",
  "oab_post_dump_hauls_overlapped.R",
  "oab_post_dump_haul_characteristics.R",
  "oab_post_dump_speed_plot_functions.R",
  "oab_post_dump_sexed_species_functions.R",
  "especies_a_medir_OAB.csv",
  "especies_objetivo_OAB.csv",
  "razon_descarte_OAB.csv",
  "metier_target_species.csv",
  "duracion_mareas.txt",
  "caracteristicas_lances.csv",
  "not_allowed_species_measured.csv",
  "origin_statistical_rectangle.csv",
  "cephalopods.csv"
)

EMAIL_TEMPLATE <- "errors_email.Rmd"

# LOAD DATASETS ----------------------------------------------------------------

especies_a_medir_OAB <- importCsvSAPMUE("especies_a_medir_OAB.csv")

especies_objetivo_oab <- importCsvSAPMUE("especies_objetivo_OAB.csv")

razon_descarte_OAB <- importCsvSAPMUE("razon_descarte_OAB.csv")

metier_target_species <- importCsvSAPMUE("metier_target_species.csv")

duracion_mareas_OAB <- importCsvSAPMUE("duracion_mareas.txt")

caracteristicas_lances <- importCsvSAPMUE("caracteristicas_lances.csv")

not_allowed_species_measured <- importCsvSAPMUE("not_allowed_species_measured.csv")

origin_statistical_rectangle <- importCsvSAPMUE("origin_statistical_rectangle.csv")
origin_statistical_rectangle$COD_ORIGEN <- sprintf("%03d", origin_statistical_rectangle$COD_ORIGEN)

# this cephalopods master has been created using the annual OAB_catches
# dataframe of 2019. All the species has been checked via API with WORM webpage
# using the function check_spe_belongs_to_taxon()
cephalopods <- importCsvSAPMUE("cephalopods.csv")

### obtain and format the cfpo.
# CFPO <- read.xlsx(paste0(getwd(), "/data-raw/", cfpo_to_use), startRow = 4, detectDates=TRUE)
CFPO <- read.xlsx(paste0(getwd(), "/data-raw/", cfpo_to_use), detectDates = TRUE)
CFPO <- CFPO[, c("CFR", "Nombre", "Matrícula", "Estado.actual")]
colnames(CFPO) <- c("CFR", "NOMBRE", "MATRICULA", "ESTADO")

#### obtain and format the SIRENO fleet file.
# Required to check the ship in the CFPO
SIRENO_FLEET <- read.csv2(paste0(getwd(), "/data-raw/", sireno_fleet_to_use),
  fileEncoding = "windows-1252"
)
SIRENO_FLEET$COD.BARCO <- sub("'", "", SIRENO_FLEET[["COD.BARCO"]])

# Get the contacts data set. This data set contains the different roles and its
# email, used in the distribution of error files. The roles are:
# - GC, GS, GN and AC: the supervisors of the influence areas.
# - sender: person responsible for sending the files.
# - cc: related people to whom the email should also be sent.
# This data set is obtained from the file contacts.csv stored in private folder
# due to the confidential information contained in it. The contacts.csv file
# must have a comma separated format with two fields: ROLE and EMAIL. The firs
# line must contain the name of the variables.
CONTACTS <- read.csv(file.path(PATH_PRIVATE_FILES, "contacts.csv"))


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
# only when one month is used.

if (length(MONTH) == 1 && MONTH %in% seq(1:12)) {
  # WARNING!! DOES NOT WORK WITH JANUARY-DECEMBER!!!! :(
  OAB_trips <- OAB_trips[
    as.POSIXlt(OAB_trips$FECHA_INI_MAREA)$mon + 1 == MONTH |
      (as.POSIXlt(OAB_trips$FECHA_INI_MAREA)$mon + 1 == MONTH - 1 & as.POSIXlt(OAB_trips$FECHA_FIN_MAREA)$mon + 1 == MONTH),
  ]
  OAB_hauls <- OAB_hauls[OAB_hauls$COD_MAREA %in% OAB_trips$COD_MAREA, ]
  OAB_catches <- OAB_catches[OAB_catches$COD_MAREA %in% OAB_trips$COD_MAREA, ]
  OAB_lengths <- OAB_lengths[OAB_lengths$COD_MAREA %in% OAB_trips$COD_MAREA, ]
}

# when multiple months are used
if (all(length(MONTH) >= 1 & MONTH %in% seq(1:12))) {
  # TODO: TEST USING OAB_trips$FECHA_FIN_MAREA instead OAB_trips$FECHA_INI_MAREA
  # I THINK THIS DOES NOT WORK:
  OAB_trips <- OAB_trips[as.POSIXlt(OAB_trips$FECHA_FIN_MAREA, format = "%d/%m/%Y")$mon + 1 %in% MONTH &
    (as.POSIXlt(OAB_trips$FECHA_FIN_MAREA, format = "%d/%m/%Y")$year + 1900) == YEAR, ]
  OAB_hauls <- OAB_hauls[OAB_hauls$COD_MAREA %in% OAB_trips$COD_MAREA, ]
  OAB_catches <- OAB_catches[OAB_catches$COD_MAREA %in% OAB_trips$COD_MAREA, ]
  OAB_lengths <- OAB_lengths[OAB_lengths$COD_MAREA %in% OAB_trips$COD_MAREA, ]
}



# FILTER BY ACRONYM ------------------------------------------------------------

# DESNOR
# OAB_trips <- OAB_trips[ grep("DESNOR", OAB_trips$COD_MAREA), ]
# OAB_hauls <- OAB_hauls[ grep("DESNOR", OAB_hauls$COD_MAREA), ]
# OAB_catches <- OAB_catches[ grep("DESNOR", OAB_catches$COD_MAREA), ]
# OAB_lengths <- OAB_lengths[ grep("DESNOR", OAB_lengths$COD_MAREA), ]

# DESSUR
# OAB_trips <- OAB_trips[ grep("DESSUR", OAB_trips$COD_MAREA), ]
# OAB_hauls <- OAB_hauls[ grep("DESSUR", OAB_hauls$COD_MAREA), ]
# OAB_catches <- OAB_catches[ grep("DESSUR", OAB_catches$COD_MAREA), ]
# OAB_lengths <- OAB_lengths[ grep("DESSUR", OAB_lengths$COD_MAREA), ]

# DESIXA
# OAB_trips <- OAB_trips[ grep("(DESIXA)(?!C)", OAB_trips$COD_MAREA, perl = T), ]
# OAB_hauls <- OAB_hauls[ grep("(DESIXA)(?!C)", OAB_hauls$COD_MAREA, perl = T), ]
# OAB_catches <- OAB_catches[ grep("(DESIXA)(?!C)", OAB_catches$COD_MAREA, perl = T), ]
# OAB_lengths <- OAB_lengths[ grep("(DESIXA)(?!C)", OAB_lengths$COD_MAREA, perl = T), ]

# FILTER BY OBSERVER -----------------------------------------------------------

# ADRIAN
# OAB_trips <- OAB_trips[ which(OAB_trips[["NOMBRE_OBS"]] == "ADRIAN"), ]
# OAB_hauls <- OAB_hauls[OAB_hauls$COD_MAREA%in%OAB_trips$COD_MAREA,]
# OAB_catches <- OAB_catches[OAB_catches$COD_MAREA%in%OAB_trips$COD_MAREA,]
# OAB_lengths <- OAB_lengths[OAB_lengths$COD_MAREA%in%OAB_trips$COD_MAREA,]


# SEARCHING ERRORS -------------------------------------------------------------

ERRORS <- check_them_all()
# ERRORS <- check_them_all_annual()

# FORMAT ERRORS ----------------------------------------------------------------

combined_errors <- formatErrorsList()

errors <- separate_df_by_acronym(combined_errors)

# Remove columns with only NA values
# Filter extracts the elements of a vector for which a predicate (logical)
# function gives true
errors <- lapply(errors, function(x) {
  Filter(function(x) {
    !all(is.na(x))
  }, x)
})

# EXPORT ERRORS ----------------------------------------------------------------

# Export to xls
exportErrorsListToXlsx2(errors,
  prefix = PREFIX_TO_EXPORT,
  suffix = SUFFIX_TO_EXPORT,
  separation = "_", path_export = PATH_ERRORS
)

# CREATE AND SAVE SPEED PLOT ---------------------------------------------------
speed_plot_interactive <- speed_boxplot_interactive()
filename <- paste("speed_outliers", YEAR, MONTH_AS_CHARACTER, sep = "_")
htmlwidgets::saveWidget(speed_plot_interactive, file.path(PATH_ERRORS, paste0(filename, ".html")))

# speed_plot_pdf <- speed_boxplot()
# print_pdf_graphic(filename, speed_plot_pdf)


# SAVE FILES TO SHARED FOLDER ----
copyErrorsFilesToSharedFolder()

# SEND EMAILS AUTOMATICALLY ----------------------------------------------------
# The first time the errors will be sent by email, a credential file must be
# generated with create_smtp_creds_file. The credentials file is generated in
# the private folder:
# blastula::create_smtp_creds_file(file = file.path(PRIVATE_FOLDER_NAME, "credentials"),
#                        user = "",
#                        host = "",
#                        port = ,
#                        use_ssl = )

# The accessory_email_info data frame must have two variables:
# - AREA_INF: influence area with the values DESIXA, DESSUR, DESSUR and ALL. All
# is for the links related to all areas.
# - INTERNAL_LINK: with the link to the error file in its AREA_INF. If there
# aren't any error file of a certain AREA_INF, must be set to "".
# - NOTES: any notes to add to the email. If there aren't, must be set to "".

# TODO: the files send to ALL have the text: "Y este es el outlier de
# velocidades por estrato rim:". The ALL should be changed to SPEED, and added
# another info to any other file that can be sent.
accessory_email_info <- data.frame(
  AREA_INF = c("DESIXA", "DESNOR", "DESSUR", "ALL"),
  LINK = c(
    "https://saco.csic.es/index.php/f/481791242",
    "",
    "https://saco.csic.es/index.php/f/481791243",
    "https://saco.csic.es/index.php/f/481791263"
  ),
  NOTES = c(
    "",
    "",
    "",
    ""
  )
)

sendErrorsByEmail(
  accessory_email_info = accessory_email_info,
  contacts = CONTACTS,
  credentials_file = "credentials",
  identification_sampling = SUFFIX_TO_EXPORT
)



# BACKUP SCRIPTS AND RELATED FILES ----
# first save all files opened
rstudioapi::documentSaveAll()
# and the backup the scripts and files:
sapmuebase::backupScripts(FILES_TO_BACKUP, path_backup = PATH_BACKUP)


# PRUEBAS CON SHINY: AL FINAL PARECE QUE NO SE PUEDEN MOSTRAR CON UN BOXPLOT
# library(shiny)
# runApp("speed", display.mode = "showcase")



# when this problem is fixed, delete it:
# sampled_discard_less_subsample_discard_FIXED <- function(df){
#
#   # usually the PESO_SUB_MUE_TOT is NA, so it is necessary detect it.
#   errors <- df[
#     which( !is.na(df$P_SUB_MUE_TOT) & df$P_SUB_MUE_TOT < df$P_MUE_DESCAR),
#     c("COD_MAREA", "COD_LANCE", "COD_ESP", "A3_ESP", "ESP",
#       "P_SUB_MUE_TOT", "P_MUE_DESCAR")
#   ]
#
#   errors <- addTypeOfError(errors, "ERROR: sampled discard weight of the species (in 'catches' screen of SIRENO) less than subsample discard weight.")
#
#   return(errors)
#
# }
# error_sampled_discard_less_subsample_discard_FIXED <- sampled_discard_less_subsample_discard_FIXED(OAB_catches)
# error_sampled_discard_less_subsample_discard <- sampled_discard_less_subsample_discard(OAB_catches)
