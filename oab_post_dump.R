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
# Uncomment??? the interested way. It's available by a xlsx file or upload directly
# to google drive. In this case an account and password is required, and a token
# is automatically generated.
# - If xlsx option is chosen, the errors file must be created in the directory
# /data/YYYY/YYYY_MM/errors. If the "errors" directory does not exists, it's
# created automatically.
# - TODO: explain speed graphics
# - A file by acronym type (DESIXA, DESSUR...) is generated in "errors"
# directory.

# YOU HAVE ONLY TO CHANGE THIS VARIABLES ---------------------------------------

trips_file <- "IEODESMAREAMARCO.TXT"
hauls_file <- "IEODESLANCEMARCO.TXT"
catches_file <- "IEODESCAPTURAMARCO.TXT"
lengths_file <- "IEODESTALLASMARCO.TXT"
litter_file <- "IEODESBASURASMARCO.TXT"
accidentals_file <- "IEODESCAPTACCIDMARCO.TXT"

# trips_file <- "IEODESMAREASIRENO_2021_2102.TXT"
# hauls_file <- "IEODESLANCESIRENO_2021_2102.TXT"
# catches_file <- "IEODESCAPTURASIRENO_2021_2102.TXT"
# lengths_file <- "IEODESTALLASSIRENO_2021_2102.TXT"
# litter_file <- "IEODESBASURASSIRENO_2021_2102.TXT"
# accidentals_file <- "IEODESCAPTACCIDSIRENO_2021_2102.TXT"

# MONTH: 1 to 12, or vector with month in numbers
# MONTH <- 12
MONTH <- c(1:12)
# In case MONTH is a vector of months, suffix to add to path:
suffix_multiple_months <- "annual"

YEAR <- 2022

# Suffix_id is a suffix added to file names when they are exported both xls and
# google drive files.
suffix_id <- ""


# cfpo to use in the script
# cfpo_to_use <- "CFPO_2021.csv"
cfpo_to_use <- "Consulta al RGFP a 21_09.xlsx"

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
library(ggplot2) #to use in view_speed_outliers
library(ggiraph) #to use in view_speed_outliers


# FUNCTIONS --------------------------------------------------------------------

# All the functions required in this script are located in the next files:
source('oab_post_dump_auxiliar_functions.R')
source('oab_post_dump_functions.R')
source('oab_post_dump_hauls_overlapped.R')
source('oab_post_dump_haul_characteristics.R')
source('create_elasmobranchii_file.R')
source('check_them_all.R')
source('check_them_all_annual.R')

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

### obtain and format the cfpo.
library(openxlsx)
CFPO <- read.xlsx(paste0(getwd(), "/data-raw/", cfpo_to_use), startRow = 4, detectDates=TRUE)
CFPO <- CFPO[, c("CFR", "Nombre", "Matrícula", "Estado.actual")]
colnames(CFPO) <- c("CFR", "NOMBRE", "MATRICULA", "ESTADO")

#### obtain and format the SIRENO fleet file.
# Required to check the ship in the CFPO
SIRENO_FLEET <- read.csv2(paste0(getwd(), "/data-raw/", "barcos_2022_12_02.TXT"),
                   fileEncoding = "windows-1252")
SIRENO_FLEET$COD.BARCO <- apply(SIRENO_FLEET, 1, function(x){
  substr(x["COD.BARCO"], 2, nchar(x["COD.BARCO"]))
  })


# GLOBAL VARIABLES -------------------------------------------------------------

# list with all errors found in dataframes:
ERRORS <- list()

# path to the work files
# PATH_FILES <- "F:/misdoc/sap/oab_post_dump/data/2019/1_checking"
# PATH_FILES <- "C:/Users/Marco IEO/Desktop/oab_post_dump/data/2019/1_checking"
if (length(MONTH) == 1 && MONTH == "annual"){
  path_text <- paste0("data/", YEAR, "/", YEAR, "_annual")
} else if(length(MONTH) != 1){
  path_text <- paste0("data/", YEAR, "/", YEAR, "_", suffix_multiple_months)
} else {
  path_text <- paste0("data/", YEAR, "/", YEAR, "_", sprintf("%02d", MONTH))
} 

PATH_FILES <- file.path(getwd(), path_text)


# path to the generated errors file
PATH_ERRORS <- file.path(PATH_FILES,"errors")
# if the errors directory does not exists, create it:
ifelse(!dir.exists(PATH_ERRORS), dir.create(PATH_ERRORS), FALSE)

# path to store files as backup
PATH_BACKUP <- file.path(PATH_FILES, "backup")

# month as character in case of monthly check
if (length(MONTH) == 1 && MONTH %in% seq(1:12)){
  MONTH_AS_CHARACTER <- ifelse(isFALSE(MONTH), "", sprintf("%02d", MONTH))
} else if (length(MONTH) > 1 & all(MONTH %in% seq(1:12))) {
  MONTH_AS_CHARACTER <- suffix_multiple_months
} else if (MONTH == "annual") {
  MONTH_AS_CHARACTER <- "annual"
}

# path to shared folder
PATH_SHARE_ERRORS <- file.path("C:/Users/ieoma/SAP_MUE/SAP_OAB - OAB_data_review - Revisión_datos", YEAR, paste0(YEAR, "_", MONTH_AS_CHARACTER))

# names to export
prefix_to_export <- "OAB"

suffix_to_export <- ""

if (length(MONTH) == 1 && MONTH %in% seq(1:12)) {
  suffix_to_export <- paste0(YEAR, "_", MONTH_AS_CHARACTER)
} else if (length(MONTH) > 1 & all(MONTH %in% seq(1:12))) {
  suffix_to_export <- paste0(YEAR, "_", suffix_multiple_months)
} else if (MONTH == "annual"){
  suffix_to_export <- paste0(YEAR, "_annual", ifelse(suffix_id!="", paste0("_",suffix_id), ""))
} 

# files to backup
FILES_TO_BACKUP <- c("oab_post_dump.R",
                     "oab_post_dump_auxiliar_functions.R",
                     "oab_post_dump_functions.R",
                     "oab_post_dump_haul_characteristics.R",
                     "oab_post_dump_hauls_overlapped.R",
                     "oab_post_dump_sexed_species_functions.R",
                     "especies_a_medir_OAB.csv",
                     "especies_objetivo_OAB.csv",
                     "razon_descarte_OAB.csv",
                     "metier_ieo_especie_objetivo_OAB.csv",
                     "duracion_mareas.txt",
                     "caracteristicas_lances.csv",
                     "not_allowed_species_measured.csv",
                     "origin_statistical_rectangle.csv",
                     "cephalopods.csv")

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

if(length(MONTH) == 1 && MONTH %in% seq(1:12)){
  # WARNING!! DOES NOT WORK WITH JANUARY-DECEMBER!!!! :(
  OAB_tripsKKK <- OAB_trips[
    as.POSIXlt(OAB_trips$FECHA_INI)$mon +1 == MONTH |
      (as.POSIXlt(OAB_trips$FECHA_INI)$mon +1 == MONTH -1 & as.POSIXlt(OAB_trips$FECHA_FIN)$mon +1 == MONTH)
    ,]
  OAB_hauls <- OAB_hauls[OAB_hauls$COD_MAREA%in%OAB_trips$COD_MAREA,]
  OAB_catches <- OAB_catches[OAB_catches$COD_MAREA%in%OAB_trips$COD_MAREA,]
  OAB_lengths <- OAB_lengths[OAB_lengths$COD_MAREA%in%OAB_trips$COD_MAREA,]
}

# when multiple months are used
if(all(length(MONTH) >=1 & MONTH %in% seq(1:12))){
  # TODO: TEST USING OAB_trips$FECHA_FIN instead OAB_trips$FECHA_INI
  # I THINK THIS DOES NOT WORK:
  OAB_trips <- OAB_trips[as.POSIXlt(OAB_trips$FECHA_FIN, format = "%d/%m/%Y")$mon +1 %in% MONTH &
                         (as.POSIXlt(OAB_trips$FECHA_FIN, format = "%d/%m/%Y")$year +1900) == YEAR,]
  OAB_hauls <- OAB_hauls[OAB_hauls$COD_MAREA%in%OAB_trips$COD_MAREA,]
  OAB_catches <- OAB_catches[OAB_catches$COD_MAREA%in%OAB_trips$COD_MAREA,]
  OAB_lengths <- OAB_lengths[OAB_lengths$COD_MAREA%in%OAB_trips$COD_MAREA,]
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
# instead use sapmuebase exportListToXlsx(), use this:

exportListToXlsx2 <- function (list, prefix = "", suffix = "", separation = "", path_export = getwd())
{
  #check if package openxlsx is instaled:
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
      
      # ---- Column widths: I don't know why, but it dosn't work in the right way
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


exportListToXlsx2(errors, prefix = prefix_to_export,
                 suffix = suffix_to_export,
                 separation = "_", path_export = PATH_ERRORS)



# Export to google sheets 
# OAB_export_list_google_sheet(errors, prefix = prefix_to_export,
#                         suffix = suffix_to_export,
#                         separation = "_")


# CHECK SPEED ------------------------------------------------------------------


view_speed_outliers()
filename <- paste("speed_outliers", YEAR,  MONTH_AS_CHARACTER, sep ="_")
printPdfGraphic(filename, view_speed_outliers)

# SAVE FILES TO SHARED FOLDER ----
copyErrorsFilesToSharedFolder()

# BACKUP SCRIPTS AND RELATED FILES ----
# first save all files opened
rstudioapi::documentSaveAll()
# and the backup the scripts and files:
sapmuebase::backupScripts(FILES_TO_BACKUP, path_backup = PATH_BACKUP)



# PRUEBAS CON SHINY: AL FINAL PARECE QUE NO SE PUEDEN MOSTRAR CON UN BOXPLOT
# library(shiny)
# runApp("speed", display.mode = "showcase")


# test upload files to Teams
# library("Microsoft365R")
# od <- personal_onedrive()
# od$list_items()
# list_teams("SAP_MUE")


# when this problem is fixed, detelte it:
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

