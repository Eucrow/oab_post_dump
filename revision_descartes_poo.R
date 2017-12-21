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
# install("F:/misdoc/sap/sapmuebase")
library(sapmuebase)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# YOU HAVE ONLY TO CHANGE THIS VARIABLES 

PATH_FILES <- "F:/misdoc/sap/revision descartes/data"
trips_file <- "IEODESMAREAMARCO.TXT" 
hauls_file <- "IEODESLANCEMARCO.TXT"
catches_file <- "IEODESCAPTURAMARCO_prueba.TXT"
lengths_file <- "IEODESTALLASMARCO.TXT"

YEAR <- "2017"


# ------------------------------------------------------------------------------
# #### SET WORKING DIRECTORY ###################################################
# ------------------------------------------------------------------------------

setwd("F:/misdoc/sap/revision descartes/")


# ------------------------------------------------------------------------------
# #### FUNCTIONS ###############################################################
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#' Check year in field YEAR
#' 
#' Search a field in a dataframe y check it.
#' 
#' @param df daraframe to check the field YEAR. The dataframe must have a field
#' called YEAR.
#' @return df with errors
#' @export
check_field_year <- function(df, year) {
  
}


# ------------------------------------------------------------------------------

#' Check year in ID_MAREA field

#' Check year in other date fields:



# ------------------------------------------------------------------------------
# #### IMPORT DATA #############################################################
# ------------------------------------------------------------------------------

discards_samples <- importOABFiles(trips_file, hauls_file, catches_file, lengths_file,
                                   path = PATH_FILES)




library(sapmuebase)

######################################################################
# Create the base MonthlyRevision

Discards <- setClass(
  # Set the name for the class
  "Discards",
  
  # Define the slots
  slots = c(
    YEAR = "numeric",
    trips_file = "character",
    hauls_file = "character",
    catches_file = "character",
    lengths_file = "character",
    PATH_FILES ="character",
    discards = "list"
  )#,
  
  # Set the default values for the slots. (optional)
  # prototype=list(
  #   YEAR = 2017,
  #   MONTH = 4,
  #   FILENAME_DES_TOT = "IEOUPMUEDESTOTMARCO.TXT",
  #   FILENAME_DES_TAL = "IEOUPMUEDESTALMARCO.TXT",
  #   FILENAME_TAL = "IEOUPMUETALMARCO.TXT",
  #   PATH_FILES = "kkk"
  # ),
  
  # Make a function to test if the data is consistent.
  # This is not called if you have an initialize function defined!
  # validity=function(object)
  # {
  #   #TODO como hacer para comprobar que el argumento se ha metido
  #   # if(!exists(object@MONTH) | !exists(object@YEAR)){
  #   #   return ("MONTH and YEAR are mandatory variables.")
  #   # }
  #   
  #   if(object@MONTH < 1 | object@MONTH > 12) {
  #     stop(paste0(object@MONTH, " months, really? How many months has your culture?"))
  #   }
  #   return(TRUE)
  # }
)


# Initilize method.
setMethod("initialize",
          "Discards",
          function(.Object, YEAR, trips_file, hauls_file, catches_file, lengths_file, PATH_FILES){
            
            .Object@YEAR <- YEAR
            .Object@trips_file <- trips_file
            .Object@hauls_file <- hauls_file
            .Object@catches_file <- catches_file
            .Object@lengths_file <- lengths_file
            .Object@PATH_FILES <- PATH_FILES
            
            .Object@discards <- importOABFiles(
              trips_file, 
              hauls_file, 
              catches_file, 
              lengths_file,
              path = PATH_FILES
              #   #theObject@path = PATH_FILES, 
              )
            
            #If a initialize method is created, validity doesn't work
            #unless validObject(.Object) is doesn't called
            # validObject(.Object)  
            return(.Object)
          }
)


trips_file <- "IEODESMAREAMARCO.TXT" 
hauls_file <- "IEODESLANCEMARCO.TXT"
catches_file <- "IEODESCAPTURAMARCO_prueba.TXT"
lengths_file <- "IEODESTALLASMARCO.TXT"

prueba <- Discards(YEAR = 2017, "IEODESMAREAMARCO.TXT", "IEODESLANCEMARCO.TXT", "IEODESCAPTURAMARCO_prueba.TXT", "IEODESTALLASMARCO.TXT", PATH_FILES)
prueba@YEAR
prueba@MONTH
prueba@FILENAME_DES_TOT
prueba@PATH_FILES
prueba@

