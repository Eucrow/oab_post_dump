#' Script to export a file with elasmobranchii species from a dataframe given.

#' Packages
library(jsonlite)

#' Remove " spp" in species name.
#' @param sp vector of species
remove_spp_from_sp <- function(sp){
  return (gsub(" spp", "", sp))
}


#' Check if one species belongs to a taxon.
#' Require jsonlite library.
#' @param specie specie to check
#' @param taxon_to_match taxon to match
#' @return TO DO
check_spe_belongs_to_taxon <- function (species, taxon_to_match){
  # Firstly, clean " spp" in species
  species <- remove_spp_from_sp(species)
  
  # Get the AphiaID of the species
  # I don't use worms package because I'm interested in catch the response in case
  # of the species does not exists in WORMS
  url_species <- paste0("http://www.marinespecies.org/rest/AphiaIDByName/", species)
  url_species <- gsub(" ", "+", url_species)
  print(url_species)
  resp <- httr::GET(url_species)
  if (resp$status_code==204){
    warning(paste(species, "doesn't match in WORMS"))
    return("this species does not match in WORMS")
  }
  if (resp$status_code==206){
    warning(paste(species, "has multiple matchs in WORMS"))
    return("multiple match in WORMS")
  }
  
  AphiaID_species <- fromJSON(url_species)
  
  #Build the URL to get the data from
  url <- sprintf("http://www.marinespecies.org/rest/AphiaClassificationByAphiaID/%d", AphiaID_species);
  
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


#' Return species belonging to certain taxon. Is the vectorized version of
#' check_spe_belongs_to_taxon().
#' @param species: vector of species to check.
#' @param taxon: taxon.
#' @return vector with the species belonging to taxon.
is_taxon <- function(species, taxon){
  # species <- lapply(species, check_spe_belongs_to_taxon, taxon)
  species <- apply(as.matrix(species), 1, check_spe_belongs_to_taxon, taxon)
}


#' Create elasmobranchii dataset.
#' Create a dataset with the elasmobranchii species from a given data frame.
#' The datafr
#' @param df: data frame of species. Must have "COD_ESP" and "ESP" variables.
#' @return : export elasmobranchii.csv file in getwd() path.
create_elasmobranchii_file <- function(df){
  if("COD_ESP" %in% colnames(df) &&
     "ESP" %in% colnames(df) &&
     sapmuebase::checkFormatVariable(df, "COD_ESP") &&
     sapmuebase::checkFormatVariable(df, "ESP")
     ){
    
    df <- df[, c("COD_ESP", "ESP")]
    
    df <- unique(df)
    
    df$is_elasmobranchii <- is_taxon(df[["ESP"]], "Elasmobranchii")
    elasmobranchii <- df[which(df[["is_elasmobranchii"]] == TRUE),]
    
    elasmobranchii <- elasmobranchii[, c("COD_ESP", "ESP")]
    
    file_name <- "elasmobranchii.csv"
    exportCsvSAPMUEBASE(elasmobranchii, file_name)
    
  } else {
    stop(paste0(substitute(df), " must have COD_ESP and ESP variables." ))
  }
}

