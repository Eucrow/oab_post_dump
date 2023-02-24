#' Check code: None, is used by other function
#' Detection of overlaping hauls in a dataframe. This function is used in
#' hauls_verlapped() function to use in group_modify from dplyr.
#' See group_modify help to understand the code
hauls_overlap <- function(df, group_info){
  
  df[["TIPO_ERROR"]] <- NA

  df[["FECHA_HORA_VIR_PREVIOUS_HAUL"]] <- lag(df[["FECHA_HORA_VIR"]])
  
  df[["FECHA_HORA_LAR_NEXT_HAUL"]] <- lead(df[["FECHA_HORA_LAR"]])
  
  df <- apply(df, 1, function(x){
    
    
    if(is.na(x[["FECHA_HORA_VIR_PREVIOUS_HAUL"]]) == FALSE &&
      x[["FECHA_HORA_LAR"]] < x[["FECHA_HORA_VIR_PREVIOUS_HAUL"]]) {
      x[["TIPO_ERROR"]] <- "ERROR: setting time before hauling time of previous haul so the haul overlaps in time with another in the same trip."
    }
    
    if(is.na(x[["FECHA_HORA_LAR_NEXT_HAUL"]]) == FALSE &&
       x[["FECHA_HORA_VIR"]] > x[["FECHA_HORA_LAR_NEXT_HAUL"]]) {
       x[["TIPO_ERROR"]] <- "ERROR: hauling time after setting time of next haul so the haul overlaps in time with another in the same trip."

    }
    
    return(x)
    
  })
  
  df <- as.data.frame(t(df))
  
  df <- df[!is.na(df[["TIPO_ERROR"]]), ]
  
  return(df)

}



#' Check code: 2048
#' Detection of overlaping hauls in the same trip -----
#' Only with metiers wich can't be overlapped, stored in
#' "caracteristicas_lances"'" dataset.
#' @param df_trips trips dataframe.
#' @param df_hauls hauls dataframe.
#' @return Dataframe with the erroneus data. A new a variable with the type
#' of error is added.
hauls_overlapped <- function() {
  
  overlapped_metier <- caracteristicas_lances[
                        caracteristicas_lances$SOLAPADO_LANCES == FALSE,
                        "METIER_IEO"]
  
  errors <- OAB_hauls %>%
    filter(METIER_IEO %in% overlapped_metier)
  
  if (nrow(errors) > 0) {
    errors <- errors %>%
      select(COD_MAREA, COD_LANCE, METIER_IEO, FECHA_HORA_LAR, FECHA_HORA_VIR) %>%
      group_by(COD_MAREA) %>%
      group_modify( hauls_overlap )
    
    return(errors)
  
  } else {
    return(NULL)
  }
    
  
} 







