#' Check code: 2083
#' Check if the last day of a trip and the day of the last haul done
#' match for a same catch. 
#' @param df_trips OAB_trips dataframe.
#' @param df_hauls OAB_hauls dataframe.
#' @return dataframe with errors.

match_last_trip_haul_day <- function(df_trips, df_hauls){

  df_trips <- unique(df_trips[, c("COD_MAREA", 
                                  "FECHA_FIN_MAREA")])

  df_trips$FECHA_FIN_MAREA <- as.Date(df_trips$FECHA_FIN_MAREA, 
                                      format="%d/%m/%Y")

  colnames(df_trips) <- c("COD_MAREA", 
                          "FECHA_LANCE")

  df_hauls <- unique(df_hauls[, c("COD_MAREA", 
                                  "FECHA_LANCE")])

  df_hauls$FECHA_LANCE <- as.Date(df_hauls$FECHA_LANCE, 
                                  format="%d/%m/%Y")

  df_hauls <- df_hauls %>% 
    group_by(COD_MAREA) %>% 
    summarise(FECHA_LANCE = max(FECHA_LANCE))

  df_merged <- merge(
                    df_hauls,
                    df_trips,
                    all.x = TRUE
                    )
  
  errors <- df_merged[is.na(df_merged$FECHA_LANCE), ]

  if (nrow(errors) > 0){
    errors <- addTypeOfError(errors, "WARNING 2083: Last trip and haul day are different for the same catch.")
    return(errors)
  }

}
