# TENGO QUE COMPROBAR:
#   - SI FUNCIONA BIEN CON UN BARCO QUE NO ESTÉ DE ALTA EN EL CENSO DE LA SECRETARÍA
#   - GESTIONAR SI HAY There are multiple vessels with this CFR: NA


shipsNotRegisteredSGP <- function(trips_df){

  # To avoid multiple request of the same ship:
  unique_vessels <- trips_df[, "COD_BARCO", drop = FALSE]

  v <- merge(unique_vessels,
             SIRENO_FLEET,
             by.x = "COD_BARCO",
             by.y = "COD.BARCO",
             all.x = TRUE)

  v$active <- lapply(v[["COD.SECRETARIA"]], function(x){
    result <- isVesselActive(x)
  })

  errors <- v[v$active == FALSE, ]
   if (nrow(errors) > 0){
    errors <- merge(trips_df,
                  errors,
                  by.x = "COD_BARCO",
                  by.y = "COD.BARCO",
                  all.x = TRUE)


    errors <- unlist(errors)
    errors <- addTypeOfError(errors, "ERROR: Days at see must be greater or equal than fishing days. Both fields are calculated, so the problem must be in date and times of the trips or hauls.")
    return(errors)
  }


}

OAB_trips$COD_BARCO <- as.character(OAB_trips$COD_BARCO)
OAB_trips[1, "COD_BARCO"] <- "200144"

registered <- shipsNotRegisteredSGP(OAB_trips[1,])

