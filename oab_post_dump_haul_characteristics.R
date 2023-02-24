#' Check code: 2050
#' Check duration of the hauls.
#' The maximun and minimun duration are collected in 'caracteristicas_lances' 
#' dataset.
#' @note If the METIER_IEO is NA, this haul is ignored and doesn't return the
#' error.
#' @return Dataframe with erroneus data of every haul characterictics.
check_hauls_duration <- function(){
  
  # OAB_hauls$DURACION <- OAB_hauls$FECHA_HORA_VIR - OAB_hauls$FECHA_HORA_LAR
  OAB_hauls$DURACION <- difftime(OAB_hauls$FECHA_HORA_VIR,
                                 OAB_hauls$FECHA_HORA_LAR,
                                 units = "mins")
  
  df <- OAB_hauls[, c("COD_MAREA", "COD_LANCE", "METIER_IEO", "FECHA_HORA_LAR",
                      "FECHA_HORA_VIR", "DURACION")]
  
  cl <- caracteristicas_lances[, c("METIER_IEO", "DURACION_MAX", "DURACION_MIN")]

  errors <- merge(df, cl, by = "METIER_IEO", all.x = T)
  
  errors <- errors[which(errors[,"DURACION"] < errors[, "DURACION_MIN"] |
                           errors[,"DURACION"] > errors[, "DURACION_MAX"]),]
  
  if (nrow(errors)>0){
    
    errors <- apply(errors, 1, function(x){
      
      x[["TIPO_ERROR"]] <- paste0("WARNING: duration of haul out of range according to master (", x[["DURACION_MIN"]], " - " , x[["DURACION_MAX"]], ")")
      
      return(x)
      
    })
    
    errors <- as.data.frame(t(errors))
    
    errors <- errors[,c("COD_MAREA", "METIER_IEO", "COD_LANCE",
                        "FECHA_HORA_LAR", "FECHA_HORA_VIR", "DURACION",
                        "TIPO_ERROR")]
    
    return(errors)
    
  }
  
}


#' Check code: 2051
#' Check speed of the hauls.
#' The maximun and minimun speeds are collected in 'caracteristicas_lances' 
#' dataset.
#' @note If the METIER_IEO is NA, this haul is ignored and doesn't return the
#' error.
#' @return Dataframe with erroneus data of every haul characterictics.
check_hauls_speed <- function(){
  
  df <- OAB_hauls[, c("COD_MAREA", "COD_LANCE", "METIER_IEO", "VELOCIDAD")]
  
  cl <- caracteristicas_lances[, c("METIER_IEO", "VELOCIDAD_MAX", "VELOCIDAD_MIN")]
  
  errors <- merge(df, cl, by = "METIER_IEO", all.x = T)
  
  errors <- errors[which(errors[,"VELOCIDAD"] < errors[, "VELOCIDAD_MIN"] |
                           errors[,"VELOCIDAD"] > errors[, "VELOCIDAD_MAX"]),]
  
  if (nrow(errors)>0){
    
    errors <- apply(errors, 1, function(x){
      
      x[["TIPO_ERROR"]] <- paste0("WARNING: speed of haul out of range according to master (", x[["VELOCIDAD_MIN"]], " - " , x[["VELOCIDAD_MAX"]], ")")
      
      return(x)
      
    })
    
    errors <- as.data.frame(t(errors))
    
    errors <- errors[,c("COD_MAREA", "METIER_IEO", "COD_LANCE", "VELOCIDAD", "TIPO_ERROR")]
    
    # all the errors variables are factor so, to avoid errors in decimal
    # conversion, it must be converted to numeric:
    errors[["VELOCIDAD"]] <- as.numeric(as.character(errors[["VELOCIDAD"]]))
    
    return(errors)
    
  }
  
}


#' Check code: 2052
#' Check depth of the hauls.
#' The maximun and minimun depths are collected in 'caracteristicas_lances' 
#' dataset.
#' @note If the METIER_IEO is NA, this haul is ignored and doesn't return the
#' error.
#' @return Dataframe with erroneus data of every haul characterictics.
check_hauls_depth <- function(){
  
  df <- OAB_hauls[, c("COD_MAREA", "COD_LANCE", "METIER_IEO", "PROF_LAR", "PROF_VIR")]
  
  cl <- caracteristicas_lances[, c("METIER_IEO", "PROFUNDIDAD_MAX", "PROFUNDIDAD_MIN")]
  
  errors <- merge(df, cl, by = "METIER_IEO", all.x = T)
  
  errors <- errors[which(errors[,"PROF_LAR"] < errors[, "PROFUNDIDAD_MIN"] |
                           errors[,"PROF_LAR"] > errors[, "PROFUNDIDAD_MAX"] |
                           errors[,"PROF_VIR"] < errors[, "PROFUNDIDAD_MIN"] |
                           errors[,"PROF_VIR"] > errors[, "PROFUNDIDAD_MAX"]),]
  
  if (nrow(errors)>0){
    
    errors <- apply(errors, 1, function(x){
      
      x[["TIPO_ERROR"]] <- paste0("WARNING: depth of haul out of range according to master (", x[["PROFUNDIDAD_MIN"]], " - " , x[["PROFUNDIDAD_MAX"]], ")")
      
      return(x)
      
    })
    
    errors <- as.data.frame(t(errors))
    
    errors <- errors[,c("COD_MAREA", "METIER_IEO", "COD_LANCE", "PROF_LAR", "PROF_VIR", "TIPO_ERROR")]
    
    return(errors)
    
  }
  
  
}







