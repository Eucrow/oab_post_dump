calculate_error <- function(x){
  if(!is.na(x[["FECHA_FIN"]]) && !is.na(x[["FECHA_VIR"]]) ){
    end_date <- as.Date(x[["FECHA_FIN"]], format = "%Y-%m-%d")
    haul_date <- as.Date(x[["FECHA_VIR"]], format = "%Y-%m-%d")
    if(x["ESTRATO_RIM"]=="BACA_AC" && end_date - haul_date > 3){
      return("ERROR: the end date of this BACA_AC trip is more than 3 days later than last haul date.")      
    } else if (x["ESTRATO_RIM"]!="BACA_AC" && end_date - haul_date > 1){
      return("ERROR: the end date of the trip is more than 3 days later than last haul date.")      
    }  
  }
}

apply(error, 1, calculate_error)
