#### Check discards from SIRENO
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
####
#### Functions file 
####
#### Convention style guide: http://r-pkgs.had.co.nz/style.html


#' Check code: 2004
#' Check if the YEAR variable of the dataframe match with year to study.
#' @param df daraframe to check the field YEAR. The dataframe must have a field
#' called YEAR.
#' @return df with errors.
#' @export
#.return_not_empty
field_year <- function(df) {

  errors <- df[df[["YEAR"]] != YEAR, ]
  
  if (nrow(errors) > 0){
    errors <- errors[, c("YEAR", "COD_MAREA")]
    errors <- unique(errors)
    errors <- addTypeOfError(errors, "ERROR: YEAR field is not ",YEAR)
  return(errors)
  }
  
}

#' Check code: 2002
#' Retained weight less than sampled retained weight.
#' 
#' @return dataframe with errors.
retained_catch_less_than_sampled_retained_catch <- function(){
  
  err <- OAB_catches %>%
    select(COD_MAREA, COD_LANCE, COD_ESP, CATEGORIA, P_RET, P_MUE_RET) %>%
    filter(P_RET < P_MUE_RET) %>%
    addTypeOfError("ERROR: captura retenida menor que captura retenida muestreada.")
  
  return(err)
  
}

#' Check code: 2003
#' Empty fields in variables
#' Return empty variables of a OAB dataframes from importOAB functions. Only
#' variables saved in formato_variables dataset as mandatory are checked.
#' Some variables are mandatory according to its gear. The file caracteristicas_arte.csv
#' contains this variables.
#' @details Require one of the dataframes returned by importOABFiles functions:
#' importOABFiles(), importOABCatches(), importOABHauls(), importOABTrips() and
#' ipmortOABLengtsh().
#' @param df: dataframe returned by one of the importOAB functions.
#' @param type_file: type of the imported file according to this values: OAB_CATCHES,
#' OAB_HAULS, OAB_TRIPS and OAB_LENGTHS.
#' @return A dataframe with the COD_MAREA and variables with values missing.
#' @export
empty_fields_in_variables <- function(df,
                                      type_file = c("OAB_TRIPS", "OAB_HAULS",
                                                    "OAB_CATCHES",
                                                    "OAB_LENGTHS")){
  
  # Detect if the variable type_file is correct:
  match.arg(type_file)
  
  # Create helper_text
  helper_text <- substr(type_file, 5, nchar(type_file))
  helper_text <- tolower(helper_text)
  
  
  mandatory_field <- paste0(type_file, "_MANDATORY")
  
  mandatory <- formato_variables[which(!is.na(formato_variables[type_file])
                                       & formato_variables[mandatory_field] == TRUE), c("name_variable")]
  df_mandatory <- df[,mandatory]
  
  err <- check_empty_values_in_variables(df_mandatory, mandatory, helper_text)
  
  # in case there aren't any errors, check_empty_values returns NULL, so:
  if (!is.null(err)){
    
    # check_empty_values return a list with one dataframe by variable, so:
    err <- do.call(rbind, err)
    
    
    # return different fields according to file type:
    # switch(type_file,
    #        OAB_TRIPS = { err <- err[, c("COD_MAREA", "TIPO_ERROR")]},
    #        OAB_HAULS = { err <- err[, c("COD_MAREA", "COD_LANCE", "TIPO_ERROR")]},
    #        OAB_CATCHES = { err <- err[, c("COD_MAREA", "COD_LANCE", "COD_ESP", "TIPO_ERROR")]},
    #        OAB_LENGTHS = { err <- err[, c("COD_MAREA", "COD_LANCE", "COD_ESP", "TIPO_ERROR")]}
    # )
    
    return(err)
  } else {
    return(NULL)
  }
}

#' Check code: Removed
#' Target specie of the hauls is the most catched specie.
coherence_target_sp_with_catch <- function(){
  tryCatch({
    COD_MAREA_with_obj <- OAB_hauls %>%
      select(ESTRATO_RIM, COD_MAREA, COD_LANCE, COD_ESP_OBJ, ESP_OBJ) %>%
      unique()
    
    catches_only_greater_catch <- OAB_catches %>%
      select(COD_MAREA, COD_LANCE, COD_ESP, ESP, P_RET) %>%
      #mutate(peso_total = PESO_RET + PESO_DESCAR) %>%
      group_by(COD_MAREA, COD_LANCE)%>%
      filter(P_RET == max(P_RET))
    
    colnames(catches_only_greater_catch)[colnames(catches_only_greater_catch)=="ESP"] <- "ESP_MAYOR_CAPTURA"
    
    catches_with_obj <- merge(catches_only_greater_catch, COD_MAREA_with_obj, by = c("COD_MAREA", "COD_LANCE"))
    
    
    # function to get the possible ESP_OBJ according to COD_ESP in OAB_catches
    # return list with COD_ESP_OBJ
    get_cod_target_specie <- function(sp_code){
      as.character(especies_objetivo_oab[especies_objetivo_oab$COD_ESP %in% sp_code,"COD_ESP_OBJ"])
    }
    
    # function to check if a COD_ESP has its COD_ESP_OBJ according to the especies_objetivo master
    coherence_sp_target_sp <- function(cod_esp, cod_esp_obj){
      possible_cod_esp_obj <- get_cod_target_specie(cod_esp)
      if (cod_esp_obj %in% possible_cod_esp_obj){
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    
    catches_with_obj$co_sp_target <- apply(catches_with_obj , 1, function(x){
      coherence_sp_target_sp(x["COD_ESP"], x["COD_ESP_OBJ"])
    })
    
    err <- catches_with_obj[catches_with_obj$co_sp_target==FALSE, ] 
    err <- err[, -which(names(err) %in% "co_sp_target")]
    err <- addTypeOfError(err, "WARNING: la especie objetivo no coincide con la especie de mayor captura del lance. Comprobar posible error de tecleo.")
    
    return(err)
  },
  error = function(err){
    print(err)
  }
  )
}

#' Check code: 2006, 2007, 2008, 2009, 2010
#' Check variable with master
#' THIS FUNCTION IS A SHIT!!!! remove and do it individually?????
check_variable_with_master <- function (df, variable){
  
  if(variable != "ESTRATO_RIM" &&
     # variable != "COD_PUERTO" &&
     # variable != "COD_PUERTO_BASE" &&
     # variable != "COD_PUERTO_LLEGADA" &&
     # variable != "COD_PUERTO_DESCARGA" &&
     variable != "COD_ORIGEN" &&
     variable != "COD_ARTE" &&
     variable != "PROCEDENCIA"){
    stop(paste("This function is not available for ", variable))
  }
  
  # If the variable begin with "COD_", the name of the data source
  # is the name of the variable without "COD_"
  variable_formatted <- variable
  if (grepl("^COD_", variable)){
    variable_formatted <- strsplit(variable, "COD_")
    variable_formatted <- variable_formatted[[1]][2]
  }
  
  # *********** CASE OF PUERTO_BASE, PUERTO_LLEGADA OR PUERTO_DESCARGA ***********
  # change the name of PUERTO_BASE, PUERTO_LLEGADA or PUERTO_DESCARGA
  # to PUERTO:
  if(variable_formatted == "PUERTO_LLEGADA" ||
     variable_formatted == "PUERTO_BASE" ||
     variable_formatted == "PUERTO_DESCARGA") {
    variable_puerto_original <- variable_formatted
    variable_formatted <- "PUERTO"
  }
  
  # ******************************************************************************
  name_dataset <- tolower(variable_formatted)
  # name_dataset <- paste0(name_dataset, "_OAB")
  
  # *********** CASE OF PUERTO_BASE, PUERTO_LLEGADA OR PUERTO_DESCARGA ***********    
  # it's required  join via the variable COD_PUERTO, so a new variable is required:
  if(variable == "COD_PUERTO_LLEGADA" ||
     # variable == "COD_PUERTO_BASE" ||
     variable == "COD_PUERTO_DESCARGA") {
    variable_to_change <- "COD_PUERTO"
  }
  # ****************************************************************************** 
  
  dataframe_variable <- get(name_dataset)
  
  # *********** CASE OF PUERTO_BASE, PUERTO_LLEGADA OR PUERTO_DESCARGA ***********  
  if (exists("variable_to_change")){
    names(dataframe_variable)[names(dataframe_variable) == variable_to_change] <- variable
  }
  
  #search the errors in variable
  # errors <- anti_join(df, get(name_dataset), by = variable)
  
  # ******************************************************************************
  
  dataset <- get(name_dataset)
  
  #prepare to return
  if (exists("variable_to_change")){
    errors <- anti_join(df, dataset, by = setNames(nm=variable, variable_to_change))
    fields_to_filter <- c("COD_MAREA", variable, variable_puerto_original)
  } else {
    errors <- anti_join(df, dataset, by = setNames(nm=variable, variable))
    fields_to_filter <- c("COD_MAREA", variable, variable_formatted)
  }
  
  
  errors <- errors %>%
    select(one_of(fields_to_filter))%>%
    unique()
  
  if (nrow(errors) > 0){
    text_type_of_error <- paste0("ERROR: ", name_dataset, " no concuerda con los maestros de SIRENO")
    errors <- addTypeOfError(errors, text_type_of_error)
  
    return(errors)
  }

}

#' Check code: 2011
#' Check year in COD_MAREA.
#' @param df: df to check. Must be a df obtained by importOAB functions.
#' @return df with errors
#' @export
check_year_in_COD_MAREA <- function(df){
  
  COD_MAREA_split <- split_COD_MAREA(df)

  errors <- which(!(COD_MAREA_split[["year"]] %in% YEAR))
  errors <- df[errors,]
  if(nrow(errors)>0){
    
    # this line add a comment to the errors dataframe wich contain the value of the
    # df variable
    errors.name <- deparse(substitute(df))
    
    errors <- errors %>%
      select(COD_MAREA, YEAR)%>%
      addTypeOfError(paste("ERROR: el año del COD_MAREA en", errors.name, "no coincide con el año a comprobar"))
    
    return(errors)  
  }

  
}

#' Check code: 2012
#' Coherence between rim stratum and gear variables.
#' @return dataframe with wrong coherence.
coherence_rim_stratum_gear <- function(df){
  
  try(variables_in_df(c("ESTRATO_RIM", "COD_ARTE"), df))
  
  BASE_FIELDS <- c("COD_MAREA", "COD_LANCE", "ESTRATO_RIM", "COD_ARTE", "ARTE")
  
  estrato_rim_arte["VALID"] <- TRUE
  
  df <- df[, c(BASE_FIELDS)]
  df <- unique(df)
  errors <- merge(x=df, y=estrato_rim_arte, by=c("ESTRATO_RIM", "COD_ARTE", "ARTE"), all.x=TRUE)
  errors <- errors[which(is.na(errors["VALID"])), ]
  if(nrow(errors)>0){
    errors <- addTypeOfError(errors, "ERROR: no concuerda el estrato_rim con el arte")
    return(errors)
  }
  # errors <- df %>%
  #   select(one_of(BASE_FIELDS)) %>%
  #   unique() %>%
  #   anti_join(y=estrato_rim_arte_OAB, by=c("ESTRATO_RIM", "COD_ARTE")) %>%
  #   addTypeOfError("ERROR: no concuerda el estrato_rim con el arte")
  # 
  # return(errors)
  
}

#' Check code: 2013
#' Coherence between rim stratrum and origin variables.
#' @return dataframe with wrong coherence.
coherence_rim_stratum_origin <- function(df){
  
  try(variables_in_df(c("ESTRATO_RIM", "COD_ORIGEN"), df))
  
  BASE_FIELDS <- c("COD_MAREA", "ESTRATO_RIM", "COD_ORIGEN", "ORIGEN")
  
  if("COD_LANCE" %in% colnames(df)){
    BASE_FIELDS <- c( BASE_FIELDS, "COD_LANCE")
  }
  
  estrato_rim_origen["VALID"] <- TRUE
  
  errors <- merge(x= df, y=estrato_rim_origen, by=c("ESTRATO_RIM", "COD_ORIGEN"), all.x=TRUE)
  errors <- errors[which(is.na(errors[["VALID"]])), ]
  
  # COMPROBAR QUE ESTO FUNCIONA BIEN!!
  
  if(nrow(errors)>0){
    
    errors <- errors[, c(BASE_FIELDS)]
    errors <- unique(errors)
    errors <- addTypeOfError(errors, "ERROR: no concuerda el estrato_rim con el origen")
    return(errors)
  }

}

#' Check code: 2014
#' Discarded sampled weigth less than zero (or NA) when there are any specimens
#' discarded.
discarded_sampled_weight_when_specimens_discarded <- function(df_catches){
  
  errors <- df_catches[which(
    df_catches[["EJEM_DESCAR"]] > 0 &
      (df_catches[["P_MUE_TOT_DESCAR"]] <= 0 | is.na(df_catches[["P_MUE_TOT_DESCAR"]]))
  ),
  c("COD_MAREA", "COD_ESP", "A3_ESP", "ESP", "EJEM_DESCAR", "P_MUE_TOT_DESCAR")]
  
  errors <- addTypeOfError(errors, "ERROR: there are specimens discarded without discarded sampled weight in catches table.")
  
}

#' Return a oulier
#' ggplot defines an outlier by default as something that's > 1.5*IQR from the
#' borders of the box. 
#' To use only in get_outliers_speed() and show_outliers_speed() functions
is_outlier <- function(x) {
  return(x < quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T) | x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T))
  }  


#' Check code: 2015
#' Get the posible outlier in speed.
#' Using the same criteria as ggplot in a boxplot: an outlier is something
#' that's > 1.5*IQR from the borders of the box.
get_speed_outliers <- function(){
  
  hauls_speed <- OAB_hauls %>%
    select(COD_MAREA, COD_LANCE, ESTRATO_RIM, ESP_OBJ, VELOCIDAD) %>%
    filter(ESTRATO_RIM %in% c("BACA_CN", "BACA_GC", "JURELERA_CN", "PAREJA_CN", "RAPANTER_AC"))
  
  hauls_speed$str <- as.character(hauls_speed$ESTRATO_RIM)
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ != "CABALLA", "str"] <- "PAREJA_CN.RESTO"
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ == "CABALLA", "str"] <- "PAREJA_CN.CABALLA"
 
  hauls_speed <- hauls_speed %>%
    group_by(str)%>%
    mutate(OUTLIER = ifelse(is_outlier(VELOCIDAD), VELOCIDAD, as.numeric(NA)))%>%
    filter(!is.na(OUTLIER))%>%
    filter(VELOCIDAD!=OUTLIER)%>% #TODO: when outlier == velocidad, is returned. With this filter I change it, but it is not the west way
    addTypeOfError("WARNING: posible outlier en el campo velocidad")

  hauls_speed <- hauls_speed[, -which(colnames(hauls_speed) %in% c("str"))]
  
  return(as.data.frame(hauls_speed))  
  
}

#' Speed boxplot graphic by ESTRATO_RIM.
#' Sow boxplot graphic with speed to check the speed by ESTRATRO_RIM.
#' @export
view_speed_outliers <-function(){
  
  library(ggplot2)  
  library(ggiraph)
  
  hauls_speed <- OAB_hauls %>%
    select(COD_MAREA, ESTRATO_RIM, ESP_OBJ, VELOCIDAD) %>%
    filter(ESTRATO_RIM %in% c("BACA_CN", "BACA_GC", "JURELERA_CN", "PAREJA_CN", "RAPANTER_AC"))
  
  hauls_speed$str <- as.character(hauls_speed$ESTRATO_RIM)
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ != "CABALLA", "str"] <- "PAREJA_CN.RESTO"
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ == "CABALLA", "str"] <- "PAREJA_CN.CABALLA"

  hauls_speed %>%
    group_by(str)%>%
    #in the next mutate line: if in the ifelse use COD_MAREA as the returned value
    #when the condition is True, it doesnt't print the COD_MAREA but a weird number??????????
    mutate(outlier = ifelse(is_outlier(VELOCIDAD), VELOCIDAD, as.numeric(NA)))%>%
      ggplot(., aes(str, VELOCIDAD))+
      # geom_boxplot_interactive()+
      geom_boxplot()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      geom_text(aes(label = outlier), na.rm=T, hjust=1.1)

  # ggiraph(code = print(p))  
  # return (p)
}

#' Check code: 2016
#' Sampled hauls without catches weight.
#' @return dataframe with COD_MAREA with errors.
hauls_sampled_with_catch_weights <- function(){

    sampled <- OAB_hauls %>%
      filter(MUESTREADO == "S") %>%
      select(COD_MAREA) %>%
      unique()
    
    catches <- OAB_catches %>%
      group_by(COD_MAREA) %>%
      summarise(P_CAP_TOT = sum(P_CAP)) %>%
      filter(P_CAP_TOT==0)
    
    if (nrow(catches) != 0){
      err <- merge(x=sampled, y=catches, by= "COD_MAREA")
      err <- addTypeOfError(err, "ERROR: Lance muestreado pero sin peso de captura.")
      return(err)
    }
  
}

#' Check code: 2017
#' Cable length is greather than 1000 m.
length_cable_1000 <- function(){
  
  tryCatch({
    # in the next line, the use of NA prevent the returned NA rows:
    war <- OAB_hauls[which(OAB_hauls$CABLE>1000),c("ESTRATO_RIM", "COD_MAREA", "COD_LANCE", "CABLE")]
    
    war <- addTypeOfError(war, "WARNING: el cable largado es mayor que 1000m.")
    
    return(war)
  },
  error = function(err){
    print(err)
  }
  )
  
}

#' Check code: 2018
#' Priority species without lengths sampled.
# priority_species_without_lengths <- function(){
#   
#   # get the list of priority species which must be measured
#   species_to_measure <- especies_a_medir_OAB[,"COD_ESP"]
#   
#   # get Species With Catch which must be measured
#   swc <- OAB_catches[which(OAB_catches[["COD_ESP"]]%in%species_to_measure),
#                      c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "P_CAP")]
#   swc <- unique(swc)
#   
#   # clean lengths
#   lengths_clean <- OAB_lengths[, c("COD_MAREA", "COD_LANCE",
#                                    "COD_ESP", "ESP", "EJEM_MEDIDOS")]
#   
#   lengths_clean <- aggregate.data.frame(lengths_clean[, c("EJEM_MEDIDOS")],
#                                         by=list(lengths_clean$COD_MAREA,
#                                                 lengths_clean$COD_LANCE,
#                                                 lengths_clean$COD_ESP,
#                                                 lengths_clean$ESP),
#                                         sum, na.rm=TRUE)
#   
#   colnames(lengths_clean) <- c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "EJEM_MEDIDOS")
#   
#   # create errors dataframe
#   errors <- merge(swc, 
#                   lengths_clean, 
#                   by.x = c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP"),
#                   all.x = TRUE)
#   errors <- errors[which(errors$EJEM_MEDIDOS==0 |
#                            is.na(errors$EJEM_MEDIDOS)),]
#   
#   errors <- addTypeOfError(errors, "ERROR: priority species which hasn't been measured")
#   
# }

priority_species_without_lengths <- function(){
  
  # clean lengths
  lengths_clean <- OAB_lengths[, c("COD_MAREA", "COD_LANCE",
                                   "COD_ESP", "ESP", "TIPO_CAPTURA", "EJEM_MEDIDOS")]
  
  lengths_clean <- aggregate.data.frame(lengths_clean[, c("EJEM_MEDIDOS")],
                                        by=list(lengths_clean$COD_MAREA,
                                                lengths_clean$COD_LANCE,
                                                lengths_clean$COD_ESP,
                                                lengths_clean$ESP,
                                                lengths_clean$TIPO_CAPTURA),
                                        sum, na.rm=TRUE)
  
  colnames(lengths_clean) <- c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "TIPO_CAPTURA", "EJEM_MEDIDOS")
  
  # get the list of priority species which must be measured
  species_to_measure <- especies_a_medir_OAB[,"COD_ESP"]
  
  # get Species With Weight which must be measured
  sww <- OAB_catches[which(OAB_catches[["COD_ESP"]]%in%species_to_measure),
                     c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "P_RET", "P_DESCAR")]
  sww <- unique(sww)
  
  #sww retain
  sww_retain <- sww[sww[["P_RET"]] != 0 & !is.na(sww[["P_RET"]]), names(sww)!=c("P_DESCAR")]
  sww_retain[["TIPO_CAPTURA"]] <- "C"
  
  errors_retain <- merge(sww_retain, 
                  lengths_clean, 
                  by.x = c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "TIPO_CAPTURA"),
                  all.x = TRUE)
  
  errors_retain <- errors_retain[which(errors_retain$EJEM_MEDIDOS==0 |
                           is.na(errors_retain$EJEM_MEDIDOS)),]
  
  errors_retain <- addTypeOfError(errors_retain, "ERROR: priority retained species not measured")
  
  #sww discard
  sww_discard <- sww[sww[["P_DESCAR"]] != 0 & !is.na(sww[["P_DESCAR"]]), names(sww)!=c("P_RET")]
  sww_discard[["TIPO_CAPTURA"]] <- "D"
  
  errors_discard <- merge(sww_discard, 
                         lengths_clean, 
                         by.x = c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "TIPO_CAPTURA"))
  errors_discard <- errors_discard[which(errors_discard$EJEM_MEDIDOS==0 |
                                         is.na(errors_discard$EJEM_MEDIDOS)),]
  
  errors_discard <- addTypeOfError(errors_discard, "ERROR: priority discarded species not measured")

  #merge errors

  if(nrow(errors_retain)!=0 & nrow(errors_discard)!=0){
    
    errors <- merge(errors_retain, errors_discard, by=c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "TIPO_CAPTURA", "EJEM_MEDIDOS", "TIPO_ERROR"), all = TRUE)
    return (errors)
    
  } else if (nrow(errors_retain) == 0) {
    return (errors_discard)    
    
  } else if (nrow(errors_discard) == 0) {
    return (errors_retain)    
    
  }
  
}

#' Check code: 2019
#' Reason discard field filled.
#' Only whith the discarded species are in the species list saved in
#' especies_a_medir_OAB.csv
reason_discard_field_filled <- function(df){
  
  species_to_measure <- especies_a_medir_OAB[,"COD_ESP"]
  
  errors <- df[which(
    df[["P_DESCAR"]]>0 & df[["COD_DESCAR"]] == ""),
    c("COD_MAREA", "COD_LANCE", "COD_ESP", "A3_ESP", "ESP", "P_DESCAR", "RAZON_DESCAR")]
  
  errors <- unique(errors)
  
  errors <- errors[errors[["COD_ESP"]] %in% species_to_measure,]
  
  errors <- addTypeOfError(errors, "ERROR: this species must have the field reason discard filled but it doesn't")
  
  return(errors)
  
}

#' Check code: 2020
#' Retained sampled weigth less than zero (or NA) when there are any specimens
# retained
retained_sampled_weight_when_specimens_retained <- function(df_catches){
  
  errors <- df_catches[which(
    df_catches[["EJEM_RET"]] > 0 &
      (df_catches[["P_MUE_RET"]] <= 0 |
         is.na(df_catches[["P_MUE_RET"]]))),
    c("COD_MAREA", "COD_LANCE", "COD_ESP", "A3_ESP", "ESP", "EJEM_RET", "P_MUE_RET")
    ]
  
  errors <- addTypeOfError(errors, "ERROR: there are specimens retained without retained sampled weight.")
  
}

#' Check code: 2021
#' Sampled discard weight less than subsample discard weight.
sampled_discard_less_subsample_discard <- function(df){
  
  # usually the PESO_SUB_MUE_TOT is NA, so it is necessary detect it.
  errors <- df[
    which( !is.na(df$P_SUB_MUE_DESCAR) & df$P_SUB_MUE_DESCAR > df$P_MUE_TOT_DESCAR),
    c("COD_MAREA", "COD_LANCE", "COD_ESP", "A3_ESP", "ESP",
      "P_SUB_MUE_DESCAR", "P_MUE_TOT_DESCAR")
    ]
  
  errors <- addTypeOfError(errors, "ERROR: sampled discard weight of the species (in 'catches' screen of SIRENO) less than subsample discard weight.")
  
  return(errors)
  
}

#' Check code: 2022
#' Species without weight caught neither weight discarded.
#' @return dataframe with COD_MAREA with errors.
species_without_caught_neither_discarded_weight <- function(){

  err <- OAB_catches %>%
    filter(P_CAP == 0 & P_DESCAR == 0) %>%
    select(COD_MAREA, COD_LANCE, COD_ESP, ESP, P_CAP, P_DESCAR) %>%
    arrange(COD_MAREA)%>%
    unique() %>%
    addTypeOfError("ERROR: Especie sin peso captura ni peso descarte.")
  
  return(err)
  
}

#' Check code: 2023
#' Coherence target species with metier ieo.
#' @note Require the file metier_ieo_especie_objetivo_OAB.txt
coherence_target_species_metier_ieo <- function(){
  
  # get dataset with relation between metier ieo and target species
  ms <- metier_ieo_especie_objetivo_OAB
  ms$OK <- "ok"
  
  # hauls cleaned
  hc <- OAB_hauls[, c("COD_MAREA", "COD_LANCE", "METIER_IEO", "COD_ESP_OBJ")]
  
  # errors
  errors <- merge(hc, ms,
                  all.x = TRUE,
                  by = c("METIER_IEO", "COD_ESP_OBJ"))
  
  if (nrow(errors)>0){
    errors <- errors[is.na(errors[["OK"]]),]
    
    errors <- addTypeOfError(errors, "ERROR: the target species is not coherent with metier ieo.")
    
    # But, in special cases where the METIER_IEO is no filed:
    errors[errors$METIER_IEO == "", "TIPO_ERROR"] <- "ERROR: the target species field can't be checked because the METIER_IEO is empty."
  
    errors <- Filter(function(x) {!all(is.na(x))}, errors)
    
    return(errors)
  }
  
}

#' Check code: 2024
#' Total discarded weight less than sampled discard weight.
discarded_weight_less_than_sampled_discarded_weight <- function(df){ 
  
  # lets assume NA are equal to 0
  
  df[is.na(df[["P_DESCAR"]]), "P_DESCAR" ] <- 0
  df[is.na(df[["P_MUE_TOT_DESCAR"]]), "P_MUE_TOT_DESCAR" ] <- 0
  
  df[["P_DESCAR"]] <- round(df[["P_DESCAR"]], 2)
  df[["P_MUE_TOT_DESCAR"]] <- round(df[["P_MUE_TOT_DESCAR"]], 2)
  
  errors <- df[
    df$P_DESCAR < df$P_MUE_TOT_DESCAR,
    c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "P_DESCAR", "P_MUE_TOT_DESCAR")
    ]
  
  errors <- addTypeOfError(errors, "ERROR: total discarded weight less than sampled discard weight.")
  
}

#' Check code: 2025
#' Shooting date of the hauls previous to hauling date 
#' Use the OAB_hauls df
#' @return df with errors
#' @export
hauling_date_before_shooting_date <- function(){
  
  # it is imperative check if any of this fields are empty: FECHA_LAR, FECHA_VIR
  # HORA_LAR and HORA_VIR:
  
  OAB_hauls <- OAB_hauls[OAB_hauls[["MUESTREADO"]] == "S",]
  
  start <- OAB_hauls$FECHA_HORA_LAR
  
  end <- OAB_hauls$FECHA_HORA_VIR
  
  start_end <- start - end
  
  errors <- OAB_hauls[(start - end) > 0 | is.na(start - end), c("COD_MAREA", "COD_LANCE", "FECHA_LAR", "HORA_LAR", "FECHA_VIR", "HORA_VIR")]
  
  errors <- addTypeOfError(errors, "ERROR: La fecha y hora de virada es anterior o igual a la fecha y hora de largada.")
  
  if (nrow(errors)==0){
    return(NULL)
  } else {
    return(errors)
  }
}

#' Check code: 2026
#' Start date of OAB_trips previous to end date.
#' Use the OAB_trips df.
#' @return df with errors
#' @export
trips_initial_date_before_final_date <- function(){
  
  start <- as.POSIXlt(OAB_trips$FECHA_INI, format="%d/%m/%Y")
  end <- as.POSIXlt(OAB_trips$FECHA_FIN, format="%d/%m/%Y") 
  
  errors <- OAB_trips[(start - end)>0,]
  
  if(length(nrow(errors)==0)) {
    return(NULL)
  } else {
    errors <- addTypeOfError(errors, "ERROR: la ficha final de la marea es anterior a la fecha inicial")
    return(errors)
  }
  
}

#' 2027
#' Final date included in id marea, only in GC samples.
#' Check the final date is included in id marea, only in GC samples (CERCO_GC and
#' BACA_GC)
#' @return df with errors
#' @export
trips_final_date_in_COD_MAREA_GC <- function(){
  errors <- OAB_trips %>%
    filter(ESTRATO_RIM=="BACA_GC" | ESTRATO_RIM == "CERCO_GC")%>%
    check_date_with_COD_MAREA("FECHA_FIN")
  
  if (!is.null(errors)) {
    errors <- errors %>%
      select(COD_MAREA, FECHA_FIN, TIPO_ERROR)
    
    return(errors)
  } else {
    return(NULL)
  }
}

#' Check code: 2034
#' Check if the hauls with discarded species has total discarded weight --------
#' @return Dataframe with the erroneus data with a new a variable with the type
#' of error.
#' @param OAB_lengths: discards lengths dataframe
#' @param OAB_hauls: hauls dataframe
#' @export
discarded_species_with_total_discarded_weight <- function(df_lengths, df_hauls){
  
  # species with discards lengths
  sd <- df_lengths[df_lengths$TIPO_CAPTURA == "D", ]
  sd <- sd[, c("COD_MAREA", "COD_LANCE")]
  sd <- unique(sd)
  
  # weight discarded by acronym and haul, from the hauls table
  wdah <- OAB_hauls[which(OAB_hauls$P_TOT_DESCAR != 0), c("COD_MAREA",
                                                          "COD_LANCE",
                                                          "P_TOT_DESCAR")]
  wdah <- unique(wdah)
  
  errors <- anti_join(sd, wdah)
  
  if(length(errors)>0){
    errors <- addTypeOfError(errors, "There are lengths discarded in hauls table but there aren't discarded weight in catches table")
    return(errors)
  }
  
}

#' Check code: 2046
#' Check Discard Reason variable with master -----------------------------------
#' Check if the discard reason variable match with master. Ignore the empty
#' fields.
#' @param df dataframe where the variable is allocated.
#' @return Dataframe with the erroneus data. A new a variable with the type
#' of error is added.
check_discard_reason_variable_with_master <- function (df){
  
  try(variable_exists_in_df("COD_DESCAR", df)== T )
  
  #remove rows with empty Reason Discard variable
  df <- df[df$COD_DESCAR != "",]
  
  errors <- anti_join(df, razon_descarte_OAB, by = "COD_DESCAR")
  
  errors <- errors %>%
    select(one_of(c("COD_MAREA", "COD_DESCAR")))%>%
    unique()
  
  text_type_of_error <- paste0("ERROR: el código descarte no concuerda con los maestros de SIRENO")
  errors <- addTypeOfError(errors, text_type_of_error)
  
  return(errors)
  
}

#' Check code: 2047
#' Check the hauls date are contained in date interval of the trips -----
#' @param df_trips trips dataframe.
#' @param df_hauls hauls dataframe.
#' @return Dataframe with the erroneus data. A new a variable with the type
#' of error is added.
date_hauls_in_date_interval_trips <- function(df_trips, df_hauls) {
  
  # it is imperative check if any of this fields are empty: FECHA_LAR, FECHA_VIR
  # HORA_LAR and HORA_VIR:

  # get date hauls
  df_hauls_simplyfied <- unique(df_hauls[, c("COD_MAREA", "COD_LANCE", "FECHA_LAR", "FECHA_VIR")])
  df_hauls_simplyfied$FECHA_LAR <- as.POSIXlt(df_hauls_simplyfied$FECHA_LAR, format="%d-%b-%y")
  df_hauls_simplyfied$FECHA_VIR <- as.POSIXlt(df_hauls_simplyfied$FECHA_VIR, format="%d-%b-%y")
  
  # get date trips
  df_trips_simplyfied <- unique(df_trips[, c("COD_MAREA", "FECHA_INI", "FECHA_FIN")])
  df_trips_simplyfied$FECHA_INI <- as.POSIXlt(df_trips_simplyfied$FECHA_INI, format="%d/%m/%Y")
  df_trips_simplyfied$FECHA_FIN <- as.POSIXlt(df_trips_simplyfied$FECHA_FIN, format="%d/%m/%Y")
  colnames(df_trips_simplyfied) <- c("COD_MAREA", "FECHA_INI_TRIP", "FECHA_FIN_TRIP")
  
  # merge hauls with trips
  df_dates <- merge(df_hauls_simplyfied, df_trips_simplyfied, by="COD_MAREA", all.x = T)
  
  # detect hauls with 
  errors <- df_dates[which(df_dates[["FECHA_LAR"]] < df_dates[["FECHA_INI_TRIP"]] |
                 df_dates[["FECHA_VIR"]] > df_dates[["FECHA_FIN_TRIP"]]),]
  

  #return
  if (nrow(errors)==0){
    return(NULL)
  } else {
    
    # add error columns
    errors[which(errors[["FECHA_LAR"]] < errors[["FECHA_INI_TRIP"]]),
           "TIPO_ERROR"] <- c("ERROR: Shooting date before initial date of the trip")
    
    errors[which(errors[["FECHA_VIR"]] > errors[["FECHA_FIN_TRIP"]]),
           "TIPO_ERROR"] <- c("ERROR: Hauling date after final date of the trip")
    
    return(errors)
  }
  
}


#' Check code: 2049
#' Check total discarded weight equal to zero but with sampled discarded
#' weight ----------------------------------------------------------------------
#' @return Dataframe with the erroneous data. A new a variable with the type
#' of error is added.
total_discarded_weight_zero_with_sampled_discard_weight <- function(){
  
  errors <- OAB_hauls[
    which( OAB_hauls$P_TOT_DESCAR == 0 & OAB_hauls$P_MUE_DESCAR > 0),
    c("COD_MAREA", "COD_LANCE", "P_TOT_DESCAR", "P_MUE_DESCAR")
    ]
  
  errors <- addTypeOfError(errors, "ERROR: total discarded weight equal to 0 but with sampled discarded weight. This erroneus data can generate multiple errors which can be fixed correcting the total discarded weight.")
  
  return(errors)
  
}

#' Check code: 2053
#' Check trips duration.
#' The duration of the trip is calculated with the date and time of setting
#' and hauling all the hauls of the trip. Then are compared with the information
#' stored in duracion_mareas_OAB dataset.
#' 
#' @return dataframe with errors
trip_duration <- function(){
  
  trip_hauls <- duracion_mareas_OAB
  
  errors <- OAB_hauls %>%
    select(COD_MAREA, COD_LANCE, ESTRATO_RIM, FECHA_HORA_LAR, FECHA_HORA_VIR) %>%
    unique() %>%
    group_by(COD_MAREA, ESTRATO_RIM) %>%
    summarise(first_date = min(FECHA_HORA_LAR), last_date = max(FECHA_HORA_VIR)) %>%
    mutate(duration_trip = difftime(last_date, first_date, units = "hours")) %>%
    merge(, y = trip_hauls, all.x = T) %>%
    filter(duration_trip > DURACION_MAX) %>%
    mutate(duration_trip = round(duration_trip, 0)) %>%
    addTypeOfError("WARNING: Trip duration too long. You should check the date and time of the shoot and hauling.")

  if(nrow(errors)>0){
    return(errors)
  }
  
  
}


# Haul sampled with empty discard weight.
# When a haul is sampled, the discard weight must be zero even there aren't any
# species discarded
# THIS CHECK CAN'T BE DONE BECAUSE PESO_DESCAR IS FILLED WITH A ZERO IN THE
# REPORT IF THE FIELD IS EMPTY IN SIRENO.
haul_sampled_with_empty_discard_weight <- function(){
  
  sampled_hauls <- OAB_hauls[which(OAB_hauls["MUESTREADO"]=="S"),
                             c("COD_MAREA", "COD_LANCE")]
  
  clean_catches <- OAB_catches[, c("COD_MAREA", "COD_LANCE", "P_DESCAR")]
  clean_catches <- unique(clean_catches)
  
  sampled_hauls <- merge(sampled_hauls,
                         clean_catches,
                         by = c("COD_MAREA", "COD_LANCE"),
                         all.x = TRUE)
  
  errors <- sampled_hauls[which(sampled_hauls[["P_TOT_DESCAR"]] == ""),]
  
  sampled_catches <- merge(OAB_catches, sampled_hauls, by=c("COD_MAREA", "COD"))
  
}

#' Check code: 2045
#' Check the coherence between origin and statistical rectangle.
#' The data is taken from the oab hauls dataset.
#' @return Dataframe with errors.
coherence_origin_statistical_rectangle <- function(){
  
  clean_hauls <- OAB_hauls[, c("COD_MAREA", "COD_LANCE", "LAT_VIR_CGS",
                               "LON_VIR_CGS", "COD_ORIGEN", "CUADRICULA_ICES" )]
  
  origin_statistical_rectangle[["VALID"]] <- TRUE
  
  err <- merge(clean_hauls, origin_statistical_rectangle,
               by.x = c("COD_ORIGEN", "CUADRICULA_ICES"),
               by.y = c("COD_ORIGEN", "CUADRICULA_ICES"),
               all.x = T )
  
  err <- err[is.na(err$VALID),]
  
  if (nrow(err) > 0){
    
    err <- addTypeOfError(err, "WARNING: Origin doesn't match with statistical rectangle according to master dataset.")
    
    err <- err[, c("COD_MAREA", "COD_LANCE", "COD_ORIGEN", "CUADRICULA_ICES",
                   "TIPO_ERROR")]
    
    return(err)
    
  }
  
}

#' Check code: 2054
#' Check if the haul date is different to shooting date.
#' This is a warning because the haul date of 'rasco' gear is taken when the
#' gear is hauling.
#' @return Dataframe with errors.
haul_date_shooting_date <- function(){
  
  err <- OAB_hauls[, c("COD_MAREA", "COD_LANCE", "FECHA_LAR", "FECHA_LANCE")]
  err[["FECHA_LAR"]] <- as.POSIXct(err[["FECHA_LAR"]], format="%d/%m/%Y")
  
  err <- err[which (err[["FECHA_LAR"]] != err[["FECHA_LANCE"]]),]
  
  if (nrow(err) > 0){
    err <- addTypeOfError(err, "WARNING: the date of the haul is different to the shooting date.")
  }
  
}

#' Check code: 2055
#' Detect positive longitudes.
#' @param var var to check (from the OAB_hauls dataframe)
#' @return Dataframe with errors
positive_longitude <- function(var){
  
  err <- OAB_hauls[OAB_hauls[[var]] > 0, c("COD_MAREA", "COD_LANCE", var)]
  
  # ignore when var contain a NA. There are other check to detect if the
  # variable is empty
  err <- err[!is.na(err[[var]]),]
  
  if( nrow(err) > 0){
    err <- addTypeOfError(err, "ERROR: the longitude is positive.")
  }
  
  return(err)
  
}


#' check code: 2056
#' The variables SOLO_MUESTRA or PORCENTAJE_OBSERVADO must be filled in this
#' way:
#' - When SOLO_MUESTRA is True, PORCENTAJE_OBSERVADO must be NA (or 0). If SOLO_MUESTRA
#' is False, PORCENTAJE_OBSERVADO must be greather than 0.
#' @return dataframe with errors
litter_sample <- function(){
  
  err <- OAB_litter[
    which( (OAB_litter[["SOLO_MUESTRA"]] == FALSE & is.na(OAB_litter[["PORCENTAJE_OBSERVADO"]])) |
             (OAB_litter[["SOLO_MUESTRA"]] == TRUE & OAB_litter[["PORCENTAJE_OBSERVADO"]]>0)) ,
    c("COD_MAREA", "COD_LANCE", "SOLO_MUESTRA", "PORCENTAJE_OBSERVADO")]
  
  err <- unique(err)
  
  err <- addTypeOfError(err, "ERROR: If SOLO_MUESTRA is TRUE PORCENTAJE_OBSERVADO must be empty. If SOLO_MUESTRA is FALSE PORCENTAJE_OBSERVADO must be greather than 0.")
  
  return(err)
  
}

#' check code: 2058
#' Total weight discarded 0 or greater than 0 in not measured hauls.
#' When a haul is not measured, the total weight discarded must be empty (and
#' not 0).
#' @return dataframe with errors
zero_discarded_weights_in_not_measured_haul <- function(){
  
  err <- OAB_hauls[which(OAB_hauls[["MUESTREADO"]] == FALSE &
                           OAB_hauls[["P_TOT_DESCAR"]] >= 0),]
  
  err <- err[, c("COD_MAREA", "COD_LANCE", "MUESTREADO", "P_TOT_DESCAR")]
  
  err <- addTypeOfError(err, "ERROR: Total weight discarded must be empty when the haul is not measured.")
  
  return(err)  
  
}

#' check code:2001
#' Check the size range of species with the rango_tallas_historico_caladero dataset.
#' In case of discarded lengths, only maximum size is considered.
#' @return dataframe with warnings lengths
check_size_range<- function (){
  
  OAB_lengths <- merge(x = OAB_lengths,
                        y = sapmuebase::caladero_origen[,c("CALADERO", "COD_ORIGEN")],
                        all.x = T,
                        by="COD_ORIGEN")

  warningsOutOfRangeCatched <- OAB_lengths %>%
    filter(TIPO_CAPTURA == "C") %>%
    select(COD_MAREA, COD_LANCE, ESP, COD_ESP, TIPO_CAPTURA, CATEGORIA, TALLA, CALADERO)%>%
    merge(y = rango_tallas_historico_caladero,
          by.x = c("COD_ESP", "CALADERO"),
          by.y = c("COD_ESP", "CALADERO"),
          all.x = T)%>%
    filter(TALLA < TALLA_MIN | TALLA > TALLA_MAX)%>%
    # it's not possible use addTypeOfError here, I don't know why
    mutate(TIPO_ERROR = paste("WARNING: Talla fuera del rango histórico de tallas por caladero:", TALLA_MIN, "-", TALLA_MAX))%>%
    select(-c(TALLA_MIN, TALLA_MAX))
  
  warningsOutOfRangeDiscarded <- OAB_lengths %>%
    filter(TIPO_CAPTURA == "D") %>%
    select(COD_MAREA, COD_LANCE, ESP, COD_ESP, TIPO_CAPTURA, CATEGORIA, TALLA, CALADERO)%>%
    merge(y = rango_tallas_historico_caladero,
          by.x = c("COD_ESP", "CALADERO"),
          by.y = c("COD_ESP", "CALADERO"),
          all.x = T)%>%
    filter(TALLA > TALLA_MAX)%>%
    # it's not possible use addTypeOfError here, I don't know why
    mutate(TIPO_ERROR = paste("WARNING: Talla máxima fuera del rango histórico de tallas por caladero:", TALLA_MAX))%>%
    select(-c(TALLA_MIN, TALLA_MAX))
  
  warnings <- merge(warningsOutOfRangeCatched, warningsOutOfRangeDiscarded, all = T)
  
  if (nrow(warnings)>0){
    return(warnings) 
  }
  
}

#' check code:2005
#' Check species included in size range by fishing ground dataset.
#' @return dataframe with warnings lengths
check_species_in_size_range_historical <- function (){
  
  OAB_lengths <- merge(x = OAB_lengths,
                       y = sapmuebase::caladero_origen[,c("CALADERO", "COD_ORIGEN")],
                       all.x = T,
                       by="COD_ORIGEN")
  
  warnings <- OAB_lengths%>%
    select(COD_MAREA, COD_LANCE, ESP, COD_ESP, TIPO_CAPTURA, CALADERO)%>%
    merge(y = rango_tallas_historico_caladero,
          by.x = c("COD_ESP", "CALADERO"),
          by.y = c("COD_ESP", "CALADERO"),
          all.x = T)%>%
    filter(is.na(TALLA_MIN) | is.na((TALLA_MAX)))%>%
    unique()%>%
    addTypeOfError("WARNING: esta especie no se encuentra en el maestro histórico por caladero de tallas mínimas y máximas por estrato rim. Por lo tanto habría que comprobar manualmente que el tamaño es coherente.")%>%
    select(-c(TALLA_MIN, TALLA_MAX))
  
  if (nrow(warnings)>0){
    return(warnings) 
  }
  
}

#' check code: 2059
#' Search not allowed species. This species are spp (and others)
#' species, which are stored in not_allowed_species_measured.csv file.
#' The file must be previously loaded to run this function.
#' @return dataframe with warnings.
species_not_allowed <- function(){
  
  OAB_catches_clean <- OAB_catches[, c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP")]
  
  not_allowed_species <- not_allowed_species_measured[, "COD_ESP"]
  
  errors <- OAB_catches_clean[OAB_catches_clean[["COD_ESP"]] %in% not_allowed_species, ]
  
  errors <- errors[, c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP")]
  
  if (nrow(errors)>0){
    errors <- addTypeOfError(errors, "WARNING: not allowed species (usually 'spp' species.)")
    return(errors) 
  }
  
}


#' check code:2060
#' Search records of not allowed taxons with lengths.
#' The not allowed species are spp, order or family taxons.
#' @return dataframe with errors.
lenghts_not_allowed_taxons <- function(){
  
  OAB_lengths_clean <- OAB_lengths[, c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "TALLA", "EJEM_MEDIDOS")]
  
  OAB_lengths_clean <- OAB_lengths_clean[OAB_lengths_clean[["EJEM_MEDIDOS"]] > 0, ]
  
  #create a dataframe with species not allowed
  # by sufixes
  to_check <- grep("(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))", OAB_lengths_clean$ESP)
  # . = any single character
  # + = one of more of previous
  # | = or
  
  errors <- OAB_lengths_clean[to_check,]
  
  # sum all the lengths by trip, species and haul
  errors <- errors %>%
    group_by(COD_MAREA, COD_LANCE, COD_ESP, ESP) %>%
    summarise(sum_ejem_medidos = sum(TALLA, na.rm = T))

  errors <- addTypeOfError(errors, "ERROR: not allowed species with lengths measured.")
  
  if (nrow(errors)>0){
    return(as.data.frame(errors)) 
  }

}


#' check code: 2061
#' Search not allowed species with retained or discarded catches but
#' number of specimens as zero.
#' The not allowed species are spp, order or family taxons.
#' @return dataframe with errors.
doubtfull_sp_number_specimens <- function(){
  
  OAB_catches_clean <- OAB_catches[, c("COD_MAREA", "COD_LANCE", "ESP", "COD_ESP", "P_RET", "P_DESCAR", "EJEM_RET", "EJEM_DESCAR")]

  # create a dataframe with species not allowed
  # by sufixes
  to_check_genus <- grep("(.+(formes$))|(.+(spp$))|(.+(sp$))|(.+(dae$))", OAB_catches_clean$ESP)
  # . = any single character
  # + = one of more of previous
  # | = or
  
  # get specimens of not allowed species
  specimens_n_a <- OAB_catches_clean[to_check_genus,]

  errors <- specimens_n_a[which( (specimens_n_a[["P_RET"]] > 0 & specimens_n_a[["EJEM_RET"]] == 0 ) |
                                 (specimens_n_a[["P_DESCAR"]] > 0 & specimens_n_a[["EJEM_DESCAR"]] == 0) ), ]

  if (nrow(errors) > 0){
    
    errors[["TIPO_ERROR"]] <- ""
    errors[which(errors[["P_RET"]] > 0 & errors[["EJEM_RET"]] == 0), "TIPO_ERROR"] <- "WARNING: This species has retained weight but doesn't have number of retained specimens."
    errors[which(errors[["P_DESCAR"]] > 0 & errors[["EJEM_DESCAR"]] == 0), "TIPO_ERROR"] <- "WARNING: This species has discarded weight but doesn't have number of discarded specimens."
    
    return(errors)
  }  
  
}


#' check code: 2063
#' PREVIOUS CHECKING DELETED



#' check code: 2064
#' Cephalopods counted.
#' Cephalopods must be counted in DESIXA, DESSUR and DESNOR samples.
#' @return dataframe with errors.
cephalopods_counted <- function(){
  
  # filter by DESIXA, DESSUR and DESNOR
  OAB_catches_filtered <- OAB_catches[c(grep("^DESIXA[0-9]+", OAB_catches[["COD_MAREA"]]),
  grep("^DESNOR.+", OAB_catches[["COD_MAREA"]]),
  grep("^DESSUR.+", OAB_catches[["COD_MAREA"]]) ), ]
  
  # find cephalopods
  are_cephalopods <- cephalopods[["COD_ESP"]]
  
  # detect errors
  errors <- OAB_catches_filtered[which(OAB_catches_filtered[["COD_ESP"]] %in% are_cephalopods), ]
  
  errors_ret <- errors[which(errors[["P_RET"]]>0 & errors[["EJEM_RET"]]==0), 
                       c("COD_MAREA", "COD_LANCE", "ESP", "COD_ESP", "P_RET", "EJEM_RET")]
  errors_ret <- addTypeOfError(errors_ret, "WARNING: retained weight of cephalopod without number of specimens (counted, not measured)")
  
  if(!("TIPO_ERROR" %in% colnames(errors_ret))){
    errors_ret <- setNames(data.frame(matrix(ncol = 7, nrow = 0)),
                           c("COD_MAREA", "COD_LANCE", "ESP", "COD_ESP", "P_RET", "EJEM_RET", "TIPO_ERROR"))
  }
  
  errors_dis <- errors[which(errors[["P_DESCAR"]]>0 & errors[["EJEM_DESCAR"]]==0), 
                       c("COD_MAREA", "COD_LANCE", "ESP", "COD_ESP", "P_DESCAR", "EJEM_DESCAR")]
  errors_dis <- addTypeOfError(errors_dis, "WARNING: discarded weight of cephalopod without number of specimens (counted, not measured)")

  if(!("TIPO_ERROR" %in% colnames(errors_dis))){
    errors_dis <- setNames(data.frame(matrix(ncol = 7, nrow = 0)),
             c("COD_MAREA", "COD_LANCE", "ESP", "COD_ESP", "P_DESCAR", "EJEM_DESCAR", "TIPO_ERROR"))
  }
  
  errors <- merge(errors_ret, errors_dis, by = c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "TIPO_ERROR"), all.x = TRUE, all.y = TRUE)

  if (nrow(errors) > 0){
    return(errors)
  }
  
}

#' check code: 2063
#' Check if the haul is checked.
#' @return dataframe with errors.
haul_is_checked <- function(df){
  
  errors <- df[df["CHEQUEADO"]==FALSE,]
  errors <- errors[, c("COD_MAREA", "COD_LANCE", "CHEQUEADO")]
  
  if (nrow(errors) > 0){
    errors <- addTypeOfError(errors, "ERROR: haul not checked.")
    return(errors)
  }
  
}

#' check code: 2065
#' Check if the trip is checked.
#' @return dataframe with errors.
trip_is_checked <- function(df){

  errors <- df[which(df["CHEQUEADO"]==FALSE | is.na(df["CHEQUEADO"])),]
  
  
  if (nrow(errors) > 0){
    errors <- errors[, c("COD_MAREA", "CHEQUEADO")]
    errors <- addTypeOfError(errors, "ERROR: trip not checked.")
    return(errors)
  }
  
}


#' check code: 2066
#' Check if the final date of the trip is one day after the hauling of the
#' last haul. In case of BACA_AC, this period can be 3 days.
#' @return dataframe with errors.
final_date_one_day_before_hauling <- function(){
  
  #get the maximum days between final trip date and final date of last haul of the trip.
  max_days <- duracion_mareas_OAB[, c("ESTRATO_RIM", "max_days_haul_trip")]
  
  # get the final date of every trips by rim stratum
  final_date_trip <- OAB_trips[, c("COD_MAREA", "FECHA_FIN", "ESTRATO_RIM")]
  final_date_trip <- unique(final_date_trip)
  final_date_trip$FECHA_FIN <- as.Date(final_date_trip$FECHA_FIN, format = "%d/%m/%Y")
  final_date_trip <- by(final_date_trip, final_date_trip$COD_MAREA, function(x){
    x[which.max(x$FECHA_FIN), ]
  })
  final_date_trip <- do.call(rbind, final_date_trip)

  # get the final date of hauls by rim stratum
  final_date_haul <- OAB_hauls[, c("COD_MAREA", "FECHA_VIR", "ESTRATO_RIM")]
  final_date_haul <- unique(final_date_haul)
  final_date_haul$FECHA_VIR <- as.Date(final_date_haul$FECHA_VIR, format = "%d/%m/%Y")
  final_date_haul <- by(final_date_haul, final_date_haul$COD_MAREA, function(x){
    x[which.max(x$FECHA_VIR), ]
  })
  final_date_haul <- do.call(rbind, final_date_haul)

  # merge trip and hauls dataframe
  errors <- merge(final_date_trip,
                 final_date_haul,
                 by=c("COD_MAREA", "ESTRATO_RIM"),
                 all = T)

  calculate_error <- function(x){
    if(!is.na(x[["FECHA_FIN"]]) && !is.na(x[["FECHA_VIR"]]) ){
      max_dur <- max_days[max_days[["ESTRATO_RIM"]]==x[["ESTRATO_RIM"]], "max_days_haul_trip"]
      trip_date <- as.Date(x[["FECHA_FIN"]], format = "%Y-%m-%d")
      haul_date <- as.Date(x[["FECHA_VIR"]], format = "%Y-%m-%d")
      
      if(length(max_dur)==0){
        error_content <- "ERROR: the ESTRATO_RIM is not in the duracion_mareas_OAB master. Check it and check manually the initial date trip and final date trip."
        return(error_content)
      }
      if(trip_date - haul_date > max_dur){
        error_content <- paste("ERROR: the end date of the trip is more than",
        max_dur, "days later than last haul date.", sep=" ")
        return(error_content)
      }
    }
  }

  errors[["TIPO_ERROR"]] <- apply(errors, 1, calculate_error)
  errors <- errors[which(errors$TIPO_ERROR!="NULL"), ]
  
  if (nrow(errors) > 0){
    return(errors)
  }
  
}

#' Check code: 2067
#' Check if there are any trip with various hauls with same code.
#' This check is required because sometimes delete a haul in SIRENO doesn't work
#' and is possible add a new haul with the same code.
trip_multiple_haul_same_code <- function(){
  
  errors <- OAB_hauls[, c("COD_MAREA", "COD_LANCE")]
  
  errors <- aggregate(errors$COD_LANCE, by=list(errors$COD_MAREA, errors$COD_LANCE), length)
  
  colnames(errors) <- c("COD_MAREA", "COD_LANCE", "num_repeated_hauls")
  
  errors <- errors[errors$num_repeated_hauls > 1,]
  
  errors <- addTypeOfError(errors, "ERROR: this trip have multiple hauls with the same haul code (COD_LANCE).")
  
  if (nrow(errors) > 0){
    return(errors)
  }
  
}


#' check code: 2068
#' Total weight discarded empty in measured hauls.
#' When a haul is measured, the total weight discarded can't be empty (must be
#' 0 o greater than 0).
#' @return dataframe with errors
empty_discarded_weights_in_measured_haul <- function(){
  
  errors <- OAB_hauls[which(OAB_hauls[["MUESTREADO"]] == TRUE &
                           is.na(OAB_hauls[["P_TOT_DESCAR"]])),]
  
  errors <- errors[, c("COD_MAREA", "COD_LANCE", "MUESTREADO", "P_TOT_DESCAR")]
  
  errors <- addTypeOfError(errors, "ERROR: Total weight can't be empty when the haul is measured.")
  
  if (nrow(errors) > 0){
    return(errors)
  } 
  
}


#' check code: 2069
#' Mandatory sexed species not sexed. 
#' Only elasmobranchii and Nephropps Norvergicus must be sexed.
#' @return dataframe with errors
species_not_sexed <- function(){
  
  # Get the elasmobranchii.csv file, which contains the elasmobranchii species.
  elasmobranchii <- importCsvSAPMUE("elasmobranchii.csv")
  
  # Create not sex species dataset:
  not_sex <- rbind(elasmobranchii, c("20194", "Nephrops norvegicus"))
  
  # Get errors
  clean_lengths <- OAB_lengths[, c("COD_MAREA", "COD_LANCE", "TIPO_CAPTURA", "COD_ESP", "ESP", "SEXO")]
  clean_lengths <- unique(clean_lengths)
  errors <- clean_lengths[which(clean_lengths[["COD_ESP"]] %in% not_sex[["COD_ESP"]] & clean_lengths[["SEXO"]] %in% c("U")), ]
  
  errors <- addTypeOfError(errors, "ERROR: this species must be sexed.")
  
  if (nrow(errors) > 0){
    return(errors)
  } 
  
}

#' Check code: ????
#' Check if the variables "COD_PUERTO", "COD_ARTE", "COD_ORIGEN", "ESTRATO_RIM",
#' "METIER_DCF" and "CALADERO_DCF" are coherent with OAB prescriptions.
#' @return dataframe with errors, if there are any.
coherenceRimMt2PrescriptionsPost <- function(df){
  
  vars_to_check <- colnames(prescripciones_oab_2021_coherencia)
  
  vars_to_check_in_df <- colnames(df)[(colnames(df) %in% vars_to_check)]
  errors_oab_trips <- df[, c("COD_MAREA", vars_to_check_in_df)]

  # OAB_trips[5:7, "ESTRATO_RIM"] <- "VOLANTA_CN"
  # OAB_trips[15:27, "COD_ORIGEN"] <- "008"
  errors_oab_trips <- unique(errors_oab_trips)
  prescripciones_oab_2021_coherencia_mod <- prescripciones_oab_2021_coherencia
  prescripciones_oab_2021_coherencia_mod$VALID <- TRUE
  errors_oab_trips <- merge(errors_oab_trips,
                            prescripciones_oab_2021_coherencia_mod,
                            by=vars_to_check_in_df,
                            all.x = TRUE)
  if(nrow(errors_oab_trips)>0){
    errors_oab_trips <- errors_oab_trips[is.na(errors_oab_trips[["VALID"]]), c(vars_to_check_in_df)]
    errors_oab_trips <- addTypeOfError(errors_oab_trips, "This combination of ", paste(vars_to_check_in_df, collapse = ", "), " are not in 2021 OAB prescriptions.")
    return (errors_oab_trips)
  }
  
}

# coherenceRimMt2PrescriptionsPost(OAB_accidentals)
# 
# vars_to_check <- c("COD_PUERTO_BASE", "COD_PUERTO_DESCARGA", "ESTRATO_RIM")
# 
# port_cols <- vars_to_check[grepl("((^)|(.+))COD_PUERTO(($)|(.+))", vars_to_check)]
# 
# pres <- prescripciones_oab_2021_coherencia
# pres[port_cols] <- pres[["REGEX_COD_PUERTO"]]
# 
# errors_port <- OAB_trips[, c("COD_MAREA", port_cols)]
# 
# errors_port <- unique(errors_port)
# 
# pres$VALID <- TRUE
# errors_oab_trips <- merge(errors_port,
#                           pres,
#                           by=port_cols,
#                           all.x = TRUE)
# # if(nrow(errors_port)>0){
#   errors_port <- errors_port[is.na(errors_port[["VALID"]]), c(vars_to_check_in_df)]
#   errors_port <- addTypeOfError(errors_port, "This combination of ", paste(port_cols, collapse = ", "), " are not in 2021 OAB prescriptions.")
#   # return (errors_port)
# # }


#' check code: 2072
#' Discard Sampled Checkbox must be equal to Haul Sampled Checkbox
#' @return dataframe with errors
haul_sampled_discard_sampled <- function(){
  
  errors <- OAB_hauls[OAB_hauls$MUESTREADO != OAB_hauls$DESCARTE_MUESTREADO,
                      c("COD_MAREA", "COD_LANCE")]
  if (nrow(errors) > 0){
    errors <- addTypeOfError(errors, "ERROR: Haul with Discard Sampled variable different to Sampled variable.")
    return(errors)
  } 
  
}

#' check code: 2073
#' Days at sea must be equal or greater than fishing days.
#' @return dataframe with errors
days_at_sea_fishing_days <- function() {
  errors <- OAB_trips[OAB_trips[["DIAS_PESCA"]] > OAB_trips[["DIAS_MAR"]],
                   c("COD_MAREA", "DIAS_MAR", "DIAS_PESCA")]
  if (nrow(errors) > 0){
    errors <- addTypeOfError(errors, "ERROR: Days at see must be greater or equal than fishing days. Both fields are calculated, so the problem must be in date and times of the trips or hauls.")
    return(errors)
  } 
  
}


#' check code: 2074
#' Detect when only one specimen has been measured but the retained weight is
#' greater than the retained sampled weight.
#' @return dataframe with errors
retained_weight_one_specimen_measured <- function() {
  errors <- OAB_catches[OAB_catches[["P_RET"]] > OAB_catches[["P_MUE_RET"]] & OAB_catches[["EJEM_RET"]]==1,
                      c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "P_RET", "P_MUE_RET", "EJEM_RET")]
  if (nrow(errors) > 0){
    errors <- addTypeOfError(errors, "TESTING WARNING: One specimen has been measured but the retained weight is
#' greather than the retained sampled weight.")
    return(errors)
  } 
  
}

#' check code: 2075
#' Since 1/1/2022 the field PINGER can't be NA, must be filled with true or false
#' @return dataframe with errors
pinger_required <- function (){
  errors <- OAB_hauls[!(OAB_hauls[["PINGER"]] %in% c(TRUE, FALSE)),
                      c("COD_MAREA", "COD_LANCE", "PINGER")]
  
  if (nrow(errors) > 0){
    errors <- addTypeOfError(errors, "ERROR: Since 1/1/2022 the field PINGER can't be NA, must be filled with true or false.")
    return(errors)
  }
  
}

