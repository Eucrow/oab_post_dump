#### Check discards from SIRENO
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
####
#### Functions file 
####
#### Convention style guide: http://r-pkgs.had.co.nz/style.html

#' Check code: 2001
#' Discarded weight less than sampled discarded weight.
#' @return dataframe with errors.
discarded_weight_less_than_sampled_discarded_weight <- function(){
  
  OAB_catches[["P_DESCAR"]] <- round(OAB_catches[["P_DESCAR"]], 2)
  OAB_catches[["P_MUE_DESCAR"]] <- round(OAB_catches[["P_MUE_DESCAR"]], 2)
  
  err <- OAB_catches %>%
    select(COD_MAREA, COD_LANCE, COD_ESP, CATEGORIA, P_DESCAR, P_MUE_DESCAR) %>%
    filter(P_DESCAR < P_MUE_DESCAR) %>%
    addTypeOfError("ERROR: peso descartado menor que peso descartado muestreado.")
  
  return(err)
  
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
empty_fields_in_variables <- function(df, type_file = c("OAB_TRIPS", "OAB_HAULS", "OAB_CATCHES", "OAB_LENGTHS")){
  
  # Detect if the variable type_file is correct:
  match.arg(type_file)
  
  
  mandatory_field <- paste0(type_file, "_MANDATORY")
  
  mandatory <- formato_variables[which(!is.na(formato_variables[type_file])
                                       & formato_variables[mandatory_field] == TRUE), c("name_variable")]
  df_mandatory <- df[,mandatory]
  
  err <- check_empty_values_in_variables(df_mandatory, mandatory)
  
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

#' Check code: 2004
#' Check if the YEAR variable of the dataframe match with year to study.
#' @param df daraframe to check the field YEAR. The dataframe must have a field
#' called YEAR.
#' @return df with errors.
#' @export
field_year <- function(df) {
  
  errors <- df %>%
    select(COD_MAREA, YEAR) %>%
    filter(YEAR != YEAR_DISCARD) %>%
    unique()%>%
    addTypeOfError("ERROR: El campo YEAR no coincide con el año a comprobar: ",YEAR_DISCARD)
  
  return(errors)
  
}

#' Check code: 2005
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
     variable != "COD_PUERTO" &&
     variable != "COD_PUERTO_BASE" &&
     variable != "COD_PUERTO_LLEGADA" &&
     variable != "COD_PUERTO_DESCARGA" &&
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
  name_data_set <- tolower(variable_formatted)
  # name_data_set <- paste0(name_data_set, "_OAB")
  
  # *********** CASE OF PUERTO_BASE, PUERTO_LLEGADA OR PUERTO_DESCARGA ***********    
  # it's required  join via the variable COD_PUERTO, so a new variable is required:
  if(variable == "COD_PUERTO_LLEGADA" ||
     # variable == "COD_PUERTO_BASE" ||
     variable == "COD_PUERTO_DESCARGA") {
    variable_to_change <- "COD_PUERTO"
  }
  # ****************************************************************************** 
  
  dataframe_variable <- get(name_data_set)
  
  # *********** CASE OF PUERTO_BASE, PUERTO_LLEGADA OR PUERTO_DESCARGA ***********  
  if (exists("variable_to_change")){
    names(dataframe_variable)[names(dataframe_variable) == variable_to_change] <- variable
  }
  
  #search the errors in variable
  # errors <- anti_join(df, get(name_data_set), by = variable)
  
  # ******************************************************************************
  
  # filter dataset with only OAB information
  data_set <- get(name_data_set)
  data_set <- data_set[data_set[["OAB"]] == TRUE, -which(names(data_set) %in% "RIM")]
  
  #prepare to return
  if (exists("variable_to_change")){
    errors <- anti_join(df, data_set, by = setNames(nm=variable, variable_to_change))
    fields_to_filter <- c("COD_MAREA", variable, variable_puerto_original)
  } else {
    errors <- anti_join(df, data_set, by = setNames(nm=variable, variable))
    fields_to_filter <- c("COD_MAREA", variable, variable_formatted)
  }
  
  
  errors <- errors %>%
    select(one_of(fields_to_filter))%>%
    unique()
  
  
  text_type_of_error <- paste0("ERROR: ", name_data_set, " no concuerda con los maestros de SIRENO")
  errors <- addTypeOfError(errors, text_type_of_error)
  
  #return
  return(errors)
}

#' Check code: 2011
#' Check year in COD_MAREA.
#' @param df: df to check. Must be a df obtained by importOAB functions.
#' @return df with errors
#' @export
check_year_in_COD_MAREA <- function(df){
  
  COD_MAREA_split <- split_COD_MAREA(df)

  errors <- which(!(COD_MAREA_split[["year"]] %in% YEAR_DISCARD))

  errors <- df[errors,]
  
  # this line add a comment to the errors dataframe wich contain the value of the
  # df variable
  errors.name <- deparse(substitute(df))
  
  errors <- errors %>%
            select(COD_MAREA, YEAR)%>%
            addTypeOfError(paste("ERROR: el año del COD_MAREA en", errors.name, "no coincide con el año a comprobar"))
  
  return(errors)
}

#' Check code: 2012
#' Coherence between rim stratrum and gear variables.
#' @return dataframe with wrong coherence.
coherence_rim_stratum_gear <- function(df){
  
  try(variables_in_df(c("ESTRATO_RIM", "COD_ARTE"), df))
  
  BASE_FIELDS <- c("COD_MAREA", "COD_LANCE", "ESTRATO_RIM", "COD_ARTE", "ARTE")
  
  estratorim_arte_OAB <- estratorim_arte[estratorim_arte[["OAB"]] == TRUE,
                                         c("ESTRATO_RIM", "COD_ARTE", "OAB")]
  
  errors <- df %>%
    select(one_of(BASE_FIELDS)) %>%
    unique() %>%
    anti_join(y=estratorim_arte_OAB, by=c("ESTRATO_RIM", "COD_ARTE")) %>%
    addTypeOfError("ERROR: no concuerda el estrato_rim con el arte")
  
  return(errors)
  
}

#' Check code: 2013
#' Coherence between rim stratrum and origin variables.
#' @return dataframe with wrong coherence.
coherence_rim_stratum_origin <- function(df){
  
  try(variables_in_df(c("ESTRATO_RIM", "COD_ORIGEN"), df))
  
  BASE_FIELDS <- c("COD_MAREA", "COD_LANCE", "ESTRATO_RIM", "COD_ORIGEN", "ORIGEN")
  
  estratorim_origen_OAB <- estratorim_origen[estratorim_origen[["OAB"]] == TRUE,
                                             c("ESTRATO_RIM", "COD_ORIGEN", "OAB")]
  
  errors <- OAB_hauls %>%
    select(one_of(BASE_FIELDS)) %>%
    unique() %>%
    anti_join(y=estratorim_origen_OAB, by=c("ESTRATO_RIM", "COD_ORIGEN")) %>%
    addTypeOfError("ERROR: no concuerda el estrato_rim con el origen")
  
  estratorim_origen_OAB <- estratorim_origen[estratorim_origen[["OAB"]] == TRUE,
                                             c("ESTRATO_RIM", "COD_ORIGEN", "OAB")]
  
  return(errors)
  
}

#' Check code: 2014
#' Discarded sampled weigth less than zero (or NA) when there are any specimens
#' discarded.
discarded_sampled_weight_when_specimens_discarded <- function(df_catches){
  
  errors <- df_catches[which(
    df_catches[["EJEM_DESCAR"]] > 0 &
      (df_catches[["P_MUE_DESCAR"]] <= 0 | is.na(df_catches[["P_MUE_DESCAR"]]))
  ),
  c("COD_MAREA", "COD_ESP", "A3_ESP", "ESP", "EJEM_DESCAR", "P_MUE_DESCAR")]
  
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
priority_species_without_lengths <- function(){
  
  # get the list of priority species which must be measured
  species_to_measure <- especies_a_medir_OAB[,"COD_ESP"]
  
  # get Species With Catch which must be measured
  swc <- OAB_catches[which(OAB_catches[["COD_ESP"]]%in%species_to_measure),
                     c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "P_CAP")]
  swc <- unique(swc)
  
  # clean lengths
  lengths_clean <- OAB_lengths[, c("COD_MAREA", "COD_LANCE",
                                   "COD_ESP", "ESP", "EJEM_MEDIDOS")]
  
  lengths_clean <- aggregate.data.frame(lengths_clean[, c("EJEM_MEDIDOS")],
                                        by=list(lengths_clean$COD_MAREA,
                                                lengths_clean$COD_LANCE,
                                                lengths_clean$COD_ESP,
                                                lengths_clean$ESP),
                                        sum, na.rm=TRUE)
  
  colnames(lengths_clean) <- c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP", "EJEM_MEDIDOS")
  
  # create errors dataframe
  errors <- merge(swc, 
                  lengths_clean, 
                  by.x = c("COD_MAREA", "COD_LANCE", "COD_ESP", "ESP"),
                  all.x = TRUE)
  errors <- errors[which(errors$EJEM_MEDIDOS==0 |
                           is.na(errors$EJEM_MEDIDOS)),]
  
  errors <- addTypeOfError(errors, "ERROR: priority species which hasn't been measured")
  
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
    c("COD_MAREA", "COD_ESP", "A3_ESP", "ESP", "EJEM_RET", "P_MUE_RET")
    ]
  
  errors <- addTypeOfError(errors, "ERROR: there are specimens retained without retained sampled weight.")
  
}

#' Check code: 2021
#' Sampled discard weight less than subsample discard weight.
sampled_discard_less_subsample_discard <- function(df){
  
  # usually the PESO_SUB_MUE_TOT is NA, so it is neccesary detect it.
  errors <- df[
    which( !is.na(df$P_SUB_MUE_TOT) & df$P_SUB_MUE_TOT > df$P_MUE_DESCAR),
    c("COD_MAREA", "COD_LANCE", "COD_ESP", "A3_ESP", "ESP",
      "P_SUB_MUE_TOT", "P_MUE_DESCAR")
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
  
  errors <- errors[is.na(errors[["OK"]]),]
  
  errors <- addTypeOfError(errors, "ERROR: the target species is not coherent with metier ieo.")
  
}

#' Check code: 2024
#' Total discard weight less than subsample discard weight.
total_discard_less_subsample_discard <- function(df){
  
  errors <- df[
    df$P_DESCAR < df$P_MUE_DESCAR,
    c("COD_MAREA", "COD_ESP", "A3_ESP", "ESP", "P_DESCAR", "P_MUE_DESCAR")
    ]
  
  errors <- addTypeOfError(errors, "ERROR: total discard weight less than subsample discard weight.")
  
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
#' @return Dataframe with the erroneus data. A new a variable with the type
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
  
  err <- OAB_hauls %>%
    select(COD_MAREA, COD_LANCE, ESTRATO_RIM, FECHA_HORA_LAR, FECHA_HORA_VIR) %>%
    unique() %>%
    group_by(COD_MAREA, ESTRATO_RIM) %>%
    summarise(first_date = min(FECHA_HORA_LAR), last_date = max(FECHA_HORA_VIR)) %>%
    mutate(duration_trip = difftime(last_date, first_date, units = "hours")) %>%
    merge(, y = trip_hauls, all.x = T) %>%
    filter(duration_trip > DURACION_MAX) %>%
    mutate(duration_trip = round(duration_trip, 0)) %>%
    addTypeOfError("WARNING: Duración de la marea excesivamente larga. Habría que comprobar las fechas y horas de largado y de virado de los lances.")
  
  # TO DO: find the haul with the erroneus date and return it 
  # duration_p95 <- OAB_hauls %>%
  #   filter(COD_MAREA%in%err$COD_MAREA) %>%
  #   group_by(COD_MAREA, ESTRATO_RIM) %>%
  #   summarise('p95'=quantile(?????, probs=0.95))
  # 
  # error_final <- OAB_hauls %>%
  #   filter(COD_MAREA%in%err$COD_MAREA) %>%
  #   merge(., duration_p95, by=c("COD_MAREA", "ESTRATO_RIM"))
  
  return(err)
  
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
  
  errors <- sampled_hauls[which(sampled_hauls[["P_DESCAR"]] == ""),]
  
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
#' Check if the haul date is different to shooting date
#' @return Dataframe with errors.
haul_date_shooting_date <- function(){
  
  err <- OAB_hauls[, c("COD_MAREA", "COD_LANCE", "FECHA_LAR", "FECHA_LANCE")]
  err[["FECHA_LAR"]] <- as.POSIXct(err[["FECHA_LAR"]], format="%d/%m/%Y")
  
  err <- err[which (err[["FECHA_LAR"]] != err[["FECHA_LANCE"]]),]
  
  if (nrow(err) > 0){
    err <- addTypeOfError(err, "ERROR: the haul date is different to the shooting date.")
  }
  
}

#' Check code: 2055
#' Detect positive longitudes.
#' @param var var to check (from the OAB_hauls dataframe)
#' @return Dataframe with errors
positive_longitude <- function(var){
  
  err <- OAB_hauls[OAB_hauls[[var]] > 0, c("COD_MAREA", "COD_LANCE", var)]
  
  err <- addTypeOfError(err, "ERROR: the longitude is positive.")
  
  return(err)
  
}



