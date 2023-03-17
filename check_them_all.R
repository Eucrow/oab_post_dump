check_them_all <- function(){
  
  ERR <- list()
  
  # ALLTOGETHER
  # ERR$all_empty_fields_in_variable <- empty_fields_in_variables()
  # ppp <- check_empty_variables()
  # ppp_number <- as.data.frame(table(ppp$TIPO_ERROR))
  
  # TRIPS
  ERR$trips_origen <- check_variable_with_master(OAB_trips, "COD_ORIGEN")
  
  ERR$trips_estrato_rim <- check_variable_with_master(OAB_trips, "ESTRATO_RIM")
  
  # ERR$trips_puerto_llegada <- check_variable_with_master(OAB_trips, "COD_PUERTO_LLEGADA")
  
  # ERR$trips_puerto_descarga <- check_variable_with_master(OAB_trips, "COD_PUERTO_DESCARGA")
  
  ERR$trips_empty_fields <- empty_fields_in_variables(OAB_trips, "OAB_TRIPS")
  
  ERR$trips_field_year <- field_year(OAB_trips)
  
  ERR$trips_year_in_COD_MAREA <- check_year_in_COD_MAREA(OAB_trips)
  
  ERR$trips_year_in_initial_date <- check_year_in_date(OAB_trips, "FECHA_INI_MAREA", YEAR)
  
  ERR$trips_year_in_final_date <- check_year_in_date(OAB_trips, "FECHA_FIN_MAREA", YEAR)
  
  ERR$trips_final_date_in_COD_MAREA_GC <- trips_final_date_in_COD_MAREA_GC()
  
  ERR$trips_initial_date_before_final_date <- trips_initial_date_before_final_date()
  
  ERR$coherencia_estrato_rim_origin <- coherence_rim_stratum_origin(OAB_trips)
  
  ERR$trip_duration <- trip_duration()
  
  ERR$days_at_sea_fishing_days <- days_at_sea_fishing_days()
  
  ERR$ships_not_in_cfpo <- shipsNotInCFPO(OAB_trips, CFPO, SIRENO_FLEET)
  
  ERR$ships_not_registered <- shipsNotRegistered(OAB_trips, CFPO, SIRENO_FLEET)
  
  # THE NEXT CHECK MUST BE DONE ONLY IN ANNUAL:
  # ERR$checked_trips <- trip_is_checked(OAB_trips)
  
  
  # HAULS
  ERR$hauls_arte <- check_variable_with_master(OAB_hauls, "COD_ARTE")
  
  ERR$hauls_empty_fields <- empty_fields_in_variables(OAB_hauls, "OAB_HAULS")
  
  ERR$hauls_field_year <- field_year(OAB_hauls)
  
  ERR$hauls_year_in_COD_MAREA_hauls <- check_year_in_COD_MAREA(OAB_hauls)
  
  ERR$hauls_year_in_shotting_date <- check_year_in_date(OAB_hauls, "FECHA_LAR", YEAR)
  
  ERR$hauls_year_in_hauling_date <- check_year_in_date(OAB_hauls, "FECHA_VIR", YEAR)
  
  ERR$hauling_date_before_shooting_date <- hauling_date_before_shooting_date()
  
  ERR$hauls_coherence_estrato_rim_origin <- coherence_rim_stratum_origin(OAB_hauls)
  
  ERR$hauls_coherence_estrato_rim_gear <- coherence_rim_stratum_gear(OAB_hauls)
  
  ERR$hauls_sampled_with_catch_weights <- hauls_sampled_with_catch_weights()
  
  ERR$hauls_possible_speed_outliers <- get_speed_outliers()
  
  # Remove this check: maybe delete the master too?
  #ERR$hauls_target_sp_with_catch <- coherence_target_sp_with_catch()
  
  ERR$hauls_coherence_target_species_metier_ieo <- coherence_target_species_metier_ieo()
  
  ERR$hauls_length_cable_1000 <- length_cable_1000()
  
  ERR$hauls_overlapped <- hauls_overlapped()
  
  ERR$total_discarded_weight_zero_with_sampled_discard_weight <- total_discarded_weight_zero_with_sampled_discard_weight()
  
  ERR$hauls_duration <- check_hauls_duration()
  
  ERR$hauls_speed <-  check_hauls_speed()
  
  ERR$hauls_depth <-  check_hauls_depth()
  
  ERR$hauls_sampled_without_discard_weight <- hauls_sampled_without_discard_weight(OAB_hauls)
  
  ERR$hauls_unsampled_with_discard_weight <- hauls_unsampled_with_discard_weight(OAB_hauls)
  
  # the next one commented temporally:
  # ERR$coherence_origin_statistical_rectangle <- coherence_origin_statistical_rectangle()
  
  ERR$haul_date_shooting_date <- haul_date_shooting_date()
  
  ERR$positive_longitude_shooting <- positive_longitude("LON_LAR_CGS")
  
  ERR$positive_longitude_hauling <- positive_longitude("LON_VIR_CGS")
  
  # THE NEXT CHECK MUST BE DONE ONLY IN ANNUAL:
  # ERR$checked_hauls <- haul_is_checked(OAB_hauls)
  
  ERR$trip_multiple_haul_same_code <- trip_multiple_haul_same_code()
  
  ERR$zero_discarded_weights_in_not_measured_haul <- zero_discarded_weights_in_not_measured_haul()
  
  ERR$empty_discarded_weights_in_measured_haul <- empty_discarded_weights_in_measured_haul()
  
  ERR$haul_sampled_discard_sampled <- haul_sampled_discard_sampled()
  
  ERR$pinger_required <- pinger_required()
  
  
  # CATCHES
  ERR$catches_empty_fields <- empty_fields_in_variables(OAB_catches, "OAB_CATCHES")
  
  ERR$catches_discard_reason <- check_discard_reason_variable_with_master(OAB_catches)
  
  ERR$catches_field_year <- field_year(OAB_catches)
  
  ERR$catches_year_in_COD_MAREA <- check_year_in_COD_MAREA(OAB_catches)
  
  ERR$species_without_caught_neither_discarded_weight <- species_without_caught_neither_discarded_weight()
  
  ERR$reason_discard_field_filled <- reason_discard_field_filled(OAB_catches)
  
  ERR$retained_catch_less_than_sampled_retained_catch <- retained_catch_less_than_sampled_retained_catch()
  
  ERR$discarded_weight_less_than_sampled_discarded_weight <- discarded_weight_less_than_sampled_discarded_weight(OAB_catches)
  
  ERR$sampled_discard_less_subsample_discard <- sampled_discard_less_subsample_discard(OAB_catches)
  
  ERR$retained_sampled_weight_when_specimens_retained <- retained_sampled_weight_when_specimens_retained(OAB_catches)
  
  ERR$discarded_sampled_weight_when_specimens_discarded <- discarded_sampled_weight_when_specimens_discarded(OAB_catches)
  
  ERR$discarded_species_with_total_discarded_weight <- discarded_species_with_total_discarded_weight(OAB_lengths, OAB_hauls)
  
  ERR$reason_discard_field_filled <- reason_discard_field_filled(OAB_catches)
  
  ERR$doubtfull_sp_number_specimens <- doubtfull_sp_number_specimens()
  
  ERR$species_not_allowed <- species_not_allowed()
  
  ERR$cephalopods_counted <- cephalopods_counted()
  
  ERR$retained_weight_one_specimen_measured <- retained_weight_one_specimen_measured()
  
  ERR$species_without_retained_and_discarded_weight <- species_without_retained_and_discarded_weight(OAB_catches)
  
  # LENGTHS
  ERR$lengths_empty_fields <- empty_fields_in_variables(OAB_lengths, "OAB_LENGTHS")
  
  ERR$lengths_field_year <- field_year(OAB_lengths)
  
  ERR$lengths_year_in_COD_MAREA <- check_year_in_COD_MAREA(OAB_lengths)
  
  ERR$priority_species_without_lengths <- priority_species_without_lengths()
  
  ERR$with_historical_size_range <- check_species_in_size_range_historical()
  
  ERR$size_range <- check_size_range()
  
  ERR$lenghts_not_allowed_taxons <- lenghts_not_allowed_taxons()
  
  ERR$species_not_sexed <- species_not_sexed()
  
  ERR$discarded_retained_weigth_zero<-discarded_retained_weigth_zero(OAB_lengths)
  
  # LITTER
  ERR$litter_sample <- litter_sample()
  
  # MIXED
  ERR$errors_date_hauls_in_date_interval_trips <- date_hauls_in_date_interval_trips(OAB_trips, OAB_hauls)
  
  ERR$final_date_one_day_before_hauling <- final_date_one_day_before_hauling()
  
  return(ERR)
}
