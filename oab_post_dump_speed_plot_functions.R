#' Export to pdf the graphic returned by a function.
#' @param filename name of the file to export (without extension)
#' @param plot plot to export
print_pdf_graphic <- function(filename, plot){

  filename <- paste0(PATH_ERRORS, "/", filename, ".pdf")

  pdf(filename)

  print(plot)

  dev.off()
}



#' Speed interactive boxplot by ESTRATO_RIM.
#' Sow boxplot graphic with speed to check the speed by ESTRATRO_RIM.
speed_boxplot_interactive <- function() {

  hauls_speed <- OAB_hauls %>%
    select(COD_MAREA, COD_LANCE, ESTRATO_RIM, ESP_OBJ, VELOCIDAD) %>%
    filter(ESTRATO_RIM %in% c("BACA_CN", "BACA_GC", "JURELERA_CN", "PAREJA_CN", "RAPANTER_AC"))

  hauls_speed$str <- as.character(hauls_speed$ESTRATO_RIM)
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ != "CABALLA", "str"] <- "PAREJA_CN.RESTO"
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ == "CABALLA", "str"] <- "PAREJA_CN.CABALLA"

  hauls_speed %>%
    group_by(str) %>%
    mutate(outlier = ifelse(is_outlier(VELOCIDAD), VELOCIDAD, as.numeric(NA)))

  speed_plot <- plotly::plot_ly(x = ~hauls_speed$str,
                                y = ~hauls_speed$VELOCIDAD,
                                type = "box",
                                text = ~paste(hauls_speed$COD_MAREA, " -  haul: ",
                                              hauls_speed$COD_LANCE, " - speed: ",
                                              hauls_speed$VELOCIDAD, " knots")) %>% 
                plotly::layout(
                  xaxis = list(title = "RIM_STRATUM"),
                  yaxis = list(title = "speed (knots)")
                )
  return(speed_plot)

}

#' Speed boxplot by ESTRATO_RIM.
#' Sow boxplot graphic with speed to check the speed by ESTRATRO_RIM.
speed_boxplot <- function() {

  hauls_speed <- OAB_hauls %>%
    select(COD_MAREA, ESTRATO_RIM, ESP_OBJ, VELOCIDAD) %>%
    filter(ESTRATO_RIM %in% c("BACA_CN", "BACA_GC", "JURELERA_CN", "PAREJA_CN", "RAPANTER_AC"))

  hauls_speed$str <- as.character(hauls_speed$ESTRATO_RIM)
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ != "CABALLA", "str"] <- "PAREJA_CN.RESTO"
  hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ == "CABALLA", "str"] <- "PAREJA_CN.CABALLA"

  hauls_speed %>%
    group_by(str)%>%
    mutate(outlier = ifelse(is_outlier(VELOCIDAD), VELOCIDAD, as.numeric(NA)))%>%
      ggplot(., aes(str, VELOCIDAD))+
      geom_boxplot()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      geom_text(aes(label = outlier), na.rm=T, hjust=1.1)
}
