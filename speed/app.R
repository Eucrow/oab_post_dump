library(shiny)
library(ggplot2)
library(ggiraph)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Let's explore speeds, cuate!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    position = "right",
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      textInput("result_marea", label = "ID_MAREA selected")
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # plotOutput(outputId = "distPlot")
      # Output: Histogram ----
      
      ggiraphOutput(outputId = "speedPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  get_id_marea <- reactive({
    if (is.null(input$speedPlot_selected)){
      character(0)
    } else {
      input$speedPlot_selected
    }
  })

  output$speedPlot <- renderggiraph({
    
    
    # I THINK THIS DOES NOT WORK WITH BOXPLOT
    hauls_speed <- OAB_hauls %>%
      select(ID_MAREA, ESTRATO_RIM, ESP_OBJ, VELOCIDAD) %>%
      filter(ESTRATO_RIM %in% c("BACA_CN", "BACA_GC", "JURELERA_CN", "PAREJA_CN", "RAPANTER_AC"))

    hauls_speed$str <- as.character(hauls_speed$ESTRATO_RIM)
    hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ != "CABALLA", "str"] <- "PAREJA_CN.RESTO"
    hauls_speed[hauls_speed$ESTRATO_RIM=="PAREJA_CN" & hauls_speed$ESP_OBJ == "CABALLA", "str"] <- "PAREJA_CN.CABALLA"

    p <- ggplot(hauls_speed, aes(str, VELOCIDAD))+
      geom_boxplot_interactive()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    ggiraph(code = print(p), selection_type = "single")
    
    
    # WITH THIS EXAMPLES DOESN'T WORK
    # p <- ggplot(mpg,
    #             aes(x = class, y = hwy, tooltip = class)) +
    #   geom_boxplot_interactive()
    # 
    # ggiraph(code = print(p), hover_css = "fill:red;r:10pt;",  selection_type = "single")
    
    # BUT WITH A NORMAL geom_point_interactive IT DOES:
    # dataset <- mtcars
    # dataset$carname <- row.names(dataset)
    # gg_point_1 <- ggplot(dataset, aes(x = disp, y = qsec, tooltip = carname, data_id = carname, color= wt) ) + 
    #   geom_point_interactive(size=3)
    # 
    # ggiraph(code = {print(gg_point_1)}, tooltip_offx = 20, tooltip_offy = -10, selection_type = "single" )
    
    
  })
  
  #observe reactive values
  observe( {
    value <- get_id_marea()
    updateTextInput(session = session, "result_marea", value = get_id_marea() )
  })
  
}


shinyApp(ui = ui, server = server)