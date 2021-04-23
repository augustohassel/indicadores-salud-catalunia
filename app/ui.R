
# UI ----------------------------------------------------------------------

dashboardPage(
  skin = "black", 
  title = "Salut",
  md = FALSE,
  
  # Cabecera ----------------------------------------------------------------
  
  header = dashboardHeader( 
    title = tagList(icon(name = "book-medical"), tags$strong("Salut")),
    disable =  FALSE,
    controlbarIcon = icon("gears")
  ),
  
  # Barra Izquierda ---------------------------------------------------------
  
  sidebar = dashboardSidebar(
    includeCSS("www/styles.css"),
    useShinyjs(),
    use_googlefont("Quicksand"),
    
    sidebarMenu(
      menuItem(text = "Tendencia", icon = icon("chart-line"), tabName = "tendencia"),
      menuItem(text = "Territorio", icon = icon("map"), tabName = "territorio"),
      menuItem(text = "Centros", icon = icon("hospital"), tabName = "centros")
    )
  ), 
  
  # Cuerpo ------------------------------------------------------------------
  
  body = dashboardBody(
    
    # CONTENIDO TABS--------------------
    tabItems(
      
      # 1 - Tendencia --------------------
      tabItem(tabName = "tendencia",
              fluidRow(
                column(
                  width = 2, 
                  box(
                    title = "Filtros", 
                    status = "maroon", 
                    width = NULL, 
                    icon = icon("filter"), 
                    solidHeader = FALSE, 
                    radioGroupButtons(
                      inputId = "tendencia_filter_tipus_centre",
                      label = "Tipo de centro",
                      choices = list(
                        "Adults" = "Adults",
                        "Infantil" = "Infantil"
                      ),
                      checkIcon = list(
                        yes = tags$i(
                          class = "fa fa-check-square", 
                          style = "color: steelblue"
                        ),
                        no = tags$i(
                          class = "fa fa-square-o", 
                          style = "color: steelblue"
                        )
                      ), 
                      selected = "Adults", 
                      justified = TRUE
                    ),
                    pickerInput(
                      inputId = "tendencia_filter_rs_centre", 
                      label = "Region Sanitaria", 
                      choices = options_tendencia_filter_rs_centre, 
                      selected = options_tendencia_filter_rs_centre, 
                      multiple = TRUE, 
                      options = list(
                        `actions-box` = TRUE, 
                        `live-search` = TRUE, 
                        `selected-text-format` = "count > 3",
                        title = "Selecciona la región de interés"
                        )
                      ),
                    pickerInput(
                      inputId = "tendencia_filter_centre", 
                      label = "Centro", 
                      choices = options_tendencia_filter_centre, 
                      selected = options_tendencia_filter_centre, 
                      multiple = TRUE, 
                      options = list(
                        `actions-box` = TRUE, 
                        `live-search` = TRUE, 
                        `selected-text-format` = "count > 3",
                        title = "Selecciona el centro de interés"
                        )
                      ),
                    checkboxGroupButtons(
                      inputId = "tendencia_filter_sexe",
                      label = "Sexo",
                      choices = list(
                        "Hombres" = 1,
                        "Mujeres" = 0
                      ),
                      checkIcon = list(
                        yes = tags$i(
                          class = "fa fa-check-square", 
                          style = "color: steelblue"
                        ),
                        no = tags$i(
                          class = "fa fa-square-o", 
                          style = "color: steelblue"
                        )
                      ), 
                      selected = c(0, 1),
                      justified = TRUE
                    ),
                    pickerInput(
                      inputId = "tendencia_filter_grup_edat", 
                      label = "Grupo de edad", 
                      choices = options_tendencia_filter_grup_edat, 
                      selected = options_tendencia_filter_grup_edat, 
                      multiple = TRUE, 
                      options = list(
                        `actions-box` = TRUE, 
                        `live-search` = TRUE, 
                        `selected-text-format` = "count > 3",
                        title = "Selecciona el grupo de interés"
                        )
                      ),
                    checkboxGroupButtons(
                      inputId = "tendencia_filter_nivell_socioeconomic_baix",
                      label = "Nivel Socioeconómico",
                      choices = list(
                        "No Bajo" = 0,
                        "Bajo" = 1
                      ),
                      checkIcon = list(
                        yes = tags$i(
                          class = "fa fa-check-square", 
                          style = "color: steelblue"
                        ),
                        no = tags$i(
                          class = "fa fa-square-o", 
                          style = "color: steelblue"
                        )
                      ), 
                      selected = c(0, 1),
                      justified = TRUE
                      ),
                    pickerInput(
                      inputId = "tendencia_filter_diagnostico", 
                      label = "Diagnóstico", 
                      choices = options_tendencia_filter_diagnostico, 
                      selected = options_tendencia_filter_diagnostico, 
                      multiple = TRUE, 
                      options = list(
                        `actions-box` = TRUE, 
                        `live-search` = TRUE, 
                        `selected-text-format` = "count > 1",
                        title = "Selecciona el diagnóstico de interés"
                        )
                      ),
                    checkboxGroupButtons(
                      inputId = "tendencia_filter_nou_pacient",
                      label = "Paciente tipo",
                      choices = list(
                        "Nuevo" = 1,
                        "No Nuevo" = 0
                      ),
                      checkIcon = list(
                        yes = tags$i(
                          class = "fa fa-check-square", 
                          style = "color: steelblue"
                        ),
                        no = tags$i(
                          class = "fa fa-square-o", 
                          style = "color: steelblue"
                        )
                      ), 
                      selected = c(0, 1),
                      justified = TRUE
                      )
                    )
                  
                  ),
                column(
                  width = 5, 
                  tabBox(
                    title = "Persones ateses", 
                    width = NULL, 
                    tabPanel(
                      title = "Evolución Anual",
                      plotlyOutput(outputId = "tendencia_plot_1", inline = TRUE)
                      ),
                    tabPanel(
                      title = "Base 2016",
                      plotlyOutput(outputId = "tendencia_plot_2", inline = TRUE)
                      )
                    )
                  ),
                column(
                  width = 5,
                  tabBox(
                    title = "Visites per persona atesa", 
                    width = NULL, 
                    tabPanel(
                      title = "Jitter Plot",
                      plotlyOutput(outputId = "tendencia_plot_3", inline = TRUE)
                    ),
                    tabPanel(
                      title = "BoxPlot"
                      )
                    )
                  )
                )
      ) # cierra el tab
    ) # cierre tab items
  )# cierra el body del dashboard
)# cierra el dashboard page


