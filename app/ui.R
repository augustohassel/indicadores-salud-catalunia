
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
      
      # 1. Tendencia --------------------
      tabItem(tabName = "tendencia",
              fluidRow(
                column(
                  width = 2, 
                  box(
                    title = "Filtros", 
                    status = "info", 
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
                      options = pickerOptions(
                        actionsBox = TRUE,
                        liveSearch = TRUE,
                        selectedTextFormat = "count > 3",
                        title = "Selecciona la región sanitaria de interés" 
                      )
                      ),
                    pickerInput(
                      inputId = "tendencia_filter_centre", 
                      label = "Centro", 
                      choices = options_tendencia_filter_centre, 
                      selected = options_tendencia_filter_centre, 
                      multiple = TRUE, 
                      options = pickerOptions(
                        actionsBox = TRUE,
                        liveSearch = TRUE,
                        selectedTextFormat = "count > 3",
                        title = "Selecciona el centro de interés" 
                      )
                      ),
                    checkboxGroupButtons(
                      inputId = "tendencia_filter_sexe",
                      label = "Sexo",
                      choices = list(
                        "Hombres" = 1L,
                        "Mujeres" = 0L
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
                      selected = c(0L, 1L),
                      justified = TRUE
                    ),
                    pickerInput(
                      inputId = "tendencia_filter_grup_edat", 
                      label = "Grupo de edad", 
                      choices = options_tendencia_filter_grup_edat, 
                      selected = options_tendencia_filter_grup_edat, 
                      multiple = TRUE, 
                      options = pickerOptions(
                        actionsBox = TRUE,
                        liveSearch = TRUE,
                        selectedTextFormat = "count > 3",
                        title = "Selecciona la edad de interés" 
                      )
                      ),
                    checkboxGroupButtons(
                      inputId = "tendencia_filter_nivell_socioeconomic_baix",
                      label = "Nivel Socioeconómico",
                      choices = list(
                        "No Bajo" = 0L,
                        "Bajo" = 1L
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
                      selected = c(0L, 1L),
                      justified = TRUE
                      ),
                    pickerInput(
                      inputId = "tendencia_filter_diagnostico", 
                      label = "Diagnóstico", 
                      choices = options_tendencia_filter_diagnostico, 
                      selected = options_tendencia_filter_diagnostico, 
                      multiple = TRUE, 
                      options = pickerOptions(
                        actionsBox = TRUE,
                        liveSearch = TRUE,
                        selectedTextFormat = "count > 3",
                        title = "Selecciona el diagnóstico de interés" 
                      )
                      ),
                    checkboxGroupButtons(
                      inputId = "tendencia_filter_nou_pacient",
                      label = "Paciente tipo",
                      choices = list(
                        "Nuevo" = 1L,
                        "No Nuevo" = 0L
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
                      selected = c(0L, 1),
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
      ), # cierra el tab
      
      # 2. Territorio --------------------
      tabItem(tabName = "territorio",
              fluidRow(
                column(
                  width = 2, 
                  box(
                    title = "Filtros", 
                    status = "info", 
                    width = NULL, 
                    icon = icon("filter"), 
                    solidHeader = FALSE, 
                    pickerInput(
                      inputId = "territorio_indicador", 
                      label = "Indicador", 
                      choices = list(
                        "Población Atendida" = 1
                      ), 
                      selected = 1, 
                      multiple = FALSE, 
                      options = pickerOptions(
                        showTick = TRUE,
                        actionsBox = TRUE,
                        liveSearch = TRUE,
                        title = "Selecciona el indicador de interés" 
                      )
                    ),
                    hr(),
                    sliderInput(
                      inputId = "territorio_filter_anio", 
                      label = "Año", 
                      min = min(activitat_territori$Any), 
                      max = max(activitat_territori$Any),
                      value = min(activitat_territori$Any),
                      step = 1, 
                      ticks = TRUE
                    ),
                    radioGroupButtons(
                      inputId = "territorio_filter_tipus_centre",
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
                      inputId = "territorio_filter_rs", 
                      label = "Region Sanitaria", 
                      choices = options_territorio_filter_rs, 
                      selected = NULL, 
                      multiple = FALSE, 
                      options = pickerOptions(
                        showTick = TRUE,
                        actionsBox = TRUE,
                        liveSearch = TRUE,
                        title = "Selecciona la región sanitaria de interés" 
                        )
                      ),
                    pickerInput(
                      inputId = "territorio_filter_aga", 
                      label = "Centro", 
                      choices = NULL, 
                      selected = NULL, 
                      multiple = TRUE, 
                      options = pickerOptions(
                        actionsBox = TRUE,
                        liveSearch = TRUE,
                        selectedTextFormat = "count > 3",
                        title = "Seleccion el área de gestion asistencial de interés" 
                      )
                    ),
                    checkboxGroupButtons(
                      inputId = "territorio_filter_sexe",
                      label = "Sexo",
                      choices = list(
                        "Hombres" = 1L,
                        "Mujeres" = 0L
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
                      selected = c(0L, 1L),
                      justified = TRUE
                    ),
                    pickerInput(
                      inputId = "territorio_filter_grup_edat", 
                      label = "Grupo de edad", 
                      choices = options_territorio_filter_grup_edat, 
                      selected = options_territorio_filter_grup_edat, 
                      multiple = TRUE, 
                      options = pickerOptions(
                        actionsBox = TRUE,
                        liveSearch = TRUE,
                        selectedTextFormat = "count > 3",
                        title = "Selecciona el grupo de edad de interés" 
                      )
                    ),
                    actionBttn(
                      inputId = "territorio_filter_clear", 
                      label = "Reset", 
                      icon = icon("redo"), 
                      style = "simple", 
                      color = "primary", 
                      size = "xs", 
                      no_outline = FALSE, 
                      block = TRUE
                    ),
                    br(),
                    actionBttn(
                      inputId = "territorio_filter_make_plot", 
                      label = "Calcular", 
                      icon = icon("location-arrow"), 
                      style = "simple", 
                      color = "success", 
                      size = "s", 
                      no_outline = FALSE, 
                      block = TRUE
                    )
                  )
                ),
                column(
                  width = 10,
                  box(
                    width = NULL,
                    title = "Mapa", 
                    status = "success", 
                    addSpinner(
                      leafletOutput("territorio_map", height = "85vh"), 
                      spin = "rotating-plane", 
                      color = "#E41A1C"
                      )
                    )
                  )
              )
      ) # cierra el tab
      
    ) # cierre tab items
  )# cierra el body del dashboard
)# cierra el dashboard page


