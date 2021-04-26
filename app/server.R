# SERVER ####

# Aplicacion ####

function(input, output, session) {
  
  # Reconectar
  
  session$allowReconnect(TRUE)
  
  sever(html = sever_default(title = "Ups...", subtitle = "Se desconectó la sesión.", button = "Reconectar"))
  
  # 1. Tendencia --------------------
  
  tendencia_data <- reactive({
    
    data <- activitat_centre %>%
      filter(tipus_centre == input$tendencia_filter_tipus_centre) %>%
      filter(id_rs_centre %in% input$tendencia_filter_rs_centre) %>%
      filter(id_centre %in% input$tendencia_filter_centre) %>%
      filter(sexe %in% input$tendencia_filter_sexe) %>%
      filter(grup_edat %in% input$tendencia_filter_grup_edat) %>%
      filter(nivell_socioeconomic_baix %in% input$tendencia_filter_nivell_socioeconomic_baix) %>%
      filter(diagnostico %in% input$tendencia_filter_diagnostico) %>%
      filter(Nou_pacient %in% input$tendencia_filter_nou_pacient)
    
    return(data)
    
  })
  
  # * 1. Evolución Anual --------------------
  
  output$tendencia_plot_1 <- renderPlotly({
    
    data <- tendencia_data()
    
    shiny::validate(
      need(nrow(data) > 0, message = "Aún no hay data.")
    )
    
    data_plot <- data %>%
      group_by(any, rs_centre) %>%
      summarise(
        pacients = sum(pacients, na.rm = TRUE),
        .groups = "drop"
      )
    
    mod <- lm(pacients ~ any, data = data_plot)
    
    data_plot <- data_plot %>%
      distinct(any) %>%
      add_predictions(mod) %>%
      left_join(
        data_plot %>%
          pivot_wider(names_from = "rs_centre", values_from = "pacients"),
        by = "any"
      )
    
    p <- plotly::plot_ly(
      data = data_plot,
      x = ~ any, 
      y = ~ pred, 
      type = "scatter",  
      mode = "lines", 
      line = list(color = "orange"),
      name = "Tendencia"
      )
    
    data_plot %>% 
      select(-c(any, pred)) %>%
      iwalk(.f = function(x, y) {
        if (startsWith(y, prefix = "CATALUNYA") == TRUE) {
          p <<- p %>% 
            plotly::add_trace(
              y = as.formula(glue::glue("~`{y}`")), 
              name = y, 
              line = list(color = "green")
              )
        } else {
          p <<- p %>% 
            plotly::add_trace(
              y = as.formula(glue::glue("~`{y}`")), 
              name = y,
              line = list(color = "lightgray")
            )
        }
      })
    
    p %>% 
      layout(
        xaxis = list(title = "Any", type = "category"),
        yaxis = list(title = "Número de pacientes")
        )
    
  })
  
  # * 2. Base 2016 --------------------
  
  output$tendencia_plot_2 <- renderPlotly({
    
    data <- tendencia_data()
    
    shiny::validate(
      need(nrow(data) > 0, message = "Aún no hay data.")
    )
    
    data_plot <- data %>%
      group_by(any, rs_centre) %>%
      summarise(
        pacients = sum(pacients, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      group_by(rs_centre) %>%
      arrange(any) %>%
      mutate(
        base = round((pacients / first(pacients)) - 1, 2)
      ) %>%
      ungroup() %>%
      select(-pacients) %>%
      pivot_wider(names_from = "rs_centre", values_from = "base")
    
    p <- plotly::plot_ly(
      data = data_plot,
      x = ~ any
    )
    
    data_plot %>% 
      select(-c(any)) %>%
      iwalk(.f = function(x, y) {
        if (startsWith(y, prefix = "CATALUNYA") == TRUE) {
          p <<- p %>% 
            plotly::add_trace(
              y = as.formula(glue::glue("~`{y}`")), 
              name = y, 
              line = list(color = "green"),
              type = "scatter",  
              mode = "lines"
            )
        } else {
          p <<- p %>% 
            plotly::add_trace(
              y = as.formula(glue::glue("~`{y}`")), 
              name = y,
              line = list(color = "lightgray"),
              type = "scatter",  
              mode = "lines"
            )
        }
      })
    
    p %>% 
      layout(
        xaxis = list(title = "Any", type = "category"),
        yaxis = list(title = "Base 2016")
      )
    
  })
  
  # * 3. Jitter Plot --------------------
  
  output$tendencia_plot_3 <- renderPlotly({
    
    data <- tendencia_data()
    
    shiny::validate(
      need(nrow(data) > 0, message = "Aún no hay data.")
    )
    
    data_plot <- data %>%
      group_by(any, centre) %>%
      summarise(
        visitas = round(sum(visites, na.rm = TRUE) / sum(pacients, na.rm = TRUE), 2), 
        .groups = "drop"
      ) %>%
      ggplot() +
      geom_jitter(
        aes(
          x = any, 
          y = visitas, 
          text = glue::glue("Centro: {centre}")
          ), 
        colour = "grey"
        ) +
      geom_line(
        data = data_referencia_tendencia_plot_3, 
        aes(
          x = any, 
          y = visitas, 
          text = glue::glue("CATALUNYA CENTRAL")
          ), 
        colour = "orange", 
        size = 0.7, 
        inherit.aes = FALSE
        ) +
      theme_minimal() +
      scale_x_continuous(breaks = unique(activitat_centre$any))
    
    ggplotly(data_plot)
    
  })
  
  # 2. Territorio --------------------
  
  observeEvent(input$territorio_filter_rs,{
    
    opciones <- relaciones_territorio %>%
      filter(id_rs == input$territorio_filter_rs) %>%
      distinct(id_aga, aga) %>%
      arrange(aga) %>% {
        setNames(object = .$id_aga, nm = .$aga)
      }
    
    updatePickerInput(
      session = session, 
      inputId = "territorio_filter_aga", 
      choices = opciones, 
      clearOptions = TRUE
      )
    
  })
  
  observeEvent(input$territorio_filter_clear, {
    updateRadioGroupButtons(
      session = session, 
      inputId = "territorio_filter_tipus_centre", 
      selected = "Adults"
      )
    updateSliderInput(
      session = session, 
      inputId = "territorio_filter_anio", 
      value = 2016
      )
    updatePickerInput(
      session = session, 
      inputId = "territorio_filter_rs",
      selected = NA, 
      clearOptions = TRUE
    )
    updatePickerInput(
      session = session, 
      inputId = "territorio_filter_aga",
      selected = NA, 
      clearOptions = TRUE
    )
    updateCheckboxGroupButtons(
      session = session, 
      inputId = "territorio_filter_sexe", 
      selected = c(0L, 1L)
      )
    updatePickerInput(
      session = session, 
      inputId = "territorio_filter_grup_edat", 
      selected = options_territorio_filter_grup_edat
      )
  })
  
  territorio_data <- reactive({
    
    data <- activitat_territori %>%
      filter(tipus_centre == input$territorio_filter_tipus_centre) %>% {
        data <- .
        if (input$territorio_filter_rs == "") {
          data
        } else {
          data %>%
            filter(id_rs == input$territorio_filter_rs)
        }
      } %>% {
        data <- .
        if (is_null(input$territorio_filter_aga) == TRUE) {
          data
        } else {
          data %>%
            filter(id_aga %in% input$territorio_filter_aga)
        }
      } %>%
      filter(sexe %in% input$territorio_filter_sexe) %>%
      filter(grup_edat %in% input$territorio_filter_grup_edat) %>%
      filter(Any == input$territorio_filter_anio)
    
    return(data)
    
  })
  
  
  
  # * 1. Mapa --------------------
  
  territorio_data_map <- eventReactive(
    list(
      input$territorio_indicador,
      input$territorio_filter_make_plot
      ), {
        
        progressSweetAlert(
          session = session, 
          id = "progress_territorio_data_map", 
          title = tagList("Calculando indicador...", loadingState()), 
          display_pct = T, 
          value = 0, 
          striped = T, 
          status = "primary"
        )
        
        data_filtrada <- territorio_data()
        
        data_indicador <- if (input$territorio_indicador == 1) {
          data_filtrada %>%
            left_join(
              poblacio_territori %>%
                select(Any, id_aga, sexe, grup_edat, poblacio), 
              by = c("id_aga" = "id_aga", "Any" = "Any", "sexe" = "sexe", "grup_edat" = "grup_edat")) %>%
            filter(!is.na(poblacio)) %>%
            group_by(
              id_aga
            ) %>%
            summarise(
              Indicador = round(sum(pacients/poblacio) * 100, 2)
            )
            
        }
        
        data <- data_territorio_map@data %>%
          left_join(
            data_indicador, 
            by = c("CODIAGA" = "id_aga")
            ) 
        
        updateProgressBar(
          session = session, 
          id = "progress_territorio_data_map", 
          value = 100, 
          status = "success"
          )
        Sys.sleep(0.5)
        closeSweetAlert(session = session)
        
        return(data)
        
  })
  
  
  output$territorio_map <- renderLeaflet({
    
    pal <- colorBin(palette = "Spectral", domain = territorio_data_map()$Indicador)
    
    leaflet(data_territorio_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        color = "444444",
        opacity = 1,
        fillOpacity = 0.5,
        fillColor = ~pal(territorio_data_map()$Indicador),
        highlightOptions = highlightOptions(
          color = "white",
          weight = 2,
          bringToFront = TRUE
        ),
        label = ~glue::glue("{aga}: {formatC(territorio_data_map()$Indicador, big.mark = \",\")}%")
        ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = territorio_data_map()$Indicador
        )
      
  })
  
  
}
