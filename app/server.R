# SERVER ####

# Aplicacion ####

function(input, output, session) {
  
  # Reconectar
  
  session$allowReconnect(TRUE)
  
  sever(html = sever_default(title = "Ups...", subtitle = "Se desconectó la sesión.", button = "Reconectar"))
  
  # 1 - Tendencia --------------------
  
  # * 1 - Evolución Anual --------------------
  
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
  
  
  
}








































