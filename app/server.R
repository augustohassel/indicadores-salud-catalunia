# SERVER ####

# Aplicacion ####

function(input, output, session) {
  
  # Reconectar
  
  session$allowReconnect(TRUE)
  
  sever(html = sever_default(title = "Ups...", subtitle = "Se desconectó la sesión.", button = "Reconectar"))
  
  # 1 - Tendencia --------------------
  
  
  
}








































