
# 1. renv & Config File ---------------------------------------------------

renv::consent(provided = TRUE); renv::restore();
config <- config::get(config = "", file = "config.yml")

# 2. Libraries ------------------------------------------------------------

paquetes <- list(
  "Reproduccion" = list("renv", "config"),
  "Shiny Core" = list("shiny", "shinydashboard"),
  "Shiny Extras" = list("shinydashboardPlus", "shinyjs", "fresh", "sever", "shinyWidgets"),
  "Graficos" = list("plotly"),
  "Tidyverse" = list("dplyr", "tidyr", "purrr", "lubridate", "readxl", "stringr", "magrittr", "forcats", "modelr")
)

lapply(as.list(c(paquetes, recursive = T, use.names = F)),
       function(x) {
         library(x, character.only = T, verbose = F)
       })
rm(list = c("paquetes"))

# 3. Global Parameters ----------------------------------------------------

options(future.globals.maxSize = config$params$future_max_size)
options(shiny.fullstacktrace = config$params$fullstacktrace)
options(shiny.reactlog = config$params$shiny_reactlog)

options(scipen = config$params$scipen)
Sys.setenv(TZ = config$params$TZ)

# 4. Cargo la data --------------------------------------------------------

data_nombres <- readxl::excel_sheets(path = "data/data.xlsx")

walk(
  .x = data_nombres, 
  .f = function(x) {
    assign(
      x = stringi::stri_trans_general(x, "latin-ascii"), 
      value = readxl::read_xlsx(path = "data/data.xlsx", sheet = x), 
      envir = .GlobalEnv)
  })

variables <- readxl::read_xlsx(path = "data/variables.xlsx")

rm(list = c("data_nombres"))

# 5. Organizo la data -----------------------------------------------------

activitat_centre <- activitat_centre %>%
  rename(
    id_rs_centre = `id_rs_centre...4`,
    rs_centre = `id_rs_centre...5`
  ) %>%
  mutate(
    grup_edat = forcats::fct_relevel(
      .f = grup_edat, 
      c("<6", "6-12", "13-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
      ),
    diagnostico = case_when(
      pcsm == 0 ~ 'paciente no crónico',
      pcsm == 1 & pccsm == 0 ~ 'paciente crónico no complejo',
      pccsm ==1 ~ 'paciente crónico complejo'
    )
  )

options_tendencia_filter_rs_centre <- activitat_centre %>%
  distinct(id_rs_centre, rs_centre) %>%
  arrange(rs_centre) %>% {
    setNames(object = .$id_rs_centre, nm = .$rs_centre)
  }

options_tendencia_filter_centre <- activitat_centre %>%
  distinct(id_centre, centre) %>% 
  arrange(centre) %>% {
    setNames(object = .$id_centre, nm = .$centre)
  }

options_tendencia_filter_grup_edat <- activitat_centre %>% 
  distinct(grup_edat) %>% 
  arrange(grup_edat) %>%
  pull() %>%
  as.character()

options_tendencia_filter_diagnostico <- activitat_centre %>% 
  distinct(diagnostico) %>%
  pull() %>%
  as.character()

data_referencia_tendencia_plot_3 <- activitat_centre %>%
  filter(id_rs_centre == 67) %>%
  group_by(any) %>%
  summarise(
    visitas = round(mean(visites/pacients, na.rm = TRUE), 2)
  )


# 5. Modules --------------------------------------------------------------

invisible(lapply(list.files(path = "modules", full.names = T), source))



























