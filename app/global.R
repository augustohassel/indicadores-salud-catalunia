
# 1. renv & Config File ---------------------------------------------------

renv::consent(provided = TRUE); renv::restore();
config <- config::get(config = "", file = "config.yml")

# 2. Libraries ------------------------------------------------------------

paquetes <- list(
  "Reproduccion" = list("renv", "config"),
  "Shiny Core" = list("shiny", "shinydashboard"),
  "Shiny Extras" = list("shinydashboardPlus", "shinyjs", "fresh", "sever", "shinyWidgets"),
  "Graficos" = list("plotly"),
  "Tidyverse" = list("dplyr", "tidyr", "purrr", "lubridate", "readxl", "stringr", "magrittr", "forcats", "modelr"),
  "Map" = list("leaflet", "rgdal")
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

# * 1. Activitat Centre ---------------------------------------------------

activitat_centre <- activitat_centre %>%
  rename(
    id_rs_centre = `id_rs_centre...4`,
    rs_centre = `id_rs_centre...5`
  ) %>%
  mutate(
    across(.cols = c("sexe", "nivell_socioeconomic_baix", "Nou_pacient"), .fns = as.integer),
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

# 2. Activitat Territori --------------------------------------------------

activitat_territori <- activitat_territori %>%
  mutate(
    id_rs_new = case_when(
      Any == 2019 ~ id_aga,
      TRUE ~ id_rs
    ),
    rs_new = case_when(
      Any == 2019 ~ aga,
      TRUE ~ rs
    ),
    id_aga_new = case_when(
      Any == 2019 ~ id_rs,
      TRUE ~ id_aga
    ),
    aga_new = case_when(
      Any == 2019 ~ rs,
      TRUE ~ aga
    ),
    across(.cols = c("sexe", "nivell_socioeconomic_baix", "Nou_pacient"), .fns = as.integer),
  ) %>%
  select(-c(id_rs, rs, id_aga, aga)) %>%
  rename(
    id_rs  = id_rs_new,
    rs     = rs_new,
    id_aga = id_aga_new,
    aga    = aga_new
  ) %>%
  filter(grup_edat != "#N/D") %>%
  mutate(
    across(.cols = c("id_rs"), .fns = as.integer),
    grup_edat = forcats::fct_relevel(
      .f = grup_edat, 
      c("<6", "6-12", "13-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
    ),
    tipus_centre = case_when(
      as.integer(grup_edat) < 4 ~ 'Infantil',
      TRUE ~ 'Adults'
    )
    ) %>% 
  filter(!is.na(id_rs))


relaciones_territorio <- activitat_territori %>%
  select(id_rs, rs, id_aga, aga) %>%
  distinct()

options_territorio_filter_rs <- relaciones_territorio %>%
  distinct(id_rs, rs) %>%
  arrange(rs) %>% {
    setNames(object = .$id_rs, nm = .$rs)
  }

options_territorio_filter_grup_edat <- activitat_territori %>% 
  distinct(grup_edat) %>% 
  arrange(grup_edat) %>%
  pull() %>%
  as.character()

data_territorio_map <- readOGR("data/AGA_202005")
data_territorio_map <- spTransform(data_territorio_map, CRS("+proj=longlat +datum=WGS84"))

data_territorio_map@data <- data_territorio_map@data %>%
  left_join(
    relaciones_territorio %>% 
      select(id_aga, aga),
    by = c("CODIAGA" = "id_aga")
    )

poblacio_territori <- poblacio_territori %>%
  mutate(
    sexe = as.integer(sexe),
    grup_edat = forcats::fct_relevel(
      .f = grup_edat, 
      c("<6", "6-12", "13-17", "18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
    )
  )

# 5. Modules --------------------------------------------------------------

invisible(lapply(list.files(path = "modules", full.names = T), source))
