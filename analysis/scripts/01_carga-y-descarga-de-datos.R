#                         Carga y Descarga de datos

# CARTO: Radios, Comuna y CABA --------------------------------------------

# wfs Conection
wfs <- "https://geoservicios.indec.gov.ar/geoserver/ows?service=wfs&version=1.0.0&request=GetCapabilities"

fileName <- tempfile()

download.file(wfs, fileName)
request <- GMLFile$new(fileName)
client <- WFSCachingClient$new(request)

# Download RADIOS CENSO 2010
RADIO <- client$getLayer(layer = "geocenso2010:radios_codigo") |>
  select(ID = link) |>
  filter(str_starts(ID, "02")) |>
  mutate(COMUNA = str_sub(ID, start = 3, end = 5) ) |>
  st_transform(crs = 4326)

rm(client, request,wfs, filename)

# COMUNA
COMUNA <- RADIO |>
  group_by(COMUNA) |>
  summarise()

# CABA
CABA <- COMUNA |>
  summarise()

# Educación ------------------------------------------------------------------

MAP_EDU <- list()

  # Base de escuelas GCBA y Universidades  ----------------------------------

  # Datos escuelas GCBA
  ESC <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/establecimientos-educativos/establecimientos_educativos_WGS84.geojson") |>
    mutate(sector = factor(sector, levels = 1:2,
                           labels = c("Estatal", "Privado")),
           #Común
           Inicial    = ifelse(str_detect(nivmod, "IniCom"), T, F),
           Primario   = ifelse(str_detect(nivmod, "PriCom"), T, F),
           Secundario = ifelse(str_detect(nivmod, "SecCom"), T, F),
           SNU        = ifelse(str_detect(nivmod, "SNUCom"), T, F),
           # Otros servicios educativos de común
           OSECom     = ifelse(str_detect(nivmod, "SNUCom"), T, F),
           # Adultos
           Adultos    = ifelse(str_detect(nivmod, "PriAdu|SecAdu"), T, F),
           # Otros servicios educativos de adultos
           Oficio     = ifelse(str_detect(nivmod, "OSEAdu"), T, F),
           # Especial
           Especial   = ifelse(str_detect(nivmod, "IniEsp|PriEsp|SecEsp|OSEEsp"), T, F)) |>
    select(sector, Inicial:Especial) |>
    st_cast("POINT")

  # Datos Universidades en CABA
  UNI <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/universidades/universidades.csv",
                  encoding = "UTF-8") |>
    rename(geometry = WKT_gkba) |>
    st_as_sf(wkt = "geometry") |>
    mutate(sector = factor(regimen, levels = c("Público", "Privado"),
                           labels = c("Estatal", "Privado")),
           tipo = "Superior Universitaria",
           objetivo = "Educación Superior Universitaria") |>
    select(tipo, objetivo, sector)

  # 01 Educación común ---------------------------------------------------------
  MAP_EDU$INICIAL <- ESC |>
    filter(Inicial) |>
    mutate(tipo = "Inicial") |>
    select(tipo, sector)

  MAP_EDU$PRIMARIO <- ESC |>
    filter(Primario) |>
    mutate(tipo = "Primario") |>
    select(tipo, sector)

  MAP_EDU$SECUNDARIO <- ESC |>
    filter(Secundario) |>
    mutate(tipo = "Secundario") |>
    select(tipo, sector)

  # 02 Educación superior ----------------------------------------------------

  MAP_EDU$SUPERIOR <- ESC |>
    filter(SNU) |>
    mutate(tipo = "Superior No Universitaria",
           objetivo = "Educación Superior No Universitaria") |>
    select(tipo, objetivo, sector) |>
    add_row(UNI)

  rm(UNI)

  # 03 Espacios de educación complementarios -----------------------------------

  MAP_aux <- list()

  # Jardines Comunitarios
  MAP_aux$JC <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-desarrollo-humano-y-habitat/jardines-comunitarios/jardin_comunitario.geojson") |>
    select(tipo, objetivo = destinatar)

  # Centros de Primera Infancia
  MAP_aux$PI <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-desarrollo-humano-y-habitat/centros-primera-infancia/centros-de-primera-infancia.geojson") |>
    select(tipo, objetivo = objetivos)

  # Juegotecas Barriales
  MAP_aux$JT <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-desarrollo-humano-y-habitat/juegotecas-barriales/juegotecas-barriales.geojson") |>
    select(tipo) |>
    mutate(objetivo = "Son espacios dedicados a actividades lúdicas creativas con el objetivo de contribuir al desarrollo integral de los niños.")

  # Centro de desarrollo infantil
  MAP_aux$CEDI <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-desarrollo-humano-y-habitat/centros-desarrollo-infantil/centros-de-desarrollo-infantil.geojson") |>
    select(tipo, objetivo = programa)

  MAP_aux <- bind_rows(MAP_aux) |>
    mutate(sector = factor("Estatal", levels = c("Estatal", "Privado")) )

  MAP_EDU$COMPLEM <- ESC |>
    filter(OSECom) |>
    mutate(tipo = "Otros Servicios Educativos de la modalidad Común",
           objetivo = "Otros Servicios Educativos de la modalidad Común") |>
    select(tipo, objetivo, sector) |>
    add_row(MAP_aux)

  rm(MAP_aux)

  # 04 Educación especial ------------------------------------------------------

  MAP_EDU$ESPECIAL <- ESC |>
    filter(Especial) |>
    mutate(tipo = "Educación especial",
           objetivo = "Educación especial (todos los niveles y servicios)") |>
    select(tipo, objetivo, sector)

  # 05 Educación de adultos ----------------------------------------------------

  MAP_EDU$ADULTOS <- ESC |>
    filter(Adultos) |>
    mutate(tipo = "Educación de adultos",
           objetivo = "Educación de adultos (todos los niveles y servicios") |>
    select(tipo, objetivo, sector)

  # 06 Oficios y Formación profesional -----------------------------------------

  MAP_EDU$OFICIO <- ESC |>
    filter(Oficio) |>
    mutate(tipo = "Formación en oficios y profesional",
           objetivo = "Formación en oficios y profesional (no superior)") |>
    select(tipo, objetivo, sector)

  rm(ESC)


# Salud y cuidado ------------------------------------------------------------------

  MAP_SAL <- list()

  # 01 Centros de salud ---------------------------------------------------------

  MAP_aux <- list()

  # CESAC
  # Se puede contar n de especialidades. Ver página para descripción de servicios.
  MAP_aux$CESAC <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/centros-salud-accion-comunitaria-cesac/centros_de_salud_nivel_1_BADATA_WGS84.geojson") |>
    mutate(tipo = "CESAC",
           sector = "Estatal") |>
    select(tipo, objetivo = OBJETO, sector#, especialidades = especialid, VIH:EFE_SALUD)
    )

  # Centros Médicos Barriales
  MAP_aux$CMB <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/centros-medicos-barriales/centros-medicos-barriales.geojson") |>
    mutate(tipo = "Centro Médico Barrial",
           sector = "Estatal") |>
    select(tipo, objetivo = OBJETO, sector#, especialidades = ESPECIALID
           )

  # Centros de Salud Privados
  MAP_aux$PRIV <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-de-salud-privados/centros-de-salud-privados.csv") |>
    st_as_sf(coords = 1:2, crs = 4326) |>
    mutate(tipo = "Centro de salud",
           objetivo = "Hospitales, sanatorios y clínicas privado",
           sector = "Privado") |>
    select(tipo, objetivo, sector)

  # Hospitales
  # Existe dataset para seleccionar los que tienen maternidades.
  MAP_aux$HOSP <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/hospitales/hospitales.geojson") |>
    mutate(sector = "Estatal") |>
    select(tipo = OBJETO, objetivo = TIPO, sector)

  MAP_SAL$MEDICO <- bind_rows(MAP_aux)

  rm(MAP_aux)

  # 02 Farmacias ---------------------------------------------------------------

  MAP_SAL$FARMACIA <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/farmacias/farmacias.csv") |>
    st_as_sf(coords = 1:2, crs = 4326) |>
    mutate(tipo = "Farmacia") |>
    select(tipo)

  # 03 DetectAR ----------------------------------------------------------------

  MAP_SAL$DETECTAR <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/operativo-detectar/operativo-detectar.geojson") |>
    mutate(tipo = "DetectAR") |>
    select(tipo)

  # 04 Vacunatorio -------------------------------------------------------------

  MAP_aux <- list()

  # COVID
  MAP_aux$COVID <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/postas-de-vacunacion-covid-19/postas_vacunacion_covid.geojson") |>
    mutate(tipo = "Vacunatorio COVID") |>
    select(tipo)

  # Infantil
  MAP_aux$INFANTIL <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/salud/vacunatorios-infantiles/vacunatorios_infantiles_WGS84.geojson") |>
    mutate(tipo = "Vacunatorio Infantil") |>
    select(tipo)

  # Adultos mayores
  MAP_aux$MAYORES <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/vacunatorios-adultos-mayores/vacunatorios-adultos-mayores.geojson") |>
    mutate(tipo = "Vacunatorio Adultos Mayores") |>
    select(tipo)

  MAP_SAL$VACUNA <- bind_rows(MAP_aux)

  rm(MAP_aux)

  # 05 Tercera edad: Geriátricos y centros de día ------------------------------

  MAP_aux <- list()

  # Geriátricos
  MAP_aux$GERIATRICO <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-justicia-y-seguridad/geriatricos/geriatricos.geojson") |>
    mutate(tipo = "Geriátrico") |>
    select(tipo)

  # Centro de día
  MAP_aux$CDIA <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-desarrollo-humano-y-habitat/centros-dia/centros-de-dia.geojson") |>
    mutate(tipo = "Centro de día") |>
    select(tipo)

  MAP_SAL$TEREDAD <- bind_rows(MAP_aux)

  rm(MAP_aux)

  # 06 Situación de calle: Paradores y Hogares ---------------------------------

  MAP_SAL$PARADOR <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/hogares-y-paradores/hogares-y-paradores.csv") |>
    st_as_sf(coords = 1:2, crs = 4326, na.fail = F) |>
    mutate(tipo = "Hogar/Parador") |>
    select(tipo)

# Oficinas públicas y de gestión, Seguridad y Justicia -----------------------------------------------

  MAP_OFP <- list()

  # 01 Sede comunal ------------------------------------------------------------

  MAP_OFP$COMUNA <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sedes-comunales/sedes-comunales.geojson") |>
    mutate(tipo = "Sede comunal") |>
    select(tipo)

  # 02 Edificio de gestión -----------------------------------------------------

  MAP_OFP$EDIFICIO <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/edificios-publicos-del-gcba/edificios-publicos-del-gcba.csv", encoding = "UTF-8") |>
    st_as_sf(coords = 1:2, crs = 4326) |>
    mutate(tipo = "Edificio administrativo del GCBA") |>
    select(tipo, nivel_gest:nivel_get_2 )

  # 03 Centros de Integración Laboral ------------------------------------------

  MAP_OFP$CIL <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/centro-de-integracion-laboral/centro-de-integracion-laboral.csv", encoding = "UTF-8") |>
    st_as_sf(coords = 1:2, crs = 4326) |>
    select(tipo = objeto)

  # 04 Comisaria --------------------------------------------------------------

  MAP_OFP$COMISARIA <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comisarias-policia-de-la-ciudad/comisarias-policia-de-la-ciudad.csv", encoding = "UTF-8") |>
    st_as_sf(coords = 1:2, crs = 4326) |>
    mutate(tipo = "Comisaría") |>
    select(tipo)

  # 05 Bomberos ---------------------------------------------------------------

  MAP_OFP$BOMBERO <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/cuarteles-y-destacamentos-de-bomberos/cuarteles-y-destacamentos-de-bomberos-de-policia-federal-argentina.csv", encoding = "UTF-8") |>
    st_as_sf(coords = 2:3, crs = 4326) |>
    mutate(tipo = "Bombero") |>
    select(tipo)

  # 06 Fiscalía ---------------------------------------------------------------

  MAP_OFP$FISCAL <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/dependencias-y-sedes-del-ministerio-publico-fiscal/dependencias-y-sedes-del-ministerio-publico-fiscal.csv", encoding = "UTF-8") |>
    st_as_sf(coords = 2:3, crs = 4326) |>
    select(tipo = objeto)

# Comercio y financiero -------------------------------------------------------

  MAP_COM <- list()

  # 01 Ferias y Mercados ------------------------------------------------------

  MAP_aux <- list()

  # Ferias
  MAP_aux$FERIA <- read.csv2("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-espacio-publico-e-higiene-urbana/ferias-mercados/ferias.csv", encoding = "UTF-8" ) |>
    st_as_sf(coords = 2:1, crs = 4326) |>
    select(tipo = OBJETO)

  # Mercados
  MAP_aux$MERCADO <- read.csv2("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-espacio-publico-e-higiene-urbana/ferias-mercados/mercados.csv", encoding = "UTF-8" ) |>
    st_as_sf(coords = c("LON", "LAT"), crs = 4326) |>
    mutate(tipo = "Mercado") |>
    select(tipo)

  # Feria Itinerante de Abastecimiento Barrial
  MAP_aux$FIAB <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-espacio-publico-e-higiene-urbana/ferias-mercados/ferias-itinerantes-de-abastecimiento-barrial.geojson") |>
    mutate(tipo = "Feria Itinerante de Abastecimiento Barrial") |>
    select(tipo)

  MAP_COM$FERIA <- bind_rows(MAP_aux)

  rm(MAP_aux)

  # 02 Oferta gastronómica ----------------------------------------------------
  MAP_COM$RESTAURANTE <- st_read("https:/cdn.buenosaires.gob.ar/datosabiertos/datasets/oferta-gastronomica/oferta-gastronomica.geojson") |>
    mutate(tipo = "Oferta gastronómica") |>
    select(tipo)

  # 03 Bancos -----------------------------------------------------------------
  MAP_COM$BANCO <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/bancos/bancos.csv", encoding = "UTF-8") |>
    st_as_sf(coords = 2:3, crs = 4326) |>
    mutate(tipo = "Banco") |>
    select(tipo)

# Redes de sociabilidad -------------------------------------------------------
  # Cultura, Deporte, Religión y Organizaciones sociales

  MAP_RED <- list()

  # 01 Espacios culturales -----------------------------------------------------

  MAP_RED$CULTURA <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-cultura/espacios-culturales/espacios-culturales.geojson") |>
    select(tipo = FUNCION_PRINCIPAL )

  # 02 Clubes y polideportivos -------------------------------------------------

  MAP_aux <- list()

  # Clubes
  MAP_aux$CLUB <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/vicejefatura-de-gobierno/clubes/clubes.geojson") |>
    mutate(tipo = "Club") |>
    select(tipo)

  # Polideportivo
  MAP_aux$POLIDEPORTIVO <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/vicejefatura-de-gobierno/polideportivos/polideportivos.geojson") |>
    mutate(tipo = "Polideportivo") |>
    select(tipo)

  MAP_RED$DEPORTE <- bind_rows(MAP_aux)

  rm(MAP_aux)

  # 03 Centros de Jubilados ----------------------------------------------------

  MAP_RED$CJUBILADO <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/centros-de-jubilados-reempadronados/centros-de-jubilados-reempadronados.csv", encoding = "UTF-8") |>
    filter(!is.na(long)) |>
    st_as_sf(coords = 1:2, crs = 4326) |>
    mutate(tipo = "Centro de Jubilados") |>
    select(tipo)

  # 04 Espacios religiosos -----------------------------------------------------

  # Lugares de Culto

  MAP_RED$CULTO <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/lugares-de-culto/lugares-de-culto.csv", encoding = "UTF-8") |>
    filter(!is.na(long)) |>
    st_as_sf(coords = c("long", "lat"), crs = 4326) |>
    mutate(tipo = "Lugar de culto") |>
    select(tipo, grupo)

  # MAP_aux <- list()
  #
  # # Capillas
  # MAP_aux$CAPILLA <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/capillas/capillas.csv", encoding = "UTF-8") |>
  #   st_as_sf(coords = 1:2, crs = 4326) |>
  #   mutate(tipo = "Capilla") |>
  #   select(tipo)
  #
  # # Parroquias
  # MAP_aux$PARROQUIA <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/parroquias/parroquias.csv", encoding = "UTF-8") |>
  #   st_as_sf(coords = 1:2, crs = 4326) |>
  #   mutate(tipo = "Parroquia") |>
  #   select(tipo)
  #
  # # Iglesias
  # MAP_aux$IGLESIA <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/iglesias/iglesias.csv", encoding = "UTF-8") |>
  #   st_as_sf(coords = 1:2, crs = 4326) |>
  #   mutate(tipo = "Iglesias") |>
  #   select(tipo)
  #
  # MAP_RED$RELIGION <- bind_rows(MAP_aux)
  #
  # rm(MAP_aux)

  # 05 Organizaciones sociales -------------------------------------------------

  MAP_RED$ORGANIZACION <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-desarrollo-humano-y-habitat/organizaciones-sociales/organizaciones-sociales.csv", encoding = "UTF-8") |>
    st_as_sf(coords = 1:2, crs = 4326) |>
    mutate(tipo = "Organizaciòn social") |>
    select(tipo)

# Transporte -------------------------------------------------------------------

  MAP_TRA <- list()

  # 01 Parada de colectivo -----------------------------------------------------

  MAP_TRA$COLECTIVO <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/colectivos/paradas-de-colectivo.geojson") |>
    mutate(tipo = "Parada de colectivo") |>
    select(tipo) |>
    st_intersection(CABA)

  # 02 Tren/Subte --------------------------------------------------------------

  MAP_aux <- list()

  # Premetro
  MAP_aux$PREMETRO <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/premetro/estaciones-premetro.csv", encoding = "UTF-8") |>
    st_as_sf(coords = 1:2, crs = 4326) |>
    mutate(tipo = "Estación de Premetro") |>
    select(tipo)

  # Subte
  MAP_aux$SUBTE <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/sbase/subte-estaciones/estaciones-de-subte.geojson") |>
    mutate(tipo = "Estación de Subte") |>
    select(tipo)

  # Tren
  MAP_aux$TREN <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/estaciones-de-ferrocarril/estaciones-de-ferrocarril.geojson") |>
    mutate(tipo = "Estación de Tren") |>
    select(tipo) |>
    st_intersection(CABA)

  MAP_TRA$TREN <- bind_rows(MAP_aux)

  rm(MAP_aux)

  # 03 Callejero -------------------------------------------------

  MAP_TRA$CALLE <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/jefatura-de-gabinete-de-ministros/calles/callejero.geojson") |>
    st_cast("LINESTRING") |>
    select(id, nomoficial, tipo_c, red_jerarq, cod_sent)

  # 04 Metrobus -------------------------------------------------------------

  MAP_TRA$METROBUS <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte-y-obras-publicas/metrobus/recorrido-de-metrobus.geojson") |>
    mutate(tipo = "Metrobus") |>
    select(tipo)

  # 05 Estación de servicio ---------------------------------------

  MAP_TRA$ESTSERV <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/estaciones-de-servicio/estaciones_servicio_caba.csv", encoding = "UTF-8") |>
    st_as_sf(coords = 1:2, crs = 4326) |>
    mutate(tipo = "Estación de servicio") |>
    select(tipo)

  # 06 Ecobici -----------------------------------------------------------------

  MAP_TRA$ECOBICI <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte/estaciones-bicicletas-publicas/nuevas-estaciones-bicicletas-publicas.geojson") |>
    mutate(tipo = "Estación de Ecobici") |>
    select(tipo)

  # 07 Ciclovia ----------------------------------------------------------------

  MAP_TRA$CICLOVIA <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/transporte-y-obras-publicas/ciclovias/ciclovias_WGS84.geojson") |>
    mutate(tipo = "Ciclovia") |>
    select(tipo)

  # 08 Parques y Espacio verdes  -----------------------------------------------

  MAP_TRA$PARQUE <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/secretaria-de-desarrollo-urbano/espacios-verdes/espacio-verde-publico.geojson") |>
    filter(clasificac %in% c("JARDÍN BOTÁNICO", "PARQUE", "PARQUE SEMIPÚBLICO",
                             "PLAZA")) |>
    mutate(tipo = case_when(clasificac %in% c("JARDÍN BOTÁNICO", "PARQUE", "PARQUE SEMIPÚBLICO") ~ "Parque",
                            T ~ clasificac) ) |>
    st_transform(AUX$crs) |>
    st_point_on_surface() |>
    st_transform(4326) |>
    select(tipo)

# Base MAP ------------------------------------------------------------------

  MAP <- c(MAP_EDU, MAP_SAL, MAP_OFP,
           MAP_COM, MAP_TRA, MAP_RED)

  rm(MAP_EDU, MAP_SAL, MAP_OFP,
     MAP_COM, MAP_TRA, MAP_RED)

# Red de calles -----------------------------------------------------------------

  calle <- MAP$CALLE

  if(AUX$bidir){
    # Armar red bidireccional
    calle <- calle |>
      mutate(cod_sent = ifelse(cod_sent %in% c(0, 2, 12), 0, cod_sent) )

    calle_dir <- calle |>
      filter(cod_sent %in% 0:1)

    calle_inv <- calle |>
      filter(cod_sent %in% -1:0) |>
      st_reverse()

    calle <- rbind(calle_dir, calle_inv)
    rm(calle_dir, calle_inv)
  }

  # Armar red de calles
  net <-  calle |>
    select(-cod_sent) |>
    as_sfnetwork(directed = AUX$bidir,
                 length_as_weight = T)

  rm(calle)

  # Limpieza de la red
  net <- net |>
    # Subdivisión: Divide los ejes en función de los nodos interiores
    convert(to_spatial_subdivision) |>
    # Calcular peso
    activate("edges") |>
    mutate(weight = edge_length()) |>
    # Quita los loops y nodos que duplican path (mejorar rendimiento)
    arrange(weight) |>
    convert(to_spatial_simple) |>
    # Filtrar nodos aislados
    activate("nodes") |>
    filter(!node_is_isolated())

  MAP$CALLE <- MAP$CALLE |>
    filter(red_jerarq %in% c("VÍA DISTRIBUIDORA PRINCIPAL", "VÍA TRONCAL"))

# Save data --------------------------------------------------------------------

  save(MAP, net, RADIO, CABA, COMUNA,
       file = here::here("analysis/data/data.RData") )
