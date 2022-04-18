# Packages -------------------------------

source(here::here("analysis/scripts/00_Packages.R") , encoding = "UTF-8")
source(here::here("analysis/scripts/00_functions.R"), encoding = "UTF-8")

# Auxiliary list ------------------------------------------------------------

AUX <- list(# Do you want to download the data?
            d = T,
            # Should the constructed network be directed?
            bidir = F,
            # Auxiliary data
            db_label = read_excel(here::here("analysis/data/base.xlsx"),
                                  sheet = "db"),
            EVO = read_excel(here::here("analysis/data/base.xlsx"),
                             sheet = "POB") |>
                    mutate(across(.cols = c(POBLACION, POB_VILLA),
                                  .fns = ~.x/1000000)),
            VIV = read_excel(here::here("analysis/data/base.xlsx"),
                             sheet = "VIV"),
            PBG = read_excel(here::here("analysis/data/base.xlsx"),
                             sheet = "PBG"),
            # CABA projection
            crs = "+proj=tmerc +lat_0=-34.6297166 +lon_0=-58.4627 +k=0.9999980000000001 +x_0=100000 +y_0=100000 +ellps=intl +towgs84=-148,136,90,0,0,0,0 + units=m +no_defs")

# Run script -------------------------------------------------------------

# Realizar la descarta y el procesamiento puede llevar alrededor de 20 minutos
#   y recursos computacionales considerables

if(AUX$d){
  cat("Inicio:", as.character(Sys.time()),"\n")
  source(here::here("analysis/scripts/01_carga-y-descarga-de-datos.R"), encoding = "UTF-8")
  source(here::here("analysis/scripts/02_data-wrangling.R"), encoding = "UTF-8")
  cat("Final:", as.character(Sys.time()),"\n")
}else{
  if(file.exists(here::here("analysis/data/data.RData") ) &&
       file.exists(here::here("analysis/data/net.RData") ) &&
       file.exists(here::here("analysis/data/esquina.RData") )  ){
    # scripts01
    load(here::here("analysis/data/data.RData") )
    # scripts02
    load(here::here("analysis/data/esquina.RData") )
  }else{
    stop("data files doesn't exist. Set AUX$d == T")
  }
}

source(here::here("analysis/scripts/03_Analisis-y-modelado.R"), encoding = "UTF-8")

source(here::here("analysis/scripts/04_Resultados.R"), encoding = "UTF-8")

