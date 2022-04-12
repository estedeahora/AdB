# Packages -------------------------------

source("scripts/00_Packages.R", encoding = "UTF-8")
source("scripts/00_functions.R", encoding = "UTF-8")

# Auxiliary list ------------------------------------------------------------

AUX <- list(# Do you want to download the data?
            d = T,
            # Should the constructed network be directed?
            bidir = F,
            # Auxiliary data
            db_label = read_excel("data/base.xlsx", sheet = "db"),
            EVO = read_excel("data/base.xlsx", sheet = "POB") |>
                    mutate(across(.cols = c(POBLACION, POB_VILLA),
                                  .fns = ~.x/1000000)),
            VIV = read_excel("data/base.xlsx", sheet = "VIV"),
            PBG = read_excel("data/base.xlsx", sheet = "PBG"),
            # CABA projection
            crs = "+proj=tmerc +lat_0=-34.6297166 +lon_0=-58.4627 +k=0.9999980000000001 +x_0=100000 +y_0=100000 +ellps=intl +towgs84=-148,136,90,0,0,0,0 + units=m +no_defs")

# Run script -------------------------------------------------------------

if(AUX$d){
  cat("Inicio:", Sys.time(),"\n")
  source("scripts/01_carga-y-descarga-de-datos.R", encoding = "UTF-8")
  source("scripts/02_data-wrangling.R", encoding = "UTF-8")
}else{
  if(file.exists("data/data.RData") &&
     file.exists("data/net.RData") &&
     file.exists("data/esquina.RData")){
    # scripts01
    load("data/data.RData")
    # scripts02
    # load("data/net.RData")
    load("data/esquina.RData")
  }else{
    stop("data files doesn't exist. Set AUX$d == T")
  }
}

source("scripts/03_Analisis-y-modelado.R", encoding = "UTF-8")
source("scripts/04_Resultados.R", encoding = "UTF-8")

