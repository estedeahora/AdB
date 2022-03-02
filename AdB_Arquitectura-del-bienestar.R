# Packages -------------------------------

source("src/00-Packages.R", encoding = "UTF-8")

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
  source("src/01-carga-y-descarga-de-datos.R", encoding = "UTF-8")
  source("src/02-data-wrangling.R", encoding = "UTF-8")
}else{
  if(file.exists("data/data.RData") &&
     file.exists("data/net.RData") &&
     file.exists("data/esquina.RData")){
    # src01
    load("data/data.RData")
    # src02
    # load("data/net.RData")
    load("data/esquina.RData")
  }else{
    stop("data files doesn't exist. Set AUX$d == T")
  }
}

source("src/03-Analisis-y-modelado.R", encoding = "UTF-8")

if(file.exists("src/04-Resultados.R") &&
   file.exists("src/_functions.R")){
  source("src/_functions.R", encoding = "UTF-8")
  source("src/04-Resultados.R", encoding = "UTF-8")
}



