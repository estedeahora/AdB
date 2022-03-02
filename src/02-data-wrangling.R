# Limpieza y formateo para el análisis

# Distancia de red a objetos puntuales ------------------------------------
  # Armar ppp ---------------------------------------------------------------

    ppp <- MAP[map_lgl(MAP,
                       \(gdf){
                         clase <- gdf |>
                           st_geometry() |>
                           class()
                         if(any(str_detect(clase, "POINT"))  ) TRUE else FALSE
                       })] |>
      bind_rows(.id = "db") |>
      select(db, tipo, sector)

    ppp <- ppp |>
      filter(!st_is_empty(ppp)) |>
      left_join(AUX$db_label, by = "db") |>
      mutate(id = 1:n(),
             CLASE = factor(CLASE,
                            levels = unique(AUX$db_label$CLASE) ) )

  # Asignar nodo a ppp ------------------------------------------------

    if(AUX$d){
      # blend red con ppp
      cat("Blend ppp")
      ini <- Sys.time()
      net_aux <- st_network_blend(net, ppp)
      fin <- Sys.time()
      cat(": done. Tiempo de procesamiento: ", fin - ini, "\n")

      # Id del nodo más cercano al punto
      ppp$near_node <- st_nearest_feature(ppp, net_aux)

      # Distancia a nodo más cercano desde posición real del punto
      ppp$near_dist <- st_distance(ppp,
                                   st_geometry(net_aux)[ppp$near_node],
                                   by_element = T) |>
        round()

      # Quitar nodos fuera de CABA (más de 500m)
      ppp <- ppp |> filter(near_dist < units::as_units(500, "m") )

      save(net_aux, ppp, file = "data/net.RData")

    }else{
      load("data/net.RData")
    }

  # Calcular matriz de distancias ------------------------------------------------

    from <- 1:nrow(st_as_sf(activate(net, "nodes")))
    to <- ppp$near_node |> unique()

    cat("Matriz de costos")
    ini <- Sys.time()
    dis <- st_network_cost(x = net_aux, from = from, to = to)
    fin <- Sys.time()
    cat(": done. Tiempo de procesamiento: ", fin - ini, "\n")
    rm(ini, fin)

    colnames(dis) <- to
    rownames(dis) <- from

  # Distancia mínima a ppp ---------------------------------------------------

    res <- data.frame(from = from)

    cat("Calculando distancia mínima\n")
    for(i in unique(ppp$db)){
      cat(i)
      ppp_aux <- ppp |>
        filter(db == i) |>
        mutate(node = as.character(near_node),
               dist = as.numeric(near_dist) )
      dis_aux <- dis[ , ppp_aux$node]
      dis_aux <- t(apply(dis_aux, 1 , function(x) x + ppp_aux$dist))
      res[i] <- apply(dis_aux, 1, min)
      cat(": done\n")
    }

    rm(from, to, i, dis_aux, ppp_aux)

# Armado de bases espaciales ---------------------------------------------

  esquina <- net |>
    st_as_sf() |>
    select(from = ".tidygraph_node_index") |>
    left_join(res, by = "from")

  rm(res, net_aux, dis)

# Guardar bases ----------------------------------------------------------
  save(esquina, file = "data/esquina.RData")
