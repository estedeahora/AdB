#                      Análisis y modelado

# Preparar datos ----------------------------------------------------------

  data <- esquina |>
    select(AUX$db_label$db[AUX$db_label$ANALISIS == 1])

# 01 Reducción de dimensionalidad ------------------------------------------
  res_PCA <- data |>
    st_drop_geometry() |>
    PCA(graph = F)

# 02 Armado de grupos -------------------------------------------------------
  AUX$n_cl <- 6

  res_clu <- HCPC(res_PCA, nb.clust = AUX$n_cl, graph = F )
  data$gr <- factor(res_clu$data.clust$clust)

# 03 Tablas --------------------------------------------------------------

  #  Descripción de grupos ---------------------------------------------------

  res_sum <- res_clu$desc.var$quanti |>
      map(\(x) x |> as_tibble(rownames = "VAR") |> filter(v.test < 0) |> select(VAR, 4:3) ) |>
      reduce(full_join, by = c("VAR", "Overall mean"))

  names(res_sum)[-c(1:2)] <- paste0("MeanCl", 1:(ncol(res_sum)-2))
  res_sum <- res_sum |>
    mutate(across(.cols = where(is.numeric),
                  .fns = ~round(.x))) |>
    left_join(AUX$db_label, by = c("VAR" = "db" ) ) |>
    select(VAR, db_lab, CLASE_lab, "Overall mean", starts_with("MeanCl") )

  res_sum <- as.data.frame(res_sum)
  rownames(res_sum) <- res_sum$VAR

  write.xlsx(res_sum |> select(-VAR), "results/desc_cluster.xlsx",
             row.names = T, showNA = F)

# 04 Figuras -----------------------------------------------


  # Fig01. Rehacer? ------------------------------------------------------
  # Ver tesis

  # Fig02. Rehacer? ------------------------------------------------------
  # Ver en Mexico

  # Fig03. Entornos urbanos ------------------------------------------------------

  TH <- c("#D1E107", "#0D9743",
          "#0AE1A7", "#E8EC6E", "#0072B2",
          "#A194FC", "#CA280E", "#F07561", "#FCCB94")

  p4 <- ggplot(RADIO$RECORTADO) +
    geom_sf(aes(fill = TIPO_HABIT), alpha = 0.7, color = NA) +
    geom_sf(data = COMUNA, fill = NA) +
    geom_sf_text(data = COMUNA,
                 color = "grey15",
                 size = 8,
                 aes(label = as.numeric(COMUNA))) +
    scale_fill_discrete("Entorno Urbano", type =  TH) +
    annotation_scale() +
    annotation_north_arrow(width = unit(1, "cm"), location='tr',
                           which_north = "grid") +
    theme_void()

  ggsave(filename = "results/04_Entornos-urbanos.png",
         plot = p4,
         width = 25, height = 18, units = "cm")

  rm(TH)

  # Fig05. NBI y MNI ---------------------------------------------------------

  p5a <- RADIO$ENTERO %>%
    # left_join(AUX$RADIO [-4], by = "ID") %>%
    # mutate(UNI = SUP25 / POB) %>%
    ggplot() +
    geom_sf(aes(fill = UNI), color = NA) +
    geom_sf(data = COMUNA, fill = NA) +
    geom_sf_text(data = COMUNA,
                 aes(label = COMUNA),
                 color = "grey15",
                 size = 8) +
    # facet_wrap(var(indicador)) +
    scale_fill_gradient2("Universitario completo",
                         na.value = rgb(0, 0, 0, alpha = 0),
                         labels = scales::percent) +
    annotation_scale() +
    annotation_north_arrow(location='tr',
                           which_north = "grid",
                           width = unit(1, "cm")) +
    theme_void() +
    theme(legend.position = "bottom")

  ggsave(filename = "results/05_NBI-MNI.png",
         plot = p5a + p5b,
         width = 25, height = 18, units = "cm")


  # Fig06. Déficit -----------------------------------------------------------
  # Ver en Mexico

  # Fig09. PBG ---------------------------------------------------------------------

  p10 <- COMUNA |>
    mutate(COMUNA = as.numeric(COMUNA)) |>
    left_join(read_excel("data/pbg.xlsx"), by = "COMUNA" ) |>
    mutate(PBG = PBG / 1000000) |>
    ggplot() +
    geom_sf(aes(fill = PBG ), alpha = 0.7 ) +
    geom_sf_text(data = COMUNA,
                 color = "grey15",
                 size = 8,
                 aes(label = as.numeric(COMUNA))) +
    scale_fill_viridis_c("Producto BrutoGeográfico\n(en millones de pesos)",
                         trans = 'log2', option = "B", begin = 0.2) +
    annotation_scale() +
    annotation_north_arrow(location='tr',
                           which_north = "grid",
                           width = unit(1, "cm")) +
    theme_void()
    # theme(legend.position = "bottom")

  ggsave(filename = "results/10_NBI-MNI.png",
         plot = p10,
         width = 25, height = 18, units = "cm")

  # Fig10. Mapa de individuos y variables (Dim 1:2) -------------------------

  p_aux <- res_PCA[["var"]][["coord"]] |>
    as_tibble(rownames = "db") |>
    left_join(AUX$db_label, by = "db")

  p2 <- fviz_pca_var(res_PCA, geom = F, title = "") +
    geom_segment(aes(xend = Dim.1, yend = Dim.2, color = CLASE_lab),
                 x = 0, y = 0, data = p_aux,
                 arrow = arrow(length = unit(0.2, "cm"))) +
    geom_text_repel(data = p_aux,
                    aes(x = Dim.1, y = Dim.2,
                        label = db, color = CLASE_lab )) +
    scale_color_uchicago() +
    labs(color = "") +
    theme(legend.position = "bottom")

  p3 <- fviz_mfa_ind(res_PCA, geom = c("point"), #shape.ind = 20,
                     alpha.ind = 0.1, habillage = data$gr) +

    theme(legend.position = "bottom")

  p3 <- p3 |>
    fviz_centr() |>
    geom_hull(axes = 1:2)

  ggsave(filename = "results/01_Variables-Individuos_1-2.png",
         plot = p2 + p3,
         width = 45, height = 23, units = "cm")
  # Fig11. Ubicación espacial de cluster ------------------------------------

  p4 <- data |>
    ggplot() +
    geom_sf(data = net |> activate(edges) |> st_as_sf(),
            alpha = 0.5, color = "grey60") +
    geom_sf(aes(color = gr), alpha = 0.5) +
    geom_sf(data = COMUNA, fill = NA ) +
    labs(color = "Clúster") +
    annotation_scale(location = 'bl') +
    annotation_north_arrow(width = unit(1, "cm"), location = 'tl') +
    theme_void()

  ggsave(filename = "results/02_Cluster6.png",
         plot = p4,
         width = 23, height = 23, units = "cm")

  # ApC. Varianza explicada y Silhouette ------------------------------------

  p1a <- fviz_eig(res_PCA,
                 main = "",
                 geom = "bar",
                 addlabels = T,
                 hjust = 0.4,
                 xlab = "Dimensión",
                 ylab = "Porcentaje de varianza explicada") +
    geom_rect(data= data.frame(x1 = 0.5, x2 = 5.5, y1 = -.3, y2 = 33 ),
              fill = NA, color = "tomato4", linetype = 2,
              mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, x = NULL, y = NULL)) +
    geom_text(x = 3.5, y = 31, label = "Dimensiones consideradas\n para el agrupamiento",
              color = "tomato4")

  p1b <- data |>
    st_drop_geometry() |>
    select(-gr) |>
    fviz_nbclust(kmeans, method = 'silhouette', print.summary = F ) +
    labs(title = "", x = "Número de clústers", y = "Silhouette") +
    theme_minimal()

  p1b[["layers"]][[3]][["data"]][["xintercept"]] <- AUX$n_cl

  ggsave(filename = "results/Ap-C_Varianza-silhouette.png",
         plot = p1a + p1b,
         width = 35, height = 15, units = "cm")

  # ApC. Contribución de variables (Dim 1:2) -----------------------------

  p2a <- fviz_contrib(res_PCA, choice = "var", axes = 1,
                      title = "Dimensión 1" ) +
    labs(y = "Contribución (%)")
  p2a$layers[[2]] <- NULL
  p2b <- fviz_contrib(res_PCA, choice = "var", axes = 2,
                      title = "Dimensión 2") +
    labs(y = "Contribución (%)")
  p2b$layers[[2]] <- NULL

  ggsave(filename = "results/Ap-C_Contribucion.png",
         plot = p2a + p2b,
         width = 35, height = 15, units = "cm")

  # ApC. Contribución de individuos y variables (Dim 3:4) -------------------------

  p2 <- fviz_pca_var(res_PCA, geom = F, title = "") +
    geom_segment(aes(xend = Dim.3, yend = Dim.4, color = CLASE_lab),
                 x = 0, y = 0, data = p_aux,
                 arrow = arrow(length = unit(0.2, "cm"))) +
    geom_text_repel(data = p_aux,
                    aes(x = Dim.3, y = Dim.4,
                        label = db, color = CLASE_lab )) +
    scale_color_uchicago() +
    labs(color = "") +
    theme(legend.position = "bottom")

  p3 <- fviz_mfa_ind(res_PCA, geom = c("point"), axes = 3:4,
                     alpha.ind = 0.1, habillage = data$gr) +
    theme(legend.position = "bottom")

  p3 <- p3 |>
    fviz_centr() |>
    geom_hull(axes = 3:4)

  ggsave(filename = "results/Ap-C_Variables-Individuos_3-4.png",
         plot = p2 + p3,
         width = 45, height = 23, units = "cm")

  rm(p_aux)

  # ApD. Boxplot de distancias por cluster ------------------------------------

  esq_aux <- data |>
    st_drop_geometry() |>
    pivot_longer(cols = -c(gr), names_to = "db")|>
    left_join(AUX$db_label, by = "db") |>
    mutate(value = round(value/1000, 1),
           name = factor(db,
                         levels = unique(AUX$db_label$db),
                         labels = unique(AUX$db_label$db_lab))) |>
    split(~CLASE)

  walk(esq_aux, gg_desc)

  # Ap.E Ubicación espacial según número de cluster ---------------------------

  if(AUX$d){
    df_cl <- data.frame(id = 1:nrow(esquina))
    for(i in 2:7){
      cat("N cluster:", i, "\n")
      if(i == AUX$n_cl){
        df_cl[paste0("cl", i)] <- res_clu$data.clust$clust
      }else{
        df_cl[paste0("cl", i)] <- HCPC(res_PCA, nb.clust = i, graph = F )$data.clust$clust
      }
    }

    p <- bind_cols(data, df_cl)  |>
      mutate(id = 1:n()) |>
      select(starts_with("cl")) |>
      pivot_longer(cols = cl2:cl7,
                   names_to = "cl_n",
                   values_to = "cl") |>
      ggplot() +
      geom_sf(aes(color = cl), alpha = 0.5) +
      geom_sf(data = COMUNA, fill = NA ) +
      scale_color_discrete("Clúster") +
      facet_wrap(vars(cl_n), ) +
      theme_void() +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank() )

    ggsave(filename = "results/Ap-D_Cluster2-7.png",
           plot = p5,
           width = 60, height = 40, units = "cm")

    rm(i, df_cl)
  }


  # Agrupamiento de variables -----------------------------------------------

  fviz_dend(tr, type = "circular") + theme(legend.position = "none", axis.text.y = NULL)


  rm(p1a, p1b, p2, p3, p4, p5)
