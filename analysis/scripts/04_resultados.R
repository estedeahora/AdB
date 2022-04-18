# Datos RADIO ------------------------------------------------------------------

load(here::here("analysis/data/radio.RData"))

# Figuras Cuerpo ----------------------------------------------------------------------

  # Fig01. Entornos urbanos ------------------------------------------------------

  AUX$TH <- c("#D1E107", "#0D9743",
              "#0AE1A7", "#E8EC6E", "#0072B2",
              "#A194FC", "#CA280E", "#F07561", "#FCCB94")

  p01 <- RADIO$REC |>
    ggplot() +
    geom_sf(aes(fill = TIPO_HABIT), alpha = 0.7, color = NA) +
    geom_sf(data = COMUNA, fill = NA) +
    geom_sf_text(data = COMUNA,
                 color = "grey15",
                 size = 8,
                 aes(label = as.numeric(COMUNA))) +
    scale_fill_discrete("Entorno Urbano", type =  AUX$TH) +
    annotation_scale() +
    annotation_north_arrow(width = unit(1, "cm"), location='tr',
                           which_north = "grid") +
    theme_void()

  ggsave(filename = here::here("analysis/results/01_Tipo-habitat.png"),
         plot = p01,
         width = 25, height = 18, units = "cm")

  # Fig02. Pirámide de población por entorno urbano -------------------------

  p02 <- RADIO$db |>
    group_by(TIPO_HABIT) |>
    summarise(across (.cols = c(starts_with("M_"), starts_with("V_")),
                      .fns = ~sum(.x, na.rm = T) ) ) |>
    pivot_longer(cols = -TIPO_HABIT) |>
    mutate(y = str_sub(name, start = 1, end = 1),
           EDADQUI = str_sub(name, start = 3, end = -1),
           EDADQUI = factor(EDADQUI,
                             levels = c(paste0(seq(0, 90, by = 5), "-",
                                             seq(5, 95, by = 5)),
                                        "+95")
                             ) ) |>
    pivot_wider(id_cols = c(TIPO_HABIT, EDADQUI), names_from = "y", values_from = "value") |>
    ungroup() |>
    ggplot() +
    geom_rect(data = data.frame(TIPO_HABIT =  unique(RADIO$db$TIPO_HABIT)),
              aes(fill = TIPO_HABIT), xmin = -Inf, xmax = Inf,
              ymin = -Inf,ymax = Inf, alpha = 0.3) +
    geom_bar(aes(x = EDADQUI, y = V), stat = "identity", fill = "blue", alpha = 0.4) +
    geom_bar(aes(x = EDADQUI, y = M), stat = "identity", fill = "red", alpha = 0.4) +
    coord_flip() +
    scale_fill_discrete(type = AUX$TH) +
    facet_wrap(~TIPO_HABIT, scales = "free_x") +
    labs(x = NULL, y = NULL) +
    scale_y_continuous(labels =  function(br) paste0(abs(br)/1000, "k")) +
    theme_minimal() +
    theme(strip.text.x = element_text(size = 7),
          legend.position = "none")

  ggsave(filename = here::here("analysis/results/02_Piramide-TH.png"),
         plot = p02,
         width = 14, height = 21, units = "cm")

  # Fig03. NBI y MNI ---------------------------------------------------------

  # NBI
  p3_aux <- RADIO$ENT %>%
    left_join(RADIO$db |> select(ID, NBI, SUP), by = "ID") %>%
    pivot_longer(cols = c(NBI, SUP)) |>
    select(ID, name, value)

  p03a <- p3_aux |>
    filter(name == "NBI") |>
    ggplot() +
    geom_sf(aes(fill = value), color = NA) +
    geom_sf(data = COMUNA, fill = NA) +
    geom_sf_text(data = COMUNA,
                 color = "grey15",
                 size = 8,
                 aes(label = as.numeric(COMUNA))) +
    scale_fill_gradient2("Hogares NBI",
                         high = scales::muted("darkred"),
                         na.value = rgb(0, 0, 0, alpha = 0),
                         labels = scales::percent) +
    annotation_north_arrow(location='tr',
                           which_north = "grid",
                           width = unit(1, "cm")) +
    annotation_scale(location = "br") +
    theme_void() +
    theme(legend.position = "bottom")

  p03b <- p3_aux |>
    filter(name == "SUP") |>
    ggplot() +
    geom_sf(aes(fill = value), color = NA) +
    geom_sf(data = COMUNA, fill = NA) +
    geom_sf_text(data = COMUNA,
                 color = "grey15",
                 size = 8,
                 aes(label = as.numeric(COMUNA))) +
    scale_fill_gradient2("Universitario completo",
                         high = scales::muted("darkorange3"),
                         na.value = rgb(0, 0, 0, alpha = 0),
                         labels = scales::percent) +
    theme_void() +
    theme(legend.position = "bottom")

  ggsave(filename = here::here("analysis/results/03_NBI-MNI.png"),
         plot = p03a + p03b,
         width = 27, height = 15, units = "cm")

  rm(p3_aux)

  # Fig04. Déficit -----------------------------------------------------------

  p04_map <-  RADIO$REC %>%
    left_join(RADIO$db |> select(ID, starts_with("x100H_")), by = "ID") |>
    bi_class(x = x100H_CUANT, y = x100H_CUALI,
             style = "jenks", dim = 3) |>
    ggplot() +
    geom_sf(aes(fill = bi_class), color = NA, alpha = 0.9) +
    bi_scale_fill(pal = "GrPink", dim = 3,   na.value = "white") +
    geom_sf(data = COMUNA, fill = NA) +
    geom_sf_text(data = COMUNA,
                 color = "grey15",
                 size = 8,
                 aes(label = as.numeric(COMUNA))) +
    annotation_scale() +
    annotation_north_arrow(width = unit(1, "cm"), location='tr',
                           which_north = "grid") +
    theme_void() +
    theme(legend.position = "none")


  p04_legend <- bi_legend(pal = "GrPink",
                      dim = 3,
                      xlab = "Déficit cuantitativo",
                      ylab = "Déficit cualitativo",
                      size = 20)

  p04 <- p04_map + (plot_spacer() / p04_legend) +
    plot_layout(ncol = 2, widths = c(3, 1))

  ggsave(filename = here::here("analysis/results/04_Deficit.png"),
         plot = p04,
         width = 30, height = 25, units = "cm")

  # Tabla déficit por TH

  p04_tab <- RADIO$db |>
    mutate(across(.cols = starts_with("x100H_"),
                  .fns = ~.x * HOGARES / 100) ) |>
    group_by(TIPO_HAB2) |>
    summarise(across(.cols = c(HOGARES, starts_with("x100H_")),
                     .fns = ~sum(.x, na.rm = T))) |>
    mutate(across(.cols = starts_with("x100H_"),
                  .fns = ~.x / HOGARES * 100) ) |>
    select(TIPO_HAB2, starts_with("x100H_"))

  xlsx::write.xlsx(p04_tab, row.names = T,
                   file = here::here("analysis/results/04_tabla-deficit.xlsx") )

  rm(p04_map, p04_legend, p04_tab)

  # Fig06. PBG ---------------------------------------------------------------------

  p06 <- COMUNA |>
    mutate(COMUNA = as.numeric(COMUNA)) |>
    left_join(AUX$PBG, by = "COMUNA" ) |>
    mutate(PBG = PBG / 1000000) |>
    ggplot() +
    geom_sf(aes(fill = PBG ), alpha = 0.6) +
    geom_sf_text(data = COMUNA,
                 color = "grey15",
                 size = 8,
                 aes(label = as.numeric(COMUNA))) +
    scale_fill_gradient2("Producto BrutoGeográfico\n(en millones de pesos)",
                         mid = "yellow",
                         low = "blue",
                         high = "darkred",
                         midpoint = 3,
                         trans = 'log2') +
    annotation_scale() +
    annotation_north_arrow(location='tr',
                           which_north = "grid",
                           width = unit(1, "cm")) +
    theme_void() +
    theme(legend.position = "bottom")

  ggsave(filename = "results/06_PBG.png",
         plot = p06,
         width = 25, height = 18, units = "cm")

  # Fig06. Mapa de individuos y variables (Dim 1:2) -------------------------

  PCA_aux <- res_PCA[["var"]][["coord"]] |>
    as_tibble(rownames = "db") |>
    left_join(AUX$db_label, by = "db")

  p07a <- fviz_pca_var(res_PCA, geom = F, title = "") +
    geom_segment(aes(xend = Dim.1, yend = Dim.2, color = CLASE_lab),
                 x = 0, y = 0, data = PCA_aux,
                 arrow = arrow(length = unit(0.2, "cm"))) +
    geom_text_repel(data = PCA_aux,
                    aes(x = Dim.1, y = Dim.2,
                        label = db, color = CLASE_lab )) +
    scale_color_uchicago() +
    labs(color = "") +
    theme(legend.position = "bottom")

  p07b <- fviz_mfa_ind(res_PCA, geom = c("point"), title = "",
                     alpha.ind = 0.1, habillage = data$gr) +
      theme(legend.position = "bottom")

  p07b <- p07b |>
    fviz_centr() |>
    geom_hull(axes = 1:2) +
    labs(fill = "Clúster") +
    guides(color = "none")

  ggsave(filename = "results/07_Variables-Individuos_1-2.png",
         plot = p07a + p07b,
         width = 45, height = 23, units = "cm")

  rm(PCA_aux)

  # Fig08. Ubicación espacial de cluster ------------------------------------

  p08 <- data |>
    ggplot() +
    geom_sf(data = net |> activate(edges) |> st_as_sf(),
            alpha = 0.5, color = "grey60") +
    geom_sf(aes(color = gr), alpha = 0.5) +
    geom_sf(data = COMUNA, fill = NA) +
    geom_sf_text(data = COMUNA,
                 color = "grey15",
                 size = 8,
                 aes(label = as.numeric(COMUNA))) +
    labs(color = "Clúster") +
    annotation_scale(location = 'bl') +
    annotation_north_arrow(width = unit(1, "cm"), location = 'tr') +
    theme_void()

  ggsave(filename = paste0("results/08_Mapa-Cluster", AUX$n_cl,".png"),
         plot = p08,
         width = 23, height = 23, units = "cm")

# Figuras Anexo ----------------------------------------------------------------

  # Fig12. Anexo C: Varianza explicada y Silhouette ------------------------------------

  p12a <- fviz_eig(res_PCA,
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

  p12b <- data |>
    st_drop_geometry() |>
    select(-gr) |>
    fviz_nbclust(kmeans, method = 'silhouette', print.summary = F ) +
    labs(title = "", x = "Número de clústers", y = "Silhouette") +
    theme_minimal()

  p12b[["layers"]][[3]][["data"]][["xintercept"]] <- AUX$n_cl

  ggsave(filename = "results/12_ApC_Varianza-silhouette.png",
         plot = p12a + p12b,
         width = 35, height = 15, units = "cm")

  # Fig13. Anexo C: Contribución de variables (Dim 1:2) -----------------------------

  p13 <-  map(1:4, \(x){
    p <- fviz_contrib(res_PCA, choice = "var", axes = x,
                      title = paste0("Dimensión ", x)) +
      labs(y = "Contribución (%)")
    return(p)
  } )

  ggsave(filename = "results/13_ApC_Contribucion.png",
         plot = wrap_plots(p13),
         width = 35, height = 30, units = "cm")


  # Fig14. Anexo D: Boxplot de distancias por cluster ------------------------------------

  esq_aux <- data |>
    st_drop_geometry() |>
    pivot_longer(cols = -c(gr), names_to = "db")|>
    left_join(AUX$db_label, by = "db") |>
    mutate(value = round(value/1000, 1),
           name = factor(db,
                         levels = unique(AUX$db_label$db),
                         labels = unique(AUX$db_label$db_lab)),
           CLASE = factor(CLASE,
                         levels = unique(AUX$db_label$CLASE))) |>
    split(~CLASE)

  walk2(esq_aux, letters[1:length(esq_aux)], gg_desc)

  # Fig15. Anexo E: Ubicación espacial según número de cluster ---------------------------

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

    p15 <- bind_cols(data, df_cl)  |>
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
            strip.text.x = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18))

    ggsave(filename = "results/15_ApE_Cluster2-7.png",
           plot = p15,
           width = 48, height = 32, units = "cm")

    rm(i, df_cl, p15)
  }

rm(p01, p02, p03, p04, p05a, p05b, p06, p07,
   p09, p10a, p10b, p11, p12a, p12b, p13)
