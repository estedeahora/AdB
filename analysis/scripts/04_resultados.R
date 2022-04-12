# Datos RADIO ------------------------------------------------------------------

load(here::here("analysis/data/radio.RData"))

# Figuras ----------------------------------------------------------------------

  # Fig01. Evolución de la población -------------------------------------------

  p01 <- AUX$EVO |>
    ggplot(mapping = aes(x = ANO) ) +
    geom_area(mapping = aes(y = POBLACION, fill = "CABA"), alpha = 0.4)+
    geom_area(mapping = aes(y = POB_VILLA, fill = "Villa"), alpha = 0.4) +
    geom_col(mapping = aes(y = POBLACION), width = 1, fill = "tomato3") +
    scale_y_continuous(name = "Población (en millones)",
                       labels = format(seq(0, 3.5, by = 0.5), digits = 1),
                       breaks = seq(0, 3.5, by = 0.5)) +
    scale_x_continuous(name = "Año",
                       breaks = c(AUX$EVO$ANO[is.na(AUX$EVO$POBLACION) == F])) +
    scale_fill_discrete("Población total") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 90, hjust = -1),
          legend.position = "bottom")

  ggsave(filename = "results/01_EVOL-POB.png",
         plot = p01,
         width = 20, height = 12, units = "cm")

  # Fig02. Evolución del stock de viviendas ------------------------------------------------------

  p02 <- AUX$VIV |>
    pivot_longer(cols = -VIV) |>
    mutate(ANO = str_remove(name, "CENSO"),
           ANO = as.numeric(ANO),
           value = value / 1000000) |>
    ggplot(aes(x = ANO, y = value, group = VIV, fill = VIV)) +
    geom_col(alpha = 0.5) +
    scale_y_continuous("Cantidad de viviendas (en millones)",
                       breaks = seq(0, 1.5, by = 0.25 )) +
    scale_x_continuous("Censo",
                       breaks = c(1991, 2001, 2010 )) +
    scale_fill_discrete("Viviendas") +
    theme_minimal() +
    theme(legend.position = "bottom")

  ggsave(filename = "results/02_EVOL_VIV.png",
         plot = p02,
         width = 15, height = 12, units = "cm")

  # Fig03. Entornos urbanos ------------------------------------------------------

  AUX$TH <- c("#D1E107", "#0D9743",
              "#0AE1A7", "#E8EC6E", "#0072B2",
              "#A194FC", "#CA280E", "#F07561", "#FCCB94")

  p03 <- RADIO$REC |>
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

  ggsave(filename = "results/03_Tipo-habitat.png",
         plot = p03,
         width = 25, height = 18, units = "cm")

  # Fig04. Pirámide de población por entorno urbano -------------------------

  p04 <- RADIO$db |>
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

  ggsave(filename = "results/04_Piramide-TH.png",
         plot = p04,
         width = 14, height = 21, units = "cm")

  # Fig05. NBI y MNI ---------------------------------------------------------

  # NBI
  p5_aux <- RADIO$ENT %>%
    left_join(RADIO$db |> select(ID, NBI, SUP), by = "ID") %>%
    pivot_longer(cols = c(NBI, SUP)) |>
    select(ID, name, value)

  p05a <- p5_aux |>
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

  p05b <- p5_aux |>
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

  ggsave(filename = "results/05_NBI-MNI.png",
         plot = p05a + p05b,
         width = 27, height = 15, units = "cm")

  rm(p5_aux)

  # Fig06. Déficit -----------------------------------------------------------

  p06_map <-  RADIO$REC %>%
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


  p06_legend <- bi_legend(pal = "GrPink",
                      dim = 3,
                      xlab = "Déficit cuantitativo",
                      ylab = "Déficit cualitativo",
                      size = 20)

  p06 <- p06_map + (plot_spacer() / p06_legend) +
    plot_layout(ncol=2, widths=c(3,1))
    # ggdraw() +
    # draw_plot(p06_map, 0, 0, 1, 1) +
    # draw_plot(p06_legend, 0, 1, 0.2, 0.2)

  # rgb(30, 0, 50, 1)

  ggsave(filename = "results/06_Deficit.png",
         plot = p06,
         width = 30, height = 25, units = "cm")

  # Tabla déficit por TH

  p06_tab <- RADIO$db |>
    mutate(across(.cols = starts_with("x100H_"),
                  .fns = ~.x * HOGARES / 100) ) |>
    group_by(TIPO_HAB2) |>
    summarise(across(.cols = c(HOGARES, starts_with("x100H_")),
                     .fns = ~sum(.x, na.rm = T))) |>
    mutate(across(.cols = starts_with("x100H_"),
                  .fns = ~.x / HOGARES * 100) ) |>
    select(TIPO_HAB2, starts_with("x100H_"))

  xlsx::write.xlsx(p06_tab, file = "results/06_tabla-deficit.xlsx", row.names = T)

  rm(p06_map, p06_legend, p06_tab)

  # Fig07. Migración  -----------------------------------------------------------

  # scale_params <- data.frame(MIG = c("Paraguay"),
  #                            style = c("bar"))

  p07 <-  RADIO$REC %>%
    left_join(RADIO$db |> select(ID, PERSONAS, starts_with("MIG_")), by = "ID")  |>
    select(ID, starts_with("MIG_")) |>
    pivot_longer(cols = starts_with("MIG_")) |>
    mutate(MIG = str_remove(name, "MIG_"),
           MIG = case_when(MIG == "PAR" ~ "Paraguay",
                           MIG == "BOL" ~ "Bolivia",
                           MIG == "PER" ~ "Perú")
           ) |>
    filter(value > 35) |>
    st_centroid() |>
    ggplot() +
    geom_sf(aes(color = MIG, size = value), alpha = 0.6) +
    geom_sf(data = COMUNA, fill = NA) +
    geom_sf_text(data = COMUNA,
                 color = "grey15",
                 size = 8,
                 aes(label = as.numeric(COMUNA))) +
    annotation_scale(data = data.frame(MIG = c("Bolivia")) ) +
    annotation_north_arrow(width = unit(1, "cm"), location='tr',
                           which_north = "grid",
                           data = data.frame(MIG = c("Perú")) ) +
    facet_wrap(vars(MIG)) +
    scale_size_continuous("Número de migrantes", trans = 'log10') +
    scale_color_futurama(name = "País de origen") +
    guides(size  = guide_legend(order = 1),
           color = "none") +
    theme_void() +
    theme(legend.position = "bottom",
          legend.box="vertical",
          legend.margin=margin(),
          strip.text.x = element_text(size = 12)
          )

  ggsave(filename = "results/07_Migrantes.png",
         plot = p07,
         width = 30, height = 15, units = "cm")

  # Fig09. PBG ---------------------------------------------------------------------

  p09 <- COMUNA |>
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

  ggsave(filename = "results/09_PBG.png",
         plot = p09,
         width = 25, height = 18, units = "cm")

  # Fig10. Mapa de individuos y variables (Dim 1:2) -------------------------

  PCA_aux <- res_PCA[["var"]][["coord"]] |>
    as_tibble(rownames = "db") |>
    left_join(AUX$db_label, by = "db")

  p10a <- fviz_pca_var(res_PCA, geom = F, title = "") +
    geom_segment(aes(xend = Dim.1, yend = Dim.2, color = CLASE_lab),
                 x = 0, y = 0, data = PCA_aux,
                 arrow = arrow(length = unit(0.2, "cm"))) +
    geom_text_repel(data = PCA_aux,
                    aes(x = Dim.1, y = Dim.2,
                        label = db, color = CLASE_lab )) +
    scale_color_uchicago() +
    labs(color = "") +
    theme(legend.position = "bottom")

  p10b <- fviz_mfa_ind(res_PCA, geom = c("point"), title = "",
                     alpha.ind = 0.1, habillage = data$gr) +
      theme(legend.position = "bottom")

  p10b <- p10b |>
    fviz_centr() |>
    geom_hull(axes = 1:2) +
    labs(fill = "Clúster") +
    guides(color = "none")

  ggsave(filename = "results/10_Variables-Individuos_1-2.png",
         plot = p10a + p10b,
         width = 45, height = 23, units = "cm")

  rm(PCA_aux)

  # Fig11. Ubicación espacial de cluster ------------------------------------

  p11 <- data |>
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

  ggsave(filename = paste0("results/11_Mapa-Cluster", AUX$n_cl,".png"),
         plot = p11,
         width = 23, height = 23, units = "cm")

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
