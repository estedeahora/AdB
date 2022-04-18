# geometria_class ---------------------------------------------------------

geometria_class <- function(gdf){
  clase <- gdf |>
    st_geometry() |>
    class()

  if(any(str_detect(clase, "POINT"))  ) TRUE else FALSE
}

# fviz_centr -------------------------------------------------------------------------

fviz_centr <- function(p){
  p[["layers"]][[2]][["aes_params"]][["colour"]] <- "grey40"
  p[["layers"]][[2]][["aes_params"]][["fill"]] <- NULL
  p[["layers"]][[2]][["mapping"]][["fill"]] <- p[["layers"]][[2]][["mapping"]][["colour"]]
  p[["layers"]][[2]][["mapping"]][["colour"]] <- NULL
  p[["layers"]][[2]][["aes_params"]][["shape"]] <- 21

  return(p)
}

# geom_hull ---------------------------------------------------------------

geom_hull <- function(p, res = res_PCA, grupo = data$gr,
                      axes = 1:2, alpha = 0.15 ) {
  find_hull <- function(df) df[chull(df[[1]], df[[2]]), ]
  res <- res$ind$coord[ , axes] |>
    as_tibble() |>
    add_column(gr = grupo)
  hulls <- plyr::ddply(res, "gr", find_hull)
  names(hulls) <- c("x", "y", "gr")

  p <- p + geom_polygon(data = hulls, color = "grey80",
                        aes(x = x, y = y, fill = gr),
                        alpha = alpha)

  move_layers(p, position = "bottom", idx = top_layer(p))
}


# gg_desc ---------------------------------------------------------

gg_desc <- function(x, subfig = NA){

  p_avg <- x |>
    group_by(name) |>
    summarise(avg = mean(value,  na.rm = T))

  p <- x |>
    ggplot(aes(x = gr, y = value, color = gr)) +
    geom_violin(aes(fill = gr), alpha = 0.3, # adjust = 1.7
                bw = 0.05) +
    geom_boxplot(width = 0.3, alpha = 0.3) +
    geom_hline(data = p_avg, mapping = aes(yintercept = avg),
               color = "tomato4", linetype = 2) +
    facet_wrap(vars(name), scales = "free") +
    scale_x_discrete("") +
    scale_y_continuous("Distancia (Km)", labels = \(x)  sprintf("%.1f", x)) +
    guides(fill = "none") +
    scale_color_discrete("Clúster") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(title = paste0("Distancia a lugar de ", unique(x$CLASE_lab)))

  n_panels <- ggplot_build(p)$data[[1]]$PANEL |>
    unique() |>
    length() |>
    wrap_dims()

  if(is.na(subfig) ) subfig <- ""
  ggsave(filename = paste0(here::here("analysis/results"),
                           "/ApD_", subfig, "_Distancia-",
                           unique(x$CLASE),
                           ".png"),
         plot = p,
         width = n_panels[2] * 13, height = n_panels[1] * 10, units = "cm")
}

