## Requirments ----
require("plotly")
require("spinifex")
load("../trees_of_cheem/apps/cheem_app/data/0local_funcs.RData")
load("../trees_of_cheem/apps/cheem_app/data/2preprocess_simulation.RData")
shap_obs <- 18L; comp_obs <- 111L;

## PCA of data/SHAP -----
THIS_linked_plotly_func <- function(
  layer_ls,
  shap_obs = NULL,
  comp_obs = NULL,
  height_px = 640L,
  width_px = 640L,
  do_include_maha_qq = FALSE
){
  .alpha <- ifelse(nrow(layer_ls$decode_df) > 999L, .1, .6)
  .xlab <- ifelse(do_include_maha_qq == FALSE, "PC1",
                  "PC1 | Quantile, chi-squared")
  .ylab <- ifelse(do_include_maha_qq == FALSE, "PC2",
                  "PC2 | Quantile, observed Mahalanobis distance")
  ## Remove QQ maha rows if needed
  plot_df <- layer_ls$plot_df ## Init
  if(do_include_maha_qq == FALSE){
    plot_df <- layer_ls$plot_df[
      layer_ls$plot_df$projection_nm != "QQ Mahalanobis distance", ]
    height_px <- height_px / 2L ## Half height display as qq maha is removed.
  }
  is_classification <- attr(layer_ls, "problem_type") == "classification"
  # ifelse("is_misclassified" %in% colnames(layer_ls$decode_df), TRUE, FALSE)
  pred_clas <- as.factor(FALSE) ## If regression; dummy pred_clas
  if(is_classification == TRUE) pred_clas <-
    layer_ls$decode_df$predicted_class %>%
    rep_len(nrow(plot_df)) %>%
    as.factor()
  
  gg <- plot_df %>%
    plotly::highlight_key(~rownum) %>%
    ggplot(aes(V1, V2))
  ## Red misclassified points, if present
  if(is_classification == TRUE){
    .rn_misclass <- which(layer_ls$decode_df$is_misclassified == TRUE)
    .idx_misclas <- plot_df$rownum %in% .rn_misclass
    if(sum(.idx_misclas) > 0L){
      .df <- plot_df[.idx_misclas, ] %>% plotly::highlight_key(~rownum)
      gg <- gg +
        geom_point(aes(V1, V2), .df,
                   color = "red", fill = NA,
                   shape = 21L, size = 3L, alpha = .alpha)
    }
  }
  ## Highlight comparison obs, if passed
  if(is.null(comp_obs) == FALSE){
    .idx_comp <- plot_df$rownum == comp_obs
    if(sum(.idx_comp) > 0L){
      .df <- plot_df[.idx_comp, ] %>% plotly::highlight_key(~rownum)
      gg <- gg +
        ## Highlight comparison obs
        geom_point(aes(V1, V2, color = pred_clas[.idx_comp]),
                   .df, size = 4L, shape = 4L)
    }
  }
  ## Highlight shap obs, if passed
  if(is.null(shap_obs) == FALSE){
    .idx_shap <- plot_df$rownum == shap_obs
    if(sum(.idx_shap) > 0L){
      .df <- plot_df[.idx_shap, ] %>% plotly::highlight_key(~rownum)
      gg <- gg +
        geom_point(aes(V1, V2, color = pred_clas[.idx_shap]),
                   .df, size = 5L, shape = 8L)
    }
  }
  ## Maha skew text,
  #### geom_text not working with plotly... & annotate() not working with facets...
  if(do_include_maha_qq == TRUE){
    gg <- gg +
      geom_text(aes(x = -Inf, y = Inf, label = ggtext),
                hjust = 0L, vjust = 1L, size = 4L)
  }
  ## Normal points
  gg <- gg +
    suppressWarnings(geom_point(
      aes(V1, V2, color = pred_clas, shape = pred_clas, tooltip = tooltip),
      alpha = .alpha)) +
    facet_grid(rows = vars(projection_nm), cols = vars(layer_nm), scales = "free") +
    theme_bw() +
    labs(x = .xlab, y = .ylab) +
    scale_color_brewer(palette = "Dark2") +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "off")
  return(gg)
}
p <- THIS_linked_plotly_func(shap_layer_ls, shap_obs, comp_obs,
                             do_include_maha_qq = FALSE)
p <- p + 
  labs(title = "PC1:2 of data and SHAP spaces",
       caption = paste0("Predicted class mapped to color & shape\n",
                        "Red circles indicate a misclassified observation\n",
                        "'*' is an observation of interest\n",
                        "'x' is comparison observation, nearby in the data, but distant in the SHAP space")
) + theme(aspect.ratio=1)
### Export static & interactive PCA data/shap of sim -----
## Save static png
ggplot2::ggsave("./figures/cheem_pca.pdf", plot = p, device = "pdf",
                width = 8, height = 4.8, units = "in")

## Save interactive html widget
ggp <- ggplotly(p, tooltip = "tooltip") %>%
  config(displayModeBar = FALSE) %>% ## Remove html buttons
  layout(dragmode = "select", showlegend = FALSE) %>% ## Set drag left mouse
  event_register("plotly_selected") %>% ## Reflect "selected", on release of the mouse button.
  highlight(on = "plotly_selected", off = "plotly_deselect")
htmlwidgets::saveWidget(ggp, "./figures/cheem_pca_widget.html",
                        selfcontained = TRUE)

## 1D manual tour of cheem ----
.df <- shap_layer_ls$shap_df
bas <- .df[shap_obs, -ncol(.df)] %>%
  as.matrix(nrow = 1L) %>% t() %>%
  tourr::normalise()
.opts <- rownames(bas)
mv_nm <- .opts[spinifex::manip_var_of(bas)]

ggt142 <- manual_tour1d_func(
  shap_layer_ls, bas, mv_nm,
  shap_obs, comp_obs,
  do_add_pcp_segements = TRUE,
  pcp_shape = c(142, 124), ## '|' plotly and gganimate respectively
  angle = 1.1)
anim <- animate_plotly(ggt142) %>% layout(dragmode = FALSE, showlegend = FALSE) %>% ## Set drag left mouse
  event_register("plotly_selected") %>% ## Reflect "selected", on release of the mouse button.
  highlight(on = "plotly_selected", off = "plotly_deselect")
htmlwidgets::saveWidget(anim, "./figures/cheem_manualtour_widget.html",
                        selfcontained = TRUE)

ggt124 <- manual_tour1d_func(
  shap_layer_ls, bas, mv_nm,
  shap_obs, comp_obs,
  do_add_pcp_segements = TRUE,
  pcp_shape = 124,
  angle = 1.3)
(gg <- spinifex::filmstrip(ggt124))
ggplot2::ggsave("./figures/cheem_manualtour.pdf", plot = gg, device = "pdf",
                width = 8, height = 9, units = "in")

