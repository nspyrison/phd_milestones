require("plotly")
load("../trees_of_cheem/apps/cheem_app/data/0local_funcs.RData")
load("../trees_of_cheem/apps/cheem_app/data/2preprocess_simulation.RData")
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
p <- THIS_linked_plotly_func(shap_layer_ls, 19L, 4L,
                             do_include_maha_qq = FALSE)
p <- p + labs(title = "PC1:2 of data and SHAP spaces", 
              caption = paste0("Predicted class mapped to color & shape\n",
                               "Red circles indicate a misclassified observation\n",
                               "'*' is out observation of interest, 'x' is a nearby comparison")
) + theme(aspect.ratio=1)
## Export static ggplot2 -----
ggplot2::ggsave("./figures/cheem_pca.pdf", plot = p, device = "pdf",
                width = 8, height = 4.8, units = "in")

## Save as interactive plotly
ggp <- ggplotly(p, tooltip = "tooltip") %>%
  config(displayModeBar = FALSE) %>% ## Remove html buttons
  layout(dragmode = "select", showlegend = FALSE) %>% ## Set drag left mouse
  event_register("plotly_selected") %>% ## Reflect "selected", on release of the mouse button.
  highlight(on = "plotly_selected", off = "plotly_deselect")

htmlwidgets::saveWidget(ggp, "./figures/cheem_pca_interactive.html",
                        selfcontained = TRUE)
