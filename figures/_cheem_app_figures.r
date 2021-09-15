## Requirements ----
{
  require("plotly")
  require("spinifex")
  load("../trees_of_cheem/apps/cheem_app/data/0local_funcs.RData")
  load("../trees_of_cheem/apps/cheem_app/data/2preprocess_simulation.RData")
  shap_obs <- 18L; comp_obs <- 111L;
  
  ## Local functions from cheem app -----
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
  
  THIS_manual_tour1d_func <- function(tour_array,
                                      layer_ls, shap_obs, comp_obs = NULL,
                                      do_add_basis_distri = FALSE,
                                      do_add_pcp_segements = FALSE,
                                      pcp_shape = c(142, 124), ## '|' plotly and gganimate respectively
                                      angle = .2
  ){
    ## Initialization
    .y <- layer_ls$decode_df$y %>% matrix(ncol = 1L)
    .col_idx <- which(!(
      colnames(layer_ls$decode_df) %in% 
        c("rownum", "class", "y", "prediction", "residual",
          "predicted_class", "is_misclassified", "tooltip")
    ))
    .x <- layer_ls$decode_df[, .col_idx] ## Numeric X variables
    
    ## Problem type: classification or regression?
    problem_type <- function(y){
      if(length(unique(y)) < 5L) return("classification")
      if(is.numeric(y) == TRUE) return("regression")
      stop("y was expected as a with less than 5 unique values, or numeric indicating a classification or regression problem respectivly.")
    }
    .prob_type <- problem_type(.y) ## Either "classification" or "regression"
    .pred_clas <- as.factor(FALSE) ## Initialize dummy predicted class
    if(.prob_type == "classification")
      .pred_clas <- layer_ls$decode_df$predicted_class
    .alpha <- ifelse(nrow(layer_ls$decode_df) > 999L, .1, 1L)
    
    # ## Manual (radial) tour 1d
    # .mv <- which(colnames(layer_ls$shap_df) == mv_name)
    # .mt_path <- manual_tour(basis, manip_var = .mv)
    
    tour_array
    ### Classification problem -----
    if(.prob_type == "classification"){
      .dat <- .x %>% scale_01
      ggt <- ggtour(tour_array, .dat, angle = angle) +
        proto_density(aes_args = list(color = .pred_clas, fill = .pred_clas)) +
        proto_origin1d() +
        proto_basis1d(manip_col = "black")## leaves space
      if(do_add_basis_distri == TRUE){
        ggt  <- ggt + proto_basis1d_distribution(
          layer_ls, group_by = .pred_clas,
          position = "top1d",
          shape = pcp_shape, ## '|' for gganimate/ggplot
          do_add_pcp_segements = as.logical(do_add_pcp_segements),
          shap_obs = shap_obs,
          comp_obs = comp_obs)
      }
      ggt <- ggt +
        ## Highlight comparison obs, if passed
        proto_highlight1d(comp_obs,
                          list(color = .pred_clas),
                          list(linetype = 3L, alpha = 0.8)) +
        ## Highlight shap obs
        proto_highlight1d(shap_obs,
                          list(color = .pred_clas),
                          list(linetype = 2L, alpha = .6, size = .8))
    }
    
    ## Return the static ggtour, animate in app
    return(ggt)
  }
}

## Create ggplot
p <- THIS_linked_plotly_func(shap_layer_ls, shap_obs, comp_obs,
                             do_include_maha_qq = FALSE)
p <- p + 
  labs(title = "PC1:2 of data and SHAP spaces")


## Export static & interactive PCA data/shap of sim -----
## Save static png
if(F){
  ggplot2::ggsave("./figures/cheem_pca.pdf",
                  plot = p + theme(aspect.ratio=1), device = "pdf",
                  width = 8, height = 4.8, units = "in")
  ggplot2::ggsave("./_slides/slide_figures/cheem_pca.png",
                  plot = p + theme(aspect.ratio=1), device = "png",
                  width = 8, height = 4.8, units = "in")
}

## Save interactive html widget
ggp <- ggplotly(p, tooltip = "tooltip") %>%
  config(displayModeBar = FALSE) %>% ## Remove html buttons
  layout(dragmode = "select", showlegend = FALSE) %>% ## Set drag left mouse
  event_register("plotly_selected") %>% ## Reflect "selected", on release of the mouse button.
  highlight(on = "plotly_selected", off = "plotly_deselect")
if(F)
  htmlwidgets::saveWidget(ggp, "./figures/cheem_pca_widget.html",
                          selfcontained = TRUE)

## 1D manual tour of cheem ----
.df <- shap_layer_ls$shap_df
bas <- .df[shap_obs, -ncol(.df)] %>%
  as.matrix(nrow = 1L) %>% t() %>%
  tourr::normalise()
mv <- 2L#spinifex::manip_var_of(bas)
.opts <- rownames(bas)
mv_nm <- .opts[mv]

ggt142 <- manual_tour1d_func(
  shap_layer_ls, bas, mv_nm,
  shap_obs, comp_obs,
  do_add_pcp_segements = TRUE,
  pcp_shape = c(142, 124), ## '|' plotly and gganimate respectively
  angle = .2)
(anim <-
    animate_plotly(ggt142) %>% layout(dragmode = FALSE, showlegend = FALSE) %>% ## Set drag left mouse
    event_register("plotly_selected") %>% ## Reflect "selected", on release of the mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect"))
if(F)
  htmlwidgets::saveWidget(anim, "./figures/cheem_radialtour_widget.html",
                          selfcontained = TRUE)
### SWAPPING shap and comp obs
bas_swap <- .df[comp_obs, -ncol(.df)] %>%
  as.matrix(nrow = 1L) %>% t() %>%
  tourr::normalise()
ggt142_swapped <- manual_tour1d_func(
  shap_layer_ls, bas_swap, mv_nm,
  shap_obs = comp_obs, comp_obs = shap_obs,
  do_add_pcp_segements = TRUE,
  pcp_shape = c(142, 124), ## '|' plotly and gganimate respectively
  angle = .2) + ggtitle("Comparison point's SHAP")
(anim_swapped <-
    animate_plotly(ggt142_swapped) %>% layout(dragmode = FALSE, showlegend = FALSE) %>% ## Set drag left mouse
    event_register("plotly_selected") %>% ## Reflect "selected", on release of the mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect"))
if(F)
  htmlwidgets::saveWidget(anim_swapped, "./figures/cheem_radialtour_swapped_widget.html",
                          selfcontained = TRUE)
  
# ggt124 <- manual_tour1d_func(
#   shap_layer_ls, bas, mv_nm,
#   shap_obs, comp_obs,
#   do_add_pcp_segements = TRUE,
#   pcp_shape = 124,
#   angle = 1.3)
# 
# debugonce(spinifex::filmstrip)
# (gg <- spinifex::filmstrip(ggt124))
# ggplot2::ggsave("./figures/cheem_radialtour.pdf", plot = gg, device = "pdf",
#                 width = 8, height = 9, units = "in")

### Manually make the frames -----

## Frames 1 at a time; init:
{
  array_dummy <- bas
  attr(array_dummy, "manip_var") <- mv
  manual_tour(array_dummy, mv)
  
  ## Initial, with distribution
  fr1_with <- THIS_manual_tour1d_func(
    tour_array = array_dummy,
    shap_layer_ls,
    shap_obs, comp_obs,
    do_add_basis_distri = TRUE,
    do_add_pcp_segements = TRUE,
    pcp_shape = 124L, ## '|' plotly and gganimate respectively
    angle = .2) + 
    theme(legend.position = "off") + 
    ggtitle(waiver(), subtitle = "Initial contribution")
  ## Initial, with/out distribution
  fr1_wo <- THIS_manual_tour1d_func(
    tour_array = array_dummy,
    shap_layer_ls,
    shap_obs, comp_obs,
    do_add_basis_distri = FALSE,
    do_add_pcp_segements = FALSE,
    pcp_shape = 124L, ## '|' plotly and gganimate respectively
    angle = .2) + 
    theme(legend.position = "off") + 
    ggtitle(waiver(), subtitle = "Initial contribution")
  msp <- create_manip_space(bas, mv)
  ## Full contribution
  bas2 <- rotate_manip_space(msp, 0, .98)
  attr(bas2, "manip_var") <- mv
  (fr2 <- THIS_manual_tour1d_func(
    tour_array = bas2,
    shap_layer_ls,
    shap_obs, comp_obs,
    FALSE, FALSE,
    pcp_shape = 124L, ## '|' plotly and gganimate respectively
    angle = .2) +
    theme(legend.position = "off") +
    ggtitle(waiver(), subtitle = "Full contribution"))
  ## Zero contribution
  bas3 <- rotate_manip_space(msp, 0, -.49)
  attr(bas3, "manip_var") <- mv
  (fr3 <- THIS_manual_tour1d_func(
    tour_array = bas3,
    shap_layer_ls,
    shap_obs, comp_obs,
    FALSE, FALSE,
    pcp_shape = 124L, ## '|' plotly and gganimate respectively
    angle = .2) +
    theme(legend.position = "off") +
    ggtitle(waiver(), subtitle = "Zero contribution"))
  
  require("patchwork")
  pw <- (fr1_wo + fr2 + fr3)
}
  
if(F){
  ## Frame 1 with distribution
  ggplot2::ggsave("./figures/cheem_radialtour_initial.pdf", 
                  fr1_with, device = "pdf", width = 8, height = 4, units = "in")
  ## Rotating points without distr.
  ggplot2::ggsave("./figures/cheem_radialtour_endpts.pdf", 
                  pw, device = "pdf", width = 8, height = 4, units = "in")
  ggplot2::ggsave("./_slides/slide_figures/cheem_radialtour_endpts.png",
                  pw, device = "png", width = 8, height = 4, units = "in")
}