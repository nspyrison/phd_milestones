require("DALEX")
require("dplyr")
require("ggplot2")

## Local func
my_parts_boxplot_df <- function(pred_parts, player_tag = "<tag unused>"){
  ## Remade from: iBreakDown:::print.break_down_uncertainty
  data.frame(
    player = player_tag,
    label  = tapply(pred_parts$label, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE),
    variable = tapply(pred_parts$variable_name, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE),
    value  = tapply(pred_parts$variable_value, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE), ## oos variable value
    ## Of the distribution of local attributions:
    min    = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), min, na.rm = TRUE),
    q1     = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), quantile, 0.25, na.rm = TRUE),
    median = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), median, na.rm = TRUE),
    q3     = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), quantile, 0.75, na.rm = TRUE),
    max    = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), max, na.rm = TRUE))
}
my_parts_distribution <- function(pred_parts, player_tag = "<tag unused>"){
  df <- data.frame(
    player = player_tag,
    label = pred_parts$label,
    variable = pred_parts$variable_name,
    value = pred_parts$variable_value, ## Obs value of X
    contribution = pred_parts$contribution, ## SHAP contribution
    B_perm_num = pred_parts$B
  )
  rownames(df) <- paste(pred_parts$label, pred_parts$variable, pred_parts$B, sep = ": ")
  return(df)
}
.lvl_ord <- c("react", "off", "mvm", "def", "pwr", "acc", "bmi", "age", "gk")
my_bd_df <- function(break_down, player_tag = "<tag unused>"){
  df <- data.frame(
    player = player_tag,
    label = break_down$label,
    variable = break_down$variable_name,
    contribution = break_down$contribution, ## SHAP contribution
    cumulative = break_down$cumulative, ## Cumulative SHAP contribution
    sign = break_down$sign
  )
  .n <- nrow(df)
  df$variable[is.na(df$variable)|df$variable==""] <- "prediction"
  df$variable <- factor(
    df$variable, rev(c("intercept", .lvl_ord, "prediction")))
  df$cumulative <- (df$cumulative - min(df$cumulative)) /
    (max(df$cumulative) - min(df$cumulative))
  df$last_cumulative <- c(NA, df$cumulative[-.n])
  rownames(df) <- paste(break_down$label, break_down$variable, break_down$B, sep = ": ")
  return(df)
}


### Create FIFA x ------
.raw <- DALEX::fifa
.dat_less_ys <- .raw %>%
  dplyr::select(-c(`nationality`, ## useless class
                   `overall`, `potential`, `value_eur`, `wage_eur`)) %>% ## potential target vars.
  as.data.frame()

if(F) ## View corrplot?
  corrplot::corrplot(cor(.dat_less_ys),
                     method = "circle", ## geom
                     type = "upper", ## only upper triangle
                     diag = F, ## remove auto correlation
                     order = "FPC", ## First principal component
                     tl.col = "black", tl.srt = 90, ## Text label color and rotation
                     tl.pos = "td")

## Munging aspects
#### Agg some highly correlated vars.
dat <- .dat_less_ys %>%
  dplyr::mutate(
    .keep = "none",
    bmi = (weight_kg+(height_cm/100L)^2L),
    age = age,
    react = movement_reactions,
    off = (attacking_finishing+skill_long_passing+attacking_volleys+
             power_long_shots+skill_curve+mentality_positioning+attacking_crossing+
             attacking_short_passing+skill_dribbling+skill_ball_control)/10L,
    def = (defending_sliding_tackle+mentality_interceptions+
             defending_standing_tackle+defending_marking+mentality_aggression)/5L,
    acc = (attacking_heading_accuracy+power_shot_power)/2L,
    mvm = (movement_sprint_speed+movement_balance+movement_acceleration+
             mentality_vision+mentality_composure+movement_agility+
             mentality_penalties+skill_fk_accuracy+power_stamina+movement_reactions)/10L,
    pwr = (power_strength+power_jumping)/2L,
    gk = (goalkeeping_diving+goalkeeping_positioning+goalkeeping_reflexes+
            goalkeeping_handling+goalkeeping_kicking)/5L
  )
## Class for the position of the player, eiter "fielder" or "goalkeeper"
position <- clas <- dplyr::case_when(
  dat$gk <= 40L ~ "fielder",
  dat$gk >  40L ~ "goalkeeper") %>%
  factor(levels = c("fielder", "goalkeeper"))

## Starting with 42 variables, we remove `nationality`, and some potential Y vars,
#### and aggregate into 9 aggregate 'aspect' dimensions based on var correlation 
X <- dat ## 9 aspects of the X's
Y <- .raw$wage_eur ## unscaled wages in Euros, assumed 2020 valuation.

## Create same RF used downstream -----
.is_y_disc <- FALSE ## regressing on continuous wages
.hp_ntrees <- sqrt(nrow(X))
.hp_mtry <- if(.is_y_disc == TRUE) sqrt(ncol(X)) else ncol(X) / 3L
.hp_node <- if(.is_y_disc == TRUE) 1L else 5L
.hp_node <- max(.hp_node, nrow(X) / 500L)

rf_mod <- randomForest::randomForest(Y~., data = data.frame(Y, X),
                                     mtry = .hp_mtry,
                                     nodesize = .hp_node,
                                     ntrees = .hp_ntrees)
## DALEX parts
rf_expl <- DALEX::explain(model = rf_mod,
                          data  = X,
                          y     = Y,
                          label = "Random Forest")

## SHAP values & plot ----
## Messi SHAP
messi <- X[1, ]
shap_messi <- predict_parts(explainer       = rf_expl,
                            new_observation = messi,
                            type            = "shap",
                            B               = 25L)
shap_messi$contribution <- shap_messi$contribution %>%
  spinifex::scale_01()
box_df_messi <- my_parts_boxplot_df(shap_messi, "Messi")

## Virgil van Dijk SHAP
dijk <- X[8, ]
shap_dijk <- predict_parts(explainer       = rf_expl,
                           new_observation = dijk,
                           type            = "shap",
                           B               = 25L,
                           order = .lvl_ord)
shap_dijk$contribution <- shap_dijk$contribution %>%
  spinifex::scale_01()
box_df_dijk <- my_parts_boxplot_df(shap_dijk, "van Dijk")

## Bind shap aggs:
# .lvl_ord <- box_df_messi$variable[order(box_df_messi$median, decreasing = TRUE)]
# ord_df <- boxplot_df %>% select(player, variable, median) %>%
#   tidyr::pivot_wider(names_from = "player", values_from = "median") %>%
#   mutate(sum = Messi + `van Dijk`)
# # .lvl_ord <- ord_df$variable[order(ord_df$sum, decreasing = TRUE)] %>%
# #   as.character()
boxplot_df <- rbind(box_df_messi, box_df_dijk)
boxplot_df$variable <- factor(boxplot_df$variable, levels = rev(.lvl_ord))

## B Distributions of the SHAPS
dist_shap_messi <- my_parts_distribution(shap_messi, "Messi")
dist_shap_dijk <- my_parts_distribution(shap_dijk, "van Dijk")
dist_df <- rbind(dist_shap_messi, dist_shap_dijk)
dist_df$variable <- factor(dist_df$variable, levels = rev(.lvl_ord))

(g_shap <- ggplot(boxplot_df) +
    ## Connecting grey line
    geom_segment(aes(x=Messi, xend=`van Dijk`, y=variable, yend=variable),
                 alpha =.7, size = 2L, color = "grey", fill = NA,
                 data = boxplot_df %>% select(player, variable, median) %>%
                   tidyr::pivot_wider(names_from = "player", values_from = "median") %>% 
                   mutate(sum = Messi + `van Dijk`)) +
    geom_point(aes(x=median, y=variable, color=player, fill=player),
               alpha =.7, size = 5L) +
    ## Shap distributions
    geom_point(aes(x=contribution, y=variable, color=player, fill=player),
               dist_df, alpha =.8, size = 3, shape = 124,
               position = position_dodge(-.5)) +
  # geom_boxplot(
  #   aes(ymin=min, lower=q1, middle=median, upper=q3, ymax=max),
  #   stat = "identity", alpha =.3) +
    theme_bw() + 
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    labs(title="SHAP values",
         y = "variable", x = "SHAP values, normalized\n The median of the contributions while permuting X's") +
    theme(legend.position = "off")
  )

## Breakdowns & plot ----
## Messi Breakdown
bd_messi <- predict_parts(explainer       = rf_expl,
                          new_observation = messi,
                          type            = "break_down",
                          order= .lvl_ord)
bd_df_messi <- my_bd_df(bd_messi, "Messi")
## Dijk Breakdown
bd_dijk <- predict_parts(explainer       = rf_expl,
                         new_observation = dijk,
                         type            = "break_down",
                         order = .lvl_ord)
bd_df_dijk <- my_bd_df(bd_dijk, "van Dijk")
## Bind, by row
bd_df <- rbind(bd_df_messi, bd_df_dijk)
bd_df <- bd_df[is.na(bd_df$variable) == FALSE, ]
(g_bd <- ggplot(bd_df) +
  geom_segment(aes(x=cumulative, xend=last_cumulative, y=variable, yend=variable, color=player),
               size=1.5, alpha=.8) + facet_grid(col=vars(player))+
  theme_bw() + 
  scale_color_brewer(palette = "Dark2") +
  labs(title="Breakdown profile of predictions",
       y = "variable", x = "contribution to prediction | variable order") +
  theme(legend.margin = margin(0,0,0,0),
        legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()))

## Relative wages and patchwork
wages_df <- tibble::tibble(
  player = factor(c("Messi", "van Dijk")),
  wages = .raw$wage_eur[c(1L, 8L)])
(g_wage <- ggplot(wages_df, aes(wages, player, xend=0, yend=player, color = player)) +
    geom_segment(size=3L) +
    theme_bw() +
    scale_color_brewer(palette = "Dark2") +
    labs(y = "player", x = "wages [2020 Euros]") +
    theme(legend.position = "off"))
### Plot together
require("patchwork")
(pw <- g_wage / g_shap / g_bd +
    plot_layout(heights = c(1, 3, 3)))

 ## SAVE -----
if(F){
  ggplot2::ggsave("./figures/cheem_fifa_messi_dijk.pdf", 
                  pw, device = "pdf", width = 6, height = 7, units = "in")
  ggplot2::ggsave("./_slides/slide_figures/cheem_fifa_messi_dijk.png",
                  pw, device = "png", width = 6, height = 7, units = "in")
}