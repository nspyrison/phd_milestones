require("DALEX")
require("dplyr")
require("ggplot2")

## Local func
my_boxplot_df <- function(pred_parts, player_tag = "<tag unused>"){
  ## Remade from: iBreakDown:::print.break_down_uncertainty
  data.frame(
    player = player_tag,
    label = tapply(pred_parts$label, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE),
    variable = tapply(pred_parts$variable_name, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE),
    value = tapply(pred_parts$variable_value, paste(pred_parts$label, pred_parts$variable, sep = ": "), unique, na.rm = TRUE), ## oos variable value
    ## Of the distribution of local attributions:
    min    = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), min, na.rm = TRUE),
    q1     = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), quantile, 0.25, na.rm = TRUE),
    median = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), median, na.rm = TRUE),
    q3     = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), quantile, 0.75, na.rm = TRUE),
    max    = tapply(pred_parts$contribution, paste(pred_parts$label, pred_parts$variable, sep = ": "), max, na.rm = TRUE))
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
## Messi SHAP
messi <- X[1, ]
shap_messi <- predict_parts(explainer       = rf_expl,
                            new_observation = messi,
                            type            = "shap",
                            B               = 25L)
box_df_messi <- my_boxplot_df(shap_messi, "Messi")
.lvl_ord <- box_df_messi$variable[order(box_df_messi$median, decreasing = TRUE)]

## Virgil van Dijk SHAP
dijk <- X[8, ]
shap_dijk <- predict_parts(explainer       = rf_expl,
                           new_observation = dijk,
                           type            = "shap",
                           B               = 25L,
                           order = .lvl_ord)
box_df_dijk <- my_boxplot_df(shap_dijk, "van Dijk")
boxplot_df <- rbind(box_df_messi, box_df_dijk)
boxplot_df$variable <- factor(boxplot_df$variable, levels = rev(.lvl_ord))
boxplot_df$Player <- boxplot_df$player

(g1 <- ggplot(boxplot_df, aes(x=variable, color=Player, fill=Player)) +
  geom_boxplot(
    aes(ymin=min, lower=q1, middle=median, upper=q3, ymax=max),
    stat = "identity", alpha =.3) +
    coord_flip() + 
    theme_bw() + 
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer(palette = "Dark2") +
    labs("SHAP Values",
         y = "SHAP value", x = "Variable") +
    theme(legend.margin = margin(0,0,0,0),
          legend.position = "top")
  )


## Messi Breakdown
bd_messi <- predict_parts(explainer       = rf_expl,
                          new_observation = messi,
                          type            = "break_down")
g2 <- plot(bd_messi, max_features = 9) +
  ggtitle("Break Down, Messi")

# ## B is the number of random variable order perms to order the magnetude of, 25 is default
# g3 <- plot(shap_dijk) +
#   ggtitle("SHAP, V. van Dijk")
## Messi Breakdown
bd_dijk <- predict_parts(explainer       = rf_expl,
                         new_observation = dijk,
                         type            = "break_down")
g4 <- plot(bd_dijk, max_features = 9) +
  ggtitle("Break Down, van Dijk",
          "Order of Messi's variables")

### Plot together
require("patchwork")
pw = (g1) / (g2 / g4)

 ## SAVE -----
ggplot2::ggsave("./figures/cheem_fifa_messi_dijk.pdf", pw, device = "pdf",
                width = 4, height = 9, units = "in")
