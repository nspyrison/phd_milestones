require("DALEX")
require("dplyr")
require("ggplot2")

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
    bdy = (weight_kg+(height_cm/100L)^2L)/2L, ## bmi wasn't working well after 01 scaling.
    age = age,
    react = movement_reactions,
    atk = (attacking_finishing+skill_long_passing+attacking_volleys+
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
messi <- X[1, ]
## Messi Shap
shap_messi <- predict_parts(explainer       = rf_expl,
                            new_observation = messi,
                            type            = "shap",
                            B               = 25L) 
## B is the number of random variable order perms to order the magnetude of, 25 is default
g1 <- plot(shap_messi) +
  ggtitle("SHAP, L. Messi")
## Messi Breakdown
bd_messi <- predict_parts(explainer       = rf_expl,
                          new_observation = messi,
                          type            = "break_down")
g2 <- plot(bd_messi, max_features = 9) +
  ggtitle("Break Down, L. Messi")
## Virgil Van Dijk
dijk <- X[8, ]
shap_dijk <- predict_parts(explainer       = rf_expl, 
                           new_observation = dijk, 
                           type            = "shap",
                           B               = 25L,
                           order = c("react", "atk", "mvm", "acc",
                                     "age", "def", "bdy", "pwr", "gk")) 
## B is the number of random variable order perms to order the magnetude of, 25 is default
g3 <- plot(shap_dijk) +
  ggtitle("SHAP, V. van Dijk")
## Messi Breakdown
bd_dijk <- predict_parts(explainer       = rf_expl,
                         new_observation = dijk,
                         type            = "break_down",
                         order = c("react", "atk", "mvm", "acc",
                                   "age", "def", "bdy", "pwr", "gk"))
g4 <- plot(bd_dijk, max_features = 9) +
  ggtitle("Break Down, V. van Dijk",
          "Order of Messi's variables")

### Plot together
require("patchwork")
(g1 + g2) / (g3 + g4)

## SAVE -----
ggplot2::ggsave("./figures/cheem_fifa_messi_dijk.pdf", device = "pdf",
                width = 8, height = 5, units = "in")
