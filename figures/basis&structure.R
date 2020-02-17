library(spinifex)
library(ggplot2)

data("TIPS")

## Flea holes tour
w_dat  <- tourr::rescale(wine[,2:14])
w_cat  <- factor(wine$Type)
w_path <- save_history(w_dat, guided_tour(holes()))
w_bas  <- matrix(w_path[,, max(dim(w_path)[3])], ncol=2)
w_mvar <- 5
w_msp  <- create_manip_space(basis = w_bas, manip_var = w_mvar)
w_proj <- data.frame(tourr::rescale(w_dat %*% w_msp[, 1:2]))

# step0, output
# Adjust centering to make axes and data side by side
(out1 <- view_basis(w_bas, lab = colnames(w_dat)) +
  geom_point(data = w_proj,
             mapping = aes(x = X1 + .75, y = X2 - .5, color = w_cat),
             pch = as.integer(w_cat) + 15, size = 2)) +
  theme(panel.border = element_rect(colour = "black", fill = NA))
ggsave("./figures/basisWINE.png", out1, width = 6, height = 3, units = "in")

# SAVE TO: basis.png
#TODO: + legened?

###### Now make the basis structure figure.
### Remove "Color"; little change
my_manip_var <- 10
colnames(w_dat)[my_manip_var]
mv_sp <- w_bas[my_manip_var,]
my_phi <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
my_theta <- atan(mv_sp[2] / mv_sp[1]) # Radial

(out2a <- spinifex::oblique_frame(basis = w_bas, data = w_dat, manip_var = my_manip_var, 
                        phi = pi/2 + my_phi, theta = my_theta,
                        col = w_cat, pch = w_cat) + 
  labs(title = "A) Removed 'Color'                            B) Removed 'Hue'"))


### Renove "Hue"; large change
my_manip_var <- 11
colnames(w_dat)[my_manip_var]
mv_sp <- w_bas[my_manip_var,]
my_phi <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
my_theta <- atan(mv_sp[2] / mv_sp[1]) # Radial


##TODO: Phi ~ pi/2 * sign(mv_sp[2]) * my_phi
(out2b <- spinifex::oblique_frame(basis = w_bas, data = w_dat, manip_var = my_manip_var, 
                        phi = pi/2 - my_phi, theta = my_theta,
                        col = w_cat, pch = w_cat))
gridExtra::grid.arrange(out2a, out2b, nrow = 1)
out2 <- gridExtra::arrangeGrob(out2a, out2b, nrow = 1)


ggsave("./figures/basisStructureWINE.png", out2, width = 6, height = 3, units = "in")





