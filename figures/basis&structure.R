library(spinifex)
library(ggplot2)


## Flea holes tour
f_dat  <- tourr::rescale(tourr::flea[, 1:6])
f_cat  <- factor(flea$species)
f_path <- save_history(f_dat, guided_tour(holes()))
f_bas  <- matrix(f_path[,, max(dim(f_path)[3])], ncol=2)
f_mvar <- 5
f_msp  <- create_manip_space(basis = f_bas, manip_var = f_mvar)
f_proj <- data.frame(tourr::rescale(f_dat %*% f_msp[, 1:2]))

# step0, output
# Adjust centering to make axes and data side by side
(out1 <- view_basis(f_bas, lab = colnames(f_dat)) +
  geom_point(data = f_proj,
             mapping = aes(x = X1 + .75, y = X2 - .5, color = f_cat),
             pch = as.integer(f_cat) + 15, size = 2) +
  theme(panel.border = element_rect(colour = "black", fill = NA))
)
ggsave("./figures/basis.png", out1, width = 6, height = 3, units = "in")

# SAVE TO: basis.png
#TODO: + legened?

###### Now make the basis structure figure.
### Remove "tars2"; little change
my_manip_var <- which(colnames(f_dat) == "tars2")
mv_sp <- f_bas[my_manip_var,]
my_phi <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
my_theta <- atan(mv_sp[2] / mv_sp[1]) # Radial

(out2a <- spinifex::oblique_frame(basis = f_bas, data = f_dat, manip_var = my_manip_var, 
                        phi = pi/2 + my_phi, theta = my_theta,
                        col = f_cat, pch = f_cat) + 
  labs(title = "A) Removed 'tars2'                            B) Removed 'aede2'"))


### Renove "aede2"; large change
my_manip_var <- which(colnames(f_dat) == "aede2")
mv_sp <- f_bas[my_manip_var,]
my_phi <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
my_theta <- atan(mv_sp[2] / mv_sp[1]) # Radial


##TODO: Phi ~ pi/2 * sign(mv_sp[2]) * my_phi
(out2b <- spinifex::oblique_frame(basis = f_bas, data = f_dat, manip_var = my_manip_var, 
                        phi = pi/2 + my_phi, theta = my_theta,
                        col = f_cat, pch = f_cat))
gridExtra::grid.arrange(out2a, out2b, nrow = 1)
out2 <- gridExtra::arrangeGrob(out2a, out2b, nrow = 1)


ggsave("./figures/basisStructure.png", out2, width = 6, height = 3, units = "in")





