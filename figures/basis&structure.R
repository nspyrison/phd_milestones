## USE FIRST CRAN VERSION SEEING ODD THINGS WITH OBLIQUE_FRAME() 18/2/2020.
if (F) {
  devtools::install_version("spinifex", version = "0.1.0", repos = "http://cran.us.r-project.org")
  #devtools::install_github("nspyrison/spinifex") # goes to current dev ver.
 }


library(spinifex)
library(ggplot2)
#?spinifex::oblique_frame() # only in dev version.

## Flea holes tour
f_dat  <- tourr::rescale(tourr::flea[, 1:6])
f_cat  <- factor(flea$species)


if (T) { #do run Di's example
  #di's flea basis (18/02 email)
  di_f_bas <- matrix(c(00.700, -0.498,
                       -0.265, -0.521,
                       00.278, -0.141,
                       00.260,  00.632,
                       00.084, -0.157,
                       00.535,  00.184), ncol = 2, byrow = T)
  f_proj <- data.frame(tourr::rescale(f_dat %*% di_f_bas))
  ##
  
  (out1 <- view_basis(di_f_bas, lab = abbreviate(colnames(f_dat),3)) +
      geom_point(data = f_proj,
                 mapping = aes(x = X1 + .75, y = X2 - .5, color = f_cat),
                 pch = as.integer(f_cat) + 15, size = 2) +
      theme(panel.border = element_rect(colour = "black", fill = NA))
  )
  ggsave("./figures/basis.png", out1, width = 6, height = 3, units = "in")
  
  
  ## "tars1" is sensitive
  mv <- which(colnames(f_dat) == "tars1")
  tr1_bas <- manual_tour(basis = di_f_bas, manip_var = mv)[,, 45]
  tr1_proj <- data.frame(tourr::rescale(f_dat %*% tr1_bas))
  
  ## "tars2" is NOT sensitive
  mv <- which(colnames(f_dat) == "tars2")
  play_manual_tour(data = f_dat, basis = di_f_bas, manip_var = mv)
  tr2_bas <- manual_tour(basis = di_f_bas, manip_var = mv)[,, 53]
  tr2_proj <- data.frame(tourr::rescale(f_dat %*% tr2_bas))
  
  (out2a <- view_basis(basis = tr1_bas, axes = "bottomleft",
                       lab = abbreviate(colnames(f_dat), 3)) +
      geom_point(data = tr1_proj,
                 mapping = aes(x = X1 - .5, y = X2 - .5, color = f_cat),
                 pch = as.integer(f_cat) + 15, size = 2) +
      labs(title = "A) Removed 'tr1', is sensitive")
  )
  
  (out2b <- view_basis(basis = tr2_bas, axes = "bottomleft",
                       lab = abbreviate(colnames(f_dat), 3)) +
      geom_point(data = tr2_proj,
                 mapping = aes(x = X1 - .5, y = X2 - .5, color = f_cat),
                 pch = as.integer(f_cat) + 15, size = 2) +
      labs(title = "B) Removed 'tr2', not sensitive")
  )
  gridExtra::grid.arrange(out2a, out2b, nrow = 1)
  out2 <- gridExtra::arrangeGrob(out2a, out2b, nrow = 1)

  ggsave("./figures/basisStructure.png", out2, width = 8, height = 4, units = "in")
}



### BUT, 
if (F) { #don't run old ver
  
  f_bas <- f_bas_backup <- matrix(c(00.75, -0.02, # holes index optimized
                                    00.02, 00.37,
                                    00.05, 00.41,
                                    -0.07, 00.46,
                                    00.63, 00.22,
                                    -0.18, 00.66), ncol = 2, byrow = T)
  
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

play_manual_tour(data = f_dat, basis = f_bas, manip_var = which(colnames(f_dat) == "tars2"), 
                 col = f_cat, pch = f_cat, axes = "bottomleft")

(out2a <- spinifex::oblique_frame(basis = f_bas, data = f_dat, manip_var = my_manip_var, 
                        phi = pi/2 + my_phi, theta = my_theta,
                        col = f_cat, pch = f_cat) + 
  labs(title = "A) Removed 'tars2'                            B) Removed 'aede2'"))


### Renove "aede2"; large change
my_manip_var <- which(colnames(f_dat) == "aede2")
mv_sp <- f_bas[my_manip_var,]
my_phi <- acos(sqrt(mv_sp[1]^2 + mv_sp[2]^2))
my_theta <- atan(mv_sp[2] / mv_sp[1]) # Radial

play_manual_tour(data = f_dat, basis = f_bas, manip_var = which(colnames(f_dat) == "tars1"), 
                 col = f_cat, pch = f_cat, axes = "bottomleft")


##TODO: Phi ~ pi/2 * sign(mv_sp[2]) * my_phi
(out2b <- spinifex::oblique_frame(basis = f_bas, data = f_dat, manip_var = my_manip_var, 
                        phi = pi/2 + my_phi, theta = my_theta,
                        col = f_cat, pch = f_cat))
gridExtra::grid.arrange(out2a, out2b, nrow = 1)
out2 <- gridExtra::arrangeGrob(out2a, out2b, nrow = 1)


ggsave("./figures/basisStructure.png", out2, width = 6, height = 3, units = "in")


}


