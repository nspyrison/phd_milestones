library(spinifex)
library(ggplot2)

rb <- basis_random(n = 6)

(gg_basis    <- view_basis(rb))
(gg_manip_sp <- THIS_MANIP_SP(rb, 1))
ggsave("./figures/basis.png", gg_basis, width = 5, height = 5, units = "in")
ggsave("./figures/manipSp.png", gg_manip_sp, width = 5, height = 5, units = "in")


THIS_MANIP_SP <- function(basis,
                          manip_var,
                          manip_col = "blue", # "#1B9E77"
                          tilt = 5/12 * pi,   
                          z_col = "red",      # "#D95F02"
                          lab = paste0("V", 1:nrow(basis))
) {
    # Initialize
    ## manip space
    m_sp <- as.matrix(create_manip_space(basis, manip_var))
    p <- nrow(m_sp)
    find_angle <- function(a,b) acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
    theta <- find_angle(m_sp[manip_var, ], c(1,0,0)) * sign(m_sp[manip_var, 2])
    phi   <- find_angle(m_sp[manip_var, ], c(m_sp[manip_var, 1:2],0))
    ## manip var asethetics
    col_v            <- rep("grey80", p)
    col_v[manip_var] <- manip_col
    siz_v            <- rep(0.3, p)
    siz_v[manip_var] <- 1
    ## basis rotation
    c <- cos(tilt)
    s <- sin(tilt)
    rot  <- matrix(c(1,0,0, 0,c,-1*s, 0,0,s),
                   ncol = 3, byrow = T)
    ## helper funcs
    make_circ <- function(ang_st = 0, ang_stop = 2 * pi){
      angle <- seq(ang_st, ang_stop, 
                   length = max(round(360 * abs(ang_st - ang_stop) / (2 * pi)),3) )
      as.matrix(data.frame(x = cos(angle), y = sin(angle), z = 0))
    }
    xyz <- function(df) {colnames(df) <- c("x", "y", "z"); as.data.frame(df)}
    ## rotated spaces
    circ_r <- xyz(make_circ() %*% rot)
    m_sp_r <- xyz(m_sp %*% rot) # rotated manip sp
    m_sp_z <- data.frame(x = m_sp[manip_var, 1],
                         y = m_sp[manip_var, 2],
                         z = m_sp[manip_var, 3],
                         xend = m_sp_r[manip_var, "x"],
                         yend = m_sp_r[manip_var, "y"],
                         lab = lab[manip_var])
    ## rotate angle curves, phi & theta
    theta_curve <- xyz(make_circ(0, theta) %*% rot)
    phi_curve   <- xyz(make_circ(0, phi))
    phi_curve$y <- phi_curve$y # * sign(m_sp[manip_var, 2])
    phi_curve$x <- phi_curve$x # * sign(m_sp[manip_var, 1])
    .x <- phi_curve$x
    .y <- phi_curve$y
    ang   <- theta + phi # * sign(m_sp[manip_var, 1])
    phi_curve$x <- .x * cos(ang) - .y * sin(ang)
    phi_curve$y <- .x * sin(ang) + .y * cos(ang)
    mid_theta   <- round(nrow(theta_curve)/2)
    mid_phi     <- round(nrow(phi_curve)/2)
    
    gg <- 
      ## Ggplot options
      ggplot2::ggplot() + 
      ggplot2::scale_color_brewer(palette = "Dark2") +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_fixed() + # Do not use with plotly!
      ## XY Circle path 
      ggplot2::geom_path(
        data = circ_r, 
        mapping = ggplot2::aes(x = x, y = y), 
        color = manip_col, size = .3, inherit.aes = F) +
      ## Basis axes line segments
      ggplot2::geom_segment(
        data = m_sp_r, 
        mapping = ggplot2::aes(x = x, y = y, xend = 0, yend = 0),
        size = siz_v, colour = col_v) +
      ## Basis variable text labels
      ggplot2::geom_text(
        data = m_sp_r, 
        mapping = ggplot2::aes(x = x, y = y, label = lab), 
        size = 4, colour = col_v, vjust = "outward", hjust = "outward") +
      ## Z circle path
      ggplot2::geom_path(
        data = as.data.frame(circ_r),
        mapping = ggplot2::aes(x = x, y = z),
        color = z_col, size = .3, inherit.aes = F) +
      ## Z manip axis segment
      ggplot2::geom_segment(
        data = m_sp_z, 
        mapping = ggplot2::aes(x = x, y = z, xend = 0, yend = 0),
        size = 1, colour = z_col) + 
      ## Z projection line segment
      ggplot2::geom_segment(
        data = m_sp_z, 
        mapping = ggplot2::aes(x = x, y = z, xend = xend, yend = yend),
        size = .3, colour = "grey80", linetype = 2) +
      ## Z projection axis text label
      ggplot2::geom_text(
        data = m_sp_z, 
        mapping = ggplot2::aes(x = x, y = z, label = lab),
        size = 4, colour = z_col, vjust = "outward", hjust = "outward")
gg 
}
