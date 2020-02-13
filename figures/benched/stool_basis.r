library(dplyr)
library(ggplot2)
den <- 36

seat_x <- seq(0, 2, by = 1/den)
seat_y <- seq(2.7, 3, by = 1/den)
seat_z <- seq(0, 2, by = 1/den)
seat <- crossing(x = seat_x, y = seat_y, z = seat_z)

leg_x <- seq(.3, .5, by = 1/den)
leg_y <- seq(0, 2.7, by = 1/den)
leg_z <- seq(.3, .5, by = 1/den)
leg1 <- crossing(x = leg_x, y = leg_y, z = leg_z)
leg2 <- crossing(x = leg_x + 1.2, y = leg_y, z = leg_z)
leg3 <- crossing(x = leg_x + 1.2, y = leg_y, z = leg_z + 1.2)
leg4 <- crossing(x = leg_x, y = leg_y, z = leg_z + 1.2)

bar_x <- seq(.5, 1.5, by = 1/den)
bar_y <- seq(1, 1.2, by = 1/den)
bar_z <- seq(.3, .5, by = 1/den)
bar1 <- crossing(x = bar_x, y = bar_y - .5, z = bar_z)
bar2 <- crossing(x = bar_x, y = bar_y - .5, z = bar_z + 1.2)
bar3 <- crossing(x = bar_z, y = bar_y + .5, z = bar_x)
bar4 <- crossing(x = bar_z + 1.2, y = bar_y + .5, z = bar_x)

stool <- rbind(seat, leg1, leg2, leg3, leg4, bar1, bar2, bar3, bar4)
dim(stool)

stool_samp <- sample_n(stool, 5000)

axis_seg <- data.frame(x = c(0, -1, -1), y = c(0, 1, 0), z = c(0, -1, 0), x_end = -1, y_end = 0)
axis_lab <- data.frame(x = c(-.1, -1.1, -1.1), y = c(-.1, .9, -.1), z = c(-.1, -1.1, -.1), lab = c("x", "y", "z"))

(gg1 <- ggplot(stool_samp) + coord_fixed() + theme_minimal() +
  geom_point(stool_samp, mapping = aes(x, y)) +
  geom_segment(axis_seg[1:2, ], mapping = aes(x, y, xend = x_end, yend = y_end)) +
  geom_text(axis_lab, mapping = aes(x, y, label = lab)))
(gg2 <- ggplot(stool_samp) + coord_fixed() + theme_minimal() +
  geom_point(aes(x, z)) +
  geom_segment(axis_seg[c(1, 3), ], mapping = aes(x, z, xend = x_end, yend = y_end)) +
  geom_text(axis_lab, mapping = aes(x, z, label = lab)))
(gg3 <- ggplot(stool_samp) + coord_fixed() + theme_minimal() +
  geom_point(aes(z, y)) +
  geom_segment(axis_seg[2:3, ], mapping = aes(z, y, xend = x_end, yend = y_end)) +
  geom_text(axis_lab, mapping = aes(z, y, label = lab)))

#TODO:
#remove axes, 
#shift axes to left.
#rotate to 3/4 perspective
#stitch together with basis.

rot3xy_of <- function(mat, ang = pi/6){ # https://en.wikipedia.org/wiki/Rotation_matrix#In_three_dimensions
  mat <- as.matrix(mat)
  ang <- -ang
  c <- cos(ang)
  s <- sin(ang)
  rot_x <- matrix(c(1, 0,  0,
                    0, c, -s,
                    0, s,  c), ncol = 3, byrow = T)
  rot_y <- matrix(c( c, 0, s,
                     0, 1, 0,
                    -s, 0, c), ncol = 3, byrow = T)
  rot_z <- matrix(c(c, -s, 0,
                    s,  c, 0,
                    0,  0, 1),   ncol = 3, byrow = T)
  ret <- mat %*% rot_x %*% rot_y
  colnames(ret) <- c("x", "y", "z")
  as.data.frame(ret)
}

stool_samp45 <- rot3xy_of(stool_samp)
axis_seg45 <- axis_seg
axis_seg45[, 1:3] <- rot3xy_of(axis_seg[, 1:3])
axis_lab45 <- axis_lab
axis_lab45[, 1:3] <- rot3xy_of(axis_lab[, 1:3])
(gg4 <- ggplot() + coord_fixed() + theme_minimal() +
    geom_point(stool_samp45, mapping = aes(x, y)) +
    geom_segment(axis_seg45, mapping = aes(x, y, xend = x_end, yend = y_end)) +
    geom_text(axis_lab45, mapping = aes(x, y, label = lab))
)


#Rotation by x, is good start.
#rotation by x & y is same /w or w/o (); and looks wrong.
#rotation of just y is good, but basis is wrong.
#rotation on z is wrong not the type of roation we want. and basis is wrong.
### IS THIS A ROTATION ABOUT THE ORIGIN ISSUE?

