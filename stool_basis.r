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

axis_seg <- data.frame(x = c(1, 0, 0), y = c(0, 1, 0), z = c(0, 0, 1), zero = 0)
axis_lab <- data.frame(x = c(1, -.1, -.1), y = c(-.1, 1, -.1), z = c(-.1, -.1, 1), lab = c("x", "y", "z"))

(gg1 <- ggplot(stool_samp) + coord_fixed() + theme_minimal() +
  geom_point(stool_samp, mapping = aes(x, y)) +
  geom_segment(axis_seg, mapping = aes(x, y, xend = zero, yend = zero)) +
  geom_text(axis_lab, mapping = aes(x, y, label = lab)))
(gg2 <- ggplot(stool_samp) + coord_fixed() + theme_minimal() +
  geom_point(aes(x, z)) + coord_fixed() +
  geom_segment(axis_seg, mapping = aes(x, z, xend = zero, yend = zero)) +
  geom_text(axis_lab, mapping = aes(x, z, label = lab)))
(gg3 <- ggplot(stool_samp) + coord_fixed() + theme_minimal() +
  geom_point(aes(z, y)) + coord_fixed() +
  geom_segment(axis_seg, mapping = aes(z, y, xend = zero, yend = zero)) +
  geom_text(axis_lab, mapping = aes(z, y, label = lab)))

#TODO:
#remove axes, 
#shift axes to left.
#rotate to 3/4 perspective
#stitch together with basis.

