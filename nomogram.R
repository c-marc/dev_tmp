#build a nomogram

library(tidyverse)

tpf <- .9
fpf <- .2

plr <- function(tpf, fpf) tpf/fpf
nlr <- function(tpf, fpf) (1-tpf)/(1-fpf)
post <- function(pre, lr) pre*lr

make_data <- function(tpf, fpf, pre) {
  tibble(
    p1 = rep(pre, 3),
    res = c("unknown", "pos", "neg"),
    lr = c(1, plr(tpf, fpf), nlr(tpf, fpf))
  ) |>
    mutate(p2 = p1 * lr)
}

#prob seq
p_half_seq <- c(.01, .05, .1, .2, .3, .4 )
p_seq <- c(p_half_seq, .5, 1-rev(p_half_seq))
p_seq

#ratio seq
r_half_seq <- c(2, 5, 10, 20, 50, 100, 1000)
r_seq <- c(1/rev(r_half_seq), 1, r_half_seq)

ax_p <- tibble(y = p_seq)
ax_r <- tibble(y = r_seq)

p_min <- min(p_seq)
r_min <- min(r_seq)
tw <- .02

ggplot()+
  #left
  geom_segment(aes(x = 0, xend = 0, y = qlogis(p_min), yend = qlogis(1 - p_min))) +
  geom_segment(data = ax_p, aes(x = -tw, xend = 0, y = qlogis(y), yend = qlogis(y))) +
  #right
  geom_segment(aes(x = 1, xend = 1, y = qlogis(p_min), yend = qlogis(1 - p_min)))+
  geom_segment(data = ax_p, aes(x = 1, xend = 1 + tw, y = qlogis(y), yend = qlogis(y))) +
  #middle
  geom_segment(aes(x = .5, xend = .5, y = log(r_min)/2, yend = log(r_min^-1)/2))+
  geom_segment(data = ax_r, aes(x = .5 - tw, xend = .5 + tw, y = log(y)/2, yend = log(y)/2)) +

  #data
  geom_segment(data=make_data(tpf, fpf, .2), aes(x=0, xend=1, y=-qlogis(p1), yend=qlogis(p2), col = res))+
  theme_minimal()
