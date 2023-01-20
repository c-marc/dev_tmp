#build a nomogram

library(tidyverse)
library(glue)

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



build_ax_data <- function(odds = c(2, 5, 20, 100)) {
  len <- length(odds)
  data <- tibble(
    a = c(rep(1, len+1), odds),
    b = c(odds, rep(1, len+1))
  )
  data |> 
    mutate(
      odds = a/b,
      p = odds / (1+odds),
      label = paste0(a, ":", b)
    )
}

data_p <- build_ax_data() |>
  mutate(y = log(odds))
data_p_lim <- range(data_p$y)

data_r <- build_ax_data(c(2, 5, 20, 100)) |>
  mutate(y = log(odds)/2)
data_r_lim <- range(data_r$y)


## Graph parameters
##

# tick width
tw <- 0.02

# from colorbrewer
col1 <- "#1b9e77"
col2 <- "#d95f02"
theme_col <- c("before" = "darkgrey", "negative" = col1, "positive" = col2)


## Data parameters
##

# se, sp, prior
se <- .9
sp <- .8
p <- 0.05

build_data <- function(se, sp, p){
  #positive and neg likelihood ratios
  plr <- se / (1-sp)
  nlr <- (1-se) / sp
  
  #prior odds
  o1 <- p / (1-p)
  
  data <- tibble(
    o1 = rep(o1, 3), 
    lr = c(1, plr, nlr),
    name = c("before", "positive", "negative"),
    label = c("", "+", "-")
  )
  data |>
    mutate(o2 = o1*lr)
}

test <- build_data(se = se, sp = sp, p = p) |>
  mutate(label2 = paste0(sprintf(o2/(1+o2)*100, fmt="%#.1f"),"%"))


## A little bit verbose, but this allows fine tuning of the layers
##
gg <- ggplot() +
  #left axis is reversed (but many things are symmetrical)
  geom_segment(aes(x = 0, xend = 0, y = data_p_lim[1], yend = data_p_lim[2])) +
  geom_segment(
    data = data_p,
    aes(x = -tw, xend = 0, y = y, yend = y)
  ) +
  geom_text(data = data_p, aes(x = 0, y = -y, label = label),
            hjust = "right", nudge_x = -tw*2, size = 3) +
  #right axis
  geom_segment(aes(x = 1, xend = 1, y = data_p_lim[1], yend = data_p_lim[2])) +
  geom_segment(
    data = data_p,
    aes(x = 1 + tw, xend = 1, y = y, yend = y)
  ) +
  geom_text(data = data_p, aes(x = 1, y = y, label = label),
            hjust = "left", nudge_x = tw*2, size = 3) +
  #middle axis
  geom_segment(aes(x = 0.5, xend = 0.5, y = data_r_lim[1], yend = data_r_lim[2])) +
  geom_segment(
    data = data_r,
    aes(x = 0.5 - tw/2, xend = 0.5 + tw/2, y = y, yend = y)
  ) +
  geom_text(data = data_r, aes(x = 0.5, y = y, label = label),
            hjust = "left", nudge_x = tw*2, size = 3) +
  
  # data
  # 3 lines
  geom_segment(data = test, aes(x=0,  xend = 1, y = -log(o1), yend = log(o2), colour = name))+
  # LR
  geom_point(data = test, aes(x= .5, y =  log(lr)/2, fill = name), shape = 21, size = 3)+
  # posterior odds
  geom_point(data = test, aes(x= 1, y =  log(o2), fill = name), shape = 21, size = 3)+
  # add values for posteriors odds but converted as proba %
  geom_label(data = test, aes(x = 1, y = log(o2), label = label2), 
             hjust = "right", nudge_x = -.05)


## Scales, coord and labs
gg <- gg  +
  coord_cartesian(xlim = c(-tw * 4, 1 + 4 * tw)) +
  scale_color_manual(values = theme_col, guide="none")+
  scale_fill_manual(values = theme_col, guide="none") +
  scale_x_continuous(
    name = NULL, 
    breaks=c(0, .5, 1),
    labels=c("Prior Odds","Likelihood Ratios","Posterior Odds")) +
  labs(
    title = "Fagan's Nomogram",
    subtitle = glue(
      "Prior probability {round(100* p)}% - ",
      "sensitivity {round(100 * se)}% - ",
      "specificity {round(100 * sp)}%"
    ))
  

## Theme

# Arg for minimal and update other param
gg + 
  theme_minimal(
    base_size = 12,
    base_family = "Roboto Condensed")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(face = "bold")
  )
