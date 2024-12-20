rm(list = ls())

library(ggplot2)

set.seed(1)

# show spread between providers
rel = c(0.5, 0.7, 0.9)
sd.b = 1
p = rnorm(5, 0, sd.b)
sd.w = sqrt(sd.b^2 * (1/rel - 1))
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442")


# plots for signal to noise notion of reliability
plot_SNR_examples <- function(p, sd, figtitle, filetitle){
  fig<- ggplot(data = data.frame(x = seq(-3, 3, .1)), aes(x=x)) +
    stat_function(fun = dnorm, args = list(mean = p[1], sd = sd), color = cbPalette[1], lwd = 1.5) +
    stat_function(fun = dnorm, args = list(mean = p[2], sd = sd), color = cbPalette[2], lwd = 1.5) +
    stat_function(fun = dnorm, args = list(mean = p[3], sd = sd), color = cbPalette[3], lwd = 1.5) +
    stat_function(fun = dnorm, args = list(mean = p[4], sd = sd), color = cbPalette[4], lwd = 1.5) +
    stat_function(fun = dnorm, args = list(mean = p[5], sd = sd), color = cbPalette[5], lwd = 1.5) +
    ggtitle(figtitle) +
    xlab('Measure performance') +
    ylab('') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14))
  fig
}

# plots for repeated sampling notion of reliability
plot_SSR_examples <- function(p, sd, reps, figtitle, filetitle){
  p1.samples <- replicate(reps, rnorm(2, mean = p[1], sd = sd))
  p2.samples <- replicate(reps, rnorm(2, mean = p[2], sd = sd))
  p3.samples <- replicate(reps, rnorm(2, mean = p[3], sd = sd))
  p4.samples <- replicate(reps, rnorm(2, mean = p[4], sd = sd))
  p5.samples <- replicate(reps, rnorm(2, mean = p[5], sd = sd))
  plot.df <- data.frame(
    provider = as.factor(rep(1:5, each = reps)),
    s1 = c(p1.samples[1,], p2.samples[1,], p3.samples[1,], 
            p4.samples[1,], p5.samples[1,]),
    s2 = c(p1.samples[2,], p2.samples[2,], p3.samples[2,], 
            p4.samples[2,], p5.samples[2,])
  )
  fig <- ggplot(data = plot.df, aes(x = s1, y = s2, group = provider)) +
    geom_point(aes(color = provider)) +
    scale_color_manual(values=cbPalette) +
    geom_abline(slope = 1, lty = 'dashed') +
    coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3)) +
    ggtitle(figtitle) +
    xlab('Sample 1') +
    ylab('Sample 2') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.position = 'none')
  fig
}

misclassification_vis <- function(p, sd, reps, figtitle, filetitle){
  ranks = 6 - rank(p)
  samples <- sapply(p, function(x) rnorm(reps, mean = x, sd = sd))
  samples.ranks <- t(apply(samples, 1, function(x) 6 - rank(x)))
  rank.error <- t(apply(samples.ranks, 1, function(x) x - ranks))
  plot.df <- data.frame(
    provider = as.factor(rep(1:5, each = reps)),
    ranks = rep(ranks, each = reps),
    sample.rank = c(samples.ranks)
  )
  fig <- ggplot(data = plot.df, aes(x = ranks, y = sample.rank, group = provider)) + 
    geom_jitter(aes(color = provider)) +
    scale_color_manual(values = cbPalette) +
    scale_x_reverse() +
    scale_y_reverse() +
    ggtitle(figtitle) +
    xlab('True rank') +
    ylab('Sample rank') +
    coord_flip() + 
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14),
          legend.position = 'none')
  fig
}

# SNR plots
plot_SNR_examples(p, sd.w[1], 'Reliability = 0.5', 'SNR_low_rel')
plot_SNR_examples(p, sd.w[2], 'Reliability = 0.7', 'SNR_med_rel')
plot_SNR_examples(p, sd.w[3], 'Reliability = 0.9', 'SNR_high_rel')

# SSR plots
plot_SSR_examples(p, sd.w[1], 100, 'Reliability = 0.5', 'SSR_low_rel')
plot_SSR_examples(p, sd.w[2], 100, 'Reliability = 0.7', 'SSR_med_rel')
plot_SSR_examples(p, sd.w[3], 100, 'Reliability = 0.9', 'SSR_high_rel')

# Misclassification visual
misclassification_vis(p, sd.w[1], 1e3, 'Reliability = 0.5', 'Misclass_low_rel')
misclassification_vis(p, sd.w[2], 1e3, 'Reliability = 0.7', 'Misclass_med_rel')
misclassification_vis(p, sd.w[3], 1e3, 'Reliability = 0.9', 'Misclass_high_rel')
