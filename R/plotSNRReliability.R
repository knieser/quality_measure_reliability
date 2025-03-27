#' Plot distributions of reliability estimates across entities
#' @description
#' This function creates boxplots of reliability estimates across entities and different methods
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom ggplot2 ggplot
#' @export

plotSNRReliability <- function(rel.out = rel.out){
HLGM.out <- rel.out$HLGM.out
BB.out <- rel.out$BB.out

rel.plot.df <- data.frame(
  method = rep(c('Hier.logit, latent scale',
                 'Hier.logit, delta approx.',
                 'Hier.logit, FE',
                 'Hier.logit, RE',
                 'Beta-binomial',
                 'Beta-binomial, FE',
                 'Beta-binomial, RE',
                 'Beta-binomial, Jeffreys'), each = length(HLGM.out$n)),
  est = c(HLGM.out$est.HLGM.latent, HLGM.out$est.HLGM.delta, HLGM.out$est.HLGM.FE, HLGM.out$est.HLGM.RE,
          BB.out$est.BB, BB.out$est.BB.FE, BB.out$est.BB.RE, BB.out$est.BB.J)
)

rel.plot.df$method <- factor(rel.plot.df$method)
rel.plot.df$method <- factor(rel.plot.df$method, levels = rev(c('Hier.logit, latent scale',
                                                   'Hier.logit, delta approx.',
                                                   'Hier.logit, FE',
                                                   'Hier.logit, RE',
                                                   'Beta-binomial',
                                                   'Beta-binomial, FE',
                                                   'Beta-binomial, RE',
                                                   'Beta-binomial, Jeffreys')))

fig <- ggplot2::ggplot(data = rel.plot.df, aes(est, method)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.6, position = position_jitter(height = 0.1, width = 0)) +
  xlab('Entity-specific reliability estimate') +
  ylab('Method') +
  theme_classic() +
  theme(
    panel.grid.major = element_line(linewidth = 1),
    plot.title = element_text(size = 16, face ="bold"),
    axis.text = element_text(size = 16),
    axis.ticks.length = unit(.25,"cm"),
    axis.title = element_text(size = 18, face = "bold"),
    legend.position = 'bottom'
  )
fig
}
