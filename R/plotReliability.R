#' Plot distributions of reliability estimates across entities
#' @description
#' This function creates boxplots of reliability estimates across entities and different methods
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom ggplot2 ggplot
#' @export

plotReliability <- function(rel.out = rel.out){

method = rel.out$rel.results$method
SSR.out <- rel.out$SSR.out
HLGM.out <- rel.out$HLGM.out
BB.out <- rel.out$BB.out

rel.plot.df <- data.frame(
  method = rep(method, each = length(HLGM.out$n)),
  est = c(rep(SSR.out$est.PSSR.oe, length(HLGM.out$n)),
          HLGM.out$est.HLGM.delta,
          BB.out$est.BB)
  )

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
