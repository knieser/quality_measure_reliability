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

data.type = rel.out$data.type
show.all = rel.out$show.all
method = rel.out$rel.results$method
SSR.out <- rel.out$SSR.out

if (data.type == 'binary'){
  HLGM.out <- rel.out$HLGM.out
  BB.out <- rel.out$BB.out
  n = length(HLGM.out$n)

  if (show.all == TRUE){
    AOV.out <- rel.out$AOV.out
    rel.plot.df <- data.frame(
      method = rep(method, each = n),
      est = c(
        rep(SSR.out$est.SSR, n),
        rep(SSR.out$est.PSSR, n),
        rep(SSR.out$est.SSR.oe, n),
        rep(SSR.out$est.SSR.pe, n),
        rep(SSR.out$est.PSSR.oe, n),
        rep(SSR.out$est.PSSR.pe, n),
        AOV.out$est.aov,
        HLGM.out$est.HLGM.latent,
        HLGM.out$est.HLGM.delta,
        HLGM.out$est.HLGM.MC,
        HLGM.out$est.HLGM.FE,
        HLGM.out$est.HLGM.RE,
        BB.out$est.BB,
        BB.out$est.BB.FE,
        BB.out$est.BB.RE,
        BB.out$est.BB.J)
    )
  } else{
    rel.plot.df <- data.frame(
      method = rep(method, each = n),
      est = c(
        rep(SSR.out$est.PSSR.pe, n),
        HLGM.out$est.HLGM.delta,
        BB.out$est.BB)
    )
  }
}

if (data.type == 'continuous'){
  AOV.out <- rel.out$AOV.out
  HLM.out <- rel.out$HLM.out
  n = length(HLM.out$n)

  rel.plot.df <- data.frame(
    method = rep(method, each = n),
    est = c(rep(SSR.out$est.PSSR, n),
            AOV.out$est.aov,
            HLM.out$est.HLM)
  )
}



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
