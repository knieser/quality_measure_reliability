#' Plot distributions of reliability estimates across entities
#' @description
#' This function creates boxplots of reliability estimates across entities and different methods
#' @param rel.out results from `calcReliability()`
#' @returns A ggplot figure
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @examples
#' ### Simulate data with binary outcome
#' df <- simulateData(n.entity = 50, n.obs = 100, mu = .2, r = .7)
#'
#' # Calculate reliability
#' out <- calcReliability(df = df, entity = 'entity', y = 'y')
#'
#' # Plot estimates
#' plotReliability(out)
#'
#'
#' # Calculate reliability with expanded set of methods
#' out1 <- calcReliability(df = df, entity = 'entity', y = 'y', show.all = T)
#'
#' # Plot estimates for expanded set of methods
#' plotReliability(out1)
#'
#'
#' ### This function also works with continuous outcome data
#' # simulate data from multilevel normal distribution
#' df.c <- simulateData(n.entity = 50, n.obs = 100, mu = 25, r = .6, data.type = 'normal')
#'
#' # calculate reliability
#' out.c <- calcReliability(df = df, entity = 'entity', y = 'y', data.type = 'continuous')
#' plotReliability(out.c)
#'
#' @importFrom ggplot2 ggplot position_jitter
#' @export

plotReliability <- function(rel.out){

  data.type = rel.out$data.type
  show.all = rel.out$show.all
  method = rel.out$rel.results$method
  SSR.out <- rel.out$SSR.out
  if ('est.PSSR.oe' %in% names(SSR.out)){
   est.SSR <- SSR.out$est.SSR.oe
   est.PSSR <- SSR.out$est.PSSR.oe
  } else{
    est.SSR <- SSR.out$est.SSR
    est.PSSR <- SSR.out$est.PSSR
  }

  if (data.type == 'binary'){
    HLGM.out <- rel.out$HLGM.out
    BB.out <- rel.out$BB.out
    n = length(HLGM.out$entity)

    if (show.all == TRUE){
      AOV.out <- rel.out$AOV.out
      RIUR.out <- rel.out$RIUR.out
      rel.plot.df <- data.frame(
        method = rep(method, each = n),
        est = c(
          rep(est.SSR, n),
          rep(est.PSSR, n),
          AOV.out$est.aov,
          HLGM.out$est.HLGM.latent,
          HLGM.out$est.HLGM.delta,
          HLGM.out$est.HLGM.MC,
          HLGM.out$est.HLGM.FE,
          HLGM.out$est.HLGM.RE,
          BB.out$est.BB,
          BB.out$est.BB.FE,
          BB.out$est.BB.RE,
          BB.out$est.BB.J,
          rep(RIUR.out$IUR, n)
          )
      )
    } else{
      rel.plot.df <- data.frame(
        method = rep(method, each = n),
        est = c(
          rep(est.PSSR, n),
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
      est = c(rep(est.PSSR, n),
              AOV.out$est.aov,
              HLM.out$est.HLM)
    )
  }

  fig <- ggplot2::ggplot(data = rel.plot.df, ggplot2::aes(.data$est, .data$method)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_point(alpha = 0.6, position = ggplot2::position_jitter(height = 0.1, width = 0)) +
    ggplot2::xlab('Entity-specific reliability estimate') +
    ggplot2::ylab('Method') +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(linewidth = 1),
      plot.title = ggplot2::element_text(size = 16, face ="bold"),
      axis.text = ggplot2::element_text(size = 16),
      axis.ticks.length = ggplot2::unit(.25,"cm"),
      axis.title = ggplot2::element_text(size = 18, face = "bold"),
      legend.position = 'bottom'
    )
  fig
  }
