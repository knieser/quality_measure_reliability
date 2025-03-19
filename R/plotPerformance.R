#' Plot measure performance across accountable entities
#' @description
#' This function creates a plot of measure performance across accountable entities, using the perf.results dataframe from calcPerformance() output.
#' @param df perf.results dataframe from calcPerformance() output
#' @param plot.type select which plot to return:
#' \item{single}{a plot of the performance across entities}
#' \item{OR}{plot of ORs comparing each facility with the average facility}
#' \item{correlation}{plot of standardization ratios against the entity random intercepts}
#' \item{multiple}{plot of measure performance across different risk-adjustment methods}
#' @param plot.y 'p' plots the unadjusted performance, 'oe' plots the observed-to-expected ratio, 'rs' plots the risk-standardized rate, and 'pe' plots the predicted-to-expected ratio'
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom ggplot2 ggplot
#' @export

plotPerformance <- function(df = perf.results, plot.type = 'single', plot.y = 'p'){

  if (plot.type == 'single'){
    if (plot.y == 'p') {
      df$y = df$p
      df$rank = df$rank.p
      df$lwr = df$p.lwr
      df$upr = df$p.upr
      ylab = 'Unadjusted rate'
    } else if (plot.y == 'oe') {
      df$y = df$rs.oe
      df$rank = df$rank.oe
      df$lwr = df$rs.oe.lwr
      df$upr = df$rs.oe.upr
      ylab = 'OE risk-standardized rate'
    } else if (plot.y == 'pe') {
      df$y = df$rs.pe
      df$rank = df$rank.pe
      df$lwr = df$rs.pe.lwr
      df$upr = df$rs.pe.lwr
      ylab = 'PE risk-standardized rate'
    }

    fig <- ggplot2::ggplot(data = df, aes(x = rank, y = y)) +
      geom_point(color = 'darkblue') +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
      xlab('Rank') +
      ylab(ylab) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 16, face ="bold"),
        axis.text = element_text(size = 16),
        axis.ticks.length = unit(.25,"cm"),
        axis.title = element_text(size = 18, face = "bold"),
        strip.text = element_text(size = 18, face = "bold"),
        legend.position = 'none'
      )
  }

  if(plot.type == 'OR'){
    # plot of ORs comparing each facility with the average facility
    fig <- ggplot(data = perf.results, aes(x = intercept.OR, y = entities)) +
      geom_point(aes(color = intercept.sig), size = 2) +
      geom_errorbar(aes(xmin = intercept.OR.lwr, xmax = intercept.OR.upr), width = 0.1, position = position_dodge(width = .7)) +
      scale_color_manual(values = c('black', 'red')) +
      scale_x_continuous(trans = 'log10') +
      geom_vline(xintercept = 1, lty = 2) +
      xlab('OR') +
      ylab('Entity') +
      theme_classic() +
      theme(
        plot.title = element_text(size = 16, face ="bold"),
        axis.text = element_text(size = 16),
        axis.text.y = element_blank(),
        axis.ticks.length.y = unit(0,'cm'),
        axis.ticks.length.x = unit(.25, 'cm'),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 55, vjust = 0.7),
        strip.text = element_text(size = 18, face = "bold"),
        legend.position = 'none'
      )
  }

  # plot of standardization ratios against the entity random intercepts
  if (plot.type == 'correlation'){
    plot.df.corr <- data.frame(
      Method = rep(c('OE', 'PE'), each = nrow(perf.results)),
      rand.int = rep(perf.results$intercept.OR, 2),
      std.ratio = c(perf.results$oe, perf.results$pe)
    )

    fig <- ggplot2::ggplot(data = plot.df.corr, aes(x = rand.int, y = std.ratio, group = Method)) +
      geom_point(aes(color = Method, shape = Method), size = 3) +
      stat_smooth(method = 'lm', formula = y ~ x, geom = 'smooth', se = F, aes(color = Method), lty = 'dashed') +
      scale_color_manual(values = c('darkgrey', 'red')) +
      xlab('Entity-specific random intercepts') +
      ylab('Standardization ratio') +
      theme_classic() +
      theme(
        plot.title = element_text(size = 16, face ="bold"),
        axis.text = element_text(size = 16),
        axis.ticks.length = unit(.25,"cm"),
        axis.title = element_text(size = 18, face = "bold"),
        legend.position = 'top',
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = 'bold')
      )
  }

  # plot of measure performance across different risk-adjustment methods
  if (plot.type == 'multiple'){
  plot.df.p <- data.frame(
    method = rep(c('A. Unadjusted', 'B. OE-standardized', 'C. PE-standardized'), each = nrow(perf.results)),
    p = c(perf.results$p, perf.results$rs.oe, perf.results$rs.pe),
    lwr = c(perf.results$p.lwr, perf.results$rs.oe.lwr, perf.results$rs.pe.lwr),
    upr = c(perf.results$p.upr, perf.results$rs.oe.upr, perf.results$rs.pe.upr),
    rank = rep(perf.results$rank.p, 3),
    rank.oe = rep(perf.results$rank.oe, 3),
    rank.pe = rep(perf.results$rank.pe, 3)
  )

  fig <- ggplot2::ggplot(data = plot.df.p, aes(x = rank, y = p, group = method)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.1) +
    geom_hline(yintercept = marg.p, col = 'red', lty = 'dashed', size = 1.2, alpha = 0.7) +
    xlab('Entity rank') +
    ylab('Measure performance') +
    facet_wrap( ~ method, nrow = 1) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 16, face ="bold"),
      axis.text = element_text(size = 16),
      axis.ticks.length = unit(.25,"cm"),
      axis.title = element_text(size = 18, face = "bold"),
      strip.text = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 16),
      legend.title = element_blank(),
      legend.position = 'bottom'
    )
  }
fig
}
