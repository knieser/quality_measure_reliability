#' Plot measure performance across accountable entities
#' @description
#' This function creates a plot of measure performance across accountable entities, using the `perf.results` dataframe from `calcPerformance()` output.
#' @param df perf.results dataframe from `calcPerformance()` output
#' @param plot.type select which plot to return:
#' * `single` (the default): a plot of the performance across entities
#' * `OR`: plot of ORs comparing each facility with the average facility
#' * `correlation`: plot of standardization ratios against the entity random intercepts
#' * `multiple`: plot of measure performance across different risk-adjustment methods
#' @param plot.y `p` plots the unadjusted performance; `oe` plots the observed-to-expected ratio; `pe` plots the predicted-to-expected ratio
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom ggplot2 ggplot position_dodge
#' @export

plotPerformance <- function(df, plot.type = 'single', plot.y = 'p'){

  marg.p = sum(df$observed) / sum(df$n)

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
      df$upr = df$rs.pe.upr
      ylab = 'PE risk-standardized rate'
    }

    fig <- ggplot2::ggplot(data = df, ggplot2::aes(x = .data$rank, y = .data$y)) +
      ggplot2::geom_point(color = 'black') +
      ggplot2::geom_errorbar(aes(ymin = .data$lwr, ymax = .data$upr), width = 0.1) +
      ggplot2::geom_hline(yintercept = marg.p, col = 'red', lty = 'dashed', size = 1.2, alpha = 0.7) +
      ggplot2::xlab('Rank') +
      ggplot2::ylab(ylab) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face ="bold"),
        axis.text = ggplot2::element_text(size = 16),
        axis.ticks.length = ggplot2::unit(.25,"cm"),
        axis.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text = ggplot2::element_text(size = 18, face = "bold"),
        legend.position = 'none'
      )
  }

  if(plot.type == 'OR'){
    # plot of ORs comparing each facility with the average facility
    df$or.rank = rank(df$intercept.OR, ties.method = 'random')
    fig <- ggplot2::ggplot(data = df, ggplot2::aes(x = .data$intercept.OR, y = .data$or.rank)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$intercept.sig), size = 2) +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$intercept.OR.lwr, xmax = .data$intercept.OR.upr), width = 0.1, position = ggplot2::position_dodge(width = .7)) +
      ggplot2::scale_color_manual(values = c('black', 'red')) +
      ggplot2::scale_x_continuous(trans = 'log10') +
      ggplot2::geom_vline(xintercept = 1, lty = 2) +
      ggplot2::xlab('OR') +
      ggplot2::ylab('Entity') +
      ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face ="bold"),
        axis.text = ggplot2::element_text(size = 16),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.length.y = ggplot2::unit(0,'cm'),
        axis.ticks.length.x = ggplot2::unit(.25, 'cm'),
        axis.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 55, vjust = 0.7),
        strip.text = ggplot2::element_text(size = 18, face = "bold"),
        legend.position = 'none'
      )
  }

  # plot of standardization ratios against the entity random intercepts
  if (plot.type == 'correlation'){
    plot.df.corr <- data.frame(
      Method = rep(c('OE', 'PE'), each = nrow(df)),
      rand.int = rep(df$intercept.OR, 2),
      std.ratio = c(df$oe, df$pe)
    )

    fig <- ggplot2::ggplot(data = plot.df.corr, ggplot2::aes(x = .data$rand.int, y = .data$std.ratio, group = .data$Method)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$Method, shape = .data$Method), size = 3) +
      ggplot2::stat_smooth(method = 'lm', formula = y ~ x, geom = 'smooth', se = F, aes(color = .data$Method), lty = 'dashed') +
      ggplot2::scale_color_manual(values = c('darkgrey', 'red')) +
      ggplot2::xlab('Entity-specific random intercepts') +
      ggplot2::ylab('Standardization ratio') +
      ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face ="bold"),
        axis.text = ggplot2::element_text(size = 16),
        axis.ticks.length = ggplot2::unit(.25,"cm"),
        axis.title = ggplot2::element_text(size = 18, face = "bold"),
        legend.position = 'top',
        legend.text = ggplot2::element_text(size = 16),
        legend.title = ggplot2::element_text(size = 18, face = 'bold')
      )
  }

  # plot of measure performance across different risk-adjustment methods
  if (plot.type == 'multiple'){
  plot.df.p <- data.frame(
    method = rep(c('A. Unadjusted', 'B. OE-standardized', 'C. PE-standardized'), each = nrow(df)),
    p = c(df$p, df$rs.oe, df$rs.pe),
    lwr = c(df$p.lwr, df$rs.oe.lwr, df$rs.pe.lwr),
    upr = c(df$p.upr, df$rs.oe.upr, df$rs.pe.upr),
    rank = rep(df$rank.p, 3),
    rank.oe = rep(df$rank.oe, 3),
    rank.pe = rep(df$rank.pe, 3)
  )

  fig <- ggplot2::ggplot(data = plot.df.p, ggplot2::aes(x = .data$rank, y = .data$p, group = .data$method)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lwr, ymax = .data$upr), width = 0.1) +
    ggplot2::geom_hline(yintercept = marg.p, col = 'red', lty = 'dashed', size = 1.2, alpha = 0.7) +
    ggplot2::xlab('Entity rank') +
    ggplot2::ylab('Measure performance') +
    ggplot2::facet_wrap( ~ method, nrow = 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face ="bold"),
      axis.text = ggplot2::element_text(size = 16),
      axis.ticks.length = ggplot2::unit(.25,"cm"),
      axis.title = ggplot2::element_text(size = 18, face = "bold"),
      strip.text = ggplot2::element_text(size = 18, face = "bold"),
      legend.text = ggplot2::element_text(size = 16),
      legend.title = ggplot2::element_blank(),
      legend.position = 'bottom'
    )
  }
fig
}
