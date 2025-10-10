#' Plot measure performance across accountable entities
#' @description
#' This function creates a plot of measure performance across accountable entities, using the `perf.results` dataframe from `calcPerformance()` output.
#' @param df perf.results dataframe from `calcPerformance()` output
#' @param plot.type specifies which plot to return:
#' * `p`: plots the entity-level unadjusted outcome rates
#' * `oe`: plots the entity-level observed-to-expected risk-standardized rates
#' * `pe`: plots the predicted-to-expected risk-standardized rates
#' * `OR`: plots the odds ratios comparing each entity with the average entity; i.e., exponentiated random intercepts from the hierarchical logistic regression model.
#' * `correlation`: plot of standardization ratios against the entity random intercepts
#' * `multiple`: plot of measure performance across different risk-adjustment methods
#' @returns A ggplot figure
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @examples
#' # simulate data
#' df <- simulateData(n.entity = 50, n.obs = 100, mu = .2, r = .7)
#'
#' # calculate measure performance
#' out <- calcPerformance(df = df, entity = 'entity', y = 'y')
#'
#' # plot performance
#' plotPerformance(out$perf.results, plot.type = 'p')
#'
#' @importFrom ggplot2 ggplot position_dodge element_line element_text unit aes
#' @importFrom rlang .data
#' @export

plotPerformance <- function(df, plot.type = 'p'){
  chk.rs <- 'rs.oe' %in% names(df)
  if (plot.type == 'correlation' & !chk.rs) stop(message('This plot can only be generated for risk-adjusted performance. Please rerun `calcPerformance()` with a risk-adjusted model first.'))
  if (plot.type == 'multiple' & !chk.rs) stop(message('This plot can only be generated for risk-adjusted performance. Please rerun `calcPerformance()` with a risk-adjusted model first.'))
  if (plot.type == 'oe' & !chk.rs) stop(message('OE ratios are not included in performance dataframe. Please rerun `calcPerformance()` with a risk-adjusted model to obtain OE ratios first.'))
  if (plot.type == 'pe' & !chk.rs) stop(message('PE ratios are not included in performance dataframe. Please rerun `calcPerformance()` with a risk-adjusted model to obtain PE ratios first.'))

  mu = sum(df$observed) / sum(df$n)

  if(plot.type == 'OR'){
    df$or.rank = rank(df$intercept.OR, ties.method = 'random')
    fig <- ggplot2::ggplot(data = df, ggplot2::aes(x = .data$intercept.OR, y = .data$or.rank)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$intercept.sig), size = 2) +
      ggplot2::geom_errorbar(ggplot2::aes(xmin = .data$intercept.OR.lwr, xmax = .data$intercept.OR.upr), width = 0.1, position = ggplot2::position_dodge(width = .7)) +
      ggplot2::scale_color_manual(values = c('black', 'red')) +
      ggplot2::scale_x_continuous(trans = 'log10') +
      ggplot2::geom_vline(xintercept = 1, lty = 2) +
      ggplot2::xlab('Odds ratio of entity-level effect') +
      ggplot2::ylab('Entity') +
      ggplot2::theme_classic() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(),
        panel.grid.minor = ggplot2::element_line(),
        axis.text = ggplot2::element_text(size = 16),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.length.y = ggplot2::unit(0,'cm'),
        axis.ticks.length.x = ggplot2::unit(.25, 'cm'),
        axis.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 55, vjust = 0.7),
        legend.position = 'none'
      )
  }

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
        axis.text = ggplot2::element_text(size = 16),
        axis.ticks.length = ggplot2::unit(.25,"cm"),
        axis.title = ggplot2::element_text(size = 18, face = "bold"),
        legend.position = 'top',
        legend.text = ggplot2::element_text(size = 16),
        legend.title = ggplot2::element_text(size = 18, face = 'bold')
      )
  }

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
      ggplot2::geom_hline(yintercept = mu, col = 'red', lty = 'dashed', size = 1.2, alpha = 0.7) +
      ggplot2::xlab('Entity rank') +
      ggplot2::ylab('Measure performance') +
      ggplot2::facet_wrap( ~ method, nrow = 1) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(),
        panel.grid.minor = ggplot2::element_line(),
        axis.text = ggplot2::element_text(size = 16),
        axis.ticks.length = ggplot2::unit(.25,"cm"),
        axis.title = ggplot2::element_text(size = 18, face = "bold"),
        strip.text = ggplot2::element_text(size = 18, face = "bold"),
        legend.text = ggplot2::element_text(size = 16),
        legend.title = ggplot2::element_blank(),
        legend.position = 'bottom'
      )
  }


  if (plot.type == 'p' || plot.type == 'oe' || plot.type == 'pe'){
    if (plot.type == 'p') {
      df$y = df$p
      df$rank = df$rank.p
      df$lwr = df$p.lwr
      df$upr = df$p.upr
      ylab = 'Unadjusted rate'
    } else if (plot.type == 'oe') {
      df$y = df$rs.oe
      df$rank = df$rank.oe
      df$lwr = df$rs.oe.lwr
      df$upr = df$rs.oe.upr
      ylab = 'OE risk-standardized rate'
    } else if (plot.type == 'pe') {
      df$y = df$rs.pe
      df$rank = df$rank.pe
      df$lwr = df$rs.pe.lwr
      df$upr = df$rs.pe.upr
      ylab = 'PE risk-standardized rate'
    }

    fig <- ggplot2::ggplot(data = df, ggplot2::aes(x = .data$rank, y = .data$y)) +
      ggplot2::geom_point(color = 'black') +
      ggplot2::geom_errorbar(aes(ymin = .data$lwr, ymax = .data$upr), width = 0.1) +
      ggplot2::geom_hline(yintercept = mu, col = 'red', lty = 'dashed', size = 1.2, alpha = 0.7) +
      ggplot2::xlab('Rank') +
      ggplot2::ylab(ylab) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(),
        panel.grid.minor = ggplot2::element_line(),
        axis.text = ggplot2::element_text(size = 16),
        axis.ticks.length = ggplot2::unit(.25,"cm"),
        axis.title = ggplot2::element_text(size = 18, face = "bold"),
        legend.position = 'none'
      )
  }

  fig
}
