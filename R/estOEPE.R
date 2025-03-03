#' Calculate measure performance by accountable entity
#' @description
#' This function calculates measure performance by accountable entity.
#' @param df dataframe; if null, will use the dataframe in the model object
#' @param model model; if null, will use an unadjusted model
#' @param entities list of accountable entities
#' @param entity variable to use as the accountable entity; default = "entity"
#' @param y variable to use as the outcome; default = "y"
#' @returns Estimated risk-standardized measure performance by accountable entity
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom stats aggregate predict
#' @importFrom lme4 glmer 
#' @export

estOEPE <- function(df, model, entities, entity = "entity", y = "y"){
  df$y         <- df[[y]]
  df$entity    <- df[[entity]]

  # refit model
  fit <- lme4::glmer(model, data = df, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'), nAGQ = 0)
  
  # calculate predictions w/ and w/o random intercept
  df$expect    <- predict(fit, newdata = df, type = 'response', re.form = ~0)
  df$predict   <- predict(fit, newdata = df, type = 'response')
  
  # calculate oe and pe ratios
  obs    <- aggregate(y ~ entity, data = df, sum)$y
  pred   <- aggregate(predict ~ entity, data = df, sum)$predict
  exp    <- aggregate(expect ~ entity, data = df, sum)$expect
  oe     <- obs / exp
  pe     <- pred / exp
  
  # calculate OE and PE risk-standardized rates
  marg.p <- mean(df$y)
  rs.oe  <- oe * marg.p
  rs.pe  <- pe * marg.p
  
  # calculate direct standardized rates
  rs.direct = vector(length = length(entities))
  df.ds <- df
  for (j in 1:length(entities)){
    df.ds$sta3n = entities[j]
    rs.direct[j] = mean(predict(fit, newdata = df.ds, type = 'response'))
  }
  
  out <- list(oe = oe, pe = pe, rs.oe = rs.oe, rs.pe = rs.pe, rs.direct = rs.direct)
  return(out)
}