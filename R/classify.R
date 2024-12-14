classify <- function(res=NULL, m=NULL, y, provider, method){
  if (method=='quintiles'){
    x = res$oe
    class <- cut(x, quantile(x, 0:5/5), labels = 1:5, include.lowest = T)
  }
  if (method=='lmh.random.int'){
    ranef <- as.data.frame(ranef(m))
    ranef1 <- ranef[ranef$term=='(Intercept)',]
    ranef1$est <- ranef1$condval
    ranef1$lwr <- ranef1$condval - 1.96*ranef1$condsd
    ranef1$upr <- ranef1$condval + 1.96*ranef1$condsd
    class <- rep(2, nrow(ranef1))
    class[ranef1$lwr > 0] <- 1
    class[ranef1$upr < 0] <- 3
  }
  if (method=='lmh.OE'){
    class <- rep(2, nrow(res))
    class[res$oe.lwr > 1] <- 1
    class[res$oe.upr < 1] <- 3
  }
  as.numeric(class)
}