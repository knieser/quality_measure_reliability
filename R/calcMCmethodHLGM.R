calcMCmethodHLGM <- function(df, n, var.b, reps){
  entities.id <- data.frame(
    id = 1:length(n),
    entity = unique(df$entity)
  )

  df <- merge(df, entities.id, by = 'entity')

  sims <- replicate(reps, {
    # Step 1: Simulate random intercepts
    u <- rnorm(length(n), 0, sqrt(var.b))

    # Step 2: Compute outcome probabilities, along with means and variances of outcome probabilities
    df$expect.logit <- log(df$expect / (1 - df$expect))
    df$p.s = plogis(df$expect.logit + u[df$id])
    df$v.s = df$p.s * (1 - df$p.s)
    m = aggregate(p.s ~ entity, data = df, mean)$p.s
    v = aggregate(v.s ~ entity, data = df, mean)$v.s
    cbind(m,v)
  })

# Step 3: Calculate variance components and reliability
  m = sims[,1,]
  v = sims[,2,]

  within = apply(v, 1, mean)

  m.avg = apply(m, 1, mean)
  m.centered = apply(m, 2, function(x) x - m.avg)
  between = apply(m.centered, 1, function(x) 1/(reps - 1)*sum(x^2))

  rel = between / (between + within / n)

  output = list(var.b.MC = between,
                var.w.MC = within / n,
                est.HLGM.MC = rel)
  return(output)
}
