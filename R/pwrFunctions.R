pwrFunProp <- function(p=0.5,n,cutoff,vals=10,
                       direction=c("greater.than","less.than")){
  if (!all(sapply(list(p,n,cutoff,vals),is.numeric)))
    stop("p, n, cutoff and vals must be numbers.")
  direction <- match.arg(direction)
  if(direction == "less.than") p <- seq(0,p,length.out = round(vals))
  else p <- seq(p,1,length.out = round(vals))
  if(direction == "less.than"){
    power <- pbinom(q = round(cutoff), size = round(n), prob = p)
  } else {
    power <- pbinom(q = round(cutoff) - 1, size = round(n), prob = p,
                    lower.tail = FALSE)
  }
  cbind(p=round(p,3),power=round(power,3))
}
## example 9.1-1
pwrFunProp(p = 0.5, n = 20, cutoff = 6, direction = "less.than")
pout <- pwrFunProp(p = 0.5, n = 20, cutoff = 6, direction = "less.than")
plot(pout, type="l")
## critical region defined by y >= 14 for n = 20
pwrFunProp(n = 20, cutoff = 14)
pout <- pwrFunProp(n = 20, cutoff = 14)
plot(pout, type="l")
## critical region defined by y >= 14 for n = 40
pwrFunProp(n = 40, cutoff = 28)
pout <- pwrFunProp(n = 40, cutoff = 28)
plot(pout, type="l")


pwrFunMean <- function(null,alt,n,sd,vals=10,mu=NULL,
                       direction=c("greater.than","less.than")){
  if (!all(sapply(list(null,alt,n,sd,vals),is.numeric)))
    stop("null, alt, n, sd and vals must be numbers.")
  if(is.null(mu)){
    if(alt < (null+round(vals))) mu <- seq(null,(null+round(vals)))
    else mu <- seq(null,(alt+round(vals)))
  }
  power <- pnorm(q = alt,mean = mu,sd = sd,
                 lower.tail = direction == "less.than")
  cbind(mu=mu,power=round(power,3))
}
## example 9.1-2
## Probability x-bar >= 62 for various alternative "true" means
pwrFunMean(null = 60, alt = 62, n = 25, sd = 2)
pout <- pwrFunMean(null = 60, alt = 62, n = 25, sd = 2)
plot(pout, type="l")

## supply your own values of mu
pwrFunMean(null = 60, alt = 62, n = 25, sd = 2, mu=60:68)
pout <- pwrFunMean(null = 60, alt = 62, n = 25, sd = 2, mu=60:68)
plot(pout, type="l")

## Probability x-bar <= mu=62 for various alternative means
pwrFunMean(null = 60, alt = 62, n = 25, sd = 2, direction = "less.than")
pout <- pwrFunMean(null = 60, alt = 62, n = 25, sd = 2)
plot(pout, type="l")

