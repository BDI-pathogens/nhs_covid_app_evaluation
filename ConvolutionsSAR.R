# Authors:  Christophe Fraser, Chris Wymant, Luca Ferretti
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# WARNING: command below deletes all objects currently in R's memory
rm(list=ls())

# A csv file with columns startDate, receivedRiskyContactNotification,
# and hasHadRiskyContact_AND_receivedPositiveTestResult. These are explained in
# supplementary material. Briefly: the date, the number of people notified,
# and the number of people reporting a positive test result after having been
# recently notified.
file.app.data <- "notifications_and_positives.csv" 

##############################################################################
# PARAMETERS:

# Time step size
dt <- 0.1

# Parameters for the exposure-to-notification distribution. These are the
# median values of all days in December.
mean.exposure.to.notification <- 2.6 # days. 
variance.exposure.to.notification <- 2.2^2 # days^2

#Incubation period: lognormal with mean 5.42 days and SD 2.7 days
mean.log.exposure.to.symptoms <- 1.58
sd.log.exposure.to.symptoms <- 0.47 

# Symptoms to positive distribution parameters
mean.symptoms.to.positive <- 3 # days
variance.symptoms.to.positive <- 2.5 # days^2

# Neglect the probability that notification to positive could take longer than
# this many days:
max.days.n.to.p <- 30

# When calculating the likelihood of the observed number of positives given
# the notifications on previous days (and the SAR), censor this many days at the
# start (because positives lag behind notifications, so "burn in" is needed to
# get enough data).
first.n.days.to.censor <- 7

################################################################################
# CALCULATION

# NB Gamma distribution: shape = mean^2/variance, scale = variance/mean  
sh.exposure.to.notification <- (mean.exposure.to.notification)^2/variance.exposure.to.notification
sc.exposure.to.notification <- variance.exposure.to.notification/mean.exposure.to.notification
pdf.exposure.to.notification <- Vectorize(function(tau) {
  return(dgamma(tau,shape = sh.exposure.to.notification, scale = sc.exposure.to.notification))
}) 

pdf.exposure.to.symptoms <- Vectorize(function(tau) { # aka the incubation period
  return(dlnorm(tau, meanlog = mean.log.exposure.to.symptoms, 
                sdlog = sd.log.exposure.to.symptoms))
}) 

sh.symptoms.to.positive <- (mean.symptoms.to.positive)^2/variance.symptoms.to.positive
sc.symptoms.to.positive <- variance.symptoms.to.positive/mean.symptoms.to.positive
pdf.symptoms.to.positive <- Vectorize(function(tau) { 
  return(dgamma(tau,shape = sh.symptoms.to.positive, scale = sc.symptoms.to.positive))
}) 

#convolution of exposure to symptoms, and symptoms to positive
pdf.exposure.to.positive <- Vectorize(function(tau) {
  if (tau==0) return(0) else {
    dtau = tau/100
    s <- seq(0, tau, by = dtau)
    conv <- dtau * sum(pdf.exposure.to.symptoms(s) * pdf.symptoms.to.positive(tau - s))
    return(conv)
  }
})

tau.precompute <- seq(0,100,by=dt)
pdf.exposure.to.positive.precompute <- sapply(tau.precompute, pdf.exposure.to.positive)

tau2.precompute <- seq(-10,90,by=dt)
convolution <- tau2.precompute
for(i in 1:length(tau2.precompute)) {
  tau2 <- tau2.precompute[i]
  conv <- 0
  blist <- seq(0,min(100,100-tau2),by=dt)
  for(j in 1:length(blist)){
    b <- blist[j]
    if(tau2+b>0) {
      # tau2 = notification-to-positive time. b = exposure-to-notification time.
      # tau2 + b = exposure-to-positive time
      conv <- conv + pdf.exposure.to.positive.precompute[1+(tau2+b)/dt] * 
        pdf.exposure.to.notification(b) 
    }
  }
  conv <- conv * dt
  convolution[i] <- conv
}

# A df with the values of notification-to-positive pdf, by time step
df <- tibble(tau = tau2.precompute, pdf = convolution)
df$pdf <- df$pdf/sum(df$pdf)

# A df with the values of notification-to-positive pdf, discretised by day
df.discrete <- tibble(tau = (-10):max.days.n.to.p,
                      pmf = NA_real_)
for (i in 1:nrow(df.discrete)) {
  df.discrete$pmf[[i]] <- df %>%
    filter(tau < df.discrete$tau[[i]] + 0.5,
           tau >= df.discrete$tau[[i]] - 0.5) %>%
    pull(pdf) %>%
    sum()
}
# Neglect negative delays (testing positive before being notified)
df.discrete <- df.discrete %>% filter(tau>=0)
df.discrete$pmf <- df.discrete$pmf/sum(df.discrete$pmf)

sar.df <- read_csv(file.app.data)
sar.df <- sar.df %>%
  # The receivedRiskyContactNotification field was introduced in the release on Dec 17:
  filter(startDate >= as.Date("2020-12-17")) %>% 
  select(startDate, receivedRiskyContactNotification,
         hasHadRiskyContact_AND_receivedPositiveTestResult)

# Check that dates in the df consistently increase by 1
stopifnot(sar.df$startDate == sort(sar.df$startDate))
num.days <- nrow(sar.df)
stopifnot(as.integer(difftime(sar.df$startDate[[num.days]], sar.df$startDate[[1]], 
                              units = "days")) == num.days - 1L)
sar.df$day.as.int <- 1:num.days

# Calculate the number of positives-among-those-recently-notified that would be
# expected if the SAR were 1:
sar.df$positives.expected.with.unit.sar <- 0 
for (day.positive in 1:num.days) {
  for (days.since.notification in 0:min(day.positive - 1, max.days.n.to.p)) {
    sar.df$positives.expected.with.unit.sar[[day.positive]] <-
      sar.df$positives.expected.with.unit.sar[[day.positive]] +
      sar.df$receivedRiskyContactNotification[[day.positive - days.since.notification]] *
      df.discrete$pmf[[days.since.notification + 1]]
  }
}

# Calculate the negative log likelihood as a function of SAR. Assume Poisson
# distribution for numbers of positives observed given the number expected.
neg.log.likelihood <- Vectorize(function(sar) {
  positives.expected <- sar.df$positives.expected.with.unit.sar * sar
  neg_log_likelihood_by_day <- positives.expected - 
    sar.df$hasHadRiskyContact_AND_receivedPositiveTestResult * 
    log(positives.expected)
  sum(neg_log_likelihood_by_day[seq(first.n.days.to.censor + 1, num.days)]) # censor early days
})
sar.ml <- optimize(neg.log.likelihood, c(0.001, 0.999))$minimum 

# Calculate 95% confidence interval by likelihood profiling
best.neg.log.likelihood <- neg.log.likelihood(sar.ml)
sar.ml.lower <- uniroot(function(sar) {
  neg.log.likelihood(sar) - 1.92 - best.neg.log.likelihood},
  lower = 0.001, upper = sar.ml)$root
sar.ml.upper <- uniroot(function(sar) {
  neg.log.likelihood(sar) - 1.92 - best.neg.log.likelihood},
  lower = sar.ml, upper = 0.99)$root

################################################################################
# RESULT
cat(paste0("SAR = ", round(100 * sar.ml, 2), "% (", round(100 * sar.ml.lower, 2),
           " - ", round(100* sar.ml.upper, 2), "%)"))

################################################################################
# DEPRECATED THINGS

# Our original SAR calculation calculated one value of SAR per day, summarising
# these values with a median:
use.deprecated.daily.sar <- FALSE
if (use.deprecated.daily.sar) {
  # Calculate the denominator terms appearing in the final expression for SAR.
  # For each day on which one could test positive, y, sum notifications from
  # previous days x multiplied by the probability of a notification-to-positive
  # delay being y-x.
  sar.df$sar.denominator.term <- NA_real_
  for(y in 1:num.days) {
    denominator_today <- 0
    for (x in 1:num.days) {
      notification.to.positive <- y - x
      if (notification.to.positive >= 0 && notification.to.positive <= max.days.n.to.p) {
        denominator_today <- denominator_today +
          df.discrete$pmf[[notification.to.positive + 1]] * 
          sar.df$receivedRiskyContactNotification[[x]]
      }
    }
    sar.df$sar.denominator.term[[y]] <- denominator_today
  }
  
  # For each day t, calculate SAR(t) by summing over the contributing
  # y terms as shown in our final expression for SAR(t)
  sar.df$sar <- NA_real_
  for(t in 1:num.days) {
    sar_today <- 0
    for (y in 1:num.days) {
      notification.to.positive <- y - t
      if (notification.to.positive >= 0 && notification.to.positive <= max.days.n.to.p) {
        sar_today <- sar_today +
          sar.df$hasHadRiskyContact_AND_receivedPositiveTestResult[[y]] * 
          df.discrete$pmf[[notification.to.positive + 1]] /
          sar.df$sar.denominator.term[[y]]
      }
    }
    sar.df$sar[[t]] <- sar_today
  }
  
  # The P_NP(t1|t2) matrix defined in our supplement: the probability of being
  # notified at time t1 conditional on testing positive at time t2. The matrix
  # row indexes t1 and the column indexes t2.
  matrix_p_np <- array(0, dim = c(num.days, num.days))
  for (t1 in 1:num.days) {
    for (t2 in 1:num.days) {
      notification.to.positive <- t2 - t1
      if (notification.to.positive < 0 ||
          notification.to.positive > max.days.n.to.p) next
      x.range.for.sum <- seq(max(1, t2 - max.days.n.to.p), t2)
      denominator <- sum(df.discrete$pmf[t2 - x.range.for.sum + 1] *
                           sar.df$receivedRiskyContactNotification[x.range.for.sum])
      matrix_p_np[t1, t2] <- df.discrete$pmf[[notification.to.positive + 1]] * 
        sar.df$receivedRiskyContactNotification[[t1]] / denominator
    }
  }
  # Summing over one column of this matrix is summing over t1, which by definition
  # gives 1.
  tolerance <- 1e-10
  stopifnot(max(abs(colSums(matrix_p_np) - 1)) < tolerance)
  
  # Calculate the SAR each day using the P_NP(t1|t2) matrix as an alternative.
  # Verify they agree.
  sar.df$sar.alt <- NA_real_
  for(t in 1:num.days) {
    y.range.for.sum <- seq(t, min(t + max.days.n.to.p, num.days))
    sar.df$sar.alt[[t]] <- sum(matrix_p_np[t, y.range.for.sum] *
                                 sar.df$hasHadRiskyContact_AND_receivedPositiveTestResult[y.range.for.sum]) /
      sar.df$receivedRiskyContactNotification[[t]]
  }
  stopifnot(max(abs(sar.df$sar - sar.df$sar.alt)) < tolerance)
  
  sar.median <- sar.df %>%
    filter(! censored) %>%
    pull(sar) %>%
    median()
  sar.median
  
}

