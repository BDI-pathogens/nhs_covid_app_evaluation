library(tidyverse)
rm(list=ls())

pdf.exposure.to.notification <- function(tau) {
  mean.exposure.to.notification <- 2.6 # days
  variance.exposure.to.notification <- 2.2^2 # days^2
  # from Luca, median December
  sh <- (mean.exposure.to.notification)^2/variance.exposure.to.notification
  sc <- variance.exposure.to.notification/mean.exposure.to.notification
  # shape = mean^2/variance, scale = variance/mean  
  return(dgamma(tau,shape = sh, scale = sc))
} 


pdf.exposure.to.symptoms <- function(tau) { # aka the incubation period
  #Incubation period: lognormal with mean 5.42 and SD 2.7
  mean.log.exposure.to.symptoms <- 1.58 # days
  sd.log.exposure.to.symptoms <- 0.47 # days
  return(dlnorm(tau, meanlog = mean.log.exposure.to.symptoms, 
                sdlog = sd.log.exposure.to.symptoms))
} 


pdf.symptoms.to.positive <- function(tau) { 
  mean.symptoms.to.positive <- 3 # days
  variance.symptoms.to.positive <- 2.5 # days^2
  sh <- (mean.symptoms.to.positive)^2/variance.symptoms.to.positive
  sc <- variance.symptoms.to.positive/mean.symptoms.to.positive
  # shape = mean^2/variance, scale = variance/mean  
  return(dgamma(tau,shape = sh, scale = sc))
} 


pdf.exposure.to.positive <- function(tau) {
  if (tau==0) return(0) else {
  #convolution of exposure to symptoms, and symptoms positive
  dtau = tau/100
  s <- seq(0, tau, by = dtau)
  conv <- 0
  for (k in s) conv <- conv + pdf.exposure.to.symptoms(k)*pdf.symptoms.to.positive(tau-k)*dtau
  return(conv)
  }
}


xax <- seq(0,40,by=0.01)
plot(xax, sapply(xax, pdf.exposure.to.positive))
sum(xax* sapply(xax, pdf.exposure.to.positive)*0.01)
sum(xax^2* sapply(xax, pdf.exposure.to.positive)*0.01)-9.42^2


dt <- 0.1
tau.precompute <- seq(0,100,by=dt)
pdf.exposure.to.positive.precompute <- sapply(tau.precompute, pdf.exposure.to.positive)

plot(tau.precompute, pdf.exposure.to.positive.precompute)

tau2.precompute <- seq(-10,90,by=dt)
convolution <- tau2.precompute
#convolution.alt <- tau2.precompute
for(i in 1:length(tau2.precompute)) {
  tau2 <- tau2.precompute[i]
#  conv.alt <- 0
  conv <- 0
  blist <- seq(0,min(100,100-tau2),by=dt)
  for(j in 1:length(blist)){
    b <- blist[j]
    if(tau2+b>0) {
      conv <- conv + pdf.exposure.to.positive.precompute[1+(tau2+b)/dt]*pdf.exposure.to.notification(b) * dt
#      conv.alt <- conv.alt + pdf.exposure.to.positive(tau2+b) * pdf.exposure.to.notification(b) * dt
    }
  }
#  print(c(conv,conv.alt))
  convolution[i] <- conv
#  convolution.alt[i] <- conv.alt
}

df <- tibble(tau = tau2.precompute, pdf = convolution)
df$pdf <- df$pdf/sum(df$pdf)

sum(df$tau*df$pdf)
sum(df$tau^2*df$pdf)-sum(df$tau*df$pdf)



ggplot(data = df) +
  geom_polygon(mapping = aes(x = tau, y = pdf)) +
  ggtitle("Time interval between receiving exposure notification and receiving positive test") +
  labs(subtitle = "for the case when the person was infected at the exposure event that led to the notification") +
  xlab("Days elapsed") +
  ylab("Probability density function") +
  theme(
    text = element_text(size=16),
    plot.title = element_text(size=18)
  )

df.discrete <- tibble(tau = (-10):30, pmf = vector(length = 41, mode="double"))
for (i in 1:nrow(df.discrete)) {
  df.discrete$pmf[i] <- as.numeric( df %>% 
    filter(tau < df.discrete$tau[i]+0.5) %>%
    filter(tau >= df.discrete$tau[i]-0.5) %>%
    summarize(pmf = sum(pdf)) 
  )
}
df.discrete$pmf <- df.discrete$pmf/sum(df.discrete$pmf)

df.discrete <- df.discrete %>% filter(tau>=0)
df.discrete$pmf <- df.discrete$pmf/sum(df.discrete$pmf)

plot(df.discrete$tau, df.discrete$pmf)

#sar.df <- read_csv(file = "notifications.csv")
sar.df <- read_csv(file = "notifications_ChrisEdit2.csv")
sar.df.alt <- read_csv(file = "notifications_from_24sep20_with_cb.csv")

plot(sar.df.alt$date, sar.df.alt$hasHadRiskyContact)
plot(sar.df.alt$date, sar.df.alt$iOS_exposure_notification_cb_count/1.6, pch=1, type = "b")
points(sar.df.alt$date, sar.df.alt$receivedRiskyContactNotification, pch = 16)
plot(sar.df.alt$date, sar.df.alt$iOS_exposure_notification_cb_count/sar.df.alt$receivedRiskyContactNotification, pch=17, type = "b")

plot(sar.df.alt$date, sar.df.alt$receivedPositiveTestResult)
plot(sar.df.alt$date, sar.df.alt$hasHadRiskyContact_AND_receivedPositiveTestResult)
plot(sar.df.alt$date, 24*sar.df.alt$hasHadRiskyContact_AND_receivedPositiveTestResult/sar.df.alt$hasHadRiskyContact)
abline(h=0.1)
sar.df.alt$compositeRiskyContactNotification <- pmax( sar.df.alt$iOS_exposure_notification_cb_count/1.6, 
                                                     sar.df.alt$receivedRiskyContactNotification, na.rm = T)
plot(sar.df.alt$date, sar.df.alt$compositeRiskyContactNotification)

#sar.df <- sar.df %>% filter(startDate>as.Date("2020-12-23", format = "%Y-%m-%d"))
# filter to look at the effect of excluding the peak in notifications

plot(sar.df$startDate, sar.df$receivedRiskyContactNotification, log="y")
points(sar.df$startDate, sar.df$hasHadRiskyContact_AND_receivedPositiveTestResult)
sar.df$simple.ratio <- sar.df$hasHadRiskyContact_AND_receivedPositiveTestResult / sar.df$receivedRiskyContactNotification
plot(sar.df$simple.ratio, ylim=c(0,0.1))


denominator.sum <- vector(length = nrow(sar.df), mode="double")
denominator.date <- sar.df$startDate
for(i in 1:length(denominator.sum)) {
  tmp <- 0
  for (j in 1:length(denominator.sum)){
    interval <- as.numeric(difftime(denominator.date[i],denominator.date[j],units = c("days")))
    if(interval>=0&&interval<=30)
#      tmp <- tmp + df.discrete$pmf[interval+11]*sar.df$receivedRiskyContactNotification[j]
    tmp <- tmp + df.discrete$pmf[interval+1]*sar.df$receivedRiskyContactNotification[j]
    #    print(c(interval,tmp))
  }
  denominator.sum[i]<-tmp
}

sar <- vector(length = nrow(sar.df), mode="double")
for(i in 1:length(denominator.sum)) {
  tmp <- 0
  for (j in 1:length(denominator.sum)){
    interval <- as.numeric(difftime(denominator.date[j],denominator.date[i],units = c("days")))
    if(interval>=0&&interval<=30)
#      tmp <- tmp + sar.df$hasHadRiskyContact_AND_receivedPositiveTestResult[j]*df.discrete$pmf[interval+11]/denominator.sum[j]
    tmp <- tmp + sar.df$hasHadRiskyContact_AND_receivedPositiveTestResult[j]*df.discrete$pmf[interval+1]/denominator.sum[j]
    #     tmp <- tmp + sar.df$hasHadRiskyContact_AND_receivedPositiveTestResult[i]*df.discrete$pmf[interval+11]/denominator.sum[j]
      #    print(c(interval,tmp))
  }
  sar[i]<-tmp
}

plot(denominator.date, sar, ylim=c(0,0.2))
sar.median <-median(sar[6:28])
abline(h=sar.median)

df.discrete$cmf <- df.discrete$pmf
for (i in 2:nrow(df.discrete)){
  df.discrete$cmf[i] <- df.discrete$cmf[i-1]+df.discrete$pmf[i]
}
view(df.discrete)

sar*(1-df.discrete$pmf[10])

sar

view(sar.df)

phe.sar <- data.frame(
  combined = c("direct.SGTF", "close.SGTF", "direct.WT", "close.WT"),
  contacts = c(239922, 22710, 240491, 21038),
  cases = c(37362, 1914, 27495, 1266)
)
sum(phe.sar$cases)/sum(phe.sar$contacts)
(1914+1266)/(22710+21038)


#####

sar.df.alt <- sar.df.alt[69:126,]

plot(sar.df.alt$date, sar.df.alt$compositeRiskyContactNotification, log="y", ylim=c(100,50000))
points(sar.df.alt$date, sar.df.alt$hasHadRiskyContact_AND_receivedPositiveTestResult)
sar.df.alt$simple.ratio <- sar.df.alt$hasHadRiskyContact_AND_receivedPositiveTestResult / sar.df.alt$compositeRiskyContactNotification
plot(sar.df.alt$simple.ratio, ylim=c(0,0.1))


denominator.sum <- vector(length = nrow(sar.df.alt), mode="double")
denominator.date <- sar.df.alt$date
for(i in 1:length(denominator.sum)) {
  tmp <- 0
  for (j in 1:length(denominator.sum)){
    interval <- as.numeric(difftime(denominator.date[i],denominator.date[j],units = c("days")))
    if(interval>=0&&interval<=30)
      #      tmp <- tmp + df.discrete$pmf[interval+11]*sar.df$receivedRiskyContactNotification[j]
      tmp <- tmp + df.discrete$pmf[interval+1]*sar.df.alt$compositeRiskyContactNotification[j]
    #    print(c(interval,tmp))
  }
  denominator.sum[i]<-tmp
}

sar <- vector(length = nrow(sar.df.alt), mode="double")
for(i in 1:length(denominator.sum)) {
  tmp <- 0
  for (j in 1:length(denominator.sum)){
    interval <- as.numeric(difftime(denominator.date[j],denominator.date[i],units = c("days")))
    if(interval>=0&&interval<=30)
      #      tmp <- tmp + sar.df$hasHadRiskyContact_AND_receivedPositiveTestResult[j]*df.discrete$pmf[interval+11]/denominator.sum[j]
      tmp <- tmp + sar.df.alt$hasHadRiskyContact_AND_receivedPositiveTestResult[j]*df.discrete$pmf[interval+1]/denominator.sum[j]
    #     tmp <- tmp + sar.df$hasHadRiskyContact_AND_receivedPositiveTestResult[i]*df.discrete$pmf[interval+11]/denominator.sum[j]
    #    print(c(interval,tmp))
  }
  sar[i]<-tmp
}

plot(denominator.date, sar, ylim=c(0,0.2))
sar.median <-median(sar[20:50])
abline(h=sar.median)
sar.median
