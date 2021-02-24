library(ggplot2)
library(gridExtra)
library(reshape2)
library(lubridate)
library(dplyr)
library(RColorBrewer)
library(tidyr)

# functions

runmean<-function(x,k){ # extracts moving average
  k<-floor(k/2)
  n<-length(x)
  isna<-is.na(x)
  x[isna]<-0
  cxs<-cumsum(x)
  cxn<-cumsum(1-isna)
  cs<-c(rep(0,k+1),cxs,rep(cxs[n],k))
  cn<-c(rep(0,k+1),cxn,rep(cxn[n],k))
  ( cs[2*k+1+1:n]-cs[1:n] ) / ( cn[2*k+1+1:n]-cn[1:n] )
}

get_dates<-function(x){ # extracts date from relative time
  as.Date("2020-01-01")+x
}


# load table with circuit breaker data
cb<-read.csv("circuit_breaker_sep24_dec31.csv")[,-1]
# load table with cases
cc<-read.csv("ltlas_epi_counts.csv")
# load table with app analytics
dd<-read.csv("ltla_summary_per_day_sep24_jan10.csv",row.names=1)
# load table with app analytics for iOS only
dd_ios<-read.csv("ltla_summary_per_day_sep24_jan10_ios.csv",row.names=1)

# combine some LTLAs
dd$LAD20NM[dd$LAD20NM=="Hackney"]<-"Hackney and City of London"
dd$LAD20NM[dd$LAD20NM=="Cornwall"]<-"Cornwall and Isles of Scilly"
dd_ios$LAD20NM[dd_ios$LAD20NM=="Hackney"]<-"Hackney and City of London"
dd_ios$LAD20NM[dd_ios$LAD20NM=="Cornwall"]<-"Cornwall and Isles of Scilly"
# get list of LTLAs
adm<-sort(unique(dd$LAD20NM))
# get relative dates
dates_dd<-as.numeric(difftime(dd$date,"2020-01-01",units="days"))
dates_ddios<-as.numeric(difftime(dd_ios$date,"2020-01-01",units="days"))
dates_cc<-as.numeric(difftime(cc$date,"2020-01-01",units="days"))
dates_cb<-as.numeric(difftime(cb$date,"2020-01-01",units="days"))
# get range of dates
dates<-seq(min(dates_dd),max(dates_dd))
first_date<-min(dates)
last_date<-as.numeric(difftime("2020-12-31","2020-01-01",units="days"))-2  # last 2 days are noisy
date_v4.1<-as.numeric(difftime("2020-12-15","2020-01-01",units="days"))
# define phases
phase0<-seq(as.numeric(difftime("2020-09-30","2020-01-01",units="days")),as.numeric(difftime("2020-08-01","2020-01-01",units="days")))
phase1<-seq(as.numeric(difftime("2020-11-06","2020-01-01",units="days")),as.numeric(difftime("2020-10-08","2020-01-01",units="days")))
phase2<-seq(as.numeric(difftime("2020-12-31","2020-01-01",units="days")),as.numeric(difftime("2020-11-07","2020-01-01",units="days")))
phase3<-seq(as.numeric(difftime("2021-01-31","2020-01-01",units="days")),as.numeric(difftime("2020-01-01","2020-01-01",units="days")))

# extract statistics for date and LTLA
# users
users<-outer(dates,adm,Vectorize(function(d,a){(dd$users/dd$population)[dates_dd==d & dd$LAD20NM==a]}))
# partial days of quarantine (in practice, initial one)
partial<-outer(dates,adm,Vectorize(function(d,a){dd$isolation_exposure_partial_cts[dates_dd==d & dd$LAD20NM==a]}))
# full days of quarantine
full<-outer(dates,adm,Vectorize(function(d,a){dd$isolation_exposure_cts[dates_dd==d & dd$LAD20NM==a]}))
# cases among app users
cases<-outer(dates,adm,Vectorize(function(d,a){dd$test_positive[dates_dd==d & dd$LAD20NM==a]}))
# reeceived notifications 
received<-outer(dates,adm,Vectorize(function(d,a){dd$received_exposure_notification[dates_dd==d & dd$LAD20NM==a]}))
# start isolation 
start<-outer(dates,adm,Vectorize(function(d,a){dd$started_isolation[dates_dd==d & dd$LAD20NM==a]}))
colnames(start)<-adm
colnames(partial)<-adm
colnames(received)<-adm
colnames(full)<-adm
colnames(cases)<-adm
colnames(users)<-adm
# and for iOS
users_ios<-outer(dates,adm,Vectorize(function(d,a){(dd_ios$users/dd_ios$population)[dates_ddios==d & dd_ios$LAD20NM==a]}))
partial_ios<-outer(dates,adm,Vectorize(function(d,a){mean(dd_ios$isolation_exposure_partial_cts[dates_ddios==d & dd_ios$LAD20NM==a])}))
full_ios<-outer(dates,adm,Vectorize(function(d,a){mean(dd_ios$isolation_exposure_cts[dates_ddios==d & dd_ios$LAD20NM==a])}))
cases_ios<-outer(dates,adm,Vectorize(function(d,a){mean(dd_ios$test_positive[dates_ddios==d & dd_ios$LAD20NM==a])}))
start_ios<-outer(dates,adm,Vectorize(function(d,a){dd_ios$started_isolation[dates_ddios==d & dd_ios$LAD20NM==a]}))
received_ios<-outer(dates,adm,Vectorize(function(d,a){mean(dd_ios$received_exposure_notification[dates_ddios==d & dd_ios$LAD20NM==a])}))
received_ios[rowSums(received_ios)==0,]<-NA
received_ios[is.na(rowSums(received_ios)),]<-NA
colnames(start_ios)<-adm
colnames(received_ios)<-adm
colnames(partial_ios)<-adm
colnames(full_ios)<-adm
colnames(cases_ios)<-adm
colnames(users_ios)<-adm
# and Android
users_andr <- users - users_ios
partial_andr <- partial - partial_ios
full_andr <- full - full_ios
cases_andr <- cases - cases_ios
received_andr <- received - received_ios
start_andr <- start - start_ios
# cases in the whole population
totcases<-outer(dates,adm,Vectorize(function(d,a){mean(cc$num_cases[dates_cc==d & cc$authority==a])}))
totcases[is.na(totcases)]<-0
colnames(totcases)<-adm
# notifications from circuit breaker, summed across all adms
notifications<-sapply(dates,function(d){sum(cb$exposure_notification_cb_count[dates_cb==d],na.rm=T)})
notifications_ios<-sapply(dates,function(d){sum(cb$iOS_exposure_notification_cb_count[dates_cb==d],na.rm=T)})
notifications[notifications==0]<-NA
notifications_ios[notifications_ios==0]<-NA
notifications_andr <- notifications - notifications_ios
# define dates with reliable values for OS-specific circuit breaker and received notifications, exclude first 2 days due to transient
dates_received<-dates[!is.na(rowSums(received_ios))][-(1:2)]
dates_notification<-dates[!is.na(notifications_ios)][-(1:2)]

# MODELLING

# total notifications via circuit breaker until December31st
sum(notifications[dates<=last_date])

# correction factor for notifications for iOS/Android:
notifications_ios_adjusted<-notifications_ios
notifications_ios_adjusted[is.na(notifications_ios_adjusted)]<-notifications[is.na(notifications_ios_adjusted)]*mean(notifications_ios,na.rm=T)/mean(notifications_andr+notifications_ios,na.rm=T)
notifications_andr_adjusted<-notifications_andr
notifications_andr_adjusted[is.na(notifications_andr_adjusted)]<-notifications[is.na(notifications_andr_adjusted)]*mean(notifications_andr,na.rm=T)/mean(notifications_andr+notifications_andr,na.rm=T)
corr_factor_ios<-notifications_ios_adjusted[dates<=last_date]/rowSums(partial_ios[dates<=last_date,])
corr_factor_andr<-notifications_andr_adjusted[dates<=last_date]/rowSums(partial_andr[dates<=last_date,])
corr_factor_ios[1]<-0
corr_factor_andr[1]<-0
# estimate corrected numbers of partial and full days of quarantine
partial<-sweep(partial_ios[dates<=last_date,],1,corr_factor_ios,"*")+sweep(partial_andr[dates<=last_date,],1,corr_factor_andr,"*")
full<-sweep(full_ios[dates<=last_date,],1,corr_factor_ios,"*")+sweep(full_andr[dates<=last_date,],1,corr_factor_andr,"*")

# extract weekly averages
k<-7
notifications_w<-runmean(notifications,k)
partial_w<-(apply(partial,2,function(x){runmean(x,k)}))
full_w<-(apply(full,2,function(x){runmean(x,k)}))
cases_w<-(apply(cases[dates<=last_date,],2,function(x){runmean(x,k)}))
totcases_w<-(apply(totcases[dates<=last_date,],2,function(x){runmean(x,k)}))

# inference of approximate duration of the quarantine in days - assumes homogeneous duration across all LTLAs
full_daily<-rowSums(full)
partial_daily<-rowSums(partial)
# simplified version
# get_length_quarantine<-function(full, partial){
#   cumsum_partial<-cumsum(partial)
#   c(0,sapply(2:length(full), function(i){
#     i-which.min(full[i]<cumsum_partial[i]-cumsum_partial[1:i])+1
#   }))
# }
# full version
shift_quar<-3*(dates[dates<=last_date]>=(date_v4.1))[-1]
get_length_quarantine<-function(vfull, vpartial){
  nspline<-10
  n <- length(vpartial)
  to_optim <- function(x) {
    meanspline<-spline(seq(1,n-1,(n-2)/(nspline-1)),x[1:nspline],n-1,method = "natural")$y-shift_quar
    sdspline<-spline(seq(1,n-1,(n-2)/(nspline-1)),x[nspline+c(1:nspline)],n-1,method = "natural")$y
    (sum((vfull[-1] - sapply(2:n, function(j) {
      sum(
        #vpartial[1:(j - 1)] * (1 - pgamma(j - c(1:(j - 1)), shape=exp(2*meanspline[1:(j - 1)]-2*sdspline[1:(j - 1)]), scale=exp(-meanspline[1:(j - 1)]+2*sdspline[1:(j - 1)])))
        vpartial[1:(j - 1)] * (1 - pnorm(j - c(1:(j - 1)), mean=meanspline[1:(j - 1)], sd=pmax(0,sdspline[1:(j - 1)])))
      )  
    })) ^ 2) 
      + (mean(c(0,1-sdspline[sdspline<1]))+mean(c(0,-(14-0.5)+(meanspline+sdspline)[(meanspline+sdspline)>14-0.5])))*sum(vfull^2) 
    )
  }
  res<-list(); 
  res[[1]]<-optim((c(rep(7.3,nspline),rep(2,nspline))),to_optim,control = list(maxit=1000))
  res[[1]]<-optim(res[[1]]$par,to_optim,control = list(maxit=1000))
  for (i in 2:1000){
    res[[i]]<-optim((c(rep(7.3,nspline)*(0.5+runif(nspline)),rep(2,nspline)*(0.5+runif(nspline)))),to_optim,control = list(maxit=1000))
    res[[i]]<-optim(res[[i]]$par,to_optim,control = list(maxit=1000))
  }
  cbind(
    spline(seq(1,n-1,(n-2)/(nspline-1)),res[[which.min(sapply(res,function(z){z$value}))]]$par[1:nspline],n-1,method = "natural")$y,
    pmax(0,spline(seq(1,n-1,(n-2)/(nspline-1)),res[[which.min(sapply(res,function(z){z$value}))]]$par[nspline+c(1:nspline)],n-1,method = "natural")$y)
  )
}
delay_from_length<-function(l){
  res<-l
  #res[,1]<-14-pmax(0,pmin(3,dates[dates<=last_date]-(date_v4.1-14)))-l[,1]-0.5
  res[,1]<-14-l[,1]-0.5
  res
}
compute_delay<-TRUE
if(compute_delay){
  length_quarantine<-get_length_quarantine(full_daily,partial_daily)
  length_quarantine<-rbind(length_quarantine[1,],length_quarantine)
  delay_quarantine<-delay_from_length(length_quarantine)
} else {
  delay_quarantine<-read.csv("delay_quarantine.csv",row.names = 1)
}
# mean generation time from Ferretti, Ledda et al 2020
generation_time<-integrate(function(x){dweibull(x,shape=3.2862,scale=6.1244)*x},0,Inf)$value
# relative infectiousness profile in time, based on the above generation time
infectiousness<-function(t){sapply(1:dim(t)[1],function(j){mean(1-pweibull(qnorm(seq(0,1,0.001),t[j,1],t[j,2]),shape=3.2862,scale=6.1244))})}

# secondary attack rate
rate<-0.061

# reduction in transmission due to quarantine
fullquarantine_reduction<-0.975
# pessimistic estimates for mean, 1st and 3rd quartile, and lower/upper 95% credibility intervals
reduction<-0.11*fullquarantine_reduction+(0.65-0.11)*0.5
reduction_q1<-0.11*fullquarantine_reduction+(0.65-0.11)*0.25
reduction_q3<-0.11*fullquarantine_reduction+(0.65-0.11)*0.75
reduction_qd<-0.11*fullquarantine_reduction+(0.65-0.11)*0.025
reduction_qu<-0.11*fullquarantine_reduction+(0.65-0.11)*0.975
# optimistic estimates
reduction_opt<-0.8*fullquarantine_reduction+(1-0.8-0.12)*0.5
reduction_opt_q1<-0.8*fullquarantine_reduction+(1-0.8-0.12)*0.25
reduction_opt_q3<-0.8*fullquarantine_reduction+(1-0.8-0.12)*0.75
reduction_opt_qd<-0.8*fullquarantine_reduction+(1-0.8-0.12)*0.025
reduction_opt_qu<-0.8*fullquarantine_reduction+(1-0.8-0.12)*0.975

# compute cases averted per date and adm assuming small impact of the app, i.e. linearity
directly_averted_max_by_ltla<-(apply(sweep(partial_w,1,infectiousness(delay_quarantine),"*"),2,function(x){sum(x)}))*rate
averted_max<-totcases_w*(apply(sweep(partial_w/totcases_w,1,infectiousness(delay_quarantine),"*"),2,function(x){cumsum(x)}))*rate/generation_time
# pessimistic
averted_pes<-averted_max*reduction
averted_pes_q1<-averted_max*reduction_q1
averted_pes_q3<-averted_max*reduction_q3
averted_pes_qd<-averted_max*reduction_qd
averted_pes_qu<-averted_max*reduction_qu
# optimistic
averted_opt<-averted_max*reduction_opt
averted_opt_q1<-averted_max*reduction_opt_q1
averted_opt_q3<-averted_max*reduction_opt_q3
averted_opt_qd<-averted_max*reduction_opt_qd
averted_opt_qu<-averted_max*reduction_opt_qu
# central estimate
averted<-averted_max*(reduction+reduction_opt)/2
averted_q1<-averted_max*(reduction_q1+reduction_opt_q1)/2
averted_q3<-averted_max*(reduction_q3+reduction_opt_q3)/2
averted_qd<-averted_max*(reduction_qd+reduction_opt_qd)/2
averted_qu<-averted_max*(reduction_qu+reduction_opt_qu)/2
# daily and total cases averted
daily_averted<-rowSums(averted,na.rm=T)
daily_averted_q1<-rowSums(averted_q1,na.rm=T)
daily_averted_q3<-rowSums(averted_q3,na.rm=T)
daily_averted_qd<-rowSums(averted_qd,na.rm=T)
daily_averted_qu<-rowSums(averted_qu,na.rm=T)
tot_averted<-sum(daily_averted)
tot_averted_q1<-sum(daily_averted_q1)
tot_averted_q3<-sum(daily_averted_q3)
tot_averted_qd<-sum(daily_averted_qd)
tot_averted_qu<-sum(daily_averted_qu)
daily_averted_opt<-rowSums(averted_opt,na.rm=T)
daily_averted_opt_q1<-rowSums(averted_opt_q1,na.rm=T)
daily_averted_opt_q3<-rowSums(averted_opt_q3,na.rm=T)
daily_averted_opt_qd<-rowSums(averted_opt_qd,na.rm=T)
daily_averted_opt_qu<-rowSums(averted_opt_qu,na.rm=T)
tot_averted_opt<-sum(daily_averted_opt)
tot_averted_opt_q1<-sum(daily_averted_opt_q1)
tot_averted_opt_q3<-sum(daily_averted_opt_q3)
tot_averted_opt_qd<-sum(daily_averted_opt_qd)
tot_averted_opt_qu<-sum(daily_averted_opt_qu)
daily_averted_pes<-rowSums(averted_pes,na.rm=T)
daily_averted_pes_q1<-rowSums(averted_pes_q1,na.rm=T)
daily_averted_pes_q3<-rowSums(averted_pes_q3,na.rm=T)
daily_averted_pes_qd<-rowSums(averted_pes_qd,na.rm=T)
daily_averted_pes_qu<-rowSums(averted_pes_qu,na.rm=T)
tot_averted_pes<-sum(daily_averted_pes)
tot_averted_pes_q1<-sum(daily_averted_pes_q1)
tot_averted_pes_q3<-sum(daily_averted_pes_q3)
tot_averted_pes_qd<-sum(daily_averted_pes_qd)
tot_averted_pes_qu<-sum(daily_averted_pes_qu)
# daily actual cases after weekly smoothing, as a comparison
daily_totcases<-rowSums(totcases_w,na.rm=T)

# constants defining lower/upper credibility intervals
c_down<-(tot_averted_qd)/(tot_averted)
c_up<-(tot_averted_qu)/(tot_averted)


### load external information, including fraction of users, in df ###
df<-load(file_RData_fraction_users)
fracusers<-sapply(adm,function(x){mean(df$fraction_users[df$authority==x])})

# some regressions (linear, quadratic, log-log, and by phase)
y<-(1/(1+1/(colSums(averted)/colSums(totcases[dates<=last_date,]))))
lm_1<-lm(y ~ fracusers )
lm_2<-lm(y ~ fracusers + I(fracusers^2) + 0 )
lm_l<-lm(I(log(y)) ~ I(log(fracusers)))
y_p1<-(1/(1+1/(colSums(averted[dates[dates<=last_date] %in% phase1,])/colSums(totcases[dates<=last_date & dates %in% phase1,]))))
lm_p1<-lm(y_p1 ~ fracusers )
y_p2<-(1/(1+1/(colSums(averted[dates[dates<=last_date] %in% phase2,])/colSums(totcases[dates<=last_date & dates %in% phase2,]))))
lm_p2<-lm(y_p2 ~ fracusers )

# coefficients for dependence of cases averted on fraction of users
coeffs_quad<-lm(y ~ fracusers + I(fracusers^2) + 0 )$coefficients
coeffs<-lm(y ~ fracusers )$coefficients


# Counterfactuals:
# increasing adherence by 20%
0.2*2/(reduction+reduction_opt)*(tot_averted)/sum(totcases[dates<=last_date,])
0.2*2/(reduction+reduction_opt)*(tot_averted-(tot_averted_q3 - tot_averted_q1)*0.475/0.5)/sum(totcases[dates<=last_date,])
0.2*2/(reduction+reduction_opt)*(tot_averted+(tot_averted_q3 - tot_averted_q1)*0.475/0.5)/sum(totcases[dates<=last_date,])
# reducing delays by 1 day
delay_reduced<-delay_quarantine; delay_reduced[,1]<-delay_reduced[,1]-1
averted_max_delay2<-totcases_w*(apply(sweep(partial_w/totcases_w,1,infectiousness(delay_reduced),"*"),2,function(x){cumsum(x)}))*rate/generation_time
(sum(averted_max_delay2)/sum(averted_max)-1)*(tot_averted)/sum(totcases[dates<=last_date,])
(sum(averted_max_delay2)/sum(averted_max)-1)*(tot_averted-(tot_averted_q3 - tot_averted_q1)*0.475/0.5)/sum(totcases[dates<=last_date,])
(sum(averted_max_delay2)/sum(averted_max)-1)*(tot_averted+(tot_averted_q3 - tot_averted_q1)*0.475/0.5)/sum(totcases[dates<=last_date,])
# how many cases could have been avoided if users would be at least 35.9% of the population everywhere?
sum((predict(lm_1,data.frame(fracusers=pmax(0.359,fracusers)))/predict(lm_1,data.frame(fracusers=fracusers))-1)*colSums(averted))/sum(totcases[dates<=last_date,])
(1-(tot_averted_q3 - tot_averted_q1)*0.475/0.5/tot_averted)*sum((predict(lm_1,data.frame(fracusers=pmax(0.359,fracusers)))/predict(lm_1,data.frame(fracusers=fracusers))-1)*colSums(averted))/sum(totcases[dates<=last_date,])
(1+(tot_averted_q3 - tot_averted_q1)*0.475/0.5/tot_averted)*sum((predict(lm_1,data.frame(fracusers=pmax(0.359,fracusers)))/predict(lm_1,data.frame(fracusers=fracusers))-1)*colSums(averted))/sum(totcases[dates<=last_date,])
# how many cases could have been avoided if users would be 20% more nationwide?
sum((predict(lm_1,data.frame(fracusers=fracusers+0.2))/predict(lm_1,data.frame(fracusers=fracusers))-1)*colSums(averted))/sum(totcases[dates<=last_date,])
(1-(tot_averted_q3 - tot_averted_q1)*0.475/0.5/tot_averted)*sum((predict(lm_1,data.frame(fracusers=fracusers+0.2))/predict(lm_1,data.frame(fracusers=fracusers))-1)*colSums(averted))/sum(totcases[dates<=last_date,])
(1+(tot_averted_q3 - tot_averted_q1)*0.475/0.5/tot_averted)*sum((predict(lm_1,data.frame(fracusers=fracusers+0.2))/predict(lm_1,data.frame(fracusers=fracusers))-1)*colSums(averted))/sum(totcases[dates<=last_date,])


# Counterfactuals for PHASE 2 ONLY:
# how many cases could have been avoided if users would be at least 35.9% of the population everywhere?
sum((predict(lm_p2,data.frame(fracusers=pmax(0.359,fracusers)))/predict(lm_p2,data.frame(fracusers=fracusers))-1)*colSums(averted[dates[dates<=last_date] %in% phase2,]))/sum(totcases[dates<=last_date & dates %in% phase2,])
c_down*sum((predict(lm_p2,data.frame(fracusers=pmax(0.359,fracusers)))/predict(lm_p2,data.frame(fracusers=fracusers))-1)*colSums(averted[dates[dates<=last_date] %in% phase2,]))/sum(totcases[dates<=last_date & dates %in% phase2,])
c_up*sum((predict(lm_p2,data.frame(fracusers=pmax(0.359,fracusers)))/predict(lm_p2,data.frame(fracusers=fracusers))-1)*colSums(averted[dates[dates<=last_date] %in% phase2,]))/sum(totcases[dates<=last_date & dates %in% phase2,])
# how many cases could have been avoided if users would be 20% more nationwide?
sum((predict(lm_p2,data.frame(fracusers=fracusers+0.2))/predict(lm_p2,data.frame(fracusers=fracusers))-1)*colSums(averted[dates[dates<=last_date] %in% phase2,]))/sum(totcases[dates<=last_date & dates %in% phase2,])
c_down*sum((predict(lm_p2,data.frame(fracusers=fracusers+0.2))/predict(lm_p2,data.frame(fracusers=fracusers))-1)*colSums(averted[dates[dates<=last_date] %in% phase2,]))/sum(totcases[dates<=last_date & dates %in% phase2,])
c_up*sum((predict(lm_p2,data.frame(fracusers=fracusers+0.2))/predict(lm_p2,data.frame(fracusers=fracusers))-1)*colSums(averted[dates[dates<=last_date] %in% phase2,]))/sum(totcases[dates<=last_date & dates %in% phase2,])
# opt-out key sharing
(0.95/0.72-1)*sum(averted[dates[dates<=last_date] %in% phase2,])/sum(totcases[dates<=last_date & dates %in% phase2,])
c_down*(0.95/0.72-1)*sum(averted[dates[dates<=last_date] %in% phase2,])/sum(totcases[dates<=last_date & dates %in% phase2,])
c_up*(0.95/0.72-1)*sum(averted[dates[dates<=last_date] %in% phase2,])/sum(totcases[dates<=last_date & dates %in% phase2,])
# increasing adherence by 20%
0.2*2/(reduction+reduction_opt)*sum(averted[dates[dates<=last_date] %in% phase2,])/sum(totcases[dates<=last_date & dates %in% phase2,])
c_down*0.2*2/(reduction+reduction_opt)*sum(averted[dates[dates<=last_date] %in% phase2,])/sum(totcases[dates<=last_date & dates %in% phase2,])
c_up*0.2*2/(reduction+reduction_opt)*sum(averted[dates[dates<=last_date] %in% phase2,])/sum(totcases[dates<=last_date & dates %in% phase2,])
# reducing delays by 1 day
delay_reduced<-delay_quarantine; delay_reduced[,1]<-delay_reduced[,1]-1
averted_max_delay2<-totcases_w*(apply(sweep(partial_w/totcases_w,1,infectiousness(delay_reduced),"*"),2,function(x){cumsum(x)}))*rate/generation_time
(sum(averted_max_delay2)/sum(averted_max)-1)*sum(averted[dates[dates<=last_date] %in% phase2,])/sum(totcases[dates<=last_date & dates %in% phase2,])
c_down*(sum(averted_max_delay2)/sum(averted_max)-1)*sum(averted[dates[dates<=last_date] %in% phase2,])/sum(totcases[dates<=last_date & dates %in% phase2,])
c_up*(sum(averted_max_delay2)/sum(averted_max)-1)*sum(averted[dates[dates<=last_date] %in% phase2,])/sum(totcases[dates<=last_date & dates %in% phase2,])
