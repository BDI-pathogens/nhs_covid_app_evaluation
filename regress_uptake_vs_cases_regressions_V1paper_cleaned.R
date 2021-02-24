
# Usage note: execute the contents of regress_uptake_vs_cases_generate_data_V1paper_cleaned.R
# before running this script (keeping the objects in the memory of the current R session)

################################################################################  
# REGRESSION RESULTS


# summary functions for linear models:


lm_details <- function(lm) {
  a <- anova(lm)
  a[,2] <- a[,2] / sum(a[,2])
  b <- summary(lm)
  #cat("\n\n\n\n\n\n")
  list(summary = summary(lm),
       confints = confint(lm),
       anova = a)
}

lmd_log_effect <- function(lmd, fraction_users, scale.factor) {
  print(fraction_users)
  list(
    main = 100*(1-exp(lmd$summary$coefficients[fraction_users,"Estimate"]*scale.factor)),
    confint = rev(100*(1-exp(lmd$confints[fraction_users,]*scale.factor)))
  )
}

anovanorm <- function(lm) {
  a <- anova(lm)
  a[,2] <- a[,2] / sum(a[,2])
  a
}


# Regressions with respect to synthetic controls

df_clus10_results <- tibble(clus10 = as.character(unique(df_england$clus10)),
                            point = NA_real_,
                            lower = NA_real_,
                            upper = NA_real_,
                            n = NA_integer_,
                            std_err = NA_real_)
for (clus10_ in df_clus10_results$clus10) {
  lm_this_clus10 <- lm(data = df %>% filter(clus10 == clus10_),
                       log(cases_phase_app_total_per_capita) ~ fraction_users +
                         log(cases_phase0_total_per_capita)) 
  point <- lm_this_clus10$coefficients[["fraction_users"]]
  lower <- confint(lm_this_clus10)["fraction_users", 1]
  upper <- confint(lm_this_clus10)["fraction_users", 2]
  std_err <- summary(lm_this_clus10)$coefficients["fraction_users","Std. Error"]
  df_clus10_results[df_clus10_results$clus10 == clus10_, ]$point <- point
  df_clus10_results[df_clus10_results$clus10 == clus10_, ]$lower <- lower
  df_clus10_results[df_clus10_results$clus10 == clus10_, ]$upper <- upper
  df_clus10_results[df_clus10_results$clus10 == clus10_, ]$n <- nrow(df %>% filter(clus10 == clus10_))
  df_clus10_results[df_clus10_results$clus10 == clus10_, ]$std_err <- std_err
}
new_row <- vector(mode = "double", length = 6)
names(new_row) <- colnames(df_clus10_results)
new_row["clus10"] <- 11
new_row["point"] <- sum(df_clus10_results$point/(df_clus10_results$std_err^2))/sum(1/(df_clus10_results$std_err^2))
new_row["n"] <- sum(df_clus10_results$n)
new_row["std_err"] <- sqrt(1/sum(1/df_clus10_results$std_err^2))
new_row["lower"] <- new_row["point"] - 1.96*new_row["std_err"]
new_row["upper"] <- new_row["point"] + 1.96*new_row["std_err"]
df_clus10_results <- rbind(df_clus10_results, new_row)
df_clus10_results[11,]$clus10 <- "Meta"
df_clus10_results$clus10 <- factor(df_clus10_results$clus10, levels = c(1:10,"Meta"))


cluster.regression <- function(cases.string, cases.adjust.string) {
  df_clus10_results <- tibble(clus10 = as.character(unique(df_england$clus10)),
                              point = NA_real_,
                              lower = NA_real_,
                              upper = NA_real_,
                              n = NA_integer_,
                              std_err = NA_real_)
  for (clus10_ in df_clus10_results$clus10) {
    df.cluster <- df %>% filter(clus10 == clus10_)
    cases.vector <- select(df.cluster, cases.string) %>% unlist(., use.names = F)
    cases.adjust.vector <- select(df.cluster, cases.adjust.string) %>% unlist(., use.names = F)
    fraction_users <- select(df.cluster, "fraction_users")  %>% unlist(., use.names = F)
    lm_this_clus10 <- lm(log(cases.vector) ~ fraction_users +
                           log(cases.adjust.vector)) 
    point <- lm_this_clus10$coefficients[["fraction_users"]]
    lower <- confint(lm_this_clus10)["fraction_users", 1]
    upper <- confint(lm_this_clus10)["fraction_users", 2]
    std_err <- summary(lm_this_clus10)$coefficients["fraction_users","Std. Error"]
    df_clus10_results[df_clus10_results$clus10 == clus10_, ]$point <- point
    df_clus10_results[df_clus10_results$clus10 == clus10_, ]$lower <- lower
    df_clus10_results[df_clus10_results$clus10 == clus10_, ]$upper <- upper
    df_clus10_results[df_clus10_results$clus10 == clus10_, ]$n <- nrow(df %>% filter(clus10 == clus10_))
    df_clus10_results[df_clus10_results$clus10 == clus10_, ]$std_err <- std_err
  }
  new_row <- vector(mode = "double", length = 6)
  names(new_row) <- colnames(df_clus10_results)
  new_row["clus10"] <- 11
  new_row["point"] <- sum(df_clus10_results$point/(df_clus10_results$std_err^2))/sum(1/(df_clus10_results$std_err^2))
  new_row["n"] <- sum(df_clus10_results$n)
  new_row["std_err"] <- sqrt(1/sum(1/df_clus10_results$std_err^2))
  new_row["lower"] <- new_row["point"] - 1.96*new_row["std_err"]
  new_row["upper"] <- new_row["point"] + 1.96*new_row["std_err"]
  df_clus10_results <- rbind(df_clus10_results, new_row)
  df_clus10_results[11,]$clus10 <- "Meta"
  df_clus10_results$clus10 <- factor(df_clus10_results$clus10, levels = c(1:10,"Meta"))
  return(df_clus10_results)
}

for(i in 1:10) {
  print(c("Cluster = ", i))
  print(unlist(df_england$authority[df_england$clus10 == i]))
}
  

df_clus10_results_phase_app <- cluster.regression( 
  cases.string = "cases_phase_app_total_per_capita",
  cases.adjust.string = "cases_phase0_total_per_capita"
)

df_clus10_results_phase1 <- cluster.regression( 
  cases.string = "cases_phase1_total_per_capita",
  cases.adjust.string = "cases_phase0_total_per_capita"
)

df_clus10_results_phase2 <- cluster.regression( 
  cases.string = "cases_phase2_total_per_capita",
  cases.adjust.string = "cases_phase0_total_per_capita"
)


df.summarise <- function(df) {
  last <- df[df$clus10=="Meta",]
  result <- c(last$point, last$upper, last$lower)
  return(100*(1-exp(0.01*result)))
}

print(df_clus10_summary <- list(
  phase1 = df.summarise(df_clus10_results_phase1), 
  phase2 = df.summarise(df_clus10_results_phase2), 
  phase_app = df.summarise(df_clus10_results_phase_app)
))


# variance explained by individual linear regressions:

individual_variance_explained<-rbind(
anovanorm(lm(
  data = df,
  I(log(cases_phase_app_total_per_capita)) ~
    neighbours_cases_phase_app_total_normed_to_me
))[1,]
,
anovanorm(lm(
  data = df,
  I(log(cases_phase_app_total_per_capita)) ~
    RUC11CD 
))[1,]
,
anovanorm(lm(
  data = df,
  I(log(cases_phase_app_total_per_capita)) ~
    gdp_2018_band_adj
))[1,]
,
anovanorm(lm(
  data = df,
  I(log(cases_phase_app_total_per_capita)) ~
    povertybhc
))[1,]
,
anovanorm(lm(
  data = df,
  I(log(cases_phase_app_total_per_capita)) ~
    cases_sep_total_per_capita
))[1,]
,
anovanorm(lm(
  data = df,
  I(log(cases_phase_app_total_per_capita)) ~
    manual_contacts
))[1,]
,
anovanorm(lm(
  data = df,
  I(log(cases_phase_app_total_per_capita)) ~
    fraction_users
))[1,]
)
individual_variance_explained[order(individual_variance_explained[,2],decreasing = T),]


# naive linear regressions
lm_details(lm(data = df,
              log(cases_phase_app_total_per_capita) ~ fraction_users))
lm_details(lm(data = df,
              log(cases_phase1_total_per_capita) ~ fraction_users))
lm_details(lm(data = df,
              log(cases_phase2_total_per_capita) ~ fraction_users))
lm_details(lm(data = df,
              log(cases_phase0_total_per_capita) ~ fraction_users))



# MATCHED PAIRS REGRESSION

# app only
lmpt <- lm_details(lm(comp_log_cases_england_phase_app[comp_users_england>0] ~ 
                        comp_rural_england[comp_users_england>0] +
                        comp_gdp_england[comp_users_england>0] +
                        comp_poverty_england[comp_users_england>0] +
                        comp_users_england[comp_users_england>0] + 0))

lmp0 <- lm_details(lm(comp_log_cases_england_phase0[comp_users_england>0] ~ 
                        comp_rural_england[comp_users_england>0] +
                        comp_gdp_england[comp_users_england>0] +
                        comp_poverty_england[comp_users_england>0] +
                        comp_users_england[comp_users_england>0] + 0))

lmp1 <- lm_details(lm(comp_log_cases_england_phase1[comp_users_england>0] ~ 
                        comp_rural_england[comp_users_england>0] +
                        comp_gdp_england[comp_users_england>0] +
                        comp_poverty_england[comp_users_england>0] +
                        comp_users_england[comp_users_england>0] + 0))

lmp2 <- lm_details(lm(comp_log_cases_england_phase2[comp_users_england>0] ~ 
                        comp_rural_england[comp_users_england>0] +
                        comp_gdp_england[comp_users_england>0] +
                        comp_poverty_england[comp_users_england>0] +
                        comp_users_england[comp_users_england>0] + 0))

lmp3 <- lm_details(lm(comp_log_cases_england_phase3[comp_users_england>0] ~ 
                        comp_rural_england[comp_users_england>0] +
                        comp_gdp_england[comp_users_england>0] +
                        comp_poverty_england[comp_users_england>0] +
                        comp_users_england[comp_users_england>0] + 0))

# app only, restricted to England
lm_details(lm(comp_log_cases_england_phase_app[comp_users_england>0] ~ 
                comp_users_england[comp_users_england>0] + 0))

lm_details(lm(comp_log_cases_england_phase1[comp_users_england>0] ~ 
                        comp_users_england[comp_users_england>0] + 0))

lm_details(lm(comp_log_cases_england_phase2[comp_users_england>0] ~ 
                        comp_users_england[comp_users_england>0] + 0))

lm_details(lm(comp_log_cases_england_phase3[comp_users_england>0] ~ 
                        comp_users_england[comp_users_england>0] + 0))

# app and manual tracing, restricted to England, only pairs belonging to different UTLAs
lmmt <- lm_details(lm(comp_log_cases_england_phase_app[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] ~ 
                        comp_rural_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_gdp_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_poverty_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_contacts_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_users_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] + 0))
lmm0 <- lm_details(lm(comp_log_cases_england_phase0[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] ~ 
                        comp_rural_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_gdp_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_poverty_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_contacts_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_users_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] + 0))
lmm1 <- lm_details(lm(comp_log_cases_england_phase1[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] ~ 
                        comp_rural_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_gdp_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_poverty_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_contacts_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_users_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] + 0))
lmm2 <- lm_details(lm(comp_log_cases_england_phase2[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] ~ 
                        comp_rural_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_gdp_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_poverty_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_contacts_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_users_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] + 0))
lmm3 <- lm_details(lm(comp_log_cases_england_phase3[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] ~ 
                        comp_rural_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_gdp_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_poverty_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_contacts_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] +
                        comp_users_england[!(comp_manual_england==0 & comp_contacts_england==0) & comp_users_england>0] + 0))


# REGRESSION MATCHING NEIGHBOURS (MAIN REGRESSION)


df.matched.phase0 <- df[df$n_neighbours_in_same_quintile_phase0>0,]
df.matched.phase1 <- df[df$n_neighbours_in_same_quintile_phase1>0,]
df.matched.phase2 <- df[df$n_neighbours_in_same_quintile_phase2>0,]


lmd0<- lm_details(lm(
  data = df.matched.phase0,
  log_cases_phase0_total_per_capita_rel_to_matched_phase0_neighbours ~
    RUC11CD_rel_to_matched_phase0_neighbours +
    gdp_2018_band_rel_to_matched_phase0_neighbours +
    povertybhc_rel_to_matched_phase0_neighbours +
    fraction_users_rel_to_matched_phase0_neighbours + 0
))


lmdt <- lm_details(lm(
  data = df.matched.phase0,
  log_cases_phase_app_total_per_capita_rel_to_matched_phase0_neighbours ~
    RUC11CD_rel_to_matched_phase0_neighbours +
    gdp_2018_band_rel_to_matched_phase0_neighbours +
    povertybhc_rel_to_matched_phase0_neighbours +
    fraction_users_rel_to_matched_phase0_neighbours + 0
))


lmd1 <- lm_details(lm1 <- lm(
  data = df.matched.phase0,
  log_cases_phase1_total_per_capita_rel_to_matched_phase0_neighbours ~
    RUC11CD_rel_to_matched_phase0_neighbours +
    gdp_2018_band_rel_to_matched_phase0_neighbours +
    povertybhc_rel_to_matched_phase0_neighbours +
    fraction_users_rel_to_matched_phase0_neighbours + 0
))


lmd2.0 <- lm_details(lm2.0 <- lm(
  data = df.matched.phase0,
  log_cases_phase2_total_per_capita_rel_to_matched_phase0_neighbours ~
    RUC11CD_rel_to_matched_phase0_neighbours +
    gdp_2018_band_rel_to_matched_phase1_neighbours +
    povertybhc_rel_to_matched_phase0_neighbours +
    fraction_users_rel_to_matched_phase0_neighbours + 0
))


lmd_log_effect(lmd0, "fraction_users_rel_to_matched_phase0_neighbours", 0.01)
lmd_log_effect(lmdt, "fraction_users_rel_to_matched_phase0_neighbours", 0.01)
lmd_log_effect(lmd1, "fraction_users_rel_to_matched_phase0_neighbours", 0.01)
lmd_log_effect(lmd2.0, "fraction_users_rel_to_matched_phase0_neighbours", 0.01)


######################################################
# BOOTSTRAP FOR MATCHING NEIGHBOURS REGRESSION

df_original<-df

lm_bootstraps<-list()

for(bootstrap in 1:n_bootstraps){
  
  lm_bootstrap<-list()
  
  df<-df_bootstrap[[bootstrap]]
  
  df.matched.phase0 <- df[df$n_neighbours_in_same_quintile_phase0>0,]
  df.matched.phase1 <- df[df$n_neighbours_in_same_quintile_phase1>0,]
  df.matched.phase2 <- df[df$n_neighbours_in_same_quintile_phase2>0,]
  
  
  # log scale regressions
  
  lm_bootstrap[["lmd0"]]<- lm_details(lm(
    data = df.matched.phase0,
    log_cases_phase0_total_per_capita_rel_to_matched_phase0_neighbours ~
      RUC11CD_rel_to_matched_phase0_neighbours +
      gdp_2018_band_rel_to_matched_phase0_neighbours +
      povertybhc_rel_to_matched_phase0_neighbours +
      fraction_users_rel_to_matched_phase0_neighbours
  ))
  
  
  lm_bootstrap[["lmdt"]] <- lm_details(lm(
    data = df.matched.phase0,
    log_cases_phase_app_total_per_capita_rel_to_matched_phase0_neighbours ~
      RUC11CD_rel_to_matched_phase0_neighbours +
      gdp_2018_band_rel_to_matched_phase0_neighbours +
      povertybhc_rel_to_matched_phase0_neighbours +
      fraction_users_rel_to_matched_phase0_neighbours
  ))
  
  
  
  
  lm_bootstrap[["lmd1"]] <- lm_details(lm(
    data = df.matched.phase0,
    log_cases_phase1_total_per_capita_rel_to_matched_phase0_neighbours ~
      RUC11CD_rel_to_matched_phase0_neighbours +
      gdp_2018_band_rel_to_matched_phase0_neighbours +
      povertybhc_rel_to_matched_phase0_neighbours +
      fraction_users_rel_to_matched_phase0_neighbours
  ))
  
  
  lm_bootstrap[["lmd2"]] <- lm_details( lm(
    data = df.matched.phase1,
    log_cases_phase2_total_per_capita_rel_to_matched_phase1_neighbours ~
      RUC11CD_rel_to_matched_phase1_neighbours +
      gdp_2018_band_rel_to_matched_phase1_neighbours +
      povertybhc_rel_to_matched_phase1_neighbours +
      fraction_users_rel_to_matched_phase1_neighbours
  ))
  
  lm_bootstrap[["lmd2.0"]] <- lm_details( lm(
    data = df.matched.phase0,
    log_cases_phase2_total_per_capita_rel_to_matched_phase0_neighbours ~
      RUC11CD_rel_to_matched_phase0_neighbours +
      gdp_2018_band_rel_to_matched_phase1_neighbours +
      povertybhc_rel_to_matched_phase0_neighbours +
      fraction_users_rel_to_matched_phase0_neighbours
  ))
  
  lm_bootstrap[["lmd3"]] <- lm_details( lm(
    data = df.matched.phase2,
    log_cases_phase3_total_per_capita_rel_to_matched_phase2_neighbours ~
      RUC11CD_rel_to_matched_phase2_neighbours +
      gdp_2018_band_rel_to_matched_phase2_neighbours +
      povertybhc_rel_to_matched_phase2_neighbours +
      fraction_users_rel_to_matched_phase2_neighbours
  ))
  
  lm_bootstrap[["lmd3.0"]] <- lm_details( lm(
    data = df.matched.phase0,
    log_cases_phase3_total_per_capita_rel_to_matched_phase0_neighbours ~
      RUC11CD_rel_to_matched_phase0_neighbours +
      gdp_2018_band_rel_to_matched_phase0_neighbours +
      povertybhc_rel_to_matched_phase0_neighbours +
      fraction_users_rel_to_matched_phase0_neighbours
  ))
  
  
  lm_bootstrap[["overall"]] <- lm_details(lm(data = df,
                                             fraction_users ~
                                               RUC11CD + gdp_2018_band_adj + povertybhc + cases_phase0_total_per_capita))
  
  
  lm_bootstraps[[bootstrap]]<-lm_bootstrap
  
}

df<-df_original


extract_bootstrap_results<-function(choose_lm){
  res<-list()
  bs<-sapply(1:n_bootstraps,function(i){lm_bootstraps[[i]][[choose_lm]]$summary$coefficients[5,1]})
  res$mean<-mean(bs)
  res$sd<-sd(bs)
  res$ci95<-quantile(bs,c(0.025,0.975))
  res$p.value.left<-mean(bs<0)
  res$p.value.right<-mean(bs>0)
  res$summary<-summary(bs)
  return(res)
}

# choose which bootstrap to view:
extract_bootstrap_results("lmd1")
extract_bootstrap_results("lmd2.0")
extract_bootstrap_results("lmdt")


#############################################


# calculations of cases averted


calculate.cases.averted <- function(cases.vector, users.vector, linear.model.summary, users.string, min.users.effect) {
  best.fit <- linear.model.summary$summary$coefficients[users.string,"Estimate"]
  confint <- linear.model.summary$confints[users.string,]
  rates <- c(best.fit, confint)
  effective.users <- pmax(users.vector-min.users.effect,0)
  cases.averted <- vector(mode = "double", length=3)
  for (i in 1:3) {
    cases.averted[i] <- sum(cases.vector * (exp(-rates[i]*effective.users)-1))
  }
  return(cases.averted)
}

# formula derived as follows:
# cases(users) = cases(users.min) exp(rates * Users - Users.min )
# cases(users.min) =  exp(- rates * Users - Users.min ) * Cases(users)
# cases(users.min) - cases(users) =  exp(- rates * Users - Users.min ) * Cases(users) - cases(users)

print( cases_phase1_averted <- calculate.cases.averted(
  cases.vector = cases_phase1_total_vector ,
  users.vector = df$fraction_users ,
  linear.model.summary = lmd1 ,
  users.string = "fraction_users_rel_to_matched_phase0_neighbours",
  min.users.effect = 0.15
) )

print( cases_phase2_averted <- calculate.cases.averted(
  cases.vector = cases_phase2_total_vector ,
  users.vector = df$fraction_users ,
  linear.model.summary = lmd2.0 ,
  users.string = "fraction_users_rel_to_matched_phase0_neighbours" ,
  min.users.effect = 0.15
) )

print(cases_total_averted <- cases_phase1_averted + cases_phase2_averted)

print( cases_phase_all_averted <- calculate.cases.averted(
  cases.vector = cases_phase_app_total_vector ,
  users.vector = df$fraction_users ,
  linear.model.summary = lmdt ,
  users.string = "fraction_users_rel_to_matched_phase0_neighbours" ,
  min.users.effect = 0.15
)
)

sum(cases_phase1_total_vector+cases_phase2_total_vector)

hist(df$fraction_users)
quantile(df$fraction_users,seq(0,1,by=0.1))
target.uptake <- quantile(df$fraction_users,0.9)

calculate.addtional.cases.averted.equity <- function(cases.vector, users.vector, linear.model.summary, users.string, target.uptake) {
  best.fit <- linear.model.summary$summary$coefficients[users.string,"Estimate"]
  confint <- linear.model.summary$confints[users.string,]
  rates <- c(best.fit, confint)
  additional.users <- pmax(target.uptake - users.vector,0)
  cases.averted <- vector(mode = "double", length=3)
  for (i in 1:3) {
    cases.averted[i] <- sum(cases.vector * (1-exp(rates[i]*additional.users)))
  }
  return(cases.averted)
}

print( cases_phase1_averted_additional <- calculate.addtional.cases.averted.equity(
  cases.vector = cases_phase1_total_vector ,
  users.vector = df$fraction_users ,
  linear.model.summary = lmd1 ,
  users.string = "fraction_users_rel_to_matched_phase0_neighbours",
  target.uptake = target.uptake
) )

print( cases_phase2_averted_additional <- calculate.addtional.cases.averted.equity(
  cases.vector = cases_phase2_total_vector ,
  users.vector = df$fraction_users ,
  linear.model.summary = lmd2.0 ,
  users.string = "fraction_users_rel_to_matched_phase0_neighbours" ,
  target.uptake = target.uptake
) )

target.uptake
cases_phase1_averted_additional + cases_phase2_averted_additional

calculate.addtional.cases.averted.equity <- function(cases.vector, users.vector, linear.model.summary, users.string, target.uptake) {
  best.fit <- linear.model.summary$summary$coefficients[users.string,"Estimate"]
  confint <- linear.model.summary$confints[users.string,]
  rates <- c(best.fit, confint)
  additional.users <- pmax(target.uptake - users.vector,0)
  cases.averted <- vector(mode = "double", length=3)
  for (i in 1:3) {
    cases.averted[i] <- sum(cases.vector * (1-exp(rates[i]*additional.users)))
  }
  return(cases.averted)
}

print( cases_phase1_averted_additional <- calculate.addtional.cases.averted.equity(
  cases.vector = cases_phase1_total_vector ,
  users.vector = df$fraction_users ,
  linear.model.summary = lmd1 ,
  users.string = "fraction_users_rel_to_matched_phase0_neighbours",
  target.uptake = target.uptake
) )

print( cases_phase2_averted_additional <- calculate.addtional.cases.averted.equity(
  cases.vector = cases_phase2_total_vector ,
  users.vector = df$fraction_users ,
  linear.model.summary = lmd2.0 ,
  users.string = "fraction_users_rel_to_matched_phase0_neighbours" ,
  target.uptake = target.uptake
) )

target.uptake
cases_phase1_averted_additional + cases_phase2_averted_additional
( cases_phase2_averted_additional) /
  sum(cases_phase2_total_vector)

calculate.addtional.cases.averted.boost <- function(cases.vector, users.vector, linear.model.summary, users.string, boost.factor) {
  best.fit <- linear.model.summary$summary$coefficients[users.string,"Estimate"]
  confint <- linear.model.summary$confints[users.string,]
  rates <- c(best.fit, confint)
  additional.users <- rep(boost.factor, length(users.vector))
  cases.averted <- vector(mode = "double", length=3)
  for (i in 1:3) {
    cases.averted[i] <- sum(cases.vector * (1-exp(rates[i]*additional.users)))
  }
  return(cases.averted)
}

print( cases_phase1_averted_additional_boost <- calculate.addtional.cases.averted.boost(
  cases.vector = cases_phase1_total_vector ,
  users.vector = df$fraction_users ,
  linear.model.summary = lmd1 ,
  users.string = "fraction_users_rel_to_matched_phase0_neighbours",
  boost.factor = 0.2
) )

print( cases_phase2_averted_additional_boost <- calculate.addtional.cases.averted.boost(
  cases.vector = cases_phase2_total_vector ,
  users.vector = df$fraction_users ,
  linear.model.summary = lmd2.0 ,
  users.string = "fraction_users_rel_to_matched_phase0_neighbours" ,
  boost.factor = 0.2
) )

( cases_phase2_averted_additional_boost) /
  sum(cases_phase2_total_vector)


