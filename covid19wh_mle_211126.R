# Clear variable environment
rm(list=ls())
# set the work directory
setwd("/Users/baoyinyuan/Documents/postdoc/research/EndofEpidemic/script") 
# Loading data
library("readxl")
library(dplyr)
library(plotly)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(magrittr)
library(tidyquant)



# load the original data of COVID-19 in Wuhan
data0 <- read_excel("/Users/baoyinyuan/Documents/postdoc/research/EndofEpidemic/data_R3.0/wuhan_covid19.xlsx") #.name_repair = "universal"
data0 %>% head(3)

df_wh <-  data0 %>% dplyr::select("Reported day", "New cases") %>% as.data.frame %>% na.omit
colnames(df_wh) <- c("Day_report", "Daily_case")
df_wh <- df_wh[nrow(df_wh):1,]
day_ini <- as.Date(df_wh$Day_report[1] )
df_wh %<>% mutate(Day_report = as.Date(Day_report), 
                  Day_num = as.numeric(Day_report - day_ini + 1),
                  Daily_case = as.numeric(Daily_case)
) %>% na.omit
df_wh %>% head(3)

# barplot of the original incidence
df_wh %>% 
  ggplot(aes(Day_report, Daily_case)) +
  geom_bar(stat="identity", fill="tomato") +
  scale_x_date(date_breaks="week") +
  theme(axis.text.x = element_text(angle = 45, hjust =1)) +
  xlab("\nDate\n") +
  ylab("Daily incidence\n")


######
# Define some special time points
#####
T1 = as.Date("2020-01-23") # City lockdown, traffic suspension, home quarantine
(T1_num = as.numeric(T1 - day_ini + 1))
T2 = as.Date("2020-03-24") # the day of the last reported case

(T2_num = df_wh$Day_num[which(df_wh$Day_report == T2)]) # the day number of the last reported case
T3 = as.Date("2020-04-30")
(T3_num = df_wh$Day_num[nrow(df_wh)]) # the last day number

df_wh %>% head(10)


############################################
### Part1: Effective reproduction number ###
############################################

inc = df_wh$Daily_case
epi_day = df_wh$Day_num

# calculate Rt based on the epi-curve of reported cases
library(EpiEstim)

res_parametric_si <- estimate_R(inc,
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 5.1,
                                  std_si = sqrt(5.3))
                                  )
                                )

(R_start_num = res_parametric_si$R$t_end[1])
(R_end_num = res_parametric_si$R$t_end[length(res_parametric_si$R$t_end)])
(R_start = df_wh$Day_report[R_start_num])
(R_end = df_wh$Day_report[R_end_num])
(R_day_seq = seq(from = R_start, to = R_end, by = "day"))
# plot(res_parametric_si, "R")
(R2 = res_parametric_si$R$`Median(R)`)
(R1 = res_parametric_si$R$`Quantile.0.025(R)`)
(R3 = res_parametric_si$R$`Quantile.0.975(R)`)


## Combine the incidence histogram with the Rt
df_inc_Rt <- data.frame(date = df_wh$Day_report,
                           day_num = df_wh$Day_num,
                           inc = df_wh$Daily_case,
                           Rt_median = c(rep(NA, length(epi_day)-length(R_day_seq)), R2),
                           Rt_q1 = c(rep(NA, length(epi_day)-length(R_day_seq)), R1),
                           Rt_q3 = c(rep(NA, length(epi_day)-length(R_day_seq)), R3)
                           )
df_inc_Rt %>% head(10)
T2 <- as.Date("2020-03-24") # the day of the last case
df_inc_Rt_subset <- df_inc_Rt %>% filter(date<= T2)


#----*---- 2021-10-30 -----*-------
df_inc_Rt_subset %>% head(3)
library(tidyquant)
df_inc_Rt_subset %>% ggplot(aes(date, inc)) +
  geom_bar(stat="identity", show.legend = TRUE, colour="darkgrey", size=1) +
  coord_x_date(xlim=c(df_wh$Day_report[1], as.Date("2020-03-26")), ylim=c(12000,14000)) +
  geom_bar(stat="identity",fill="white",position='dodge',colour="grey") +
  geom_ribbon(mapping=aes(ymin = Rt_q1*500, ymax = Rt_q3*500), alpha=0.5,fill="grey50") +
  geom_line(mapping=aes(x = date, y = Rt_median*500),color="black",linetype="solid",size=0.8) +
  ylab(" ") + 
  # xlab("Reported day") +
  # scale_x_date(date_breaks="7 day",date_labels="%Y-%m-%d") +
  scale_y_continuous(expand = c(0, 10), 
                     breaks=c(0, 12000, 13000,14000),
                     sec.axis = sec_axis(~./500, name=" ",
                                          breaks=seq(0,10,2)
                                         )
                     ) +
  geom_vline(xintercept = as.Date(T1),color="black",size=0.8,linetype="dashed") +
  geom_vline(xintercept = as.Date(T2),color="black",size=0.8,linetype="dashed") +
  geom_hline(yintercept = 1*500, size=0.8,color="black",linetype="dotdash") +
  theme_bw(base_size=12) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=8),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        axis.title.y = element_text(size=11),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size = 11, face="bold"),
        legend.background = element_rect(fill="transparent"),
        panel.grid= element_blank(),
        legend.text=element_text(size=11),
        legend.position = c(0.95,0.99),#"left",#c(.75, .95),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        
  ) -> f1
f1

df_inc_Rt_subset %>% ggplot(aes(x = date, y= inc)) + 
  #ggtitle(ww)+
  geom_bar(stat="identity", show.legend = TRUE, colour="darkgrey", size=1) +
  coord_x_date(xlim=c(df_wh$Day_report[1], as.Date("2020-03-26")),ylim=c(0,4000)) +
  geom_bar(stat="identity",fill="white",position='dodge',colour="grey") +
  geom_ribbon(mapping=aes(ymin = Rt_q1*500, ymax = Rt_q3*500), alpha=0.5,fill="grey50") +
  geom_line(mapping=aes(x = date, y=Rt_median*500),color="black",linetype="solid",size=0.8) +
  ylab("Number of reported cases") + xlab("Reported day") +
  scale_x_date(date_breaks="7 day",date_labels="%Y-%m-%d") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks=seq(0,4000,by=1000),
                     sec.axis = sec_axis(~./500,name="R(t)",
                                         breaks=seq(0,6,1)
                     )
  ) +
  geom_vline(xintercept = as.Date(T1),color="black",size=0.8,linetype="dashed") +
  geom_vline(xintercept = as.Date(T2),color="black",size=0.8,linetype="dashed") +
  geom_hline(yintercept = 1*500, size=0.8,color="black",linetype="dotdash") +
  theme_bw(base_size=12) +
  theme(axis.text.x=element_text(size=8, angle = 90, vjust=0.5, hjust = 1),
        axis.text.y=element_text(size=8),
        axis.title=element_text(size=11),
        plot.title = element_text(size = 11,face="bold"),
        legend.background = element_rect(fill="transparent"),
        panel.grid= element_blank(),
        legend.text=element_text(size=11),
        legend.position = c(0.95,0.99),#"left",#c(.75, .95),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.justification = c("right", "top")
  ) +
  annotate(
    geom = "curve", x = as.Date("2020-01-23"), y = 3500, xend = as.Date("2020-01-15"), yend = 3500, 
    curvature = 0, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text",  x = as.Date("2020-01-12"), y = 3500, label = expression(T[1]), hjust = "left") +
  annotate(
    geom = "curve", x = as.Date("2020-03-24"), y = 3500, xend = as.Date("2020-03-16"), yend = 3500, 
    curvature = 0, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text",  x = as.Date("2020-03-13"), y = 3500, label = expression(T[2]), hjust = "left") -> f2

f2

f1
ggsave(filename = "../figs_R3.0/f1.tiff",
       plot = f1,
       width = 18.5,  # <=19.05cm
       height = 16*0.2, # <=22.225cm
       units= "cm",
       dpi= 300,
       compression = "lzw")

f2
ggsave(filename = "../figs_R3.0/f2.tiff",
       plot = f2,
       width = 18.5,  # <=19.05cm
       height = 16*0.8, # <=22.225cm
       units= "cm",
       dpi= 300,
       compression = "lzw")


#####################################################################
###Part2: MLE of quarantine rate by two methods: exp and lps###
#####################################################################


#####
# 2.1 Using the exp method: exponential form of quarantine rate
####


# set the prameters for the serial interval
si.para = c(5.1, 5.3) # (mean, std)
g.mu = si.para[1]  # mean
g.var = si.para[2]^2 # variance
shap = g.mu^ 2/g.var # shape
rat = g.mu/g.var # rate

# the probability mass function of gamma distribution 
gam= function(t, shap, rat){
  gt =  pgamma(t, shape = shap, rate = rat) - pgamma((t-1),shape = shap, rate = rat)
  return(gt)
}


#######
# 2.2 Using the ls method: linear spline form of quarantine rate
####

library(R2jags)
library(MASS)
library(splines)

(n <- T2_num - T1_num + 1)  # Find number of datapoints in x 
(x <- 1:n)
X <- cbind(rep(1, n), x) # Generate X matrix with column 1 being degree 0, column 2 being degree 1
colnames(X) <- NULL


#######
R0 = 2.0 # can be adjusted for comparison
######

### for each R0, we change num.knots
num.knots <- 40 ## can be changed. Proven that 24 is the best setting.
###
knots <- quantile(unique(x), seq(0, 1, length=(num.knots+2))[-c(1, num.knots + 2)]) # Evenly space the knots across the x-values
Z1 <- outer(x, knots, "-")
Z1 <- Z1*(Z1>0)
C.mat_lps <- cbind(X, Z1) 
(num_coef <- ncol(C.mat_lps))

## Simulate data
set.seed(123)
inc <- df_wh$Daily_case
inc_eff <- c(inc[1 : T1_num], rep(0, n))
beta <- rnorm(num_coef, 0, 0.01) # parameter needs to be estimated

## negative log likelihood
NegLogLik_lps = function(param){
  # R0 = param[1]
  beta = param
  q_lps <- rep(0, T2_num-T1_num + 1) # NPIs rate
  i.mu_lps <- rep(0, T2_num-T1_num + 1) # expected infections
  for(t in T1_num : T2_num){
    # t = T1_num
    renew <-  R0*sum(inc_eff[(t-1):1] * gam(1:(t-1), shap,rat)) # expected incidence
    i.mu_lps[t-T1_num+1] <- max(round(renew), 0.1)
    q_lps[t-T1_num+1] <- 1/(1 + exp(-sum(beta * C.mat_lps[t-T1_num+1,])))
    q.bound <- max(0, min(1, q_lps[t-T1_num+1])) # Constrain the range of q from 0 to 1
    inc_eff[t] <- rbinom(n = 1, 
                         size = inc[t],
                         prob = q.bound
                        ) # cases missing from quarantine prior to T1
  }
  negloglik = -sum(inc[T1_num:T2_num]*log(i.mu_lps) - i.mu_lps)

  (nll = negloglik) 
  param_num = length(param)
  (AIC_ls = 2*param_num+ 2*nll) 
  (AICc_ls = AIC_ls  + (2*param_num^2 + 2*param_num)/(62-param_num-1))
  
  return(AICc_ls) # Return the AICc

}

(num_coef <- ncol(C.mat_lps))
taub = 0.01
(param0 = c(rnorm(num_coef, 0, taub)))
# (param0 = c(5, 8))
(param_num_ls = length(param0))
sample_num_ls = T2_num - T1_num + 1
(AICc_ls = NegLogLik_lps(param0))
AIC_ls = AICc_ls - (2*param_num_ls^2 + 2*param_num_ls)/(sample_num_ls - param_num_ls - 1)
negloglik_lps = (AIC_ls - 2*param_num_ls)/2

## calculate the CI of quarantine rate function
df.par_lps_est <- data.frame(matrix(ncol = param_num_ls + 2, nrow = 0)) # define the initial dataframe of quarantine rate function parameter and AIC, AICc
coeff.name <- c("beta1_est", "beta2_est", paste0(rep("mu_est",num.knots), as.character(1:num.knots)),"AIC_ls","AICc_ls")
colnames(df.par_lps_est) <- coeff.name

# set the value range with three SD
taub = 0.01
beta_ini <- c(-3*taub, 3*taub) # value range of beta

library(dfoptim)
nsample = 1000 # the number of re-samples
n=1
taub = 0.01
(param0 = c(rnorm(num_coef, 0, taub)))

while(n < nsample){
  dfopt_lps <- nmkb(
    par = param0,
    fn = NegLogLik_lps,
    lower = c(rep(beta_ini[1], num_coef)),
    upper = c(rep(beta_ini[2], num_coef))
  )
  df.app <- data.frame(matrix(dfopt_lps$par, nrow=1))
  colnames(df.app) <- coeff.name[1:(length(coeff.name)-2)]
  param_num = length(param0)
  sample_num = T2_num - T1_num + 1
  (nll = dfopt_lps$value)
  (df.app$AIC_ls = 2*param_num + 2*nll) 
  (df.app$AICc_ls = df.app$AIC_ls  + (2*param_num^2 + 2*param_num)/(sample_num-param_num-1))
  
  df.par_lps_est <- rbind(df.par_lps_est, df.app)
  
  n = nrow(df.par_lps_est)
  print(n)
}
round(df.par_lps_est, 2)
# save estimation result to csv
write.csv(df.par_lps_est, 
          "/Users/baoyinyuan/Documents/postdoc/research/EndofEpidemic/data_R6.0/par_lps_est_36knots.csv", 
          row.names = FALSE)


#---- The section below is to use estimated results above for projection
####
## read the estimated parameters with 24 knots for 6 R
####
df.par_lps_est.knot24R1.5 <- read.csv(file="/Users/baoyinyuan/Documents/postdoc/research/EndofEpidemic/data_24knot/par_lps_est_24knotsR1.5.csv")
df.par_lps_est.knot24R2.0 <- read.csv(file="/Users/baoyinyuan/Documents/postdoc/research/EndofEpidemic/data_24knot/par_lps_est_24knotsR2.0.csv")
df.par_lps_est.knot24R3.0 <- read.csv(file="/Users/baoyinyuan/Documents/postdoc/research/EndofEpidemic/data_24knot/par_lps_est_24knotsR3.0.csv")
df.par_lps_est.knot24R4.0 <- read.csv(file="/Users/baoyinyuan/Documents/postdoc/research/EndofEpidemic/data_24knot/par_lps_est_24knotsR4.0.csv")
df.par_lps_est.knot24R5.0 <- read.csv(file="/Users/baoyinyuan/Documents/postdoc/research/EndofEpidemic/data_24knot/par_lps_est_24knotsR5.0.csv")
df.par_lps_est.knot24R6.0 <- read.csv(file="/Users/baoyinyuan/Documents/postdoc/research/EndofEpidemic/data_24knot/par_lps_est_24knotsR6.0.csv")

# function to calculate the confidence interval for estimated prameters 
CI_qr_lps <- function(df.par_lps_est){
  sample_num <- nrow(df.par_lps_est)
  param_num = ncol(df.par_lps_est)-2 # total number of unknown parameters
  knot_num <- param_num-2 # knot number
  knots <- quantile(unique(x), seq(0, 1, length=(knot_num + 2))[-c(1, knot_num + 2)]) # Evenly space the knots across the x-values
  (n <- T2_num - T1_num + 1)  # Find number of datapoints in x 
  (x <- 1:n)
  X <- cbind(rep(1, n), x) # Generate X matrix with column 1 being degree 0, column 2 being degree 1
  colnames(X) <- NULL
  Z1 <- outer(x, knots, "-")
  Z1 <- Z1*(Z1>0)
  C.mat_lps <- cbind(X, Z1) 
  (num_coef <- ncol(C.mat_lps))

  quar_rate_sample2 = data.frame(matrix(rep(0, sample_num * n), ncol = sample_num))
  for (i in 1:sample_num){
    # (R0_est = 1.5)
    #(R0_est = df.par_lps_est[i, 1])
    (beta_est = df.par_lps_est[i, 1:param_num])
    (qr_est = 1/(1 + exp(-C.mat_lps %*% t(as.matrix(beta_est)))))
    quar_rate_sample2[,i] = qr_est  # calculated quarantine rate
  }
  ci_value = c(0.05, 0.95, 0.5) # 90% CI
  ci_name =c("lower_lps","upper_lps","median_lps")
  nci = length(ci_value)
  
  CI_qr2 = data.frame(matrix(rep(0, nci*n), ncol = nci))
  colnames(CI_qr2) <- ci_name
  
  for (j in 1:t_len){
    CI_qr2[j,] = as.numeric(quantile(quar_rate_sample2[j,], probs = ci_value))
  }
  
  ## add 46 rows NA values to the CI_qr dataframe (T1_num=46)
  na.mat <- matrix(NA, nrow = T1_num - 1, ncol = nci)
  colnames(na.mat) <- colnames(CI_qr2)
  CI_qr2.na <- rbind(na.mat, CI_qr2)
  CI_qr2.na$day_num <- 1:T2_num
  CI_qr2.na$date <- df_wh$Day_report[1:T2_num]
  CI_qr2.na$inc <- df_wh$Daily_case[1:T2_num]
  
  return(CI_qr2.na)
}

CI_qr_na.knot24R1.5 <- CI_qr_lps(df.par_lps_est.knot24R1.5)
CI_qr_na.knot24R2.0 <- CI_qr_lps(df.par_lps_est.knot24R2.0)
CI_qr_na.knot24R3.0 <- CI_qr_lps(df.par_lps_est.knot24R3.0)
CI_qr_na.knot24R4.0 <- CI_qr_lps(df.par_lps_est.knot24R4.0)
CI_qr_na.knot24R5.0 <- CI_qr_lps(df.par_lps_est.knot24R5.0)
CI_qr_na.knot24R6.0 <- CI_qr_lps(df.par_lps_est.knot24R6.0)

CI_qr.na.knot24 <- rbind(CI_qr_na.knot24R1.5, CI_qr_na.knot24R2.0, CI_qr_na.knot24R3.0,
                      CI_qr_na.knot24R4.0, CI_qr_na.knot24R5.0, CI_qr_na.knot24R6.0)
CI_qr.na.knot24$R <- as.factor(rep(c(1.5, 2.0, 3.0, 4.0, 5.0, 6.0), each = T2_num)) 


# # plot the CI_qr
# ggplot(CI_qr.na.knot24, aes(x = date, y = upper_lps, color = R)) +
#   geom_line()

##
T2 = as.Date("2020-03-24") # the day of the last reported case
(T2_num = df_wh$Day_num[which(df_wh$Day_report == T2)]) # the day number of the last reported case


# plot CI of quarantine rate for each reproduction number
ci_qr.knot24 <- ggplot(CI_qr.na.knot24, aes(x = date, y= inc, group= R)) + 
  geom_bar(stat="identity",position = "identity", show.legend = TRUE, colour="darkgrey", size=1) +
  coord_x_date(xlim = c(CI_qr.na.knot24$date[1], as.Date("2020-03-26")), ylim = c(0, 14000)) +
  geom_bar(stat="identity",fill="white",position='dodge',colour="grey") +
  geom_ribbon(mapping=aes(ymin = lower_lps*14000, ymax = upper_lps*14000, fill= R ), alpha=0.5) +
  geom_line(mapping=aes(x = date, y=median_lps*14000, colour=R),linetype="solid",size=0.8) +
  ylab("Number of reported cases") + xlab("Calendar day") +
  scale_x_date(date_breaks="7 day",date_labels="%Y-%m-%d") +
  scale_y_continuous(expand = c(0, 0), 
                     breaks=seq(0,14000,by=1000),
                     sec.axis = sec_axis(~./14000, name="1 - NPIs rate", breaks=seq(0,1,0.1))) +
  scale_colour_discrete(name=" R",labels = c("1.5", "2.0", "3.0", "4.0", "5.0", "6.0")) +
  scale_fill_discrete(name=" R",labels = c("1.5", "2.0", "3.0", "4.0", "5.0", "6.0")) +
  geom_vline(xintercept = as.Date(T1),color="black",size=0.7,linetype="dashed") +
  geom_vline(xintercept = as.Date(T2),color="black",size=0.7,linetype="dashed") +
  theme_bw(base_size=12) +
  theme(axis.text.x=element_text(size=8,angle = 90, vjust=0.5,hjust = 1),
        axis.text.y=element_text(size=8),
        axis.title=element_text(size=11),
        plot.title = element_text(size = 11,face="bold"),
        legend.background = element_rect(fill="transparent"),
        legend.box.background = element_rect(colour = "black"),
        panel.grid= element_blank(),
        legend.title=element_text(size= 8),
        legend.text=element_text(size= 8),
        legend.position = c(0.2,0.97),#"left",#c(.75, .95),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.justification = c("right", "top")
  ) +
  geom_segment(
    x = as.Date("2020-01-23"), y = 3000, xend = as.Date("2020-01-15"), yend = 3000, size= 0.4,  
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text",  x = as.Date("2020-01-10"), y =3000, size= 4, label = expression(T[1]), hjust = "left") +
  geom_segment(
   x = as.Date("2020-03-24"), y = 3000, xend = as.Date("2020-03-16"), yend = 3000, size= 0.4, 
   arrow = arrow(length = unit(2, "mm"))
) +
  annotate(geom = "text",  x = as.Date("2020-03-11"), y =3000, size= 4, label = expression(T[2]), hjust = "left")


plot(ci_qr.knot24)

ggsave(filename = "../fig_24knot/fig 3_24knot.tiff",
       plot = ci_qr.knot24,
       width = 14,  # <=19.05cm
       height = 12, # <=22.225cm
       units= "cm",
       dpi= 300,
       compression = "lzw")




##################################################################################
## Part3: Calculate the prob of ending outbreak via predicted epidemic trajectory##
##################################################################################

## 3.1 calculate the expected incidence after the last case(2020-03-24)

#------- for the lps method: CI_i.mu_lps

case_predict <- function(df.par_lps_est,R){
  sample_num <- nrow(df.par_lps_est)
  param_num = ncol(df.par_lps_est)-2 # total number of unknown parameters
  knot_num <- param_num-2 # knot number
  knots <- quantile(unique(x), seq(0, 1, length=(knot_num + 2))[-c(1, knot_num + 2)]) # Evenly space the knots across the x-values
  (n <- T2_num - T1_num + 1)  # Find number of datapoints in x 
  (x <- 1:n)
  X <- cbind(rep(1, n), x) # Generate X matrix with column 1 being degree 0, column 2 being degree 1
  colnames(X) <- NULL
  Z1 <- outer(x, knots, "-")
  Z1 <- Z1*(Z1>0)
  C.mat_lps <- cbind(X, Z1) 
  (num_coef <- ncol(C.mat_lps))
  
  quar_rate_sample2 = data.frame(matrix(rep(0, sample_num * n), ncol = sample_num))
  # calculate the quarantine rate for each estimated sample(nsample=1000,here!)
  for (i in 1:sample_num){
    (beta_est = df.par_lps_est[i, 1:param_num])
    (qr_est = 1/(1 + exp(-C.mat_lps %*% t(as.matrix(beta_est)))))
    quar_rate_sample2[,i] = qr_est  # calculated quarantine rate
  }
  
  i.mu_lps <- data.frame(matrix(rep(NA, sample_num * (T3_num - T1_num + 1)), ncol = sample_num)) # expected incidence
  inc_uq_lps <- data.frame(matrix(rep(NA, sample_num * length(inc)), ncol = sample_num)) # un-quarantined cases 
  
  quar_rate_sample2_toT3_num = data.frame(matrix(rep(quar_rate_sample2[t_len,],times = T3_num-T2_num), ncol =sample_num, byrow = TRUE))
  quar_rate_sample2_toT3_num = rbind(quar_rate_sample2, quar_rate_sample2_toT3_num)
  
  for (i in 1:sample_num){
    inc_uq_lps[1:(T1_num-1), i]<- inc[1:(T1_num-1)] # there is no quarantine yet before T1
    for(t in T1_num:T3_num){
      renew <- R * sum(inc_uq_lps[(t-1):1, i] * gam(1:(t-1), shap, rat)) # to calculate the expected number of the total infections
      i.mu_lps[t-T1_num+1,i]<- renew
      if(t<=T2_num){
        inc_uq_lps[t, i] <- rbinom(n = 1, 
                                   size = inc[t],
                                   prob = quar_rate_sample2_toT3_num[[t-T1_num+1,i]]
        )
      }else{ 
        inc_uq_lps[t, i] <- rbinom(n = 1, 
                                   size = round(i.mu_lps[t-T1_num+1,i]),
                                   prob = quar_rate_sample2_toT3_num[[t-T1_num+1,i]]
                                   )
      }
      }
    }
  
  # calcualte the confidence interval for expected incidence
  ci_value = c(0.025,0.975, 0.5) # 95% CI
  ci_name =c("lower_i.mu_lps","upper_i.mu_lps","median_i.mu_lps")
  nci = length(ci_value)
  
  CI_i.mu_lps = data.frame(matrix(rep(0, nci*(T3_num-T1_num+1)), ncol = nci))
  colnames(CI_i.mu_lps) <- ci_name
  for (j in 1:(T3_num-T1_num+1)){
    CI_i.mu_lps[j,] = as.numeric(quantile(i.mu_lps[j,], probs = ci_value))
  }
  
  CI_i.mu_lps$day_num = df_wh$Day_num[T1_num:T3_num]
  CI_i.mu_lps$date = df_wh$Day_report[T1_num:T3_num]
  CI_i.mu_lps$inc = inc[T1_num:T3_num]
  CI_i.mu_lps$R= as.factor(R)
  
  return(list(CI_i.mu_lps, i.mu_lps))
  
}

i_mu_ci.knot24R1.5 <- case_predict(df.par_lps_est.knot24R1.5, 1.5)
i_mu_ci.knot24R2.0 <- case_predict(df.par_lps_est.knot24R2.0, 2)
i_mu_ci.knot24R3.0 <- case_predict(df.par_lps_est.knot24R3.0, 3)
i_mu_ci.knot24R4.0 <- case_predict(df.par_lps_est.knot24R4.0, 4)
i_mu_ci.knot24R5.0 <- case_predict(df.par_lps_est.knot24R5.0, 5)
i_mu_ci.knot24R6.0 <- case_predict(df.par_lps_est.knot24R6.0, 6)

# confidence interval of predicted expected cases
CI_i.mu.knot24R1.5 <- i_mu_ci.knot24R1.5[[1]]
CI_i.mu.knot24R2.0 <- i_mu_ci.knot24R2.0[[1]]
CI_i.mu.knot24R3.0 <- i_mu_ci.knot24R3.0[[1]]
CI_i.mu.knot24R4.0 <- i_mu_ci.knot24R4.0[[1]]
CI_i.mu.knot24R5.0 <- i_mu_ci.knot24R5.0[[1]]
CI_i.mu.knot24R6.0 <- i_mu_ci.knot24R6.0[[1]]

# number of predicted expected cases over time
i.mu.knot24R1.5 <- i_mu_ci.knot24R1.5[[2]]
i.mu.knot24R2.0 <- i_mu_ci.knot24R2.0[[2]]
i.mu.knot24R3.0 <- i_mu_ci.knot24R3.0[[2]]
i.mu.knot24R4.0 <- i_mu_ci.knot24R4.0[[2]]
i.mu.knot24R5.0 <- i_mu_ci.knot24R5.0[[2]]
i.mu.knot24R6.0 <- i_mu_ci.knot24R6.0[[2]]


CI_i.mu.knot24 <- rbind(CI_i.mu.knot24R1.5, CI_i.mu.knot24R2.0, CI_i.mu.knot24R3.0,
                      CI_i.mu.knot24R4.0, CI_i.mu.knot24R5.0, CI_i.mu.knot24R6.0)
CI_i.mu.knot24_subset <- CI_i.mu.knot24 %>% dplyr::filter(day_num > T2_num)

#####
# 3.2 plot the error-bar CI of expected incidences by lps method for 24 knots for each R
#####

CI_i.mu.knot24_subset_plot <- ggplot(CI_i.mu.knot24_subset, aes(x = date, y= inc)) +
  geom_errorbar(mapping=aes(ymin = lower_i.mu_lps, ymax = upper_i.mu_lps, colour=R),size=0.4, position = position_dodge(0.8)) +
  geom_point(mapping=aes(x = date, y = median_i.mu_lps, color=R), shape=16, size = 0.9, position = position_dodge(0.8),fill="white") +
  ylab("Number of predicted cases") + xlab("Calendar day") + 
  coord_x_date(xlim = c(T2, T3))+  
  scale_x_date(date_breaks="2 day",date_labels="%Y-%m-%d") +
  scale_y_continuous(expand = c(0, 0.1), 
                     limits = c(0,6),
                     breaks=seq(0,6)
                     ) +
  scale_colour_discrete(name=" R",labels = c("1.5", "2.0", "3.0", "4.0", "5.0", "6.0")) +
  geom_hline(yintercept = 1, color="darkgrey",size=0.8,linetype="dotted") +
  geom_vline(xintercept = as.Date("2020-04-08"), color="darkgrey", size=0.8, linetype="dashed") +
  theme_bw(base_size=12) +
  theme(axis.text.x=element_text(size=8,angle = 90, vjust=0.5, hjust = 1),
        axis.text.y=element_text(size=8),
        axis.title=element_text(size=11),
        plot.title = element_text(size = 11,face="bold"),
        legend.background = element_rect(fill="transparent"),
        legend.box.background = element_rect(colour = "black"),
        panel.grid= element_blank(),
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        legend.position = c(0.97,0.97),#"left",#c(.75, .95),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.justification = c("right", "top")
  ) +
  annotate(
    geom = "curve", x = as.Date("2020-04-08"), y = 3, xend = as.Date("2020-04-10"), yend = 3, 
    curvature = 0, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text",  x = as.Date("2020-04-10"), y = 3, label = "The lockdown was lifted", hjust = "left")

plot(CI_i.mu.knot24_subset_plot)

ggsave(filename = "../fig_24knot/fig 4_24knot.tiff",
       plot = CI_i.mu.knot24_subset_plot ,
       #width = 18.5,  # <=19.05cm
       #height = 16, # <=22.225cm
       width = 14,  # <=19.05cm
       height = 12, # <=22.225cm
       units= "cm",
       dpi= 300,
       compression = "lzw")



##
# 3.3 calculate the ending probability with 95% as threshold for both lps methods
##

##-- ending prob for lps method
# we count the total number of expected incidence smaller than th over all the nsample-expected incidence for each calendar day,
# then we have the frequency by dividing the nsample


# number of predicted expected cases over time
i.mu.knot24R1.5 <- i_mu_ci.knot24R1.5[[2]]
i.mu.knot24R2.0 <- i_mu_ci.knot24R2.0[[2]]
i.mu.knot24R3.0 <- i_mu_ci.knot24R3.0[[2]]
i.mu.knot24R4.0 <- i_mu_ci.knot24R4.0[[2]]
i.mu.knot24R5.0 <- i_mu_ci.knot24R5.0[[2]]
i.mu.knot24R6.0 <- i_mu_ci.knot24R6.0[[2]]

prob_end <-  function(i.mu_lps, th=1){ # set the threshold to define the ending of outbreak
  sample_num = ncol(i.mu_lps)
  sum_inc_lps = rep(NA, nrow(df_wh) - T2_num + 1)
  i.mu_lps_na <- rbind(data.frame(matrix(NA, nrow = T1_num-1, ncol = sample_num)), i.mu_lps)
  for(day in seq(T2_num, T3_num)){
    count = c()
    for(j in seq(sample_num)){
      if (sum(i.mu_lps_na[day:T3_num, j]>=th)==0){
        count = c(count, j)
      }
    }
    sum_inc_lps[day-T2_num+1] = length(count)/sample_num # proportion of incidence smaller than one among all the samples
  }
  
  return(sum_inc_lps)
}

prob_end_lps.knot24R1.5 <- prob_end(i.mu.knot24R1.5)
prob_end_lps.knot24R2.0 <- prob_end(i.mu.knot24R2.0)
prob_end_lps.knot24R3.0 <- prob_end(i.mu.knot24R3.0)
prob_end_lps.knot24R4.0 <- prob_end(i.mu.knot24R4.0)
prob_end_lps.knot24R5.0 <- prob_end(i.mu.knot24R5.0)
prob_end_lps.knot24R6.0 <- prob_end(i.mu.knot24R6.0)


# combine all the prob_end
prob_end_lps <- data.frame(date= df_wh$Day_report[T2_num:T3_num],
                           day_num = seq(T2_num, T3_num),
                           prob_end_lps.knot24R1.5 = prob_end_lps.knot24R1.5*100, # 100%
                           prob_end_lps.knot24R2.0 = prob_end_lps.knot24R2.0*100,
                           prob_end_lps.knot24R3.0 = prob_end_lps.knot24R3.0*100,
                           prob_end_lps.knot24R4.0 = prob_end_lps.knot24R4.0*100,
                           prob_end_lps.knot24R5.0 = prob_end_lps.knot24R5.0*100,
                           prob_end_lps.knot24R6.0 = prob_end_lps.knot24R6.0*100)


v1 = prob_end_lps$date[min(which(prob_end_lps$prob_end_lps.knot24R1.5 > 95))] # the first day on which the ending prob is greater than 95% by the method of exp
v2 = prob_end_lps$date[min(which(prob_end_lps$prob_end_lps.knot24R2.0 > 95))] # the first day on which the ending prob is greater than 95% by the method of lps
v3 = prob_end_lps$date[min(which(prob_end_lps$prob_end_lps.knot24R3.0 > 95))]
v4 = prob_end_lps$date[min(which(prob_end_lps$prob_end_lps.knot24R4.0 > 95))]
v5 = prob_end_lps$date[min(which(prob_end_lps$prob_end_lps.knot24R5.0 > 95))]
v6 = prob_end_lps$date[min(which(prob_end_lps$prob_end_lps.knot24R6.0 > 95))]


prob_end_lps.plot = prob_end_lps %>% tidyr::gather("method", "prob_end", 3:8)
prob_end_lps.plot$method = factor(prob_end_lps.plot$method)

prob_end_lps_plot <- ggplot(prob_end_lps.plot, aes(x = date, y = prob_end, shape = method)) +
  # geom_point(size = 0.8) +
  geom_hline(yintercept = 95, color="darkgrey", size=0.7, linetype="dotted", alpha =0.8) +
  geom_vline(xintercept = v1, color="darkgrey", size=0.5, linetype="dashed", alpha =0.8) +
  geom_vline(xintercept = v2, color="darkgrey", size=0.5, linetype="dashed", alpha =0.8) +
  geom_vline(xintercept = v3, color="darkgrey", size=0.5, linetype="dashed", alpha =0.8) +
  geom_vline(xintercept = v4, color="darkgrey", size=0.5, linetype="dashed", alpha =0.8) +
  geom_vline(xintercept = v5, color="darkgrey", size=0.5, linetype="dashed", alpha =0.8) +
  geom_vline(xintercept = v6, color="darkgrey", size=0.5, linetype="dashed") +
  geom_point(size = 0.9) +
  geom_vline(xintercept = as.Date("2020-04-08"), color="darkgrey", size=0.7,alpha =0.8, linetype="dashed") +
  ylab("The ending probability of the outbreak (%)") + xlab("Calendar day") +
  scale_x_date(date_breaks="2 day",date_labels="%b %d") +
  scale_y_continuous(expand = c(0, 5), 
                     limits = c(0, 100),
                     breaks = c(0, 25, 50, 75, 95, 100)
                     ) +
  scale_shape_discrete(name=" R",labels = c("1.5", "2.0", "3.0", "4.0", "5.0", "6.0")) +
  theme_bw(base_size=12) +
  theme(axis.text.x=element_text(size=8,angle = 90, vjust=0.5, hjust = 1),
        axis.text.y=element_text(size=8),
        axis.title=element_text(size=11),
        plot.title = element_text(size = 11,face="bold"),
        legend.background = element_rect(fill="transparent"),
        legend.box.background = element_rect(colour = "black"),
        legend.title=element_text(size=8),
        panel.grid= element_blank(),
        legend.text=element_text(size=8),
        legend.position = c(0.95,0.8),#"left",#c(.75, .95),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.justification = c("right", "top")
  ) +
  geom_segment(
    x = as.Date("2020-04-08"), y = 25, xend = as.Date("2020-04-11"), yend = 25, size = 0.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text",  x = as.Date("2020-04-12"), y = 25, size=3, label = "The lockdown was lifted", hjust = "left") + 
  geom_segment(
    x = v1, y = 15, xend = as.Date("2020-03-30"), yend = 15, size =0.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text",  x = as.Date("2020-03-28"), y = 15, size=3, label = "v1", hjust = "left") + 
  geom_segment(
    x = v2, y = 25, xend = as.Date("2020-03-30"), yend = 25, size =0.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text",  x = as.Date("2020-03-28"), y = 25, size=3, label = "v2", hjust = "left") + 
  geom_segment(
    x = v3, y = 35, xend = as.Date("2020-03-30"), yend = 35, size = 0.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text",  x = as.Date("2020-03-28"), y = 35, size=3, label = "v3", hjust = "left") + 
  geom_segment(
    x = v4, y = 45, xend = as.Date("2020-03-30"), yend = 45, size = 0.3, 
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text",  x = as.Date("2020-03-28"), y = 45, size=3, label = "v4", hjust = "left") + 
  geom_segment(
    x = v5, y = 55, xend = as.Date("2020-03-30"), yend = 55, size = 0.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text",  x = as.Date("2020-03-28"), y = 55, size=3, label = "v5", hjust = "left") + 
  geom_segment(
    x = v6, y = 65, xend = as.Date("2020-03-30"), yend = 65, size = 0.3,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text",  x = as.Date("2020-03-28"), y = 65, size=3, label = "v6", hjust = "left")

plot(prob_end_lps_plot)

ggsave(filename = "../fig_24knot/fig 5_24knots.tiff",
       plot = prob_end_lps_plot,
       width = 14,  # <=19.05cm
       height = 12, # <=22.225cm
       units= "cm",
       dpi= 300,
       compression = "lzw")






