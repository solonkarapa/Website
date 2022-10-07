




X <- 19
Z <- 26
n <- 7 

(factorial(X) * factorial(Z - n))/(factorial(X - n) * factorial(Z))

calc_prbs <- function(X, Z, n){
    (factorial(X) * factorial(Z - n))/(factorial(X - n) * factorial(Z))
}    

library(purrr)

X <- c(10, 9, 8, 7)

map(X, calc_prbs, Z = Z, n = n)

# hypergeomtric
m <- seq(6, 26 - 7, by = 1)

dhyper(x = 7, m = 10, n = 26 - 10, k = 7, log = FALSE)

dhyper(x = 4, m = 5, n = 45, k = 10, log = FALSE)


prbs <- map_dbl(m, ~ dhyper(x = 7, m = .x, n = 26 - .x, k = 7))

df <- data.frame(m, prbs)

# prb of exact "qwerty" word sampled 
pr_exact <- (1/26)*(1/25)*(1/24)*(1/23)*(1/22)**(1/21)*(1/20)*(1/19)

ggplot(df) +
    geom_point(aes(x = m, y = prbs)) +
    labs(y = "Probability", x = "K") +
    geom_point(data = df %>% filter(m == "10"), aes(x = m, y = prbs), col = 'red') +
    geom_hline(yintercept = pr_exact, linetype = "dashed") +
    scale_x_continuous(breaks = m, labels = m) +
    theme_bw()


ggplot(df) +
    geom_point(aes(x = m, y = log(prbs))) +
    labs(y = "Log(Probability)", x = "Top row balls") +
    geom_point(data = df %>% filter(m == "10"), aes(x = m, y = log(prbs)), col = 'red') +
    geom_hline(yintercept = log(pr_exact), linetype = "dashed") +
    scale_x_continuous(breaks = m, labels = m) +
    theme_bw()

percentage_diff <- c()
for(i in 2:length(price_cap)){
percentage_diff[i] <- (price_cap[i] - price_cap[i-1])/price_cap[i-1]
}

percentage_diff*100
    
price_cap <- c(0.52, 0.55, 0.59, 0.61, 0.63, 0.65, 0.685)

usage <- 3600

price_cap*usage - (0.685*usage)

(0.28*3600)+(0.45*365)

(0.5209*3600)+(0.46*365)

(0.61*3600)+(0.46*365)



# TDVC
(0.5209*2900)+(0.46*365)

(q4*2900)+(0.48*365)

(0.68*2900)+(0.48*365)

# expected rates per quarter 
q4_22 <- 0.52
q1 <- 0.52
q2 <- 0.52
q3 <- 0.5
#q4 <- 0.67
q1 <-  0.85
q2 <- 0.99
q3 <- q4 <- 0.8
    
oct <- 256
nov <- 250
dec <- 470
jan <- 490
feb <- 380
mar <- 360
april <- 222
may <- 220
jun <- 200
jul <- 140
aug <- 150
sep <- 250

months <- c(oct, nov, dec, jan, feb, mar, april, may, jun, jul, aug, sep)

calc <- function(q4_22, q1, q2, q3){
    exp_variable <- ( (oct + nov + dec) * q4_22) + 
        ( (jan + feb + mar) * q1) + 
        ( (april + may + jun) * q2) + 
        ( (jul + aug + sep) * q3)  
    
    return(exp_variable)
}

pred_02 <- calc(0.52, 0.52, 0.52, 0.5)
pred_09 <- calc(0.52, 0.64, 0.65, 0.52)
pred_22 <- calc(0.52, 0.7, 0.74, 0.64)
pred_26 <- calc(0.52, 0.8, 0.92, 0.78)

df_preds <- data.frame(cost = c(pred_02, pred_09, pred_22, pred_26))
df_preds$fixed <- exp_fixed
df_preds$Date <- Date

df_preds <- df_preds %>% mutate(diffs = cost-fixed)

ggplot(df_preds) +
    ggtitle("costs") +
    geom_point(aes(x = Date, y = diffs)) 
    #geom_hline(aes(yintercept = fixed), linetype = "dashed") 
    #facet_grid(.~ prediction)


# under variable rate
#cons <- (295*3)    
exp_variable <- ( (oct + nov + dec) * q4_22) + 
    ( (jan + feb + mar) * q1) + 
    ( (april + may + jun) * q2) + 
    ( (jul + aug + sep) * q3)  
exp_variable

forecast_opt <- 2200
forecast_pess <- 2600
# under fixed rate
exp_fixed <-  sum(months * 0.6849) #+ (0.38*365)
exp_fixed

forecast_opt - exp_fixed
forecast_pess - exp_fixed

0.47*2900 + 365*0.40
0.52*2900 + 365*0.46


0.50*2900 + 365*0.47
0.53*2900 + 365*0.45
0.65*2900 + 365*0.46
0.78*2900 + 365*0.5

#q1 initial
0.52
0.64
0.7
0.8

Date <- c("02/08/22", "09/08/22", "22/08/22", "26/08/22")
pred <- c(0.52, 0.64, 0.7, 0.8, 0.52, 0.65, 0.74, 0.92, 0.5, 0.53, 0.64, 0.78)    
prediction <- c(rep("Q1", 4), rep("Q2", 4), rep("Q3", 4))

df <- data.frame(Date = rep(Date, 3), pred, prediction)

df %>% filter(Date == "26/08/22" )

ggplot(df) +
    ggtitle("Q1-Q3 2023") +
    geom_point(aes(x = Date, y = pred)) +
    geom_hline(yintercept = 0.68, linetype = "dashed") +
    facet_grid(.~ prediction)


