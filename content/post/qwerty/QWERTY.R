




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

