

path <- "/Users/work/Desktop/Website/content/post/new_scientist_puzzle_238/"
source(paste0(path, "knight_moves.R"))


df <- data.frame()
m <- 20
for(i in 1:m){
   res <- final_path(c(1, 1), 9)
   bb <- data.frame(matrix(unlist(res), ncol = 2, byrow = T))
   bb$m <- i
   df <- rbind(df, bb)
}

str(df)

setwd("~/Desktop/Website/content/post/new_scientist_puzzle_238/")
#save(df, file = "results.RData")


##############
load(paste0(path, "results.RData"))



library(ggplot2)

ggplot(df) +
   geom_point(aes(x = X1, y = X2)) +
   geom_abline(intercept = 0, slope = 1) +
   facet_wrap(.~m)

df %>% mutate(sum_coord = X1+X2) %>% ggplot(.) + 
   geom_point(aes(x = m, y = sum_coord)) +
   geom_abline(intercept = 0, slope = 1)
   facet_grid(.~m)


df %>% group_by(m) %>% summarise(sums = sum(X1==1 & X2==1) + sum(X1 ==8 & X2 == 1)) %>% filter(sums <= 2)


# unique solutions 1, 3, 6, 8, 
df %>% filter(m %in% c(1, 3, 6, 8)) %>%
   ggplot(.) +
   geom_point(aes(x = X1, y = X2)) +
   geom_abline(intercept = 0, slope = 1) +
   facet_wrap(.~m)



#unique_solutions <- 4, 8, 9, 11, 12
df %>% filter(m == 4) # unique
df %>% filter(m == 8) # unique
df %>% filter(m == 9) # unique
df %>% filter(m == 11) # same as 9 
df %>% filter(m == 12) # unique

unique_solutions <- c(4, 8, 9, 11, 12)

df %>% filter(m %in% unique_solutions) %>% group_by(m) %>% mutate(order = factor(1:10)) %>% 
   mutate(X1 = as.character(X1), X2 = as.character(X2)) %>%
   ggplot(.) +
   geom_point(aes(x = order, y = X1, col = as.factor(order)), size = 2) +
   geom_jitter(aes(x = order, y = X2, col = as.factor(order)), shape = 3, width = 0, height = 0.2, size = 2) +
   facet_wrap(.~m) +
   labs(y = "Position; x = circle, y = cross", x = "Sequence of moves") +
   theme_linedraw(13)

   






