

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
save(df, file = "results.RData")


load(paste0(path, "results.RData"))



