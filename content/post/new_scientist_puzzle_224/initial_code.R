# pkgs used in this post
library(gtools)
library(stringi)
library(stringr)
library(rebus) 

colour <- c("blue", "yellow", "pink", "red", "green") # all the different colours

numb <- 1:5 # the size index (1 is the largest)

opt <- paste0(rep(colour, each = 5), "_", numb) # combine colour and index

perms <- permutations(n = 25, r = 5, v = opt, repeats.allowed = F) # all the combinations of 5 dolls

##### add restrictions
# 1: select only the ones where blue is the smallest doll
blue_smallest_doll <- which(perms[,5] == "blue_5") # blue is the smallest doll

perms2 <- perms[blue_smallest_doll,]

# 2: select only the combinations where each colour appears once and indexes are sorted correctly 
combs <- c()
for(i in 1:nrow(perms2)){
   perms3 <- stri_split_fixed(perms2[i,], "_", simplify = T) # split the vectors into two
   dupl <- sum(stri_duplicated(perms3[,1])) # check for uniqueness
   sorted <- is.unsorted(perms3[,2], strictly = T) # check for correct order 
   
   if(dupl == 0 & sorted == 0){
      combs <- rbind(perms2[i,], combs)
   }
}

combs 

# 3: apply first restriction
# no doll contains a pink with a red doll anywhere within it
pattern_pink_red <- ("pink" %R% zero_or_more(WRD) %R% "red") 

ind <- c()
for(i in 1:nrow(combs)){
   
   splitted <- stri_split_fixed(combs[i,], "_", simplify = T) # split the vectors into two
   
   ind[i] <- str_detect(paste(splitted[,1], collapse = ""), pattern_pink_red, negate = T) # find patterns
}

combs_update <- combs[ind, ]
combs_update

##### 1st interpretation 
# no yellow doll contains a green doll with a pink inside it
pattern_yellow_green_pink <- ("yellow" %R% zero_or_more(WRD) %R% "green" %R% zero_or_more(WRD) %R% "pink") 

ind <- c()
for(i in 1:nrow(combs_update)){
   
   splitted <- stri_split_fixed(combs_update[i,], "_", simplify = T) # split the vectors into two
   
   ind[i] <- str_detect(paste(splitted[,1], collapse = ""), pattern_yellow_green_pink, negate = T) # find patterns
   
}

combs_update[ind, ]

##### 2nd interpretation 
# no green doll with a pink doll anywhere within it
pattern_green_pink <- ("green" %R% zero_or_more(WRD) %R% "pink") 

ind <- c()
for(i in 1:nrow(combs_update)){
   
   splitted <- stri_split_fixed(combs_update[i,], "_", simplify = T) # split the vectors into two
   
   ind[i] <- str_detect(paste(splitted[,1], collapse = ""), or(pattern_yellow_green_pink, pattern_green_pink), 
                        negate = T) # find patterns
   
}

combs_update[ind, ]

#### 3rd interpretation
pattern_yellow_green<- ("yellow" %R% zero_or_more(WRD) %R% "green")

ind <- c()
for(i in 1:nrow(combs_update)){
   
   splitted <- stri_split_fixed(combs_update[i,], "_", simplify = T) # split the vectors into two
   
   ind[i] <- str_detect(paste(splitted[,1], collapse = ""), 
                        or(pattern_yellow_green_pink, pattern_green_pink, pattern_yellow_green), 
                        negate = T) # find patterns
   
}

combs_update[ind, ]
