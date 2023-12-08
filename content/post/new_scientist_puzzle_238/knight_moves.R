

# Function to simulate knight moves
knight_moves <- function(start_pos, num_moves) {
    
   # Possible moves for the knight
   moves <- matrix(c(2, 1, -2, -1, 2, -1, -2, 1, 1, 2, -1, -2, 1, -2, -1, 2),
                   ncol = 2, byrow = TRUE)
   
   # Function to check if the position is valid on an 8x8 chessboard
   is_valid_pos <- function(coord) {
      x <- coord[1]
      y <- coord[2]
      return(x >= 1 && x <= 8 && y >= 1 && y <= 8)
    }
    
    # Starting position
    path <- list(start_pos)
    
    # Simulating knight moves
    for (i in 1:num_moves) {
        curr_pos <- path[[i]]
        
        # Generating all possible next moves
        #next_moves <- curr_pos + moves - wrong
        
        # Generating all possible next moves
        next_moves_x <- curr_pos[1] + moves[,1]
        next_moves_y <- curr_pos[2] + moves[,2]
        
        next_moves <- matrix(c(next_moves_x, next_moves_y),
                             ncol = 2, byrow = F)
        
        # Filtering valid moves
        next_moves <- next_moves[apply(next_moves, 1, is_valid_pos), ]
        
        # Randomly selecting the next move
        next_move <- next_moves[sample(nrow(next_moves), 1), ]
        path <- append(path, list(next_move))
    }
    
    return(path)
}

# Example usage
#start_pos <- c(3, 2) # Starting position
#num_moves <- 9 # Number of moves to simulate
#path <- knight_moves(start_pos, num_moves)
# Displaying the path
#for (i in 1:length(path)) {
#    cat("Move #", i, ": ", path[[i]], "\n")
#}



final_path <- function(start_pos, num_moves){
    
    any_path <- knight_moves(start_pos, num_moves)
    
    path_matrix <- matrix(unlist(any_path), ncol = 2, byrow = T)
    
    ### additional restrictions 
    
    # visit at least one square in every column 
    every_col <- seq(1, 8, by = 1)
    res1 <- every_col %in% path_matrix[, 1]
    
    # make exactly one hop into the lower half of the board
    lower_half <- 4
    res2 <- path_matrix[, 2] > lower_half

    # last one should be 8,1
    res3 <- path_matrix[10, ] == c(8, 1)
    
    # need to be at (2, 3) (or (3, 2)) after 1st hop and (3, 2) (or (2, 3)) at 3rd hop
    res4 <- (path_matrix[2, ] == c(3, 2) & path_matrix[4, ] == c(2, 3)) |
       (path_matrix[4, ] == c(3, 2) & path_matrix[2, ] == c(2, 3))
    
    # need to be at (6, 2) (or (7, 3)) at 7th hop and (7, 3) (or (6, 2)) at 8th hop
    res5 <- (path_matrix[9, ] == c(6, 2) & path_matrix[7, ] == c(7, 3)) |
       (path_matrix[7, ] == c(6, 2) & path_matrix[9, ] == c(7, 3))
    
    
    while(sum(res1) != 8 | sum(res2) != 1 | sum(res3) != 2 | sum(res4) != 2 | sum(res5) != 2) {
     
        any_path <- knight_moves(start_pos, num_moves)
        
        path_matrix <- matrix(unlist(any_path), ncol = 2, byrow = T)
        
        # visit at least one square in every column 
        res1 <- every_col %in% path_matrix[, 1]
        
        # make exactly one hop into the lower half of the board
        res2 <- path_matrix[, 2] > lower_half
       
        # last one should be 8,1
        res3 <- path_matrix[10, ] == c(8, 1)
        
        # 
        res4 <- (path_matrix[2, ] == c(3, 2) & path_matrix[4, ] == c(2, 3)) |
           (path_matrix[4, ] == c(3, 2) & path_matrix[2, ] == c(2, 3))
        
        # 
        res5 <- (path_matrix[9, ] == c(6, 2) & path_matrix[7, ] == c(7, 3)) |
           (path_matrix[7, ] == c(6, 2) & path_matrix[9, ] == c(7, 3))
        
        #print(c(sum(res1), sum(res2), sum(res3), sum(res4)))
    }
    
    return(any_path)
    
}

#set.seed(1234)
#set.seed(346)
#set.seed(7043)
#path1 <- final_path(c(1, 1), 9)
#for (i in 1:length(path1)) {
#   cat("Move #", i, ": ", path1[[i]], "\n")
#}
#beep()



result_wrapper <- function(res){
   for (i in 1:length(res)) {
     cat("Move #", i, ": ", res[[i]], "\n")
   }
}



