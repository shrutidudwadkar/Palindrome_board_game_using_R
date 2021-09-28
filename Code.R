#Grid to to store letters in letter grid
lgrid <- matrix(NA, nrow = 8, ncol = 8)
lgrid[1,] <- c("r", "l", "q", "s", "t", "z", "c", "a")
lgrid[2,] <- c("i", "v", "d", "z", "h", "l", "t", "p")
lgrid[3,] <- c("u", "r", "o", "y", "w", "c", "a", "c")
lgrid[4,] <- c("x", "r", "f", "n", "d", "p", "g", "v")
lgrid[5,] <- c("h", "j", "f", "f", "k", "h", "g", "m")
lgrid[6,] <- c("k", "y", "e", "x", "x", "g", "k", "i")
lgrid[7,] <- c("l", "q", "e", "q", "f", "u", "e", "b")
lgrid[8,] <- c("l", "s", "d", "h", "i", "k", "y", "n")

to_print <- T

#Initialize the green squares to a vector
green_square <- c("B6", "C7", "F2", "G3")


#Calculates the frequency for each letter in the grid
library("Rfast")
centre_elements <- as.vector(t(lgrid))
letter_frequency <- c()
for(ind in 1:length(centre_elements)) {
  letter_frequency <- c(letter_frequency, count_value(centre_elements, centre_elements[ind]))
}

#Creates a neighborgrid which stores all the neighbors to the North(N), NorthEast(NE), East(E), 
#SouthEast(SE), South(S), SouthWest(SW), West(W), NorthWest(NW) of the CENTRE square. 
#It also stores the frequency of for each letters in the grid
n = 8
lgrid.pad = rbind(NA, cbind(NA, lgrid, NA), NA)
ind = 2: (n+1)
neighborgrid = rbind(
  
  CENTRE = centre_elements,
  N  = as.vector(lgrid.pad[ind - 1, ind    ]),
  NE = as.vector(lgrid.pad[ind - 1, ind + 1]),
  E  = as.vector(lgrid.pad[ind    , ind + 1]),
  SE = as.vector(lgrid.pad[ind + 1, ind + 1]),
  S  = as.vector(lgrid.pad[ind + 1, ind    ]),
  SW = as.vector(lgrid.pad[ind + 1, ind - 1]),
  W  = as.vector(lgrid.pad[ind    , ind - 1]),
  NW = as.vector(lgrid.pad[ind - 1, ind - 1]),
  FREQUENCY = letter_frequency
)

#Adds columns to the neighborgrid matrix i.e. A1, A2..H7, H8
colnames(neighborgrid) <- paste0(rep(LETTERS[1:8], each=8), rep(1:8, 8))

#Samples a random value on the grid
fetch_current_square <- function() {
  return(paste0(sample(LETTERS[1:8], size = 1), sample(c(1:8), size = 1)))
  
}

#Checks if the current square is a border element and returns true
#else returns false if on white square
library(tidyverse)
check_if_border_element <- function(current_square) {
  if(anyNA(neighborgrid[, current_square])) {
    return (TRUE);
  } else {
    return (FALSE);
  }
  
}

#Checks if players token is on the edge of the board, if yes, then move to a random of one of the 64 squares
#If player is not on the edge of the board, it derives a random direction adjacent to the current square and 
#returns the new token on the grid.
next_move <- function(current_square) {
  
  if(to_print == T) cat("Current square: ", current_square, "\n")
  if(check_if_border_element(current_square)) {
    current_square <- fetch_current_square()
    next_move(current_square)
    
  } else {
    
    direction <- sample(c("N", "NE", "E", "SE", "W", "NW", "SW", "S"), size = 1)
    x_cord <- str_sub(current_square, 1, 1)
    y_cord <- str_sub(current_square, 2, 2)

    new_current_square = switch(   
      direction,   
      "N"= paste0(LETTERS[match(x_cord, LETTERS) - 1] , as.numeric(y_cord)),   
      "NE"= paste0(LETTERS[match(x_cord, LETTERS) - 1] , as.numeric(y_cord) + 1),   
      "E"= paste0(x_cord, as.numeric(y_cord) + 1),   
      "SE"= paste0(LETTERS[match(x_cord, LETTERS) + 1] , as.numeric(y_cord) + 1), 
      "W"= paste0(x_cord, as.numeric(y_cord) - 1),  
      "NW"= paste0(LETTERS[match(x_cord, LETTERS) - 1] , as.numeric(y_cord) - 1),  
      "SW"= paste0(LETTERS[match(x_cord, LETTERS) + 1] , as.numeric(y_cord) - 1),  
      "S"= paste0(LETTERS[match(x_cord, LETTERS) + 1] , as.numeric(y_cord)) 
    ) 
    if(to_print == T) cat("Direction: ", direction, "\t", "New current square: ", new_current_square, "\n")
    return(new_current_square)
  } 
  
  
}

#Checks if the current token is on green square, if yes, returns true, else returns false
is_green_square <- function(current_square) {
  
  if(current_square  %in% green_square) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

#This function handles the two events incase if the current token is on green square.
execute_event_if_green_square <- function(current_square_value, p, collection, collection_copy) {
  event = sample(LETTERS[1:2], size= 1, prob= c(p,1-p))
  
  switch(   
    event,   
    "A" = {
      collection <- c("f", "f", "h", "k")
      collection_copy <- c("h", "k")
    },   
    "B" = {
      #removes the existing current square values from the collection
      collection <- collection[!collection %in% current_square_value]
      # if the value if already present in copy remove it, else add the element
      if(current_square_value %in% collection_copy) {
        collection_copy <- collection_copy[collection_copy != current_square_value] 
      } else {
        collection_copy <- c(collection_copy, current_square_value)
      }
    }
  ) 
  if(to_print == T) cat("Green square Event: ", event, "\t", "Collection", collection, "\t", "Collection copy", collection_copy)
  
}


#fetch non edge element if not provided by user
count_num_moves <- function(current_square, prob, to_print) {
  move_counter <- 0
  is_palindrome <- FALSE
  collection <- c()
  collection_copy <- c()
  
  while(is_palindrome == FALSE) {
    
    if(to_print == T) cat("Current square: ", current_square, "\t", "Value: ", neighborgrid["CENTRE", current_square],"\n")
    
    freq <- neighborgrid["FREQUENCY", current_square]
    value <- neighborgrid["CENTRE", current_square]
    
    if(is_green_square(current_square)) {
      
      execute_event_if_green_square(neighborgrid["CENTRE", current_square], prob, collection, collection_copy)
      
    } else if(length(collection) < 3){
      
      if(freq == 4 || freq == 3) {
        collection <- c(collection, value)
        
        # if the value if already present in copy remove it, else add the element
        if(value %in% collection_copy) {
          collection_copy <- collection_copy[collection_copy != value] 
        } else {
          collection_copy <- c(collection_copy, value)
        }
        
      }
    } else if(length(collection) >= 3 && length(collection) <5 ) {
      
      #while adding the 4th element the it should add only those elements present in the collection
      if(length(collection) == 3 && value %in% collection) {
        collection <- c(collection, value)
        
        if(value %in% collection_copy) {
          collection_copy <- collection_copy[collection_copy != value] 
        } else {
          collection_copy <- c(collection_copy, value)
        }
        
      }
      
      #while adding the fifth character
      else if(length(collection) == 4) {
        
        #if copy is null add any character to the collection to form a palindrome
        if(length(collection_copy) == 0) {
          collection <- c(collection, value)
          
        } else if(value %in% collection_copy) {
          collection <- c(collection, value)
        }
      }
    } 
    if(to_print == T) cat("Collection", collection, "\t", "Collection copy", collection_copy, "\n")
    move_counter = move_counter + 1
    is_palindrome <- if(length(collection) == 5) ? break else FALSE
    
    current_square <- next_move(current_square)
  }
  
  if(to_print == T) cat("Total moves: ", move_counter, "\n")
  if(to_print == T) cat("Palindrome: ", collection, "\n")
  return(move_counter)
}


#Part 3
move_count <- replicate(1, count_num_moves("D4", 0.95, to_print))
hist(move_count, main ="Histogram to plot the number of moves required to complete the game", xlab = "Number of moves")

#Part 4
probabilities <- c(0.10, 0.30, 0.50, 0.75, 0.90)
average_moves <- c()
for(ind in 1:length(probabilities)) {
  move_count <- replicate(100, count_num_moves("D4", probabilities[ind], to_print))
  #hist(move_count, main ="Histogram of the number of moves to complete the game", xlab = "Number of Moves" )
  average_moves <- c(average_moves, mean(move_count))
  cat("Average moves required for probability ", probabilities[ind], " :", mean(move_count), "\n")
}

# Load ggplot2
library(ggplot2)

# Create data
alldata <- data.frame(
  probabilities ,  
  average_moves
)
ggplot(alldata, aes(x=probabilities, y=average_moves)) +
  geom_bar(stat = "identity")

#Part 5
move_count <- replicate(100, count_num_moves("D4", 0.95, to_print))
hist(move_count, main ="Histogram to plot the number of moves required to complete the game", xlab = "Number of moves")

move_count <- replicate(100, count_num_moves("F6", 0.05, to_print))
hist(move_count, main ="Histogram to plot the number of moves required to complete the game", xlab = "Number of moves")


#Part 6

moves_A <- c(25,13,16,24,11,12,24,26,15,19,34)
moves_B <- c(35,41,23,26,18,15,33,42,18,47,21,26)


t.test(x = moves_A, y = moves_B, alternative = "two.sided", paired = F)






