#Write a function to find the Least moves to get the result by either double or add 1
#https://blog.csdn.net/OldDriver1995/article/details/105529928

#Create a random value between 1 and 100.
random_val <- sample(1:100,1)

#Build the function
Least_moves <- function(num){
  if (num <= 3) move_res <- num-1
  if (num > 3){
    if (num%%2!=0) move_res <- 1+ Least_moves(num-1)
    else
      move_res <-  1 + min(c(Least_moves(num-1), Least_moves(round(num/2))))
    }
  return(move_res)
}

Least_moves(2)
Least_moves(5)

#Output the random value and its least moves
paste("The number is:",random_val,",and the result is:")
Least_moves(random_val)


