#The program takes a number k and prints the pascal¡¯s triangle having k number of rows.
#I got inspired by reading " https://github.com/ramandixit13/PascalsTriangle "
Pascal_triangle <- function(k){
  Pas_result <- matrix(0,nrow = k,ncol = k)
  Pas_result[,1] <- 1  
  for (i in 2:k){
    for(j in 2:i){
      Pas_result[i,j] <- Pas_result[i-1,j-1] + Pas_result[i-1,j]
    }
  }
  print(Pas_result[k,]) 
}

Pascal_triangle(100)
Pascal_triangle(200)