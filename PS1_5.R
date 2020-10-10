#create Sign function
Sign <- function(x){
  if(x>0) return(1) 
  else{return(-1)}
    
}

#Create Find_exp_n function to add different numbers
Find_exp_n <- function(n){
  data_fra <- data.frame(ID <- 1,
                   solution_res <- 1,
                   solution_end <- 1,
                   solution_exp <- "1 = 1") 
  
  if(n == 1){return(data_fra)}else{
    for (x in Find_exp_n(n-1)) {
      solution_res_add <- c(as.numeric(x[1])+1,as.numeric(x[2])+n,+n,paste(x[3],"+"))
      data_fra[as.numeric(x[1]),] <- solution_res_add
    } 
    
    for (x in Find_exp_n(n-1)) {
      solution_res_minus <- c(x[1]+1,x[2]-n,-n,paste(x[3],"-"))
      data_fra[x[1],] <- solution_res_minus
    }
    
    for (x in Find_exp_n(n-1)) {
      solution_res_null <- c(x[1]+1,x[2]*9+Sign(x[2])*n,x[2]*10+Sign(x[2])*n,paste(x[3],""))
      data_fra[x[1],] <- solution_res_null
    }
      return(data_fra)
  }
}
  

#Create Find_expression function     
Find_expression <- function(target_res){
  data_fra <- Find_exp_n(9)
  solution_res <- data_fra[,2]
  solution_exp <- data_fra[,4][which(solution_res == target_res)]
  print(solution_exp)

  #Find the Total_solutions and plot
  for(i in letters(1:100)){
    xlim[i] <- i
    Total_sol_list <- data_fra[,4][which(solution_res == i)]
    Total_solutions[i] <- table(Total_sol_list)
  }
  plot(xlim,Total_solutions,lwd=0.5,type="p",col="blue")
  min_sol_num <- xlim[which.min(Total_solutions)]
}


