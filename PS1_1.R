Print_values <- function(a,b,c) {
  if(a>b) 
    if(b>c) My_Result<-c(a,b,c)
    else {
      if(a>c) My_Result<-c(a,c,b)
      else My_Result<-c(c,a,b)
    }
      
  else{
    if(b>c)
      if(a>c) My_Result<-c(b,a,c)
      else My_Result<-c(b,c,a)
    else My_Result<-c(c,b,a)
  }
  print(My_Result)
}

Print_values(3,5,2)
