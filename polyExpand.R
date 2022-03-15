## polyExpand
polyExpand<-function(x,k){
  Xp<-c()
  for (n in 1:k){
    Xp<-cbind(Xp,x^n)
  }
  return(Xp)
}