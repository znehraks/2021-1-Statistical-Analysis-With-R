fibolist = function(size){
  if(size <=2){
    stop("The Size should be greater than 2")
  }
  num1 = 1
  num2 = 1
  fibonacci=c(num1,num2)
  count=2
  repeat{
    num2 = fibonacci[length(fibonacci)]+fibonacci[length(fibonacci)-1]
    if(count == size)break
    fibonacci=c(fibonacci,num2)
    count = count+1
  }
  print(fibonacci)
}
fibolist(10)
