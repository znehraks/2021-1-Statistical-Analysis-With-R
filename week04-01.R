sum3 = 0 
i<-1
repeat{
  sum3 = sum3+i
  print(i)
  i = i+1
  if(i>10)break
}
print(sum3)

i=0
while(i<=30){
  i = i+1
    if(i%%2==0){
      print(i)
    }
}

fibo = function(n){
  if(n==1 || n==2){
    return(1)
  }
  return(fibo(n-1)+fibo(n-2))
}
fibo(1)
fibo(2)
fibo(3)
fibo(5)
fibo(10)
fibo(30)

calculator = function(x, type){
  switch (type,
    "mean" = mean(x),
    "sum" = sum(x),
    print("unexpected type")
  )
}
x = c(1:10)
calculator(x, "mean")
calculator(x, "sum")
calculator(x, "sd")

myF = function(x){
  return(x*x)
}
myF(2)


queue = function(){
  
  q = c()
  q_size = 0
  enqueue = function(data){
    q <<- c(q,data)
    q_size <<- q_size+1
  }
  
  dequeue = function(){
    if(is.na(q[1])){
      return() 
    }
    first = q[1]
    q <<- q[-1]
    q_size <<- q_size-1
    return(first)
  }
  
  size = function(){
    return(q_size) 
  }
  return(list(enqueue=enqueue, dequeue=dequeue, size=size))
}
q = queue()
q$enqueue(1)
q$enqueue(2)
q$size()

q$dequeue()
q$size()

fun = function(){
  answer = readline("y/n을 입력하세요: ")
  if(substr(answer,1,1) == "n"){
    cat("n을 입력받았습니다.")
  }else{
    cat("y를 입력받았습니다.")
  }
}
fun2 = function(){
  x = readline("문장을 입력하세요: ")
  unlist(strsplit(x," "))
}

fun2()
