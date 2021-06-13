stack = function(){
  s = c()
  s_size = 0
  
  push = function(data){
    if(s_size == 20){
      print("stack is full")
      return()
    }
    s <<- c(s,data)
    s_size <<- s_size + 1
  }
  
  pop = function(){
    if(s_size ==0){
      print("stack is empty")
      return()
    }
    last <<- s[s_size]
    s <<- s[-s_size]
    s_size <<- s_size - 1
    return(last)
  }
  
  size = function(){
    return(s_size)
  }
  
  empty = function() {
    return(is.na(s[1]))
  }
  
  full = function(){
    return(s_size == 20)
  }
  return(list(push=push,pop=pop,size=size,empty=empty,full=full))
}
s = stack()
s$push(1)
s$push(2)
s$pop()
s$size()
s$empty()
s$full()
