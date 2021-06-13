#s1,s2,s3를 각각 학생성적으로 보는경우
pass_fail = function(score){
  result = c()
  for(i in 1:ncol(score)){
    if(score[1,i]<50 | score[2,i] < 50 | score[3,i] < 50){
      result[i] = "fail"
    }else{
      result[i] = "pass"
    }
    cat(i, '번째 학생은', result[i], '입니다\n')
  }
}

s1 = c(100,100,49)
s2 = c(100,100,100)
s3 = c(50,50,50)
score = data.frame(s1,s2,s3)
score
result = pass_fail(score)
