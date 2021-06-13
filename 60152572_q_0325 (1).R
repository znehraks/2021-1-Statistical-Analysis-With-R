#s1,s2,s3를 각각 과목으로 보는 경우우
pass_fail = function(score){
  result = c()
  for(i in 1:nrow(score)){
    if(score[i,1]<50 | score[i,2] < 50 | score[i,3] < 50){
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
result = pass_fail(score)
