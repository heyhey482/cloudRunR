install.packages("lpSolveAPI")
install.packages("linprog")
install.packages("nloptr")
library(lpSolveAPI)
library(linprog)
library(nloptr)

#* @apiTitle Plumber Example API
#* Echo back the input
#* @param strawberry_pr 딸기 가격
#* @param pumpkin_pr 호박 가격
#* @param tomato_pr 토마토 가격
#* @param pepper_pr 고추 가격
#* @param strawberry_s_pr 수막 딸기 가격
#* @param pumpkin_s_pr 수막 호박 가격
#* @param tomato_s_pr 수막 토마토 가격
#* @param pepper_s_pr 수막 고추 가격
#* @param strawberry_yr 딸기 단수
#* @param pumpkin_yr 호박 단수
#* @param tomato_yr 토마토 단수
#* @param pepper_yr 고추 단수
#* @param strawberry_s_yr 수막 딸기 단수
#* @param pumpkin_s_yr 수막 호박 단수
#* @param tomato_s_yr 수막 토마토 단수
#* @param pepper_s_yr 수막 고추 단수
#* @param strawberry_cr 딸기 비용
#* @param pumpkin_cr 호박 비용
#* @param tomato_cr 토마토 비용
#* @param pepper_cr 고추 비용
#* @param strawberry_s_cr 수막 딸기 비용
#* @param pumpkin_s_cr 수막 호박 비용
#* @param tomato_s_cr 수막 토마토 비용
#* @param pepper_s_cr 수막 고추 비용
#* @param energy_cost_r 에너지 가격 변화
#* @param water_constraint_r 가용 용수량
#* @param land_constraint_r 전체 토지 용수량
#* 품목별 비중
#* @param rice_ratio 쌀
#* @param rice_o_ratio 유기농 쌀
#* @param tomato_ratio 토마토
#* @param tomato_s_ratio 수막토마토
#* @param strawberry_ratio 딸기
#* @param strawberry_s_ratio 수막딸기
#* @param pumpkin_ratio 호박
#* @param pumpkin_s_ratio 수막호박
#* @param pepper_ratio 고추
#* @param pepper_s_ratio 수막고추
#* @param subsidy_r 수막보조금
#* @get /danmok
function(  #input 
  #price
  strawberry_pr = 0, pumpkin_pr = 0, tomato_pr = 0, pepper_pr = 0,  
  strawberry_s_pr = 0, pumpkin_s_pr = 0, tomato_s_pr = 0, pepper_s_pr = 0,  
  #단수(yield)
  strawberry_yr = 0, pumpkin_yr = 0, tomato_yr = 0, pepper_yr = 0,  
  strawberry_s_yr = 0, pumpkin_s_yr = 0, tomato_s_yr = 0, pepper_s_yr = 0,  
  #비용(cost)
  strawberry_cr = 0, pumpkin_cr = 0, tomato_cr = 0, pepper_cr = 0,  
  strawberry_s_cr = 0, pumpkin_s_cr = 0, tomato_s_cr = 0, pepper_s_cr = 0,  
  #에너지 가격 변화
  energy_cost_r = 0, 
  #가용 용수량
  water_constraint_r = 0,
  #전체 토지 용수량
  land_constraint_r = 0,
  #품목별 비중   
  rice_ratio = 0,         #쌀
  rice_o_ratio = 0,       #유기농 쌀 
  tomato_ratio = 0,       #토마토
  tomato_s_ratio = 0,     #수막토마토
  strawberry_ratio = 0,   #딸기
  strawberry_s_ratio = 0, #수막딸기
  pumpkin_ratio = 0,      #호박
  pumpkin_s_ratio = 0,    #수막호박
  pepper_ratio = 0,       #고추
  pepper_s_ratio = 0,     #수막고추
  
  #수막보조금 
  subsidy_r = 0) 
{
  
  #Basic data
  price<-data.frame(product=c("strawberry",   "pumpkin",    "tomato",   "pepper", 
                              "strawberry_w", "pumpkin_w",  "tomato_w", "pepper_w"), #won/kg
                    price=c(6097, 1125, 2016, 4218, 
                            6097, 1125, 2016, 4218))
  
  yield<-data.frame(product=c("strawberry",   "pumpkin",   "tomato",   "pepper", 
                              "strawberry_w", "pumpkin_w", "tomato_w", "pepper_w"), # kg/10a
                    yield=c(3665, 7087, 20925, 7070, 
                            3665, 7087, 20925, 7070))
  
  #10a 기준 비용자료
  #수막 사용 및 미사용
  #수막 설치 비용: 신진에너텍 SJ-1400, SJ-2000 평균 사용 -> 7610원/m2
  #수막설치비용 2000m2 기준으로 
  #내용 연수 10년 
  
  cost_curtain<-204750
  subsidy_curtain<-0
  
  #에너지비용
  energy_cost<-data.frame(product=c("strawberry",  "pumpkin",  "tomato",    "pepper", 
                                    "strawberry_w","pumpkin_w","tomato_w",  "pepper_w"),
                          cost=c(963129, 267474, 3331029, 3830361, 
                                 963129/20, 267474/20, 3331029/20, 3830361/20))
  #생산비가 아닌 경영비 기준
  cost<-data.frame(product=c("strawberry",   "pumpkin",   "tomato",    "pepper", 
                             "strawberry_w", "pumpkin_w", "tomato_w",  "pepper_w"),
                   cost=c(9047331, 3442858, 14123519, 9702380,  
                          9047331, 3442858, 14123519, 9702380))
  
  resource_use<-data.frame(product=c("strawberry",   "pumpkin",   "tomato",   "pepper", 
                                     "strawberry_w", "pumpkin_w", "tomato_w", "pepper_w"), 
                           land=c( 19.247,  321.581,  9.620,  261.499,
                                   118.680,  264.854,    3.220, 12.400),
                           water=c(31.190,  679.924 ,  38.312,  448.1620,
                                   8699.896, 15767.206, 71.38,  920.92))
  
  constraint<-data.frame(resource=c("land_cal", "water_cal"), 
                         constraint=c(sum(resource_use[,2]), sum(resource_use[,3])))
  
  #A matrix
  endow<-matrix(0, 2, 8)
  endow[1,]<-1
  endow[2,]<-resource_use[,3]/resource_use[,2]
  
  #가격 시나리오적용
  price_s<-price
  price_r<-c(1+strawberry_pr, 1+pumpkin_pr, 1+tomato_pr, 1+pepper_pr, 
             1+strawberry_s_pr, 1+pumpkin_s_pr, 1+tomato_s_pr, 1+pepper_s_pr)
  price_s[,2]<-price[,2]*price_r
  
  #단수(yield) 시나리오적용
  yield_s<-yield
  yield_r<-c(1+strawberry_yr, 1+pumpkin_yr, 1+tomato_yr, 1+pepper_yr, 
             1+strawberry_s_yr, 1+pumpkin_s_yr, 1+tomato_s_yr, 1+pepper_s_yr)
  yield_s[,2]<-yield[,2]*yield_r
  
  #비용관련 시나리오적용
  cost_s<-cost
  cost_r<-c(1+strawberry_cr, 1+pumpkin_cr, 1+tomato_cr, 1+pepper_cr, 
            1+strawberry_s_cr, 1+pumpkin_s_cr, 1+tomato_s_cr, 1+pepper_s_cr)
  
  cost_s[,2]<-cost[,2]*cost_r
  
  #에너지 가격(경유 가격) 관련 시나리오적용
  energy_cost_s<-energy_cost
  if(energy_cost_r>0|energy_cost_r<0){
    energy_cost_s[,2]<-energy_cost[,2]*(1+energy_cost_r)
  }
  
  #전체토지, 쌀 이용관련 시나리오적용
  constraint_l<-constraint
  constraint_l[1,2]<-constraint[1,2]*(1+land_constraint_r)
  
  #용수관련 시나리오적용
  constraint_l[2,2]<-constraint[2,2]*(1+water_constraint_r)
  
  
  #품목관련 토지 이용 제약
  #품목별 면적을 외생적으로 가정할 때
  
  #rice
  constraint_l<-constraint
  if(rice_ratio>0){
    constraint_l[1,2]<-constraint_l[1,2]*(1+land_constraint_r)*(1-rice_ratio)
  }
  #organic rice
  if(rice_o_ratio>0){
    constraint_l[1,2]<-constraint_l[1,2]*(1+land_constraint_r)*(1-rice_o_ratio)
  }
  #strawberry
  if(strawberry_ratio>0){
    constraint_l[1,2]<-constraint_l[1,2]*(1+land_constraint_r)*(1-strawberry_ratio)
  }
  #strawberry_s
  if(strawberry_s_ratio>0){
    constraint_l[1,2]<-constraint_l[1,2]*(1+land_constraint_r)*(1-strawberry_s_ratio)
  }
  #pumpkin
  if(pumpkin_ratio>0){
    constraint_l[1,2]<-constraint_l[1,2]*(1+land_constraint_r)*(1-pumpkin_ratio)
  }
  #pumpkin_s
  if(pumpkin_s_ratio>0){
    constraint_l[1,2]<-constraint_l[1,2]*(1+land_constraint_r)*(1-pumpkin_s_ratio)
  }
  #tomato
  if(tomato_ratio>0){
    constraint_l[1,2]<-constraint_l[1,2]*(1+land_constraint_r)*(1-tomato_ratio)
  }
  #tomato_s
  if(tomato_s_ratio>0){
    constraint_l[1,2]<-constraint_l[1,2]*(1+land_constraint_r)*(1-tomato_s_ratio)
  }
  #pepper
  if(pepper_ratio>0){
    constraint_l[1,2]<-constraint_l[1,2]*(1+land_constraint_r)*(1-pepper_ratio)
  }
  #pepper_s
  if(pepper_s_ratio>0){
    constraint_l[1,2]<-constraint_l[1,2]*(1+land_constraint_r)*(1-pepper_s_ratio)
  }
  
  #수막 보조금 비용 관련 시나리오적용
  curtain<-energy_cost
  curtain[5:8, 2]<-175000
  curtain[1:4, 2]<-0
  
  subsidy_curtain<-43750-14000*(1+subsidy_r)
  annual_cost_curtain<-(cost_curtain-subsidy_curtain)  #10a, 10년 기준
  curtain_s<-energy_cost
  curtain_s[5:8, 2]<-annual_cost_curtain
  curtain_s[1:4, 2]<-0
  
  #calibration cost(from original data)
  
  lambda<-c(8072926, 0, 20466103, 12026320, 8721536,0, 23410201, 15398053)
  dat<-cbind(cost_s, lambda, energy_cost_s[,2], curtain_s[,2], resource_use[,2], price_s[,2], yield_s[,2])
  
  #making results
  #result_r<-matrix(NA, 2, 2)
  #result_r[,1]<-c(0,0)
  #rownames(result_r)<-c("rice", "rice_organic") 
  
  results<-matrix(NA, 8, 2)
  results[,1]<-resource_use[,2]
  rownames(results)<-c("strawberry",  "pumpkin",  "tomato",    "pepper", 
                       "strawberry_w","pumpkin_w","tomato_w",  "pepper_w")
  
  #  if (rice_ratio>0){result_r[1, 2]<-rice_ratio*constraint_l[1,2]}
  #  if (rice_o_ratio>0){result_r[2, 2]<-rice_o_ratio*constraint_l[1,2]}
  if (strawberry_ratio>0){dat<-dat[!(dat$product %in% c("strawberry")),]
  results[1, 2]<-strawberry_ratio*constraint_l[1,2]}
  if (pumpkin_ratio>0){dat<-dat[!(dat$product %in% c("pumpkin")),]
  results[2, 2]<-pumpkin_ratio*constraint_l[1,2]}
  if (tomato_ratio>0){dat<-dat[!(dat$product %in% c("tomato")),]
  results[3, 2]<-tomato_ratio*constraint_l[1,2]}
  if (pepper_ratio>0){dat<-dat[!(dat$product %in% c("pepper")),]
  results[4, 2]<-pepper_ratio*constraint_l[1,2]}
  if (strawberry_s_ratio>0){dat<-dat[!(dat$product %in% c("strawberry_w")),]
  results[5, 2]<-strawberry_s_ratio*constraint_l[1,2]}
  if (pumpkin_s_ratio>0){dat<-dat[!(dat$product %in% c("pumpkin_w")),]
  results[6, 2]<-pumpkin_s_ratio*constraint_l[1,2]}
  if (tomato_s_ratio>0){dat<-dat[!(dat$product %in% c("tomato_w")),]
  results[7, 2]<-tomato_s_ratio*constraint_l[1,2]}
  if (pepper_s_ratio>0){dat<-dat[!(dat$product %in% c("pepper_w")),]
  results[8, 2]<-pepper_s_ratio*constraint_l[1,2]}
  
  
  if (nrow(dat)>=2) {
    
    cost_8<-dat[,2]
    lambda_8<-dat[,3]
    energy_cost_8<-dat[,4]
    curtain_8<-dat[,5]
    resource_use_8<-dat[,6]
    price_8<-dat[,7]
    yield_8<-dat[,8]
    
    #from 박경원 & 권오상 (농업경제연구 2011)      
    d<-cost_8-lambda+energy_cost_8+curtain_8
    q<-2*lambda/resource_use_8
    
    #optimization
    #objective function
    eval_f<-function(x) {
      return(sum(-price_8*yield_8*x+d*x+q*x^2/2))
    }
    
    #  gradient
    #  eval_grad_f<-function(x) {
    #    return(-price_8*yield_8+d+q*x)
    #  }
    
    
    #inequality constraints
    eval_g_ineq<-function(x){
      constr<-c(endow%*%x-constraint_l[,2])
      return("constraints"=constr)
    }
    
    #initial_value
    x0<-resource_use_8
    lb<-rep(0,8)
    ub<-rep(constraint_l[1,2],8)
    
    #testing the model with baseline data 
    
    res <-nloptr(x0=x0,
                 eval_f=eval_f,
                 #               eval_grad_f=eval_grad_f,
                 eval_g_ineq=eval_g_ineq,
                 lb=lb, 
                 ub=ub,
                 opts=list("algorithm"="NLOPT_LN_COBYLA", maxeval = 100000, xtol_rel = 1e-10)
    )
  }
  #making results
  
  results[which(is.na(results[,2]==TRUE)),2]<-res$solution
  
  if (nrow(dat)==1) {
    results[1, 2]<-strawberry_ratio*constraint_l[1,2]
    results[2, 2]<-pumpkin_ratio*constraint_l[1,2]
    results[3, 2]<-tomato_ratio*constraint_l[1,2]
    results[4, 2]<-pepper_ratio*constraint_l[1,2]
    results[5, 2]<-strawberry_s_ratio*constraint_l[1,2]
    results[6, 2]<-pumpkin_s_ratio*constraint_l[1,2]
    results[7, 2]<-tomato_s_ratio*constraint_l[1,2]
    results[8, 2]<-pepper_s_ratio*constraint_l[1,2]
    
  }
  
  colnames(results)<-c("realized", "simulated")
  
  
  #results
  result_water<-results*resource_use[,3]/resource_use[,2]
  #result_water
  
  Net_matrix<-cbind(price[,2]*yield[,2]-(cost[,2]+energy_cost[,2]+curtain[,2]), 
                    price_s[,2]*yield_s[,2]-(cost_s[,2]+energy_cost_s[,2]+curtain_s[,2]))
  
  result_income<-as.data.frame(matrix(0, nrow(results), ncol(results)))
  for ( i in 1:ncol(results)) {
    result_income[,i]<-results[,i]*Net_matrix[,i]
  }
  rownames(result_income)<-rownames(result_water)
  colnames(result_income)<-colnames(result_water)
  
  results<-rbind(results, c(0, rice_ratio*constraint[1,2]))
  results<-rbind(results, c(0, rice_o_ratio*constraint[1,2]))
  
  results<-rbind(results,colSums(results))
  result_water<-rbind(result_water,colSums(result_water))
  result_income<-rbind(result_income,colSums(result_income))
  
  rownames(result_water)<-rownames(result_income)<-c("strawberry",   "pumpkin",   "tomato",   "pepper", 
                                                     "strawberry_w", "pumpkin_w", "tomato_w", "pepper_w", "total")
  
  rownames(results)<-c("strawberry",   "pumpkin",   "tomato",   "pepper", 
                       "strawberry_w", "pumpkin_w", "tomato_w", "pepper_w", "rice", "rice_organic", "total")                                                                     
  
  #return
  return(list(land=results, water=result_water, income=result_income))
  
}


#' @get /hello
#' @html
function(){
  "<html><h1>r script api</h1></html>"
}

#' Echo the parameter that was sent in
#' @param msg The message to echo back.
#' @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#' Plot out data from the iris dataset
#' @param spec If provided, filter the data to only this species (e.g. 'setosa')
#' @get /plot
#' @png
function(spec){
  myData <- iris
  title <- "All Species"

  # Filter if the species was specified
  if (!missing(spec)){
    title <- paste0("Only the '", spec, "' Species")
    myData <- subset(iris, Species == spec)
  }

  plot(myData$Sepal.Length, myData$Petal.Length,
       main=title, xlab="Sepal Length", ylab="Petal Length")
}
