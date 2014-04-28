# Function to get the maximum number of allowed failured given a test length,
# a reliability requirement, and a confidenc level
MaxFailFun <- function(TL=500, RR=30, CL=0.8, AFList=seq(0,49,1)){
  if(TL<=0){
    stop("Test length can't be equal to or less than zero")
  }
  if(CL<=0){
    stop("Confidence level can't be equal to or less than zero")
  }  
  pos     <- exp(-TL/RR)*(TL/RR)^AFList/factorial(AFList)
  MaxFail <- tail(AFList[cumsum(pos)<=(1-CL)], 1)  
  return(MaxFail)
}