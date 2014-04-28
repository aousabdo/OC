MaxX <- function(AF, test, max=70, tolerance=1e-2, MaxValue=0.99){
  repeat {
    value <- ppois(AF, test/max)
    if(abs(value-MaxValue) < tolerance){
      break
    }
    else{
      max <- max +1
    }
  }
  return(max)
}

