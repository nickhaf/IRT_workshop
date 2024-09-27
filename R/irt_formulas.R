calc_pcm <- function(theta, x, delta){

  m <- length(delta)

  zaehler <- exp(sum(

    sapply(0:x, function(j){

      if(j == 0){
        diff_td <- 0
      }else{
        diff_td <- theta-delta[j]
      }
      return(diff_td)
    }
    )

  ))

  nenner <- sum(sapply(0:m, function(r){
    exp(sum(sapply(0:r, function(j){
      if(j == 0){
        diff_td <- 0
      }else{
        diff_td <- theta-delta[j]
      }
      return(diff_td)
    })))
  }))

  return(zaehler/nenner)

}
