standard01 <-
function(score, direction = "-1+1to01") {
  # version 1.0 (18 June 2013)
  # standardizes a value between -1 and 1 to its corresponding value in the 0-1 scale
  if (direction == "-1+1to01") {
    score <- score + ((1 - score) / 2)
  }  # end if -1+1to01
  else stop ("The 'direction' you chose is not implemented; check for misspelling.")
  return(score)
}
