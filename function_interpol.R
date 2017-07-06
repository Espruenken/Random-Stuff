#' A Linear Interpolation Function for numeric vectors
#'
#' The function detects NA's and 0's in the specified argument and does linear interpolation
#' between the last known numeric value bigger than 0 and the next known numeric value bigger than 0
#' @param y A numeric vector
#' @keywords interpolation, linear, vector, data, na, 0, null, interpolate
#' @export
#' @examples
#' interpolate()

interpolate = function(y){
  b =  deparse(substitute(y))
  name = (paste0(b,"_est"))
  temp = which(y == 0)
  for (i in 1:length(temp)){
    t = temp[i]
    y[t] = NA
  }
  
  if (!(0 %in% y)){
   x = which(is.na(y))  
  #b =  deparse(substitute(y))
  #name = (paste0(b,"_est"))
  for (i in 1:length(x)){
    a = x[i]
    e = 1
    while (is.na(y[a+e])) {
      e = e + 1
    }
    if (!is.na(y[a+e])){
      y[a] = y[a-1] + ((y[a+e] - y[a-1])/(e+1))
    }
  }
  result = y
  mat.3 = as.matrix(result)
  assign(name,mat.3, envir = globalenv())}
}
