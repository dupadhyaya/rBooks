myfunc <-
function(x) {
  tmp = seq_along(x)
  for (i in 1:length(tmp)) tmp[i] = median(x[1:i])
  print(tmp)
}
