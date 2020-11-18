#from Handl, A., & Kuhlenkasper, T. (2017). Multivariate Analysemethoden. https://doi.org/10.1007/978-3-662-54754-0 (Kapitel 13 - Clusteranalyse)
gammakoeffizient <- function(v1, v2){
  m1 <- outer(v1, v1, FUN = "<")
  m1 <- m1[lower.tri(m1)]
  m2 <- outer(v2, v2, FUN = "<")
  m2 <- m2[lower.tri(m2)]
  m3 <- outer(v1, v1, FUN = ">")
  m3 <- m3[lower.tri(m3)]
  m4 <- outer(v2, v2, FUN = ">")
  m4 <- m4[lower.tri(m4)]
  C <- sum((m1 + m2) == 2)
  C <- C + sum((m3 + m4) == 2)
  D <- sum((m1 + m4) == 2)
  D <- D + sum((m2 + m3) == 2)
  (C - D)/(C + D)
}