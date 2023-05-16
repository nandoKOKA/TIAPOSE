repair <- function(s) {
  s = round(s)
  
  arm <- s[1:7]
  v1 <- s[8:14]
  v2 <- s[15:21]
  v3 <- s[22:28]
  pac_stella <- s[29:35]
  pac_bud <- s[36:42]
  
  for (i in 1:7) {
    ## tratar veículos e bebidas empacotadas
    
    if (pac_stella[i] > (arm[i] * max_pac_rec) ||
        pac_bud[i] > (arm[i] * max_pac_rec))
    {
      pac_stella[i] <- pac_stella[i] / max_pac_rec
      pac_bud[i] <- pac_bud[i] / max_pac_rec
    } 
    
    if ((pac_stella[i] + pac_bud[i]) > ((v1[i] * max_dist_v1) + (v2[i] * max_dist_v2) + (v3[i] * max_dist_v3))) {
      resto <- (pac_stella[i] + pac_bud[i]) - ((v1[i] * max_dist_v1) + (v2[i] * max_dist_v2) + (v3[i] * max_dist_v3))
      
      while (resto > 0) {
        if (resto < 60) {
          v1[i] <- v1[i] + 1
          resto <- resto - 60
        } else if (resto < 90) {
          v2[i] <- v2[i] + 1
          resto <- resto - 90
        } else if (resto < 120) {
          v3[i] <- v3[i] + 1
          resto <- resto - 120
        } else {
          v3[i] <- v3[i] + 1
          resto <- resto - 120
        }
      }
    }
  }
  
  s1 <- c(arm, v1, v2, v3, pac_stella, pac_bud)
  return(s1)
}

arm=c(6, 3, 0, 1, 1, 0, 1)
v1 <- c(2, 0, 1, 0, 2, 0, 0)
v2 <- c(1, 0, 0, 1, 0, 0, 0)
v3 <- c(0, 0, 1, 0, 0, 0, 0)
pac_stella <- c(100, 0, 45, 66, 0, 0, 0)
pac_bud <- c(200, 0, 0, 52, 0, 78, 0)

s1 <- c(arm, v1, v2, v3, pac_stella, pac_bud)

max_pac_rec <- 72
max_dist_v1 <- 60
max_dist_v2 <- 90
max_dist_v3 <- 120

s2 <- round(repair(s1))

#Imprimir ultimo plano
cat("arm:", s1[1:7], "\nv1:", s1[8:14], "\nv2:", s1[15:21], "\nv3:", s1[22:28], "\npac_stella:", s1[29:35], "\npac_bud:", s1[36:42])

cat("\n\n")
#Imprimir ultimo plano
cat("arm:", s2[1:7], "\nv1:", s2[8:14], "\nv2:", s2[15:21], "\nv3:", s2[22:28], "\npac_stella:", s2[29:35], "\npac_bud:", s2[36:42])

