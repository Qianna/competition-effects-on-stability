# Function for calculating temporal stability and asynchrony, and decomposing asynchrony into CPE and SAE
# Modified from coded used in Zhao et al. 2022 Nature Communications
# X should be abundance arranged by time * species


decomposition <- function(X){
  
  X1 <- X[,colSums(X, na.rm=T)>0]  #remove sp not showing
  #if all sp. in this year are NAs, delete this year from tot 
  i.not.na <- !(apply(X1, 1, function(xx) all(is.na(xx))))
  X2 <- X1[i.not.na,]
  
  
  tot <- rowSums(X2,na.rm=T)
  mu <- mean(tot)
  stab.comm <- mu / sd(tot, na.rm=T)       #the reverse of CVcom
  p <- colSums(X2,na.rm=T) / sum(tot)
  alpha.inv.simpson <- 1/sum(p^2)      #Simpson
  alpha.shannon <- -sum(p * log(p))  #Shannon
  nn <- apply(X2, 1, function(xx) sum(xx>0))
  richness <- mean(nn)  #species richness
  evenness.shannon <- alpha.shannon / log(length(p))  #evenness of the distribution of density across sp.
  
  stab.sp <- mu / sum(apply(X2, 2, sd, na.rm=T))   #the reverse of CVpop
  asyn <- stab.comm / stab.sp      #based on LdM
  
  compensatory <- sqrt(sum(apply(X2, 2, var, na.rm=T)) / var(tot))
  statistical <- sum(apply(X2, 2, sd, na.rm=T), na.rm=T) / sqrt(sum(apply(X2, 2, var, na.rm=T), na.rm=T))
  
  ### asyn = compensatory * statistical
  
  
  return(c(stab.comm=stab.comm,  stab.pop=stab.sp, asyn=asyn, 
           compensatory=compensatory, statistical=statistical,
           richness=richness, shannon=alpha.shannon,
           inv.simpson=alpha.inv.simpson, evenness=evenness.shannon))
}
