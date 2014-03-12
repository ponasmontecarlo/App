# tas pats, kas normaliuoju atveju. tik paduodam realizacijas su parinktu 
# degrees of freedom skaihiumi
# input: a.d. realizacijos, reikpmk
# output: aproksimuota tikimybk

tUnivariate <- function(X,t) {
  mean(X<t)
}

# skaiciuoja dispersija pagal apibreziam
# input: realizacijx skaihius, a.d. realizacijos, tikrosios reikpmks, averhiai
# output: dispersijos reikpmks

tUnivariateVar <- function(n,X,t,est) {
  return((sum(((X<t)*1-est)^2))/(n^2))
}

# suskaiciuoja daugiamciu atveju
# input: a.d. realizaciju matrica, reiksmiu vektorius ir realizaciju sk
# output: tikimybes ivertis

tMultivariate <- function(X,t,n){
  return(sum(apply(X<unlist(t),1,all))/n)
}