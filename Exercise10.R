
## initial values and parameters
N0=99
M0=1
rN=0.1
rM=0.1
K=1000000
timesteps=700

## set treatment growth rates
rNt=-.1
rMt=.05

## set when treatment begins
treatment_time=250

## create vectors
Nt=numeric(length=timesteps)
Nt[1]=N0
Mt=numeric(length=timesteps)
Mt[1]=M0
Total=numeric(length=timesteps)

## simulation loop
for(t in 1:(timesteps-1)){
  if (t<treatment_time){
  Nt[t+1] <- Nt[t]+rN*Nt[t]*(1-(Nt[t]+Mt[t])/K)
  Mt[t+1] <- Mt[t]+rM*Mt[t]*(1-(Nt[t]+Mt[t])/K)}else{
    Nt[t+1] <- Nt[t]+rNt*Nt[t]*(1-(Nt[t]+Mt[t])/K)
    Mt[t+1] <- Mt[t]+rMt*Mt[t]*(1-(Nt[t]+Mt[t])/K)
  }
}

# plot simulation
library(ggplot2)
sim<-data.frame(time=1:length(Nt), Nt, Mt)
ggplot(data=sim,aes(x=time))+geom_line(aes(y=Mt, colour="Mutant"))+geom_line(aes(y=Nt, colour="Nonmutant"))+theme_classic()