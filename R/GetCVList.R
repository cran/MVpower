`GetCVList` <-
function(RandomPartParams, statevec){

PercentOut <- RandomPartParams[1]
NumRandomFold <- RandomPartParams[2]

NumInState <- table(statevec)
MinNum <- min(NumInState, na.rm=T)

#NumOut <- floor(PercentOut*NumInState)
NumOut <- round(PercentOut*NumInState)

MinNumOut <- min(NumOut)

NumFolds <- min(floor(NumInState/NumOut))


u.states <- sort(unique(statevec))
ListOfIds <- vector("list", length(u.states))

for (i in 1:length(u.states)){
ListOfIds[[i]] <- which(statevec == u.states[i])}


CVList.L <- vector("list", NumFolds)

for (i in 1:length(u.states)){

StateI.Ids <- sample(ListOfIds[[i]], size=length(ListOfIds[[i]]), replace=F)
StateI.NumOut <- NumOut[i]

for (j in 1:(NumFolds-1)){
index <- seq((StateI.NumOut*j - (StateI.NumOut-1)),StateI.NumOut*j)
CVList.L[[j]] <- c(CVList.L[[j]], StateI.Ids[index])
}

lastindex <- seq(StateI.NumOut*(NumFolds-1)+1,length(StateI.Ids))
CVList.L[[NumFolds]] <- c(CVList.L[[NumFolds]], StateI.Ids[lastindex])
}

### Next --(1) get max length of out ids, make CVList.L into matrix

NumIdsPerFold <-  unlist(lapply(CVList.L, myfun <- function(vec){return(length(vec))}))
max.NumIdsPerFold <- max(NumIdsPerFold)

CVlist <- matrix(NA, NumFolds, max.NumIdsPerFold)

for (i in 1:NumFolds){
ind <- CVList.L[[i]]
CVlist[i,1:length(ind)] <- ind}

return(CVlist)

}

