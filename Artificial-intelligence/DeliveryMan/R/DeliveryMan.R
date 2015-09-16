dumbDM=function(roads,car,packages){
  car$nextMove=sample(c(2,4,6,8),1)
  return (car)
}
basicDM=function(roads,car,packages) {
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) {
    toGo=which(packages[,5]==0)[1]
  } else {
    toGo=car$load  
    offset=2
  }
  if (car$x<packages[toGo,1+offset]) {nextMove=6}
  else if (car$x>packages[toGo,1+offset]) {nextMove=4}
  else if (car$y<packages[toGo,2+offset]) {nextMove=8}
  else if (car$y>packages[toGo,2+offset]) {nextMove=2}
  else {nextMove=5}
  car$nextMove=nextMove
  car$mem=list()
  return (car)
}
manualDM=function(roads,car,packages) {
  if (car$load>0) {
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }  
  car$nextMove=readline("Enter next move. Valid moves are 2,4,6,8,0 (directions as on keypad) or q for quit.")
  if (car$nextMove=="q") {stop("Game terminated on user request.")}
  return (car)
}

#AV TIM
# A very basic least-cost test where the car will always take the road
# with the least cost.
basicATest=function(roads,car,packages) {
  print(packages)
  up = roads$vroads[car$y+1,car$x]
  right = roads$vroads[car$y,car$x+1]
  print(paste("cost up:",up))
  print(paste("cost right:",right))
  if (up >= right){
    car$nextMove = 6
    print("MOVED RIGHT")
    print("-------------------")
    
  } else {
    car$nextMove = 8
    print("MOVED UP")
    print("-------------------")
  }
  return (car)
}

testFunction=function(roads,car,packages){
  findPackagePath(packages,car)
}

#AV TIM
findPackagePath=function(packages){
  route = c()
  cost=c()
  currentx = 1
  currenty = 1
  packages <- cbind(packages, 0)
  packages[,5] = (abs(currentx-packages[,1])+abs(currenty-packages[,2]))
  packages[,6] = (abs(packages[,1]-packages[,3])+abs(packages[,2]-packages[,4]))
  next_route = 0
  for(i in 1:5){
    copy = packages
    currentcost = copy[i,5]+copy[i,6]
    route = append(route, i)
    copy[i,1] = NA
    currentx = packages[i,3]
    currenty = packages[i,4]
    for(j in 1:4){
      #print(copy)
      copy[,5] <- (abs(currentx-copy[,1])+abs(currenty-copy[,2]))
      min = min(copy[,5], na.rm=TRUE)
      next_route = which(copy[,5] == min)[1]
      route = append(route,next_route)
      copy[next_route,1] = NA
      currentx = copy[next_route,3]
      currenty = copy[next_route,4]
      currentcost = currentcost + copy[next_route,5] + copy[next_route,6]
      #print(currentx)
      #print(currenty)
    }
    cost = append(cost,currentcost)
  }
  leastCost = which.min(cost)[1]
  optimalRoute = c()
  for(i in 1:5){
    optimalRoute = append(optimalRoute, route[i+((leastCost-1)*5)])
  }
  return(optimalRoute)
}

choosePackage=function(route,packages){
  if(packages[route[1],5] == 0){
    return (route[1])
  } else if(packages[route[2],5] == 0){
    return (route[2])
  } else if(packages[route[3],5] == 0){
    return (route[3])
  } else if(packages[route[4],5] == 0){
    return (route[4])
  } else if(packages[route[5],5] == 0){
    return (route[5])
  }
}

#AV TIM
#The total distance needed for every packet
packetDistance=function(roads,car,packages) {
  distance = abs(packages[,1]-packages[,2])+abs(packages[,3]-packages[,4])-packages[,5]
  return (distance)
}


#AV TIM
#Choosing which path to take
calculateNextMove=function(car,destX,destY) {
  print(destX)
  print(destY)
  if (car$x < destX){
    return (6)
  }
  if (car$x > destX){
    return (4)
  }
  if (car$y < destY){
    return (8)
  }
  if (car$y > destY){
    return (2)
  }
}

# AV TIM
# Finding the closest package
closestPackage=function(roads,car,packages) {
  if (car$load == 0) {
    distanceForPackage = abs(packages[,1]-packages[,3])+abs(packages[,2]-packages[,4])
    distanceForCar = abs(car$x-packages[,1])+abs(car$y-packages[,2])
    #totalDistance = distanceForPackage + distanceForCar
    totalDistance = distanceForCar
    #distance = packetDistance(packages)
    goal=which(totalDistance==min(totalDistance,na.rm=TRUE))
    car$mem.xdest=packages[goal,1]
    car$mem.ydest=packages[goal,2]
    
    print(totalDistance)
    print(paste("Pickup Location: X",packages[goal,1],"Y",packages[goal,2]))
  }
  if (car$load>0) {
    car$mem.xdest=packages[car$load,3]
    car$mem.ydest=packages[car$load,4]
    print(paste("Current load:",car$load))
    print(paste("Destination: X",packages[car$load,3],"Y",packages[car$load,4]))
  }  
  car$nextMove=calculateNextMove(car,car$mem.xdest,car$mem.ydest)
  return (car)
}

#AV ANNA
bruteForcePackageOrder=function(packages){
  toBeSearched = c(1:length(packages[1, ]))
  bruteForcePackageOrderHelpis(-1, toBeSearched, packages)
}

#AV ANNA
#Distance from A deliver to B pick up
distanceFromAtoB=function(a, b, packages){
  if (a == -1){
    diffInX = abs(1 - packages[b, 1])
    diffInY = abs(1 - packages[b, 2])
  } else {
    diffInX = abs(packages[a, 3] - packages[b, 1])
    diffInY = abs(packages[a, 4] - packages[b, 2])
  }
  return(diffInX + diffInY)
}

#AV ANNA
bruteForcePackageOrderHelpis=function(currentNode,toBeSearched,packages){
  if(length(toBeSearched) != 0){
    minScore = list(s=-1, n=NULL)
    for(i in 1:(length(toBeSearched))){
      futureScoreAndNode = bruteForcePackageOrderHelpis(i, toBeSearched[-i], packages)

      score = distanceFromAtoB(currentNode, toBeSearched[i], packages) + 
        distanceFromAtoB(toBeSearched[i], toBeSearched[i], packages) +
        futureScoreAndNode$s
        
      if((minScore$s == -1) || (minScore$s > score)){
        minScore$s = score
        minScore$n = append(toBeSearched[i], futureScoreAndNode$n)
      }
    }
    return(minScore)
  }
  return(list(s=0, n=NULL))
}

#AV TIM, ANNA och LINUS
createFrontiers=function(){
  frontiers = list(xcord=list(),ycord=list(),cost=list())
  return (frontiers)
}

#AV TIM, ANNA och LINUS
isEmptyFrontiers=function(frontiers){
  return (length(frontiers$xcord)!=0)
}

#AV TIM, ANNA och LINUS
popFrontiers=function(frontiers){
  index = which.min(frontiers$cost)
  value = list(xcord=frontiers$xcord[[index]],ycord=frontiers$ycord[[index]],
               cost=frontiers$cost[[index]],front=NULL)
  frontiers$xcord[[index]] <- NULL
  frontiers$ycord[[index]] <- NULL
  frontiers$cost[[index]] <- NULL
  value$front = frontiers
  return (value)
}

#AV TIM, ANNA och LINUS
pushFrontiers=function(frontiers,newxcord,newycord,newcost){
  frontiers$xcord = append(frontiers$xcord,newxcord)
  frontiers$ycord = append(frontiers$ycord,newycord)
  frontiers$cost = append(frontiers$cost,newcost)
  return (frontiers)
}

#AV TIM, ANNA och LINUS
simplePathfinding=function(roads,car,xdest,ydest,dim){
  hArray=findHeuristics(xdest,ydest,dim)
  frontiers = createFrontiers()
  frontiers = pushFrontiers(frontiers,car$x,car$y,hArray[car$x,car$y])
  visited = matrix(0,nrow=dim,ncol=dim)
  moves = matrix(0,nrow=dim,ncol=dim)
  while(isEmptyFrontiers(frontiers)){
    value = popFrontiers(frontiers)
    frontiers = value$front
    if((value$xcord == xdest) && (value$ycord == ydest)){
      break
    }else {
      if(value$ycord<dim) {
        if(visited[value$xcord,value$ycord+1]==0){
          frontiers = pushFrontiers(frontiers,value$xcord,value$ycord+1,
                                    (hArray[value$xcord,value$ycord+1]+roads$vroads[value$ycord,value$xcord]))
          moves[value$xcord,value$ycord+1]=8
        }
      }
      if(value$ycord>1) { 
        if(visited[value$xcord,value$ycord-1]==0){
          frontiers = pushFrontiers(frontiers,value$xcord,value$ycord-1,
                                    (hArray[value$xcord,value$ycord-1]+roads$vroads[value$ycord-1,value$xcord]))
          moves[value$xcord,value$ycord-1]=2
        } 
      }
      if(value$xcord<dim) { 
        if(visited[value$xcord+1,value$ycord]==0){
          frontiers = pushFrontiers(frontiers,value$xcord+1,value$ycord,
                                    (hArray[value$xcord+1,value$ycord]+roads$hroads[value$ycord,value$xcord]))
          moves[value$xcord+1,value$ycord]=6
        } 
      }
      if(value$xcord>1) { 
        if(visited[value$xcord-1,value$ycord]==0){
          frontiers = pushFrontiers(frontiers,value$xcord-1,value$ycord,
                                    (hArray[value$xcord-1,value$ycord]+roads$hroads[value$ycord,value$xcord-1]))
          moves[value$xcord-1,value$ycord]=4
        } 
      }
      visited[value$xcord,value$ycord]=1 #Siffra för hur vi kom till denna nod! FIXED?
    }
  }
  return (moves)
}

#AV TIM
findPath=function(xdest,ydest,moves){
  x=xdest
  y=ydest
  last=0
  flag = TRUE
  while(flag){
    current = moves[x,y]
    #print("current")
    #print(current)
    if(is.null(current) | current == 0){
      flag = FALSE
      current=10
    } else {
      if(current == 8){
        last = 8
        y=y-1
        
      }
      if(current == 2){
        last = 2
        y=y+1
      }
      if(current==6){
        last = 6
        x=x-1
      }
      if(current==4){
        last = 4
        x=x+1
      }
    }
    
  }
  #print(last)
  return (last)
}

#AV TIM, ANNA och LINUS
findHeuristics=function(xdest,ydest,d) {
  hArray<-array(0,dim = c(d,d))
  for(x in 1:d){
    for(y in 1:d){
      hArray[x,y]=(abs(x-xdest)+abs(y-ydest))*3
    }
  }
  #print(hArray)
  return (hArray)
}

#AV TIM
simpleTest=function(roads,car,packages) {
  dim = car$mem$size
  if(car$load==0){
    #distanceForPackage = abs(packages[,1]-packages[,3])+abs(packages[,2]-packages[,4])
    #distanceForCar = abs(car$x-packages[,1])+abs(car$y-packages[,2])
    #totalDistance = distanceForCar + distanceForPackage
    #goal=which(totalDistance==min(totalDistance,na.rm=TRUE))[1]
    goal = choosePackage(car$mem$route, packages)
    xdest=packages[goal,1]
    ydest=packages[goal,2]
    moves = simplePathfinding(roads,car,xdest,ydest,dim)
    nextMove = findPath(xdest,ydest,moves)
  } else if(car$load > 0){
    xdest=packages[car$load,3]
    ydest=packages[car$load,4]
    moves = simplePathfinding(roads,car,xdest,ydest,dim)
    nextMove = findPath(xdest,ydest,moves)
  }
  #print(nextMove)
  car$nextMove=nextMove
  return (car)
}

# AV TIM
benchmark <- function (runs){
  Teachers = 0
  Ours = 0
  for(i in 1:runs){
    rand <- sample(1:100000,1)
    print(rand)
    set.seed(rand)
    Teachers = Teachers + runDeliveryMan(carReady = basicDM, dim = 10, turns = 2000, pause = 0,del = 5)
    set.seed(rand)
    Ours = Ours + runDeliveryMan(carReady = simpleTest, dim = 10, turns = 2000, pause = 0,del = 5)
  }
  OurAvg = Ours/runs
  TeacherAvg = Teachers/runs
  howMuchBetter = (TeacherAvg-OurAvg)/TeacherAvg
  print(paste("Our average is:", OurAvg))
  print(paste("Teachers average is:", TeacherAvg))
  print(paste("We are this much better (percent): ", (howMuchBetter*100)))
}

# AV TIM
benchmarkIntervall <- function (start,end){
  Teachers = 0
  Ours = 0
  for(i in start:end){
    set.seed(i)
    Teachers = Teachers + runDeliveryMan(carReady = basicDM, dim = 10, turns = 2000, pause = 0,del = 5)
    set.seed(i)
    Ours = Ours + runDeliveryMan(carReady = simpleTest, dim = 10, turns = 2000, pause = 0,del = 5)
  }
  OurAvg = Ours/(end-start)
  TeacherAvg = Teachers/(end-start)
  howMuchBetter = (TeacherAvg-OurAvg)/TeacherAvg
  print(paste("Our average is:", OurAvg))
  print(paste("Teachers average is:", TeacherAvg))
  print(paste("We are this much better (percent): ", (howMuchBetter*100)))
}

seed <- function(seed){
  set.seed(seed)
  Teachers = runDeliveryMan(carReady = basicDM, dim = 10, turns = 2000, pause = 0,del = 5) 
  set.seed(seed)
  Ours = runDeliveryMan(carReady = simpleTest, dim = 10, turns = 2000, pause = 0,del = 5)
  print(paste("Our turns: ",Ours))
  print(paste("Teachers turns: ",Teachers))
  }
#' Run Delivery Man
#' 
#' Runs the delivery man game. In this game, deliveries are randomly placed on a city grid. You
#' must pick up and deliver the deliveries as fast as possible under changing traffic conditions.
#' Your score is the time it takes for you to complete this task. To play manually pass manualDM
#' as the carReady function and enter the number pad direction numbers to make moves.
#' @param carReady Your function that takes three arguments: (1) a list of two matrices giving the 
#' traffice conditions. The first matrix is named 'hroads' and gives a matrix of traffice conditions
#' on the horizontal roads. The second matrix is named 'vroads' and gives a matrix of traffic 
#' conditional on the vertical roads. (2) a list providing information about your car. This
#' list includes the x and y coordinates of the car with names 'x' and 'y', the package the car 
#' is carrying, with name 'load' (this is 0 if no package is being carried), a list called
#' 'mem' that you can use to store information you want to remember from turn to turn, and
#' a field called nextMove where you will write what you want the car to do. Moves are 
#' specified as on the number-pad (2 down, 4 left, 6 right, 8 up, 5 stay still). (3) A
#' matrix containing information about the packages. This contains five columns and a row for each
#' package. The first two columns give x and y coordinates about where the package should be picked
#' up from. The next two columns give x and y coordinates about where the package should be 
#' delivered to. The final column specifies the package status (0 is not picked up, 1 is picked up but not delivered, 2 is delivered).
#' Your function should return the car object with the nextMove specified.
#' @param dim The dimension of the board. You will be scored on a board of dimension 10.
#' @param turns The number of turns the game should go for if deliveries are not made. Ignore this 
#' except for noting that the default is 2000 so if you have not made deliveries after 2000 turns
#' you fail.
#' @param pause The pause period between moves. Ignore this.
#' @param del The number of deliveries. You will be scored on a board with 5 deliveries.
#' @return A string describing the outcome of the game.
#' @export
runDeliveryMan <- function (carReady=manualDM,dim=10,turns=2000,pause=0.1,del=5) {
  roads=makeRoadMatrices(dim)
  packages=matrix(sample(1:dim,replace=T,5*del),ncol=5)
  packages[,5]=rep(0,del)
  car=list(x=1,y=1,wait=0,load=0,nextMove=NA,mem=list(xdest=0,ydest=0,size=dim,route=findPackagePath(packages)))
  #AV ANNA OBS SKA BORT!!!!!!!!!!!!!!!!!!!!!!
  #return (packages)
  #bruteForcePackageOrder(packages)  
  #------------------------------------------
  for (i in 1:turns) {
    #makeDotGrid(dim,i) 
    roads=updateRoads(roads$hroads,roads$vroads)
    #plotRoads(roads$hroads,roads$vroads) 
    #points(car$x,car$y,pch=16,col="blue",cex=3)  
    #plotPackages(packages)
    if (car$wait==0) {
      if (car$load==0) {
        on=packageOn(car$x,car$y,packages)
        if (on!=0) {
          packages[on,5]=1
          car$load=on
        }
      } else if (packages[car$load,3]==car$x && packages[car$load,4]==car$y) {
        packages[car$load,5]=2
        packages[car$load,1]=NA #Tillagt av TIM
        car$load=0
        if (sum(packages[,5])==2*nrow(packages)) {
          #print (paste("Congratulations! You suceeded in",i,"turns!"))
          return (i)
        }
      }      
      car=carReady(roads,car,packages)
      car=processNextMove(car,roads,dim)
    } else {
      car$wait=car$wait-1
    }
    Sys.sleep(pause)
  }
  print (paste("You failed to complete the task. Try again."))
  return (NA)
}
packageOn<-function(x,y,packages){
  notpickedup=which(packages[,5]==0)
  onX=which(packages[,1]==x)
  onY=which(packages[,2]==y)
  available=intersect(notpickedup,intersect(onX,onY))
  if (length(available)!=0) {
    return (available[1])
  } 
  return (0)
}
processNextMove<-function(car,roads,dim) {
  nextMove=car$nextMove
  if (nextMove==8) {
    if (car$y!=dim) {
      car$wait=roads$vroads[car$y,car$x]
      car$y=car$y+1
    } else {
      warning(paste("Cannot move up from y-position",car$y))
    }
  } else if (nextMove==2) {
    if (car$y!=1) {
      car$y=car$y-1
      car$wait=roads$vroads[car$y,car$x]
    } else {
      warning(paste("Cannot move down from y-position",car$y))
    }
  }  else if (nextMove==4) {
    if (car$x!=1) {
      car$x=car$x-1
      car$wait=roads$hroads[car$y,car$x]
    } else {
      warning(paste("Cannot move left from x-position",car$x))
    }
  }  else if (nextMove==6) {
    if (car$x!=dim) {
      car$wait=roads$hroads[car$y,car$x]
      car$x=car$x+1
    } else {
      warning(paste("Cannot move right from x-position",car$x))
    }
  } else if (nextMove!=5) {
    warning("Invalid move. No move made. Use 5 for deliberate no move.")    
  }
  car$nextMove=NA
  return (car)
} 

plotPackages=function(packages) {
  notpickedup=which(packages[,5]==0) 
  notdelivered=which(packages[,5]!=2)
  points(packages[notpickedup,1],packages[notpickedup,2],col="green",pch=18,cex=3)
  points(packages[notdelivered,3],packages[notdelivered,4],col="red",pch=18,cex=3)
}

makeRoadGrid<-function() {
  
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}

makeRoadGrid<-function() {
  out=matrix(rep("S",51*51),ncol=51)
  out[26,]=rep("H",51)
  out[,26]=rep("H",51)
}
#' @export
makeDotGrid<-function(n,i) {
  plot(rep(seq(1,n),each=n),rep(seq(1,n),n),xlab="X",ylab="Y",main=paste("Delivery Man. Turn ", i,".",sep=""))
}

#' @export
makeRoadMatrices<-function(n){
  hroads=matrix(rep(1,n*(n-1)),nrow=n)
  vroads=matrix(rep(1,(n-1)*n),nrow=n-1)
  list(hroads=hroads,vroads=vroads)
}

#' @export
plotRoads<- function (hroads,vroads) {
  for (row in 1:nrow(hroads)) {
    for (col in 1:ncol(hroads)) {
      lines(c(col,col+1),c(row,row),col=hroads[row,col])
    }
  }
  for (row in 1:nrow(vroads)) {
    for (col in 1:ncol(vroads)) {
      lines(c(col,col),c(row,row+1),col=vroads[row,col])
    }
  }
}

#' @export
updateRoads<-function(hroads,vroads) {
  r1=runif(length(hroads))
  r2=runif(length(hroads))
  for (i in 1:length(hroads)) {
    h=hroads[i]
    if (h==1) {
      if (r1[i]<.05) {
        hroads[i]=2
      }
    }
    else {
      if (r1[i]<.05) {
        hroads[i]=h-1
      } else if (r1[i]<.1) {
        hroads[i]=h+1
      }
    }
    v=vroads[i]
    if (v==1) {
      if (r2[i]<.05) {
        vroads[i]=2
      }
    }
    else {
      if (r2[i]<.05) {
        vroads[i]=v-1
      } else if (r2[i]<.1) {
        vroads[i]=v+1
      }
    }    
  }
  list (hroads=hroads,vroads=vroads)
}
