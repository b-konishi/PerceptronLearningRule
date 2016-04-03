# A program of Perceptron Learning Rule
# 2016/4/3
frame()

cols <- c('red', 'blue')

weight <- c(0, 0)
new_weight <- weight
learning <- 0.5
count <- 0

X <- NULL

## Input for CommandLine
#b <- NULL
#repeat {
#  cat('Please input information.\nEx) Position,Class(1 or 2) \n')
#  input <- readline()
#  if (input == '')  break
#  input <- as.numeric(unlist(strsplit(input,',')))
#  X <- cbind(X, c(1, input[1]))
#  b <- cbind(b,input[length(input)])
#}

## Input for Text-File
input <- matrix(scan('./data.txt', sep=','), 2)
X <- rbind(rep(1,ncol(input)), input[1,])
b <- input[2,]

repeat {
  cat('Waiting...\n')
  for (i in 1:ncol(X)) {
    judge <- t(weight) %*% X[,i]
    cat(paste('This is Class', b[i], '\n'))

    if (judge >= 0 && b[i] == 2) {
      cat('Worse! This is Class2\n')
      new_weight <- weight - (learning * X[,i])
    } else if (judge <= 0 && b[i] == 1) {
      cat('Worse! This is Class1\n')
      new_weight <- weight + (learning * X[,i])
    } else {
      cat('Match!\n')
      count <- count + 1
    }

    max <- apply(cbind(weight,new_weight),1,max)
    min <- apply(cbind(weight,new_weight),1,min)
    print(cbind(weight,new_weight))
    # plot(c(weight[2],new_weight[2]),c(weight[1],new_weight[1]),
    #     pch=16,col=cols,cex=2,xlim=c(min[2]-3,max[2]+3), ylim=c(min[1]-3,max[1]+3))
    plot(c(weight[2],new_weight[2]),c(weight[1],new_weight[1]),
         pch=16,col=cols,cex=2,xlim=c(-5,5), ylim=c(-5,5))
    title(main=paste('Class', b[i], ': Weight Space'))
    legend('topleft', legend=c('old', 'new'), col=cols, pch=16)
    abline(0, -X[2,i])
    readline()

    weight <- new_weight
  }
  if (count == ncol(X)) {
    cat('Finish!\nweight: ')
    print(weight)
    break
  } 
  count <- 0
}

plot(X[2,], rep(0,ncol(X)), pch=16, col=cols[b], cex=2,
     xlim=c(min(X[2,]), max(X[2,])), ylim=c(-1,1))
points(X[2,], rep(0,ncol(X)), pch=16, col=cols[b], cex=2)
title(main='Result')
legend('topleft', legend=c('Class1','Class2'), col=cols, pch=16)
abline(weight[1], weight[2])


