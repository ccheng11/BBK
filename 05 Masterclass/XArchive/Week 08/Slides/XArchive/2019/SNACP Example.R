##################################################
## Social Network Analysis in Comparative Politics
## Chao-Yo Cheng
## Dec 3, 2019
##################################################

##### One-mode projection #####
B = matrix(NA, 4, 5)
B[1,] = c(1,0,0,1,0)
B[2,] = c(1,1,1,1,0)
B[3,] = c(0,1,1,0,1)
B[4,] = c(0,0,1,1,1)

proj <- t(B) %*% B
diag(proj) 

##### Example of simple graph #####
A <- matrix(NA, 6, 6)
A[,1] <- c(0,1,0,1,1,1)
A[,2] <- c(1,0,0,0,1,0)
A[,3] <- c(0,0,0,1,0,1)
A[,4] <- c(1,0,1,0,0,0)
A[,5] <- c(1,1,0,0,0,0)
A[,6] <- c(1,0,1,0,0,0)

colnames(A) <- 1:6
rownames(A) <- 1:6

dim(A) # 6x6
table(as.numeric(A)) # 0 or 1 (binary ties)
diag(A) # no self-edges
isSymmetric(A) # symmetric

library(igraph)
Anet <- graph.adjacency(A, mode="undirected")
V(Anet)$name <- 1:6
plot.igraph(Anet, vertex.size=40)

##### Degree #####
hist(rowSums(A))
sum(colSums(A))/2

sum(degree(Anet))/6
ecount(Anet)*2/6

##### Density #####
edge_density(Anet)
(sum(degree(Anet))/6)/(6-1)

##### Walks #####
w1 <- A
w2 <- A %*% A
w3 <- A %*% A %*% A 
w4 <- A %*% A %*% A %*% A 
w5 <- A %*% A %*% A %*% A %*% A

# shortest distance between 1 and 2?
w1[1,2]
w2[1,2]
w3[1,2]
w4[1,2]
w5[1,2]

# shortest distance between 1 and 3?
w1[1,3]
w2[1,3]
w3[1,3]
w4[1,3]
w5[1,3]

# shortest distance between 2 and 3? 
w1[2,3]
w2[2,3]
w3[2,3]
w4[2,3]
w5[2,3]
