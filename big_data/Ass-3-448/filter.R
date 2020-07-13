x <- matrix(c(1,0,0,0,1,0,1,0,1,0,0,1,1,1,0,0,1,1,1,0,1,0,0,0,1), nrow=5, byrow=TRUE)

f3 <- matrix(c(0,-1,0,-1,5,-1,0,-1,0), nrow=3, byrow=TRUE)

m <- matrix(rep(NA,9), nrow=3)

for (row in 1:3) {
    for (col in 1:3) {
        m[row,col] <- sum(x[eval(row):eval(row+2),eval(col):eval(col+2)]*f3)
    }
}

print(m)