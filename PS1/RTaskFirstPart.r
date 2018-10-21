#Author: Patrick Glettig
#Date: 05.10.2018
#Title: Problemset 1 Empirical Methods, R Part

#Because tasks a) to d) are all the same but with different N, I decided to write a function
#that does the job for us. It is called solveTask(N=1,R=200)

set.seed(42)
N1R200 <- solveTask() #Task a) with N=1 and R=200 (default of function)
N5R200 <- solveTask(N=5) #Task b) with N=5 and R=200
N20R200 <- solveTask(N=20) #Task c) with N=20 and R=200
N1000R200 <- solveTask(N=1000) #Task d) with N=1000 and R=200

cominbed <- data.frame()