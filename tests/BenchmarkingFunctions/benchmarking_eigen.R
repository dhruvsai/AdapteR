Renv = new.env(parent = globalenv())

N = 10000
upper = 2000000
lower = -2000000

Renv$mat1 = matrix(runif(N,lower,upper),nrow = 100,ncol = 100,byrow = TRUE)

FLenv = as.FL(Renv)

#Initialisation of variable containing global environment.
env = globalenv()
final = data.frame(description = "",r.Runtime = 0,fl.Runtime =0)

#To run function for n times.
#n = readline(prompt = "Enter number of times you want to run function")
#Default value is set to be 1 i.e. will run for 1 time.
n = 1
for(x in 1:n){
test_that("Benchmarking for diag ",
  {
    result1=eval_expect_equal({
            test1=eigen(mat1)}
            ,Renv,FLenv,check.attributes = FALSE)
    env$final = rbind(final,result1) 
  })
}

assist_benchmarking(FunctionName = "eigen",final,RecentResults = "Y",OverallResults = "N")

