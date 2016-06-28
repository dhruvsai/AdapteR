#Masked this function in a way that makes R gam less powerful.

Renv = new.env(parent = globalenv())
set.seed(2) 
Renv$data1 = gamSim(1,n=400,dist="normal",scale=2)
Renv$data2 = gamSim(1,n=2000,dist="poisson",scale=.1)
Renv$data3 = gamSim(1,n=400,dist="poisson",scale=.25)
Renv$data4 = gamSim(1,n=400,dist="binary",scale=.33)
FLenv = as.FL(Renv)

#Test failed .Incorrect number of dimensions error in AdapteR.
#Asana ticket - https://app.asana.com/0/143316600934101/149032129314100
test_that("Check for gam function with normal data ",{
    result = eval_expect_equal({
             test1 = gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=data1)
             gam.check(test1) 
        },Renv,FLenv)
    print(result)
    })

#Test failed . Incorrect number of dimensions and ObsID should be of BIGINT and Integer type.
#Masking of R function is making it less powerful.gam(test2 = test2) is not working.

test_that("Check for gam function with fit argument",{
    result = eval_expect_equal({
             test2 = gam(y~s(x0)+s(x1)+s(x2)+s(x3),fit=FALSE,data=data1)
             gam(test2 = test2)
        },Renv,FLenv)
    print(result)
    })
#Incorrect number of dimensions.
#Test failed.
test_that("Check for gam function with REML smoothness method",{
    result = eval_expect_equal({
             test3 = gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=data1,method="REML")
        },Renv,FLenv)
    print(result)
    })
 
#Incorrect number of dimensions.
#Test failed.
test_that("Check for gam function with tensor product of x0 and x1",{
    result = eval_expect_equal({
             test4 = gam(y~te(x0,x1,k=7)+s(x2)+s(x3),data=data1,
                         method="REML")
        },Renv,FLenv)
    print(result)
    })

#Test failed.
#Incorrect number of dimensions.
#May be due to connection problem or may be due to SQL query problem.
test_that("Check for gam function with smooth anova and decomposition for x1 and x2",{
    result = eval_expect_equal({
             test5 =  gam(y~s(x0)+s(x1)+s(x2)+s(x3)+ti(x1,x2,k=6),
                      data=data1,method="REML")
        },Renv,FLenv)
    print(result)
    })

#Test failed.
#Incorrect number of dimensions.
test_that("Check for gam function when x0 and x1 are treated isotropically",{
    result = eval_expect_equal({
             test6 = gam(y~s(x0,x1,k=40)+s(x2)+s(x3),data=data1,
                     method="REML")
        },Renv,FLenv)
    print(result)
    })

#Test Failed.
#Incorrect number of dimensions and error in SQL.
test_that("Check for gam function with automatic selection to be true",{
    result = eval_expect_equal({
             test7 = gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=data1,
                     method="REML",select=TRUE)
        },Renv,FLenv)
    print(result)
    })
#Test Failed.
#Incorrect number of dimensions and error in SQL.
test_that("Check for gam function when smoothing parameter set for first term",{
    result = eval_expect_equal({
             test8 = gam(y~s(x0)+s(x1)+s(x2)+s(x3),sp=c(0.01,-1,-1,-1),data=data1)
             test9 = gam(y~s(x0,sp=.01)+s(x1)+s(x2)+s(x3),data=data1)
    },Renv,FLenv)
    print(result)
})

#Test Failed.
#Incorrect number of dimensions and error in SQL.
test_that("Check for gam function with minimum bound on smoothness parameter",{
    result = eval_expect_equal({
             test10 = gam(y~s(x0)+s(x1)+s(x2)+s(x3),
                     min.sp=c(0.001,0.01,0,10),data=data1)
    },Renv,FLenv)
    print(result)
 })   

#Test Failed.
#Incorrect number of dimensions and error in SQL.
test_that("Check for gam function with minimum bound on smoothness parameter and method to be REML",{
    result = eval_expect_equal({
             test11 = gam(y~s(x0)+s(x1)+s(x2)+s(x3),
                      min.sp=c(0.1,0.1,0,10),data=data1,method="REML")
    },Renv,FLenv)
    print(result)
 })   

#Test Failed.
#Incorrect number of dimensions and error in SQL.
test_that("Check for gam function with 3 df regression spline and 2 penalised terms",{
    result = eval_expect_equal({
             test12 = gam(y~s(x0,k=4,fx=TRUE,bs="tp")+s(x1,k=12)+s(x2,k=15),data=data1)
    },Renv,FLenv)
    print(result)
})

#Test Failed . Error - use s and te for spline specification.
#Asana ticket - https://app.asana.com/0/143316600934101/149032129314110
test_that("Check for gam function with poisson family",{
    result = eval_expect_equal({
             test13 = gam(y~s(x0,bs="cr")+s(x1,bs="cr")+s(x2,bs="cr")+
                      s(x3,bs="cr"),family=poisson,data=data2,method="REML")
    },Renv,FLenv)
    print(result)
   }) 

#Test Failed . Error - use s and te for spline specification.
#R gam also failin stating that arg 3 is NA/NaN.
test_that("Check for gam function with x3 being dropped and using previous fit",{
    result = eval_expect_equal({
             test14 = gam(y~s(x0,bs="cr")+s(x1,bs="cr")+s(x2,bs="cr"),
                      family=poisson,data=data2,method="REML",
                      in.out=list(sp=test12$sp[1:3],scale=1))
    },Renv,FLenv)
    print(result)
    })

#Test failed. Incorrect number of dimensions.
test_that("Check for gam function with data3 and with performance iterations",{
    result = eval_expect_equal({
             test15 =gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,
             data=data3,optimizer="perf")
    },Renv,FLenv)
    print(result)
   })
    
#Test failed. Incorrect number of dimensions.
test_that("Check for gam function using GACV method",{
    result = eval_expect_equal({
             test16 = gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,
                      data=data3,method="GACV.Cp",scale=-1)
    },Renv,FLenv)
    print(result)
})

#Test failed. Incorrect number of dimensions.
test_that("Check for gam function with REML method",{
    result = eval_expect_equal({
             test17 = gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=poisson,
                      data=data3,method="REML")
    },Renv,FLenv)
    print(result)
})

#Test failed. Incorrect number of dimensions.
test_that("Check for gam function with binomial family",{
    result = eval_expect_equal({
             test18 =  gam(y~s(x0)+s(x1)+s(x2)+s(x3),family=binomial,
                       data=data4,method="REML")
             test19 = gam(y~s(x0)+s(x1)+s(x2),family=binomial,
                      data=data4,method="REML")
             test20 = gam(y~s(x1)+s(x2),family=binomial,
                       data=data4,method="REML")
    },Renv,FLenv)
})









