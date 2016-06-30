FL_benchmarking_generic<-function(specs=list(list(n=5,isSquare = TRUE,...),list(n =5,isRowVec = FALSE,...)),
                          classes = c("FLMatrix","FLVector"),operator = "+"){
    
  FLenv<-new.env()
  #browser()
  lapply(1:length(classes),function(i){
    obj<-initFgeneric(specs[[i]],classes[i])
    x=i
    assign(paste0("a",x),obj,envir = FLenv)
  })
  Renv<-as.Renvironment(FLenv)
  x = Sys.time()
  obj1<-do.call(operator,lapply(ls(FLenv),function(x)do.call("$",list(FLenv,paste0(x)))))
  y= Sys.time()
  obj2<-do.call(operator,lapply(ls(Renv),function(x)do.call("$",list(Renv,paste0(x)))))
  z = Sys.time()
  FLobjectsize = as.numeric(object.size(obj1))
  Robjectsize = as.numeric(object.size(obj2))
  result<-data.frame(FLTime = y-x,RTime =z-y,FLobjectsize = FLobjectsize,Robjectsize = Robjectsize)
  result
  
}


# To write Benchmarking results into file on one's machine.
# To see recent result and overall results of Benchmarking in R.
assist_Benchmarking <- function(FunctionName = "",final,RecentResults = "Y",OverallResults = "N"){
write.table(final,paste0("C:/Users/admin/Downloads/Benchmarking_Results/benchmarking_",FunctionName,".csv"),append = TRUE,row.names = FALSE)

#Returns recent benchmarking results.
recent_result = final
print(recent_result)

#Shows all the benchmarking results done .

#ans=readline(prompt = "Do you want to see overall benchmarking results.If yes, type "y" else type "n"")

overall_results = read.table(paste0("C:/Users/admin/Downloads/Benchmarking_Results/benchmarking_",FunctionName,".csv"))
print(overall_results)
}  


}



