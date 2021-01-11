data=readRDS(system.file("extdata", "data.rds", package = "dataprep"))
data1=cbind(data[1:4],Nucleation=rowSums(data[5:34],na.rm=T)/20,
      Aitken=rowSums(data[35:45],na.rm=T)/20,
      Accumulation=rowSums(data[46:65],na.rm=T)/20)[c(1,4:7,2,3)]
