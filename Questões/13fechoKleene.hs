fechoKleene xs = [""]++[ y++[x] | y<- (fechoKleene xs), x<-xs]
