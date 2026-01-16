
#split train val and test set
set.seed(10)
test_index<-sample(1:nrow(data),0.25*nrow(data))
test<-data[test_index,]
train_val<-data[-test_index,]
