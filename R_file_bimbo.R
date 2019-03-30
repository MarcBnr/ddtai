library(tibble) # data wrangling
library(data.table)
library(pROC)
library(dummies)
library(dplyr) 
library(gbm)

dt = as.tibble(fread("reduced_train.csv"))
dt$V1 <- NULL
dt$Observation <- NULL

names(dt) <- c("Semana","Agencia_ID","Canal_ID","Ruta_SAK","ID","Producto_ID","Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil","Client_Type")
setDT(dt)

# Demanda_uni_equil
dt=dt[order(Cliente_ID,Semana)]
#which weeks 
train<-dt[dt$Semana <= 8,]                   

test<-dt[dt$Semana == 9,]             

#test with one id
train1= train[Cliente_ID==26,]
test1=test[Cliente_ID==26,]
model1=glm(Demanda_uni_equil~ Agencia_ID+Canal_ID+Ruta_SAK+Cliente_ID+Producto_ID,data = train1)
summary(model1)
test1[,pred:= predict(model1, newdata=test1,type="response")]

#for whole dataset
train_ID<- list()
test_ID <- list()
model_Id <- list()
for(id in train$ID){
  train_ID[[paste0("ID",id)]]=train[ID==id,]
  test_ID[[paste0("ID",id)]]=test[ID==id,]
  model_ID[[paste0("ID",id)]] = glm(Demanda_uni_equil~ Agencia_ID+Canal_ID+Ruta_SAK+ID+Producto_ID,data = train_ID[[paste0("ID",id)]])
  test_ID[[paste0("ID",id)]]=test_ID[[paste0("ID",id)]][,pred:= predict(model_ID[[paste0("ID",id)]], newdata= test_ID[[paste0("ID",id)]], type="response")]
}
