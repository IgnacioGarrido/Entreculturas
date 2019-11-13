setwd("/Users/ignacio/Documents/Proyecto Entreculturas/Curriculums/NombresApellidos")

IDs=read.csv("IDsfilesnames.csv",header=T,encoding = "utf-8",stringsAsFactors = F)
archivoAinara=read.csv("0_Contabilidad.csv",header=T,encoding = "utf-8",stringsAsFactors = F, sep = ";")

aux_array = sapply(strsplit(IDs$FICHERO, "_"), function(x) x[1])
aux_array=as.integer(aux_array)

for(i in 1:length(aux_array)){
  print(i)
 IDs[i,"NOMBRE"] = archivoAinara[which(archivoAinara$X == aux_array[i]),"n"]
 IDs[i, "APELLIDO.1"] = archivoAinara[which(archivoAinara$X == aux_array[i]),"a"]
 IDs[i,"FASE.1"] = archivoAinara[which(archivoAinara$X == aux_array[i]),"X1º.fase"]
 IDs[i,"FASE.2"] = archivoAinara[which(archivoAinara$X == aux_array[i]),"X2ºfase"]
 IDs[i,"FASE.3"] = archivoAinara[which(archivoAinara$X == aux_array[i]),"X3º.fase"]
 IDs[i,"RESULTADO"] = archivoAinara[which(archivoAinara$X == aux_array[i]),"selección"]
 #IDs[i,"RESULTADO_AINARA"] = archivoAinara[which(archivoAinara$X.1 == aux_array[i]),"X"]
 IDs[i,"RESULTADO_AINARA"]=NA
 }

write.csv(IDs, file = "IDsfilesnames.csv")


