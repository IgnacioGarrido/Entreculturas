#BUCLE PARA NAVEGAR POR EL SISTEMA DE DIRECTORIOS:----

#
#IMPORTANTE: Hay que colocarse justo encima del directorio Doc_emails que contiene todas las carpetas por años...
#

dirOrigen <- getwd()
#Nos movemos a Doc_emails -> contiene todos los mails por año:
setwd("Doc_emails")
#Directorio raíz:
dirRaiz <- getwd()
#Se hace un array con todas las carpetas del directorio:
carpetasDirRaiz <- dir()

#Bucle para recorrer el sistema de carpetas:
for(i in 1:length(carpetasDirRaiz)){
  #Estamos en Doc_emails y nos movemos a Doc_emails/año
  setwd(carpetasDirRaiz[i])
  #Dirección del nuevo directorio(dentro del año)
  dirAño <- getwd()
  #Se hace un array con todas las carpetas del directorio(dentro del año):
  carpetasDirAño <- dir()
  for(n in 1:length(carpetasDirAño)){
    #Estamos en Doc_emails/año y nos movemos a Doc_emails/año/campaña
    setwd(carpetasDirAño[n])
    dirCampaña <- getwd()
    #Se hace un array con todas las carpetas del directorio(dentro del año/campaña):
    carpetasDirCampaña <- dir()
    for(t in 1:length(carpetasDirCampaña)){
      #Estamos en Doc_emails/año/campaña y nos movemos a Doc_emails/año/campaña/envío
      setwd(carpetasDirCampaña[t])
      #DENTRO DEL DIRECTORIO FINAL:----
      #Aquí estás dentro del directorio final (el que tiene los csv)
      
      
      
      
      
      
      
      
      
      
      #SALIDA DEL DIRECTORIO DE TRABAJO:----
      #Volvemos al nivel superior
      setwd(dirCampaña)  
    }
    #Volvemos al nivel superior
    setwd(dirAño) 
  }
  #Volvemos al nivel superior
  setwd(dirRaiz)
}

#FINALIZACIÓN:
setwd(dirOrigen)
rm(carpetasDirAño, carpetasDirCampaña, carpetasDirRaiz, dirAño, dirCampaña, dirOrigen,dirRaiz, i, n, t)
