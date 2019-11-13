library(stringr)

#Creación del dataframe de envíos
envios_df <- data.frame(campaña=NA, asunto=NA, fecha=NA,hora=NA,correo=NA,abierto=NA,fecha_abierto=NA,hora_abierto=NA,n_aperturas=NA)


#FUNCIONES:----

#Función que comprueba si un usuario se ha metido en el email
#Devuelve un df con:
# email: Lista de emails
# fecha_abierto: Fechas apertura
# abierto: Array de tipo boolean de longitud = enviados_df con TRUE en los usuarios que abrieron el email
#enviados_df -> dataframe de los emails enviados
#abiertos_df -> dataframe de los emails que lo abrieron
comprueba_lectura_email <- function(enviados_df, abiertos_df){
  
  df_aux <- merge(enviados_df, abiertos_df, by.x = "email", by.y = "email", all.x = TRUE)
  abierto <- df_aux$email %in% abiertos_df$email
  df_aux <- cbind(df_aux, abierto)
  names(df_aux)[2] = "fecha_abierto"
  names(df_aux)[3] = "n_aperturas"
  return(df_aux)
}

#Función que devuelve un vector con el número de aperturas por usuario.
#El vector es de tipo int y se deverá añadir a tasaAperturas
#df: Es el df del que queremos ver las repeticiones de un elemento
añade_numero_aperturas <- function(df){
  #Añadimos una nueva columna nº de aperturas:
  n_aperturas <- rep(1, length(df[,1]))
  df <- cbind(df, n_aperturas)
  #Vector de los emails duplicados:
  duplicado <- duplicated(df[,1])
  #Se recorre el df(solo los duplicados)
  for(i in df[duplicado,1]){
    df[df$email==toString(i),3] <- length(df[df$email==toString(i),1])
  }
  return(df$n_aperturas)
}


#BUCLE PARA NAVEGAR POR EL SISTEMA DE DIRECTORIOS:----

setwd("/Users/ignacio/Documents/Proyecto Entreculturas/R/OrdenDFMailSolitions")

dirOrigen <- getwd()
#Nos movemos a Doc_emails -> contiene todos los emails por año:
setwd("Doc_emails")

#¡PARA PRUEBA! : 
#setwd("Prueba")

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
      
      
      #CONVERSION A DATAFRAMES DE LOS CSV:----
      
      #Se importan los csv en formato dataframe:
      print(getwd())
      emailsEnviados=read.csv("emailsEnviados.csv",sep=";",stringsAsFactors = F, skipNul=TRUE, header = TRUE)
      tablaInfoGeneral=read.csv("tablaInfoGeneral.csv",sep=",",stringsAsFactors = F)
      tasaAperturas=read.csv("tasaAperturas.csv", sep = ";", skipNul = TRUE, header = TRUE, stringsAsFactors = F)
      
      #RELLENO DEL DF ENVIOS_DF:----
      
      #Se añade la columna n_aperturas con el nº de aperturas a tasaAperturas:
      tasaAperturas <- cbind(tasaAperturas, n_aperturas = añade_numero_aperturas(tasaAperturas))
      #Se borran los duplicados:
      tasaAperturas <- tasaAperturas[!duplicated(tasaAperturas[,1]),]
      
      #Se hacen vectores auxiliares para añadirlos en formato dataframe a envios_df
      #La longitud del nuevo df depende de los correos:
      longitud <- length(emailsEnviados$email)
      #data.frame(campaña,asunto,fecha,hora,correo,abierto,fecha_abierto,hora_abierto,n_aperturas)
      campaña <- rep(toString(tablaInfoGeneral[2,3]), longitud)
      asunto <- rep(toString(tablaInfoGeneral[3,3]), longitud)
      fecha <- rep(strsplit(toString(tablaInfoGeneral[4,3]), " ")[[1]][1], longitud)
      hora <- rep(strsplit(toString(tablaInfoGeneral[4,3]), " ")[[1]][2], longitud)
      
      #aux_ab: df auxiliar que contiene apertura(TRUE/FALSE), fecha y n_aperturas
      aux_ab <- comprueba_lectura_email(emailsEnviados,tasaAperturas)
      correo <- aux_ab$email
      abierto <- aux_ab$abierto
      fecha_abierto <- str_split_fixed(aux_ab$fecha_abierto, " ", 2)[,1]
      hora_abierto <- str_split_fixed(aux_ab$fecha_abierto, " ", 2)[,2]
      n_aperturas <- aux_ab$n_aperturas
      
      #Se añade a envios_df los nuevos elementos
      envios_df <- rbind(envios_df, data.frame(campaña=campaña, asunto=asunto, fecha=fecha,hora=hora,correo=correo,abierto=abierto,fecha_abierto=fecha_abierto,hora_abierto=hora_abierto,n_aperturas=n_aperturas))
     
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

#Se hace el csv en OrdenDFMailSolitions:
write.csv(envios_df, file = "~/Documents/Proyecto Entreculturas/R/OrdenDFMailSolitions/envios_df.csv",na="", row.names = FALSE)
#FINALIZACIÓN:
setwd(dirOrigen)
rm(envios_df, emailsEnviados, tablaInfoGeneral, tasaAperturas, carpetasDirAño, carpetasDirCampaña, carpetasDirRaiz, dirAño, dirCampaña, dirOrigen,dirRaiz, i, n, t, aux_ab, abierto, asunto, campaña,correo, fecha, fecha_abierto, hora, hora_abierto, longitud, n_aperturas)
