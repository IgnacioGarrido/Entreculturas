library(RSelenium)
library(rvest)

#CREACIÓN DEL DRIVER:----
#Se hace el driver
rD <- rsDriver(port = 4445L, browser = "chrome", verbose= FALSE)

#Se asigna el cliente a una nueva variable (remDR -> Driver)
remDr <- rD$client

#NAVEGACIÓN:----
#INICIALIZACIÓN:----
remDr$navigate("https://admin.fagms.net/top.php")
#Introducción de usuario y contraseña
Sys.sleep(4)
webElem <- remDr$findElement(using = 'id', value = "networkId")
webElem$sendKeysToElement(list("1335"))
webElem <- remDr$findElement(using = 'id', value = "username")
webElem$sendKeysToElement(list("entreculturas"))
webElem <- remDr$findElement(using = 'id', value = "password")
webElem$sendKeysToElement(list("frxlKp5JZMBG"))
webElem <- remDr$findElement(using = 'id', value = "sendUser")
webElem$clickElement()
Sys.sleep(10)

#PG PRINCIPAL:----
#Informes:
iframe <- remDr$findElement(using='name', value="menue")
remDr$switchToFrame(iframe)
option <- remDr$findElement(using = 'xpath', "//*[@id='Reports']/td[2]/a")
option$clickElement()
Sys.sleep(4)

#Configuración:  
#Volvemos al panel principal:
remDr$switchToFrame(NULL)
#Elegimos campañas...
iframe <- remDr$findElement(using='name', value="admin")
remDr$switchToFrame(iframe)
iframe <- remDr$findElement(using='name', value="seite")
remDr$switchToFrame(iframe)

#Comprobamos longitud de la lista de campañas:
option <- remDr$findElement(using = 'xpath', "//*[@id='gobjMessagesForm_CampaignSelectIS_id']")
optsBucleSup <- option$selectTag()

#BUCLE:----
#
#INSTRUCCIONES:
# Google Chrome no deja descargar muchos archivos seguidos y con rsDriver no se puede hacer configuración previa :(
# Para configurar correctamente Google Chrome se deberá hacer manualmente:
#   1º Descomentar "i <- 2" y "n <- 1" (justo debajo de cada "for") y ejecutar todas las instrucciones dentro del bucle si ejecutar el bucle (IMPORTANTE: sin ejecutar los "for") -> Es recomendable ir ejecutando instrucción por instrucción
#   2º AL ejecutar la instrucción "if" de error -> Al descargar el segundo archivo el navegador lanzará un mensaje en la primera página -> contestar "Permitir"
#   3º Una vez se ha ejecutado la última instrucción del bucle "remDr$switchToFrame(iframe)" volver a comentar "i <- 2" y "n <- 1"
#   4º Ya se puede ejecutar todo el bucle :)
#
#Elegimos la campaña en concreto
for(i in 2:length(optsBucleSup$value)){
#i <- 2
print(i)
campaña <- paste("//*[@id='gobjMessagesForm_CampaignSelectIS_id']/option[",i,"]" ,sep="")
option <- remDr$findElement(using = 'xpath', campaña)
carpeta <- option$getElementText()
option$clickElement()
Sys.sleep(1)

#Elección de la lista de Mailing: 
optionM <- remDr$findElement(using = 'xpath', "//*[@id='gobjMessagesForm_MailingSelectIS_id']")
optsBucleSub <- optionM$selectTag()
if(length(optsBucleSub$value)>0){
#Selección de un evento de lista Mailing 
for(n in 1:length(optsBucleSub$value)){
#n <- 1
  
eventoMailing <- paste("//*[@id='gobjMessagesForm_MailingSelectIS_id']/option[",n,"]" ,sep="")
optionM <- remDr$findElement(using = 'xpath', eventoMailing)
optionM$clickElement()

Sys.sleep(1)
#Cambiamos de frame (botón "Informes"):
remDr$switchToFrame(NULL)
#Elegimos campañas...
iframe <- remDr$findElement(using='name', value="button")
remDr$switchToFrame(iframe)
#Click en el botón
optionM <- remDr$findElement(using = 'xpath', "//*[@id='gcmd18']")
optionM$clickElement()
Sys.sleep(4)

#NUEVA VENTANA:----
#Se cambia a la nueva ventana

remDr$switchToWindow(remDr$getWindowHandles()[[2]])
#Para Rvest
pgweb <- try(read_html(remDr$getCurrentUrl()[[1]]))

#Nombre de los documentos (campaña)
nombre <- pgweb %>%
  html_nodes("br+ font span b") %>%
  html_text()
print(nombre)

#Año
año <- pgweb %>%
  html_nodes("tr:nth-child(5) td:nth-child(2) b") %>%
  .[[1]]%>%
  html_text()
año <- strsplit(año, "/")[[1]][3]
año <- substring(año,1,4)
print(año)

#Distinto fromato de páginas -> con quejas/sin quejas
quejas <- pgweb %>%
  html_nodes("a+ font b") %>%
  .[[4]]%>%
  html_text()

#Se crea el directorio
if(length(año)!=0 & length(nombre) != 0){
if(año == 2017 | año == 2016 | año == 2015 | año == 2014 | año == 2013 | año == 2012 | año == 2011 | año == 2010 | año == 2009 | año == 2008 | año == 2007 | año == 2006 ){
dir.create(paste("Doc_emails",año, carpeta, sep = "/"))
dir.create(paste("Doc_emails",año, carpeta, nombre, sep = "/"))
}else{
dir.create(paste("Doc_emails","Otros", carpeta, sep = "/"))
dir.create(paste("Doc_emails","Otros", carpeta, nombre, sep = "/"))
año <- "Otros"
}
#DESCARGA DE DOCUMENTOS:----
#Descarga de emails enviados
docm <- remDr$findElement(using = 'xpath', "//*[@id='div_2']/table/tbody/tr[2]/td[4]/font/b/a")
docm$clickElement()
Sys.sleep(3)
file.rename(from = "/Users/ignacio/Downloads/export.csv",to =paste("/Users/ignacio/Documents/Proyecto Entreculturas/R/Descarga_Excel/Doc_emails", año, carpeta, nombre, "emailsEnviados.csv" ,sep = "/"))
Sys.sleep(2)

#Rutas sin apartado de quejas:
xpathDocm <- "//*[@id='div_5']/table[1]/tbody/tr[6]/td[4]/font/b/a[2]"
tablaUno <- 18
tablaDos <- 22
#Rutas con apartado de quejas:
if(quejas == "Complaints"){
tablaUno <- 20
tablaDos <- 24
xpathDocm <-   "//*[@id='div_6']/table[1]/tbody/tr[6]/td[4]/font/b/a[2]"
} 
#Descarga de tasa de aperturas
docm <- remDr$findElement(using = 'xpath', xpathDocm)
docm$clickElement()
Sys.sleep(3)
file.rename(from = "/Users/ignacio/Downloads/export.csv",to =paste("/Users/ignacio/Documents/Proyecto Entreculturas/R/Descarga_Excel/Doc_emails", año, carpeta, nombre, "tasaAperturas.csv" ,sep = "/"))

#DESCARGA DE TABLAS:----

#Tabla de tasa de apertura única
tabla_aperturas <- pgweb %>%
  html_nodes("table") %>%
  .[[tablaUno]] %>%
  html_table()

#Tabla de clicks
tabla_clicks <- pgweb %>%
  html_nodes("table") %>%
  .[[tablaDos]] %>%
  html_table()

#Tabla de información general
tabla_info <- pgweb %>%
  html_nodes("table") %>%
  .[[6]] %>%
  html_table()

#Conversión a csv y se crean ficheros:
write.csv(tabla_info ,file = paste("Doc_emails", año, carpeta, nombre, "tablaInfoGeneral.csv", sep="/"))
write.csv(tabla_aperturas ,file = paste("Doc_emails", año, carpeta, nombre, "tablaAperturasPorHora.csv", sep="/"))
write.csv(tabla_clicks ,file = paste("Doc_emails", año, carpeta, nombre, "tablaClicks.csv", sep="/"))
}else{
  print("¡WARNING!-> Página errónea.............................")
}
#Cerramos la ventana (y se vuelve a la principal)
remDr$closeWindow()
Sys.sleep(1)
remDr$switchToWindow(remDr$getWindowHandles()[[1]])
#Cambiamos de frame (elección de campaña/evento):
remDr$switchToFrame(NULL)
#Elegimos campañas...
iframe <- remDr$findElement(using='name', value="admin")
remDr$switchToFrame(iframe)
iframe <- remDr$findElement(using='name', value="seite")
remDr$switchToFrame(iframe)
}
}
}

#FINALIZACIÓN:----
# close client/server
remDr$close()
rD$server$stop()
# check server -> "state     : terminated"
rD$server$process
#Borrado de variables:
rm(rD, tabla_info, remDr,webElem, carpeta, tablaUno, tablaDos, tabla_aperturas, tabla_clicks ,iframe, quejas, xpathDocm, pgweb, nombre, año, option, campaña, docm, i, n, optsBucleSup, optsBucleSub , eventoMailing, optionM)
