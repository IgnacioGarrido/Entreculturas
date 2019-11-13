library(rvest)

#Para rvest:
pgweb <- try(read_html("https://admin.fagms.net/_reports/M-10462199-1950330-3cca1478e85a256ad3c81338525ad83b-1489233504-16NL5HNH3KA9B1973PN0.html"))

#Nombre de los documentos (campaña)
nombre <- pgweb %>%
  html_nodes("br+ font span b") %>%
  html_text()

#Año
año <- pgweb %>%
  html_nodes("tr:nth-child(5) td:nth-child(2) b") %>%
  .[[1]]%>%
  html_text()
año <- substring(año,2,5)

#Distinto fromato de páginas -> con quejas/sin quejas
quejas <- pgweb %>%
  html_nodes("a+ font b") %>%
  .[[4]]%>%
  html_text()

#Documentos:

#Renombrar un fichero : file.rename(from = "/Users/ignacio/Downloads/unsubscribers.csv",to = "/Users/ignacio/Documents/Proyecto Entreculturas/R/Descarga_Excel/unsubscribers.csv")

#Tablas

#Tabla de tasa de apertura única
tabla_aperturas <- pgweb %>%
  html_nodes("table") %>%
  .[[18]] %>%
  html_table()
#tabla_aperturas

#Tabla de clicks
tabla_clicks <- pgweb %>%
  html_nodes("table") %>%
  .[[22]] %>%
  html_table()
#tabla_clicks



#CONVERSIÓN DE LOS DATOS A CSV#


#Se crea el directorio
dir.create(nombre)
#Se crean los ficheros con las tablas
write.csv(tabla_aperturas ,file = paste(nombre, "tabla_aperturas.csv", sep="/"))
write.csv(tabla_clicks ,file = paste(nombre, "tabla_clicks.csv", sep="/"))

