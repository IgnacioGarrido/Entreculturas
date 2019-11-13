#############################################################
#Programa para leer ficheros
#############################################################

#Para procesar pdfs
require(pdftools)
require(xml2)
#devtools::install_github("kota7/striprtf")
require(striprtf)
require(tokenizers)
#Para detectar el idioma (funciona regular)
require(textcat)
#Para corpus
require(tm)
#require(SnowballC)

#Se establece el directorio
#setwd("/Users/ignacio/Documents/Proyecto Entreculturas/Curriculums/ficheros")
setwd("/Users/ignacio/Documents/Proyecto Entreculturas/Curriculums/ficheros/2017_Contabilidad_Externa")


##FUNCIONES QUANTICAE##
#############################################################
# Quanticae RRHH
#
# Function: get_text_word
#
# Recibe: 
# El nombre del fichero word
#
# Devuelve:
# el texto
#
#
#############################################################
get_status_word <- function(word_doc_tmp) {
  
  
  file_tmp=word_doc_tmp
  
  #Nombre del fichero
  #Por defecto
  res_status="UNKNOWN_ERROR"
  res_pages=0
  res_size=0
  res_char=0
  res_word=0
  
  #Si el fichero no existe o está vacío
  if (file.exists(file_tmp)==FALSE|file.info(file_tmp, extra_cols = TRUE)$size==0)
  {
    res_status="NO_FILE"
  } else {
    res_size=file.info(file_tmp, extra_cols = TRUE)$size
    res_status="WORD"
    
    tmpd <- tempdir()
    tmpf <- tempfile(tmpdir=tmpd, fileext=".zip")
    
    file.copy(word_doc_tmp, tmpf)
    unzip(tmpf, exdir=sprintf("%s/docdata", tmpd))
    
    doc <- read_xml(sprintf("%s/docdata/word/document.xml", tmpd))
    
    unlink(tmpf) #Para borrar el fichero
    unlink(sprintf("%s/docdata", tmpd), recursive=TRUE)
    
    ns <- xml_ns(doc)
    
    res_text = xml_text(xml_find_all(doc, ".//w:p", ns=ns))
    res_text = limpia_contenido_RRHH(res_text)
    
    res_pages=length(xml_find_all(doc,".//w:lastRenderedPageBreak", ns=ns))+1
    res_word=length(unlist(tokenize_words(res_text)))
    res_char=nchar(paste(res_text,collapse="\n"))
  }
  
  #file.remove(tmpf)
  
  return (c(fichero=word_doc_tmp,status=res_status,size=res_size,npages=res_pages,nword=res_word,nchar=res_char))
  
  
}
get_status_old_word <- function(file_old_word_tmp) {
  
  
  file_tmp=file_old_word_tmp
  
  #Nombre del fichero
  #Por defecto
  res_status="UNKNOWN_ERROR"
  res_pages=0
  res_size=0
  res_char=0
  res_word=0
  
  #Si el fichero no existe o está vacío
  if (file.exists(file_tmp)==FALSE|file.info(file_tmp, extra_cols = TRUE)$size==0)
  {
    res_status="NO_FILE"
  } else {
    res_size=file.info(file_tmp, extra_cols = TRUE)$size
    res_status="OLD_WORD"
    
    res_text=system2(ANTIWORD, c("",file_tmp),stdout = TRUE)
    
    res_text=limpia_contenido_RRHH(res_text)
    
    res_pages=1
    res_word=length(unlist(tokenize_words(res_text)))
    res_char=nchar(paste(res_text,collapse="\n"))
  }
  
  #file.remove(tmpf)
  
  return (c(fichero=file_old_word_tmp,status=res_status,size=res_size,npages=res_pages,nword=res_word,nchar=res_char))
  
  
}
get_status_rtf <- function(file_rtf_tmp) {
  
  
  file_tmp=file_rtf_tmp
  
  #Nombre del fichero
  #Por defecto
  res_status="UNKNOWN_ERROR"
  res_pages=0
  res_size=0
  res_char=0
  res_word=0
  
  #Si el fichero no existe o está vacío
  if (file.exists(file_tmp)==FALSE|file.info(file_tmp, extra_cols = TRUE)$size==0)
  {
    res_status="NO_FILE"
  } else {
    res_size=file.info(file_tmp, extra_cols = TRUE)$size
    res_status="RTF"
    
    
    res_text = read_rtf(file_tmp)
    res_text = limpia_contenido_RRHH(res_text)
    
    res_pages=1
    res_word=length(unlist(tokenize_words(res_text)))
    res_char=nchar(paste(res_text,collapse="\n"))
  }
  
  #file.remove(tmpf)
  
  return (c(fichero=file_rtf_tmp,status=res_status,size=res_size,npages=res_pages,nword=res_word,nchar=res_char))
  
  
}
get_status_txt <- function(file_txt_tmp) {
  
  
  file_tmp=file_txt_tmp
  
  #Nombre del fichero
  #Por defecto
  res_status="UNKNOWN_ERROR"
  res_pages=0
  res_size=0
  res_char=0
  res_word=0
  
  #Si el fichero no existe o está vacío
  if (file.exists(file_tmp)==FALSE|file.info(file_tmp, extra_cols = TRUE)$size==0)
  {
    res_status="NO_FILE"
  } else {
    res_size=file.info(file_tmp, extra_cols = TRUE)$size
    res_status="TXT"
    
    
    res_text = readLines(file_tmp,encoding="UTF-8")
    res_text = limpia_contenido_RRHH(res_text)
    
    res_pages=1
    res_word=length(unlist(tokenize_words(res_text)))
    res_char=nchar(paste(res_text,collapse="\n"))
  }
  
  #file.remove(tmpf)
  
  return (c(fichero=file_txt_tmp,status=res_status,size=res_size,npages=res_pages,nword=res_word,nchar=res_char))
  
  
}
#############################################################
# Quanticae RRHH
#
# Function: status_pdf_RRHH
#
# Recibe: 
# El nombre del fichero como un char
#
# Devuelve:
# status del fichero PDF: NO_FILE,PASSWORD,FILE_IMG,FILE_TXT,UNKNOW_ERROR
# el tamaño del fichero
# el número de páginas
# el número de caracteres
#
#
#############################################################

status_doc_RRHH <- function(file_tmp)
{
  print(file_tmp)
  
  #Nombre del fichero
  #Por defecto
  res_status="UNKNOWN_ERROR"
  res_pages=0
  res_size=0
  res_char=0
  res_word=0
  
  #Si el fichero no existe o está vacío
  if (file.exists(file_tmp)==FALSE|file.info(file_tmp, extra_cols = TRUE)$size==0)
  {
    res_status="NO_FILE"
  } else {
    res_size=file.info(file_tmp, extra_cols = TRUE)$size
    
    
    extension=gsub(".*\\.([[:alpha:]]+)$","\\1",file_tmp)
    
    if (extension=="rtf")
    {
      return (get_status_rtf(file_tmp))
    } 
    
    if (extension=="txt")
    {
      return (get_status_txt(file_tmp))
    } 
    
    if (extension=="docx")
    {
      return (get_status_word(file_tmp))
    }
    
    if (extension=="doc")
    {
      return (get_status_old_word(file_tmp))
    }
    
    if (extension=="pdf"|extension=="PDF")
    {
      res_pages=pdf_info(file_tmp)$pages
      
      res_text=limpia_contenido_RRHH(pdf_text(file_tmp))
      res_word=length(unlist(tokenize_words(res_text)))
      res_char=nchar(paste(res_text,collapse="\n"))
      
      
      my_status=try(system(paste('pdftk.exe "',file_tmp,'" dump_data',sep=""),intern=T,wait=T))
      
      
      #Se ha producido un error 
      if (length(attributes(my_status)$status))
      {
        if (length(grep ("PASSWORD REQUIRED",my_status)))
        {
          res_status="PASSWORD"
        } 
      } else {
        
        #Trato de obtener los caracteres (cuidado porque solo es de las 5 primeras páginas)
        my_status=try(system(paste('pdftotext.exe -enc UTF-8 -layout "',file_tmp,'" -',sep=""), intern=T,wait = T))
        
        #Si no tiene caracteres es que es imagen
        if (res_char==0)
        {
          res_status="FILE_IMAGE"
        } else {
          res_status="PDF"
        }
      }
      
    }
    
    if (!extension%in%c("pdf","PDF","docx"))
    {
      res_status=paste("OTHER:",extension)
    }
  }
  
  return (c(fichero=file_tmp,status=res_status,size=res_size,npages=res_pages,nword=res_word,nchar=res_char))
  
}

#############################################################
# Quanticae RRHH
#
# Function: limpia_contenido_RRHH
#
# Limpia el texto
#
# Recibe: 
# El texto
# Umbral de caracteres por línea para no usar
#
# Devuelve:
# el texto limpio
#
#
#############################################################

limpia_contenido_RRHH<-function(txt_content)
{
  txt_content=paste(txt_content,collapse="\n")
  txt_content=unlist(strsplit(txt_content,"\\n"))  
  
  txt_content=gsub("\\|","",txt_content)
  txt_content=gsub("\\[pic\\]","",txt_content)
  txt_content=gsub("[\t]+"," ",txt_content)
  txt_content=gsub("[ ]+"," ",txt_content)
  txt_content=gsub("^[ ]+","",txt_content)
  txt_content=gsub("[ ]+$","",txt_content)
  txt_content=gsub("\r","",txt_content)
  txt_content=gsub("<U\\+FEFF>","",txt_content)
  
  txt_content=gsub("[^[:print:]]+","",txt_content)
  
  n_char_alpha=nchar(gsub("[^[:alpha:]]","",txt_content))
  
  if (length(which(n_char_alpha==0)))
  {
    txt_content=txt_content[-which(n_char_alpha==0)]
    
  }
  
  
  
  txt_content
}


#############################################################
# Quanticae RRHH
#
# Function: get_text_word
#
# Recibe: 
# El nombre del fichero word
#
# Devuelve:
# el texto
#
#
#############################################################

get_text_word <- function(word_doc_tmp) {
  
  print(word_doc_tmp)
  
  file_tmp=word_doc_tmp
  
  #Por defecto
  res_txt=NULL
  
  #Si el fichero no existe o está vacío
  if (file.exists(file_tmp)==FALSE|file.info(file_tmp, extra_cols = TRUE)$size==0)
  {
  } else {
    res_size=file.info(file_tmp, extra_cols = TRUE)$size
    
    tmpd <- tempdir()
    tmpf <- tempfile(tmpdir=tmpd, fileext=".zip")
    
    file.copy(word_doc_tmp, tmpf)
    unzip(tmpf, exdir=sprintf("%s/docdata", tmpd))
    
    doc <- read_xml(sprintf("%s/docdata/word/document.xml", tmpd))
    
    unlink(tmpf)
    unlink(sprintf("%s/docdata", tmpd), recursive=TRUE)
    
    ns <- xml_ns(doc)
    
    res_txt = xml_text(xml_find_all(doc, ".//w:p", ns=ns))
    res_txt=limpia_contenido_RRHH(res_txt)
    
    
  }
  
  #file.remove(tmpf)
  
  return (res_txt)
  
  
}


#############################################################
# Quanticae RRHH
#
# Function: procesa_RRHH
#
# Recibe:
# El registro como un char
#
# Devuelve:
# status del fichero PDF: NO_FILE,PASSWORD,FILE_IMG,FILE_TXT,UNKNOW_ERROR
# el tamaño del fichero
# el número de páginas
# el número de caracteres
#
#
#############################################################
procesa_RRHH <- function(reg_file_tmp)
{
  print(reg_file_tmp["fichero"])
  
  #Nombre del fichero
  file_tmp=as.character(reg_file_tmp["fichero"])
  
  #Por defecto mantengo el lenguage
  res_lang="es"
  
  #Por defecto no devuelve nada
  res_txt=""
  
  #Si viene como NO_FILE no haré nada
  
  #Si viene como PASSWORD la única opción es hacerlo con el pdf_text
  if (reg_file_tmp["status"]=="PASSWORD")
  {
    res_txt=pdf_text(file_tmp)
    
    res_txt=limpia_contenido_RRHH(res_txt)
    
    print("PDF CON PASSWORD PROCESADO")
  }
  
  #Si viene como IMAGEN paso
  if (reg_file_tmp["status"]=="FILE_IMAGE")
  {
    print("IMG NO PROCESADO")
    
  }
  
  #Si es PDF
  if (reg_file_tmp["status"]=="PDF")
  {
    res_txt=pdf_text(file_tmp)
    
    res_txt=limpia_contenido_RRHH(res_txt)
    
    print("PDF PROCESADO")
    
  }
  
  if (reg_file_tmp["status"]=="WORD")
  {
    res_txt=get_text_word(file_tmp)
    
    print("WORD PROCESADO")
    
  }
  
  if (reg_file_tmp["status"]=="RTF")
  {
    res_txt=read_rtf(file_tmp)
    
    res_txt=limpia_contenido_RRHH(res_txt)
    
    print("RTF PROCESADO")
    
    
  }
  
  if (reg_file_tmp["status"]=="OLD_WORD")
  {
    res_txt=system2(ANTIWORD, c("",file_tmp),stdout = TRUE)
    
    res_txt=limpia_contenido_RRHH(res_txt)
    
    print("OLD WORD PROCESADO")
    
  }
  
  if (reg_file_tmp["status"]=="TXT")
  {
    res_txt=readLines(file_tmp,encoding="UTF-8")
    
    res_txt=limpia_contenido_RRHH(res_txt)
    
    print("TXT PROCESADO")
    
  }
  
  #if (paste(res_txt,collapse="")!="")
  #{
  #res_google=analiza_texto_con_google(res_txt)
  #res_txt_google=res_google[[1]]
  #res_lang=res_google[[2]]
  #}
  
  #Si viene como IMAGEN paso
  if (length(grep ("OTHER:",reg_file_tmp["status"])))
  {
    print(reg_file_tmp["status"])
    
    print("OTHER NO PROCESADO")
    
  }
  
  if (length(res_txt))
  {
    res_lang=ifelse(textcat(paste(res_txt,collapse=" "))=="english","en","es")
  }
  
  #return(list(res_txt,res_txt_google,res_lang))
  return(list(res_txt,res_lang))#
}


#############################################################
##FUNCIONES QUANTICAE##

#Lista con todos los archivos en formato pdf
files_pdf <- list.files(pattern = "pdf$")
files_docx <- list.files(pattern = "docx$")
files_names <- c(files_pdf, files_docx)

list_files <- sapply(files_names, function(x) list(status_doc_RRHH(x)))
size <- sapply(list_files, function(x) as.numeric(x["size"]))
npages <- sapply(list_files, function(x) as.numeric(x["npages"]))
nword <- sapply(list_files, function(x) as.numeric(x["nword"]))
nchar <- sapply(list_files, function(x) as.numeric(x["nchar"]))


#Se guardan en formato txt -> AQUÍ ES DONDE OBTENGO LOS TEXTOS
txt_files <- sapply(list_files, function(x) list(procesa_RRHH(x)))
txt_files <- sapply(txt_files, function(x) x[[1]])

###########
##NOMBRES##
###########

##Array tío Rafa
# nombres=read.csv("nombres.csv",header=T,encoding = "utf-8",stringsAsFactors = F)
# nombres=nombres$Nombre
# nombres=nombres[-grep(" ",nombres)]
# nombres=tolower(nombres)
# nombres_apellidos=c(nombres)#,apellidos)

##Array IDs

IDs=read.csv("../../NombresApellidos/IDs.csv",header=T,encoding = "utf-8",stringsAsFactors = F, sep = ";")
IDs$FICHERO=files_names
write.csv(IDs, file = "../../NombresApellidos/IDsfilesnames.csv")

#############################################################
#############################################################



#Ejecutar ordenExcels.R

setwd("/Users/ignacio/Documents/Proyecto Entreculturas/Curriculums/ficheros/2017_Contabilidad_Externa")


#############################################################
#############################################################
##Completar csv
IDs=read.csv("../../NombresApellidos/IDsfilesnames.csv",header=T,encoding = "utf-8",stringsAsFactors = F)
id=IDs$ID
nombre=IDs$NOMBRE
apellido1=IDs$APELLIDO.1
apellido2=IDs$APELLIDO.2
fase1=IDs$FASE.1
fase2=IDs$FASE.2
fase3=IDs$FASE.3
seleccion_final=IDs$RESULTADO
seleccion_ainara=IDs$RESULTADO_AINARA

# nombre=nombre[-grep(" ",nombre)]
# apellido1=apellido1[-grep(" ",apellido1)]
# apellido2=apellido2[-grep(" ",apellido2)]
nombres_apellidos=c(nombre,apellido1, apellido2)
nombres_apellidos=tolower(nombres_apellidos)

###########
##CORPUS:##
###########

corpus <- Corpus(VectorSource(txt_files))
#corpus <- Corpus(VectorSource(corpus))
#writeLines(as.character(corpus[[5]]))
#writeLines(as.character(corpus_anal[[5]]))

corpus_anal=corpus

unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                          'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                          'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                          'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                          'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )

no_accent<-function(s)
{
  out <- s
  #iconv(s, to='ASCII//TRANSLIT')
  for(i in seq_along(unwanted_array))
    out <- gsub(names(unwanted_array)[i],unwanted_array[i],out)
  
  out
}

corpus_anal <- tm_map(corpus_anal, content_transformer(tolower))
corpus_anal <- tm_map(corpus_anal, removeWords, stopwords("es"))
corpus_anal <- tm_map(corpus_anal, removeWords, stopwords("en"))
corpus_anal <- tm_map(corpus_anal, content_transformer(no_accent))
corpus_anal <- tm_map(corpus_anal, removeWords, nombres_apellidos)
corpus_anal <- tm_map(corpus_anal, removePunctuation)
corpus_anal <- tm_map(corpus_anal, removeNumbers)
corpus_anal <- tm_map(corpus_anal, stripWhitespace)

#my_dtm <- DocumentTermMatrix(VCorpus(VectorSource(corpus_anal)), control=list(wordLengths=c(3,20)))
my_dtm <- DocumentTermMatrix(corpus_anal, control=list(wordLengths=c(3,20)))

#Para ver las 10 palabras que mas se repiten:
# m <- as.matrix(my_dtm)
# v <- sort(colSums(m), decreasing=TRUE)
# head(v, 10)

#Para quitar las palabras y ponerlas en formato "Word.id.nchar"
#my_dtm$dimnames$Terms=paste("Word",seq(1:length(my_dtm$dimnames$Terms)),as.character(sapply(my_dtm$dimnames$Terms,function(x) nchar(x))),sep=".")

my_dtm_df=as.data.frame(as.matrix(my_dtm))
save(my_dtm_df,file="../../NombresApellidos/my_dtm_df.RData")
write.csv(my_dtm_df, file = "../../NombresApellidos/my_dtm_df.csv")

df_final <- cbind(id = id, seleccion_ainara=seleccion_ainara, fase1 = fase1, fase2 = fase2, fase3 = fase3, seleccion_final = seleccion_final, size = size, npages = npages, nword = nword , nchar = nchar, my_dtm_df)

#row.names(df_final) <- row.names(my_dtm)

save(df_final,file="../../NombresApellidos/df_final.RData")
write.csv(df_final, file = "../../NombresApellidos/df_final.csv")

