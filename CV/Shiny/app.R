library(shiny)
library(flexdashboard)
library(zip)
  
# Machine learning
require(xgboost)
require(randomForest)
require(deepnet)

library(DT)
library(ggplot2)

library(plyr)
library(reshape2)

library(scales)

#NECESARIO INSTALAR RTOOLS!!!!
library(openxlsx) 

#Se pueden subir hasta 150 MB
options(shiny.maxRequestSize=150*1024^2)



human_numbers <- function(x = NULL, smbl =""){
  humanity <- function(y){             
    
    if (!is.na(y)){
      
      b <- round_any(abs(y) / 1000000000, 0.1)
      m <- round_any(abs(y) / 1000000, 0.1)
      k <- round_any(abs(y) / 1000, 0.1)
      
      if ( y >= 0 ){ 
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      if  ( k < 1){
        paste (y_is_positive, smbl,  y , sep = "")
      } else if  ( m < 1){
        paste (y_is_positive, smbl,  k , "k", sep = "")
      } else if (b < 1){
        paste (y_is_positive, smbl, m ,"m", sep = "")
      } else {
        paste (y_is_positive, smbl,  comma(b), "b", sep = "")    
      }
    } else {
      NA
    }
  }
  
  sapply(x,humanity)
}

human_percent <- function(x = NULL){
  humanity <- function(y){             
    
    if (!is.na(y)){
      
      k <- round_any(abs(y) * 100, 0.01)
      
      if ( y >= 0 ){ 
        y_is_positive <- ""
      } else {
        y_is_positive <- "-"
      }
      
      paste (y_is_positive, k ,"%", sep = "")
      
    } else {
      NA
    }
  }
  
  sapply(x,humanity)
}





#' Human versions of large currency numbers - extensible via smbl

human_gbp   <- function(x){human_numbers(x, smbl = "£")}
human_usd   <- function(x){human_numbers(x, smbl = "$")}
human_euro  <- function(x){human_numbers(x, smbl = "€")} 
human_num   <- function(x){human_numbers(x, smbl = "")} 
human_log   <- function(x){human_numbers(round(x,3), smbl = "")} 
human_log_percent   <- function(x){human_percent(round(x/100,6))} 


human_millon_euro  <- function(x){human_numbers(x*1000000, smbl = "€")} 


breaks_tmp<-function(lim)
{
  li=0
  ls=lim[2]
  if (ls<=10) return (seq(li,ls,1))
  if (ls<=20) return (seq(li,ls,2))
  if (ls<=150) return (seq(li,ls,5))
  if (ls<=200) return (seq(li,ls,10))
  return (seq(li,ls,200))
}

#Para ver las variables escogidas
variables_escogidas<-function(model_xgb,model_rf, names_filter_aux)
{
  #Miro las variables escogidas
  importance_xgb <- xgb.importance(model = model_xgb,feature_names = names_filter_aux)
  importance_rf <- as.data.frame(importance(model_rf))
  importance_rf$n=row.names(importance_rf)
  importance_rf=importance_rf[order(importance_rf$IncNodePurity),]

  plot_importance_xgb=data.frame(n=importance_xgb$Feature,
                                 v=importance_xgb$Gain)
  plot_importance_xgb$n=factor(plot_importance_xgb$n,levels=rev(importance_xgb$Feature))
  #plot_xgb=ggplot(plot_importance_xgb,aes(n,v))+geom_bar(stat="identity")+coord_flip()+labs(title="XGB",y="Aportación",x="Campo")
  plot_importance_rf=data.frame(n=importance_rf$n,
                                v=importance_rf$IncNodePurity)
  plot_importance_rf=plot_importance_rf[order(plot_importance_rf$v,decreasing=T),]
  plot_importance_rf$n=factor(plot_importance_rf$n,levels=plot_importance_rf[order(plot_importance_rf$v,decreasing=F),"n"])

  #plot_rf=ggplot(plot_importance_rf,aes(n,v))+geom_bar(stat="identity")+coord_flip()+labs(title="Random Forest",y="Aportación",x="Campo")
  
  plot_importance=merge(plot_importance_rf,plot_importance_xgb,by=c("n"),all=T)
  plot_importance[which(is.na(plot_importance$v.x)),"v.x"]=0
  plot_importance[which(is.na(plot_importance$v.y)),"v.y"]=0
  plot_importance$v.x=plot_importance$v.x/max(plot_importance$v.x,na.rm=T)
  plot_importance$v.y=plot_importance$v.y/max(plot_importance$v.y,na.rm=T)
  #plot_importance=subset(plot_importance,v.x>=0.25|v.y>=0.25)
  
  plot_importance_order=plot_importance$n[order((plot_importance$v.x+plot_importance$v.y)/2,decreasing=F)]
  
  plot_importance=melt(plot_importance,id=c("n"))
  
  plot_importance$n=factor(plot_importance$n,levels=plot_importance_order)
  
  plot_importance$variable=ifelse(plot_importance$variable=="v.x","RF","XGB")
  
  names(plot_importance)[which(names(plot_importance)=="variable")]="Modelo"
  
  plot_all=ggplot(plot_importance,aes(n,value,fill=Modelo,group=Modelo))+geom_bar(stat="identity",position="dodge")+coord_flip()+labs(y="Aportación",x="Campo")+
    theme(plot.title=element_text(size=20,hjust=0.5),
          axis.text=element_text(size=16),
          axis.text.x=element_text(size=18),
          axis.text.y=element_text(size=18),
          axis.title=element_text(size=18),
          legend.text=element_text(size=18),
          strip.text.y=element_text(size=12,angle=0),
          legend.title=element_text(size=18),
          legend.position="bottom")
  
  return(plot_all)
}





#Formula de predicci?n sin el ensemble
predice_no_ens<- function(tmp_data,tmp_train,tmp_reg,tmp_res)
{
  #Si quiero todas las columnas
  if (is.na(tmp_reg[1]))
  { 
    tmp_reg=seq_along(tmp_data)
  }
  
  #De test son los que no son de train
  tmp_test=seq(1:length(tmp_res))[-tmp_train]
  
  #Preparo el dataset de entrenamiento (modo matriz)
  tmp_data_train=tmp_data[tmp_train,tmp_reg]
  tmp_data_res=tmp_res[tmp_train]
  
  #Preparo el dataset de test
  tmp_data_test=tmp_data[tmp_test,tmp_reg]
  tmp_data_test_res=tmp_res[tmp_test]
  
  
  dtrain <- xgb.DMatrix(data = data.matrix(tmp_data_train),
                        label=tmp_data_res)
  
  my_model_xgb<<-xgboost(dtrain,nrounds=50,params=list(eta = 0.3,max_depth = 6, gamma = 0, colsample_bytree = 0.8, min_child_weight = 1 , subsample = 1,objective="reg:logistic"),verbose=0)
  #my_model_xgb<<-xgboost(dtrain,nrounds=50,params=list(objective="reg:logistic"),verbose=0)
  my_model_rf<<-randomForest(x=tmp_data_train,y=tmp_data_res,mtry=2)
  
 
  #Predigo
  
  predichos_xgb=predict(my_model_xgb,data.matrix(tmp_data_test))
  #predichos_ab=predict(my_model_ab,tmp_data_test)$prob[,2]
  predichos_rf=predict(my_model_rf,tmp_data_test)
  
  res_pred_1=rbind(data.frame(M=predichos_xgb,tipo="XGB"),
                   data.frame(M=predichos_rf,tipo="RF"),
                   data.frame(M=tmp_res[tmp_test],tipo="Real"))
  
  res_pred_2=rbind(data.frame(M=predichos_xgb,D=tmp_res[tmp_test],tipo="XGB",subtipo="1"),
                   data.frame(M=predichos_rf,D=tmp_res[tmp_test],tipo="RF",subtipo="1"))
  
  return(list(res_pred_1,res_pred_2,my_model_xgb,my_model_rf))
  
}


predice_general_no_folds <-function(data,label,columns,inTrain)
{
  set.seed(343)
  
  if (is.na(columns[1]))
  {
    columns=seq_along(data)
  }
  
  res_aprende=predice_no_ens(data,inTrain,columns,label)
  
  #Tengo que corregir el ensemble
  predichos_xgb=predict(res_aprende[[3]],data.matrix(data[inTrain,columns]))
  predichos_rf=predict(res_aprende[[4]],data[inTrain,columns])
  
  
  
  #El ensemble lo hago sobre el total
  x <- matrix(c(predichos_xgb,predichos_rf),ncol=2)
  #x <- matrix(c(predichos_xgb,predichos_ab,predichos_rf),ncol=2)
  y <- as.numeric(label[inTrain])
  nn <- dbn.dnn.train(x,y,hidden = c(1),
                      activationfun = "sigm",learningrate = 0.2,momentum = 0.8)
  nn_predict <- nn.predict(nn,x)
  
  
  #Predigo
  
  predichos_xgb=predict(res_aprende[[3]],data.matrix(data[-inTrain,columns]))
  predichos_rf=predict(res_aprende[[4]],data[-inTrain,columns])
  
  #x_p <- matrix(c(predichos_xgb,predichos_ab,predichos_rf),ncol=2)
  x_p <- matrix(c(predichos_xgb,predichos_rf),ncol=2)
  nn_predict_p <- nn.predict(nn,x_p)
  #res_pred=data.frame(M=nn_predict_p,D=subset(res_pred,tipo=="XGB")$D,tipo="ENS",subtipo=1)
  
  res_pred=nn_predict_p
  
  res_pred
}

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

  ui <- fluidPage(titlePanel(fluidRow(column(width=12,align="center",img(src="quant.png", height = 100)))),
                  
      pageWithSidebar(
      headerPanel(fluidRow(column(width=3,align="center",offset=0.5,"Quanticae RRHH"))),
      sidebarPanel(width=3,
                   fileInput('file1', 'Suba una carpeta comprimida .zip con todos los documentos en formato .pdf o .docx', accept=c('.zip'), multiple = TRUE),
                   tags$style("input[type='radio']:checked+span{ 
                              font-weight: bold; 
                              }
                              input[type='radio']+span{ 
                              color: black; 
                              }") ,
                   tags$hr(),
                   h5(tags$b("IA: ")),
                   radioButtons("AI", label = NULL,
                               choices = c("Si" = T,
                                  "No" = F)),
                   helpText("Nota: Seleccione si quiere ayuda de la IA."),
                   uiOutput("semaphore"),
                   tags$br(),
                   gaugeOutput("medidor"),
                   actionButton('iniciar', "Reiniciar"),
                   helpText("Pulse para empezar otra vez."),
                   tags$hr(),
                   h5(tags$b("Descargar candidatos que pasan la fase:")),
                   downloadButton("downloadData", "Descargar"),
                   tags$br()
                   
                   ),
      mainPanel(
        
        tabsetPanel(tabPanel("Vista de documentos", 
                             uiOutput("html_pdf"),
                             tags$br(),
                             actionButton("Pasa_button", label = "Pasa la fase", style = "color: black; 
                                          background-color: green; 
                                          position: relative; 
                                          left: 3%;
                                          height: 35px;
                                          width: 150px;
                                          text-align:center;
                                          text-indent: -2px;
                                          border-radius: 6px;
                                          border-width: 2px"),
                             actionButton("NoPasa_button", label = "No pasa la fase", style = "color: black; 
                                          background-color: red; 
                                          position: relative; 
                                          left: 3%;
                                          height: 35px;
                                          width: 150px;
                                          text-align:center;
                                          text-indent: -2px;
                                          border-radius: 6px;
                                          border-width: 2px"),
                             actionButton("Atras_button", label = "Atrás", style = "color: black; 
                                          background-color: white; 
                                          position: relative; 
                                          left: 3%;
                                          height: 35px;
                                          width: 150px;
                                          text-align:center;
                                          text-indent: -2px;
                                          border-radius: 6px;
                                          border-width: 2px"),
                             tags$hr()
                             ),
                    tabPanel('Tabla de documentos',dataTableOutput("mytable", height = "800px")),
                    tabPanel('Encontrados',plotOutput("myplot", height = "600px")),
                    tabPanel('Variables usadas',plotOutput("myvar", height = "800px")),
                    tabPanel('Datos del proceso',verbatimTextOutput("data_process"),
                             tags$p("Descargar archivo excel con los datos del proceso"),
                             downloadButton("downloadCSV", "Descargar"),
                             tags$hr(),
                             tags$h4("Guardar proceso:"),
                             tags$p("Introduzca el nombre del proceso de selección: "),
                             textInput("name_process", label = NULL),
                             actionButton("button_saveProcess", "Guardar proceso"),
                             helpText("Nota: Al guardar el proceso de selección podrá reutilizar los datos de entrenamiento de la IA en futuros procesos de selección.")
                             
                             
                             ))
        
      )
    )
  )

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

  server <- function(input, output, session) {
  
    #Variables globales:
    my_dtm_df <- c()
    df_final <- c()
    datos_df <- c()
    palabras=10
    names_filter<-c()
    terms <- c()
    my_dtm_filter <- c()
    df_rrhh <-c()
    n=0
    n_ai=c()
    set.seed(343)
    inResto<-c()
    inTrain<-c()
    my_model_xgb<<-NULL
    my_model_rf<<-NULL
   
    
    #df con tres columnas, el id del participante (nombre de las filas), el nombre del fichero y si pasa la fase de selecci?n
    #Ahora my_label es files_df$pasaFase
    files_df <- c()
    #id del que se está analizando ahora
    id_analizing <- c()
    #Boolean que indica si se ha llegado al final del proceso
    finProcess <- FALSE
    #Boolean que indica si se está utilizando IA
    uso_IA <- reactiveVal(FALSE)
    #Para el gauge
    probabilidad <- reactiveVal(0)
    listaProb <- c()
    #Boolean que dice si el proceso ha comenzado
    start_process <- FALSE
    
    #Se extrae el zip con los pdf en www/CV_list (Se crea la carpeta temporal CV_list que guarda todos los archivos de la sesión)
    #CAMBIAR -> NÓTESE QUE CV_LIST ES UN FICHERO TEMPORAL QUE SE BORRA AL CERRAR LA SESIÓN.
    observe({
      req(input$file1)
      start_process <<- TRUE
      unlink("www/CV_list", recursive = TRUE)
      unzip(input$file1$datapath, exdir="www/CV_list")
      names_files_pdf <- list.files("www/CV_list", pattern = "pdf$")
      names_files_docx <- list.files("www/CV_list", pattern = "docx$")
      names_files <- c(names_files_pdf, names_files_docx)
      
      #CAMBIAR -> Cambio de los nombres para poder leer en windows
      if(Sys.info()['sysname'] == "Windows") {
        sustitucionNombres <- c(letters, LETTERS, as.character(0:9), ",", "-", ".", " ", "_")
        names_files_aux<- names_files
        
        # names_files_aux<- gsub(",","e", names_files_aux)
        # names_files_aux<- gsub("?","i", names_files_aux)
        # names_files_aux<- gsub("?","o", names_files_aux)
        # names_files_aux<- gsub("?","u", names_files_aux)
        # names_files_aux<- gsub("?","A", names_files_aux)
        # names_files_aux<- gsub("?","I", names_files_aux)
        # names_files_aux<- gsub("?","O", names_files_aux)
        # names_files_aux<- gsub("?","U", names_files_aux)
        names_files_aux <- gsub(paste0("[^", paste(sustitucionNombres, collapse=""), "]+"), "", names_files_aux)
    
        file.rename(from =  file.path("www/CV_list", names_files), to = file.path("www/CV_list", names_files_aux))
        names_files <- names_files_aux
      }
      
      
      files_df <<- data.frame(file_name = names_files)
      
      #id es el nombre de la columna
      row.names(files_df) <- seq.int(length(names_files))
      
      #Creamos corpus
      write.csv(files_df, file = "www/CV_list/listaArchivos.csv", row.names = FALSE)
      source("crearCorpus.R") #, local = TRUE)
      unlink("www/CV_list/listaArchivos.csv")
      
      names_files <- paste("CV_list", names_files,sep = "/")
      files_df <<- data.frame(file_name = names_files, pasaFase = rep(NA, length(names_files)))
      row.names(files_df) <- seq.int(length(names_files))
     
      #Cargo los corpus
      load("www/CV_list/my_dtm_df.RData")
      load("www/CV_list/df_final.RData")
    
      my_dtm_df<<-my_dtm_df[,-which(sapply(names(my_dtm_df),function(x) nchar(gsub("[^[:alnum:]]","",x)))<=3)]

      datos_df<<-df_final[,c(1:5)]
      
      my_dtm_df<<-my_dtm_df[,which(colSums(my_dtm_df)>0)]
      terms<<-colSums(my_dtm_df>0)
      #Elijo los terminos. Me quedo solo con n palabras
      my_dtm_filter<<-my_dtm_df[,which(terms>=tail(head(sort(terms,decreasing=T),n=palabras),1))]
        
      names_filter<<-names(my_dtm_filter)
      df_rrhh<<-cbind(file_names = c(names_files_pdf, names_files_docx), id = row.names(my_dtm_df), fase1 = rep(NA, length(names_files)) ,my_dtm_filter)
      df_rrhh$seleccion<<-0
      
      #Lista de los archivos a analizar
      inResto <<- sample(1:length(files_df$file_name),length(files_df$file_name))
      inTrain<<-c()
      
      })
    
    ###################
    ##Función procesa##
    ###################
    
    procesa<<-function()
    {

      if (!length(which(files_df$pasaFase[inTrain]==0))|!length(which(files_df$pasaFase[inTrain]==1))|isolate(input$AI)==F)
      {
        #inTrain<<-c(inTrain,inResto[1])
        
        if(length(inResto) > 0){
        id_analizing <<- inResto[1]
        #inResto<<-inResto[-1]
        inResto_orden=inResto
        
        if(isolate(input$AI)==F){
          id_analizing <<- inResto[sample(1:length(inResto), 1)]
        }
        
        }
        n<<-n+1
        n_ai[n]<<-"blue"
        uso_IA(FALSE)
        probabilidad(0)
        print("Espera")
        
        #      if (sum(files_df$pasaFase[inTrain])==sum(files_df$pasaFase)) showNotification("Ya han aparecido todos los candidatos",duration=5,type="error")
        
      } else {
        #Es el primero
        #if (which(files_df$pasaFase[inTrain]==1)[1]==n)
        #{
        #        res_predict=predice_general_no_folds(my_dtm_filter,files_df$pasaFase,NA,inTrain)
        #     } 
        
        if(length(inResto) > 0){
    
        #Da un array con todos los valores de regresión
        res_predict=predice_general_no_folds(my_dtm_filter,files_df$pasaFase,NA,inTrain)

        probabilidad(sort(res_predict,decreasing=T)[1])
        
        listaProb <<- c(sort(res_predict,decreasing=T)[1],listaProb)
     
        quedan=length(files_df$pasaFase)-length(inTrain)
        
        inResto<<-seq(1:length(files_df$pasaFase))[-inTrain]
        
        #-------------------      #Se mete en inTrain la siguiente
        id_analizing<<-inResto[order(res_predict,decreasing=T)[1]]
        inResto_orden=inResto[order(res_predict,decreasing=T)]
        }

        print("Predigo")
    
        n<<-n+1
        n_ai[n]<<-"red"
        uso_IA(TRUE)
        
        ######
        ######NO SE SI SE PONE LO SIGUIENTE:
        ######
        #if (sum(files_df$pasaFase[inTrain])==sum(files_df$pasaFase)) showNotification("Ya han aparecido todos los candidatos",duration=5,type="error")
        
        
      }
      
     
    }
    
    ###################
    ###################
    ###################
    
    
    observeEvent(input$iniciar, {
      if(start_process){
      n<<-0
      n_ai<<-c()
      my_model_rf<<-NULL
      set.seed(343)
      
      uso_IA(FALSE)
      probabilidad(0)
      finProcess <<- FALSE
      
      names_files_pdf <- list.files("www/CV_list", pattern = "pdf$")
      names_files_docx <- list.files("www/CV_list", pattern = "docx$")
      names_files <- c(names_files_pdf, names_files_docx)
      names_files <- paste("CV_list", names_files,sep = "/")
      files_df <<- data.frame(file_name = names_files, pasaFase = rep(NA, length(names_files)))
      row.names(files_df) <- seq.int(length(names_files))
      
      load("www/CV_list/my_dtm_df.RData")
      load("www/CV_list/df_final.RData")

      my_dtm_df<<-my_dtm_df[,-which(sapply(names(my_dtm_df),function(x) nchar(gsub("[^[:alnum:]]","",x)))<=3)]
      
      datos_df<<-df_final[,c(1:5)]
      
      my_dtm_df<<-my_dtm_df[,which(colSums(my_dtm_df)>0)]
      terms<<-colSums(my_dtm_df>0)
      #Elijo los terminos. Me quedo solo con n palabras
      my_dtm_filter<<-my_dtm_df[,which(terms>=tail(head(sort(terms,decreasing=T),n=palabras),1))]
      
      names_filter<<-names(my_dtm_filter)
      df_rrhh<<-cbind(file_names = c(names_files_pdf, names_files_docx), id = row.names(my_dtm_df), fase1 = rep(NA, length(names_files)) ,my_dtm_filter)
      df_rrhh$seleccion<<-0
      
      #Lista de los archivos a analizar
      inResto <<- sample(1:length(files_df$file_name),length(files_df$file_name))
      inTrain<<-c()
      
      #Empezamos a analizar el primer archivo
      id_analizing <<- inResto[1] #Empezamos a analizar el primer archivo
      inResto <<- inResto[-1]
      new_file <- files_df$file_name[which(row.names(files_df)==id_analizing)]
      print(new_file)
      
      output$html_pdf <- renderUI({
        tags$iframe(style="height:600px; width:100%", src=new_file)
      })
      }
    })
    
    #Display los pdf cuando se sube el archivo zip
    observeEvent(input$file1,
                 output$html_pdf <- renderUI({
                   id_analizing <<- inResto[1] #Empezamos a analizar el primer archivo
                   inResto <<- inResto[-1]
                   new_file <- files_df$file_name[which(row.names(files_df)==id_analizing)]
                   print(new_file)
                   tags$iframe(style="height:600px; width:100%", src=new_file)
                 }))
    
    #Botón "Pasa la fase"
    observeEvent(input$Pasa_button,
                 output$html_pdf <- renderUI({
                   #Pasa la fase:
                   files_df$pasaFase[which(row.names(files_df)==id_analizing)] <<- 1
                   inTrain <<- c(inTrain, id_analizing)
                   if(!finProcess){
                     procesa()
                   }
                   if(length(inResto) > 0){
                     inResto <<- inResto[-which(inResto == id_analizing)]
                     new_file <- as.character(files_df$file_name[which(row.names(files_df)==id_analizing)])
                     print(new_file)
                     tags$iframe(style="height:600px; width:100%", src=new_file)
                   }else{
                     finProcess <<- TRUE
                     id_analizing <<- NULL
                     print("Fin del proceso")
                     uso_IA(FALSE)
                     probabilidad(0)
                     listaProb <<- c(0,listaProb)
                   }
                 }))
    
    #Botón "No pasa la fase"
    observeEvent(input$NoPasa_button,
                 output$html_pdf <- renderUI({
                   #No pasa la fase:
                   files_df$pasaFase[which(row.names(files_df)==id_analizing)] <<- 0
                   inTrain <<- c(inTrain, id_analizing)
                   if(!finProcess){
                     procesa()
                   }
                   if(length(inResto) > 0){
                     inResto <<- inResto[-which(inResto == id_analizing)]
                     new_file <- as.character(files_df$file_name[which(row.names(files_df)==id_analizing)])
                     print(new_file)
                     tags$iframe(style="height:600px; width:100%", src=new_file)
                   }else{
                     finProcess<<-TRUE
                     id_analizing <<- NULL
                     print("Fin del proceso")
                     uso_IA(FALSE)
                     probabilidad(0)
                     listaProb <<- c(0,listaProb)
                   }
                   }))
   
    observeEvent(input$Atras_button,
                output$html_pdf <- renderUI({
                   
                   finProcess <<- FALSE
                   
                   if(length(inTrain) > 0){
                     
                    n_ai <<- n_ai[-n]
                    n <<- n-1
                    
                    files_df$pasaFase[which(row.names(files_df)==inTrain[length(inTrain)])] <<- NA
                    
                    if(!length(which(files_df$pasaFase[inTrain]==0))|!length(which(files_df$pasaFase[inTrain]==1))|isolate(input$AI)==F){
                      uso_IA(FALSE)
                      probabilidad(0)
                    }else{
                      uso_IA(TRUE)
                      listaProb<<-listaProb[-1]
                      probabilidad(listaProb[1])
                    }
                    
                    
                    if(!is.null(id_analizing)){
                      inResto <<- c(id_analizing, inResto) 
                    }
                    
                    id_analizing <<- inTrain[length(inTrain)]
                    inTrain <<- inTrain[-length(inTrain)]
                    
                                    
                    if((0 %in% files_df$pasaFase)&(1 %in% files_df$pasaFase)&(length(inResto)>0)){
                      inResto <<- seq(1:length(files_df$pasaFase))[-inTrain]
                    }
                
                    # if(length(inTrain) == 0){
                    #   inResto <<- inResto[-1]
                    # }
                    
                    new_file <- as.character(files_df$file_name[which(row.names(files_df)==id_analizing)])
                    tags$iframe(style="height:600px; width:100%", src=new_file)
                   } else {
                     new_file <- files_df$file_name[which(row.names(files_df)==id_analizing)]
                     print(new_file)
                     tags$iframe(style="height:600px; width:100%", src=new_file)
                   }
                  
                   }))
   #Para semáforo  
   observeEvent(input$AI,
                if(!length(which(files_df$pasaFase[inTrain]==0))|!length(which(files_df$pasaFase[inTrain]==1))){
                  uso_IA(FALSE)
                }else if(input$AI){
                  uso_IA(TRUE)
                }else{
                  uso_IA(FALSE)
                })
    
   observeEvent(uso_IA(),
                output$semaphore <- renderUI({

                  if(uso_IA()){
                    h5(tags$b("Uso de IA: Si"), br(), tags$img(src='sem_verde.png', width="40px", height="20px"))
                  }else{
                    h5(tags$b("Uso de IA: No"), br(), tags$img(src='sem_rojo.png', width="40px", height="20px"))
                    
                  }
                }))
    
    
    #Tabla con los CV que pasa /no pasan
    output$mytable = DT::renderDataTable({
      variable=input$file1
      variable=input$Pasa_button
      variable=input$NoPasa_button
      variable=input$Atras_button
      variable=input$iniciar
      
      if(length(inTrain)>0){
        
      df_presenta=files_df[inTrain,c("file_name","pasaFase")][seq(length(inTrain),1,-1),]

      names(df_presenta)=c("Candidato","Seleccionado")
      df_presenta$Candidato = sub("CV_list/", "",df_presenta$Candidato)
      #Quitamos ".pdf" y ".docx" de los nombres
      df_presenta$Candidato = sub(".pdf", "",df_presenta$Candidato)
      df_presenta$Candidato = sub(".docx", "",df_presenta$Candidato)
      
      df_presenta$Seleccionado=ifelse(df_presenta$Seleccionado,"ELEGIDO","DESCARTADO")
      
      datatable(df_presenta, rownames = FALSE,filter="bottom")%>% formatStyle(
        'Seleccionado',
        target = 'row',
        backgroundColor = styleEqual(c("ELEGIDO","DESCARTADO"), c('red', 'white'))
      )
      }
    })
    
    
    #Gráfico de los que pasan/no pasan
    output$myplot <- renderPlot({
      variable=input$file1
      variable=input$Pasa_button
      variable=input$NoPasa_button
      variable=input$Atras_button
      variable=input$iniciar
      
      if((length(inTrain)>0)&(n>0)){
        
        res=data.frame(n=c(0:n),encontrado_i=c(0,files_df$pasaFase[inTrain]),ai=c("blue",n_ai))
        
        for (i in 1:n)
        {
          res[i+1,"encontrado_i"]=res[i,"encontrado_i"]+res[i+1,"encontrado_i"]
        }
        
        res$encontrado_f=c(res$encontrado_i[-1],res$encontrado_i[nrow(res)])
        
        res=res[-nrow(res),]


        g=(ggplot(res)+geom_segment(aes(x=n,xend=(n+1),y=encontrado_i,yend=encontrado_f),size=1.5,color=n_ai)+
            
             coord_cartesian(xlim=c(0,nrow(df_rrhh)),ylim=c(0,length(which(files_df$pasaFase==1))+1))+
             labs(title="Encontrados",x="CV",y="Candidatos")+
             geom_line(data=data.frame(x=c(0,length(files_df$pasaFase)),
                                       y=c(0,sum(files_df$pasaFase))),aes(x=x,y=y),linetype="dashed",size=1.1,color="black")+
             scale_x_continuous(label=human_numbers,breaks=breaks_tmp)+
             scale_y_continuous(label=human_numbers,breaks=breaks_tmp)+
             theme(plot.title=element_text(size=20,hjust=0.5),
                   axis.text=element_text(size=16),
                   axis.text.x=element_text(size=18),
                   axis.text.y=element_text(size=18),
                   axis.title=element_text(size=18),
                   legend.text=element_text(size=18),
                   strip.text.y=element_text(size=12,angle=0),
                   legend.title=element_text(size=18),
                   legend.position="none"))
        
        print(g)
        
    
    }
    })
    
    output$myvar <- renderPlot({
      variable=input$file1
      variable=input$Pasa_button
      variable=input$NoPasa_button
      variable=input$Atras_button
      variable=input$iniciar
      
      if((length(inTrain)>0)&(n>0)){
        g=try(variables_escogidas(my_model_xgb,my_model_rf, names_filter))
        print(class(g))
        print(g)
      }
      
      
    })
    
    #Función del gauge
    output$medidor <- renderGauge({
      
      variable=input$file1
      variable=input$Pasa_button
      variable=input$NoPasa_button
      variable=input$Atras_button
      variable=input$iniciar
      variable=probabilidad()
      
      gauge(value=trunc(probabilidad()*100), min = 0, max = 100, symbol = "%", sectors=gaugeSectors(
        danger = c(80, 100), warning = c(40, 79), success = c(0, 39)
      ))
    })
    
    #Función un poco cutre para sacar los datos del proceso por pantalla
    output$data_process <- renderText({ 
      variable=input$file1
      variable=input$Pasa_button
      variable=input$NoPasa_button
      variable=input$Atras_button
      variable=input$iniciar
      
       listaPasanFase <- as.character(files_df$file_name[which(files_df$pasaFase == 1)])
       listaPasanFase = sub("CV_list/", "",listaPasanFase)
       #Quitamos ".pdf" y ".docx" de los nombres
       listaPasanFase = sub(".pdf", "",listaPasanFase)
       listaPasanFase = sub(".docx", "",listaPasanFase)
       listaPasanFase_aux <- c()
        
       if(length(listaPasanFase)>0){
       for(i in 1:length(listaPasanFase)){
         listaPasanFase_aux <- c(listaPasanFase_aux, "      ", i, ".  ", listaPasanFase[i], "\n")
       }
       }else{
         listaPasanFase_aux <- c()
       }
     
       if((length(files_df$pasaFase[which(!is.na(files_df$pasaFase))])>0)){
       datos <- c(" Número de candidatos (total): ", length(files_df$file_name), "\n", 
                  "Número de candidatos analizados: ", length(files_df$pasaFase[which(!is.na(files_df$pasaFase))]), "\n",
                  "Número de candidatos que han pasado el proceso de selección: ", length(files_df$pasaFase[which(files_df$pasaFase==1)]), "\n",
                  "Lista de candidatos que han pasado el proceso de selección: ", "\n", listaPasanFase_aux
                  )
       }else{
         datos <- c(" Número de candidatos (total): ", "\n", 
                    "Número de candidatos analizados: ", "\n",
                    "Número de candidatos que han pasado el proceso de selección: ", "\n",
                    "Lista de candidatos que han pasado el proceso de selección: ", "\n"
         )
         
       }

       print(datos)
       
      
      })
    
    
     #Para descargar los documentos que pasan la fase
     output$downloadData <- downloadHandler(
       
       filename = function() {
         "CV que pasan la fase.zip"
       },
       content = function(file) {
         setwd("www")
         zip(file,  as.character(files_df$file_name[which(files_df$pasaFase == 1)]))
         setwd("..")
       },
       contentType = "application/zip"
     )
     
     
     #Para descargar el csv con los datos del proceso
     output$downloadCSV <- downloadHandler(
     
       filename = function() {
         "datosDelProceso.xlsx"
       },
       content = function(file) {
         data <- files_df
         #Cambiamos los nombres:
         data$file_name = sub("CV_list/", "",as.character(data$file_name))
         #Quitamos ".pdf" y ".docx" de los nombres
         data$file_name = sub(".pdf", "",data$file_name)
         data$file_name = sub(".docx", "",data$file_name)
         
         for(i in 1:length(data$pasaFase)){
           if(is.na(data$pasaFase[i])){
             data$pasaFase[i] <- "No analizado"
           }else if(data$pasaFase[i] == 0){
             data$pasaFase[i] <- "No seleccionado"
           }else{
             data$pasaFase[i] <- "Seleccionado"
           }
         }
         
         data <- data[rev(order(data$pasaFase)),]
          
         #Creo el excel
         colnames(data) <- c("Participante", "Resultado")
         ## Create a blank workbook
         wb <- createWorkbook()
         
         sheet <- addWorksheet(wb,  sheetName = "Resultados")
         
         writeData(wb, sheet = 1, data, rowNames = FALSE)
         
         headerStyle <- createStyle(textDecoration = "bold")
         
         addStyle(wb, sheet = 1, headerStyle, rows = 1, cols = 1:2, gridExpand = TRUE)
         
         setColWidths(wb, 1, cols=1, widths = 50)
         setColWidths(wb, 1, cols=2, widths = 20)
         
         saveWorkbook(wb, file, overwrite = TRUE)
         #write.csv(data, file)
       }
     )
     
     observeEvent(input$button_saveProcess, {
       
    
       if(input$name_process== ""){
         showNotification("Escriba el nombre del proceso de selección",
                          type = "message"
         )
       }else{
         if(!dir.exists(paste("www/oldProcesses/",input$name_process,sep=""))){
           #Creamos el directorio
           dir.create(paste("www/oldProcesses/",input$name_process,sep=""))
           #Copiamos los archivos
           file.copy("www/CV_list/", paste("www/oldProcesses/",input$name_process,sep=""), recursive=TRUE)
           
           
           #Guardamos los datos del proceso
           data <- files_df
           data$file_name = sub("CV_list/", "",as.character(data$file_name))
           save(data,file=paste(paste("www/oldProcesses/",input$name_process,sep=""),"/CV_list/data_selection_process.Rdata",sep=""))
           
           #¿Guardamos xgboost y rf?
           xgb.save(my_model_xgb, paste(paste("www/oldProcesses/",input$name_process,sep=""),"/CV_list/xgb.model",sep=""))
           save(my_model_rf,file =  paste(paste("www/oldProcesses/",input$name_process,sep=""),"/CV_list/rfModel.RData",sep=""))
           
          }else{
          showNotification("El nombre del proceso de selección que ha puesto ya existe. Elija otro.",
                          type = "message"
           )
          }
       }
       
     })
     
     
      
     #Al finalizar la sesion borramos la carpeta temporal
     session$onSessionEnded(function() {
       unlink("www/CV_list", recursive = TRUE)
     })
     
}
  
  
shinyApp(ui, server)
