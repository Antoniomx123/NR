#### NR LICENCIATURA ###
t <- proc.time()
#Cargamos CSV desde Front
Reporte <- read.csv("C:/Users/jvelazhe/Google Drive/reportes/lic/reporte_completo.csv", header = TRUE, sep = ",",encoding = "UTF-8")
Reporte <- subset(Reporte,grupo != "Grupo_S")
Reporte <- subset(Reporte,grupo != "Grupo_O")
#Reporte <- rbind(Reporte,gruposraros)
Reporte$semana_2 <- NULL
Reporte$id <- NULL
Reporte$ciudad <-NULL
Reporte$p_key <- NULL
Reporte$X <- NULL
#Convertimos matriculas a numeros
Reporte[,5] <- as.numeric(as.character(Reporte[,5]))
Reporte <- subset(Reporte,matricula != 0) 
names(Reporte)
Demos <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/Demos.csv", header = TRUE,sep = ",",encoding="UTF-8")
Demos[,1]<- as.numeric(as.character(Demos[,1]))
Reporte <- dplyr::left_join(Reporte,Demos, by="matricula")
Reporte[,42]<- as.character(as.character(Reporte[,42]))
Reporte$tipo[is.na(Reporte$tipo)]<-"No quitar"
Reporte <- subset(Reporte,tipo == "No quitar")
Reporte$nombre <- NULL
Reporte$tipo <- NULL
library(dplyr)
Reporte <- Reporte %>% group_by(id_us)%>% dplyr::mutate(cuenta = row_number())
Reporte <- subset(Reporte,cuenta == 1)
Reporte$id_us <- NULL
R1 <- grep("3108",Reporte$clave, ignore.case = TRUE)
Reporte1 <- Reporte[R1,]
R2 <- grep("1708",Reporte$clave, ignore.case = TRUE)
Reporte2 <- Reporte[R2,]
R3 <- grep("1409",Reporte$clave, ignore.case = TRUE)
Reporte3 <- Reporte[R3,]
R4 <- grep("2809",Reporte$clave, ignore.case = TRUE)
Reporte4 <- Reporte[R4,]
R5 <- grep("2206",Reporte$clave, ignore.case = TRUE)
Reporte5 <- Reporte[R5,]



Reporte <- rbind(Reporte1,Reporte2,Reporte3,Reporte4)
Reporte <- Reporte[!duplicated(Reporte),]

Reporte$clavemat <- Reporte$clave
Reporte <- tidyr::separate(Reporte,clavemat,c("ciclo","num","inicio_plus","clave_materia"),sep = "_")

FechasInicio <-readxl::read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/inicios.xlsx")
Reporte<- plyr::join(Reporte,FechasInicio, by="inicio_plus",type = "left",match = "all")
Reporte$inic <- Reporte$inicio_plus
Reporte$day <- Sys.Date()
Reporte <- tidyr::unite(Reporte,"Con",c(inic,day),sep="_")
library(readxl)
SemanasRanngos <- read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/SemanasRanngos.xlsx", 
                             col_types = c("text", "skip", "skip", 
                                           "numeric"))
Reporte <- dplyr::left_join(Reporte,SemanasRanngos,by="Con")
Reporte$semana <- Reporte$semans
Reporte$semans <- NULL
Reporte$Con <- NULL

Reporte$semanas <- Reporte$semana
Reporte <- tidyr::unite(Reporte,"CONCA",c(inicio_plus,semanas),sep="")




library(readxl)
ponderacioneslic <- read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/ponderacioneslic.xlsx", 
                               col_types = c("text", "numeric", "numeric", 
                                             "numeric"))


Reporte <- dplyr::left_join(Reporte,ponderacioneslic,by="CONCA")
Reporte$nr <- if_else(Reporte$ultimo_acceso_m == "Nunca","Nuncas",
                      if_else(Reporte$calificacion == 0,"Np",
                              if_else(Reporte$calificacion <= Reporte$REPROBO,"Reprob?",
                                      if_else(Reporte$calificacion > Reporte$REPROBO & Reporte$calificacion < Reporte$APROBADO,"Por aprobar",
                                              if_else(Reporte$calificacion >= Reporte$APROBADO,"Aprobado","Sin Registro")))))





Reporte$ciclo <- NULL
Reporte$num <- NULL
Reporte$CONCA<- NULL
Reporte$clave_materia<- NULL
Reporte$`Fecha Inicio`<- NULL
Reporte$Semanainicio<- NULL
Reporte$Semanaactual20<- NULL
Reporte$REPROBO<- NULL
Reporte$`POR APROBAR`<- NULL
Reporte$APROBADO<- NULL




Utel <- subset(Reporte,institucion == "UTEL")
Teleton <- subset(Reporte,institucion == "Teleton")

names(Utel)
Utel$matricula_prof<-NULL
Utel$profesor<-NULL
Utel$mail_profesor<-NULL
Utel$Matricula.GCA<-NULL
Utel$Nombre.completo.de.GCA<-NULL
Utel$Correo.electronico.GCA<-NULL
Utel$Matricula.RE<-NULL
Utel$Nombre.completo.de.RE<-NULL
Utel$Correo.electronico.RE<-NULL


#Operacion Academica Fase 1####
Fase0 <- Utel
Fase0$claves <- Fase0$clave
Fase0$grupos <- Fase0$grupo
Fase0 <- tidyr::unite(Fase0,"claves_g",c(claves,grupos),sep="")
Fase0$clave_g <- Fase0$claves_g
Fase0$claves_g <- NULL
opacad <-googlesheets4::read_sheet(ss="1l5rskQ9LHQLsBwgtmkwwiZQAkY21z2PWyU93p4CUtKk",range="A1:R2000",col_types = "cccccccccccdcccccc")
write.csv(opacad, file = "D:/Users/eescobev/Documents/REPORTES/ESTADOS/opacademica.csv")
opacad <- read.csv("D:/Users/eescobev/Documents/REPORTES/ESTADOS/opacademica.csv", header = TRUE, sep = ",",encoding = "UTF-7",stringsAsFactors = FALSE)
opacad$Tipo.de.materia <- NULL
opacad$X <- NULL
opacad$Claves <- opacad$Clave
opacad$Grupos <- opacad$Grupo
opacad <-tidyr::unite(opacad,"clave_g",c(Claves,Grupos),sep="")
opacad <- tidyr::unite(opacad,"Profesor",c(Nombre.titular,Apellidos.titular),sep=" ")
opacad <- dplyr::select(opacad,clave_g,Matricula.GCA,Nombre.completo.de.GCA,Correo.electronico.GCA,Matricula.RE,Nombre.completo.de.RE,Correo.electronico.RE,clave.materia,Nombre.de.materia,Grupo,Matricula.de.profesor,Profesor,Correo)
Academico <- opacad
titulos <- c("clave_g","Matricula GCA","Nombre completo de GCA","Correo electronico GCA","Matricula RE","Nombre completo de RE","Correo electronico RE","clave materia","Nombre de materia","Grupo","Matricula de profesor","Profesor","Correo")
colnames(Academico)<- titulos
write.csv(Academico, file = "D:/Users/eescobev/Documents/REPORTES/ESTADOS/opacademica.csv")

Fase1 <- plyr::join(Fase0,Academico, by ="clave_g", type = "left", match="all")
names(Fase1)[4]="MATRICULA"

#Espacios Fase 2####
Espacios <- read.csv("D:/Users/jvelazhe/Desktop/Licenciaturas/Sabana/espacios.csv", header = TRUE, sep = ",")
Fase2 <- plyr::join(Fase1,Espacios, by ="MATRICULA", type = "left", match="all")
#Convertimos  a numeros
Fase2[,44] <- as.numeric(as.character(Fase2[,44]))

library(dplyr)
Fase2 <- Fase2 %>%
  mutate(Visitas = if_else(is.na(Visitas),0,Visitas))
Fase2[,45] <- as.character(as.character(Fase2[,45]))
Fase2$Espacios[is.na(Fase2$Espacios)]<-"-"


#Estado General Fase 3####
GENERAL <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/ESTADOGENERAL.csv", header = TRUE,sep = ",",encoding="UTF-7",stringsAsFactors = FALSE)
GENERAL$X <- NULL
GENERAL <- subset(GENERAL,NIVEL == "LI")
GENERAL <- subset(GENERAL,CAMPUS != "UVE"&CAMPUS!="UNI"&CAMPUS!="UMM"&CAMPUS!="UVE")
GENERAL$fechi <- lubridate::parse_date_time(GENERAL$FECHA_INICIO,"dmy")
GENERAL$desciende <- xtfrm(GENERAL$fechi)
GENERAL <- GENERAL[with(GENERAL,order(-GENERAL$desciende)),]
GENERAL <- GENERAL %>% group_by(MATRICULA)%>% dplyr::mutate(conteos = row_number())
GENERAL <- GENERAL[!duplicated(GENERAL),]
GENERAL <- subset(GENERAL,conteos == 1)
GENERAL$fechi <- NULL
GENERAL$desciende <- NULL
GENERAL$conteos <- NULL
write.csv(GENERAL, file="D://Users/eescobev/Documents/REPORTES/ESTADOS/GENERALLIC.csv") 
GENERAL <- dplyr::select(GENERAL,MATRICULA,ESTATUS,FECHA_ESTATUS,CODE_PROG,FECHA_NAC,ESTADO,SEXO,Matr)
names(GENERAL)[4]="Claveprog"
names(GENERAL)[5]="FECHA_NAC"
#Cruce con Reporte.
Fase3 <- plyr::join(Fase2,GENERAL, by="MATRICULA", type = "left",match="all")
Entidatt <-readxl::read_excel("D:/Users/jvelazhe/Desktop/Licenciaturas/Sabana/Entidat.xlsx")
Fase3 <- plyr::join(Fase3,Entidatt, by="ESTADO", type = "left", match = "all")
Fase3$Entidad[is.na(Fase3$Entidad)]<-"Resto del pa?s"


#Fase 4 financiero para datos de email, telefono, adeudo y mora####
Financierosir <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/FinancieroGeneral.csv", header = TRUE,sep = ",",encoding="UTF-7")
Financierosir <- subset(Financierosir,NIVEL == "LI")
Financierosir <- subset(Financierosir,CAMPUS != "UMM" & CAMPUS != "UNA"& CAMPUS != "UOC"& CAMPUS != "
UVE"& CAMPUS != "UTS")
Financierosir <- select(Financierosir,MATRICULA,MAIL_PRIN,MAIL_SEC,TELF_CEL,SALDO_TOTAL,MORA,FECHA_MATRICULACION,ESTADO_ALUMNO,PROGRAMA_CODE,NOMBRE_PROGRAMA)
titulosf <- c( "MATRICULA","CORREO_PRINCIPAL","CORREO_ALTERNO", "TELEFONO_CELULAR","SALDO_TOTAL","MORA","FECHA_MATRICULACION","ESTADO_ALUMNO_PROGRAMA","PROGRAMA_CODE","NOMBRE_PROGRAMA")      
colnames(Financierosir)<- titulosf
Financierosir[,1] <- as.numeric(as.character(Financierosir[,1]))
Financierosir[,4] <- as.numeric(as.character(Financierosir[,4]))
#Quitamos Duplicados
Financierosir <- Financierosir[!duplicated(Financierosir), ]
#Convertimos matriculas a numeros
Financierosir$mat <- Financierosir$MATRICULA
Financierosir$clave <- Financierosir$PROGRAMA_CODE
Financierosir <- tidyr::unite(Financierosir,"Matr",c(mat,clave),sep="")
#Quitamos duplicados
Financierosir<-Financierosir[!duplicated(Financierosir),]
Financierosir$MATRICULA<-NULL
#Cruce vs Financiero
Fase4 <- plyr::join(Fase3,Financierosir, by= "Matr", type="left", match="all")



##Fase 5 Asignaci?n####
Asignacion <-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/asignacion.csv", header = TRUE,sep = ",")
names(Asignacion)[2]="MATRICULA"
names(Asignacion)
Asignacion <- Asignacion[,c(2,5,6,9,28,46)]
names(Asignacion)[4]="Asignacion"



#Convertimos matriculas a numeros
Asignacion[,1] <- as.numeric(as.character(Asignacion[,1]))

Fase5 <- plyr::join(Fase4,Asignacion, by="MATRICULA", type = "left", match="all")

Fase5$clavemat <-Fase5$clave
Fase5 <- tidyr::separate(Fase5,clavemat,c("ciclo","num","inicio_plus","clave_materia"),sep = "_")
Fase5$Matrr <-Fase5$MATRICULA
Fase5$clave_cortas<-Fase5$clave_corta
Fase5$grupos<-Fase5$grupo
Fase5<- tidyr::unite(Fase5,"id_us",c(Matrr,clave_cortas,grupos),sep="")

#Fase6 Calcular semanas#### 
Fase6 <- Fase5
Fase6$fecha_hora_extraccion<- Sys.Date()
FechasInicio <-readxl::read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/inicios.xlsx")
Fase6<- plyr::join(Fase6,FechasInicio, by="inicio_plus",type = "left",match = "all")
Fase6[,73]<- as.Date(Fase6$fecha_hora_extraccion,"%Y-%m-%d")
Fase6[,74]<- as.Date(Fase6$`Fecha Inicio`,"%Y-%m-%d")
Fase6$dia <- Fase6$fecha_hora_extraccion - Fase6$`Fecha Inicio`
semana<- readxl::read_excel("D:/Users/jvelazhe/Desktop/Licenciaturas/Sabana/semana.xlsx")
Fase6<- plyr::join(Fase6,semana, by="dia",type = "left",match = "all")


#Fase 7 Potencial Asignaci?n####
Fase7 <- Fase5
Fase7$fecha_hora_extraccion<- Sys.Date()
Fase7$inip <- Fase7$inicio_plus
Fase7$sem <- Fase7$semana
Fase7<-tidyr::unite(Fase7,"Conca",c(inip,sem),sep="_")
potencial<-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/potencial.csv", header = TRUE,sep = ",")
Fase7<- plyr::join(Fase7,potencial, by= "Conca", type="left", match="all")
Fase7$ESTATUS2 <- Fase7$ESTATUS



#Fase8 Bloques de seguimiento,cuatrimestre etc/####
bloqueseg<- readxl::read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/bloqueseg.xlsx")
Fase8<- plyr::join(Fase7,bloqueseg, by= "MATRICULA", type="left", match="all")
names(Fase8)



#Teleton####
#Fase8t <- Teleton
#names(Fase8t)[4]="MATRICULA"
#Teledatos <- readxl::read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/Base alumnos teleton.xlsx")



#Fase8t<- plyr::join(Fase8t,Teledatos, by= "MATRICULA", type="left", match="all")
#Fase8t$claves <- Fase8t$clave_corta
#Fase8t$grupos <- Fase8t$grupo
#Fase8t$matrr <- Fase8t$MATRICULA
#Fase8t<- tidyr::unite(Fase8t,"id_us",c(matrr,claves,grupos),sep="")

#Fase8tac <- plyr::join(Fase8t,Academico, by ="clave_g", type = "left", match="all")
#Fase8tac$clavemat <-Fase8tac$clave
#Fase8tac <- tidyr::separate(Fase8tac,clavemat,c("ciclos","num","inicio_plus","clave_materia"),sep = "_")

#Fase8tac$inip <- Fase8tac$inicio_plus
#Fase8tac$sem <- Fase8tac$semana
#Fase8tac<-tidyr::unite(Fase8tac,"Conca",c(inip,sem),sep="_")
#potencial<-read.csv("D://Users/eescobev/Documents/REPORTES/ESTADOS/potencial.csv", header = TRUE,sep = ",")
#Fase8tac<- plyr::join(Fase8tac,potencial, by= "Conca", type="left", match="all")

#Fase8tac$fecha_hora_extraccion<- Sys.Date()
#names(Fase8tac)[39]="Visitas"
#Fase8te<-dplyr::select(Fase8tac,id_us,cuatrimestre,MATRICULA,`Bloque seguimiento`,`Bloque ingreso`,BTI,`Tipo alumno`,Contenci?n,Espacios,Visitas,`clave materia`,aula,Nombre.Alumno,Nombre,Apellidos,CORREO_PRINCIPAL,TELEFONO_CELULAR,grupo,asignatura,`Matricula de profesor`,Profesor,Tutor,Supervisor,`Matricula GCA`,`Nombre completo de GCA`,`Matricula RE`,`Nombre completo de RE`,ultimo_acceso_m,ultimo_acceso_auvi,calificacion_potencial,calificacion,Docs,inicio_plus,nr,Correo,`Correo electronico GCA`,`Correo electronico RE`,Correo.Tutor,status,status_plataforma,ciclo_ingreso,FECHA_MATRICULACION,PROGRAMA_CODE,NOMBRE_PROGRAMA,SEXO,FECHA_NAC,modalidad,Entidad,ESTATUS,FECHA_ESTATUS,Asignacion,ESTATUS2,fecha_hora_extraccion,semana)




#Fase 9 cambio de nombres a columnnas####
Fase9<-dplyr::select(Fase8,id_us,cuatrimestre,MATRICULA,`Bloque seguimiento`,`Bloque ingreso`,BTI,`Tipo alumno`,Contenci?n,Espacios,Visitas,`clave materia`,aula,Nombre.Alumno,Nombre,Apellidos,CORREO_PRINCIPAL,TELEFONO_CELULAR,grupo,asignatura,`Matricula de profesor`,Profesor,Tutor,Supervisor,`Matricula GCA`,`Nombre completo de GCA`,`Matricula RE`,`Nombre completo de RE`,ultimo_acceso_m,ultimo_acceso_auvi,calificacion_potencial,calificacion,Docs,inicio_plus,nr,Correo,`Correo electronico GCA`,`Correo electronico RE`,Correo.Tutor,status,status_plataforma,ciclo_ingreso,FECHA_MATRICULACION,PROGRAMA_CODE,NOMBRE_PROGRAMA,SEXO,FECHA_NAC,modalidad,Entidad,ESTATUS,FECHA_ESTATUS,Asignacion,ESTATUS2,fecha_hora_extraccion,semana)

#************* Union Teleton - UTEL *********************
Fase9 <- rbind(Fase9)#Fase8te)
Fase9a <- Fase9
Fase9$Nombre.Alumno <-NULL
names(Fase9)[1]="id_us"
names(Fase9)[2]="cuatrimestre"
names(Fase9)[3]="matricula"
names(Fase9)[4]="Bloque seguimiento"
names(Fase9)[5]="Bloque ingreso"
names(Fase9)[6]="BTI"
names(Fase9)[7]="Tipo alumno"
names(Fase9)[8]="Contenci?n"
names(Fase9)[9]="Espacios"
names(Fase9)[10]="# Visitas"
names(Fase9)[11]="clave_materia"
names(Fase9)[12]="Aula"
names(Fase9)[13]="nombre"
names(Fase9)[14]="apellidos"
names(Fase9)[15]="email"
names(Fase9)[16]="telefono"
names(Fase9)[17]="grupo"
names(Fase9)[18]="asignatura"
names(Fase9)[19]="idprof"
names(Fase9)[20]="profesor"
names(Fase9)[21]="tutor"
names(Fase9)[22]="supervisor"
names(Fase9)[23]="idgc"
names(Fase9)[24]="gc"
names(Fase9)[25]="idrevisor"
names(Fase9)[26]="revisor"
names(Fase9)[27]="acceso"
names(Fase9)[28]="acceso_aula"
names(Fase9)[29]="calificacion_potencial"
names(Fase9)[30]="calificacion"
names(Fase9)[31]="documentos"
names(Fase9)[32]="inicio_plus"
names(Fase9)[33]="nivel_riesgo"
names(Fase9)[34]="email_profesor"
names(Fase9)[35]="email_rm"
names(Fase9)[36]="email.revisor"
names(Fase9)[37]="email tutor"
names(Fase9)[38]="edo_moodle"
names(Fase9)[39]="Estado plataforma"
names(Fase9)[40]="ciclo_ingreso"
names(Fase9)[41]="fecha_matriculacion"
names(Fase9)[42]="Clave programa"
names(Fase9)[43]="programa"
names(Fase9)[44]="genero"
names(Fase9)[45]="fechanacimiento"
names(Fase9)[46]="modalidad_evaluacion"
names(Fase9)[47]="domicilio"
names(Fase9)[48]="stutus"
names(Fase9)[49]="Max edo"
names(Fase9)[50]="Asignaci?n"
names(Fase9)[51]="estado escolares"
names(Fase9)[52]="fecha_hora_extraccion"
names(Fase9)[53]="Semana en curso"

Fase9 <- Fase9 %>%
  dplyr::mutate(`# Visitas` = if_else(is.na(`# Visitas`), 0, `# Visitas`))

Fase9$documentos[is.na(Fase9$documentos)]<-"Sin documentos"
Fase9[,21] <- as.character(as.character(Fase9[,21]))
Fase9[,22] <- as.character(as.character(Fase9[,22]))
Fase9[,50] <- as.character(as.character(Fase9[,50]))
Fase9$tutor[is.na(Fase9$tutor)]<-"por_asignar"
Fase9$supervisor[is.na(Fase9$supervisor)]<-"por_asignar"
Fase9$Asignaci?n[is.na(Fase9$Asignaci?n)]<-"por_asignar"
tiposbajas <- read.csv("D:/Users/eescobev/Documents/REPORTES/ESTADOS/tiposbajas.csv",header = T,sep = ",",encoding = "UTF-8")
Fase9 <- dplyr::left_join(Fase9,tiposbajas,by="stutus")
Fase9$Mon <- lubridate::month(Fase9$`Max edo`)
Fase9$alv <- dplyr::if_else(Fase9$Tipo == "Quitar" & Fase9$Mon < 9,"ALV","NO")
Fase9 <- subset(Fase9,alv == "NO")
Fase9$Tipo <- NULL
Fase9$Mon <- NULL
Fase9$alv <- NULL

Semi <- Fase9
Semif <- grep("SEMI", Fase9$`Tipo alumno`, ignore.case = TRUE)
Semi <- Semi[Semif,]



Latam <- Fase9
names(Latam)[7]="Tipo.alumno"
library(readxl)
tablaalianzas <- read_excel("D:/Users/eescobev/Documents/REPORTES/ESTADOS/LATAM.xlsx")
Latam <- dplyr::left_join(Latam,tablaalianzas,by="Tipo.alumno")
Latam$Alianza[is.na(Latam$Alianza)]<-"Mexico"
Latam <- subset(Latam,Alianza != "Mexico")
Latam <- Latam[,c(54,1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53)]

write.csv(Fase9, file="D://Users/eescobev/Documents/REPORTES/FRONT/sabana.csv") 
url1 <- c("C://Users/jvelazhe/Google Drive/Niveles/Sabana_")
dia <- Sys.Date()
url2 <- c(".csv")
url3 <- paste(url1,dia,url2,sep="")
write.csv(Fase9, file=url3,row.names = F) 
url1 <- c("C://Users/jvelazhe/Desktop/Conectivida/Sabanas Mayo/Sabana_")
dia <- Sys.Date()
url2 <- c(".csv")
url3 <- paste(url1,dia,url2,sep="")
write.csv(Fase9, file=url3,row.names = F) 

url1 <- c("C://Users/jvelazhe/Google Drive/Niveles LATAM/Sabana_LATAM_")
dia <- Sys.Date()
url2 <- c(".csv")
url3 <- paste(url1,dia,url2,sep="")
write.csv(Latam, file=url3,row.names = F) 

url1 <- c("C://Users/jvelazhe/Google Drive/Niveles Ejecutivas/Sabana_Ejecutivas_")
dia <- Sys.Date()
url2 <- c(".csv")
url3 <- paste(url1,dia,url2,sep="")
write.csv(Semi, file=url3,row.names = F) 

proc.time() - t

