#Script cod
##### PARA ABRIR PROYECTOS DESCARGADOS DE SHINY #############
# con el cmd
#tar -xvzf mapa_v1_DMQ.tar
####################################### espacios en blanco ##############
sum(is.na(REPORTE_GENERAL$FS_Paterno))
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "                      ")),
    na.rm = T)#1
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "                  ")),
    na.rm = T)#2
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "                                              ")),
    na.rm = T)#3
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "              ")),
    na.rm = T)#4
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "                     ")),
    na.rm = T)#5
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "    ")),
    na.rm = T)#6
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "   ")),
    na.rm = T)#7
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "  ")),
    na.rm = T)#8
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "    ")),
    na.rm = T)#9
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "                                           ")),
    na.rm = T)#10
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "                         ")),
    na.rm = T)#11
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "                           ")),
    na.rm = T)#12
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "                                                       ")),
    na.rm = T)#13
sum(as.numeric(str_detect(REPORTE_GENERAL$BT_Nombres,
                          "                                              ")),
    na.rm = T)#14
#
sum(str_detect(REPORTE_GENERAL$nombre_de_Cliente, "“|»|‘|&|@|——|—|¥|¢"), na.rm = T)
# reemplazo total de los 14 casos +1
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "                      "," ")
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "                  "," ")
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "                                              "," ")
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "                      "," ")
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "                     "," ")#5
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "    "," ")#6
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "   "," ")#7
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "  "," ")#8
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "    "," ")#9
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "                                           "," ")#10
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "                         "," ")#11
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "                           "," ")#12
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "                                                       "," ")#13
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres, "                                              "," ")#14
REPORTE_GENERAL$BT_Nombres= str_replace_all(REPORTE_GENERAL$BT_Nombres,"“|»|‘|&|@|——|—|¥|¢","")
####
casos_60_1= casos_60_1 %>% mutate(NOMBRE1= NOMBRE)
casos_60_1= separate(casos_60_1,NOMBRE1, c("N1","N2","N3","N4","N5","N6"),sep = " ")
#
apellidos_1=NULL
apellidos_2=NULL

for (i in 1:nrow(casos_60_1)) {
  print(i)
  if(is.na(casos_60_1$N1[i]) &is.na(casos_60_1$N2[i]) & is.na(casos_60_1$N3[i]) & is.na(casos_60_1$N4[i])
     & is.na(casos_60_1$N5[i])&is.na(casos_60_1$N6[i]))
  {
    apellidos_1[i]=NA
    apellidos_2[i]=NA
  }
  else if(!is.na(casos_60_1$N3[i]) & !is.na(casos_60_1$N4[i]) )
  {
    apellidos_1[i]=casos_60_1$N3[i]
    apellidos_2[i]=casos_60_1$N4[i]
  }
  else if(!is.na(casos_60_1$N3[i]) & is.na(casos_60_1$N4[i]))
  {
    apellidos_1[i]=casos_60_1$N2[i]
    apellidos_2[i]=casos_60_1$N3[i]
  }
  else if( !is.na(casos_60_1$N2[i])& is.na(casos_60_1$N3[i])& is.na(casos_60_1$N4[i]))
  {
    apellidos_1[i]=casos_60_1$N3[i]
    apellidos_2[i]=NA
  }
  else {
    apellidos_1[i]=NA
    apellidos_2[i]=NA
       }
}
#
Base_apellidos= data.frame(apellidos_1, apellidos_2)
Base_apellidos= Base_apellidos %>% filter(!is.na(apellidos_1) & !is.na(apellidos_2))
persona= PERSONASGG %>% filter(Nombres %in% unique(c(Base_apellidos$apellidos_1, Base_apellidos$apellidos_2)) )
# Base Money Gram
#### este for busca espacio al final de la cadena y lo reemplaza por nada #######
for (i in 1:nrow(casos_60_1)) {
  print(i)
  if(!is.na(casos_60_1$FS_CodProducto[i]))
  {
    if(substr(casos_60_1$FS_CodProducto[i],nchar(casos_60_1$FS_CodProducto[i]),nchar(casos_60_1$FS_CodProducto[i]))==" ")
    {
      print("xxxxxxxxxxxxxxx")
      casos_60_1$FS_CodProducto[i]=substr(casos_60_1$FS_CodProducto[i],1,nchar(casos_60_1$FS_CodProducto[i])-1)
    }
  }
}
casos_60_1$FS_CodProducto[1]
#
sum(str_detect(casos_60_1$NOMBRE,"  "), na.rm = T)
sum(str_detect(casos_60_1$Photo_Id, "E "), na.rm = T)
sum(str_detect(casos_60_1$Photo_Id, " "), na.rm = T)
#
casos_60_1$Photo_Id= str_replace(casos_60_1$Photo_Id, "E ","")
casos_60_1$Photo_Id= str_replace(casos_60_1$Photo_Id, " ","-")
#
Transac_Money_Gram_Recib= Transac_Money_Gram_Recib %>% mutate(nombre_ap=paste(Receiver_First_Name, Receiver_Middle_Name,
                                                                              Receiver_Last_Name, Receiver_Maternal_Name))
Transac_Money_Gram_Recib$nombre_ap= str_replace(Transac_Money_Gram_Recib$nombre_ap, " NA "," ")
Transac_Money_Gram_Recib$nombre_ap= str_replace(Transac_Money_Gram_Recib$nombre_ap, " NA"," ")
#Transac_Money_Gram_Recib$nombre_ap= str_replace(Transac_Money_Gram_Recib$nombre_ap, "NA "," ")
library(readxl)
library(readr)
Base_Evaluada <- read_delim("C:/Users/dmamaniq/Desktop/Base_Evaluada.csv",
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
Base_Evaluada= Base_Evaluada %>% mutate(nombre_N_ap= paste(Nombres, ApPaterno, ApMaterno))
Base_Evaluada$nombre_N_ap= str_replace(Base_Evaluada$nombre_N_ap, " NA "," ")
Base_Evaluada$nombre_N_ap= str_replace(Base_Evaluada$nombre_N_ap, " NA"," ")
Base_Evaluada$nombre_N_ap= str_replace(Base_Evaluada$nombre_N_ap, "NA "," ")
#
Transac_Money_Gram_Recib= cbind(Transac_Money_Gram_Recib, correlativo=1:nrow(Transac_Money_Gram_Recib))
Transac_Money_Gram_Recib_1= Transac_Money_Gram_Recib %>% left_join(Base_Evaluada[,c(1,3,18,16,7)], by=c("Photo_Id"="Pendoc"))
Transac_Money_Gram_Recib_1= Transac_Money_Gram_Recib_1 %>% group_by(correlativo) %>% filter(row_number()==1)

####################### 18 de mayo #####################
#
BFS_Captaciones_v3_vf= BFS_Captaciones_v3_vf %>% filter(TIPO_PERSONA=="Persona Natural")
sum(str_detect(BFS_Captaciones_v3_vf$NOMBRE,"Ã‘"))
BFS_Captaciones_v3_vf$NOMBRE= str_replace(BFS_Captaciones_v3_vf$NOMBRE,"Ã‘","Ñ")
#
sum(str_detect(BFS_Captaciones_v3_vf$NOMBRE, "/"))
BFS_Captaciones_v3_vf= BFS_Captaciones_v3_vf %>% mutate(nombre1=NOMBRE, num_ci=NRO_DOC_IDENTIDAD,
                                                                            fec_nac1= FECHA_NACIMIENTO)
BFS_Captaciones_v3_vf= separate(BFS_Captaciones_v3_vf,
                                          nombre1, c("n1","n2","n3","n4","n5","n6","n7","n8","n9","n10"), sep = "/")
BFS_Captaciones_v3_vf= separate(BFS_Captaciones_v3_vf,
                                          num_ci, c("nn1","nn2","nn3","nn4","nn5","nn6","nn7","nn8","nn9","nn10"), sep = "/")
BFS_Captaciones_v3_vf= separate(BFS_Captaciones_v3_vf,
                                          fec_nac1, c("nnn1","nnn2","nnn3","nnn4","nnn5","nnn6","nnn7","nnn8","nnn9","nnn10"), sep = "/")
#
base1= BFS_Captaciones_v3_vf[,c(65,75,85)]
names(base1)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base1$fecha_nacimiento=as.Date(base1$fecha_nacimiento, format = "%d-%m-%Y")
#
base2= BFS_Captaciones_v3_vf[,c(66,76,86)]
names(base2)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base2$fecha_nacimiento=as.Date(base2$fecha_nacimiento, format = "%d-%m-%Y")
#
base3= BFS_Captaciones_v3_vf[,c(67,77,87)]
names(base3)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base3$fecha_nacimiento=as.Date(base3$fecha_nacimiento, format = "%d-%m-%Y")
#
base4= BFS_Captaciones_v3_vf[,c(68,78,88)]
names(base4)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base4$fecha_nacimiento=as.Date(base4$fecha_nacimiento, format = "%d-%m-%Y")
#
base5= BFS_Captaciones_v3_vf[,c(69,79,89)]
names(base5)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base5$fecha_nacimiento=as.Date(base5$fecha_nacimiento, format = "%d-%m-%Y")
#
base6= BFS_Captaciones_v3_vf[,c(70,80,90)]
names(base6)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base6$fecha_nacimiento=as.Date(base6$fecha_nacimiento, format = "%d-%m-%Y")
#
base7= BFS_Captaciones_v3_vf[,c(71,81,91)]
names(base7)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base7$fecha_nacimiento=as.Date(base7$fecha_nacimiento, format = "%d-%m-%Y")
#
base8= BFS_Captaciones_v3_vf[,c(72,82,92)]
names(base8)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base8$fecha_nacimiento=as.Date(base8$fecha_nacimiento, format = "%d-%m-%Y")
#
base9= BFS_Captaciones_v3_vf[,c(73,83,93)]
names(base9)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base9$fecha_nacimiento=as.Date(base9$fecha_nacimiento, format = "%d-%m-%Y")
#
base10= BFS_Captaciones_v3_vf[,c(74,84,94)]
names(base10)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base10$fecha_nacimiento=as.Date(base10$fecha_nacimiento, format = "%d-%m-%Y")
#
base_bfs_cap_v3_vf= rbind(base1, base2, base3,base4, base5, base6, base7, base8,base9,base10)
#save(base_bfs_cap_v3_vf, file = "base_bfs_cap_v3_vf.RData")
# for (i in 1:nrow(base_bfs_cap_v3_cump)) {
#   print(i)
#   if(!is.na(base_bfs_cap_v3_cump$nombre_emp_ap[i]))
#   {
#     if(substr(base_bfs_cap_v3_cump$nombre_emp_ap[i],nchar(base_bfs_cap_v3_cump$nombre_emp_ap[i]),nchar(base_bfs_cap_v3_cump$nombre_emp_ap[i]))==" ")
#     {
#       print("xxxxxxxxxxxxxxxxxxxxxxxx")
#       base_bfs_cap_v3_cump$nombre_emp_ap[i]=substr(base_bfs_cap_v3_cump$nombre_emp_ap[i],1,nchar(base_bfs_cap_v3_cump$nombre_emp_ap[i])-1)
#     }
#   }
# }
base_bfs_cap_v3_vf= base_bfs_cap_v3_vf %>% mutate(nom=nombre_emp_ap, numero_ci=num_CI, fec_nac= fecha_nacimiento)
base_bfs_cap_v3_vf_1= separate(base_bfs_cap_v3_vf, nom,c("n1","n2","n3","n4","n5","n6"), sep = " ")
base_bfs_cap_v3_vf_1= separate(base_bfs_cap_v3_vf_1, numero_ci,c("nn1","nn2","nn3"), sep = "-")
indica=NULL
for (i in 1:nrow(base_total)) {
  print(i)
  if(sum(is.na(base_total[i,]))==13)
    indica[i] =1
  else indica[i] =2
}
base_total= cbind(base_total, indica)
datos= base_total %>% filter(indica==2)
datos$nombres= str_replace(datos$nombres, " NA"," ")
#


#save(base_bfs_cap_v3_cump_1, file = "base_bfs_cap_v3_cump_1.RData")
#es la segunda base de 168098
#save(base_bfs_cap_v3_vf_1, file = "base_bfs_cap_v3_vf_1.RData")
#load("base_bfs_cap_v3_cump_1.RData")
########################## para dpf
BFS_Captaciones_v3_Cumplimiento= BFS_Captaciones_v3_Cumplimiento %>% filter(TIPO_PERSONA=="Persona Natural")
sum(str_detect(BFS_Captaciones_v3_Cumplimiento$NOMBRE_ACTUAL,"Ã‘"))
#BFS_Captaciones_v3_Cumplimiento$NOMBRE= str_replace(BFS_Captaciones_v3_Cumplimiento$NOMBRE,"Ã‘","Ñ")
#
sum(str_detect(BFS_Captaciones_v3_Cumplimiento$NOMBRE_ACTUAL, "/"))
BFS_Captaciones_v3_Cumplimiento= BFS_Captaciones_v3_Cumplimiento %>% mutate(nombre1=NOMBRE_ACTUAL, num_ci=NRO_DOC_IDENTIDAD,
                                                                            fec_nac1= FECHA_NACIMIENTO)
BFS_Captaciones_v3_Cumplimiento= separate(BFS_Captaciones_v3_Cumplimiento,
                                          nombre1, c("n1","n2","n3","n4","n5","n6","n7","n8","n9","n10"), sep = "/")
BFS_Captaciones_v3_Cumplimiento= separate(BFS_Captaciones_v3_Cumplimiento,
                                          num_ci, c("nn1","nn2","nn3","nn4","nn5","nn6","nn7","nn8","nn9","nn10"), sep = "/")
BFS_Captaciones_v3_Cumplimiento= separate(BFS_Captaciones_v3_Cumplimiento,
                                          fec_nac1, c("nnn1","nnn2","nnn3","nnn4","nnn5","nnn6","nnn7","nnn8","nnn9","nnn10"), sep = "/")
#
base1= BFS_Captaciones_v3_Cumplimiento[,c(79,89,99)]
names(base1)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base1$fecha_nacimiento=as.Date(base1$fecha_nacimiento, format = "%d-%m-%Y")
#
base2= BFS_Captaciones_v3_Cumplimiento[,c(80,90,100)]
names(base2)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base2$fecha_nacimiento=as.Date(base2$fecha_nacimiento, format = "%d-%m-%Y")
#
base3= BFS_Captaciones_v3_Cumplimiento[,c(81,91,101)]
names(base3)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base3$fecha_nacimiento=as.Date(base3$fecha_nacimiento, format = "%d-%m-%Y")
#
base4= BFS_Captaciones_v3_Cumplimiento[,c(82,92,102)]
names(base4)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base4$fecha_nacimiento=as.Date(base4$fecha_nacimiento, format = "%d-%m-%Y")
#
base5= BFS_Captaciones_v3_Cumplimiento[,c(83,93,103)]
names(base5)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base5$fecha_nacimiento=as.Date(base5$fecha_nacimiento, format = "%d-%m-%Y")
#
base6= BFS_Captaciones_v3_Cumplimiento[,c(84,94,104)]
names(base6)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base6$fecha_nacimiento=as.Date(base6$fecha_nacimiento, format = "%d-%m-%Y")
#
base7= BFS_Captaciones_v3_Cumplimiento[,c(85,95,105)]
names(base7)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base7$fecha_nacimiento=as.Date(base7$fecha_nacimiento, format = "%d-%m-%Y")
#
base8= BFS_Captaciones_v3_Cumplimiento[,c(86,96,106)]
names(base8)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base8$fecha_nacimiento=as.Date(base8$fecha_nacimiento, format = "%d-%m-%Y")
#
base9= BFS_Captaciones_v3_Cumplimiento[,c(87,97,107)]
names(base9)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base9$fecha_nacimiento=as.Date(base9$fecha_nacimiento, format = "%d-%m-%Y")
#
base10= BFS_Captaciones_v3_Cumplimiento[,c(88,98,108)]
names(base10)=c("nombre_emp_ap","num_CI","fecha_nacimiento")
base10$fecha_nacimiento=as.Date(base10$fecha_nacimiento, format = "%d-%m-%Y")
#
base_bfs_cap_v3_cump_dpf= rbind(base1, base2, base3,base4, base5, base6, base7, base8,base9,base10)
base_bfs_cap_v3_cump_dpf= base_bfs_cap_v3_cump_dpf %>% mutate(nom=nombre_emp_ap, numero_ci=num_CI, fec_nac= fecha_nacimiento)
base_bfs_cap_v3_cump_dpf_1= separate(base_bfs_cap_v3_cump_dpf, nom,c("n1","n2","n3","n4","n5","n6"), sep = " ")
base_bfs_cap_v3_cump_dpf_1= separate(base_bfs_cap_v3_cump_dpf_1, numero_ci,c("nn1","nn2","nn3"), sep = "-")
####
long_telf=NULL
for (i in 1:nrow(Base_clientes_BSol)) {
  print(i)
  long_telf[i]= nchar(Base_clientes_BSol$TELEFONO_TITULAR[i])
}
table(long_telf)
Base_clientes_BSol= cbind(Base_clientes_BSol, longitud_telf=long_telf)
Base_clientes_BSol= Base_clientes_BSol[,c(1:17,39,18:38)]
#
Base_clientes_BSol$FECHA_NACIMIENTO= substr(Base_clientes_BSol$FECHA_NACIMIENTO,1,10)
Base_clientes_BSol$FECHA_NACIMIENTO= as.Date(Base_clientes_BSol$FECHA_NACIMIENTO, format = "%Y-%m-%d")
fec_nac_recod=NULL
for (i in 1:nrow(Transacciones_Money_Gram)) {
  print(i)
    if(!is.na(Transacciones_Money_Gram$Birth_Date[i]))
    {
      if(str_detect(Transacciones_Money_Gram$Birth_Date[i],"/"))
        fec_nac_recod[i]= as.numeric(as.Date(Transacciones_Money_Gram$Birth_Date[i], format = "%m/%d/%Y"))
      else fec_nac_recod[i]= Transacciones_Money_Gram$Birth_Date[i]
    }
    else fec_nac_recod[i]=NA
}
Transacciones_Money_Gram= cbind(Transacciones_Money_Gram, fecha_nac_recod= fec_nac_recod)
write.xlsx(Transacciones_Money_Gram, file = "prueba.xlsx")

################### 22 de mayo de 2023 ################
datos_comp_1= datos_comp %>% mutate(ci_compara= IDC==JSBN05NDoc)


##
datos_1= separate(datos,nombre_de_Cliente,c("n1","n2","n3","n4","n5","n6","n7","n8","n9","10"), sep = " ")
#
datos_comp_1= as.data.frame(datos_comp_1)
datos_comp_1= datos_comp_1 %>% mutate(compara_ci= IDC== JSBN05Raiz)
datos_comp_1= datos_comp_1 %>% mutate(nombres= paste(Pfnom1, Pfnom2))
datos_comp_1$nombres= str_replace(datos_comp_1$nombres, " NA","")
datos_comp_1= datos_comp_1 %>% mutate(compara_nom1= NOMBRES==nombres)
datos_comp_1= datos_comp_1 %>% mutate(compara_ap_pat= `APELLIDO PATERNO`==Pfape1)
datos_comp_1= datos_comp_1 %>% mutate(compara_ap_mat= `APELLIDO MATERNO`==Pfape2)
datos_comp_1= datos_comp_1 %>% mutate(fecha_nac1= substr(`FECHA NACIMIENTO`,1,10))
datos_comp_1= datos_comp_1 %>% mutate(fecha_nac2= substr(Pffnac,1,10))
datos_comp_1$fecha_nac1= as.Date(datos_comp_1$fecha_nac1, format = "%Y-%m-%d")
datos_comp_1$fecha_nac2= as.Date(datos_comp_1$fecha_nac2, format = "%Y-%m-%d")
#
datos_comp_1= datos_comp_1 %>% mutate(compara_fec_nac= fecha_nac1== fecha_nac2)
###################


sum(c(Libroprueba$FS_Paterno[1], Libroprueba$FS_Materno[1],
      Libroprueba$FS_Nombres[1]) %in% c(Libroprueba$BT_Paterno[1],
                                        Libroprueba$BT_Materno[1], Libroprueba$BT_Nombres[1]) )

#
valida=c()
for (i in 1:nrow(Libroprueba)) {
  print(i)
  if(sum(c(Libroprueba$FS_Paterno[i], Libroprueba$FS_Materno[i],
           Libroprueba$FS_Nombres[i]) %in% c(Libroprueba$BT_Paterno[i],
                                             Libroprueba$BT_Materno[i], Libroprueba$BT_Nombres[i]) )>=2)
    valida[i]=2
  else valida[i]=1
}
table(valida)
Libroprueba= cbind(Libroprueba, valid= valida)




valida=c()
for (i in 1:nrow(Libroprueba)) {
  print(i)
  if(sum(c(Libroprueba$FS_Paterno[i], Libroprueba$FS_Materno[i],
           Libroprueba$FS_Nombres[i]) %in% c(Libroprueba$x1[i],Libroprueba$x2[i],Libroprueba$x3[i],
                                             Libroprueba$x4[i],Libroprueba$x5[i],Libroprueba$x6[i]))>=2)
    valida[i]=2
  else valida[i]=1
}
table(valida)
Libroprueba= cbind(Libroprueba, valid= valida)
#################
#aux= REPORTE_GENERAL
REPORTE_GENERAL$Saldo=as.numeric(REPORTE_GENERAL$Saldo)
#
datos= REPORTE_GENERAL %>% filter(!is.na(FS_TipoPersona)) %>%
  group_by(FS_TipoPersona) %>% summarise(cantidad_per= length(unique(FS_CodCliente)),
                                         cantidad_cuentas= length(FS_CodCliente))
datos1= REPORTE_GENERAL %>% group_by(FS_TipoPersona ,TIPO) %>% summarise(monto= sum(Saldo))
write.xlsx(datos, file = "el_1.xlsx")
write.xlsx(datos1, file = "el_2.xlsx")
###
datos= REPORTE_GENERAL %>% filter(is.na(BT_FS_Validaciones)) %>%
  group_by(FS_TipoPersona) %>% summarise(cantidad_per= length(unique(FS_CodCliente)),
                                         cantidad_cuentas= length(FS_CodCliente))
write.xlsx(datos, file = "el_3.xlsx")
datos1= REPORTE_GENERAL %>% filter(is.na(BT_FS_Validaciones)) %>%
  group_by(FS_TipoPersona ,TIPO) %>% summarise(monto= sum(Saldo))
write.xlsx(datos1, file = "el_4.xlsx")
###
#REPORTE_GENERAL$FS_NumDoc= as.numeric(REPORTE_GENERAL$FS_NumDoc)
para_cruce_info$carnet= as.character(para_cruce_info$carnet)
REPORTE_GENERAL= cbind(REPORTE_GENERAL, core=1:nrow(REPORTE_GENERAL))
REPORTE_GENERAL= REPORTE_GENERAL %>% left_join(para_cruce_info, by=c("FS_NumDoc"="carnet") )
REPORTE_GENERAL= REPORTE_GENERAL %>% group_by(core) %>% filter(row_number()==1)
#
para_cruce_info_3k$carnet= as.character(para_cruce_info_3k$carnet)
REPORTE_GENERAL= REPORTE_GENERAL %>% left_join(para_cruce_info_3k, by=c("FS_NumDoc"="carnet"))
REPORTE_GENERAL= REPORTE_GENERAL %>% group_by(core) %>% filter(row_number()==1)
write.xlsx(REPORTE_GENERAL, file = "eliminar2.xlsx")
#######################################################
pertenece=c()
for (i in 1:nrow(base_14k)) {
  print(i)
  if(base_14k$FS_DocDocFassil[i] %in% unique(datos$FS_DocDocFassil) )
    pertenece[i]=1
  else pertenece[i]=2
}
table(pertenece)
base_14k= cbind(base_14k, pertenece_1=pertenece)


envio=c()
for (i in 1:nrow(BASE_CONSOLIDADA_OBS)) {
  print(i)
  if(BASE_CONSOLIDADA_OBS$FS_DocDocFassil[i] %in% unique(Base_envio_vf$FS_DocDocFassil) )
    envio[i]=1
  else envio[i]=2
}
table(envio)
BASE_CONSOLIDADA_OBS= cbind(BASE_CONSOLIDADA_OBS, envio_5k=envio)
####
anul=c()
for (i in 1:nrow(BASE_CONSOLIDADA_OBS)) {
  print(i)
  if(BASE_CONSOLIDADA_OBS$FS_DocDocFassil[i] %in% unique(anulados$JSBN05NDoc) )
    anul[i]=1
  else anul[i]=2
}
table(anul)
BASE_CONSOLIDADA_OBS= cbind(BASE_CONSOLIDADA_OBS, anula=anul)
######################################
Base_de_Migracion_30_5_2023=as.data.frame(Base_de_Migracion_30_5_2023)
Base_de_Migracion_30_5_2023$TIPO[Base_de_Migracion_30_5_2023$TIPO=="CCA-INA"]="CCA"
Base_de_Migracion_30_5_2023$TIPO[Base_de_Migracion_30_5_2023$TIPO=="DPF-RET"]="DPF"

#prueba=Base_Consolidada_1 %>% group_by(BASE, FS_TipoPersona) %>% summarise(cont_tot=length(FS_DocDocFassil),
 #                                                                     cont_unic=length(unique(FS_DocDocFassil)))




prueba=Base_Consolidada_1 %>% group_by(BASE, FS_TipoPersona, TIPO.x) %>% summarise(cont_tot=n(),
                                                                             cont_unic=length(unique(FS_DocDocFassil)), monto=sum(Saldo))
prueba2=Base_de_Migracion_30_5_2023 %>% group_by(BASE, saldo_may_700 ,FS_TipoPersona, TIPO) %>% summarise(cont_tot=n(),
                                                                                   cont_unic=length(unique(FS_DocDocFassil)), monto=sum(Saldo))

###

prueba3= REVISION_3010_CASOS_VF_1 %>%   group_by(FS_DocDocFassil) %>% arrange(desc(producto_es)) %>% filter(row_number()==1)



###
Base_Consolidada_11= Base_Consolidada_1 %>% mutate(saldo_may_700= ifelse(Saldo>=700,1,2))
##
nom=c("Saldo" , "Estado","TIPO","FS_CodProducto","FS_TipoIntegracion","FS_Codigo_Cliente_BF","FS_NumDoc" ,   "FS_DocDocFassil")
names(casos_60_1)= nom
casos_60_1= separate(casos_60,"Saldo",
                                  c("x1","x2","x3","x4","x5","x6","x7","x8"), sep = " ")




prod_esta=c()
for (i in 1:nrow(Base_Consolidada_11)) {
  print(i)
  if(Base_Consolidada_11$FS_CodProducto[i] %in% unique(casos_60_1$cod_prod) )
    prod_esta[i]=1
  else prod_esta[i]=2
}
table(prod_esta)
Base_Consolidada_11= cbind(Base_Consolidada_11, producto_es= prod_esta)




##################### PARA IMP TRANSAC ###################
persona= Reporte_2023_02[1:10000,]
#rm(Reporte_2021_06)
library(readr)
######load("report_2020_01.RData")
Reporte_2023_05 <- read_delim("D:/Nueva carpeta1/bases transacciones/2023/Reporte_2023_05.csv",
                             delim = ";", escape_double = FALSE, col_names = FALSE,
                             trim_ws = TRUE)
names(Reporte_2023_05)= names(cabecera)
Reporte_2023_05= as.data.frame(Reporte_2023_05)
save(Reporte_2023_05, file = "Reporte_2023_05.RData")
rm(Reporte_2023_05)
###########


###### PRUEBA CON BASE DE CAJAS DE AHORRO #####
library(tidyverse)
library(openxlsx)
seleccionados= report_2020_01 %>% filter(ID_Cliente %in% unique(Configuracion_POE_PJ$IdPersona))
datos= seleccionados %>% group_by(ID_Cliente) %>% summarise(max_dep= max(Importe_Depositos),
                                                      max_ret= max(Importe_Retiros),
                                                      tot_dep= sum(Importe_Depositos),
                                                      to_ret=sum(Importe_Retiros))
datos_c= seleccionados %>%  mutate(dep_may= ifelse(Importe_Depositos>70000,1,0)) %>% group_by(ID_Cliente) %>%
  filter(dep_may==1) %>% summarise(cant_may_corte_dep=n())
datos_c1= seleccionados %>%  mutate(dep_may= ifelse(Importe_Retiros>70000,1,0)) %>% group_by(ID_Cliente) %>%
  filter(dep_may==1) %>% summarise(cant_may_corte_ret=n())
datos= datos %>% mutate(corre=1:nrow(datos))
datos1= seleccionados %>% group_by(ID_Cliente) %>% filter(Importe_Depositos!=0) %>% summarise(cant_dep=n())
datos2= seleccionados %>% group_by(ID_Cliente) %>% filter(Importe_Retiros!=0) %>% summarise(cant_ret=n())
datos3= datos %>% left_join(datos1, by="ID_Cliente")
datos3= datos3 %>% left_join(datos2, by="ID_Cliente")
datos3= datos3 %>% left_join(datos_c, by="ID_Cliente")
datos3= datos3 %>% left_join(datos_c1, by="ID_Cliente")
datos3$ID_Cliente=as.character(datos3$ID_Cliente)
datos3= datos3 %>% left_join(Configuracion_POE_PJ[,c(1,2,3,6)], by=c("ID_Cliente"="IdPersona"))
datos3= datos3 %>% group_by(corre) %>% filter(row_number()==1)
datos3= datos3[,c(11:13,1:10)]
write.xlsx(datos3, file = "dato_1.xlsx")
###########################################
dato_11= dato_11 %>% mutate(date= paste(año,mes,"1", sep = "-"))
dato_11$fecha= as.Date(dato_11$fecha)
persona= dato_11 %>% filter(ID_Cliente==527089)
#
library(dygraphs)
dygraph(as.ts(persona$tot_dep, start=c(2020,1), end=c(2020,9), frequency==30))
plot(as.ts(persona$tot_dep, start=c(2020,1), end=c(2020,9), frequency==30))

fec=ts(c(persona$tot_dep,persona$tot_dep,persona$tot_dep), start=c(2020,1), frequency=12)
plot(fec)
dygraph(fec)


HoltWinters(fec)

plot(fec)
plot(ldeaths)
x <- uspop + rnorm(uspop, sd = 5)
m <- HoltWinters(fec, gamma = F, beta = F)
m <- HoltWinters(fec, gamma = F, beta = F)

plot(m)
lines(fitted(m)[,1], col = 3)

hw <- HoltWinters(ldeaths)
hw= HoltWinters(fec)
predicted <- predict(hw, n.ahead = 12, prediction.interval = TRUE)
dygraph(predicted, main = "Pred") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "D") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))

dygraph(predicted, main = "(UK)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))



(m <- auto.arima(fec))
plot(m)
plot(fitted(m))
hchart(fitted(m))
predict(m, n.ahead=12, prediction=TRUE)

hw <- HoltWinters(m)
predicted <- predict(m, n.ahead = 12, prediction.interval = TRUE)
plot(predicted)
predicted[[1]]
class(predicted[[1]])
c(fec,predicted[[1]])
class()
mm= ts(c(fec,predicted[[1]]), start = c(2020,1))
plot(mm)

#
library(fpp2)
m3= hw(fec, 12, seasonal="multiplicative" )
autoplot(m3)
#
hw <- HoltWinters(ldeaths)
predicted <- predict(hw, n.ahead = 12, prediction.interval = TRUE)
#
modelo_arima=auto.arima(fec)
m4= forecast(modelo_arima, h=12)
autoplot(m4)
#
dygraph(predicted, main = "Predicted Lung Deaths (UK)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))

##
library(highcharter)
hchart(ts(datos1))
#################### 5/6/2023 ######################
load("REPORT_2022_01.RData")
persona= REPORT_2022_01 %>% filter(ID_Cliente==527072)
######################
save(Base_resumen_pj_extract_h21, file = "Base_resumen_pj_extract_h21.RData")

economics_long2 <- economics_long %>%
  filter(variable %in% c("pop", "uempmed", "unemploy")) %>%
  print()
#
persona= Base_resumen_pj_extract_m23 %>% filter(ID_Cliente==2176086)
persona= persona %>% mutate(date= paste(año,mes,"1", sep = "-"))
persona$date=as.Date(persona$date)
total_dep= persona[,c(11,20)]
total_dep= total_dep %>% mutate(variable="Total_depositos")
total_ret= persona[,c(12,20)]
total_ret= total_ret %>% mutate(variable="Total_retiros")
names(total_ret)= names(total_dep)
total_u= rbind(total_dep, total_ret)
total_u= total_u[,c(2,3,1)]
hchart(total_u, "line" ,hcaes(x=date, y=tot_dep,group= variable))  %>% hc_add_theme(hc_theme_google())
hchart(economics_long2, "line", hcaes(x = date, y = value01, group = variable)) %>% hc_add_theme(hc_theme_google())

class(total_u)
class(economics_long2)

############## 2176086 id con comportamiento entre retiro y deposito uniforme #######
#save(Base_resumen_pj_extract_m23_extns, file = "Base_resumen_pj_extract_m23_extns.RData")
load("Base_resumen_pj_extract_m23_extns.RData")
# Devengado Cuentas Vista
filtrado=grep("transf",persona_j_vigentes$Descripcion, ignore.case = T)
persona= persona_j_vigentes[filtrado,]
info_ACH= unique(persona$Descripcion)
#info_ACH= info_ACH[-5]
#
persona1= Base_resumen_extract %>% filter(ID_Cliente==2706649)
###
#Base_resumen_pj_extract_m23_extns= Base_resumen_pj_extract_m23_extns %>% filter(Descripcion %in% unique(categorias$variable))
#### particion de los 304 pj
persona_j_vigentes= Base_resumen_extract %>% filter(ID_Cliente %in% unique(PERSONAS_JURIDICAS_bajo_riesgo$USERID))
persona_j_vigentes_c= PERSONAS_JURIDICAS_bajo_riesgo %>% filter(!(USERID %in% persona_j_vigentes$ID_Cliente))
#
persona_j_vigentes_1= persona_j_vigentes %>% filter(año>=2023)
persona_j_vigentes_2= persona_j_vigentes %>% filter(!(ID_Cliente %in% unique(persona_j_vigentes_1$ID_Cliente)))#sin movimiento en 2023
#persona_j_vigentes_1= persona_j_vigentes_1 %>% filter(!(Descripcion %in% c("Devengado Cuentas Vista","Ajuste Operaciones Canceladas")))
#Base_resumen_pj_extract_m23_extns= Base_resumen_pj_extract_m23_extns %>% filter(!(Descripcion %in% c("Devengado Cuentas Vista")))
#tipo_op=NULL
# for (i in 1:nrow(persona_j_vigentes_1)) {
#   if(persona_j_vigentes_1$Descripcion[i] %in% categorias$variable )
#     tipo_op[i]="ACH"
#   else tipo_op[i]= "Regular"
# }
#persona_j_vigentes_1= data.frame(persona_j_vigentes_1, tipo_op)
tipo_op=NULL
tipo_ach= categorias %>% filter(tipo=="ACH")
for (i in 1:nrow(Base_resumen_extract)) {
  print(i)
  if(Base_resumen_extract$Descripcion[i] %in% unique(tipo_ach$variable) )
    tipo_op[i]="ACH"
  else tipo_op[i]= "Regular"
}
Base_resumen_extract= data.frame(Base_resumen_extract, tipo_op)
prueba= persona_j_vigentes %>% group_by(ID_Cliente, tipo_op) %>% summarise(total=sum(tot_dep))
####
LCL_tot_dep=NULL
LC_tot_dep=NULL
UCL_tot_dep=NULL
#
LCL_to_ret=NULL
LC_to_ret=NULL
UCL_to_ret=NULL
#
LCL_cant_dep=NULL
LC_cant_dep=NULL
UCL_cant_dep=NULL
#
LCL_cant_ret=NULL
LC_cant_ret=NULL
UCL_cant_ret=NULL
#
prueba_r= prueba %>% filter(tipo_op=="Regular")
prueba_r= prueba_r %>% group_by(ID_Cliente) %>% filter(row_number()==1)
prueba_c= prueba %>% filter(tipo_op=="ACH")
prueba_c= prueba_c %>% group_by(ID_Cliente) %>% filter(row_number()==1)

for (i in 1:nrow(prueba_r)) {
  control1=NULL
  control2=NULL
  control3=NULL
  control4=NULL

  persona= Base_resumen_extract %>% filter(ID_Cliente==prueba_r$ID_Cliente[i])
  persona$cant_dep[is.na(persona$cant_dep)]=0
  persona$cant_ret[is.na(persona$cant_ret)]=0
  #persona= persona %>% filter(año>2021)
  persona= persona %>% mutate(date= paste(año,mes,"1", sep = "-"))
  persona$date= as.Date(persona$date)
  persona= persona %>% filter(tipo_op=="Regular") %>% group_by(date) %>% summarise(tot_dep=sum(tot_dep),to_ret= sum(to_ret),
                                                                                   cant_dep= sum(cant_dep),
                                                                                   cant_ret= sum(cant_ret))
  if(dim(persona)[1]==1)
  {
    #
      LCL_tot_dep[i] =persona$tot_dep
      LC_tot_dep[i]=persona$tot_dep
      UCL_tot_dep[i]=persona$tot_dep
      #
      LCL_to_ret[i]=persona$to_ret
      LC_to_ret[i]=persona$to_ret
      UCL_to_ret[i]=persona$to_ret
      #
      LCL_cant_dep[i]=persona$cant_dep
      LC_cant_dep[i]=persona$cant_dep
      UCL_cant_dep[i]=persona$cant_dep
      #
      LCL_cant_ret[i]=persona$cant_ret
      LC_cant_ret[i]=persona$cant_ret
      UCL_cant_ret[i]=persona$cant_ret

  }
  else {
  #######
    pers1= persona %>% filter(tot_dep!=0)
    if(dim(pers1)[1]==0)
    {
      LCL_tot_dep[i] =0
      LC_tot_dep[i]=0
      UCL_tot_dep[i]=0
    }
    else if(dim(pers1)[1]==1)
    {
      LCL_tot_dep[i] =persona$tot_dep
      LC_tot_dep[i]=persona$tot_dep
      UCL_tot_dep[i]=persona$tot_dep
    }
    else {
      control1=qcc(pers1$tot_dep, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_tot_dep[i] =control1$limits[1]
      LC_tot_dep[i]=control1$center
      UCL_tot_dep[i]=control1$limits[2]
    }
    #
    pers2= persona %>% filter(to_ret!=0)
    if(dim(pers2)[1]==0)
    {
      LCL_to_ret[i] =0
      LC_to_ret[i]=0
      UCL_to_ret[i]=0
    }
    else if(dim(pers2)[1]==1)
    {
      LCL_to_ret[i] =persona$to_ret
      LC_to_ret[i]=persona$to_ret
      UCL_to_ret[i]=persona$to_ret
    }
    else {
      control2=qcc(pers2$to_ret, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_to_ret[i]=control2$limits[1]
      LC_to_ret[i]=control2$center
      UCL_to_ret[i]=control2$limits[2]
    }
    #
    pers3= persona %>% filter(cant_dep!=0)
    if(dim(pers3)[1]==0)
    {
      LCL_cant_dep[i] =0
      LC_cant_dep[i]=0
      UCL_cant_dep[i]=0
    }
    else if(dim(pers3)[1]==1)
    {
      LCL_cant_dep[i] =persona$cant_dep
      LC_cant_dep[i]=persona$cant_dep
      UCL_cant_dep[i]=persona$cant_dep
    }
    else {
      control3=qcc(pers3$cant_dep, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_cant_dep[i]=control3$limits[1]
      LC_cant_dep[i]=control3$center
      UCL_cant_dep[i]=control3$limits[2]
    }
    #
    pers4= persona %>% filter(cant_ret!=0)
    if(dim(pers4)[1]==0)
    {
      LCL_cant_dep[i] =0
      LC_cant_dep[i]=0
      UCL_cant_dep[i]=0
    }
    else if(dim(pers4)[1]==1)
    {
      LCL_cant_dep[i] =persona$cant_ret
      LC_cant_dep[i]=persona$cant_ret
      UCL_cant_dep[i]=persona$cant_ret
    }
    else {
      control4=qcc(pers4$cant_ret, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_cant_ret[i]=control4$limits[1]
      LC_cant_ret[i]=control4$center
      UCL_cant_ret[i]=control4$limits[2]
    }

  }
}
base_limites_R= data.frame(prueba_r, LCL_tot_dep, LC_tot_dep, UCL_tot_dep,LCL_to_ret,LC_to_ret,UCL_to_ret,
                           LCL_cant_dep,LC_cant_dep,UCL_cant_dep,LCL_cant_ret,LC_cant_ret,UCL_cant_ret)
base_limites_R$LCL_cant_dep= trunc(base_limites_R$LCL_cant_dep+1)
base_limites_R$LC_cant_dep= trunc(base_limites_R$LC_cant_dep+1)
base_limites_R$UCL_cant_dep= trunc(base_limites_R$UCL_cant_dep+1)
base_limites_R$LCL_cant_ret= trunc(base_limites_R$LCL_cant_ret+1)
base_limites_R$LC_cant_ret= trunc(base_limites_R$LC_cant_ret+1)
base_limites_R$UCL_cant_ret= trunc(base_limites_R$UCL_cant_ret+1)
base_limites_R$LCL_cant_dep[is.na(base_limites_R$LCL_cant_dep)]=0
base_limites_R$LC_cant_dep[is.na(base_limites_R$LC_cant_dep)]=0
base_limites_R$UCL_cant_dep[is.na(base_limites_R$UCL_cant_dep)]=0
base_limites_R$LCL_cant_ret[is.na(base_limites_R$LCL_cant_ret)]=0
base_limites_R$LC_cant_ret[is.na(base_limites_R$LC_cant_ret)]=0
base_limites_R$UCL_cant_ret[is.na(base_limites_R$UCL_cant_ret)]=0
#
PERSONAS_JURIDICAS_bajo_riesgo$USERID= as.character(PERSONAS_JURIDICAS_bajo_riesgo$USERID)
PERSONAS_JURIDICAS_bajo_riesgo_1= PERSONAS_JURIDICAS_bajo_riesgo %>% mutate(corre=1:nrow(PERSONAS_JURIDICAS_bajo_riesgo))
PERSONAS_JURIDICAS_bajo_riesgo_1= PERSONAS_JURIDICAS_bajo_riesgo %>% left_join(base_limites_R, by=c("USERID"="ID_Cliente"))

#
PERSONAS_JURIDICAS$USERID= as.character(PERSONAS_JURIDICAS$USERID)
PERSONAS_JURIDICAS_1= PERSONAS_JURIDICAS %>% mutate(corre=1:nrow(PERSONAS_JURIDICAS))
PERSONAS_JURIDICAS_1= PERSONAS_JURIDICAS %>% left_join(base_limites_R, by=c("USERID"="ID_Cliente"))
##########################
####
LCL_tot_dep=NULL
LC_tot_dep=NULL
UCL_tot_dep=NULL
#
LCL_to_ret=NULL
LC_to_ret=NULL
UCL_to_ret=NULL
#
LCL_cant_dep=NULL
LC_cant_dep=NULL
UCL_cant_dep=NULL
#
LCL_cant_ret=NULL
LC_cant_ret=NULL
UCL_cant_ret=NULL
for (i in 1:nrow(prueba_c)) {
  control1=NULL
  control2=NULL
  control3=NULL
  control4=NULL

  persona= Base_resumen_pj_extract_m23_extns %>% filter(ID_Cliente==prueba_c$ID_Cliente[i])
  persona$cant_dep[is.na(persona$cant_dep)]=0
  persona$cant_ret[is.na(persona$cant_ret)]=0
  #persona= persona %>% filter(año>2021)
  persona= persona %>% mutate(date= paste(año,mes,"1", sep = "-"))
  persona$date= as.Date(persona$date)
  persona= persona %>% filter(tipo_op=="ACH") %>% group_by(date) %>% summarise(tot_dep=sum(tot_dep),to_ret= sum(to_ret),
                                                                                   cant_dep= sum(cant_dep),
                                                                                   cant_ret= sum(cant_ret))
  if(dim(persona)[1]==1)
  {
    #
    LCL_tot_dep[i] =persona$tot_dep
    LC_tot_dep[i]=persona$tot_dep
    UCL_tot_dep[i]=persona$tot_dep
    #
    LCL_to_ret[i]=persona$to_ret
    LC_to_ret[i]=persona$to_ret
    UCL_to_ret[i]=persona$to_ret
    #
    LCL_cant_dep[i]=persona$cant_dep
    LC_cant_dep[i]=persona$cant_dep
    UCL_cant_dep[i]=persona$cant_dep
    #
    LCL_cant_ret[i]=persona$cant_ret
    LC_cant_ret[i]=persona$cant_ret
    UCL_cant_ret[i]=persona$cant_ret

  }
  else {
    #######
    pers1= persona %>% filter(tot_dep!=0)
    if(dim(pers1)[1]==0)
    {
      LCL_tot_dep[i] =0
      LC_tot_dep[i]=0
      UCL_tot_dep[i]=0
    }
    else if(dim(pers1)[1]==1)
    {
      LCL_tot_dep[i] =persona$tot_dep
      LC_tot_dep[i]=persona$tot_dep
      UCL_tot_dep[i]=persona$tot_dep
    }
    else {
      control1=qcc(pers1$tot_dep, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_tot_dep[i] =control1$limits[1]
      LC_tot_dep[i]=control1$center
      UCL_tot_dep[i]=control1$limits[2]
    }
    #
    pers2= persona %>% filter(to_ret!=0)
    if(dim(pers2)[1]==0)
    {
      LCL_to_ret[i] =0
      LC_to_ret[i]=0
      UCL_to_ret[i]=0
    }
    else if(dim(pers2)[1]==1)
    {
      LCL_to_ret[i] =persona$to_ret
      LC_to_ret[i]=persona$to_ret
      UCL_to_ret[i]=persona$to_ret
    }
    else {
      control2=qcc(pers2$to_ret, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_to_ret[i]=control2$limits[1]
      LC_to_ret[i]=control2$center
      UCL_to_ret[i]=control2$limits[2]
    }
    #
    pers3= persona %>% filter(cant_dep!=0)
    if(dim(pers3)[1]==0)
    {
      LCL_cant_dep[i] =0
      LC_cant_dep[i]=0
      UCL_cant_dep[i]=0
    }
    else if(dim(pers3)[1]==1)
    {
      LCL_cant_dep[i] =persona$cant_dep
      LC_cant_dep[i]=persona$cant_dep
      UCL_cant_dep[i]=persona$cant_dep
    }
    else {
      control3=qcc(pers3$cant_dep, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_cant_dep[i]=control3$limits[1]
      LC_cant_dep[i]=control3$center
      UCL_cant_dep[i]=control3$limits[2]
    }
    #
    pers4= persona %>% filter(cant_ret!=0)
    if(dim(pers4)[1]==0)
    {
      LCL_cant_dep[i] =0
      LC_cant_dep[i]=0
      UCL_cant_dep[i]=0
    }
    else if(dim(pers4)[1]==1)
    {
      LCL_cant_dep[i] =persona$cant_ret
      LC_cant_dep[i]=persona$cant_ret
      UCL_cant_dep[i]=persona$cant_ret
    }
    else {
      control4=qcc(pers4$cant_ret, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_cant_ret[i]=control4$limits[1]
      LC_cant_ret[i]=control4$center
      UCL_cant_ret[i]=control4$limits[2]
    }

  }
}
base_limites_ACH= data.frame(prueba_c, LCL_tot_dep_ACH=LCL_tot_dep, LC_tot_dep_ACH=LC_tot_dep, UCL_tot_dep_ACH=UCL_tot_dep,
                           LCL_to_ret_ACH=LCL_to_ret,LC_to_ret_ACH=LC_to_ret,UCL_to_ret_ACH=UCL_to_ret,
                           LCL_cant_dep_ACH=LCL_cant_dep,LC_cant_dep_ACH=LC_cant_dep,
                           UCL_cant_dep_ACH=UCL_cant_dep,LCL_cant_ret_ACH=LCL_cant_ret,
                           LC_cant_ret_ACH=LC_cant_ret,UCL_cant_ret_ACH=UCL_cant_ret)
base_limites_ACH$LCL_cant_dep_ACH= trunc(base_limites_ACH$LCL_cant_dep_ACH+1)
base_limites_ACH$LC_cant_dep_ACH= trunc(base_limites_ACH$LC_cant_dep_ACH+1)
base_limites_ACH$UCL_cant_dep_ACH= trunc(base_limites_ACH$UCL_cant_dep_ACH+1)
base_limites_ACH$LCL_cant_ret_ACH= trunc(base_limites_ACH$LCL_cant_ret_ACH+1)
base_limites_ACH$LC_cant_ret_ACH= trunc(base_limites_ACH$LC_cant_ret_ACH+1)
base_limites_ACH$UCL_cant_ret_ACH= trunc(base_limites_ACH$UCL_cant_ret_ACH+1)
base_limites_ACH$LCL_cant_dep_ACH[is.na(base_limites_ACH$LCL_cant_dep_ACH)]=0
base_limites_ACH$LC_cant_dep_ACH[is.na(base_limites_ACH$LC_cant_dep_ACH)]=0
base_limites_ACH$UCL_cant_dep_ACH[is.na(base_limites_ACH$UCL_cant_dep_ACH)]=0
base_limites_ACH$LCL_cant_ret_ACH[is.na(base_limites_ACH$LCL_cant_ret_ACH)]=0
base_limites_ACH$LC_cant_ret_ACH[is.na(base_limites_ACH$LC_cant_ret_ACH)]=0
base_limites_ACH$UCL_cant_ret_ACH[is.na(base_limites_ACH$UCL_cant_ret_ACH)]=0
#
PERSONAS_JURIDICAS_bajo_riesgo$USERID= as.character(PERSONAS_JURIDICAS_bajo_riesgo$USERID)
PERSONAS_JURIDICAS_bajo_riesgo_2= PERSONAS_JURIDICAS_bajo_riesgo %>% mutate(corre=1:nrow(PERSONAS_JURIDICAS_bajo_riesgo))
PERSONAS_JURIDICAS_bajo_riesgo_2= PERSONAS_JURIDICAS_bajo_riesgo %>% left_join(base_limites_ACH, by=c("USERID"="ID_Cliente"))
PERSONAS_JURIDICAS__bajo_riesgo_vf= cbind(PERSONAS_JURIDICAS_bajo_riesgo_1, PERSONAS_JURIDICAS_bajo_riesgo_2[,25:38])
#save(PERSONAS_JURIDICAS__bajo_riesgo_vf, file = "PERSONAS_JURIDICAS__bajo_riesgo_vf.RData")
#
PERSONAS_JURIDICAS$USERID= as.character(PERSONAS_JURIDICAS$USERID)
PERSONAS_JURIDICAS_2= PERSONAS_JURIDICAS %>% mutate(corre=1:nrow(PERSONAS_JURIDICAS))
PERSONAS_JURIDICAS_2= PERSONAS_JURIDICAS %>% left_join(base_limites_ACH, by=c("USERID"="ID_Cliente"))
#
PERSONAS_JURIDICAS_vf= cbind(PERSONAS_JURIDICAS_1, PERSONAS_JURIDICAS_2[,29:40])
#################### personas juridicas bajo riesgo
PERSONAS_JURIDICAS_bajo_riesgo= BD_Personas_Juridicas %>% filter(CLASIFICACION=="BAJO RIESGO")


##################
Estado=NULL
for (i in 1:nrow(PERSONAS_JURIDICAS_1)) {
  if(PERSONAS_JURIDICAS_1$USERID[i] %in% base_limites_R$ID_Cliente )
  {
    Estado[i]="Re_asignado"
  }
  else if(PERSONAS_JURIDICAS_1$USERID[i] %in% persona_j_vigentes_2$ID_Cliente)
  {
    Estado[i]="Sin_movimiento_en_2023"
  }
}
table(Estado)
PERSONAS_JURIDICAS_1= data.frame(PERSONAS_JURIDICAS_1, Estado)
PERSONAS_JURIDICAS_1$Estado[is.na(PERSONAS_JURIDICAS_1$Estado)]="Sin_Registros_antes_de_2020"
#######
LCL_R2=NULL
UCL_R2=NULL
LC_R2=NULL
LCL_R1=NULL
UCL_R1=NULL
LC_R1=NULL
for (i in 1:nrow(prueba_c)) {
  control1=NULL
  persona= Base_resumen_pj_extract_m23_extns %>% filter(ID_Cliente==prueba_c$ID_Cliente[i])
 # persona= persona %>% filter(año>2021)
  persona= persona %>% mutate(date= paste(año,mes,"1", sep = "-"))
  persona$date= as.Date(persona$date)
  persona= persona %>% filter(tipo_op=="ACH") %>% group_by(date) %>% summarise(tot_dep=sum(tot_dep))
  if(dim(persona)[1]==1)
  {
    UCL_R2[i]= persona$tot_dep
    LC_R2[i]=  persona$tot_dep
    LCL_R2[i]=persona$tot_dep
  }
  else {
    control2=qcc(persona$tot_dep, type = "xbar.one", std.dev = "MR", nsigmas = 2)
    UCL_R2[i]=control2$limits[2]
    LC_R2[i]=control2$center
    LCL_R2[i]=control2$limits[1]
    #
    control1=qcc(persona$tot_dep, type = "xbar.one", std.dev = "MR", nsigmas = 1)
    UCL_R1[i]=control1$limits[2]
    LC_R1[i]=control1$center
    LCL_R1[i]=control1$limits[1]
  }
}
base_limites_ACH= data.frame(prueba_c,LC_R2,UCL_R2,LC_R1,UCL_R1)
PERSONAS_JURIDICAS$USERID= as.character(PERSONAS_JURIDICAS$USERID)
PERSONAS_JURIDICAS_2= PERSONAS_JURIDICAS %>% mutate(corre=1:nrow(PERSONAS_JURIDICAS))
PERSONAS_JURIDICAS_2= PERSONAS_JURIDICAS %>% left_join(base_limites_ACH, by=c("USERID"="ID_Cliente"))
#
Estado=NULL
for (i in 1:nrow(PERSONAS_JURIDICAS_2)) {
  if(PERSONAS_JURIDICAS_2$USERID[i] %in% base_limites_R$ID_Cliente )
  {
    Estado[i]="Re_asignado"
  }
  else if(PERSONAS_JURIDICAS_2$USERID[i] %in% persona_j_vigentes_2$ID_Cliente)
  {
    Estado[i]="Sin_movimiento_en_2023"
  }
}
table(Estado)
PERSONAS_JURIDICAS_2= data.frame(PERSONAS_JURIDICAS_2, Estado)
PERSONAS_JURIDICAS_2$Estado[is.na(PERSONAS_JURIDICAS_2$Estado)]="Sin_Registros_antes_de_2020"
#save(PERSONAS_JURIDICAS_1, file = "PERSONAS_JURIDICAS_1.RData")


##########################
#grafico de total dep y total retiros
cliente=2176086
persona= Base_resumen_pj_extract_m23 %>% filter(ID_Cliente==cliente)
persona= persona %>% mutate(date= paste(año,mes,"1", sep = "-"))
persona$date=as.Date(persona$date)
total_dep= persona[,c(11,20)]
total_dep= total_dep %>% mutate(variable="Total_depositos")
total_ret= persona[,c(12,20)]
total_ret= total_ret %>% mutate(variable="Total_retiros")
names(total_ret)= names(total_dep)
total_u= rbind(total_dep, total_ret)
total_u= total_u[,c(2,3,1)]
hchart(total_u, "line" ,hcaes(x=date, y=tot_dep,group= variable)) %>% hc_title(text=paste("PJ:",unique(persona$RAZON_SOCIAL)) )  %>% hc_add_theme(hc_theme_google())
# grafico limites
control1=NULL
persona= Base_resumen_pj_extract_m23 %>% filter(ID_Cliente==cliente)
persona= persona %>% mutate(date= paste(año,mes,"1", sep = "-"))
persona$date=as.Date(persona$date)
persona= persona %>% filter(año>2021)
control1=qcc(persona$tot_dep, type = "xbar.one", std.dev = "MR", nsigmas = 2)
persona= persona %>% mutate(LCL=control1$limits[1])
persona= persona %>% mutate(CL=control1$center)
persona= persona %>% mutate(UCL=control1$limits[2])
#
total_1= persona[,c(20,11)]
names(total_1)[2]="Valor"
total_1= total_1 %>% mutate(variable="Total depósitos")
#
total_2= persona[,c(20,22)]
total_2= total_2 %>% mutate(variable="Centro")
names(total_2)= names(total_1)
#
total_3= persona[,c(20,23)]
total_3= total_3 %>% mutate(variable="Límite de confianza superior")
names(total_3)= names(total_1)
total_graf= rbind(total_1, total_2, total_3)
total_graf$Valor= round(total_graf$Valor,1)
hchart(total_graf, "line" ,hcaes(x=date, y=Valor,group= variable)) %>% hc_title(text=paste("PJ:",unique(persona$RAZON_SOCIAL)) )  %>% hc_add_theme(hc_theme_google())

#persona= persona %>% mutate(Fuera_de_control= ifelse(tot_dep>UCL,1,2))
#persona= persona %>% filter(Fuera_de_control==2)




### con base segip
unique(REPORTE_PERSONAS$CASO)
SOLICITUDES_CIERRES_CAJA_DE_AHORRO$Fecha_de_Registro=as.Date(SOLICITUDES_CIERRES_CAJA_DE_AHORRO$Fecha_de_Registro)
save(SOLICITUDES_CIERRES_CAJA_DE_AHORRO, file = "SOLICITUDES_CIERRES_CAJA_DE_AHORRO.RData")


mpgman2 <- mpg %>%
  count(class, year) %>%
  glimpse()
datos_cierre_caja= SOLICITUDES_CIERRES_CAJA_DE_AHORRO[,c(10,14,11,13,16)]

#### con tipo estado cca
datos_cierre_caja= datos_cierre_caja %>% group_by(Fecha_de_Registro ,Estado_CCA, Moneda) %>% summarise(cantidad=length(Importe_de_cierre),
                                                                                    total=sum(Importe_de_cierre))
datos_cierre_caja_1= datos_cierre_caja[,1:4]
datos_cierre_caja_2= datos_cierre_caja[,c(1:3,5)]
datos_cierre_caja_1$Estado_CCA[datos_cierre_caja_1$Estado_CCA=="Operación Cancelada"]="Operación cancelada cantidad"
datos_cierre_caja_1$Estado_CCA[datos_cierre_caja_1$Estado_CCA=="BLOQUEO A CREDITOS"]="BLOQUEO A CREDITOS cantidad"
datos_cierre_caja_1$Estado_CCA[datos_cierre_caja_1$Estado_CCA=="NORMAL"]="NORMAL cantidad"
names(datos_cierre_caja_1)= names(datos_cierre_caja_2)
datos_cierre_caja_1= datos_cierre_caja_1 %>% mutate(Estado_CCA= paste(Estado_CCA, Moneda))
datos_cierre_caja_2= datos_cierre_caja_2 %>% mutate(Estado_CCA= paste(Estado_CCA, Moneda))

datos_cierre_caja_vf= rbind(datos_cierre_caja_1, datos_cierre_caja_2)
datos_cierre_caja_vf= datos_cierre_caja_vf %>% filter(total!=0)
hchart(datos_cierre_caja_vf, "line" ,hcaes(x=Fecha_de_Registro, y=total,group= Estado_CCA)) %>%
  hc_title(text=paste("Solicitudes de","cierres de cajas de ahorro"))  %>% hc_add_theme(hc_theme_google())
##### con tipo de cuenta
datos_cierre_caja= datos_cierre_caja %>% group_by(Fecha_de_Registro ,Tipo_Cuenta, Moneda) %>% summarise(cantidad=length(Importe_de_cierre),
                                                                                                       total=sum(Importe_de_cierre))
datos_cierre_caja_1= datos_cierre_caja[,1:4]
datos_cierre_caja_2= datos_cierre_caja[,c(1:3,5)]
datos_cierre_caja_1$Tipo_Cuenta[datos_cierre_caja_1$Tipo_Cuenta=="Individual"]="Individual cantidad"
datos_cierre_caja_1$Tipo_Cuenta[datos_cierre_caja_1$Tipo_Cuenta=="Colectiva"]="Colectiva cantidad"
#datos_cierre_caja_1$Estado_CCA[datos_cierre_caja_1$Estado_CCA=="NORMAL"]="NORMAL cantidad"
names(datos_cierre_caja_1)= names(datos_cierre_caja_2)
datos_cierre_caja_1= datos_cierre_caja_1 %>% mutate(Tipo_Cuenta= paste(Tipo_Cuenta, Moneda))
datos_cierre_caja_2= datos_cierre_caja_2 %>% mutate(Tipo_Cuenta= paste(Tipo_Cuenta, Moneda))

datos_cierre_caja_vf= rbind(datos_cierre_caja_1, datos_cierre_caja_2)
datos_cierre_caja_vf= datos_cierre_caja_vf %>% filter(total!=0)
hchart(datos_cierre_caja_vf, "line" ,hcaes(x=Fecha_de_Registro, y=total,group= Tipo_Cuenta)) %>%
  hc_title(text=paste("Solicitudes de","cierres de cajas de ahorro"))  %>% hc_add_theme(hc_theme_google())

##
hchart(mpgman2, "column", hcaes(x = class, y = n, group = year)) %>% hc_add_theme(hc_theme_darkunica())
hchart(datos_cierre_caja, "column", hcaes(x = Estado_CCA, y = total, group = Moneda)) %>% hc_add_theme(hc_theme_gridlight())


########### base ros ############
BASE_ROS= BASE_ROS %>% mutate(fecha_ident_inusual= paste(substr(Fecha_Identificacion_INUSUAL,1,7),"1", sep = "-"))
BASE_ROS= BASE_ROS %>% mutate(fecha_emis_informe= paste(substr(Fecha_Identificacion_INUSUAL,1,7),"1", sep = "-"))
#

datos= BASE_ROS %>% group_by(fecha_ident_inusual) %>% summarise(cantidad=n())
datos= datos %>% mutate(categoria= "cantidad identificación inusuales")
names(datos)[1]="fecha"
datos1= BASE_ROS %>% group_by(fecha_emis_informe) %>% summarise(cantidad=n())
datos1= datos1 %>% mutate(categoria= "cantidad emisión informes")
names(datos1)[1]="fecha"
#datos2= datos %>% left_join(datos1, by=c("fecha_ident_inusual"="fecha_emis_informe"))
datos2= rbind(datos, datos1)
datos2$fecha=as.Date(datos2$fecha)
hchart(datos2, "spline" ,hcaes(x=fecha, y=cantidad,group= categoria)) %>%
  hc_title(text=paste("Reportes"," ROS"))  %>% hc_add_theme(hc_theme_google()) %>% hc_rangeSelector(enabled= TRUE,
                                                                                                    verticalAlign = "top")
#


############### PARA OBTENER MUESTRAS ALEATORIA DE UNA BASE DE DATOS ######
muestra= Base_Clientes_BSol_vf[sample(1:nrow(Base_Clientes_BSol_vf), 10000, replace=F),]
#
datos= BASE_ROS %>% left_join(Base_Clientes_BSol_vf[,c(5,9)], by=c("CI"="NUMERO_DOC"))
#
datos1= BASE_INUSUALIDADES %>% filter(año>=2022)
datos1= datos1 %>% mutate(fecha_emis_informe= paste(substr(FECHA_DE_EMISION_DEL_INFORME,1,7),"1", sep = "-"))
datos2= datos1 %>% group_by(fecha_emis_informe, ROS) %>% summarise(cantidad=n())
datos2= datos2 %>% filter(!is.na(ROS))
datos2= as.data.frame(datos2)
datos2$fecha_emis_informe= as.Date(datos2$fecha_emis_informe)
datos2$ROS[datos2$ROS=="NO"]="Inusualidad"
datos2$ROS[datos2$ROS=="SI"]="ROS"
hchart(datos2, "spline" ,hcaes(x=fecha_emis_informe, y=cantidad,group= ROS)) %>%
  hc_title(text=paste("Reporte"," Inusualidades"))  %>% hc_add_theme(hc_theme_google()) %>% hc_rangeSelector(enabled= TRUE,
                                                                                                    verticalAlign = "top")
##

datos1= SOLICITUDES_CIERRES_CAJA_DE_AHORRO %>% filter(Fecha_de_Registro>=as.Date("2023-03-01"))
casos= datos1 %>% group_by(NOMBRE_REGIONAL, menor_a_1000) %>% summarise(cantidad=n(), importe= sum(Importe_de_cierre))
##
datos1= BASE_ROS %>% filter(fecha_emis_informe>=as.Date("2023-3-1"))
datos1= datos1 %>% mutate(corre= 1:nrow(datos1))
datos1= datos1 %>% left_join(Base_Clientes_BSol_vf[,c(5,27)], by=c("CI"="NUMERO_DOC"))


##
datos1= BASE_INUSUALIDADES %>% filter(FECHA_DE_EMISION_DEL_INFORME>=as.Date("2023-3-1"))


#hc_theme_gridlight()
# barras normales
hchart(datos2, "column", hcaes(x = fecha_emis_informe, y = cantidad, group = ROS))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_rangeSelector(enabled= TRUE, verticalAlign = "top")
#
# barras normales
hchart(datos2, "bar", hcaes(x = fecha_emis_informe, y = cantidad, group = ROS),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
        # , # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
         )
       )  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_rangeSelector(enabled= TRUE, verticalAlign = "top")
# barras apiladas
hchart(datos2, "column", hcaes(x = fecha_emis_informe, y = cantidad, group = ROS),
       stacking = list(enabled = TRUE))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_rangeSelector(enabled= TRUE, verticalAlign = "top")
# en porcentaje
hchart(datos2, "column", hcaes(x = fecha_emis_informe, y = cantidad, group = ROS),
       stacking = "percent")  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_rangeSelector(enabled= TRUE, verticalAlign = "top")
#barras mostrando porcentajes de cada barra
hchart(datos2, "column", hcaes(x = fecha_emis_informe, y = cantidad, group = ROS),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE, # Añadir etiquetas
         format = "{point.percentage:.0f}%"))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_rangeSelector(enabled= TRUE, verticalAlign = "top") %>% hc_title(text="Gráfico de barras porcentuales" ) %>%
  hc_colors(c("#701F81FF", "#F8765CFF"))
## diagrama de areas con etiquetas
hchart(datos2, "area" ,hcaes(x=fecha_emis_informe, y=cantidad,group= ROS),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE )) %>%
  hc_title(text=paste("Reporte"," Inusualidades"))  %>% hc_add_theme(hc_theme_google()) %>%
  hc_rangeSelector(enabled= TRUE, verticalAlign = "top") %>% hc_colors(c("#701F81FF", "#F8765CFF"))






library(viridisLite)

cols <- viridis(3)
cols <- substr(cols, 0, 7)

highchart() %>%
  hc_add_series(data = sample(1:12)) %>%
  hc_add_series(data = sample(1:12) + 10) %>%
  hc_add_series(data = sample(1:12) + 20) %>%
  hc_colors(cols)

c("#440154", "#21908C", "#FDE725")

 c("#32B67AFF", "#34B679FF", "#35B779FF", "#37B878FF", "#38B977FF", "#3ABA76FF", "#3BBB75FF", "#3DBC74FF", "#3FBC73FF", "#40BD72FF", "#42BE71FF", "#44BF70FF",
 "#46C06FFF", "#48C16EFF", "#4AC16DFF", "#4CC26CFF", "#4EC36BFF", "#50C46AFF", "#52C569FF", "#54C568FF", "#56C667FF", "#58C765FF", "#5AC864FF", "#5CC863FF",
 "#5EC962FF", "#60CA60FF", "#63CB5FFF", "#65CB5EFF", "#67CC5CFF", "#69CD5BFF", "#6CCD5AFF", "#6ECE58FF", "#70CF57FF", "#73D056FF", "#75D054FF", "#77D153FF",
 "#7AD151FF", "#7CD250FF", "#7FD34EFF", "#81D34DFF", "#84D44BFF", "#86D549FF", "#89D548FF", "#8BD646FF", "#8ED645FF", "#90D743FF", "#93D741FF", "#95D840FF",
 "#98D83EFF", "#9BD93CFF", "#9DD93BFF", "#A0DA39FF", "#A2DA37FF", "#A5DB36FF", "#A8DB34FF", "#AADC32FF", "#ADDC30FF", "#B0DD2FFF", "#B2DD2DFF", "#B5DE2BFF",
 "#B8DE29FF", "#BADE28FF", "#BDDF26FF", "#C0DF25FF", "#C2DF23FF", "#C5E021FF", "#C8E020FF", "#CAE11FFF", "#CDE11DFF", "#D0E11CFF", "#D2E21BFF", "#D5E21AFF",
 "#D8E219FF", "#DAE319FF", "#DDE318FF", "#DFE318FF", "#E2E418FF", "#E5E419FF", "#E7E419FF", "#EAE51AFF", "#ECE51BFF", "#EFE51CFF", "#F1E51DFF", "#F4E61EFF",
 "#F6E620FF", "#F8E621FF", "#FBE723FF", "#FDE725FF")



viridis(256, option = "F")

c( "#000004FF", "#010005FF", "#010106FF", "#010108FF", "#020109FF", "#02020BFF", "#02020DFF", "#03030FFF", "#030312FF", "#040414FF", "#050416FF", "#060518FF"
 ,"#06051AFF", "#07061CFF", "#08071EFF", "#090720FF", "#0A0822FF", "#0B0924FF", "#0C0926FF", "#0D0A29FF", "#0E0B2BFF", "#100B2DFF", "#110C2FFF", "#120D31FF"
 ,"#130D34FF", "#140E36FF", "#150E38FF", "#160F3BFF", "#180F3DFF", "#19103FFF", "#1A1042FF", "#1C1044FF", "#1D1147FF", "#1E1149FF", "#20114BFF", "#21114EFF"
 ,"#221150FF", "#241253FF", "#251255FF", "#271258FF", "#29115AFF", "#2A115CFF", "#2C115FFF", "#2D1161FF", "#2F1163FF", "#311165FF", "#331067FF", "#341069FF"
 ,"#36106BFF", "#38106CFF", "#390F6EFF", "#3B0F70FF", "#3D0F71FF", "#3F0F72FF", "#400F74FF", "#420F75FF", "#440F76FF", "#451077FF", "#471078FF", "#491078FF"
 ,"#4A1079FF", "#4C117AFF", "#4E117BFF", "#4F127BFF", "#51127CFF", "#52137CFF", "#54137DFF", "#56147DFF", "#57157EFF", "#59157EFF", "#5A167EFF", "#5C167FFF"
 ,"#5D177FFF", "#5F187FFF", "#601880FF", "#621980FF", "#641A80FF", "#651A80FF", "#671B80FF", "#681C81FF", "#6A1C81FF", "#6B1D81FF", "#6D1D81FF", "#6E1E81FF"
 ,"#701F81FF", "#721F81FF", "#732081FF", "#752181FF", "#762181FF", "#782281FF", "#792282FF", "#7B2382FF", "#7C2382FF", "#7E2482FF", "#802582FF", "#812581FF"
 ,"#832681FF", "#842681FF", "#862781FF", "#882781FF", "#892881FF","#8B2981FF", "#8C2981FF", "#8E2A81FF", "#902A81FF", "#912B81FF", "#932B80FF", "#942C80FF"
 ,"#962C80FF", "#982D80FF", "#992D80FF", "#9B2E7FFF", "#9C2E7FFF", "#9E2F7FFF", "#A02F7FFF", "#A1307EFF", "#A3307EFF", "#A5317EFF", "#A6317DFF", "#A8327DFF"
 ,"#AA337DFF", "#AB337CFF", "#AD347CFF", "#AE347BFF", "#B0357BFF", "#B2357BFF", "#B3367AFF", "#B5367AFF", "#B73779FF", "#B83779FF", "#BA3878FF", "#BC3978FF"
 ,"#BD3977FF", "#BF3A77FF", "#C03A76FF", "#C23B75FF", "#C43C75FF", "#C53C74FF", "#C73D73FF", "#C83E73FF", "#CA3E72FF", "#CC3F71FF", "#CD4071FF", "#CF4070FF"
 ,"#D0416FFF", "#D2426FFF", "#D3436EFF", "#D5446DFF", "#D6456CFF", "#D8456CFF", "#D9466BFF", "#DB476AFF", "#DC4869FF", "#DE4968FF", "#DF4A68FF", "#E04C67FF"
 ,"#E24D66FF", "#E34E65FF", "#E44F64FF", "#E55064FF", "#E75263FF", "#E85362FF", "#E95462FF", "#EA5661FF", "#EB5760FF", "#EC5860FF", "#ED5A5FFF", "#EE5B5EFF"
 ,"#EF5D5EFF", "#F05F5EFF", "#F1605DFF", "#F2625DFF", "#F2645CFF", "#F3655CFF", "#F4675CFF", "#F4695CFF", "#F56B5CFF", "#F66C5CFF", "#F66E5CFF", "#F7705CFF"
 ,"#F7725CFF", "#F8745CFF", "#F8765CFF", "#F9785DFF", "#F9795DFF", "#F97B5DFF", "#FA7D5EFF", "#FA7F5EFF", "#FA815FFF", "#FB835FFF", "#FB8560FF", "#FB8761FF"
 ,"#FC8961FF", "#FC8A62FF", "#FC8C63FF", "#FC8E64FF", "#FC9065FF", "#FD9266FF", "#FD9467FF", "#FD9668FF", "#FD9869FF", "#FD9A6AFF", "#FD9B6BFF", "#FE9D6CFF"
 ,"#FE9F6DFF", "#FEA16EFF", "#FEA36FFF", "#FEA571FF", "#FEA772FF", "#FEA973FF", "#FEAA74FF", "#FEAC76FF", "#FEAE77FF", "#FEB078FF", "#FEB27AFF", "#FEB47BFF"
 ,"#FEB67CFF", "#FEB77EFF", "#FEB97FFF", "#FEBB81FF" ,"#FEBD82FF", "#FEBF84FF", "#FEC185FF", "#FEC287FF", "#FEC488FF", "#FEC68AFF", "#FEC88CFF", "#FECA8DFF"
 ,"#FECC8FFF", "#FECD90FF", "#FECF92FF", "#FED194FF", "#FED395FF", "#FED597FF", "#FED799FF", "#FED89AFF", "#FDDA9CFF", "#FDDC9EFF", "#FDDEA0FF", "#FDE0A1FF"
 ,"#FDE2A3FF", "#FDE3A5FF", "#FDE5A7FF", "#FDE7A9FF", "#FDE9AAFF", "#FDEBACFF", "#FCECAEFF", "#FCEEB0FF", "#FCF0B2FF", "#FCF2B4FF", "#FCF4B6FF", "#FCF6B8FF"
 ,"#FCF7B9FF", "#FCF9BBFF", "#FCFBBDFF", "#FCFDBFFF")



 viridis(256, option = "G")


 c("#03051AFF", "#04051AFF", "#05061BFF", "#06071CFF", "#07071DFF", "#08081EFF", "#0A091FFF", "#0B0920FF", "#0D0A21FF", "#0E0B22FF", "#100B23FF", "#110C24FF",
 "#130D25FF", "#140E26FF", "#160E27FF", "#170F28FF", "#180F29FF", "#1A102AFF", "#1B112BFF", "#1D112CFF", "#1E122DFF", "#20122EFF", "#211330FF", "#221331FF",
 "#241432FF", "#251433FF", "#271534FF", "#281535FF", "#2A1636FF", "#2B1637FF", "#2D1738FF", "#2E1739FF", "#30173AFF", "#31183BFF", "#33183CFF", "#34193DFF",
 "#35193EFF", "#37193FFF", "#381A40FF", "#3A1A41FF", "#3C1A42FF", "#3D1A42FF", "#3F1B43FF", "#401B44FF", "#421B45FF", "#431C46FF", "#451C47FF", "#461C48FF",
 "#481C48FF", "#491D49FF", "#4B1D4AFF", "#4C1D4BFF", "#4E1D4BFF", "#501D4CFF", "#511E4DFF", "#531E4DFF", "#541E4EFF", "#561E4FFF", "#581E4FFF", "#591E50FF",
 "#5B1E51FF", "#5C1E51FF", "#5E1F52FF", "#601F52FF", "#611F53FF", "#631F53FF", "#641F54FF", "#661F54FF", "#681F55FF", "#691F55FF", "#6B1F56FF", "#6D1F56FF",
 "#6E1F57FF", "#701F57FF", "#711F57FF", "#731F58FF", "#751F58FF", "#761F58FF", "#781F59FF", "#7A1F59FF", "#7B1F59FF", "#7D1F5AFF", "#7F1E5AFF", "#811E5AFF",
 "#821E5AFF", "#841E5AFF", "#861E5BFF", "#871E5BFF", "#891E5BFF", "#8B1D5BFF", "#8C1D5BFF", "#8E1D5BFF", "#901D5BFF", "#921C5BFF", "#931C5BFF", "#951C5BFF",
 "#971C5BFF", "#981B5BFF", "#9A1B5BFF", "#9C1B5BFF", "#9E1A5BFF", "#9F1A5BFF", "#A11A5BFF", "#A3195BFF", "#A4195BFF", "#A6195AFF", "#A8185AFF", "#AA185AFF",
 "#AB185AFF", "#AD1759FF", "#AF1759FF", "#B01759FF", "#B21758FF", "#B41658FF", "#B51657FF", "#B71657FF", "#B91657FF", "#BA1656FF", "#BC1656FF", "#BD1655FF",
 "#BF1654FF", "#C11754FF", "#C21753FF", "#C41753FF", "#C51852FF", "#C71951FF", "#C81951FF", "#CA1A50FF", "#CB1B4FFF", "#CD1C4EFF", "#CE1D4EFF", "#CF1E4DFF",
 "#D11F4CFF", "#D2204CFF", "#D3214BFF", "#D5224AFF", "#D62449FF", "#D72549FF", "#D82748FF", "#D92847FF", "#DB2946FF", "#DC2B46FF", "#DD2C45FF", "#DE2E44FF",
 "#DF2F44FF", "#E03143FF", "#E13342FF", "#E23442FF", "#E33641FF", "#E43841FF", "#E53940FF", "#E63B40FF", "#E73D3FFF", "#E83F3FFF", "#E8403EFF", "#E9423EFF",
 "#EA443EFF", "#EB463EFF", "#EB483EFF", "#EC4A3EFF", "#EC4C3EFF", "#ED4E3EFF", "#ED503EFF", "#EE523FFF", "#EE543FFF", "#EF5640FF", "#EF5840FF", "#EF5A41FF",
 "#F05C42FF", "#F05E42FF", "#F06043FF", "#F16244FF", "#F16445FF", "#F16646FF", "#F26747FF", "#F26948FF", "#F26B49FF", "#F26D4BFF", "#F26F4CFF", "#F3714DFF",
 "#F3734EFF", "#F37450FF", "#F37651FF", "#F37852FF", "#F47A54FF", "#F47C55FF", "#F47D57FF", "#F47F58FF", "#F4815AFF", "#F4835BFF", "#F4845DFF", "#F4865EFF",
 "#F58860FF", "#F58A61FF", "#F58B63FF", "#F58D64FF", "#F58F66FF", "#F59067FF", "#F59269FF", "#F5946BFF", "#F5966CFF", "#F5976EFF", "#F59970FF", "#F69B71FF",
 "#F69C73FF", "#F69E75FF", "#F6A077FF", "#F6A178FF", "#F6A37AFF", "#F6A47CFF", "#F6A67EFF", "#F6A880FF", "#F6A981FF", "#F6AB83FF", "#F6AD85FF", "#F6AE87FF",
 "#F6B089FF", "#F6B18BFF", "#F6B38DFF", "#F6B48FFF", "#F6B691FF", "#F6B893FF", "#F6B995FF", "#F6BB97FF", "#F6BC99FF", "#F6BE9BFF", "#F6BF9DFF", "#F6C19FFF",
 "#F7C2A2FF", "#F7C4A4FF", "#F7C6A6FF", "#F7C7A8FF", "#F7C9AAFF", "#F7CAACFF", "#F7CCAFFF", "#F7CDB1FF", "#F7CFB3FF", "#F7D0B5FF", "#F8D1B8FF", "#F8D3BAFF",
 "#F8D4BCFF", "#F8D6BEFF", "#F8D7C0FF", "#F8D9C3FF", "#F8DAC5FF", "#F8DCC7FF", "#F9DDC9FF", "#F9DFCBFF", "#F9E0CDFF", "#F9E2D0FF", "#F9E3D2FF", "#F9E5D4FF",
 "#FAE6D6FF", "#FAE8D8FF", "#FAE9DAFF", "#FAEBDDFF")


 c("#0B0405FF", "#0D0406FF", "#0E0508FF", "#0F0609FF", "#10060AFF", "#11070CFF", "#12080DFF", "#13090FFF", "#140910FF", "#150A12FF", "#160B13FF", "#170C15FF",
  "#180D16FF", "#190E18FF", "#1A0E19FF", "#1B0F1AFF", "#1C101CFF", "#1D111DFF", "#1E111FFF", "#1F1220FF", "#201322FF", "#211423FF", "#221425FF", "#231526FF",
  "#241628FF", "#251729FF", "#26172BFF", "#27182DFF", "#28192EFF", "#291930FF", "#291A31FF", "#2A1B33FF", "#2B1C35FF", "#2C1C36FF", "#2D1D38FF", "#2E1E39FF",
  "#2E1E3BFF", "#2F1F3DFF", "#30203EFF", "#312140FF", "#312142FF", "#322243FF", "#332345FF", "#342447FF", "#342548FF", "#35254AFF", "#35264CFF", "#36274DFF",
  "#37284FFF", "#372851FF", "#382953FF", "#382A54FF", "#392B56FF", "#3A2C58FF", "#3A2C59FF", "#3B2D5BFF", "#3B2E5DFF", "#3B2F5FFF", "#3C3060FF", "#3C3162FF",
  "#3D3164FF", "#3D3266FF", "#3E3367FF", "#3E3469FF", "#3E356BFF", "#3F366DFF", "#3F366FFF", "#3F3770FF", "#403872FF", "#403974FF", "#403A76FF", "#403B78FF",
  "#403C79FF", "#413D7BFF", "#413E7DFF", "#413E7FFF", "#413F80FF", "#414082FF", "#414184FF", "#414285FF", "#414387FF", "#414488FF", "#40468AFF", "#40478BFF",
  "#40488DFF", "#40498EFF", "#3F4A8FFF", "#3F4B90FF", "#3F4C92FF", "#3E4D93FF", "#3E4F94FF", "#3E5095FF", "#3D5195FF", "#3D5296FF", "#3C5397FF", "#3C5598FF",
  "#3B5698FF", "#3B5799FF", "#3B589AFF", "#3A599AFF", "#3A5B9BFF", "#3A5C9BFF", "#395D9CFF", "#395E9CFF", "#385F9CFF", "#38619DFF", "#38629DFF", "#38639DFF",
  "#37649EFF", "#37659EFF", "#37669EFF", "#37689FFF", "#36699FFF", "#366A9FFF", "#366B9FFF", "#366CA0FF", "#366DA0FF", "#366FA0FF", "#3670A0FF", "#3671A0FF",
  "#3572A1FF", "#3573A1FF", "#3574A1FF", "#3575A1FF", "#3576A2FF", "#3578A2FF", "#3579A2FF", "#357AA2FF", "#357BA3FF", "#357CA3FF", "#357DA3FF", "#357EA4FF",
  "#347FA4FF", "#3480A4FF", "#3482A4FF", "#3483A5FF", "#3484A5FF", "#3485A5FF", "#3486A5FF", "#3487A6FF", "#3488A6FF", "#3489A6FF", "#348BA6FF", "#348CA7FF",
  "#348DA7FF", "#348EA7FF", "#348FA7FF", "#3490A8FF", "#3491A8FF", "#3492A8FF", "#3493A8FF", "#3495A9FF", "#3496A9FF", "#3497A9FF", "#3498A9FF", "#3499AAFF",
  "#349AAAFF", "#359BAAFF", "#359CAAFF", "#359EAAFF", "#359FABFF", "#35A0ABFF", "#35A1ABFF", "#36A2ABFF", "#36A3ABFF", "#36A4ABFF", "#37A5ACFF", "#37A6ACFF",
  "#37A8ACFF", "#38A9ACFF", "#38AAACFF", "#39ABACFF", "#39ACACFF", "#3AADACFF", "#3AAEADFF", "#3BAFADFF", "#3CB1ADFF", "#3CB2ADFF", "#3DB3ADFF", "#3EB4ADFF",
  "#3FB5ADFF", "#3FB6ADFF", "#40B7ADFF", "#41B8ADFF", "#42B9ADFF", "#43BAADFF", "#44BCADFF", "#45BDADFF", "#46BEADFF", "#47BFADFF", "#48C0ADFF", "#49C1ADFF",
  "#4BC2ADFF", "#4CC3ADFF", "#4DC4ADFF", "#4FC5ADFF", "#50C6ADFF", "#52C7ADFF", "#53C9ADFF", "#55CAADFF", "#57CBADFF", "#59CCADFF", "#5BCDADFF", "#5ECDADFF",
  "#60CEACFF", "#62CFACFF", "#65D0ADFF", "#68D1ADFF", "#6AD2ADFF", "#6DD3ADFF", "#70D4ADFF", "#73D4ADFF", "#76D5AEFF", "#79D6AEFF", "#7CD6AFFF", "#7FD7AFFF",
  "#82D8B0FF", "#85D9B1FF", "#88D9B1FF", "#8BDAB2FF", "#8EDBB3FF", "#91DBB4FF", "#94DCB5FF", "#96DDB5FF", "#99DDB6FF", "#9CDEB7FF", "#9EDFB8FF", "#A1DFB9FF",
  "#A4E0BBFF", "#A6E1BCFF", "#A9E1BDFF", "#ABE2BEFF", "#AEE3C0FF", "#B0E4C1FF", "#B2E4C2FF", "#B5E5C4FF", "#B7E6C5FF", "#B9E6C7FF", "#BBE7C8FF", "#BEE8CAFF",
  "#C0E9CCFF", "#C2E9CDFF", "#C4EACFFF", "#C6EBD1FF", "#C8ECD2FF", "#CAEDD4FF", "#CCEDD6FF", "#CEEED7FF", "#D0EFD9FF", "#D2F0DBFF", "#D4F1DCFF", "#D6F1DEFF",
  "#D8F2E0FF", "#DAF3E1FF", "#DCF4E3FF", "#DEF5E5FF")




### grafico 1
Datos_Para_Grad <- read_excel("Datos_Para_Grad.xlsx")
Datos_Para_Grad1 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto2")
Datos_Para_Grad= Datos_Para_Grad %>% mutate(DD_nivel_de_riesgo= paste(Debida_Diligencia, Nivel_de_riesgo))
Datos_Para_Grad3 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto3")
Datos_Para_Grad3.1 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto3.1")
Datos_Para_Grad4 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto4")
Datos_Para_Grad4.1 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto4.1")
Datos_Para_Grad5 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto5")
Datos_Para_Grad5= Datos_Para_Grad5 %>% mutate(Mes_Tipo= paste(Mes, Tipo))
Datos_Para_Grad5.1 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto5.1")
Datos_Para_Grad6 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto6")
Datos_Para_Grad7 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto7")
Datos_Para_Grad7.1 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto7.1")
Datos_Para_Grad8 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto8")
Datos_Para_Grad8.1 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto8.1")
Datos_Para_Grad1.1 <- read_excel("Datos_Para_Grad.xlsx",sheet = "punto1.1")
puntoGhost_Comp <- read_excel("Datos_Para_Grad.xlsx",sheet = "puntoGhost_Comp")
ult <- read_excel("Datos_Para_Grad.xlsx",sheet = "ult")



#save(Datos_Para_Grad, file = "Datos_Para_Grad.RData")
#save(Datos_Para_Grad1, file = "Datos_Para_Grad1.RData")
save(Datos_Para_Grad3, file = "Datos_Para_Grad3.RData")
save(Datos_Para_Grad3.1, file = "Datos_Para_Grad3.1.RData")
save(Datos_Para_Grad4, file = "Datos_Para_Grad4.RData")
save(Datos_Para_Grad4.1, file = "Datos_Para_Grad4.1.RData")
save(Datos_Para_Grad5, file = "Datos_Para_Grad5.RData")
save(Datos_Para_Grad5.1, file = "Datos_Para_Grad5.1.RData")
save(Datos_Para_Grad6, file = "Datos_Para_Grad6.RData")
save(Datos_Para_Grad7, file = "Datos_Para_Grad7.RData")
save(Datos_Para_Grad7.1, file = "Datos_Para_Grad7.1.RData")
save(Datos_Para_Grad8, file = "Datos_Para_Grad8.RData")
save(Datos_Para_Grad8.1, file = "Datos_Para_Grad8.1.RData")
save(Datos_Para_Grad1.1, file = "Datos_Para_Grad1.1.RData")
save(puntoGhost_Comp, file = "puntoGhost_Comp.RData")
#
hchart(Datos_Para_Grad, "column", hcaes(x = DD_nivel_de_riesgo, y = Cantidad_de_Personas, group = Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE, # Añadir etiquetas
         format = "{point.percentage:.0f}%"))  %>% hc_add_theme(hc_theme_gridlight())%>%
   hc_title(text="RESULTADO APLICACIÓN DEL PROCESO DE DEBIDA DILIGENCIA 2023" ) %>%
  hc_colors(c("#FC9065FF","#F9795DFF","#F2645CFF","#E75263FF",
              "#D6456CFF","#C53C74FF","#B2357BFF","#9E2F7FFF"))

############## para el grafico del punto 1 y punto 2
hchart(Datos_Para_Grad, "column", hcaes(x = DD_nivel_de_riesgo, y = Cantidad_de_Personas, group = Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE, # Añadir etiquetas
         format = "{point.percentage:.0f}%"))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="RESULTADO APLICACIÓN DEL PROCESO DE DEBIDA DILIGENCIA 2023" ) %>%
  hc_colors(c("#FC9065FF","#F9795DFF","#F2645CFF","#E75263FF",
              "#D6456CFF","#C53C74FF","#B2357BFF","#9E2F7FFF")) %>%
  hc_add_series(Datos_Para_Grad1, "pie", hcaes(name = Tipo_Persona, y = Cantidad), name = "Listas internas",
                stacking = list(enabled = TRUE),
                dataLabels = list(
                  enabled = TRUE, # Añadir etiquetas
                  format = "{point.percentage:.0f}%")) %>%
  hc_plotOptions(
    pie = list(
      colorByPoint = TRUE, center = c('70%', '10%'),
      size = 90, dataLabels = list(enabled = FALSE)
    ))
####### para el grafico del punto 3
hchart(Datos_Para_Grad3, "column", hcaes(x = categoria, y = Coincidencias, group = Tipo_Persona),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
         ))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="RESULTADO DE LA APLICACIÓN DE CONSULTA MASIVA DE LA CARTERA DE CLIENTES EN LISTAS SENSIBLES" ) %>%
  hc_colors(c("#FED194FF","#E75263FF",
              "#C53C74FF","#9E2F7FFF")) %>%
  hc_add_series(Datos_Para_Grad3.1, "pie", hcaes(name = Tipo_de_Alerta , y = Detalle), name = "Tipo de Alerta",
                stacking = list(enabled = TRUE),
                dataLabels = list(
                  enabled = TRUE, # Añadir etiquetas
                  format = "{point.percentage:.0f}%")) %>%
  hc_plotOptions(
    pie = list(
      colorByPoint = TRUE, center = c('70%', '10%'),
      size = 90, dataLabels = list(enabled = FALSE)
    ))

########### para el punto 4
hchart(Datos_Para_Grad4, "column", hcaes(x = Cliente  , y = Nro_de_personas, group = Descripcion),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE, # Añadir etiquetas
         format = "{point.percentage:.0f}%"))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="RESULTADO DE LA APLICACIÓN DE CONSULTA MASIVA DE LA CARTERA DE CLIENTES MIGRADOS DE FASSIL" ) %>%
  hc_colors(c("#F2645CFF","#E75263FF",
              "#C53C74FF","#9E2F7FFF")) %>%
  hc_add_series(Datos_Para_Grad4.1 , "pie", hcaes(name = Tipo_de_coincidencia , y = Nro_de_personas), name = "Tipo de Alerta",
                stacking = list(enabled = TRUE),
                dataLabels = list(
                  enabled = TRUE, # Añadir etiquetas
                  format = "{point.percentage:.0f}%")) %>%
  hc_plotOptions(
    pie = list(
      colorByPoint = TRUE, center = c('40%', '10%'),
      size = 90, dataLabels = list(enabled = FALSE)
    ))
############### para el punto 5
hchart(Datos_Para_Grad5, "column", hcaes(x = Mes_Tipo  , y = Total, group = Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE, # Añadir etiquetas
         format = "{point.percentage:.0f}%"))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="SISTEMA DE MONITOREO DE ALERTAS FINDER GHOST COMPLIANCE" ) %>%
  hc_colors(c("#F2645CFF","#E75263FF",
              "#C53C74FF","#9E2F7FFF")) %>%
  hc_add_series(Datos_Para_Grad5.1 , "pie", hcaes(name = Mes , y = Cantidad_de_Clientes), name = "Tipo de Alerta",
                stacking = list(enabled = TRUE),
                dataLabels = list(
                  enabled = TRUE, # Añadir etiquetas
                  format = "{point.percentage:.0f}%")) %>%
  hc_plotOptions(
    pie = list(
      colorByPoint = TRUE, center = c('70%', '10%'),
      size = 90, dataLabels = list(enabled = FALSE)
    ))

############### para el punto 6
Datos_Para_Grad6_1= Datos_Para_Grad6
Datos_Para_Grad6_1$Tipo[Datos_Para_Grad6_1$Tipo=="Saldo Menor a Bs 1000"]="Cantidad Menor a Bs 1000"
Datos_Para_Grad6_1$Tipo[Datos_Para_Grad6_1$Tipo=="Saldo Mayor a Bs 1000"]="Cantidad Mayor a Bs 1000"
Datos_Para_Grad6= Datos_Para_Grad6[,c(1,2,4)]
names(Datos_Para_Grad6)[3]="total"
Datos_Para_Grad6_1=Datos_Para_Grad6_1[,1:3]
names(Datos_Para_Grad6_1)=names(Datos_Para_Grad6)
Datos_Para_Grad6_2= rbind(Datos_Para_Grad6, Datos_Para_Grad6_1)
hchart(Datos_Para_Grad6_2, "column", hcaes(x = Regional  , y = total, group = Tipo),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE, # Añadir etiquetas
         format = "{point.percentage:.0f}%"))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="INFORME ESTADÍSTICO CORRESPONDIENTE AL CIERRE DE CUENTAS INSTRUIDAS POR CUMPLIMIENTO" ) %>%
  hc_colors(c("#F2645CFF","#E75263FF",
              "#C53C74FF","#9E2F7FFF"))

############### para el punto 7
hchart(Datos_Para_Grad7, "column", hcaes(x = Regional  , y = Cantidad, group = Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE, # Añadir etiquetas
         format = "{point.percentage:.0f}%"))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="Cantidad de ROS" ) %>%
  hc_colors(c("#F2645CFF","#E75263FF",
              "#C53C74FF","#9E2F7FFF"))

############### para el punto 7.1
hchart(Datos_Para_Grad7.1, "column", hcaes(x = Regional  , y = Cantidad, group = Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE, # Añadir etiquetas
         format = "{point.percentage:.0f}%"))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="Cantidad de Inusualidades" ) %>%
  hc_colors(c("#F2645CFF","#E75263FF",
              "#C53C74FF","#9E2F7FFF"))
############### para el punto 8
hchart(Datos_Para_Grad8, "column", hcaes(x = TIPO  , y = Cantidad, group = Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE, # Añadir etiquetas
         format = "{point.percentage:.0f}%"))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="Capacitación ASOBAN y ASOFIN" ) %>%
  hc_colors(c("#F2645CFF","#E75263FF",
              "#C53C74FF","#9E2F7FFF"))
######## para graf 1.1
hchart(Datos_Para_Grad1.1 %>% filter(Tipo=="Clientes"), "column", hcaes(x = Tipo_persona  , y = Cantidad, group = Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE, # Añadir etiquetas
         format = "{point.percentage:.0f}%"))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="PROCESO DE DEBIDA DILIGENCIA 2023" ) %>%
  hc_colors(c("#F2645CFF","#E75263FF",
              "#C53C74FF","#9E2F7FFF")) %>%
  hc_add_series(Datos_Para_Grad1.1 %>% filter(Tipo=="Usuarios") , "pie", hcaes(name = Nivel_de_exposicion_al_riesgo ,
                                                                               y = Cantidad), name = "Debida Diligencia",
                stacking = list(enabled = TRUE),
                dataLabels = list(
                  enabled = TRUE, # Añadir etiquetas
                  format = "{point.percentage:.0f}%")) %>%
  hc_plotOptions(
    pie = list(
      colorByPoint = TRUE, center = c('70%', '10%'),
      size = 90, dataLabels = list(enabled = FALSE)
    ))


###
# barras normales
hchart(Datos_Para_Grad3.1, "bar", hcaes(x = Tipo_de_Alerta, y = Detalle, group = Tipo),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         # , # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )
       )%>%  hc_colors(c("#FC8E64FF","#F2645CFF"))
################
######## para graf 1.1
hchart(Datos_Para_Grad1.1 %>% filter(Tipo=="Clientes"), "bar", hcaes(x = Nivel_de_exposicion_al_riesgo  , y = Cantidad, group = Tipo_persona),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>%
  hc_colors(c("#F2645CFF","#9E2F7FFF"))

####
categoria=NULL
for (i in 1:nrow(datos_fs)) {
  if(datos_fs$SALDO_BS[i]<100)
    categoria[i]=1
  else if(datos_fs$SALDO_BS[i]>=100 & datos_fs$SALDO_BS[i]<500)
    categoria[i]=2
  else if(datos_fs$SALDO_BS[i]>=500 & datos_fs$SALDO_BS[i]<1000)
    categoria[i]=3
  else if(datos_fs$SALDO_BS[i]>=1000 & datos_fs$SALDO_BS[i]<5000)
    categoria[i]=4
  else if(datos_fs$SALDO_BS[i]>=5000 & datos_fs$SALDO_BS[i]<10000)
    categoria[i]=5
  else if(datos_fs$SALDO_BS[i]>=10000)
    categoria[i]=6
}
datos_fs= data.frame(datos_fs, categoria)
resumen= datos_fs %>% group_by(TIPO_PERSONA ,categoria) %>% summarise(cantidad=n())
resumen$categoria[resumen$categoria==1]="Saldo de Bs 0 a 99"
resumen$categoria[resumen$categoria==2]="Saldo de Bs 100 a 499"
resumen$categoria[resumen$categoria==3]="Saldo de Bs 500 a 999"
resumen$categoria[resumen$categoria==4]="Saldo de Bs 1000 a 4999"
resumen$categoria[resumen$categoria==5]="Saldo de Bs 5000 a 9999"
resumen$categoria[resumen$categoria==6]="Saldo mayor a Bs 10.000"
#########
datos_res$Descripcion[datos_res$Descripcion=="Con saldo de Bs 0 a 99"]="Saldo de Bs 0 a 99"
datos_res$Descripcion[datos_res$Descripcion=="Con saldo de Bs 100 a 499"]="Saldo de Bs 100 a 499"
datos_res$Descripcion[datos_res$Descripcion=="Con saldo de Bs 500 a 999"]="Saldo de Bs 500 a 999"
datos_res$Descripcion[datos_res$Descripcion=="Con saldo de Bs 1000 a 4999"]="Saldo de Bs 1000 a 4999"
datos_res$Descripcion[datos_res$Descripcion=="Con saldo de Bs 5000 a 9999"]="Saldo de Bs 5000 a 9999"
datos_res$Descripcion[datos_res$Descripcion=="Con saldo mayor a Bs 10.000"]="Saldo mayor a Bs 10.000"
########### para el punto 4 Descripcion
hchart(datos_res, "bar", hcaes(x =  Descripcion , y = Nro_de_personas, group = Cliente),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>%
  hc_colors(c("#FCFBBDFF","#FECF92FF","#F2645CFF","#E75263FF",
              "#C53C74FF","#9E2F7FFF"))
##
hchart(datos_res %>% filter(Cliente=="Segmentacion"), "bar", hcaes(x = Descripcion  , y = Nro_de_personas, group = Cliente),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
         ))%>%
  hc_title(text="" ) %>%
  hc_colors(c("#E75263FF",
              "#C53C74FF","#9E2F7FFF"))



hchart(Datos_Para_Grad3.1, "bar", hcaes(x = Tipo_de_Alerta, y = Detalle, group = Tipo),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         # , # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )
)%>%  hc_colors(c("#FC8E64FF","#F2645CFF"))
#######################
hchart(Datos_Para_Grad4.1, "bar", hcaes(x = Tipo_de_coincidencia, y = Nro_de_personas)
       #,
       #stacking = list(enabled = TRUE),
       #dataLabels = list(
        # enabled = TRUE
         # , # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ) %>%  hc_colors(c("#FC8E64FF","#F2645CFF","#E75263FF",
                  "#C53C74FF","#9E2F7FFF"))

###########################
Datos_Para_Grad6_1= Datos_Para_Grad6
Datos_Para_Grad6_1$Tipo[Datos_Para_Grad6_1$Tipo=="Saldo Menor a Bs 1000"]="Cantidad Menor a Bs 1000"
Datos_Para_Grad6_1$Tipo[Datos_Para_Grad6_1$Tipo=="Saldo Mayor a Bs 1000"]="Cantidad Mayor a Bs 1000"
Datos_Para_Grad6= Datos_Para_Grad6[,c(1,2,4)]
names(Datos_Para_Grad6)[3]="total"
Datos_Para_Grad6_1=Datos_Para_Grad6_1[,1:3]
names(Datos_Para_Grad6_1)=names(Datos_Para_Grad6)
Datos_Para_Grad6_2= rbind(Datos_Para_Grad6, Datos_Para_Grad6_1)
hchart(Datos_Para_Grad6_2, "column", hcaes(x = Regional  , y = total, group = Tipo),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
         ))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="INFORME ESTADÍSTICO CORRESPONDIENTE AL CIERRE DE CUENTAS INSTRUIDAS POR CUMPLIMIENTO" ) %>%
  hc_colors(c("#FDE7A9FF","#E75263FF",
              "#C53C74FF","#9E2F7FFF"))
##
hchart(Datos_Para_Grad6_2[1:10,], "bar", hcaes(x = Regional  , y = total, group = Tipo),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="" ) %>%
  hc_colors(c("#E75263FF",
              "#C53C74FF","#9E2F7FFF"))
############ para el grafico de ghost compliance
puntoGhost_Comp= as.data.frame(puntoGhost_Comp)
puntoGhost_Comp= puntoGhost_Comp %>% filter(Cantidad!=0)
puntoGhost_Comp$Mes=as.Date(puntoGhost_Comp$Mes)
hchart(puntoGhost_Comp, "spline" ,hcaes(x=Mes, y=Cantidad,group= Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>%  hc_colors(c("#FDE7A9FF","#37B878FF",
                            "#E75263FF","#9E2F7FFF","#E75263FF"))
##########
datos= BASE_ROS %>% group_by(fecha_ident_inusual) %>% summarise(cantidad=n())
datos= datos %>% mutate(categoria= "cantidad identificación inusuales")
names(datos)[1]="fecha"
datos1= BASE_ROS %>% group_by(fecha_emis_informe) %>% summarise(cantidad=n())
datos1= datos1 %>% mutate(categoria= "cantidad emisión informes")
names(datos1)[1]="fecha"
#datos2= datos %>% left_join(datos1, by=c("fecha_ident_inusual"="fecha_emis_informe"))
datos2= rbind(datos, datos1)
datos2$fecha=as.Date(datos2$fecha)
hchart(datos2, "spline" ,hcaes(x=fecha, y=cantidad,group= categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE)) %>%
  hc_title(text=paste("Reportes"," ROS"))  %>% hc_add_theme(hc_theme_google()) %>% hc_rangeSelector(enabled= TRUE,
                                                                                                    verticalAlign = "top")
####################
casos_ros_1= casos_ros %>% group_by(Fecha_Identificacion_INUSUAL, Delito_de_LGI_yo_Delitos_Precedentes) %>% summarise(cantidad=n())
casos_ros_1$Fecha_Identificacion_INUSUAL=as.Date(casos_ros_1$Fecha_Identificacion_INUSUAL)
casos_ros_1= casos_ros_1 %>% filter(Fecha_Identificacion_INUSUAL>=as.Date("2023-3-1") & Fecha_Identificacion_INUSUAL<=as.Date("2023-6-1"))
casos_ros_1= casos_ros_1 %>% mutate(fecha_emis_informe= paste(substr(Fecha_Identificacion_INUSUAL,1,7),"1", sep = "-"))
casos_ros_2=casos_ros_1 %>% group_by(fecha_emis_informe,Delito_de_LGI_yo_Delitos_Precedentes) %>% summarise(total=n())
#casos_ros_1$cantidad[casos_ros_1$cantidad==1]=NA
hchart(casos_ros_2, "spline" ,hcaes(x=fecha_emis_informe, y=total,group= Delito_de_LGI_yo_Delitos_Precedentes),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE)) %>%
  hc_title(text=paste("Reportes"," ROS"))  %>% hc_add_theme(hc_theme_google()) %>% hc_rangeSelector(enabled= TRUE,
                                                                                                    verticalAlign = "top")
casos_ros_1= casos_ros %>% group_by(Fecha_Identificacion_INUSUAL) %>% summarise(cantidad=n())




hchart(Datos_Para_Grad7 %>% filter(Cantidad!=0), "bar", hcaes(x = Regional  , y = Cantidad, group = Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="" ) %>%
  hc_colors(c("#E75263FF",
              "#C53C74FF","#9E2F7FFF"))


hchart(Datos_Para_Grad7.1 %>% filter(Cantidad!=0), "bar", hcaes(x = Regional  , y = Cantidad, group = Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="" ) %>%
  hc_colors(c("#E75263FF",
              "#C53C74FF","#9E2F7FFF"))







SOLICITUDES_CIERRES_CAJA_DE_AHORRO= SOLICITUDES_CIERRES_CAJA_DE_AHORRO %>% mutate(fecha= paste(substr(Fecha_de_Registro,1,7),"1", sep = "-"))




hchart(cierres, "spline" ,hcaes(x=fecha, y=Cantidad,group= NOMBRE_REGIONAL),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>%  hc_colors(c("#FDE7A9FF","#37B878FF",
                            "#E75263FF","#9E2F7FFF","#E75263FF"))
##########
cierres$Monto_de_cierre_Bs=round(cierres$Monto_de_cierre_Bs,0)
hchart(cierres, "spline" ,hcaes(x=fecha, y=Monto_de_cierre_Bs,group= NOMBRE_REGIONAL),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>%  hc_colors(c("#FDE7A9FF","#37B878FF",
                            "#E75263FF","#9E2F7FFF","#E75263FF"))

#########
ult=as.data.frame(ult)
ult$Mes=as.Date(ult$Mes)
hchart(ult, "bar", hcaes(x = Mes  , y = Total, group = Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="" ) %>%
  hc_colors(c("#FDE7A9FF","#FF0000",
              "#E75263FF","#9E2F7FFF","#E75263FF","#FD9668FF"))


#################
datos_res_1= datos_res %>% filter(Cliente=="Segmentacion")

hchart(datos_res_2, "bar", hcaes(x = Descripcion  , y = Nro_de_personas, group = Cliente),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))%>%
  hc_title(text="" ) %>%
  hc_colors(c("#FD9668FF","#FF0000"))

# barr




## 9-10


hchart(cierres, "spline" ,hcaes(x=fecha, y=Cantidad,group= NOMBRE_REGIONAL),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>%  hc_colors(c("#FDE7A9FF","#FF0000",
                            "#37B878FF","#9E2F7FFF","#3A5C9BFF"))
##########
cierres1$Monto_de_cierre_Bs=round(cierres1$Monto_de_cierre_Bs,0)
cierres= cierres %>% mutate(fecha1= paste(substr(fecha,1,7),"1", sep = "-"))

hchart(cierres1, "bar" ,hcaes(x=fecha1, y=total,group= NOMBRE_REGIONAL),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>%  hc_colors(c("#FDE7A9FF","#37B878FF",
                            "#FF0000","#9E2F7FFF","#3A5C9BFF"))





hchart(puntoGhost_Comp, "spline" ,hcaes(x=Mes, y=Cantidad,group= Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>%  hc_colors(c("#37B878FF",
                            "#E75263FF","#9E2F7FFF","#E75263FF"))



hchart(ult1, "bar", hcaes(x = Mes  , y = total),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="" ) %>%
  hc_colors(c(
              "#E75263FF","#9E2F7FFF","#E75263FF","#FD9668FF"))







hchart(Datos_Para_Grad , "column", hcaes(x = DD_nivel_de_riesgo, y = Cantidad_de_Personas, group = Categoria),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_gridlight())%>%
  hc_title(text="RESULTADO APLICACIÓN DEL PROCESO DE DEBIDA DILIGENCIA 2023" ) %>%
  hc_colors(c("#FC9065FF","#F9795DFF","#F2645CFF","#E75263FF",
              "#D6456CFF","#C53C74FF","#B2357BFF","#9E2F7FFF"))









c("#0B0405FF", "#28192FFF", "#3B2F5EFF", "#40498EFF", "#366A9FFF", "#348AA6FF", "#38AAACFF", "#54C9ADFF", "#A0DFB9FF", "#DEF5E5FF")


c("#0B0405FF", "#1D111DFF", "#2D1D38FF", "#382A54FF", "#403872FF", "#40498EFF", "#395D9CFF",
  "#3671A0FF", "#3484A5FF", "#3497A9FF", "#38AAACFF", "#45BDADFF", "#60CEACFF", "#91DBB4FF", "#BBE7C8FF", "#DEF5E5FF")

#A
c("#000004FF", "#0B0924FF", "#20114BFF", "#3B0F70FF", "#57157EFF", "#721F81FF", "#8C2981FF", "#A8327DFF", "#C43C75FF",
  "#DE4968FF", "#F1605DFF", "#FA7F5EFF", "#FE9F6DFF", "#FEBF84FF", "#FDDEA0FF", "#FCFDBFFF")

#B
c("#000004FF", "#0C0826FF", "#240C4FFF", "#420A68FF", "#5D126EFF", "#781C6DFF", "#932667FF","#AE305CFF",
  "#C73E4CFF", "#DD513AFF", "#ED6925FF", "#F8850FFF", "#FCA50AFF", "#FAC62DFF", "#F2E661FF", "#FCFFA4FF")
#C
c("#0D0887FF", "#330597FF", "#5002A2FF", "#6A00A8FF", "#8405A7FF", "#9C179EFF", "#B12A90FF", "#C33D80FF",
  "#D35171FF", "#E16462FF", "#ED7953FF", "#F68F44FF", "#FCA636FF", "#FEC029FF", "#F9DC24FF", "#F0F921FF")

#D
c("#440154FF", "#481A6CFF", "#472F7DFF", "#414487FF", "#39568CFF", "#31688EFF", "#2A788EFF", "#23888EFF",
  "#1F988BFF", "#22A884FF", "#35B779FF", "#54C568FF", "#7AD151FF", "#A5DB36FF", "#D2E21BFF", "#FDE725FF")
#E
c("#00204DFF", "#002C6AFF", "#0F386EFF", "#31446BFF", "#46506BFF", "#575C6DFF", "#666970FF", "#757575FF",
  "#848279FF", "#958F78FF", "#A69D75FF", "#B8AB70FF", "#CBBA69FF", "#DDC95FFF", "#F1D951FF", "#FFEA46FF")

#F
c("#03051AFF", "#1A102AFF", "#33183CFF", "#4C1D4BFF", "#681F55FF", "#841E5AFF", "#A11A5BFF", "#BD1655FF",
  "#D62449FF", "#E83F3FFF", "#F06043FF", "#F47F58FF", "#F69C73FF", "#F6B893FF", "#F8D1B8FF", "#FAEBDDFF")

#G
c("#0B0405FF", "#1D111DFF", "#2D1D38FF", "#382A54FF", "#403872FF", "#40498EFF", "#395D9CFF", "#3671A0FF",
  "#3484A5FF", "#3497A9FF", "#38AAACFF", "#45BDADFF", "#60CEACFF", "#91DBB4FF", "#BBE7C8FF", "#DEF5E5FF")

#H
c("#30123BFF", "#4143A7FF", "#4771E9FF", "#3E9BFEFF", "#22C5E2FF", "#1AE4B6FF", "#46F884FF", "#88FF4EFF",
  "#B9F635FF", "#E1DD37FF", "#FABA39FF", "#FD8D27FF", "#F05B12FF", "#D63506FF", "#AF1801FF", "#7A0403FF")
#
################## PARA ORDENAR COLORES EN UN VECTOR FILA ##########
colores=matrix(viridis(100, option = "A"), ncol = 12, byrow = T)
colores= as.data.frame(colores)
##########################################################################


library("forecast")
airforecast <- forecast(auto.arima(AirPassengers), level = 99)
hchart(airforecast) %>% hc_add_theme(hc_theme_sandsignika())

datos3= datos2 %>% filter(categoria=="Operaciones sospechosas")
datos4= ts(datos3$cantidad,start = c(2017,2), frequency = 12)
airforecast <- forecast(auto.arima(datos4), level = 95)
hchart(airforecast) %>% hc_add_theme(hc_theme_sandsignika())
################################################################################
codeudores=NULL
for (i in 1:nrow(Rev_30_6_23)) {
  if(Rev_30_6_23$NDOC_FS[i] %in% datos$NDOC_FS )
    codeudores[i]=1
  else codeudores[i]=2
}
table(codeudores)
Rev_30_6_23= data.frame(Rev_30_6_23, codeudores)


################### 3 de julio #################
Base_resumen_extract$ID_Cliente= as.numeric(Base_resumen_extract$ID_Cliente)
datos= Reporte_2023_05 %>% filter(ID_Cliente %in% Base_resumen_extract$ID_Cliente )

datos= Reporte_2023_05 %>% filter(ID_Cliente %in% Base_resumen_extract$ID_Cliente )
Base_resumen_extract= Base_resumen_extract %>% mutate(corre= 1:nrow(Base_resumen_extract))
Base_resumen_extract_1= Base_resumen_extract %>% left_join(datos[,c(9:11,13:14)], by="ID_Cliente")
###
Base_resumen_extract_1= Base_resumen_extract_1 %>% left_join(Base_clientes_BSol[,c(1,6,8,16,17,18,20)], by=c("ID_Cliente"="USERID"))
datos= Base_resumen_extract_1 %>% group_by(ID_Cliente, TIPO_PERSONA) %>% summarise(total_depo=sum(tot_dep),
                                                                                   total_ret=sum(to_ret),
                                                                                   cant_dep_tot=sum(cant_dep),
                                                                                   cant_ret_tot=sum(cant_ret))
###
persona1= Reporte_2023_05 %>% filter(ID_Cliente==3138069) %>% filter(Descripcion %in% categorias$variable )
persona= Base_resumen_extract_1 %>% filter(CI_TITULAR=="7166766")
#esto sirve para la union con la cantidad de transacciones
persona2= datos %>% filter(ID_Cliente==3138069)
#
persona1= Base_resumen_extract_1 %>% filter(ID_Cliente==2808036)
persona1= Reporte_2023_05 %>% filter(ID_Cliente==2808036) %>% filter(Descripcion %in% categorias$variable )
table(persona1$Importe_Depositos)
reporte_cant= as.data.frame(table(persona1$Importe_Retiros))
names(reporte_cant)=c("Valor","Cantidad")
reporte_cant= reporte_cant %>% filter(Valor!=0)
reporte_cant_1= reporte_cant %>% arrange(desc(Cantidad)) %>% filter(row_number()==1)
reporte_cant_1= data.frame(reporte_cant_1, ID_Cliente=persona1$ID_Cliente[1])
### Vec
persona1= Reporte_2023_05 %>% filter(CI_TITULAR %in% datos$CI_TITULAR ) %>% filter(Descripcion %in% categorias$variable )
reporte_cant= as.data.frame(table(persona1$Importe_Depositos))
names(reporte_cant)=c("Valor","Cantidad")
reporte_cant= reporte_cant %>% filter(Valor!=0)
reporte_cant_1= reporte_cant %>% arrange(desc(Cantidad)) %>% filter(row_number()==1)
reporte_cant_1= data.frame(reporte_cant_1, CI_TITULAR=persona1$ID_ClieCI_TITULARnte[1])
unido=data.frame()
for (i in 1:length(unique(persona1$CI_TITULAR))) {
  print(i)
  persona2= persona1 %>% filter(CI_TITULAR== unique(persona1$CI_TITULAR)[i])
  reporte_cant= as.data.frame(table(persona2$Importe_Depositos))
  names(reporte_cant)=c("Valor","Cantidad")
  if( dim(reporte_cant)[1]==1)
  {
    unido= rbind(unido, reporte_cant_1)
  }
  else{
    reporte_cant= reporte_cant %>% filter(Valor!=0)
    reporte_cant_1= reporte_cant %>% arrange(desc(Cantidad)) %>% filter(row_number()==1)
    reporte_cant_1= data.frame(reporte_cant_1, CI_TITULAR=persona2$CI_TITULAR[1])
    unido= rbind(unido, reporte_cant_1)
  }

}


#####################################################
mapdata <- get_data_from_map(download_map_data("countries/bo/bo-all"))
glimpse(mapdata)

set.seed(1234)

data_fake <- mapdata %>%
  select(code = `hc-a2`) %>%
  mutate(value = 1e5 * abs(rt(nrow(.), df = 10)))

glimpse(data_fake)
############### PARA REALIZAR MAPA CON CANTIDADES DE ROS POR DEPARTAMENTO ############
casos_reporte= BASE_INUSUALIDADES %>% filter(año==2023 & ROS=="SI")
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="EL ALTO"]="LP"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="LA PAZ"]="LP"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="SANTA CRUZ"]="SC"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="POTOSI"]="PO"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="COCHABAMBA"]="CB"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="SUCRE"]="CQ"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="BENI"]="EB"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="ORURO"]="OR"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="PANDO"]="PA"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="TARIJA"]="TR"
casos_reporte= casos_reporte %>% count(REGIONAL)
###
hcmap("countries/bo/bo-all", data = casos_reporte, value = "n",
      joinBy =c("hc-a2","REGIONAL") , name = "ROS",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#0EAD82", borderWidth = 0.7,
      tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = " Reportes"))
###
casos_reporte= BASE_INUSUALIDADES %>% filter(año==2023 & ROS=="SI")
casos_reporte_1= casos_reporte %>% filter(REGIONAL=="SANTA CRUZ")
casos_reporte_1= casos_reporte_1 %>% count(MEDIO_DE_IDENTIFICACION_DEL_CLIENTE)
hchart(casos_reporte_1, "treemap", hcaes(x=MEDIO_DE_IDENTIFICACION_DEL_CLIENTE, value=n, color=n),
       borderColor = "#0EAD82", borderWidth = 2)

####################################
nombres_alertas=c("Nro_ID_Cliente","Nro_de_identificacion","Nombre_completo","Flujo_de_alerta","Tipo_de_alerta",
  "Detalle","Fecha_de_generacion"
  ,"Categoria_anterior","Categoria_actual","Fecha_de_cambio_categoria","Usuario_en_proceso",
  "Usuario_de_finalización","Perfil_alerta","Inicio_periodo"
  ,"Fin_periodo","Valor_esperado","Valor_operado","Excedido","Tipo_alerta")
length(nombres_alertas)
names(MS103084)= nombres_alertas
Base_Alertas= MS103084 %>% filter(!(Detalle=="Alerta generada por JOB - Debida Diligencia pasa a pendiente."))
Base_Alertas= Base_Alertas %>% mutate(Inicio_periodo= substr(Inicio_periodo,1,10))
Base_Alertas= Base_Alertas %>% mutate(Fin_periodo= substr(Fin_periodo,1,10))
Base_Alertas= Base_Alertas %>% mutate(Fecha_de_cambio_categoria= substr(Fecha_de_cambio_categoria,1,10))
Base_Alertas= Base_Alertas %>% mutate(Fecha_de_generacion= substr(Fecha_de_generacion,1,10))
Base_Alertas= as.data.frame(Base_Alertas)
Base_Alertas= Base_Alertas_2
Base_Alertas$Inicio_periodo=as.Date(Base_Alertas$Inicio_periodo)
Base_Alertas$Fin_periodo=as.Date(Base_Alertas$Fin_periodo)
Base_Alertas$Fecha_de_cambio_categoria=as.Date(Base_Alertas$Fecha_de_cambio_categoria)
Base_Alertas$Fecha_de_generacion=as.Date(Base_Alertas$Fecha_de_generacion)
Base_Alertas= Base_Alertas %>% mutate(cantidad=1)
Base_Alertas_1= Base_Alertas %>% group_by(Fecha_de_cambio_categoria, Categoria_actual) %>% summarise(total= sum(cantidad))
Base_Alertas_1= Base_Alertas_1 %>% filter(!(Categoria_actual=="Finalizada"))
Base_Alertas_1= Base_Alertas_1 %>% mutate(gestion= substr(Fecha_de_cambio_categoria,1,4))
datos=Base_Alertas_1 %>% group_by(gestion, Categoria_actual) %>%
  summarise(total=sum(total)) %>% arrange(gestion)
datos$gestion=as.numeric(datos$gestion)
datos %>% group_by(gestion) %>% summarise(total=sum(total))
datos$gestion[datos$gestion==2021]= "2021: 12541"
datos$gestion[datos$gestion==2022]= "2022: 32559"
datos$gestion[datos$gestion==2023]= "2023: 34075"
datos[3,2]="1. Finalizada"
datos[2,2]="1. Finalizada"
datos[7,2]="1. Finalizada"
datos[1,2]="1. Finalizada"
datos= datos %>% group_by(gestion, Categoria_actual) %>% summarise(total= sum(total))
#############
#Base_Alertas= Base_Alertas %>% mutate(Regional= Flujo_de_alerta)
hchart(Base_Alertas_1, "spline" ,hcaes(x=Fecha_de_cambio_categoria, y= total, group= Categoria_actual),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))
#########
hchart(datos, "column", hcaes(x = gestion  , y = total, group=Categoria_actual),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>%
  hc_title(text="" ) %>%
  hc_colors(c(
    "#3484A5FF","#0EAD82","#9E2F7FFF","#E75263FF","#3484A5FF"))
############## grafico de cuadros
casos_reporte_1= datos %>% count(PROFESION_TITULAR)
casos_reporte_1= casos_reporte_1 %>% arrange(desc(n))
casos_reporte_1= casos_reporte_1[1:5,]
hchart(casos_reporte_1 , "treemap",
       hcaes(x=PROFESION_TITULAR , value=n, color=n),
       borderColor = "#0EAD82", borderWidth = 2)
#################
Ros=NULL
for (i in 1:nrow(Base_Alertas)) {
  print(i)
  if(Base_Alertas$Nro_de_identificacion[i] %in% base_ci_ros$CI )
    Ros[i]=1
  else Ros[i]=2
}
table(Ros)
Base_Alertas= data.frame(Base_Alertas, Ros)
###
Inusualidad=NULL
for (i in 1:nrow(Base_Alertas)) {
  print(i)
  if(Base_Alertas$Nro_de_identificacion[i] %in% base_ci_ros_inusuales$Documento )
    Inusualidad[i]=1
  else Inusualidad[i]=2
}
table(Inusualidad)
Base_Alertas= data.frame(Base_Alertas, Inusualidad)
################################################################################

class(datos)
datos$mes=as.numeric(datos$mes)
datos$gestion=as.numeric(datos$gestion)

hchart(datos1, "spline" ,hcaes(x=fecha, y=n,group= gestion),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>%  hc_colors(c("#3A5C9BFF", "#37B878FF","#FF0000",
                            "#37B878FF","#9E2F7FFF","#3A5C9BFF"))


hchart(datos2, "spline" ,hcaes(x=fecha, y=n,group= gestion),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_google()) %>%  hc_colors(c("#3A5C9BFF", "#37B878FF","#FF0000",
                            "#37B878FF","#9E2F7FFF","#3A5C9BFF"))
################################
Base_para_alertas_cp= datos1 %>% filter(TIPO_PERSONA=="NATURAL")
persona= Base_Alertas_2 %>% filter(Nro_de_identificacion==Base_para_alertas_cp$Nro_de_identificacion[4] &
                                     Caracteristica_2==Base_para_alertas_cp$Caracteristica_2[4],
                                   Caracteristica_1== Base_para_alertas_cp$Caracteristica_1[4])
control1=qcc(persona$Valor_operado , type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
control1=qcc(persona$Valor_operado , type = "xbar.one", std.dev = "MR", nsigmas = 1)

####

####
LCL=NULL
LC=NULL
UCL=NULL
#
for (i in 1:nrow(Base_para_alertas_cp)) {
  print(i)
  persona= Base_Alertas_2 %>% filter(Nro_de_identificacion==Base_para_alertas_cp$Nro_de_identificacion[i] &
                                       Caracteristica_2==Base_para_alertas_cp$Caracteristica_2[i],
                                     Caracteristica_1== Base_para_alertas_cp$Caracteristica_1[i])
  control1=qcc(persona$Valor_operado , type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
  LCL[i]=round(control1$limits[1],0)
  LC[i]=round(control1$center,0)
  UCL[i]=round(control1$limits[2],0)
}
Base_para_alertas_cp= data.frame(Base_para_alertas_cp, LCL_1=LCL, LC_1=LC, UCL_1=UCL)
####################################
datos_para_graf= Base_Alertas_2 %>% filter(Nro_de_identificacion==Base_para_alertas_cp$Nro_de_identificacion[4] &
                                             Caracteristica_2==Base_para_alertas_cp$Caracteristica_2[4] &
                                             Caracteristica_1== Base_para_alertas_cp$Caracteristica_1[4]) %>%
                                          select(Fecha_de_generacion, Valor_operado,
                                            Caracteristica_2, Caracteristica_1)
datos_para_graf$Fecha_de_generacion= as.Date(datos_para_graf$Fecha_de_generacion)
datos_para_graf_1= datos_para_graf %>% select(Fecha_de_generacion)
datos_para_graf_1= datos_para_graf_1 %>% mutate(Valor_operado=Base_para_alertas_cp$LC[4])
datos_para_graf_1= datos_para_graf_1 %>% mutate(Caracteristica_2= "Valor Central")
datos_para_graf_1$Fecha_de_generacion=datos_para_graf_1$Fecha_de_generacion+1
datos_para_graf_2= datos_para_graf %>% select(Fecha_de_generacion)
datos_para_graf_2= datos_para_graf_2 %>% mutate(Valor_operado=Base_para_alertas_cp$UCL[4])
datos_para_graf_2= datos_para_graf_2 %>% mutate(Caracteristica_2= "Lim. superior")
datos_para_graf_2$Fecha_de_generacion=datos_para_graf_2$Fecha_de_generacion+2
datos_para_graf= datos_para_graf %>% arrange(Fecha_de_generacion)
datos_para_graf_1= datos_para_graf_1 %>% arrange(Fecha_de_generacion)
datos_para_graf_2= datos_para_graf_2 %>% arrange(Fecha_de_generacion)
datos_para_graf_3= rbind(datos_para_graf[,1:3], datos_para_graf_1, datos_para_graf_2)
datos_para_graf_3$Fecha_de_generacion= as.Date(datos_para_graf_3$Fecha_de_generacion)
#
hchart(datos_para_graf_3, "spline" ,hcaes(x=Fecha_de_generacion,
                              y=Valor_operado,group= Caracteristica_2)) %>%
  hc_title(text=paste("Lim. de control: ",unique(datos_para_graf$Caracteristica_1)))  %>%
  hc_add_theme(hc_theme_google()) %>% hc_rangeSelector(enabled= TRUE,verticalAlign = "top")
###
for (i in 1:nrow(prueba_c)) {
  control1=NULL
  control2=NULL
  control3=NULL
  control4=NULL

  persona= Base_resumen_pj_extract_m23_extns %>% filter(ID_Cliente==prueba_c$ID_Cliente[i])
  persona= persona %>% mutate(date= paste(año,mes,"1", sep = "-"))
  persona$date= as.Date(persona$date)
  persona1= persona %>% filter(tipo_op=="ACH") %>% group_by(date) %>% summarise(tot_dep=sum(tot_dep),to_ret= sum(to_ret),
                                                                               cant_dep= sum(cant_dep),
                                                                               cant_ret= sum(cant_ret))
  if(dim(persona)[1]==1)
  {
    #
    LCL_tot_dep[i] =persona$tot_dep
    LC_tot_dep[i]=persona$tot_dep
    UCL_tot_dep[i]=persona$tot_dep
    #
    LCL_to_ret[i]=persona$to_ret
    LC_to_ret[i]=persona$to_ret
    UCL_to_ret[i]=persona$to_ret
    #
    LCL_cant_dep[i]=persona$cant_dep
    LC_cant_dep[i]=persona$cant_dep
    UCL_cant_dep[i]=persona$cant_dep
    #
    LCL_cant_ret[i]=persona$cant_ret
    LC_cant_ret[i]=persona$cant_ret
    UCL_cant_ret[i]=persona$cant_ret

  }
  else {
    #######
    pers1= persona %>% filter(tot_dep!=0)
    if(dim(pers1)[1]==0)
    {
      LCL_tot_dep[i] =0
      LC_tot_dep[i]=0
      UCL_tot_dep[i]=0
    }
    else if(dim(pers1)[1]==1)
    {
      LCL_tot_dep[i] =persona$tot_dep
      LC_tot_dep[i]=persona$tot_dep
      UCL_tot_dep[i]=persona$tot_dep
    }
    else {
      control1=qcc(pers1$tot_dep, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_tot_dep[i] =control1$limits[1]
      LC_tot_dep[i]=control1$center
      UCL_tot_dep[i]=control1$limits[2]
    }
    #
    pers2= persona %>% filter(to_ret!=0)
    if(dim(pers2)[1]==0)
    {
      LCL_to_ret[i] =0
      LC_to_ret[i]=0
      UCL_to_ret[i]=0
    }
    else if(dim(pers2)[1]==1)
    {
      LCL_to_ret[i] =persona$to_ret
      LC_to_ret[i]=persona$to_ret
      UCL_to_ret[i]=persona$to_ret
    }
    else {
      control2=qcc(pers2$to_ret, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_to_ret[i]=control2$limits[1]
      LC_to_ret[i]=control2$center
      UCL_to_ret[i]=control2$limits[2]
    }
    #
    pers3= persona %>% filter(cant_dep!=0)
    if(dim(pers3)[1]==0)
    {
      LCL_cant_dep[i] =0
      LC_cant_dep[i]=0
      UCL_cant_dep[i]=0
    }
    else if(dim(pers3)[1]==1)
    {
      LCL_cant_dep[i] =persona$cant_dep
      LC_cant_dep[i]=persona$cant_dep
      UCL_cant_dep[i]=persona$cant_dep
    }
    else {
      control3=qcc(pers3$cant_dep, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_cant_dep[i]=control3$limits[1]
      LC_cant_dep[i]=control3$center
      UCL_cant_dep[i]=control3$limits[2]
    }
    #
    pers4= persona %>% filter(cant_ret!=0)
    if(dim(pers4)[1]==0)
    {
      LCL_cant_dep[i] =0
      LC_cant_dep[i]=0
      UCL_cant_dep[i]=0
    }
    else if(dim(pers4)[1]==1)
    {
      LCL_cant_dep[i] =persona$cant_ret
      LC_cant_dep[i]=persona$cant_ret
      UCL_cant_dep[i]=persona$cant_ret
    }
    else {
      control4=qcc(pers4$cant_ret, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_cant_ret[i]=control4$limits[1]
      LC_cant_ret[i]=control4$center
      UCL_cant_ret[i]=control4$limits[2]
    }

  }
}
base_limites_ACH= data.frame(prueba_c, LCL_tot_dep_ACH=LCL_tot_dep, LC_tot_dep_ACH=LC_tot_dep, UCL_tot_dep_ACH=UCL_tot_dep,
                             LCL_to_ret_ACH=LCL_to_ret,LC_to_ret_ACH=LC_to_ret,UCL_to_ret_ACH=UCL_to_ret,
                             LCL_cant_dep_ACH=LCL_cant_dep,LC_cant_dep_ACH=LC_cant_dep,
                             UCL_cant_dep_ACH=UCL_cant_dep,LCL_cant_ret_ACH=LCL_cant_ret,
                             LC_cant_ret_ACH=LC_cant_ret,UCL_cant_ret_ACH=UCL_cant_ret)
base_limites_ACH$LCL_cant_dep_ACH= trunc(base_limites_ACH$LCL_cant_dep_ACH+1)
base_limites_ACH$LC_cant_dep_ACH= trunc(base_limites_ACH$LC_cant_dep_ACH+1)
base_limites_ACH$UCL_cant_dep_ACH= trunc(base_limites_ACH$UCL_cant_dep_ACH+1)
base_limites_ACH$LCL_cant_ret_ACH= trunc(base_limites_ACH$LCL_cant_ret_ACH+1)
base_limites_ACH$LC_cant_ret_ACH= trunc(base_limites_ACH$LC_cant_ret_ACH+1)
base_limites_ACH$UCL_cant_ret_ACH= trunc(base_limites_ACH$UCL_cant_ret_ACH+1)
base_limites_ACH$LCL_cant_dep_ACH[is.na(base_limites_ACH$LCL_cant_dep_ACH)]=0
base_limites_ACH$LC_cant_dep_ACH[is.na(base_limites_ACH$LC_cant_dep_ACH)]=0
base_limites_ACH$UCL_cant_dep_ACH[is.na(base_limites_ACH$UCL_cant_dep_ACH)]=0
base_limites_ACH$LCL_cant_ret_ACH[is.na(base_limites_ACH$LCL_cant_ret_ACH)]=0
base_limites_ACH$LC_cant_ret_ACH[is.na(base_limites_ACH$LC_cant_ret_ACH)]=0
base_limites_ACH$UCL_cant_ret_ACH[is.na(base_limites_ACH$UCL_cant_ret_ACH)]=0
######################## ENLACE FINDER PRODUCCION ##############
# https://vfindersrv/Account/Login?ReturnUrl=%2F
######################## ENLACE MESAS DE SERVICIO #################
#https://vma2srv/
############################ PARA SOLUCIONAR Error in file(out, "wt") : cannot open the connection ########################
#tempdir()
# [1] "C:\Users\XYZ~1\AppData\Local\Temp\Rtmp86bEoJ\Rtxt32dcef24de2"
#dir.create(tempdir())
################################################################################
datos= datos_1 %>% filter(n>5 & n < 10)
datos= datos %>% filter(TIPO_PERSONA=="NATURAL")
#
persona= Base_Alertas_2 %>% filter(Nro_de_identificacion=="9213402LP")
############## Alertas efectividad ############
class(Base_Alertas_1)
Base_Alertas_1$Fecha_de_generacion= as.Date(Base_Alertas_1$Fecha_de_generacion)
Base_Alertas_1$Fecha_de_cambio_categoria= as.Date(Base_Alertas_1$Fecha_de_cambio_categoria)
Base_Alertas_1$Inicio_periodo= as.Date(Base_Alertas_1$Inicio_periodo)
Base_Alertas_1$Fin_periodo= as.Date(Base_Alertas_1$Fin_periodo)
##
Base_Alertas_1= Base_Alertas_1 %>% mutate(dias_transcurridos= Fecha_de_cambio_categoria- Fecha_de_generacion)
Base_Alertas_1$dias_transcurridos= as.numeric(Base_Alertas_1$dias_transcurridos)
#
Categoria=NULL
for (i in 1:nrow(Base_Alertas_1)) {
  if(Base_Alertas_1$dias_transcurridos[i]>=0 & Base_Alertas_1$dias_transcurridos[i]<=7)
    Categoria[i]="Muy efectivo"
  else if(Base_Alertas_1$dias_transcurridos[i]>7 & Base_Alertas_1$dias_transcurridos[i]<=14)
    Categoria[i]="Efectivo"
  else if(Base_Alertas_1$dias_transcurridos[i]>14 & Base_Alertas_1$dias_transcurridos[i]<=21)
    Categoria[i]="Regular"
  else if(Base_Alertas_1$dias_transcurridos[i]>21 & Base_Alertas_1$dias_transcurridos[i]<=30)
    Categoria[i]="Negativo"
  else if(Base_Alertas_1$dias_transcurridos[i]>30)
    Categoria[i]="Muy negativo"
}
Base_Alertas_1= data.frame(Base_Alertas_1, Categoria)
#
Base_Alertas_11= Base_Alertas_1 %>% filter(Categoria_actual!="NULL")
Base_Alertas_11= Base_Alertas_11 %>% filter(Usuario_de_finalizacion!="NULL")
# desde aqui para cambiar la gestion
Base_Alertas_11= Base_Alertas_11 %>% filter(Fecha_de_generacion>=as.Date("2023-1-1") & Fecha_de_generacion<=as.Date("2023-12-31"))
ejemplo=data.frame(table(Base_Alertas_11$Usuario_de_finalizacion, Base_Alertas_11$Categoria_anterior))
ejemplo= ejemplo %>% filter(Freq!=0)
ejemplo1=spread(ejemplo, Var2, Freq)
ejemplo1$`1ra notificación`[is.na(ejemplo1$`1ra notificación`)]=0
ejemplo1$`2da notificación`[is.na(ejemplo1$`2da notificación`)]=0
ejemplo1$`3ra notificación`[is.na(ejemplo1$`3ra notificación`)]=0
ejemplo1= ejemplo1 %>% mutate(total= sum(`1ra notificación`+`2da notificación`+`3ra notificación`))
ejemplo1= ejemplo1 %>% mutate(total= `1ra notificación`+`2da notificación`+`3ra notificación`)
ejemplo1= ejemplo1 %>% mutate(finalizada_1ra_alerta= round(`1ra notificación`/ total,3))
ejemplo1= ejemplo1 %>% mutate(finalizada_2da_alerta= round(`2da notificación`/ total,3))
ejemplo1= ejemplo1 %>% mutate(finalizada_3ra_alerta= round(`3ra notificación`/ total,3))
ejemplo1= ejemplo1 %>% arrange(desc(total))
#
unido=data.frame()
for (i in 1:nrow(ejemplo1)) {
  info_1=NULL
  info_2=NULL
  info_3=NULL
  info_4=NULL
  info_5=NULL
  info_6=NULL
  info_resumen= Base_Alertas_11 %>% filter(Usuario_de_finalizacion==ejemplo1$Var1[i])
  info_1= as.numeric(summary(info_resumen$dias_transcurridos)[1])
  info_2= as.numeric(summary(info_resumen$dias_transcurridos)[2])
  info_3= as.numeric(summary(info_resumen$dias_transcurridos)[3])
  info_4= as.numeric(summary(info_resumen$dias_transcurridos)[4])
  info_5= as.numeric(summary(info_resumen$dias_transcurridos)[5])
  info_6= as.numeric(summary(info_resumen$dias_transcurridos)[6])
  unido1=data.frame(nombre=ejemplo1$Var1[i], minimo=info_1, Primer_Q= info_2, Mediana=info_3,
                   Media= info_4, Tercer_Q= info_5, Maximo= info_6)
  unido= rbind(unido, unido1)
}
Total_resumen= cbind(ejemplo1, unido[,2:7])
#

#
datos_alertas= Base_Alertas_1 %>% group_by(Usuario_de_finalizacion, Categoria_actual) %>% summarise(cantidad= sum(cantidad))
datos_alertas= datos_alertas %>% filter(Usuario_de_finalizacion!="NULL")
hchart(ejemplo, "column", hcaes(x =  Var1 , y = Freq, group = Var2),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>%
  hc_colors(c("#FCFBBDFF","#FECF92FF","#F2645CFF","#E75263FF",
              "#C53C74FF","#9E2F7FFF"))
#########################################
Base_Alertas_11= Base_Alertas_1 %>% filter(Categoria_actual!="NULL")
Base_Alertas_11= Base_Alertas_11 %>% filter(Usuario_de_finalizacion!="NULL")
Base_Alertas_11= Base_Alertas_11 %>% filter(Fecha_de_generacion>=as.Date("2023-1-1") & Fecha_de_generacion<=as.Date("2023-12-31"))
ejemplo=data.frame(table(Base_Alertas_11$Usuario_de_finalizacion, Base_Alertas_11$Categoria_anterior, Base_Alertas_11$TIPO_PERSONA))
ejemplo= ejemplo %>% filter(Freq!=0)
ejemplo1=spread(ejemplo, Var2, Freq)
ejemplo1$`1ra notificación`[is.na(ejemplo1$`1ra notificación`)]=0
ejemplo1$`2da notificación`[is.na(ejemplo1$`2da notificación`)]=0
ejemplo1$`3ra notificación`[is.na(ejemplo1$`3ra notificación`)]=0
ejemplo1= ejemplo1 %>% mutate(total= sum(`1ra notificación`+`2da notificación`+`3ra notificación`))
ejemplo1= ejemplo1 %>% mutate(total= `1ra notificación`+`2da notificación`+`3ra notificación`)
ejemplo1= ejemplo1 %>% mutate(finalizada_1ra_alerta= round(`1ra notificación`/ total,3))
ejemplo1= ejemplo1 %>% mutate(finalizada_2da_alerta= round(`2da notificación`/ total,3))
ejemplo1= ejemplo1 %>% mutate(finalizada_3ra_alerta= round(`3ra notificación`/ total,3))
ejemplo1= ejemplo1 %>% arrange(desc(total))
#
unido=data.frame()
for (i in 1:nrow(ejemplo1)) {
  info_1=NULL
  info_2=NULL
  info_3=NULL
  info_4=NULL
  info_5=NULL
  info_6=NULL
  info_resumen= Base_Alertas_11 %>% filter(Usuario_de_finalizacion==ejemplo1$Var1[i])
  info_1= as.numeric(summary(info_resumen$dias_transcurridos)[1])
  info_2= as.numeric(summary(info_resumen$dias_transcurridos)[2])
  info_3= as.numeric(summary(info_resumen$dias_transcurridos)[3])
  info_4= as.numeric(summary(info_resumen$dias_transcurridos)[4])
  info_5= as.numeric(summary(info_resumen$dias_transcurridos)[5])
  info_6= as.numeric(summary(info_resumen$dias_transcurridos)[6])
  unido1=data.frame(nombre=ejemplo1$Var1[i], minimo=info_1, Primer_Q= info_2, Mediana=info_3,
                    Media= info_4, Tercer_Q= info_5, Maximo= info_6)
  unido= rbind(unido, unido1)
}
Total_resumen= cbind(ejemplo1, unido[,2:7])
####################### RESUMEN EFECTIVIDAD #################
datos= Base_Alertas_1 %>% filter(Fecha_de_generacion>=as.Date("2023-1-1"))
datos$MEDIO_DE_IDENTIFICACION_DEL_CLIENTE[datos$MEDIO_DE_IDENTIFICACION_DEL_CLIENTE=="ALERTAS FINDER GOST COMPLIANCE"]="ALERTAS FINDER"
datos1= datos %>% filter(Ros==1) %>% group_by(Caracteristica_2,MEDIO_DE_IDENTIFICACION_DEL_CLIENTE,
                                              Valor_esperado) %>% tally()
names(datos1)[4]="Ros"
datos2= datos  %>% group_by(gestion, Caracteristica_2, Delito_de_LGI_y_o_Delitos_Precedentes) %>% tally()
datos22= datos %>% group_by(Caracteristica_2,Valor_esperado) %>% tally()
datos22_1= datos22 %>% filter(Valor_esperado %in% c(25,15,40,30,3,4,5,6,15000,22000,350,500,7500,2500,3000))
names(datos2)[4]="Total"
##
persona111= datos %>% filter(ROS=="NO") %>% group_by(Caracteristica_2, MEDIO_DE_IDENTIFICACION_DEL_CLIENTE,
                                                              Valor_esperado) %>% tally()
persona111= persona111 %>% filter(MEDIO_DE_IDENTIFICACION_DEL_CLIENTE=="ALERTAS FINDER")
names(persona111)[4]="Inusual"
datos11= datos1 %>% filter(MEDIO_DE_IDENTIFICACION_DEL_CLIENTE=="ALERTAS FINDER")
datos222= datos22 %>% left_join(datos11, by=c("Caracteristica_2", "Valor_esperado"))
datos222_1= datos222 %>% filter(!is.na(MEDIO_DE_IDENTIFICACION_DEL_CLIENTE))
datos222_1= datos222_1 %>% left_join(persona111, by=c("Caracteristica_2",
                                                      "Valor_esperado","MEDIO_DE_IDENTIFICACION_DEL_CLIENTE"
                                                      ))
datos222_1$Inusual[is.na(datos222_1$Inusual)]=0
persona= datos %>% filter(Ros==1 & Valor_esperado==15000 & Caracteristica_2=="DEPOSITOS CAJA DE AHORRO"&
                            MEDIO_DE_IDENTIFICACION_DEL_CLIENTE=="ALERTAS FINDER")
#
datos222_2= datos22 %>% filter(!(Valor_esperado %in% datos222_1$Valor_esperado) )
datos222_2= datos222_2 %>% filter(Valor_esperado %in% c(25,15,40,30,3,4,5,6,15000,22000,350,500,7500,2500,3000))
datos222_2= datos222_2 %>% arrange(desc(n))

#############################################
Base_Alertas_1= Base_Alertas_1 %>% left_join(Base_Ros_a_julio_21_23_1[,c(1,3,15,16,32,33)], by=c("Nro_de_identificacion"="CI"))
names(Base_Alertas_1)[32]="Detalle_Actividad_Economica_1"
names(Base_Alertas_1)[33]="Nivel_Ingresos_1"
names(Base_Alertas_1)[34]="MEDIO_DE_IDENTIFICACION_DEL_CLIENTE_1"
names(Base_Alertas_1)[35]="RESPONSABLE_1"
Base_Alertas_1= Base_Alertas_1 %>% left_join(BASE_INUSUALIDADES[,c(2,5,17:19)], by=c("Nro_de_identificacion"="NUMERO_DE_DOCUMENTO"))
Base_Alertas_1= Base_Alertas_1 %>% group_by(corre) %>% filter(row_number()==1)
##### 31/7/2023
datos= BASE_INUSUALIDADES %>% filter(NUMERO_DE_DOCUMENTO %in% Base_Ros_a_julio_21_23_1$CI )
datos= datos %>% filter(TIPO_DE_PERSONA=="NATURAL")
datos1= datos %>% filter(!is.na(datos$FECHA_DE_NACIMIENTO))
datos1= datos1 %>% mutate(edad= round(as.numeric((today()- FECHA_DE_NACIMIENTO)/365.25),0))
#
grupo_1= datos1 %>% select(edad, SEXO, REGIONAL)
#
persona= datos %>% filter(Caracteristica_2==datos222_1$Caracteristica_2[1]&
                                     Valor_esperado== datos222_1$Valor_esperado[1])
#
unido=data.frame()
for (i in 1:nrow(datos222_1)) {
  info_1=NULL
  info_2=NULL
  info_3=NULL
  info_4=NULL
  info_5=NULL
  info_6=NULL
  info_resumen= datos %>% filter(Caracteristica_2==datos222_1$Caracteristica_2[i]&
                                   Valor_esperado== datos222_1$Valor_esperado[i])
  info_1= as.numeric(summary(info_resumen$Valor_operado)[1])
  info_2= as.numeric(summary(info_resumen$Valor_operado)[2])
  info_3= as.numeric(summary(info_resumen$Valor_operado)[3])
  info_4= as.numeric(summary(info_resumen$Valor_operado)[4])
  info_5= as.numeric(summary(info_resumen$Valor_operado)[5])
  info_6= as.numeric(summary(info_resumen$Valor_operado)[6])
  unido1=data.frame(nombre=datos222_1$Caracteristica_2[i], minimo=info_1, Primer_Q= info_2, Mediana=info_3,
                    Media= info_4, Tercer_Q= info_5, Maximo= info_6)
  unido= rbind(unido, unido1)
}



####
#4998710001027317 nchar(4998710001027317)
###
#49987100010273172802 nchar(49987100010273172802)

######################## SEGMENTACION DE LA BASE DE TRANSACCIONES ############
Base_resumen_extract=Base_resumen_extract %>% mutate(max_dep_usd= round(ifelse(max_dep!=0,max_dep/6.86,max_dep),2))
Base_resumen_extract=Base_resumen_extract %>% mutate(max_ret_usd= round(ifelse(max_ret!=0,max_ret/6.86,max_ret),2))
Base_resumen_extract=Base_resumen_extract %>% mutate(tot_dep_usd= round(ifelse(tot_dep!=0,tot_dep/6.86,tot_dep),2))
Base_resumen_extract=Base_resumen_extract %>% mutate(to_ret_usd= round(ifelse(to_ret!=0,to_ret/6.86,to_ret),2))
#

#
Base_resumen_extract= Base_resumen_extract %>% filter(!(Descripcion %in% c("ACH ENVÍO RECHAZADO","Regularización ACH",
                                                                         "Transferencia Cobranzas ACH")))
Base_resumen_extract= Base_resumen_extract %>% mutate(Descripcion_1= Descripcion)
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="DEP.CHEQUES OTROS BANCOS"]="DEPOSITO EN EFECTIVO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="DEPOSITO EFECTIVO BOLIVIANOS"]="DEPOSITO EN EFECTIVO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="DEPOSITO EFECTIVO DOLARES"]="DEPOSITO EN EFECTIVO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="DEPOSITO EFECTIVO DOLARES"]="DEPOSITO EN EFECTIVO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="RETIRO EFECTIVO BOLIVIANOS"]="RETIRO EN EFECTIVO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="RETIRO EFECTIVO DOLARES"]="RETIRO EN EFECTIVO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="DEP.CHEQUES OTROS BANCOS"]="DEPOSITO EN EFECTIVO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="ACH ENVÍO DÉBITO CA"]="ACH ENVIO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="Transferencia Cobranzas ACH"]="ACH ENVIO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="ACH ENVÍO DÉBITO CA"]="ACH ENVIO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="Transferencias ACH Empresas"]="ACH ENVIO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="Transf. de Fondos Seguros"]="ACH RECIBIDA"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="ACH Recibida (Express)"]="ACH RECIBIDA"

Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="Regularización ACH"]="ACH ENVIO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="Transferencia entre Ctas (Emp)"]="ACH"
#
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="BTS Transferencia ACH"]="ACH ENVIO"
Base_resumen_extract$Descripcion_1[Base_resumen_extract$Descripcion_1=="BTS Transferencia ACH Express"]="ACH ENVIO"
Base_resumen_extract= Base_resumen_extract %>% mutate(Descripcion_1= ifelse(Descripcion_1=="BTS Transferencia Ctas de 3ros",
                                                                            "ACH",Descripcion_1))
Base_resumen_extract= Base_resumen_extract %>% mutate(Descripcion_1= ifelse(Descripcion_1=="TRANSFERENCIA" &
                                                                              tot_dep!=0,
                                                                            "ACH ENVIO",Descripcion_1))
Base_resumen_extract= Base_resumen_extract %>% mutate(Descripcion_1= ifelse(Descripcion_1=="TRANSFERENCIA" &
                                                                              tot_dep==0,
                                                                            "ACH RECIBIDA",Descripcion_1))
#
Categoria_dep_usd=NULL
for (i in 1:nrow(Base_resumen_extract)) {
  print(i)
  if(Base_resumen_extract$tot_dep_usd[i]==0)
    Categoria_dep_usd[i]=99
  else if(Base_resumen_extract$tot_dep_usd[i]>0 & Base_resumen_extract$tot_dep_usd[i]<= 350)
    Categoria_dep_usd[i]=1
  #
  else if(Base_resumen_extract$tot_dep_usd[i]>350 & Base_resumen_extract$tot_dep_usd[i]<= 1000)
    Categoria_dep_usd[i]=2
  else if(Base_resumen_extract$tot_dep_usd[i]>1000 & Base_resumen_extract$tot_dep_usd[i]<= 5000)
    Categoria_dep_usd[i]=3
  #
  else if(Base_resumen_extract$tot_dep_usd[i]>5000 & Base_resumen_extract$tot_dep_usd[i]<= 10000)
    Categoria_dep_usd[i]=4
  else if(Base_resumen_extract$tot_dep_usd[i]>10000 & Base_resumen_extract$tot_dep_usd[i]<= 18000)
    Categoria_dep_usd[i]=5
  else if(Base_resumen_extract$tot_dep_usd[i]>18000 & Base_resumen_extract$tot_dep_usd[i]<= 22000)
    Categoria_dep_usd[i]=6
  else if(Base_resumen_extract$tot_dep_usd[i]>22000 & Base_resumen_extract$tot_dep_usd[i]<= 30000)
    Categoria_dep_usd[i]=7
  else if(Base_resumen_extract$tot_dep_usd[i]>30000)
    Categoria_dep_usd[i]=8
}
Base_resumen_extract= data.frame(Base_resumen_extract, Categoria_dep_usd)
#
Categoria_ret_usd=NULL
for (i in 1:nrow(Base_resumen_extract)) {
  print(i)
  if(Base_resumen_extract$to_ret_usd[i]==0)
    Categoria_ret_usd[i]=99
  else if(Base_resumen_extract$to_ret_usd[i]>0 & Base_resumen_extract$to_ret_usd[i]<= 350)
    Categoria_ret_usd[i]=1
  #
  else if(Base_resumen_extract$to_ret_usd[i]>350 & Base_resumen_extract$to_ret_usd[i]<= 1000)
    Categoria_ret_usd[i]=2
  else if(Base_resumen_extract$to_ret_usd[i]>1000 & Base_resumen_extract$to_ret_usd[i]<= 5000)
    Categoria_ret_usd[i]=3
  #
  else if(Base_resumen_extract$to_ret_usd[i]>5000 & Base_resumen_extract$to_ret_usd[i]<= 10000)
    Categoria_ret_usd[i]=4
  else if(Base_resumen_extract$to_ret_usd[i]>10000 & Base_resumen_extract$to_ret_usd[i]<= 18000)
    Categoria_ret_usd[i]=5
  else if(Base_resumen_extract$to_ret_usd[i]>18000 & Base_resumen_extract$to_ret_usd[i]<= 22000)
    Categoria_ret_usd[i]=6
  else if(Base_resumen_extract$to_ret_usd[i]>22000 & Base_resumen_extract$to_ret_usd[i]<= 30000)
    Categoria_ret_usd[i]=7
  else if(Base_resumen_extract$to_ret_usd[i]>30000)
    Categoria_ret_usd[i]=8
}
Base_resumen_extract= data.frame(Base_resumen_extract, Categoria_ret_usd)
####
datos_alertas= Base_resumen_extract %>% filter(!is.na(cant_may_corte_dep))
datos_alertas_comp= Base_resumen_extract %>% filter(is.na(cant_may_corte_dep))
sum(datos_alertas$cant_may_corte_dep)
datos_alertas %>% count(tipo_op, año, wt= cant_may_corte_dep)
datos_alertas %>% count(tipo_op, año, wt= cant_may_corte_dep) %>% select(n) %>% sum()
summary(datos_alertas$Categoria_dep_usd)
#### ↓ TOTAL DE TRANSACCIONES PARAMETRIZADOS #####
Base_resumen_extract %>% count(año, tipo_op,Categoria_dep_usd)
grafico= Base_resumen_extract %>% filter(Categoria_dep_usd!=99)
grafico=grafico %>% filter(Categoria_dep_usd!=99 & tipo_op=="Regular")
grafico= grafico %>% filter(!(Descripcion=="Cancelación Depósitos Moneda"))
grafico=grafico %>% filter(Categoria_dep_usd!=99 & tipo_op=="Regular") %>%  count(año, tipo_op,Categoria_dep_usd)
#hchart(grafico$tot_dep_usd)%>% hc_add_theme(hc_theme_elementary())
hchart(grafico, "column", hcaes(x = Categoria_dep_usd  , y = n, group=año),
       #stacking = list(enabled = TRUE), # Esta linea es para apilar las columnas
       dataLabels = list(
        # enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>%
  hc_title(text="") %>%
  hc_colors(c(
    "#3484A5FF","#0EAD82","#9E2F7FFF","#E75263FF","#3484A5FF"))
#trabajo con la base de alertas
alertas_1= Base_Alertas_1 %>% filter(Fecha_de_generacion>=as.Date("2022-1-1"))
alertas_11= alertas_1 %>% filter(Caracteristica_1=="Cantidad de Operaciones Mensuales") #	Importe Maximo por operacion
# Cantidad de Operaciones Mensuales
Categoria_mov=NULL
for (i in 1:nrow(alertas_11)) {
  print(i)
  if(alertas_11$Valor_operado[i]==0)
    Categoria_mov[i]=99
  else if(alertas_11$Valor_operado[i]>0 & alertas_11$Valor_operado[i]<= 350)
    Categoria_mov[i]=1
  #
  else if(alertas_11$Valor_operado[i]>350 & alertas_11$Valor_operado[i]<= 1000)
    Categoria_mov[i]=2
  else if(alertas_11$Valor_operado[i]>1000 & alertas_11$Valor_operado[i]<= 5000)
    Categoria_mov[i]=3
  #
  else if(alertas_11$Valor_operado[i]>5000 & alertas_11$Valor_operado[i]<= 10000)
    Categoria_mov[i]=4
  else if(alertas_11$Valor_operado[i]>10000 & alertas_11$Valor_operado[i]<= 18000)
    Categoria_mov[i]=5
  else if(alertas_11$Valor_operado[i]>18000 & alertas_11$Valor_operado[i]<= 22000)
    Categoria_mov[i]=6
  else if(alertas_11$Valor_operado[i]>22000 & alertas_11$Valor_operado[i]<= 30000)
    Categoria_mov[i]=7
  else if(alertas_11$Valor_operado[i]>30000)
    Categoria_mov[i]=8
}
alertas_11= data.frame(alertas_11, Categoria_mov)
alertas_11= alertas_11 %>% mutate(año= substr(Fecha_de_generacion,1,4))
table(alertas_11$Caracteristica_2, alertas_11$Categoria_mov, alertas_11$año)
#
alertas_111= alertas_11 %>% filter((Caracteristica_2 %in% c("DEPOSITOS CAJA DE AHORRO")) )
alertas_111=alertas_111 %>% count(año, Categoria_mov)
#
names(alertas_111)[3]="Cantidad Alertas"
grafico= data.frame(grafico, Cantidad_Alertas=alertas_111[,3])
## para las cantidades
datos_cantidad=Base_resumen_extract %>% filter(Categoria_dep_usd!=99 & tipo_op=="Regular")
datos_cantidad= datos_cantidad %>% filter(!(Descripcion=="Cancelación Depósitos Moneda"))
datos_cantidad= datos_cantidad %>% mutate(categoria_bin_dep=ifelse(cant_dep>=25,1,2))
categoria_bin_dep_1=NULL
for (i in 1:nrow(datos_cantidad)) {
  print(i)
  if(datos_cantidad$cant_dep[i]<=10)
    categoria_bin_dep_1[i]=11
  else if(datos_cantidad$cant_dep[i]>10 & datos_cantidad$cant_dep[i]<=15)
    categoria_bin_dep_1[i]=12
  else if(datos_cantidad$cant_dep[i]>15 & datos_cantidad$cant_dep[i]<=20)
    categoria_bin_dep_1[i]=13
  else if(datos_cantidad$cant_dep[i]>20 & datos_cantidad$cant_dep[i]<25)
    categoria_bin_dep_1[i]=14
  else if(datos_cantidad$cant_dep[i]>=25)
    categoria_bin_dep_1[i]=1
}
datos_cantidad= data.frame(datos_cantidad, categoria_bin_dep_1)
table(datos_cantidad$categoria_bin_dep, datos_cantidad$categoria_bin_dep_1, datos_cantidad$año)
datos_cantidad_2= datos_cantidad %>% filter(categoria_bin_dep==2)
table(datos_cantidad_2$categoria_bin_dep_1, datos_cantidad_2$Categoria_dep_usd, datos_cantidad_2$año)
#
datos_cantidad_1= datos_cantidad %>% count(año, Categoria_dep_usd, categoria_bin_dep)
table(datos_cantidad$Categoria_dep_usd, datos_cantidad$categoria_bin_dep, datos_cantidad$año)
write.xlsx(table(datos_cantidad$Categoria_dep_usd, datos_cantidad$categoria_bin_dep, datos_cantidad$año), file = "elim_1.xlsx")
#
datos_cantidad_basAl= Base_Alertas_1 %>% filter(Caracteristica_1=="Cantidad de Operaciones Mensuales" &
                                                  Caracteristica_2=="DEPOSITOS CAJA DE AHORRO" & año>2021)
#
datos_cantidad_basAl_1= datos_cantidad_basAl %>% count(año ,Valor_esperado)
###############################
##################################### PARA ACH RECIBIDAS ###################
grafico= Base_resumen_extract %>% filter(Categoria_dep_usd!=99)
grafico=grafico %>% filter(Categoria_dep_usd!=99 & tipo_op=="Regular")
grafico= grafico %>% filter(!(Descripcion %in% c("Transf. de Fondos Seguros", "SPT", "ACH ENVÍO RECHAZADO",
                                                 "Cancelación Depósitos Moneda")))
grafico=grafico %>% filter(Categoria_dep_usd!=99 & tipo_op=="Regular") %>%  count(año, tipo_op,Categoria_dep_usd, wt=cant_dep)
#
#trabajo con la base de alertas
alertas_1= Base_Alertas_1 %>% filter(Fecha_de_generacion>=as.Date("2022-1-1"))
alertas_11= alertas_1 %>% filter(Caracteristica_1=="Importe Maximo por operacion") #	Importe Maximo por operacion
# Cantidad de Operaciones Mensuales
Categoria_mov=NULL
for (i in 1:nrow(alertas_11)) {
  print(i)
  if(alertas_11$Valor_operado[i]==0)
    Categoria_mov[i]=99
  else if(alertas_11$Valor_operado[i]>0 & alertas_11$Valor_operado[i]<= 350)
    Categoria_mov[i]=1
  #
  else if(alertas_11$Valor_operado[i]>350 & alertas_11$Valor_operado[i]<= 1000)
    Categoria_mov[i]=2
  else if(alertas_11$Valor_operado[i]>1000 & alertas_11$Valor_operado[i]<= 5000)
    Categoria_mov[i]=3
  #
  else if(alertas_11$Valor_operado[i]>5000 & alertas_11$Valor_operado[i]<= 10000)
    Categoria_mov[i]=4
  else if(alertas_11$Valor_operado[i]>10000 & alertas_11$Valor_operado[i]<= 18000)
    Categoria_mov[i]=5
  else if(alertas_11$Valor_operado[i]>18000 & alertas_11$Valor_operado[i]<= 22000)
    Categoria_mov[i]=6
  else if(alertas_11$Valor_operado[i]>22000 & alertas_11$Valor_operado[i]<= 30000)
    Categoria_mov[i]=7
  else if(alertas_11$Valor_operado[i]>30000)
    Categoria_mov[i]=8
}
alertas_11= data.frame(alertas_11, Categoria_mov)
alertas_11= alertas_11 %>% mutate(año= substr(Fecha_de_generacion,1,4))
table(alertas_11$Caracteristica_2, alertas_11$Categoria_mov, alertas_11$año)
#
alertas_111= alertas_11 %>% filter((Caracteristica_2 %in% c("DEPOSITOS CAJA DE AHORRO")))
alertas_111=alertas_111 %>% count(año, Categoria_mov)
#
names(alertas_111)[3]="Cantidad Alertas"
grafico= data.frame(grafico, Cantidad_Alertas=alertas_111[,3])
## para las cantidades
datos_cantidad=Base_resumen_extract %>% filter(Categoria_dep_usd!=99 & tipo_op=="Regular")
datos_cantidad= datos_cantidad %>% filter(!(Descripcion%in% c("Transf. de Fondos Seguros", "SPT", "ACH ENVÍO RECHAZADO",
                                                              "Cancelación Depósitos Moneda")))
datos_cantidad= datos_cantidad %>% mutate(categoria_bin_dep=ifelse(cant_dep>=30,1,2))
categoria_bin_dep_1=NULL
# for (i in 1:nrow(datos_cantidad)) {
#   print(i)
#   if(datos_cantidad$cant_dep[i]<=10)
#     categoria_bin_dep_1[i]=11
#   else if(datos_cantidad$cant_dep[i]>10 & datos_cantidad$cant_dep[i]<=20)
#     categoria_bin_dep_1[i]=12
#   else if(datos_cantidad$cant_dep[i]>20 & datos_cantidad$cant_dep[i]<=30)
#     categoria_bin_dep_1[i]=13
#   else if(datos_cantidad$cant_dep[i]>30 & datos_cantidad$cant_dep[i]<36)
#     categoria_bin_dep_1[i]=14
#   else if(datos_cantidad$cant_dep[i]>=36)
#     categoria_bin_dep_1[i]=1
# }
for (i in 1:nrow(datos_cantidad)) {
  print(i)
  if(datos_cantidad$cant_dep[i]<=10)
    categoria_bin_dep_1[i]=11
  else if(datos_cantidad$cant_dep[i]>10 & datos_cantidad$cant_dep[i]<=20)
    categoria_bin_dep_1[i]=12
  else if(datos_cantidad$cant_dep[i]>20 & datos_cantidad$cant_dep[i]<=25)
    categoria_bin_dep_1[i]=13
  else if(datos_cantidad$cant_dep[i]>25 & datos_cantidad$cant_dep[i]<30)
    categoria_bin_dep_1[i]=14
  else if(datos_cantidad$cant_dep[i]>=30)
    categoria_bin_dep_1[i]=1
}
datos_cantidad= data.frame(datos_cantidad, categoria_bin_dep_1)
# el siguiente es para ver los mayores a 36 y menores a 36
table(datos_cantidad$categoria_bin_dep, datos_cantidad$Categoria_dep_usd, datos_cantidad$año)
table(datos_cantidad$categoria_bin_dep, datos_cantidad$categoria_bin_dep_1, datos_cantidad$año)
datos_cantidad_2= datos_cantidad %>% filter(categoria_bin_dep==2)
# el siguiente es para los menores al corte
table(datos_cantidad_2$categoria_bin_dep_1, datos_cantidad_2$Categoria_dep_usd, datos_cantidad_2$año)
#
datos_cantidad_21=datos_cantidad %>% count(año, categoria_bin_dep_1, Categoria_dep_usd, wt=cant_dep)
datos_cantidad_22= datos_cantidad_21 %>% spread(key=Categoria_dep_usd, value = n)
#
datos_cantidad_1= datos_cantidad %>% count(año, Categoria_dep_usd, categoria_bin_dep)
table(datos_cantidad$Categoria_dep_usd, datos_cantidad$categoria_bin_dep, datos_cantidad$año)
write.xlsx(table(datos_cantidad$Categoria_dep_usd, datos_cantidad$categoria_bin_dep, datos_cantidad$año), file = "elim_20.xlsx")
#
datos_cantidad_basAl= Base_Alertas_1 %>% filter(Caracteristica_1=="Cantidad de Operaciones Mensuales" &
                                                  Caracteristica_2=="TRANSACCIONES ACH RECIBIDA" & año>2021)
#
datos_cantidad_basAl_1= datos_cantidad_basAl %>% count(año ,Valor_esperado)
write.xlsx(datos_cantidad_basAl_1, file = "elim_11.xlsx")
###
###############################
##################################### PARA ACH ENVIO ###################
grafico= Base_resumen_extract %>% filter(Categoria_ret_usd!=99)
grafico=grafico %>% filter(Categoria_ret_usd!=99 & tipo_op=="ACH")
grafico= grafico %>% filter(!(Descripcion %in% c("Transf. de Fondos Seguros", "SPT", "ACH ENVÍO RECHAZADO")))
grafico=grafico %>% filter(Categoria_ret_usd!=99 & tipo_op=="ACH") %>%  count(año, tipo_op,Categoria_ret_usd, wt=cant_ret)
#
#trabajo con la base de alertas
alertas_1= Base_Alertas_1 %>% filter(Fecha_de_generacion>=as.Date("2022-1-1"))
alertas_11= alertas_1 %>% filter(Caracteristica_1=="Importe Maximo por operacion") #	Importe Maximo por operacion
# Cantidad de Operaciones Mensuales
Categoria_mov=NULL
for (i in 1:nrow(alertas_11)) {
  print(i)
  if(alertas_11$Valor_operado[i]==0)
    Categoria_mov[i]=99
  else if(alertas_11$Valor_operado[i]>0 & alertas_11$Valor_operado[i]<= 350)
    Categoria_mov[i]=1
  #
  else if(alertas_11$Valor_operado[i]>350 & alertas_11$Valor_operado[i]<= 1000)
    Categoria_mov[i]=2
  else if(alertas_11$Valor_operado[i]>1000 & alertas_11$Valor_operado[i]<= 5000)
    Categoria_mov[i]=3
  #
  else if(alertas_11$Valor_operado[i]>5000 & alertas_11$Valor_operado[i]<= 10000)
    Categoria_mov[i]=4
  else if(alertas_11$Valor_operado[i]>10000 & alertas_11$Valor_operado[i]<= 18000)
    Categoria_mov[i]=5
  else if(alertas_11$Valor_operado[i]>18000 & alertas_11$Valor_operado[i]<= 22000)
    Categoria_mov[i]=6
  else if(alertas_11$Valor_operado[i]>22000 & alertas_11$Valor_operado[i]<= 30000)
    Categoria_mov[i]=7
  else if(alertas_11$Valor_operado[i]>30000)
    Categoria_mov[i]=8
}
alertas_11= data.frame(alertas_11, Categoria_mov)
alertas_11= alertas_11 %>% mutate(año= substr(Fecha_de_generacion,1,4))
table(alertas_11$Caracteristica_2, alertas_11$Categoria_mov, alertas_11$año)
#
alertas_111= alertas_11 %>% filter((Caracteristica_2 %in% c("TRANSACCIONES ACH ENVIO")) )
alertas_111=alertas_111 %>% count(año, Categoria_mov)
#
names(alertas_111)[3]="Cantidad Alertas"
grafico= data.frame(grafico, Cantidad_Alertas=alertas_111[,3])
## para las cantidades
datos_cantidad=Base_resumen_extract %>% filter(Categoria_ret_usd!=99 & tipo_op=="ACH")
datos_cantidad= datos_cantidad %>% filter(!(Descripcion%in% c("Transf. de Fondos Seguros", "SPT", "ACH ENVÍO RECHAZADO")))
datos_cantidad= datos_cantidad %>% mutate(categoria_bin_dep=ifelse(cant_ret>=48,1,2))
categoria_bin_dep_1=NULL
for (i in 1:nrow(datos_cantidad)) {
  print(i)
  if(datos_cantidad$cant_ret[i]<=20)
    categoria_bin_dep_1[i]=11
  else if(datos_cantidad$cant_ret[i]>20 & datos_cantidad$cant_ret[i]<=30)
    categoria_bin_dep_1[i]=12
  else if(datos_cantidad$cant_ret[i]>30 & datos_cantidad$cant_ret[i]<=40)
    categoria_bin_dep_1[i]=13
  else if(datos_cantidad$cant_ret[i]>40 & datos_cantidad$cant_ret[i]<48)
    categoria_bin_dep_1[i]=14
  else if(datos_cantidad$cant_ret[i]>=48)
    categoria_bin_dep_1[i]=1
}
datos_cantidad= data.frame(datos_cantidad, categoria_bin_dep_1)
# el siguiente es para ver los mayores a 48 y menores a 48
table(datos_cantidad$categoria_bin_dep, datos_cantidad$Categoria_ret_usd, datos_cantidad$año)
table(datos_cantidad$categoria_bin_dep, datos_cantidad$categoria_bin_dep_1, datos_cantidad$año)
datos_cantidad_2= datos_cantidad %>% filter(categoria_bin_dep==2)
# el siguiente es para los menores al corte
table(datos_cantidad_2$categoria_bin_dep_1, datos_cantidad_2$Categoria_ret_usd, datos_cantidad_2$año)
#
#
datos_cantidad_21=datos_cantidad %>% count(año, categoria_bin_dep_1, Categoria_ret_usd, wt=cant_ret)
datos_cantidad_22= datos_cantidad_21 %>% spread(key=Categoria_ret_usd, value = n)
#
datos_cantidad_1= datos_cantidad %>% count(año, Categoria_ret_usd, categoria_bin_dep)
table(datos_cantidad$Categoria_ret_usd, datos_cantidad$categoria_bin_dep, datos_cantidad$año)
write.xlsx(table(datos_cantidad$Categoria_ret_usd, datos_cantidad$categoria_bin_dep, datos_cantidad$año), file = "elim_13.xlsx")
#
datos_cantidad_basAl= Base_Alertas_1 %>% filter(Caracteristica_1=="Cantidad de Operaciones Mensuales" &
                                                  Caracteristica_2=="TRANSACCIONES ACH ENVIO" & año>2021)
#
datos_cantidad_basAl_1= datos_cantidad_basAl %>% count(año ,Valor_esperado)
write.xlsx(datos_cantidad_basAl_1, file = "elim_15.xlsx")
### importacion de la base de datos de clientes con corte al 31/7/2023
BASE_4 <- read_excel("D:/Nueva carpeta1/Base_Clientes_31_7_23/BASE 4.xlsx")
BASE_5= as.data.frame(BASE_5)
#
BASE_4$FECHA_NACIMIENTO= substr(BASE_4$FECHA_NACIMIENTO, 1,10)
BASE_4$F_ALTA_CLIENTE= substr(BASE_4$F_ALTA_CLIENTE,1,10)
###
Base_Clientes_BSol_vf= rbind(BASE_1, BASE_2, BASE_3, BASE_4, BASE_5)
Base_Clientes_BSol_vf$FECHA_NACIMIENTO= as.Date(Base_Clientes_BSol_vf$FECHA_NACIMIENTO)
Base_Clientes_BSol_vf$F_ALTA_CLIENTE= as.Date(Base_Clientes_BSol_vf$F_ALTA_CLIENTE)
####
vec_dias= c(1:25,1:25, 1:25, 1:25)
transac_cli= Base_resumen_extract %>% filter(ID_Cliente==1976709) #vec_dias[1:nrow(transac_cli)]
transac_cli= transac_cli %>% mutate(fecha= paste(año,mes,"1", sep = "-"))
transac_cli$fecha=as.Date(transac_cli$fecha)
transac_cli_1= transac_cli %>% filter(Categoria_dep_usd!=99)
transac_cli_2= transac_cli %>% filter(Categoria_ret_usd!=99)
transac_cli_1= transac_cli_1[,c(32,31,27)]
transac_cli_1= transac_cli_1 %>% group_by(fecha, tipo_op) %>% summarise(tot_dep_usd= sum(tot_dep_usd))
names(transac_cli_1)[3]="Importe"
transac_cli_1$tipo_op[transac_cli_1$tipo_op=="ACH"]="Deposito en Linea"
transac_cli_1$tipo_op[transac_cli_1$tipo_op=="Regular"]="Deposito en Cajas"
#
transac_cli_2= transac_cli_2[,c(32,31,28)]
transac_cli_2= transac_cli_2 %>% group_by(fecha, tipo_op) %>% summarise(to_ret_usd= sum(to_ret_usd))
names(transac_cli_2)[3]="Importe"
transac_cli_2$tipo_op[transac_cli_2$tipo_op=="ACH"]="Retiro en Linea"
transac_cli_2$tipo_op[transac_cli_2$tipo_op=="Regular"]="Retiro en Cajas"
transac_cli_un= rbind(transac_cli_1, transac_cli_2)
transac_cli_un$Importe= round(transac_cli_un$Importe,0)
transac_cli_un=as.data.frame(transac_cli_un)
hchart(transac_cli_un, "column" ,hcaes(x=fecha, y=Importe,group= tipo_op),
       stacking = list(enabled = TRUE),
       dataLabels = list(
        # enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )
       )  %>% hc_add_theme(hc_theme_google())
            # %>%  hc_colors(c("#3A5C9BFF", "#37B878FF","#FF0000","#37B878FF","#9E2F7FFF","#3A5C9BFF"))
####
unido= data.frame()
for (i in 1:length(unique(REVISION_NOMBRES_Y_CI_IDENTIFICADOS_CORRECTOS$numeros_raiz))) {
  print(i)
  caso= REVISION_NOMBRES_Y_CI_IDENTIFICADOS_CORRECTOS %>% filter(numeros_raiz== unique(REVISION_NOMBRES_Y_CI_IDENTIFICADOS_CORRECTOS$numeros_raiz)[i])
  if(sum(caso$CLASIFICACION %in% c("C","T","N"))>=1)
    caso= data.frame(caso,tipo=1)
  else
    caso= data.frame(caso,tipo=2)
  unido= rbind(unido, caso)
}
##
unido= data.frame()
for (i in 1:length(unique(REVISION_CI_DUPLICADOS$numeros_carnet))) {
  print(i)
  caso= REVISION_CI_DUPLICADOS %>% filter(numeros_carnet==unique(REVISION_CI_DUPLICADOS$numeros_carnet)[i])
  if(sum(is.na(caso$PRODUCTOS))>=1)
    caso= data.frame(caso, tipo=sum(is.na(caso$PRODUCTOS)))
  else{caso= data.frame(caso, tipo=99)}

  unido= rbind(unido, caso)
}

########## SECTOR POE DE ACUERDO A TRANSACCIONES ###
unique(Base_resumen_extract$Descripcion)
persona= Base_resumen_extract %>% filter(ID_Cliente==unique(extraccion$Nro_ID_Cliente)[108])
persona= persona %>% filter(!(lDescripcion %in% c("ABONO CTA. AHORROS TRANSF. BCB", "Transf. de Fondos Seguros",
                                               "Regularización ACH", "ACH ENVÍO RECHAZADO", "BTS Transferencia Ctas propias",
                                               "BTS Transferencia Ctas de 3ros")))
persona$Descripcion[persona$Descripcion=="BTS Transferencia ACH"]="ACH ENVÍO DÉBITO CA"
persona$Descripcion[persona$Descripcion=="BTS Transferencia ACH Express"]="ACH ENVÍO DÉBITO CA"
persona$Descripcion[persona$Descripcion=="DEPOSITO EFECTIVO BOLIVIANOS"]="DEPOSITO EFECTIVO"
persona$Descripcion[persona$Descripcion=="DEPOSITO EFECTIVO DOLARES"]="DEPOSITO EFECTIVO"
persona$Descripcion[persona$Descripcion=="DEP.CHEQUES OTROS BANCOS"]="DEPOSITO EFECTIVO"
persona$Descripcion[persona$Descripcion=="RETIRO EFECTIVO BOLIVIANOS"]="RETIRO EFECTIVO"
persona$Descripcion[persona$Descripcion=="RETIRO EFECTIVO DOLARES"]="RETIRO EFECTIVO"
persona$Descripcion[persona$Descripcion=="ACH Recibida (Express)"]="ACH RECIBIDA"
persona$Descripcion[persona$Descripcion=="BTS Transferencia 3ro Express"]="BTS Transferencia Ctas de 3ros"
persona$Descripcion[persona$Descripcion=="Transferencia a otras cuentas"]="BTS Transferencia Ctas de 3ros"
persona$Descripcion[persona$Descripcion=="Transferencias a 3ros Empresas"]="BTS Transferencia Ctas de 3ros"
persona$Descripcion[persona$Descripcion=="Retiro en ATM propio Fuera Hor"]="Retiro en ATM"
persona$Descripcion[persona$Descripcion=="Retiro en ATM propio"]="Retiro en ATM"
persona$Descripcion[persona$Descripcion=="Depósito efectivo ATM propio"]="Deposito en ATM"
persona= persona %>% filter(!(Descripcion %in% c("BTS Transferencia Ctas de 3ros")))
unique(persona$Descripcion)
persona= persona %>% mutate(date= paste(año,mes,"1", sep = "-"))
persona$date= as.Date(persona$date)
persona= persona %>% filter(tipo_op=="ACH") %>% group_by(date) %>% summarise(tot_dep=sum(Opr_dep_usd),to_ret= sum(Opr_ret_usd),
                                                                                 cant_dep= sum(cant_dep),
                                                                                 cant_ret= sum(cant_ret))
## DESDE AQUI LA GENERACION DE PARAMETROS 25/8/2023 ###############
####
LCL_tot_dep=NULL
LC_tot_dep=NULL
UCL_tot_dep=NULL
#
LCL_to_ret=NULL
LC_to_ret=NULL
UCL_to_ret=NULL
#
LCL_cant_dep=NULL
LC_cant_dep=NULL
UCL_cant_dep=NULL
#
LCL_cant_ret=NULL
LC_cant_ret=NULL
UCL_cant_ret=NULL
#
corte_fecha= as.Date("2022-9-1")
casos_analisis= Base_resumen_extract %>% filter(ID_Cliente %in% extraccion$Nro_ID_Cliente )
#length(unique(extraccion$Nro_ID_Cliente))
for (i in 1:length(unique(extraccion$Nro_ID_Cliente))) {
  print(i)
  control1=NULL
  control2=NULL
  control3=NULL
  control4=NULL
  persona= casos_analisis %>% filter(ID_Cliente==unique(extraccion$Nro_ID_Cliente)[i])
  persona= persona %>% filter(!(Descripcion %in% c("ABONO CTA. AHORROS TRANSF. BCB", "Transf. de Fondos Seguros",
                                                   "Regularización ACH", "ACH ENVÍO RECHAZADO", "BTS Transferencia Ctas propias",
                                                   "BTS Transferencia Ctas de 3ros")))
  persona$Descripcion[persona$Descripcion=="BTS Transferencia ACH"]="ACH ENVÍO DÉBITO CA"
  persona$Descripcion[persona$Descripcion=="BTS Transferencia ACH Express"]="ACH ENVÍO DÉBITO CA"
  persona$Descripcion[persona$Descripcion=="DEPOSITO EFECTIVO BOLIVIANOS"]="DEPOSITO EFECTIVO"
  persona$Descripcion[persona$Descripcion=="DEPOSITO EFECTIVO DOLARES"]="DEPOSITO EFECTIVO"
  persona$Descripcion[persona$Descripcion=="DEP.CHEQUES OTROS BANCOS"]="DEPOSITO EFECTIVO"
  persona$Descripcion[persona$Descripcion=="RETIRO EFECTIVO BOLIVIANOS"]="RETIRO EFECTIVO"
  persona$Descripcion[persona$Descripcion=="RETIRO EFECTIVO DOLARES"]="RETIRO EFECTIVO"
  persona$Descripcion[persona$Descripcion=="ACH Recibida (Express)"]="ACH RECIBIDA"
  persona$Descripcion[persona$Descripcion=="BTS Transferencia 3ro Express"]="BTS Transferencia Ctas de 3ros"
  persona$Descripcion[persona$Descripcion=="Transferencia a otras cuentas"]="BTS Transferencia Ctas de 3ros"
  persona$Descripcion[persona$Descripcion=="Transferencias a 3ros Empresas"]="BTS Transferencia Ctas de 3ros"
  persona$Descripcion[persona$Descripcion=="Retiro en ATM propio Fuera Hor"]="Retiro en ATM"
  persona$Descripcion[persona$Descripcion=="Retiro en ATM propio"]="Retiro en ATM"
  persona$Descripcion[persona$Descripcion=="Depósito efectivo ATM propio"]="Deposito en ATM"
  persona= persona %>% filter(!(Descripcion %in% c("BTS Transferencia Ctas de 3ros")))
  persona= persona %>% mutate(date= paste(año,mes,"1", sep = "-"))
  persona$date= as.Date(persona$date)
  persona= persona %>% filter(tipo_op=="ACH") %>% group_by(date) %>% summarise(tot_dep=sum(Opr_dep_usd),to_ret= sum(Opr_ret_usd),
                                                                               cant_dep= sum(cant_dep),
                                                                               cant_ret= sum(cant_ret))
  if(dim(persona)[1]==1)
  {
    #
    LCL_tot_dep[i] =persona$tot_dep
    LC_tot_dep[i]=persona$tot_dep
    UCL_tot_dep[i]=persona$tot_dep
    #
    LCL_to_ret[i]=persona$to_ret
    LC_to_ret[i]=persona$to_ret
    UCL_to_ret[i]=persona$to_ret
    #
    LCL_cant_dep[i]=persona$cant_dep
    LC_cant_dep[i]=persona$cant_dep
    UCL_cant_dep[i]=persona$cant_dep
    #
    LCL_cant_ret[i]=persona$cant_ret
    LC_cant_ret[i]=persona$cant_ret
    UCL_cant_ret[i]=persona$cant_ret

  }
  else {
    #######
    pers1= persona %>% filter(tot_dep!=0 & tot_dep>= summary(persona$tot_dep)[2])
    {
      if(dim(pers1)[1]==0)
    {
      LCL_tot_dep[i] =0
      LC_tot_dep[i]=0
      UCL_tot_dep[i]=0
    }
    else if(dim(pers1)[1]==1)
    {
      LCL_tot_dep[i] =pers1$tot_dep
      LC_tot_dep[i]=pers1$tot_dep
      UCL_tot_dep[i]=pers1$tot_dep
    }
    else {
      control1=qcc(pers1$tot_dep, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_tot_dep[i] =control1$limits[1]
      LC_tot_dep[i]=control1$center
      UCL_tot_dep[i]=control1$limits[2]
    }}
    #
    pers2= persona %>% filter(to_ret!=0 & to_ret>= summary(persona$to_ret)[2])
    {
      if(dim(pers2)[1]==0)
    {
      LCL_to_ret[i] =0
      LC_to_ret[i]=0
      UCL_to_ret[i]=0
    }
    else if(dim(pers2)[1]==1)
    {
      LCL_to_ret[i] =pers2$to_ret
      LC_to_ret[i]=pers2$to_ret
      UCL_to_ret[i]=pers2$to_ret
    }
    else {
      control2=qcc(pers2$to_ret, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_to_ret[i]=control2$limits[1]
      LC_to_ret[i]=control2$center
      UCL_to_ret[i]=control2$limits[2]
    }}
    #
    pers3= persona %>% filter(cant_dep!=0 & cant_dep>= summary(persona$cant_dep)[2])
    {if(dim(pers3)[1]==0)
    {
      LCL_cant_dep[i] =0
      LC_cant_dep[i]=0
      UCL_cant_dep[i]=0
    }
    else if(dim(pers3)[1]==1)
    {
      LCL_cant_dep[i] =pers3$cant_dep
      LC_cant_dep[i]=pers3$cant_dep
      UCL_cant_dep[i]=pers3$cant_dep
    }
    else {
      control3=qcc(pers3$cant_dep, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_cant_dep[i]=control3$limits[1]
      LC_cant_dep[i]=control3$center
      UCL_cant_dep[i]=control3$limits[2]
    }}
    #
    pers4= persona %>% filter(cant_ret!=0 & cant_ret>= summary(persona$cant_ret)[2])
    {if(dim(pers4)[1]==0)
    {
      LCL_cant_ret[i] =0
      LC_cant_ret[i]=0
      UCL_cant_ret[i]=0
    }
    else if(dim(pers4)[1]==1)
    {
      LCL_cant_ret[i] =pers4$cant_ret
      LC_cant_ret[i]=pers4$cant_ret
      UCL_cant_ret[i]=pers4$cant_ret
    }
    else {
      control4=qcc(pers4$cant_ret, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_cant_ret[i]=control4$limits[1]
      LC_cant_ret[i]=control4$center
      UCL_cant_ret[i]=control4$limits[2]
    }}

  }
}
base_limites_R= data.frame(Nro_ID_Cliente=unique(extraccion$Nro_ID_Cliente), LCL_tot_dep, LC_tot_dep, UCL_tot_dep,LCL_to_ret,LC_to_ret,UCL_to_ret,
                           LCL_cant_dep,LC_cant_dep,UCL_cant_dep,LCL_cant_ret,LC_cant_ret,UCL_cant_ret)
base_limites_R$LCL_cant_dep= trunc(base_limites_R$LCL_cant_dep+1)
base_limites_R$LC_cant_dep= trunc(base_limites_R$LC_cant_dep+1)
base_limites_R$UCL_cant_dep= trunc(base_limites_R$UCL_cant_dep+1)
base_limites_R$LCL_cant_ret= trunc(base_limites_R$LCL_cant_ret+1)
base_limites_R$LC_cant_ret= trunc(base_limites_R$LC_cant_ret+1)
base_limites_R$UCL_cant_ret= trunc(base_limites_R$UCL_cant_ret+1)
base_limites_R$LCL_cant_dep[is.na(base_limites_R$LCL_cant_dep)]=0
base_limites_R$LC_cant_dep[is.na(base_limites_R$LC_cant_dep)]=0
base_limites_R$UCL_cant_dep[is.na(base_limites_R$UCL_cant_dep)]=0
base_limites_R$LCL_cant_ret[is.na(base_limites_R$LCL_cant_ret)]=0
base_limites_R$LC_cant_ret[is.na(base_limites_R$LC_cant_ret)]=0
base_limites_R$UCL_cant_ret[is.na(base_limites_R$UCL_cant_ret)]=0
#
base_limites_R$LCL_tot_dep= trunc(base_limites_R$LCL_tot_dep)
base_limites_R$LC_tot_dep= trunc(base_limites_R$LC_tot_dep)
base_limites_R$UCL_tot_dep= trunc(base_limites_R$UCL_tot_dep)
base_limites_R$LCL_to_ret= trunc(base_limites_R$LCL_to_ret)
base_limites_R$LC_to_ret= trunc(base_limites_R$LC_to_ret)
base_limites_R$UCL_to_ret= trunc(base_limites_R$UCL_to_ret)

#
PERSONAS_JURIDICAS_bajo_riesgo$USERID= as.character(PERSONAS_JURIDICAS_bajo_riesgo$USERID)
PERSONAS_JURIDICAS_bajo_riesgo_1= PERSONAS_JURIDICAS_bajo_riesgo %>% mutate(corre=1:nrow(PERSONAS_JURIDICAS_bajo_riesgo))
PERSONAS_JURIDICAS_bajo_riesgo_1= PERSONAS_JURIDICAS_bajo_riesgo %>% left_join(base_limites_R, by=c("USERID"="ID_Cliente"))

#
PERSONAS_JURIDICAS$USERID= as.character(PERSONAS_JURIDICAS$USERID)
PERSONAS_JURIDICAS_1= PERSONAS_JURIDICAS %>% mutate(corre=1:nrow(PERSONAS_JURIDICAS))
PERSONAS_JURIDICAS_1= PERSONAS_JURIDICAS %>% left_join(base_limites_R, by=c("USERID"="ID_Cliente"))
##########################
####
LCL_tot_dep=NULL
LC_tot_dep=NULL
UCL_tot_dep=NULL
#
LCL_to_ret=NULL
LC_to_ret=NULL
UCL_to_ret=NULL
#
LCL_cant_dep=NULL
LC_cant_dep=NULL
UCL_cant_dep=NULL
#
LCL_cant_ret=NULL
LC_cant_ret=NULL
UCL_cant_ret=NULL
for (i in 1:nrow(prueba_c)) {
  control1=NULL
  control2=NULL
  control3=NULL
  control4=NULL

  persona= Base_resumen_pj_extract_m23_extns %>% filter(ID_Cliente==prueba_c$ID_Cliente[i])
  persona$cant_dep[is.na(persona$cant_dep)]=0
  persona$cant_ret[is.na(persona$cant_ret)]=0
  #persona= persona %>% filter(año>2021)
  persona= persona %>% mutate(date= paste(año,mes,"1", sep = "-"))
  persona$date= as.Date(persona$date)
  persona= persona %>% filter(tipo_op=="ACH") %>% group_by(date) %>% summarise(tot_dep=sum(tot_dep),to_ret= sum(to_ret),
                                                                               cant_dep= sum(cant_dep),
                                                                               cant_ret= sum(cant_ret))
  if(dim(persona)[1]==1)
  {
    #
    LCL_tot_dep[i] =persona$tot_dep
    LC_tot_dep[i]=persona$tot_dep
    UCL_tot_dep[i]=persona$tot_dep
    #
    LCL_to_ret[i]=persona$to_ret
    LC_to_ret[i]=persona$to_ret
    UCL_to_ret[i]=persona$to_ret
    #
    LCL_cant_dep[i]=persona$cant_dep
    LC_cant_dep[i]=persona$cant_dep
    UCL_cant_dep[i]=persona$cant_dep
    #
    LCL_cant_ret[i]=persona$cant_ret
    LC_cant_ret[i]=persona$cant_ret
    UCL_cant_ret[i]=persona$cant_ret

  }
  else {
    #######
    pers1= persona %>% filter(tot_dep!=0)
    {if(dim(pers1)[1]==0)
    {
      LCL_tot_dep[i] =0
      LC_tot_dep[i]=0
      UCL_tot_dep[i]=0
    }
    else if(dim(pers1)[1]==1)
    {
      LCL_tot_dep[i] =persona$tot_dep
      LC_tot_dep[i]=persona$tot_dep
      UCL_tot_dep[i]=persona$tot_dep
    }
    else {
      control1=qcc(pers1$tot_dep, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_tot_dep[i] =control1$limits[1]
      LC_tot_dep[i]=control1$center
      UCL_tot_dep[i]=control1$limits[2]
    }}
    #
    pers2= persona %>% filter(to_ret!=0)
    {if(dim(pers2)[1]==0)
    {
      LCL_to_ret[i] =0
      LC_to_ret[i]=0
      UCL_to_ret[i]=0
    }
    else if(dim(pers2)[1]==1)
    {
      LCL_to_ret[i] =persona$to_ret
      LC_to_ret[i]=persona$to_ret
      UCL_to_ret[i]=persona$to_ret
    }
    else {
      control2=qcc(pers2$to_ret, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_to_ret[i]=control2$limits[1]
      LC_to_ret[i]=control2$center
      UCL_to_ret[i]=control2$limits[2]
    }}
    #
    pers3= persona %>% filter(cant_dep!=0)
    {if(dim(pers3)[1]==0)
    {
      LCL_cant_dep[i] =0
      LC_cant_dep[i]=0
      UCL_cant_dep[i]=0
    }
    else if(dim(pers3)[1]==1)
    {
      LCL_cant_dep[i] =persona$cant_dep
      LC_cant_dep[i]=persona$cant_dep
      UCL_cant_dep[i]=persona$cant_dep
    }
    else {
      control3=qcc(pers3$cant_dep, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_cant_dep[i]=control3$limits[1]
      LC_cant_dep[i]=control3$center
      UCL_cant_dep[i]=control3$limits[2]
    }}
    #
    pers4= persona %>% filter(cant_ret!=0)
    {if(dim(pers4)[1]==0)
    {
      LCL_cant_dep[i] =0
      LC_cant_dep[i]=0
      UCL_cant_dep[i]=0
    }
    else if(dim(pers4)[1]==1)
    {
      LCL_cant_dep[i] =persona$cant_ret
      LC_cant_dep[i]=persona$cant_ret
      UCL_cant_dep[i]=persona$cant_ret
    }
    else {
      control4=qcc(pers4$cant_ret, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
      LCL_cant_ret[i]=control4$limits[1]
      LC_cant_ret[i]=control4$center
      UCL_cant_ret[i]=control4$limits[2]
    }}

  }
}
base_limites_ACH= data.frame(prueba_c, LCL_tot_dep_ACH=LCL_tot_dep, LC_tot_dep_ACH=LC_tot_dep, UCL_tot_dep_ACH=UCL_tot_dep,
                             LCL_to_ret_ACH=LCL_to_ret,LC_to_ret_ACH=LC_to_ret,UCL_to_ret_ACH=UCL_to_ret,
                             LCL_cant_dep_ACH=LCL_cant_dep,LC_cant_dep_ACH=LC_cant_dep,
                             UCL_cant_dep_ACH=UCL_cant_dep,LCL_cant_ret_ACH=LCL_cant_ret,
                             LC_cant_ret_ACH=LC_cant_ret,UCL_cant_ret_ACH=UCL_cant_ret)
base_limites_ACH$LCL_cant_dep_ACH= trunc(base_limites_ACH$LCL_cant_dep_ACH+1)
base_limites_ACH$LC_cant_dep_ACH= trunc(base_limites_ACH$LC_cant_dep_ACH+1)
base_limites_ACH$UCL_cant_dep_ACH= trunc(base_limites_ACH$UCL_cant_dep_ACH+1)
base_limites_ACH$LCL_cant_ret_ACH= trunc(base_limites_ACH$LCL_cant_ret_ACH+1)
base_limites_ACH$LC_cant_ret_ACH= trunc(base_limites_ACH$LC_cant_ret_ACH+1)
base_limites_ACH$UCL_cant_ret_ACH= trunc(base_limites_ACH$UCL_cant_ret_ACH+1)
base_limites_ACH$LCL_cant_dep_ACH[is.na(base_limites_ACH$LCL_cant_dep_ACH)]=0
base_limites_ACH$LC_cant_dep_ACH[is.na(base_limites_ACH$LC_cant_dep_ACH)]=0
base_limites_ACH$UCL_cant_dep_ACH[is.na(base_limites_ACH$UCL_cant_dep_ACH)]=0
base_limites_ACH$LCL_cant_ret_ACH[is.na(base_limites_ACH$LCL_cant_ret_ACH)]=0
base_limites_ACH$LC_cant_ret_ACH[is.na(base_limites_ACH$LC_cant_ret_ACH)]=0
base_limites_ACH$UCL_cant_ret_ACH[is.na(base_limites_ACH$UCL_cant_ret_ACH)]=0
#
PERSONAS_JURIDICAS_bajo_riesgo$USERID= as.character(PERSONAS_JURIDICAS_bajo_riesgo$USERID)
PERSONAS_JURIDICAS_bajo_riesgo_2= PERSONAS_JURIDICAS_bajo_riesgo %>% mutate(corre=1:nrow(PERSONAS_JURIDICAS_bajo_riesgo))
PERSONAS_JURIDICAS_bajo_riesgo_2= PERSONAS_JURIDICAS_bajo_riesgo %>% left_join(base_limites_ACH, by=c("USERID"="ID_Cliente"))
PERSONAS_JURIDICAS__bajo_riesgo_vf= cbind(PERSONAS_JURIDICAS_bajo_riesgo_1, PERSONAS_JURIDICAS_bajo_riesgo_2[,25:38])
#save(PERSONAS_JURIDICAS__bajo_riesgo_vf, file = "PERSONAS_JURIDICAS__bajo_riesgo_vf.RData")
#
PERSONAS_JURIDICAS$USERID= as.character(PERSONAS_JURIDICAS$USERID)
PERSONAS_JURIDICAS_2= PERSONAS_JURIDICAS %>% mutate(corre=1:nrow(PERSONAS_JURIDICAS))
PERSONAS_JURIDICAS_2= PERSONAS_JURIDICAS %>% left_join(base_limites_ACH, by=c("USERID"="ID_Cliente"))
####################################
prueba= CASOS %>% count(gestion, Sexo)
prueba$Sexo[is.na(prueba$Sexo)]="Juridica"
hchart(prueba, "column" ,hcaes(x=gestion, y=n,group= Sexo),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_google())
###
rango_edad= NULL
CASOS= CASOS %>% filter(!is.na(Edad))
for (i in 1:nrow(CASOS)) {
  if(CASOS$Edad[i]>=18 & CASOS$Edad[i]<=25)
    rango_edad[i]="18 - 25"
  else if(CASOS$Edad[i]>25 & CASOS$Edad[i]<=30)
    rango_edad[i]="26 - 30"
  else if(CASOS$Edad[i]>30 & CASOS$Edad[i]<=35)
    rango_edad[i]="31 - 35"
  else if(CASOS$Edad[i]>35 & CASOS$Edad[i]<=45)
    rango_edad[i]="36 - 45"
  else if(CASOS$Edad[i]>45 & CASOS$Edad[i]<=55)
    rango_edad[i]="46 - 55"
  else if(CASOS$Edad[i]>55)
    rango_edad[i]=">55"
}
CASOS= data.frame(CASOS, rango_edad)
CASOS= CASOS %>% mutate(gestion= substr(Fecha_Envio,1,4))
#
prueba= CASOS %>% count(gestion, rango_edad)
hchart(prueba, "column" ,hcaes(x=rango_edad, y=n,group= gestion),
      # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_google())
##############
prueba= CASOS %>% count(Detalle_Actividad_Economica, `Detalle Regional_Alta_Cliente`)
hchart(prueba, "spline" ,hcaes(x=Edad, y=n,group= gestion),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         # enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_google())
####
###
casos_reporte= CASOS %>% filter(gestion==2023)
names(casos_reporte)[18]="REGIONAL"
casos_reporte= casos_reporte %>% count(NOMBRE_REGIONAL ,REGIONAL)
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="EL ALTO"]="LP"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="LA PAZ"]="LP"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="SANTA CRUZ"]="SC"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="POTOSI"]="PO"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="COCHABAMBA"]="CB"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="SUCRE"]="CQ"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="BENI"]="EB"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="ORURO"]="OR"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="PANDO"]="PA"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="TARIJA"]="TR"
hcmap("countries/bo/bo-all", data = casos_reporte, value = "n",
      joinBy =c("hc-a2","REGIONAL") , name = "ROS",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#0EAD82", borderWidth = 0.7,
      tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = " Reportes"))
###

##
casos_reporte= BASE_INUSUALIDADES %>% filter(año==2023 & ROS=="SI")
casos_reporte_1= casos_reporte %>% filter(REGIONAL=="SANTA CRUZ")
casos_reporte_1= casos_reporte_1 %>% count(MEDIO_DE_IDENTIFICACION_DEL_CLIENTE)
hchart(casos_reporte_1, "treemap", hcaes(x=MEDIO_DE_IDENTIFICACION_DEL_CLIENTE, value=n, color=n),
       borderColor = "#0EAD82", borderWidth = 2)






mapdata <- get_data_from_map(download_map_data("countries/us/us-all"))
glimpse(mapdata)
set.seed(1234)

data_fake <- mapdata %>%
  select(code = `hc-a2`) %>%
  mutate(value = 1e5 * abs(rt(nrow, df = 10)))

glimpse(data_fake)

casos_reporte$REGIONAL[casos_reporte$REGIONAL=="EL ALTO"]="LP"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="LA PAZ"]="LP"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="SANTA CRUZ"]="SC"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="POTOSI"]="PO"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="COCHABAMBA"]="CB"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="SUCRE"]="CQ"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="BENI"]="EB"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="ORURO"]="OR"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="PANDO"]="PA"
casos_reporte$REGIONAL[casos_reporte$REGIONAL=="TARIJA"]="TR"
hcmap("countries/us/us-all", data = data_fake, value = "value",
      joinBy = c("hc-a2", "code"), name = "Fake data",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#0EAD82", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2, valuePrefix = "$", valueSuffix = " USD"))

hcmap("countries/bo/bo-all", data = casos_reporte, value = "value",
      joinBy = c("hc-a2", "REGIONAL"), name = "Fake data",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#0EAD82", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2, valuePrefix = "", valueSuffix = " "))
###
reporte= CASOS %>% count(gestion,rango_edad,Delito.de.LGI.y.o.Delitos.Precedentes)
reporte1= reporte %>% spread(key = Delito.de.LGI.y.o.Delitos.Precedentes, value = n)
#
reporte= CASOS %>% count(gestion, Delito.de.LGI.y.o.Delitos.Precedentes, NOMBRE_REGIONAL)
reporte1= reporte %>% spread(key = NOMBRE_REGIONAL, value = n)
#
reporte= CASOS %>% count(gestion, NOMBRE_REGIONAL)
reporte1= reporte %>% spread(key = NOMBRE_REGIONAL, value = n)





data <- tibble(
  country =
    c("PT", "IE", "GB", "IS",

      "NO", "SE", "DK", "DE", "NL", "BE", "LU", "ES", "FR", "PL", "CZ", "AT",
      "CH", "LI", "SK", "HU", "SI", "IT", "SM", "HR", "BA", "YF", "ME", "AL", "MK",

      "FI", "EE", "LV", "LT", "BY", "UA", "MD", "RO", "BG", "GR", "TR", "CY",

      "RU"),
  tz = c(rep("UTC", 4), rep("UTC + 1",25), rep("UCT + 2",12), "UTC + 3")
)

# auxiliar variable
data <- data |>
  mutate(value = cumsum(!duplicated(tz)))
#
dta_clss <- data |>
  mutate(value = cumsum(!duplicated(tz))) |>
  group_by(tz) |>
  summarise(value = unique(value)) |>
  arrange(value) |>
  rename(name = tz, from = value) |>
  mutate(to = from + 1) |>
  list_parse()
##
dta_clss_1= dta_clss
dta_clss_1= dta_clss_1 %>% list_parse



Regional_Sur <- read_excel("D:/Nueva carpeta1/Altas 22-8-23/Regional Sur.xlsx")





muestra=c(247871,2994460,54678,1307782,609758,2962904)
###########################
persona= Base_resumen_extract %>% filter(ID_Cliente==muestra[1])
persona= persona %>% filter(!(Descripcion %in% c("ABONO CTA. AHORROS TRANSF. BCB", "Transf. de Fondos Seguros",
                                                 "Regularización ACH", "ACH ENVÍO RECHAZADO")) )
persona$Descripcion[persona$Descripcion=="BTS Transferencia ACH"]="ACH ENVÍO DÉBITO CA"
persona$Descripcion[persona$Descripcion=="BTS Transferencia ACH Express"]="ACH ENVÍO DÉBITO CA"
persona$Descripcion[persona$Descripcion=="DEPOSITO EFECTIVO BOLIVIANOS"]="DEPOSITO EFECTIVO"
persona$Descripcion[persona$Descripcion=="DEPOSITO EFECTIVO DOLARES"]="DEPOSITO EFECTIVO"
persona$Descripcion[persona$Descripcion=="DEP.CHEQUES OTROS BANCOS"]="DEPOSITO EFECTIVO"
persona$Descripcion[persona$Descripcion=="RETIRO EFECTIVO BOLIVIANOS"]="RETIRO EFECTIVO"
persona$Descripcion[persona$Descripcion=="RETIRO EFECTIVO DOLARES"]="RETIRO EFECTIVO"
persona$Descripcion[persona$Descripcion=="ACH Recibida (Express)"]="ACH RECIBIDA"
persona$Descripcion[persona$Descripcion=="BTS Transferencia 3ro Express"]="BTS Transferencia Ctas de 3ros"
persona$Descripcion[persona$Descripcion=="Transferencia a otras cuentas"]="BTS Transferencia Ctas de 3ros"
persona$Descripcion[persona$Descripcion=="Transferencias a 3ros Empresas"]="BTS Transferencia Ctas de 3ros"
persona= persona %>% filter(!(Descripcion %in% c("BTS Transferencia Ctas de 3ros", "BTS Transferencia Ctas propias")))
persona= persona %>% mutate(date= paste(año,mes,"1", sep = "-"))
persona$date= as.Date(persona$date)
persona= persona %>% filter(tipo_op=="ACH") %>% group_by(date) %>% summarise(tot_dep=sum(prom_dep_usd),to_ret= sum(prom_ret_usd),
                                                                                 cant_dep= sum(cant_dep),
                                                                                 cant_ret= sum(cant_ret))
##
comprim=persona %>% gather(key="Categoria", value="total", tot_dep:cant_ret)
comprim= comprim %>% filter(total!=0)
#
casos= PARA_R_LIM_PERS_NAT %>% filter(Nro_ID_Cliente==muestra[1])
caso_1= comprim %>% filter(Categoria=="tot_dep")
caso_1$Categoria[caso_1$Categoria=="tot_dep"]="Limite Total Depositos"
caso_1= caso_1 %>% mutate(total=casos$UCL_DEP[1])
#
caso_2= comprim %>% filter(Categoria=="to_ret")
caso_2$Categoria[caso_2$Categoria=="to_ret"]="Limite Total Retiros"
caso_2= caso_2 %>% mutate(total=casos$UCL_RET[1])
#
caso_3= comprim %>% filter(Categoria=="cant_dep")
caso_3$Categoria[caso_3$Categoria=="cant_dep"]="Limite Cantidad Depositos"
caso_3= caso_3 %>% mutate(total=casos$UCL_CANT_DEP[1])
#
caso_4= comprim %>% filter(Categoria=="cant_ret")
caso_4$Categoria[caso_4$Categoria=="cant_ret"]="Limite Cantidad Retiros"
caso_4= caso_4 %>% mutate(total=casos$UCL_CANT_RET[1])
#
unido= rbind(comprim, caso_1, caso_2, caso_3, caso_4)
unido$total= round(unido$total,0)
#
hchart(unido, "spline" , hcaes(x=date, y=total,group= Categoria),
       #stacking = list(enabled = TRUE),
       dataLabels = list(
         # enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_google())
### para ACH
persona= Base_resumen_extract %>% filter(ID_Cliente==muestra[1])
persona= persona %>% filter(!(Descripcion %in% c("ABONO CTA. AHORROS TRANSF. BCB", "Transf. de Fondos Seguros",
                                                 "Regularización ACH", "ACH ENVÍO RECHAZADO")) )
persona$Descripcion[persona$Descripcion=="BTS Transferencia ACH"]="ACH ENVÍO DÉBITO CA"
persona$Descripcion[persona$Descripcion=="BTS Transferencia ACH Express"]="ACH ENVÍO DÉBITO CA"
persona$Descripcion[persona$Descripcion=="DEPOSITO EFECTIVO BOLIVIANOS"]="DEPOSITO EFECTIVO"
persona$Descripcion[persona$Descripcion=="DEPOSITO EFECTIVO DOLARES"]="DEPOSITO EFECTIVO"
persona$Descripcion[persona$Descripcion=="DEP.CHEQUES OTROS BANCOS"]="DEPOSITO EFECTIVO"
persona$Descripcion[persona$Descripcion=="RETIRO EFECTIVO BOLIVIANOS"]="RETIRO EFECTIVO"
persona$Descripcion[persona$Descripcion=="RETIRO EFECTIVO DOLARES"]="RETIRO EFECTIVO"
persona$Descripcion[persona$Descripcion=="ACH Recibida (Express)"]="ACH RECIBIDA"
persona$Descripcion[persona$Descripcion=="BTS Transferencia 3ro Express"]="BTS Transferencia Ctas de 3ros"
persona$Descripcion[persona$Descripcion=="Transferencia a otras cuentas"]="BTS Transferencia Ctas de 3ros"
persona$Descripcion[persona$Descripcion=="Transferencias a 3ros Empresas"]="BTS Transferencia Ctas de 3ros"
persona= persona %>% filter(!(Descripcion %in% c("BTS Transferencia Ctas de 3ros", "BTS Transferencia Ctas propias")))
unique(persona$Descripcion)
persona= persona %>% mutate(date= paste(año,mes,"1", sep = "-"))
persona$date= as.Date(persona$date)
persona= persona %>% filter(tipo_op=="ACH") %>% group_by(date) %>% summarise(tot_dep=sum(prom_dep_usd),to_ret= sum(prom_ret_usd),
                                                                                 cant_dep= sum(cant_dep),
                                                                                 cant_ret= sum(cant_ret))
##
comprim=persona %>% gather(key="Categoria", value="total", tot_dep:cant_ret)
comprim= comprim %>% filter(total!=0)
#
casos= PARA_R_LIM_PERS_NAT %>% filter(Nro_ID_Cliente==muestra[1])
caso_1= comprim %>% filter(Categoria=="tot_dep")
caso_1$Categoria[caso_1$Categoria=="tot_dep"]="Limite Total Depositos"
caso_1= caso_1 %>% mutate(total=casos$UCL_REC_ACH[1])
#
caso_2= comprim %>% filter(Categoria=="to_ret")
caso_2$Categoria[caso_2$Categoria=="to_ret"]="Limite Total Retiros"
caso_2= caso_2 %>% mutate(total=casos$UCL_ENV_ACH[1])
#
caso_3= comprim %>% filter(Categoria=="cant_dep")
caso_3$Categoria[caso_3$Categoria=="cant_dep"]="Limite Cantidad Depositos"
caso_3= caso_3 %>% mutate(total=casos$UCL_CANT_REC_ACH[1])
#
caso_4= comprim %>% filter(Categoria=="cant_ret")
caso_4$Categoria[caso_4$Categoria=="cant_ret"]="Limite Cantidad Retiros"
caso_4= caso_4 %>% mutate(total=casos$UCL_CANT_RET_ACH[1])
#
unido= rbind(comprim, caso_1, caso_2, caso_3, caso_4)
unido$total= round(unido$total,0)
#
hchart(unido, "spline" , hcaes(x=date, y=total,group= Categoria),
       #stacking = list(enabled = TRUE),
       dataLabels = list(
         # enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_google())

#############
prueba= CASOS %>% count(gestion, rango_edad)
names(prueba)[2]="Edad"
names(prueba)[3]="Cantidad"
hchart(prueba, "column" ,hcaes(x=Edad, y=Cantidad,group= gestion),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())
##
prueba= CASOS %>% count(gestion, NOMBRE_REGIONAL, rango_edad)
names(prueba)[3]="Edad"
names(prueba)[4]="Cantidad"
prueba$gestion=as.numeric(prueba$gestion)
prueba$Edad=factor(prueba$Edad, levels = c("18 - 25","26 - 30","31 - 35","36 - 45","46 - 55",">55"))
#
ordena=NULL
for (i in 1:nrow(prueba)) {
  if(prueba$Edad[i]=="18 - 25")
    ordena[i]=1
  else if(prueba$Edad[i]=="26 - 30")
    ordena[i]=2
  else if(prueba$Edad[i]=="31 - 35")
    ordena[i]=3
  else if(prueba$Edad[i]=="36 - 45")
    ordena[i]=4
  else if(prueba$Edad[i]=="46 - 55")
    ordena[i]=5
  else if(prueba$Edad[i]==">55")
    ordena[i]=6

}
prueba= data.frame(prueba, ordena)
#
para_graf= prueba %>% filter(NOMBRE_REGIONAL=="CENTRO")
prueba_1= para_graf %>% group_by(ordena) %>% tally()
prueba_1= prueba_1 %>% filter(n==1)
prueba_2= para_graf %>% filter(ordena %in% prueba_1$ordena )
prueba_3= prueba_2
prueba_3$gestion[prueba_3$gestion=="2023"]="2022"
prueba_3$Cantidad[prueba_3$Cantidad!=0]=0
para_graf= rbind(para_graf, prueba_3)
para_graf= para_graf %>% arrange(ordena)
#
hchart(para_graf, "column" ,hcaes(x=Edad, y=Cantidad,group= gestion),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())
####
ros_por_sexo= CASOS  %>% count(gestion, Sexo)
ros_por_sexo$Sexo[is.na(ros_por_sexo$Sexo)]="Juridica"
ros_por_sexo$Sexo[ros_por_sexo$Sexo=="F"]="Femenino"
ros_por_sexo$Sexo[ros_por_sexo$Sexo=="M"]="Masculino"
hchart(ros_por_sexo, "column" ,hcaes(x=gestion, y=n,group= Sexo),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_google())
#
ros_por_sexo= CASOS %>% count(gestion, NOMBRE_REGIONAL,Sexo)
ros_por_sexo$Sexo[is.na(ros_por_sexo$Sexo)]="Juridica"
ros_por_sexo$Sexo[ros_por_sexo$Sexo=="F"]="Femenino"
ros_por_sexo$Sexo[ros_por_sexo$Sexo=="M"]="Masculino"
names(ros_por_sexo)[4]="Cantidad"
ros_por_sexo$gestion=as.numeric(ros_por_sexo$gestion)
hchart(ros_por_sexo %>% filter(NOMBRE_REGIONAL=="OCCIDENTE") %>% arrange(gestion), "column" ,hcaes(x=gestion, y=Cantidad,group= Sexo),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())
########
ros_por_delito= CASOS %>% count(Delito_precedente)
ros_por_delito= ros_por_delito %>% arrange(desc(n))
names(ros_por_delito)[1]="Delito"
names(ros_por_delito)[2]="Cantidad"
hchart(ros_por_delito , "bar" ,hcaes(x=Delito, y=Cantidad),
        #stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())
########
ros_por_dep= CASOS %>% count(Regional_nombre)
hchart(ros_por_dep, "treemap", hcaes(x = Regional_nombre, value = n))
##########
ros_por_gest= CASOS %>% count(gestion)
ros_por_gest= data.frame(ros_por_gest, Reporte="ROS")
#
hchart(ros_por_gest , "bar" ,hcaes(x=gestion, y=n),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())
############
hchart(ros_por_gest , "bar" ,hcaes(x=gestion, y=n, group=Reporte),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         , # Añadir etiquetas
         format = "{point.percentage:.0f}%"
       ))%>% hc_add_theme(hc_theme_538())
######### GAUGES ####################
library(c3)
data.frame(Riesgo=35, alt=75) %>%
  c3() %>%
  c3_gauge(title = 'Colours' ,values=c(30,
                                       60, 90, 100),pattern = c("#60B044", "#FDE725FF", "#F97600","#FF0000"
                                                                ))
################ grafico de DONA ##############
hchart(ros_por_gest , "pie",innerSize = 200, hcaes(x = gestion, y = n, group=Reporte),
      # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         , # Añadir etiquetas
         format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
       # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())
hchart(ros_por_gest, "treemap", hcaes(x = gestion, value = n, color=n))
###################
reporte_inusualidades= BASE_INUSUALIDADES %>% filter(FECHA_DE_EMISION_DEL_INFORME>=as.Date("2022-1-1"))
reporte_inusualidades= reporte_inusualidades %>% filter(!is.na(ROS))
reporte_inusualidades= reporte_inusualidades %>% mutate(Fecha=paste(substr(FECHA_DE_EMISION_DEL_INFORME,1,7),"1", sep = "-"))
reporte_inusualidades$Fecha= as.Date(reporte_inusualidades$Fecha)
reporte_inusualidades_1= reporte_inusualidades %>% count(Fecha, ROS)
names(reporte_inusualidades_1)[2]="Tipo_Informe"
reporte_inusualidades_1$Tipo_Informe[reporte_inusualidades_1$Tipo_Informe=="SI"]="ROS"
reporte_inusualidades_1$Tipo_Informe[reporte_inusualidades_1$Tipo_Informe=="NO"]="INUSUALIDAD"
names(reporte_inusualidades_1)[3]="Cantidad"
reporte_inusualidades_2= reporte_inusualidades_1 %>% group_by(Fecha) %>% summarise(Cantidad= sum(Cantidad))
reporte_inusualidades_2= data.frame(reporte_inusualidades_2[,1],Tipo_Informe="TOTAL REPORTES",reporte_inusualidades_2[,2])
reporte_inusualidades_1= rbind(reporte_inusualidades_1, reporte_inusualidades_2)
#
hchart(reporte_inusualidades_1, "spline" , hcaes(x=Fecha, y=Cantidad,group= Tipo_Informe),
       #stacking = list(enabled = TRUE),
       dataLabels = list(
          enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_google()) %>%  hc_rangeSelector(enabled= TRUE, verticalAlign = "top")
#########
rep_inus_med_id= BASE_INUSUALIDADES %>% filter(FECHA_DE_EMISION_DEL_INFORME>=as.Date("2022-1-1"))
rep_inus_med_id= rep_inus_med_id %>% filter(!is.na(ROS))
rep_inus_med_id_1= rep_inus_med_id %>% count(MEDIO_DE_IDENTIFICACION_DEL_CLIENTE)
rep_inus_med_id_1= rep_inus_med_id_1 %>% arrange(desc(n))
rep_inus_med_id_1= rep_inus_med_id_1 %>% mutate(corre=1:nrow(rep_inus_med_id_1))
rep_inus_med_id_11= rep_inus_med_id %>% count(MEDIO_DE_IDENTIFICACION_DEL_CLIENTE, ROS) %>% group_by(MEDIO_DE_IDENTIFICACION_DEL_CLIENTE) %>% tally()
rep_inus_med_id_2=rep_inus_med_id %>% count(MEDIO_DE_IDENTIFICACION_DEL_CLIENTE, ROS)
rep_inus_med_id_2= rep_inus_med_id_2 %>% left_join(rep_inus_med_id_1[,c(1,3)], by="MEDIO_DE_IDENTIFICACION_DEL_CLIENTE")
rep_inus_med_id_11= rep_inus_med_id_11 %>% filter(n==1)
rep_inus_med_id_11= rep_inus_med_id_2 %>% filter(MEDIO_DE_IDENTIFICACION_DEL_CLIENTE %in% unique(rep_inus_med_id_11$MEDIO_DE_IDENTIFICACION_DEL_CLIENTE) )
rep_inus_med_id_11$n=0
rep_inus_med_id_11$ROS[rep_inus_med_id_11$ROS=="SI"]="NO"
rep_inus_med_id_2= rbind(rep_inus_med_id_2, rep_inus_med_id_11)
rep_inus_med_id_2= rep_inus_med_id_2 %>% arrange(corre)
names(rep_inus_med_id_2)[2] = "Tipo_Informe"
names(rep_inus_med_id_2)[3] = "Cantidad"
rep_inus_med_id_2$Tipo_Informe[rep_inus_med_id_2$Tipo_Informe=="NO"]="INUSUALIDAD"
rep_inus_med_id_2$Tipo_Informe[rep_inus_med_id_2$Tipo_Informe=="SI"]="ROS"
hchart(rep_inus_med_id_2, "bar" , hcaes(x=MEDIO_DE_IDENTIFICACION_DEL_CLIENTE, y=Cantidad, group=Tipo_Informe),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_google())
###########################
rep_ROS_reg= CASOS %>% count(NOMBRE_REGIONAL)
rep_ROS_reg= rep_ROS_reg %>% arrange(desc(n))
rep_ROS_reg= data.frame(rep_ROS_reg,Reporte="ROS")
names(rep_ROS_reg)[2]="Cantidad"
hchart(rep_ROS_reg, "bar" , hcaes(x=NOMBRE_REGIONAL, y=Cantidad, group=Reporte),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_elementary())

############
repor_find= Base_Alertas_1 %>% filter(Fecha_de_generacion>=as.Date("2023-1-1"))
repor_find_1=repor_find %>% count(Categoria_actual)
repor_find_1= data.frame(repor_find_1, Tipo="Alertas")
hchart(repor_find_1 , "pie",
       innerSize = 200,
       hcaes(x = Categoria_actual, y = n, group=Tipo),
        stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         , # Añadir etiquetas
         format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.1f}%"
       )) %>% hc_add_theme(hc_theme_google()) %>% hc_tooltip(useHTML = TRUE,
                                                             style = list(fontSize = "10px"),
                                                             headerFormat = "",
         pointFormat = "<div style='text-align: center;'> <b>{point.name}</b><br> Total: {point.total}</div>"
         ,
         positioner = JS(
           "function () {

        /* IMPORTANT! */
        xp =  this.chart.chartWidth/2 - this.label.width/2
        yp =  this.chart.chartHeight/2 - this.label.height/2

        return { x: xp, y: yp };

      }"
         )
         )
###############
repor_find= Base_Alertas_1 %>% filter(Fecha_de_generacion>=as.Date("2023-1-1"))
repor_find$Flujo_de_alerta= str_replace(repor_find$Flujo_de_alerta, " - ", "-")
repor_find= repor_find %>% mutate(aux= Flujo_de_alerta)
repor_find= repor_find %>% separate(aux, c("X","REGIONAL"), sep = "-")
repor_find= repor_find[,-39]
repor_find$REGIONAL[repor_find$REGIONAL=="Centro"]="CENTRO"
repor_find$REGIONAL[repor_find$REGIONAL=="El Alto"]="EL ALTO"
repor_find$REGIONAL[repor_find$REGIONAL=="Oriente"]="ORIENTE"
repor_find$REGIONAL[repor_find$REGIONAL=="Sur"]="SUR"
repor_find$REGIONAL[repor_find$REGIONAL=="Occidente"]="OCCIDENTE"
repor_find= repor_find %>% mutate(dias_cierre= as.numeric(Fecha_de_cambio_categoria- Fecha_de_generacion))
cat_dias= NULL
for (i in 1:nrow(repor_find)) {
  if(repor_find$dias_cierre[i]<=15)
    cat_dias[i]="1 - 15"
  else if(repor_find$dias_cierre[i]>15 & repor_find$dias_cierre[i]<=30)
    cat_dias[i]="16 - 30"
  else if(repor_find$dias_cierre[i]>30 & repor_find$dias_cierre[i]<=45)
    cat_dias[i]="31 - 45"
  else if(repor_find$dias_cierre[i]>45 & repor_find$dias_cierre[i]<=60)
    cat_dias[i]="46 - 60"
  else if(repor_find$dias_cierre[i]>60 & repor_find$dias_cierre[i]<=90)
    cat_dias[i]="61 - 90"
  else if(repor_find$dias_cierre[i]>90)
    cat_dias[i]="> 90"
}
repor_find= data.frame(repor_find, cat_dias)
#
repor_find_1=repor_find %>% count(Categoria_actual)
repor_find_1= data.frame(repor_find_1, Tipo="Alertas")
hchart(repor_find_1 , "pie",
       innerSize = 200,
       hcaes(x = Categoria_actual, y = n, group=Tipo),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         , # Añadir etiquetas
         format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.1f}%"
       )) %>% hc_add_theme(hc_theme_google())
###############


#
repor_find %>% count(Usuario_de_finalizacion, cat_dias)
repor_find_2= repor_find %>% filter(Categoria_actual=="Finalizada" &
                                      REGIONAL=="Occidente") %>%  count(Usuario_de_finalizacion, cat_dias)
#
hchart(repor_find_2, "bar" , hcaes(x=cat_dias, y=n, group=Usuario_de_finalizacion),
       #stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_ffx())
####
repor_find$Flujo_de_alerta= str_replace(repor_find$Flujo_de_alerta, " - ", "-")
repor_find= repor_find %>% mutate(aux= Flujo_de_alerta)
repor_find= repor_find %>% separate(aux, c("X","REGIONAL"), sep = "-")
#
repor_find_3= repor_find %>% filter(Categoria_actual=="Finalizada") %>% count(cat_dias)
repor_find_3= data.frame(repor_find_3, Tipo="Alertas")
names(repor_find_3)[1]="Días"
hchart(repor_find_3, "bar" , hcaes(x=Días, y=n, group=Tipo),
       #stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_elementary()) %>% hc_subtitle(text="Cierre de alertas en días")
##########################
repor_find= repor_find %>% mutate(ESTADO= ifelse(Categoria_actual=="Finalizada","Finalizada","Pendiente"))
repor_find_3= repor_find %>% count(TIPO,ESTADO)
names(repor_find_3)[3]="Cantidad"
hchart(repor_find_3, "bar" , hcaes(x=ESTADO, y=Cantidad, group=TIPO),
       #stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_elementary()) %>% hc_subtitle(text="Cierre de alertas en días")
##############
repor_find_4= repor_find %>% group_by(Nro_ID_Cliente) %>% tally()
cantidad_alertas=NULL
for (i in 1:nrow(repor_find_4)) {
  if(repor_find_4$n[i] <=5)
    cantidad_alertas[i]="1 - 5"
  else if(repor_find_4$n[i]>5 & repor_find_4$n[i]<=10)
    cantidad_alertas[i]="6 - 10"
  else if(repor_find_4$n[i]>10 & repor_find_4$n[i]<=20)
    cantidad_alertas[i]="11 - 20"
  else if(repor_find_4$n[i]>20 & repor_find_4$n[i]<=30)
    cantidad_alertas[i]="21 - 30"
  else if(repor_find_4$n[i]>30 & repor_find_4$n[i]<=40)
    cantidad_alertas[i]="31 - 40"
  else if(repor_find_4$n[i]>40 & repor_find_4$n[i]<=50)
    cantidad_alertas[i]="41 - 50"
  else if(repor_find_4$n[i]>50 & repor_find_4$n[i]<=80)
    cantidad_alertas[i]="51 - 80"
  else if(repor_find_4$n[i]>80 & repor_find_4$n[i]<=100)
    cantidad_alertas[i]="81 - 100"
  else if(repor_find_4$n[i]>100)
    cantidad_alertas[i]="> 100"
}
repor_find_4= data.frame(repor_find_4, cantidad_alertas)
repor_find_4= repor_find_4 %>% count(cantidad_alertas)
ordena=NULL
for (i in 1:nrow(repor_find_4)) {
  if(repor_find_4$cantidad_alertas[i]=="1 - 5")
    ordena[i]=1
  else if(repor_find_4$cantidad_alertas[i]=="6 - 10")
    ordena[i]=2
  else if(repor_find_4$cantidad_alertas[i]=="11 - 20")
    ordena[i]=3
  else if(repor_find_4$cantidad_alertas[i]=="21 - 30")
    ordena[i]=4
  else if(repor_find_4$cantidad_alertas[i]=="31 - 40")
    ordena[i]=5
  else if(repor_find_4$cantidad_alertas[i]=="41 - 50")
    ordena[i]=6
  else if(repor_find_4$cantidad_alertas[i]=="51 - 80")
    ordena[i]=7
  else if(repor_find_4$cantidad_alertas[i]=="81 - 100")
    ordena[i]=8
  else if(repor_find_4$cantidad_alertas[i]=="> 100")
    ordena[i]=9
}
repor_find_4= data.frame(repor_find_4, ordena)
repor_find_4= repor_find_4 %>% arrange(ordena)
repor_find_4= repor_find_4[,1:2]
repor_find_4= data.frame(repor_find_4, Reporte="Clientes")
names(repor_find_4)[2]="Cantidad"
#
hchart(repor_find_4, "bar" , hcaes(x=cantidad_alertas, y=Cantidad, group=Reporte),
       #stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))  %>% hc_add_theme(hc_theme_elementary()) %>% hc_subtitle(text="Cierre de alertas en días")
################
repor_find_5= repor_find %>% filter(!is.na(FECHA_NACIMIENTO) & !is.na(GENERO_TITULAR))
repor_find_5= repor_find_5 %>% mutate(Edad= round(as.numeric(Fecha_de_generacion- FECHA_NACIMIENTO)/365.25,0) )
#
cat_edad= NULL
for (i in 1:nrow(repor_find_5)) {
  if(repor_find_5$Edad[i]<=25)
    cat_edad[i]="18 - 25"
  else if(repor_find_5$Edad[i]>25 & repor_find_5$Edad[i]<=30)
  cat_edad[i]="26 - 30"
  else if(repor_find_5$Edad[i]>30 & repor_find_5$Edad[i]<=40)
    cat_edad[i]="31 - 40"
  else if(repor_find_5$Edad[i]>40 & repor_find_5$Edad[i]<=50)
    cat_edad[i]="41 - 50"
  else if(repor_find_5$Edad[i]>50 & repor_find_5$Edad[i]<=60)
    cat_edad[i]="51 - 60"
  else if(repor_find_5$Edad[i]>60)
    cat_edad[i]="> 60"
}
repor_find_5= data.frame(repor_find_5, cat_edad)
repor_find_51= repor_find_5 %>% count(cat_edad, GENERO_TITULAR)
names(repor_find_51)[1]="Edad"
names(repor_find_51)[3]="Cantidad"
repor_find_51$GENERO_TITULAR[repor_find_51$GENERO_TITULAR=="F"]="Femenino"
repor_find_51$GENERO_TITULAR[repor_find_51$GENERO_TITULAR=="M"]="Masculino"
hchart(repor_find_51, "bar" , hcaes(x=Edad, y=Cantidad, group=GENERO_TITULAR),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         , # Añadir etiquetas
         format = "{point.percentage:.1f}%"
       ))  %>% hc_add_theme(hc_theme_elementary()) %>% hc_subtitle(text="Alertas generadas por edad y género")
##############
repor_find_6= repor_find %>% count(fecha,TIPO, ESTADO)
repor_find_6= repor_find_6 %>% mutate(Categoria=paste(TIPO, ESTADO))
repor_find_6= repor_find_6[,-c(2,3)]
repor_find_61= repor_find_6 %>% count(fecha, wt=n)
repor_find_61= data.frame(repor_find_61,Categoria="TOTAL ALERTAS")
repor_find_61$fecha= repor_find_61$fecha+1
unido=NULL
for (i in 1:length(unique(repor_find_6$Categoria))) {
  unido1= repor_find_6 %>% filter(Categoria== unique(repor_find_6$Categoria)[i])
  unido1$fecha= unido1$fecha+i
  unido=rbind(unido, unido1)
}
repor_find_6= unido
repor_find_6= rbind(repor_find_6, repor_find_61)
#
hchart(repor_find_6, "spline" , hcaes(x=fecha, y=n, group=Categoria),
      # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.1f}%"
       ))  %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_colors(c("blue","#F8850FFF","red","green","maroon")) %>%
  hc_subtitle(text="Estado de Alertas FINDER GC")

################# CONVERSOR DE NUMERO A HORA EN EXCEL #############
#=--TEXTO(F13;"0\:00\:00")
repor_find_7= repor_find %>% count(Caracteristica_1, Caracteristica_2)
repor_find_77= repor_find %>% count(Caracteristica_2) %>% arrange(desc(n))
repor_find_77= repor_find_77 %>% mutate(ordena=1:nrow(repor_find_77))
repor_find_777= repor_find_7 %>% group_by(Caracteristica_2) %>% tally() %>% filter(n==1)
repor_find_777= repor_find_7 %>% filter(Caracteristica_2 %in% repor_find_777$Caracteristica_2)
repor_find_777$n=0
repor_find_777$Caracteristica_1[repor_find_777$Caracteristica_1=="Cantidad de Operaciones Mensuales"]="x"
repor_find_777$Caracteristica_1[repor_find_777$Caracteristica_1=="Importe Maximo por operacion"]="Cantidad de Operaciones Mensuales"
repor_find_777$Caracteristica_1[repor_find_777$Caracteristica_1=="x"]="Importe Maximo por operacion"
repor_find_7= rbind(repor_find_7, repor_find_777)
repor_find_7= repor_find_7 %>% mutate(corre= 1:nrow(repor_find_7))
repor_find_7= repor_find_7 %>% left_join(repor_find_77[,c(1,3)], by=c("Caracteristica_2"))
repor_find_7= repor_find_7 %>% arrange(ordena)
repor_find_7= repor_find_7[,1:3]
#
hchart(repor_find_7, "bar" , hcaes(x=Caracteristica_2, y=n, group=Caracteristica_1),
        stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
        # , # Añadir etiquetas
        # format = "{point.percentage:.0f}%"
       ))  %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_colors(c("blue","#F8850FFF","red","green","maroon")) %>%
  hc_subtitle(text="Alertas FINDER GC por tipo de transacción")
########

persona1= Base_Alertas_1 %>% filter(Nro_de_identificacion=="15619701")
persona2= Base_resumen_extract %>% filter(NUMERO_DOCUMENTO=="15619701")
#
datos= Base_resumen_extract %>% filter(año==2023 & mes==5
                                         &
                                         cant_dep>=20
                                         & Descripcion=="DEPOSITO EFECTIVO BOLIVIANOS"
                                         & cant_dep<30
                                         )
datos1= datos %>% filter(!(ID_Cliente %in% Base_Alertas_1$Nro_ID_Cliente))
datos1= datos1 %>% filter(Caracteristica_1=="Cantidad de Operaciones Mensuales")
#
datos1= Base_Alertas_1 %>% filter(Nro_de_identificacion %in% CASOS$CI)
hchart(datos1$Valor_operado)
#
Base_resumen_extract %>% filter(año==2023 &
                                  mes==5 &
                                  cant_dep>0 & cant_dep<=30) %>% nrow/(Base_resumen_extract %>% filter(año==2023 & mes==5 & Categoria_dep_usd!=99) %>% nrow)
#
round(Base_resumen_extract %>% filter(año==2023 &
                                        mes==5 &
                                        cant_dep>0 & cant_dep<=30) %>% nrow/(Base_resumen_extract %>% filter(año==2023 & mes==5 & Categoria_dep_usd!=99) %>% nrow),3)
#
hchart(datos$cant_dep)
#
datos1= datos %>% filter(!(ID_Cliente %in% Base_Alertas_1$Nro_ID_Cliente))
###
categoria= NULL
for (i in 1:nrow(datos)) {
  if(datos$cant_dep[i]<=5)
    categoria[i]=1
  else if(datos$cant_dep[i]>5 & datos$cant_dep[i]<=10)
    categoria[i]=2
  else if(datos$cant_dep[i]>10 & datos$cant_dep[i]<=20)
    categoria[i]=3
  else if(datos$cant_dep[i]>20 & datos$cant_dep[i]<=25)
    categoria[i]=4
  else if(datos$cant_dep[i]>25 & datos$cant_dep[i]<=30)
    categoria[i]=5
  else if(datos$cant_dep[i]>30 & datos$cant_dep[i]<=40)
    categoria[i]=6
  else if(datos$cant_dep[i]>40 & datos$cant_dep[i]<=50)
    categoria[i]=7
  else if(datos$cant_dep[i]>50)
    categoria[i]=8
}
datos= data.frame(datos, categoria)
hchart(datos$categoria)

###
casos_analisis= Base_resumen_extract %>% filter(año==2023) %>% group_by(ID_Cliente) %>% summarise(total= sum(tot_dep)-sum(to_ret))
casos_analisis_1= casos_analisis %>% filter(abs(total)<50)
casos_analisis_2= Base_resumen_extract %>% filter(ID_Cliente %in% unique(casos_analisis_1$ID_Cliente))
casos_analisis_2= casos_analisis_2 %>% filter(!(ID_Cliente %in% Base_Alertas_1$Nro_ID_Cliente) )
##
datos_cli= Base_Clientes_BSol_vf %>% filter(CLASIFICACION %in% c("C","T"))
####### PARA EL FILTRADO DE UNA FILA EN ESPECIFICO ###########
# datos_train[str_detect(datos_train$corre,"285237"),]
#FILTRADO DATOS TRANSACCIONES MES
###
load("Base_Clientes_BSol_vf.RData")
load("BASE_INUSUALIDADES.RData")
load("Base_Alertas_1.RData")
load("Base_resumen_extract.RData")
inus_ros= BASE_INUSUALIDADES %>% filter(ROS=="SI" & año>=2022 & TIPO_DE_PERSONA=="NATURAL" & MEDIO_DE_IDENTIFICACION_DEL_CLIENTE=="ALERTAS FINDER")
inus_ros= inus_ros %>% left_join(Base_Clientes_BSol_vf[,c(4,11)], by=c("NUMERO_DE_DOCUMENTO"="NUMERO_DOCUMENTO"))
inus_ros= inus_ros %>% filter(!is.na(USERID_TITULAR))
ros_datos= Base_Alertas_1 %>% filter(Nro_ID_Cliente %in% inus_ros$USERID_TITULAR)
ros_datos_1= ros_datos %>% filter(Caracteristica_1=="Cantidad de Operaciones Mensuales")
summary(ros_datos_1$Valor_operado)[[2]]
ros_datos_2= ros_datos %>% filter(Caracteristica_1=="Importe Maximo por operacion" & Valor_esperado>500)
summary(ros_datos_2$Valor_operado*6.86)[[2]]
datos_transac= Base_resumen_extract %>% filter(año>=2022)
#FILTRADO CLIENTES QUE REALIZARON TRANSAC EN EL MES DE FILTRO
datos_cli= Base_Clientes_BSol_vf %>% filter(USERID_TITULAR %in% datos_transac$ID_Cliente)
datos_cli= datos_cli %>% filter(!is.na(FECHA_NACIMIENTO) & FECHA_NACIMIENTO!=as.Date("1900-1-1"))
datos_cli= datos_cli %>% mutate(Edad= round(as.numeric((as.Date("2023-5-31")-FECHA_NACIMIENTO)/365.25),0))
hchart(datos_cli$Edad)
datos_cli= datos_cli %>% filter(Edad>=18 & Edad<=90)
datos_cli_1= datos_cli %>% select(USERID_TITULAR , NUMERO_DOCUMENTO,GENERO_TITULAR, Edad, NOMBRE_REGIONAL,
                                  DEPARTAMENTO, ESTADO_CIVIL, PROFESION_TITULAR)
datos_transac_1= datos_transac[,c(1,26,2,5,10,14,15,16,21)]
datos_transac_1= datos_transac_1 %>% mutate(corre=1:nrow(datos_transac_1))
datos_transac_1= datos_transac_1 %>% left_join(datos_cli_1, by=c("ID_Cliente"="USERID_TITULAR","NUMERO_DOCUMENTO"="NUMERO_DOCUMENTO"))
datos_transac_1= datos_transac_1 %>% filter(Categoria_dep_usd!=99)
datos_transac_1= datos_transac_1 %>% left_join(inus_ros[,c(15,21)], by=c("ID_Cliente"="USERID_TITULAR"))
##
#datos_transac_1= datos_transac_1 %>% mutate(ROS= ifelse(!is.na(ACTIVOS),1,0))
datos_transac_1= datos_transac_1 %>% filter(!is.na(Edad))
length(unique(datos_transac_1$corre))
####
datos_sobre_1= Base_Alertas_1 %>% filter(Fecha_de_generacion>=as.Date("2022-1-1")) %>% group_by(Nro_ID_Cliente) %>% tally()
names(datos_sobre_1)[2]="libera_rec"
datos_sobre_1= datos_sobre_1 %>% filter(libera_rec>=4)
####
datos_transac_1= datos_transac_1 %>% mutate(libera= ifelse(ID_Cliente %in% datos_sobre_1$Nro_ID_Cliente,1,0))
datos_transac_1= datos_transac_1 %>% left_join(datos_sobre_1, by=c("ID_Cliente"="Nro_ID_Cliente"))
datos_transac_1$libera_rec[is.na(datos_transac_1$libera_rec)]=0
datos_transac_1= datos_transac_1 %>% left_join(Base_Clientes_BSol_vf[,c(11,29)], by=c("ID_Cliente"="USERID_TITULAR"))
datos_transac_1= datos_transac_1 %>% group_by(corre) %>% filter(row_number()==1)
datos_transac_1$Meses <- interval(datos_transac_1$F_ALTA_CLIENTE,  as.Date("2023-5-31")) %/% months(1)
#datos_transac_1= datos_transac_1 %>% mutate(libera= ifelse(ID_Cliente %in% datos_sobre_1$Nro_ID_Cliente,2,libera ))
datos_transac_1= datos_transac_1 %>% mutate(ROS=
                                              ifelse(((
                                                cant_dep>summary(ros_datos_1$Valor_operado)[[2]]|
                                                       tot_dep> summary(ros_datos_2$Valor_operado*6.86)[[1]])
                                                       & libera_rec==0 & Meses<=20) |!is.na(ACTIVOS),1,0))

#grafico
#
persona= datos_transac_1 %>% filter(cant_dep>30)
persona= persona[,-17]
persona$ROS= as.factor(persona$ROS)
persona$libera= as.factor(persona$libera)
persona$GENERO_TITULAR=as.factor(persona$GENERO_TITULAR)
persona$F_ALTA_CLIENTE=as.Date(persona$F_ALTA_CLIENTE)
persona$NOMBRE_REGIONAL= as.factor(persona$NOMBRE_REGIONAL)
summary(persona$F_ALTA_CLIENTE)
persona= as.data.frame(persona)
#
sum(is.na(datos_cli$GENERO_TITULAR))
sum(is.na(datos_cli$Edad))
sum(is.na(datos_cli$DEPARTAMENTO))
sum(is.na(datos_cli$NOMBRE_REGIONAL))
sum(is.na(datos_cli$ESTADO_CIVIL))
#######

modelo=glm(ROS~Edad+cant_dep+tot_dep+Meses+libera_rec
             , data = persona, family = "binomial")
summary(modelo)
#
library(tidyverse)
library(highcharter)
library(brglm)
library(ResourceSelection)
library(pROC)
library(gmodels)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
######
######
modelo1= brglm(ROS~Edad+tot_dep+cant_dep+Meses+libera_rec, data = persona[train,] , family=binomial(link = "logit"))
#modelo1= brglm(ROS~cant_dep+tot_dep+libera+Meses, data = persona, family=binomial(link = "logit"))
summary(modelo1)
#
predicciones <- ifelse(test = modelo1$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(predicciones, modelo1$model$ROS,
                          dnn = c("predicciones", "observaciones"))
matriz_confusion
confusionMatrix(data=as.factor(predicciones), reference = as.factor(modelo1$model$ROS))
CrossTable(as.factor(modelo1$model$ROS), as.factor(predicciones))

#### PARA QUE LOS FORMATOS DE NUMEROS NO SE MUESTREN EN FORMAT CIENTIFICO #################
#options(scipen = 999)
datos_train$Prob_pred <- round(predict(modelo1, newdata = persona[-train,], "response"),3)
predicciones <- ifelse(test = datos_train$Prob_pred > 0.5, yes = 1, no = 0)
table(datos_train$ROS,predicciones,
      dnn = c("observaciones","predicciones"))
#
confusionMatrix(data=as.factor(predicciones), reference = as.factor(modelo1$model$ROS))
#
roc_graf=roc(as.factor(persona[train,]$ROS), modelo1$fitted.values, plot=TRUE, legacy.axes = TRUE,
             percent = TRUE, ylab = "Sensitividad \n (Porcentaje de verdaderos positivos)",
             xlab = "1- especificidad \n (Porcentaje de falsos positivos)", col = "darkblue", lwd = 2,
             print.auc = TRUE, auc.polygon = TRUE, auc.polygon.col = "aliceblue", grid=TRUE)
#
hoslem.test(modelo1$model$ROS, fitted(modelo1))
step(modelo1, direction = "backward")
#
options(scipen = 999) #Para evitar que mis datos me salgan en notación
#científica
# RANDOM FOREST
set.seed(2023)
#
train <- createDataPartition(y = persona$ROS, p = 0.7, list = FALSE)
modelo_r= randomForest(x= persona[train,c(12,4,5,20,18)],
                       y= persona[train,21],
                       ntree=500,
                       keep.forest=TRUE)
##
datos_train= persona[-train,]
pred= predict(modelo_r, persona[-train,], type = "class")
table(persona[-train,"ROS"], pred, dnn = c("Actual","Predicciones"))
#
probs= predict(modelo_r,persona[-train,], type = "prob")
head(probs)
#################
datos_train= data.frame(datos_train, probs)
datos_train= datos_train %>% mutate(prob_may= ifelse(Prob_pred>X1, Prob_pred, X1))
########################
# ARBOL ALEATORIO
datos_train= persona[train,]
casos_train <- createDataPartition(y = datos_mod$target, p = 0.7, list = FALSE)
modelo2= rpart(target~Valor_operado+GENERO_TITULAR+ESTADO_CIVIL+
                 NOMBRE_REGIONAL+Antiguedad_Meses+Edad, data= datos_mod[casos_train,],
               method = "class",
               control = rpart.control(minsplit = 20,cp=0.01))
#modelo2
# extra = 101 para numeros enteros
# extra = 104 para porcentaje de los numeros
#prp(modelo2, type = 2, extra = 101, nn=TRUE, fallen.leaves = TRUE, faclen = 4,
#    varlen = 8, shadow.col = "gray")
#
rpart.plot(modelo2,
              digits=-3,
              type=2,
              extra=101, cex=0.7, nn=TRUE
              )

prediccion= predict(modelo2, newdata= datos_mod[-casos_train,], type="class")
mt_conf=confusionMatrix(prediccion, datos_mod[-casos_train,"target"])
#####
Edad=25
cant_dep=433
tot_dep=500000
libera_rec=5
Meses=0
round(1/(1+ exp(-(modelo1$coefficients[1]+(modelo1$coefficients[2])*Edad+(modelo1$coefficients[3])*tot_dep+
                    (modelo1$coefficients[4])*cant_dep+ (modelo1$coefficients[5])*Meses
                   +(modelo1$coefficients[6])*libera_rec

))),4)

############## MODELO LOGIT EVENTOS RAROS ############
modelo1= brglm(ROS~Edad+tot_dep+cant_dep+Meses+libera_rec, data = persona, family=binomial(link = "logit"))
summary(modelo1)
#
predicciones <- ifelse(test = modelo1$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(predicciones, modelo1$model$ROS,
                          dnn = c("predicciones", "observaciones"))
matriz_confusion
confusionMatrix(data=as.factor(predicciones), reference = as.factor(modelo1$model$ROS))
CrossTable(as.factor(modelo1$model$ROS), as.factor(predicciones))
#######################
Edad=30
tot_dep=55000
cant_dep=90
Meses=2
libera_rec=1
prueba=data.frame(Edad, tot_dep, cant_dep, Meses, libera_rec)
prediccion= predict(modelo2, newdata= prueba, type="class")
probs= predict(modelo2,prueba, type = "prob")
signif(probs[2]*100,4)
####
library(UBL)
library(caret)
library(skimr)
################
# load("persona.RData")
# persona$ROS=as.numeric(persona$ROS)
# persona$ROS[persona$ROS==2]="SI"
# persona$ROS[persona$ROS==1]="NO"
# persona$ROS=as.factor(persona$ROS)
# persona$ROS= factor(as.character(persona$ROS), levels=rev(levels(persona$ROS)))
# table(persona$ROS)
# datos_mod= persona[,c(12,4,5,20,18,21)]
#################### METODOLOGIA COMPLETA DE ESTIMACION RANDOM FOREST ##########################
load("datos_analisis.RData")
datos_analisis= Alertas
datos_analisis= datos_analisis %>% mutate(corre=1:nrow(datos_analisis))
datos_analisis= datos_analisis %>% left_join(Base_Clientes_BSol_vf[,c(1,10,11,13,14,15,17,29,34,35)],
                                             by=c("Nro_ID_Cliente"="USERID_TITULAR"))
datos_analisis= datos_analisis %>% group_by(corre) %>% filter(row_number()==1)
#save(datos_analisis, file = "datos_analisis.RData")
datos_analisis= datos_analisis %>% left_join(BASE_INUSUALIDADES[,c(17:19,21)],
                                             by=c("Nro_ID_Cliente"="USERID_TITULAR"))
datos_analisis= datos_analisis %>% group_by(corre) %>% filter(row_number()==1)
datos_analisis= datos_analisis %>% mutate(target= ifelse(ROS=="NO"| is.na(ROS), "NO", "SI"))
datos_analisis$Antiguedad_Meses <- interval(datos_analisis$F_ALTA_CLIENTE,  as.Date("2023-9-30")) %/% months(1)
datos_analisis= datos_analisis %>% mutate(Edad= round(as.numeric((as.Date("2023-9-30")-FECHA_NACIMIENTO)/365.25),0))
datos_analisis= as.data.frame(datos_analisis)
#
datos_prueba= datos_analisis %>% filter(target=="SI")
#
orden_evalua= datos_prueba %>% count(Caracteristica_1, Caracteristica_2)  %>% arrange(desc(n))
#
prueba= datos_analisis %>% filter(Caracteristica_1==orden_evalua$Caracteristica_1[1] &
                                    Caracteristica_2== orden_evalua$Caracteristica_2[1]
                                  #& TIPO_PERSONA=="NATURAL"
)
prueba1= prueba %>% filter(Valor_esperado<=500)
prueba1= prueba1 %>% filter(!is.na(Edad))
prueba1= prueba1 %>% filter(Categoria_actual=="Finalizada")
skim(prueba1)
# columna 17 valor operado
datos_mod= prueba1[,c(1,36,37,30,27,24,35)]
datos_mod$target= as.factor(datos_mod$target)
datos_mod$target= factor(datos_mod$target, levels = rev(levels(datos_mod$target)))
table(datos_mod$target)
#
datos_mod= datos_mod %>% mutate(corre=1:nrow(datos_mod))
datos= datos_analisis %>% filter(Categoria_actual=="Finalizada") %>%  group_by(Nro_ID_Cliente) %>% tally()
datos_mod= datos_mod %>% left_join(datos, by="Nro_ID_Cliente")
names(datos_mod)[9]="Cant_Cierre_Alertas"
datos_mod= datos_mod[,-c(8,1)]
datos_mod= datos_mod[,c(7,1:6)]
datos_mod$GENERO_TITULAR= as.factor(datos_mod$GENERO_TITULAR)
datos_mod$ESTADO_CIVIL= as.factor(datos_mod$ESTADO_CIVIL)
datos_mod$NOMBRE_REGIONAL=as.factor(datos_mod$NOMBRE_REGIONAL)
datos_mod= na.omit(datos_mod)
#####
skim(datos_mod)
##
n=nrow(datos_mod)
indices=1:n
ient=sample(indices,floor(n*0.6))
ival=sample(setdiff(indices,ient),floor(n*0.15))
itest=setdiff(indices,union(ient,ival))
training = datos_mod[ient,]
validation = datos_mod[ival,]
testing = datos_mod[itest,]
training_valid=rbind(training,validation)
dim(training)
dim(training_valid)
dim(testing)
Index= 1:nrow(training)
predictors = names(training)[names(training) != "target"]
predictors #Nombres de las variables predictoras
testResults = data.frame(target =
                           testing$target)
validResults = data.frame(target =
                            validation$target)
#
fiveStats = function(...) {
  c(twoClassSummary(...), defaultSummary(...))
}
#
ctrlcv = trainControl(method = "cv",number=3,
                      classProbs = TRUE,
                      summaryFunction = fiveStats,
                      verboseIter=TRUE)
#
rfFit = train(target ~ ., data = training
              ,
              method = "rf",
              trControl = ctrlcv,
              do.trace=TRUE,
              tuneLength=3,
              metric = "Sens"
              )
##
rfFit
#
#probabilidades estimadas de "Yes"
validResults$RF = predict(rfFit, validation,
                          type = "prob")[,1]
testResults$RF = predict(rfFit, testing,
                         type = "prob")[,1]
#
library(pROC)
rfTestROC = roc(testResults$target,
                testResults$RF,levels =
                  rev(levels(testResults$target)))
#
rfTestROC
plot(rfTestROC)
#
rfTestCM = confusionMatrix(predict(rfFit, testing),
                           testResults$target)
rfTestCM
#########
rfvalidROC = roc(validResults$target, validResults$RF,
                 levels = rev(levels(validResults$target)))
rfvalidROC
rfThresh = coords(rfvalidROC, x = "best", ret="threshold",
                  best.method="closest.topleft")
rfThresh
#
validResults$rfAlt = factor(ifelse(validResults$RF > rfThresh$threshold,
                                   "SI", "NO"))
validResults$rfAlt = factor(validResults$rfAlt , levels = rev(levels(validResults$rfAlt)))
testResults$rfAlt = factor(ifelse(testResults$RF > rfThresh$threshold,
                                  "SI", "NO" ))
#
rfAltvalidCM = confusionMatrix(validResults$rfAlt,
                               validResults$target)
##entrenamiento
downSampled = downSample(training[, -ncol(training)],
                         training$target)
dim(downSampled)
table(downSampled$Class)
# validation
downSampled_valid = downSample(validation[, -ncol(validation)],
                               validation$target)
dim(downSampled_valid)
table(downSampled_valid$Class)
#
downSampled_train_valid=rbind(downSampled,downSampled_valid )
dim(downSampled_train_valid)
table(downSampled_train_valid$Class)
##
upSampled = upSample(training[, -ncol(training)],
                     training$target)
dim(upSampled)
table(upSampled$Class)
table(training$target)
##
upSampled_valid = upSample(validation[, -ncol(validation)],
                           validation$target)
dim(upSampled_valid)
table(upSampled_valid$Class)
table(validation$target)
#
upSampled_train_valid=rbind(upSampled,upSampled_valid)
dim(upSampled_train_valid)
table(upSampled_train_valid$Class)
## SMOTE
library(DMwR2)
smoted = SmoteClassif(target ~ ., dat = training, dist = "HEOM",C.perc = "balance")
smoted_valid = SmoteClassif(target ~ ., dat = validation, dist = "HEOM",C.perc = "balance")
smoted_train_valid=rbind(smoted,smoted_valid)
table(smoted_train_valid$target)
####
library(ROSE)
roseoverN<-ovun.sample (target ~ .,data = training,
                        method = "over",seed=1)$data
table(roseoverN$target)
#
table(training$target)
####
roseoverp<-ovun.sample (target ~ .,
                        data = training,method = "over"
                        ,p=0.5,seed=1)$data
table(roseoverp$target)
table(training$target)
###
roseunderp <-ovun.sample(target ~ .,
                         data = training,method = "under"
                         ,p=0.5,seed=1)$data
table(roseunderp$target)
table(training$target)
###
roseboth<-ovun.sample(target ~ .,
                      data = training,method = "both"
                      ,p=0.5,seed=1)$data
table(roseboth$target)
table(training$target)
####
rose <- ROSE(target~., data=training, seed=3)$data
table(rose$target)
##
rose_valid = ROSE(target ~ .,
                  data = validation)$data
rose_train_valid=rbind(rose,rose_valid)
table(rose_train_valid$target)
### METODO RANDOM FOREST
ctrcvdown=ctrlcv
ctrcvdown$index=list(1:nrow(downSampled))
ctrcvdown$indexFinal=1:nrow(downSampled)
rfDown = train(Class ~ ., data = downSampled_train_valid,
               "rf",
               trControl = ctrcvdown,
               ntree = 500,do.trace=FALSE,
               tuneLength = 3,
               metric = "Sens")
##
testResults$RFdown = predict(rfDown, testing,
                             type = "prob")[,1]
rfDownROCT = roc(testResults$target, testResults$RFdown,
                 levels = rev(levels(validResults$target)), plot=TRUE)
rfDownROCT


######
Base_resumen_extract= Base_resumen_extract %>% left_join(Base_Clientes_BSol_vf[,c(10,11,14,13,34,35,29)],
                                                         by=c("ID_Cliente"="USERID_TITULAR"))
####
library(sqldf)
prueba= sqldf('SELECT * from Base_resumen_extract where Descripcion ="ACH RECIBIDA" AND año=2023 and mes=8')
class(prueba)
############################ PARTE DEL CODIGO PARA LOS POE DE LAS PERSONAS NATURALES 5/10/2023 #########
library(qcc)
casos= Base_Alertas_1 %>% count(Nro_ID_Cliente,Nombre_completo,
                                Nro_de_identificacion,TIPO_PERSONA,
                                Caracteristica_1, Caracteristica_2)
casos1=casos %>% filter(n>=10)
prueba= Base_Alertas_1 %>% filter(Nro_ID_Cliente==casos1$Nro_ID_Cliente[69]& Caracteristica_1==casos1$Caracteristica_1[69]&
                            Caracteristica_2==casos1$Caracteristica_2[69] & Categoria_actual=="Finalizada")
######################
prueba= Base_Alertas_1 %>% filter(Nro_ID_Cliente==unique(casos1$Nro_ID_Cliente)[66])
prueba1= prueba[,c(28,29,7,17)]
prueba1= prueba1 %>% mutate(Caracteristica= paste(Caracteristica_2, Caracteristica_1, sep=" : "))
prueba1= prueba1 %>% arrange(Fecha_de_generacion,Caracteristica_2)
#
prueba2= prueba1 %>% filter(Caracteristica==unique(prueba1$Caracteristica)[1])
prueba2= prueba2 %>% mutate(Tipo=ifelse(Caracteristica_1=="Importe Maximo por operacion",
                                        "Importe en $","Cantidad"))
#
prueba2= prueba2[,3:6]
control=qcc(prueba2$Valor_operado, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
prueba3= prueba2
prueba3$Valor_operado=round(control$limits[2],0)
prueba3$Tipo="Banda de confianza superior 1"
#
prueba6= prueba2
prueba6$Valor_operado=round(control$center,0)
prueba6$Tipo="Centro"
#
control=qcc(prueba2$Valor_operado, type = "xbar.one", std.dev = "MR", nsigmas = 2, plot = FALSE)
prueba4= prueba2
prueba4$Valor_operado=round(control$limits[2],0)
prueba4$Tipo="Banda de confianza superior 2"
#
control=qcc(prueba2$Valor_operado, type = "xbar.one", std.dev = "MR", nsigmas = 3, plot = FALSE)
prueba5= prueba2
prueba5$Valor_operado=round(control$limits[2],0)
prueba5$Tipo="Banda de confianza superior 3"
#
#
prueba_vf= rbind(prueba2, prueba3, prueba4, prueba5, prueba6)
prueba_vf= data.frame(prueba_vf, ID_Cliente=casos1$Nro_ID_Cliente[69])
unido= rbind(unido, prueba_vf)
#
hchart(prueba_vf, "spline" ,hcaes(x=Fecha_de_generacion, y=Valor_operado,group=Tipo),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         # enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))%>% hc_add_theme(hc_theme_elementary()) %>%
  hc_colors(c("blue","#F8850FFF","red","green","maroon")) %>%
  hc_subtitle(text=unique(prueba_vf$Caracteristica))
#


unido= data.frame()
for (i in 1:nrow(casos1)) {
  print(i)
  prueba= Base_Alertas_1 %>% filter(Nro_ID_Cliente==casos1$Nro_ID_Cliente[i]& Caracteristica_1==casos1$Caracteristica_1[i]&
                                      Caracteristica_2==casos1$Caracteristica_2[i])
  prueba1= prueba[,c(28,29,7,17)]
  prueba1= prueba1 %>% mutate(Caracteristica= paste(Caracteristica_2, Caracteristica_1, sep=" : "))
  prueba1= prueba1 %>% arrange(Fecha_de_generacion,Caracteristica_2)
  #
  prueba2= prueba1 %>% filter(Caracteristica==unique(prueba1$Caracteristica)[1])
  prueba2= prueba2 %>% mutate(Tipo=ifelse(Caracteristica_1=="Importe Maximo por operacion",
                                          "Importe en $","Cantidad"))
  #
  prueba2= prueba2[,3:6]
  control=qcc(prueba2$Valor_operado, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
  prueba3= prueba2
  prueba3$Valor_operado=round(control$limits[2],0)
  prueba3$Tipo="Banda de confianza superior 1"
  #
  # prueba6= prueba2
  # prueba6$Valor_operado=round(control$center,0)
  # prueba6$Tipo="Centro"
  #
  control=qcc(prueba2$Valor_operado, type = "xbar.one", std.dev = "MR", nsigmas = 2, plot = FALSE)
  prueba4= prueba2
  prueba4$Valor_operado=round(control$limits[2],0)
  prueba4$Tipo="Banda de confianza superior 2"
  #
  control=qcc(prueba2$Valor_operado, type = "xbar.one", std.dev = "MR", nsigmas = 3, plot = FALSE)
  prueba5= prueba2
  prueba5$Valor_operado=round(control$limits[2],0)
  prueba5$Tipo="Banda de confianza superior 3"
  #
  #
  prueba_vf= rbind(prueba2, prueba3, prueba4, prueba5)
  prueba_vf= data.frame(prueba_vf, ID_Cliente=casos1$Nro_ID_Cliente[i])
  unido= rbind(unido, prueba_vf)
}
#
prueba_vf= unido %>% filter()
hchart(prueba_vf, "spline" ,hcaes(x=Fecha_de_generacion, y=Valor_operado,group=Tipo),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         # enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))%>% hc_add_theme(hc_theme_elementary()) %>%
  hc_colors(c("blue","#F8850FFF","red","green","maroon")) %>%
  hc_subtitle(text=unique(prueba_vf$Caracteristica))

###
graf_d= casos1 %>% filter(Nro_ID_Cliente==1307782)
hchart(graf_d , "pie",innerSize = 200, hcaes(x = codigo, y = n),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         , # Añadir etiquetas
         format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())
#
prueba_vf= unido %>% filter(codigo=="1302409 TRANSACCIONES ACH RECIBIDA : Importe Maximo por operacion" )
graf_d= casos1 %>% filter(Nro_ID_Cliente==unique(prueba_vf$ID_Cliente))
hchart(graf_d , "pie",innerSize = 200, hcaes(x = codigo, y = n),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         , # Añadir etiquetas
         format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())
##############################






prueba= Base_Alertas_1 %>% filter(Nro_ID_Cliente==casos1$Nro_ID_Cliente[i]& Caracteristica_1==casos1$Caracteristica_1[i]&
                                    Caracteristica_2==casos1$Caracteristica_2[i])
prueba1= prueba[,c(28,29,7,17)]
prueba1= prueba1 %>% mutate(Caracteristica= paste(Caracteristica_2, Caracteristica_1, sep=" : "))
prueba1= prueba1 %>% arrange(Fecha_de_generacion,Caracteristica_2)
#
prueba2= prueba1 %>% filter(Caracteristica==unique(prueba1$Caracteristica)[1])
prueba2= prueba2 %>% mutate(Tipo=ifelse(Caracteristica_1=="Importe Maximo por operacion",
                                        "Importe en $","Cantidad"))
#
prueba2= prueba2[,3:6]
control=qcc(prueba2$Valor_operado, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
control1=qcc(prueba2$Valor_operado, type = "xbar.one", std.dev = "MR", nsigmas = 2, plot = FALSE)
control2=qcc(prueba2$Valor_operado, type = "xbar.one", std.dev = "MR", nsigmas = 3, plot = FALSE)

# "Banda de confianza superior 1"
Banda_de_confianza_superior_1= round(control$limits[2],0)
Banda_de_confianza_superior_2= round(control1$limits[2],0)
Banda_de_confianza_superior_3= round(control2$limits[2],0)
unido=data.frame(Banda_de_confianza_superior_1, Banda_de_confianza_superior_2, Banda_de_confianza_superior_3)

unido= data.frame()
for (i in 1:nrow(casos1)) {
  print(i)
  prueba= Base_Alertas_1 %>% filter(Nro_ID_Cliente==casos1$Nro_ID_Cliente[i]& Caracteristica_1==casos1$Caracteristica_1[i]&
                                      Caracteristica_2==casos1$Caracteristica_2[i])
  prueba1= prueba[,c(28,29,7,17)]
  prueba1= prueba1 %>% mutate(Caracteristica= paste(Caracteristica_2, Caracteristica_1, sep=" : "))
  prueba1= prueba1 %>% arrange(Fecha_de_generacion,Caracteristica_2)
  #
  prueba2= prueba1 %>% filter(Caracteristica==unique(prueba1$Caracteristica)[1])
  prueba2= prueba2 %>% mutate(Tipo=ifelse(Caracteristica_1=="Importe Maximo por operacion",
                                          "Importe en $","Cantidad"))
  #
  prueba2= prueba2[,3:6]
  control=qcc(prueba2$Valor_operado, type = "xbar.one", std.dev = "MR", nsigmas = 1, plot = FALSE)
  control1=qcc(prueba2$Valor_operado, type = "xbar.one", std.dev = "MR", nsigmas = 2, plot = FALSE)
  control2=qcc(prueba2$Valor_operado, type = "xbar.one", std.dev = "MR", nsigmas = 3, plot = FALSE)

  # "Banda de confianza superior 1"
  Banda_de_confianza_superior_1= round(control$limits[2],0)
  Banda_de_confianza_superior_2= round(control1$limits[2],0)
  Banda_de_confianza_superior_3= round(control2$limits[2],0)
  unido=rbind(unido, data.frame(Banda_de_confianza_superior_1, Banda_de_confianza_superior_2, Banda_de_confianza_superior_3))
}
#########


i=850
prueba= Base_Alertas_1 %>% filter(Nro_ID_Cliente==casos1$Nro_ID_Cliente[i]& Caracteristica_1==casos1$Caracteristica_1[i]&
                                    Caracteristica_2==casos1$Caracteristica_2[i])
Ultima_fecha_revision= max(prueba$Fecha_de_cambio_categoria)
Valor_esperado= unique(prueba$Valor_esperado)
unido= data.frame()
for (i in 1:nrow(casos1)) {
  print(i)
  prueba= Base_Alertas_1 %>% filter(Nro_ID_Cliente==casos1$Nro_ID_Cliente[i]& Caracteristica_1==casos1$Caracteristica_1[i]&
                                      Caracteristica_2==casos1$Caracteristica_2[i])
  Ultima_fecha_revision= max(prueba$Fecha_de_cambio_categoria)
  Valor_esperado= max(unique(prueba$Valor_esperado))
  unido= rbind(unido, data.frame(Ultima_fecha_revision, Valor_esperado))
}
#######
load("datos_analisis.RData")
# datos_analisis= Base_Alertas_1[,c(1,7,8,9,10,16:18,22,28,29)]
# datos_analisis$Nro_ID_Cliente= as.numeric(datos_analisis$Nro_ID_Cliente)
datos_analisis= Alertas
datos_analisis= datos_analisis %>% mutate(corre=1:nrow(datos_analisis))
datos_analisis= datos_analisis %>% left_join(Base_Clientes_BSol_vf[,c(1,10,11,13,14,15,17,29,34,35)],
                                             by=c("Nro_ID_Cliente"="USERID_TITULAR"))
datos_analisis= datos_analisis %>% group_by(corre) %>% filter(row_number()==1)

##
#save(datos_analisis, file = "datos_analisis.RData")
library(skimr)
datos_analisis= datos_analisis %>% left_join(BASE_INUSUALIDADES[,c(17:19,21)],
                                             by=c("Nro_ID_Cliente"="USERID_TITULAR"))
datos_analisis= datos_analisis %>% group_by(corre) %>% filter(row_number()==1)
datos_analisis= datos_analisis %>% mutate(target= ifelse(ROS=="NO"| is.na(ROS), "NO", "SI"))
datos_analisis$Antiguedad_Meses <- interval(datos_analisis$F_ALTA_CLIENTE,  as.Date("2023-7-31")) %/% months(1)
datos_analisis= datos_analisis %>% mutate(Edad= round(as.numeric((as.Date("2023-9-30")-FECHA_NACIMIENTO)/365.25),0))
datos_analisis= as.data.frame(datos_analisis)
#
datos_prueba= datos_analisis %>% filter(target=="SI")
#
orden_evalua= datos_prueba %>% count(Caracteristica_1, Caracteristica_2)  %>% arrange(desc(n))
#
prueba= datos_analisis %>% filter(Caracteristica_1==orden_evalua$Caracteristica_1[1] &
                                    Caracteristica_2== orden_evalua$Caracteristica_2[1]
                                  #& TIPO_PERSONA=="NATURAL"
                                  )
prueba1= prueba %>% filter(Valor_esperado<=500)
prueba1= prueba1 %>% filter(!is.na(Edad))
prueba1= prueba1 %>% filter(Categoria_actual=="Finalizada")
skim(prueba1)
#
datos_mod= prueba1[,c(1,36,37,30,27,24,17,35)]
datos_mod$target= as.factor(datos_mod$target)
datos_mod$target= factor(datos_mod$target, levels = rev(levels(datos_mod$target)))
table(datos_mod$target)
#
datos_mod= datos_mod %>% mutate(corre=1:nrow(datos_mod))
datos= datos_analisis %>% filter(Categoria_actual=="Finalizada") %>%  group_by(Nro_ID_Cliente) %>% tally()
datos_mod= datos_mod %>% left_join(datos, by="Nro_ID_Cliente")
names(datos_mod)[10]="Cant_Cierre_Alertas"
datos_mod= datos_mod[,-c(9,1)]
datos_mod= datos_mod[,c(8,2:7)]
datos_mod$GENERO_TITULAR= as.factor(datos_mod$GENERO_TITULAR)
datos_mod$ESTADO_CIVIL= as.factor(datos_mod$ESTADO_CIVIL)
datos_mod$NOMBRE_REGIONAL=as.factor(datos_mod$NOMBRE_REGIONAL)
########### PRUEBA CONEXION BIGRQUERY #################
# library(bigrquery)
# library(pak)
# billing <- bq_test_project() # replace this with your project ID
# sql <- "SELECT year, month, day, weight_pounds FROM `publicdata.samples.natality`"
#
# tb <- bq_project_query(billing, sql)
# bq_table_download(tb, n_max = 10)
# ##
# con <- dbConnect(
#   bigrquery::bigquery(),
#   project = "publicdata",
#   dataset = "samples",
# )
# con
# dbListTables(con)
# tbl(con, "natality")


####
# library(RODBC)
# con = odbcConnect("Documentos\eliminar.xlsx") # open a connection to the Excel file
# sqlTables(con)$TABLE_NAME # show all sheets
# df = sqlFetch(con, "Sheet1") # read a sheet
# df = sqlQuery(con, "select * from [Sheet1 $]") # read a sheet (alternative SQL syntax)
# close(con) # close the connection to the file
#####
# con <- dbConnect(odbc::odbc(), "prueba_odbc", timeout = 10)
# casos=dbSendQuery(con, "SELECT * FROM `D:\\Documentos\\archivo_prueba_conex.xlsx`.`'Sheet 1$'`
#                   WHERE CountryCode='NLD'")
# #
# casos=dbSendQuery(con, "SELECT * FROM `D:\\Documentos\\Parametros_Personas_Naturales_10_23.xlsx`.`'Sheet 1$'`
#                   ")
# #
# datos=dbFetch(casos)
# dbDisconnect(con)
#  user="BSOL\\dmamaniq"
#  pasword="Estadistico671&$&0000"
#https://bsolbo-my.sharepoint.com/:x:/r/personal/dmamaniq_bancosol_com_bo/Documents/ALERTAS%20PERSONAS%20NATURALES.xlsx?d=we3f9a55438a642779bc9dfb10b55d80c&csf=1&web=1&e=fcbhBq
conn = dbConnect(odbc(),
  Driver = "SQL Server",
  Server = "synw-scala-productivo.sql.azuresynapse.net",
  Database = "ro_bsol",
  #UID =user ,
  #PWD = pasword,
  Port = 1433,
  Trusted_Connection= TRUE
)
##
conn =DBI::dbConnect(
  odbc::databricks(),
  httpPath = "sql/protocolv1/o/1770393953602906/0425-152452-7rpfmcg9",
  Port=443
)


######## CONEXION BASE DE DATOS PRODUCCION #############
library(DBI)
library(odbc)
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "VFINDERDBSRV",
                 Database = "FINDER_V5",
                 #UID =user ,
                 #PWD = pasword,
                 Port = 1433,
                 Trusted_Connection= TRUE
                 )
#########
dbDisconnect(con)
#########
dbListTables(con)
# consulta 22/3/2024
cons="
SELECT *

FROM Administracion.EntidadFinder					AS EF
INNER JOIN Mantenimiento.PerfilAlerta				AS PA		ON PA.IdPerfilAlerta = EF.IdPerfilAlerta

"
#para obtener las agencias
cons="
SELECT *

FROM Administracion.Agencia

"

cons="

	SELECT
		EF.IdEntidadFinder, EF.PersonaFisica,EF.Documento, PA.Descripcion,
		--PA.IdPerfilAlerta,
		PA.Descripcion AS PerfilAlerta,
		--CTC.IdTipoCliente,
		TC.Descripcion AS TipoCliente,
		--CTC.IdControl,
		Valor,
		GeneraAlarma,
		BloqueaOperativa,
		--C.IdTipoControl,
		TCL.Descripcion AS TipoControl,
		--PS.IdProductoServicio,
		PS.Descripcion AS ProductoServicio,
		Habilitado

		FROM
			Administracion.EntidadFinder as EF
				INNER JOIN Mantenimiento.PerfilAlerta			AS PA	ON PA.IdPerfilAlerta = EF.IdPerfilAlerta
				INNER JOIN Mantenimiento.Control_PerfilAlerta	AS CP	ON PA.IdPerfilAlerta = CP.IdPerfilAlerta
				RIGHT JOIN Mantenimiento.Control_TipoCliente	AS CTC	ON CTC.IdControl = CP.IdControl --AND CTC.IdTipoCliente = @IdTipoCliente
				INNER JOIN Mantenimiento.TipoCliente			AS TC	ON CTC.IdTipoCliente = TC.IdTipoCliente
				INNER JOIN Mantenimiento.Control				AS C	ON C.IdControl = CTC.IdControl AND C.Activo = 1
				INNER JOIN Mantenimiento.ProductoServicio		AS PS	ON PS.IdProductoServicio = C.IdProductoServicio AND PS.Activo = 1
				INNER JOIN Mantenimiento.TipoControl			AS TCL	ON TCL.IdTipoControl = C.IdTipoControl
		WHERE		TCL.Descripcion='Cantidad de Operaciones Mensuales'
		ORDER BY EF.IdEntidadFinder


"
##
cons= "

SELECT EF.IdEntidadFinder, PA.IdPerfilAlerta, C.IdControl, T.Importe, C_PA.Valor, T.FechaTran, TC.InicioPeriodo,T.IdCuenta AS IdCuenta,T.Moneda,M.TipoCambio_aBs AS CotizacionDolar,
						'(CLIENTE) Fecha.Tran.: ' + CONVERT(VARCHAR(20), T.FechaTran, 103) + ' / Sucursal: ' + CONVERT(VARCHAR(20), T.Sucursal) + ' / IdCuenta: ' + CONVERT(VARCHAR(20), T.IdCuenta) + ' / Operación: ' + CONVERT(VARCHAR(20), T.Operacion)
						+ ' / Sub Operación: ' + CONVERT(VARCHAR(20), T.SubOperacion) + ' / Cuenta Operación: ' + CONVERT(VARCHAR(20), T.Cuenta_Operacion) + ' / Importe: ' + CONVERT(VARCHAR(20), T.Importe) + ' / Canal: ' + CONVERT(VARCHAR(20), T.Canal)
						+ ' / Módulo: ' + CONVERT(VARCHAR(20), T.Modulo) + ' / Transaccion: ' + CONVERT(VARCHAR(20), T.Transaccion) + ' / SubTipo Transacción: ' + STT.Descripcion + ' / Relación: ' + CONVERT(VARCHAR(20), T.Relacion)
						+ ' / DC: ' + IIF(T.DebeHaber = 1, 'Debe', IIF(T.DebeHaber = 2, 'Haber', '')) + ' / Rubro: ' + ISNULL(CONVERT(VARCHAR(20),T.Rubro),'') AS Detalle

						FROM Administracion.EntidadFinder					AS EF
						INNER JOIN Mantenimiento.PerfilAlerta				AS PA		ON PA.IdPerfilAlerta = EF.IdPerfilAlerta
						INNER JOIN Mantenimiento.Control_PerfilAlerta		AS C_PA		ON C_PA.IdPerfilAlerta = PA.IdPerfilAlerta
						INNER JOIN Mantenimiento.Control					AS C		ON C.IdControl = C_PA.IdControl
						INNER JOIN Mantenimiento.TipoControl				AS TC		ON TC.IdTipoControl = C.IdTipoControl
						INNER JOIN Mantenimiento.Control_TipoTransaccion	AS C_TT		ON C_TT.IdControl = C.IdControl
						INNER JOIN Administracion.SubTipoTransaccion		AS STT		ON C_TT.IdSubTipoTransaccion = STT.IdSubTipoTransaccion
						INNER JOIN Historico.Transaccion6M					AS T		ON T.Modulo = STT.IdModulo AND T.Transaccion = STT.IdTransaccion AND C_TT.DebeHaber = T.DebeHaber
						INNER JOIN Administracion.Moneda					AS M		ON M.CodMoneda = T.Moneda
						INNER JOIN Administracion.Cuenta_Integrante			AS CI		ON T.IdCuenta = CI.Cuenta AND CI.IdPersona = EF.IdEntidadFinder

						WHERE EF.IdTipoCliente = 1
							AND PA.IdTipoCliente = 1
							AND C_PA.Habilitado = 1
							AND C_PA.IdTipoCliente = 1
							AND C.Activo = 1
							AND TC.ControlaSaldo = 0
							AND CI.Titular = 'T'
							AND EF.IdEntidadFinder = 3238284

					UNION ALL
					--USAURIOS
					SELECT EF.IdEntidadFinder, PA.IdPerfilAlerta, C.IdControl, T.Importe, C_PA.Valor, T.FechaTran, TC.InicioPeriodo,NULL AS IdCuenta,T.Moneda,M.TipoCambio_aBs AS CotizacionDolar,
						'(USUARIO) Fecha.Tran.: ' + CONVERT(VARCHAR(20), T.FechaTran, 103) + ' / Sucursal: ' + CONVERT(VARCHAR(20), T.Sucursal) + ' / IdCuenta: ' + CONVERT(VARCHAR(20), T.IdCuenta) + ' / Operación: ' + CONVERT(VARCHAR(20), T.Operacion)
						+ ' / Sub Operación: ' + CONVERT(VARCHAR(20), T.SubOperacion) + ' / Cuenta Operación: ' + CONVERT(VARCHAR(20), T.Cuenta_Operacion) + ' / Importe: ' + CONVERT(VARCHAR(20), T.Importe) + ' / Canal: ' + CONVERT(VARCHAR(20), T.Canal)
						+ ' / Módulo: ' + CONVERT(VARCHAR(20), T.Modulo) + ' / Transaccion: ' + CONVERT(VARCHAR(20), T.Transaccion) + ' / SubTipo Transacción: ' + STT.Descripcion + ' / Relación: ' + CONVERT(VARCHAR(20), T.Relacion)
						+ ' / DC: ' + IIF(T.DebeHaber = 1, 'Debe', IIF(T.DebeHaber = 2, 'Haber', '')) + ' / Rubro: ' + ISNULL(CONVERT(VARCHAR(20),T.Rubro),'') AS Detalle

						FROM Administracion.EntidadFinder	AS EF

						INNER JOIN Mantenimiento.PerfilAlerta				AS PA		ON PA.IdPerfilAlerta = EF.IdPerfilAlerta
						INNER JOIN Mantenimiento.Control_PerfilAlerta		AS C_PA		ON C_PA.IdPerfilAlerta = PA.IdPerfilAlerta
						INNER JOIN Mantenimiento.Control					AS C		ON C.IdControl = C_PA.IdControl
						INNER JOIN Mantenimiento.TipoControl				AS TC		ON TC.IdTipoControl = C.IdTipoControl
						INNER JOIN Mantenimiento.Control_TipoTransaccion	AS C_TT		ON C_TT.IdControl = C.IdControl
						INNER JOIN Administracion.SubTipoTransaccion		AS STT		ON C_TT.IdSubTipoTransaccion = STT.IdSubTipoTransaccion
						INNER JOIN Historico.Transaccion6M	 				AS T		ON T.Modulo = STT.IdModulo AND T.Transaccion = STT.IdTransaccion AND C_TT.DebeHaber = T.DebeHaber AND T.IdPersona = EF.IdEntidadFinder
						INNER JOIN Administracion.Moneda					AS M		ON M.CodMoneda = T.Moneda

						WHERE
						EF.IdTipoCliente = 2
						AND PA.IdTipoCliente = 2
						AND C_PA.Habilitado = 1
						AND C_PA.IdTipoCliente = 2
						AND C.Activo = 1
						AND TC.ControlaSaldo = 0
						AND IdPersona NOT IN (SELECT IdPersona FROM Administracion.Cuenta_Integrante)
						AND EF.IdEntidadFinder = 3238284

OPTION(RECOMPILE)
"
##
cons="
DECLARE @FechaDesde DATE = CONVERT(DATE, '2023-01-01')

SELECT * FROM Administracion.LogBatch WHERE Info = 'error' AND Fecha >= @FechaDesde ORDER BY IdLog DESC"

cons="SELECT
E.IdLog AS [Identificador],
E.Fecha AS [Fecha de Error],
E.Mensaje AS [Mensaje],
E.Excepcion AS [Detalle],
U.UserName AS [Usuario]
FROM [Log4Net].[Error] AS E
INNER JOIN DBO.AspNetUsers AS U ON E.Usuario = U.Id
WHERE E.Fecha >= CONVERT(DATE, '2022-1-1',121) AND E.Fecha <= CONVERT(DATE, '2022-12-31',121)
ORDER BY E.Fecha DESC"
## persona natural
cons="SELECT
      PerNat.IdPersona, PerNat.TipoAlta, PDoc.Documento , PerNat.CanalInformacion, PerNat.Nombres, PerNat.ApellidoPaterno, PerNat.ApellidoMaterno, PerNat.Sexo, PerNat.EstadoCivil
      ,PerNat.EstadoCivilDeclarado, PerNat.FechaNacimiento, PerNat.PaisNacimiento, PerNat.Departamento_Provincia, PerNat.NroHijos,
      PerNat.NroDependientes, PerNat.IdPersonaConyuge
      , PerNat.FechaVencimiento_Documento, PerNat.EmpleadoBanco, PerNat.FechaIngreso_EmpleadoBanco, PerNat.UbicacionEstablecimiento, PerNat.NIT_Persona,Estado_NIT
      , PerNat.PromedioIngresosMensuales,PerNat.PropositoRelacionComercial, PerNat.EsPropietario_DineroCuenta, PerNat.DeclaracionFACTA
      ,PerNat.Nivel_Ingresos, PerNat.PaisResidencia, PerNat.ClasificacionCliente,
      PerNat.TipoClientePEP, PerNat.TipoClienteActAltoRiesgo,PerNat.Nacionalidad2,PerNat.FechaAltaCliente,
      PDir.Direccion,
       AOcup.Descripcion AS [Ocupacion],
      NivEd.Descripcion AS [Nivel_Educativo],
     Prof.Descripcion AS [Profesion],
     ActEco.Detalle   AS [Actividad_Economica],
      Telef.Telefono,
      PerNat.ClasificacionCliente       AS       [Clasificacion_Cliente],
      PDoc.IdTipoDocumento              AS       [Tipo_Persona],
      Agen.Descripcion                  AS       [Regional]

     FROM Administracion.PersonaNatural AS PerNat
     INNER JOIN Administracion.EntidadFinder AS PDoc ON Pernat.IdPersona= PDoc.IdEntidadFinder

     LEFT JOIN Administracion.Persona_Direccion AS PDir ON  PDir.IdPersona=Pernat.IdPersona
     LEFT JOIN Administracion.Ocupacion AS AOcup ON AOcup.IdOcupacion= Pernat.IdOcupacion
     LEFT JOIN Administracion.NivelEducativo AS NivEd ON NivEd.IdNivelEducativo= Pernat.IdNivelEducativo
     LEFT JOIN Administracion.Profesion AS Prof ON Prof.IdProfesion= Pernat.IdProfesion
     LEFT JOIN Administracion.Persona_ActividadEconomica AS ActEco ON ActEco.IdPersona= Pernat.IdPersona
     LEFT JOIN Administracion.Persona_Telefono AS Telef ON Telef.IdPersona=Pernat.IdPersona
     LEFT JOIN Administracion.Agencia AS Agen ON Agen.IdAgencia= Pernat.Departamento_Provincia
     "
### persona juridica
cons=" SELECT
     AdmPerJ.IdPersona, AdmPerJ.Estado, AdmPerJ.TipoAlta, AdmPerJ.VinculacionOrganismoRegulador, AdmPerJ.CanalDistribucion,
     AdmPerJ.RazonSocial, AdmPerJ.NombreReducido, AdmPerJ.TipoNaturalezaJuridica,
      AdmPerJ.ObjetoSocialRgn1, AdmPerJ.ObjetoSocialRgn2, AdmPerJ.FechaConstitucion,
     AdmPerJ.OficinaResgistral_Departamento, AdmPerJ.TipoInscripcion,
     AdmPerJ.Folio_NumeroNotarial, AdmPerJ.CapitalSocial, AdmPerJ.PresentaBalance, AdmPerJ.FechaExpiracion,
     AdmPerJ.SubSedeRegistral_Ciudad, AdmPerJ.NroRegistro, AdmPerJ.NroPoderRepresentanteLegal, AdmPerJ.InstitucionFinanciera,
     AdmPerJ.Exportador, AdmPerJ.Importador,
      AdmPerJ.FechaBalance, AdmPerJ.SegmentoMercado, AdmPerJ.PropositoRelacionComercial, AdmPerJ.DeclaracionFACTA,
     AdmPerJ.ClasificacionCliente, AdmPerJ.FechaAltaCliente,
     AdmNat.Descripcion AS [NaturalezaJuridica],
     PerDoc.Documento AS [NIT]


    FROM Administracion.PersonaJuridica AS AdmPerJ

    INNER JOIN Administracion.NaturalezaJuridica AS AdmNat ON AdmNat.IdNaturalezaJuridica= AdmPerJ.NaturalezaJuridica
    LEFT JOIN Administracion.Persona_Documento AS PerDoc ON PerDoc.IdPersona=AdmPerJ.IdPersona

"
##
cons="SELECT * FROM Administracion.PersonaNatural"
# CONSULTA INICIAL PROVEEDOR
cons="--Variables utilizadas para determinar el rango de Fecha de Generación de las alertas.
--El resultado final, considera las alertas con fecha de generación desde '@FechaDesde' hasta '@FechaHasta', INCLUYENDO ambos valores.
DECLARE @FechaDesde DATE = '2023-06-01'
DECLARE @FechaHasta DATE = '2024-12-31'

SELECT
	EF.IdEntidadFinder																				AS [Nro. ID de cliente],
	EF.Documento																					AS [Nro. de Identificación],
	EF.PersonaFisica                                    AS [Tipo_Persona],
	EF.Descripcion																					AS [Nombre completo],
	F.Descripcion																					AS [Flujo de alerta],
	TA.Descripcion																					AS [Tipo de alerta],
	A.Detalle																						AS [Detalle],
	A.FechaGeneracion																				AS [Fecha de Generación],
	CAT1.Descripcion																				AS [Categoría Anterior],
	CAT.Descripcion																					AS [Categoría Actual],
	A.FechaCambioCategoria																			AS [Fecha de Cambio de Categoría],
	U.UserName																						AS [Usuario en Proceso],
	U1.UserName																						AS [Usuario Finalización],
	PA.Descripcion																					AS [Perfil Alerta],
	PC.InicioPeriodo																				AS [Inicio Periodo],
	PC.FinPeriodo																					AS [Fin Periodo],
	PC.ValorEsperado																				AS [Valor Esperado],
	PC.ValorOperado																					AS [Valor Operado],
	IIF((PC.ValorOperado - PC.ValorEsperado) > 0, PC.ValorOperado - PC.ValorEsperado, 0)			AS [Excedido],
	IIF(A.IdCategoria = 5, 'Finalizada', IIF(A.FechaEnProceso IS NOT NULL, 'En Proceso', 'Pendiente')) AS [Estado de la Alerta]


	FROM Alerta.Alerta							AS A
	INNER JOIN Administracion.EntidadFinder		AS EF	ON A.IdEntidadFinder = EF.IdEntidadFinder

	LEFT JOIN Alerta.InfoAlerta_PerfilCliente	AS PC	ON PC.IdAlerta = A.IdAlerta
	LEFT JOIN Mantenimiento.PerfilAlerta		AS PA	ON PA.IdPerfilAlerta = PC.IdPerfilAlerta
	LEFT JOIN Alerta.FlujoAlerta				AS F	ON F.IdFlujoAlerta = A.IdFlujoAlerta
  LEFT JOIN Alerta.PasoFlujoAlerta    AS FA ON FA.IdPasoFlujoAlerta = A.IdAlerta
	LEFT JOIN Alerta.TipoAlerta					AS TA	ON TA.IdTipoAlerta = A.IdTipoAlerta
	LEFT JOIN Alerta.Categoria					AS CAT	ON CAT.IdCategoria = A.IdCategoria
	LEFT JOIN Alerta.Categoria					AS CAT1	ON CAT1.IdCategoria = A.IdCategoriaAnterior
	LEFT JOIN Alerta.Estado						AS E	ON E.IdEstadoAlerta = A.IdEstado
	LEFT JOIN dbo.AspNetUsers					AS U	ON U.Id = A.IdUsuario_EnProceso
	LEFT JOIN dbo.AspNetUsers					AS U1	ON U1.Id = A.IdUsuario_Finalizacion

	WHERE A.FechaGeneracion >= @FechaDesde AND A.FechaGeneracion <= @FechaHasta"
# CONSULTA INICIAL PROVEEDOR con OCUPACION
cons="--Variables utilizadas para determinar el rango de Fecha de Generación de las alertas.
--El resultado final, considera las alertas con fecha de generación desde '@FechaDesde' hasta '@FechaHasta', INCLUYENDO ambos valores.
DECLARE @FechaDesde DATE = '2024-01-01'
DECLARE @FechaHasta DATE = '2024-12-31'

SELECT
	EF.IdEntidadFinder																				AS [Nro. ID de cliente],
	EF.Documento																					AS [Nro. de Identificación],
	EF.PersonaFisica                                    AS [Tipo_Persona],
	EF.Descripcion																					AS [Nombre completo],
	F.Descripcion																					AS [Flujo de alerta],
	TA.Descripcion																					AS [Tipo de alerta],
	A.Detalle																						AS [Detalle],
	A.FechaGeneracion																				AS [Fecha de Generación],
	CAT1.Descripcion																				AS [Categoría Anterior],
	CAT.Descripcion																					AS [Categoría Actual],
	A.FechaCambioCategoria																			AS [Fecha de Cambio de Categoría],
	U.UserName																						AS [Usuario en Proceso],
	U1.UserName																						AS [Usuario Finalización],
	PA.Descripcion																					AS [Perfil Alerta],
	PC.InicioPeriodo																				AS [Inicio Periodo],
	PC.FinPeriodo																					AS [Fin Periodo],
	PC.ValorEsperado																				AS [Valor Esperado],
	PC.ValorOperado																					AS [Valor Operado],
	APerNat.IdOcupacion                                    AS [IdOcupacion],
  Ocup.Descripcion                                       AS [Descripcion_Ocupacion],
	IIF((PC.ValorOperado - PC.ValorEsperado) > 0, PC.ValorOperado - PC.ValorEsperado, 0)			AS [Excedido],
	IIF(A.IdCategoria = 5, 'Finalizada', IIF(A.FechaEnProceso IS NOT NULL, 'En Proceso', 'Pendiente')) AS [Estado de la Alerta]


	FROM Alerta.Alerta							AS A
	INNER JOIN Administracion.EntidadFinder		AS EF	ON A.IdEntidadFinder = EF.IdEntidadFinder

	LEFT JOIN Alerta.InfoAlerta_PerfilCliente	AS PC	ON PC.IdAlerta = A.IdAlerta
	LEFT JOIN Mantenimiento.PerfilAlerta		AS PA	ON PA.IdPerfilAlerta = PC.IdPerfilAlerta
	LEFT JOIN Alerta.FlujoAlerta				AS F	ON F.IdFlujoAlerta = A.IdFlujoAlerta
  LEFT JOIN Alerta.PasoFlujoAlerta    AS FA ON FA.IdPasoFlujoAlerta = A.IdAlerta
	LEFT JOIN Alerta.TipoAlerta					AS TA	ON TA.IdTipoAlerta = A.IdTipoAlerta
	LEFT JOIN Alerta.Categoria					AS CAT	ON CAT.IdCategoria = A.IdCategoria
	LEFT JOIN Alerta.Categoria					AS CAT1	ON CAT1.IdCategoria = A.IdCategoriaAnterior
	LEFT JOIN Alerta.Estado						AS E	ON E.IdEstadoAlerta = A.IdEstado
	LEFT JOIN dbo.AspNetUsers					AS U	ON U.Id = A.IdUsuario_EnProceso
	LEFT JOIN dbo.AspNetUsers					AS U1	ON U1.Id = A.IdUsuario_Finalizacion

	LEFT JOIN Administracion.PersonaNatural AS APerNat ON APerNat.IdPersona= EF.IdEntidadFinder
  LEFT JOIN Administracion.Ocupacion AS Ocup ON Ocup.IdOcupacion= APerNat.IdOcupacion

	WHERE A.FechaGeneracion >= @FechaDesde AND A.FechaGeneracion <= @FechaHasta"
 ############### PARA DESCRIPCION DE OBSERVACION DE ALERTA COMPLETA
cons="SELECT * FROM Administracion.PersonaNatural "
## 2da CONSULTA CON LAS DESCRIPCION DE LOS CIERRES
cons="--Variables utilizadas para determinar el rango de Fecha de Generación de las alertas.
--El resultado final, considera las alertas con fecha de generación desde '@FechaDesde' hasta '@FechaHasta', INCLUYENDO ambos valores.
DECLARE @FechaDesde DATE = '2023-10-01'
DECLARE @FechaHasta DATE = '2024-12-31'

SELECT
	EF.IdEntidadFinder																				AS [Nro. ID de cliente],
	EF.Documento																					AS [Nro. de Identificación],
	EF.PersonaFisica                                    AS [Tipo_Persona],
	EF.Descripcion																					AS [Nombre completo],
	F.Descripcion																					AS [Flujo de alerta],
	TA.Descripcion																					AS [Tipo de alerta],
	A.Detalle																						AS [Detalle],
	A.FechaGeneracion																				AS [Fecha de Generación],
	CAT1.Descripcion																				AS [Categoría Anterior],
	CAT.Descripcion																					AS [Categoría Actual],
	A.FechaCambioCategoria																			AS [Fecha de Cambio de Categoría],
	U.UserName																						AS [Usuario en Proceso],
	U1.UserName																						AS [Usuario Finalización total],
	PA.Descripcion																					AS [Perfil Alerta],
	PC.InicioPeriodo																				AS [Inicio Periodo],
	PC.FinPeriodo																					AS [Fin Periodo],
	PC.ValorEsperado																				AS [Valor Esperado],
	PC.ValorOperado																					AS [Valor Operado],
  FA.Descripcion                                          AS [Desc1],
  OBS.Texto                                               AS [Descripcion_cierre],
  OBS.Fecha                                               AS [Fecha_cierre],
  U2.UserName																						AS [Usuario finalizacion],
  EF.IdTipoDocumento                                    AS [Tipo_Documento],
	IIF((PC.ValorOperado - PC.ValorEsperado) > 0, PC.ValorOperado - PC.ValorEsperado, 0)			AS [Excedido],
	IIF(A.IdCategoria = 5, 'Finalizada', IIF(A.FechaEnProceso IS NOT NULL, 'En Proceso', 'Pendiente')) AS [Estado de la Alerta]


	FROM Alerta.Alerta							AS A
	INNER JOIN Administracion.EntidadFinder		AS EF	ON A.IdEntidadFinder = EF.IdEntidadFinder

	LEFT JOIN Alerta.InfoAlerta_PerfilCliente	AS PC	ON PC.IdAlerta = A.IdAlerta
	LEFT JOIN Mantenimiento.PerfilAlerta		AS PA	ON PA.IdPerfilAlerta = PC.IdPerfilAlerta
	LEFT JOIN Alerta.FlujoAlerta				AS F	ON F.IdFlujoAlerta = A.IdFlujoAlerta
  LEFT JOIN Alerta.PasoFlujoAlerta    AS FA ON FA.IdPasoFlujoAlerta = A.IdAlerta
  LEFT JOIN Alerta.Observacion        AS OBS ON OBS.IdAlerta = A.IdAlerta
	LEFT JOIN Alerta.TipoAlerta					AS TA	ON TA.IdTipoAlerta = A.IdTipoAlerta
	LEFT JOIN Alerta.Categoria					AS CAT	ON CAT.IdCategoria = A.IdCategoria
	LEFT JOIN Alerta.Categoria					AS CAT1	ON CAT1.IdCategoria = A.IdCategoriaAnterior
	LEFT JOIN Alerta.Estado						AS E	ON E.IdEstadoAlerta = A.IdEstado
	LEFT JOIN dbo.AspNetUsers					AS U	ON U.Id = A.IdUsuario_EnProceso
	LEFT JOIN dbo.AspNetUsers					AS U1	ON U1.Id = A.IdUsuario_Finalizacion
  LEFT JOIN dbo.AspNetUsers					AS U2	ON U2.Id = OBS.IdUsuario

	WHERE A.FechaGeneracion >= @FechaDesde AND A.FechaGeneracion <= @FechaHasta"
## 2da CONSULTA CON LAS DESCRIPCION DE LOS CIERRES con ocupacion
cons="--Variables utilizadas para determinar el rango de Fecha de Generación de las alertas.
--El resultado final, considera las alertas con fecha de generación desde '@FechaDesde' hasta '@FechaHasta', INCLUYENDO ambos valores.
DECLARE @FechaDesde DATE = '2023-06-01'
DECLARE @FechaHasta DATE = '2024-12-31'

SELECT
	EF.IdEntidadFinder																				AS [Nro. ID de cliente],
	EF.Documento																					AS [Nro. de Identificación],
	EF.PersonaFisica                                    AS [Tipo_Persona],
	EF.Descripcion																					AS [Nombre completo],
	F.Descripcion																					AS [Flujo de alerta],
	TA.Descripcion																					AS [Tipo de alerta],
	A.Detalle																						AS [Detalle],
	A.FechaGeneracion																				AS [Fecha de Generación],
	CAT1.Descripcion																				AS [Categoría Anterior],
	CAT.Descripcion																					AS [Categoría Actual],
	A.FechaCambioCategoria																			AS [Fecha de Cambio de Categoría],
	U.UserName																						AS [Usuario en Proceso],
	U1.UserName																						AS [Usuario Finalización total],
	PA.Descripcion																					AS [Perfil Alerta],
	PC.InicioPeriodo																				AS [Inicio Periodo],
	PC.FinPeriodo																					AS [Fin Periodo],
	PC.ValorEsperado																				AS [Valor Esperado],
	PC.ValorOperado																					AS [Valor Operado],
  FA.Descripcion                                          AS [Desc1],
  OBS.Texto                                               AS [Descripcion_cierre],
  OBS.Fecha                                               AS [Fecha_cierre],
  U2.UserName																						AS [Usuario finalizacion],
  APerNat.IdOcupacion                                    AS [IdOcupacion],
  Ocup.Descripcion                                       AS [Descripcion_Ocupacion],
  ActEco.Detalle                                        AS [Actividad_Economica],
	IIF((PC.ValorOperado - PC.ValorEsperado) > 0, PC.ValorOperado - PC.ValorEsperado, 0)			AS [Excedido],
	IIF(A.IdCategoria = 5, 'Finalizada', IIF(A.FechaEnProceso IS NOT NULL, 'En Proceso', 'Pendiente')) AS [Estado de la Alerta]


	FROM Alerta.Alerta							AS A
	INNER JOIN Administracion.EntidadFinder		AS EF	ON A.IdEntidadFinder = EF.IdEntidadFinder

	LEFT JOIN Alerta.InfoAlerta_PerfilCliente	AS PC	ON PC.IdAlerta = A.IdAlerta
	LEFT JOIN Mantenimiento.PerfilAlerta		AS PA	ON PA.IdPerfilAlerta = PC.IdPerfilAlerta
	LEFT JOIN Alerta.FlujoAlerta				AS F	ON F.IdFlujoAlerta = A.IdFlujoAlerta
  LEFT JOIN Alerta.PasoFlujoAlerta    AS FA ON FA.IdPasoFlujoAlerta = A.IdAlerta
  LEFT JOIN Alerta.Observacion        AS OBS ON OBS.IdAlerta = A.IdAlerta
	LEFT JOIN Alerta.TipoAlerta					AS TA	ON TA.IdTipoAlerta = A.IdTipoAlerta
	LEFT JOIN Alerta.Categoria					AS CAT	ON CAT.IdCategoria = A.IdCategoria
	LEFT JOIN Alerta.Categoria					AS CAT1	ON CAT1.IdCategoria = A.IdCategoriaAnterior
	LEFT JOIN Alerta.Estado						AS E	ON E.IdEstadoAlerta = A.IdEstado
	LEFT JOIN dbo.AspNetUsers					AS U	ON U.Id = A.IdUsuario_EnProceso
	LEFT JOIN dbo.AspNetUsers					AS U1	ON U1.Id = A.IdUsuario_Finalizacion
  LEFT JOIN dbo.AspNetUsers					AS U2	ON U2.Id = OBS.IdUsuario

  LEFT JOIN Administracion.PersonaNatural AS APerNat ON APerNat.IdPersona= EF.IdEntidadFinder
  LEFT JOIN Administracion.Ocupacion AS Ocup ON Ocup.IdOcupacion= APerNat.IdOcupacion
  LEFT JOIN Administracion.Persona_ActividadEconomica AS ActEco ON ActEco.IdPersona= APerNat.IdPersona

	WHERE A.FechaGeneracion >= @FechaDesde AND A.FechaGeneracion <= @FechaHasta"
## 3RA CONSULTA CON LAS DESCRIPCION DE LOS CIERRES - ALERTAS EN EXTENSO POR CADA TRANSACCION
cons="--Variables utilizadas para determinar el rango de Fecha de Generación de las alertas.
--El resultado final, considera las alertas con fecha de generación desde '@FechaDesde' hasta '@FechaHasta', INCLUYENDO ambos valores.
DECLARE @FechaDesde DATE = '2023-11-01'
DECLARE @FechaHasta DATE = '2024-12-31'

SELECT
	EF.IdEntidadFinder																				AS [Nro. ID de cliente],
	EF.Documento																					AS [Nro. de Identificación],
	EF.Descripcion																					AS [Nombre completo],
	F.Descripcion																					AS [Flujo de alerta],
	TA.Descripcion																					AS [Tipo de alerta],
	A.Detalle																						AS [Detalle],
	A.FechaGeneracion																				AS [Fecha de Generación],
	CAT1.Descripcion																				AS [Categoría Anterior],
	CAT.Descripcion																					AS [Categoría Actual],
    A1.IdCuenta                                     AS [Cuenta],
    A1.Detalle										                 	AS [Descripcion_transaccion],

	A.FechaCambioCategoria																			AS [Fecha de Cambio de Categoría],
	U.UserName																						AS [Usuario en Proceso],
	U1.UserName																						AS [Usuario Finalización Total],
	PA.Descripcion																					AS [Perfil Alerta],
	PC.InicioPeriodo																				AS [Inicio Periodo],
	PC.FinPeriodo																					AS [Fin Periodo],
	PC.ValorEsperado																				AS [Valor Esperado],
	PC.ValorOperado																					AS [Valor Operado],
  FA.Descripcion                                          AS [Desc1],
  OBS.Texto                                               AS [Descripcion_Cierre_Alerta],
  OBS.Fecha                                               AS [Fecha_Cierre_Alerta],
  U2.UserName																						AS [Usuario finalizacion],

	IIF((PC.ValorOperado - PC.ValorEsperado) > 0, PC.ValorOperado - PC.ValorEsperado, 0)			AS [Excedido],
	IIF(A.IdCategoria = 5, 'Finalizada', IIF(A.FechaEnProceso IS NOT NULL, 'En Proceso', 'Pendiente')) AS [Estado de la Alerta]


	FROM Alerta.Alerta							AS A
	INNER JOIN Administracion.EntidadFinder		AS EF	ON A.IdEntidadFinder = EF.IdEntidadFinder

	LEFT JOIN Alerta.InfoAlerta_PerfilCliente	AS PC	ON PC.IdAlerta = A.IdAlerta
	LEFT JOIN Mantenimiento.PerfilAlerta		AS PA	ON PA.IdPerfilAlerta = PC.IdPerfilAlerta
	LEFT JOIN Alerta.FlujoAlerta				AS F	ON F.IdFlujoAlerta = A.IdFlujoAlerta
  LEFT JOIN Alerta.PasoFlujoAlerta    AS FA ON FA.IdPasoFlujoAlerta = A.IdAlerta
  LEFT JOIN Alerta.Observacion        AS OBS ON OBS.IdAlerta = A.IdAlerta
  LEFT JOIN Alerta.MovimientoAcumulado_PerfilCliente      AS A1 ON A1.IdAlerta = A.IdAlerta
	LEFT JOIN Alerta.TipoAlerta					AS TA	ON TA.IdTipoAlerta = A.IdTipoAlerta
	LEFT JOIN Alerta.Categoria					AS CAT	ON CAT.IdCategoria = A.IdCategoria
	LEFT JOIN Alerta.Categoria					AS CAT1	ON CAT1.IdCategoria = A.IdCategoriaAnterior
	LEFT JOIN Alerta.Estado						AS E	ON E.IdEstadoAlerta = A.IdEstado
	LEFT JOIN dbo.AspNetUsers					AS U	ON U.Id = A.IdUsuario_EnProceso
	LEFT JOIN dbo.AspNetUsers					AS U1	ON U1.Id = A.IdUsuario_Finalizacion
  LEFT JOIN dbo.AspNetUsers					AS U2	ON U2.Id = OBS.IdUsuario

	WHERE A.FechaGeneracion >= @FechaDesde AND A.FechaGeneracion <= @FechaHasta"
###########
cons= "
DECLARE @IdFinder AS INT;
SELECT @IdFinder = IdEntidadFinder FROM Administracion.EntidadFinder WHERE Documento = '8150686SC';


--TRANSACCIONES PROCESADAS POR EL SISTEMA (DE UN CLIENTE EN PARTICULAR)
SELECT T.FechaTran,T.FechaImpactada,T.Importe,T.Moneda,T.Sucursal,T.DebeHaber,T.Modulo,T.Rubro,T.Transaccion
FROM		Administracion.Cuenta_Integrante	AS CI
INNER JOIN	Historico.Transaccion6M				AS T		ON T.IdCuenta = CI.Cuenta
WHERE  CI.Titular = 'T' AND CI.IdPersona = @IdFinder AND FechaTran > DATEADD(DAY,-30,GETDATE())


--TRANSACCIONES ACUMULADAS POR EL SISTEMA (DE UN CLIENTE EN PARTICULAR)
SELECT M.*
FROM Historico.MovimientoAcumulado_PerfilCliente AS M
WHERE  M.IdEntidadFinder = @IdFinder AND Fecha > DATEADD(DAY,-30,GETDATE())


--ACUMULADOS (DE UN CLIENTE EN PARTICULAR)
SELECT AC.*
FROM Alerta.ControlAcumulado_PerfilCliente AS AC
WHERE  AC.IdEntidadFinder = @IdFinder

--PERFIL ASIGNADO
SELECT PA.*,CPA.*
FROM Administracion.EntidadFinder				AS EF
INNER JOIN Mantenimiento.PerfilAlerta			AS PA	ON EF.IdPerfilAlerta = PA.IdPerfilAlerta
INNER JOIN Mantenimiento.Control_PerfilAlerta	AS CPA	ON PA.IdPerfilAlerta = CPA.IdPerfilAlerta
WHERE EF.IdEntidadFinder = @IdFinder


--TIPO DE TRANSACCIONES CONTROLADAS
SELECT CTT.DebeHaber,STT.*
FROM Administracion.EntidadFinder					AS EF
INNER JOIN Mantenimiento.PerfilAlerta				AS PA	ON EF.IdPerfilAlerta = PA.IdPerfilAlerta
INNER JOIN Mantenimiento.Control_PerfilAlerta		AS CPA	ON PA.IdPerfilAlerta = CPA.IdPerfilAlerta
INNER JOIN Mantenimiento.Control_TipoTransaccion	AS CTT	ON  CPA.IdControl = CTT.IdControl
INNER JOIN Administracion.SubTipoTransaccion		AS STT	ON CTT.IdSubTipoTransaccion = STT.IdSubTipoTransaccion
WHERE EF.IdEntidadFinder = @IdFinder

"
#### 23961308
resultado=dbSendQuery(con, cons)

rest <- dbFetch(resultado)
#
rest= rest %>% filter(!(Detalle=="Alerta generada por JOB - Debida Diligencia pasa a pendiente."))
rest= rest %>% filter(!(Detalle=="Alerta generada por cambio de perfil operativo - Debida Diligencia pasa a pendiente."))
#
library(openxlsx)
Alertas_Pendientes= rest %>% filter(`Estado de la Alerta`=="Pendiente" & is.na(Descripcion_cierre))
write.xlsx(Alertas_Pendientes, file = "Pendientes_09112024.xlsx")
##########
rest= rest %>% mutate(cod= paste(`Nro. ID de cliente`, Detalle, `Fecha de Generación`,Fecha_cierre))
# PARA ALERTA EN EXTENSO USAR LA 3RA
prueba= alertas_extenso %>% group_by(`Nro. ID de cliente`, Detalle, `Fecha de Generación`, Fecha) %>% summarise(total_operaciones=n(),cant_cuentas=length(unique(Cuenta)), cuentas= list(unique(Cuenta)))
prueba= prueba %>% mutate(cod= paste(`Nro. ID de cliente`, Detalle, `Fecha de Generación`, Fecha))
#
rest_1= rest %>% mutate(corre=1:nrow(rest))
rest_1= rest_1 %>% left_join(prueba[,5:8],by="cod")
#
prueba1= rest %>% count(`Nro. ID de cliente`, Detalle, `Fecha de Generación`)
prueba1$`Fecha de Generación`= as.Date(prueba1$`Fecha de Generación`)

# dbListTables(con, table_name = "I%")
#df <- DBI::dbGetQuery(con,"Select *  from ")
##################
Descripciones=c("ACH Recibida (Express)","RETIRO EFECTIVO BOLIVIANOS","ACH RECIBIDA","BTS Transferencia ACH"
  ,"DEPOSITO EFECTIVO BOLIVIANOS","BTS Transferencia ACH Express"
  ,"RETIRO EFECTIVO DOLARES","Depósito efectivo ATM propio","Retiro en ATM ajeno Fuera Hora"
  ,"ACH ENVÍO DÉBITO CA","BTS Envío Giros Nacionales","DEPOSITO EFECTIVO DOLARES"
  ,"Transferencias ACH","Envío Giros al Exterior MN","Envío Giros Nacionales"
  ,"Pago Giros Locales","Envío Giros al Exterior ME","Pago Giros en CA/ Bco Chile",
  "Retiro en ATM propio","Retiro en ATM propio Fuera Hor","Retiro en ATM ajeno"
  )
#save(Transacciones, file = "Transacciones.RData")
Transacciones_1= Transacciones %>% filter(Descripcion %in% Descripciones)
# PARA EL INCISO a)
regla_a= Transacciones_1 %>% filter(Importe_Depositos>= 68600 | Importe_Retiros>=68600)
regla_a= regla_a %>% mutate(cod= paste(Fecha, Hora, Descripcion, CI_TITULAR, Importe_Depositos,
                                                       Importe_Retiros))
# PARA EL INCISO b)
Transacciones_1= Transacciones_1 %>% mutate(cod= paste(Fecha, Hora, Descripcion, CI_TITULAR, Importe_Depositos,
                                                       Importe_Retiros))
Transacciones_2= Transacciones_1 %>% filter(!(cod %in% regla_a$cod) )
###
corte_1= Transacciones_2 %>% filter(Fecha<=as.Date("2023-10-10")& Descripcion %in% c("RETIRO EFECTIVO BOLIVIANOS",
                                                                                    "DEPOSITO EFECTIVO BOLIVIANOS",
                                                                                    "RETIRO EFECTIVO DOLARES",
                                                                                    "Depósito efectivo ATM propio",
                                                                                    "Retiro en ATM ajeno Fuera Hora",
                                                                                    "DEPOSITO EFECTIVO DOLARES",
                                                                                    "Retiro en ATM propio",
                                                                                    "Retiro en ATM propio Fuera Hor",
                                                                                    "Retiro en ATM ajeno"
                                                                                    ))
###
corte_11= corte_1 %>% filter(Importe_Depositos!=0) %>% group_by(CI_TITULAR) %>% summarise(total=sum(Importe_Depositos),
                                                                                          cantidad=n())
corte_11= corte_11 %>% filter(total>= 68600)
corte_11= corte_11 %>% mutate(rango_fecha=1)
corte_11= corte_11 %>% mutate(tipo_op="Depositos")
#
corte_12= corte_1 %>% filter(Importe_Retiros!=0) %>% group_by(CI_TITULAR) %>% summarise(total=sum(Importe_Retiros),
                                                                                          cantidad=n())
corte_12= corte_12 %>% filter(total>= 68600)
corte_12= corte_12 %>% mutate(rango_fecha=1)
corte_12= corte_12 %>% mutate(tipo_op="Retiros")
###
corte_2= Transacciones_2 %>% filter(!(CI_TITULAR %in% corte_11$CI_TITULAR)&
                                      !(CI_TITULAR %in% corte_12$CI_TITULAR)) %>%
  filter(Fecha>=as.Date("2023-10-2") &Fecha<=as.Date("2023-10-11")& Descripcion %in% c("RETIRO EFECTIVO BOLIVIANOS",
                                                                                     "DEPOSITO EFECTIVO BOLIVIANOS",
                                                                                     "RETIRO EFECTIVO DOLARES",
                                                                                     "Depósito efectivo ATM propio",
                                                                                     "Retiro en ATM ajeno Fuera Hora",
                                                                                     "DEPOSITO EFECTIVO DOLARES",
                                                                                     "Retiro en ATM propio",
                                                                                     "Retiro en ATM propio Fuera Hor",
                                                                                     "Retiro en ATM ajeno"
))
###
corte_21= corte_2 %>% filter(Importe_Depositos!=0) %>% group_by(CI_TITULAR) %>% summarise(total=sum(Importe_Depositos),
                                                                                          cantidad=n())
corte_21= corte_21 %>% filter(total>= 68600)
corte_21= corte_21 %>% mutate(rango_fecha=2)
corte_21= corte_21 %>% mutate(tipo_op="Depositos")
#
corte_22= corte_2 %>% filter(Importe_Retiros!=0) %>% group_by(CI_TITULAR) %>% summarise(total=sum(Importe_Retiros),
                                                                                        cantidad=n())
corte_22= corte_22 %>% filter(total>= 68600)
corte_22= corte_22 %>% mutate(rango_fecha=2)
corte_22= corte_22 %>% mutate(tipo_op="Retiros")
#######
corte_3= Transacciones_2 %>% filter(!(CI_TITULAR %in% corte_11$CI_TITULAR)&
                                      !(CI_TITULAR %in% corte_12$CI_TITULAR)&
                                      !(CI_TITULAR %in% corte_21$CI_TITULAR)&
                                      !(CI_TITULAR %in% corte_22$CI_TITULAR)) %>%
  filter(Fecha>=as.Date("2023-10-3") &Fecha<=as.Date("2023-10-12")& Descripcion %in% c("RETIRO EFECTIVO BOLIVIANOS",
                                                                                       "DEPOSITO EFECTIVO BOLIVIANOS",
                                                                                       "RETIRO EFECTIVO DOLARES",
                                                                                       "Depósito efectivo ATM propio",
                                                                                       "Retiro en ATM ajeno Fuera Hora",
                                                                                       "DEPOSITO EFECTIVO DOLARES",
                                                                                       "Retiro en ATM propio",
                                                                                       "Retiro en ATM propio Fuera Hor",
                                                                                       "Retiro en ATM ajeno"
  ))
###
corte_31= corte_3 %>% filter(Importe_Depositos!=0) %>% group_by(CI_TITULAR) %>% summarise(total=sum(Importe_Depositos),
                                                                                          cantidad=n())
corte_31= corte_31 %>% filter(total>= 68600)
corte_31= corte_31 %>% mutate(rango_fecha=3)
corte_31= corte_31 %>% mutate(tipo_op="Depositos")
#
corte_32= corte_3 %>% filter(Importe_Retiros!=0) %>% group_by(CI_TITULAR) %>% summarise(total=sum(Importe_Retiros),
                                                                                        cantidad=n())
corte_32= corte_32 %>% filter(total>= 68600)
corte_32= corte_32 %>% mutate(rango_fecha=3)
corte_32= corte_32 %>% mutate(tipo_op="Retiros")
##
regla_b= rbind(corte_11, corte_12, corte_21, corte_22, corte_31, corte_32)

######  PARA EL INCISO c) no se reconoce de momento



######  PARA EL INCISO d)
descrip_giro_nac=c("Envío Giros Nacionales","BTS Envío Giros Nacionales")
Transacciones_d= Transacciones %>% filter(Descripcion %in% descrip_giro_nac)
corte_4= Transacciones_d %>% filter(Fecha<=as.Date("2023-10-5"))
corte_41= corte_4 %>% group_by(CI_TITULAR) %>% summarise(total= sum(Importe_Retiros))
corte_41= corte_41 %>% filter(total>=13720)
#
# corte_5= Transacciones_d %>% filter(!(CI_TITULAR %in% corte_41$CI_TITULAR)) %>%
#   filter(Fecha>=as.Date("2023-10-8") & Fecha<=as.Date("2023-10-12"))
# corte_51= corte_5 %>% group_by(CI_TITULAR) %>% summarise(total= sum(Importe_Retiros))
# corte_51= corte_51 %>% filter(total>=13720)
regla_c=corte_41
#
regla_a= regla_a %>% mutate(form_generado= ifelse(regla_a$CI_TITULAR %in% unique(datos_pcc1$numero_identidad),1,0 ))
regla_b= regla_b %>% mutate(form_generado= ifelse(regla_b$CI_TITULAR %in% unique(datos_pcc1$numero_identidad),1,0 ))
regla_c= regla_c %>% mutate(form_generado= ifelse(regla_c$CI_TITULAR %in% unique(datos_pcc1$numero_identidad),1,0 ))





############## PARA SEGMENTACION USUARIOS ##############
#9999999999LP
library(readxl)
Base_Usuarios <- read_excel("D:/Nueva carpeta1/BASE USUARIOS/Usuarios 2023/Enero a Junio.xlsx",
                                 sheet = "Hoja1")
##
Base_Usuarios= as.data.frame(Base_Usuarios)
Base_Usuarios$Fecha= as.Date(Base_Usuarios$Fecha)
Base_Usuarios= Base_Usuarios %>% filter(!is.na(Importe))
Base_Usuarios= Base_Usuarios %>% mutate(Importe_Bs= round(ifelse(Moneda==1, Importe, Importe*6.86),2))
Base_Usuarios= Base_Usuarios %>% mutate(NUMERO_DOCUMENTO = toupper(NroDoc))
Base_Usuarios= Base_Usuarios %>% filter(!is.na(NUMERO_DOCUMENTO))
Base_Usuarios= Base_Usuarios %>% filter(`Contabilizado?`=="S")
Base_Usuarios= Base_Usuarios %>% mutate(corre= 1:nrow(Base_Usuarios))
Base_Usuarios= Base_Usuarios %>% left_join(Base_Clientes_BSol_vf[,c(4,6)], by=c("NUMERO_DOCUMENTO"))
Base_Usuarios= Base_Usuarios %>% group_by(corre) %>% filter(row_number()==1)
Base_Usuarios= as.data.frame(Base_Usuarios)
#
aux=Base_Usuarios
Base_Usuarios= Base_Usuarios %>% filter(is.na(CLASIFICACION)| CLASIFICACION %in%c("U","I","Q","L",
                                                                                  "R","S","V") )
#
# round(as.numeric(summary(persona$Fecha)[[6]]-summary(persona$Fecha)[[1]])/12)
# #
# seq.Date(summary(persona$Fecha)[[1]], summary(persona$Fecha)[[6]], by=12)
# valores=NULL
# valor_max=NULL
# valor_sum_mayores=NULL
# #
# for (i in 1:round(as.numeric(summary(persona$Fecha)[[6]]-summary(persona$Fecha)[[1]])/12)) {
#   print(i)
#     intervalo_1=seq.Date(summary(persona$Fecha)[[1]], summary(persona$Fecha)[[6]], by=12)[i]
#     intervalo_2= intervalo_1+11
#     valores[i]= persona %>% filter(Fecha>=intervalo_1 & Fecha<= intervalo_2) %>% select(Importe) %>% sum()
# }
# valor_max[j]= max(valores)
# valor_sum_mayores[j]= sum(valores[valores>=34300])

valores=NULL
valor_max=NULL
valor_sum_mayores=NULL
alerta_12=NULL
nro_transac=NULL
# for (j in 1:length(unique(Base_Usuarios$NUMERO_DOCUMENTO))) {
#   print(j)
#   persona= Base_Usuarios %>% filter(NUMERO_DOCUMENTO==unique(Base_Usuarios$NUMERO_DOCUMENTO)[j])
#    # round(as.numeric(summary(persona$Fecha)[[6]]-summary(persona$Fecha)[[1]])/12)
#    # seq.Date(summary(persona$Fecha)[[1]], summary(persona$Fecha)[[6]], by=12)
#   # seq.Date(summary(persona$Fecha)[[1]], summary(persona$Fecha)[[6]], by=12)-1
#   # seq.Date(summary(persona$Fecha)[[1]], summary(persona$Fecha)[[6]], by=12)
#   #
#   valores=NULL
#   if((round(as.numeric(summary(persona$Fecha)[[6]]-summary(persona$Fecha)[[1]])/12)+1)!=0)
#   {
#     for (i in 1:(round(as.numeric(summary(persona$Fecha)[[6]]-summary(persona$Fecha)[[1]])/12)+1)) {
#       intervalo_1=seq.Date(summary(persona$Fecha)[[1]], summary(persona$Fecha)[[6]], by=12)[i]
#       intervalo_2= intervalo_1+11
#       valores[i]= persona %>% filter(Fecha>=intervalo_1 & Fecha<= intervalo_2) %>% select(Importe_Bs) %>% sum()
#     }
#     valor_max[j]= max(valores)
#     valor_sum_mayores[j]= sum(valores)
#     nro_transac[j]=nrow(persona)
#     if(sum(valores>=34300)>=1)
#     {alerta_12[j]=1}
#     else {alerta_12[j]=0}
#
#   }
#   else{
#     valor_max[j]= max(persona$Importe_Bs)
#     valor_sum_mayores[j]= sum(persona$Importe_Bs)
#     alerta_12[j]=0
#     nro_transac[j]=nrow(persona)
#   }
#
# }
# #
# unido= data.frame(doc_id=unique(Base_Usuarios$NUMERO_DOCUMENTO),valor_max,valor_sum_mayores,
#                   alerta_12,nro_transac)
############ CODIGO MOD #######
valores=NULL
valor_max=NULL
valor_sum_mayores=NULL
alerta_12=NULL
nro_transac=NULL
for (j in 1:length(unique(Base_Usuarios$NUMERO_DOCUMENTO))) {
  print(j)
  persona= Base_Usuarios %>% filter(NUMERO_DOCUMENTO==unique(Base_Usuarios$NUMERO_DOCUMENTO)[j])
   # round(as.numeric(summary(persona$Fecha)[[6]]-summary(persona$Fecha)[[1]])/12)
   # seq.Date(summary(persona$Fecha)[[1]], summary(persona$Fecha)[[6]], by=12)
   # seq.Date(summary(persona$Fecha)[[1]], summary(persona$Fecha)[[6]], by=12)-1
   # seq.Date(summary(persona$Fecha)[[1]], summary(persona$Fecha)[[6]], by=12)
  #
  valores=NULL
  c=0
  if(dim(persona)[1] >1)
  {
    for (i in 1:(as.numeric(summary(persona$Fecha)[[6]]-summary(persona$Fecha)[[1]])+1)) {
      intervalo_1=summary(persona$Fecha)[[1]]+c
      intervalo_2= intervalo_1+11
      valores[i]= persona %>% filter(Fecha>=intervalo_1 & Fecha<= intervalo_2) %>% select(Importe_Bs) %>% sum()
      c=c+1
    }
    valor_max[j]= max(valores)
    valor_sum_mayores[j]= sum(persona$Importe_Bs)
    nro_transac[j]=nrow(persona)
    if(sum(valores>=34300)>=1)
    {alerta_12[j]=1}
    else {alerta_12[j]=0}

  }
  else{
    valor_max[j]= sum(persona$Importe_Bs)
    valor_sum_mayores[j]= sum(persona$Importe_Bs)
    if(persona$Importe_Bs>=34300)
    {alerta_12[j]=1}
    else {alerta_12[j]=0}
    nro_transac[j]=1
  }

}
#
unido= data.frame(doc_id=unique(Base_Usuarios$NUMERO_DOCUMENTO),valor_max,valor_sum_mayores,
                  alerta_12,nro_transac)
#####
unido= unido %>% left_join(Base_Usuarios[,c(21,16)], by=c("doc_id"="NUMERO_DOCUMENTO"))
Base_limpia= unido  %>% group_by(doc_id) %>% filter(row_number()==1)
Base_limpia= as.data.frame(Base_limpia)
#
Base_limpia= Base_limpia %>% left_join(CONSOLIDADO_JUNIO_Listas_sf[,c(1,8,9,7)],by=c("numeros_carnet"="NUMERO_DOCUMENTO"))
Base_limpia= Base_limpia  %>% group_by(doc_id) %>% filter(row_number()==1)
#
Base_limpia= Base_limpia[,1:7]
Base_limpia= Base_limpia %>% left_join(CONSOLIDADO_JUNIO_Listas_sf[,c(1,8,9,7)], by=c("numeros_carnet"="NUMERO_DOCUMENTO"))
Base_limpia= Base_limpia  %>% group_by(doc_id) %>% filter(row_number()==1)
#

# https://cibercongreso2023.app.emi.edu.bo/#/

persona= Base_Usuarios %>% filter(NUMERO_DOCUMENTO %in% casos2$doc_id )
datos= persona %>% group_by(Origen) %>% summarise(total=sum(Importe_Bs), cantidad=n()) %>% arrange(desc(cantidad))

############### PARA LIMPIAR DATOS FINDER #############
Alertas= rest %>% filter(Detalle!="Alerta generada por JOB - Debida Diligencia pasa a pendiente.")
Alertas$Tipo_Persona[Alertas$Tipo_Persona==TRUE]="Natural"
Alertas$Tipo_Persona[Alertas$Tipo_Persona==FALSE]="Jurídica"
class(Alertas)
Alertas= Alertas[,1:20]
Alertas= as.data.frame(Alertas)
names(Alertas)[1:19]=c("Nro_ID_Cliente", "Nro_de_identificacion", "Tipo_Persona", "Nombre_completo", "Flujo_de_alerta",
  "Tipo_de_alerta", "Detalle", "Fecha_de_generacion", "Categoria_anterior",
  "Categoria_actual", "Fecha_de_cambio_categoria", "Usuario_en_proceso", "Usuario_de_finalizacion",
  "Perfil_alerta", "Inicio_periodo",  "Fin_periodo", "Valor_esperado",
  "Valor_operado", "Excedido")
#
Alertas= Alertas %>% mutate(Detalle1= Detalle)
Alertas= Alertas %>% filter(Detalle!="Alerta generada por cambio de perfil operativo - Debida Diligencia pasa a pendiente.")
Alertas= Alertas %>% separate(Detalle1,c("Caracteristica_1","Caracteristica_2"),
                              sep = " / ")
Alertas$Caracteristica_2= str_replace_all(Alertas$Caracteristica_2, " _","")
Alertas$Fecha_de_cambio_categoria=as.Date(Alertas$Fecha_de_cambio_categoria)
Alertas$Fecha_de_generacion= as.Date(Alertas$Fecha_de_generacion)
Alertas$Fin_periodo= as.Date(Alertas$Fin_periodo)
Alertas$Inicio_periodo= as.Date(Alertas$Inicio_periodo)
#
Alertas= Alertas %>% mutate(Flujo_de_alerta1= Flujo_de_alerta)
Alertas= Alertas %>% separate(Flujo_de_alerta1,c("Car_1","REGIONAL"),
                              sep = " - ")
Alertas$Caracteristica_2= str_replace(Alertas$Caracteristica_2,"_","")
Alertas= Alertas %>% mutate(REGIONAL= ifelse(is.na(REGIONAL), Car_1, REGIONAL))
Alertas= Alertas[,-24]
Alertas$REGIONAL[Alertas$REGIONAL=="Oriente"]="ORIENTE"
Alertas$REGIONAL[Alertas$REGIONAL=="Occidente"]="OCCIDENTE"
Alertas$REGIONAL[Alertas$REGIONAL=="Sur"]="SUR"
Alertas$REGIONAL[Alertas$REGIONAL=="El Alto"]="EL ALTO"
Alertas$REGIONAL[Alertas$REGIONAL=="Usuario"]="USUARIO"
Alertas$REGIONAL[Alertas$REGIONAL=="Usuarios"]="USUARIO"
Alertas$REGIONAL[Alertas$REGIONAL=="Centro"]="CENTRO"
#
Alertas$Caracteristica_2= str_replace(Alertas$Caracteristica_2, " _","")
Alertas$Caracteristica_2= str_replace(Alertas$Caracteristica_2, "\\( ","\\(")
Alertas$Caracteristica_2= str_replace(Alertas$Caracteristica_2, " _","")
Alertas$Caracteristica_2= str_replace(Alertas$Caracteristica_2, "_","")
Alertas$Caracteristica_2= str_replace(Alertas$Caracteristica_2, "- ","")
Alertas$Caracteristica_2= str_replace(Alertas$Caracteristica_2, "\t- ","")
Alertas$Caracteristica_2= str_replace(Alertas$Caracteristica_2, "\t","")
####
Alertas_1= Alertas %>% filter(`Estado de la Alerta`=="Pendiente")
Alertas_1= Alertas_1 %>% filter(Caracteristica_1!="Cantidad de Operaciones Mensuales")
Alertas_1$Nro_de_identificacion= str_replace(Alertas_1$Nro_de_identificacion,"SC","")
Alertas_1$Nro_de_identificacion= str_replace(Alertas_1$Nro_de_identificacion,"LP","")
Alertas_1$Nro_de_identificacion= str_replace(Alertas_1$Nro_de_identificacion,"CB","")
Alertas_1$Nro_de_identificacion= str_replace(Alertas_1$Nro_de_identificacion,"CH","")
Alertas_1$Nro_de_identificacion= str_replace(Alertas_1$Nro_de_identificacion,"TJ","")
Alertas_1$Nro_de_identificacion= str_replace(Alertas_1$Nro_de_identificacion,"OR","")
Alertas_1$Nro_de_identificacion= str_replace(Alertas_1$Nro_de_identificacion,"PO","")
Alertas_1$Nro_de_identificacion= str_replace(Alertas_1$Nro_de_identificacion,"BE","")
Alertas_1$Nro_de_identificacion= str_replace(Alertas_1$Nro_de_identificacion,"PA","")
Alertas_1= Alertas_1 %>% mutate(corre=1:nrow(Alertas_1))
##
Base_pcc= Base_PCC_2024_02[,c(8:10,13,21,22,26:29,31:33,51,52,58,65,66,71:73)]
Base_pcc= Base_pcc %>% mutate(monto_usd= ifelse(is.na(`Datos_del_PCC:_Monto`),`Datos_del_PCC:_Monto_de_la_operacion2`, `Datos_del_PCC:_Monto`))
Base_pcc$monto_usd= as.numeric(Base_pcc$monto_usd)
Alertas_filt= Alertas_1 %>% left_join(Base_pcc, by=c("Nro_de_identificacion"="Cliente_(Natural):_Numero_de_Documento_de_Identidad_(Datos_de_la_persona)"))
Alertas_filt= Alertas_filt %>% mutate(diferencia= abs(monto_usd- Valor_operado))
Alertas_filt_1= Alertas_filt %>% filter(!is.na(diferencia) & diferencia<=50)
filtrados= Alertas_filt_1 %>% group_by(corre) %>% filter(row_number()==1)

#### caso diferencia de fechas finder 3370982LP fecha generacion 7/2/24 y real 8/2/24 ####

# Alertas_res=Alertas %>% count(Caracteristica_2, Fecha_de_generacion)
# Alertas_res_1=Alertas_res %>% spread(Fecha_de_generacion,n)
# Alertas_res=Alertas %>% count(Caracteristica_2, REGIONAL)
# Alertas_res_2=Alertas_res %>% spread( REGIONAL,n)
###
# table(Alertas$Categoria_actual)
# table(Alertas$Categoria_anterior)
# table(Alertas$Categoria_anterior, Alertas$Categoria_actual)
# sort(table(Alertas$Usuario_de_finalizacion), decreasing = T)
#### PARA

datos= datos %>% mutate(cod=paste( Nro_ID_Cliente))
datos= datos %>% mutate(corre=1:nrow(datos))
base= base %>% mutate(cod=paste( ID_Cliente))
datos1= datos %>% left_join(base[,c(4:6,8,11,23)],by="cod")
datos2= datos1 %>% filter(!is.na(Descripcion))
###
para_conteo= Alertas %>% filter(Categoria_actual!="Finalizada"&
                                  Categoria_anterior!="Finalizada")
table(para_conteo$Categoria_anterior, para_conteo$Categoria_actual)
sum(table(para_conteo$Categoria_anterior, para_conteo$Categoria_actual))
######
Base_Alertas= Alertas %>% filter(Fecha_de_generacion>=as.Date("2023-1-1"))
Base_Alertas= Base_Alertas %>% mutate(dias_cierre= as.numeric(Fecha_de_cambio_categoria- Fecha_de_generacion))
cat_dias= NULL
for (i in 1:nrow(Base_Alertas)) {
  if(Base_Alertas$dias_cierre[i]<=15)
    cat_dias[i]="1 - 15"
  else if(Base_Alertas$dias_cierre[i]>15 & Base_Alertas$dias_cierre[i]<=30)
    cat_dias[i]="16 - 30"
  else if(Base_Alertas$dias_cierre[i]>30 & Base_Alertas$dias_cierre[i]<=45)
    cat_dias[i]="31 - 45"
  else if(Base_Alertas$dias_cierre[i]>45 & Base_Alertas$dias_cierre[i]<=60)
    cat_dias[i]="46 - 60"
  else if(Base_Alertas$dias_cierre[i]>60 & Base_Alertas$dias_cierre[i]<=90)
    cat_dias[i]="61 - 90"
  else if(Base_Alertas$dias_cierre[i]>90)
    cat_dias[i]="> 90"
}
Base_Alertas= data.frame(Base_Alertas, cat_dias)
Base_Alertas= Base_Alertas %>% mutate(fecha= paste(substr(Fecha_de_generacion,1,7),"1", sep = "-"))
Base_Alertas$fecha= as.Date(Base_Alertas$fecha)
Base_Alertas= Base_Alertas %>% mutate(TIPO= ifelse(REGIONAL=="USUARIO","Usuario","Cliente"))
Base_Alertas= Base_Alertas %>% filter(REGIONAL!="P. Defecto")
Base_Alertas= Base_Alertas[,-c(2:6,11)]
Base_Alertas= Base_Alertas %>% mutate(ESTADO=ifelse(Categoria_actual=="Finalizada","Finalizada","Pendiente"))
Base_cantidad= Alertas %>% mutate(gestion= substr(Fecha_de_generacion,1,4))
datos_cant= Base_cantidad %>% filter(REGIONAL!="P. Defecto" & gestion>=2022) %>%  count(gestion, REGIONAL)
#
Base_Alertas= Base_Alertas %>% mutate(corre=1:nrow(Base_Alertas))
Base_Alertas= Base_Alertas %>% left_join(Base_Clientes_BSol_vf[ ,c(11,34:36)],by=c("Nro_ID_Cliente"="USERID_TITULAR"))
Base_Alertas= Base_Alertas %>% group_by(corre) %>% filter(row_number()==1)
#
save(Base_Alertas, file = "Base_Alertas.RData")
save(datos_cant, file = "datos_cant.RData")
#
load("Base_Alertas.RData")
load("datos_cant.RData")
#
hchart(datos_cant, "column" , hcaes(x=gestion, y=n, group=REGIONAL),
        stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.1f}%"
       ))  %>%
  hc_add_theme(hc_theme_google())  %>%
  hc_subtitle(text="Alertas por gestión") %>%
  hc_colors(c("#E75263FF","#F8850FFF","red","green","maroon","#38AAACFF","#FDE725FF","#56147DFF"))
##############
casos_fun= Base_Alertas %>% filter(Usuario_de_finalizacion=="maldana")
casos_fun= casos_fun %>% count(Categoria_anterior)
casos_fun$n= round((casos_fun$n/sum(casos_fun$n))*100,1)
casos_fun= casos_fun %>% spread(Categoria_anterior,n)
table(casos_fun$Categoria_anterior)
casos_fun %>%
  c3() %>%
  c3_gauge(title = 'Colours' ,pattern = c("#60B044", "#52C7ADFF", "#F97600","#FF0000"
                                       ),
           threshold = list(unit="value", max=100,values=c(30,
                                                            50 ,80, 100)))
###
casos_fun= Base_Alertas %>% filter(Usuario_de_finalizacion=="apanca")
casos_fun= casos_fun %>% count(Categoria_anterior, cat_dias)
names(casos_fun)[2]="Dias"
names(casos_fun)[3]="Cantidad"
hchart(casos_fun, "column" , hcaes(x=Dias, y=Cantidad, group=Categoria_anterior),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.1f}%"
       ))  %>%
  hc_add_theme(hc_theme_google())  %>%
  hc_subtitle(text="Cierre de alertas en días") %>%
  hc_colors(c("#E75263FF","#F8850FFF","red","green","maroon","#38AAACFF","#FDE725FF","#56147DFF"))
#############
casos_fun= Base_Alertas %>% filter(Usuario_de_finalizacion=="etorricos")
casos_fun= casos_fun %>% count(Categoria_anterior)
hchart(casos_fun , "pie",innerSize = 200, hcaes(x = Categoria_anterior, y = n),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         , # Añadir etiquetas
         format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())%>%
  hc_colors(c("#E75263FF","#F8850FFF","red","green","maroon","#38AAACFF","#FDE725FF","#56147DFF"))
##############
casos_fun= Base_Alertas %>% filter(Usuario_de_finalizacion==c("epaz","apanca"))
casos_fun= casos_fun %>% mutate(fecha1= paste(substr(Fecha_de_cambio_categoria,1,7),"1", sep = "-"))
casos_fun= casos_fun %>% count(fecha1, Usuario_de_finalizacion) %>% arrange(fecha1)

#casos_fun= data.frame(casos_fun, Cierre="Cierre de Alertas")
hchart(casos_fun, "spline" , hcaes(x=fecha1, y=n, group=Usuario_de_finalizacion),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.1f}%"
       ))  %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_colors(c("#F8850FFF","red","green","maroon")) %>%
  hc_subtitle(text="Estado de Alertas")
## para dia
casos_fun= casos_fun %>% count(Fecha_de_cambio_categoria)
casos_fun= data.frame(casos_fun, Cierre="Cierre de Alertas")
hchart(casos_fun, "spline" , hcaes(x=Fecha_de_cambio_categoria, y=n, group=Cierre),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.1f}%"
       ))  %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_colors(c("red","green","maroon")) %>%
  hc_subtitle(text="Estado de Alertas") %>% hc_rangeSelector(enabled= TRUE, verticalAlign = "top")
##################
Tipo_de_Oficina_1
casos_fun= Base_Alertas %>% filter(Usuario_de_finalizacion==c("epaz","apanca"))
casos_fun= casos_fun %>% mutate(fecha1= paste(substr(Fecha_de_cambio_categoria,1,7),"1", sep = "-"))
casos_fun= casos_fun %>% count(cat_dias, Usuario_de_finalizacion)
hchart(casos_fun, "column" , hcaes(x=cat_dias, y=n, group=Usuario_de_finalizacion),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         , # Añadir etiquetas
         format = "{point.percentage:.1f}%"
       ))  %>%
  hc_add_theme(hc_theme_google())  %>%
  hc_subtitle(text="Cierre de alertas en días") %>%
  hc_colors(c("#E75263FF","#F8850FFF","red","green","maroon","#38AAACFF","#FDE725FF","#56147DFF"))
##################  PARA MAPAS CON LEAFLET ##########
library(leaflet)
primer_mapa= leaflet() %>% addTiles()

#Aquí debe colocar el nombre de usuario de su cuenta en GeoNames
options(geonamesUsername="Deyvis")
library(geonames)

# Lista de las ciudades de Colombia
ciudades <- c("La Paz", "Cochabamba", "Santa Cruz")

# conveninence function to look up and format results
GNsearchAF <- function(city) {
  res <- GNsearch(name=city, country="BO")
  return(res[1,c("name","adminName1","population","lng","lat")])
}

# loop over city names and reformat
GNresult <- do.call(rbind, lapply(ciudades, GNsearchAF))
colnames(GNresult) <- c("Ciudad", "Departamento", "Población", "Longitud", "Latitud")
GNresult[,3:5] <- sapply(GNresult[,3:5], as.numeric)


leaflet() %>% addProviderTiles(providers$Stamen.Toner)

####
library(leaflet)
library(sf)
library(RColorBrewer)
library(tidyverse)
library(htmltools)
arch_ubicaciones
#shape= st_read(system.file("d:/Documentos/shape/gadm41_BOL_1.shp", package = "sf"))
#arch_shape= st_read("gadm41_BOL_1.shp")
### por departamento
arch_shape=read_sf("d:/Documentos/shape/gadm41_BOL_1.shp")
arch_shape$NAME_1= toupper(arch_shape$NAME_1)
arch_shape$NAME_1= str_replace(arch_shape$NAME_1,"Í","I")
### por provincia
arch_shape=read_sf("d:/Documentos/shape/gadm41_BOL_2.shp")
### por municipio
arch_shape=read_sf("d:/Documentos/shape/gadm41_BOL_3.shp")
### solamente un punto bolivia
arch_shape=read_sf("d:/Documentos/shape/gadm41_BOL_0.shp")

### para el mapa de riesgos del dosier
datos_municipios= Datos_estadisticas_Bolivia %>% count(MUNICIPIO, wt=CANTIDAD)
arch_shape$NAME_3= toupper(arch_shape$NAME_3)
arch_shape= arch_shape %>% left_join(datos_municipios, by=c("NAME_3"="MUNICIPIO"))
paleta<-colorBin(palette = 'Reds',domain = unique(arch_shape$n),bins =unique(arch_shape$n) )
##
load("arch_shape.RData")
load("arch_ubicaciones.RData")
library(tidyverse)
library(htmltools)
library(leaflet)
library(sf)
paleta<-colorBin(palette = 'Reds',domain = unique(arch_shape$CANTIDAD1),bins =unique(arch_shape$CANTIDAD1) )
##addProviderTiles(providers$Esri.WorldImagery, options= providerTileOptions(opacity = 0.99))
## Colores Paired, Pastel1, Pastel2, Dark2, Accent
#lng = -64.9, lat =-14.83333
# BN ,LP ,SC
lg=c(-68.15,-64.9,-63.18117, -68.76918, -66.1568, -67.15, -65.26274, -64.72956, -65.75306)
lat=c(-16.5,-14.83333,-17.78629,-11.02671, -17.3895, -17.98333,-19.03332, -21.53549, -19.58361)
coorden=c("LA PAZ","BENI","SANTA CRUZ","PANDO","COCHABAMBA", "ORURO", "CHUQUISACA", "TARIJA", "POTOSI")
coorden_data= data.frame(cbind(lg,lat, coorden))
leaflet(arch_shape) %>% addTiles() %>%
                       addPolygons(
                         weight = 2, stroke = 1,
                                              color =~paleta(n) ,
                                              #fillColor = ~brewer.pal(nrow(arch_shape),"Paired"),
                                              fillOpacity = 0.7,
                                              dashArray = "3",
                                              label = ~NAME_1,
                                              popup = ~paste("Municipio", NAME_3,":",n,
                                                             "<br/>",
                                                             "Departamento:", NAME_1),
                                              highlight = highlightOptions(
                                                weight = 2,
                                                dashArray = "",
                                                color = "blue",
                                                bringToFront = TRUE
                                              )
                                   ) %>% addAwesomeMarkers(~arch_ubicaciones$Longitud,
                                                    ~arch_ubicaciones$Latitud,
                                                    label = ~htmlEscape(arch_ubicaciones$Nombre_Oficina),
                                                                popup = paste("Calificación de Riesgo:",arch_ubicaciones$Calificacion_Riesgo,
                                                                              "<br/>",
                                                                              "Tipo de Oficina:",arch_ubicaciones$Tipo_de_Oficina,
                                                                              "<br/>",
                                                                              "Nombre de Oficina:",arch_ubicaciones$Nombre_Oficina,
                                                                              "<br/>",
                                                                              "Ciudad:",arch_ubicaciones$Ciudad,
                                                                              "<br/>",
                                                                              "Direccion:",arch_ubicaciones$Direccion,
                                                                              "<br/>"),
                                                    icon = icons,
                                                    clusterOptions = markerClusterOptions()) %>%
  addControl("asdsa",position = "topright")
####
df.20 <- quakes[1:20,]

#
getColor <- function(arch_ubicaciones) {

  sapply(arch_ubicaciones$Calificacion_Riesgo, function(Calificacion_Riesgo) {
    if(Calificacion_Riesgo == 1) {
      "green"
    } else if(Calificacion_Riesgo == 2) {
      "orange"
    } else {
      "red"
    } })
}
#
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(arch_ubicaciones)
)
## prueba departamento
datos_clientes_dep_2= BASE_CLIENTES_NATURALES_01 %>% count(Regional_nombre, CLASIFICACION_TIPO_RIESGO)
#
arch_shape_dep= arch_shape %>% filter(NAME_1=="CHUQUISACA")
datos_sel= datos_clientes_dep_2 %>% filter(Regional_nombre=="CHUQUISACA")
coorden_data= data.frame(cbind(lg,lat, coorden))
arch_shape_dep= cbind(arch_shape_dep, datos_sel[,2:3] %>% spread(CLASIFICACION_TIPO_RIESGO,n) )
#
leaflet(arch_shape_dep) %>% addTiles() %>%
  addPolygons(
    weight = 1, stroke = TRUE,
   # color = "white",
   # fillColor = ~brewer.pal(nrow(arch_shape),"Paired"),
   fillColor = "#51d1f6",
    fillOpacity = 0.2,
    dashArray = "3",
    label = ~NAME_1,
    popup = ~paste("Clienes de Alto Riesgo:", ALTO,
                   "<br/>",
                   "Clienes de Medio Riesgo:", MEDIO,
                   "<br/>",
                   "Clienes de Bajo Riesgo:", BAJO,
                   "<br/>",
                   "ROS:","% en proceso"),
    highlight = highlightOptions(
      weight = 2,
      dashArray = "",
      color = "red",
      bringToFront = TRUE
    )
  ) %>%  addAwesomeMarkers(~arch_ubicaciones$Longitud,
                              ~arch_ubicaciones$Latitud,
                              label = ~htmlEscape(arch_ubicaciones$Nombre_Oficina),
                              popup = paste("Calificación de Riesgo:",arch_ubicaciones$Calificacion_Riesgo,
                                            "<br/>",
                                            "Tipo de Oficina:",arch_ubicaciones$Tipo_de_Oficina,
                                            "<br/>",
                                            "Nombre de Oficina:",arch_ubicaciones$Nombre_Oficina,
                                            "<br/>",
                                            "Ciudad:",arch_ubicaciones$Ciudad,
                                            "<br/>",
                                            "Direccion:",arch_ubicaciones$Direccion,
                                            "<br/>"),
                              icon = icons)
####### req ASFI ##########
#BASE_A1= rbind(BASE_A1, BASE_A1_2)
#save(BASE_A1, file = "BASE_A1.RData")
ASFI_PC_TRN_CLIENTES_012023_p1_v1= as.data.frame(ASFI_PC_TRN_CLIENTES_012023_p1_v1)
nombres= names(ASFI_PC_TRN_CLIENTES_012023_p1_v1)
names(ASFI_PC_TRN_CLIENTES_012023_p1_v1)= substr(nombres,4,nchar(nombres))
class(ASFI_PC_TRN_CLIENTES_012023_p1_v1)
######
skim(ASFI_PC_TRN_CLIENTES_012023_p1_v1)



library(readr)
Enero_2022 <- read_delim("D:/Nueva carpeta1/pcc/2022/Enero.csv",
                         delim = "|", escape_double = FALSE, col_names = FALSE,locale = locale(encoding = "ISO-8859-1"),
                         trim_ws = TRUE)

Base_202_23= rbind(Enero_2022, Febrero_2022, Marzo_2022, Abril_2022, Mayo_2022, Junio_2022,
                   Julio_2022, Agosto_2022, Septiembre_2022, Octubre_2022, Noviembre_2022, Diciembre_2022,
                   Enero_2023, Febrero_2023, Marzo_2023, Abril_2023, Mayo_2023, Junio_2023,
                   Julio_2023, Agosto_2023, Septiembre_2023)
names(Base_202_23)= names(Libro2)

names(base)= names(Libro2)
Base_202_23_c= rbind(Base_202_23, base)
#
#
Base_202_23_c= Base_202_23_c %>% mutate(cod= paste(Identificador_unico_enviado_por_el_Sujeto_Obligado,Fecha_Transaccion))

BASE_A3_ASFI_PC_FORMULARIOS_PCC_v1= BASE_A3_ASFI_PC_FORMULARIOS_PCC_v1 %>% mutate(cod= paste(NRO_FORM, ))


persona= BASE_A3_ASFI_PC_FORMULARIOS_PCC_v1 %>% filter(cod=="637397 49929.25")

base_extrac= Base_202_23 %>% filter(!(cod %in% BASE_A3_ASFI_PC_FORMULARIOS_PCC_v1$cod) )

####
BASE_A3_ASFI_PC_FORMULARIOS_PCC_1= BASE_A3_ASFI_PC_FORMULARIOS_PCC_1 %>% mutate(gestion= substr(FECHA_FORM,7, nchar(FECHA_FORM)))
BASE_A3_ASFI_PC_FORMULARIOS_PCC_1= BASE_A3_ASFI_PC_FORMULARIOS_PCC_1 %>% mutate(mes= substr(FECHA_FORM,4, 5))
#
BASE_A3_ASFI_PC_FORMULARIOS_PCC_1 %>% count(gestion, mes)
#
datos= BASE_A3_ASFI_PC_FORMULARIOS_PCC_v1 %>% filter(gestion==2022 & mes=="01")
datos1= Enero_2022 %>% filter(!(Identificador_unico_enviado_por_el_Sujeto_Obligado %in% datos$NRO_FORM))

#####
BASE_CLIENTES_NATURALES_02 <- read_delim("D:/Nueva carpeta1/ReqASFI/A1. Base de datos de Clientes Personas Naturales/9_11_hr_14/BASE_A1_ASFI_PC_CLIENTES_NATURALES_02_.csv",
                                          delim = "^", quote = "", escape_double = FALSE,
                                          col_types = cols(CODIGO = col_character()),
                                          locale = locale(encoding = "ISO-8859-1"),
                                          trim_ws = TRUE)
#
BASE_CLIENTES_NATURALES_01= rbind(BASE_CLIENTES_NATURALES_01, BASE_CLIENTES_NATURALES_02)
BASE_CLIENTES_NATURALES_01$CODIGO= as.numeric(BASE_CLIENTES_NATURALES_01$CODIGO)
#save(BASE_CLIENTES_NATURALES, file = "BASE_CLIENTES_NATURALES.RData")
sum(str_detect(BASE_CLIENTES_NATURALES_01$CODIGO ,"\\\""))
#
BASE_CLIENTES_NATURALES_01= BASE_CLIENTES_NATURALES_01 %>% left_join(Base_Clientes_BSol_vf[
  ,c(11,34:36)],by=c("CODIGO"="USERID_TITULAR"))
BASE_CLIENTES_NATURALES_01= BASE_CLIENTES_NATURALES_01 %>% group_by(corre) %>% filter(row_number()==1)
##
BASE_CLIENTES_NATURALES_01$Regional_nombre[BASE_CLIENTES_NATURALES_01$Regional_nombre=="SUCRE"]="CHUQUISACA"
BASE_CLIENTES_NATURALES_01= BASE_CLIENTES_NATURALES_01 %>% filter(!is.na(Regional_nombre))
datos_clientes_dep_1= BASE_CLIENTES_NATURALES_01 %>% count(Regional_nombre, CLASIFICACION_TIPO_RIESGO,
                                                         OPERACION_ASOCIADA_ALTA)
datos_clientes_dep_1= datos_clientes_dep_1 %>% filter(!is.na(OPERACION_ASOCIADA_ALTA))
#
datos_clientes_dep_2= datos_clientes_dep_1 %>% count(Regional_nombre, CLASIFICACION_TIPO_RIESGO, wt=n)
###
datos_clientes_dep_3= datos_clientes_dep_1 %>% count(CLASIFICACION_TIPO_RIESGO,
                                                   OPERACION_ASOCIADA_ALTA, wt=n)
ord=NULL
for (i in 1:nrow(datos_clientes_dep_3)) {
  if(datos_clientes_dep_3$CLASIFICACION_TIPO_RIESGO[i]=="BAJO")
    ord[i]=1
  else if(datos_clientes_dep_3$CLASIFICACION_TIPO_RIESGO[i]=="MEDIO")
    ord[i]=2
  else ord[i]=3
}
datos_clientes_dep_3= data.frame(datos_clientes_dep_3, ord)
datos_clientes_dep_3= datos_clientes_dep_3 %>% arrange(ord)
hchart(datos_clientes_dep_3, "bar" , hcaes(x=OPERACION_ASOCIADA_ALTA, y=n, group=CLASIFICACION_TIPO_RIESGO),
        #stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.1f}%"
       ))  %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_colors(c("#E75263FF","#F8850FFF","red","green","maroon","#38AAACFF","#FDE725FF","#56147DFF")) %>%
  hc_subtitle(text="Clasificación de clientes por producto")
#############
names(Base_Alertas)
names(Base_Clientes_BSol_vf)
Base_Alertas= Base_Alertas %>% mutate(corre= 1:nrow(Base_Alertas))
Base_Alertas= Base_Alertas %>% left_join(Base_Clientes_BSol_vf[,c(11,34:36)],
                                         by=c("Nro_ID_Cliente"="USERID_TITULAR"))
Base_Alertas= Base_Alertas %>% group_by(corre) %>% filter(row_number()==1)
###
library(leafdown)
Leafdown$new()
####

arch_shape_dep= arch_shape %>% filter(NAME_1=="SANTA CRUZ")
datos_sel= datos_clientes_dep_2 %>% filter(Regional_nombre=="SANTA CRUZ")
coorden_data= data.frame(cbind(lg,lat, coorden))
arch_shape_dep= cbind(arch_shape_dep, datos_sel[,2:3] %>% spread(CLASIFICACION_TIPO_RIESGO,n) )
#
leaflet(arch_shape_dep) %>% addTiles() %>%
  addPolygons(
    weight = 1, stroke = TRUE,
    # color = "white",
    # fillColor = ~brewer.pal(nrow(arch_shape),"Paired"),
    fillColor = "#51d1f6",
    fillOpacity = 0.2,
    dashArray = "3",
    label = ~NAME_1,
    popup = ~paste("Clienes de Alto Riesgo:", ALTO,
                   "<br/>",
                   "Clienes de Medio Riesgo:", MEDIO,
                   "<br/>",
                   "Clienes de Bajo Riesgo:", BAJO,
                   "<br/>",
                   "ROS:","% en proceso"),
    highlight = highlightOptions(
      weight = 2,
      dashArray = "",
      color = "red",
      bringToFront = TRUE
    )
  ) %>% addMarkers(as.numeric(coorden_data[coorden==arch_shape_dep$NAME_1,][[1]]),
                   as.numeric(coorden_data[coorden==arch_shape_dep$NAME_1,][[2]]), label = ~htmlEscape(NAME_1),
                   popup = paste("asdas", "<br/>", "Prov"))
##
dat_arch_ubic= arch_ubicaciones %>% count(Departamento_, Tipo_de_Oficina)
dat_arch_ubic_1= dat_arch_ubic %>% spread(Tipo_de_Oficina,n)
##################### PARA ACTUALIZAR BASE INUSUALES Y ROS #######
length(Inusuales_a_nov_23$NUMERO_DE_DOCUMENTO)
names(Inusuales_a_nov_23)
names(Base_Clientes_BSol_vf)
Inusuales_a_nov_23= Inusuales_a_nov_23 %>% left_join(Base_Clientes_BSol_vf[,c(4,11)],
                                                     by=c("NUMERO_DE_DOCUMENTO"="NUMERO_DOCUMENTO"))
Inusuales_a_nov_23= Inusuales_a_nov_23 %>% group_by(NUMERO_DE_DOCUMENTO) %>% filter(row_number()==1)
##
Base_ROS <- read_excel("Base_ROS_a_nov_23.xlsx")
Base_ROS= as.data.frame(Base_ROS)
Base_ROS$Fecha_Envio= as.Date(Base_ROS$Fecha_Envio)
Base_ROS$Fecha_Identificacion_INUSUAL= as.Date(Base_ROS$Fecha_Identificacion_INUSUAL)
Base_ROS$Fecha_Nacimiento= as.Date(Base_ROS$Fecha_Nacimiento)
CASOS= Base_ROS %>% filter(Fecha_Envio>=as.Date("2022-1-1"))
######### RANGO EDAD ROS ###############
rango_edad= NULL
CASOS= CASOS %>% filter(!is.na(Edad))
for (i in 1:nrow(CASOS)) {
  if(CASOS$Edad[i]>=18 & CASOS$Edad[i]<=25)
    rango_edad[i]="18 - 25"
  else if(CASOS$Edad[i]>25 & CASOS$Edad[i]<=30)
    rango_edad[i]="26 - 30"
  else if(CASOS$Edad[i]>30 & CASOS$Edad[i]<=35)
    rango_edad[i]="31 - 35"
  else if(CASOS$Edad[i]>35 & CASOS$Edad[i]<=45)
    rango_edad[i]="36 - 45"
  else if(CASOS$Edad[i]>45 & CASOS$Edad[i]<=55)
    rango_edad[i]="46 - 55"
  else if(CASOS$Edad[i]>55)
    rango_edad[i]=">55"
}
CASOS= data.frame(CASOS, rango_edad)
CASOS= CASOS %>% mutate(gestion= substr(Fecha_Envio,1,4))
names(CASOS)
names(BASE_INUSUALIDADES)
BASE_INUSUALIDADES_1= BASE_INUSUALIDADES %>% filter(ROS=="SI")
CASOS= CASOS %>% left_join(BASE_INUSUALIDADES_1[,c(2,16:18)], by=c("CI"="NUMERO_DE_DOCUMENTO"))
CASOS= CASOS %>% group_by(CI) %>% filter(row_number()==1)
CASOS= as.data.frame(CASOS)
CASOS= CASOS %>% mutate(Producto= ifelse(is.na(PASIVOS), ACTIVOS, paste(ACTIVOS, PASIVOS, sep = "-")))
######



ros_por_gest= CASOS %>% filter(gestion==2023) %>%  count(Producto) %>% arrange(desc(n))
names(ros_por_gest)[2]="Cantidad"
ros_por_gest= data.frame(ros_por_gest, Reporte="ROS")
hchart(ros_por_gest , "bar" ,hcaes(x=Producto, y=Cantidad, group=Reporte),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_elementary()) %>% hc_title(text="Persona Natural")%>%
  hc_colors(c("#f6a700","#E75263FF","#F8850FFF","#4b0082","maroon","#38AAACFF","#FDE725FF"))



#### 2106131-1P  15/09/1955 ####


"#56147DFF" #56147DFF
#####################################
arch_ubicaciones_2= arch_ubicaciones_2[,-8]
arch_ubicaciones_2= arch_ubicaciones_2 %>% left_join(arch_ubicaciones[,c(9,8)], by=c("N_PAF"="N_PAF"))
#
arch_ubicaciones_2$Calificacion_Riesgo[is.na(arch_ubicaciones_2$Calificacion_Riesgo)]=1
sum(is.na(arch_ubicaciones_2$Calificacion_Riesgo))
arch_ubicaciones_2= arch_ubicaciones_2 %>% mutate(Calificacion_Riesgo_1= Calificacion_Riesgo)
arch_ubicaciones_2$Calificacion_Riesgo_1[arch_ubicaciones_2$Calificacion_Riesgo_1==1]="BAJO RIESGO"
arch_ubicaciones_2$Calificacion_Riesgo_1[arch_ubicaciones_2$Calificacion_Riesgo_1==2]="MEDIO RIESGO"
arch_ubicaciones_2$Calificacion_Riesgo_1[arch_ubicaciones_2$Calificacion_Riesgo_1==3]="ALTO RIESGO"



##### ENLACES DE FOTOS DE PRUEBA PARA MAPA ####
#https://lh5.googleusercontent.com/p/AF1QipMkhYaT51etbIm5XgZ7uWLFDVDmIMGTpfE7mUzU=w512-h240-k-no

#https://www.google.com/maps/place/BancoSol+ATM+Central/@-16.5025043,-68.1346375,3a,75y,67.18h,82.36t/data=!3m7!1e1!3m5!1s5-MNCKwG5IXY3C3i5eYLqA!2e0!6shttps:%2F%2Fstreetviewpixels-pa.googleapis.com%2Fv1%2Fthumbnail%3Fpanoid%3D5-MNCKwG5IXY3C3i5eYLqA%26cb_client%3Dmaps_sv.tactile.gps%26w%3D224%26h%3D298%26yaw%3D35.405407%26pitch%3D0%26thumbfov%3D100!7i13312!8i6656!4m9!3m8!1s0x915f20708219af77:0x3c8bdb29fb8da6a0!8m2!3d-16.5024446!4d-68.1345854!10e5!14m1!1BCgIgARICCAI!16s%2Fg%2F11dx9gk1pl?entry=ttu#

#https://bsolbo-my.sharepoint.com/:i:/r/personal/dmamaniq_bancosol_com_bo/Documents/BANCOSOL.png?csf=1&web=1&e=rtTVKA

#https://drive.google.com/file/d/1j5TavEJ0sNKwDySOlgnrq0DNPUnmsbW8
#https://drive.google.com/uc?export=view&id=1j5TavEJ0sNKwDySOlgnrq0DNPUnmsbW8
#https://drive.google.com/file/d/1rBery6yrhJTmsGvFnLmsa9VyaiaSRLfZ
#https://drive.google.com/uc?export=view&id=1rBery6yrhJTmsGvFnLmsa9VyaiaSRLfZ
## ESTA PRIMERA SIRVER PARA EXPORTAR LA IMAGEN CAMBIARLO DESDE ID
#https://drive.google.com/uc?export=view&id=
#########
#https://drive.google.com/file/d/1hqw5kghO3qeeTKm5mfIImQ25rBdq4ZIQ/view?usp=sharing
#https://drive.google.com/uc?export=view&id=1hqw5kghO3qeeTKm5mfIImQ25rBdq4ZIQ/view?usp=sharing

https://drive.google.com/file/d/1kFff3GwcT4cZPCJOWOdTf0MgwaIJCp6Y/view?usp=drive_link

https://drive.google.com/file/d/1yy4wUrUeUNA-cF2Gm6lrZIST_Qk23pKP/view?usp=sharing
https://drive.google.com/uc?export=view&id=1yy4wUrUeUNA-cF2Gm6lrZIST_Qk23pKP

https://drive.google.com/file/d/1yy4wUrUeUNA-cF2Gm6lrZIST_Qk23pKP/view?usp=sharing


https://drive.google.com/file/d/1Zn1uBYkFewshIJb7CmubNrwgHNiCUacd/view?usp=sharing
https://drive.google.com/uc?export=view&id=1Zn1uBYkFewshIJb7CmubNrwgHNiCUacd


https://drive.google.com/file/d/1Rjk04ePlmdfh3JVGBfzgBBPSjTOnBp-t/view?usp=sharing
https://drive.google.com/uc?export=view&id=1Rjk04ePlmdfh3JVGBfzgBBPSjTOnBp-t


https://drive.google.com/file/d/1kFff3GwcT4cZPCJOWOdTf0MgwaIJCp6Y/view?usp=drive_link
https://drive.google.com/uc?export=view&id=1kFff3GwcT4cZPCJOWOdTf0MgwaIJCp6Y


https://drive.google.com/file/d/1Rjk04ePlmdfh3JVGBfzgBBPSjTOnBp-t/view?usp=drive_link
https://drive.google.com/uc?export=view&id=1Rjk04ePlmdfh3JVGBfzgBBPSjTOnBp-t

#### NUEVO PARA IMAGENES GOOGLE Y SHINY ##########

https://drive.google.com/file/d/11vctbgIBzAgQyaxts0N05N64y6ASkJ7_/view?usp=sharing
#ESTE FORMATO
https://drive.google.com/thumbnail?id=11vctbgIBzAgQyaxts0N05N64y6ASkJ7_&sz=w1000

https://drive.google.com/file/d/1GnwXuhaXQSCr0xI97zPpIS1mLHLJY-IO/view?usp=sharing
https://drive.google.com/thumbnail?id=1GnwXuhaXQSCr0xI97zPpIS1mLHLJY-IO&sz=w1000

https://drive.google.com/file/d/1MTsPdDxlTQK7m5Pq6yd1ya7tw9uz1XVI/view?usp=sharing
https://drive.google.com/thumbnail?id=1MTsPdDxlTQK7m5Pq6yd1ya7tw9uz1XVI&sz=w1000

https://drive.google.com/file/d/1jlio0WBN78fvLP7z1imo6I9DX3fvnqrU/view?usp=sharing
https://drive.google.com/thumbnail?id=1jlio0WBN78fvLP7z1imo6I9DX3fvnqrU&sz=w1000

# la imagen por defecto
https://drive.google.com/file/d/1xl0oNTJBVh9eK6pKwhofJnT47_8KNjQj/view?usp=sharing
https://drive.google.com/thumbnail?id=1xl0oNTJBVh9eK6pKwhofJnT47_8KNjQj&sz=w1000

data(reuters, package = "kernlab")



text = paste(
  reuters[[1]])

textcld <- text %>%
  map(str_to_lower) %>%
  reduce(str_c) %>%
  str_split("\\s+") %>%
  unlist() %>%
  data_frame(word = .) %>%
  count(word, sort = TRUE) %>%
  anti_join(tidytext::stop_words)

hchart(textcld, "wordcloud", hcaes(name = word, weight = n))
#, spiral="archimedean"
hchart(casos_ag, "wordcloud" ,hcaes(name = Agencia, weight = `Total funcionarios`))



hchart(casos_ag , "pareto" ,hcaes(x=Agencia, y=`Total funcionarios`, group=porcent_aprob),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))

casos= resumen_cap %>% mutate(carac= ifelse(Calificacion_Final>=80,1,2))
casos=casos %>% count(AGENCIA, carac)

hchart(casos , "column" ,hcaes(x=AGENCIA, y=n, group=carac),
      #stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google()) %>%
  hc_colors(c("#3a0a5e","#F8850FFF","#FDE725FF","green","maroon","#38AAACFF"))%>%
  hc_subtitle(text="Aprobados y no aprobados por Agencia")

####
casos=resumen_cap %>% count(Riesgo_Final_funcionario)
casos= data.frame(casos, Tipo="Riesgo")
hchart(casos , "pie",
       innerSize = 80,
       hcaes(x = Riesgo_Final_funcionario, y = n, group=Tipo),
       stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         , # Añadir etiquetas
         format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.1f}%"
       )) %>% hc_add_theme(hc_theme_google()) %>%
  hc_colors(c("#3a0a5e","#F8850FFF","#FDE725FF","green","maroon","#38AAACFF"))%>%
  hc_subtitle(text="Funcionarios de acuerdo al riesgo de calificación")

###
hchart(density(resumen_cap$Calificacion_Final), type="area", color = "#3a0a5e", name = "Calificación")

##
res_por_gest= REPORTE_DIR %>% count(GESTION, RIESGO)
res_por_gest$RIESGO[res_por_gest$RIESGO=="BAJO"]="BAJO RIESGO"
names(res_por_gest)[3]="Cantidad"
hchart(res_por_gest , "column" ,hcaes(x=GESTION, y=Cantidad, group=RIESGO),
       #stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google()) %>%
  hc_colors(c("#F8850FFF","#FDE725FF","green","maroon","#38AAACFF"))%>%
  hc_subtitle(text="Evaluación del Riesgo por Gestión")
###
res_por_gest= REPORTE_DIR %>% count(GESTION, Listas_Sensibles_BDN)
names(res_por_gest)[3]="Cantidad"
hchart(res_por_gest , "bar" ,hcaes(x=GESTION, y=Cantidad, group=Listas_Sensibles_BDN),
       #stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google()) %>%
  hc_subtitle(text="Evaluación en listas sensibles por Gestión")%>%
  hc_colors(c("#F8850FFF","maroon","#3a0a5e","green","maroon","#38AAACFF"))


######## CRUCE DE INFORMACION PARA REPORTE DE CALIFICACIONES VS PERSONAL ###########
REPORTE_DE_CALIFICACIONES_MICA= as.data.frame(REPORTE_DE_CALIFICACIONES_MICA)
PERSONAL_POR_CARGOS_AGENCIAS_VF= as.data.frame(PERSONAL_POR_CARGOS_AGENCIAS_VF)
REPORTE_DE_CALIFICACIONES_MICA_1= REPORTE_DE_CALIFICACIONES_MICA %>% left_join(PERSONAL_POR_CARGOS_AGENCIAS_VF[,
                                                                                                               c(1:7)],
                                                                               by=c("NOMBRE_COMPLETO"="APELLIDOS_Y_NOMBRES"))
#
persona= Alertas %>% filter(`Fecha de Generación`>= as.Date("2023-1-1"))
persona1= persona %>% group_by(`Nro. ID de cliente`) %>% tally()
persona1= persona1 %>% filter(n>1)
resumen=persona1 %>% filter(n>=5)
resumen1= persona %>% filter(`Nro. ID de cliente` %in% resumen$`Nro. ID de cliente` )
resumen2=resumen1 %>% count(`Nro. ID de cliente`, `Estado de la Alerta`)
resumen2=resumen2 %>% spread(`Estado de la Alerta`, n)
sum(resumen2$`En Proceso`, na.rm = T)+sum(resumen2$Finalizada, na.rm = T)+sum(resumen2$Pendiente, na.rm = T)
resumen2= resumen2 %>% left_join(resumen, by="Nro. ID de cliente")
#
clase= NULL
for (i in 1:nrow(resumen2)) {
  if(resumen2$n[i] <=10)
    clase[i] ="5 - 10"
  else if(resumen2$n[i]>10 & resumen2$n[i]<=20)
    clase[i] ="11 - 20"
  else if(resumen2$n[i]>20 & resumen2$n[i]<=30)
    clase[i] ="21 - 30"
  else if(resumen2$n[i]>30 & resumen2$n[i]<=40)
    clase[i] ="31 - 40"
  else if(resumen2$n[i]>40 & resumen2$n[i]<=50)
    clase[i] ="41 - 50"
  else if(resumen2$n[i]>50 & resumen2$n[i]<=60)
    clase[i] ="11 - 20"
  else if(resumen2$n[i]>60 & resumen2$n[i]<=70)
    clase[i] ="11 - 20"
  else if(resumen2$n[i]>70 & resumen2$n[i]<=80)
    clase[i] ="11 - 20"
  else if(resumen2$n[i]>80 & resumen2$n[i]<=90)
    clase[i] ="11 - 20"
  else if(resumen2$n[i]>90 & resumen2$n[i]<=100)
    clase[i] ="11 - 20"
  else if(resumen2$n[i]>100)
    clase[i] ="> 100"
}
resumen2= data.frame(resumen2, clase)
resumen2$En.Proceso[is.na(resumen2$En.Proceso)]=0
resumen2$Finalizada[is.na(resumen2$Finalizada)]=0
resumen2$Pendiente[is.na(resumen2$Pendiente)]=0
#resumen3= resumen2 %>% count(clase, Finalizada, Pendiente, wt=n)
resumen3= resumen2 %>% count(clase, Finalizada, Pendiente, wt=n)
#
dato1=data.frame( resumen2 %>% count(clase, wt=En.Proceso),resumen2 %>% count(clase, wt=Finalizada),
                  resumen2 %>% count(clase, wt=Pendiente))
dato1= dato1[,-c(3,5)]
names(dato1)[2]="En_Proceso"
names(dato1)[3]="Finalizada"
names(dato1)[4]="Pendiente"
dato2=dato1 %>% gather(key = "Estado",value = "Cantidad",En_Proceso:Pendiente)
#
resumen3=resumen2 %>% count(clase)
resumen3= data.frame(resumen3, Tipo="Cliente")
resumen3= resumen3 %>% arrange(desc(n))
names(resumen3)[2]="Cantidad_Clientes"
hchart(resumen3, "bar" ,hcaes(x=clase, y=Cantidad_Clientes,group=Tipo),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
          enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))%>% hc_add_theme(hc_theme_elementary()) %>%
  hc_colors(c("#F8850FFF","maroon","#3a0a5e","green","maroon","#38AAACFF"))%>%
  hc_title(text="Cantidad de Clientes")
#
hchart(dato2, "bar" ,hcaes(x=clase, y=Cantidad,group=Estado),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))%>% hc_add_theme(hc_theme_elementary()) %>%
  hc_colors(c("red","#3a0a5e","#F8850FFF","maroon","#38AAACFF")) %>%
  hc_title(text="Cantidad de Alertas")
############### tipo de oficina para mapa ######################
Tipo_de_Oficina_1=NULL
for (i in 1:nrow(arch_ubicaciones)) {
  if(arch_ubicaciones$Tipo_de_Oficina[i]=="Oficina Central")
    Tipo_de_Oficina_1[i]=1
  else if(arch_ubicaciones$Tipo_de_Oficina[i]=="Agencia Fija"|arch_ubicaciones$Tipo_de_Oficina[i]=="Agencia Móvil"|
          arch_ubicaciones$Tipo_de_Oficina[i]=="Oficina Externa" | arch_ubicaciones$Tipo_de_Oficina[i]=="Sucursal")
    Tipo_de_Oficina_1[i]=2
  else if(arch_ubicaciones$Tipo_de_Oficina[i]=="Punto Corresponsal No Financiero")
    Tipo_de_Oficina_1[i]=3
  else if(arch_ubicaciones$Tipo_de_Oficina[i]=="ATM")
    Tipo_de_Oficina_1[i]=4
  else{Tipo_de_Oficina_1[i]=5}
}
arch_ubicaciones= data.frame(arch_ubicaciones, Tipo_de_Oficina_1)
#################################################################
load("Persona_Juridica_a_Sep_23.RData")
load("categorias.RData")
categ_transac= categorias %>% filter(!(variable %in% c("SPT","Transferencias a Ctas de 3ros","Transferencias a 3ros Empresas",
                                                     "Transferencias 3ros (Express)","BTS Transferencia Ctas de 3ros",
                                                     "BTS Transferencia 3ro Express")))
##
datos_transac= Reporte_2023_11 %>% filter(Descripcion %in% categ_transac$variable)
datos_transac= datos_transac %>% mutate(Importe_Depositos_usd= round(Importe_Depositos/6.86,0))
datos_transac= datos_transac %>% filter(Importe_Depositos_usd > 1)
datos_transac= datos_transac %>% filter(!(ID_Cliente %in% Persona_Juridica_a_Sep_23$CODIGO) )
#
categoria=NULL
for (i in 1:nrow(datos_transac)) {
  if(datos_transac$Importe_Depositos_usd[i]<=350)
    categoria[i]="Hasta 350"
  else if(datos_transac$Importe_Depositos_usd[i]> 350 & datos_transac$Importe_Depositos_usd[i]<=700)
    categoria[i]="350 - 700"
  else if(datos_transac$Importe_Depositos_usd[i]> 700 & datos_transac$Importe_Depositos_usd[i]<=1400)
    categoria[i]=">700 - 1400"
  else if(datos_transac$Importe_Depositos_usd[i]> 1400 & datos_transac$Importe_Depositos_usd[i]<=3500)
    categoria[i]=">1400 - 3500"
  else if(datos_transac$Importe_Depositos_usd[i]> 3500 & datos_transac$Importe_Depositos_usd[i]<=7000)
    categoria[i]=">3500 - 7000"
  else if(datos_transac$Importe_Depositos_usd[i]> 7000 & datos_transac$Importe_Depositos_usd[i]<=15000)
    categoria[i]=">7000 - 15000"
  else if(datos_transac$Importe_Depositos_usd[i]> 15000 & datos_transac$Importe_Depositos_usd[i]<=18000)
    categoria[i]=">15000 - 18000"
  else if(datos_transac$Importe_Depositos_usd[i]> 18000 & datos_transac$Importe_Depositos_usd[i]<=35000)
    categoria[i]=">18000 - 35000"
  else if(datos_transac$Importe_Depositos_usd[i]> 35000 & datos_transac$Importe_Depositos_usd[i]<=70000)
    categoria[i]=">35000 - 70000"
  else if(datos_transac$Importe_Depositos_usd[i]> 70000)
    categoria[i]=">70000"
}
table(categoria)
datos_transac= data.frame(datos_transac, categoria)
resumen=datos_transac %>% count(Agencia,categoria)
resumen_1= datos_transac %>% group_by(Agencia ,categoria) %>% summarise(Total= sum(Importe_Depositos_usd))
resumen= resumen %>% left_join(resumen_1, by=c("Agencia","categoria"))
resumen= resumen %>% filter(Agencia==295)
ordena= NULL
for (i in 1:nrow(resumen)) {
  if(resumen$categoria[i]=="Hasta 350")
    ordena[i]=1
  else if(resumen$categoria[i]=="350 - 700")
    ordena[i]=2
  else if(resumen$categoria[i]==">700 - 1400")
    ordena[i]=3
  else if(resumen$categoria[i]==">1400 - 3500")
    ordena[i]=4
  else if(resumen$categoria[i]==">3500 - 7000")
    ordena[i]=5
  else if(resumen$categoria[i]==">7000 - 15000")
    ordena[i]=6
  else if(resumen$categoria[i]==">15000 - 18000")
    ordena[i]=7
  else if(resumen$categoria[i]==">18000 - 35000")
    ordena[i]=8
  else if(resumen$categoria[i]==">35000 - 70000")
    ordena[i]=9
  else if(resumen$categoria[i]==">70000")
    ordena[i]=10
}
resumen= data.frame(resumen, ordena)
resumen= resumen %>% arrange(ordena)
resumen$Total= round(resumen$Total,0)
hchart(resumen %>% filter(categoria!="Hasta 350") , "column" ,hcaes(x=categoria, y=n,group=Agencia),
      #  stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_elementary()) %>%
  hc_colors(c("#38AAACFF","#3a0a5e","#F8850FFF","red","maroon")) %>%
  hc_title(text="Importe Depósitos")





datos=c("Entidad_Financiera_1","Tipo_Cuenta_1","Numero_de_Cuenta_1","Moneda_1","Saldo_1",
"Entidad_Financiera_2","Tipo_Cuenta_2","Numero_de_Cuenta_2","Moneda_2","Saldo_2","Entidad_Financiera_3",
"Tipo_Cuenta_3","Numero_de_Cuenta_3","Moneda_3","Saldo_3",
"Tipo_de_Bien_1","Identificacion_Bien_1","Con_deuda_sn_deuda_1","Fecha_Adquisicion_1",
"Valor_pat_1","Tipo_de_Bien_2","Identificacion_Bien_2","Con_deuda_sn_deuda_2","Fecha_Adquisicion_2",
"Valor_pat_2", "Tipo_de_Bien_3","Identificacion_Bien_3","Con_deuda_sn_deuda_3","Fecha_Adquisicion_3","Valor_pat_3",
"Tipo_de_Bien_nom_terc_1","Nombre_Titular_nom_terc_1","Identificacion_Bien_nom_terc_1",
"Con_deuda_sn_deuda_nom_terc_1","Fecha_Adquisicion_nom_terc_1","Valor_pat_nom_terc_1", "Tipo_de_Bien_nom_terc_2",
"Nombre_Titular_nom_terc_2","Identificacion_Bien_nom_terc_2",
"Con_deuda_sn_deuda_nom_terc_2","Fecha_Adquisicion_nom_terc_2","Valor_pat_nom_terc_2", "Tipo_de_Bien_nom_terc_3",
"Nombre_Titular_nom_terc_3","Identificacion_Bien_nom_terc_3",
"Con_deuda_sn_deuda_nom_terc_3","Fecha_Adquisicion_nom_terc_3","Valor_pat_nom_terc_3",
"Entidad_Persona_1","Concepto_1","Valor_pasivo_1","Cuota_Pago_Mensual_1",
"Entidad_Persona_2","Concepto_2", "Valor_pasivo_2", "Cuota_Pago_Mensual_2",
"Entidad_Persona_3","Concepto_3","Valor_pasivo_3", "Cuota_Pago_Mensual_3",
"Entidad_Institucion_1", "Calidad_Miembro_1",
"Entidad_Institucion_2", "Calidad_Miembro_2",
"Entidad_Institucion_3", "Calidad_Miembro_3",
"Corp_Soci_Asoc_1", "Porc_Part_Accion_1","Calidad_Socio_1",
"Corp_Soci_Asoc_2", "Porc_Part_Accion_2","Calidad_Socio_2",
"Corp_Soci_Asoc_3", "Porc_Part_Accion_3","Calidad_Socio_3",
"Detalle_Actividades_1","Forma_Participacion_1",
"Detalle_Actividades_2","Forma_Participacion_2",
"Detalle_Actividades_3", "Forma_Participacion_3")

aapl <- quantmod::getSymbols("AAPL",
                             src = "yahoo",
                             from = "2019-01-01",
                             auto.assign = FALSE
)
highchart(type = "stock") %>%
  hc_title(text = "AAPLE") %>%
  hc_add_series(aapl, yAxis = 0, showInLegend = TRUE) %>%
  hc_add_yAxis(nid = 1L, title = list(text = "Prices"), relative = 0) %>%
  hc_add_series(aapl[, "AAPL.Volume"], yAxis = 1, type = "column", showInLegend = FALSE) %>%
  hc_add_yAxis(nid = 2L, title = list(text = "Volume"), relative = 1)

res_por_gest= REPORTE_DIR %>% count(GESTION, Listas_Sensibles_BDN)
names(res_por_gest)[3]="Cantidad"
hchart(res_por_gest , "line" ,hcaes(x=GESTION, y=Cantidad, group=Listas_Sensibles_BDN),
       #stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google()) %>%
  hc_subtitle(text="Evaluación de Debida Diligencia a Directores")%>%
  hc_colors(c("#F8850FFF","maroon","#3a0a5e","green","maroon","#38AAACFF")) %>%
  hc_yAxis_multiples(
    list(title = list(text = "Cantidad"),
      opposite = TRUE,
      lineWidth = 0,
         minorGridLineWidth = 0,
         gridLineWidth = 0,
      max=60,
         plotBands = list(
           list(from = 0, to = 20, color = "#76cb47",
                label = list(text = "Bajo Riesgo")),
           list(from = 20, to = 40, color = "#ff8c00",
                label = list(text = "Medio Riesgo")),
           list(from = 40, to = 60, color = "#e8513c",
                label = list(text = "Alto Riesgo"))
         ),
      visible= TRUE
    ),
    list(title = list(text = "Nivel de Riesgo"),minorGridLineWidth = 0,gridLineWidth = 0,
         showLastLabel = T, opposite = FALSE))
#########

  hc_yAxis(title = list(text = ""),
           opposite = FALSE,
           minorTickInterval = "auto",
           minorGridLineDashStyle = "LongDashDotDot",
           #mostrar la primera etiqueta
           showFirstLabel = TRUE,
           #mostrar la ultima etiqueta
           showLastLabel = TRUE,
           #agregar una banda de área
           plotBands = list(
             list(from = 0, to = 5, color = "green",
                  label = list(text = "Area")))) %>%
  hc_yAxis(title = list(text = ""),
           opposite = FALSE,
           minorTickInterval = "auto",
           minorGridLineDashStyle = "LongDashDotDot",
           #mostrar la primera etiqueta
           showFirstLabel = TRUE,
           #mostrar la ultima etiqueta
           showLastLabel = FALSE,
           #agregar una banda de área
           plotBands = list(list(from = 0, to = 5, color = "red",
                                                    label = list(text = "Area"))))



"#FF0000" "#fff" "#ABD7E9" "#2071B5"
"rgba(100, 0, 0, 0.1)"


highchart() %>%
  hc_yAxis_multiples(create_axis(naxis = 2, heights = c(2, 1))) %>%
  hc_add_series(data = c(1, 3, 2), yAxis = 0) %>%
  hc_add_series(data = c(20, 40, 10), yAxis = 1)



highchart() %>%
  hc_yAxis_multiples(create_axis(naxis = 4, lineWidth = 2, title = list(text = NULL))) %>%
  hc_add_series(data = c(1, 3, 2)) %>%
  hc_add_series(data = c(20, 40, 10), type = "area", yAxis = 1) %>%
  hc_add_series(data = c(200, 400, 500), yAxis = 2) %>%
  hc_add_series(data = c(500, 300, 400), type = "areaspline", yAxis = 2)

######
load("REPORTE_DIR.RData")
##
REPORTE_DIR_1= REPORTE_DIR[,c(1,3,2,4,6,8,10,12,14)]
REPORTE_DIR_1[, 4] <-
  ifelse(REPORTE_DIR_1[, 4] == "SIN OBSERVACION", paste(REPORTE_DIR_1[, 4], fa("check-circle", fill =
                                                               "green")), paste(REPORTE_DIR_1[, 4], fa("exclamation-circle", fill = "orange")))
##
kbl(REPORTE_DIR_1, escape = FALSE) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = F
  ) %>%
  column_spec(4, color = "teal", bold = TRUE)


################## PARA COLOCAR ENLACES EN EL SERVER DE SHINY ##############
# else if(input$fun == "ESTEBAN ANDRÉS ALTSCHUL"){
#   img(height =600 , width = "95%", src = "https://drive.google.com/uc?export=view&id=1Zn1uBYkFewshIJb7CmubNrwgHNiCUacd")
# }
# else if(input$fun == "ESTEBAN ANDRÉS ALTSCHUL"){
#   img(height = 600, width = "95%", src = "https://drive.google.com/uc?export=view&id=1j5TavEJ0sNKwDySOlgnrq0DNPUnmsbW8")
# }
# else if(input$fun == "ESTEBAN ANDRÉS ALTSCHUL"){
#   img(height = 600, width = "95%", src = "https://drive.google.com/uc?export=view&id=1rBery6yrhJTmsGvFnLmsa9VyaiaSRLfZ")
# }
# else if(input$fun == "ESTEBAN ANDRÉS ALTSCHUL"){
#   tags$a(target = "_blank",href="https://drive.google.com/file/d/1Q4JKA2UOV1q_EjNGKeOhpf44Ji5QPHOS/view?usp=sharing", "Click aquí! Para el enlace del documento",
#   )
# }
# else if(input$fun == "ESTEBAN ANDRÉS ALTSCHUL"){
#   tags$a(target = "_blank",href="https://drive.google.com/file/d/1yy4wUrUeUNA-cF2Gm6lrZIST_Qk23pKP/view?usp=drive_link", "Click aquí! Para el enlace del documento",
#   )
# }
#ARCHIVOS PDF
https://drive.google.com/file/d/1Q4JKA2UOV1q_EjNGKeOhpf44Ji5QPHOS/view?usp=sharing
# IMAGEN
https://drive.google.com/uc?export=view&id=1rBery6yrhJTmsGvFnLmsa9VyaiaSRLfZ

https://drive.google.com/file/d/1Rjk04ePlmdfh3JVGBfzgBBPSjTOnBp-t/view?usp=drive_link
https://drive.google.com/uc?export=view&id=1Rjk04ePlmdfh3JVGBfzgBBPSjTOnBp-t


https://drive.google.com/file/d/1voO3dJNTAcKwz4VReTm_vEQzEfwB9EVd/view?usp=sharing
https://drive.google.com/file/d/1voO3dJNTAcKwz4VReTm_vEQzEfwB9EVd/view?usp=drive_link
https://drive.google.com/file/d/1voO3dJNTAcKwz4VReTm_vEQzEfwB9EVd/view?usp=drive_link
https://drive.google.com/uc?export=view&id=1voO3dJNTAcKwz4VReTm_vEQzEfwB9EVd
https://drive.google.com/uc?export=view&id=1voO3dJNTAcKwz4VReTm_vEQzEfwB9EVd

https://drive.google.com/file/d/1rYJmREi_DMlaWcPcqLHngQMIxutugT1u/view?usp=drive_link
https://drive.google.com/uc?export=view&id=1rYJmREi_DMlaWcPcqLHngQMIxutugT1u
######### letra javat y kamel ############
# Silencio porque uh, yeah, uh
# Silencio porque grabo, me suda el nabo
# Quién de esos pavos está a mi favor
# Pavor cuando las clavo, es agotador
# Ser domador y esclavo del público
# Pero soy único en esta labor, el amo
# Abrid los ojos y soñad, todo va muy deprisa
# Es preciosa y precisa una sonrisa
# En mi misa se levanta el vaso
# Me oirás cantar borracho en tus oídos borrachos
# Bebí del biberón de Cicerón y de Virgilio
# Hoy soy poeta con emoción a domicilio
# Escribo en el exilio voluntario
# Dado que siempre hay alguien que me llama para echar algo en el barrio
# Azuara tiene el tiempo envasado al vacío
# Disco móvil propia, inspiración en el río
# Azuara me tiene de crío, de nieto
# Y a las personas mayores paciencia y respeto
# ¡Eh tú! El amor está en el aire y a mí me va la locura
# Sampleamos el vinilo y su fritura, yo amo esta cultura
# Ni una puta duda que es un lío
# ¿Cuántos MCs ahí fuera están esperando un fallo mío?
#   Con esta presión, cada rima es un palacio
# Hay que escribir despacio, quiero causar sensación
# También quiero inyectarle ilusión al proyecto
# Y demostrar que el sonido perfecto es una anécdota
# Al lado del talento innato, yo aún compito
# Es más, yo soy el límite, le dije al micro
# "Co, estas manos que te palpan luego irán al pan de mi mejor fan, así que fluye"
# Dime quién me sustituye, di en qué mares te zambulles
# Cuando yo no te lloro desde el lodo
# Yo doy clases de retórica en su foro
# Y pido un micro de platino pa' mi público al mejor coro
# Me rallo con cada palabra
# Dicen: "¡Te rallas por todo, joder te rallas por nada!"
# Oka, no hay silencio en una ciudad y eso es muy duro
# Tan duro como vivir con este miedo al futuro
# Voy a gastar mi dinero en regalos
# Quiero dar vacaciones a mi ego y liberar el caos
# No guardaré ni un duro para el cementerio
# Si la conciencia pide cuentas tengo un proyecto paralelo
# Es un secreto, pero esta noche, no sé si voy a ser discreto
# Como que si esa chavala se pone a bailar
# Acabará bailando todo el bar, así es mi estilo
# Pero soy tímido y he asumido
# Tantos comentarios sobre mí que ya no veo nada nítido
# Trae aquí ese líquido, sea lo que sea soy el don
# ¿Yo un ligón? Non, yo borracho de ron
# Desde el principio y mis mejores alegrías
# Cuando el Aborto y los Bufank me aceptaban como a un homeboy más
# Unidos por el alcohol con gas, el bombo clap
# Crecidos en la escuela del ¡no podrás!
# De la Plaza Del Rollo y todos esos buenos rappers
# Que aún siguen haciendo de la ciudad del viento
# Un crudo referente, ¿sabes que estoy diciendo?
# ¿No? Pues vente, aquí es corriente
# Que el pavo más normal de un bar
# Sea el mejor Mc de su barrio y quien de las chicas lo va a notar
# So, sólo una bruja en una burbuja
# Será capaz de arriesgar, el mayor riesgo es no arriesgar
# Yo ya no escribo letras, ya
# Solo pienso frases que nunca me apetece rimar
# Con el sudor de los atletas y a base de oír bases
# Me derramo en esta puta incontinencia verbal
# Me gusta el funk, el reggae y el jazz
# Pero lo único, lo único que quieren es rap
# Vacilo como Too $hort, en este curso
# Soy el futuro, el precursor del hedonismo más puro
# Tengo fe en la fonética, me escuchas y me sientes
# Creo que no giramos en órbitas diferentes
# Ya que son patentes las fuentes de amor que generé en las gentes
# Mis letras son puentes suficientes
# Para unir los puertos, las costas, las playas
# Chica lleva nuestra cinta vayas donde vayas
# Yo no siempre supe lo que hacer
# Estuve herido de muerte, hoy mi dolor es fuente de placer
# Me escuchan jóvenes de espíritu, hablo de tú a tú
# Alguna absurda luz cegó a la juventud
# Los que nos aspiran a nada ni respiran
# Porque no les despidan
# Mientras puedan disfrutar de Zidane
# Y la fórmula funciona, la tele ilusiona
# Coma, drogan a la persona
# Luego lo difícil es no volverse loco
# Porque no hay moda que cure la soledad
# Yo escribo en este saco de boxeo de papel
# Escribo lo que siento y lo que veo también
# Que no sé criticar sin insultar también
# Que yo puedo hacer que las cosas cambien
# Yo solo necesito imaginar para tener
# Tengo un chalet en la playa de Babia que mantener
# ¡No es Jauja! me llaman Javat por algo
# Soy el guaja de marzo, me oirás cantar borracho en tus oídos borrachos
# Tarde en la noche me desplazo, buscando un regazo, tu regazo
# Porque encuentro tu belleza infinita
# Multiplicando los puntos de dónde mirarte y a dónde mírate chica
# Qué suerte va a ser la cariátide que sostenga mi cuerpo
# Mi cuerpo es el templo de los frágiles
# Si tan fácil es ser persona
# ¿Por qué a mí se me amontonan las deudas
# Por lo de "lo prometido es deuda"?
# Voy a tocar en el piano de tus costillas
# Sin hacerte cosquillas mi mejor pieza
# Jardín Imaginario allí mi pez
# Respira el aire puro descalzo sobre el fresco césped de tu esbeltez
# Las cosas se aprenden desordenadamente
# La mente teme, la mente miente
# Y ya veremos lo que hacemos con los sueños rehenes
# Sé que los tienes, ¡Va, dime que vienes!
# ¿Dónde está mi estilo?, ¿Dónde está mi ruina?
# Yo, si entro en el área me meto hasta en la cocina
# Idealista, más mi clientela es fina
# Y jóvenes de todas las edades fuman mi china
# Lo que siento no es traducible
# Mas mis palabras consiguen fecundar al oído sensible
# Es lo que queda, y alguno me cuenta su vida
# Que mi canción anestesió en parte su herida
# Yo no sé quién soy ni lo pretendiera
# "Ningún tipo de orden" es mi quinto alias
# ¡Coño yo soy la palmera que se dobla pero aguanta el huracán!
# Desde que escribimos un gran plan, ¿qué coño pensaban?
# Escúchame sentado con los ojos cerrados
# Si tu estado normal es cansado cercano al enfado
# Las cosas vendrán como nunca las habías pensado
# Ahora duerme, y escribe en un papel lo que has soñado
# Yo suelo hablar poco y cuando hablo hablo demasiado
# Y me equivoco en el cincuenta por ciento de mis no actos
# Muchos de mis pensamientos siguen intactos
# Tras los impactos, ey ¡No quiero sufrir!


##### para base EVALUACION CUMPLIMIENTO #####
RIESGO_INDIVIDUAL=NULL
for (i in 1:nrow(EVALUACION_CUMPLIMIENTO)) {
   if(EVALUACION_CUMPLIMIENTO$NOTA_FINAL[i]<80)
     RIESGO_INDIVIDUAL[i]="ALTO"
   else if(EVALUACION_CUMPLIMIENTO$NOTA_FINAL[i]>=80 & EVALUACION_CUMPLIMIENTO$NOTA_FINAL[i]<90)
     RIESGO_INDIVIDUAL[i]="MEDIO"
   else{
     RIESGO_INDIVIDUAL[i]="ALTO"
   }
}
EVALUACION_CUMPLIMIENTO= data.frame(EVALUACION_CUMPLIMIENTO, RIESGO_INDIVIDUAL)
RIESGO_INDIVIDUAL_1=NULL
for (i in 1:nrow(EVALUACION_CUMPLIMIENTO)) {
   if(EVALUACION_CUMPLIMIENTO$NOTA_FINAL[i]<80)
     RIESGO_INDIVIDUAL_1[i]=2.5
   else if(EVALUACION_CUMPLIMIENTO$NOTA_FINAL[i]>=80 & EVALUACION_CUMPLIMIENTO$NOTA_FINAL[i]<90)
     RIESGO_INDIVIDUAL_1[i]=1.5
   else{
     RIESGO_INDIVIDUAL_1[i]=0.5
   }
}
EVALUACION_CUMPLIMIENTO= data.frame(EVALUACION_CUMPLIMIENTO, RIESGO_INDIVIDUAL_1)
resumen=EVALUACION_CUMPLIMIENTO %>% group_by(AGENCIA) %>% summarise(RIESGO_AGENCIA_1= mean(RIESGO_INDIVIDUAL_1))
EVALUACION_CUMPLIMIENTO_1= EVALUACION_CUMPLIMIENTO %>% left_join(resumen, by="AGENCIA")
RIESGO_AGENCIA=NULL
for (i in 1:nrow(EVALUACION_CUMPLIMIENTO_1)) {
  if(EVALUACION_CUMPLIMIENTO_1$RIESGO_AGENCIA_1[i]<1)
    RIESGO_AGENCIA[i]="BAJO"
  else if(EVALUACION_CUMPLIMIENTO_1$RIESGO_AGENCIA_1[i]>=1 & EVALUACION_CUMPLIMIENTO_1$RIESGO_AGENCIA_1[i]<2)
    RIESGO_AGENCIA[i]="MEDIO"
  else if(EVALUACION_CUMPLIMIENTO_1$RIESGO_AGENCIA_1[i]>=2 & EVALUACION_CUMPLIMIENTO_1$RIESGO_AGENCIA_1[i]<3)
    RIESGO_AGENCIA[i]="ALTO"
}
EVALUACION_CUMPLIMIENTO_1= data.frame(EVALUACION_CUMPLIMIENTO_1, RIESGO_AGENCIA)
EVALUACION_CUMPLIMIENTO_1= EVALUACION_CUMPLIMIENTO_1 %>% mutate(RIESGO_TOTAL_1= mean(RIESGO_AGENCIA_1))
RIESGO_TOTAL=NULL
for (i in 1:nrow(EVALUACION_CUMPLIMIENTO_1)) {
  if(EVALUACION_CUMPLIMIENTO_1$RIESGO_TOTAL_1[i]<1)
    RIESGO_TOTAL[i]="BAJO"
  else if(EVALUACION_CUMPLIMIENTO_1$RIESGO_TOTAL_1[i]>=1 & EVALUACION_CUMPLIMIENTO_1$RIESGO_TOTAL_1[i]<2)
    RIESGO_TOTAL[i]="MEDIO"
  else if(EVALUACION_CUMPLIMIENTO_1$RIESGO_TOTAL_1[i]>=2 & EVALUACION_CUMPLIMIENTO_1$RIESGO_TOTAL_1[i]<3)
    RIESGO_TOTAL[i]="ALTO"
}
EVALUACION_CUMPLIMIENTO_1= data.frame(EVALUACION_CUMPLIMIENTO_1, RIESGO_TOTAL)

#####
CLIENTES_Base1 <- read_excel("D:/Nueva carpeta1/BASE CLIENTES/BASE CLIENTES/Nueva carpeta/CLIENTES_Base1.xlsx")
CLIENTES_Base2 <- read_excel("D:/Nueva carpeta1/BASE CLIENTES/BASE CLIENTES/Nueva carpeta/CLIENTES_Base2.xlsx")
CLIENTES_Base3 <- read_excel("D:/Nueva carpeta1/BASE CLIENTES/BASE CLIENTES/Nueva carpeta/CLIENTES_Base3.xlsx")
CLIENTES_Base4 <- read_excel("D:/Nueva carpeta1/BASE CLIENTES/BASE CLIENTES/Nueva carpeta/CLIENTES_Base4.xlsx")
CLIENTES_Base5 <- read_excel("D:/Nueva carpeta1/BASE CLIENTES/BASE CLIENTES/Nueva carpeta/CLIENTES_Base5.xlsx", col_types = c("text"))
##
Base= rbind(CLIENTES_Base1, CLIENTES_Base2, CLIENTES_Base3, CLIENTES_Base4)
save(Base, file = "Base.RData")




### app con prueba de widgets ##########
if (interactive()) {

  library("shiny")
  library("shinyWidgets")

  ui <- fluidPage(
    column(
      width = 7,
      tags$b("Default"), br(),
      progressBar(id = "pb1", value = 50,
                  display_pct = TRUE, status = "warning"),
      sliderInput(
        inputId = "up1",
        label = "Update",
        min = 0,
        max = 100,
        value = 50
      ),
      br(),
      tags$b("Other options"), br(),
      progressBar(
        id = "pb2",
        value = 0,
        total = 100,
        title = "",
        display_pct = TRUE
      ),
      actionButton(
        inputId = "go",
        label = "Launch calculation"
      )
    )
  )

  server <- function(input, output, session) {
    observeEvent(input$up1, {
      if (input$up1 < 33) {
        status <- "danger"
      } else if (input$up1 >= 33 & input$up1 < 67) {
        status <- "warning"
      } else {
        status <- "success"
      }
      updateProgressBar(
        session = session,
        id = "pb1",
        value = input$up1,
        status = status,
      )
    })
    observeEvent(input$go, {

      for (i in 1:100) {
        if (i < 33) {
          status <- "danger"
        } else if (i >= 33 & i < 67) {
          status <- "warning"
        } else {
          status <- "success"
        }
        updateProgressBar(
          session = session,
          id = "pb2",
          value = i, total = 100,
          status = status,
          title = paste("Process", trunc(i/10))
        )
        Sys.sleep(0.1)
      }
    })
  }

  shinyApp(ui = ui, server = server)

}

##### otra app de prueba #####
mt <- mtcars %>%
  select(mpg, cyl) %>%
  head()

ui <- fluidPage(

  DTOutput(outputId = "final_tbl")
)

server <- function(input, output){
  df1 <- reactiveValues(data=NULL)
  dat <- reactive({
    d <- mt %>%
      mutate(total = mpg + cyl)
    d
  })

  observe({
    df1$data <- dat()
  })

  output$final_tbl <- renderDT({

    df1$data %>%
      datatable(editable = TRUE)

  })

  observeEvent(input$final_tbl_cell_edit, {
    info = input$final_tbl_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value

    # Without this line the table does not change but with it it jumps to row 1 after an edit.
    df1$data[i, j] <<- (DT::coerceValue(v, df1$data[i, j]))
    df1$data[,"total"] <<- df1$data[,"mpg"] + df1$data[,"cyl"]  ## update the total column
  })

}

shinyApp(ui, server)





## página que muestra la conexion a googlesheets ################
#https://community.rstudio.com/t/trying-to-display-a-table-from-live-poll-results-using-r-shiny/84330




############ app de prueba para la conexion de Shiny con googlesheets ####
#ui.r

library(shiny)
library(shinyWidgets)
library(googlesheets4)
ui <- shinyUI(fluidPage(
  # Application title
  useSweetAlert("sweetalert2", ie = FALSE),
  titlePanel("CONTROL DE ACTIVIDADES"),

  sidebarPanel(
    textInput("texto","Actividad:"),
    numericInput("x", "Enter Value of X", 1),
    numericInput("y", "Enter Value of Y", 1),
    dateRangeInput("fecha","PERIODO", separator = " hasta ", format = "dd-mm-yyyy", language = "es"),
    actionButton("add_data", "Agregar", width="100%"),
    numericInput("cant","Eliminar la fila:", value = 0),
    actionButton("elim_fila", "Eliminar")
  ),
  mainPanel(
    tableOutput("xy_Table"),
    dataTableOutput("tabla")
  )
)
)

#server.R

library(shiny)
library(tidyverse)

server <- shinyServer(function(input, output,session) {

  x <- vector("numeric")
  y <- vector("numeric")
  ACTIVIDAD <- vector("character")
  fec <- vector("character")
  fec_ult <- vector("character")
  tiempo<- vector("numeric")
  tiempo_transcurrido<- vector("numeric")
  #
  xyTable <- reactiveValues()
  xyTable$df <- tibble(ACTIVIDAD,x, y,fec, fec_ult, tiempo, tiempo_transcurrido)
  a1 <- reactive(input$texto)
  e <- reactive(input$x)
  f <- reactive(input$y)
  fec1<- reactive(as.character(input$fecha[1]))
  fec_ult1 <- reactive(as.character(input$fecha[2]))
  ##
  observeEvent(input$add_data, {
    xyTable$df <- xyTable$df %>% add_row(ACTIVIDAD=toupper(a1()),x=e(), y=f(),fec= fec1(),fec_ult= fec_ult1(),
                                         tiempo= as.numeric(as.Date(fec_ult1())-as.Date(fec1())),
                                         tiempo_transcurrido= as.numeric(today()-as.Date(fec1())))
  })

  observeEvent(input$add_data, {
    sendSweetAlert(
      session = session,
      title = "Finalizado !!",
      text = "Registrado",
      type = "success"
    )
  })
  ##
  observeEvent(input$elim_fila,{
    xyTable$df <- xyTable$df[-input$cant,]
  })
  ##
  output$xy_Table <- renderTable({
    xyTable$df
  })
  #####
  reactivo_1<- reactive({
    invalidateLater(10000)
    googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WFQQUlbAihYROjOyu_IpP1mZfb8Lf38pYpEzga9d_-4/edit?usp=sharing",
                              col_types = "ciD")
  })
  output$tabla <- renderDataTable({
    reactivo_1()
  })


})

shinyApp(ui,server)




########### app de inicio la prueba conexion google con R #####
#ui.r

library(shiny)
library(shinyWidgets)
library(googlesheets4)
ui <- shinyUI(fluidPage(
  # Application title
  useSweetAlert("sweetalert2", ie = FALSE),
  titlePanel("CONTROL DE ACTIVIDADES"),

  sidebarPanel(
    textInput("texto","Actividad:"),
    numericInput("x", "Enter Value of X", 1),
    numericInput("y", "Enter Value of Y", 1),
    dateRangeInput("fecha","PERIODO", separator = " hasta ", format = "dd-mm-yyyy", language = "es"),
    actionButton("add_data", "Agregar", width="100%"),
    numericInput("cant","Eliminar la fila:", value = 0),
    actionButton("elim_fila", "Eliminar")
  ),
  mainPanel(
    tableOutput("xy_Table"),
    dataTableOutput("tabla")
  )
)
)

#server.R

library(shiny)
library(tidyverse)

server <- shinyServer(function(input, output,session) {

  x <- vector("numeric")
  y <- vector("numeric")
  ACTIVIDAD <- vector("character")
  fec <- vector("character")
  fec_ult <- vector("character")
  tiempo<- vector("numeric")
  tiempo_transcurrido<- vector("numeric")
  #
  xyTable <- reactiveValues()
  xyTable$df <- tibble(ACTIVIDAD,x, y,fec, fec_ult, tiempo, tiempo_transcurrido)
  a1 <- reactive(input$texto)
  e <- reactive(input$x)
  f <- reactive(input$y)
  fec1<- reactive(as.character(input$fecha[1]))
  fec_ult1 <- reactive(as.character(input$fecha[2]))
  ##
  observeEvent(input$add_data, {
    xyTable$df <- xyTable$df %>% add_row(ACTIVIDAD=toupper(a1()),x=e(), y=f(),fec= fec1(),fec_ult= fec_ult1(),
                                         tiempo= as.numeric(as.Date(fec_ult1())-as.Date(fec1())),
                                         tiempo_transcurrido= as.numeric(today()-as.Date(fec1())))
  })

  observeEvent(input$add_data, {
    sendSweetAlert(
      session = session,
      title = "Finalizado !!",
      text = "Registrado",
      type = "success"
    )
  })
  ##
  observeEvent(input$elim_fila,{
    xyTable$df <- xyTable$df[-input$cant,]
  })
  ##
  output$xy_Table <- renderTable({
    xyTable$df
  })
  #####
  reactivo_1<- reactive({
    invalidateLater(10000)
    googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WFQQUlbAihYROjOyu_IpP1mZfb8Lf38pYpEzga9d_-4/edit?usp=sharing",
                              col_types = "ciD")
  })
  output$tabla <- renderDataTable({
    reactivo_1()
  })


})

shinyApp(ui,server)

##### enlace que muestra la integracion de shinysurvey con googlesheets ####
# https://www.jdtrat.com/blog/connect-shiny-google/
# ↓ el enlace es una muestra de conexion con respecto al tema anterior ---stackoverflow---
# https://stackoverflow.com/questions/63535190/connect-to-googlesheets-via-shiny-in-r-with-googlesheets4


list.files(".secrets/")

#Abono de Sueldos CA

c("aalvarez"
,"djordan"
,"dmamaniq"
,"epaz"
,"etorricos"
,"evallejos"
,"fordonez"
,"gsoria"
,"jvalenciac"
,"lpacheco"
,"maldana"
,"mdblanco"
,"mtola"
,"rmedrano"
,"rrojasl"
,"umorales")
#####################

fechas= seq(today()-2, today()+10, by=1)
fechas_festivas=c("2024-1-1","2024-1-22","2024-2-12","2024-2-13","2024-3-29","2024-5-1","2024-5-30",
                  "2024-6-21","2024-8-6"
                  ,"2024-11-2","2024-12-25")
fechas_festivas= as.Date(fechas_festivas)
fechas= fechas[!(fechas %in% fechas_festivas) ]
fechas_1= weekdays(fechas)
length(fechas_1)
length(fechas_1[!(fechas_1 %in%c("sábado","domingo")) ])



3136
4393
5368
4606
5911
6240
4801
4774
4922
4887
5607
6135
60780
#####

Alertas= rest
filtrado=grep("Importe", Alertas$Detalle , ignore.case = T)
persona_d= Alertas[filtrado,]



datos= prueba %>% filter(FUNCIONARIO==unique(prueba$FUNCIONARIO)[2])
datos=datos %>% gather(key = "grupo", value = "cant",c(ESTIMADO_TOTAL, ESTIMADO_A_LA_FECHA, CONCLUIDO))
datos= as.data.frame(datos[,c(2,7,8)])
hchart(datos %>% filter(ACTIVIDAD==unique(datos$ACTIVIDAD)[1]) , "bar" ,hcaes(x=ACTIVIDAD, y=cant, group=grupo),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))%>% hc_add_theme(hc_theme_google()) %>%
  hc_subtitle(text="Evaluación de Debida Diligencia a Directores")%>%
  hc_colors(c("red","green","#38AAACFF", "#3a0a5e","green","maroon","#38AAACFF"))


highcharter::hw_grid(hchart(datos %>% filter(ACTIVIDAD==unique(datos$ACTIVIDAD)[1]) , "bar" ,hcaes(x=ACTIVIDAD, y=cant, group=grupo),
                            # stacking = list(enabled = TRUE),
                            dataLabels = list(
                              enabled = TRUE
                              #, # Añadir etiquetas
                              #format = "{point.percentage:.0f}%"
                            ))%>% hc_add_theme(hc_theme_google()),
                     hchart(datos %>% filter(ACTIVIDAD==unique(datos$ACTIVIDAD)[2]) , "bar" ,hcaes(x=ACTIVIDAD, y=cant, group=grupo),
                            # stacking = list(enabled = TRUE),
                            dataLabels = list(
                              enabled = TRUE
                              #, # Añadir etiquetas
                              #format = "{point.percentage:.0f}%"
                            ))%>% hc_add_theme(hc_theme_google()),ncol = 1)%>% htmltools::browsable()

####################
REPORTE_ACCIDENTES_TRABAJO= REPORTE_ACCIDENTES_TRABAJO %>% mutate(mes= format(FECHA_ACCIDENTE, "%B"))
REPORTE_ACCIDENTES_TRABAJO= REPORTE_ACCIDENTES_TRABAJO %>% mutate(meses= paste(substr(FECHA_ACCIDENTE,1,7),"1", sep = "-"))
REPORTE_ACCIDENTES_TRABAJO$meses= as.Date(REPORTE_ACCIDENTES_TRABAJO$meses)
#
datos=REPORTE_ACCIDENTES_TRABAJO %>% count(mes, TIPO_ACCIDENTE_AT_AII)
hchart(datos , "spline" ,hcaes(x=mes, y=n, group=TIPO_ACCIDENTE_AT_AII),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google()) %>% hc_title(text="REPORTE DE ACCIDENTES")
# %>%
#   hc_colors(c("#f6a700","#E75263FF","#F8850FFF","#4b0082","maroon","#38AAACFF","#FDE725FF"))
datos_1= REPORTE_ACCIDENTES_TRABAJO %>% count(EDAD, SEXO, wt= TOTAL_DIAS_DE_BAJA)
hchart(datos_1 , "column" ,hcaes(x=EDAD, y=n, group=SEXO),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google()) %>% hc_title(text="DÍAS DE BAJA")

datos_2= REPORTE_ACCIDENTES_TRABAJO %>% count(CLASIFICACION_DEL_ACCIDENTE)
datos_2= data.frame(datos_2, grupo="CANTIDAD")
hchart(datos_2 , "pie",innerSize = 200, hcaes(x=CLASIFICACION_DEL_ACCIDENTE, y=n, group=grupo),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         , # Añadir etiquetas
         format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())
##
datos_3= REPORTE_ACCIDENTES_TRABAJO %>% count(CLASIFICACION_GENERAL)
datos_3= data.frame(datos_3, grupo="CANTIDAD")
hchart(datos_3 , "bar",innerSize = 200, hcaes(x=CLASIFICACION_GENERAL, y=n, group=grupo),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE,
         borderRadius= 5,
         # Añadir etiquetas
         format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())
##
datos_4= REPORTE_ACCIDENTES_TRABAJO %>% count(NATURALEZA_DE_LA_LESION)
datos_4= data.frame(datos_4, grupo="CANTIDAD")
datos_4= datos_4 %>% arrange(desc(n))
hchart(datos_4 , "bar",hcaes(x=NATURALEZA_DE_LA_LESION, y=n, group=grupo),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())
###
datos_5=REPORTE_ACCIDENTES_TRABAJO %>% count(meses, TIPO_ACCIDENTE_AT_AII)
hchart(datos_5 , "spline",hcaes(x=meses, y=n, group=TIPO_ACCIDENTE_AT_AII),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE,
         borderRadius= 5,
         borderWidth=1
         #, # Añadir etiquetas
         #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())%>% hc_rangeSelector(enabled= TRUE, verticalAlign = "top")

######################
datos= control %>% filter(FUNCIONARIO==unique(control$FUNCIONARIO)[1])
nombre= unique(datos$FUNCIONARIO)
datos1= datos %>% group_by(ACTIVIDAD) %>% tally()
if(2 %in% datos1$n |3 %in% datos1$n|4 %in% datos1$n|5 %in% datos1$n|6 %in% datos1$n){
  unido= data.frame()
  datos2= datos %>% filter(ACTIVIDAD %in% (datos %>% group_by(ACTIVIDAD) %>% tally() %>% filter(n>1))$ACTIVIDAD)
  for (i in 1:length(unique(datos2$ACTIVIDAD))) {
    datos3= datos2 %>% filter(ACTIVIDAD== unique(datos2$ACTIVIDAD)[i])
    datos3= datos3 %>% mutate(corre=1:nrow(datos3))
    datos3= datos3 %>% mutate(ACTIVIDAD= paste(ACTIVIDAD, corre))
    unido= rbind(unido, datos3)
    unido= unido[,1:(ncol(unido)-1)]
  }
  datos= rbind(datos %>% filter(ACTIVIDAD %in% (datos %>% group_by(ACTIVIDAD) %>% tally() %>% filter(n==1)) ),
               unido)
  datos=datos %>% gather(key = "grupo", value = "cant",c(ESTIMADO_TOTAL, ESTIMADO_A_LA_FECHA, CONCLUIDO))
  datos= as.data.frame(datos[,c(2,7,8)])
}
else{
  datos=datos %>% gather(key = "grupo", value = "cant",c(ESTIMADO_TOTAL, ESTIMADO_A_LA_FECHA, CONCLUIDO))
  datos= as.data.frame(datos[,c(2,7,8)])
}
################
hchart(datos , "bar" ,hcaes(x=ACTIVIDAD, y=TIEMPO_TRANSCURRIDO, group=TIEMPO),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       ))%>% hc_add_theme(hc_theme_google(chart = list(backgroundColor = '#e1ffea'))) %>% hc_colors(c("red","#3a0a5e","orange")) %>%
  hc_title(text=paste("FUNCIONARIO: ", nombre)) %>% hc_size(height = 200)

"#fcd1c6"
"#e1ffea"

###
control= reactivo_3()
CARGA_LABORAL_AS=NULL
for (i in 1:nrow(control)) {
  if(control$ACTIVIDAD[i]=="ALERTAS FINDER")
    CARGA_LABORAL_AS[i]=control$TIEMPO[i]*4
  else CARGA_LABORAL_AS[i]=control$TIEMPO[i]
}
control= data.frame(control, CARGA_LABORAL_AS)
control1=control %>% group_by(FUNCIONARIO) %>% summarise(DIAS_TOTAL_ASIGNADO=sum(TIEMPO) ,CARGA_LABORAL_DISPONIBLE=sum(TIEMPO*8- CARGA_LABORAL_AS),
                                                CARGA_LABORAL_ASIGNADA=sum(CARGA_LABORAL_AS))
control2=control1 %>% gather(key = "grupo", value = "Disponible", c(CARGA_LABORAL_DISPONIBLE, CARGA_LABORAL_ASIGNADA))
grafico= control2 %>% filter(FUNCIONARIO==unique(control2$FUNCIONARIO))
grafico$grupo= factor(grafico$grupo, levels = c("CARGA_LABORAL_ASIGNADA","CARGA_LABORAL_DISPONIBLE"))
#
hchart(grafico, "bar" ,hcaes(x=FUNCIONARIO, y=Disponible, group=grupo),
        stacking = list(enabled = TRUE),
       dataLabels = list(
          enabled = TRUE
          , # Añadir etiquetas
          format = "{point.percentage:.0f}%"
       ))%>% hc_add_theme(hc_theme_google(chart = list(backgroundColor = '#e1ffea'))) %>% hc_colors(c("red","green")) %>%
  hc_title(text=paste("DISTRIBUCIÓN CARGA LABORAL")) %>% hc_size(height = 200)%>% hc_yAxis(title=list(text=""))%>% hc_xAxis(title=list(text=""))
####
table(REPORTE_ACCIDENTES_TRABAJO$SEXO,
      REPORTE_ACCIDENTES_TRABAJO$CLASIFICACION_GENERAL) %>% kbl()%>%
  kable_paper("hover",full_width = F)%>%
  scroll_box(width = "100%")
#####################################################################
#cuenta destino
filtrado=grep("1927128", monitoreador_de_transacciones$CtaDest , ignore.case = T)
persona_d= monitoreador_de_transacciones[filtrado,]
#cuenta origen
filtrado=grep("1927128", Base_202_23_c$`Datos_del_PCC:_Numero_cuenta_origen` , ignore.case = T)
persona_d= Base_202_23_c[filtrado,]
############ REPORTE COMPRA VENTA DE DIVISAS MICA ###############

for (i in 1:nrow(datos3)) {
  if(datos3$NombreRazonSocial[i]=="AGROCHIQUITANO S.R.L.")
    datos3$IdIdentificacion[i]=393124023
  else if(datos3$NombreRazonSocial[i]=="CHRISTIAN AUTOMOTORS S.A.")
    datos3$IdIdentificacion[i]=1028579022
  else if(datos3$NombreRazonSocial[i]=="CIDRE I.F.D.")
    datos3$IdIdentificacion[i]=1021241020
  else if(datos3$NombreRazonSocial[i]=="COORPORACION DE LOGISTICA INTE")
    datos3$IdIdentificacion[i]=4368632014
  else if(datos3$NombreRazonSocial[i]=="EMPRESA DE TRANSPORTE NACIONAL")
    datos3$IdIdentificacion[i]=405825022
  else if(datos3$NombreRazonSocial[i]=="FABRICA BOLIVIANA DE CERAMICA")
    datos3$IdIdentificacion[i]=1023273024
  else if(datos3$NombreRazonSocial[i]=="FORTALEZA SAFI S.A.")
    datos3$IdIdentificacion[i]=1006933020
  else if(datos3$NombreRazonSocial[i]=="FUNDACIÓN MANO A MANO APOYO AÉ")
    datos3$IdIdentificacion[i]=153520027
  else if(datos3$NombreRazonSocial[i]=="FUNDACION MUSICAL BRAVURA")
    datos3$IdIdentificacion[i]=283114029
  else if(datos3$NombreRazonSocial[i]=="IMPORT CHINREPUESTOS S.R.L.")
    datos3$IdIdentificacion[i]=388340026
  else if(datos3$NombreRazonSocial[i]=="KITE XPERIENCE S.R.L.")
    datos3$IdIdentificacion[i]=342018020
  else if(datos3$NombreRazonSocial[i]=="LA BOLIVIANA CIACRUZ SEGUROS P")
    datos3$IdIdentificacion[i]=1006989027
  else if(datos3$NombreRazonSocial[i]=="LA CAIXA")
    datos3$IdIdentificacion[i]=512
  else if(datos3$NombreRazonSocial[i]=="SALUD INTEGRAL SANGRE DE CRIST")
    datos3$IdIdentificacion[i]=319980026
  else if(datos3$NombreRazonSocial[i]=="SOYAGRAIN S.R.L.")
    datos3$IdIdentificacion[i]=407120029
  else if(datos3$NombreRazonSocial[i]=="TRANSPORTE NACIONAL E INTERNAC")
    datos3$IdIdentificacion[i]=363359020

}
############################
prueba_fua= as.data.frame(prueba_fua)
prueba_fua$IdIdentificacion= str_replace(prueba_fua$IdIdentificacion," SC","")
prueba_fua$IdIdentificacion= str_replace(prueba_fua$IdIdentificacion," CB","")
prueba_fua$IdIdentificacion= str_replace(prueba_fua$IdIdentificacion," OR","")
prueba_fua$IdIdentificacion= str_replace(prueba_fua$IdIdentificacion," LP","")
prueba_fua$IdIdentificacion= str_replace(prueba_fua$IdIdentificacion," PA","")
prueba_fua$IdIdentificacion= str_replace(prueba_fua$IdIdentificacion," CH","")
prueba_fua$IdIdentificacion= str_replace(prueba_fua$IdIdentificacion," TJ","")
prueba_fua$IdIdentificacion= str_replace(prueba_fua$IdIdentificacion," PO","")
prueba_fua$IdIdentificacion= str_replace(prueba_fua$IdIdentificacion," BE","")
prueba_fua$IdIdentificacion= str_replace(prueba_fua$IdIdentificacion," ","-")
prueba_fua$FechaOperacion= as.Date(prueba_fua$FechaOperacion)

prueba_fua=prueba_fua %>% mutate(cod= paste(IdIdentificacion,FechaOperacion))
prueba_fua= prueba_fua %>% mutate(corre=1:nrow(prueba_fua))
#Enero_2024_2$Fecha_Transaccion=as.Date(Enero_2024_2$Fecha_Transaccion, format="%d/%m/%Y")
datos= prueba_fua %>% left_join(Enero_2024_2[,c(8:10,13,14,23,24,51,52,65:66,71:73)], by="cod")
datos1= datos %>% group_by(corre) %>% tally() %>% filter(n>1)
datos2= datos %>% filter(!(corre %in% datos1$corre) )
write.xlsx(datos2, file = "eliminar.xlsx")
eliminar <- read_excel("eliminar.xlsx")
datos2= rbind(datos2, eliminar)
datos2= datos2 %>% arrange(corre)
##
datos3= prueba_fua %>% filter(IdIdentificacion==0)
datos33= prueba_fua %>% filter(IdIdentificacion %in% Enero_2024_2$`Cliente_(Juridica):_Nit_(Datos_de_Empresa)` )
datos3= rbind(datos3, datos33)
datos4= datos2 %>% filter(!(IdIdentificacion %in% datos3$IdIdentificacion) )
##
datos3=datos3 %>% mutate(cod= paste(IdIdentificacion,FechaOperacion))
Enero_2024_22= Enero_2024_2 %>% mutate(cod= paste(`Cliente_(Juridica):_Nit_(Datos_de_Empresa)`, Fecha_Transaccion))
dato5= datos3 %>% left_join(Enero_2024_22[,c(8:10,13,14,23,24,51,52,65:66,71:73)], by="cod")
write.xlsx(dato5, file = "eliminar.xlsx")
eliminar <- read_excel("eliminar.xlsx")
##
datos6= rbind(datos4, eliminar)
datos6= datos6 %>% arrange(corre)
############################
Base_PCC_2023_12= Base_Fer_pcc
Base_Fer_pcc$Fecha_Transaccion= as.Date(Base_Fer_pcc$Fecha_Transaccion)
Base_PCC_2023_12= Base_PCC_2023_12 %>% mutate(gestion= substr(Fecha_Transaccion,1,4))
Base_PCC_2023_12= Base_PCC_2023_12 %>% mutate(mes= substr(Fecha_Transaccion,6,7))
Base_PCC_2023_12$gestion= as.numeric(Base_PCC_2023_12$gestion)
Base_PCC_2023_12$mes= as.numeric(Base_PCC_2023_12$mes)
#
library(readr)
pcc_diciembre_2024 <- read_delim("D:/Documentos/Noviembre 2024 2.csv",
                               delim = "|", escape_double = FALSE, col_names = FALSE,
                               col_types = cols(x52 = col_character(),
                                                X66 = col_character()), locale = locale(encoding = "ISO-8859-1"),
                               trim_ws = TRUE)
View(pcc_diciembre_2024)
pcc_diciembre_2024= pcc_diciembre_2024[,1:72]
names(pcc_diciembre_2024)= names(pcc_junio_2024)[1:72]
pcc_diciembre_2024$Fecha_Transaccion= as.Date(pcc_diciembre_2024$Fecha_Transaccion, format="%d/%m/%Y")
#
datos_error= pcc_diciembre_2024 %>% filter(is.na(Fecha_Transaccion))
################
pcc_diciembre_2024= as.data.frame(pcc_diciembre_2024)
pcc_diciembre_2024$Fecha_Transaccion= as.Date(pcc_diciembre_2024$Fecha_Transaccion)
pcc_diciembre_2024= pcc_diciembre_2024 %>% mutate(gestion= substr(Fecha_Transaccion,1,4))
pcc_diciembre_2024= pcc_diciembre_2024 %>% mutate(mes= substr(Fecha_Transaccion,6,7))
pcc_diciembre_2024$gestion= as.numeric(pcc_diciembre_2024$gestion)
pcc_diciembre_2024$mes= as.numeric(pcc_diciembre_2024$mes)
#
pcc_diciembre_2024= pcc_diciembre_2024 %>% mutate(documento_identidad= ifelse(str_detect(`Cliente_(Natural):_Numero_de_Documento_de_Identidad_(Datos_de_la_persona)`,"-"),`Cliente_(Natural):_Numero_de_Documento_de_Identidad_(Datos_de_la_persona)`,paste(`Cliente_(Natural):_Numero_de_Documento_de_Identidad_(Datos_de_la_persona)`, `Cliente_(Natural):_Lugar_de_Expedicion_(Datos_de_la_persona)`, sep = "")))
pcc_diciembre_2024$documento_identidad= str_replace(pcc_diciembre_2024$documento_identidad,"NA","")
pcc_diciembre_2024$documento_identidad= toupper(pcc_diciembre_2024$documento_identidad)
######
Detalle_FUA_NUEVO= Detalle_FUA_NUEVO %>% mutate(cod=paste(DOC_Numero, FechaOperacion))
Detalle_FUA_NUEVO= Detalle_FUA_NUEVO %>% mutate(corre=1:nrow(Detalle_FUA_NUEVO))
Detalle_FUA_NUEVO$Monto= as.numeric(Detalle_FUA_NUEVO$Monto)
pcc_diciembre_2024= pcc_diciembre_2024 %>% mutate(cod=paste(documento_identidad, Fecha_Transaccion))
pcc_diciembre_2024= pcc_diciembre_2024 %>% mutate(MONTO_PCC=ifelse(!is.na(`Datos_del_PCC:_Monto`),`Datos_del_PCC:_Monto`, `Datos_del_PCC:_Monto_de_la_operacion2`))
pcc_diciembre_2024$MONTO_PCC= as.numeric(pcc_diciembre_2024$MONTO_PCC)
datos_revisados= Detalle_FUA_NUEVO %>% left_join(pcc_diciembre_2024[,c(8:10,51,52,65,66,71:77)], by="cod")
datos= datos_revisados %>% filter(!is.na(MONTO_PCC))
datos= datos %>% mutate(diferencia= abs(MONTO_PCC- Monto))
#####################
nested_questions <- teaching_r_questions %>%
  group_by(question) %>%
  nest() %>%
  ungroup()
######
question=c("¿Cuál es tu nombre?", #1
           "¿Cuál es tu edad?", #2
           rep("¿Cuál opción describe mejor tu género?",3),#3
           rep("¿El sujeto obligado debe conservar en medios físicos o digitales: Todos los registros
               transferencias electrónicas, instrumentos electrónicos de pago y órdenes de pago, nacionales como internacionales
               durante:?",3),#4
           rep("Todos los registros y documentación obtenidos a través de los procedimientos y medidas de D.D.,
               archivos de cuenta, correspondencia comercial y los resultados de análisis realizados,
               debe conservarse durante, (Escoger opción), despues de finalizada la relación comercial o después de la fecha
               de la transacción ocasional",3),#5
           rep("¿Debería ser una señal de alerta cuando, un cliente realiza pequeñas pero frecuentes cantidades de diner en efectivo,
               cuya suma constituye un importe relevante?",2),#6
           rep("Pregunta 4",2),#7
           rep("Pregunta 5",2)
)
option=c("Nombres y Apellidos",#1
         "27",#2
         c("Masculino","Femenino","Otro"),#3
         c("10 años","Máximo 10 años", "Al menos 10 años"),#4
         c(">= 10 años", "10 años","<= 10 años" ),#5
         c("SI","NO"),#6
         c("SI","NO"),#7
         c("SI","NO")
)
input_type=c("text", #1
             "numeric",#2
             c("mc","mc","mc"),#3
             c("mc","mc","mc"),#4
             c("mc","mc","mc"),#5
             c("y/n","y/n"),#6
             c("y/n","y/n"),#7
             c("y/n","y/n")
)
input_id=c("nombre",#1
           "edad",#2
           c("genero","genero","genero"),#3
           c("p1","p1","p1"),#4
           c("p2","p2","p2"),#5
           c("p3","p3"),#6
           c("p4","p4"),#7
           c("p5","p5")
)
dependence=c(NA,#1
             NA,#2
             c(NA,NA,NA),#3
             c(NA,NA,NA),#4
             c(NA,NA,NA),#5
             c(NA,NA),#6
             c(NA,NA),#7
             c(NA,NA)
)
dependence_value=c(NA,#1
                   NA,#2
                   c(NA,NA,NA),#3
                   c(NA,NA,NA),#4
                   c(NA,NA,NA),#5
                   rep(NA,2),#6
                   rep(NA,2),#7
                   rep(NA,2)
)
required=c(T,#1
           T,#2
           c(T,T,T),#3
           c(T,T,T),#4
           c(T,T,T),#5
           rep(T,2),#6
           rep(T,2),#7
           rep(T,2)
)

dep_questions=data.frame(question, option, input_type, input_id, dependence, dependence_value, required)
nested_questions <- dep_questions %>%
  group_by(question) %>%
  nest() %>%
  ungroup()
##
multiQuestions <- nested_questions %>%
  mutate(page = c(
    rep(1, 3),
    rep(2, 1),
    rep(3, 1),
    rep(4, 1),
    rep(5, 1),
    rep(6, 1)
    )
  )
##
dep_questions <- multiQuestions %>%
  unnest(cols = data)
dato1= dep_questions[1:5,]
dato2= dep_questions[6:nrow(dep_questions),]
dato2=dato2 %>% filter(input_id %in% sample(unique(dato2$input_id),3,replace=F))
dep_questions= rbind(dato1, dato2)
##
multiQuestions %>%
  group_by(page, question) %>%
  slice_head() %>%
  ungroup() %>%
  select(question, page)




timeline_data <- dplyr::tribble(
  ~name, ~label, ~description,
  'Apple I', 'April 1976: Apple I and company formation', 'On April 1, 1976, Apple Computer Company was founded by Steve Jobs, Steve Wozniak, and Ronald Wayne.',
  'Apple II', 'August 1976: Apple II was presented', 'Wozniak moved on from the Apple I and began designing a greatly improved computer: the Apple II.',
  'Macintosh','January 1984: Macintosh and the "1984" commercial.', 'On January 24, 1984, the Macintosh went on sale with a retail price of $2,495',
  'Jobs Leaves Apple', '1985: Jobs and Wozniak leave Apple','In April 1985, Sculley decided to remove Jobs as the general manager of the Macintosh division, and gained unanimous support from the Apple board of directors'
)

hchart(
  timeline_data,
  type = "timeline"
)  |>
  hc_xAxis(
    visible = FALSE
  )  |>
  hc_yAxis(
    visible = FALSE
  )  |>
  hc_title(
    text = "Historia de Apple Inc"
  ) |>
  hc_subtitle(
    text = "Fuente: <a href='https://en.wikipedia.org/wiki/History_of_Apple_Inc.'>www.wikipedia.org</a>'"
  ) |>
  hc_colors(
    RColorBrewer::brewer.pal(6, "Spectral")
  ) |>
  hc_credits(enabled = TRUE,
             text = "<a href='elartedeldato.com'>El arte del dato bien contado</a>")
####
observeEvent({
  input$SepalLength != NULL |
    input$SepalWidth != NULL |
    input$Species!= NULL
},{
  shinyjs::enable("filtrar")
})

datos_resumen= datos %>% count(mes_gen, `Estado de la Alerta`)
names(datos_resumen)[2]="Estado_de_la_Alerta"
hchart(datos_resumen, "column" , hcaes(x=mes_gen, y=n, group=Estado_de_la_Alerta),
      # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.1f}%"
       ))  %>%
  hc_add_theme(hc_theme_google())  %>%
  hc_subtitle(text="Alertas por mes") %>%
  hc_colors(c("#E75263FF","#F8850FFF","maroon","#38AAACFF","#FDE725FF","#56147DFF"))
####


Alertas_con_cierre_1= Alertas %>% mutate(cod= paste(Nro_de_identificacion, Nombre_completo, Fecha_de_generacion,
                                                Caracteristica_1, Caracteristica_2))
Alertas_con_cierre_1= Alertas_con_cierre_1 %>%  mutate(cod1= paste(Nro_de_identificacion, Nombre_completo,
                                                              Caracteristica_1, Caracteristica_2))
#
pendientes= Alertas_sin_cierre_1 %>% filter(`Estado de la Alerta`=="Pendiente")
pendientes_con_cierre= Alertas_con_cierre_1 %>% filter(cod %in% pendientes$cod )
pendientes_con_cierre_m= pendientes_con_cierre %>% filter(!is.na(`Usuario finalizacion`))
pendientes_con_cierre_m= pendientes_con_cierre_m %>% mutate(detect=ifelse(str_detect(Descripcion_cierre,"INUSUALIDAD")|
                                                                            str_detect(Descripcion_cierre,"INUSUAL"),1,2))
pendientes_con_cierre_reales= pendientes_con_cierre %>% filter(is.na(`Usuario finalizacion`))
#
cierres= Alertas_sin_cierre_1 %>% filter(`Estado de la Alerta`=="Finalizada")
cierres=cierres %>% mutate(cod1= paste(Nro_de_identificacion, Nombre_completo,
                               Caracteristica_1, Caracteristica_2))
cierres_1= cierres %>% mutate(cod1= paste(Nro_de_identificacion, Nombre_completo,
                                             Caracteristica_1, Caracteristica_2))
cierres_1= cierres_1 %>% group_by(cod1) %>% tally()
cierres_1= cierres_1 %>% filter(n>=3)
cierres_11= Alertas_con_cierre_1 %>% filter(`Estado de la Alerta`=="Finalizada")
cierres_11= cierres_11 %>% filter(cod1 %in% cierres_1$cod1)
cierres_11= cierres_11 %>% group_by(cod1) %>% summarise(cantidad=n(), descripcion= list(Descripcion_cierre),
                                                        cantidad_unicos= length(unique(Descripcion_cierre)),
                                                        unicos= list(unique(Descripcion_cierre)),
                                                        funcionarios_resp_cierre= list(unique(`Usuario finalizacion`)))
cierres_2= cierres %>% filter(cod1 %in% cierres_1$cod1)
#
pendientes_con_cierre_reales_1= pendientes_con_cierre_reales %>% mutate(cod1= paste(Nro_de_identificacion, Nombre_completo,
                                                                                    Caracteristica_1, Caracteristica_2))
#
pendientes_con_cierre_reales_1= pendientes_con_cierre_reales_1 %>% mutate(corre=1:nrow(pendientes_con_cierre_reales_1))
base_filt= pendientes_con_cierre_reales_1 %>% left_join(cierres_11, by="cod1")
base_filt$descripcion[base_filt$descripcion=="NULL"]=NA
#
Alertas_con_cierre_1= Alertas_con_cierre_1 %>%  mutate(cod1= paste(Nro_de_identificacion, Nombre_completo,
                                                              Caracteristica_1, Caracteristica_2))
datos= base_filt %>% filter(descripcion=="NA")
datos= datos %>% mutate(mes= substr(Fecha_de_generacion,1,7))
##
res_datos= datos %>% count(mes, REGIONAL)
hchart(res_datos, "column" , hcaes(x=mes, y=n, group=REGIONAL),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.1f}%"
       ))  %>%
  hc_add_theme(hc_theme_google())  %>%
  hc_subtitle(text="Alertas por mes") %>%
  hc_colors(c("#E75263FF","#F8850FFF","maroon","#38AAACFF","#FDE725FF","#56147DFF"))
##
datos1= base_filt %>% filter(descripcion!="NA")
datos1= datos1 %>% mutate(mes= substr(Fecha_de_generacion,1,7))
res_datos= datos1 %>% count(mes, REGIONAL)
hchart(res_datos, "column" , hcaes(x=mes, y=n, group=REGIONAL),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.1f}%"
       ))  %>%
  hc_add_theme(hc_theme_google())  %>%
  hc_subtitle(text="Alertas por mes") %>%
  hc_colors(c("#E75263FF","#F8850FFF","maroon","#38AAACFF","#FDE725FF","#56147DFF"))
##
res_alertas= datos %>% filter(`Estado de la Alerta`=="Finalizada")
res_alertas_1= res_alertas %>% count(mes, REGIONAL)
hchart(res_alertas_1, "column" , hcaes(x=mes, y=n, group=REGIONAL),
        stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.1f}%"
       ))  %>%
  hc_add_theme(hc_theme_google())  %>%
  hc_subtitle(text="Alertas por mes") %>%
  hc_colors(c("#E75263FF","#F8850FFF","maroon","#38AAACFF","#FDE725FF","#56147DFF"))
####
cerradas= datos %>% filter(`Estado de la Alerta`=="Finalizada")
cerradas= cerradas %>% filter(Fecha_de_generacion>=as.Date("2023-7-1")& Fecha_de_generacion<=as.Date("2023-12-31"))
cerradas_1=cerradas %>%  count(mes_gen, REGIONAL)
res=cerradas_1 %>% group_by(REGIONAL) %>% summarise(promedio= mean(n))
res$promedio= round(res$promedio)
kbl(res, escape = F) %>%
  kable_paper("hover", full_width = F)
############
cierres= Alertas_sin_cierre_1 %>% filter(`Estado de la Alerta`=="Finalizada")%>% mutate(cod1= paste(Nro_de_identificacion, Nombre_completo,
                                                                                                    Caracteristica_1, Caracteristica_2))
cierres_1= cierres %>% group_by(cod1) %>% tally() %>% filter(n==1)
alertas_una= Alertas_con_cierre_1 %>% filter(cod1 %in% cierres_1$cod1 )
alertas_una= alertas_una %>% mutate(mes_gen= substr(Fecha_de_generacion,1,7))
##
datos= Alertas_con_cierre_1 %>% group_by(cod1) %>% arrange(Fecha_de_generacion) %>% filter(row_number()==1)
datos$Fecha_cierre= as.Date(datos$Fecha_cierre)
datos= datos %>% mutate(tiempo_cierre_dias= Fecha_cierre- Fecha_de_generacion)
datos= datos %>% mutate(mes_gen= substr(Fecha_de_generacion,1,7))
datos1= datos %>% group_by(mes_gen, REGIONAL, `Usuario finalizacion`) %>% summarise(promedio_dias= mean(tiempo_cierre_dias), cantidad=n())
################ CONVERTIR LETRA A CODIGO ASCII ############
charToRaw("mVGv")
rawToChar(charToRaw("EAA"))




reactivo_1<- reactive({
  invalidateLater(45000)
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WFQQUlbAihYROjOyu_IpP1mZfb8Lf38pYpEzga9d_-4/edit?usp=sharing",
                            col_types = "cciicciii")

})
#########  EJEMPLO CI, PROPIEDAD DE DINERO Q NO COINCIDE EN BANTOTAL Y FINDER 11383826 #######



c('"IdPersona"                  "TipoAlta"                   "CanalInformacion"           "Nombres"
 "ApellidoPaterno"            "ApellidoMaterno"            "Sexo"                       "EstadoCivil"
 "EstadoCivilDeclarado"       "FechaNacimiento"            "PaisNacimiento"             "Departamento_Provincia"
"NroHijos"                   "NroDependientes"            "IdPersonaConyuge"           "FechaVencimiento_Documento"
"FechaDefuncion"             "IdDiscapacidad"             "IdNivelEducativo"           "EmpleadoBanco"
"FechaIngreso_EmpleadoBanco" "IdOcupacion"                "IdSegmento"                 "IdProfesion"
"UbicacionEstablecimiento"   "NIT_Persona"                "Estado_NIT"                 "PromedioIngresosMensuales"
"PropositoRelacionComercial" "EsPropietario_DineroCuenta" "DeclaracionFACTA"           "Nivel_Ingresos"
"PaisResidencia"             "ClasificacionCliente"       "TipoClientePEP"             "TipoClienteActAltoRiesgo"
 "Nacionalidad2"              "FechaAltaCliente"           "IdPersona"                  "Documento"
 "TipoDocumento"              "CodPais"                    "Complemento_SEGIP"          "TipoPersona"
"PaisExpedidoDocumento"      "Denominacion"')


# IdPersona,TipoAlta,CanalInformacion,Nombres,ApellidoPaterno,ApellidoMaterno,Sexo,EstadoCivil,
# EstadoCivilDeclarado,FechaNacimiento,PaisNacimiento,Departamento_Provincia,NroHijos,NroDependientes,
# IdPersonaConyuge,FechaVencimiento_Documento,FechaDefuncion,IdDiscapacidad,
########### datos es la hoja del drive
datos1=preguntas %>% left_join(datos, by=c("INPUT_ID"="question_id"))
datos1= datos1 %>% filter(!is.na(NOMBRE))
datos1= datos1 %>% mutate(calificacion= ifelse(Respuestas_correctas==RESPUESTA,1,0))
datos1= datos1 %>% group_by(FECHA, hora,NOMBRE, CARGO_POSTULANTE) %>% summarise(Calificacion= sum(calificacion, na.rm = T)/length(calificacion))
resumen= datos1 %>% arrange(FECHA, hora,NOMBRE)
resumen= resumen %>% mutate(puntaje= paste(Calificacion*100,"%"), Estado= ifelse(Calificacion>=0.8,"Aprobado","Reprobado"))






substr(format(Sys.time(), "%a %b %d %X %Y"),14,(nchar(format(Sys.time(), "%a %b %d %X %Y"))-5))



output$report <- downloadHandler(
  filename = "report.html",
  content = function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.rmd")
    file.copy("report.rmd", tempReport, overwrite = TRUE)

    # Set up parameters to pass to Rmd document
    # params <- list(
    #   MRN_patient = input$MRN,
    #   First_name_donor = input$DonorID
    # )

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, output_file = file,
                      #params = params,
                      envir = new.env(parent = globalenv())

    )

  }
)



output$downloadReport <- downloadHandler(
  filename = function() {
    paste('my-report', sep = '.', switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },

  content = function(file) {
    src <- normalizePath('report.Rmd')

    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd', overwrite = TRUE)

    library(rmarkdown)
    out <- render('report.Rmd', switch(
      input$format,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    file.rename(out, file)
  }
)
########

datos$ESTADO <- ifelse(
  datos$ESTADO =="Reprobado",
  cell_spec(datos$ESTADO, color = "red", bold = T),
  cell_spec(datos$ESTADO, color = "green", bold = T)
)
kbl(datos, escape = FALSE) %>% kable_paper("hover", full_width = T) %>%
  add_header_above(c(" "=2, "DATOS PERSONALES"=2, "RESULTADOS DE LA EVALUACIÓN"=3)) %>%
  kable_styling(
    bootstrap_options = c("hover", "condensed", "responsive"),
    full_width = TRUE
 )
###################### preguntas evaluaciones postulantes #############
#question
question=c("¿Cuál es tu nombre?", #1
           "Número de documento de identidad:",#1.1
           "¿Cuál es tu edad?", #2
           rep("¿Cuál opción describe mejor tu género?",2),#3
rep("¿Un cliente o persona que se niegue a dar información requerida o que mienta en sus declaraciones, es considerado cliente sospechoso?",2),#4
rep("¿Las señales de alerta permiten identificar operaciones que presentan situaciones
    inusuales y que en algunos casos son operaciones de Lavado de Dinero y Financiamiento del Terrorismo?",2),#5
rep("¿Un ROS es la comunicación mediante la cual los Sujetos Obligados reportan cualquier hecho u operación con independencia de su cuantía,
    por hechos o situaciones que posiblemente están relacionados con el Lavado de Activos o Financiación del Terrorismo?",2),#6
rep("¿El Financiamiento al Terrorismo es el acto de proporcionar apoyo por cualquier medio a
    terroristas u organizaciones terroristas a fin de permitirles realizar actos de terrorismo?",2),#7
rep("¿Los clientes que frecuentemente solicitan que se incrementen los límites de excepción, son considerados como clientes sospechosos?",2),#8
rep("¿Los clientes cuyo teléfono se encuentra desconectado, o el número telefónico al momento de efectuar
    la llamada de verificación, no concuerda con la información inicialmente suministrada, es considerado como una señal de alerta?"),#9
rep("¿Una operación inusual es aquella cuyo monto o magnitud, características particulares,
    y periodicidad o velocidad de rotación con que son ejecutadas, no guardan relación
    con la actividad económica del cliente, salen de los parámetros de comportamiento transaccional,
    o si fuera el caso no tienen un fundamento legal evidente al momento de requerir el respaldo correspondiente?",2),#10
rep("¿Una persona o cliente que presiona a cualquier funcionario del Banco para no
    diligenciar formularios que impliquen el registro de la información o el reporte de la operación,
    es considerado como cliente sospechoso?",2),#11
rep("¿Las cuentas que muestran elevadas transacciones en efectivo para negocios que generalmente
    NO manejan grandes sumas de dinero en efectivo, es considerada una señal de alerta?",2),#12
rep("¿El objeto de lavado de activos consiste en hacer que los fondos o activos obtenidos a través
    de actividades ilícitas aparezcan como el fruto de actividades legítimas y circulen sin problema en el sistema financiero?",2),#13 hasta la 10
rep("¿El Código Penal Boliviano como define al Lavado de Dinero?",4),#14
rep("¿Qué es el GAFI?",3),#15
rep("¿El Financiamiento del Terrorismo puede ser financiado por medio de Fondos obtenidos de manera?",4),#16
rep("¿Qué es una Operación inusual?",4),#17
rep("¿Que es una operación sospechosa?",4),#18
rep("¿A qué se denomina ROS?",3),#19
rep("La Política “conozca a su cliente” consiste en:",4),#20
rep("¿Cuantos años se sanciona con privación de libertad el Delito de Legitimación de Ganancias Ilícitas en Bolivia?",4),#21
rep("¿El delito al Financiamiento del Terrorismo es autónomo y será investigado, enjuiciado y sentenciado sin necesidad de sentencia previa por Delitos Conexos?",2),#22
rep("¿Es importante actualizar en forma periódica y sistemática los datos suministrados por los clientes?",2),#23
rep("Las transferencias electrónicas,
    sin aparente razón comercial ni consistencia con los negocios habituales o actividad económica declarada del cliente. Representan una:",3),#24
rep("Cuando el cliente ofrece pagar jugosas comisiones, sin justificativo legal y lógico. ¿Representa una señal de alerta?",2),#25
rep("La cancelación repentina de préstamos,
    sin justificación aparente sobre la razón del pago súbito o el origen de los fondos. ¿Representa una señal de alerta?",2),#26
rep("El frecuente envió o recepción de grandes volúmenes de transferencias electrónicas de o hacia instituciones. ¿Representa una señal de alerta?",2),#27
rep("La Ley N° 262, es el Régimen de congelamiento de:",3),#28
rep("El Decreto Supremo 910, cuya finalidad es:",3),#29
rep("¿Toda transacción por montos relevantes debe tener un respaldo del motivo del movimiento, depósito, origen o destino?",2),#30
rep("¿Cuáles son delitos precedentes para la Legitimación de Ganancias Ilícitas?",3),#31
rep("¿Cuáles son los riesgos que podría llegar a tener Banco Sol en caso de ser usado en operaciones de Lavado de Dinero y Financiamiento del Terrorismo?",3),#32
rep("¿Cuáles son las 3 etapas del Lavado de Dinero?",3),#33
rep("¿Cuáles son las 3 etapas del Financiamiento del Terrorismo?",3),#34
rep("¿Cuántos años se sanciona con privación de libertad al Delito de Financiamiento del Terrorismo?",3),#35
rep("¿La Legitimación de Ganancias Ilícitas también es conocida cómo?",3),#36
rep("¿Cuál es el artículo en el código penal que menciona al Financiamiento del Terrorismo?",3),#37
rep("¿Cuál es el artículo en el código penal que menciona al Legitimación de Ganancias Ilícitas?",3),#38
rep("El dinero de origen ilícito, nunca llega a ser LEGAL",2),#39
rep("¿Cuál es una de las etapas del proceso de Legitimación de Ganancias Ilícitas?",3),#40
rep("¿Cuál es una de las etapas del proceso de Financiamiento del Terrorismo?",4),#41
rep("¿Cuál es una de las etapas del proceso de Legitimación de Ganancias Ilícitas?",4),#42
rep("Uno de los riesgos que podrían llegar a tener el Banco Solidario S.A.
    en caso de ser usado en operaciones de Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería el siguiente:",3),#43
rep("Uno de los riesgos que podrían llegar a tener el Banco Solidario S.A. en caso de ser usado
    en operaciones de Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería el siguiente:",4),#44
rep("Uno de los riesgos que podrían llegar a tener el Banco Solidario S.A. en caso de ser usado en operaciones de
    Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería el siguiente:",3),#45
rep("¿Cuál es una de las etapas del proceso de Financiamiento del Terrorismo?",3),#46
rep("¿Cuál es una de las etapas del proceso de Financiamiento del Terrorismo?",4),#47
rep("¿Cuál es una de las etapas del proceso de Financiamiento del Terrorismo?",3),#48
rep("¿Cuál es el artículo en el código penal que menciona al Financiamiento De La Proliferación De Armas De Destrucción Masiva?",3),#49
rep("La Política de Conocimiento del Cliente no debe mirarse como una actividad aislada que no
    cumple con los objetivos principales del negocio. Por el contrario, una política efectiva de
    Conocimiento del Cliente puede servir de base para una exitosa gestión comercial y de administración del riesgo",2),#50
rep("La Política de Conocimiento del Cliente es la recolección de fuentes de financiación, ya sean Legales o Ilegales",2),#51
rep("¿El Financiamiento al Terrorismo es la implementación de bases fundamentales para el conocimiento financiero?",2),#52
rep("¿Que es una operación sospechosa?",3)#53



)

#option
option=c("Nombres y Apellidos",#1
         "",#1.1
         "27",#2
         c("Masculino","Femenino"),#3
c("a) Verdadero","b) Falso"),#4
c("a) Verdadero","b) Falso"),#5
c("a) Verdadero","b) Falso"),#6
c("a) Verdadero","b) Falso"),#7
c("a) Verdadero","b) Falso"),#8
c("a) Verdadero","b) Falso"),#9
c("a) Verdadero","b) Falso"),#10
c("a) Verdadero","b) Falso"),#11
c("a) Verdadero","b) Falso"),#12
c("a) Verdadero","b) Falso"),#13 hasta la 10
c("a) Lavado de Activos","b) Lavado de Dinero","c) Legitimación de Ganancias Ilícitas","d) Todas son Correctas"),#14
c("a) Grupo de Acción Financiamiento Internacional","b) Grupo de Acción Financiera Internacional",
  "c) Grupo Antilavado Financiero Internacional"),#15
c("a) Fondos de origen Legítimos","b) Fondos de origen Ilegítimos","c) Fondos de origen Legítimos e Ilegítimos","d) Ninguna es correcta"),#16
c("a) Toda aquella operación que tiene como característica el manejo de dinero únicamente en efectivo",
  "b) Es toda operación realizada por personas en forma sospechosa",
  "c) Son aquellas operaciones que se alejan al perfil transaccional del cliente o no guardan relación con sus ingresos de acuerdo con su actividad, negocio o profesión",
  "d) Todas las anteriores son correctas"),#17
c("a) Toda aquella transacción financiera que es realizada por un criminal",
  "b) Es la operativa de un cliente o usuario que se realiza a nombre de un tercero",
  "c) Es una operación calificada como inusual cuya observación no puedo ser justificada o respaldada",
  "d) Ninguna es correcta"),#18
c("a) Al reporte de Retenciones, Observaciones y Sospechas","b) Al procedimiento de Reconocer Objetos Sospechosos",
  "c) A los reportes de Operaciones Sospechosas"),#19
c("a) Conocer los datos relacionados a su fuente de trabajo y sus ingresos económicos",
  "b) Saber dónde vive",
  "c) Obtener del cliente toda la información posible, relacionada a su entorno de vida, entorno económico o de actividad laboral ya sea principal o secundarias",
  "d) Saber su nivel de vida, y la constitución de su Familia"),#20
c("a)  De 1 a 5 años","b)  De 3 a 5 años","c)  De 5 a 7 años","d) De 5 a 10 años"),#21
c("a) Verdadero","b) Falso"),#22
c("a) Verdadero","b) Falso"),#23
c("a) operación normal","b) señal habitual","c) señal de alerta"),#24
c("a) Verdadero","b) Falso"),#25
c("a) Verdadero","b) Falso"),#26
c("a) Verdadero","b) Falso"),#27
c("a) fondos y otros activos de personas vinculadas con acciones de terrorismo y financiamiento del terrorismo",
  "b) habilitación de personas","c) tratado de armas"),#28
c("a) habilitar cuentas de los clientes","b) emitir sanciones por incumplimiento","c) normar las alertas de clientes"),#29
c("a) Verdadero","b) Falso"),#30
c("a) tráfico de sustancias ilícitas, contrabando, corrupción, organización criminal y estafas",
  "b) pagos basados en acciones, Combinaciones de Negocios, Contratos de Seguro y reformas de crédito",
  "c) Boletas de pago actualizadas, Certificado de trabajo actualizado, Estado de ahorro previsional AFPs"),#31
c("a) Riesgo Reputacional, Riesgo Operativo, Riesgo Legal y Riesgo de Contagio",
  "b) Riesgo conceptual, Riesgo variable, Riesgo de Contagio",
  "c) Riesgo de evaluación, Riesgo sumatorio y Riesgo de Operativo"),#32
c("a) contrabando, lesiones gravísimas y secuestro","b) Condonación, colocación e Información",
  "c) Colocación, Estratificación o diversificación e Integración"),#33
c("a) Restitución, colocación e Información","b) Recolección, Disposición y Utilización","c) Recolección, defraudación y distribución"),#34
c("a) Será sancionado con presidio de treinta (30) a cincuenta (50) años",
  "b) Será sancionado con presidio de quince (15) a veinte (20) años","c) Será sancionado con presidio de dos (2) a cinco (5) años"),#35
c("a) Delito de sobornos","b) Lavado de Activos","c) Corrupción"),#36
c("a) Artículo 133 Bis","b) Artículo 20 Bis","c) Articulo 103"),#37
c("a) Artículo 55","b) Artículo 88 Bis","c) Artículo 185 Bis"),#38
c("a) Verdadero","b) Falso"),#39
c("a) Colocación","b) Utilización","c) Confrontación"),#40
c("a) Colocación","b) Transformación","c) Integración","d) Ninguna"),#41
c("a) Conciliación","b) Transformación","c) Restitución","d) Ninguna"),#42
c("a) Riesgo Reputacional","b) Riesgo Financiero","c) Riesgo Ocupacional"),#43
c("a) Riesgo Financiero","b) Riesgo Sistemático","c) Riesgo Operativo","d) Ninguno"),#44
c("a) Riesgo Legal","b) Riesgo Crediticio","c) Riesgo Ocupacional"),#45
c("a) Corrupción","b) Disposición","c) Fabricación"),#46
c("a) Restablecimiento","b) Disponibilidad","c) Utilización"),#47
c("a) Recaudación","b) Sustitución","c) Restablecimiento"),#48
c("a) Artículo 105 Bis","b) Artículo 155 Bis","c) Artículo 141 Bis"),#49
c("a) Verdadero","b) Falso"),#50
c("a) Verdadero","b) Falso"),#51
c("a) Verdadero","b) Falso"),#52
c("a) Es una operación calificada como inusual cuya observación no puedo ser justificada o respaldada",
  "b) Es el sistema de consultas de alertas","c) Es la última fase del lavado de activos")#53




)


# input_type
input_type=c("text", #1
             "text",#1.1
             "numeric",#2
             c("mc","mc"),#3
             # "text",#3.1
c("mc","mc"),#4
c("mc","mc"),#5
c("mc","mc"),#6
c("mc","mc"),#7
c("mc","mc"),#8
c("mc","mc"),#9
c("mc","mc"),#10
c("mc","mc"),#11
c("mc","mc"),#12
c("mc","mc"),#13 hasta la 10
c("mc","mc","mc","mc"),#14
c("mc","mc","mc"),#15
c("mc","mc","mc","mc"),#16
c("mc","mc","mc","mc"),#17
c("mc","mc","mc","mc"),#18
c("mc","mc","mc"),#19
c("mc","mc","mc","mc"),#20
c("mc","mc","mc","mc"),#21
c("mc","mc"),#22
c("mc","mc"),#23
c("mc","mc","mc"),#24
c("mc","mc"),#25
c("mc","mc"),#26
c("mc","mc"),#27
c("mc","mc","mc"),#28
c("mc","mc","mc"),#29
c("mc","mc"),#30
c("mc","mc","mc"),#31
c("mc","mc","mc"),#32
c("mc","mc","mc"),#33
c("mc","mc","mc"),#34
c("mc","mc","mc"),#35
c("mc","mc","mc"),#36
c("mc","mc","mc"),#37
c("mc","mc","mc"),#38
c("mc","mc"),#39
c("mc","mc","mc"),#40
c("mc","mc","mc","mc"),#41
c("mc","mc","mc","mc"),#42
c("mc","mc","mc"),#43
c("mc","mc","mc","mc"),#44
c("mc","mc","mc"),#45
c("mc","mc","mc"),#46
c("mc","mc","mc"),#47
c("mc","mc","mc"),#48
c("mc","mc","mc"),#49
c("mc","mc"),#50
c("mc","mc"),#51
c("mc","mc"),#52
c("mc","mc","mc")#53

)

# input_id
input_id=c("nombre",#1
           "num_doc",#1.1
           "edad",#2
           c("genero","genero"),#3
           # "otro",#3.1
rep("p1",2),#4
rep("p2",2),#5
rep("p3",2),#6
rep("p4",2),#7
rep("p5",2),#8
rep("p6",2),#9
rep("p7",2),#10
rep("p8",2),#11
rep("p9",2),#12
rep("p10",2),#13 hasta la 10
rep("p11",4),#14
rep("p12",3),#15
rep("p13",4),#16
rep("p14",4),#17
rep("p15",4),#18
rep("p16",3),#19
rep("p17",4),#20
rep("p18",4),#21
rep("p19",2),#22
rep("p20",2),#23
rep("p21",3),#24
rep("p22",2),#25
rep("p23",2),#26
rep("p24",2),#27
rep("p25",3),#28
rep("p26",3),#29
rep("p27",2),#30
rep("p28",3),#31
rep("p29",3),#32
rep("p30",3),#33
rep("p31",3),#34
rep("p32",3),#35
rep("p33",3),#36
rep("p34",3),#37
rep("p35",3),#38
rep("p36",2),#39
rep("p37",3),#40
rep("p38",4),#41
rep("p39",4),#42
rep("p40",3),#43
rep("p41",4),#44
rep("p42",3),#45
rep("p43",3),#46
rep("p44",3),#47
rep("p45",3),#48
rep("p46",3),#49
rep("p47",2),#50
rep("p48",2),#51
rep("p49",2),#52
rep("p50",3)#53
)



# dependence
dependence=c(NA,#1
             NA,#1.1
             NA,#2
             c(NA,NA),#3
             #"genero", #3.1
rep(NA,2),#4
rep(NA,2),#5
rep(NA,2),#6
rep(NA,2),#7
rep(NA,2),#8
rep(NA,2),#9
rep(NA,2),#10
rep(NA,2),#11
rep(NA,2),#12
rep(NA,2),#13 hasta la 10
rep(NA,4),#14
rep(NA,3),#15
rep(NA,4),#16
rep(NA,4),#17
rep(NA,4),#18
rep(NA,3),#19
rep(NA,4),#20
rep(NA,4),#21
rep(NA,2),#22
rep(NA,2),#23
rep(NA,3),#24
rep(NA,2),#25
rep(NA,2),#26
rep(NA,2),#27
rep(NA,3),#28
rep(NA,3),#29
rep(NA,2),#30
rep(NA,3),#31
rep(NA,3),#32
rep(NA,3),#33
rep(NA,3),#34
rep(NA,3),#35
rep(NA,3),#36
rep(NA,3),#37
rep(NA,3),#38
rep(NA,2),#39
rep(NA,3),#40
rep(NA,4),#41
rep(NA,4),#42
rep(NA,3),#43
rep(NA,4),#44
rep(NA,3),#45
rep(NA,3),#46
rep(NA,3),#47
rep(NA,3),#48
rep(NA,3),#49
rep(NA,2),#50
rep(NA,2),#51
rep(NA,2),#52
rep(NA,3)#53
)


dependence_value=c(NA,#1
                   NA,#1.1
                   NA,#2
                   c(NA,NA),#3
                   # "Otro", #3.1
rep(NA,2),#4
rep(NA,2),#5
rep(NA,2),#6
rep(NA,2),#7
rep(NA,2),#8
rep(NA,2),#9
rep(NA,2),#10
rep(NA,2),#11
rep(NA,2),#12
rep(NA,2),#13 hasta la 10
rep(NA,4),#14
rep(NA,3),#15
rep(NA,4),#16
rep(NA,4),#17
rep(NA,4),#18
rep(NA,3),#19
rep(NA,4),#20
rep(NA,4),#21
rep(NA,2),#22
rep(NA,2),#23
rep(NA,3),#24
rep(NA,2),#25
rep(NA,2),#26
rep(NA,2),#27
rep(NA,3),#28
rep(NA,3),#29
rep(NA,2),#30
rep(NA,3),#31
rep(NA,3),#32
rep(NA,3),#33
rep(NA,3),#34
rep(NA,3),#35
rep(NA,3),#36
rep(NA,3),#37
rep(NA,3),#38
rep(NA,2),#39
rep(NA,3),#40
rep(NA,4),#41
rep(NA,4),#42
rep(NA,3),#43
rep(NA,4),#44
rep(NA,3),#45
rep(NA,3),#46
rep(NA,3),#47
rep(NA,3),#48
rep(NA,3),#49
rep(NA,2),#50
rep(NA,2),#51
rep(NA,2),#52
rep(NA,3)#53
)

# required
required=c(T,#1
           T,#1.1
           T,#2
           rep(T,2),#3
           # F,#3.1
rep(T,2),#4
rep(T,2),#5
rep(T,2),#6
rep(T,2),#7
rep(T,2),#8
rep(T,2),#9
rep(T,2),#10
rep(T,2),#11
rep(T,2),#12
rep(T,2),#13 hasta la 10
rep(T,4),#14
rep(T,3),#15
rep(T,4),#16
rep(T,4),#17
rep(T,4),#18
rep(T,3),#19
rep(T,4),#20
rep(T,4),#21
rep(T,2),#22
rep(T,2),#23
rep(T,3),#24
rep(T,2),#25
rep(T,2),#26
rep(T,2),#27
rep(T,3),#28
rep(T,3),#29
rep(T,2),#30
rep(T,3),#31
rep(T,3),#32
rep(T,3),#33
rep(T,3),#34
rep(T,3),#35
rep(T,3),#36
rep(T,3),#37
rep(T,3),#38
rep(T,2),#39
rep(T,3),#40
rep(T,4),#41
rep(T,4),#42
rep(T,3),#43
rep(T,4),#44
rep(T,3),#45
rep(T,3),#46
rep(T,3),#47
rep(T,3),#48
rep(T,3),#49
rep(T,2),#50
rep(T,2),#51
rep(T,2),#52
rep(T,3)#53
)

##
"question"         "option"           "input_type"       "input_id"         "dependence"       "dependence_value" "required"
#### datos para el dataframe anterior de acuerdo al instructivo de la UIF ####
question=c("¿Cuál es tu nombre?", #1
           "Número de documento de identidad:",#1.1
           "¿Cuál es tu edad?", #2
           rep("¿Cuál opción describe mejor tu género?",2),#3
           #rep("¿Cuál opción describe mejor tu género?",1),#3.1
           #desde aqui las preguntas
           rep("¿El sujeto obligado debe conservar en medios físicos o digitales: Todos los registros
               transferencias electrónicas, instrumentos electrónicos de pago y órdenes de pago, nacionales como internacionales
               durante:?",3),#4
           rep("Todos los registros y documentación obtenidos a través de los procedimientos y medidas de D.D.,
               archivos de cuenta, correspondencia comercial y los resultados de análisis realizados,
               debe conservarse durante, (Escoger opción), despues de finalizada la relación comercial o después de la fecha
               de la transacción ocasional",3),#5
           rep("¿Debería ser una señal de alerta cuando, un cliente realiza pequeñas pero frecuentes cantidades de dinero en efectivo,
               cuya suma constituye un importe relevante?",2),#6
           rep("¿Es una señal de alerta cuándo (Escoger opción) tienen un aumento sustancial y sin razón aparente de ingresos en efectivo, y
                    que a continuación ordenan transferencias hacia destinos que, a juicio del Sujeto Obligado,
                    no guardan relación con ellos?",2),#7
           rep("¿No debería ser una señal de alerta cuando existen cuentas que reciben ingresos en efectivo realizados por numerosas personas y/o desde
                    distintas oficinas o localidades?",2),#8
           rep("¿Debería ser una señal de alerta cuando, un cliente recibe ingresos en efectivo realizados por numerosas
                   personas y/o desde distintas oficinas o localidades?",2),#9
           rep("El Plan Anual de Trabajo para la siguiente gestión, elaborado por el Funcionario Responsable,
                   no es necesario ser puesto en conocimiento del Directorio u Órgano equivalente para su aprobación",2),#10
           rep("El Sujeto Obligado debe conformar la Unidad de Cumplimiento como parte
                   de su estructura orgánica u organizativa, considerando que dicha Unidad No debe:",5),#11
           rep("Los Sujetos Obligados que conforman un Grupo Financiero con registro en ASFI,
                   no es necesario contar con una Unidad de Cumplimiento (Funcionario Responsable y Analistas)
                   independientemente de las Unidades de Cumplimiento de los Sujetos Obligados que forman parte del Grupo Financiero.",2),#12
           rep("El tamaño de la Unidad de Cumplimiento dependerá de los siguientes factores:",7),#13
           rep("El Sujeto Obligado debe mantener un registro histórico documentado de los nombramientos
                   del Funcionario Responsable, Funcionario Responsable Suplente y Analistas de la
                   Unidad de Cumplimiento, así como los reemplazos efectuados del Funcionario Responsable.",2),#14
           rep("El Sujeto Obligado, a través del Funcionario Responsable, debe elaborar un Programa Anual de
                    Capacitación, que no debe estar incluido dentro del Plan Anual de Trabajo de la Unidad de Cumplimiento
                    y aprobado por el Directorio u Órgano equivalente; dicho programa debe tener un cronograma
                    específico y contener como mínimo las capacitaciones señaladas en el Artículo 60 del presente
                    instructivo. Asimismo, el citado programa puede formar parte del Plan Anual de Capacitación del
                    Sujeto Obligado.",2),#15
           rep("¿Es una señal de alerta cuándo se realizan depósitos importantes de dinero en efectivo hechos por una persona natural o jurídica cuyas
                    actividades aparentes deben normalmente producir ingresos de este tipo?",2),#16
           rep("¿Es una señal de alerta cuándo personas (Escoger Opción) realizan depósitos por importes altos y que realizan
                    actividades que habitualmente generan movimientos de cheques?",3),#17
           rep("¿Es una señal de alerta cuándo clientes depositan o cobran más de tres veces billetes premiados de Lotería?",2),#18
           rep("¿Es una señal de alerta cuándo clientes depositan sumas de dinero obtenidas por el sector de juegos de azar y casinos?",2)

)
option=c("Nombres y Apellidos",#1
         "",#1.1
         "27",#2
         c("Masculino","Femenino"),#3
         # NA,#3.1
         c("10 años","Máximo 10 años", "Al menos 10 años"),#4
         c(">= 10 años", "10 años","<= 10 años" ),#5
         c("SI","NO"),#6
         c("Personas Naturales","Clientes"),#7
         c("Falso","Verdadero"),#8
         c("SI","NO"),#9
         c("Falso","Verdadero"),#10
         c("a)Depender orgánica y funcionalmente del Directorio u Órgano equivalente",
           "b)Estar conformada por el funcionario Responsable, y cuando corresponda, por
               el Analista o los Analistas o funcionarios de la Unidad de Cumplimiento,
               ambos con dedicación exclusiva y sin conflicto de intereses",
           "c)Estar dirigida por el Funcionario Responsable quien debe tener nivel jerárquico gerencial",
           "d)Ninguno",
           "a), b) y c)"),#11
         c("Falso","Verdadero"),#12
         c("a) Tamaño y características de la entidad","b) Volumen de transacciones y complejidad de operaciones",
           "c) Cantidad de clientes","d) Nivel de exposición de Riesgo de LGI/FT y FPADM",
           "a) y d)","Ninguno","a), b), c) y d)"),#13
         c("Falso","Verdadero"),#14
         c("Verdadero","Falso"),#15
         c("Verdadero","Falso"),#16
         c("a) Naturales","b) Jurídicas","a) y b)"),#17
         c("Falso","Verdadero"),#18
         c("Falso","Verdadero")
)
input_type=c("text", #1
             "text",#1.1
             "numeric",#2
             c("mc","mc"),#3
             # "text",#3.1
             c("mc","mc","mc"),#4
             c("mc","mc","mc"),#5
             c("y/n","y/n"),#6
             c("mc","mc"),#7
             c("mc","mc"),#8
             c("y/n","y/n"),#9
             c("mc","mc"),#10
             c("mc","mc","mc","mc","mc"),#11
             c("mc","mc"),#12
             c("mc","mc","mc","mc","mc","mc","mc"),#13
             c("mc","mc"),#14
             c("mc","mc"),#15
             c("mc","mc"),#16
             c("mc","mc","mc"),#17
             c("mc","mc"),#18
             c("mc","mc")
)
input_id=c("nombre",#1
           "num_doc",#1.1
           "edad",#2
           c("genero","genero"),#3
           # "otro",#3.1
           c("p1","p1","p1"),#4
           c("p2","p2","p2"),#5
           c("p3","p3"),#6
           c("p4","p4"),#7
           c("p5","p5"),#8
           c("p6","p6"),#9
           c("p7","p7"),#10
           c("p8","p8","p8","p8","p8"),#11
           c("p9","p9"),#12
           c("p10","p10","p10","p10","p10","p10","p10"),#13
           c("p11","p11"),#14
           c("p12","p12"),#15
           c("p13","p13"),#16
           c("p14","p14","p14"),#17
           c("p15","p15"),#18
           c("p16","p16")
)
dependence=c(NA,#1
             NA,#1.1
             NA,#2
             c(NA,NA),#3
             #"genero", #3.1
             c(NA,NA,NA),#4
             c(NA,NA,NA),#5
             c(NA,NA),#6
             c(NA,NA),#7
             c(NA,NA),#8
             c(NA,NA),#9
             c(NA,NA),#10
             rep(NA,5),#11
             rep(NA,2),#12
             rep(NA,7),#13
             rep(NA,2),#14
             rep(NA,2),#15
             rep(NA,2),#16
             rep(NA,3),#17
             rep(NA,2),#18
             rep(NA,2)
)
dependence_value=c(NA,#1
                   NA,#1.1
                   NA,#2
                   c(NA,NA),#3
                   # "Otro", #3.1
                   c(NA,NA,NA),#4
                   c(NA,NA,NA),#5
                   rep(NA,2),#6
                   rep(NA,2),#7
                   rep(NA,2),#8
                   rep(NA,2),#9
                   rep(NA,2),#10
                   rep(NA,5),#11
                   rep(NA,2),#12
                   rep(NA,7),#13
                   rep(NA,2),#14
                   rep(NA,2),#15
                   rep(NA,2),#16
                   rep(NA,3),#17
                   rep(NA,2),#18
                   rep(NA,2)
)
required=c(T,#1
           T,#1.1
           T,#2
           rep(T,2),#3
           # F,#3.1
           c(F,F,F),#4
           c(F,F,F),#5
           rep(T,2),#6
           rep(T,2),#7
           rep(T,2),#8
           rep(T,2),#9
           rep(T,2),#10
           rep(T,5),#11
           rep(T,2),#12
           rep(T,7),#13
           rep(T,2),#14
           rep(T,2),#15
           rep(T,2),#16
           rep(T,3),#17
           rep(T,2),#18
           rep(T,2)
)


# respuestas correctas
Respuestas_correctas= c("a) Verdadero","a) Verdadero","a) Verdadero","a) Verdadero" ,"a) Verdadero",
                        "a) Verdadero","a) Verdadero","a) Verdadero","a) Verdadero","a) Verdadero",#hasta la 10
                        "c) Legitimación de Ganancias Ilícitas","b) Grupo de Acción Financiera Internacional","c) Fondos de origen Legítimos e Ilegítimos",
                        "c) Son aquellas operaciones que se alejan al perfil transaccional del cliente o no guardan relación con sus ingresos de acuerdo con su actividad, negocio o profesión",
                        "c) Es una operación calificada como inusual cuya observación no puedo ser justificada o respaldada",
                        "c) A los reportes de Operaciones Sospechosas",
                        "c) Obtener del cliente toda la información posible, relacionada a su entorno de vida, entorno económico o de actividad laboral ya sea principal o secundarias",
                        "d) De 5 a 10 años",
                        "a) Verdadero",
                        "a) Verdadero",#hasta la 20
                        "c) señal de alerta","a) Verdadero","a) Verdadero","a) Verdadero",
                        "a) fondos y otros activos de personas vinculadas con acciones de terrorismo y financiamiento del terrorismo",
                        "b) emitir sanciones por incumplimiento","a) Verdadero",
                        "a) tráfico de sustancias ilícitas, contrabando, corrupción, organización criminal y estafas",
                        "a) Riesgo Reputacional, Riesgo Operativo, Riesgo Legal y Riesgo de Contagio",
                        "c) Colocación, Estratificación o diversificación e Integración", # hasta la 30
                        "b) Recolección, Disposición y Utilización",
                        "b) Será sancionado con presidio de quince (15) a veinte (20) años",
                        "b) Lavado de Activos","a) Artículo 133 Bis","c) Artículo 185 Bis","a) Verdadero","a) Colocación","d) Ninguna","b) Transformación",
                        "a) Riesgo Reputacional",# hasta la 40
                        "c) Riesgo Operativo","a) Riesgo Legal","b) Disposición","c) Utilización","a) Recaudación","c) Artículo 141 Bis","a) Verdadero",
                        "b) Falso","b) Falso","a) Es una operación calificada como inusual cuya observación no puedo ser justificada o respaldada"
                        )
Respuestas_correctas= Respuestas_correctas[-c(43:45,39,50)]

######## dataframen de las respuestas correctas ############
question=c("¿Cuál es tu nombre?", #1
           "Número de documento de identidad:",#1.1
           "¿Cuál es tu edad?", #2
           rep("¿Cuál opción describe mejor tu género?",2),#3
           rep("¿Un cliente o persona que se niegue a dar información requerida o que mienta en sus declaraciones, es considerado cliente sospechoso?",2),#4
           rep("¿Las señales de alerta permiten identificar operaciones que presentan situaciones
    inusuales y que en algunos casos son operaciones de Lavado de Dinero y Financiamiento del Terrorismo?",2),#5
    rep("¿Un ROS es la comunicación mediante la cual los Sujetos Obligados reportan cualquier hecho u operación con independencia de su cuantía,
    por hechos o situaciones que posiblemente están relacionados con el Lavado de Activos o Financiación del Terrorismo?",2),#6
    rep("¿El Financiamiento al Terrorismo es el acto de proporcionar apoyo por cualquier medio a
    terroristas u organizaciones terroristas a fin de permitirles realizar actos de terrorismo?",2),#7
    rep("¿Los clientes que frecuentemente solicitan que se incrementen los límites de excepción, son considerados como clientes sospechosos?",2),#8
    rep("¿Los clientes cuyo teléfono se encuentra desconectado, o el número telefónico al momento de efectuar
    la llamada de verificación, no concuerda con la información inicialmente suministrada, es considerado como una señal de alerta?",2),#9
    rep("¿Una operación inusual es aquella cuyo monto o magnitud, características particulares,
    y periodicidad o velocidad de rotación con que son ejecutadas, no guardan relación
    con la actividad económica del cliente, salen de los parámetros de comportamiento transaccional,
    o si fuera el caso no tienen un fundamento legal evidente al momento de requerir el respaldo correspondiente?",2),#10
    rep("¿Una persona o cliente que presiona a cualquier funcionario del Banco para no
    diligenciar formularios que impliquen el registro de la información o el reporte de la operación,
    es considerado como cliente sospechoso?",2),#11
    rep("¿Las cuentas que muestran elevadas transacciones en efectivo para negocios que generalmente
    NO manejan grandes sumas de dinero en efectivo, es considerada una señal de alerta?",2),#12
    rep("¿El objeto de lavado de activos consiste en hacer que los fondos o activos obtenidos a través
    de actividades ilícitas aparezcan como el fruto de actividades legítimas y circulen sin problema en el sistema financiero?",2),#13 hasta la 10
    rep("¿El Código Penal Boliviano como define al Lavado de Dinero?",4),#14
    rep("¿Qué es el GAFI?",3),#15
    rep("¿El Financiamiento del Terrorismo puede ser financiado por medio de Fondos obtenidos de manera?",4),#16
    rep("¿Qué es una Operación inusual?",4),#17
    rep("¿Que es una operación sospechosa?",4),#18
    rep("¿A qué se denomina ROS?",3),#19
    rep("La Política “conozca a su cliente” consiste en:",4),#20
    rep("¿Cuantos años se sanciona con privación de libertad el Delito de Legitimación de Ganancias Ilícitas en Bolivia?",4),#21
    rep("¿El delito al Financiamiento del Terrorismo es autónomo y será investigado, enjuiciado y sentenciado sin necesidad de sentencia previa por Delitos Conexos?",2),#22
    rep("¿Es importante actualizar en forma periódica y sistemática los datos suministrados por los clientes?",2),#23
    rep("Las transferencias electrónicas,
    sin aparente razón comercial ni consistencia con los negocios habituales o actividad económica declarada del cliente. Representan una:",3),#24
    rep("Cuando el cliente ofrece pagar jugosas comisiones, sin justificativo legal y lógico. ¿Representa una señal de alerta?",2),#25
    rep("La cancelación repentina de préstamos,
    sin justificación aparente sobre la razón del pago súbito o el origen de los fondos. ¿Representa una señal de alerta?",2),#26
    rep("El frecuente envió o recepción de grandes volúmenes de transferencias electrónicas de o hacia instituciones. ¿Representa una señal de alerta?",2),#27
    rep("La Ley N° 262, es el Régimen de congelamiento de:",3),#28
    rep("El Decreto Supremo 910, cuya finalidad es:",3),#29
    rep("¿Toda transacción por montos relevantes debe tener un respaldo del motivo del movimiento, depósito, origen o destino?",2),#30
    rep("¿Cuáles son delitos precedentes para la Legitimación de Ganancias Ilícitas?",3),#31
    rep("¿Cuáles son los riesgos que podría llegar a tener Banco Sol en caso de ser usado en operaciones de Lavado de Dinero y Financiamiento del Terrorismo?",3),#32
    rep("¿Cuáles son las 3 etapas del Lavado de Dinero?",3),#33
    rep("¿Cuáles son las 3 etapas del Financiamiento del Terrorismo?",3),#34
    rep("¿Cuántos años se sanciona con privación de libertad al Delito de Financiamiento del Terrorismo?",3),#35
    rep("¿La Legitimación de Ganancias Ilícitas también es conocida cómo?",3),#36
    rep("¿Cuál es el artículo en el código penal que menciona al Financiamiento del Terrorismo?",3),#37
    rep("¿Cuál es el artículo en el código penal que menciona al Legitimación de Ganancias Ilícitas?",3),#38
    rep("El dinero de origen ilícito, nunca llega a ser LEGAL",2),#39
    rep("¿Cuál es una de las etapas del proceso de Legitimación de Ganancias Ilícitas?",3),#40
    rep("¿Cuál es una de las etapas del proceso de Financiamiento del Terrorismo?",4),#41
    #rep("¿Cuál es una de las etapas del proceso de Legitimación de Ganancias Ilícitas?",4),#42
    rep("Uno de los riesgos que podrían llegar a tener el Banco Solidario S.A.
    en caso de ser usado en operaciones de Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería el siguiente:",3),#43
    rep("Uno de los riesgos que podrían llegar a tener el Banco Solidario S.A. en caso de ser usado
    en operaciones de Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería el siguiente:",4),#44
    rep("Uno de los riesgos que podrían llegar a tener el Banco Solidario S.A. en caso de ser usado en operaciones de
    Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería el siguiente:",3),#45
    # rep("¿Cuál es una de las etapas del proceso de Financiamiento del Terrorismo?",3),#46
    # rep("¿Cuál es una de las etapas del proceso de Financiamiento del Terrorismo?",3),#47
    # rep("¿Cuál es una de las etapas del proceso de Financiamiento del Terrorismo?",3),#48
    rep("¿Cuál es el artículo en el código penal que menciona al Financiamiento De La Proliferación De Armas De Destrucción Masiva?",3),#49
    rep("La Política de Conocimiento del Cliente no debe mirarse como una actividad aislada que no
    cumple con los objetivos principales del negocio. Por el contrario, una política efectiva de
    Conocimiento del Cliente puede servir de base para una exitosa gestión comercial y de administración del riesgo",2),#50
    rep("La Política de Conocimiento del Cliente es la recolección de fuentes de financiación, ya sean Legales o Ilegales",2),#51
    rep("¿El Financiamiento al Terrorismo es la implementación de bases fundamentales para el conocimiento financiero?",2)#52
)
input_id=c("nombre",#1
           "num_doc",#1.1
           "edad",#2
           c("genero","genero"),#3
           # "otro",#3.1
           rep("p1",2),#4
           rep("p2",2),#5
           rep("p3",2),#6
           rep("p4",2),#7
           rep("p5",2),#8
           rep("p6",2),#9
           rep("p7",2),#10
           rep("p8",2),#11
           rep("p9",2),#12
           rep("p10",2),#13 hasta la 10
           rep("p11",4),#14
           rep("p12",3),#15
           rep("p13",4),#16
           rep("p14",4),#17
           rep("p15",4),#18
           rep("p16",3),#19
           rep("p17",4),#20
           rep("p18",4),#21
           rep("p19",2),#22
           rep("p20",2),#23
           rep("p21",3),#24
           rep("p22",2),#25
           rep("p23",2),#26
           rep("p24",2),#27
           rep("p25",3),#28
           rep("p26",3),#29
           rep("p27",2),#30
           rep("p28",3),#31
           rep("p29",3),#32
           rep("p30",3),#33
           rep("p31",3),#34
           rep("p32",3),#35
           rep("p33",3),#36
           rep("p34",3),#37
           rep("p35",3),#38
           rep("p36",2),#39
           rep("p37",3),#40
           rep("p38",4),#41
           # rep("p39",4),#42
           rep("p40",3),#43
           rep("p41",4),#44
           rep("p42",3),#45
           # rep("p43",3),#46
           # rep("p44",3),#47
           # rep("p45",3),#48
           rep("p46",3),#49
           rep("p47",2),#50
           rep("p48",2),#51
           rep("p49",2)#52
)
Respuestas_correctas= c("a) Verdadero","a) Verdadero","a) Verdadero","a) Verdadero" ,"a) Verdadero",
                        "a) Verdadero","a) Verdadero","a) Verdadero","a) Verdadero","a) Verdadero",#hasta la 10
                        "c) Legitimación de Ganancias Ilícitas","b) Grupo de Acción Financiera Internacional","c) Fondos de origen Legítimos e Ilegítimos",
                        "c) Son aquellas operaciones que se alejan al perfil transaccional del cliente o no guardan relación con sus ingresos de acuerdo con su actividad, negocio o profesión",
                        "c) Es una operación calificada como inusual cuya observación no puedo ser justificada o respaldada",
                        "c) A los reportes de Operaciones Sospechosas",
                        "c) Obtener del cliente toda la información posible, relacionada a su entorno de vida, entorno económico o de actividad laboral ya sea principal o secundarias",
                        "d) De 5 a 10 años",
                        "a) Verdadero",
                        "a) Verdadero",#hasta la 20
                        "c) señal de alerta","a) Verdadero","a) Verdadero","a) Verdadero",
                        "a) fondos y otros activos de personas vinculadas con acciones de terrorismo y financiamiento del terrorismo",
                        "b) emitir sanciones por incumplimiento","a) Verdadero",
                        "a) tráfico de sustancias ilícitas, contrabando, corrupción, organización criminal y estafas",
                        "a) Riesgo Reputacional, Riesgo Operativo, Riesgo Legal y Riesgo de Contagio",
                        "c) Colocación, Estratificación o diversificación e Integración", # hasta la 30
                        "b) Recolección, Disposición y Utilización",
                        "b) Será sancionado con presidio de quince (15) a veinte (20) años",
                        "b) Lavado de Activos","a) Artículo 133 Bis","c) Artículo 185 Bis","a) Verdadero","a) Colocación","d) Ninguna","b) Transformación",
                        "a) Riesgo Reputacional",# hasta la 40
                        "c) Riesgo Operativo","a) Riesgo Legal","b) Disposición","c) Utilización","a) Recaudación","c) Artículo 141 Bis","a) Verdadero",
                        "b) Falso","b) Falso","a) Es una operación calificada como inusual cuya observación no puedo ser justificada o respaldada"
)
Respuestas_correctas= Respuestas_correctas[-c(43:45,39,50)]
preguntas= data.frame(PREGUNTAS=unique(question), INPUT_ID=unique(input_id))[-c(1:4),]
preguntas= data.frame(preguntas, Respuestas_correctas)
###########
googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/15-oQH6F94dSl3hSQc28ziwc2YhjsbWOe7g964d-2lOI/edit?usp=sharing",
                          col_types = "c", sheet = "Hoja 3")

datos=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/15-oQH6F94dSl3hSQc28ziwc2YhjsbWOe7g964d-2lOI/edit?usp=sharing",
                          col_types = "DDDccccccccccDcDcDcDcc", sheet = "Hoja 2")
##




cargos_1=tibble(CARGO_POSTULANTE=c("ANALISTA DD",
"ANALISTA ANALITICA DE DATOS",
"ANALISTA ANALITICA DE RIESGOS",
"ANALISTA LGI FT",
"ANALISTA DE RIESGOS 1",
"ANALISTA DD 1",
"GERENTE",
"ANALISTA DD 1"))
########################


createValueBoxes <- function(df, h = 4, w = 6, padding=0.5, rows = 2){
  # required packages
  library(ggplot2)
  library(emojifont)
  # verify our inputs
  if (!is.data.frame(df)) {
    stop(paste("Argument", deparse(substitute(df)), "must be a data.frame."))
  }
  if(!all(i <- rlang::has_name(df,c("values", "infos", "icons")))){
    stop(sprintf(
      "%s does not contain: %s",
      deparse(substitute(df)),
      paste(columns[!i], collapse=", ")))
  }

  boxes = nrow(df) # number of items passed
  # calculate the grid
  cols = boxes/rows
  plotdf <- data.frame(
    x = rep(seq(0, (w+padding)*cols-1, w+padding), times=rows),
    y = rep(seq(0, (h+padding)*rows-1, h+padding), each=cols),
    h = rep(h, boxes),
    w = rep(w, boxes),
    value = df$values,
    info = df$infos,
    icon = fontawesome(df$icons),
    font_family = c(rep("fontawesome-webfont", boxes)),
    color = c("green","#3a0a5e", "#3a0a5e")
  )
  print(plotdf)
  ggplot(plotdf, aes(x, y, height = h, width = w, label = info)) +
    ## Create the tiles using the `color` column
    geom_tile(aes(fill = color)) +
    ## Add the numeric values as text in `value` column
    geom_text(color = "white", fontface = "bold", size = 10,
              aes(label = value, x = x - w/2.2, y = y + h/4), hjust = 0) +
    ## Add the labels for each box stored in the `info` column
    geom_text(color = "white", fontface = "bold",
              aes(label = info, x = x - w/2.2, y = y-h/4), hjust = 0) +
    coord_fixed() +
    scale_fill_brewer(type = "qual",palette = "Dark2") +
    ## Use `geom_text()` to add the icons by specifying the unicode symbol.
    geom_text(size = 20, aes(label = icon, family = font_family,
                             x = x + w/4, y = y + h/8), alpha = 0.25) +
    theme_void() +
    guides(fill = FALSE)

}
df <- data.frame(
  values=c("50%", "7", "1245"),
  infos=c("now", "super", "hours"),
  icons=c("fa-gear", "fa-diamond", "fa-tasks")
)
createValueBoxes(df, rows=1)

########## preguntas para evaluaciones final #####
## preguntas
question=c("¿Cuál es tu nombre?", #1
           "Número de documento de identidad:",#1.1
           "¿Cuál es tu edad?", #2
           rep("¿Cuál opción describe mejor tu género?",2),#3
           rep("¿Un cliente o persona que se niegue a dar información requerida o que mienta en sus declaraciones, es considerado cliente sospechoso?",2),#4
           rep("¿Las señales de alerta permiten identificar operaciones que presentan situaciones inusuales y que
               en algunos casos son operaciones de Lavado de Dinero y Financiamiento del Terrorismo?",2),#5
           rep("¿Un ROS es la comunicación mediante la cual los Sujetos Obligados reportan cualquier hecho u operación
               con independencia de su cuantía, por hechos o situaciones que posiblemente están relacionados con el Lavado
               de Activos o Financiación del Terrorismo?",2),#6
           rep("¿El Financiamiento al Terrorismo es el acto de proporcionar apoyo por cualquier medio a terroristas u organizaciones
               terroristas a fin de permitirles realizar actos de terrorismo?",2),#7
           rep("¿Una operación inusual es aquella cuyo monto o magnitud, características particulares, y periodicidad o velocidad de rotación
               con que son ejecutadas, no guardan relación con la actividad económica del cliente, salen de los parámetros de comportamiento
               transaccional, o si fuera el caso no tienen un fundamento legal evidente al momento de requerir el respaldo correspondiente?",2),#8
           rep("¿Una persona o cliente que presiona a cualquier funcionario del Banco para no diligenciar formularios que impliquen el
               registro de la información o el reporte de la operación, es considerado como cliente sospechoso?",2),#9
           rep("¿Las cuentas que muestran elevadas transacciones en efectivo para negocios que generalmente NO manejan grandes sumas
               de dinero en efectivo, es considerada una señal de alerta?",2),#10
           rep("¿El objeto de lavado de activos consiste en hacer que los fondos o activos obtenidos a través de actividades ilícitas
               aparezcan como el fruto de actividades legítimas y circulen sin problema en el sistema financiero?",2),#11
           rep("¿El Código Penal Boliviano como define al Lavado de Dinero?",3),#12
           rep("¿Qué es el GAFI?",3),#13
           rep("¿El Financiamiento del Terrorismo puede ser financiado por medio de Fondos obtenidos de manera?",3),#14
           rep("¿Qué es una Operación inusual?",3),#15
           rep("¿Que es una operación sospechosa?",3),#16
           rep("¿A qué se denomina ROS?",3),#17
           rep("La Política “conozca a su cliente” consiste en:",4),#18
           rep("¿Cuantos años se sanciona con privación de libertad el Delito de Legitimación de Ganancias Ilícitas en Bolivia?",3),#19
           rep("¿El delito al Financiamiento del Terrorismo es autónomo y será investigado, enjuiciado y sentenciado sin necesidad de sentencia previa por Delitos Conexos?",2),#20
           rep("¿Es importante actualizar en forma periódica y sistemática los datos suministrados por los clientes?",2),#21
           rep("¿Cuántas recomendaciones fueron generadas por el GAFI?",3),#22
           rep("¿Cuántas etapas se consideran en el proceso de Lavado de Dinero?",4),#23
           rep("¿Mediante que normativa fue creada la Unidad de Investigaciones Financieras?",3),#24
           rep("¿Cuál de los siguientes incisos es un delito precedente para la Legitimación de Ganancias Ilícitas?",3),#25
           rep("¿Cuál de las siguientes opciones es un delito precedente para la Legitimación de Ganancias Ilícitas?",3),#26
           rep("¿Cuál de las siguientes alternativas es un delito precedente para la Legitimación de Ganancias Ilícitas?",3),#27
           rep("¿Cuál de los siguientes presentados es un delito precedente para la Legitimación de Ganancias Ilícitas?",3),#28
           rep("¿Cuál de los siguientes es un delito precedente para la Legitimación de Ganancias Ilícitas?",3),#29
           rep("¿Cuál de las opciones es un delito precedente para la Legitimación de Ganancias Ilícitas?",3),#30
           rep("¿Cuáles son los principales delitos precedentes para la Legitimación de Ganancias Ilícitas?",3),#31
           rep("¿Cuáles son los riesgos que podría llegar a tener Banco Sol en caso de ser usado en operaciones de Lavado de Dinero y Financiamiento del Terrorismo?",3),#32
           rep("¿Cuáles son las etapas del Lavado de Dinero?",3),#33
           rep("¿Cuáles son las etapas del Financiamiento del Terrorismo?",3),#34
           rep("¿Con cuántos años se sanciona con privación de libertad al Delito de Financiamiento del Terrorismo?",3),#35
           rep("¿La Legitimación de Ganancias Ilícitas también es conocida como?",3),#36
           rep("¿Cuál es el artículo en el código penal que menciona al Financiamiento del Terrorismo?",3),#37
           rep("¿Cuál es el artículo en el código penal que menciona al Legitimación de Ganancias Ilícitas?",3),#38
           rep("El dinero de origen ilícito, nunca llega a ser LEGAL",2),#39
           rep("¿Cuál de las opciones es una de las etapas del proceso de Legitimación de Ganancias Ilícitas?",3),#40
           rep("¿Cuál de las siguientes, es una de las etapas del proceso de Legitimación de Ganancias Ilícitas?",4),#41
           rep("¿Cuál de las siguientes pertenece a una de las etapas del proceso de Legitimación de Ganancias Ilícitas?",4),#42
           rep("Uno de los riesgos que podrían llegar a tener el Banco Solidario S.A. en caso de ser usado en operaciones de
               Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería el siguiente:",3),#43
           rep("Uno de los riesgos que podrían llegar a tener el Banco Solidario S.A. en caso de ser usado
               en operaciones de Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería",4),#44
           rep("Un riesgo que podrían llegar a tener el Banco Solidario S.A. en caso de ser usado en operaciones de
               Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería el siguiente:",3),#45
           rep("Uno de los riesgos que podrían llegar a tener el Banco Solidario S.A. en caso de ser usado en operaciones de
               Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería el siguiente:",4),#46
           rep("¿Cuál es una de las etapas del proceso de Financiamiento del Terrorismo?",4),#47
           rep("¿Cuál de las opciones es una de las etapas del proceso de Financiamiento del Terrorismo?",4),#48
           rep("¿Alguna de las siguientes opciones es una de las etapas del proceso de Financiamiento del Terrorismo?",4),#49
           rep("¿Cuál es el artículo en el código penal que menciona al Financiamiento De La Proliferación De Armas De Destrucción Masiva?",3),#50
           rep("La Política de Conocimiento del Cliente no debe mirarse como una actividad aislada que no cumple con los objetivos
               principales del negocio. Por el contrario, una política efectiva de Conocimiento del Cliente puede servir de base
               para una exitosa gestión comercial y de administración del riesgo",2),#51
           rep("La Política de Conocimiento del Cliente es la recolección de fuentes de financiación, ya sean Legales o Ilegales",2),#52
           rep("¿El Financiamiento al Terrorismo es la implementación de bases fundamentales para el conocimiento financiero?",2),#53
           rep("Las cuentas que muestran varios depósitos por debajo del umbral. ¿Representa una señal de alerta?",2),#54
           rep("Las operaciones realizadas frecuentemente o de importes llamativos a nombre de terceros, sin que exista
               justificativo para ello. ¿Representa una señal de alerta?",2),#55
           rep("¿El Banco Solidario S.A. puede ser usado en operaciones de Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo?",2),#56
           rep("¿Una Operación Inusual es igual a una Operación Sospechosa?",2),#57
           rep("¿Qué significa las siglas UIF?",3),#58
           rep("¿La Ley N°004, es denominada?",3),#59
           rep("¿La Legitimación de Ganancias Ilícitas puede ser financiada por medio de Fondos obtenidos de manera?",3),#60
           rep("¿Cuántas etapas tiene el proceso de Financiamiento del Terrorismo?",3),#61
           rep("La Ley N° 262, es el Régimen de congelamiento de:",3),#62
           rep("¿El Financiamiento de la Proliferación de Armas de Destrucción Masiva es todo acto que provea fondos o utilice servicios
               financieros, en todo o en parte, para la fabricación, adquisición, posesión, desarrollo, exportación, fraccionamiento,
               transporte, transferencia, depósito o uso de armas nucleares, químicas o biológicas, sus medios de lanzamiento y otros
               materiales relacionados?",2),#63
           rep("¿Qué cree usted que es el Lavado de Dinero?",3),#64
           rep("¿En qué consiste la primera etapa del Lavado de Dinero?",3),#65
           rep("En el Decreto Supremo 910 se menciona una multa según el grado de responsabilidad y la gravedad de la infracción, ¿Hasta cuánto es la remuneración?",3),#66
           rep("¿Qué es Financiamiento del Terrorismo?",3),#67
           rep("¿Qué es el Riesgo Reputacional?",3),#68
           rep("¿Qué es el Riesgo Legal?",3),#69
           rep("¿Qué es el Riesgo Operativo?",3),#70
           rep("¿Qué es el Riesgo de Contagio?",3),#71
           rep("¿Cuándo fue creado el GAFI?",3),#72
           rep("¿Las 40 recomendaciones fueron generadas por?",3),#73
           rep("¿Los clientes que frecuentemente solicitan que se incrementen los límites de excepción, son considerados como clientes sospechosos?",2),#74
           rep("¿Los clientes cuyo teléfono se encuentra desconectado, o el número telefónico al momento de efectuar la llamada de verificación, no concuerda con la información inicialmente suministrada, es considerado como una señal de alerta?",2),#75
           rep("Las transferencias electrónicas, sin aparente razón comercial ni consistencia con los negocios habituales o actividad económica declarada del cliente. Representan una:",3),#76
           rep("Cuando el cliente ofrece pagar jugosas comisiones, sin justificativo legal y lógico. ¿Representa una señal de alerta?",2),#77
           rep("La cancelación repentina de préstamos, sin justificación aparente sobre la razón del pago súbito o el origen de los fondos. ¿Representa una señal de alerta?",2),#78
           rep("El frecuente envió o recepción de grandes volúmenes de transferencias electrónicas de o hacia instituciones. ¿Representa una señal de alerta?",2),#79
           rep("Toda transacción por montos relevantes debe tener un respaldo del motivo del movimiento, depósito, origen o destino",2),#80
           rep("¿En qué consiste la segunda etapa del Lavado de Dinero? ",3),#81
           rep("¿En qué consiste la tercera etapa del Lavado de Dinero?",3),#82
           rep("¿En qué consiste la primera etapa del proceso de Financiamiento del Terrorismo?",3),#83
           rep("¿En qué consiste la segunda etapa del proceso de Financiamiento del Terrorismo?",3),#84
           rep("¿En qué consiste la tercera etapa del proceso de Financiamiento del Terrorismo?",3),#85
           rep("Una persona que, de un momento a otro, sin fundamento, aparece como dueño de importantes negocios. ¿Representa una señal de alerta?",2),#86
           rep("Un cliente que brinda la información necesaria para su registro en el sistema, y que realiza la actualización de su información cuando se lo solicita ¿Es un cliente sospechoso?",2),#87
           rep("Las Empresas que se abstienen de proporcionar información completa, como actividad principal de la empresa, referencias bancarias, nombre de apoderados y directores, etc. ¿Representan una señal de alerta?",2),#88
           rep("El Consejo Nacional de Lucha Contra la Legitimación de Ganancias Ilícitas y el Financiamiento del Terrorismo está compuesto por los ministerios de:",3),#89
           rep("¿A qué se refiere el Decreto Supremo 910?",3),#90
           rep("¿Cuál es la Ley N° 004?",3),#91
           rep("¿Cómo describirías al Financiamiento de la Proliferación de Armas de Destrucción Masiva?",3),#92
           rep("La Unidad de Investigaciones Financieras es:",3),#93
           rep("La comunicación mediante la cual los Sujetos Obligados (Entidades de Intermediación Financiera) reportan cualquier hecho u operación con independencia de su cuantía, por hechos o situaciones que posiblemente están relacionadas con la Legitimación de Ganancias Ilícitas o la Financiación del Terrorismo. Es el:",4)#94


)
##
option=c("Nombres y Apellidos",#1
         "",#1.1
         "27",#2
         c("Masculino","Femenino"),#3
         c("a) Verdadero","b) Falso"),#4
         c("a) Verdadero","b) Falso"),#5
         c("a) Verdadero","b) Falso"),#6
         c("a) Verdadero","b) Falso"),#7
         c("a) Verdadero","b) Falso"),#8
         c("a) Verdadero","b) Falso"),#9
         c("a) Verdadero","b) Falso"),#10
         c("a) Verdadero","b) Falso"),#11
         c("a) Lavado de Activos","a) Lavado de Activos","c) Legitimación de Ganancias Ilícitas"),#12
         c("a) Grupo de Acción Financiamiento Internacional","b) Grupo de Acción Financiera Internacional",
           "c) Grupo Antilavado Financiero Internacional"),#13
         c("a) Fondos de origen Legítimos","b) Fondos de origen Ilegítimos","c) Fondos de origen Legítimos e Ilegítimos"),#14
         c("a) Toda aquella operación que tiene como característica el manejo de dinero únicamente en efectivo",
           "b) Es toda operación realizada por personas en forma sospechosa","c) Son aquellas operaciones que se alejan al perfil
           transaccional del cliente o no guardan relación con sus ingresos de acuerdo con su actividad, negocio o profesión"),#15
         c("a) Toda aquella transacción financiera que es realizada por un criminal","b) Es la operativa de un cliente o usuario que se realiza a nombre de un tercero",
           "c) Es una operación calificada como inusual cuya observación no puedo ser justificada o respaldada"),#16
         c("a) Al reporte de Retenciones, Observaciones y Sospechas","b) Al procedimiento de Reconocer Objetos Sospechosos",
           "c) A los reportes de Operaciones Sospechosas"),#17
         c("a) Conocer los datos relacionados a su fuente de trabajo y sus ingresos económicos","
           b) Saber dónde vive","c) Obtener del cliente toda la información posible, relacionada a su entorno de vida, entorno económico o de actividad laboral ya sea principal o secundarias",
           "d) Ninguno"),#18
         c("a) De 5 a 10 años","b) De 3 a 5 años","c) De 5 a 7 años"),#19
         c("a) Verdadero","b) Falso"),#20
         c("a) Verdadero","b) Falso"),#21
         c("a) 10 Recomendaciones","b) 40 Recomendaciones","c) 25 Recomendaciones"),#22
         c("a) Tres etapas","b) Dos etapas","c) Una sola etapa","d) Ninguna de las anteriores"),#23
         c("a) Ley 1768","b) Ley 004","c) DS 24771"),#24
         c("a) Contrabando","b) Falta a la moral","c) Violación"),#25
         c("a) Sabotaje","b) Corrupción","c) Estupro"),#26
         c("a) Tráfico de sustancias ilícitas","b) Incumplimiento de contrato","c) Daños al ornato publico"),#27
         c("a) Delito Tributario","b) Delitos de Terrorismo","c) Todas las anteriores"),#28
         c("a) Trata de personas","b) Injuriar","c) Bigamia"),#29
         c("a) Conducción peligrosa","b) Fabricación de sustancias controladas","c) Bigamia"),#30
         c("a) Contrabando, Fabricación, Transporte, Comercialización o Tráfico Ilícito de Sustancias Controladas, Delitos de Corrupción, Terrorismo, Financiamiento del Terrorismo, Trata y Tráfico de Personas, Uso Indebido de Información Privilegiada",
           "b) Estupro, bigamia, faltas a la moral, conducción peligrosa, violencia intrafamiliar",
           "c) Colocación, transformación e integración"),#31
         c("a) Riesgo Reputacional, Riesgo Operativo, Riesgo Legal y Riesgo de Contagio",
           "b) Riesgo conceptual, Riesgo variable, Riesgo de Contagio","c) Riesgo de evaluación, Riesgo sumatorio y Riesgo de Operativo"),#32
         c("a) Concentración, Asignación y Distribución","b) Recaudación, colocación e Información",
           "c) Colocación, Transformación e Integración"),#33
         c("a) Restitución, Colocación e Información","b) Recolección, Disposición y Utilización","c) Recolección, Diversificación y Distribución"),#34
         c("a) Será sancionado con presidio de treinta (30) a cincuenta (50) años","b) Será sancionado con presidio de quince (15) a veinte (20) años",
           "c) Será sancionado con presidio de dos (2) a cinco (5) años"),#35
         c("a) Delito de bigamia","b) Lavado de Activos","c) Corrupción"),#36
         c("a) Artículo 133 Bis","b) Artículo 20 Bis","c) Artículo 103 Bis"),#37
         c("a) Artículo 55 Bis","b) Artículo 88 Bis","c) Artículo 185 Bis"),#38
         c("a) Verdadero","b) Falso"),#39
         c("a) Colocación","b) Utilización","c) Consolidación"),#40
         c("a) Recaudación","b) Transformación","c) Restitución","d) Ninguna"),#41
         c("a) Recaudación","b) Integración","c) Restitución","d) Ninguna"),#42
         c("a) Riesgo Reputacional","b) Riesgo Financiero","c) Riesgo Ocupacional"),#43
         c("a) Riesgo Financiero","b) Riesgo Sistemático","c) Riesgo Operativo","d) Ninguno"),#44
         c("a) Riesgo Legal","b) Riesgo Crediticio","c) Riesgo Ocupacional"),#45
         c("a) Riesgo Financiero","b) Riesgo Ocupacional","c) Riesgo de Contagio","d) Ninguno"),#46
         c("a) Corrupción","b) Disposición","c) Fabricación","d) Ninguno"),#47
         c("a) Información","b) Establecimiento","c) Utilización","d) Ninguno"),#48
         c("a) Recaudación","b) Sustitución","c) Restablecimiento","d) Ninguno"),#49
         c("a) Artículo 105 Bis","b) Artículo 155 Bis","c) Artículo 141 Bis"),#50
         c("a) Verdadero","b) Falso"),#51
         c("a) Verdadero","b) Falso"),#52
         c("a) Verdadero","b) Falso"),#53
         c("a) Verdadero","b) Falso"),#54
         c("a) Verdadero","b) Falso"),#55
         c("a) Verdadero","b) Falso"),#56
         c("a) Verdadero","b) Falso"),#57
         c("a) Unidad de Inteligencia de Finanzas","b) Unidad de Investigaciones Financieras","c) Unidad de Instancias Financieras"),#58
         c("a) Ley de Lucha contra el Terrorismo","b) Hugo Banzer Suarez","c) Marcelo Quiroga Santa Cruz"),#59
         c("a) Fondos de origen Legal","b) Fondos de origen Ilegal","c) Ambos son correctos"),#60
         c("a) 5 etapas","b) 2 etapas","c) 3 etapas"),#61
         c("a) fondos y otros activos de personas vinculadas con acciones de terrorismo y financiamiento del terrorismo",
           "b) información de personas","c) tráfico de armas"),#62
         c("a) Verdadero","b) Falso"),#63
         c("a) Es el acto de cambiar la divisa del dinero obtenido ilegalmente","b) Es el proceso de esconder o disfrazar la existencia, origen y el destino de bienes o dinero producto de actividades ilegales para transformarlo en dinero licito",
           "c) Es el proceso de cambiar los cortes de los billetes de menor a mayor corte"),#64
         c("a) En Transformar el dinero ilícito en licito","b) En integrar el dinero licito al torrente financiero","c) Colocar el dinero dentro del sistema utilizando una serie de artilugios"),#65
         c("a) Hasta cinco (5) veces la remuneración mensual del infractor de la Entidad Financiera",
           "b) Hasta dos (2) veces la remuneración mensual del infractor de la Entidad Financiera",
           "c) Hasta tres (3) veces la remuneración mensual del infractor de la Entidad Financiera"),#66
         c("a) Es el proceso por el cual se captan capitales que pueden ser lícitos o ilícitos para ser destinados a financiar actividades terroristas",
           "b) Es cualquier forma de acción económica, ayuda o mediación que proporcione apoyo financiero a las actividades financieras",
           "c) Ninguna de las anteriores"),#67
         c("a) Definido como la capacidad de que el nombre y la imagen corporativa de la entidad sea menoscabada",
           "b) Definido como la capacidad de administrar los fondos de una institución",
           "c) Falta de información para tomar decisiones"),#68
         c("a) Definido como la capacidad de que tiene un deudor de cumplir con sus obligaciones de pago",
           "b) Entendido como la posibilidad de que la entidad sea sancionada o condenada al pago de indemnizaciones y que sus funcionarios se vean involucrados en procesos administrativos y penales, con penas de orden económico o de privación de libertad",
           "c) Posibilidad de que una empresa no consiga cumplir con sus compromisos"),#69
         c("a) Se refiere al cambio de valor que se produce en las acciones, bonos, etc",
           "b) Definido como la capacidad de administrar los fondos de una institución",
           "c) Es posibilidad de pérdidas económicas a causa de fallas humanas, técnicas o procedimentales"),#70
         c("a) Es la posibilidad de que la entidad pueda sufrir una afectación reputacional, legal o económica a causa de la acción propia de una empresa relacionada o asociada a ella",
           "b) Falta de información para tomar decisiones",
           "c) Posibilidad de que una empresa no consiga cumplir con sus compromisos"),#71
         c("a) En la década de los 80","b) En la década de los 70","c) En la década de los 60"),#72
         c("a) La UIF","b) El GAFI","c) Ninguno"),#73
         c("a) Verdadero","b) Falso"),#74
         c("a) Verdadero","b) Falso"),#75
         c("a) operación normal","b) señal habitual","c) señal de alerta"),#76
         c("a) Verdadero","b) Falso"),#77
         c("a) Verdadero","b) Falso"),#78
         c("a) Verdadero","b) Falso"),#79
         c("a) Verdadero","b) Falso"),#80
         c("a) En Transformar el dinero ilícito en licito","b) En integrar el dinero licito al torrente financiero",
           "c) Colocar el dinero dentro del sistema utilizando una serie de artilugios"),#81
         c("a) En Transformar el dinero ilícito en licito","b) En integrar el dinero licito al torrente financiero",
           "c) Colocar el dinero dentro del sistema utilizando una serie de artilugios"),#82
         c("a) En la recolección de fuentes de financiación, ya sean Legales o Ilegales",
           "b) Movilizar los fondos ocultando sus movimientos y destino final",
           "c) Consiste en la utilización de los fondos para la financiación, planeación y ejecución de actos terroristas"),#83
         c("a) En la recolección de fuentes de financiación, ya sean Legales o Ilegales",
           "b) Movilizar los fondos ocultando sus movimientos y destino final","c) Consiste en la utilización de los fondos para la financiación, planeación y ejecución de actos terroristas"),#84
         c("a) En la recolección de fuentes de financiación, ya sean Legales o Ilegales","b) Movilizar los fondos ocultando sus movimientos y destino final",
           "c) Consiste en la utilización de los fondos para la financiación, planeación y ejecución de actos terroristas"),#85
         c("a) Verdadero","b) Falso"),#86
         c("a) Verdadero","b) Falso"),#87
         c("a) Verdadero","b) Falso"),#88
         c("a) Economía y Finanzas Públicas, Gobierno, Defensa y Justicia","b) Transparencia institucional y Lucha Contra la Corrupción",
           "c) Todas las anteriores"),#89
         c("a) A la creación de la Unidad de Investigación Financiera",
           "b) A la sanción por incumplimiento al proceso normativo para la Prevención, Detección y Reporte de operaciones de Legitimación de Ganancias Ilícitas",
           "c) A la sanción por incumplimiento de armas de destrucción masiva"),#90
         c("a) La de los fondos y otros activos de personas vinculadas al financiamiento del terrorismo",
           "b) La que crea a la Unidad de Investigaciones Financieras",
           "c) Lucha Contra la Corrupción, Enriquecimiento Ilícito e Investigación de Fortunas"),#91
         c("a) Es el proceso de esconder o disfrazar la existencia, fuente ilegal, movimiento, destino o uso ilegal de bienes o fondos, producto de actividades ilegales para luego hacerlos aparentar como legítimos",
           "b) Es todo acto que provea fondos o utilice servicios financieros, en todo o en parte, para la fabricación, adquisición, posesión, desarrollo, exportación, fraccionamiento, transporte, transferencia, depósito o uso de armas nucleares, químicas o biológicas, sus medios de lanzamiento y otros materiales relacionados",
           "c) Ninguna es correcta"),#92
         c("a) La encargada de normar y promulgar decretos y leyes para la lucha contra el lavado de dinero y financiamiento del terrorismo",
           "b) Un grupo intergubernamental cuya oficina central tiene sede en París","c) El organismo encargado del planeamiento y ejecución de la política económica del Estado"),#93
         c("a) Reporte de Operación (es) Sospechosa (S)","b) Informe de Inusualidad","c) Reporte de Actualización de Clientes",
           "d) Todas son correctas")#94


)
##
input_type=c("text", #1
             "text",#1.1
             "numeric",#2
             c("mc","mc"),#3
             c("mc","mc"),#4
             c("mc","mc"),#5
             c("mc","mc"),#6
             c("mc","mc"),#7
             c("mc","mc"),#8
             rep("mc",2),#9
             rep("mc",2),#10
             rep("mc",2),#11
             rep("mc",3),#12
             rep("mc",3),#13
             rep("mc",3),#14
             rep("mc",3),#15
             rep("mc",3),#16
             rep("mc",3),#17
             rep("mc",4),#18
             rep("mc",3),#19
             rep("mc",2),#20
             rep("mc",2),#21
             rep("mc",3),#22
             rep("mc",4),#23
             rep("mc",3),#24
             rep("mc",3),#25
             rep("mc",3),#26
             rep("mc",3),#27
             rep("mc",3),#28
             rep("mc",3),#29
             rep("mc",3),#30
             rep("mc",3),#31
             rep("mc",3),#32
             rep("mc",3),#33
             rep("mc",3),#34
             rep("mc",3),#35
             rep("mc",3),#36
             rep("mc",3),#37
             rep("mc",3),#38
             rep("mc",2),#39
             rep("mc",3),#40
             rep("mc",4),#41
             rep("mc",4),#42
             rep("mc",3),#43
             rep("mc",4),#44
             rep("mc",3),#45
             rep("mc",4),#46
             rep("mc",4),#47
             rep("mc",4),#48
             rep("mc",4),#49
             rep("mc",3),#50
             rep("mc",2),#51
             rep("mc",2),#52
             rep("mc",2),#53
             rep("mc",2),#54
             rep("mc",2),#55
             rep("mc",2),#56
             rep("mc",2),#57
             rep("mc",3),#58
             rep("mc",3),#59
             rep("mc",3),#60
             rep("mc",3),#61
             rep("mc",3),#62
             rep("mc",2),#63
             rep("mc",3),#64
             rep("mc",3),#65
             rep("mc",3),#66
             rep("mc",3),#67
             rep("mc",3),#68
             rep("mc",3),#69
             rep("mc",3),#70
             rep("mc",3),#71
             rep("mc",3),#72
             rep("mc",3),#73
             rep("mc",2),#74
             rep("mc",2),#75
             rep("mc",3),#76
             rep("mc",2),#77
             rep("mc",2),#78
             rep("mc",2),#79
             rep("mc",2),#80
             rep("mc",3),#81
             rep("mc",3),#82
             rep("mc",3),#83
             rep("mc",3),#84
             rep("mc",3),#85
             rep("mc",2),#86
             rep("mc",2),#87
             rep("mc",2),#88
             rep("mc",3),#89
             rep("mc",3),#90
             rep("mc",3),#91
             rep("mc",3),#92
             rep("mc",3),#93
             rep("mc",4)#94

)
##
# input_id
input_id=c("nombre",#1
           "num_doc",#1.1
           "edad",#2
           c("genero","genero"),#3
           c("p1","p1"),#4
           c("p2","p2"),#5
           c("p3","p3"),#6
           c("p4","p4"),#7
           c("p5","p5"),#8
           rep("p6",2),#9
           rep("p7",2),#10
           rep("p8",2),#11
           rep("p9",3),#12
           rep("p10",3),#13
           rep("p11",3),#14
           rep("p12",3),#15
           rep("p13",3),#16
           rep("p14",3),#17
           rep("p15",4),#18
           rep("p16",3),#19
           rep("p17",2),#20
           rep("p18",2),#21
           rep("p19",3),#22
           rep("p20",4),#23
           rep("p21",3),#24
           rep("p22",3),#25
           rep("p23",3),#26
           rep("p24",3),#27
           rep("p25",3),#28
           rep("p26",3),#29
           rep("p27",3),#30
           rep("p28",3),#31
           rep("p29",3),#32
           rep("p30",3),#33
           rep("p31",3),#34
           rep("p32",3),#35
           rep("p33",3),#36
           rep("p34",3),#37
           rep("p35",3),#38
           rep("p36",2),#39
           rep("p37",3),#40
           rep("p38",4),#41
           rep("p39",4),#42
           rep("p40",3),#43
           rep("p41",4),#44
           rep("p42",3),#45
           rep("p43",4),#46
           rep("p44",4),#47
           rep("p45",4),#48
           rep("p46",4),#49
           rep("p47",3),#50
           rep("p48",2),#51
           rep("p49",2),#52
           rep("p50",2),#53
           rep("p51",2),#54
           rep("p52",2),#55
           rep("p53",2),#56
           rep("p54",2),#57
           rep("p55",3),#58
           rep("p56",3),#59
           rep("p57",3),#60
           rep("p58",3),#61
           rep("p59",3),#62
           rep("p60",2),#63
           rep("p61",2),#64
           rep("p62",3),#65
           rep("p63",3),#66
           rep("p64",3),#67
           rep("p65",3),#68
           rep("p66",3),#69
           rep("p67",3),#70
           rep("p68",3),#71
           rep("p69",3),#72
           rep("p70",3),#73
           rep("p71",3),#74
           rep("p72",2),#75
           rep("p73",3),#76
           rep("p74",2),#77
           rep("p75",2),#78
           rep("p76",2),#79
           rep("p77",2),#80
           rep("p78",3),#81
           rep("p79",3),#82
           rep("p80",3),#83
           rep("p81",3),#84
           rep("p82",3),#85
           rep("p83",2),#86
           rep("p84",2),#87
           rep("p85",2),#88
           rep("p86",3),#89
           rep("p87",3),#90
           rep("p88",3),#91
           rep("p89",3),#92
           rep("p90",3),#93
           rep("p91",4)#94


)
# dependence
dependence=c(NA,#1
             NA,#1.1
             NA,#2
             c(NA,NA),#3
             c(NA,NA),#4
             c(NA,NA),#5
             c(NA,NA),#6
             c(NA,NA),#7
             c(NA,NA),#8
             rep(NA,2),#9
             rep(NA,2),#10
             rep(NA,2),#11
             rep(NA,3),#12
             rep(NA,3),#13
             rep(NA,3),#14
             rep(NA,3),#15
             rep(NA,3),#16
             rep(NA,3),#17
             rep(NA,4),#18
             rep(NA,3),#19
             rep(NA,2),#20
             rep(NA,2),#21
             rep(NA,3),#22
             rep(NA,4),#23
             rep(NA,3),#24
             rep(NA,3),#25
             rep(NA,3),#26
             rep(NA,3),#27
             rep(NA,3),#28
             rep(NA,3),#29
             rep(NA,3),#30
             rep(NA,3),#31
             rep(NA,3),#32
             rep(NA,3),#33
             rep(NA,3),#34
             rep(NA,3),#35
             rep(NA,3),#36
             rep(NA,3),#37
             rep(NA,3),#38
             rep(NA,2),#39
             rep(NA,3),#40
             rep(NA,4),#41
             rep(NA,4),#42
             rep(NA,3),#43
             rep(NA,4),#44
             rep(NA,3),#45
             rep(NA,4),#46
             rep(NA,4),#47
             rep(NA,4),#48
             rep(NA,4),#49
             rep(NA,3),#50
             rep(NA,2),#51
             rep(NA,2),#52
             rep(NA,2),#53
             rep(NA,2),#54
             rep(NA,2),#55
             rep(NA,2),#56
             rep(NA,2),#57
             rep(NA,3),#58
             rep(NA,3),#59
             rep(NA,3),#60
             rep(NA,3),#61
             rep(NA,3),#62
             rep(NA,2),#63
             rep(NA,3),#64
             rep(NA,3),#65
             rep(NA,3),#66
             rep(NA,3),#67
             rep(NA,3),#68
             rep(NA,3),#69
             rep(NA,3),#70
             rep(NA,3),#71
             rep(NA,3),#72
             rep(NA,3),#73
             rep(NA,2),#74
             rep(NA,2),#75
             rep(NA,3),#76
             rep(NA,2),#77
             rep(NA,2),#78
             rep(NA,2),#79
             rep(NA,2),#80
             rep(NA,3),#81
             rep(NA,3),#82
             rep(NA,3),#83
             rep(NA,3),#84
             rep(NA,3),#85
             rep(NA,2),#86
             rep(NA,2),#87
             rep(NA,2),#88
             rep(NA,3),#89
             rep(NA,3),#90
             rep(NA,3),#91
             rep(NA,3),#92
             rep(NA,3),#93
             rep(NA,4)#94
)
##
dependence_value=c(NA,#1
                   NA,#1.1
                   NA,#2
                   c(NA,NA),#3
                   c(NA,NA),#4
                   c(NA,NA),#5
                   c(NA,NA),#6
                   c(NA,NA),#7
                   c(NA,NA),#8
                   rep(NA,2),#9
                   rep(NA,2),#10
                   rep(NA,2),#11
                   rep(NA,3),#12
                   rep(NA,3),#13
                   rep(NA,3),#14
                   rep(NA,3),#15
                   rep(NA,3),#16
                   rep(NA,3),#17
                   rep(NA,4),#18
                   rep(NA,3),#19
                   rep(NA,2),#20
                   rep(NA,2),#21
                   rep(NA,3),#22
                   rep(NA,4),#23
                   rep(NA,3),#24
                   rep(NA,3),#25
                   rep(NA,3),#26
                   rep(NA,3),#27
                   rep(NA,3),#28
                   rep(NA,3),#29
                   rep(NA,3),#30
                   rep(NA,3),#31
                   rep(NA,3),#32
                   rep(NA,3),#33
                   rep(NA,3),#34
                   rep(NA,3),#35
                   rep(NA,3),#36
                   rep(NA,3),#37
                   rep(NA,3),#38
                   rep(NA,2),#39
                   rep(NA,3),#40
                   rep(NA,4),#41
                   rep(NA,4),#42
                   rep(NA,3),#43
                   rep(NA,4),#44
                   rep(NA,3),#45
                   rep(NA,4),#46
                   rep(NA,4),#47
                   rep(NA,4),#48
                   rep(NA,4),#49
                   rep(NA,3),#50
                   rep(NA,2),#51
                   rep(NA,2),#52
                   rep(NA,2),#53
                   rep(NA,2),#54
                   rep(NA,2),#55
                   rep(NA,2),#56
                   rep(NA,2),#57
                   rep(NA,3),#58
                   rep(NA,3),#59
                   rep(NA,3),#60
                   rep(NA,3),#61
                   rep(NA,3),#62
                   rep(NA,2),#63
                   rep(NA,3),#64
                   rep(NA,3),#65
                   rep(NA,3),#66
                   rep(NA,3),#67
                   rep(NA,3),#68
                   rep(NA,3),#69
                   rep(NA,3),#70
                   rep(NA,3),#71
                   rep(NA,3),#72
                   rep(NA,3),#73
                   rep(NA,2),#74
                   rep(NA,2),#75
                   rep(NA,3),#76
                   rep(NA,2),#77
                   rep(NA,2),#78
                   rep(NA,2),#79
                   rep(NA,2),#80
                   rep(NA,3),#81
                   rep(NA,3),#82
                   rep(NA,3),#83
                   rep(NA,3),#84
                   rep(NA,3),#85
                   rep(NA,2),#86
                   rep(NA,2),#87
                   rep(NA,2),#88
                   rep(NA,3),#89
                   rep(NA,3),#90
                   rep(NA,3),#91
                   rep(NA,3),#92
                   rep(NA,3),#93
                   rep(NA,4)#94
)
##
# required
required=c(T,#1
           T,#1.1
           T,#2
           rep(T,2),#3
           rep(T,2),#4
           rep(T,2),#5
           rep(T,2),#6
           rep(T,2),#7
           rep(T,2),#8
           rep(T,2),#9
           rep(T,2),#10
           rep(T,2),#11
           rep(T,3),#12
           rep(T,3),#13
           rep(T,3),#14
           rep(T,3),#15
           rep(T,3),#16
           rep(T,3),#17
           rep(T,4),#18
           rep(T,3),#19
           rep(T,2),#20
           rep(T,2),#21
           rep(T,3),#22
           rep(T,4),#23
           rep(T,3),#24
           rep(T,3),#25
           rep(T,3),#26
           rep(T,3),#27
           rep(T,3),#28
           rep(T,3),#29
           rep(T,3),#30
           rep(T,3),#31
           rep(T,3),#32
           rep(T,3),#33
           rep(T,3),#34
           rep(T,3),#35
           rep(T,3),#36
           rep(T,3),#37
           rep(T,3),#38
           rep(T,2),#39
           rep(T,3),#40
           rep(T,4),#41
           rep(T,4),#42
           rep(T,3),#43
           rep(T,4),#44
           rep(T,3),#45
           rep(T,4),#46
           rep(T,4),#47
           rep(T,4),#48
           rep(T,4),#49
           rep(T,3),#50
           rep(T,2),#51
           rep(T,2),#52
           rep(T,2),#53
           rep(T,2),#54
           rep(T,2),#55
           rep(T,2),#56
           rep(T,2),#57
           rep(T,3),#58
           rep(T,3),#59
           rep(T,3),#60
           rep(T,3),#61
           rep(T,3),#62
           rep(T,2),#63
           rep(T,3),#64
           rep(T,3),#65
           rep(T,3),#66
           rep(T,3),#67
           rep(T,3),#68
           rep(T,3),#69
           rep(T,3),#70
           rep(T,3),#71
           rep(T,3),#72
           rep(T,3),#73
           rep(T,2),#74
           rep(T,2),#75
           rep(T,3),#76
           rep(T,2),#77
           rep(T,2),#78
           rep(T,2),#79
           rep(T,2),#80
           rep(T,3),#81
           rep(T,3),#82
           rep(T,3),#83
           rep(T,3),#84
           rep(T,3),#85
           rep(T,2),#86
           rep(T,2),#87
           rep(T,2),#88
           rep(T,3),#89
           rep(T,3),#90
           rep(T,3),#91
           rep(T,3),#92
           rep(T,3),#93
           rep(T,4)#94
)
#########
Respuestas_correctas= c("a) Verdadero","a) Verdadero","a) Verdadero","a) Verdadero","a) Verdadero","a) Verdadero","a) Verdadero","a) Verdadero",
                        "c) Legitimación de Ganancias Ilícitas","b) Grupo de Acción Financiera Internacional",# hasta la 10
                        "c) Fondos de origen Legítimos e Ilegítimos","c) Son aquellas operaciones que se alejan al perfil transaccional del cliente o no guardan relación con sus ingresos de acuerdo con su actividad, negocio o profesión",
                        "c) Es una operación calificada como inusual cuya observación no puedo ser justificada o respaldada",
                        "c) A los reportes de Operaciones Sospechosas",
                        "c) Obtener del cliente toda la información posible, relacionada a su entorno de vida, entorno económico o de actividad laboral ya sea principal o secundarias",
                        "a) De 5 a 10 años","a) Verdadero","a) Verdadero","b) 40 Recomendaciones","a) Tres etapas",# hasta la 20
                        "a) Ley 1768","a) Contrabando","b) Corrupción","a) Tráfico de sustancias ilícitas","c) Todas las anteriores",
                        "a) Trata de personas","b) Fabricación de sustancias controladas",
                        "a) Contrabando, Fabricación, Transporte, Comercialización o Tráfico Ilícito de Sustancias Controladas, Delitos de Corrupción, Terrorismo, Financiamiento del Terrorismo, Trata y Tráfico de Personas, Uso Indebido de Información Privilegiada",
                        "a) Riesgo Reputacional, Riesgo Operativo, Riesgo Legal y Riesgo de Contagio","c) Colocación, Transformación e Integración",# hasta la 30
                        "b) Recolección, Disposición y Utilización","b) Será sancionado con presidio de quince (15) a veinte (20) años",
                        "b) Lavado de Activos","a) Artículo 133 Bis","c) Artículo 185 Bis","a) Verdadero","a) Colocación",
                        "b) Transformación","b) Integración","a) Riesgo Reputacional",# hasta la 40
                        "c) Riesgo Operativo","a) Riesgo Legal","c) Riesgo de Contagio","b) Disposición","c) Utilización","a) Recaudación",
                        "c) Artículo 141 Bis","a) Verdadero","b) Falso","b) Falso",# hasta la 50
                        "a) Verdadero","a) Verdadero","a) Verdadero","b) Falso","b) Unidad de Investigaciones Financieras","c) Marcelo Quiroga Santa Cruz",
                        "b) Fondos de origen Ilegal","c) 3 etapas","a) fondos y otros activos de personas vinculadas con acciones de terrorismo y financiamiento del terrorismo",
                        "a) Verdadero",#hasta la 60
                        "b) Es el proceso de esconder o disfrazar la existencia, origen y el destino de bienes o dinero producto de actividades ilegales para transformarlo en dinero licito",
                        "c) Colocar el dinero dentro del sistema utilizando una serie de artilugios",
                        "a) Hasta cinco (5) veces la remuneración mensual del infractor de la Entidad Financiera",
                        "a) Es el proceso por el cual se captan capitales que pueden ser lícitos o ilícitos para ser destinados a financiar actividades terroristas",
                        "a) Definido como la capacidad de que el nombre y la imagen corporativa de la entidad sea menoscabada",
                        "b) Entendido como la posibilidad de que la entidad sea sancionada o condenada al pago de indemnizaciones y que sus funcionarios se vean involucrados en procesos administrativos y penales, con penas de orden económico o de privación de libertad",
                        "c) Es posibilidad de pérdidas económicas a causa de fallas humanas, técnicas o procedimentales",
                        "a) Es la posibilidad de que la entidad pueda sufrir una afectación reputacional, legal o económica a causa de la acción propia de una empresa relacionada o asociada a ella",
                        "a) En la década de los 80","b) El GAFI","a) Verdadero","a) Verdadero","c) señal de alerta","a) Verdadero","a) Verdadero","a) Verdadero",
                        "a) Verdadero","a) En Transformar el dinero ilícito en licito","b) En integrar el dinero licito al torrente financiero",
                        "a) En la recolección de fuentes de financiación, ya sean Legales o Ilegales",
                        "b) Movilizar los fondos ocultando sus movimientos y destino final",
                        "c) Consiste en la utilización de los fondos para la financiación, planeación y ejecución de actos terroristas",
                        "a) Verdadero","b) Falso","a) Verdadero","c) Todas las anteriores",
                        "b) A la sanción por incumplimiento al proceso normativo para la Prevención, Detección y Reporte de operaciones de Legitimación de Ganancias Ilícitas",
                        "c) Lucha Contra la Corrupción, Enriquecimiento Ilícito e Investigación de Fortunas",
                        "b) Es todo acto que provea fondos o utilice servicios financieros, en todo o en parte, para la fabricación, adquisición, posesión, desarrollo, exportación, fraccionamiento, transporte, transferencia, depósito o uso de armas nucleares, químicas o biológicas, sus medios de lanzamiento y otros materiales relacionados",
                        "a) La encargada de normar y promulgar decretos y leyes para la lucha contra el lavado de dinero y financiamiento del terrorismo",
                        "a) Reporte de Operación (es) Sospechosa (S)"
                        )
#3
preguntas= data.frame(PREGUNTAS=unique(question), INPUT_ID=unique(input_id))[-c(1:4),]
preguntas= data.frame(preguntas, Respuestas_correctas)
##
################ BUSQUEDA POR GREP ########################
filtrado=grep(":", datos1$Descripcion_cierre , ignore.case = T)
persona_d= datos1[filtrado,]
### para app perfil personal #####
datos=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/15-oQH6F94dSl3hSQc28ziwc2YhjsbWOe7g964d-2lOI/edit?usp=sharing",
                                col_types = "Dcccccc", sheet = "Hoja 5")
persona= datos %>% filter(question_id!="actividad")
persona$response= as.numeric(persona$response)
persona= persona %>% filter(question_id=="cantidad")
datos1= persona %>% count(ACTIVIDAD, wt=response) %>% arrange(desc(n))
datos1= datos1 %>% mutate(corre= 1:nrow(datos1))
persona=persona %>% left_join(datos1[,c(1,3)], by="ACTIVIDAD")
#
hchart(persona %>% arrange(corre), "bar" , hcaes(x=ACTIVIDAD, y=response, group=question_id),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.1f}%"
       ))  %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_subtitle(text="")  %>%
  hc_colors(c("#38AAACFF","#E75263FF","#FDE725FF","#56147DFF"))
######
datos_prueba= datos_prueba
datos_prueba$FECHA=datos_prueba$FECHA+2
datos_prueba$response=datos_prueba$response+15

persona= rbind(persona, datos_prueba)
datos_graf= persona %>% filter(question_id!="horas")
##
hchart(datos_graf , "spline" ,hcaes(x=FECHA, y=response, group=ACTIVIDAD),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google()) %>% hc_title(text="")%>%
  hc_colors(c("#382A54FF","orange", "#403872FF", "#40498EFF","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>% hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
#
persona= datos %>% filter(question_id!="actividad")
persona$response= as.numeric(persona$response)
persona= persona %>% filter(question_id=="horas")

hchart(persona , "spline",hcaes(x=FECHA, y=response, group=ACTIVIDAD),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())%>% hc_rangeSelector(enabled= TRUE, verticalAlign = "top")%>%
  hc_colors(c("#382A54FF","orange", "#403872FF", "#40498EFF","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
  hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))

#### SEGMENTACION ALERTAS FINDER ############
datos= Alertas %>% filter(`Estado de la Alerta`=="Pendiente")
unique(Alertas$`Estado de la Alerta`)
datos1= Alertas %>% filter(`Estado de la Alerta`!="Pendiente")
datos_pd= datos %>% mutate(cod= paste(Nro_de_identificacion,Caracteristica_2)) %>% group_by(cod) %>% tally()
datos_pd= datos %>% mutate(cod= paste(Nro_de_identificacion)) %>% group_by(cod) %>% tally()
datos_fin= datos1 %>% mutate(cod= paste(Nro_de_identificacion)) %>% group_by(cod) %>% tally()
names(datos_pd)[2]="Pendientes"
names(datos_fin)[2]="Finalizados"
datos_res= datos_pd %>% left_join(datos_fin, by="cod")
sum(is.na(datos_res$Finalizados))
# pendientes totales
sum((datos_res %>% filter(is.na(Finalizados)))$Pendientes)
# clientes pendientes
dim((datos_res %>% filter(is.na(Finalizados))))[1]
#
datos_res_1= datos_res %>% filter(Finalizados>=5)
datos_res_2= datos_res %>% filter(Finalizados<5)
#
datos_res_desc= Alertas %>% filter(`Estado de la Alerta`=="Finalizada")
pendientes_para_cierre= datos_res_desc %>% filter(!is.na(Descripcion_cierre))
res_desc_cierre= datos_res_desc %>% group_by(`Nro. de Identificación`) %>%
  summarise(Descripcion_cierres= list(unique(Descripcion_cierre)), funcionarios_de_cierre= list(unique(`Usuario finalizacion`)))
#
datos_res_11= datos_res_1 %>% left_join(res_desc_cierre, by=c("cod"="Nro. de Identificación"))
#
datos_res_desc= Alertas %>% filter(`Estado de la Alerta`=="Finalizada")
res_desc_cierre= datos_res_desc %>% group_by(Nro_de_identificacion) %>%
  summarise(Descripcion_alerta= list(unique(Caracteristica_1)), Descripcion_alerta1= list(unique(Caracteristica_2)),
            REGIONAL= list(unique(REGIONAL)))
datos_res_111= datos_res_11 %>% left_join(res_desc_cierre, by=c("cod"="Nro_de_identificacion"))





question=c("¿Cuál es tu nombre?", #1
           "Número de documento de identidad:",#1.1
           "¿Cuál es tu edad?", #2
           rep("¿Cuál opción describe mejor tu género?",2),#3
           rep("¿Un cliente o persona que se niegue a dar información requerida o que mienta en sus declaraciones, es considerado cliente sospechoso?",2),#4
           rep("¿Las señales de alerta permiten identificar operaciones que presentan situaciones inusuales y que
               en algunos casos son operaciones de Lavado de Dinero y Financiamiento del Terrorismo?",2),#5
           rep("¿Un ROS es la comunicación mediante la cual los Sujetos Obligados reportan cualquier hecho u operación
               con independencia de su cuantía, por hechos o situaciones que posiblemente están relacionados con el Lavado
               de Activos o Financiación del Terrorismo?",2),#6
           rep("¿El Financiamiento al Terrorismo es el acto de proporcionar apoyo por cualquier medio a terroristas u organizaciones
               terroristas a fin de permitirles realizar actos de terrorismo?",2),#7
           rep("¿Una operación inusual es aquella cuyo monto o magnitud, características particulares, y periodicidad o velocidad de rotación
               con que son ejecutadas, no guardan relación con la actividad económica del cliente, salen de los parámetros de comportamiento
               transaccional, o si fuera el caso no tienen un fundamento legal evidente al momento de requerir el respaldo correspondiente?",2),#8
           rep("¿Una persona o cliente que presiona a cualquier funcionario del Banco para no diligenciar formularios que impliquen el
               registro de la información o el reporte de la operación, es considerado como cliente sospechoso?",2),#9
           rep("¿Las cuentas que muestran elevadas transacciones en efectivo para negocios que generalmente NO manejan grandes sumas
               de dinero en efectivo, es considerada una señal de alerta?",2),#10
           rep("¿El objeto de lavado de activos consiste en hacer que los fondos o activos obtenidos a través de actividades ilícitas
               aparezcan como el fruto de actividades legítimas y circulen sin problema en el sistema financiero?",2),#11
           rep("¿El Código Penal Boliviano como define al Lavado de Dinero?",3),#12
           rep("¿Qué es el GAFI?",3),#13
           rep("¿El Financiamiento del Terrorismo puede ser financiado por medio de Fondos obtenidos de manera?",3),#14
           rep("¿Qué es una Operación inusual?",3),#15
           rep("¿Que es una operación sospechosa?",3),#16
           rep("¿A qué se denomina ROS?",3),#17
           rep("La Política “conozca a su cliente” consiste en:",4),#18
           rep("¿Cuantos años se sanciona con privación de libertad el Delito de Legitimación de Ganancias Ilícitas en Bolivia?",3),#19
           rep("¿El delito al Financiamiento del Terrorismo es autónomo y será investigado, enjuiciado y sentenciado sin necesidad de sentencia previa por Delitos Conexos?",2),#20
           rep("¿Es importante actualizar en forma periódica y sistemática los datos suministrados por los clientes?",2),#21
           rep("¿Cuántas recomendaciones fueron generadas por el GAFI?",3),#22
           rep("¿Cuántas etapas se consideran en el proceso de Lavado de Dinero?",4),#23
           rep("¿Mediante que normativa fue creada la Unidad de Investigaciones Financieras?",3),#24
           rep("¿Cuál de los siguientes incisos es un delito precedente para la Legitimación de Ganancias Ilícitas?",3),#25
           rep("¿Cuál de las siguientes opciones es un delito precedente para la Legitimación de Ganancias Ilícitas?",3),#26
           rep("¿Cuál de las siguientes alternativas es un delito precedente para la Legitimación de Ganancias Ilícitas?",3),#27
           rep("¿Cuál de los siguientes presentados es un delito precedente para la Legitimación de Ganancias Ilícitas?",3),#28
           rep("¿Cuál de los siguientes es un delito precedente para la Legitimación de Ganancias Ilícitas?",3),#29
           rep("¿Cuál de las opciones es un delito precedente para la Legitimación de Ganancias Ilícitas?",3),#30
           rep("¿Cuáles son los principales delitos precedentes para la Legitimación de Ganancias Ilícitas?",3),#31
           rep("¿Cuáles son los riesgos que podría llegar a tener Banco Sol en caso de ser usado en operaciones de Lavado de Dinero y Financiamiento del Terrorismo?",3),#32
           rep("¿Cuáles son las etapas del Lavado de Dinero?",3),#33
           rep("¿Cuáles son las etapas del Financiamiento del Terrorismo?",3),#34
           rep("¿Con cuántos años se sanciona con privación de libertad al Delito de Financiamiento del Terrorismo?",3),#35
           rep("¿La Legitimación de Ganancias Ilícitas también es conocida como?",3),#36
           rep("¿Cuál es el artículo en el código penal que menciona al Financiamiento del Terrorismo?",3),#37
           rep("¿Cuál es el artículo en el código penal que menciona al Legitimación de Ganancias Ilícitas?",3),#38
           rep("El dinero de origen ilícito, nunca llega a ser LEGAL",2),#39
           rep("¿Cuál de las opciones es una de las etapas del proceso de Legitimación de Ganancias Ilícitas?",3),#40
           rep("¿Cuál de las siguientes, es una de las etapas del proceso de Legitimación de Ganancias Ilícitas?",4),#41
           rep("¿Cuál de las siguientes pertenece a una de las etapas del proceso de Legitimación de Ganancias Ilícitas?",4),#42
           rep("Uno de los riesgos que podrían llegar a tener el Banco Solidario S.A. en caso de ser usado en operaciones de
               Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería el siguiente:",3),#43
           rep("Uno de los riesgos que podrían llegar a tener el Banco Solidario S.A. en caso de ser usado
               en operaciones de Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería",4),#44
           rep("Un riesgo que podrían llegar a tener el Banco Solidario S.A. en caso de ser usado en operaciones de
               Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería el siguiente:",3),#45
           rep("Uno de los riesgos que podrían llegar a tener el Banco Solidario S.A. en caso de ser usado en operaciones de
               Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo, sería:",4),#46
           rep("¿Cuál es una de las etapas del proceso de Financiamiento del Terrorismo?",4),#47
           rep("¿Cuál de las opciones es una de las etapas del proceso de Financiamiento del Terrorismo?",4),#48
           rep("¿Alguna de las siguientes opciones es una de las etapas del proceso de Financiamiento del Terrorismo?",4),#49
           rep("¿Cuál es el artículo en el código penal que menciona al Financiamiento De La Proliferación De Armas De Destrucción Masiva?",3),#50
           rep("La Política de Conocimiento del Cliente no debe mirarse como una actividad aislada que no cumple con los objetivos
               principales del negocio. Por el contrario, una política efectiva de Conocimiento del Cliente puede servir de base
               para una exitosa gestión comercial y de administración del riesgo",2),#51
           rep("La Política de Conocimiento del Cliente es la recolección de fuentes de financiación, ya sean Legales o Ilegales",2),#52
           rep("¿El Financiamiento al Terrorismo es la implementación de bases fundamentales para el conocimiento financiero?",2),#53
           rep("Las cuentas que muestran varios depósitos por debajo del umbral. ¿Representa una señal de alerta?",2),#54
           rep("Las operaciones realizadas frecuentemente o de importes llamativos a nombre de terceros, sin que exista
               justificativo para ello. ¿Representa una señal de alerta?",2),#55
           rep("¿El Banco Solidario S.A. puede ser usado en operaciones de Legitimación de Ganancias Ilícitas y Financiamiento del Terrorismo?",2),#56
           rep("¿Una Operación Inusual es igual a una Operación Sospechosa?",2),#57
           rep("¿Qué significa las siglas UIF?",3),#58
           rep("¿La Ley N°004, es denominada?",3),#59
           rep("¿La Legitimación de Ganancias Ilícitas puede ser financiada por medio de Fondos obtenidos de manera?",3),#60
           rep("¿Cuántas etapas tiene el proceso de Financiamiento del Terrorismo?",3),#61
           rep("La Ley N° 262, es el Régimen de congelamiento de:",3),#62
           rep("¿El Financiamiento de la Proliferación de Armas de Destrucción Masiva es todo acto que provea fondos o utilice servicios
               financieros, en todo o en parte, para la fabricación, adquisición, posesión, desarrollo, exportación, fraccionamiento,
               transporte, transferencia, depósito o uso de armas nucleares, químicas o biológicas, sus medios de lanzamiento y otros
               materiales relacionados?",2),#63
           rep("¿Qué cree usted que es el Lavado de Dinero?",3),#64
           rep("¿En qué consiste la primera etapa del Lavado de Dinero?",3),#65
           rep("En el Decreto Supremo 910 se menciona una multa según el grado de responsabilidad y la gravedad de la infracción, ¿Hasta cuánto es la remuneración?",3),#66
           rep("¿Qué es Financiamiento del Terrorismo?",3),#67
           rep("¿Qué es el Riesgo Reputacional?",3),#68
           rep("¿Qué es el Riesgo Legal?",3),#69
           rep("¿Qué es el Riesgo Operativo?",3),#70
           rep("¿Qué es el Riesgo de Contagio?",3),#71
           rep("¿Cuándo fue creado el GAFI?",3),#72
           rep("¿Las 40 recomendaciones fueron generadas por?",3),#73
           rep("¿Los clientes que frecuentemente solicitan que se incrementen los límites de excepción, son considerados como clientes sospechosos?",2),#74
           rep("¿Los clientes cuyo teléfono se encuentra desconectado, o el número telefónico al momento de efectuar la llamada de verificación, no concuerda con la información inicialmente suministrada, es considerado como una señal de alerta?",2),#75
           rep("Las transferencias electrónicas, sin aparente razón comercial ni consistencia con los negocios habituales o actividad económica declarada del cliente. Representan una:",3),#76
           rep("Cuando el cliente ofrece pagar jugosas comisiones, sin justificativo legal y lógico. ¿Representa una señal de alerta?",2),#77
           rep("La cancelación repentina de préstamos, sin justificación aparente sobre la razón del pago súbito o el origen de los fondos. ¿Representa una señal de alerta?",2),#78
           rep("El frecuente envió o recepción de grandes volúmenes de transferencias electrónicas de o hacia instituciones. ¿Representa una señal de alerta?",2),#79
           rep("Toda transacción por montos relevantes debe tener un respaldo del motivo del movimiento, depósito, origen o destino",2),#80
           rep("¿En qué consiste la segunda etapa del Lavado de Dinero?",3),#81
           rep("¿En qué consiste la tercera etapa del Lavado de Dinero?",3),#82
           rep("¿En qué consiste la primera etapa del proceso de Financiamiento del Terrorismo?",3),#83
           rep("¿En qué consiste la segunda etapa del proceso de Financiamiento del Terrorismo?",3),#84
           rep("¿En qué consiste la tercera etapa del proceso de Financiamiento del Terrorismo?",3),#85
           rep("Una persona que, de un momento a otro, sin fundamento, aparece como dueño de importantes negocios. ¿Representa una señal de alerta?",2),#86
           rep("Un cliente que brinda la información necesaria para su registro en el sistema, y que realiza la actualización de su información cuando se lo solicita ¿Es un cliente sospechoso?",2),#87
           rep("Las Empresas que se abstienen de proporcionar información completa, como actividad principal de la empresa, referencias bancarias, nombre de apoderados y directores, etc. ¿Representan una señal de alerta?",2),#88
           rep("El Consejo Nacional de Lucha Contra la Legitimación de Ganancias Ilícitas y el Financiamiento del Terrorismo está compuesto por los ministerios de:",3),#89
           rep("¿A qué se refiere el Decreto Supremo 910?",3),#90
           rep("¿Cuál es la Ley N° 004?",3),#91
           rep("¿Cómo describirías al Financiamiento de la Proliferación de Armas de Destrucción Masiva?",3),#92
           rep("La Unidad de Investigaciones Financieras es:",3),#93
           rep("La comunicación mediante la cual los Sujetos Obligados (Entidades de Intermediación Financiera) reportan cualquier hecho u operación con independencia de su cuantía, por hechos o situaciones que posiblemente están relacionadas con la Legitimación de Ganancias Ilícitas o la Financiación del Terrorismo. Es el:",4)#94

)
##
option=c("Nombres y Apellidos",#1
         "",#1.1
         "27",#2
         c("Masculino","Femenino"),#3
         c("a) Verdadero","b) Falso"),#4
         c("a) Verdadero","b) Falso"),#5
         c("a) Verdadero","b) Falso"),#6
         c("a) Verdadero","b) Falso"),#7
         c("a) Verdadero","b) Falso"),#8
         c("a) Verdadero","b) Falso"),#9
         c("a) Verdadero","b) Falso"),#10
         c("a) Verdadero","b) Falso"),#11
         c("a) Lavado de Activos","a) Lavado de Activos","c) Legitimación de Ganancias Ilícitas"),#12
         c("a) Grupo de Acción Financiamiento Internacional","b) Grupo de Acción Financiera Internacional",
           "c) Grupo Antilavado Financiero Internacional"),#13
         c("a) Fondos de origen Legítimos","b) Fondos de origen Ilegítimos","c) Fondos de origen Legítimos e Ilegítimos"),#14
         c("a) Toda aquella operación que tiene como característica el manejo de dinero únicamente en efectivo",
           "b) Es toda operación realizada por personas en forma sospechosa","c) Son aquellas operaciones que se alejan al perfil
           transaccional del cliente o no guardan relación con sus ingresos de acuerdo con su actividad, negocio o profesión"),#15
         c("a) Toda aquella transacción financiera que es realizada por un criminal","b) Es la operativa de un cliente o usuario que se realiza a nombre de un tercero",
           "c) Es una operación calificada como inusual cuya observación no puedo ser justificada o respaldada"),#16
         c("a) Al reporte de Retenciones, Observaciones y Sospechas","b) Al procedimiento de Reconocer Objetos Sospechosos",
           "c) A los reportes de Operaciones Sospechosas"),#17
         c("a) Conocer los datos relacionados a su fuente de trabajo y sus ingresos económicos","
           b) Saber dónde vive","c) Obtener del cliente toda la información posible, relacionada a su entorno de vida, entorno económico o de actividad laboral ya sea principal o secundarias",
           "d) Ninguno"),#18
         c("a) De 5 a 10 años","b) De 3 a 5 años","c) De 5 a 7 años"),#19
         c("a) Verdadero","b) Falso"),#20
         c("a) Verdadero","b) Falso"),#21
         c("a) 10 Recomendaciones","b) 40 Recomendaciones","c) 25 Recomendaciones"),#22
         c("a) Tres etapas","b) Dos etapas","c) Una sola etapa","d) Ninguna de las anteriores"),#23
         c("a) Ley 1768","b) Ley 004","c) DS 24771"),#24
         c("a) Contrabando","b) Falta a la moral","c) Violación"),#25
         c("a) Sabotaje","b) Corrupción","c) Estupro"),#26
         c("a) Tráfico de sustancias ilícitas","b) Incumplimiento de contrato","c) Daños al ornato publico"),#27
         c("a) Delito Tributario","b) Delitos de Terrorismo","c) Todas las anteriores"),#28
         c("a) Trata de personas","b) Injuriar","c) Bigamia"),#29
         c("a) Conducción peligrosa","b) Fabricación de sustancias controladas","c) Bigamia"),#30
         c("a) Contrabando, Fabricación, Transporte, Comercialización o Tráfico Ilícito de Sustancias Controladas, Delitos de Corrupción, Terrorismo, Financiamiento del Terrorismo, Trata y Tráfico de Personas, Uso Indebido de Información Privilegiada",
           "b) Estupro, bigamia, faltas a la moral, conducción peligrosa, violencia intrafamiliar",
           "c) Colocación, transformación e integración"),#31
         c("a) Riesgo Reputacional, Riesgo Operativo, Riesgo Legal y Riesgo de Contagio",
           "b) Riesgo conceptual, Riesgo variable, Riesgo de Contagio","c) Riesgo de evaluación, Riesgo sumatorio y Riesgo de Operativo"),#32
         c("a) Concentración, Asignación y Distribución","b) Recaudación, colocación e Información",
           "c) Colocación, Transformación e Integración"),#33
         c("a) Restitución, Colocación e Información","b) Recolección, Disposición y Utilización","c) Recolección, Diversificación y Distribución"),#34
         c("a) Será sancionado con presidio de treinta (30) a cincuenta (50) años","b) Será sancionado con presidio de quince (15) a veinte (20) años",
           "c) Será sancionado con presidio de dos (2) a cinco (5) años"),#35
         c("a) Delito de bigamia","b) Lavado de Activos","c) Corrupción"),#36
         c("a) Artículo 133 Bis","b) Artículo 20 Bis","c) Artículo 103 Bis"),#37
         c("a) Artículo 55 Bis","b) Artículo 88 Bis","c) Artículo 185 Bis"),#38
         c("a) Verdadero","b) Falso"),#39
         c("a) Colocación","b) Utilización","c) Consolidación"),#40
         c("a) Recaudación","b) Transformación","c) Restitución","d) Ninguna"),#41
         c("a) Recaudación","b) Integración","c) Restitución","d) Ninguna"),#42
         c("a) Riesgo Reputacional","b) Riesgo Financiero","c) Riesgo Ocupacional"),#43
         c("a) Riesgo Financiero","b) Riesgo Sistemático","c) Riesgo Operativo","d) Ninguno"),#44
         c("a) Riesgo Legal","b) Riesgo Crediticio","c) Riesgo Ocupacional"),#45
         c("a) Riesgo Financiero","b) Riesgo Ocupacional","c) Riesgo de Contagio","d) Ninguno"),#46
         c("a) Corrupción","b) Disposición","c) Fabricación","d) Ninguno"),#47
         c("a) Información","b) Establecimiento","c) Utilización","d) Ninguno"),#48
         c("a) Recaudación","b) Sustitución","c) Restablecimiento","d) Ninguno"),#49
         c("a) Artículo 105 Bis","b) Artículo 155 Bis","c) Artículo 141 Bis"),#50
         c("a) Verdadero","b) Falso"),#51
         c("a) Verdadero","b) Falso"),#52
         c("a) Verdadero","b) Falso"),#53
         c("a) Verdadero","b) Falso"),#54
         c("a) Verdadero","b) Falso"),#55
         c("a) Verdadero","b) Falso"),#56
         c("a) Verdadero","b) Falso"),#57
         c("a) Unidad de Inteligencia de Finanzas","b) Unidad de Investigaciones Financieras","c) Unidad de Instancias Financieras"),#58
         c("a) Ley de Lucha contra el Terrorismo","b) Hugo Banzer Suarez","c) Marcelo Quiroga Santa Cruz"),#59
         c("a) Fondos de origen Legal","b) Fondos de origen Ilegal","c) Ambos son correctos"),#60
         c("a) 5 etapas","b) 2 etapas","c) 3 etapas"),#61
         c("a) fondos y otros activos de personas vinculadas con acciones de terrorismo y financiamiento del terrorismo",
           "b) información de personas","c) tráfico de armas"),#62
         c("a) Verdadero","b) Falso"),#63
         c("a) Es el acto de cambiar la divisa del dinero obtenido ilegalmente","b) Es el proceso de esconder o disfrazar la existencia, origen y el destino de bienes o dinero producto de actividades ilegales para transformarlo en dinero licito",
           "c) Es el proceso de cambiar los cortes de los billetes de menor a mayor corte"),#64
         c("a) En Transformar el dinero ilícito en licito","b) En integrar el dinero licito al torrente financiero","c) Colocar el dinero dentro del sistema utilizando una serie de artilugios"),#65
         c("a) Hasta cinco (5) veces la remuneración mensual del infractor de la Entidad Financiera",
           "b) Hasta dos (2) veces la remuneración mensual del infractor de la Entidad Financiera",
           "c) Hasta tres (3) veces la remuneración mensual del infractor de la Entidad Financiera"),#66
         c("a) Es el proceso por el cual se captan capitales que pueden ser lícitos o ilícitos para ser destinados a financiar actividades terroristas",
           "b) Es cualquier forma de acción económica, ayuda o mediación que proporcione apoyo financiero a las actividades financieras",
           "c) Ninguna de las anteriores"),#67
         c("a) Definido como la capacidad de que el nombre y la imagen corporativa de la entidad sea menoscabada",
           "b) Definido como la capacidad de administrar los fondos de una institución",
           "c) Falta de información para tomar decisiones"),#68
         c("a) Definido como la capacidad de que tiene un deudor de cumplir con sus obligaciones de pago",
           "b) Entendido como la posibilidad de que la entidad sea sancionada o condenada al pago de indemnizaciones y que sus funcionarios se vean involucrados en procesos administrativos y penales, con penas de orden económico o de privación de libertad",
           "c) Posibilidad de que una empresa no consiga cumplir con sus compromisos"),#69
         c("a) Se refiere al cambio de valor que se produce en las acciones, bonos, etc",
           "b) Definido como la capacidad de administrar los fondos de una institución",
           "c) Es posibilidad de pérdidas económicas a causa de fallas humanas, técnicas o procedimentales"),#70
         c("a) Es la posibilidad de que la entidad pueda sufrir una afectación reputacional, legal o económica a causa de la acción propia de una empresa relacionada o asociada a ella",
           "b) Falta de información para tomar decisiones",
           "c) Posibilidad de que una empresa no consiga cumplir con sus compromisos"),#71
         c("a) En la década de los 80","b) En la década de los 70","c) En la década de los 60"),#72
         c("a) La UIF","b) El GAFI","c) Ninguno"),#73
         c("a) Verdadero","b) Falso"),#74
         c("a) Verdadero","b) Falso"),#75
         c("a) operación normal","b) señal habitual","c) señal de alerta"),#76
         c("a) Verdadero","b) Falso"),#77
         c("a) Verdadero","b) Falso"),#78
         c("a) Verdadero","b) Falso"),#79
         c("a) Verdadero","b) Falso"),#80
         c("a) En Transformar el dinero ilícito en licito","b) En integrar el dinero licito al torrente financiero",
           "c) Colocar el dinero dentro del sistema utilizando una serie de artilugios"),#81
         c("a) En Transformar el dinero ilícito en licito","b) En integrar el dinero licito al torrente financiero",
           "c) Colocar el dinero dentro del sistema utilizando una serie de artilugios"),#82
         c("a) En la recolección de fuentes de financiación, ya sean Legales o Ilegales",
           "b) Movilizar los fondos ocultando sus movimientos y destino final",
           "c) Consiste en la utilización de los fondos para la financiación, planeación y ejecución de actos terroristas"),#83
         c("a) En la recolección de fuentes de financiación, ya sean Legales o Ilegales",
           "b) Movilizar los fondos ocultando sus movimientos y destino final","c) Consiste en la utilización de los fondos para la financiación, planeación y ejecución de actos terroristas"),#84
         c("a) En la recolección de fuentes de financiación, ya sean Legales o Ilegales","b) Movilizar los fondos ocultando sus movimientos y destino final",
           "c) Consiste en la utilización de los fondos para la financiación, planeación y ejecución de actos terroristas"),#85
         c("a) Verdadero","b) Falso"),#86
         c("a) Verdadero","b) Falso"),#87
         c("a) Verdadero","b) Falso"),#88
         c("a) Economía y Finanzas Públicas, Gobierno, Defensa y Justicia","b) Transparencia institucional y Lucha Contra la Corrupción",
           "c) Todas las anteriores"),#89
         c("a) A la creación de la Unidad de Investigación Financiera",
           "b) A la sanción por incumplimiento al proceso normativo para la Prevención, Detección y Reporte de operaciones de Legitimación de Ganancias Ilícitas",
           "c) A la sanción por incumplimiento de armas de destrucción masiva"),#90
         c("a) La de los fondos y otros activos de personas vinculadas al financiamiento del terrorismo",
           "b) La que crea a la Unidad de Investigaciones Financieras",
           "c) Lucha Contra la Corrupción, Enriquecimiento Ilícito e Investigación de Fortunas"),#91
         c("a) Es el proceso de esconder o disfrazar la existencia, fuente ilegal, movimiento, destino o uso ilegal de bienes o fondos, producto de actividades ilegales para luego hacerlos aparentar como legítimos",
           "b) Es todo acto que provea fondos o utilice servicios financieros, en todo o en parte, para la fabricación, adquisición, posesión, desarrollo, exportación, fraccionamiento, transporte, transferencia, depósito o uso de armas nucleares, químicas o biológicas, sus medios de lanzamiento y otros materiales relacionados",
           "c) Ninguna es correcta"),#92
         c("a) La encargada de normar y promulgar decretos y leyes para la lucha contra el lavado de dinero y financiamiento del terrorismo",
           "b) Un grupo intergubernamental cuya oficina central tiene sede en París","c) El organismo encargado del planeamiento y ejecución de la política económica del Estado"),#93
         c("a) Reporte de Operación (es) Sospechosa (S)","b) Informe de Inusualidad","c) Reporte de Actualización de Clientes",
           "d) Todas son correctas")#94
)
##
input_type=c("text", #1
             "text",#1.1
             "numeric",#2
             c("mc","mc"),#3
             c("mc","mc"),#4
             c("mc","mc"),#5
             c("mc","mc"),#6
             c("mc","mc"),#7
             c("mc","mc"),#8
             rep("mc",2),#9
             rep("mc",2),#10
             rep("mc",2),#11
             rep("mc",3),#12
             rep("mc",3),#13
             rep("mc",3),#14
             rep("mc",3),#15
             rep("mc",3),#16
             rep("mc",3),#17
             rep("mc",4),#18
             rep("mc",3),#19
             rep("mc",2),#20
             rep("mc",2),#21
             rep("mc",3),#22
             rep("mc",4),#23
             rep("mc",3),#24
             rep("mc",3),#25
             rep("mc",3),#26
             rep("mc",3),#27
             rep("mc",3),#28
             rep("mc",3),#29
             rep("mc",3),#30
             rep("mc",3),#31
             rep("mc",3),#32
             rep("mc",3),#33
             rep("mc",3),#34
             rep("mc",3),#35
             rep("mc",3),#36
             rep("mc",3),#37
             rep("mc",3),#38
             rep("mc",2),#39
             rep("mc",3),#40
             rep("mc",4),#41
             rep("mc",4),#42
             rep("mc",3),#43
             rep("mc",4),#44
             rep("mc",3),#45
             rep("mc",4),#46
             rep("mc",4),#47
             rep("mc",4),#48
             rep("mc",4),#49
             rep("mc",3),#50
             rep("mc",2),#51
             rep("mc",2),#52
             rep("mc",2),#53
             rep("mc",2),#54
             rep("mc",2),#55
             rep("mc",2),#56
             rep("mc",2),#57
             rep("mc",3),#58
             rep("mc",3),#59
             rep("mc",3),#60
             rep("mc",3),#61
             rep("mc",3),#62
             rep("mc",2),#63
             rep("mc",3),#64
             rep("mc",3),#65
             rep("mc",3),#66
             rep("mc",3),#67
             rep("mc",3),#68
             rep("mc",3),#69
             rep("mc",3),#70
             rep("mc",3),#71
             rep("mc",3),#72
             rep("mc",3),#73
             rep("mc",2),#74
             rep("mc",2),#75
             rep("mc",3),#76
             rep("mc",2),#77
             rep("mc",2),#78
             rep("mc",2),#79
             rep("mc",2),#80
             rep("mc",3),#81
             rep("mc",3),#82
             rep("mc",3),#83
             rep("mc",3),#84
             rep("mc",3),#85
             rep("mc",2),#86
             rep("mc",2),#87
             rep("mc",2),#88
             rep("mc",3),#89
             rep("mc",3),#90
             rep("mc",3),#91
             rep("mc",3),#92
             rep("mc",3),#93
             rep("mc",4)#94

)
##
# input_id
input_id=c("nombre",#1
           "num_doc",#1.1
           "edad",#2
           c("genero","genero"),#3
           c("p1","p1"),#4
           c("p2","p2"),#5
           c("p3","p3"),#6
           c("p4","p4"),#7
           c("p5","p5"),#8
           rep("p6",2),#9
           rep("p7",2),#10
           rep("p8",2),#11
           rep("p9",3),#12
           rep("p10",3),#13
           rep("p11",3),#14
           rep("p12",3),#15
           rep("p13",3),#16
           rep("p14",3),#17
           rep("p15",4),#18
           rep("p16",3),#19
           rep("p17",2),#20
           rep("p18",2),#21
           rep("p19",3),#22
           rep("p20",4),#23
           rep("p21",3),#24
           rep("p22",3),#25
           rep("p23",3),#26
           rep("p24",3),#27
           rep("p25",3),#28
           rep("p26",3),#29
           rep("p27",3),#30
           rep("p28",3),#31
           rep("p29",3),#32
           rep("p30",3),#33
           rep("p31",3),#34
           rep("p32",3),#35
           rep("p33",3),#36
           rep("p34",3),#37
           rep("p35",3),#38
           rep("p36",2),#39
           rep("p37",3),#40
           rep("p38",4),#41
           rep("p39",4),#42
           rep("p40",3),#43
           rep("p41",4),#44
           rep("p42",3),#45
           rep("p43",4),#46
           rep("p44",4),#47
           rep("p45",4),#48
           rep("p46",4),#49
           rep("p47",3),#50
           rep("p48",2),#51
           rep("p49",2),#52
           rep("p50",2),#53
           rep("p51",2),#54
           rep("p52",2),#55
           rep("p53",2),#56
           rep("p54",2),#57
           rep("p55",3),#58
           rep("p56",3),#59
           rep("p57",3),#60
           rep("p58",3),#61
           rep("p59",3),#62
           rep("p60",2),#63
           rep("p61",3),#64
           rep("p62",3),#65
           rep("p63",3),#66
           rep("p64",3),#67
           rep("p65",3),#68
           rep("p66",3),#69
           rep("p67",3),#70
           rep("p68",3),#71
           rep("p69",3),#72
           rep("p70",3),#73
           rep("p71",2),#74
           rep("p72",2),#75
           rep("p73",3),#76
           rep("p74",2),#77
           rep("p75",2),#78
           rep("p76",2),#79
           rep("p77",2),#80
           rep("p78",3),#81
           rep("p79",3),#82
           rep("p80",3),#83
           rep("p81",3),#84
           rep("p82",3),#85
           rep("p83",2),#86
           rep("p84",2),#87
           rep("p85",2),#88
           rep("p86",3),#89
           rep("p87",3),#90
           rep("p88",3),#91
           rep("p89",3),#92
           rep("p90",3),#93
           rep("p91",4)#94
)
# dependence
dependence=c(NA,#1
             NA,#1.1
             NA,#2
             c(NA,NA),#3
             c(NA,NA),#4
             c(NA,NA),#5
             c(NA,NA),#6
             c(NA,NA),#7
             c(NA,NA),#8
             rep(NA,2),#9
             rep(NA,2),#10
             rep(NA,2),#11
             rep(NA,3),#12
             rep(NA,3),#13
             rep(NA,3),#14
             rep(NA,3),#15
             rep(NA,3),#16
             rep(NA,3),#17
             rep(NA,4),#18
             rep(NA,3),#19
             rep(NA,2),#20
             rep(NA,2),#21
             rep(NA,3),#22
             rep(NA,4),#23
             rep(NA,3),#24
             rep(NA,3),#25
             rep(NA,3),#26
             rep(NA,3),#27
             rep(NA,3),#28
             rep(NA,3),#29
             rep(NA,3),#30
             rep(NA,3),#31
             rep(NA,3),#32
             rep(NA,3),#33
             rep(NA,3),#34
             rep(NA,3),#35
             rep(NA,3),#36
             rep(NA,3),#37
             rep(NA,3),#38
             rep(NA,2),#39
             rep(NA,3),#40
             rep(NA,4),#41
             rep(NA,4),#42
             rep(NA,3),#43
             rep(NA,4),#44
             rep(NA,3),#45
             rep(NA,4),#46
             rep(NA,4),#47
             rep(NA,4),#48
             rep(NA,4),#49
             rep(NA,3),#50
             rep(NA,2),#51
             rep(NA,2),#52
             rep(NA,2),#53
             rep(NA,2),#54
             rep(NA,2),#55
             rep(NA,2),#56
             rep(NA,2),#57
             rep(NA,3),#58
             rep(NA,3),#59
             rep(NA,3),#60
             rep(NA,3),#61
             rep(NA,3),#62
             rep(NA,2),#63
             rep(NA,3),#64
             rep(NA,3),#65
             rep(NA,3),#66
             rep(NA,3),#67
             rep(NA,3),#68
             rep(NA,3),#69
             rep(NA,3),#70
             rep(NA,3),#71
             rep(NA,3),#72
             rep(NA,3),#73
             rep(NA,2),#74
             rep(NA,2),#75
             rep(NA,3),#76
             rep(NA,2),#77
             rep(NA,2),#78
             rep(NA,2),#79
             rep(NA,2),#80
             rep(NA,3),#81
             rep(NA,3),#82
             rep(NA,3),#83
             rep(NA,3),#84
             rep(NA,3),#85
             rep(NA,2),#86
             rep(NA,2),#87
             rep(NA,2),#88
             rep(NA,3),#89
             rep(NA,3),#90
             rep(NA,3),#91
             rep(NA,3),#92
             rep(NA,3),#93
             rep(NA,4)#94
)
##
dependence_value=c(NA,#1
                   NA,#1.1
                   NA,#2
                   c(NA,NA),#3
                   c(NA,NA),#4
                   c(NA,NA),#5
                   c(NA,NA),#6
                   c(NA,NA),#7
                   c(NA,NA),#8
                   rep(NA,2),#9
                   rep(NA,2),#10
                   rep(NA,2),#11
                   rep(NA,3),#12
                   rep(NA,3),#13
                   rep(NA,3),#14
                   rep(NA,3),#15
                   rep(NA,3),#16
                   rep(NA,3),#17
                   rep(NA,4),#18
                   rep(NA,3),#19
                   rep(NA,2),#20
                   rep(NA,2),#21
                   rep(NA,3),#22
                   rep(NA,4),#23
                   rep(NA,3),#24
                   rep(NA,3),#25
                   rep(NA,3),#26
                   rep(NA,3),#27
                   rep(NA,3),#28
                   rep(NA,3),#29
                   rep(NA,3),#30
                   rep(NA,3),#31
                   rep(NA,3),#32
                   rep(NA,3),#33
                   rep(NA,3),#34
                   rep(NA,3),#35
                   rep(NA,3),#36
                   rep(NA,3),#37
                   rep(NA,3),#38
                   rep(NA,2),#39
                   rep(NA,3),#40
                   rep(NA,4),#41
                   rep(NA,4),#42
                   rep(NA,3),#43
                   rep(NA,4),#44
                   rep(NA,3),#45
                   rep(NA,4),#46
                   rep(NA,4),#47
                   rep(NA,4),#48
                   rep(NA,4),#49
                   rep(NA,3),#50
                   rep(NA,2),#51
                   rep(NA,2),#52
                   rep(NA,2),#53
                   rep(NA,2),#54
                   rep(NA,2),#55
                   rep(NA,2),#56
                   rep(NA,2),#57
                   rep(NA,3),#58
                   rep(NA,3),#59
                   rep(NA,3),#60
                   rep(NA,3),#61
                   rep(NA,3),#62
                   rep(NA,2),#63
                   rep(NA,3),#64
                   rep(NA,3),#65
                   rep(NA,3),#66
                   rep(NA,3),#67
                   rep(NA,3),#68
                   rep(NA,3),#69
                   rep(NA,3),#70
                   rep(NA,3),#71
                   rep(NA,3),#72
                   rep(NA,3),#73
                   rep(NA,2),#74
                   rep(NA,2),#75
                   rep(NA,3),#76
                   rep(NA,2),#77
                   rep(NA,2),#78
                   rep(NA,2),#79
                   rep(NA,2),#80
                   rep(NA,3),#81
                   rep(NA,3),#82
                   rep(NA,3),#83
                   rep(NA,3),#84
                   rep(NA,3),#85
                   rep(NA,2),#86
                   rep(NA,2),#87
                   rep(NA,2),#88
                   rep(NA,3),#89
                   rep(NA,3),#90
                   rep(NA,3),#91
                   rep(NA,3),#92
                   rep(NA,3),#93
                   rep(NA,4)#94
)
##
# required
required=c(T,#1
           T,#1.1
           T,#2
           rep(T,2),#3
           rep(T,2),#4
           rep(T,2),#5
           rep(T,2),#6
           rep(T,2),#7
           rep(T,2),#8
           rep(T,2),#9
           rep(T,2),#10
           rep(T,2),#11
           rep(T,3),#12
           rep(T,3),#13
           rep(T,3),#14
           rep(T,3),#15
           rep(T,3),#16
           rep(T,3),#17
           rep(T,4),#18
           rep(T,3),#19
           rep(T,2),#20
           rep(T,2),#21
           rep(T,3),#22
           rep(T,4),#23
           rep(T,3),#24
           rep(T,3),#25
           rep(T,3),#26
           rep(T,3),#27
           rep(T,3),#28
           rep(T,3),#29
           rep(T,3),#30
           rep(T,3),#31
           rep(T,3),#32
           rep(T,3),#33
           rep(T,3),#34
           rep(T,3),#35
           rep(T,3),#36
           rep(T,3),#37
           rep(T,3),#38
           rep(T,2),#39
           rep(T,3),#40
           rep(T,4),#41
           rep(T,4),#42
           rep(T,3),#43
           rep(T,4),#44
           rep(T,3),#45
           rep(T,4),#46
           rep(T,4),#47
           rep(T,4),#48
           rep(T,4),#49
           rep(T,3),#50
           rep(T,2),#51
           rep(T,2),#52
           rep(T,2),#53
           rep(T,2),#54
           rep(T,2),#55
           rep(T,2),#56
           rep(T,2),#57
           rep(T,3),#58
           rep(T,3),#59
           rep(T,3),#60
           rep(T,3),#61
           rep(T,3),#62
           rep(T,2),#63
           rep(T,3),#64
           rep(T,3),#65
           rep(T,3),#66
           rep(T,3),#67
           rep(T,3),#68
           rep(T,3),#69
           rep(T,3),#70
           rep(T,3),#71
           rep(T,3),#72
           rep(T,3),#73
           rep(T,2),#74
           rep(T,2),#75
           rep(T,3),#76
           rep(T,2),#77
           rep(T,2),#78
           rep(T,2),#79
           rep(T,2),#80
           rep(T,3),#81
           rep(T,3),#82
           rep(T,3),#83
           rep(T,3),#84
           rep(T,3),#85
           rep(T,2),#86
           rep(T,2),#87
           rep(T,2),#88
           rep(T,3),#89
           rep(T,3),#90
           rep(T,3),#91
           rep(T,3),#92
           rep(T,3),#93
           rep(T,4)#94
)



#### descargar csv #############
output$Desc <- downloadHandler(
  filename = function() {
    paste("Datos_Postulantes", ".csv", sep="")
  },
  content = function(file) {
    cv_cases_sub= reactivo_1()
    write.csv(cv_cases_sub, file)
  }
)
##
downloadButton("Desc", "Descargar"),tags$br(),tags$br(),
"Adapted from timeline data published by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series",
                                                   "Johns Hopkins Center for Systems Science and Engineering.")

##########
Reg_oriente <- read_excel("Lore/base 2/Reg_oriente.xlsx")
Base_consolidada= rbind(Reg_Alto, Reg_centro, Reg_Occidente, Reg_oriente, Reg_Sur)
Base_consolidada= Base_consolidada %>% filter(CLASIFICACION=="T"| CLASIFICACION=="C")
MS164689= MS164689 %>% mutate(corre=1:nrow(MS164689))
Base_filtro= MS164689 %>% left_join(Base_consolidada[,c(5,12,14,15,16)], by=c("nro_documento"="NUMERO_DOC"))
Base_filtro= Base_filtro %>% group_by(corre) %>% filter(row_number()==1)
Base_filtro= as.data.frame(Base_filtro)

########## limpieza de informacion BANTOTAL ABRIL 2024 ##########
DatosCumplimientoClientes_BASE1 <- read_excel("base clientes/marzo_2024/DatosCumplimientoClientes_BASE1.xlsx")
DatosCumplimientoClientes_BASE2 <- read_excel("base clientes/marzo_2024/DatosCumplimientoClientes_BASE2.xlsx")
DatosCumplimientoClientes_BASE3 <- read_excel("base clientes/marzo_2024/DatosCumplimientoClientes_BASE3.xlsx")
DatosCumplimientoClientes_BASE4 <- read_excel("base clientes/marzo_2024/DatosCumplimientoClientes_BASE4.xlsx")
DatosCumplimientoClientes_BASE5 <- read_excel("base clientes/marzo_2024/DatosCumplimientoClientes_BASE5.xlsx")

######## casp que no está en baseclientesBsol 8953707SC 8735278 ###########

hchart(resumen , "spline",hcaes(x=mes_red, y=n, group=Informe),
       # stacking = list(enabled = TRUE),

       dataLabels = list(
         enabled = FALSE,
         lang= list(shortMonths=list('Enero', 'Febrero', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'))
         #, # Añadir etiquetas
         #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())%>% hc_rangeSelector(enabled= TRUE, verticalAlign = "top")%>%
  hc_colors(c("#382A54FF","orange", "#403872FF", "#40498EFF","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
  hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))




a <- c("a", "b", "c", "d", "e")
b <- c(1:5)

x <- data.frame(a,b)
categoria
##### Cambiar parametros de opcies de grafico HIGHCHARTER ####
hcoptslang <- getOption("highcharter.lang")
hcoptslang$months <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')
hcoptslang$shortMonths <- c('Ene', 'Feb', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic')

options(highcharter.lang = hcoptslang)
#
hchart(resumen , "spline",hcaes(x=mes_red, y=n, group=Informe),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = FALSE,
         lang= list(shortMonths=list('Enero', 'Febrero', 'Mar', 'Abr', 'May', 'Jun', 'Jul', 'Ago', 'Sep', 'Oct', 'Nov', 'Dic'))
         #, # Añadir etiquetas
         #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())%>% hc_rangeSelector(enabled= TRUE, verticalAlign = "top")%>%
  hc_colors(c("#56147DFF","orange", "#403872FF", "#40498EFF","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
  hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
#####
informe_filt= Informes %>% filter(año==2023 & Informe=="SI") %>% filter(FECHA_DE_EMISION_DEL_INFORME>=as.Date("2023-09-01") &
                                                                          FECHA_DE_EMISION_DEL_INFORME<=as.Date("2023-11-30")  )
informe_filt_1= informe_filt %>% count(REGIONAL, DELITO_PRECEDENTE)
informe_filt_1= informe_filt_1 %>% spread(DELITO_PRECEDENTE, n)




kbl(informe_filt_1, escape = FALSE)  %>%
  add_header_above(c(" "=1, "Delito de LGI y/o Delitos precedentes"=12)) %>%
  kable_styling(
    bootstrap_options = c("hover", "condensed", "responsive"),
    full_width = TRUE
  )




kbl(informe_filt_1, escape = FALSE)  %>%
  add_header_above(c(" "=1, "Delito de LGI y/o Delitos precedentes"=12)) %>% row_spec(0, font_size = 8) %>%
  column_spec(1:13,width = "3cm")




informe_filt_1= Informes %>% count(REGIONAL, DELITO_PRECEDENTE)
informe_filt_1= informe_filt_1 %>% spread(DELITO_PRECEDENTE, n)
informe_filt_1 %>%  kable(booktabs = TRUE,format = "latex")  %>%
  kable_styling(
    latex_options = c("striped", "condensed","scale_down"),
    position = "center",
    full_width = FALSE)


kable(informe_filt_1,booktabs = TRUE)  %>%
  kable_styling(
    latex_options = c("striped", "condensed","scale_down"),
    position = "center",
    full_width = FALSE)




hchart(consult_filt , "column",hcaes(x=TIPO, y=CANTIDAD, group=TIPO_PERSONA),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())%>%
  hc_colors(c("#382A54FF","orange", "#403872FF", "#40498EFF","#C73D73FF"))%>%
  hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))

### FINDER ALERTAS REPORTE ####
cant_datos_doc= Alertas %>% count(Nro_de_identificacion, sort = T)
#
datos= Alertas %>% filter(`Estado de la Alerta`=="Pendiente")
datos_pendientes_reales= datos %>% filter(is.na(Descripcion_cierre))
#
datos_pend_doc= datos_pendientes_reales %>% count(Nro_de_identificacion)
#
datos_pendientes_con_cierre= datos %>% filter(!is.na(Descripcion_cierre))
datos_pendientes_con_cierre$Descripcion_cierre= toupper(datos_pendientes_con_cierre$Descripcion_cierre)

#datos_pendientes_con_cierre= datos_pendientes_con_cierre %>% mutate(indica= ifelse(str_detect(Descripcion_cierre,"INUSUAL"),1,0))
#datos_pendientes_con_cierre_real= datos_pendientes_con_cierre %>% filter(indica==0)
#write.xlsx(datos_pendientes_con_cierre_real, file = "datos_pendientes_con_cierre_real.xlsx")
#
Alertas_sin_cierre_3004_1=Alertas_sin_cierre_3004 %>% filter(`Estado de la Alerta`=="Finalizada") %>% group_by(`Nro. de Identificación`) %>% tally()
#no cerradas
datos_pendientes_reales_1= datos_pendientes_reales %>% filter(!(`Nro. de Identificación` %in% Alertas_sin_cierre_3004_1$`Nro. de Identificación`))
datos_pendientes_reales_1= datos_pendientes_reales_1 %>% filter(!is.na(`Valor Operado`))
indica=NULL
for (i in 1:nrow(datos_pendientes_reales_1)) {
  if(datos_pendientes_reales_1$`Valor Esperado`[i] %in% c(350,500,1663,7500,30,15000,22000)){
    indica[i]= datos_pendientes_reales_1$`Valor Esperado`[i]
  }
  else{indica[i]= 555}
}
table(indica)
datos_pendientes_reales_1= cbind(datos_pendientes_reales_1, indica)
## cerradas
datos_pendientes_reales_2= datos_pendientes_reales %>% filter((`Nro. de Identificación` %in% Alertas_sin_cierre_3004_1$`Nro. de Identificación`))
datos_pendientes_reales_2=datos_pendientes_reales_2 %>% mutate(corre=1:nrow(datos_pendientes_reales_2))
datos_pend_reales_2_cant= datos_pendientes_reales_2 %>% left_join(Alertas_sin_cierre_3004_1, by="Nro. de Identificación")
datos_pend_reales_2_cant= datos_pend_reales_2_cant %>% group_by(corre) %>% filter(row_number()==1)
##
categoria=NULL
for (i in 1:nrow(datos_pend_reales_2_cant_1)) {
  if(datos_pend_reales_2_cant_1$n[i]<=5){
    categoria[i]="1 - 5"
  }
  else if(datos_pend_reales_2_cant_1$n[i]>5 & datos_pend_reales_2_cant_1$n[i]<=10)
  {categoria[i]="6 - 10"}
  else if(datos_pend_reales_2_cant_1$n[i]>10 & datos_pend_reales_2_cant_1$n[i]<=20)
  {categoria[i]="11 - 20"}
  else if(datos_pend_reales_2_cant_1$n[i]>20 & datos_pend_reales_2_cant_1$n[i]<=30)
  {categoria[i]="21 - 30"}
  else if(datos_pend_reales_2_cant_1$n[i]>30 & datos_pend_reales_2_cant_1$n[i]<=40)
  {categoria[i]="31 - 40"}
  else if(datos_pend_reales_2_cant_1$n[i]>40 & datos_pend_reales_2_cant_1$n[i]<=50)
  {categoria[i]="41 - 50"}
  else if(datos_pend_reales_2_cant_1$n[i]>50 & datos_pend_reales_2_cant_1$n[i]<=70)
  {categoria[i]="51 - 70"}
  else if(datos_pend_reales_2_cant_1$n[i]>70 & datos_pend_reales_2_cant_1$n[i]<=100)
  {categoria[i]="71 - 100"}
  else if(datos_pend_reales_2_cant_1$n[i]>100)
  {categoria[i]=">100"}
}
table(categoria)
datos_pend_reales_2_cant_1= data.frame(datos_pend_reales_2_cant_1, categoria)
datos_pend_reales_2_cant_1=datos_pend_reales_2_cant %>% group_by(`Nro. de Identificación`,n)
#
resumen=datos_pend_reales_2_cant_1 %>% group_by(categoria) %>% summarise(clientes=n(), alertas_pendientes= sum(nn))


##### para ros categoria #####
categoria=rep(NA,nrow(Base_ROS_junio2024))
for (i in 1:nrow(Base_ROS_junio2024)) {
  if(Base_ROS_junio2024$Edad[i]<=25){
    categoria[i]="18 - 25"
  }
  else if(Base_ROS_junio2024$Edad[i]>25 & Base_ROS_junio2024$Edad[i]<=30)
  {categoria[i]="26 - 30"}
  else if(Base_ROS_junio2024$Edad[i]>30 & Base_ROS_junio2024$Edad[i]<=40)
  {categoria[i]="31 - 40"}
  else if(Base_ROS_junio2024$Edad[i]>40 & Base_ROS_junio2024$Edad[i]<=50)
  {categoria[i]="41 - 50"}
  else if(Base_ROS_junio2024$Edad[i]>50 & Base_ROS_junio2024$Edad[i]<=60)
  {categoria[i]="51 - 60"}
  else if(Base_ROS_junio2024$Edad>60)
  {categoria[i]="> 61"}
}
table(categoria)
Base_ROS_junio2024= data.frame(Base_ROS_junio2024, categoria)

datos_conteo= Informes %>% count(año, Informe)
hchart(datos_conteo , "column",hcaes(x=año, y=n, group=Informe),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())%>%
  hc_colors(c("#56147DFF","orange"))%>%
  hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
##

highcharter::hw_grid(hchart(datos_conteo , "column",hcaes(x=año, y=n, group=Informe),
                            # stacking = list(enabled = TRUE),
                            dataLabels = list(
                              enabled = TRUE
                              #, # Añadir etiquetas
                              #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
                              # format = "{point.percentage:.0f}%"
                            )) %>% hc_add_theme(hc_theme_google())%>%
                       hc_colors(c("#56147DFF","orange"))%>%
                       hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text="")),
                     hchart(datos_conteo , "bar",hcaes(x=año, y=n, group=Informe),
                            # stacking = list(enabled = TRUE),
                            dataLabels = list(
                              enabled = TRUE
                              #, # Añadir etiquetas
                              #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
                              # format = "{point.percentage:.0f}%"
                            )) %>% hc_add_theme(hc_theme_google())%>%
                       hc_colors(c("#56147DFF","orange"))%>%
                       hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text="")),
                     ncol = 2)

############
# por gestion y sexo
graf_informes= Informes_realizados %>% filter(!is.na(Sexo)) %>% count(año, Sexo)
hchart(graf_informes , "column",hcaes(x=año, y=n, group=Sexo),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())%>%
  hc_colors(c("#56147DFF","orange"))%>%
  hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
##
graf_informes1= Informes_realizados %>% filter(!is.na(Sexo)) %>% count(año, categoria)
hchart(graf_informes , "column",hcaes(x=categoria, y=n, group=año),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())%>%
  hc_colors(c("#56147DFF","orange", "#40498EFF","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
  hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
##
graf_informes2= Informes_realizados %>% filter(!is.na(Sexo)) %>% count(Sexo, categoria)
hchart(graf_informes2 , "column",hcaes(x=categoria, y=n, group=Sexo),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())%>%
  hc_colors(c("#56147DFF","orange", "#40498EFF","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
  hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
#
graf_6= Informes_realizados %>% count(TIPO_DE_PERSONA)
graf_6<- data.frame(graf_6, TIPO="ROS")
hchart(graf_6 , "pie",innerSize = 120, hcaes(x = TIPO_DE_PERSONA, y = n, group=TIPO),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE,
         #, # Añadir etiquetas
         #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         format = "{point.percentage:.1f}%"
       )) %>% hc_add_theme(hc_theme_google()) %>%
  hc_colors(c("#56147DFF","orange"))
##
graf_informes2= Informes_realizados %>% filter(!is.na(Sexo)) %>% count(categoria, Detalle_Regional_Alta_Cliente)
hchart(graf_informes2 , "bar",hcaes(x=Detalle_Regional_Alta_Cliente, y=n, group=categoria),
       # stacking = list(enabled = TRUE),
       dataLabels = list(
         enabled = TRUE
         #, # Añadir etiquetas
         #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
         # format = "{point.percentage:.0f}%"
       )) %>% hc_add_theme(hc_theme_google())%>%
  hc_colors(c("#56147DFF","orange", "#40498EFF","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
  hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
#####
# categoria= rep(NA,nrow(Informes_realizados))
# for (i in 1:nrow(Informes_realizados)) {
#   if(!is.na(Informes_realizados$Edad[i])){
#     if(Informes_realizados$Edad[i]<=25)
#       categoria[i]="18 - 25"
#     else if(Informes_realizados$Edad[i]>25 & Informes_realizados$Edad[i]<=30)
#       categoria[i]="26 - 30"
#     else if(Informes_realizados$Edad[i]>30 & Informes_realizados$Edad[i]<=40)
#       categoria[i]="31 - 40"
#     else if(Informes_realizados$Edad[i]>40 & Informes_realizados$Edad[i]<=50)
#       categoria[i]="41 - 50"
#     else if(Informes_realizados$Edad[i]>50 & Informes_realizados$Edad[i]<=60)
#       categoria[i]="51 - 60"
#     else if(Informes_realizados$Edad[i]>60)
#       categoria[i]="> 61"
#   }
# }
# table(categoria)
# Informes_realizados= data.frame(Informes_realizados, categoria)



?HoltWinters
datos=c(rep(1.7,2),rep(1.92,2),rep(2.23,2),rep(1.57,2),rep(2.14,2),rep(2.29,2))
datos=c(rep(2,2),rep(7,2),rep(1,2),rep(1,2),rep(5,2),rep(5,2))
serie=ts(datos, frequency = 2, start = 2017)
#
autoplot(decompose(serie))
#
graf=HoltWinters(serie, seasonal = "additive")
plot(graf)
#
library(forecast)
pronostico= forecast(graf, h=4)
pronostico %>% autoplot()

round(as.numeric(as.character(pronostico[4][[1]])))[c(1,3)]




  nested <- Datos_estadisticas_Bolivia %>%
    group_by(cod) %>%
    nest() %>%
    ungroup()
  nested<- nested %>% mutate(grupo=1:nrow(nested))
  nested<- nested %>% unnest(cols = data)



  cantidad=c(rep(datos$CANTIDAD[1],2),rep(datos$CANTIDAD[2],2),rep(datos$CANTIDAD[3],2),
             rep(datos$CANTIDAD[4],2),rep(datos$CANTIDAD[5],2),rep(datos$CANTIDAD[6],2))
  serie=ts(cantidad, frequency = 2, start = 2017)
  graf=HoltWinters(serie, seasonal = "additive")
  pronostico= forecast(graf, h=4)
  valores=round(as.numeric(as.character(pronostico[4][[1]])))[c(1,3)]
  adjunto=datos[1:2,]
  adjunto[1,6]=valores[1]
  adjunto[2,6]=valores[2]
  adjunto[1,5]=2023
  adjunto[2,5]=2024
  datos= rbind(datos, adjunto)

  unido= data.frame()
 for (i in 1:length(unique(nested$grupo))) {
   print(i)
   datos= nested %>% filter(grupo==i)
   cantidad=c(rep(datos$CANTIDAD[1],2),rep(datos$CANTIDAD[2],2),rep(datos$CANTIDAD[3],2),
              rep(datos$CANTIDAD[4],2),rep(datos$CANTIDAD[5],2),rep(datos$CANTIDAD[6],2))
   serie=ts(cantidad, frequency = 2, start = 2017)
   graf=HoltWinters(serie, seasonal = "additive")
   pronostico= forecast(graf, h=4)
   valores=round(as.numeric(as.character(pronostico[4][[1]])))[c(1,3)]
   adjunto=datos[1:2,]
   adjunto[1,6]=valores[1]
   adjunto[2,6]=valores[2]
   adjunto[1,5]=2023
   adjunto[2,5]=2024
   datos= rbind(datos, adjunto)
   unido= rbind(unido, datos)
 }

#############

  informes_tiempo= Informes_realizados %>% count(año, Detalle_Regional_Alta_Cliente)
  informes_tiempo$año= factor(informes_tiempo$año)
  hchart(informes_tiempo , "spline",hcaes(x=año, y=n, group=Detalle_Regional_Alta_Cliente),
         # stacking = list(enabled = TRUE),
         dataLabels = list(
           enabled = TRUE
           #, # Añadir etiquetas
           #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
           # format = "{point.percentage:.0f}%"
         )) %>% hc_add_theme(hc_theme_google())%>%
    hc_colors(c("#56147DFF","orange", "#40498EFF","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
    hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
####
######### RESUMEN ALERTAS FINDER ######
  #para resumen controlador
  datos_totales= Alertas %>% count(Nro_de_identificacion, Tipo_Persona, sort = TRUE)
  pendientes_totales= Alertas %>% filter(`Estado de la Alerta`=="Pendiente") %>% count(Nro_de_identificacion)
  names(pendientes_totales)[2]="p_totales"
  Alertas_pendientes= Alertas %>% filter((`Estado de la Alerta`=="Pendiente" | `Estado de la Alerta`=="En Proceso") & is.na(Descripcion_cierre)) %>%
    count(Nro_de_identificacion)
  names(Alertas_pendientes)[2]="pendientes_reales"
  resumen_total= datos_totales %>% left_join(pendientes_totales, by="Nro_de_identificacion")
  resumen_total= resumen_total %>% left_join(Alertas_pendientes, by="Nro_de_identificacion")
  resumen_total$p_totales[is.na(resumen_total$p_totales)]=0
  resumen_total$pendientes_reales[is.na(resumen_total$pendientes_reales)]=0
  resumen_total= resumen_total %>% arrange(desc(n))
  #
  Alertas_pendientes= Alertas %>% filter((`Estado de la Alerta`=="Pendiente" | `Estado de la Alerta`=="En Proceso") & is.na(Descripcion_cierre))
  Alertas_finalizadas= Alertas %>% filter(`Estado de la Alerta`=="Finalizada" | (`Estado de la Alerta`=="Pendiente"
                                                                                    & !is.na(Descripcion_cierre)) |
                                            (`Estado de la Alerta`=="En Proceso" & !is.na(Descripcion_cierre)))
  Alertas_pendientes= Alertas_pendientes %>% mutate(`Estado de la Alerta`="Pendiente")
  Alertas_finalizadas= Alertas_finalizadas %>% mutate(`Estado de la Alerta`="Finalizada")
  # filtrar las columnas y luego unir
  Alertas_para_resumen= rbind(Alertas_finalizadas, Alertas_pendientes)
  #### para filtrar casos para controlador ####
  load("Alertas.RData")
  load("Alertas_para_resumen.RData")
  #load("Casos_Marcados.RData")
  # Alertas_cerradas<- Alertas_para_resumen %>% filter(`Estado de la Alerta`=="Finalizada") %>% count(Nro_de_identificacion, Tipo_Documento)
  # names(Alertas_cerradas)[3]="Cerradas"
  Alertas_totales<- Alertas_para_resumen %>% count(Nro_de_identificacion, Tipo_Documento)
  names(Alertas_totales)[3]="Totales"
  #Alertas_pendientes_reales<- Alertas_para_resumen %>% filter((`Estado de la Alerta`=="Pendiente")  & Nro_de_identificacion %in% Alertas_cerradas$Nro_de_identificacion)
  Alertas_pendientes_reales<- Alertas_para_resumen %>% filter((`Estado de la Alerta`=="Pendiente"))
  Alertas_pendientes_reales<- Alertas_pendientes_reales %>% count(Nro_de_identificacion)
  names(Alertas_pendientes_reales)[2]="Pendientes"
  # datos_prueba<- Alertas_totales %>% left_join(Alertas_cerradas, by="Nro_de_identificacion")
  # datos_prueba<- datos_prueba %>% left_join(Alertas_pendientes_reales, by="Nro_de_identificacion")
  # datos_prueba$Pendientes[is.na(datos_prueba$Pendientes)]=0
  #alertas_para_cierre<- Alertas %>% filter(Nro_de_identificacion %in% Alertas_cerradas$Nro_de_identificacion &  (!is.na(Descripcion_cierre) & `Estado de la Alerta`=="Pendiente" | `Estado de la Alerta`=="En proceso"))
  alertas_para_cierre<- Alertas %>% filter((!is.na(Descripcion_cierre) & `Estado de la Alerta`=="Pendiente" | `Estado de la Alerta`=="En proceso"))

  alertas_para_cierre<- alertas_para_cierre %>% mutate(cod= paste(Nro_de_identificacion, Tipo_Documento, Fecha_de_generacion, Valor_esperado,
                                                                  Valor_operado, Excedido, REGIONAL))
  alertas_para_cierre<- alertas_para_cierre %>% group_by(cod) %>% filter(row_number()==1)
  alertas_para_cierre<- as.data.frame(alertas_para_cierre)
  alertas_para_cierre<- alertas_para_cierre %>% count(Nro_de_identificacion)
  names(alertas_para_cierre)[2]="Para_cerrar"
  datos_prueba<- Alertas_totales %>% left_join(alertas_para_cierre, by="Nro_de_identificacion")
  datos_prueba<- datos_prueba %>% left_join(Alertas_pendientes_reales, by="Nro_de_identificacion")
  datos_prueba<- datos_prueba %>% filter(!is.na(Para_cerrar) | !is.na(Pendientes))
  datos_prueba$Para_cerrar[is.na(datos_prueba$Para_cerrar)]=0
  datos_prueba$Pendientes[is.na(datos_prueba$Pendientes)]=0
  #datos_prueba<- datos_prueba %>% filter(Para_cerrar>0)
  load("D:/Documentos/Base_Informes.RData")
  datos_prueba<- datos_prueba %>% filter(!(Nro_de_identificacion %in% Base_Informes$NUMERO_DE_DOCUMENTO))
  datos_prueba<- datos_prueba %>% mutate(total_alertas=Para_cerrar+Pendientes)
  datos_prueba<- datos_prueba %>% filter(!(Nro_de_identificacion %in% c("11109080","10701521TJ","10083919","11340051", "5412305",
                                                                        "10324135", "10665852TJ","9768487SC","10800801","11262214",
                                                                        "11341894SC","10131034","12503270","12508203SC","12531930TJ",
                                                                        "12599958SC","120791021BDP","12527666","13783820SC",
                                                                        "14413564","2723486OR","1563041","3071861OR",
                                                                        "3905859SC","4255402LP","4280992","4577630","4579972SC",
                                                                        "4417659","1020441026","4884551LP","617439","6200414SC","6235648SC",
                                                                        "6353957SC","6382613SC","6245504","5852074SC","7772592SC")))
  datos_prueba<- datos_prueba[,-3]
  #### paso extra para
  Alertas_pendientes_reales<- Alertas_para_resumen %>% filter((`Estado de la Alerta`=="Pendiente"))
  alertas_para_cierre<- Alertas %>% filter((!is.na(Descripcion_cierre) & `Estado de la Alerta`=="Pendiente" | `Estado de la Alerta`=="En proceso"))

  alertas_para_cierre<- alertas_para_cierre %>% mutate(cod= paste(Nro_de_identificacion, Tipo_Documento, Fecha_de_generacion, Valor_esperado,
                                                                   Excedido, REGIONAL))
  alertas_para_cierre<- alertas_para_cierre %>% group_by(cod) %>% filter(row_number()==1)
  alertas_para_cierre<- as.data.frame(alertas_para_cierre)
  alertas_extenso<- rbind(Alertas_pendientes_reales, alertas_para_cierre[,1:29])
  alertas_extenso<- alertas_extenso %>% mutate(cod= paste(Nro_de_identificacion, Tipo_Documento, Valor_esperado, Fecha_de_generacion))
  library(readxl)
  Alertas_para_cierre <- read_excel("Alertas_para_cierre.xlsx", sheet = "Hoja1")
  Alertas_para_cierre <- Alertas_para_cierre %>% mutate(cod= paste(Nro_de_identificacion, Tipo_Persona, Valor_esperado,
                                                                   Fecha_de_generacion))
  Alertas_para_cierre_1 <- Alertas_para_cierre %>% filter(cod %in% alertas_extenso$cod)
  Alertas_para_cierre_1$usuario<- tolower(Alertas_para_cierre_1$usuario)
  library(openxlsx)
  write.xlsx(Alertas_para_cierre_1, file = "cierre alertas.xlsx")
  ###
  ###reaperturadas
  casos_para_cierre<- Alertas %>% filter(`Estado de la Alerta`=="Pendiente" & !is.na(Descripcion_cierre))
  casos_para_cierre<- casos_para_cierre %>% mutate(cod= paste(Nro_de_identificacion, Flujo_de_alerta, Detalle, Fecha_de_generacion, Valor_esperado))
  casos_para_cierre<- casos_para_cierre %>% group_by(cod) %>% filter(row_number()==1)
  casos_cierre<- casos_para_cierre %>% arrange(Nro_de_identificacion)
  # casos_cierre<- casos_cierre %>% filter(!(Nro_de_identificacion %in% c("11109080","10701521TJ","10083919","11340051", "5412305",
  #                                                                       "10324135", "10665852TJ","9768487SC","10800801","11262214",
  #                                                                       "11341894SC","10131034","12503270","12508203SC","12531930TJ",
  #                                                                       "12599958SC","120791021BDP","12527666","13783820SC",
  #                                                                       "14413564","2723486OR","1563041","3071861OR",
  #                                                                       "3905859SC","4255402LP","4280992","4577630","4579972SC",
  #                                                                       "4417659","1020441026","4884551LP","617439","6200414SC","6235648SC",
  #                                                                       "6353957SC","6382613SC","6245504","5852074SC","7772592SC")))
  casos_cierre<- casos_cierre[,c(2,23,4,8,17,18,20)]
  ### 11340051 5412305 10324135 10665852TJ
  library(openxlsx)
  write.xlsx(casos_cierre, file = "casos_cierre.xlsx")
#
  Alertas_para_resumen= Alertas_para_resumen %>% mutate(mes_generacion= as.Date(paste(substr(Fecha_de_generacion,1,7),"1",sep = "-")))
  Alertas_para_resumen= Alertas_para_resumen %>% mutate(año= as.numeric(substr(Fecha_de_generacion,1,4)))
  Alertas_para_resumen= Alertas_para_resumen %>% mutate(`Usuario finalizacion`= ifelse(is.na(`Usuario finalizacion`) &
                                                                                         `Estado de la Alerta`=="Finalizada",
                                                                                       Usuario_de_finalizacion, `Usuario finalizacion`))
  Alertas_para_resumen= Alertas_para_resumen %>% mutate(Fecha_cierre_1= as.Date(Fecha_cierre))
  ### GRAF 1
  resumen_año1= Alertas_para_resumen %>% count(mes_generacion, `Estado de la Alerta`)
  hchart(resumen_año1 , "spline",hcaes(x=mes_generacion, y=n, group=`Estado de la Alerta`),
         # stacking = list(enabled = TRUE),
         dataLabels = list(
           enabled = FALSE
           #, # Añadir etiquetas
           #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
           # format = "{point.percentage:.0f}%"
         )) %>% hc_add_theme(hc_theme_google())%>%
    hc_colors(c("#56147DFF","red","orange", "#40498EFF","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
    hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
  ## GRAF 2
  resumen_año2= Alertas_para_resumen %>% count(año, `Estado de la Alerta`)
  hchart(resumen_año2 , "column",hcaes(x=año, y=n, group=`Estado de la Alerta`),
         # stacking = list(enabled = TRUE),
         dataLabels = list(
           enabled = TRUE
           #, # Añadir etiquetas
           #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
           # format = "{point.percentage:.0f}%"
         )) %>% hc_add_theme(hc_theme_google())%>%
    hc_colors(c("orange","red"))%>%
    hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
  ## GRAF 3
  resumen_año3= Alertas_para_resumen %>% count(año, REGIONAL) %>% filter(REGIONAL!="P. Defecto")
  hchart(resumen_año3 , "column",hcaes(x=año, y=n, group=REGIONAL),
         stacking = list(enabled = TRUE),
         dataLabels = list(
           enabled = TRUE
           , # Añadir etiquetas
           #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
            format = "{point.percentage:.0f}%"
         )) %>% hc_add_theme(hc_theme_google())%>%
    hc_colors(c("#56147DFF","#40498EFF","orange", "red","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
    hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
  ### graf 4
  resumen_año4= Alertas_para_resumen %>% count(Caracteristica_2, `Estado de la Alerta`)
  resumen_año4= resumen_año4 %>% mutate(corre=1:nrow(resumen_año4))
  aux= Alertas_para_resumen %>% count(Caracteristica_2) %>% arrange(desc(n))
  aux= aux %>% mutate(ordena=1:nrow(aux))
  resumen_año4= resumen_año4 %>% left_join(aux[,c(1,3)],by="Caracteristica_2") %>% group_by(corre) %>% filter(row_number()==1)
  resumen_año4= (resumen_año4 %>% arrange(ordena))[,-4]
  hchart(resumen_año4 , "bar",hcaes(x=Caracteristica_2, y=n, group=`Estado de la Alerta`),
         #stacking = list(enabled = TRUE),
         dataLabels = list(
           enabled = TRUE
           #, # Añadir etiquetas
           #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
           #format = "{point.percentage:.0f}%"
         )) %>% hc_add_theme(hc_theme_google())%>%
    hc_colors(c("orange", "red","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
    hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
  ### graf 5
  resumen_1= Alertas_para_resumen %>% filter(`Estado de la Alerta`=="Finalizada" & `Usuario finalizacion` %in% c("epaz","maldana") ) %>%  count(mes_generacion, `Usuario finalizacion`)
  hchart(resumen_1 , "spline",hcaes(x=mes_generacion, y=n, group=`Usuario finalizacion`),
         #stacking = list(enabled = TRUE),
         dataLabels = list(
           enabled = TRUE
           #, # Añadir etiquetas
           #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
           #format = "{point.percentage:.0f}%"
         )) %>% hc_add_theme(hc_theme_google())%>%
    hc_colors(c("#56147DFF","orange", "red","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
    hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
  ## categoria cierre dias
    cant_cierre= Alertas_para_resumen %>% filter(`Estado de la Alerta`=="Finalizada" & !is.na(dias_cierre))
    cant_cierre= cant_cierre %>% mutate()

  ##
    datos_filt3= reactivo_3() %>% filter(TIPO_PERSONA=="NATURAL")
    datos_filt3=datos_filt3 %>% spread(TIPO, CANTIDAD)
    kable(datos_filt3,
          align = "c") %>% kable_styling(font_size = 7) %>%
      column_spec(c(1:3,5,6), width = "1.2cm", latex_valign = "m")


     %>%
      row_spec(0,font_size = 3) %>% column_spec(4,width = "4cm", latex_valign = "m")

    consult_filt= CONSULTA_MASIVA %>% filter(GESTION==2023 & PERIODO=="3ER TRIMESTRE")
    datos_filt3= CONSULTA_MASIVA %>% filter(GESTION==2023 & PERIODO=="3ER TRIMESTRE" & TIPO_PERSONA=="NATURAL")
    datos_filt3=datos_filt3 %>% spread(TIPO, CANTIDAD)

    aux= datos_filt3 %>% count(TIPO_PERSONA, wt=CANTIDAD)
    datos_filt3= datos_filt3 %>% left_join(aux, by="TIPO_PERSONA") %>% mutate(TIPO_PERSONA= paste(TIPO_PERSONA,n,sep = ": "))
    datos_filt3= datos_filt3[,1:6]


    datos_filt2= reactivo_2() %>% filter(Informe=="SI")
    informe_filt_serv2= datos_filt2 %>% count(REGIONAL ,DELITO_PRECEDENTE)
    informe_filt_serv2=informe_filt_serv2 %>% spread(DELITO_PRECEDENTE,n)


    datos_filt2= reactivo_2()
    informe_filt_serv2= datos_filt2 %>% count(REGIONAL ,MEDIO_DE_IDENTIFICACION_DEL_CLIENTE)
    informe_filt_serv2=informe_filt_serv2 %>% spread(MEDIO_DE_IDENTIFICACION_DEL_CLIENTE,n)
    informe_filt_serv2[is.na(informe_filt_serv2)]<-0
    kable(informe_filt_serv2, latex_valign = "m") %>% kable_styling(font_size = 5) %>% column_spec(1:ncol(informe_filt_serv2), width = "0.8cm") %>% row_spec(0, font_size = 3)
    ##
    estado_dd_1<- ESTADO_DD %>% filter(GESTION=="2023" & PERIODO=="3ER TRIMESTRE") %>%
      filter(TIPO_PERSONA=="NATURAL" & SEGMENTO=="ALTO RIESGO")
    estado_dd_1= estado_dd_1 %>% mutate(Porcentaje= paste(round(CANTIDAD/sum(CANTIDAD),3)*100,"%"))
    estado_dd_1= estado_dd_1[,5:7]
    adjunto= data.frame(ESTADO="TOTAL",CANTIDAD=sum(estado_dd_1$CANTIDAD),Porcentaje="100 %")
    estado_dd_1= rbind(estado_dd_1, adjunto)

    consult_f= Lista_interna %>% filter(GESTION==2023 & PERIODO=="3ER TRIMESTRE")
    consult_f= consult_f %>% mutate(Porcentaje= paste(round(CANTIDAD/sum(CANTIDAD),3)*100,"%"))
    consult_f= consult_f[,3:5]
    adjunto= data.frame(TIPO_PERSONA="TOTAL",CANTIDAD=sum(estado_dd_1$CANTIDAD),Porcentaje="100 %")
    consult_f= rbind(consult_f, adjunto)
    ########
    datos_filt2= Informes %>% filter(Informe=="SI")
    informe_filt_serv2= datos_filt2 %>% count(REGIONAL ,MEDIO_DE_IDENTIFICACION_DEL_CLIENTE)
    informe_filt_serv2=informe_filt_serv2 %>% filter(!is.na(REGIONAL))
    informe_filt_serv2=informe_filt_serv2 %>% spread(REGIONAL,n)
    informe_filt_serv2[is.na(informe_filt_serv2)]<-0
    columnas= ncol(informe_filt_serv2)-1
    aux=data.frame(matrix(as.numeric(unlist(lapply(informe_filt_serv2[,2:ncol(informe_filt_serv2)], sum)[1:5])), nrow = 1))
    aux= data.frame(DELITO_PRECEDENTE="TOTAL", aux)
    names(aux)[2:ncol(aux)] = names(unlist(lapply(informe_filt_serv2[,2:ncol(informe_filt_serv2)], sum)[1:5]))
    informe_filt_serv2= rbind(informe_filt_serv2, aux)

    #########
    resumen_alertas=reactivo_finder_gen()
    resumen_alertas= Alertas_para_resumen %>% filter(mes_generacion>=as.Date("2024-1-1")) %>%  count(mes_generacion, REGIONAL)
    resumen_alertas= resumen_alertas %>% spread(REGIONAL,n)
    resumen_alertas$mes_generacion= paste(months(resumen_alertas$mes_generacion, abbreviate=FALSE),"de",
                                          year(resumen_alertas$mes_generacion))
    resumen_alertas[is.na(resumen_alertas)]<-0
    aux=data.frame(matrix(as.numeric(unlist(lapply(resumen_alertas[,2:ncol(resumen_alertas)], sum)[1:ncol(resumen_alertas)])), nrow = 1))
    aux= data.frame(mes_generacion="TOTAL", aux)
    names(aux)[2:ncol(aux)] = names(unlist(lapply(resumen_alertas[,2:ncol(resumen_alertas)], sum)[1:ncol(resumen_alertas)]))
    resumen_alertas= rbind(resumen_alertas, aux)
    kable(resumen_alertas, format = "latex" ,latex_valign = "m") %>%
      kable_styling(font_size = 5,bootstrap_options = "striped", full_width = FALSE, latex_options = "hold_position") %>%
      add_header_above(header = c(" "=1,"REGIONAL"=(ncol(resumen_alertas)-1)))  %>%  row_spec(0, bold = TRUE) %>% row_spec(nrow(resumen_alertas),
                                                                                                          color = "white", bold = TRUE,
                                                                                                          background = "#40498EFF")
    ##################### tabla de tarjetas de credito ####
    "domingo, "
    "lunes, "
    "martes, "
    "miércoles, "
    "jueves, "
    "viernes, "
    "sábado, "
    SFTP_TXT= SFTP_TXT %>% mutate(Fecha_transaccion= `Fecha Transacción`)
    SFTP_TXT$Fecha_transaccion= str_replace(SFTP_TXT$Fecha_transaccion, "domingo, ","")
    SFTP_TXT$Fecha_transaccion= str_replace(SFTP_TXT$Fecha_transaccion, "lunes, ","")
    SFTP_TXT$Fecha_transaccion= str_replace(SFTP_TXT$Fecha_transaccion, "martes, ","")
    SFTP_TXT$Fecha_transaccion= str_replace(SFTP_TXT$Fecha_transaccion, "miércoles, ","")
    SFTP_TXT$Fecha_transaccion= str_replace(SFTP_TXT$Fecha_transaccion, "jueves, ","")
    SFTP_TXT$Fecha_transaccion= str_replace(SFTP_TXT$Fecha_transaccion, "viernes, ","")
    SFTP_TXT$Fecha_transaccion= str_replace(SFTP_TXT$Fecha_transaccion, "sábado, ","")
    SFTP_TXT$Fecha_transaccion= str_replace_all(SFTP_TXT$Fecha_transaccion, "de ","")
    SFTP_TXT$Fecha_transaccion= as.Date(SFTP_TXT$Fecha_transaccion, format="%d %B %Y")
    SFTP_TXT= SFTP_TXT %>% mutate(Tipo_Pais= ifelse(País=="BO",1,2))
    SFTP_TXT= SFTP_TXT %>% mutate(Tipo_Tarjeta=`Descripción PEM`)
    resumen= SFTP_TXT %>% filter(Tipo_Pais==2 & Tipo_Tarjeta=="TARJETA NO PRESENTE")
    resumen_tot= resumen %>% count(mes_transaccion, País ,`Nombre Comercio`, wt=Monto_USD)
    names(resumen_tot)[4]="Monto_USD"
    #resumen= resumen %>% mutate(mes_transaccion= as.Date(paste(substr(Fecha_transaccion,1,7),"1",sep = "-")))
    resumen1=resumen_tot %>% count(mes_transaccion, `Nombre Comercio`, wt= Monto_USD)
    # resumen2= resumen1 %>% group_by(mes_transaccion) %>% arrange(desc(n)) %>%
    #   filter(row_number() %in% 1:10) %>% filter(mes_transaccion>=as.Date("2023-11-1"))
    #
    resumen2= (resumen1 %>% filter(mes_transaccion>=as.Date("2023-11-1")) %>%
      group_by(`Nombre Comercio`) %>% summarise(total=sum(n)) %>% arrange(desc(total)))[1:10,]
    resumen_meses= resumen1 %>% filter(mes_transaccion>=as.Date("2023-11-1")) %>% filter(`Nombre Comercio` %in% resumen2$`Nombre Comercio` )
    resumen_meses$n= round(resumen_meses$n)
    resumen_meses
    #
    hchart(resumen_meses , "column",hcaes(x=mes_transaccion, y=n, group=`Nombre Comercio`),
           stacking = list(enabled = TRUE),
           dataLabels = list(
             enabled = TRUE
             #, # Añadir etiquetas
             #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
             #format = "{point.percentage:.0f}%"
           )) %>% hc_add_theme(hc_theme_google())%>%
      hc_colors(c("#56147DFF","orange", "red","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
      hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text="")) %>% hc_legend(align="right",layout = "vertical",
                                                                                    verticalAlign = "top",
                                                                                    x = 0,
                                                                                    y = 140)
    #segundo grafico
    resumen_cant= (resumen %>% filter(mes_transaccion>=as.Date("2023-11-1") & Tipo_Pais==2 & Tipo_Tarjeta=="TARJETA NO PRESENTE") %>%
                         group_by(`Nombre Comercio`) %>% summarise(total=n()) %>% arrange(desc(total)))[1:10,]
    resumen_meses1= resumen %>% filter(mes_transaccion>=as.Date("2023-11-1")) %>% filter(`Nombre Comercio` %in% resumen_cant$`Nombre Comercio` )
    #resumen_meses= resumen_meses %>% filter(Monto_USD!=0)
    resumen_meses1= resumen_meses1 %>% count(mes_transaccion, `Nombre Comercio`)
    hchart(resumen_meses1 , "column",hcaes(x=mes_transaccion, y=n, group=`Nombre Comercio`),
           stacking = list(enabled = TRUE),
           dataLabels = list(
             #enabled = TRUE
             #, # Añadir etiquetas
             #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
             #format = "{point.percentage:.0f}%"
           )) %>% hc_add_theme(hc_theme_google())%>%
      hc_colors(c("#56147DFF","orange", "red","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
      hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text="")) %>% hc_legend(align="right",layout = "vertical",
                                                                                    verticalAlign = "top",
                                                                                    x = 0,
                                                                                    y = 140) %>% hc_plotOptions(
                                                                                      series = list(
                                                                                        boderWidth = 0,
                                                                                        dataLabels = list(enabled = TRUE)
                                                                                      ))
    ##########
    persona= unido %>% filter(USUARIO=="mirian")
    #horas
    resumen_1= persona %>% filter(question_id=="horas")
    resumen_1$response= as.numeric(resumen_1$response)
    resumen_1$FECHA= as.Date(resumen_1$FECHA)
    resumen_horas= resumen_1 %>% count(FECHA, wt=response)
    resumen_actividad= resumen_1 %>% count(ACTIVIDAD, wt= response) %>% arrange(desc(n))
    resumen_act1=resumen_actividad[1:5,]
    resumen_act2=resumen_actividad[6:nrow(resumen_actividad),]
    res_serie= resumen_1 %>% filter(ACTIVIDAD %in% unique(resumen_act1$ACTIVIDAD))
    res_serie= res_serie %>% count(FECHA, ACTIVIDAD, wt=response)
    names(res_serie)[3]="response"
    res_serie %>% group_by(ACTIVIDAD) %>% summarise(promedio_general= mean(response), Mediana= median(response))
    hchart(res_serie , "column",hcaes(x=FECHA, y=response, group=ACTIVIDAD),
           stacking = list(enabled = TRUE),
           dataLabels = list(
             enabled = TRUE
             #, # Añadir etiquetas
             #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
             #format = "{point.percentage:.0f}%"
           )) %>% hc_add_theme(hc_theme_google())%>%
      hc_colors(c("#56147DFF","orange", "red","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
      hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
    # cantidad
    #horas
    resumen_1= persona %>% filter(question_id=="cantidad")
    resumen_1$response= as.numeric(resumen_1$response)
    resumen_1$FECHA= as.Date(resumen_1$FECHA)
    resumen_horas= resumen_1 %>% count(FECHA, wt=response)
    resumen_actividad= resumen_1 %>% count(ACTIVIDAD, wt= response) %>% arrange(desc(n))
    resumen_act1=resumen_actividad[1:5,]
    resumen_act2=resumen_actividad[6:nrow(resumen_actividad),]
    res_serie= resumen_1 %>% filter(ACTIVIDAD %in% unique(resumen_act1$ACTIVIDAD))
    res_serie= res_serie %>% count(FECHA, ACTIVIDAD, wt=response)
    names(res_serie)[3]="response"
    res_serie %>% group_by(ACTIVIDAD) %>% summarise(promedio_general= mean(response), Mediana= median(response))
    hchart(res_serie , "column",hcaes(x=FECHA, y=response, group=ACTIVIDAD),
           stacking = list(enabled = TRUE),
           dataLabels = list(
             enabled = TRUE
             #, # Añadir etiquetas
             #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
             #format = "{point.percentage:.0f}%"
           )) %>% hc_add_theme(hc_theme_google())%>%
      hc_colors(c("#56147DFF","orange", "red","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
      hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
    ####
######### SELENIUM ##########
    library(RSelenium)
    library(wdman)
    library(netstat)

    selenium()
    objeto_selenium<- selenium(retcommand =T, check = F)
    #
    binman::list_versions("chromedriver")
    remote_driver<- rsDriver(browser = "chrome", chromever = "130.0.6723.116",
                             verbose = F)
    # CONEXION A FINDER
    remDr<-   remote_driver$client
    #remDr$open()
    remDr$navigate("https://vfindersrv/Account/Login")
    #
    objeto_nombre<- remDr$findElement(using = 'id', 'usernameinput')
    objeto_nombre$sendKeysToElement(list('dmamaniq'))
    #
    objeto_pwd<- remDr$findElement(using = 'id', 'passwordinput')
    objeto_pwd$sendKeysToElement(list('Estadistico671&$&&&0+'))
    #
    objeto_wind<- remDr$findElement(using = 'xpath','/html/body/div/div[1]/div[4]/div[1]/label')
    objeto_wind$clickElement()
    #
    ibj_iniciar<- remDr$findElement(using = 'xpath', '/html/body/div/div[1]/button')
    ibj_iniciar$clickElement()
    #colocar lapso de 3 segundos
    #dentro del finder las 3lineas para busqueda
    ibj_treslin<- remDr$findElement(using = 'xpath', '//*[@id="btn-menu-default"]')
    ibj_treslin$clickElement()
    #
    ibj_ent<- remDr$findElement(using = 'id', 'AccesoEntidad')
    ibj_ent$clickElement()
###### el siguiente para actualizar la pagina de busqueda ####
    remDr$refresh()
    #ibj_num_ent$refresh()
    ibj_click<- remDr$findElement(using = 'xpath', '//*[@id="div-ci-rut"]')
    ibj_click$clickElement()
    # 1seg
    ibj_click1<- remDr$findElement(using = 'xpath', '//*[@id="dropdownMenuButton"]')
    ibj_click1$clickElement()
    # 1 seg
    ibj_click2<- remDr$findElement(using = 'xpath', '//*[@id="1"]')
    ibj_click2$clickElement()
    # 2 seg
    ibj_num_ent<- remDr$findElement(using = 'id', 'txtEntidadFidner')
    ibj_num_ent$sendKeysToElement(list('12631585SC', key='enter'))
    # 8 segundos
    ibj_click3<- remDr$findElement(using = 'xpath', '//*[@id="nav-Alertas-tab"]')
    ibj_click3$clickElement()
    #
    ibj_click4<- remDr$findElement(using = 'xpath', '//*[@id="collapse-aviso"]/div[1]/div[2]/div/button')
    ibj_click4$clickElement()
    #
    ibj_click5<- remDr$findElement(using = 'xpath', '//*[@id="collapse-aviso"]/div[1]/div[2]/div/div/div/ul/li[1]/a')
    ibj_click5$clickElement()
    #
    ibj_click51<- remDr$findElement(using = 'xpath', '//*[@id="collapse-aviso"]/div[1]/div[5]/div/button')
    ibj_click51$clickElement()
    ibj_click52<- remDr$findElement(using = 'xpath', '//*[@id="collapse-aviso"]/div[1]/div[5]/div/div/div/ul/li[2]/a/span[2]')
    ibj_click52$clickElement()
    ### para la 1ra alerta
    ibj_click8<- NULL
    ibj_click8<- remDr$findElement(using = 'xpath', value = '//*[@id="content-alerta"]/div/table/tbody/tr[1]')
    if(!is.null(ibj_click8)){
      ibj_click8$clickElement()
      valor=remDr$findElement(using = 'xpath', value = '//*[@id="content-observaciones-alerta"]')
      valor=unlist(valor$getElementText())
      if(valor!="No existen observaciones."){
        ibj_click_proc<- remDr$findElement(using = 'xpath', value = '//*[@id="btn-procesar-pendiente"]')
        ibj_click_proc$clickElement()
        #2segundos de pausa
        ibj_click_final<- remDr$findElement(using = 'xpath', value = '//*[@id="btn-finalizar"]')
        ibj_click_final$clickElement()
      }
    }
    ### para la 2da alerta
    ibj_click81=NULL
    ibj_click81<- remDr$findElement(using = 'xpath', value = '//*[@id="content-alerta"]/div/table/tbody/tr[2]')
    if(!is.null(ibj_click81)){
      ibj_click81$clickElement()
      valor=remDr$findElement(using = 'xpath', value = '//*[@id="content-observaciones-alerta"]')
      valor=unlist(valor$getElementText())
      if(valor!="No existen observaciones."){
        ibj_click_proc<- remDr$findElement(using = 'xpath', value = '//*[@id="btn-procesar-pendiente"]')
        ibj_click_proc$clickElement()
        #2segundos de pausa
        ibj_click_final<- remDr$findElement(using = 'xpath', value = '//*[@id="btn-finalizar"]')
        ibj_click_final$clickElement()
      }
    }
    ### para la 3ra alerta
    ibj_click82=NULL
    ibj_click82<- remDr$findElement(using = 'xpath', value = '//*[@id="content-alerta"]/div/table/tbody/tr[3]')
    if(!is.null(ibj_click82)){
      ibj_click82$clickElement()
      valor=remDr$findElement(using = 'xpath', value = '//*[@id="content-observaciones-alerta"]')
      valor=unlist(valor$getElementText())
      if(valor!="No existen observaciones."){
        ibj_click_proc<- remDr$findElement(using = 'xpath', value = '//*[@id="btn-procesar-pendiente"]')
        ibj_click_proc$clickElement()
        #2segundos de pausa
        ibj_click_final<- remDr$findElement(using = 'xpath', value = '//*[@id="btn-finalizar"]')
        ibj_click_final$clickElement()
      }
    }

    ######
     c("9782445SC","9784646SC","9789239SC","9594256SC","9636721SC","9000065SC",
       "8836319CB","7690556SC")
    # para cerrarlo
    remDr$close()
    ##
    ibj_click_obs<- remDr$findElement(using = 'xpath', value = '//*[@id="content-observaciones-alerta"]')
    ibj_click_obs$getElementText()
    unlist(ibj_click_obs$getElementText())=="No existen observaciones."
    //*[@id="content-observaciones-alerta"]
    //*[@id="content-alerta"]
    #
    //*[@id="content-alerta"]/div/table/tbody/tr[1]
    #
    //*[@id="content-alerta"]/div/table/tbody/tr[1]
    //*[@id="content-alerta"]/div/table/tbody/tr[2]
    //*[@id="content-alerta"]/div/table/tbody/tr[3]
    ibj_click$refresh()
    # encontrar elementos
    objeto_electronics<- remDr$findElement(using = 'link text', 'Tecnología')
    objeto_electronics$getElementAttribute('href')
    objeto_electronics$clickElement()
    # ir atras
    remDr$goBack()
    # para buscar
    caja_buscador<- remDr$findElement(using = 'id', 'gh-ac')
    caja_buscador$sendKeysToElement(list('play station 5', key='enter'))
    # click check box 1ra forma copiando el xpath del navegador
    us_check<- remDr$findElement(using = 'xpath','//*[@id="x-refine__group_1__1"]/ul/li[1]/div/a/div/span/input')
    # click check box 2daa forma
    us_check<- remDr$findElement(using = 'xpath','//input[@aria-label="Estados Unidos"]')
    us_check$getElementAttribute('aria-label')
    us_check$clickElement()
    ### SEGUNDA PRUEBA CON LA EVALUACION
    objeto_nombre<- remDr$findElement(using = 'id', 'nombre')
    objeto_nombre$sendKeysToElement(list('Mamani Quispe Deyvis omar', key='enter'))
    #
    objeto_ci<- remDr$findElement(using = 'id', 'num_doc')
    objeto_ci$sendKeysToElement(list('8321157', key='enter'))
    #
    objeto_edad<- remDr$findElement(using = 'id', 'edad')
    objeto_edad$sendKeysToElement(list('32'))
    #
    objeto_cargo<-remDr$findElement(using = 'xpath', '//*[@id="cargo-question"]/div/div/div/div/div[1]')
    objeto_cargo$clickElement()
    #
    objeto_cargo1<-remDr$findElement(using = 'xpath', '//*[@id="cargo-question"]/div/div/div/div/div[2]')
    objeto_cargo1$clickElement()
    # bton envio
    objeto_sig1<-remDr$findElement(using = 'xpath', '//*[@id="next-1"]')
    objeto_sig1$clickElement()
    # para cerrarlo
    remote_driver$server$stop()
    ####
    ### TERCERA PRUEBA CON LA EVALUACION
    remDr<-   remote_driver$client
    remDr$open()
    remDr$navigate("https://bancosol.shinyapps.io/BuscadorBDN/")
    objeto_nombre<- remDr$findElement(using = 'xpath', '//*[@id="auth-user_id"]')
    objeto_nombre$sendKeysToElement(list('deyvis'))
    #
    objeto_cont<- remDr$findElement(using = 'xpath', '//*[@id="auth-user_pwd"]')
    objeto_cont$sendKeysToElement(list('Est&$&00'))
    #
    objeto_enter<-remDr$findElement(using = 'xpath', '//*[@id="auth-go_auth"]')
    objeto_enter$clickElement()

    objeto_edad<- remDr$findElement(using = 'id', 'edad')
    objeto_edad$sendKeysToElement(list('32'))
    #
    objeto_cargo<-remDr$findElement(using = 'xpath', '//*[@id="cargo-question"]/div/div/div/div/div[1]')
    objeto_cargo$clickElement()
    #
    objeto_cargo1<-remDr$findElement(using = 'xpath', '//*[@id="cargo-question"]/div/div/div/div/div[2]')
    objeto_cargo1$clickElement()
    # bton envio
    objeto_sig1<-remDr$findElement(using = 'xpath', '//*[@id="next-1"]')
    objeto_sig1$clickElement()
    # para cerrarlo
    remote_driver$server$stop()

   # unido= data.frame()
    unido= rbind(unido, persona)
######## PARA EL ERROR DE INSTALACION DE LIBRERIA #####
    options("install.lock"=FALSE)
######### SELENIUM ultima ##########
    library(RSelenium)
    library(wdman)
    library(netstat)
    #selenium()
    objeto_selenium<- selenium(retcommand =T, check = F)
    #
    binman::list_versions("chromedriver")
    remote_driver<- rsDriver(browser = "chrome", chromever = "130.0.6723.116",
                             verbose = F)
    # CONEXION A FINDER
    remDr<-   remote_driver$client
    #remDr$open()
    remDr$navigate("https://ahrefs.com/es/writing-tools/paragraph-generator")




    resumen= datos1 %>% count(RESPUESTA,var6)
    hchart(resumen , "bar",hcaes(x=RESPUESTA, y=n, group=var6),
           # stacking = list(enabled = TRUE),
           dataLabels = list(
             enabled = TRUE
             #, # Añadir etiquetas
             #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
             # format = "{point.percentage:.0f}%"
           )) %>% hc_add_theme(hc_theme_google()) %>%
      hc_colors(c("#56147DFF","orange", "#40498EFF","#C73D73FF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
      hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))



    df_cat <- dplyr::tribble(
      ~x, ~y,
      "A", 10,
      "B", 23,
      "C", 7,
      "D", 16
    )
    hchart(
      df_cat,
      name = "Barras", # Nombre de la serie
      type = "bar", # Tipo de gráfico
      hcaes(x = x, y = y) # Mapeado de variables
    ) %>%
      hc_title(text = "<b>Gráfico de barras</b>") %>%  # Título
      hc_subtitle(text = "<i>Orden por defecto</i>")  %>%  # Subtítulo
      hc_credits(enabled = TRUE, text = "http://elartedeldato.com")
########
    datos_res= pcc_Base %>% filter(Conclusion=="Observado")
    datos_res= datos_res %>% count(gestion, Regional, sort = TRUE)
    hchart(datos_res , "column",hcaes(x=gestion, y=n, group=Regional),
           # stacking = list(enabled = TRUE),
           dataLabels = list(
             enabled = TRUE
             #, # Añadir etiquetas
             #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
             # format = "{point.percentage:.0f}%"
           )) %>% hc_add_theme(hc_theme_google()) %>% hc_title(text="Errores en el origen y destino de los fondos") %>%
      hc_colors(c("#C73D73FF","orange","#56147DFF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
      hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
    ##
      datos_res_1= pcc_Base %>% filter(Conclusion=="Observado")
      datos_res_1= datos_res_1 %>% count(NombreAgencia, sort = TRUE)
      aux= datos_res_1 %>% filter(n==1)
      aux= data.frame(NombreAgencia="Otros",n=sum(aux$n))
      datos_res_1= datos_res_1 %>% filter(n>1)
      datos_res_1= rbind(datos_res_1, aux)
      datos_res_1= datos_res_1 %>% mutate(PCC="Error formulario PCC")
      hchart(datos_res_1 , "bar",hcaes(x=NombreAgencia, y=n, group=PCC),
             # stacking = list(enabled = TRUE),
             dataLabels = list(
               enabled = TRUE
               #, # Añadir etiquetas
               #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
               # format = "{point.percentage:.0f}%"
             )) %>% hc_add_theme(hc_theme_google()) %>%
      hc_colors(c("#56147DFF","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
        hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text="Agencia")) %>% hc_title(text="Errores en el origen y destino de los fondos")






      datos= data.frame()
      unido= PCC_2023_2024_v2 %>% filter(etipo_8==TRUE) %>% count(Fecha,Regional,Nombre_Agencia,etipo_8)
      names(unido)[4]="Error"
      unido$Error[unido$Error==TRUE]="Tipo 8"
      datos= rbind(datos, unido)
#######
      datos= errores_pcc %>% count(Regional,Error, wt=n)
      datos$Error=factor(datos$Error, levels = c("Tipo 2", "Tipo 3", "Tipo 4", "Tipo 5", "Tipo 6", "Tipo 7", "Tipo 8"))
      hchart(datos , "bar",hcaes(x=Error, y=n, group=Regional),
             # stacking = list(enabled = TRUE),
             dataLabels = list(
               enabled = TRUE
               #, # Añadir etiquetas
               #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
               # format = "{point.percentage:.0f}%"
             )) %>% hc_add_theme(hc_theme_google()) %>%
        hc_colors(c("#56147DFF","orange","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
        hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text="cantidad")) %>% hc_title(text="Recuento de errores por tipo")
###
      datos_dona= pcc_Base %>% filter(Conclusion=="ok" & etipo_8==TRUE)
      datos_dona= datos_dona %>% count(Regional)
      datos_dona= data.frame(datos_dona, Ident="Declaracion de origen y destino iguales")
      datos_dona= datos_dona %>% mutate(Regional= paste(Regional,n,sep = ": "))
      hchart(datos_dona , "pie",innerSize = 200, hcaes(x = Regional, y = n, group=Ident),
             # stacking = list(enabled = TRUE),
             dataLabels = list(
               enabled = TRUE
               , # Añadir etiquetas
               #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
                format = "{point.percentage:.1f}%"
             )) %>% hc_add_theme(hc_theme_google())%>%
        hc_colors(c("#C73D73FF","orange","#56147DFF","red","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
        hc_title(text="Origen y destino iguales")
###
      datos_us=usuarios_capacitacion %>% count(gestion, NOMBRE_REGIONAL, sort = T)
      hchart(datos_us , "bar",hcaes(x=gestion, y=n, group=NOMBRE_REGIONAL),
             # stacking = list(enabled = TRUE),
             dataLabels = list(
               enabled = TRUE
               #, # Añadir etiquetas
               #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
               # format = "{point.percentage:.0f}%"
             )) %>% hc_add_theme(hc_theme_google()) %>%
        hc_colors(c("#56147DFF","orange","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
        hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text="cantidad")) %>% hc_title(text="Recuento de errores llenado de información")

############
      library(magick)
      imagen= image_read("C:/Users/DEYVIS OMAR/Downloads/pixelcut.png")
      class(imagen)
      print(imagen)
      #
      prueba<- image_normalize(imagen)
      image_contrast(imagen)
       image_noise(imagen) %>% image_enhance()
      #
       prueba<-image_flatten(prueba, 'Modulate')

      prueba<- prueba %>% image_convert(type = "grayscale")
       cat(image_ocr(prueba))








       highchart(type = "stock") |>
         hc_add_series(AirPassengers) |>
         hc_rangeSelector(
           verticalAlign = "bottom",
           selected = 4
         )






       hchart(datos_us1 , "column",hcaes(x=NOMBRE_AGENCIA, y=n, group=gestion),
              #stacking = list(enabled = TRUE),
              dataLabels = list(
                enabled = TRUE
                #, # Añadir etiquetas
                #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
                # format = "{point.percentage:.0f}%"
              )) %>% hc_add_theme(hc_theme_google()) %>% hc_scrollbar(enabled = TRUE)


       div(style='width:200px;overflow-x: scroll;height:200px;overflow-y: scroll;',
           hchart(datos_us1 , "column",hcaes(x=NOMBRE_AGENCIA, y=n, group=gestion),
                  #stacking = list(enabled = TRUE),
                  dataLabels = list(
                    enabled = TRUE
                    #, # Añadir etiquetas
                    #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
                    # format = "{point.percentage:.0f}%"
                  )) %>% hc_add_theme(hc_theme_google()) %>% hc_scrollbar(enabled = TRUE))

######### DRILL DOWN ###########
       library(highcharter)
       library(purrr)

       df <- tibble(
         name = c("Animals", "Fruits"),
         y = c(5, 2),
         drilldown = tolower(name)
       )

       datos_us1=usuarios_capacitacion %>% count(NOMBRE_REGIONAL, sort = T) %>% mutate(drilldown= tolower(NOMBRE_REGIONAL))
       #
       df_down<-data.frame(
         tree= c(rep(df$name[1],5), rep(df$name[2],2)),
         name= c(c("Cats", "Dogs", "Cows", "Sheep", "Pigs"),c("Apple", "Organes")),
         value=c(c(4, 3, 1, 2, 1),c(4, 2))
       )
       df_down<- df_down %>% group_nest(id=tree) %>% mutate(type = "bar",
                                                       data = map(data, mutate, name = name, y = value),
                                                       data = map(data, list_parse))

       #
       colores<-c( "#1A102AFF", "#33183CFF", "#4C1D4BFF", "#681F55FF", "#841E5AFF", "#A11A5BFF", "#BD1655FF",
         "#D62449FF", "#E83F3FFF", "#F06043FF", "#F47F58FF")
       #
       df_down<- usuarios_capacitacion %>% count(NOMBRE_REGIONAL, NOMBRE_AGENCIA) %>% arrange(NOMBRE_REGIONAL,desc(n))
       df_down<-data.frame(df_down, color=sample(colores, size = nrow(df_down), replace = T ))
       df_down<- df_down %>% group_nest(id=NOMBRE_REGIONAL) %>% mutate(type = "bar", name="Cantidad",
                                                            data = map(data, mutate, name =NOMBRE_AGENCIA , y = n),
                                                            data = map(data, list_parse))
       ##
       hchart(
         datos_us1,
         "column",
         hcaes(x=NOMBRE_REGIONAL, y=n , drilldown = NOMBRE_REGIONAL)
         , name="Cantidad",
         colorByPoint = TRUE,
         dataLabels = list(
           enabled = TRUE,
           style= list(color="red")
           #, # Añadir etiquetas
           #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
           # format = "{point.percentage:.0f}%"
         )
       ) %>% hc_drilldown(
         breadcrumbs = list(
           format = 'back to {level.name} series',
           # enabled = FALSE,
           showFullPath = TRUE
         ),
         allowPointDrilldown = TRUE,
         series = list_parse(df_down)
       )%>% hc_add_theme(hc_theme_google()) %>%
         hc_colors(c("#FA7D5EFF","orange","#56147DFF", "#AF1801FF", "red","#7A0403FF","#FCECAEFF","#02020DFF"))%>%
         hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text="cantidad")) %>%
         hc_title(text="Recuento de errores llenado de información") %>%
         hc_yAxis(title = list(text  = "")) %>%
         hc_xAxis(title = list(text  = ""))

### ejemplo 2 drill down
       dtrees <- tibble(
         tree = c("A", "B"),
         apples = c(5, 7),
         species = c("Fuji", "Gala"),
         trunk_size = c(30, 40)
       ) |>
         # rowise is used to avoid vectorization in tags$td, ie, do it row by row
         rowwise() |>
         mutate(
           tooltip_text = list(
             tags$table(
               tags$tr(tags$th("Tree"), tags$td(tree)),
               tags$tr(tags$th("# Apples"), tags$td(apples))
             )
           )
         ) |>
         ungroup() |>
         mutate(
           tooltip_text = map_chr(tooltip_text, as.character),
           # clean text
           tooltip_text = str_trim(str_squish(tooltip_text))
         )

       dflowers <- tibble(
         tree = c(rep("A", 3), rep("B", 4)),
         rose = c("R1", "R2", "R3", "R4", "R5", "R6", "R7"),
         petals = c(10, 13, 15, 20, 24, 26, 27),
         color = c(
           "gray",
           "#FFB6C1",
           "#8B0000",
           "purple",
           "#FF10F0",
           "#ffffbf",
           "red"
         ),
         price = c(3, 2, 4, 3.5, 5, 2.5, 4.5)
       ) |>
         rowwise() |>
         mutate(
           tooltip_text = list(
             tags$table(
               tags$tr(tags$th("Flower"), tags$td(rose)),
               tags$tr(tags$th("# Petals"), tags$td(petals)),
               tags$tr(tags$th("Price"), tags$td(str_c("$ ", price)))
             )
           )
         ) |>
         ungroup() |>
         mutate(
           tooltip_text = map_chr(tooltip_text, as.character),
           # clean text
           tooltip_text = str_trim(str_squish(tooltip_text))
         )

       dflowers_dd <- dflowers |>
         group_nest(id = tree) |>
         mutate(
           type = "bar",
           data = map(data, mutate, name = rose, y = petals),
           data = map(data, list_parse),
           name = "Petals"
         )

       hchart(
         dtrees,
         "column",
         hcaes(tree, apples, drilldown = tree),
         name = "Apples",
         colorByPoint = TRUE
       ) |>
         hc_drilldown(
           breadcrumbs = list(
             format = 'back to {level.name} series',
             # enabled = FALSE,
             showFullPath = TRUE
           ),
           allowPointDrilldown = TRUE,
           series = list_parse(dflowers_dd)
         ) |>
         hc_yAxis(title = list(text  = "")) |>
         hc_xAxis(title = list(text  = "")) |>
         hc_tooltip(
           headerFormat = "", # remove header
           pointFormat = "{point.tooltip_text}",
           useHTML = TRUE
         )

    ######

       library(kableExtra)
       kbl(head(iris,10))
       ##
       resultado<- data.frame(Argentina= reactivo_1()$ronda_1, Vs="",  Canadá= reactivo_1()$ronda_2)
       kable(resultado, align = 'c') %>%
         kable_styling(bootstrap_options = "striped", full_width = F, position = "center") %>%
         add_header_above(c('jueves 20 de junio, 20:00, G-A'=3), font_size = 'x-small') %>%
         add_header_above(c('1ª Ronda - Fase de grupos'=3))
      ####
       if(input$agencia_pcc=="Total"){
         datos_us1=pcc_Base %>% filter(Conclusion=="Observado") %>% count(Regional, sort = T) %>% mutate(drilldown= tolower(Regional))
         df_down<- pcc_Base %>% filter(Conclusion=="Observado") %>% count(Regional, NombreAgencia) %>% arrange(Regional,desc(n))
       }
       else{
         datos_us1=pcc_Base %>% filter(Conclusion=="Observado") %>% filter(gestion==input$agencia_pcc) %>% count(Regional, sort = T) %>% mutate(drilldown= tolower(Regional))
         df_down<- pcc_Base %>% filter(Conclusion=="Observado") %>% filter(gestion==input$agencia_pcc) %>% count(Regional, NombreAgencia) %>% arrange(Regional,desc(n))
       }
       #
       colores<-c( "#1A102AFF", "#33183CFF", "#4C1D4BFF", "#681F55FF", "#841E5AFF", "#A11A5BFF", "#BD1655FF",
                   "#D62449FF", "#E83F3FFF", "#F06043FF", "#F47F58FF")
       datos_us1<- DATOS_CONOC_BASICOS %>% count(REGIONAL, sort = T) %>% mutate(drilldown= tolower(REGIONAL))
       df_down <- DATOS_CONOC_BASICOS %>% count(REGIONAL, OFICINA) %>% arrange(REGIONAL,desc(n))
       df_down<-data.frame(df_down, color=sample(colores, size = nrow(df_down), replace = T ))
       #
       df_down<- df_down %>% group_nest(id=REGIONAL) %>% mutate(type = "pie", name="Cantidad",
                                                                data = map(data, mutate, name =OFICINA , y = n),
                                                                data = map(data, list_parse))
       hchart(
         datos_us1,
          "pie",innerSize = 200,
         hcaes(x=REGIONAL, y=n , drilldown = REGIONAL)
         , name="Cantidad",
         colorByPoint = TRUE,
         dataLabels = list(
           enabled = TRUE
           #, # Añadir etiquetas
           #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
           # format = "{point.percentage:.0f}%"
         )
       ) %>% hc_drilldown(
         breadcrumbs = list(
           format = 'volver {level.name} series',
           # enabled = FALSE,
           showFullPath = TRUE
         ),
         allowPointDrilldown = TRUE,
         series = list_parse(df_down)
       )%>% hc_add_theme(hc_theme_google()) %>%
         hc_colors(c("#FA7D5EFF","orange","#56147DFF", "#AF1801FF", "red","#7A0403FF","#FCECAEFF","#02020DFF"))%>%
         hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text="cantidad")) %>%
         hc_title(text="Recuento de errores llenado de información",
                  style = list(fontSize='35px')
         ) %>%
         hc_yAxis(title = list(text  = "")) %>%
         hc_xAxis(title = list(text  = ""))


       datos= POSTULANTES %>% filter(OBSERVACIONES=="APROBADO")
       datos1= datos %>% mutate(Instancia= ifelse(PRIMERA_INSTANCIA>=80,"PRIMERA_INSTANCIA",
                                          ifelse(SEGUNDA_INSTANCIA>=80,"SEGUNDA_INSTANCIA",
                                                 ifelse(TERCERA_INSTANCIA>=80,"TERCERA_INSTANCIA",
                                                        ifelse(`CUARTA_INSTANCIA__(EXCEPCIONAL)`>=80,"CUARTA_INSTANCIA")))))




       REGIONAL=NULL
       for (i in 1:nrow(ACTUALIZACION)) {
         if(ACTUALIZACION$cod_agencia[i] >=100 &  ACTUALIZACION$cod_agencia[i] <=109)
           REGIONAL[i]="SUR"
         else if(ACTUALIZACION$cod_agencia[i] >=200 &  ACTUALIZACION$cod_agencia[i] <=107)

       }





       dffru <- data.frame(
         name = c("Apple", "Organes"),
         value = c(4, 2)
       )
       list_parse2(dffru)

      ######
       datos_fil<- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WFQQUlbAihYROjOyu_IpP1mZfb8Lf38pYpEzga9d_-4/edit?usp=sharing",
                                         col_types = "cciiDDiiiiiii", sheet = 2)

       datos<- datos_fil %>% mutate(fechas= paste(FECHA_INICIO, "hasta", FECHA_FINAL))
       #
       datos_res<- datos %>% group_by(FUNCIONARIO, fechas) %>% summarise(Total_porcentaje= sum(PORCENTAJE_ASIGNADO))
       datos_res_1<- datos %>% group_by(FUNCIONARIO,
                                        DESCRIPCION_DE_LA_ACTIVIDAD, fechas) %>% summarise(PORCENTAJE_ASIGNADO= sum(PORCENTAJE_ASIGNADO),
                                                                                           HORAS_DISPONIBLES= as.numeric(unique(HORAS_DISPONIBLES)),
                                                                                           HORAS_ASIGNADAS=as.numeric(unique(HORAS_ASIGNADAS)))
       datos_pers<- datos_res_1 %>% filter(FUNCIONARIO==unique(datos_res_1$FUNCIONARIO)[1])
       datos_pers_1<- datos_pers[1,]
       datos_pers_1[1,6]<- ifelse( sum(datos_pers$PORCENTAJE_ASIGNADO)<100, datos_pers_1$HORAS_DISPONIBLES[1]-sum(datos_pers$HORAS_ASIGNADAS),0 )
       datos_pers_1[1,4]<- ifelse( sum(datos_pers$PORCENTAJE_ASIGNADO)<100, 100-sum(datos_pers$HORAS_ASIGNADAS),0 )
       datos_pers_1$DESCRIPCION_DE_LA_ACTIVIDAD="Disponible"
       datos_pers<- rbind(datos_pers,datos_pers_1)
       datos_pers$DESCRIPCION_DE_LA_ACTIVIDAD=factor(unique(datos_pers$DESCRIPCION_DE_LA_ACTIVIDAD), levels = unique(datos_pers$DESCRIPCION_DE_LA_ACTIVIDAD))



       colores<- c(sample(c("#FA7D5EFF","orange","#56147DFF", "#AF1801FF", "red","#7A0403FF","#FCECAEFF","#02020DFF"), (nrow(datos_pers)-1), replace = T),"green")
       datos_pers<- data.frame(datos_pers, colores)
       hchart(datos_pers, "bar" ,hcaes(x=fechas, y=HORAS_DISPONIBLES, group=DESCRIPCION_DE_LA_ACTIVIDAD),
              stacking = list(enabled = TRUE),
              dataLabels = list(
                enabled = TRUE
                , # Añadir etiquetas
                format = "{point.percentage:.0f}%"
              ))%>% hc_add_theme(hc_theme_google(chart = list(backgroundColor = '#e1ffea'))) %>% hc_colors(datos_pers$colores) %>%
         hc_title(text=paste("")) %>% hc_size(height = 200)%>% hc_yAxis(title=list(text=""))%>% hc_xAxis(title=list(text=""))
      #####################

       datos_1<-datos_pers_v %>% gather(key = "grupo", value = "cant",c(CANTIDAD_ESTIMADA_TOTAL, CANTIDAD_ESTIMADA_A_LA_FECHA, CONCLUIDO))

       load("Reporte_2023_06.RData")
       rm(Reporte_2023_05)
       #unido= data.frame() "ALTA DE PRESTAMOS MDA. WFLOW"
       datos_prestamos<- Reporte_2023_06 %>% filter(Descripcion=="Alta de Préstamos Mda. Wflow")
       datos_prestamos<- Reporte_2023_06 %>% filter(Descripcion=="ALTA DE PRESTAMOS MDA. WFLOW")
       datos_prestamos_resumen<- datos_prestamos %>% count(Mes="Mayo-2023",Cantidad=n(), Cantidad_Personas= length(unique(CI_TITULAR)),
                                                           minimo= min(Importe_Depositos), maximo= max(Importe_Depositos),
                                                           promedio_desembolso= mean(Importe_Depositos))
       unido= rbind(unido, datos_prestamos_resumen)

       #####
       info= as.character((Agencias_Finder %>% filter(IdAgencia==101))[1,2])
       for (i in 1:nrow(Informes)) {
         if(!is.na(as.numeric(Informes$AGENCIA[i])))
         Informes$AGENCIA[i]= as.character((Agencias_Finder %>% filter(IdAgencia==Informes$AGENCIA[i]))[1,2])
       }

############# pagina para resumir parrafos nchar <2200 ##########
      # https://ahrefs.com/es/writing-tools/summarizer

       persona= Alertas_para_resumen %>% filter(Nro_de_identificacion %in% datos_prueba$Nro_de_identificacion & `Estado de la Alerta`=="Finalizada")
       persona$Descripcion_cierre= toupper(persona$Descripcion_cierre)
       persona<- persona %>% group_by(Nro_de_identificacion) %>% summarise(Descripcion_cierre_total= list(unique(Descripcion_cierre)), total=n(),
                                                                           unicos=length(unique(Descripcion_cierre)))
       persona$Descripcion_cierre_total= as.character(persona$Descripcion_cierre_total)
       persona$Descripcion_cierre_total= str_replace_all(persona$Descripcion_cierre_total,"c","")
       persona$Descripcion_cierre_total= str_replace_all(persona$Descripcion_cierre_total,"\\(","")
       persona$Descripcion_cierre_total= str_replace_all(persona$Descripcion_cierre_total,"\\)","")
       persona$Descripcion_cierre_total= str_replace_all(persona$Descripcion_cierre_total,"\"","")
       persona$Descripcion_cierre_total<- str_replace_all(persona$Descripcion_cierre_total,"\n","")
       persona$Descripcion_cierre_total<- str_replace_all(persona$Descripcion_cierre_total,"\\n","")
       persona$Descripcion_cierre_total<- str_replace_all(persona$Descripcion_cierre_total,"n","")
       persona$Descripcion_cierre_total<- str_replace_all(persona$Descripcion_cierre_total,"[[:punct:]]","")
       persona$Descripcion_cierre_total[1]



       ############ resumen de parrafos SELENIUM #########
       library(RSelenium)
       library(wdman)
       library(netstat)

       #selenium()
       objeto_selenium<- selenium(retcommand =T, check = F)
       #
       binman::list_versions("chromedriver")
       remote_driver<- rsDriver(browser = "chrome", chromever = "130.0.6723.91",
                                verbose = F)
       # CONEXION A FINDER
       remDr<-   remote_driver$client
       #remDr$open()
       remDr$navigate("https://ahrefs.com/es/writing-tools/summarizer")

       # objeto_nombre<- remDr$findElement(using = 'xpath', '//*[@id="root"]/div[1]/section[1]/div/div/div/div[2]/div[1]/section/div/div/form/div/div/div[2]/div/div/textarea')
       # objeto_nombre$sendKeysToElement(list('dmamaniq'))
       # objeto_wind<- remDr$findElement(using = 'xpath','//*[@id="root"]/div[1]/section[1]/div/div/div/div[2]/div[1]/section/div/div/form/div/div/div[3]/button')
       # objeto_wind$clickElement()
       #
       # objeto_pwd<- remDr$findElement(using = 'id', 'content')
       for (i in 1:nrow(arch_ubicaciones)) {
         if(arch_ubicaciones$Departamento[i]=="COCHABAMBA" | arch_ubicaciones$Departamento[i]=="LA PAZ" | arch_ubicaciones$Departamento[i]=="SANTA CRUZ")
         {arch_ubicaciones$Calificacion_Riesgo[i]=3
         arch_ubicaciones$Calificacion_Riesgo_1[i]="ALTO RIESGO"}

           else if(arch_ubicaciones$Departamento[i]=="BENI" | arch_ubicaciones$Departamento[i]=="TARIJA")
           {arch_ubicaciones$Calificacion_Riesgo[i]=2
           arch_ubicaciones$Calificacion_Riesgo_1[i]="MEDIO RIESGO"
             }
             else{arch_ubicaciones$Calificacion_Riesgo[i]=1
             arch_ubicaciones$Calificacion_Riesgo_1[i]="BAJO RIESGO"
             }
       }
  #################################
       B_chile<-arrow::read_parquet('D:/Documentos/19 Giros y Remensas/BCHILE_PAGADO_ABONO.parquet')
  #########



       `r reactivo_rmark()[cont,12]`





       persona= Alertas_rev %>% filter(mes_generacion==as.Date("2024-7-1") &
                                         Caracteristica_1=="Cantidad de Operaciones Mensuales" &
                                         Valor_esperado=="40" & `Descripcion Ocupacion`=="Trabajador independiente      ")
       ################################
       resumen<- Alertas %>% group_by(Caracteristica_2,
                                      Caracteristica_1, Valor_esperado) %>% summarise(Total= n(),
                                                                                       ROS= sum(Inus),
                                                                                       Porcentaje_ROS= round(ROS/Total,3)*100,
                                                                                       Estadisticos= list(round(summary(Valor_operado))),
                                                                                       Estadisticos_ad= list(round(quantile(Valor_operado,
                                                                                                                            probs = c(0.85,0.90,
                                                                                                                                      0.95),
                                                                                                                            type = 6)))
                                                                                       )
       ##
       resumen_1<- Alertas_1 %>% group_by(Caracteristica_2,
                                      Caracteristica_1, Valor_esperado) %>% summarise(Total= n(),
                                                                                      Estadisticos= list(round(summary(Valor_operado))),
                                                                                      Estadisticos_ad= list(round(quantile(Valor_operado,
                                                                                                                           probs = c(0.85,0.90,
                                                                                                                                     0.95),
                                                                                                                           type = 6)))
                                      )
       ####
       resumen_activ<- Alertas_1 %>% group_by(Caracteristica_2,
                                      Caracteristica_1, Valor_esperado, Detalle.y) %>% summarise(Total= n()
                                      )
       ###
       datos<- Personas_Naturales %>% filter(IdPersona %in% unique(Alertas_1$Nro_ID_Cliente) )
       #



       casos=c("ESTUDIANTES                                                 ","OTROS SERVICIOS NCP                                         ",
         "VENTA AL POR MENOR DE PRENDAS Y ACCESORIOS DE VESTIR        ","AMA DE CASA                                                 ",
         "VENTA AL POR MAYOR DE BEBIDAS CIGARRILLOS Y  TABACO         ","CONSTRUCCION REFORMA Y REPARACION DE EDIFICIOS              ",
         "SERVICIOS DE PELUQUERIA Y OTROS TRATAMIENTOS DE BELLEZA     ","VENTA AL POR MAYOR DE PRENDAS Y ACCESORIOS DE VESTIR        ",
         "VENTA AL POR MENOR DE ARTICULOS DE JOYERIA Y FANTASIAS      ","VENTA AL POR MENOR DE ARTICULOS PARA EL HOGAR NCP           ")
       #######
       load("Alertas_para_resumen.RData")
       library(readxl)
       datos <- read_excel("alertas_para_cierre_28082024.xlsx", sheet = "Base")
       View(datos)
       #
       #alertas_para_cierre_28082024$FECHA_GENERACION= as.Date(alertas_para_cierre_28082024$FECHA_GENERACION, format = "%m/%d/%Y")
       Alertas_para_resumen<- Alertas_para_resumen %>% mutate(cod= paste(Nro_de_identificacion, Detalle, Fecha_de_generacion))
       datos$DETALLE[datos$DETALLE=="IMPORTE MAXIMO POR OPERACION / _TRANSACCIONES ACH (RECIBIDA)"]="Importe Máximo por operación /  _TRANSACCIONES ACH (RECIBIDA)"
       datos$DETALLE[datos$DETALLE=="CANTIDAD DE OPERACIONES MENSUALES / _TRANSACCIONES ACH (RECIBIDA)"]="Cantidad de Operaciones Mensuales /  _TRANSACCIONES ACH (RECIBIDA)"
       datos$DETALLE[datos$DETALLE=="IMPORTE MAXIMO POR OPERACION / _DEPOSITOS (CAJA DE AHORRO)"]="Importe Máximo por operación /  _DEPOSITOS (CAJA DE AHORRO)"
       datos$DETALLE[datos$DETALLE=="CANTIDAD DE OPERACIONES MENSUALES / _RETIROS (CAJA DE AHORRO)"]="Cantidad de Operaciones Mensuales / _RETIROS (CAJA DE AHORRO)"
       datos$DETALLE[datos$DETALLE=="IMPORTE MAXIMO POR OPERACION / _TRANSACCIONES ACH (ENVIO)"]="Importe Máximo por operación /  _TRANSACCIONES ACH (ENVIO)"
       datos$DETALLE[datos$DETALLE=="CANTIDAD DE OPERACIONES MENSUALES / _TRANSACCIONES ACH (ENVIO)"]="Cantidad de Operaciones Mensuales / _TRANSACCIONES ACH (ENVIO)"
       datos$DETALLE[datos$DETALLE=="CANTIDAD DE OPERACIONES MENSUALES / _TRANSACCIONES ACH (RECIBIDA)"]="Cantidad de Operaciones Mensuales / _TRANSACCIONES ACH (RECIBIDA)"
       datos$DETALLE[datos$DETALLE=="CANTIDAD DE OPERACIONES MENSUALES / _DEPOSITOS (CAJA DE AHORRO)"]="Cantidad de Operaciones Mensuales / _DEPOSITOS (CAJA DE AHORRO)"
       datos$DETALLE[datos$DETALLE=="IMPORTE MAXIMO ACUM. POR MES / _RETIROS (CAJA DE AHORRO)"]="Importe Máximo Acum. por mes /  _RETIROS (CAJA DE AHORRO)"
       datos$DETALLE[datos$DETALLE=="IMPORTE MAXIMO POR OPERACION / GIROS EXTERIOR ENVIADOS"]="Importe Máximo por operación / GIROS EXTERIOR ENVIADOS"
       datos$DETALLE[datos$DETALLE=="IMPORTE MAXIMO POR OPERACION / _RETIRO ATM"]="Importe Máximo por operación / _Retiro ATM"

       unique(datos$DETALLE) %in% unique(Alertas_para_resumen$Detalle)
       datos<- datos %>% mutate(corre=1:nrow(datos))
       datos<- datos %>% mutate(cod= paste(DOCUMENTO, DETALLE, FECHA_GENERACION))
       prueba1<- datos %>% left_join(Alertas_para_resumen[,c(2,7,8,17,18,23,30)], by="cod")
       prueba1<- prueba1 %>% group_by(corre) %>% filter(row_number()==1)
       prueba1= prueba1 %>% filter(!is.na(Valor_esperado))
       prueba1= prueba1 %>% mutate(analisis_cierre= paste(ASIGNACIÓN, ANÁLISIS,sep = ": "))

       list.files("\\vfsnalsrv\\GNPC\\Jefatura Nacional de Cumplimiento\\CARLOS ANDRES\\DEBIDA DILIGENCIA 2024\\01 Clientes P. Naturales con riesgo alto\\Mirian Tola Mamani")
       list.files("D:\\Documentos\\Report_Aut")
       list.files("D:\\DD Prueba")
       #stringr::str_extract(list.files("D:\\DD Prueba"), pattern = "^(.+?)_")
       #nchar(stringr::str_extract(list.files("D:\\DD Prueba"), pattern = "^(.+?)_"))
       substr(list.files("D:\\DD Prueba"),nchar(stringr::str_extract(list.files("D:\\DD Prueba"), pattern = "^(.+?)_")), nchar(list.files("D:\\DD Prueba")))
       ######### PARA DATOS CARGA DD ##############
       library(tidyverse)
       load("Personas_Naturales.RData")
       Direccion_inicial= "C:\\Users\\dmamaniq\\OneDrive - Banco Solidario SA\\Escritorio\\Debida_Diligencia"
       stringr::str_replace_all(substr(list.files("D:\\DD Prueba"),nchar(stringr::str_extract(list.files("D:\\DD Prueba"),
                                                                                          pattern = "^(.+?)_")),
                                   nchar(list.files("D:\\DD Prueba"))),"_","")
       datos_documentos_ruta<- data.frame(Nro_de_identificacion=str_replace_all(substr(list.files(Direccion_inicial),
                                                                                       nchar(stringr::str_extract(list.files(Direccion_inicial),
                                                                                                                  pattern = "^(.+?)_")),
                                                                                       nchar(list.files(Direccion_inicial))),"_",""),
                                          Ruta=list.files(Direccion_inicial))
       base_documentos <- readxl::read_excel("data DD.xlsx", sheet = "Hoja1")
       datos_documentos_ruta<- datos_documentos_ruta %>% left_join(base_documentos[,c(1,3)], by=c("Nro_de_identificacion"="NUMERO_DOCUMENTO"))
       datos_documentos_ruta<- datos_documentos_ruta %>% left_join(Personas_Naturales[,c(1,3,41)], by=c("USER_ID"="IdPersona"))
       #datos_documentos_ruta<- datos_documentos_ruta %>% left_join(Personas_Naturales[,c(1,3,41)], by=c("Nro_de_identificacion"="Documento"))
       #
       Direccion<- "C:\\Users\\dmamaniq\\OneDrive - Banco Solidario SA\\Escritorio\\Debida_Diligencia"
       list.files(Direccion)
       library(readxl)
       Conclusiones=NULL
       gestion_DD=2024
       for (i in 1:nrow(datos_documentos_ruta)) {
         print(i)
         casos_filtrados<-list.files(paste(Direccion,"\\",datos_documentos_ruta$Ruta[i], sep = ""))[!str_detect(list.files(paste(Direccion,"\\",
                                                                                                                                 datos_documentos_ruta$Ruta[i],
                                                                                                                                 sep = "")),"\\$|.db") ]
         casos_filtrados<- casos_filtrados[str_detect(casos_filtrados,".xlsm")]
         Direccion<- "C:\\Users\\dmamaniq\\OneDrive - Banco Solidario SA\\Escritorio\\Debida_Diligencia"
         Direccion_individual<-paste(Direccion,"\\", datos_documentos_ruta$Ruta[i],"\\",casos_filtrados , sep="")
         Conclusiones[i]<- paste("El Análisis para la gestion ",gestion_DD ,"del cliente es:",
                                 names(read_excel(Direccion_individual, sheet = '1. Planilla', range = 'B65:B65')),
                                 names(read_excel(Direccion_individual, sheet = '1. Planilla', range = 'B74:B74')))
       }
       datos_documentos_ruta= data.frame(datos_documentos_ruta, Conclusiones)
       datos_documentos_ruta<- datos_documentos_ruta[,c(1:4,6,5)]
       datos_documentos_ruta$Conclusiones= str_replace_all(datos_documentos_ruta$Conclusiones,"\r\n","")
       datos_documentos_ruta$Conclusiones= str_replace_all(datos_documentos_ruta$Conclusiones,"\r\n","")
##### PARA EXTRAER DE UNA CADENA LAS MAYUSCULAS #############
       str_extract_all("The Cat in the Hat", "[a-z]+")
       str_extract_all("The Cat in the Hat", regex("[a-z]+", TRUE))
       ###
       casos_filtrados<-list.files(paste(Direccion,"\\",datos_documentos_ruta$Ruta[4], sep = ""))[!str_detect(list.files(paste(Direccion,"\\",
                                                                                                                               datos_documentos_ruta$Ruta[1],
                                                                                                                               sep = "")),"\\$|.db") ]
       casos_filtrados<- casos_filtrados[str_detect(casos_filtrados,".xlsm")]
       Direccion<- "C:\\Users\\dmamaniq\\OneDrive - Banco Solidario SA\\Escritorio\\Debida_Diligencia"
       Direccion_individual<-paste(Direccion,"\\", datos_documentos_ruta$Ruta[4],"\\",casos_filtrados , sep="")
       paste("El Análisis para el cliente es:",
             names(read_excel(Direccion_individual, sheet = '1. Planilla', range = 'B65:B65')),
             "y la conclución general acerca del caso es:",names(read_excel(Direccion_individual, sheet = '1. Planilla', range = 'B74:B74')))
       ### PRUEBA USO LIBRERIA MICROSOFT365 ################
       library(Microsoft365R)
       outl <- get_personal_outlook()
       outl <-get_business_outlook()
       ######## libreria janitor para tablas ########
       library(janitor)
       datos<- POSTULANTES %>% filter(REGIONAL=="ORIENTE") %>% count(CARGO_AL_QUE_POSTULA, sort = T)
       names(datos)[2]="Cantidad"
       datos<- datos %>% mutate(Porcentaje= round(Cantidad/ sum(Cantidad),3))
       datos<- datos %>% mutate(Porcentaje= paste(Porcentaje,"%",sep = ""))
       #### con tabyl
       datos<- POSTULANTES %>% filter(REGIONAL=="ORIENTE")
       datos<- datos %>% tabyl(Instancia)
       datos_resumen<- datos %>% arrange(desc(n)) %>% adorn_totals("row") %>% adorn_pct_formatting()
       kable(datos_resumen, latex_valign = "m", align = c('l','c','c'))
       #
       datos<- POSTULANTES %>% filter(REGIONAL=="ORIENTE")
       datos1<-datos %>% tabyl(CARGO_AL_QUE_POSTULA ,Instancia)
       ### con solo agrupacion por instancia
       datos<- POSTULANTES %>% filter(REGIONAL=="ORIENTE")
       datos<- datos %>% count(Instancia, CARGO_AL_QUE_POSTULA) %>% arrange(Instancia, desc(n))
       datos$Instancia= factor(datos$Instancia, levels = c("PRIMERA INSTANCIA","SEGUNDA INSTANCIA","TERCERA INSTANCIA","REPROBADO","DESISTIÓ"))
       datos<- datos %>% arrange(Instancia)
       datos1<- datos %>% split(.[,c("Instancia")]) %>%
         map_df(., janitor::adorn_totals)
       datos1<- datos1 %>% mutate(corre= 1:nrow(datos1))
       filas<- (datos1 %>% filter(Instancia=="Total"))$corre
       kable(datos, latex_valign = "m", align = c('l','c','c'))
       #datos1<- datos %>% spread(key = Instancia, value = n)
       ###
       datos<- POSTULANTES %>% filter(REGIONAL=="ORIENTE")
       datos<- datos %>% tabyl(Instancia, OFICINA) %>% adorn_totals("row")
       ## para tabla induccion
       datos<- Induccion %>% filter(REGIONAL=="ORIENTE") %>% tabyl(CARGO) %>% arrange(desc(n))
       datos_resumen<- datos %>% adorn_totals("row") %>% adorn_pct_formatting()
       kable(datos_resumen, latex_valign = "m", align = c('l','c','c'))
       #########
       my_update_function <- function(x){
         tryCatch(
           # This is what I want to do...
           {
             ibj_click8=x
             return(ibj_click8)
           },
           # ... but if an error occurs, tell me what happened:
           error=function(error_message) {
             message("This is my custom message.")
             message("And below is the error message from R:")
             message(error_message)
             Sys.sleep(1)
             remDr$refresh()
             Sys.sleep(2)
           }
         )
       }
       my_update_function(remDr$findElement(using = 'xpath', value = paste('//*[@id="content-alerta"]/div/table/tbody/tr[',contador_ingreso,']' ,sep = "")))



       #//*[@id="contenedor-lista-entidades"]/div/table/thead/tr/th[2]

       library(readxl)
       CIERRE_MASIVO <- read_excel("CIERRE_MASIVO.xlsx",
                                   sheet = "Hoja2")
       View(CIERRE_MASIVO)
       CIERRE_MASIVO<- CIERRE_MASIVO %>% filter(!is.na(USERID))
       datos= CIERRE_MASIVO %>% filter(is.na(OBSERVACIÓN))
       names(datos)[3]="DOCUMENTO"
       datos= datos
       #########
       library(RSelenium)
       #library(rvest)
       #library(magrittr)
       library(foreach)
       library(doParallel)
       library(tidyverse)
       # using  docker run -d -p 4445:4444 selenium/standalone-chrome:3.5.3
       # in windows
       URLsPar <- c("https://vfindersrv/Account/Login", "https://vfindersrv/Account/Login",
                    "https://vfindersrv/Account/Login")

       appHTML <- c()

       (cl <- (detectCores() - 1) %>%  makeCluster) %>% registerDoParallel
       # open a remoteDriver for each node on the cluster
       clusterEvalQ(cl, {
         library(RSelenium)
         remote_driver <- rsDriver(browser = "chrome", chromever = "128.0.6613.86", verbose = F)
         remDr<- remote_driver$client
       })
       foreach(x = 1:length(URLsPar),
                     .packages = c("RSelenium", "RSelenium", "RSelenium"))  %dopar%  {
                       print(x)
                       print(URLsPar[x])
                       remDr$navigate(URLsPar[x])
                       if(x==1){
                         objeto_nombre<- remDr$findElement(using = 'id', 'usernameinput')
                         Sys.sleep(2)
                         objeto_nombre$sendKeysToElement(list('dmamaniq'))
                         Sys.sleep(2)
                         #
                         objeto_pwd<- remDr$findElement(using = 'id', 'passwordinput')
                         Sys.sleep(2)
                         objeto_pwd$sendKeysToElement(list('Estadistico671&$&&&0+++'))
                         Sys.sleep(1)
                         #
                         objeto_wind<- remDr$findElement(using = 'xpath','/html/body/div/div[1]/div[4]/div[1]/label')
                         Sys.sleep(2)
                         objeto_wind$clickElement()
                         Sys.sleep(2)
                         #
                         ibj_iniciar<- remDr$findElement(using = 'xpath', '/html/body/div/div[1]/button')
                         Sys.sleep(1)
                         ibj_iniciar$clickElement()
                         Sys.sleep(3)
                         client<- 1
                       }
                       else if(x==2){
                         objeto_nombre<- remDr$findElement(using = 'id', 'usernameinput')
                         Sys.sleep(2)
                         objeto_nombre$sendKeysToElement(list('dmamaniq1'))
                         Sys.sleep(2)
                         #
                         objeto_pwd<- remDr$findElement(using = 'id', 'passwordinput')
                         Sys.sleep(2)
                         objeto_pwd$sendKeysToElement(list('Estadistico671&$&&&0+++'))
                         Sys.sleep(1)
                         #
                         objeto_wind<- remDr$findElement(using = 'xpath','/html/body/div/div[1]/div[4]/div[1]/label')
                         Sys.sleep(2)
                         objeto_wind$clickElement()
                         Sys.sleep(2)
                         #
                         ibj_iniciar<- remDr$findElement(using = 'xpath', '/html/body/div/div[1]/button')
                         Sys.sleep(1)
                         ibj_iniciar$clickElement()
                         Sys.sleep(3)
                         client<- 1
                       }
                       else{
                         objeto_nombre<- remDr$findElement(using = 'id', 'usernameinput')
                         Sys.sleep(2)
                         objeto_nombre$sendKeysToElement(list('dmamaniq2'))
                         Sys.sleep(2)
                         #
                         objeto_pwd<- remDr$findElement(using = 'id', 'passwordinput')
                         Sys.sleep(2)
                         objeto_pwd$sendKeysToElement(list('Estadistico671&$&&&0+++'))
                         Sys.sleep(1)
                         #
                         objeto_wind<- remDr$findElement(using = 'xpath','/html/body/div/div[1]/div[4]/div[1]/label')
                         Sys.sleep(2)
                         objeto_wind$clickElement()
                         Sys.sleep(2)
                         #
                         ibj_iniciar<- remDr$findElement(using = 'xpath', '/html/body/div/div[1]/button')
                         Sys.sleep(1)
                         ibj_iniciar$clickElement()
                         Sys.sleep(3)
                         client<- 1
                       }
                     }



       library(RSelenium)
       #library(rvest)
       #library(magrittr)
       library(foreach)
       library(doParallel)
       library(tidyverse)
       # using  docker run -d -p 4445:4444 selenium/standalone-chrome:3.5.3
       # in windows
       URLsPar <- c("https://bancosol.shinyapps.io/BuscadorBDN/", "https://bancosol.shinyapps.io/BuscadorBDN/",
                    "https://bancosol.shinyapps.io/BuscadorBDN/")

       (cl <- (detectCores() - 13) %>%  makeCluster) %>% registerDoParallel
       # open a remoteDriver for each node on the cluster
       clusterEvalQ(cl, {
         library(RSelenium)
         remote_driver <- rsDriver(browser = "chrome", chromever = "128.0.6613.86", verbose = F)
         remDr<- remote_driver$client
       })
       ws <- foreach(x = 1:length(URLsPar),
                     .packages = c("RSelenium", "RSelenium", "RSelenium"))  %dopar%  {
                       print(x)
                       print(URLsPar[x])
                       remDr$navigate(URLsPar[x])
                       objeto_nombre<- remDr$findElement(using = 'xpath', '//*[@id="auth-user_id"]')
                       objeto_nombre$sendKeysToElement(list('deyvis'))
                       #
                       objeto_cont<- remDr$findElement(using = 'xpath', '//*[@id="auth-user_pwd"]')
                       objeto_cont$sendKeysToElement(list('Est&$&00'))
                       objeto_enter<-remDr$findElement(using = 'xpath', '//*[@id="auth-go_auth"]')
                       objeto_enter$clickElement()

                       objeto_edad<- remDr$findElement(using = 'id', 'edad')
                       objeto_edad$sendKeysToElement(list('32'))
                     }

       ############
       library(RSelenium)
       library(foreach)
       library(doParallel)

       # Función para abrir un navegador, navegar a una URL y extraer datos
       extraer_datos <- function(url) {
         # Iniciar el servidor Selenium
         remote_driver <- rsDriver(browser = "chrome", chromever = "128.0.6613.86",
                        verbose = F)
         remDr<-   remote_driver$client
         # Navegar a la URL
         remote_driver$navigate(url)

         # Extraer datos (ejemplo: obtener el título de la página)
         titulo_elemento <- remote_driver$findElement(using = 'css selector', value = "title")
         titulo <- titulo_elemento$getElementText()[[1]]

         # Cerrar el navegador
         #remote_driver$close()
         #rD$server$stop()

         return(titulo)
       }

       # Lista de URLs a visitar
       urls <- c("https://www.google.com", "https://www.r-project.org", "https://www.wikipedia.org")

       # Detectar el número de núcleos disponibles
       numCores <- detectCores()

       # Registrar el backend paralelo
       registerDoParallel(cores = numCores)

       # Ejecutar las tareas en paralelo y guardar los resultados
       resultados <- foreach(url = urls, .packages = c("RSelenium")) %dopar% {
         extraer_datos(url)
       }
       foreach(x = 1:length(URLsPar),
               .packages = c("RSelenium", "RSelenium", "RSelenium"))
       # Mostrar los resultados
       for (i in 1:length(urls)) {
         cat("El título de la página", urls[i], "es:\n")
         print(resultados[[i]])
       }

       # Detener el backend paralelo
       stopCluster()




       #### 10:51
       library(RSelenium)
       library(foreach)
       library(doParallel)
       library(tidyverse)
       URLsPar <- c("http://www.bbc.com/", "http://www.cnn.com", "http://www.google.com")
       appHTML <- c()
       # start a Selenium Server
       selServ <- startServer()

       (cl <- (detectCores() - 13) %>%  makeCluster) %>% registerDoParallel
       # open a remoteDriver for each node on the cluster
       clusterEvalQ(cl, {
         library(RSelenium)
         remDr <- remoteDriver()
         remDr$open()
       })
       myTitles <- c()
       ws <- foreach(x = 1:length(URLsPar), .packages = c("RSelenium", "RSelenium", "RSelenium"))  %dopar%  {
         remDr$navigate(URLsPar[x])
         remDr$getTitle()[[1]]
       }
       ### 12.27
       library(RSelenium)
       library(foreach)
       library(doParallel)

       # Función para extraer datos de una página
       extraer_datos <- function(url) {
         remDr <- rsDriver(browser = "chrome")
         remote_driver <- remDr$client
         remote_driver$navigate(url)

         # Esperar hasta que el elemento esté presente (ajusta el timeout según sea necesario)
         wait <- WebDriverWait(remote_driver, timeout = 10)
         title_element <- wait.until(presence_of_element_located(by.xpath("//title")))

         # Intentar extraer el título, manejando posibles errores
         tryCatch({
           titulo <- title_element$getElementText()
         }, error = function(e) {
           print(paste("Error al extraer el título:", e))
           titulo <- NA  # O cualquier valor por defecto que desees
         })

         # ... otros extractores de datos, manejando errores de forma similar

         # Cerrar el navegador
         remote_driver$close()

         return(list(titulo = titulo, ...))
       }

       # Lista de URLs
       urls <- c("https://www.ejemplo1.com", "https://www.ejemplo2.com", ...)

       # Registrar backend paralelo
       registerDoParallel(cores = detectCores() - 1)

       # Ejecutar en paralelo
       resultados <- foreach(url = urls, .packages = c('RSelenium'), .combine = rbind) %dopar% {
         extraer_datos(url)
       }
       # Detener el backend paralelo
       stopCluster()

       #### 15:08
       library(RSelenium)
       library(magrittr)
       library(foreach)
       library(doParallel)

       URLsPar <- c("http://www.bbc.com/", "http://www.cnn.com", "http://www.google.com",
                    "http://www.yahoo.com", "http://www.twitter.com")
       appHTML <- c()

       (cl <- (detectCores() - 1) %>%  makeCluster) %>% registerDoParallel
       # open a remoteDriver for each node on the cluster
       clusterEvalQ(cl, {
         library(RSelenium)
         remDr <- remoteDriver$new()
         remDr$open()
       })
       myTitles <- c()
       ws <- foreach(x = 1:length(URLsPar), .packages = c("rvest", "magrittr", "RSelenium"))  %dopar%  {
         remDr$navigate(URLsPar[x])
         remDr$getTitle()[[1]]
       }

       # close browser on each node
       clusterEvalQ(cl, {
         remDr$close()
       })

       stopImplicitCluster()



       library(parallel)
       library(RSelenium)
       # Start the Selenium server (if not already running)
       #system("java -jar selenium-server-standalone-4.x.x.jar", wait = FALSE)

       # Create a cluster
       cl <- makeCluster(detectCores()-13)

       # Export necessary libraries and functions to the cluster
       clusterEvalQ(cl, {
         library(RSelenium)
       })

       # Try to connect to the Selenium server
       tryCatch({
         clusterEvalQ(cl, {
           remote_driver<- rsDriver(port = 4577L ,browser = "chrome", chromever = "128.0.6613.86",
                                    verbose = F)
           remDr<-   remote_driver$client
           remDr$navigate("https://vfindersrv/Account/Login")
           remDr<-   remote_driver$client
         })
       }, error = function(e) {
         print("Error connecting to Selenium server:")
         print(e)
       })

       # Stop the cluster
       stopCluster(cl)
################ clave de pdf infocred ################
   #    W160952

 ####### CASO DE REAPERTURA DE ALERTAS ###########
    #   1009379021
####### libreria editbl ###############
       # tibble support
       library(editbl)
       modifiedData <- editbl::eDT(tibble::as_tibble(mtcars))

       # data.frame support
       modifiedData <- editbl::eDT(mtcars)

       # data.table support
       modifiedData <- editbl::eDT(data.table::data.table(mtcars))

       # database support
       tmpFile <- tempfile(fileext = ".sqlite")
       file.copy(system.file("extdata", "chinook.sqlite", package = 'editbl'), tmpFile)
       conn <- editbl::connectDB(dbname = tmpFile)
       modifiedData <- editbl::eDT(dplyr::tbl(conn, "Artist"), in_place = TRUE)
       DBI::dbDisconnect(conn)
       unlink(tmpFile)

       # excel integration
       xlsx_file <- system.file("extdata",
                                "artists.xlsx",
                                package="editbl")
       xlsx_file <- "D:/Documentos/tablas_edit/ARCHIVO_PRUEBA.xlsx"
       xlsx_tbl <- tibble::as_tibble(
         readxl::read_excel(xlsx_file)
       )
       modified <- eDT(xlsx_tbl)
       openxlsx::write.xlsx(modified, xlsx_file)

    ###

       filtrado=grep(datos$documento, Alertas$Nro_de_identificacion , ignore.case = T)
       persona_d= Alertas[filtrado,]

       unido= data.frame()
       for (i in 1:nrow(base_Andy)) {
         filtrado=grep(base_Andy$numeros_carnet[i], Alertas$Nro_de_identificacion , ignore.case = T)
         persona_d= Alertas[filtrado,]
         unido= rbind(unido, persona_d)
       }
 ############# OPENXLSX EJEMPLOS #######
       hs <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                         halign = "center", valign = "center", textDecoration = "Bold",
                         border = "TopBottomLeftRight", textRotation = 45)

       write.xlsx(iris, file = "writeXLSX4.xlsx", borders = "rows", headerStyle = hs)
       write.xlsx(iris, file = "writeXLSX5.xlsx", borders = "columns", headerStyle = hs)

       write.xlsx(iris, "writeXLSXTable4.xlsx", asTable = TRUE,
                  headerStyle = createStyle(textRotation = 45))
       ##
       df <- data.frame("Date" = Sys.Date()-0:19, "LogicalT" = TRUE,
                        "Time" = Sys.time()-0:19*60*60,
                        "Cash" = paste("$",1:20), "Cash2" = 31:50,
                        "hLink" = "https://CRAN.R-project.org/",
                        "Percentage" = seq(0, 1, length.out=20),
                        "TinyNumbers" = runif(20) / 1E9,  stringsAsFactors = FALSE)
       l <- list("IRIS" = iris, "colClasses" = df)
       write.xlsx(l, file = "writeXLSX6.xlsx", borders = "columns", headerStyle = hs)
       write.xlsx(l, file = "writeXLSXTable5.xlsx", asTable = TRUE, tableStyle = "TableStyleLight2")

       openXL("writeXLSX6.xlsx")
       openXL("writeXLSXTable5.xlsx")
###########
       library(openxlsx)
       library(jpeg)
       library(ggplot2)

       plotFn <- function(x, ...) {
         colvec <- grey(x)
         colmat <- array(match(colvec, unique(colvec)), dim = dim(x)[1:2])
         image(x = 0:(dim(colmat)[2]), y = 0:(dim(colmat)[1]), z = t(colmat[rev(seq_len(nrow(colmat))) , ]),
               col = unique(colvec), xlab = "", ylab = "", axes = FALSE, asp = 1,
               bty ="n", frame.plot=FALSE, ann=FALSE)
       }

       ## Create workbook and add a worksheet, hide gridlines
       library(openxlsx)
       library(ggplot2)
       wb <- createWorkbook()
       addWorksheet(wb, "Original Image", gridLines = FALSE)
       #
       writeData(wb, 1, "Primera fila", xy = c(2,2)) # x =columna y = fila
       writeData(wb, 1, "Segunda fila", xy = c(5,3))
       #
       p1 <- qplot(mpg,
                   data = mtcars, geom = "density",
                   fill = as.factor(gear), alpha = I(.5), main = "Distribution of Gas Mileage"
       )
       print(p1) # plot needs to be showing
       insertPlot(wb, 1, width = 5, height = 3.5, fileType = "png", units = "in"
                  , startRow= 5, startCol = 2)
       #insertPlot(wb, 1, width, height, units="px", startRow= 5, startCol = 2)
       #
       saveWorkbook(wb, "prueba_con_Image.xlsx", overwrite = TRUE)
       ## SVD of covariance matrix
       rMeans <- rowMeans(A)
       rowMeans <- do.call("cbind", lapply(seq_len(ncol(A)), function(X) rMeans))
       A <- A - rowMeans
       E <- svd(A %*% t(A) / (ncol(A) - 1)) # SVD on covariance matrix of A
       pve <- data.frame("Eigenvalues" = E$d,
                         "PVE" = E$d/sum(E$d),
                         "Cumulative PVE" = cumsum(E$d/sum(E$d)))

       ## write eigenvalues to worksheet
       addWorksheet(wb, "Principal Component Analysis")
       hs <- createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                         halign = "CENTER", textDecoration = "Bold",
                         border = "TopBottomLeftRight", borderColour = "#4F81BD")

       writeData(wb, 2, x="Proportions of variance explained by Eigenvector" ,startRow = 2)
       mergeCells(wb, sheet=2, cols=1:4, rows=2)

       setColWidths(wb, 2, cols = 1:3, widths = c(14, 12, 15))
       writeData(wb, 2, x=pve, startRow = 3, startCol = 1, borders="rows", headerStyle=hs)

       ## Plots
       pve <- cbind(pve, "Ind" = seq_len(nrow(pve)))
       ggplot(data = pve[1:20,], aes(x = Ind, y = 100*PVE)) +
         geom_bar(stat="identity", position = "dodge") +
         xlab("Principal Component Index") + ylab("Proportion of Variance Explained") +
         geom_line(size = 1, col = "blue") + geom_point(size = 3, col = "blue")

       ## Write plot to worksheet 2
       insertPlot(wb, 2, width = 5, height = 4, startCol = "E", startRow = 2)

       ## Plot of cumulative explained variance
       ggplot(data = pve[1:50,], aes(x = Ind, y = 100*Cumulative.PVE)) +
         geom_point(size=2.5) + geom_line(size=1) + xlab("Number of PCs") +
         ylab("Cumulative Proportion of Variance Explained")
       insertPlot(wb, 2, width = 5, height = 4, xy= c("M", 2))


       ## Reconstruct image using increasing number of PCs
       nPCs <- c(5, 7, 12, 20, 50, 200)
       startRow <- rep(c(2, 24), each = 3)
       startCol <- rep(c("B", "H", "N"), 2)

       ## create a worksheet to save reconstructed images to
       addWorksheet(wb, "Reconstructed Images", zoom = 90)

       for(i in seq_len(length(nPCs))) {

         V <- E$v[, 1:nPCs[i]]
         imgHat <- t(V) %*% A  ## project img data on to PCs
         imgSize <- object.size(V) + object.size(imgHat) + object.size(rMeans)

         imgHat <- V %*% imgHat + rowMeans  ## reconstruct from PCs and add back row means
         imgHat <- round((imgHat - min(imgHat)) / (max(imgHat) - min(imgHat))*255) # scale
         plotFn(imgHat/255)

         ## write strings to worksheet 3
         writeData(wb, "Reconstructed Images",
                   sprintf("Number of principal components used:  %s",
                           nPCs[[i]]), startCol[i], startRow[i])

         writeData(wb, "Reconstructed Images",
                   sprintf("Sum of component object sizes: %s bytes",
                           format(as.numeric(imgSize), big.mark=',')), startCol[i], startRow[i]+1)

         ## write reconstruced image
         insertPlot(wb, "Reconstructed Images", width, height, units="px",
                    xy = c(startCol[i], startRow[i]+3))

       }

       # hide grid lines
       showGridLines(wb, sheet = 3, showGridLines = FALSE)

       ## Make text above images BOLD
       boldStyle <- createStyle(textDecoration="BOLD")

       ## only want to apply style to specified cells (not all combinations of rows & cols)
       addStyle(wb, "Reconstructed Images", style=boldStyle,
                rows = c(startRow, startRow+1), cols = rep(startCol, 2),
                gridExpand = FALSE)

       ## save workbook to working directory
       saveWorkbook(wb, "prueba_con_Image.xlsx", overwrite = TRUE)


       ## remove example files for cran test
       if (identical(Sys.getenv("NOT_CRAN", unset = "true"), "false")) {
         file_list<-list.files(pattern="\\.xlsx",recursive = TRUE)
         file_list<-fl[!grepl("inst/extdata",file_list)&!grepl("man/",file_list)]

         if(length(file_list)>0) {
           rm(file_list)
         }
       }
##################
       library(dbplyr)
       con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
       copy_to(con, mtcars)
       mtcars2 <- tbl(con, "mtcars")
       nrow(mtcars2) %>% show_query()
       summary <- mtcars2 %>%
         group_by(cyl) %>%
         summarise(mpg = mean(mpg, na.rm = TRUE)) %>%
         arrange(desc(mpg))%>% show_query()
       #
       mtcars2 %>% group_by()
       ###
       con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
       copy_to(con, prueba_11)
       tabla <- tbl(con, "prueba_11")
       ###
       datos= tabla %>% group_by(codPersona,descTipoOperacion) %>%
         arrange(desc(fechaUltimoMovimiento)) %>% filter(row_number()==1) %>% show_query()
       ##########
       //*[@id="inpValor576773-6"]

       //*[@id="inpValor35826-6"]

       #
       //*[starts-with(@id,'inpValor')][contains(@id,'-6')]
       #


       //*[@id="libraryPicker-1529936071347-datasets"]
       //*[starts-with(@id,'libraryPicker')][contains(@id,'datasets')]



       objeto_insert_texto=remDr$findElement(using = 'xpath', "//*[starts-with(@id,'inpValor')][contains(@id,'-6')]")
       objeto_insert_texto$clearElement()
       objeto_insert_texto$sendKeysToElement(list('50000'))




       valor_1=remDr$findElement(using = 'xpath', '//*[@id="inpValor576773-6"]')
       valor_1=unlist(valor_1$getElementText())

       //*[@id="inpValor576773-6"]

       "rgba(47, 79, 79, 1)"







       Cambio_parametros<- Cambio_parametros %>% left_join(Alertas_para_resumen[,c(2,23)],by="Nro_de_identificacion")
       Cambio_parametros<- Cambio_parametros %>% group_by(Nro_de_identificacion) %>% filter(row_number()==1)
       Cambio_parametros<- Cambio_parametros %>% mutate(Tipo_Persona= Tipo_Documento)
       library(openxlsx)
       write.xlsx(Cambio_parametros, file = "eliminar.xlsx")



       persona= rest %>% filter(Documento %in% Division$DOCUMENTO  & TipoControl=="Cantidad de Operaciones Mensuales" & ProductoServicio==" _TRANSACCIONES ACH (RECIBIDA)")
       Division<- Division %>% left_join(persona[,c(3,7,12)], by=c("DOCUMENTO"="Documento"))
       resumen=Division %>% count(USUARIO,Valor)

       ####################
       Error=NULL
       for (i in 1:nrow(ERRORES_PCC_3TRIM)) {
         if(ERRORES_PCC_3TRIM$CONCLUSION[i]=="Dirección sin información relevante. Cliente: Direccion (Direccion Persona)"){
           Error[i]="Error 1"}
           else if(ERRORES_PCC_3TRIM$CONCLUSION[i]=="Origen y destino idénticos"){
             Error[i]="Error 2"
           }
           else if(ERRORES_PCC_3TRIM$CONCLUSION[i]=="Campo numérico. Cliente (Natural): Institucion/Empresa donde trabaja (Datos de la persona)"){
             Error[i]="Error 3"
           }
           else if(ERRORES_PCC_3TRIM$CONCLUSION[i]=="Información escasa en el origen y desitno de los fondos"){
             Error[i]="Error 4"
           }
         else if(ERRORES_PCC_3TRIM$CONCLUSION[i]=="Sin especificación del destino de recursos"){
           Error[i]="Error 5"
         }
         else if(ERRORES_PCC_3TRIM$CONCLUSION[i]=="Campo numérico. Beneficiario: Zona (Direccion Persona)"){
           Error[i]="Error 6"
         }
         else if(ERRORES_PCC_3TRIM$CONCLUSION[i]=="Dirección sin información relevante. Beneficiario: Direccion (Direccion Persona)"){
           Error[i]="Error 7"
         }
         else if(ERRORES_PCC_3TRIM$CONCLUSION[i]=="Campo numérico. Cliente (Natural): Direccion Trabajo (Datos de la persona)"){
           Error[i]="Error 8"
         }
         else if(ERRORES_PCC_3TRIM$CONCLUSION[i]=="Campo mayormente numérico. Cliente (Natural): Direccion Trabajo (Datos de la persona)"){
           Error[i]="Error 9"
         }
         else if(ERRORES_PCC_3TRIM$CONCLUSION[i]=="Campo mayormente numérico. Cliente (Natural): Institucion/Empresa donde trabaja (Datos de la persona)"){
           Error[i]="Error 10"
         }
         else if(ERRORES_PCC_3TRIM$CONCLUSION[i]=="Campo numérico. Cliente: Direccion (Direccion Persona)"){
           Error[i]="Error 11"
         }
         else if(ERRORES_PCC_3TRIM$CONCLUSION[i]=="Sin especificación del origen de los recursos"){
           Error[i]="Error 12"
         }
       }
       unique(Error)
       ERRORES_PCC_3TRIM= data.frame(ERRORES_PCC_3TRIM, Error)
    ####################### gaficas complemento presentacion regional #####
       datos<- POSTULANTES %>% tabyl(Instancia)
       datos_resumen<- datos %>% arrange(desc(n)) %>% adorn_totals("row") %>% adorn_pct_formatting()
       columnas= ncol(datos_resumen)-1
       names(datos_resumen)=c("Estado","Cantidad","Porcentaje")
       kable(datos_resumen,latex_valign = "m", caption = "Evaluación a postulantes") %>%
         kable_styling(font_size = 8,bootstrap_options = "striped", full_width = FALSE, latex_options = "HOLD_position") %>%
         add_header_above(header = c(" "=1,"Frecuencias"=columnas))  %>%  row_spec(0, bold = TRUE) %>% row_spec(nrow(datos_resumen),
                                                                                                                color = "white", bold = TRUE,
                                                                                                                background = "#40498EFF")
       ###
       datos_res= REQ_UIF %>% filter(FECHA_DE_SOLICITUD >= as.Date("2024-6-1") &
                                       FECHA_DE_SOLICITUD <= as.Date("2024-8-31")) %>% filter(REGIONAL=="EL ALTO") %>% count(PRODUCTO, CLIENTE)
       # datos_res1= datos_res %>% count(mes_solicitud, wt=n)
       # datos_res1= data.frame(mes_solicitud=datos_res1[,1], CLIENTE="Personas Requeridas",n=datos_res1[,2])
       # datos_res= rbind(datos_res, datos_res1) %>% arrange(mes_solicitud)
       datos_res<- datos_res %>% split(.[,c("mes_solicitud")]) %>%
         map_df(., janitor::adorn_totals)
       datos_res<- datos_res %>% mutate(corre= 1:nrow(datos_res))
       filas<- (datos_res %>% filter(mes_solicitud=="Total"))$corre
       datos_res<- datos_res[,-ncol(datos_res)]
       names(datos_res)[2]
       columnas= ncol(datos_res)-1
       kable(datos_res,latex_valign = "m", caption = "Requerimientos UIF") %>%
         kable_styling(font_size = 8,bootstrap_options = "striped", full_width = FALSE, latex_options = "HOLD_position") %>%
         add_header_above(header = c(" "=1,"Frecuencias"=columnas))  %>%  row_spec(0, bold = TRUE) %>% row_spec(filas,
                                                                                                                color = "white", bold = TRUE,
                                                                                                                background = "#40498EFF")
       ##
       datos_res= REQ_UIF %>% filter(FECHA_DE_SOLICITUD >= as.Date("2024-6-1") &
                                       FECHA_DE_SOLICITUD <= as.Date("2024-8-31")) %>%
         filter(REGIONAL=="EL ALTO") %>% count(PRODUCTO, CLIENTE) %>% arrange(desc(n))
       datos_res<- as.data.frame(datos_res %>% adorn_totals("row"))
       datos_res<-datos_res %>% mutate(porcentaje= paste(round(n/datos_res[nrow(datos_res),
                                                                           ncol(datos_res)],4)*100,"%"))

       REQ_UIF %>% filter(REGIONAL==input$regionales & FECHA_DE_SOLICITUD>= req(input$range[1]) &
                            FECHA_DE_SOLICITUD<= req(input$range[2])) %>% count(PRODUCTO, CLIENTE) %>% arrange(desc(n))

       ##
       datos<- Alertas_para_resumen %>% filter(REGIONAL=="EL ALTO") %>% tabyl(Caracteristica_2) %>% arrange(desc(n))
       datos_resumen<- datos %>% adorn_totals("row") %>% adorn_pct_formatting()
       ############
       datos<- AVANCE_DD %>% filter(REGIONAL=="EL ALTO") %>% count(RIESGO,ESTADO) %>% arrange(RIESGO, desc(n))
       datos<- datos %>% split(.[,c("RIESGO")]) %>%
         map_df(., janitor::adorn_totals, tabyl)

       datos %>% split(.[,c("RIESGO")]) %>%
         map_df(., janitor::adorn_pct_formatting)

       datos<- as.data.frame(datos)
       datos<- datos %>% mutate(porcentaje= paste(round(n/datos[nrow(datos),
                                                                    ncol(datos)],4)*100,"%"),corre= 1:nrow(datos))
       filas<- (datos %>% filter(mes_solicitud=="Total"))$corre
       datos<- datos[,-ncol(datos)]
       names(datos)[2]
       columnas= ncol(datos)-1


       datos %>% tabyl(RIESGO, ESTADO) %>%
         adorn_percentages("row")



       AVANCE_DD %>% filter(REGIONAL=="EL ALTO") %>% tabyl(RIESGO, ESTADO) %>% adorn_totals("col") %>% adorn_percentages("row") %>%
                     adorn_pct_formatting(digits = 2) %>%
                     adorn_ns()

       datos_resumen<- AVANCE_DD %>% filter(REGIONAL=="EL ALTO") %>% tabyl(RIESGO, ESTADO) %>% adorn_totals("col") %>% adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       kable(datos_resumen ,latex_valign = "m", caption = paste("Avance de debida diligencia",)) %>%
         kable_styling(font_size = 7,bootstrap_options = "striped", full_width = FALSE, latex_options = "HOLD_position") %>%
         row_spec(0, bold = TRUE) %>% column_spec(ncol(datos_resumen),color = "white", bold = TRUE,background = "#40498EFF")

       ####
       datos<- ALTA_PEP %>% count(REGIONAL, TIPO)
       hchart(datos , "column",hcaes(x=REGIONAL, y=n, group=TIPO),
              # stacking = list(enabled = TRUE),
              dataLabels = list(
                enabled = TRUE
                #, # Añadir etiquetas
                #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
                # format = "{point.percentage:.0f}%"
              )) %>% hc_add_theme(hc_theme_google())%>%
         hc_colors(c("#56147DFF","orange", "#FA7D5EFF","#403872FF", "#40498EFF","#C73D73FF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
         hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
       ##
       datos<- ALTA_PEP %>% count(REGIONAL, PRODUCTOS) %>% arrange(REGIONAL, desc(n))
       hchart(datos , "bar",hcaes(x=REGIONAL, y=n, group=PRODUCTOS),
               stacking = list(enabled = TRUE),
              dataLabels = list(
                enabled = TRUE
                #, # Añadir etiquetas
                #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
                # format = "{point.percentage:.0f}%"
              )) %>% hc_add_theme(hc_theme_google())%>%
         hc_colors(c("#56147DFF","#AF1801FF","orange"))%>%
         hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
       #####
       for (i in 1:nrow(REQ_UIF)) {
         if(!is.na(REQ_UIF$REGIONAL[i])){
           if(REQ_UIF$REGIONAL[i]=="TARIJA"){
             REQ_UIF$REGIONAL[i]="SUR"
           }
           else if(REQ_UIF$REGIONAL[i]=="PANDO"){
             REQ_UIF$REGIONAL[i]="EL ALTO"
           }
           else if(REQ_UIF$REGIONAL[i]=="ORURO"){
             REQ_UIF$REGIONAL[i]="OCCIDENTE"
           }
           else if(REQ_UIF$REGIONAL[i]=="BENI"){
             REQ_UIF$REGIONAL[i]="ORIENTE"
           }
         }
       }
       ############
       datos<- ERRORES_PCC_3TRIM %>% filter(REGIONAL=="EL ALTO") %>% tabyl(CONCLUSION)
       datos_resumen<- datos %>% arrange(desc(n)) %>% adorn_totals("row") %>% adorn_pct_formatting()
       columnas= ncol(datos_resumen)-1
       names(datos_resumen)[2:3]=c("Cantidad","porcentaje")
       kable(datos_resumen,latex_valign = "m", caption = paste("Errores PCC-01 regional")) %>%
         kable_styling(font_size = 8,bootstrap_options = "striped", full_width = FALSE, latex_options = "HOLD_position") %>%
         add_header_above(header = c(" "=1,"Frecuencias"=columnas))  %>%  row_spec(0, bold = TRUE) %>% row_spec(nrow(datos_resumen),
                                                                                                                color = "white", bold = TRUE,
                                                                                                                background = "#40498EFF")
       #
       kable(datos_resumen,latex_valign = "m", caption = paste("Errores PCC-01 regional", input$regionales)) %>%
         kable_styling(font_size = 8,bootstrap_options = "striped", full_width = FALSE, latex_options = "HOLD_position") %>%
         add_header_above(header = c(" "=1,"Frecuencias"=columnas))  %>%  row_spec(0, bold = TRUE) %>% row_spec(nrow(datos_resumen),
                                                                                                                color = "white", bold = TRUE,
                                                                                                                background = "#40498EFF")
       ######
       library(RDCOMClient)

       wordApp <- COMCreate("Word.Application")
       wordApp[["Visible"]] <- TRUE
       wordApp[["DisplayAlerts"]] <- FALSE
       path_To_PDF_File <- "yyy.pdf"
       path_To_Word_File <- "yyy.docx"

       doc <- wordApp[["Documents"]]$Open(normalizePath(path_To_PDF_File),
                                          ConfirmConversions = FALSE)
       doc$SaveAs2(path_To_Word_File)
       #########
       library(reticulate)

       py_run_string("from pdf2docx import parse")

       # path of pdf file
       py_run_string("pdf_file = 'tests/demo_custom.pdf'")

       # will create .docx in same path
       py_run_string("docx_file = 'tests/demo_custom.docx'")

       # Here is where we convert pdf to docx
       py_run_string("parse(pdf_file, docx_file, start=0, end=None)")

       ##########
       library(Convert2Docx)
       resume="INFORME.pdf"
       Converter(pdf_file = resume, docx_filename = "informe.docx")
       ############ BUSQUEDA EN GOOGLE SELENIUM #########
       library(RSelenium)
       library(wdman)
       library(magick)
       #
       binman::list_versions("chromedriver")
       remote_driver<- rsDriver(browser = "chrome", chromever = "133.0.6943.141",
                                verbose = F)
       # CONEXION A FINDER
       remDr<-   remote_driver$client
       #remDr$open()
       remDr$navigate("https://www.google.com")
       search_box <- remDr$findElement(using = 'name', value = 'q')
       nombres<- "'DEYVIS OMAR MAMANI QUISPE'"
       #
       ruta_escritorio <- "C:/Users/DEYVIS/Desktop/R/pruebas_webshoot"
       nombre_carpeta <- gsub(" |'","_",nombres)
       nombre_carpeta<- substr(nombre_carpeta, 2, nchar(nombre_carpeta)-1)
       ruta_completa <- file.path(ruta_escritorio, nombre_carpeta)
       dir.create(ruta_completa)
       #
       buscar<- paste(nombres,"-videos")
       search_box$sendKeysToElement(list(buscar, key = "enter"))
       
       # Obtener la altura total de la página
       js <- "return document.body.scrollHeight"
       total_height <- unlist(remDr$executeScript(js))
       # Altura de cada captura (ajustar según sea necesario)
       screenshot_height <- 1000
       # Inicializar la lista para almacenar las capturas
       cont<- 1
       # Iterar sobre la página, capturando por secciones
       for (i in seq(0, total_height, by = screenshot_height)) {
         # Desplazar la página
         js <- paste0("window.scrollTo(0, ", i, ");")
         remDr$executeScript(js)

         # Capturar la sección
         remDr$screenshot(file=paste0(ruta_completa,"/",nombre_carpeta,cont,".png"))
         cont<- cont +1
       }
       dir=ruta_completa
       fl = list.files(dir, full.names = TRUE, pattern = '.png')
       img = image_read(fl)
       img2 = image_append(img, stack = TRUE)
       image_write(img2, format = "pdf", file.path(dir, paste0("Medios_Comunicacion",nombre_carpeta,Sys.Date(),".pdf")))
       file.remove(fl)
       ###
       library(tidyverse)
       library(highcharter)
       datos=googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Hupgb_-law6ubD5h6VitUT-U_qFbzcy57fMXSF4AvBU/edit?usp=sharing",
                                       col_types = "ccccc", sheet = "Hoja 9")
       datos_filt= datos %>% filter(DESCRIPCION=="trafico_armas")
       unique(datos_filt$VARIABLE)
       datos_filt$VARIABLE<- str_replace(datos_filt$VARIABLE,"3. ","")
       # VALUE_NUM=NULL
       # for (i in 1:nrow(datos_filt)) {
       #   if(datos_filt$VALUE[i]=="Muy bajo"){VALUE_NUM[i]=1}
       #   else if(datos_filt$VALUE[i]=="Bajo"){VALUE_NUM[i]=2}
       #   else if(datos_filt$VALUE[i]=="Regular"){VALUE_NUM[i]=3}
       #   else if(datos_filt$VALUE[i]=="Bueno"){VALUE_NUM[i]=4}
       #   else if(datos_filt$VALUE[i]=="Muy Bueno"){VALUE_NUM[i]=5}
       #   else{VALUE_NUM[i]=99}
       # }
       # datos_filt_res<- data.frame(datos_filt, VALUE_NUM)
       # datos_filt_res %>% group_by(PARTICIPANTE) %>% summarise(puntaje= round(mean(VALUE_NUM)))
       datos_res= datos_filt %>% count(VARIABLE,VALUE) 
       
       hchart(datos_res , "column",hcaes(x=VARIABLE, y=n, group=VALUE),
              stacking = list(enabled = TRUE),
              dataLabels = list(
                enabled = TRUE
                , # Añadir etiquetas
                #format = "<br>{point.percentage:.1f} %<br>total: {point.total}"
                format = "{point.percentage:.0f}%"
              )) %>% hc_add_theme(hc_theme_google())%>%
         hc_colors(c("#56147DFF","#C73D73FF", "red","orange","#FA7D5EFF","#FEAA74FF","#FCECAEFF","#02020DFF"))%>%
         hc_yAxis(title=list(text="")) %>% hc_xAxis(title=list(text=""))
      # %>% hc_legend(align="right",layout = "vertical",verticalAlign = "top",x = 0, y = 140)
       ##############
       library(readxl)
       ENCUESTA_DATOS <- read_excel("D:/INFORMACION T rev/ENCUESTA_DATOS.xlsx")
       View(ENCUESTA_DATOS)
       ### 1
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="control")
       persona<- as.data.frame(persona)
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,5,2,7,3,6,4)]
       #promedio
       unlist(as.vector(datos_res[1,2:6]))*c(1:5)
       rep(1:5,unlist(as.vector(datos_res[1,2:6])))
       round(mean(rep(1:5,unlist(as.vector(datos_res[1,2:6])))))
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Muy bajo"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Bajo"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Regular"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Bueno"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy Bueno"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Muy bajo"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Bajo"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Regular"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Bueno"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy Bueno"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       library(knitr)
       kable(generales)
       ######### pregunta terro
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="trafico_armas")
       persona<- as.data.frame(persona)
       unique(persona$VARIABLE)
       persona$VARIABLE= str_replace(persona$VARIABLE,"3. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,3,6,4,7,5,2)]
       # datos= persona %>% tabyl(VARIABLE, VALUE)%>%
       #   adorn_totals("col") %>%
       #   adorn_percentages("row") %>%
       #   adorn_pct_formatting(digits = 2) %>%
       #   adorn_ns()
       # datos<- datos[,c(1,3,6,4,7,5,2)]
       #
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       #promedio
       unlist(as.vector(datos_res[1,2:6]))*c(1:5)
       rep(1:5,unlist(as.vector(datos_res[1,2:6])))
       round(mean(rep(1:5,unlist(as.vector(datos_res[1,2:6])))))
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Muy bajo"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Bajo"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Regular"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Bueno"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy Bueno"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Muy bajo"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Bajo"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Regular"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Bueno"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy Bueno"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       ######### pregunta financ terro
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="finan_ter")
       persona<- as.data.frame(persona)
       persona$VARIABLE= str_replace(persona$VARIABLE,"2. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,3,6,4,7,5,2)]
       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,3,6,4,7,5,2)]
       #
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
       
       #### trafico armas
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="trafico_armas")
       persona<- as.data.frame(persona)
       persona$VARIABLE= str_replace(persona$VARIABLE,"3. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,3,6,4,7,5,2)]
       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,3,6,4,7,5,2)]
       kable(datos)
       #
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
       
       ####### por departamento
       #### la paz
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="La Paz")
       persona<- as.data.frame(persona)
       persona$VARIABLE= str_replace(persona$VARIABLE,"1. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,3,6,4,7,5,2)]
       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,3,6,4,7,5,2)]
       kable(datos)
       ##
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
       #### santa cruz
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="Santa Cruz")
       persona<- as.data.frame(persona)
       persona$VARIABLE= str_replace(persona$VARIABLE,"2. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,2,5,3,6,4)]
       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,2,5,3,6,4)]
       kable(datos)
       ##
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
       #### cochabamba
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="Cochabamba")
       persona<- as.data.frame(persona)
       persona$VARIABLE= str_replace(persona$VARIABLE,"3. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,4,2,5,3)]
       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,4,2,5,3)]
       kable(datos)
       ##
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(2:5,unlist(as.vector(datos_res[i,2:5])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
       #### cochabamba
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="Beni")
       persona<- as.data.frame(persona)
       persona$VARIABLE= str_replace(persona$VARIABLE,"4. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,3,6,4,7,5,2)]
       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,3,6,4,7,5,2)]
       kable(datos)
       ##
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
       #### cochabamba
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="Oruro")
       persona<- as.data.frame(persona)
       persona$VARIABLE= str_replace(persona$VARIABLE,"5. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,3,6,4,7,5,2)]
       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,3,6,4,7,5,2)]
       kable(datos)
       ##
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
       #### cochabamba
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="Pando")
       persona<- as.data.frame(persona)
       persona$VARIABLE= str_replace(persona$VARIABLE,"6. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,3,6,4,7,5,2)]
       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,3,6,4,7,5,2)]
       kable(datos)
       ##
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
       ##
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
       ############## potosi
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="Potosi")
       persona<- as.data.frame(persona)
       persona$VARIABLE= str_replace(persona$VARIABLE,"7. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,3,6,4,7,5,2)]
       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,3,6,4,7,5,2)]
       kable(datos)
       ##
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
       #### cochabamba
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="Tarija")
       persona<- as.data.frame(persona)
       persona$VARIABLE= str_replace(persona$VARIABLE,"8. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,3,6,4,7,5,2)]
       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,3,6,4,7,5,2)]
       kable(datos)
       ##
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
       #### cochabamba
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="Chuquisaca")
       persona<- as.data.frame(persona)
       persona$VARIABLE= str_replace(persona$VARIABLE,"9. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,3,6,4,7,5,2)]
       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,3,6,4,7,5,2)]
       kable(datos)
       ##
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
       ### dosier
      datos= Datos_estadisticas_Bolivia1 %>% uncount(n)
      datos_res= datos %>% tabyl(DEPARTAMENTO, TIPO_DELITO)%>%
        adorn_totals("col") %>%
        adorn_percentages("row") %>%
        adorn_pct_formatting(digits = 2) %>%
        adorn_ns()
       kable(datos_res)
       #promedio
       unlist(as.vector(datos_res[1,2:6]))*c(1:5)
       rep(1:5,unlist(as.vector(datos_res[1,2:6])))
       round(mean(rep(1:5,unlist(as.vector(datos_res[1,2:6])))))
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Muy bajo"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Bajo"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Regular"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Bueno"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy Bueno"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Muy bajo"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Bajo"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Regular"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Bueno"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy Bueno"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       
       ####
       datos_res1<-Datos_estadisticas_Bolivia %>% count(DEPARTAMENTO, TIPO_DELITO, wt=CANTIDAD)
       resumen<-datos_res1 %>% spread(TIPO_DELITO, value = n)
       resumen<-resumen %>% adorn_totals("row", name = "TOTALES")


       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,5,2,7,3,6,4,8)]
############# inicio de la nueva metodologia ############# 
       library(readxl)
       delitos_res1 <- read_excel("C:/Users/dmamaniq/Downloads/delitos_res.xlsx", 
                                 sheet = "Hoja1")
       Datos_estadisticas_Bolivia<- Datos_estadisticas_Bolivia %>% left_join(delitos_res1, by=c("TIPO_DELITO"="delito_ini"))
       Datos_estadisticas_Bolivia$DEPARTAMENTO= toupper(Datos_estadisticas_Bolivia$DEPARTAMENTO)
       
       #### la paz
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="La Paz")
       persona<- as.data.frame(persona)
       persona$VARIABLE= str_replace(persona$VARIABLE,"1. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       persona1$VARIABLE[persona1$VARIABLE=="Proxenestismo"]="Proxenetismo"
       persona1$VARIABLE[persona1$VARIABLE=="Tráfico ilícito de sustancias controladas."]="Uso indebido de información privilegiada"
       ##nuevo paso
       persona1_1<- persona1 %>% left_join(delitos_res,by=c("VARIABLE"="Delitos_Correspondientes"))
       persona1_1$VARIABLE[persona1_1$VARIABLE=="Tráfico ilícito de sustancias controladas."]="Tráfico ilícito de sustancias controladas"
       persona1_1<- persona1_1[,-1]
       persona1_1<- persona1_1[,c(3,1,2)]
       persona1_1$grupo_numeral[persona1_1$grupo_numeral==1]="Inseguridad,Crimen organizado y/o narcotráfico"
       persona1_1$grupo_numeral[persona1_1$grupo_numeral==2]="Delitos financieros y corrupción"
       persona1_1$grupo_numeral[persona1_1$grupo_numeral==3]="Control de información y comunicación"
       persona1_1<- persona1_1 %>% count(grupo_numeral, VALUE, wt=n)
       persona1_1<- persona1_1 %>% spread(VALUE, n)
       persona1_1[is.na(persona1_1)]<-0
       persona1_1<- persona1_1[,c(1,3,6,4,7,5,2)]
       porcentajes=persona1_1 %>% mutate(Improbable= Improbable*0)
       porcentajes=porcentajes %>% mutate(`Poco probable`= `Poco probable`*0.25)
       porcentajes=porcentajes %>% mutate(Indiferente= Indiferente*0.5)
       porcentajes=porcentajes %>% mutate(Probable= Probable*0.75)
       porcentajes=porcentajes %>% mutate(`Muy probable`= `Muy probable`*1)
       Indicador<-c(round(sum(porcentajes[1,2:6])/sum(persona1_1[1,2:6]),4)*100,round(sum(porcentajes[2,2:6])/sum(persona1_1[2,2:6]),4)*100,
                  round(sum(porcentajes[3,2:6])/sum(persona1_1[3,2:6]),4)*100)
       Indicador<- paste0(Indicador,"%")
       resumen<- data.frame(Grupo_Delito= porcentajes$grupo_numeral, Indicador)
       ##
       datos_res<- persona1 %>% spread(VALUE, n)
       datos_res[is.na(datos_res)]<-0
       datos_res<- datos_res[,c(1,3,6,4,7,5,2)]
       datos= persona %>% tabyl(VARIABLE, VALUE)%>%
         adorn_totals("col") %>%
         adorn_percentages("row") %>%
         adorn_pct_formatting(digits = 2) %>%
         adorn_ns()
       datos<- datos[,c(1,5,3,6,4,2,7)]
       kable(datos)
       ##
       ponderaciones<- NULL
       for (i in 1:nrow(datos_res)) {
         ponderaciones[i]<-round(mean(rep(1:5,unlist(as.vector(datos_res[i,2:6])))))
       }
       generales<- data.frame(DEPARTAMENTO= datos_res[,1], Percepcion=ponderaciones)
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]==1){generales$Percepcion[i]="Improbable"}
         else if(generales$Percepcion[i]==2){generales$Percepcion[i]="Poco probable"}
         else if(generales$Percepcion[i]==3){generales$Percepcion[i]="Indiferente"}
         else if(generales$Percepcion[i]==4){generales$Percepcion[i]="Probable"}
         else if(generales$Percepcion[i]==5){generales$Percepcion[i]="Muy probable"}
         else{generales$Percepcion[i]=99}
       }
       VALUE_NUM=NULL
       for (i in 1:nrow(generales)) {
         if(generales$Percepcion[i]=="Improbable"){VALUE_NUM[i]=1}
         else if(generales$Percepcion[i]=="Poco probable"){VALUE_NUM[i]=2}
         else if(generales$Percepcion[i]=="Indiferente"){VALUE_NUM[i]=3}
         else if(generales$Percepcion[i]=="Probable"){VALUE_NUM[i]=4}
         else if(generales$Percepcion[i]=="Muy probable"){VALUE_NUM[i]=5}
         else{VALUE_NUM[i]=99}
       }
       generales<- data.frame(generales, VALUE_NUM)
       kable(generales[,1:2])
###############
       library(readxl)
       #ENCUESTA_DATOS <- read_excel("D:/Documentos/ENCUESTA_DATOS.xlsx")
       ENCUESTA_DATOS <- read_excel("ENCUESTA_DATOS.xlsx")
       
       # delitos_res <- read_excel("D:/Documentos/delitos_res.xlsx", 
       #                           sheet = "Hoja2")
       delitos_res <- read_excel("delitos_res.xlsx", 
                                 sheet = "Hoja2")
       library(tidyverse)
       #### la paz
       resumen_unido<- data.frame()
       unique(ENCUESTA_DATOS$DESCRIPCION)
       persona<- ENCUESTA_DATOS %>% filter(DESCRIPCION=="Chuquisaca")
       persona<- as.data.frame(persona)
       unique(persona$VARIABLE)
       persona$VARIABLE= str_replace(persona$VARIABLE,"9. ","")
       persona1<-persona %>% count(VARIABLE, VALUE)
       persona1$VARIABLE[persona1$VARIABLE=="Proxenestismo"]="Proxenetismo"
       persona1$VARIABLE[persona1$VARIABLE=="Tráfico ilícito de sustancias controladas."]="Tráfico ilícito de sustancias controladas"
       ##nuevo paso
       persona1_1<- persona1 %>% left_join(delitos_res,by=c("VARIABLE"="Delitos_Correspondientes"))
       #persona1_1$VARIABLE[persona1_1$VARIABLE=="Tráfico ilícito de sustancias controladas."]="Tráfico ilícito de sustancias controladas"
       persona1_1<- persona1_1[,-1]
       persona1_1<- persona1_1[,c(3,1,2)]
       persona1_1$grupo_numeral[persona1_1$grupo_numeral==1]="Legitimación de ganancias ilícitas"
       persona1_1$grupo_numeral[persona1_1$grupo_numeral==2]="Financiamiento del terrorismo"
       persona1_1$grupo_numeral[persona1_1$grupo_numeral==3]="Financiamiento de la proliferación de armas de destrucción masiva"
       persona1_1<- persona1_1 %>% count(grupo_numeral, VALUE, wt=n)
       persona1_1<- persona1_1 %>% spread(VALUE, n)
       persona1_1[is.na(persona1_1)]<-0
       persona1_1<- persona1_1[,c(1,3,6,4,7,5,2)] #la paz beni oruro pando
       #persona1_1<- persona1_1[,c(1,2,5,3,6,4)] # santa cruz
       #persona1_1<- persona1_1[,c(1,4,2,5,3)] # cocha
       sum(persona1_1[1,2:6])
       library(knitr)
       kable(persona1_1)
       porcentajes=persona1_1 %>% mutate(Improbable= Improbable*0)
       porcentajes=porcentajes %>% mutate(`Poco probable`= `Poco probable`*0.25)
       porcentajes=porcentajes %>% mutate(Indiferente= Indiferente*0.5)
       porcentajes=porcentajes %>% mutate(Probable= Probable*0.75)
       porcentajes=porcentajes %>% mutate(`Muy probable`= `Muy probable`*1)
       Indicador<-c(round(sum(porcentajes[1,2:6])/sum(persona1_1[1,2:6]),4)*100,round(sum(porcentajes[2,2:6])/sum(persona1_1[2,2:6]),4)*100,
                    round(sum(porcentajes[3,2:6])/sum(persona1_1[3,2:6]),4)*100)
       Indicador<- paste0(Indicador,"%")
       resumen<- data.frame(Grupo_Delito= porcentajes$grupo_numeral, Indicador)
       kable(resumen)
       #
       Indicador1<-c(round(sum(porcentajes[1,2:6])/sum(persona1_1[1,2:6]),4),round(sum(porcentajes[2,2:6])/sum(persona1_1[2,2:6]),4),
                     round(sum(porcentajes[3,2:6])/sum(persona1_1[3,2:6]),4))
       resumen1<- data.frame(Departamento=unique(persona$DESCRIPCION),Grupo_Delito= porcentajes$grupo_numeral, Indicador1)
       resumen1<- resumen1 %>% mutate(w1=0.3)
       resumen_unido<- rbind(resumen_unido, resumen1)
       ### a nivel nacional
       library(readxl)
       #load("D:/Documentos/mapa_v1_DMQ/Datos_estadisticas_Bolivia.RData")
       POBLACION_BOLIVIA <- read_excel("POBLACION_BOLIVIA.xlsx")
       POBLACION_BOLIVIA<- as.data.frame(POBLACION_BOLIVIA)
       POBLACION_BOLIVIA$DEPARTAMENTO_MUNICIPIO[POBLACION_BOLIVIA$DEPARTAMENTO_MUNICIPIO=="POTOSÍ"]="POTOSI"
       datos<- POBLACION_BOLIVIA %>% filter(DEPARTAMENTO_MUNICIPIO %in% c("CHUQUISACA","LA PAZ","COCHABAMBA","SANTA CRUZ","PANDO","BENI","POTOSI","TARIJA","ORURO") )
       #load("C:/Users/DEYVIS/Documents/Datos_estadisticas_Bolivia.RData")
       # delitos_res1 <- read_excel("D:/Documentos/delitos_res.xlsx", 
       #                            sheet = "Hoja1")
       delitos_res1 <- read_excel("delitos_res.xlsx", 
                                 sheet = "Hoja1")
       Datos_estadisticas_Bolivia <- read_excel("Datos_estadisticas_Bolivia.xlsx")
       Datos_estadisticas_Bolivia1<- Datos_estadisticas_Bolivia %>% left_join(delitos_res1, by=c("TIPO_DELITO"="delito_ini"))
       Datos_estadisticas_Bolivia1<- Datos_estadisticas_Bolivia1 %>% filter(TIPO_DELITO!="CASOS PENALES POR DELITOS DE CORRUPCION")
       resumen<-Datos_estadisticas_Bolivia1 %>% count(DEPARTAMENTO, Grupo_Correspondiente, wt= CANTIDAD)
       resumen_1<- resumen %>% filter(DEPARTAMENTO=="LA PAZ")
       resumen_1<-resumen_1 %>% left_join(datos, by=c("DEPARTAMENTO"="DEPARTAMENTO_MUNICIPIO"))
       resumen_1<- resumen_1 %>% mutate(tasa= round(n/`Población Total`,7))
       resumen_1<- resumen_1 %>% mutate(tasa_porcentaje= paste0(tasa*100,"%"))
       resumen_1<- resumen_1 %>% mutate(umbral= ifelse(DEPARTAMENTO=="SANTA CRUZ"|DEPARTAMENTO=="LA PAZ"|
                                                       DEPARTAMENTO=="COCHABAMBA",tasa*2, tasa*3))
       resumen_1<- resumen_1 %>% mutate(umbral_porcentaje= paste0(umbral*100,"%"))
       resumen_1<- resumen_1 %>% mutate(tasa_normalizada= tasa/umbral)
       library(knitr)
       kable(resumen_1)
       
       
       
       resumen_unido1<- cbind(resumen_unido, tasa_norm=resumen_1[,c(1,9)])
       resumen_unido1<- resumen_unido1 %>% mutate(w2= 0.7)
       
       
       resumen_unido1<- resumen_unido %>% mutate(umbral= ifelse(DEPARTAMENTO=="SANTA CRUZ"|DEPARTAMENTO=="LA PAZ"|
                                                         DEPARTAMENTO=="COCHABAMBA",tasa*2, tasa*3))
       #########################
       agente<- c("comercial1","comercial2","comercial3")
       comerciales<- as.data.frame(agente)
       numero<- c("123","456","789","111","222")
       ###
       # Definir los agentes
       agentes <- c("comercial1", "comercial2", "comercial3")
       
       # Generar un vector de números aleatorios (por ejemplo, 10 números entre 1 y 100)
       set.seed(123)  # Para reproducibilidad
       numeros_aleatorios <- sample(1:100, 10, replace = TRUE)
       
       # Inicializar un data frame para distribuir los números entre los agentes
       distribucion <- data.frame(agente = rep(NA, length(numeros_aleatorios)),
                                  numero = numeros_aleatorios)
       
       # Contadores de asignación para cada agente
       asignaciones <- rep(0, length(agentes))
       
       # Distribuir los números aleatorios de manera equitativa
       for (i in 1:length(numeros_aleatorios)) {
         # Encontrar el agente con el menor número total de asignaciones
         agente_asignado <- agentes[which.min(asignaciones)]
         
         # Asignar el número al agente
         distribucion$agente[i] <- agente_asignado
         
         # Actualizar el contador de asignación para ese agente
         asignaciones[which(agentes == agente_asignado)] <- asignaciones[which(agentes == agente_asignado)] + numeros_aleatorios[i]
       }
       
       # Mostrar la distribución final
       print(distribucion)
       
       # Ver las sumas finales de asignación por agente
       cat("\nAsignaciones finales por agente:\n")
       print(data.frame(agente = agentes, asignacion = asignaciones))
       ############## otra opcion ############
       ######################################
       # Definir los agentes
       agentes <- c("comercial1", "comercial2", "comercial3")
       
       # Generar un vector inicial de números aleatorios (por ejemplo, 10 números entre 1 y 100)
       set.seed(123)  # Para reproducibilidad
       numeros_aleatorios_iniciales <- sample(1:100, 10, replace = TRUE)
       
       # Función para distribuir los números entre los agentes de forma equitativa en cantidad
       distribuir_numeros <- function(numeros_aleatorios, agentes, asignaciones_previas = NULL) {
         # Si no hay asignaciones previas, inicializamos a 0
         if (is.null(asignaciones_previas)) {
           asignaciones_previas <- rep(0, length(agentes))  # Cantidad de números asignados
           numeros_asignados <- list(comercial1 = numeric(0), comercial2 = numeric(0), comercial3 = numeric(0))
         } else {
           # Si ya hay asignaciones previas, inicializamos el historial de números asignados
           numeros_asignados <- list(
             comercial1 = asignaciones_previas[1],
             comercial2 = asignaciones_previas[2],
             comercial3 = asignaciones_previas[3]
           )
         }
         
         # Inicializamos un data frame para almacenar la distribución
         distribucion <- data.frame(agente = rep(NA, length(numeros_aleatorios)),
                                    numero = numeros_aleatorios)
         
         # Distribuir los números de forma equitativa por cantidad
         for (i in 1:length(numeros_aleatorios)) {
           # Encontrar el agente con la menor cantidad de números asignados
           agente_asignado <- agentes[which.min(sapply(numeros_asignados, length))]
           
           # Asignar el número al agente
           distribucion$agente[i] <- agente_asignado
           
           # Agregar el número asignado al historial de ese agente
           numeros_asignados[[agente_asignado]] <- c(numeros_asignados[[agente_asignado]], numeros_aleatorios[i])
         }
         
         # Devolver el resultado
         return(list(distribucion = distribucion, 
                     numeros_asignados = numeros_asignados))
       }
       
       # Distribuir los números iniciales
       resultado_inicial <- distribuir_numeros(numeros_aleatorios_iniciales, agentes)
       
       # Mostrar la distribución inicial
       cat("Distribución inicial:\n")
       print(resultado_inicial$distribucion)
       
       # Ver los números asignados a cada comercial (inicial)
       cat("\nNúmeros asignados a cada comercial (inicial):\n")
       print(resultado_inicial$numeros_asignados)
       
       # Ahora supongamos que tenemos una nueva lista de números
       numeros_aleatorios_nuevos <- sample(100:500, 5, replace = TRUE)
       
       # Distribuir los nuevos números considerando las asignaciones previas
       resultado_nuevos <- distribuir_numeros(numeros_aleatorios_nuevos, agentes, 
                                              sapply(resultado_inicial$numeros_asignados, length))
       
       # Mostrar la distribución de los nuevos números
       cat("\nDistribución de los nuevos números:\n")
       print(resultado_nuevos$distribucion)
       
       # Ver los números asignados a cada comercial (final)
       cat("\nNúmeros asignados a cada comercial (final):\n")
       print(resultado_nuevos$numeros_asignados)
       
       # Ver la cantidad total de números asignados por comercial
       cat("\nCantidad total de números asignados por comercial:\n")
       cantidad_asignada <- sapply(resultado_nuevos$numeros_asignados, length)
       print(data.frame(agente = agentes, cantidad_asignada = cantidad_asignada))
       ############# metodo funciona 2 ####################
       # Definir los agentes
       agentes <- c("comercial1", "comercial2", "comercial3")
       
       # Generar un vector inicial de números aleatorios (por ejemplo, 10 números entre 1 y 100)
       set.seed(123)  # Para reproducibilidad
       numeros_aleatorios_iniciales <- sample(10000:20000, 10, replace = TRUE)
       
       # Función para distribuir los números entre los agentes de forma equitativa en cantidad
       distribuir_numeros <- function(numeros_aleatorios, agentes, asignaciones_previas = NULL) {
         # Si no hay asignaciones previas, inicializamos a 0
         if (is.null(asignaciones_previas)) {
           asignaciones_previas <- rep(0, length(agentes))  # Cantidad de números asignados
           numeros_asignados <- list(comercial1 = numeric(0), comercial2 = numeric(0), comercial3 = numeric(0))
         } else {
           # Si ya hay asignaciones previas, inicializamos el historial de números asignados
           numeros_asignados <- list(
             comercial1 = asignaciones_previas[1],
             comercial2 = asignaciones_previas[2],
             comercial3 = asignaciones_previas[3]
           )
         }
         
         # Inicializamos un dataframe para almacenar la distribución
         distribucion <- data.frame(agente = character(0), numero = numeric(0), stringsAsFactors = FALSE)
         
         # Distribuir los números de forma equitativa por cantidad
         for (i in 1:length(numeros_aleatorios)) {
           # Encontrar el agente con la menor cantidad de números asignados
           agente_asignado <- agentes[which.min(sapply(numeros_asignados, length))]
           
           # Asignar el número al agente
           distribucion <- rbind(distribucion, data.frame(agente = agente_asignado, numero = numeros_aleatorios[i]))
           
           # Agregar el número asignado al historial de ese agente
           numeros_asignados[[agente_asignado]] <- c(numeros_asignados[[agente_asignado]], numeros_aleatorios[i])
         }
         
         # Devolver el resultado
         return(list(distribucion = distribucion, 
                     numeros_asignados = numeros_asignados))
       }
       
       # Distribuir los números iniciales
       resultado_inicial <- distribuir_numeros(numeros_aleatorios_iniciales, agentes)
       
       # Mostrar la distribución inicial
       cat("Distribución inicial:\n")
       print(resultado_inicial$distribucion)
       
       # Ver los números asignados a cada comercial (inicial)
       cat("\nNúmeros asignados a cada comercial (inicial):\n")
       print(resultado_inicial$numeros_asignados)
       
       # Ahora supongamos que tenemos una nueva lista de números
       numeros_aleatorios_nuevos <- sample(10000:20000, 5, replace = TRUE)
       
       # Distribuir los nuevos números considerando las asignaciones previas
       resultado_nuevos <- distribuir_numeros(numeros_aleatorios_nuevos, agentes, 
                                              sapply(resultado_inicial$numeros_asignados, length))
       
       # Mostrar la distribución de los nuevos números
       cat("\nDistribución de los nuevos números:\n")
       print(resultado_nuevos$distribucion)
       
       # Ver los números asignados a cada comercial (final)
       cat("\nNúmeros asignados a cada comercial (final):\n")
       print(resultado_nuevos$numeros_asignados)
       
       # Ver la cantidad total de números asignados por comercial
       cat("\nCantidad total de números asignados por comercial:\n")
       cantidad_asignada <- sapply(resultado_nuevos$numeros_asignados, length)
       print(data.frame(agente = agentes, cantidad_asignada = cantidad_asignada))
       
       # Ver todos los números asignados en un único dataframe
       cat("\nTodos los números asignados:\n")
       todos_los_numeros <- rbind(resultado_inicial$distribucion, resultado_nuevos$distribucion)
       print(todos_los_numeros)
       ###
       numeros_aleatorios_nuevos1 <- sample(1:100,3, replace = TRUE)
       
       # Distribuir los nuevos números considerando las asignaciones previas
       resultado_nuevos <- distribuir_numeros(numeros_aleatorios_nuevos1, agentes, 
                                              sapply(resultado_inicial$numeros_asignados, length))
       
       # Mostrar la distribución de los nuevos números
       cat("\nDistribución de los nuevos números:\n")
       print(resultado_nuevos$distribucion)
       
       # Ver los números asignados a cada comercial (final)
       cat("\nNúmeros asignados a cada comercial (final):\n")
       print(resultado_nuevos$numeros_asignados)
       
       # Ver la cantidad total de números asignados por comercial
       cat("\nCantidad total de números asignados por comercial:\n")
       cantidad_asignada <- sapply(resultado_nuevos$numeros_asignados, length)
       print(data.frame(agente = agentes, cantidad_asignada = cantidad_asignada))
       
       # Ver todos los números asignados en un único dataframe
       cat("\nTodos los números asignados:\n")
       todos_los_numeros <- rbind(todos_los_numeros, resultado_nuevos$distribucion)
       print(todos_los_numeros)
       
       
       ########### otro metodo ###########
       # Definir los agentes
       agentes <- c("comercial1", "comercial2", "comercial3")
       
       # Generar un vector inicial de números aleatorios (por ejemplo, 10 números entre 1 y 100)
       set.seed(123)  # Para reproducibilidad
       numeros_aleatorios_iniciales <- sample(10000:20000, 10, replace = TRUE)
       
       # Función para distribuir los números entre los agentes de forma equitativa en cantidad
       distribuir_numeros <- function(numeros_aleatorios, agentes, asignaciones_previas = NULL) {
         # Si no hay asignaciones previas, inicializamos a 0
         if (is.null(asignaciones_previas)) {
           asignaciones_previas <- rep(0, length(agentes))  # Cantidad de números asignados
           numeros_asignados <- list(comercial1 = numeric(0), comercial2 = numeric(0), comercial3 = numeric(0))
         } else {
           # Si ya hay asignaciones previas, inicializamos el historial de números asignados
           numeros_asignados <- list(
             comercial1 = asignaciones_previas[1],
             comercial2 = asignaciones_previas[2],
             comercial3 = asignaciones_previas[3]
           )
         }
         
         # Inicializamos un dataframe para almacenar la distribución
         distribucion <- data.frame(agente = character(0), numero = numeric(0), stringsAsFactors = FALSE)
         
         # Total de números que deben ser distribuidos (numeros_aleatorios)
         total_numeros <- length(numeros_aleatorios)
         
         # Función para calcular el agente con menos números asignados
         agente_menos_asignado <- function(numeros_asignados) {
           # Devuelve el agente con menos números asignados
           agente_index <- which.min(sapply(numeros_asignados, length))
           return(names(numeros_asignados)[agente_index])
         }
         
         # Función para asegurar que la distribución sea equilibrada
         equilibrar_distribucion <- function(numeros_asignados) {
           # Calculamos la cantidad total de números y la cantidad que deberían recibir los agentes idealmente
           total_numeros <- sum(sapply(numeros_asignados, length))
           ideal_asignacion <- total_numeros %/% length(numeros_asignados)
           sobra <- total_numeros %% length(numeros_asignados)
           
           # Redistribuir los números, asegurando que la diferencia entre los agentes sea 1 como máximo
           distribucion_final <- list()
           for (i in 1:length(numeros_asignados)) {
             distribucion_final[[i]] <- rep(ideal_asignacion, length(numeros_asignados[[i]]))
             if (i <= sobra) {
               distribucion_final[[i]] <- c(distribucion_final[[i]], 1)
             }
           }
           return(distribucion_final)
         }
         
         # Distribuir los números de forma equitativa por cantidad
         for (i in 1:total_numeros) {
           # Priorizar al agente con menos números asignados
           agente_asignado <- agente_menos_asignado(numeros_asignados)
           
           # Asignar el número al agente
           distribucion <- rbind(distribucion, data.frame(agente = agente_asignado, numero = numeros_aleatorios[i]))
           
           # Agregar el número asignado al historial de ese agente
           numeros_asignados[[agente_asignado]] <- c(numeros_asignados[[agente_asignado]], numeros_aleatorios[i])
         }
         
         # Redistribuir para equilibrar la asignación entre los agentes
         distribucion_final <- equilibrar_distribucion(numeros_asignados)
         
         # Devolver el resultado
         return(list(distribucion = distribucion, 
                     numeros_asignados = numeros_asignados))
       }
       
       # Distribuir los números iniciales
       resultado_inicial <- distribuir_numeros(numeros_aleatorios_iniciales, agentes)
       resultado_inicial$distribucion
       # Mostrar la distribución inicial
       # cat("Distribución inicial:\n")
       # print(resultado_inicial$distribucion)
       # grupo1<- resultado_inicial$distribucion
       # grupo1<-grupo1 %>% count(agente, sort = T)
       # Ver los números asignados a cada comercial (inicial)
       #cat("\nNúmeros asignados a cada comercial (inicial):\n")
       #print(resultado_inicial$numeros_asignados)
       # resultado_nuevos <- distribuir_numeros(numeros_aleatorios_iniciales, agentes, 
       #                                        sapply(resultado_inicial$numeros_asignados, length))
       # resultado_nuevos$distribucion
       # Ahora supongamos que tenemos una nueva lista de números
       numeros_aleatorios_nuevos <- sample(10000:20000, 5, replace = TRUE)
       
       # Redistribuir los nuevos números considerando las asignaciones previas
       resultado_nuevos <- distribuir_numeros(numeros_aleatorios_nuevos, agentes, 
                                              sapply(resultado_inicial$numeros_asignados, length))
       
       # Mostrar la distribución de los nuevos números
       # cat("\nDistribución de los nuevos números:\n")
       # print(resultado_nuevos$distribucion)
       #resultado_nuevos$distribucion$agente
       resultado_nuevos$distribucion$agente[resultado_nuevos$distribucion$agente=="comercial1"]="x"
       resultado_nuevos$distribucion$agente[resultado_nuevos$distribucion$agente=="comercial3"]="comercial1"
       resultado_nuevos$distribucion$agente[resultado_nuevos$distribucion$agente=="x"]="comercial3"
       # grupo2<- resultado_nuevos$distribucion$agente
       # grupo2<-grupo2 %>% count(agente, sort = T)
       # grupo2$agente[grupo2$agente=="comercial1"]="x"
       # grupo2$agente[grupo2$agente=="comercial3"]="comercial1"
       # grupo2$agente[grupo2$agente=="x"]="comercial3"
       # Ver los números asignados a cada comercial (final)
       cat("\nNúmeros asignados a cada comercial (final):\n")
       print(resultado_nuevos$numeros_asignados)
       
       # Ver la cantidad total de números asignados por comercial
       cat("\nCantidad total de números asignados por comercial:\n")
       cantidad_asignada <- sapply(resultado_nuevos$numeros_asignados, length)
       print(data.frame(agente = agentes, cantidad_asignada = cantidad_asignada))
       
       # Ver todos los números asignados en un único dataframe
       cat("\nTodos los números asignados:\n")
       datos_consolidados <- rbind(resultado_inicial$distribucion, resultado_nuevos$distribucion)
       print(todos_los_numeros)
       todos_los_numeros %>% count(agente)
       
       ####
       agentes <- c("comercial1", "comercial2", "comercial3")
       datos_consolidados<- data.frame()
       numeros_aleatorios_iniciales <- sample(10000:20000, 7, replace = TRUE)
       resultado_inicial <- distribuir_numeros(numeros_aleatorios_iniciales, agentes)
       if(nrow(datos_consolidados)!=0){
         resumen<-datos_consolidados %>% count(agente, sort = T)
         print(resumen)
         # resumen<-resultado_inicial$distribucion %>% count(agente) 
         # resumen<- resumen %>% mutate(corre=1:nrow)
         if(length(numeros_aleatorios_iniciales)==1){
           resultado_inicial <- distribuir_numeros(numeros_aleatorios_iniciales, resumen$agente[3])
           resultado_inicial$distribucion$agente<-resumen$agente[3]
         }
         else if(length(numeros_aleatorios_iniciales)==2){
           resultado_inicial <- distribuir_numeros(numeros_aleatorios_iniciales, resumen$agente[2:3])
           resultado_inicial$distribucion$agente[1]<-resumen$agente[2]
           resultado_inicial$distribucion$agente[2]<-resumen$agente[3]
         }
         else{
           indice<-resultado_inicial$distribucion %>% count(agente, sort = T)
           resultado_inicial$distribucion$agente[resultado_inicial$distribucion$agente==
                                                   resumen$agente[1]]="x"
           resultado_inicial$distribucion$agente[resultado_inicial$distribucion$agente==
                                                   indice$agente[1]]=resumen$agente[3]
           resultado_inicial$distribucion$agente[resultado_inicial$distribucion$agente==
                                                   indice$agente[2]]=resumen$agente[2]
           resultado_inicial$distribucion$agente[resultado_inicial$distribucion$agente=="x"]=resumen$agente[1]
          
         }

       }
       datos_consolidados<-rbind(datos_consolidados,resultado_inicial$distribucion)
       datos_consolidados %>% count(agente, sort = T)
       
       
       
       
       #### PREGUNTA 1
       url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/aptos2015'
       datos <- read.table(file=url, header=T)
       tabla2 <- table(datos$parqueadero, datos$estrato)
       barplot(tabla2, beside = TRUE, las=1,
               xlab='Estrato', ylab='Frecuencia',
               col = c("lightblue", "mistyrose"),
               ylim = c(0, 250))
       legend('topleft', legend=rownames(tabla2), bty='n',
              fill=c("lightblue", "mistyrose"))
       
       #### PREGUNTA 2
       # Función para verificar si un número es primo
       es_primo <- function(num) {
         if (num <= 1) return(FALSE)
         for (i in 2:sqrt(num)) {
           if (num %% i == 0) return(FALSE)
         }
         return(TRUE)
       }
       
       # Generar lista de números primos hasta 50
       n <- 50
       primos <- c()
       i <- 2
       while (i <= n) {
         if (es_primo(i)) {
           primos <- c(primos, i)
         }
         i <- i + 1
       }
       
       primos
       ###### PREGUNTA 3
       # Función para clasificar por edad
       clasificar_edad <- function(edad) {
         if (edad < 12) {
           return("Niño")
         } else if (edad >= 12 & edad <= 17) {
           return("Adolescente")
         } else if (edad >= 18 & edad <= 64) {
           return("Adulto")
         } else {
           return("Adulto mayor")
         }
       }
       
       # Lista de edades
       edades <- c(10, 15, 25, 70)
       
       # Clasificar las edades
       clasificaciones <- sapply(edades, clasificar_edad)
       clasificaciones
       
       ######## PREGUNTA 4
       # Instalar y cargar tidyr si no está instalado
       # install.packages("tidyr")
       library(tidyr)
       
       # Crear un dataframe ficticio
       resultados <- data.frame(
         estudiante = c("Juan", "Ana", "Luis"),
         matematica = c(80, 90, 85),
         lengua = c(75, 88, 92),
         ciencias = c(85, 95, 80)
       )
       
       # Usar gather para transformar a formato largo
       resultados_largo <- gather(resultados, key = "asignatura", value = "nota", matematica:ciencias)
       
       # Usar spread para restaurar al formato original
       resultados_ancho <- spread(resultados_largo, key = "asignatura", value = "nota")
       
       resultados_largo
       resultados_ancho
       ####### pregunta 5
       # Convertir las columnas mpg, cyl y hp a formato largo
       mtcars_long <- mtcars %>%
         pivot_longer(cols = c(mpg, cyl, hp), names_to = "variable", values_to = "valor")
       
       mtcars_long
view(mtcars)       
       
       
       
       
       
       
       
       #1. mediante programacion Calcular la Raíz Cuadrada de un Número
        # Aproximar la raíz cuadrada de un número usando un bucle while
        aproximar_raiz <- function(n) {
              estimacion <- n / 2
        while (abs(estimacion^2 - n) > 0.0001) {
           estimacion <- (estimacion + n / estimacion) / 2
            }
         return(estimacion)
          }

       # Aproximar la raíz cuadrada de 16
          aproximar_raiz(16)
           
          
          #2. Crear una Función para verificar si un número es primo
          es_primo <- function(num) {
            if (num <= 1) return(FALSE)
            for (i in 2:sqrt(num)) {
              if (num %% i == 0) return(FALSE)
            }
            return(TRUE)
          }
          
          # Generar lista de números primos hasta 50
          n <- 50
          primos <- c()
          i <- 2
          while (i <= n) {
            if (es_primo(i)) {
              primos <- c(primos, i)
            }
            i <- i + 1
          }
          
          primos
          
          
          #3 Cree una funcion para Contar los Dígitos de un Número
          # Contar los dígitos de un número
          contar_digitos <- function(num) {
            contador <- 0
            while (num > 0) {
              num <- floor(num / 10)
              contador <- contador + 1
            }
            return(contador)
          }
          
          # Contar los dígitos del número 12345
          contar_digitos(12345)
          ################ key api chat gpt #################
          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
          
          library(devtools)
          install_github("isinaltinkaya/gptchatteR")
          library(gptchatteR)
          library(openai)
          openai::create_completion(# Aquí ya se pasa el modelo
            model = "gpt-3.5-turbo-instruct"        # Y aquí otra vez, causando el error
          )
          
          chatter.auth("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")
          chatter.create()
          respuesta<- chatter.chat("librearias de R para interactuar con la api de chatgpt", return_response = TRUE)
          
          ######
          library(httr)
          library(jsonlite)
          
          api_key <- "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"
          url <- "https://api.openai.com/v1/chat/completions"
          
          body <- list(
            model = "gpt-3.5-turbo",  # Asegúrate de especificar el modelo
            messages = list(
              list(role = "system", content = "Eres un asistente útil."),
              list(role = "user", content = "Enlista las librerías de R para interactuar con ChatGPT.")
            ),
            max_tokens = 100  # Ajusta según lo necesario
          )
          response <- POST(
            url,
            add_headers(Authorization = paste("Bearer", api_key)),
            body = toJSON(body, auto_unbox = TRUE),
            encode = "json"
          )
          
          # Ver el contenido de la respuesta
          cat(content(response, "text", encoding = "UTF-8"))
          
          ############## otro
          chatGPT_API  <- "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"
          
          chatGPT_response <- POST(
            # use chatGPT website (you can copy paste)
            url = "https://api.openai.com/v1/chat/completions",
            # Authorize
            add_headers(Authorization = paste("Bearer", chatGPT_API)),
            # Output type: use JSON
            content_type_json(),
            # encode the value to json format
            encode = "json",
            # Controlling what to show as the output, it's going to be a list of following things
            body = list(
              model = "gpt-3.5-turbo", # Use gpt-3.5 is very fast
              messages = list(list(role = "user", content = "por que se puede aplicar el aloe vera en la piel"))
            )
          )
          # Print chatGPT's Answer
          respuesta<-content(chatGPT_response)
          respuesta$choices[[1]]$message$content
          ##############
          library(chatgpt)
          Sys.setenv(OPENAI_API_KEY = "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")
          respuesta<-ask_chatgpt("como completar el codigo en R en los scripts?")
          ###########
          ## Not run: 
          complete_code("# A function to square each element of a vector\nsquare_each <- function(")
          Sys.setenv("OPENAI_RETURN_LANGUAGE" = "Español")
          es_primo <- function(num) {
            if (num <= 1) {
              return(FALSE)
            }
            for (i in 2:(num-1)) {
              if (num %% i == 0) {
                return(FALSE)
              }
            }
            return(TRUE)
          }
          
          numeros_primos <- function(limite) {
            resultado <- c()
            for (i in 2:limite) {
              if (es_primo(i)) {
                resultado <- c(resultado, i)
              }
            }
            return(resultado)
          }
          
          limite <- 100  # Puedes ajustar este límite según tus necesidades
          print(numeros_primos(limite))

       ###############
          library(httr)
          library(jsonlite)
          library(tidyverse)
          chatGPT_API<-"AIzaSyB6BURFz8pQNKuI9Epon0xhcpIjSRr7kJ0"
          chatgpt_response <- function(prompt) {
            chatGPT_response <- POST(
              # use chatGPT website (you can copy paste)
              url = "https://generativelanguage.googleapis.com/v1beta/models/",
              # Authorize
              add_headers(Authorization = paste("Bearer", chatGPT_API)),
              # Output type: use JSON
              content_type_json(),
              # encode the value to json format
              encode = "json",
              # Controlling what to show as the output, it's going to be a list of following things
              body = list(
                model = "gemini-1.5-flash", # Use gpt-3.5 is very fast
                messages = list(list(role = "user", content = prompt))
              )
            )
            
            respuesta <- content(chatGPT_response)
            return(respuesta$choices[[1]]$message$content)
          }
          chatgpt_response("hola")
          library(httr)
          library(jsonlite)
          
          chatgpt_response <- function(prompt) {
            api_key <- "AIzaSyB6BURFz8pQNKuI9Epon0xhcpIjSRr7kJ0"
            url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=", api_key)
            
            body <- list(
              contents = list(
                list(role = "user", parts = list(list(text = prompt)))
              )
            )
            
            response <- POST(
              url = url,
              body = toJSON(body, auto_unbox = TRUE),
              content_type_json()
            )
            
            # Procesar la respuesta
            respuesta <- content(response, as = "parsed")
            
            # Extraer el contenido del modelo
            if (!is.null(respuesta$candidates[[1]]$content$parts[[1]]$text)) {
              return(respuesta$candidates[[1]]$content$parts[[1]]$text)
            } else {
              return("Error en la respuesta")
            }
          }
          
          # Ejemplo de uso
          resultado <- chatgpt_response("¿Qué es la inteligencia artificial?")
          print(resultado)
          
          ##
          library(httr)
          library(jsonlite)
          
          gemini_response <- function(prompt, api_key) {  # Añade api_key como argumento
            url <- "https://generativelanguage.googleapis.com/v1beta/models/" # Endpoint correcto para Gemini
            headers <- add_headers(Authorization = paste0("Bearer ", api_key),  # Usa Bearer token
                                   "Content-Type" = "application/json")
            body <- list(prompt = list(text = prompt)) # Estructura del body correcta para Gemini
            
            response <- POST(url, headers = headers, body = toJSON(body, auto_unbox = TRUE))
            
            if (http_error(response)) { # Manejo de errores
              print(content(response)) # Imprime el error para depuración
              return(NULL) # Devuelve NULL en caso de error
            }
            
            respuesta <- content(response)
            
            # Extracción del texto generado (adapta según la respuesta de Gemini)
            if (!is.null(respuesta$candidates) && length(respuesta$candidates) > 0) {
              return(respuesta$candidates[[1]]$output)
            } else {
              print(respuesta) # Imprime la respuesta completa para depuración
              return(NULL)
            }
          }
          
          
          # Ejemplo de uso (RECUERDA REEMPLAZAR con tu API Key real):
          api_key <- "YOUR_API_KEY" # Reemplaza con tu clave de API real
          respuesta <- gemini_response("hola", "AIzaSyB6BURFz8pQNKuI9Epon0xhcpIjSRr7kJ0")
          print(respuesta)
          ##
          gemini_api <- function(prompt, api_key, model_version = "gemini-pro") {
            url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_version, ":generateText")
            headers <- add_headers(Authorization = paste0("Bearer ", api_key),
                                   "Content-Type" = "application/json")
            body <- list(prompt = list(text = prompt))
            
            response <- POST(url, headers = headers, body = toJSON(body, auto_unbox = TRUE))
            content(response)$candidates[[1]]$output
          }
          prompt <- "Escribe un poema sobre el otoño."
          api_key <- "AIzaSyB6BURFz8pQNKuI9Epon0xhcpIjSRr7kJ0" # Reemplaza con tu clave de API real
          model_version <- "gemini-pro" # Especifica la versión del modelo
          
          texto_generado <- gemini_api(prompt, api_key, model_version)





          library(gemini.R)
          
          setAPI("AIzaSyB6BURFz8pQNKuI9Epon0xhcpIjSRr7kJ0") # check https://makersuite.google.com/app/apikey
         respuesta_gemini<- gemini(paste("Eres un vendedor cordial de una empresa que vende repuestos para vehículos.",
                                         "La empresa tiene las siguientes políticas:",
                                         "Misión: Ofrecer repuestos de calidad para autos de diversas marcas.",
                                         "Visión: Ser líderes en la distribución de repuestos en el mercado local.",
                                         "Todas las respuestas deben ser amables y en base a las políticas de la empresa.",
                                         "El cliente ha solicitado los siguientes repuestos:","3 motores de 500 dolares, 2 inyectores a diesel
                                         para toyota"))
         respuesta_gemini 
          # text 
          # "Gemini, the third astrological sign, is associated with 
          # communication, adaptability, and a thirst for knowledge." 
         Sys.getenv(api_key <-"AIzaSyB6BURFz8pQNKuI9Epon0xhcpIjSRr7kJ0")
         chatgpt_response <- function(prompt) {
           
           url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent?key=", api_key)
           
           body <- list(
             contents = list(
               list(role = "user", parts = list(list(text = prompt)))
             )
           )
           
           response <- POST(
             url = url,
             body = toJSON(body, auto_unbox = TRUE),
             content_type_json()
           )
           
           # Procesar la respuesta
           respuesta <- content(response, as = "parsed")
           
           # Extraer el contenido del modelo
           if (!is.null(respuesta$candidates[[1]]$content$parts[[1]]$text)) {
             return(respuesta$candidates[[1]]$content$parts[[1]]$text)
           } else {
             return("Error en la respuesta")
           }
         }




















