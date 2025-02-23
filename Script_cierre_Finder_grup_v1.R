# script cierre finder recurrentes
load("datos_prueba.RData")
library(readxl)
library(tidyverse)
Alertas_para_cierre <- read_excel("Alertas_para_cierre.xlsx", sheet = "Hoja1")
Alertas_para_cierre<- as.data.frame(Alertas_para_cierre)
Alertas_para_cierre$Fecha_de_generacion= as.Date(Alertas_para_cierre$Fecha_de_generacion)
#Alertas_para_cierre <-Alertas_para_cierre[501:nrow(Alertas_para_cierre),]
datos_prueba_1= datos_prueba %>% filter(Nro_de_identificacion %in% Alertas_para_cierre$Nro_de_identificacion )
print(nrow(datos_prueba_1))
##funciones
my_update_function <- function(x){
  tryCatch(
    {
      ibj_click8=x
      return(ibj_click8)
    },
    error=function(error_message) {
      message("1er message Sc2.")
      message("el que viene posicion R:")
      message(error_message)
      return(1)
    }
  )
}
######### SELENIUM ##########
library(RSelenium)
library(wdman)
#library(netstat)
#selenium()
#objeto_selenium<- selenium(retcommand =T, check = F)
#
#binman::list_versions("chromedriver")
Sys.sleep(2)
#4567L port = 4577L ,
# rsDriver(check = FALSE)
#remote_driver<- rsDriver(browser = "chrome", port = 4577L ,chromever = "131.0.6778.108", verbose = FALSE)
remote_driver<- rsDriver(check = FALSE)
Sys.sleep(20)
# CONEXION A FINDER
remDr<-   remote_driver$client
Sys.sleep(3)
#remDr$open()
remDr$navigate("https://vfindersrv/Account/Login")
Sys.sleep(3)
#
for (k in 3:nrow(users)) {
  Alertas_para_cierre_1<- Alertas_para_cierre %>% filter(usuario==users$name[k])
  datos_prueba<- datos_prueba_1 %>% filter(Nro_de_identificacion %in% Alertas_para_cierre_1$Nro_de_identificacion )
  print(paste("alertas para cierre:", nrow(datos_prueba)))
  objeto_nombre<- remDr$findElement(using = 'id', 'usernameinput')
  Sys.sleep(1)
  objeto_nombre$sendKeysToElement(list(users$name[k]))
  Sys.sleep(1.5)
  #
  objeto_pwd<- remDr$findElement(using = 'id', 'passwordinput')
  Sys.sleep(1.5)
  objeto_pwd$sendKeysToElement(list(users$password[k]))
  Sys.sleep(1)
  #
  objeto_wind<- remDr$findElement(using = 'xpath','/html/body/div/div[1]/div[4]/div[1]/label')
  Sys.sleep(1)
  objeto_wind$clickElement()
  Sys.sleep(1)
  #
  ibj_iniciar<- remDr$findElement(using = 'xpath', '/html/body/div/div[1]/button')
  Sys.sleep(1)
  ibj_iniciar$clickElement()
  Sys.sleep(20)
  client<- 1
  Sys.sleep(1)
  ibj_treslin<- remDr$findElement(using = 'xpath', '//*[@id="btn-menu-default"]')
  Sys.sleep(2)
  ibj_treslin$clickElement()
  Sys.sleep(1)
  #
  ibj_ent<- remDr$findElement(using = 'id', 'AccesoEntidad')
  Sys.sleep(2)
  ibj_ent$clickElement()
  Sys.sleep(1)
  #colocar lapso de 3 segundos
  while(client<=nrow(datos_prueba)) {
    ###### el siguiente para actualizar la pagina de busqueda ####
    #remDr$refresh()
    #ibj_num_ent$refresh() 
    ibj_click<- remDr$findElement(using = 'xpath', '//*[@id="div-ci-rut"]')
    Sys.sleep(1)
    ibj_click$clickElement()
    Sys.sleep(1)
    # 1seg
    ibj_click1<- remDr$findElement(using = 'xpath', '//*[@id="dropdownMenuButton"]')
    Sys.sleep(1)
    ibj_click1$clickElement()
    Sys.sleep(1)
    # 1 seg
    ibj_click2<- remDr$findElement(using = 'xpath', ifelse(datos_prueba[client,2]==9,'//*[@id="9"]',
                                                           ifelse(datos_prueba[client,2]==1,'//*[@id="1"]','//*[@id="3"]')))
    Sys.sleep(2)
    ibj_click2$clickElement()
    Sys.sleep(1)
    # 2 seg
    ibj_num_ent<- remDr$findElement(using = 'id', 'txtEntidadFidner')
    Sys.sleep(1)
    ibj_num_ent$sendKeysToElement(list(datos_prueba[client,1], key='enter'))
    Sys.sleep(6)
    # 8 segundos
    ibj_click3<- remDr$findElement(using = 'xpath', '//*[@id="nav-Alertas-tab"]')
    Sys.sleep(1)
    ibj_click3$clickElement()
    Sys.sleep(1)
    #
    ibj_click4<- remDr$findElement(using = 'xpath', '//*[@id="collapse-aviso"]/div[1]/div[2]/div/button')
    Sys.sleep(1)
    ibj_click4$clickElement()
    Sys.sleep(1)
    ibj_dup<- NULL
    # tipo de alerta
    if(!is.numeric(my_update_function(remDr$findElement(using = 'xpath', '//*[@id="collapse-aviso"]/div[1]/div[2]/div/div/div/ul/li[1]/a')))){
      Sys.sleep(1)
      ibj_click5= remDr$findElement(using = 'xpath', '//*[@id="collapse-aviso"]/div[1]/div[2]/div/div/div/ul/li[1]/a')
      Sys.sleep(1)
      ibj_click5$clickElement()
      Sys.sleep(1)
      ibj_click51<- remDr$findElement(using = 'xpath', '//*[@id="collapse-aviso"]/div[1]/div[5]/div/button')
      Sys.sleep(1)
      ibj_click51$clickElement()
      Sys.sleep(1)
      ibj_click52<- remDr$findElement(using = 'xpath', '//*[@id="collapse-aviso"]/div[1]/div[5]/div/div/div/ul/li[2]/a/span[2]')
      Sys.sleep(1)
      ibj_click52$clickElement()
      
      s_prueba= datos_prueba[client,]
      Sys.sleep(1)
      print(s_prueba)
      ingresos= s_prueba$total_alertas
      contador_ingreso<-1
      ### HASTA AQUI INGRESA A VER LAS ALERTAS TOTALES
      while(ingresos>=0 & contador_ingreso<= ingresos){
        Sys.sleep(1)
        #
        cont=1
        ibj_click8=NULL
        swc=1
        controla_error<- 1
        while (swc==1) {
          ibj_click8=my_update_function(remDr$findElement(using = 'xpath', value = paste('//*[@id="content-alerta"]/div/table/tbody/tr[',contador_ingreso,']' ,sep = "")))
          Sys.sleep(1)
          if(!is.null(ibj_click8)){
            cont= cont+1
            swc=0
          }
          else{cont= cont+1
          }
        }
        print(paste("contador de click para presionar las alertas", cont,sep = "_____"))
        Sys.sleep(1)
        if(is.numeric(ibj_click8)){contador_ingreso<-ingresos+2}
        if(!is.null(ibj_click8) & !is.numeric(ibj_click8)){
          Sys.sleep(1)
          ibj_click8$clickElement()
          Sys.sleep(1)
          #valor=remDr$findElement(using = 'xpath', value = '//*[@id="content-observaciones-alerta"]')
          contador=1
          valor=NULL
          sw=1
          valor_1=""
          valor_2=""
          # while (sw==1) {
          #   Sys.sleep(1)
          #contenido de la caja de cierre
          valor_1=remDr$findElement(using = 'id', 'span-alerta-fecha-generacion')
          Sys.sleep(1)
          valor_1=unlist(valor_1$getElementText())
          valor_1=as.Date(valor_1, format="%d/%m/%Y")
          # valor_1= str_replace_all(valor_1,"\\.","")
          # valor_1= str_replace_all(valor_1,"USD ","")
          # Suponiendo que el elemento con scroll tiene el id "scrollContainer"
          js <- "return document.body.scrollHeight"
          total_height <- unlist(remDr$executeScript(js))
          scrollContainer <- remDr$findElement(using = 'id', value = 'divBodyContent')
          remDr$executeScript(paste0("arguments[0].scrollTop = arguments[0].scrollTop + ",total_height,";"), list(scrollContainer))
          Sys.sleep(1)
          #
          valor_2<- remDr$findElement(using = 'id', 'span-alerta-valor-esperado')
          Sys.sleep(1)
          valor_2=unlist(valor_2$getElementText())
          valor_2= str_replace_all(valor_2,"\\.","")
          valor_2= str_replace_all(valor_2,"USD ","")
          #filtrado= Alertas_para_cierre %>% filter(Nro_de_identificacion==datos_prueba[client,1] & Valor_esperado==valor_2 & Fecha_de_generacion== as.Date(valor_1))
          filtrado= Alertas_para_cierre %>% filter(Nro_de_identificacion==datos_prueba[client,1] & Valor_esperado==valor_2)
          #filtrado<- filtrado %>% filter(Fecha_de_generacion<= as.Date("2024-12-31"))
          if(nrow(filtrado)>0){
            if(nrow(filtrado)>1){filtrado= (filtrado)[1,]}
            print(paste("valor esperado:",valor_2))
            print(paste("fecha generacion:",valor_1))
            Sys.sleep(1)
            ##
            ibj_click_proc<- remDr$findElement(using = 'xpath', value = '//*[@id="btn-procesar-pendiente"]')
            Sys.sleep(1)
            ibj_click_proc$clickElement()
            Sys.sleep(2)
            #
            prueba<- my_update_function(remDr$findElement(using = 'xpath', value = '//*[@id="div-agregar-nueva-observacion"]/div[2]/div[2]/div/span'))
            if(unlist(prueba$getElementText())!=""){
              
              #envia texto
              objeto_insert_texto<- remDr$findElement(using = 'id', 'textarea-nueva-observacion-alerta')
              Sys.sleep(2)
              #
              objeto_insert_texto$clearElement()
              Sys.sleep(1)
              #
              objeto_insert_texto$sendKeysToElement(list(unique(filtrado$Detalle_cierre)))
              Sys.sleep(2)
              ibj_click_guardar<- remDr$findElement(using = 'xpath', value = '//*[@id="div-agregar-nueva-observacion"]/div[2]/div[2]/div/button')
              Sys.sleep(2)
              ibj_click_guardar$clickElement()
              Sys.sleep(1)
              #hasta aqui envia texto
              #2segundos de pausa
              ibj_click_final<- remDr$findElement(using = 'xpath', value = '//*[@id="btn-finalizar"]')
              Sys.sleep(1)
              ibj_click_final$clickElement()
              Sys.sleep(1)
              actualiza_cant_alerta=remDr$findElement(using = 'id', 'nav-Alertas-tab')
              Sys.sleep(2)
              actualiza_cant_alerta$clickElement()
              Sys.sleep(1)
              ingresos<- ingresos-1
              contador_ingreso<-contador_ingreso+1
            }
            else{
              actualiza_cant_alerta=remDr$findElement(using = 'id', 'nav-Alertas-tab')
              Sys.sleep(2)
              actualiza_cant_alerta$clickElement()
              Sys.sleep(1)
              contador_ingreso<-contador_ingreso+1
            }
            #
          }
          else{
            print("****************************************")
            print(paste("No se tiene análisis para cerrar "
                        ,sep = "____"))
            contador_ingreso<-contador_ingreso+1
          }
        }
      }
      Sys.sleep(1)
      remDr$refresh()
      Sys.sleep(2)
      client<- client+1
    }
    else{
      ibj_dup<- my_update_function(remDr$findElement(using = 'xpath', '//*[@id="contenedor-lista-entidades"]/div/table/thead/tr/th[2]'))
      Sys.sleep(1)
      if(!is.numeric(ibj_dup)){
        client= client+1
        Sys.sleep(1)
        remDr$refresh()
        Sys.sleep(2)}
      else{Sys.sleep(1)
        remDr$refresh()
        Sys.sleep(2)}
    }
  }
  ##
  objeto_new_pwd<- remDr$findElement(using = 'xpath', '//*[@id="logoutForm"]/div/div[2]/div[2]/button')
  Sys.sleep(1.5)
  objeto_new_pwd$clickElement()
  Sys.sleep(1)
}
### para la 2da alerta
# datos= datos_prueba[1:(client-1),]
# Alertas_para_cierre<- Alertas_para_cierre %>% filter(!(Nro_de_identificacion %in% datos$Nro_de_identificacion) )
# library(openxlsx)
# write.xlsx(Alertas_para_cierre, file = "eliminar1.xlsx")









