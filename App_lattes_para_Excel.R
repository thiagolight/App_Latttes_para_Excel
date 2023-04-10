#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rm(list = ls())
library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(vars)
library(forecast)
library(urca)
library(uroot)
library(zoo)
library(DT)
library(rvest)
library(dplyr)
library(XML)
library(stringr)
library(xlsx)
library(writexl)
options(encoding = "utf-8")



 #titulo_modelo = "Extrair dados do Lattes Para Excel"


# Define UI for application that draws a histogram
ui <- dashboardPage(
 
  # Application title
        dashboardHeader(title = tags$h4("Extrair dados do Lattes Para Excel"), titleWidth = 300),
  
  # Sidebar with a slider input for number of bins 
        dashboardSidebar(width = 300,
 
             
                     sidebarMenu(
                                menuItem("Input",tabName = "up",icon=icon("upload"),
                                        
                                         fileInput("file", label = h3("Arquivo Lattes input")), 
                                         
                                           
                                           
                                          helpText(tags$h4("Escolha um Lattes em formato XML.")) 
                                        ),  
                                            
                                menuItem("Baixar dados",tabName = "down", icon = icon("download"),
                                         tags$h5("Baixar dados em formato EXCEL"),
                                         downloadButton("downloadData", "Download")
                                         
                                         
                                         ),
                                menuItem("Informações", tabName = "info", icon=icon("info-circle"),
                                         
                                         htmlOutput("text")
                                         )
                              )
      
      
             
      
                       ),
          dashboardBody(
             
                    img(src = "logo_lattes.png", height = 200, width = 400),
                    textOutput("aviso")

                 
                    
                       )
    
               )
  


# back-end = server-side
server <- function(input, output, session) {
   
   
   dados<<-function(){
      file1<<-input$file
      if(is.null(file1)){ 
         return()
      }else{
         dados=read_xml(file1$datapath)
         return(dados)
         
      }
      
   }
   
   
   tab_Final<<-function(){
      
      if(is.null(dados())){ 
         return()
      }else{
         ################################################
         ################################################
         ################################################
         dados = dados()
         Base_autores <-matrix(NA, nrow = 1, ncol = 30)
         Base_autores <- as.data.frame(Base_autores)
         
         
         # criando nomes das colunas de autores
         nomes_novos<-NULL
         for (i in 1:30){ 
            
            nomes_novos<-rbind(nomes_novos, gsub(" ","", paste("Autor", i)))
         }
         
         nomes_novos
         
         colnames(Base_autores)<-nomes_novos
         
         ####################################
         ######################################
         ### Criando base Sigla dos autores
         
         Base_Sigla_autores<-matrix(NA, nrow = 1, ncol = 30)
         Base_Sigla_autores <- as.data.frame(Base_autores)
         
         
         # criando nomes das colunas de autores
         nomes_Sigla_novos<-NULL
         for (i in 1:30){ 
            
            nomes_Sigla_novos<-rbind(nomes_Sigla_novos, gsub(" ","", paste("Sigla_Autor", i)))
         }
         
         nomes_Sigla_novos
         
         colnames(Base_Sigla_autores)<-nomes_Sigla_novos
         #########################################
         ############################################
         
         
         
         ############################ Criando tabela de artigos
         
         
         Artigos_Publicados=NULL
         
         Dados_periodico=NULL
         
         Quantidade_Autores=NULL
         
         for(i in 1:xml_length(xml_child(dados, 2) %>% xml_child(2))){
            
            aux=xml_child(dados, 2) %>% xml_child(2) %>% xml_child(i)%>% 
               xml_child(1) %>%  xml_attrs(1)
            
            aux=t(aux)
            
            Artigos_Publicados = rbind(Artigos_Publicados, aux)
            
            
            ############################ Pegando NOme do periodico
            
            aux2=xml_child(dados, 2) %>% xml_child(2) %>% xml_child(i)%>% 
               xml_child(2) %>%  xml_attrs(1)
            aux2=t(aux2)
            Dados_periodico=rbind(Dados_periodico, aux2)
            
            ############################# Segunda parte
            
            
            
            #Achando autores
            Autores=xml_child(dados, 2) %>%xml_child(2) %>% 
               xml_child(i) %>% xml_find_all("AUTORES") %>% xml_comment()
            
            
            ## quantidade de autores no paper
            quant_autores_no_paper <- xml_child(dados, 2) %>%xml_child(2) %>% 
               xml_child(i) %>% xml_find_all("AUTORES") %>%  NROW()
            
            
            ## colocando os autores nos seus lugares da base
            for (j in 1:quant_autores_no_paper){
               Autor_i=Autores[[j]]%>% xml_attrs() %>% as.data.frame() %>% t()
               Autor_i[1]
               
               Base_autores[i,j] <- Autor_i[1]
               
               
            }
            
            
            ## colocando os Sigla autores nos seus lugares da base
            for (j in 1:quant_autores_no_paper){
               Autor_i=Autores[[j]]%>% xml_attrs() %>% as.data.frame() %>% t()
               Autor_i[2]
               
               Base_Sigla_autores[i,j] <- Autor_i[2]
               print(Autor_i[2])
               
               
               
            }
            
            
            
            
            Quantidade_Autores[i] <- quant_autores_no_paper
            
            
            
            
            
            
         }
         ########### Finalizando base Artigos
         
         base_artigos_final<-qpcR:::cbind.na(Artigos_Publicados,Dados_periodico,Quantidade_Autores, Base_autores, Base_Sigla_autores)
         
         
         ##Salvando em sheets do excel
         # library(xlsx)
         # write.xlsx(base_artigos_final,file = "myworkbook.xlsx", 
         #            sheetName = "artigo", append = FALSE)
         # 
         ##############################
         #################################
         ####################################
         #######################################
         ##########################################
         #############################################
         ################################################
         ###################################################
         ######################################################
         #########################################################
         
         
         ### Criando base autores
         #Criando base autores
         
         Base_autores<-matrix(NA, nrow = 1, ncol = 30)
         Base_autores <- as.data.frame(Base_autores)
         
         
         # criando nomes das colunas de autores
         nomes_novos<-NULL
         for (i in 1:30){ 
            
            nomes_novos<-rbind(nomes_novos, gsub(" ","", paste("Autor", i)))
         }
         
         nomes_novos
         
         colnames(Base_autores)<-nomes_novos
         
         ####################################
         ######################################
         ### Criando base Sigla dos autores
         
         Base_Sigla_autores<-matrix(NA, nrow = 1, ncol = 30)
         Base_Sigla_autores <- as.data.frame(Base_autores)
         
         
         # criando nomes das colunas de autores
         nomes_Sigla_novos<-NULL
         for (i in 1:30){ 
            
            nomes_Sigla_novos<-rbind(nomes_Sigla_novos, gsub(" ","", paste("Sigla_Autor", i)))
         }
         
         nomes_Sigla_novos
         
         colnames(Base_Sigla_autores)<-nomes_Sigla_novos
         #########################################
         ############################################
         
         
         
         ############################ Criando tabela de artigos
         
         
         Trabalhos_Em_Eventos=NULL
         
         Dados_periodico=NULL
         
         Quantidade_Autores=NULL
         
         for(i in 1:xml_length(xml_child(dados, 2) %>% xml_child(1))){
            
            aux=xml_child(dados, 2) %>% xml_child(1) %>% xml_child(i)%>% 
               xml_child(1) %>%  xml_attrs(1)
            
            aux=t(aux)
            
            Trabalhos_Em_Eventos = rbind(Trabalhos_Em_Eventos, aux)
            
            
            ############################ Pegando NOme do periodico
            
            aux2=xml_child(dados, 2) %>% xml_child(1) %>% xml_child(i)%>% 
               xml_child(2) %>%  xml_attrs(1)
            aux2=t(aux2)
            Dados_periodico=rbind(Dados_periodico, aux2)
            
            ############################# Segunda parte
            
            
            
            #Achando autores
            Autores=xml_child(dados, 2) %>%xml_child(1) %>% 
               xml_child(i) %>% xml_find_all("AUTORES") %>% xml_comment()
            
            
            ## quantidade de autores no paper
            quant_autores_no_paper <- xml_child(dados, 2) %>%xml_child(1) %>% 
               xml_child(i) %>% xml_find_all("AUTORES") %>%  NROW()
            
            
            ## colocando os autores nos seus lugares da base
            for (j in 1:quant_autores_no_paper){
               Autor_i=Autores[[j]]%>% xml_attrs() %>% as.data.frame() %>% t()
               Autor_i[1]
               
               Base_autores[i,j] <- Autor_i[1]
               
               
            }
            
            
            ## colocando os Sigla autores nos seus lugares da base
            for (j in 1:quant_autores_no_paper){
               Autor_i=Autores[[j]]%>% xml_attrs() %>% as.data.frame() %>% t()
               Autor_i[2]
               
               Base_Sigla_autores[i,j] <- Autor_i[2]
               print(Autor_i[2])
               
               
               
            }
            
            
            
            
            Quantidade_Autores[i] <- quant_autores_no_paper
            
            
            
            
            
            
         }
         ########### Finalizando base Artigos
         
         Trabalhos_Em_Eventos_final<-qpcR:::cbind.na(Trabalhos_Em_Eventos,Dados_periodico,Quantidade_Autores, Base_autores, Base_Sigla_autores)
         
         
         ##Salvando em sheets do excel
         # library(xlsx)
         # write.xlsx(Trabalhos_Em_Eventos_final,file = "myworkbook.xlsx", 
         #            sheetName = "Trabalhos_Em_Eventos", append = TRUE)
         # 
         ##############################
         #################################
         ####################################
         #######################################
         ##########################################
         #############################################
         ################################################
         ###################################################
         ######################################################
         #########################################################
         
         
         
         ### Criando base autores
         #Criando base autores
         
         Base_autores<-matrix(NA, nrow = 1, ncol = 30)
         Base_autores <- as.data.frame(Base_autores)
         
         
         # criando nomes das colunas de autores
         nomes_novos<-NULL
         for (i in 1:30){ 
            
            nomes_novos<-rbind(nomes_novos, gsub(" ","", paste("Autor", i)))
         }
         
         nomes_novos
         
         colnames(Base_autores)<-nomes_novos
         
         ####################################
         ######################################
         ### Criando base Sigla dos autores
         
         Base_Sigla_autores<-matrix(NA, nrow = 1, ncol = 30)
         Base_Sigla_autores <- as.data.frame(Base_autores)
         
         
         # criando nomes das colunas de autores
         nomes_Sigla_novos<-NULL
         for (i in 1:30){ 
            
            nomes_Sigla_novos<-rbind(nomes_Sigla_novos, gsub(" ","", paste("Sigla_Autor", i)))
         }
         
         nomes_Sigla_novos
         
         colnames(Base_Sigla_autores)<-nomes_Sigla_novos
         #########################################
         ############################################
         
         
         
         ############################ Criando tabela de artigos
         
         
         Livros_Publicados=NULL
         
         Dados_periodico=NULL
         
         Quantidade_Autores=NULL
         
         for(i in 1:xml_length(xml_child(dados, 2) %>% xml_child(3)%>% xml_child(1))){
            
            aux=xml_child(dados, 2) %>% xml_child(3) %>% xml_child(1) %>%xml_child(i)%>% 
               xml_child(1) %>%  xml_attrs(1)
            
            aux=t(aux)
            
            Livros_Publicados = rbind(Livros_Publicados, aux)
            
            
            ############################ Pegando NOme do periodico
            
            aux2=xml_child(dados, 2) %>% xml_child(3) %>% xml_child(1) %>%xml_child(i)%>% 
               xml_child(2) %>%  xml_attrs(1)
            aux2=t(aux2)
            Dados_periodico=rbind(Dados_periodico, aux2)
            
            ############################# Segunda parte
            
            
            
            #Achando autores
            Autores=xml_child(dados, 2) %>%xml_child(3) %>% xml_child(1) %>% 
               xml_child(i) %>% xml_find_all("AUTORES") %>% xml_comment()
            
            
            ## quantidade de autores no paper
            quant_autores_no_paper <- xml_child(dados, 2) %>% xml_child(3) %>% xml_child(1) %>%
               xml_child(i) %>% xml_find_all("AUTORES") %>%  NROW()
            
            
            ## colocando os autores nos seus lugares da base
            for (j in 1:quant_autores_no_paper){
               Autor_i=Autores[[j]]%>% xml_attrs() %>% as.data.frame() %>% t()
               Autor_i[1]
               
               Base_autores[i,j] <- Autor_i[1]
               
               
            }
            
            
            ## colocando os Sigla autores nos seus lugares da base
            for (j in 1:quant_autores_no_paper){
               Autor_i=Autores[[j]]%>% xml_attrs() %>% as.data.frame() %>% t()
               Autor_i[2]
               
               Base_Sigla_autores[i,j] <- Autor_i[2]
               print(Autor_i[2])
               
               
               
            }
            
            
            
            
            Quantidade_Autores[i] <- quant_autores_no_paper
            
            
            
            
            
            
         }
         ########### Finalizando base Artigos
         
         Livros_Publicados_final<-qpcR:::cbind.na(Livros_Publicados,Dados_periodico,Quantidade_Autores, Base_autores, Base_Sigla_autores)
         
         
         ##Salvando em sheets do excel
         # library(xlsx)
         # write.xlsx(Livros_Publicados_final,file = "myworkbook.xlsx", 
         #            sheetName = "Livros_Publicados", append = TRUE)
         # 
         # 
         
         
         
         
         ##############################
         #################################
         ####################################
         #######################################
         ##########################################
         #############################################
         ################################################
         ###################################################
         ######################################################
         #########################################################
         
         
         
         ### Criando base autores
         #Criando base autores
         
         Base_autores<-matrix(NA, nrow = 1, ncol = 30)
         Base_autores <- as.data.frame(Base_autores)
         
         
         # criando nomes das colunas de autores
         nomes_novos<-NULL
         for (i in 1:30){ 
            
            nomes_novos<-rbind(nomes_novos, gsub(" ","", paste("Autor", i)))
         }
         
         nomes_novos
         
         colnames(Base_autores)<-nomes_novos
         
         ####################################
         ######################################
         ### Criando base Sigla dos autores
         
         Base_Sigla_autores<-matrix(NA, nrow = 1, ncol = 30)
         Base_Sigla_autores <- as.data.frame(Base_autores)
         
         
         # criando nomes das colunas de autores
         nomes_Sigla_novos<-NULL
         for (i in 1:30){ 
            
            nomes_Sigla_novos<-rbind(nomes_Sigla_novos, gsub(" ","", paste("Sigla_Autor", i)))
         }
         
         nomes_Sigla_novos
         
         colnames(Base_Sigla_autores)<-nomes_Sigla_novos
         #########################################
         ############################################
         
         
         
         ############################ Criando tabela de artigos
         
         
         Capitulos_Livros_Publicados=NULL
         
         Dados_periodico=NULL
         
         Quantidade_Autores=NULL
         
         for(i in 1:xml_length(xml_child(dados, 2) %>% xml_child(3)%>% xml_child(2))){
            
            aux=xml_child(dados, 2) %>% xml_child(3) %>% xml_child(2) %>%xml_child(i)%>% 
               xml_child(1) %>%  xml_attrs(1)
            
            aux=t(aux)
            
            Capitulos_Livros_Publicados = rbind(Capitulos_Livros_Publicados, aux)
            
            
            ############################ Pegando NOme do periodico
            
            aux2=xml_child(dados, 2) %>% xml_child(3) %>% xml_child(2) %>%xml_child(i)%>% 
               xml_child(2) %>%  xml_attrs(1)
            aux2=t(aux2)
            Dados_periodico=rbind(Dados_periodico, aux2)
            
            ############################# Segunda parte
            
            
            
            #Achando autores
            Autores=xml_child(dados, 2) %>%xml_child(3) %>% xml_child(2) %>% 
               xml_child(i) %>% xml_find_all("AUTORES") %>% xml_comment()
            
            
            ## quantidade de autores no paper
            quant_autores_no_paper <- xml_child(dados, 2) %>% xml_child(3) %>% xml_child(2) %>%
               xml_child(i) %>% xml_find_all("AUTORES") %>%  NROW()
            
            
            ## colocando os autores nos seus lugares da base
            for (j in 1:quant_autores_no_paper){
               Autor_i=Autores[[j]]%>% xml_attrs() %>% as.data.frame() %>% t()
               Autor_i[1]
               
               Base_autores[i,j] <- Autor_i[1]
               
               
            }
            
            
            ## colocando os Sigla autores nos seus lugares da base
            for (j in 1:quant_autores_no_paper){
               Autor_i=Autores[[j]]%>% xml_attrs() %>% as.data.frame() %>% t()
               Autor_i[2]
               
               Base_Sigla_autores[i,j] <- Autor_i[2]
               print(Autor_i[2])
               
               
               
            }
            
            
            
            
            Quantidade_Autores[i] <- quant_autores_no_paper
            
            
            
            
            
            
         }
         ########### Finalizando base Artigos
         
         Capitulos_Livros_Publicados_final<-qpcR:::cbind.na(Capitulos_Livros_Publicados,Dados_periodico,Quantidade_Autores, Base_autores, Base_Sigla_autores)
         
         
         ##Salvando em sheets do excel
         # library(xlsx)
         # write.xlsx(Capitulos_Livros_Publicados_final,file = "myworkbook.xlsx", 
         #            sheetName = "Capitulos_Livros_Publicados", append = TRUE)
         # 
         # ################################################
         ################################################
         ################################################
         
         
         
         
      tab_Final<<- list("artigo" = base_artigos_final,
                     "Trabalhos_Em_Eventos" = Trabalhos_Em_Eventos_final, 
                     "Livros_Publicados"=Livros_Publicados_final, 
                     "Capitulos_Livros_Publicados"=Capitulos_Livros_Publicados_final)
      
      
      return(tab_Final)   
            
      }
      
      return(tab_Final)   
      
   }
   
   
   
   
   
   
   
   output$aviso<- renderText({
      if(is.null(dados())){
         
         return()
         
      }else{
         
         nome = xml_attrs(xml_child(dados(), 1))[["NOME-COMPLETO"]]
         
         aviso = paste("Por favor, clique em 'download'  e baixe o arquivo de (o)", nome)
         
         
         return(aviso)
         
      }
      
      
   })
   
 

 
   
   
   output$text<-renderText({
     
     
           
                   HTML( "<p align='center'><strong></h2>About</h2></strong></p>
                         <h6 align='center'>Esse soft foi criado para auxiliar<br/> 
                          a organização de dados de produções bibliográficas <br/>
                          do arquivo lattes.<p/>
                         Ele não extrai informações de produções tecnicas, <br/>
                         ou complementares.  <br/>
                         Esse soft apenas extrai as seguintes informações <br/> 
                        de produções bibliográficas: <br/>
                        <ol>
                         <li>Trabalhos em Eventos </li>
                         <li>Artigos Publicados</li>
                         <li>Livros Publicados ou Organizados</li>
                         <li>Capitulos de Livros Publicados.</li></h6>
                        </ol></p>
                         Autor: Thiago da Luz Ferreira. <br/>
                         e-mail: thiagolight27@gmail.com <br/>"
                        )
             
                         })
   
   
   
   
   
   # Downloadable csv of selected dataset ----
   output$downloadData <- downloadHandler(
     
                                     filename = function() {
                                        
                                                    nome = xml_attrs(xml_child(dados(), 1))[["NOME-COMPLETO"]]
                                                    nome = str_match(nome, "\\w+")
                                                       
                                                    paste(nome, ".xlsx", sep = "")
                                                           },
                                     content = function(file) {
                                       
                                                 downdados<-tab_Final()
                                                     
                                                 openxlsx::write.xlsx(downdados, file)
                                                              }
                                          )
   
   
}


#### Run the application 

shinyApp(ui = ui, server = server)


