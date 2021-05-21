library(shiny)
library(magick)
#### Tab inicial ####




inicial_ui <- function(){
  url = "https://noticias.unb.br/images/Noticias/Fotos_frequentes/ICC/09052018_ICC_Secom.jpg"
  
  fluidRow(
    solidHeader = F,
    collapsible = T, width = 12,
    column(12, align = "center", 
           tags$div(
             class='floating-box about-text',
             tags$h2('Conheça o Perfil do Estudante da UNB', class='about-title'),
             img(src=url))
           
    ))
}

#### Linha temporal 
Linha_temporal_ui <- function(){
  tabPanel("Linha Temporal",plotlyOutput('linhatemporal')
           
  )
}

##### Raca cor #######
Raca_cor_ui <- function(){
  tabPanel("Raça & Cor",
           plotlyOutput('raca_cor')
           
  )
}


#### Escolaridade dos Pais ######

Escolaridae_ui <- function(){
  tabPanel("Escolaridade dos Pais",
           plotlyOutput('escolaridade_pais')
           
  )
}



#### Ensino médio ######

Ensino_medio_ui <- function(){
  tabPanel("Ensino Médio",
           plotlyOutput('ensino_medio')
           
  )
}


Ensino_fundamental_ui <- function(){
  tabPanel("Ensino Fundamental",
           plotlyOutput('ensino_fundamental')
           
  )
}



##### Mapas ######

mapa_ui <- function(){
  tabPanel("mapas",
           leafletOutput("mapa1")
           
  )
}


##### Instituto ######


Instituto_ui <- function(){
  tabPanel("Instituto",
           plotlyOutput('Instituto_plot'))
  
}

##### Curso #####


Curso_ui <- function() {
  escolhas_instituto <- unique(Estudantes$instituto)
  escolhas_instituto <- escolhas_instituto[-28]
  escolhas_instituto <- sort(escolhas_instituto)
  
  tabPanel("Curso",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "instituto", 
                           label = "Instituto UNB",
                          choices = escolhas_instituto,
                          selected = 'IE'),
               width = 2),
             
             
             mainPanel(
               plotlyOutput('Curso_plot'),
               style='overflow-y: scroll; max-height: 82vh;'
             )))
}


##### ABA SOBRE  #####
#readLines('www/sobre.txt', encoding='UTF-8')



sobre_ui <- function() {
  texto <- readLines('www/sobre.txt', encoding='UTF-8')
  
  fluidRow(
    solidHeader = F,
    collapsible = T, width = 12,
    column(12, align = "left", 
           tags$div(
             class='floating-box about-text',
             tags$h2('Sobre este trabalho', class='about-title'),
             HTML(texto)
           )
    ))
}


