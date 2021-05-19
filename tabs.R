#### Tab inicial ####

Inicial_ui <- function(){
  tabPanel("Inicial",
             mainPanel(
               img(src= 'www/unb-corredor.jpg',align = "right", width=140))
           )
           
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


