library(shiny)

nomes_variaveis_filtro <- function(){
  nomes_de_apresentacao <- c('Modalidade de Ingresso',"Atendimento Especial","Sexo",'Idade','Nacionalidade',
                             'Cor & Raca','Religião','Renda Familiar','Benefício Social',
                             'Pessoas vivem da renda','Escolaridade Pai','Escolaridade Mãe','Ensino Fundamental',
                             'Ensino Médio','Tipo de ensino médio','Necessidade Especial','Tentativas','Auxilio',
                             'Idioma','Curso Desejado','Trocaria de Curso','Curso Preparatório','Transporte',
                             'Sistema de Ingresso')
  return(nomes_de_apresentacao)
}

##### Filtros iniciais #####

filtros_lateral_inicial <- function() {
  escolhas_rendas <- 'Todas'
  escolhas_rendas[2:5] <- levels(Estudantes$renda_familiar)
  
  escolhas_semestre <- 'Todos'
  escolhas_semestre[2:13] <- levels(Estudantes_RA$semestre_ingresso)
  
  escolhas_cotas <- "Todos"
  
  escolhas_cotas[2:3] <- unique(Estudantes$sistema_ingresso)[1:2]
  
  escolhas_campus <- "Todos"
  
  escolhas_campus[2:5] <- unique(Estudantes_RA$campus)
  
  
  escolhas_semestres_2 <- escolhas_semestre[c(1,7:13)]
  box(class='floating-box',
      selectInput("variable", "Variáveis",
                  choices = nomes_variaveis_filtro(),
                  selected = "Sistema de Ingresso"
      ),
      width = 3
  )
}

##### Filtros Raça Cor #####


filtros_lateral_raca_cor <- function() {
  escolhas_rendas <- 'Todas'
  escolhas_rendas[2:5] <- levels(Estudantes$renda_familiar)
  
  escolhas_semestre <- 'Todos'
  escolhas_semestre[2:13] <- levels(Estudantes_RA$semestre_ingresso)
  
  escolhas_cotas <- "Todos"
  
  escolhas_cotas[2:3] <- unique(Estudantes$sistema_ingresso)[1:2]
  
  escolhas_campus <- "Todos"
  
  escolhas_campus[2:5] <- unique(Estudantes_RA$campus)
  
  
  escolhas_semestres_2 <- escolhas_semestre[c(1,7:13)]
  box(class='floating-box',
      selectInput(inputId = 'semestre_ingresso','Semestre de ingresso',
                  choices =escolhas_semestre,
                  selected = "22017" ),
      
      selectInput(inputId = 'renda_familiar','Renda Familiar',
                  choices =escolhas_rendas,
                  selected =escolhas_rendas[1]  ),
      
      selectInput(inputId = 'cotas','Sistema de Ingresso',
                  choices =escolhas_cotas,
                  selected =escolhas_cotas[1]  ),
      width = 3
  )
}

#### Filtros mapas #####

filtros_mapas <- function(){
  escolhas_rendas <- 'Todas'
  escolhas_rendas[2:5] <- levels(Estudantes$renda_familiar)
  
  escolhas_semestre <- 'Todos'
  escolhas_semestre[2:13] <- levels(Estudantes_RA$semestre_ingresso)
  
  escolhas_cotas <- "Todos"
  
  escolhas_cotas[2:3] <- unique(Estudantes$sistema_ingresso)[1:2]
  
  escolhas_campus <- "Todos"
  
  escolhas_campus[2:5] <- unique(Estudantes_RA$campus)
  
  
  escolhas_semestres_2 <- escolhas_semestre[c(1,7:13)]
  box(class='floating-box',
      selectInput(inputId = 'semestre_mapas','Semestre de ingresso',
                  choices =escolhas_semestres_2,
                  selected = "22017" ),
      
      selectInput(inputId = 'campus_mapas','Campus Universitário',
                  choices =escolhas_campus,
                  selected =escolhas_campus[1]  ),
      width = 3
  )
  
}

