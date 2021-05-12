library(tidyverse)
library(readxl)
library(stringr)


###### leitura de dados #####

`%notin%` <- Negate(`%in%`)

Estudantes = read_xlsx('estudanteunb.xlsx')


### Fazendo mudanças pertinentes nas variáveis ###########


### criação de fator semestre ingresso ####

Estudantes$semestre_ingresso <- factor(Estudantes$semestre_ingresso,
                                       levels = unique(Estudantes$semestre_ingresso),
                                       labels = unique(Estudantes$semestre_ingresso))




Estudantes$semestre_ingresso <- ordered(Estudantes$semestre_ingresso,
                                        levels= c("12012","22012","12013","22013","12014","22014"
                                                  ,"12015","22015","12016"
                                                  ,"22016","12017","22017"))




### Cruiação de Fator renda familiar e exlusão de elementos ######

Estudantes <- Estudantes %>% filter(renda_familiar %notin% c("Ignorado","Não possui renda mensal"))

Estudantes$renda_familiar <- as.factor(Estudantes$renda_familiar)

levels(Estudantes$renda_familiar)
Estudantes$renda_familiar <- ordered(Estudantes$renda_familiar,
                                        levels= c("Até 3 SM", "De 3 a 10 SM","De 10 a 20 SM" ,
                                                  "Mais de 20 SM"))




#### Mudança na modalidade de ingresso #######

Estudantes$modalidade_ingresso %>% unique()

Estudantes$modalidade_ingresso[Estudantes$modalidade_ingresso==
                                 "Admissão para Portador de Diploma de Curso Superior (DCS)"] <- "DCS"


#### Filtro de cor e Raça #######


Estudantes <- Estudantes %>% filter(cor_raca%notin% c('Ignorado','Outros'))


Estudantes$sexo %>% unique()

Estudantes$sexo <- as.factor(Estudantes$sexo)

Estudantes$sexo <- ordered(Estudantes$sexo, 
                           levels = c('Feminino',"Masculino",'Ignorado'))



##### Criação de vetores com os principais filtros das principais variáveis #######

modalidades = Estudantes$modalidade_ingresso %>% unique()

semestres = Estudantes$semestre_ingresso %>% unique()

rendas = Estudantes$renda_familiar %>% unique()

em_s = Estudantes$ensino_medio %>% unique()

ef_s = Estudantes$ensino_fundamental %>% unique()

sexos = Estudantes$sexo %>% unique()

transportes = Estudantes$transporte %>% unique()

tentativas = Estudantes$tentativas %>% unique()


#### Criando Variável com nomes das variáveis e variáveis em groups que podem ser chamadas #####


nomes_de_apresentacao <- c('Modalidade de Ingresso',"Atendimento Especial","Sexo",'Nacionalidade',
                           'Cor & Raca','Religião','Renda Familiar','Benefício Social',
                           'Pessoas vivem da renda','Escolaridade Pai','Escolaridade Mãe','Ensino Fundamental',
                           'Ensino Médio','Tipo de ensino médio','Necessidade Especial','Tentativas','Auxilio',
                           'Idioma','Curso Desejado','Trocaria de Curso','Curso Preparatório','Transporte',
                           'Sistema de Ingresso')


names(groups)


groups = c(quo(modalidade_ingresso), quo(atendimento_especial),quo(sexo),quo(nacionalidade),
           quo(cor_raca),quo(religiao),quo(renda_familiar),quo(beneficio_social),quo(pessoas_vivem_da_renda),
           quo(escolaridade_pai),quo(escolaridade_mae),quo(ensino_fundamental),quo(ensino_medio),quo(tipo_ensino_medio),
           quo(necessidade_especial),quo(tentativas),quo(auxilio),quo(idioma),quo(curso_desejado),quo(trocaria_curso),
           quo(curso_preparatorio),quo(transporte),quo(sistema_ingresso))


names(groups) <- nomes_de_apresentacao

ordem <- order(names(groups))

new_groups <- list()

for(i in 1: length(ordem)){
  new_groups[[i]] = groups[[ordem[i]]]
  
}



nomes_de_apresentacao <- sort(nomes_de_apresentacao)

names(new_groups) <- nomes_de_apresentacao

save(new_groups,file = 'new_groups.RDATA')


#### Salvando arquivo rdata ######


save(Estudantes, file = 'Estudantes.RDATA')

##### Gráficos ########


###página inicial ####

Teste = Estudantes %>% group_by(semestre_ingresso,cor_raca) %>% count()

Teste %>% filter( cor_raca %notin%c('Outro','Nao se aplica','Ignorado')) %>% 
  ggplot(aes(x=semestre_ingresso,y=n,group=cor_raca,colour=cor_raca))+
  geom_line(size=1.2)+
  ggtitle(paste0('Acompanhamento da Variável ','cor_raca'))+
  
  scale_linetype_manual(values = c('dotted', 'solid')) +
  
  scale_colour_manual(name='Origem do Banco',values=c('#A11D21','#003366','darkgreen',2,3,5,'green')) +
  theme_bw()+
  labs(x='Semestres',y='Número de Alunos Ingressos')+
  theme(legend.position = 'top',
        plot.title = element_text(hjust=0.5),
        axis.title.y=element_text(colour='black',size=10),
        axis.title.x=element_text(colour='black',size=10),
        axis.text=element_text(colour='black',size=6.5),
        panel.border=element_blank(),
        axis.line=element_line(colour='black'))

##### Descritiva de cor e raça ########

agrupado <- Estudantes %>% group_by(cor_raca,semestre_ingresso,renda_familiar) %>% count()



Rosca_cor <- agrupado %>% filter(semestre_ingresso%in%semestres,
                     renda_familiar%in%rendas,
                     ensino_medio%in%em_s,
                     ensino_fundamental%in%ef_s,
                     sexo%in%sexos,
                     modalidade_ingresso%in%modalidades)



Rosca_cor <- Rosca_cor %>% filter(cor_raca!='Ignorado')

Rosca_cor %>% 
ggplot(aes(x="", y=n, fill=cor_raca)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(name='Cor & Raça',values=c('#A11D21','#003366','darkgreen',2,3,5,'green')) +
  coord_polar("y", start=0) +
  theme_bw()+
  theme(legend.position = 'top',
        axis.title=element_blank(),
        axis.text=element_blank(),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        panel.grid=element_blank())


agrupado$transporte %>% table()

