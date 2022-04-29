
# Taxa de juros considerada: 6,4% pois foi a média do CDI no período de 01/01/2019 a 20/04/2022
#

library(shiny)
library(shinythemes)
library(readxl)
library(dplyr)

# Baixando a Tabua-BR-EMS da SUSEP
tmp = tempfile(fileext = ".xlsx")
url = "http://www.susep.gov.br/setores-susep/cgpro/copep/Tabuas%20BR-EMS%202010%202015%202021-010721.xlsx"
download.file(url, destfile = tmp, quiet = T)

# a) Pagamento Único
pgtoUnico = function(Beneficio, idadeIni, idadeFim, Taxa, Tabela){
  
  # Compromisso da Seguradora
  nEx = 1 / (1 + Taxa/100)^(idadeFim - idadeIni) * Tabela[Tabela[,1] == idadeFim,2] / Tabela[Tabela[,1] == idadeIni,2]
  
  # Compromisso do Segurado
  a12x_n = (Tabela[Tabela[,1] == idadeIni,5] - Tabela[Tabela[,1] == idadeFim,5]) / Tabela[Tabela[,1] == idadeIni,4] -
    11/24 * (1 - nEx)
  
  # Prêmio
  P = Beneficio * nEx / a12x_n
  
  return(P)
  
}

# b) Renda Mensal Vitalícia
rendaMV = function(Beneficio, idadeIni, idadeFim, Taxa, Tabela){
  
  # Compromisso da Seguradora
  nEx = 1 / (1 + Taxa/100)^(idadeFim - idadeIni) * Tabela[Tabela[,1] == idadeFim,2] / Tabela[Tabela[,1] == idadeIni,2]
  a12r = Tabela[Tabela[,1] == idadeFim,6] - 11/24 # Método alternativo encontrado no livro do Bower, página 151, fórmula 5.4.10
  
  # Compromisso do Segurado
  a12x_n = (Tabela[Tabela[,1] == idadeIni,5] - Tabela[Tabela[,1] == idadeFim,5]) / Tabela[Tabela[,1] == idadeIni,4] -
    11/24 * (1 - nEx)
  
  # Prêmio
  P = Beneficio * nEx * a12r / a12x_n
  
  return(P)
  
}

# c) Renda Mensal Temporária
rendaMT = function(Beneficio, idadeIni, idadeFim, Taxa, Tabela, Tempo){
  
  # Compromisso da Seguradora
  nEx = 1 / (1 + Taxa/100)^(idadeFim - idadeIni) * Tabela[Tabela[,1] == idadeFim,2] / Tabela[Tabela[,1] == idadeIni,2]
  tEr = 1 / (1 + Taxa/100)^(Tempo) * Tabela[Tabela[,1] == (idadeFim + Tempo),2] / Tabela[Tabela[,1] == idadeFim,2]
  a12r_t = ((Tabela[Tabela[,1] == idadeFim,5] - Tabela[Tabela[,1] == (idadeFim+Tempo),5]) / Tabela[Tabela[,1] == idadeIni, 4]) - 
    (11/24 * (1 - tEr))
  
  # Compromisso do Segurado
  a12x_n = (Tabela[Tabela[,1] == idadeIni,5] - Tabela[Tabela[,1] == idadeFim,5]) / Tabela[Tabela[,1] == idadeIni,4] -
    11/24 * (1 - nEx)
  
  # Prêmio
  P = Beneficio * nEx * a12r_t / a12x_n
  
  return(P)
  
}

# d) Renda Mensal Vitalícia com Prazo Mínimo Garantido
rendaMVcPMG = function(Beneficio, idadeIni, idadeFim, Taxa, Tabela, Tempo){
  
  # Compromisso da Seguradora
  nEx = 1 / (1 + Taxa/100)^(idadeFim - idadeIni) * Tabela[Tabela[,1] == idadeFim,2] / Tabela[Tabela[,1] == idadeIni,2]
  a12t = 1/12 * ((1-(1/(1+Taxa/100)^Tempo)) / (1-(1/(1+Taxa/100)^(1/12))))
  a12rt = Tabela[Tabela[,1] == (idadeIni + Tempo + 1),6]
  tEr = 1 / (1 + Taxa/100)^(Tempo) * Tabela[Tabela[,1] == (idadeFim + Tempo),2] / Tabela[Tabela[,1] == idadeFim,2]
  
  a12x_m_ = a12t + a12rt * tEr
  
  # Compromisso do Segurado
  a12x_n = (Tabela[Tabela[,1] == idadeIni,5] - Tabela[Tabela[,1] == idadeFim,5]) / Tabela[Tabela[,1] == idadeIni,4] -
    11/24 * (1 - nEx)
  
  P = Beneficio * nEx * a12x_m_ / a12x_n
  
  return(P)
  
}

# e) Renda Mensal por Prazo Certo
rendaMpPC = function(Beneficio, idadeIni, idadeFim, Taxa, Tabela, Tempo){
  
  # Compromisso da Seguradora
  nEx = 1 / (1 + Taxa/100)^(idadeFim - idadeIni) * Tabela[Tabela[,1] == idadeFim,2] / Tabela[Tabela[,1] == idadeIni,2]
  an = (1-(1/(1+Taxa/100)^Tempo)) / (1-(1/(1+Taxa/100)))
  
  a12n = 1/12 * (1-(1/(1+Taxa/100)^Tempo)) / (1-(1/(1+Taxa/100)^(1/12)))
  
  # Compromisso do Segurado
  a12x_n = (Tabela[Tabela[,1] == idadeIni,5] - Tabela[Tabela[,1] == idadeFim,5]) / Tabela[Tabela[,1] == idadeIni,4] -
    11/24 * (1 - nEx)
  
  P = Beneficio * a12n * nEx / a12x_n
  
  return(P)
  
}

options(scipen = 999)

sexo = c("Masculino", "Feminino")

pagamentoUnico = c(`Pagamento Único` = "Pagamento Único")

coberturas = c(`Renda Mensal Vitalícia` = "Renda Mensal Vitalícia", 
               `Renda Mensal Temporária` = "Renda Mensal Temporária", 
               `Renda Mensal Vitalícia com Prazo Mínimo Garantido` = "Renda Mensal Vitalícia com Prazo Mínimo Garantido", 
               `Renda Mensal por Prazo Certo` = "Renda Mensal por Prazo Certo")

ui <- fluidPage(
  
  theme = shinytheme("slate"),
  
  navbarPage("Simulador de Benefício Definido",
             id = "main-navbar",
             
             tabPanel("Início",
                      fluidRow(
                        column(width = 12,
                               h4(HTML("Simulador de benefício definido, elaborado pelos discentes:
                               <br/> - Nadson Ângelo, 
                               <br/> - Stephane Fernanda e 
                               <br/> - Rafael Orlandi; 
                               <br/> ambos da UFRN, 
                                       para a disciplina de Práticas Atuariais em Pensões, ministrada pela docente
                                       Jordana Cristina de Jesus, tendo relevância apenas para fins de prática acadêmica, 
                                       colocando em exercício o conteúdo ministrado em aulas.<br/> <br/>")))),
                      fluidRow(
                        h4(strong(HTML("Premissas Utilizadas: <br/>"))),
                        strong("Tábua de Vida: "),
                        em(HTML("BR-EMS 2020, pois reflete a realidade do mercado segurador no Brasil. <br/>")),
                        strong("Taxa de Juros: "),
                        em(HTML("6,4% (Média do CDI entre 01/01/2019 a 20/04/2022)."))
                      ),
                      
                      fluidRow(
                        column(width = 6,
                               h4(strong(HTML("<br/>
                               Abaixo segue a tabela contendo notação e fórmulas 
                               das coberturas utilizadas na atividade. <br/><br/>"))),
                               uiOutput("table")
                        )
                      ),
                      strong(HTML("Observação: ")),
                      HTML("<p> O código utilizado para a elaboração da aplicação encontra-se disponível em repositório 
                           GitHub, podendo ser acessado <a href='https://github.com/nadsu077/Projeto-BD'>clicando aqui!</a>!</p>")
             ),
             
             tabPanel("Simulador",
                      sidebarLayout(
                        sidebarPanel(width = 4,
                                     selectInput("seguro", "Seguro dotal puro - Pago no momento de elegibilidade", choices = pagamentoUnico),
                                     
                                     selectInput("cobertura", "Coberturas para aposentadoria", choices = coberturas),
                                     conditionalPanel(condition = "input.cobertura == 'Renda Mensal Temporária'",
                                                      numericInput("tempo", "Prazo dos Pagamentos Temporários", 15, min = 1, max = 100)),
                                     conditionalPanel(condition = "input.cobertura == 'Renda Mensal Vitalícia com Prazo Mínimo Garantido'",
                                                      numericInput("prazoM", "Prazo Mínimo Garantido", 15, min = 1, max = 100)),
                                     conditionalPanel(condition = "input.cobertura == 'Renda Mensal por Prazo Certo'",
                                                      numericInput("prazoC", "Prazo Certo", 15, min = 1, max = 100)),
                                    
                                     selectInput("sexo", "Sexo ", choices = sexo),
                                     
                                     numericInput("idadeIni", "Idade de entrada ", 30, min = 0, max = 117),
                                     
                                     numericInput("idadeFim", "Idade de aposentadoria", 65, min = 1, max = 117),
                                    
                                     numericInput("taxa", "Taxa de Juros\n ", 6.4, min = 0, max = 100),
                                     
                                     numericInput("valorSG", "Insira o valor do benefício do seguro a ser recebido", 700000, min = 1, max = 9*10^17),
                                     
                                     numericInput("valorAP", "Insira o valor do benefício de aposentadoria a ser recebida", 6000, min = 1, max = 9*10^17),
                                     actionButton("simular","Simular")
                                     
                        ),
                        sidebarPanel(width = 4,
                                     h4(htmlOutput("Premio")),
                                     br()
                        )
                      )
             )
  )
)

server <- function(input, output) {
  
  # Cálculo do valor do prêmio
  observeEvent(input$simular,{
    # Lendo as tábuas no R
    tabua = list()
    for(i in 2:5){
      
      a = readxl::read_excel(tmp, sheet = i, skip = 4)
      a = a[,-c(2,3,4,6)]
      
      a$V = (1/(1+input$taxa/100))^a$Idade
      a$Dx = a$V * a$lx
      a$Nx = do.call(rbind, lapply(1:nrow(a), FUN = function(rows){colSums(a[rows:nrow(a),"Dx"])}))[,1]
      a$ax = a$Nx/a$Dx
      
      tabua[[i-1]] = a
    }
    
    names(tabua) = c("Masculino - Sob", "Masculino - Mort",
                     "Feminino - Sob", "Feminino - Mort")
    
    # Output do prêmio baseado nas entradas (inputs)
    
    Tabua = if(input$sexo == "Masculino"){Tabua = tabua[[1]] %>% data.frame()} else {Tabua = tabua[[3]] %>% data.frame()}
    
    # a) Pagamento Único
    
    PremioSG = pgtoUnico(Beneficio = input$valorSG, # Valor do Benefício
                       idadeIni = input$idadeIni, # Idadei de Entrada
                       idadeFim = input$idadeFim, # Idade de Aposentadoria
                       Taxa = input$taxa, # Taxa de Rentabilidade
                       Tabela = Tabua # Selecionando a Tabua [Masculina ou Feminina]
                       )
      
    if(input$cobertura == "Renda Mensal Vitalícia"){ # b) Renda Mensal Vitalícia
      
      PremioAP = rendaMV(Beneficio = input$valorAP, # Valor do Benefício
                       idadeIni = input$idadeIni, # Idadei de Entrada
                       idadeFim = input$idadeFim, # Idade de Aposentadoria
                       Taxa = input$taxa, # Taxa de Rentabilidade
                       Tabela = Tabua # Selecionando a Tabua [Masculina ou Feminina]
      )
      
    } else if(input$cobertura == "Renda Mensal Temporária"){ # c) Renda Mensal Temporária
      
      PremioAP = rendaMT(Beneficio =  input$valorAP, # Valor do Benefício
                       idadeIni = input$idadeIni, # Idadei de Entrada
                       idadeFim = input$idadeFim, # Idade de Aposentadoria
                       Taxa = input$taxa, # Taxa de Rentabilidade
                       Tabela = Tabua, # Selecionando a Tabua [Masculina ou Feminina]
                       input$tempo # Período de pagamentos temporários
      )
      
    } else if(input$cobertura == "Renda Mensal Vitalícia com Prazo Mínimo Garantido"){ # d) Renda Mensal Vitalícia com Prazo Mínimo Garantido
      
      PremioAP = rendaMVcPMG(Beneficio =  input$valorAP, # Valor do Benefício
                           idadeIni =   input$idadeIni, # Idadei de Entrada
                           idadeFim = input$idadeFim, # Idade de Aposentadoria
                           Taxa = input$taxa, # Taxa de Rentabilidade
                           Tabela = Tabua, # Selecionando a Tabua [Masculina ou Feminina]
                           Tempo = input$prazoM # Prazo Mínimo Garantido
      )
      
    } else if(input$cobertura == "Renda Mensal por Prazo Certo"){ # e) Renda Mensal por Prazo Certo
      
      PremioAP = rendaMpPC(Beneficio =  input$valorAP, # Valor do Benefício
                         idadeIni = input$idadeIni, # Idadei de Entrada
                         idadeFim = input$idadeFim, # Idade de Aposentadoria
                         Taxa = input$taxa, # Taxa de Rentabilidade
                         Tabela = Tabua, # Selecionando a Tabua [Masculina ou Feminina]
                         Tempo = input$prazoC #Prazo Certo
      )
      
    }
    
    textoAdicional = ""
    
    if(input$cobertura == "Renda Mensal Temporária"){
      textoAdicional = paste0("<br/><br/> Com duração de pagamentos por ",
                              input$tempo, " anos.")
    } else if(input$cobertura == "Renda Mensal Vitalícia com Prazo Mínimo Garantido"){
      textoAdicional = paste0("<br/><br/> Com pagamentos certos durante os primeiros ",
                              input$prazoM, " anos, a partir da elegibilidade, tornando-se vitalícia após o período.")
    } else if(input$cobertura == "Renda Mensal por Prazo Certo"){
      textoAdicional = paste0("<br/><br/> Com pagamentos certos durante ",
                              input$prazoM, " anos.")
    }
    
    Premio = paste0("O contratante realizará pagamentos mensais de: <br/><br/> R$ ", 
                    format(round(PremioAP, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
                    "<br/><br/> Desde a entrada na idade de ", input$idadeIni, " anos",
                    " até a idade de ", input$idadeFim - 1, " anos. ",
                    "A partir dos ", input$idadeFim, " anos terá início os pagamentos do benefício da ",
                    input$cobertura,", no valor de: <br/><br/> R$ ", 
                    format(round(input$valorAP, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
                    textoAdicional,
                    "<br/><br/> Caso opte pela cobertura do seguro, 
                    o contratante realizará pagamentos mensais de: <br/><br/> R$ ",
                    format(round(PremioSG, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
                    "<br/><br/> até a idade de ", input$idadeFim - 1, " anos, onde o benefício de: <br/><br/> R$ ",
                    format(round(input$valorSG, 2), nsmall = 2, big.mark = ".", decimal.mark = ","),
                    "<br/><br/> será pago a partir do momento em que o contratante completar ", input$idadeFim, " anos.")
    
    output$Premio = renderUI({HTML(Premio)})
    
  }
  )
  
  output$table = renderUI({
    cob = c(
      "\\text{Pagamento Único}",
      "\\text{Renda Mensal Vitalícia}",
      "\\text{Renda Mensal Temporária}",
      "\\text{Renda Mensal Vitalícia com Prazo Mínimo Garantido}",
      "\\text{Renda Mensal por Prazo Certo}"
    )
    
    not = c(
      "{}_{n}E_x",
      "\\ddot{a}^{(12)}_r * {}_{n}E_x",
      "\\ddot{a}^{(12)}_{r:\\overline{t|}} * {}_{n}E_x",
      "\\ddot{a}^{(12)}_{\\overline{r:\\overline{t|}}} * {}_{n}E_x",
      "\\ddot{a}^{(12)}_{\\overline{n|}} * {}_{n}E_x"
    )
    
    descr = c(
      "\\text{Seguro dotal puro; pagamento único em caso de sobrevivência ao período de deferimento}",
      "\\text{Pago após a aposentadoria mensalmente até a data de óbito}",
      "\\text{Renda paga temporariamente até o beneficiário vir a óbito ou existe o fim da temporariedade contratada}",
      "\\text{Renda com pagamento certo nos primeiros t anos e vitalícia após o prazo}", 
      "\\text{Renda com pagamento certo até n anos}"
    )
    
    form = c(
      "V^n * {}_{n}p_x",
      "(\\ddot{a}_r - \\frac{11}{24}) * {}_{n}E_x",
      "\\Big[ \\ddot{a}_{r:\\overline{t|}} - \\frac{11}{24} * (1 - {}_{t}E_r) \\Big] * {}_{n}E_x",
      "\\Big[ \\ddot{a}^{(12)}_{\\overline{t|}} + \\ddot{a}^{(12)}_{r+t} * {}_{t}E_r \\Big] * {}_{n}E_x",
      "\\frac{1}{12} * \\Big[ \\frac{1 - V^n}{1 - V^{1/12}} \\Big] * {}_{n}E_x")
    
    tab = data.frame(Cobertura = cob,
                     `Notação` = not,
                     `Descrição` = descr,
                     `Fórmula` = form)
    
    latexTab = print(xtable::xtable(tab, align = rep("c", ncol(tab)+1)),
                     floating = F, tabular.environment = "array", comment = F,
                     print.results = F, 
                     sanitize.text.function = function(x) x)
    
    tagList(
      withMathJax(),
      HTML(paste0("$$", latexTab, "$$"))
    )
    
  })
  
}

shinyApp(ui = ui, server = server)
