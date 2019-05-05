####
# Autor: Pedro Nascimento de Lima, 2017
# Código fonte desenvolvido para a Dissertação de Mestrado.
# Arquivo: funcoes.R
# Objetivo: Este arquivo contém funções utilizadas para as análises 
# RDM realizadas durante a dissertação.
####

# Bibliotecas Utilizadas
library(plotly)
library(lhs)
library(deSolve)
library(dplyr)
library(ggplot2)
library(GGally)
library(viridis)
library(season)
library(gridExtra)
library(akima)
library(parallel)
library(FME)
library(readxl)
library(gdata)
library(scales)
library(Quandl)
library(tidyr)

# library(prim)

##### CONSTANTES #####
VAR_SCENARIO = "Scenario"
VAR_LEVER = "Lever"

##### MODELO #####

#' solve_modelo_dissertacao
#' Inicializa Parâmetros e Estoques do Modelo da Dissertação, e retorna os resultados do modelo como um dataframe.
#'
#' @param parametros Vetor de parâmetros a serem utilizados na simulação. Deve incluir parâmetros para a inicialização dos estoques.
#' @param modelo Modelo de Dinâmica de Sistemas  (conforme os moldes da biblioteca deSolve).
#' @param simtime Vetor de Tempo da Simulação.
#'
#' @return matriz com resultados da simulação.
#' @export
#' 
solve_modelo_dissertacao <- function(parametros, modelo, simtime){
  
  # Número de Players no modelo
  N_PLAYERS <<- 4
  
  # All the stocks are initialised here...
  
  n_tempo = length(simtime)
  
  ordem_vetores_players = order(names(parametros[grep("aSwitchForCapacityStrategy", x = names(parametros))]))
  
  ##### VARIÁVEIS DE ENTRADA - AUXILIARES #####
  auxs    <- list(
                  # Variáveis informadas de modo Independente por Player:
                  # Estratégia de Capacidade:
                  # A ordem dos números a seguir obedece a ordem na planilha e a ordem na geração do ensemble.
                  aSwitchForCapacityStrategy = unname(round(parametros[grep("aSwitchForCapacityStrategy", x = names(parametros))][c(4,1,2,3)], 0))
                  ,aDesiredMarketShare = unname(parametros[grep("aDesiredMarketShare", x = names(parametros))][c(4,1,2,3)])
                  # Variáveis de Decisão - Existem para o player analisado e para os outros Players:
                  ,aOrcamentoPeD =  unname(parametros[grep("aOrcamentoPeD", x = names(parametros))][c(4,1,2,3)])
                  ,aPercPeDAberto =  unname(parametros[grep("aPercPeDAberto", x = names(parametros))][c(4,1,2,3)])
                  # A Initial Price
                  ,aInitialPrice = unname(parametros[grep("aInitialPrice", x = names(parametros))])
                  ,aPatentShare = unname(parametros[grep("aPatentShare", x = names(parametros))])
                  ,aInitialSharePlayers = unname(parametros[grep("aInitialSharePlayers", x = names(parametros))])
                  
                  
                  # Outras Variáveis.
                  ,aDiscountRate = unname(parametros["aDiscountRate"])
                  ,aNormalDeliveryDelay = unname(parametros["aNormalDeliveryDelay"])
                  ,aSwitchForCapacity = unname(parametros["aSwitchForCapacity"])
                  # Vamos testar apenas um parâmetro por enquanto
                  ,aFractionalDiscardRate = unname(parametros["aFractionalDiscardRate"]) # unname(pars["aFractionalDiscardRate"]) # Original 0.1
                  ,aInitialDiffusionFraction = unname(parametros["aInitialDiffusionFraction"])
                  ,aReferencePrice = unname(parametros["aReferencePrice"])
                  ,aReferenceIndustryDemandElasticity = unname(parametros["aReferenceIndustryDemandElasticity"])
                  ,aReferencePopulation = unname(parametros["aReferencePopulation"])
                  ,aInnovatorAdoptionFraction = unname(parametros["aInnovatorAdoptionFraction"])
                  ,aWOMStrength = unname(parametros["aWOMStrength"]) # unname(pars["aWOMStrength"]) # Original 1
                  ,aPopulation = unname(parametros["aPopulation"]) #100000000 # Original Sterman: 100000000
                  ,aUnitsPerHousehold = unname(parametros["aUnitsPerHousehold"])
                  ,aSwitchForShipmentsInForecast = unname(parametros["aSwitchForShipmentsInForecast"])
                  ,aVolumeReportingDelay = unname(parametros["aVolumeReportingDelay"])
                  ,aForecastHorizon = unname(parametros["aForecastHorizon"])
                  ,aCapacityAcquisitionDelay = unname(parametros["aCapacityAcquisitionDelay"])
                  ,aTimeForHistoricalVolume = unname(parametros["aTimeForHistoricalVolume"])
                  # Market Sector
                  ,aReferenceDeliveryDelay = unname(parametros["aReferenceDeliveryDelay"])
                  ,aSensOfAttractToAvailability = unname(parametros["aSensOfAttractToAvailability"])
                  ,aSensOfAttractToPrice = unname(parametros["aSensOfAttractToPrice"])
                  # Learning Curve Params
                  ,aLCStrength = unname(parametros["aLCStrength"])
                  ,aInitialProductionExperience = rep(unname(parametros["aInitialProductionExperience"]), times = N_PLAYERS)
                  ,aRatioOfFixedToVarCost = unname(parametros["aRatioOfFixedToVarCost"])
                  ,aNormalProfitMargin = unname(parametros["aNormalProfitMargin"])
                  ,aNormalCapacityUtilization = unname(parametros["aNormalCapacityUtilization"])
                  #Target Capacity Sector
                  ,aMinimumEfficientScale = unname(parametros["aMinimumEfficientScale"]) # Original 100000
                  
                  # Esta variavel é desdobrada por player.
                  ,aWeightOnSupplyLine= unname(parametros["aWeightOnSupplyLine"])
                  ,aTimeToPerceiveCompTargetCapacity = unname(parametros["aTimeToPerceiveCompTargetCapacity"])
                  
                  # Price Sector
                  ,aPriceAdjustmentTime = unname(parametros["aPriceAdjustmentTime"])
                  ,aSensOfPriceToCosts = unname(parametros["aSensOfPriceToCosts"])
                  ,aSensOfPriceToDSBalance = unname(parametros["aSensOfPriceToDSBalance"])
                  ,aSensOfPriceToShare = unname(parametros["aSensOfPriceToShare"])
                  # Capacity Sector
                  ,aSwitchForPerfectCapacity = unname(parametros["aSwitchForPerfectCapacity"])
                  
                  # Pesquisa e Desenvolvimento
                  ,aPeDLigado = unname(parametros["aPeDLigado"])
                  
                  ,aTempoMedioRealizacaoPeD = unname(parametros["aTempoMedioRealizacaoPeD"])
                  ,aCustoMedioPatente = unname(parametros["aCustoMedioPatente"])
                  ,aTempoMedioAvaliacao = unname(parametros["aTempoMedioAvaliacao"])
                  ,aTaxaRejeicao = unname(parametros["aTaxaRejeicao"])
                  ,aTempoVencimentoPatentes = unname(parametros["aTempoVencimentoPatentes"])
                  ,aTempodeInutilizacaoPatente = unname(parametros["aTempodeInutilizacaoPatente"])
                  ,aPerfSlope = unname(parametros["aPerfSlope"])
                  ,aPerfMin = unname(parametros["aPerfMin"])
                  ,aPerfMax = unname(parametros["aPerfMax"])
                  ,aSensOfAttractToPerformance = unname(parametros["aSensOfAttractToPerformance"])
                  ,aReferencePerformance = unname(parametros["aReferencePerformance"])
                  
                  ,aInitialInvestimentoNaoRealizadoPeD = rep(unname(parametros["aInitialInvestimentoNaoRealizadoPeD"]), times = N_PLAYERS)
                  ,aInitialPatentesRequisitadas = rep(unname(parametros["aInitialPatentesRequisitadas"]), times = N_PLAYERS)
                  ,aInitialPatentesEmpresa = rep(unname(parametros["aInitialPatentesEmpresa"]), times = N_PLAYERS)
                  ,aInitialsPatentesEmDominioPublicoUteis = unname(parametros["aInitialsPatentesEmDominioPublicoUteis"])
                  ,aInitialsInvestimentoPeDDepreciar = rep(unname(parametros["aInitialsInvestimentoPeDDepreciar"]), times = N_PLAYERS)
                  ,aInitialPatentLefts = unname(parametros["aInitialPatentLefts"])
                  # Novas Variáveis de Condições Iniciais:
                  
                  ,aInitialReorderShare =unname(parametros["aInitialReorderShare"])
                  ,aTotalInitialInstalledBase = unname(parametros["aTotalInitialInstalledBase"])
                  ,aInitialIndustryShipments = unname(parametros["aInitialIndustryShipments"])
                  ,aModoInitialCumulativeAdopters = unname(parametros["aModoInitialCumulativeAdopters"])
                  
                  
                  # Variáveis Adicionais
                  ,Scenario = unname(parametros["Scenario"])
                  ,Lever = unname(parametros["Lever"])
  )
  
  
  ##### VARIÁVEIS DE ENTRADA - ESTOQUES INICIAIS, SEM AJUSTES #####
  
  # Informando Estoques Iniciais, sem ajustes, apenas para calcular o primeiro tempo.
  stocks_iniciais  <- c(
    sNPVProfit = rep(0, times = N_PLAYERS)
    ,sValueOfBacklog = rep(100000, times = N_PLAYERS)
    ,sBacklog = rep(100, times = N_PLAYERS) 
    ,sInstalledBase = rep(2000, times = N_PLAYERS)  # rep(30000, times = N_PLAYERS) # Este estoque possui uma fórmula, verificar como fazer aqui no R.
    ,sPrice = unname(auxs$aInitialPrice)
    ,sCumulativeAdopters = 2000 # Este estoque possui uma fórmula, verificar como fazer aqui no R.
    # Teste 28/12/10:05: Removendo a Replicação Inicial desta variável.
    # ,sReportedIndustryVolume = rep(101904, times = N_PLAYERS)
    ,sReportedIndustryVolume = 100
    ,sCumulativeProduction = rep(1e+007, times = N_PLAYERS) # Este estoque possui formula
    ,sPerceivedCompTargetCapacity = rep(1000, times = N_PLAYERS) # Este estoque possui formula
    ,sSmoothCapacity1 = rep(100, times = N_PLAYERS) # Este estoque possui formula
    ,sSmoothCapacity2 = rep(100, times = N_PLAYERS) # Este estoque possui formula
    ,sSmoothCapacity3 = rep(100, times = N_PLAYERS) # Este estoque possui formula
    
    ,sInvestimentoNaoRealizadoPeD = rep(1000, times = N_PLAYERS)
    ,sPatentesRequisitadas = rep(100, times = N_PLAYERS)
    ,sPatentesEmpresa = rep(100, times = N_PLAYERS)
    ,sPatentesEmDominioPublicoUteis = 200
    ,sInvestimentoPeDDepreciar = rep(1000, times = N_PLAYERS)
    ,sPatentLefts = 0
    
  ) 
  
  # Calculando estoques para o t0.
  iteracoes_aquecimento_estoques = 3
  
  for(i in 1:iteracoes_aquecimento_estoques){
    
    # Criando List com variaveis globais - Antes de Inicializar o Modelo.
    list.variaveis.globais <<- list(
      # sReportedIndustryVolume = matrix(NA, ncol = N_PLAYERS, nrow = n_tempo),
      sReportedIndustryVolume = matrix(NA, ncol = 1, nrow = n_tempo),
      aExpectedIndustryDemand = matrix(NA, ncol = 1, nrow = n_tempo),
      aIndustryShipments = matrix(NA, ncol = 1, nrow = n_tempo)
    )
    
    estoques_calculados = modelo(time = START, stocks = stocks_iniciais, auxs = auxs, modo = "inicial")
    
    stocks_iniciais  <- c(
      sNPVProfit = unname(stocks_iniciais[grep("sNPVProfit", x = names(stocks_iniciais))]) 
      ,sValueOfBacklog = unname(estoques_calculados$ValueOfBacklogIni)
      ,sBacklog = unname(estoques_calculados$BacklogIni)
      ,sInstalledBase = unname(estoques_calculados$InstalledBaseIni)
      ,sPrice = unname(stocks_iniciais[grep("sPrice", x = names(stocks_iniciais))])
      ,sCumulativeAdopters = unname(estoques_calculados$CumulativeAdoptersIni)
      ,sReportedIndustryVolume = unname(estoques_calculados$ReportedIndustryVolumeIni)
      ,sCumulativeProduction = unname(estoques_calculados$CumulativeProductionIni)
      ,sPerceivedCompTargetCapacity = unname(estoques_calculados$PerceivedCompTargetCapacityIni)
      ,sSmoothCapacity1 = unname(estoques_calculados$CapacityIni)
      ,sSmoothCapacity2 = unname(estoques_calculados$CapacityIni)
      ,sSmoothCapacity3 = unname(estoques_calculados$CapacityIni)
      ,sInvestimentoNaoRealizadoPeD = unname(estoques_calculados$InitialInvestimentoNaoRealizadoPeD)
      ,sPatentesRequisitadas = unname(estoques_calculados$InitialPatentesRequisitadas)
      ,sPatentesEmpresa = unname(estoques_calculados$InitialPatentesEmpresa)
      ,sPatentesEmDominioPublicoUteis = unname(estoques_calculados$InitialsPatentesEmDominioPublicoUteis)
      ,sInvestimentoPeDDepreciar = unname(estoques_calculados$InitialsInvestimentoPeDDepreciar)
      ,sPatentLefts = unname(estoques_calculados$IntialPatentLefts)
    ) 
    
  }
  
  stocks = stocks_iniciais
  
  # Criando List Novamente, para manter a list limpa.
  list.variaveis.globais <<- list(
    # sReportedIndustryVolume = matrix(NA, ncol = N_PLAYERS, nrow = n_tempo),
    sReportedIndustryVolume = matrix(NA, ncol = 1, nrow = n_tempo),
    aExpectedIndustryDemand = matrix(NA, ncol = 1, nrow = n_tempo),
    aIndustryShipments = matrix(NA, ncol = 1, nrow = n_tempo)
  )
  
  
  resultado_completo = data.frame(deSolve::ode(y=stocks, simtime, func = modelo, 
                                      parms=auxs, method="euler"))
  # Posso filtrar os resultados ou não:
  # resultado_completo[variaveis_calibracao]
  resultado_completo
}

##### MODELO ####
#' modelo
#'
#' Esta função contém o modelo de equações diferenciais empregado pelo trabalho. Este modelo específico foi baseado no modelo de Sterman (2007) (Getting Big Too Fast: Strategic Dynamics with Increasing Returns and Bounded Rationality) e possui diveras modificações para representar o caso da manufatura aditiva (destacando-se o setor de P&D).
#' @param time tempo a ser simulado (inteiro)
#' @param stocks objeto com estoques a serem computados pelo modelo
#' @param auxs objeto com vetor de parâmetros utilizados pelo modelo
#' @param modo "completo" (realiza a simulação completa) ou "inicial" (calcula valores iniciais de estoques).
#'
#' @return matriz com resultados das equações (de um dt).
#' @export
#'
modelo <- function(time, stocks, auxs, modo = "completo"){
  with(as.list(c(stocks, auxs)),{
    
    
    # Variáveis Necessárias para tratar período histórico de modo diferente.
    if(SIMULAR_HISTORICO_DIFERENTE){
      ano_futuro_inicial = ANO_INICIO_AVALIACAO  
    } else {
      ano_futuro_inicial = START
    }
    
    
    ## Browser: Se o Tempo for igual ao Start, verificar se existe algum estoque negativo.
    if(time == START){
      if(any(stocks<0)){
        browser ()
      }
    }
    
    variaveis_periodo_historico = list(
      # Todas as Estratégias no Cenário Base são agressivas
      aSwitchForCapacityStrategy = rep(1, times = N_PLAYERS),
      
      # O Lucro não é computado no período Histórico,
      fNPVProfitChange = rep(0, times = N_PLAYERS),
      
      # Nenhum Player possui política de PeD aberto, com excessão do player 4 (outros).
      aPercPeDAberto = c(rep(0, times = N_PLAYERS-1), 0.1),
      
      # Os players desejam o mesmo market share inicial que tem inicialmente.
      aDesiredMarketShare = unname(aInitialSharePlayers),
      
      # O Orçamento de P e D de todos os players é 0.06 (valor inicial da 3D Systems).
      aOrcamentoPeD = rep(0.6, N_PLAYERS)
    )
    
    # Variáveis que podem ser definidas no início (são parâmetros não calculados):
    # Se estou no período de histórico da simulação, usar parâmetros do período histórico.
    if(time < ano_futuro_inicial) {
      aSwitchForCapacityStrategy = variaveis_periodo_historico$aSwitchForCapacityStrategy
      aPercPeDAberto = variaveis_periodo_historico$aPercPeDAberto
      aDesiredMarketShare = variaveis_periodo_historico$aDesiredMarketShare
      aOrcamentoPeD = variaveis_periodo_historico$aOrcamentoPeD
    }
    

    
    # Criando uma variavel n_tempo local
    n_tempo = nrow(list.variaveis.globais$sReportedIndustryVolume)
    
 
    ##### VETORIZANDO ESTOQUES #####
    #Estoques Vetorizados = substituindo estoques pela forma vetorizada (pra que seja possivel formular equações de forma mais simples).
    # Esta implementação tem por objetivo não gerar a necessidade de referenciar os estoque spelo seu nome único
    sNPVProfit = stocks[grep("sNPVProfit", x = names(stocks))]
    sValueOfBacklog = stocks[grep("sValueOfBacklog", x = names(stocks))]
    sBacklog = stocks[grep("sBacklog", x = names(stocks))]
    sInstalledBase = stocks[grep("sInstalledBase", x = names(stocks))]
    sPrice = stocks[grep("sPrice", x = names(stocks))]
    sCumulativeAdopters = stocks[grep("sCumulativeAdopters", x = names(stocks))]
    sReportedIndustryVolume = stocks[grep("sReportedIndustryVolume", x = names(stocks))]
    sCumulativeProduction = stocks[grep("sCumulativeProduction", x = names(stocks))]
    sPerceivedCompTargetCapacity = stocks[grep("sPerceivedCompTargetCapacity", x = names(stocks))]
    sSmoothCapacity1 = stocks[grep("sSmoothCapacity1", x = names(stocks))]
    sSmoothCapacity2 = stocks[grep("sSmoothCapacity2", x = names(stocks))]
    sSmoothCapacity3 = stocks[grep("sSmoothCapacity3", x = names(stocks))]
    
    sInvestimentoNaoRealizadoPeD = stocks[grep("sInvestimentoNaoRealizadoPeD", x = names(stocks))]
    sPatentesRequisitadas = stocks[grep("sPatentesRequisitadas", x = names(stocks))]
    sPatentesEmpresa = stocks[grep("sPatentesEmpresa", x = names(stocks))]
    sPatentesEmDominioPublicoUteis = stocks[grep("sPatentesEmDominioPublicoUteis", x = names(stocks))]
    sInvestimentoPeDDepreciar = stocks[grep("sInvestimentoPeDDepreciar", x = names(stocks))]
    sPatentLefts = stocks[grep("sPatentLefts", x = names(stocks))]
    
    
    #Obtendo o número da linha no qual estou
    linha = ((time - START)* (n_tempo - 1)) / (FINISH - START) + 1
    
    # Gravando a Variável sReportedIndustryVolume no vetor global
    list.variaveis.globais$sReportedIndustryVolume[linha,] <<- sReportedIndustryVolume
    
    
    ##### DIFFUSION SECTOR  - PT 1 #####
    aDemandCurveSlope = - aReferenceIndustryDemandElasticity * (aReferencePopulation / aReferencePrice )
    
    aLowestPrice = min(sPrice)
    
    aIndustryDemand = min(
      aPopulation,
      aReferencePopulation * max(
        0,
        1 + aDemandCurveSlope * (aLowestPrice - aReferencePrice) / aReferencePopulation
      )
    )
    
    checkIndustryDemand = aIndustryDemand
    
    
    ##### CONDIÇÕES INICIAIS - CUMULATIVE ADOPTERS #####
    
    # Calculando Variáveis Necessárias para Definir os Cumulative Adopters
    
    aTotalInitialInstalledBase = min(aIndustryDemand,
                                     (aInitialReorderShare * aInitialIndustryShipments) / aFractionalDiscardRate) 
    
    aEstimatedAdopters = aTotalInitialInstalledBase / aUnitsPerHousehold
    
    aInitialNewAdoptersOrderRate = aInitialIndustryShipments*(1-aInitialReorderShare)
    
    aInitialAdoptionRate = aInitialNewAdoptersOrderRate / aUnitsPerHousehold
    
    
    
    # Initial Cumulative Adopters 1 - Opção Original
    aInitialCumulativeAdopters1 = aInitialDiffusionFraction * aIndustryDemand
    
    
    # Initial Cumulative Adopters 2 - Opção Calculada pelo Reorder Share
    aInitialCumulativeAdopters2 = aTotalInitialInstalledBase / aUnitsPerHousehold
    
    
    # Initial Cumulative Adopters 3 - Opção calculada pelo Initial Adoption Rate:
    aInitialCumulativeAdopters3 = (aInitialAdoptionRate/(aIndustryDemand-aEstimatedAdopters))*(aPopulation/aWOMStrength)
    
    
    aInitialCumulativeAdopters = if(aModoInitialCumulativeAdopters == 1) {
      aInitialCumulativeAdopters1
    } else if (aModoInitialCumulativeAdopters == 2) {
      aInitialCumulativeAdopters2
    } else {
      aInitialCumulativeAdopters3
    }
    
    ##### DIFFUSION SECTOR  - PT 2 #####
    
    aNonAdopters = aIndustryDemand - sCumulativeAdopters
    
    checkNonAdopters = aNonAdopters
    
    # Ajuste temporário: Colocar o adoption Rate como Fluxo apenas positivo.
    
    fAdoptionRate = max(0, 
                        aNonAdopters * (aInnovatorAdoptionFraction + aWOMStrength * sCumulativeAdopters/aPopulation)) 
    
    checkAdoptionRate = fAdoptionRate
    
    ##### ORDERS SECTOR - PT 1 #####
    
    fDiscardRate = sInstalledBase * aFractionalDiscardRate
    
    ##### INDUSTRY DEMAND SECTOR #####
    
    fReorderRate = sum(fDiscardRate)
    
    aInitialOrderRate = aUnitsPerHousehold * fAdoptionRate
    
    fIndustryOrderRate = fReorderRate + aInitialOrderRate
    
    # if(modo == "completo") {
    #   browser()
    # }
    
    
    checkIndustryOrderRate = fIndustryOrderRate
    
    ##### ORDERS SECTOR - PT 2 #####
    
    aDesiredShipments = sBacklog / aNormalDeliveryDelay
    
    ### CAPACITY SECTOR - PT 1 ####
    
    aCapacity = aSwitchForPerfectCapacity * (aDesiredShipments / aNormalCapacityUtilization) + (1-aSwitchForPerfectCapacity) * sSmoothCapacity3
    
    aNormalProduction = aCapacity * aNormalCapacityUtilization
    
    aIndustryNormalProduction = sum(aNormalProduction)
    
    ##### ORDERS SECTOR - PT 3 #####
    
    fShipments = aSwitchForCapacity * pmin(aDesiredShipments, aCapacity) + (1-aSwitchForCapacity) * aDesiredShipments
    
    aCapacityUtilization = fShipments / aCapacity
    
    aIndustryShipments = sum(fShipments)
    
    list.variaveis.globais$aIndustryShipments[linha,] <<- aIndustryShipments
    
    # Calculando a Variação no Industry Shipments - Ajuda a Calcular a Variação Percentual em Demanda para Avaliar a Plausibilidade do Modelo (principalmente em relação às condições iniciais).
    VariacaoDemanda = if(time == START){
      ((aIndustryShipments - aInitialIndustryShipments) / aInitialIndustryShipments)
    } else {
      ((aIndustryShipments - list.variaveis.globais$aIndustryShipments[linha-1,]) / list.variaveis.globais$aIndustryShipments[linha-1,])
    }
    
    # if(modo == "completo"){
    #   browser()
    # }
    
    aMarketShare = fShipments / aIndustryShipments
    
    aDeliveryDelay = sBacklog / fShipments
    
    checkIndustryShipments = aIndustryShipments
    
    ##### MARKET SECTOR #####
    
    # Patentes e Performance
    

    
    aPatentesEmpresaTemAcesso = sPatentesRequisitadas + sPatentesEmpresa + sPatentesEmDominioPublicoUteis + sPatentLefts
    
    aPerformanceCalculada = aPerfSlope * aPatentesEmpresaTemAcesso
    
    aPerformance = pmax(aPerfMin, pmin(aPerfMax, aPerformanceCalculada))
    
    checkPerformance = mean(aPerformance)
    
    aAttractivenessFromPerformance = aPeDLigado * exp(aSensOfAttractToPerformance*(aReferencePerformance/aPerformance)) + (1 - aPeDLigado)
    
    aAttractivenessFromAvailability = exp(aSensOfAttractToAvailability*(aDeliveryDelay/aReferenceDeliveryDelay))
    
    aAttractivenessFromPrice = exp(aSensOfAttractToPrice*(sPrice/aReferencePrice))
    
    aAttractiveness = aAttractivenessFromAvailability * aAttractivenessFromPrice * aAttractivenessFromPerformance

    aTotalAttractiveness = sum(aAttractiveness)
    
    aOrderShare = aAttractiveness / aTotalAttractiveness
    
    # if(time == FINISH){
    #   browser()  
    # }
    # 
    
    
    ##### ORDERS SECTOR - PT 3 #####
    
    fOrders = fIndustryOrderRate * aOrderShare
    
    checkOrders = sum(fOrders)
    
    ##### EXPECTED INDUSTRY DEMAND SECTOR #####
    
    aInitialDemandForecast = fReorderRate
    
    aIndustryVolume = pmax(aInitialDemandForecast,
                           aSwitchForShipmentsInForecast*aIndustryShipments+
                             (1-aSwitchForShipmentsInForecast)*fIndustryOrderRate)
    
    
    # Variavel com SMOOTH - Primeira Ordem: - Retirando o DT, o calculo funcionou corretamente!
    fsmooth_ReportedIndustryVolume = ((aIndustryVolume - sReportedIndustryVolume) / aVolumeReportingDelay) # * STEP # Multiplicando pelo step para ajustar o calculo.
    
    # Variavel com DELAY - A definição das constantes aqui devem ser alteradas se as condicoes iniciais do modelo mudarem
    # Esta implementacao considera que os delays sempre serao iguais. Se os delays nao forem iguais, deve-se encontrar outra forma de implementar os delays (talvez com a equacao multiplicativa 1*(time > tempodelay)
    if((time - START) > aTimeForHistoricalVolume) {
      nlinhas_delay = aTimeForHistoricalVolume / STEP
      aLaggedIndustryVolume = list.variaveis.globais$sReportedIndustryVolume[(linha - nlinhas_delay),]
    } else {
      aLaggedIndustryVolume = list.variaveis.globais$sReportedIndustryVolume[1,]
    }
    
    if(aLaggedIndustryVolume < 0) {
      browser()
    }

    aExpGrowthInVolume =  log(sReportedIndustryVolume/aLaggedIndustryVolume)/aTimeForHistoricalVolume
    
    aExpectedIndustryDemand = sReportedIndustryVolume*exp(aForecastHorizon*aCapacityAcquisitionDelay*aExpGrowthInVolume)
    
    list.variaveis.globais$aExpectedIndustryDemand[linha,] <<- aExpectedIndustryDemand
    
    
    # Mais uma variável com delay
    if((time - START) > aCapacityAcquisitionDelay) {
      nlinhas_delay = aCapacityAcquisitionDelay / STEP
      aLaggedVolumeForecast = list.variaveis.globais$aExpectedIndustryDemand[linha-nlinhas_delay,]
    } else {
      aLaggedVolumeForecast = list.variaveis.globais$aExpectedIndustryDemand[1,]
    }
    
    aForecastError = (aLaggedVolumeForecast - aIndustryVolume)/(1e-009+aIndustryVolume)
    
    checkLaggedVolumeForecast = mean(aLaggedVolumeForecast)
    
    ##### TARGET CAPACITY SECTOR #####
    
    aIndustryCapacity = sum(aCapacity)
    
    aCompetitorCapacity = aIndustryCapacity - aCapacity
    
    aExpectedCompCapacity = aNormalCapacityUtilization*(aWeightOnSupplyLine*sPerceivedCompTargetCapacity+(1-aWeightOnSupplyLine)*aCompetitorCapacity)
    
    aUncontestedDemand = pmax(0, aExpectedIndustryDemand - aExpectedCompCapacity)
    
    aUncontestedMarketShare = aUncontestedDemand / aExpectedIndustryDemand
    
    # Definindo Estratégia no Cenário Base
    
    
    
    aSwitchForCapacityStrategy1 = ifelse(aSwitchForCapacityStrategy == 1, 1, 0)
    aSwitchForCapacityStrategy2 = ifelse(aSwitchForCapacityStrategy == 2, 1, 0)
    aSwitchForCapacityStrategy3 = ifelse(aSwitchForCapacityStrategy == 3, 1, 0)
    aSwitchForCapacityStrategy4 = ifelse(aSwitchForCapacityStrategy == 4, 1, 0)
    
    aTargetMarketShare = {
      aSwitchForCapacityStrategy1*pmax(aDesiredMarketShare,aUncontestedMarketShare) +
        aSwitchForCapacityStrategy2*pmin(aDesiredMarketShare,aUncontestedMarketShare) +
        aSwitchForCapacityStrategy3*aDesiredMarketShare +
        aSwitchForCapacityStrategy4*aUncontestedMarketShare
    }
    
    
    
    aTargetCapacity = pmax(aMinimumEfficientScale,
                           aTargetMarketShare*aExpectedIndustryDemand/aNormalCapacityUtilization)
    
    
    aTargetNormalProduction = aTargetCapacity * aNormalCapacityUtilization
    
    aIndustryTotalTargetCapacity = sum(aTargetCapacity)
    
    aCompetitorTargetCapacity = aIndustryTotalTargetCapacity - aTargetCapacity
    
    fChangePerceivedCompTargetCapacity = (aCompetitorTargetCapacity - sPerceivedCompTargetCapacity) / aTimeToPerceiveCompTargetCapacity
    
    checkCompetitorTargetCapacity = mean(aCompetitorTargetCapacity)
    
    ##### CAPACITY SECTOR  - PT 2 - FLUXOS #####
    fchangeSmoothCapacity1 = (aTargetCapacity - sSmoothCapacity1) / (aCapacityAcquisitionDelay / 3)
    fchangeSmoothCapacity2 = (sSmoothCapacity1 - sSmoothCapacity2) / (aCapacityAcquisitionDelay / 3)
    fchangeSmoothCapacity3 = (sSmoothCapacity2 - sSmoothCapacity3) / (aCapacityAcquisitionDelay / 3)
    
    
    
    ##### Custo P e D ####
    aTempoDepreciacao = aTempoMedioAvaliacao + aTempoVencimentoPatentes + aTempoMedioRealizacaoPeD
    
    fDepreciacaoInvPeD = sInvestimentoPeDDepreciar / aTempoDepreciacao
    
    aPeDUnitCost = fDepreciacaoInvPeD / fShipments
    
    
    
    
    ##### LEARNING CURVE SECTOR #####
    fProduction = fShipments
    
    aLCExponent = log(aLCStrength)/log(2)
    
    aLearning = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent
    
    aInitialUnitFixedCost = (aInitialPrice/(1+aNormalProfitMargin))*aRatioOfFixedToVarCost*(1/(1+aRatioOfFixedToVarCost/aNormalCapacityUtilization))
    
    aInitialUnitVariableCost = (aInitialPrice/(1+aNormalProfitMargin))*(1/(1+aRatioOfFixedToVarCost/aNormalCapacityUtilization))
    
    aUnitFixedCost = aLearning * aInitialUnitFixedCost + aPeDUnitCost * aPeDLigado
    
    aUnitVariableCost = aLearning * aInitialUnitVariableCost
    
    checkUnitFixedCost = mean(aUnitFixedCost)
    
    checkUnitVariableCost = mean(aUnitVariableCost)
    
    ##### PRICE SECTOR #####
    
    aBasePrice = (1+aNormalProfitMargin)*(aUnitVariableCost+aUnitFixedCost/aNormalCapacityUtilization)
    
    aDemandSupplyBalance = aDesiredShipments/(aNormalCapacityUtilization*aCapacity)
    
    # Trava do Preço: Os players nunca precificarão acima de 2 vezes o preço inicial
    
    aTargetPrice = 
      pmin(aInitialPrice * 2,
           pmax(aUnitVariableCost,
                sPrice*
                  (1+aSensOfPriceToCosts*((aBasePrice/sPrice)-1))*
                  (1+aSensOfPriceToDSBalance*(aDemandSupplyBalance-1))*
                  (1+aSensOfPriceToShare*((aTargetMarketShare-aMarketShare))))
           )


    checkTargetPrice = mean(aTargetPrice)
    
    fChangeInPrice = (aTargetPrice - sPrice) / aPriceAdjustmentTime
    
    # if(time == FINISH){
    #   browser()  
    # }
    # 
    
    ##### NET INCOME SECTOR #####
    
    aDiscountFactor = exp(-aDiscountRate*(time - ano_futuro_inicial)) # 
    
    fValueOfNewOrders = fOrders * sPrice
    
    checkValueOfNewOrders1 = fValueOfNewOrders[1] #
    
    aAveragePriceOfOrderBook = sValueOfBacklog / sBacklog
    
    fRevenue = fShipments * aAveragePriceOfOrderBook #
    
    ##### P&D - Investimento #####
    
    fInvestimentoPeD = fRevenue * aOrcamentoPeD * aPeDLigado
    
    fInvestimentoPeDRealizado = sInvestimentoNaoRealizadoPeD / aTempoMedioRealizacaoPeD
    
    fPatentesSolicitadas = (fInvestimentoPeDRealizado) / aCustoMedioPatente
    
    fPatentesRejeitadas = (sPatentesRequisitadas/aTempoMedioAvaliacao) * aTaxaRejeicao
    
    fPatentesConcedidas = (sPatentesRequisitadas/aTempoMedioAvaliacao) * (1-aTaxaRejeicao) * (1-aPercPeDAberto)
    
    fPatentLeftsGeradas = (sPatentesRequisitadas/aTempoMedioAvaliacao) * (1-aTaxaRejeicao) * (aPercPeDAberto)
    
    fPatentLeftsVencidas = sPatentLefts / aTempoVencimentoPatentes
    
    # Estou somando as Patentes com investimenti (direto em domínio publico na equação abaixo, sem passar por outros estoques).
    # Eventualmente é possível modelar este comportamento passando por outros estoques.
    fPatentesVencidas = sPatentesEmpresa / aTempoVencimentoPatentes 
    
    #fPatentesAbertas = ((aPercPeDAberto * fInvestimentoPeDRealizado) / aCustoMedioPatente) * (1-aTaxaRejeicao)
    
    fPatentesUtilidadeExpirada = sPatentesEmDominioPublicoUteis / aTempodeInutilizacaoPatente
    
    ##### NET INCOME - PARTE 2 #####
    
    checkRevenue1 = fRevenue[1] #
    
    aVariableCost = fShipments * aUnitVariableCost #
    
    aFixedCost = aCapacity * (aUnitFixedCost - (aPeDUnitCost * aPeDLigado)) #
    
    fCost = aFixedCost + aVariableCost #
    
    fNetIncome = fRevenue - fCost - fInvestimentoPeD #
    
    fNPVProfitChange = fNetIncome * aDiscountFactor #
    
    # Considerar a Mudança do Lucro igual a zero enquanto a simulação estiver no período histórico.
    if(time < ano_futuro_inicial) {
      fNPVProfitChange = variaveis_periodo_historico$fNPVProfitChange
    }
    
    
    # if(modo=="completo" & time %in% c(2008, 2019)){
    #   browser()
    # }
    
    checkNPVProfitChange = mean(fNPVProfitChange) #
    
    checkNPVProfitChange1 = fNPVProfitChange[1]
    
    aNPVIndustryProfits = sum(sNPVProfit) #
    
    
    ##### ESTOQUES #####
    
    d_NPVProfit_dt = fNPVProfitChange
    
    d_ValueOfBacklog_dt = fValueOfNewOrders - fRevenue
    
    d_Backlog_dt = fOrders - fShipments
    
    d_InstalledBase_dt = fShipments - fDiscardRate
    
    d_Price_dt = fChangeInPrice
    
    d_CumulativeAdopters_dt = fAdoptionRate
    
    d_sReportedIndustryVolume_dt = fsmooth_ReportedIndustryVolume
    
    d_CumulativeProduction_dt = fProduction
    
    d_PerceivedCompTargetCapacity_dt = fChangePerceivedCompTargetCapacity
    
    d_SmoothCapacity1_dt = fchangeSmoothCapacity1
    
    d_SmoothCapacity2_dt = fchangeSmoothCapacity2
    
    d_SmoothCapacity3_dt = fchangeSmoothCapacity3
    
    #Estoques do Investimento em PeD
    
    d_InvestimentoNaoRealizadoPeD_dt = fInvestimentoPeD - fInvestimentoPeDRealizado
    
    d_PatentesRequisitadas_dt = fPatentesSolicitadas - fPatentesConcedidas - fPatentesRejeitadas - fPatentLeftsGeradas
    
    d_PatentesEmpresa_dt = fPatentesConcedidas - fPatentesVencidas
    
    d_PatentesEmDominioPublicoUteis_dt = sum(fPatentesVencidas) + fPatentLeftsVencidas - fPatentesUtilidadeExpirada
    
    d_InvestimentoPeDDepreciar_dt = fInvestimentoPeD - fDepreciacaoInvPeD
    
    d_PatentLefts_dt = sum(fPatentLeftsGeradas) - fPatentLeftsVencidas
    
    
    # Variaveis de Estoques Iniciais
    
    aInitialOrderRateCalibracao = if(aModoInitialCumulativeAdopters == 1){
      fIndustryOrderRate
    } else {
      aInitialIndustryShipments
    }
    
    # Alteração para Calibração de dados Iniciais: Calibrar o Backlog inicial com a Demanda Inicial Informada.
    # BacklogIni = aInitialSharePlayers * fIndustryOrderRate * aNormalDeliveryDelay
    BacklogIni = aInitialSharePlayers * aInitialOrderRateCalibracao * aNormalDeliveryDelay
    
    # Alteração Importante para a Calibração dos Dados Iniciais do Modelo!
    # A variável de Base de Usuários inicial deve partir da mesma estimativa
    
    # Antes da Alteração:
    # InstalledBaseIni = aInitialCumulativeAdopters * aInitialSharePlayers * aUnitsPerHousehold
    
    #Depois:
    # O Reorder Share deve depender do tempo de vida médio:
    # 
    InstalledBaseIni = aInitialCumulativeAdopters * aInitialSharePlayers
    
    CumulativeAdoptersIni = aInitialCumulativeAdopters
    
    # Alterando o Valor do Backlog para considerar a demanda inicial:
    ValueOfBacklogIni = aInitialSharePlayers * aInitialOrderRateCalibracao * aNormalDeliveryDelay * aInitialPrice
    
    ReportedIndustryVolumeIni = aIndustryVolume
    
    CumulativeProductionIni = aInitialProductionExperience
    
    PerceivedCompTargetCapacityIni = aCompetitorCapacity
    
    # Alterando a Capacidade para Considerar a Demanda Inicial
    CapacityIni = aInitialSharePlayers * aInitialOrderRateCalibracao / aNormalCapacityUtilization
    
    InitialInvestimentoNaoRealizadoPeD = aInitialInvestimentoNaoRealizadoPeD * aPatentShare
    
    InitialPatentesRequisitadas = aInitialPatentesRequisitadas * aPatentShare
    
    InitialPatentesEmpresa = aInitialPatentesEmpresa * aPatentShare
    
    InitialsPatentesEmDominioPublicoUteis =  aInitialsPatentesEmDominioPublicoUteis
    
    InitialsInvestimentoPeDDepreciar = aInitialsInvestimentoPeDDepreciar * aPatentShare
    
    IntialPatentLefts = aInitialPatentLefts
    
    
    ##### ESTOQUES - INICIAIS #####
    
    if(INICIALIZAR_ESTOQUES_COM_CASO_BASE){
      
      stocks_ini = list(
        BacklogIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sBacklog", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        InstalledBaseIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sInstalledBase", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        CumulativeAdoptersIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sCumulativeAdopters", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        ValueOfBacklogIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sValueOfBacklog", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        ReportedIndustryVolumeIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sReportedIndustryVolume", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        CumulativeProductionIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sCumulativeProduction", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        PerceivedCompTargetCapacityIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sPerceivedCompTargetCapacity", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        CapacityIni = VARIAVEIS_FINAIS_CASO_BASE[grep("sSmoothCapacity3", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        
        InitialInvestimentoNaoRealizadoPeD = VARIAVEIS_FINAIS_CASO_BASE[grep("sInvestimentoNaoRealizadoPeD", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        InitialPatentesRequisitadas = VARIAVEIS_FINAIS_CASO_BASE[grep("sPatentesRequisitadas", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        InitialPatentesEmpresa = VARIAVEIS_FINAIS_CASO_BASE[grep("sPatentesEmpresa", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        InitialsPatentesEmDominioPublicoUteis = VARIAVEIS_FINAIS_CASO_BASE[grep("sPatentesEmDominioPublicoUteis", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        InitialsInvestimentoPeDDepreciar = VARIAVEIS_FINAIS_CASO_BASE[grep("sInvestimentoPeDDepreciar", x = names(VARIAVEIS_FINAIS_CASO_BASE))],
        IntialPatentLefts = VARIAVEIS_FINAIS_CASO_BASE[grep("sIntialPatentLefts", x = names(VARIAVEIS_FINAIS_CASO_BASE))]
        )
      
      # Estes valores vieram como colunas e devem se transformar em vetores:
      stocks_ini = lapply(stocks_ini, transf_colunas_em_vetor)
      
      
    } else {
      stocks_ini = list(
        BacklogIni = BacklogIni,
        InstalledBaseIni = InstalledBaseIni,
        CumulativeAdoptersIni = CumulativeAdoptersIni,
        ValueOfBacklogIni = ValueOfBacklogIni,
        ReportedIndustryVolumeIni = ReportedIndustryVolumeIni,
        CumulativeProductionIni = CumulativeProductionIni,
        PerceivedCompTargetCapacityIni = PerceivedCompTargetCapacityIni,
        CapacityIni = CapacityIni,
        
        InitialInvestimentoNaoRealizadoPeD = InitialInvestimentoNaoRealizadoPeD,
        InitialPatentesRequisitadas = InitialPatentesRequisitadas,
        InitialPatentesEmpresa = InitialPatentesEmpresa,
        InitialsPatentesEmDominioPublicoUteis = InitialsPatentesEmDominioPublicoUteis,
        InitialsInvestimentoPeDDepreciar = InitialsInvestimentoPeDDepreciar,
        IntialPatentLefts = IntialPatentLefts
      )  
    }
    
    if(time == START){
      if(any(unlist(stocks_ini)<0)){
        browser()  
      }
    }
    
    ##### COMPARAR RESULTADOS COM O ITHINK #####
    
    if(VERIFICAR_STOCKS & modo == "completo"){
      for (variavel in variaveis_ithink_stocks) {
        # Definir o tipo de variavel
        # Variavel é um estoque?
        variavel_ithink_alterada = gsub(pattern = "\\[", replacement = "", x = variavel, ignore.case = TRUE)
        variavel_ithink_alterada = gsub(pattern = "\\]", replacement = "", x = variavel_ithink_alterada, ignore.case = TRUE)
        
        # Verificar apenas Estoques:
        variavel_ithink_alterada = paste("s", variavel_ithink_alterada, sep = "")
        
        # Valor da Variavel Calculada
        valor_variavel_R = eval(parse(text = variavel_ithink_alterada))
        
        valor_variavel_ithink = dados_ithink_stocks[[linha,variavel]]
        
        diferenca = valor_variavel_R - valor_variavel_ithink
        
        if (abs(x = diferenca) > CHECK_PRECISION){
          message(paste("Estoque Diff:", time, linha, variavel, diferenca, sep = " - "))
          if(BROWSE_ON_DIFF){
            browser()  
          }
        }
      }  
    }
    
    
    if(VERIFICAR_CHECKS & modo == "completo"){
      for (variavel in variaveis_ithink_checks) {
        # Definir o tipo de variavel
        # Variavel é um estoque?
        variavel_ithink_alterada = gsub(pattern = "\\[", replacement = "", x = variavel, ignore.case = TRUE)
        variavel_ithink_alterada = gsub(pattern = "\\]", replacement = "", x = variavel_ithink_alterada, ignore.case = TRUE)
        
        # Verificar apenas Estoques:
        #variavel_ithink_alterada = paste("s", variavel_ithink_alterada, sep = "")
        
        # Valor da Variavel Calculada
        valor_variavel_R = eval(parse(text = variavel_ithink_alterada))
        
        valor_variavel_ithink = dados_ithink_checks[[linha,variavel]]
        
        diferenca = valor_variavel_R - valor_variavel_ithink
        
        if(!is.na(diferenca)){
          if (abs(x = diferenca) > CHECK_PRECISION){
            message(paste("Check Diff:", time, linha, variavel, diferenca, sep = " - "))
            if(BROWSE_ON_DIFF){
              browser()  
            }
          }  
        }
        
      }
    }
    
    # Colocar isso dentro do IF abaixo e verificar!
    if(VERIFICAR_GLOBAL & modo == "completo"){
      
      # Forma que usa todas as variaveis do ambiente:
      # variaveis_disponiveis_ambiente = ls()
      # variaveis_auxiliares = variaveis_disponiveis_ambiente[grep("^[aA].*", variaveis_disponiveis_ambiente)]
      # 
      # Forma que usa as variaveis globais definidas em um vetor
      variaveis_auxiliares = variaveis_globais_a_verificar
      
      
      seletor_players = paste("[",1:N_PLAYERS,"]", sep = "")
      
      variaveis_a_verificar = expand.grid(variaveis_auxiliares, seletor_players)
      
      variaveis_a_verificar = paste(variaveis_a_verificar[,1], variaveis_a_verificar[,2], sep = "")
      
      variaveis_a_verificar_no_ithink = substring(variaveis_a_verificar, 2)
      
      
      verificar_variaveis_globais = function(n_variavel){
        valor_variavel_R = eval(parse(text = variaveis_a_verificar[n_variavel]))
        
        valor_variavel_ithink = dados_ithink_global[[linha,variaveis_a_verificar_no_ithink[n_variavel]]]
        
        if((length(valor_variavel_ithink) > 0)) {
          decisao = !is.na(valor_variavel_ithink) & is.numeric(valor_variavel_ithink)
          if(decisao == FALSE) {
            valor_variavel_ithink = NA
          }
        } else {valor_variavel_ithink = NA}
        
        
        if((length(valor_variavel_R) > 0)) {
          decisao = !is.na(valor_variavel_R) & is.numeric(valor_variavel_R)
          if(decisao == FALSE) {
            valor_variavel_R = NA
          }
        } else {valor_variavel_R = NA}
        
        
        diferenca = unname(valor_variavel_R)  - unname(valor_variavel_ithink)
        
        diferenca
      }
      
      diferencas = lapply(X = 1:length(variaveis_a_verificar), FUN = verificar_variaveis_globais)
      
      diferencas = do.call(rbind, diferencas)
      
      matriz_diferencas = data.frame(
        variaveis_a_verificar,
        diferencas
      )
      
      diferencas_a_reportar = subset(matriz_diferencas, abs(diferencas) > CHECK_PRECISION)
      
      if(nrow(diferencas_a_reportar)>1) {
        message(paste("Check Diferenças Globais:", time, linha, sep = " - "))
        if(BROWSE_ON_DIFF){
          browser()  
        }
      }
      
    }
    
    # if(modo == "completo" & time == FINISH) {
    #   browser()
    # }
    ##### VARIÁVEIS RETORNADAS #####
    
    resultado_completo = list(c(
      d_NPVProfit_dt
      ,d_ValueOfBacklog_dt
      ,d_Backlog_dt
      ,d_InstalledBase_dt
      ,d_Price_dt
      ,d_CumulativeAdopters_dt
      ,d_sReportedIndustryVolume_dt
      ,d_CumulativeProduction_dt
      ,d_PerceivedCompTargetCapacity_dt
      ,d_SmoothCapacity1_dt
      ,d_SmoothCapacity2_dt
      ,d_SmoothCapacity3_dt
      ,d_InvestimentoNaoRealizadoPeD_dt
      ,d_PatentesRequisitadas_dt
      ,d_PatentesEmpresa_dt
      ,d_PatentesEmDominioPublicoUteis_dt
      ,d_InvestimentoPeDDepreciar_dt
      ,d_PatentLefts_dt
    )
    ,fIndustryOrderRate = unname(fIndustryOrderRate) 
    ,aOrderShare = unname(aOrderShare)
    ,aPerformance = unname(aPerformance)
    ,aPatentesEmpresaTemAcesso = unname(aPatentesEmpresaTemAcesso)
    ,aNonAdopters = unname(aNonAdopters)
    ,fReorderRate = unname(fReorderRate) 
    ,aIndustryShipments = unname(aIndustryShipments)
    ,aIndustryVolume = unname(aIndustryVolume) 
    ,fNPVProfitChange = unname(fNPVProfitChange) 
    ,fNetIncome = unname(fNetIncome) 
    ,aNPVIndustryProfits = unname(aNPVIndustryProfits)
    ,VariacaoDemanda = unname(VariacaoDemanda)
    ,fInvestimentoPeD = unname(fInvestimentoPeD)
    ,aAttractivenessFromPerformance = unname(aAttractivenessFromPerformance)
    ,aAttractivenessFromAvailability = unname(aAttractivenessFromAvailability)
    ,aAttractivenessFromPrice = unname(aAttractivenessFromPrice)
    )
    
    
    return (if(modo == "inicial"){
      stocks_ini
    } else {
      resultado_completo
    })   
  })
}


#### OBJETO SDMODEL #####

# Nomeando o Dataframe de Saída (este vetor não é mais utilizado)
nomes_variaveis = c("Tempo", "d_NPVProfit_dt", "aDiscountFactor", "aDiscountRate", "fNPVProfitChange", "fNetIncome", "aNPVIndustryProfits")

# Inicializando um list com Tudo o que é necessário para a Simulação.
# Este objeto contém informações necessárias sobre o modelo para a sua simulação.
sdmodel = list(
  Start = START,
  Finish = FINISH,
  Step = STEP,
  SimTime = SIM_TIME,
  # Auxs = auxs,
  # Stocks = stocks,
  Modelo = modelo,
  Variaveis = nomes_variaveis
)

##### SIMULAR RDM E ESCOLHER ESTRATEGIA #####

#' simularRDM_e_escolher_estrategia
#'
#' @param inputs caminho para o arquivo de inputs
#' @param sdmodel list com modelo e suas opções
#' @param opcoes list com opções para a simulação e analise de Regret.
#'
#' @return list com resultados da simulacao e uma estratégia candidata.
simularRDM_e_escolher_estrategia = function(inputs = "params.xlsx", sdmodel = sdmodel, opcoes = opcoes, ensemble) {
  
  output_simulacao = simular_RDM(arquivo_de_inputs=inputs ,sdmodel = sdmodel, n = opcoes$N, opcoes = opcoes, ensemble = ensemble)
  
  ## Simular
  dados_simulacao = output_simulacao$DadosSimulacao
  
  # Selecionando dados do último ano:
  dados = selecionar_ultimo_periodo(dados_simulacao = dados_simulacao, var_tempo = opcoes$VarTempo)
  
  # Analisar Regret
  analise_regret = calcular_e_resumir_regret(dados = dados, var_resposta = opcoes$VarResposta, var_cenarios = opcoes$VarCenarios, var_estrategias = opcoes$VarEstrategias)
  
  # Escolher a Estratégia Candidata, com base no critério de robustez dos percentis
  estrategia_candidata = escolher_estrategia_candidata(dados = analise_regret$Dados, resumo_estrategias = analise_regret$ResumoEstrategias, var_resposta = opcoes$VarResposta, var_criterio = opcoes$VarCriterio, sentido = opcoes$SentidoCriterio)
  
  message(paste("A Estrategia candidata é a ", estrategia_candidata$Lever))
  
  output = list(
    DadosSimulados = dados_simulacao,
    DadosUltimoPeriodo = dados,
    AnaliseRegret = analise_regret,
    Inputs = output_simulacao$Inputs,
    Ensemble = output_simulacao$Ensemble,
    EstrategiaCandidata =  estrategia_candidata[opcoes$VarEstrategias],
    Opcoes = opcoes,
    SdModel = sdmodel
  )
  
  output
  
}


filtrar_casos_plausiveis = function(dados_simulacao, ensemble, ranges, opcoes = opcoes){
  
  n_cenarios_simulados = length(unique(dados_simulacao[,opcoes$VarCenarios]))
  
  cenarios_fora_do_range = sapply(1:nrow(ranges), definir_cenarios_fora_range, ranges = ranges, dados_simulacao = dados_simulacao, opcoes = opcoes)
  
  cenarios_fora_do_range = unique(unlist(cenarios_fora_do_range))
  
  if(length(cenarios_fora_do_range > 0)){
    message(paste0("Filtrando ",length(cenarios_fora_do_range), " ( ", round(100 * length(cenarios_fora_do_range)/n_cenarios_simulados,2)," %) dos ", n_cenarios_simulados ," cenarios simulados."))
  }
  
  dados_simulacao_filtrados = subset(dados_simulacao, !(Scenario %in% cenarios_fora_do_range)) 
  ensemble_filtrado = subset(dados_simulacao, !(Scenario %in% cenarios_fora_do_range)) 
  
  list(
    dados_simulacao_filtrados = dados_simulacao_filtrados,
    ensemble_filtrado = ensemble_filtrado,
    cenarios_fora_do_range = cenarios_fora_do_range
  )
}


#' definir_cenarios_fora_range
#' #' Esta função retorna o número dos cenários fora dos ranges definidos. Isto pode ser usado para filtrar cenários plausíveis ou ainda definir se um caso está ou não em um cenário (obtido com o PRIM, por exemplo).
#' 
#' @param n_var índice da variavel no data.frame de ranges
#' @param ranges data.framde de ranges (com as colunas Variavel, Min, e Max)
#' @param dados_simulacao data.frame com colunas correspondendo às variáveis do range.
#' @param opcoes objeto de opções.
#'
#' @return
#' @export
#'
#' @examples
definir_cenarios_fora_range = function(n_var, ranges = ranges, dados_simulacao = dados_simulacao, opcoes = opcoes) {
  
  variavel = ranges$Variavel[n_var]
  minimo = ranges$Min[n_var]
  maximo = ranges$Max[n_var]
  
  # cenarios onde a variavel não está no range:
  cenarios_viola_maximo = unique(dados_simulacao[which(dados_simulacao[,variavel] > maximo),opcoes$VarCenarios])
  cenarios_viola_minimo = unique(dados_simulacao[which(dados_simulacao[,variavel] < minimo),opcoes$VarCenarios])
  
  cenarios_fora_range = c(cenarios_viola_maximo, cenarios_viola_minimo)
  
  # informar o que aconteceu:
  if(length(cenarios_viola_maximo)>0){
    message(paste("Variavel", variavel, "acima do máximo(",maximo,").",  length(cenarios_viola_maximo), "cenarios descartados."))
  }
  
  # informar o que aconteceu:
  if(length(cenarios_viola_minimo)>0){
    message(paste("Variavel", variavel, "abaixo do minimo(",minimo,").", length(cenarios_viola_minimo), "cenarios descartados."))
  }
  
  cenarios_fora_range
}


##### CARREGAR INPUTS #####

#' carregar_inputs
#'
#' @param arquivo_de_inputs caminho para o arquivo de inputs com estratégias e incertezas
#' @param abas_a_ler abas a ler do arquivo de inputs
#' @param nomes_inputs Nome a ser atribuido aos dataframes de input.
#'
#' @return list com inputs para a simulação.
carregar_inputs = function (arquivo_de_inputs="params.xlsx", abas_a_ler = c("params", "levers", "Levers_FullDesign", "RangesPlausiveis"), nomes_inputs = c("Parametros", "Levers", "LeversFull", "RangesPlausiveis"), opcoes = opcoes) {
  
  # Criando uma list para os inputs
  message(
    paste("01. funcoes.R/carregar_inputs: Iniciando Carregamento de Inputs (funcao carregar_inputs()",
          "arquivo_de_inputs = ", arquivo_de_inputs)
  )
  inputs = vector(mode = "list", length = length(nomes_inputs))
  names(inputs) = nomes_inputs
  
  # Preenchendo os Dados dos Inputs
  for (aba in abas_a_ler) {
    n_aba = which(aba == abas_a_ler)
    inputs[[n_aba]] = readxl::read_excel(arquivo_de_inputs,sheet = aba)
  }
  
  
  # Substituir Levers aqui mesmo:
  # Substituindo Levers por Proketo Fatorial Completo, se isto foi selecionado:
  # Gerar um Fatorial Completo das Variáveis, se for necessário
  if(opcoes$FullFactorialDesign){
    var_levers = na.omit(expand.grid(inputs$LeversFull))
    n_levers = nrow(var_levers)
    inputs$Levers = data.frame(Lever = 1:n_levers,
                               LeverCode = as.character(1:n_levers),
                               CasoBase = c(1, rep(0, n_levers-1)),
                               var_levers)
  }
  
  message("01. funcoes.R/carregar_inputs: Finalizando Carregamento de Inputs.")
  return(inputs)
  
  
  
}


##### OBTER ENSEMBLE - PARÂMETROS #####

#' obter_lhs_ensemble
#'
#' @param params dataframe de parâmetros a usar (no padrão pré-determinado)
#' @param n tamanho do ensemble a montar
#'
#' @return dataframe com ensemble montado (pronto para a simulação)
obter_lhs_ensemble = function (params, n=100, opcoes = opcoes) {
  message("01. funcoes.R/obter_lhs_ensemble: Iniciando Obtenção do Ensemble.")
  #Obtendo DataFrame de Parâmetros
  
  nvar = length(params$Variavel)
  pontos = n
  
  # Obtendo um Hypercubo com as Variáveis que eu quero
  randomLHS <- lhs::randomLHS(pontos, nvar)
  
  p = as.data.frame(randomLHS)
  min = as.vector(params$Min)
  max = as.vector(params$Max)
  variaveis = as.vector(params$Variavel)
  
  # Transformando o Hypercubo em variáveis
  # var <- matrix(nrow=pontos, ncol=variaveis)
  ensemble = matrix(nrow = pontos, ncol = nvar+1)
  
  # Montando o Ensemble
  for (var in variaveis) {
    i = which(x = variaveis == var)
    
    # Aqui o i é +1 porque a primeira coluna será o cenário.
    ensemble[,i+1] = qunif(p = randomLHS[,i], min = min[i], max = max[i])
  }
  
  # Adicionando A variável "Scenario"
  variaveis = c(c(opcoes$VarCenarios),variaveis)
  
  colnames(ensemble) = variaveis
  
  ensemble[,opcoes$VarCenarios] = 1:nrow(ensemble)
  
  ensemble
}

##### AMPLIAR ENSEMBLE COM ESTRATÉGIAS #####

#' ampliar_ensemble_com_levers
#'
#' @param ensemble conjunto de cenarios a simular
#' @param levers conjunto de estratégias a simular
#'
#' @return dataframe com a combinação de todas as estratégias em todos os cenários.
ampliar_ensemble_com_levers = function(ensemble, levers, levers_full, opcoes) {
  
  variaveis_adicionais = names(dplyr::select(levers, -LeverCode, -CasoBase))
  
    # Filtrar cenários a simular caso seja necessário apenas simular o Caso Base:
  if(opcoes$SimularApenasCasoBase){
    levers = subset(levers, as.logical(CasoBase))
  }
  
  linhas_ensemble_incial = nrow(ensemble)
  novo_ensemble = matrix(0, nrow = nrow(ensemble)*length(levers$Lever), ncol = ncol(ensemble) + length(variaveis_adicionais))
  
  names_old_ensemble = colnames(ensemble)
  names_novo_ensemble = c(names_old_ensemble, variaveis_adicionais)
  
  colnames(novo_ensemble) = names_novo_ensemble
  
  j = 1
  for (l in seq_along(levers$Lever)) {
    lini = j
    lfim = j + linhas_ensemble_incial-1
    matriz_var_adicionais = as.matrix(levers[l,variaveis_adicionais])
    novo_ensemble[lini:lfim,names_old_ensemble] = ensemble
    novo_ensemble[lini:lfim,variaveis_adicionais] = matrix(matriz_var_adicionais, nrow = linhas_ensemble_incial, ncol = ncol(matriz_var_adicionais), byrow = TRUE)
    j = j + linhas_ensemble_incial
  }
  
  novo_ensemble
  
}


##### SIMULAR #####

#' simular
#'
#' @param stocks integrais a serem resolvidas numéricamente. (numeric) 
#' @param simtime Tempo de simulação (numeric)
#' @param modelo Modelo de dinâmica de sistemas no padrão do deSolve (function)
#' @param ensemble ensemble montado (pronto para a simulação)
#' @param nomes_variaveis_final vetor com nomes de variáveis
#' @param paralelo TRUE ou false (roda no modo paralelo ou Falso)
#' @param modo_paralelo "FORK" ou "PSOCK". Usar "FORK" no Linux e Mac, e "PSOCK" no Windows.
#'
#' @return
#' @export
#'
#' @examples
simular = function(simtime, modelo, ensemble, nomes_variaveis_final, opcoes = opcoes) {
  message("01. funcoes.R/simular: Iniciando Simulação.")
  
  paralelo = opcoes$Paralelo
  
  modo_paralelo = opcoes$ModoParalelo
  
  # Rodando a Simulação (uma vez), com a primeira linha do ensemble - Ajuda a saber se funciona.
  # Esta função apenas funciona com o estoque inicial fixo, será necessário implementar de outra forma depois.
  t_inicio_teste = Sys.time()
  o = as.data.frame(solve_modelo_dissertacao(parametros = ensemble[1,], modelo = modelo, simtime = simtime)) 
  
  t_fim_teste = Sys.time()
  t_uma_rodada = as.numeric(difftime(time1 = t_fim_teste, time2 = t_inicio_teste, units = "secs"))
  
  # Esta funcao Executa a Resolução do Modelo para uma dada linha do Ensemble.
  # O Ideal para o modo paralelo seria executar "n" linhas do ensemble por vez.
  
  solve_modelo = function(n_linha_ensemble) {
    params = ensemble[n_linha_ensemble,]
    res = solve_modelo_dissertacao(parametros = params, modelo = modelo, simtime = simtime)
    # Gerar Matriz de Resultados
    cbind(res,
          Lever = ensemble[n_linha_ensemble,opcoes$VarEstrategias],
          Scenario = ensemble[n_linha_ensemble,opcoes$VarCenarios])
  }
  

  
  # Forma não elegante de resolver o problema:
  solve_modelo_batch = function(n_linhas_por_vez, linha_inicial) {
    
    first_result = solve_modelo(linha_inicial)
    
    for (i in (linha_inicial+1):(n_linhas_por_vez+linha_inicial-1)) {
      results_solve_modelo = solve_modelo(i)
      
      if (i == (linha_inicial + 1)) {
        results = rbind(first_result, results_solve_modelo)
      } else {
        results = rbind(results, results_solve_modelo)
      }
      
    }
    
    results
    
  }
  
  
  # Teste para o Azure - Passar tudo por parâmetro
  solve_modelo_batch_azure = function(n_linhas_por_vez, linha_inicial, ensemble, modelo, simtime, START, FINISH, VERIFICAR_STOCKS, VERIFICAR_CHECKS, VAR_LEVER, VAR_SCENARIO, INICIALIZAR_ESTOQUES_COM_CASO_BASE, SIMULAR_HISTORICO_DIFERENTE, ANO_INICIO_AVALIACAO, VERIFICAR_GLOBAL, opcoes, STEP, solve_modelo_dissertacao) {
    
    ensemble = ensemble
    modelo = modelo
    simtime = simtime
    START = START
    FINISH = FINISH
    VERIFICAR_STOCKS = VERIFICAR_STOCKS
    VERIFICAR_CHECKS = VERIFICAR_CHECKS
    VAR_LEVER = VAR_LEVER
    VAR_SCENARIO = VAR_SCENARIO
    INICIALIZAR_ESTOQUES_COM_CASO_BASE = INICIALIZAR_ESTOQUES_COM_CASO_BASE
    SIMULAR_HISTORICO_DIFERENTE = SIMULAR_HISTORICO_DIFERENTE
    ANO_INICIO_AVALIACAO = ANO_INICIO_AVALIACAO
    VERIFICAR_GLOBAL = VERIFICAR_GLOBAL
    opcoes = opcoes
    STEP = STEP
    solve_modelo_dissertacao = solve_modelo_dissertacao
    
    first_result = solve_modelo(linha_inicial)
    
    for (i in (linha_inicial+1):(n_linhas_por_vez+linha_inicial-1)) {
      results_solve_modelo = solve_modelo(i)
      
      if (i == (linha_inicial + 1)) {
        results = rbind(first_result, results_solve_modelo)
      } else {
        results = rbind(results, results_solve_modelo)
      }
      
    }
    
  }
  
  if(paralelo == TRUE) {
   
    t_inicio = Sys.time()
    message(t_inicio)
    
    if((modo_paralelo == "PSOCK") | (modo_paralelo == "FORK")){
      # Calculate the number of cores
      no_cores <- detectCores() - 1
      # Inicializar Cluster
      cl <- makeCluster(no_cores, type = modo_paralelo)
      
      # Carregando bibliotecas no cluster:
      #clusterEvalQ(cl, source("funcoes.R"))
      #clusterEvalQ(cl, eval(parse('funcoes.R')))
      # textConnection(animal.R)
      
      if(modo_paralelo  == "PSOCK") {
        # Se o modo paralelo é "PSOCK", é necessário definir explícitamente as variáveis que devem ir para o cluster
        # http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/
        clusterEvalQ(cl, library(deSolve))
        
        # Exportando objetos que preciso ter nos clusters:
        clusterExport(cl, varlist = list("ensemble", 
                                         "modelo", 
                                         "simtime",
                                         "START",
                                         "FINISH", 
                                         "VERIFICAR_STOCKS", 
                                         "solve_modelo", 
                                         "VERIFICAR_CHECKS",
                                         "VAR_LEVER",
                                         "VAR_SCENARIO",
                                         "INICIALIZAR_ESTOQUES_COM_CASO_BASE",
                                         "SIMULAR_HISTORICO_DIFERENTE",
                                         "ANO_INICIO_AVALIACAO",
                                         "VERIFICAR_GLOBAL",
                                         "opcoes",
                                         "STEP",
                                         "solve_modelo_dissertacao"), envir = environment())  
      }
      
      
      message(paste("Iniciando Simulacao em modo paralelo. Usando", no_cores, "núcleos."))
      
      # t_uma_rodada = 0.583717
      
      t_estimado = t_uma_rodada * nrow(ensemble)/no_cores
      message(paste("Tempo estimado (segundos):"),t_estimado)
      
      # Aplicando Função paralela para a computação dos resultados
      dados_simulacao <- parLapply(cl, 1:nrow(ensemble), solve_modelo)
      stopCluster(cl)
      
      # Unindo Dados da Simulação, seja entregues pelo Azure seja pelo meu
      dados_simulacao = do.call(rbind, dados_simulacao)
      
    }
    
    if(modo_paralelo == "Azure") {
      
      browser()
      
      # 3. Set your credentials - you need to give the R session your credentials to interact with Azure
      setCredentials("credentials.json")
      
      # 4. Register the pool. This will create a new pool if your pool hasn't already been provisioned
      browser()
      
      cluster <- doAzureParallel::makeCluster("cluster.json")
      
      browser()
      
      # 5. Register the pool as your parallel backend
      registerDoAzureParallel(cluster)
      
      browser()
      
      # 6. Check that your parallel backend has been registered
      # getDoParWorkers()
      
      # GErando resultado
      # browser()
      
      # O Tamanho do Ensemble precisa ser multiplo do número de tarefas
      numero_de_tarefas = 10
      
      linhas_por_vez = nrow(ensemble) / numero_de_tarefas
      
      vetor_tarefas = seq(from = 1, by = linhas_por_vez, length.out = numero_de_tarefas)
      
      browser()
      
      # Primeira tentativa
      # dados_simulacao <- foreach(i = vetor_tarefas, .combine='rbind') %dopar% {
      #   solve_modelo_batch(n_linhas_por_vez = linhas_por_vez, linha_inicial = i)
      # }
      
      # Segunda Tentativa - Também Não funcionou. Parece que tudo precisa estar encapsulado na função.
      # dados_simulacao <- foreach(i = 1:nrow(ensemble), .combine='rbind') %dopar% {
      #    as.data.frame(solve_modelo(i))
      # }
      
      # Terceira tentativa -Tentando passar os parâmetros por "força bruta"
      dados_simulacao <- foreach(i = vetor_tarefas, .combine='rbind') %dopar% {
        solve_modelo_batch_azure(n_linhas_por_vez = linhas_por_vez, linha_inicial = i, ensemble, modelo, simtime, START, FINISH, VERIFICAR_STOCKS, VERIFICAR_CHECKS, VAR_LEVER, VAR_SCENARIO, INICIALIZAR_ESTOQUES_COM_CASO_BASE, SIMULAR_HISTORICO_DIFERENTE, ANO_INICIO_AVALIACAO, VERIFICAR_GLOBAL, opcoes, STEP, solve_modelo_dissertacao)
      }
      
      
      browser()
      
    }
    
    
    
    t_fim = Sys.time()
    
    # perf = t_estimado / as.numeric(difftime(time1 = t_fim, time2 = t_inicio, units = "secs")) / 0.75
    # message(t_fim)
    message("Finalizando Simulacao. Finalizando Cluster")
    # message(paste("Indice de Performance",perf))
    #resultados_paralelo = lapply(1:nrow(ensemble), solve_modelo)
    dados_simulacao = as.data.frame(dados_simulacao)
    
  }
  
  if(paralelo == FALSE){
    # J é o índice dos dados simulados
    j = 1
    # Rodando a Simulacao Em todo o Ensemble
    # Fazer os calculos da maneira anterior aqui.
    nomes_temporario = names(o)
    
    # o<-data.frame(ode(y=stocks, times=simtime, func = modelo, 
    #                   parms=ensemble[1,], method="euler"))
    pontos = nrow(ensemble)
    
    nlinhas = nrow(o)
    
    ncolunas = ncol(o)+2
    
    # Montando uma matriz com todos os dados para a simulação
    dados_simulacao = matrix(nrow = pontos*nlinhas, ncol = ncolunas)
    
    for (i in 1:nrow(ensemble)) {
      resultados_simulacao = as.data.frame(solve_modelo_dissertacao(parametros = ensemble[i,], modelo = modelo, simtime = simtime)) 
      
      resultados_simulacao = as.matrix(resultados_simulacao)
      
      linhas = nrow(resultados_simulacao)
      
      # Avançando a linha inicial e Final da Simulação
      l_inicial = j
      l_final = j + linhas-1
      
      # Adicionando o resultado ao ensemble
      dados_simulacao[l_inicial:l_final,1:(ncolunas-2)] = resultados_simulacao
      
      # Adicionando o Número do Lever
      dados_simulacao[l_inicial:l_final,(ncolunas-1)] = ensemble[i,opcoes$VarEstrategias]
      
      # Adicionando o Número do Cenário
      dados_simulacao[l_inicial:l_final,ncolunas] = ensemble[i,opcoes$VarCenarios]
      
      # Exibindo uma Mensagem de Status
      if (i %% 5 == 0) {
        message(paste(i, "simulações finalizadas."))
      }
      # Avançando o índice dos dados simulados
      j = j + linhas
    }
    # Usando nomes temporario
    colnames(dados_simulacao) = c(nomes_temporario, opcoes$VarEstrategias, opcoes$VarCenarios)
    # colnames(dados_simulacao) = nomes_variaveis_final
    
    dados_simulacao = as.data.frame(dados_simulacao)
    names(dados_simulacao) = c(nomes_temporario, opcoes$VarEstrategias, opcoes$VarCenarios)
    #names(dados_simulacao) = nomes_variaveis_final
  }
  
  message("01. funcoes.R/simular: Finalizando Simulacao.")
  
  dados_simulacao
}


##### SIMULAR RDM #####

#' simular_RDM
#'
#' @param arquivo_de_inputs Caminho para o arquivo de dados padronizado com Estrategias e Incertezas (character)
#' @param sdmodel Lista com variáveis para simulação de dinamica de sistemas
#' @param n Número de cenarios a gerar (numeric)
#'
#' @return data.frame com resultados da simulação
simular_RDM = function(arquivo_de_inputs="params.xlsx", sdmodel, n = opcoes$N, opcoes = opcoes, ensemble){
  t_inicio = Sys.time()
  message("Bem vindo ao SIMULADOR RDM! Pedro Lima.")
  message(paste("Iniciando Simulacao RDM: ", t_inicio))
  
  # Carregando Inputs
  inputs = carregar_inputs(arquivo_de_inputs = arquivo_de_inputs, opcoes = opcoes)
  
  # Substituindo Levers por Proketo Fatorial Completo, se isto foi selecionado:
  # Gerar um Fatorial Completo das Variáveis, se for necessário
  # if(opcoes$FullFactorialDesign){
  #   var_levers = na.omit(expand.grid(inputs$LeversFull))
  #   n_levers = nrow(var_levers)
  #   inputs$Levers = data.frame(Lever = 1:n_levers,
  #                       LeverCode = as.character(1:n_levers),
  #                       CasoBase = c(1, rep(0, n_levers-1)),
  #                       var_levers)
  # }
  # 
  
  # Obter Ensemble LHS (Sem Variáveis das Estratégias)
  
  # Se um ensemble não foi informado, gerar um ensemble.
  if(missing(ensemble)){
    # Usar o primeiro lever que eu achar
    ensemble = obter_lhs_ensemble(params = inputs$Parametros, n = n, opcoes = opcoes)
  }
  
  # Ampliar Ensemble com as variáveis das Estratégias
  novo_ensemble = ampliar_ensemble_com_levers(ensemble = ensemble, levers = inputs$Levers,levers_full = inputs$LeversFull, opcoes = opcoes)
  
  # Rodando a Simulação
  nestrategias = length(inputs$Levers$Lever)
  nfuturos = nrow(ensemble)
  ntempo = ((sdmodel$Finish - sdmodel$Start)/sdmodel$Step)
  
  message(paste("Esta rotina realizará", nestrategias * nfuturos, "Simulacoes.\n (", nestrategias, "estratégias x", nfuturos, "futuros, em", ntempo , "periodos de tempo."))
  
  # TODO: Esta Chamada vai precisar mudar para considerar a nova funcao
  dados_simulacao = simular(simtime = sdmodel$SimTime, modelo = sdmodel$Modelo, ensemble = novo_ensemble, nomes_variaveis_final = sdmodel$Variaveis, opcoes = opcoes)
  
  t_fim = Sys.time()
  
  message("Finalizando Simulacao. Tempo de Simulacao: ", t_fim - t_inicio)
  
  
  # Filtrando Cenarios que sao plausiveis
  if(opcoes$FiltrarCasosPlausiveis) {
    message("Filtrando Casos Plausiveis.")
    resultados_filtrados = filtrar_casos_plausiveis(dados_simulacao, ensemble = novo_ensemble, ranges = inputs$RangesPlausiveis, opcoes = opcoes)  
    dados_simulacao = resultados_filtrados$dados_simulacao_filtrados
    novo_ensemble = resultados_filtrados$ensemble_filtrado
    message(paste("Índices dos Cenarios Filtrados:", resultados_filtrados$cenarios_fora_do_range, collapse = ", "))
  }
  
  
  output = list(
    Inputs = inputs,
    Ensemble = ensemble,
    NovoEnsemble = novo_ensemble,
    DadosSimulacao = dados_simulacao
  )
  
  output
  
}

##### CALCULO DO REGRET (PERDA DE OPORTUNIDADE) #####

#' calcular_regret
#'
#' @param dados dataframe de dados simulados para o calculo do Regret.
#' @param var_resposta variável de resposta a utilizar no calculo de regret (quanto mais, melhor)
#' @param var_group variável a agrupar (ex.: Cenários)
#'
#' @return mesmo dataframe de entrada com variáveis a mais.
calcular_regret = function(dados, var_resposta, var_group, sentido = "max") {
  var_maximo = paste("MaximoPor", var_group, sep = "")
  var_minimo = paste("MinimoPor", var_group, sep = "")
  var_regret = paste(var_resposta, "Regret", sep = "")
  var_regret_perc = paste(var_regret, "Perc", sep = "")
  
  dados[var_maximo] = calcular_maximo_por_variavel(var_resposta = var_resposta, var_group = var_group, dados = dados)
  
  dados[var_minimo] = calcular_minimo_por_variavel(var_resposta = var_resposta, var_group = var_group, dados = dados)
  
  if (sentido == "max") {
    dados[var_regret] = dados[var_maximo] - dados[var_resposta]  
  } else {
    dados[var_regret] = dados[var_resposta] - dados[var_minimo]
  }
  
  dados[var_regret_perc] = dados[var_regret] / (dados[var_maximo] - dados[var_minimo])
  
  dados  
}


##### RESUMIR VARIÁVEL DE RESPOSTA PARA A ANÁLISE DO REGRET #####
#' resumir_variavel_resposta
#'
#' @param dados dataframe com dados para analise do regret.
#' @param var_resposta variável de resposta para análise do RDM.
#' @param var_group 
#'
#' @return dataframe com resumo das variaveis por grupo definido.
resumir_variavel_resposta = function(dados = dados_ano_final, var_resposta = "Cash", var_group = "Lever") {
  var_regret = paste(var_resposta, "Regret", sep = "")
  var_regret_perc = paste(var_regret, "Perc", sep = "")
  
  call = substitute(
    expr =
      dplyr::group_by(dados, VarGroup) 
    %>% dplyr::select(VarGroup, VarResposta, VarRegret, VarRegretPerc)
    %>% dplyr::summarise(
                  VarMedio = mean(VarResposta, na.rm = TRUE),
                  VarDev = sd(VarResposta, na.rm = TRUE),
                  VarMediaSobDesvio = mean(VarResposta, na.rm = TRUE) / sd(VarResposta, na.rm = TRUE),
                  Percentil25Var = quantile(VarResposta, probs = c(0.25), na.rm = TRUE),
                  Percentil75Var = quantile(VarResposta, probs = c(0.75), na.rm = TRUE),
                  RegretMedio = mean(VarRegret, na.rm = TRUE),
                  DesvioRegret = sd(VarRegret, na.rm = TRUE),
                  Percentil25Regret = quantile(VarRegret, probs = c(0.25), na.rm = TRUE),
                  Percentil75Regret = quantile(VarRegret, probs = c(0.75), na.rm = TRUE),
                  RegretMedioPerc = mean(VarRegretPerc, na.rm = TRUE),
                  DesvioRegretPerc = sd(VarRegretPerc, na.rm = TRUE),
                  Percentil25RegretPerc = quantile(VarRegretPerc, probs = c(0.25), na.rm = TRUE),
                  Percentil75RegretPerc = quantile(VarRegretPerc, probs = c(0.75), na.rm = TRUE)
    )
    ,
    env = list(VarGroup = as.name(var_group),
               VarResposta = as.name(var_resposta),
               VarRegret = as.name(var_regret),
               VarRegretPerc = as.name(var_regret_perc)
    )
  )
  
  resumo = eval(call)  
  
  colnames(resumo) = c(
    var_group,
    paste(var_resposta, "Medio", sep = ""),
    paste(var_resposta, "Desvio", sep = ""),
    paste(var_resposta, "MediaSobreDesvio", sep = ""),
    paste(var_resposta, "Percentil25", sep = ""),
    paste(var_resposta, "Percentil75", sep = ""),
    paste(var_regret, "Medio", sep = ""),
    paste(var_regret, "Desvio", sep = ""),
    paste(var_regret, "Percentil25", sep = ""),
    paste(var_regret, "Percentil75", sep = ""),
    paste(var_regret_perc, "Medio", sep = ""),
    paste(var_regret_perc, "Desvio", sep = ""),
    paste(var_regret_perc, "Percentil25", sep = ""),
    paste(var_regret_perc, "Percentil75", sep = "")
  )
  
  resumo
}


##### ESCOLHER ESTRATÉGIA CANDIDATA #####

#' escolher_estrategia_candidata
#'
#' Escolhe a estratégia candidata "mais robusta" (dentre as disponíveis) a partir de dados simulados e de um resumo das estratégias contendo índices de perda de oportunidade.
#' @param dados Dataframe com dados simulados
#' @param resumo_estrategias dataframe de resumo das estratégias (gerado pela função calcular_e_resumir_regret).
#' @param var_resposta VAriávei de Resposta a considerar (string).
#' @param var_criterio Variável a usar como critério (default é o Percentil 75 do Regret Relativo).
#' @param sentido Sentido a utilizar na análise (default é min para minimizar o regret).
#'
#' @return uma (ou mais) estratégias candidatas. (ID da estratégia).
escolher_estrategia_candidata = function(dados, resumo_estrategias, var_resposta, var_criterio = "RegretPercPercentil75", sentido = "min") {
  
  var_respota_criterio = paste(var_resposta, var_criterio, sep = "")
  
  
  # Esta lista de criterios deve ser mantida igual à lista que a funcao resumir_variavel_resposta()
  possiveis_var_criterios = c("Percentil25", "Percentil75", "Medio", "Desvio", "RegretMedio", "RegretDesvio", "RegretPercentil25", "RegretPercentil75", "RegretPercMedio", "RegretPercDesvio", "RegretPercPercentil25", "RegretPercPercentil75")
  
  # Conferindo alguns pressupostos basicos:
  possiveis_var_respota_e_criterios = paste(var_resposta, possiveis_var_criterios, sep = "")
  
  # Conferindo se a variável de resposta e variável de critério combinam corretamente:
  if (!all(possiveis_var_respota_e_criterios %in% names(resumo_estrategias))){
    stop("Existe algo errado com a sua variavel de resposta ou variavel de criterio (a combinacao das duas no existe no resumo de estrategias).")
  }
  
  # Conferindo se a Variavel de criterio está correta.
  if(!var_criterio %in% possiveis_var_criterios){
    stop(paste("Esta variavel de criterio esta incorreta. escolha entre:",possiveis_var_criterios))
  }
  
  
  # Agora sim, posso escolhenr a estratégia que tem o menor percentil percentual 75 (assim como Lempert):
  estrategias_candidatas = switch(sentido,
                                  "min" = escolher_estrategia_min(resumo_estrategias, var_respota_criterio),
                                  "max" = escolher_estrategia_max(resumo_estrategias, var_respota_criterio))
  
  estrategias_candidatas
}


##### CALCULAR E RESUMIR REGRET #####

calcular_e_resumir_regret = function(dados, var_resposta, var_cenarios, var_estrategias, sentido = "max") {
  
  if (sentido == "max") {
    dados = calcular_regret(dados = dados, var_resposta = var_resposta, var_group = var_cenarios, sentido = "max")  
  } else {
    dados = calcular_regret(dados = dados, var_resposta = var_resposta, var_group = var_cenarios, sentido = "min")
  }
  
  
  # Resumindo Variável de Resposta Cash:
  resumo_estrategias = resumir_variavel_resposta(dados = dados, var_resposta = var_resposta, var_group = var_estrategias)
  
  # Formar lista de outputs dessta análise
  output = list(
    Dados = dados,
    ResumoEstrategias = resumo_estrategias
  )
  
  output
}

##### ESCOLHER ESTRATÉGIA #####

escolher_estrategia_min = function(resumo_estrategias, criterio) {
  linha_estrategia = which(resumo_estrategias[criterio] == min(resumo_estrategias[criterio]))
  estrategia = resumo_estrategias[linha_estrategia, "Lever"]  
  estrategia
}


escolher_estrategia_max = function(resumo_estrategias, criterio) {
  linha_estrategia = which(resumo_estrategias[criterio] == max(resumo_estrategias[criterio]))
  estrategia = resumo_estrategias[linha_estrategia, "Lever"]  
  estrategia
}

##### ANALISAR ENSEMBLE DETERMINANDO A MELHOR ESTRATÉGIA #####
analisar_ensemble_com_melhor_estrategia = function(ensemble, dados_regret, var_cenarios, var_estrategias, var_resposta, estrategia_candidata) {
  
  
  ensemble = as.data.frame(ensemble)
  dados_regret = as.data.frame(dados_regret)
  
  
  dados_regret["MelhorEstrategia"] = dados_regret[var_resposta] == dados_regret$MaximoPorScenario
  
  linhas_melhores_estrategias = which(dados_regret[var_resposta] == dados_regret$MaximoPorScenario)
  
  variaveis = c(var_cenarios, var_estrategias, var_resposta)
  
  melhores_estrategias = as.data.frame(dados_regret[linhas_melhores_estrategias, variaveis])
  
  ensemble_com_melhor_estrategia = dplyr::inner_join(ensemble, melhores_estrategias)
  
  ensemble_com_melhor_estrategia["EstrategiaCandidata"] = ensemble_com_melhor_estrategia[var_estrategias] == estrategia_candidata
  
  #ensemble_com_melhor_estrategia = as.factor(ensemble_com_melhor_estrategia[var_estrategias])
  
  ensemble_com_melhor_estrategia
  
}


##### ANÁLISE DE VULNERABILIDADE #####

#' obter_df_analise_vulnerabilidade
#'
#' @param results results da função de simulação do RDM (contendo análise de regret, necessáriamente).
#' @param estrategia_candidata número da estratégia ser analisada.
#' @param variavel_resposta nome da variávei de respota analisada nesta estratégia  (que deverá estar acima ou abaixo do threshold).
#' @param threshold número acima ou abaixo do qual a variável de resposta estará para ser considerada "interessante" para a análise.
#' @param planilha_inputs planilha de inputs com as variáveis incertas, para filtrarmos somente as variáveis incertas.
#' @param sentido_vulnerabilidade pode ser ">=" ou "<=". Se ">=", os casos de interesse serão aqueles onde a variáveil de interesse seja >= ao threshold.
#'
#' @return dataframe com o ensemble, variável de resposta e indicação se cada caso é um caso de interesse ou não.
#' @export
obter_df_vulnerabilidade = function(results, estrategia_candidata, variavel_resposta = "sNPVProfit1RegretPerc" , threshold = 0.1, planilha_inputs, sentido_vulnerabilidade = ">=") {
  
  if(sentido_vulnerabilidade == ">=") {
    results$AnaliseRegret$Dados$CasoInteresse = as.numeric(results$AnaliseRegret$Dados[,variavel_resposta] >= threshold)  
  } else {
    results$AnaliseRegret$Dados$CasoInteresse = as.numeric(results$AnaliseRegret$Dados[,variavel_resposta] <= threshold)  
  }
  
  # Obter Ensemble com Dados Simulados:
  ensemble_e_resultados = dplyr::inner_join(as.data.frame(results$Ensemble), results$AnaliseRegret$Dados, by = "Scenario")
  
  ensemble_e_resultados = ensemble_e_resultados[which(ensemble_e_resultados$Lever == estrategia_candidata),]
  
  # Retirar NAs do Ensemble
  ensemble_e_resultados = na.omit(ensemble_e_resultados)
  
  parametros_completos = readxl::read_xlsx(planilha_inputs, sheet = "params")
  
  variaveis_incertas = parametros_completos$Variavel[which(parametros_completos$Tipo=="Incerto")]
  
  x = ensemble_e_resultados[,c(variavel_resposta,"Scenario", "Lever",variaveis_incertas)]
  y = as.numeric(ensemble_e_resultados$CasoInteresse)
  
  data.frame(CasoInteresse = y, x)
}


#' obter_df_diff_media
#' Esta função serve para listar as variáveis que potencialmente mais distinguem os Casos de Interesse dos demais casos.
#' Esta função compara a média de cada variável dos casos de interesse com a média de todo o ensemble.
#' @param df_vulnerabilidade data.frame com a análise de vulnerabilidade retornado pela função obter_df_vulnerabilidade.
#'
#' @return data.frame que é um ranking de variáveis.
#' @export
#'
obter_df_diff_media_casos_interesse = function(df_vulnerabilidade) {
  medias_interesse = df_vulnerabilidade %>% dplyr::filter(CasoInteresse == 1) %>% dplyr::select(-CasoInteresse, -Scenario, -Lever, -sNPVProfit1Regret) %>% summarise_all(mean)
  
  medias_global = df_vulnerabilidade %>% dplyr::filter(CasoInteresse == 0) %>% dplyr::select(-CasoInteresse, -Scenario, -Lever, -sNPVProfit1Regret)  %>% dplyr::summarise_all(mean)
  
  max_global = df_vulnerabilidade %>% dplyr::select(-CasoInteresse, -Scenario, -Lever, -sNPVProfit1Regret) %>% dplyr::summarise_all(max)
  
  min_global = df_vulnerabilidade %>% dplyr::select(-CasoInteresse, -Scenario, -Lever, -sNPVProfit1Regret) %>% dplyr::summarise_all(min)
  
  range_global = max_global - min_global
  
  medias_dif = (medias_interesse - medias_global) / range_global
  
  v_medias_analisadas = colnames(medias_dif)
  
  v_medias_dif = unname(t(medias_dif)[,1])
  
  v_medias_global = unname(t(medias_global)[,1])
  
  v_range_global = unname(t(range_global)[,1])
  
  v_medias_interesse = unname(t(medias_interesse)[,1])
  
  ordem = order(abs(v_medias_dif), decreasing = TRUE)
  
  df_analise_medias = data.frame(
    Ranking = 1:length(v_medias_analisadas),
    Variavel = v_medias_analisadas[ordem],
    DifMediaRelativa = v_medias_dif[ordem],
    MediaCasosInteresse = v_medias_interesse[ordem],
    MediaGlobal = v_medias_global[ordem],
    Range = v_range_global[ordem]
  )  
}


#' obter_df_teste_t_casos_interesse
#' Realiza um Teste T para cada variável de incerteza. 
#' @param df_vulnerabilidade data.frame com a análise de vulnerabilidade retornado pela função obter_df_vulnerabilidade.
#'
#' @return data.frame que é um ranking de variáveis.
#' @export
#'
obter_df_teste_t_casos_interesse = function(df_vulnerabilidade) {
  
  casos_para_teste = df_vulnerabilidade %>% dplyr::select(-Scenario, -Lever, -sNPVProfit1Regret)
  
  # Teste T para Diferença de Médias
  casos_para_teste$CasoInteresse = as.factor(casos_para_teste$CasoInteresse)
  resultados_teste_t = t(sapply(casos_para_teste[-1], function(x) 
    unlist(t.test(x~casos_para_teste$CasoInteresse)[c("estimate","p.value","statistic")])))
  
  resultados_teste_t = as.data.frame(resultados_teste_t)
  
  resultados_teste_t$RejeitaH0_95Conf = resultados_teste_t$p.value < 0.05
  
  resultados_teste_t$RejeitaH0_99Conf = resultados_teste_t$p.value < 0.01
  
  resultados_teste_t$Variavel = rownames(resultados_teste_t)
  
  rownames(resultados_teste_t) = NULL
  
  est = unique(df_vulnerabilidade$Lever)
  
  names(resultados_teste_t) = c(paste("Média Est.",est,"não Falha"), paste("Média Est.",est,"Falha"), "Valor_P", "Est. T", "Rej. H0 95%", "Rej. H0 99%", "Variável")
  
  resultados_teste_t = dplyr::arrange(.data = resultados_teste_t, Valor_P)
  
  resultados_teste_t$Rank = 1:nrow(resultados_teste_t)
  
  resultados_teste_t = resultados_teste_t[,c(8,7,3,4,5,6,1,2)]
  
  resultados_teste_t
}




#' plot_violino_casos_interesse_por_variavel
#'
#' @param df_vulnerabilidade data.frame retornado pela função obter_df_vulnerabilidade
#' @param variavel nome da variável incerta
#' @param nome_amigavel_var nome amigável da variável incerta
#'
#' @return plot "violino" exibindo densidade da variável nos demais casos.
#' @export
#'
plot_violino_casos_interesse_por_variavel  = function(df_vulnerabilidade, variavel, nome_amigavel_var) {
  call_grafico = substitute(
    expr = ggplot2::ggplot(df_vulnerabilidade, aes(factor(CasoInteresse), Variavel)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +  geom_jitter(height = 0, width = 0.1)
    ,env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico) + xlab("Estratégia Vulnerável") + ylab(nome_amigavel_var) + scale_y_continuous(labels = format_for_humans)
  p
}



#' plot_dispersao_casos_interesse_por_variavel
#'
#' @param df_vulnerabilidade data.frame retornado pela função obter_df_vulnerabilidade
#' @param variavel1 nome da variável incerta 1 a considerar
#' @param nome_amigavel_var1 nome amigável desta variável
#' @param variavel2 nome da variável incerta 2 a considera
#' @param nome_amigavel_var2 nome amigável desta variável
#'
#' @return plot de dispersão sinalizando os casos de interesse
#' @export
#'
#' @examples
plot_dispersao_casos_interesse_por_variavel  = function(df_vulnerabilidade, variavel1, nome_amigavel_var1,  variavel2, nome_amigavel_var2) {
  call_grafico = substitute(
    expr =  ggplot(as.data.frame(df_vulnerabilidade), aes(x=Variavel1, y=Variavel2, color = as.factor(CasoInteresse)))
    ,env = list(Variavel1 = as.name(variavel1), Variavel2 = as.name(variavel2))
  )
  
  p =  eval(call_grafico)
  
  p = p + geom_point() + scale_color_manual(values = c("blue", "red"), name = "Caso de Interesse") + theme(legend.position = "bottom")
  
  p = p + xlab(nome_amigavel_var1) + ylab(nome_amigavel_var2) + scale_y_continuous(labels = format_for_humans) + scale_x_continuous(labels = format_for_humans)
  
  p
  
}


#' plot_dispersao_duas_variaveis
#' Gera um gráfico de dispersão de duas variáveis agrupando por lever.
#' @param df_dados data.frame retornado pela função obter_df_vulnerabilidade
#' @param variavel1 nome da variável incerta 1 a considerar
#' @param nome_amigavel_var1 nome amigável desta variável
#' @param variavel2 nome da variável incerta 2 a considera
#' @param nome_amigavel_var2 nome amigável desta variável
#' @param method pode ser lm, glm ou loess (consultar geom_smooth)
#' @param se default é TRUE, mostra intervalo de confiança.
#'
#' @return plot de dispersão sinalizando os casos de interesse
#' @export
#'
#' @examples
plot_dispersao_duas_variaveis  = function(df_dados, variavel1, nome_amigavel_var1,  variavel2, nome_amigavel_var2, linha_regr = TRUE, method = lm, se = TRUE, facet = TRUE) {
  call_grafico = substitute(
    expr =  ggplot(as.data.frame(df_dados), aes(x=Variavel1, y=Variavel2, color = factor(Lever)))
    ,env = list(Variavel1 = as.name(variavel1), Variavel2 = as.name(variavel2))
  )
  
  p =  eval(call_grafico)
  
  p = p + geom_point() + theme(legend.position = "bottom") # + scale_color_manual(values = c("blue", "red"), name = "Caso de Interesse") 
  
  # Adicionando Linha de Regressão
  if(linha_regr == TRUE){
    p = p + geom_smooth(method=method, aes(fill=factor(Lever)), show.legend = F, se = se)  
  }
  
  p = p + xlab(nome_amigavel_var1) + ylab(nome_amigavel_var2) + scale_y_continuous(labels = format_for_humans) + scale_x_continuous(labels = format_for_humans)

  
  p$labels$colour <- "Strategy"
  
  
  if(facet == TRUE){
    p = p + facet_wrap(~factor(Lever))
  }
  
  p
  
}


plot_dispersao_duas_variaveis_cor  = function(df_dados, variavel1, nome_amigavel_var1,  variavel2, nome_amigavel_var2, variavel_cor, method = lm, se = TRUE, facet = TRUE) {
  call_grafico = substitute(
    expr =  ggplot(as.data.frame(df_dados), aes(x=Variavel1, y=Variavel2, color = VariavelCor))
    ,env = list(Variavel1 = as.name(variavel1), Variavel2 = as.name(variavel2), VariavelCor = as.name(variavel_cor))
  )
  
  p =  eval(call_grafico)
  
  p = p + geom_point() + theme(legend.position = "bottom") # + scale_color_manual(values = c("blue", "red"), name = "Caso de Interesse") 
  
  p = p + xlab(nome_amigavel_var1) + ylab(nome_amigavel_var2) + scale_y_continuous(labels = format_for_humans) + scale_x_continuous(labels = format_for_humans)
  
  p$labels$colour <- variavel_cor
  
  p = p + scale_colour_gradient(low="red", high="green")
  
  if(facet == TRUE){
    p = p + facet_wrap(~factor(Lever))
  }
  
  p
  
}





##### FUNÇÕES AUXILIARES #####

library(dplyr)
selecionar_ultimo_periodo = function(dados_simulacao, var_tempo) {
  call = substitute(
    expr = dados_simulacao %>% dplyr::filter(Tempo == max(Tempo)),
    env = list(Tempo = as.name(var_tempo)))
  eval(call)  
}


selecionar_ultimo_periodo = function(dados_simulacao, var_tempo) {
  call = substitute(
    expr = dados_simulacao %>% dplyr::filter(Tempo == max(Tempo)),
    env = list(Tempo = as.name(var_tempo)))
  eval(call)  
}


calcular_maximo_por_variavel = function(var_resposta, var_group, dados) {
  call = substitute(
    expr = {dplyr::group_by(dados, VarGroup) %>%
        dplyr::summarise(Maximo = max(VarResposta))
    }
    ,
    env = list(VarGroup = as.name(var_group), VarResposta = as.name(var_resposta)))
  
  max_variavel_resposta = eval(call)
  
  dados_join = dplyr::inner_join(dados, max_variavel_resposta)
  
  dados_join$Maximo
}

calcular_minimo_por_variavel = function(var_resposta, var_group, dados) {
  call = substitute(
    expr = {dplyr::group_by(dados, VarGroup) %>%
        dplyr::summarise(Minimo = min(VarResposta))
    }
    ,
    env = list(VarGroup = as.name(var_group), VarResposta = as.name(var_resposta)))
  
  max_variavel_resposta = eval(call)
  
  dados_join = dplyr::inner_join(dados, max_variavel_resposta)
  
  dados_join$Minimo
}

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

##### CALIBRAÇÃO ####
getCost<-function(p, modelo, dados_calibracao){
  
  #browser()
  output_modelo = solve_modelo_dissertacao(parametros = p, modelo = modelo, simtime = SIM_TIME)
  #output_modelo <- solve_modelo_dissertacao(parametros, modelo, simtime = SIM_TIME)
  #http://www.inside-r.org/packages/cran/FME/docs/modCost
  #browser()
  
  cost <- modCost(obs=dados_calibracao, model=output_modelo)
  
  #browser()
  
  return(cost)
  
}


adicionar_erro_ao_ensemble = function(results, variavel_calibracao, planilha_calibracao, lever, opcoes = opcoes) {
  
  dados_calibracao <- as.data.frame(read_xlsx(path = planilha_calibracao, sheet = "Plan1"))
  
  variaveis_a_utilizar_modelo = c(opcoes$VarCenarios, opcoes$VarTempo, variavel_calibracao)
  
  variaveis_a_utilizar_dados = c(opcoes$VarTempo, variavel_calibracao)
  
  # Se um lever não foi informado, usar o primeiro do ensemble
  if(missing(lever)){
    # Usar o primeiro lever que eu achar
    lever = results$DadosSimulados$Lever[1]
  }
  
  dados_modelo = results$DadosSimulados[which(results$DadosSimulados$Lever == lever),variaveis_a_utilizar_modelo]
  
  cenarios = unique(dados_modelo$Scenario)
  
  # Função para Obter Estatísticas de Fit
  obter_estatisticas_fit = function(cenario, dados_modelo = dados_modelo , dados_calibracao = dados_calibracao, variaveis_a_utilizar_dados = variaveis_a_utilizar_dados) {
    modcost = FME::modCost(model = dados_modelo[which(dados_modelo$Scenario == cenario),],
                           obs = dados_calibracao[,variaveis_a_utilizar_dados])
    
    data_model = modcost$residuals$mod
    
    data_obs = modcost$residuals$obs
    
    n = modcost$var$N
    
    mean_model = mean(data_model, na.rm = TRUE)
    
    mean_data = mean(data_obs, na.rm = TRUE)
    
    sd_model = sqrt(sum((data_model - mean_model)^2)/n) 
    
    sd_data = sqrt(sum((data_obs - mean_data)^2)/n)
    
    correlation_coef = sum(((data_model - mean_model) / sd_model) * ((data_obs - mean_data) / sd_data)) / n
    
    # Como o objetivo não é predição não irei inserir R Squared.
    RSquared = correlation_coef ^ 2
    
    # Calcular os dados segundo sterman (para o R2 fechar, os índices também).
    correlacao = cor(x = modcost$residuals$obs, y = modcost$residuals$mod)
    
    # Soma dos Erros Quadrados
    SumOfSquareResiduals = modcost$model
    
    # Mean Square error
    MeanSquareError = SumOfSquareResiduals / modcost$var$N
    
    # Root Mean Square Error
    RootMeanSquareError = sqrt(MeanSquareError)
    
    # Mean Absolute Error
    MeanAbsoluteError = sum(abs(modcost$residuals$res)) / modcost$var$N
    
    # Mean Absolute Percent Error
    MeanAbsolutePercentError = (sum(abs(modcost$residuals$res) / abs(modcost$residuals$obs))) / modcost$var$N
    
    # Thiel Statistics: Morecroft (2007), pg. 399. 
    UM_ThielBiasDiffMeans =  ((mean_model - mean_data)^2) / MeanSquareError
    #V Sterman: UM_ThielBiasDiffMeans =  (mean_model^2 - mean_data^2) / MeanSquareError
    
    US_ThielUnequalVariation = ((sd_model - sd_data)^2) / MeanSquareError
    #V Sterman: US_ThielUnequalVariation = (sd_model^2 - sd_data^2) / MeanSquareError
    
    UC_ThielUnequalCovariation = (1 / MeanSquareError) * (sd_model * sd_data) * (2 * (1 - correlation_coef))
    
    # if(cenario == 140){
    #   browser()
    # }
    
    stats_fit = c(RSquared = RSquared,
                  r = correlation_coef,
                  RootMeanSquareError = RootMeanSquareError,
                  SumOfSquareResiduals = SumOfSquareResiduals,
                  MeanSquareError = MeanSquareError,
                  MeanAbsoluteError = MeanAbsoluteError,
                  MeanAbsolutePercentError = MeanAbsolutePercentError,
                  UM_ThielBiasDiffMeans = UM_ThielBiasDiffMeans,
                  US_ThielUnequalVariation = US_ThielUnequalVariation,
                  UC_ThielUnequalCovariation = UC_ThielUnequalCovariation)
    stats_fit
  }
  
  stats_fit = obter_estatisticas_fit(cenario = 1, dados_modelo = dados_modelo, dados_calibracao = dados_calibracao, variaveis_a_utilizar_dados = variaveis_a_utilizar_dados)
  
  stats_fit = lapply(1:length(cenarios), obter_estatisticas_fit, dados_modelo = dados_modelo, dados_calibracao = dados_calibracao, variaveis_a_utilizar_dados = variaveis_a_utilizar_dados)
  stats_fit = do.call(rbind, stats_fit)

  ensemble = cbind(results$Ensemble, stats_fit)
  
  ensemble

  # Só a partir deste ponto os calculos são realizados.
  # SomaSSR =  do.call(rbind, lapply(results$Ensemble[,opcoes$VarCenarios], obter_custo_cenario))
  # colnames(SomaSSR) = c("SomaSSR")
  # 
  # # Retornando o Ensemble com o Erro
  # cbind(results$Ensemble, SomaSSR)
  
}

##### GRÁFICOS ####
gerar_grafico_superficie = function(dados_ultimo_ano,variaveis, estrategia) {
  dadosplot = subset.data.frame(dados_ultimo_ano, (Lever == estrategia))
  
  dadosplot = dadosplot[variaveis]
  
  dadosplot = as.matrix(dadosplot)
  
  names = colnames(dadosplot)
  
  s = interp(dadosplot[,1],dadosplot[,2],dadosplot[,3])
  
  names(s) = names
  
  # Plotando a População Final
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  x <- list(
    title = "Taxa Nascimento",
    titlefont = f
  )
  y <- list(
    title = "TaxaMorte",
    titlefont = f
  )
  z <- list(
    title = "Populacao",
    titlefont = f
  )
  
  plot_ly(x = s[[1]], y = s[[2]], z = s[[3]]) %>% add_surface() %>% layout(xaxis = x, yaxis = y)
  
}

plot_clientes_uma_estrategia = function(dados, estrategia) {
  gr2_dados = subset(dados, (Lever == estrategia))
  ggplot2::ggplot(gr2_dados,
                  aes(x=Tempo, y=Adopters, color=factor(Lever), group=Scenario)) + 
    geom_line() + 
    ylab("Adopters") + 
    xlab("Time") +
    labs(color = "Strategy")
}

plot_cash_uma_estrategia = function(dados, estrategia) {
  gr2_dados = subset(dados, (Lever == estrategia))
  ggplot2::ggplot(gr2_dados,
                  aes(x=Tempo, y=Cash, color=factor(Lever), group=Scenario)) + 
    geom_line() + 
    ylab("Net Present Value") + 
    xlab("Time") +
    labs(color = "Strategy")
}

plot_linha_uma_variavel_ensemble_uma_estrategia = function(dados, variavel, nome_amigavel_variavel, estrategia) {
  
  gr2_dados = subset(dados, (Lever %in% estrategia))
  
  call_grafico = substitute(
    expr = ggplot2::ggplot(gr2_dados, aes(x= time, y= Variavel, color=factor(Lever) , group= interaction(Lever, Scenario) 
                                          )),
    env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico)
  
  p + 
    geom_line() + 
    ylab(nome_amigavel_variavel) + 
    xlab("Time") + 
    theme(legend.position="bottom")  +
    labs(color = "Strategy") +
    scale_y_continuous(labels = format_for_humans)
}

plot_linha_uma_variavel_players_um_cenario = function(dados = results$DadosSimulados, estrategia = 1, cenario = 4, variavel = "sNPVProfit", nome_amigavel_variavel = "VPL", opcoes = opcoes){
  variaveis_players = paste(variavel, 1:N_PLAYERS, sep="")
  # Filtrando Dados de Interesse
  dados = dados[,c(opcoes$VarTempo, opcoes$VarCenarios, opcoes$VarEstrategias, variaveis_players)] %>% subset(., Lever == estrategia & Scenario == cenario)
  
  # Trazer Dados Simulados para o formato "Longo" para permitir que o gráfico seja realizado
  dados_longo = tidyr::gather(dados, player, variavel, 4:(3+N_PLAYERS))
  
  # Removendo Nome longo da Variável
  dados_longo$player =  gsub(variavel,"P", dados_longo$player,ignore.case=T)
  
  # Adicionar ruído à variável para o gráfico distinguir melhor os players
  
  p = ggplot2::ggplot(dados_longo, aes(x= time, y= variavel, color=player, group = player))
  p + geom_line() +
    ylab(nome_amigavel_variavel) + 
    xlab("Time") + 
    theme(legend.position="bottom")  +
    labs(color = "Player") +
    scale_y_continuous(labels = format_for_humans)
}



plot_linha_uma_variavel_ensemble = function(dados, variavel, nome_amigavel_variavel, estrategia) {
  
  levers_no_ensemble = unique(dados$Lever)
  
  # Caso exista mais de uma estratégia, usar somente a primeira.
  if(length(levers_no_ensemble) > 1) {
    dados = subset(dados, Lever == 1)
  }
  
  call_grafico = substitute(
    expr = ggplot2::ggplot(dados, aes(x= time, y= Variavel, color=Scenario, group= Scenario 
    )),
    env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico)
  
  p + 
    geom_line() + 
    ylab(nome_amigavel_variavel) + 
    xlab("Time") + 
    theme(legend.position="bottom")  +
    labs(color = "Case") +
    scale_y_continuous(labels = format_for_humans)
}



plot_linha_uma_variavel = function(dados, variavel, nome_amigavel_variavel) {
  
  call_grafico = substitute(
    expr = ggplot2::ggplot(dados, aes(x= time, y= Variavel)),
    env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico)
  
  p + 
    geom_line() + 
    ylab(nome_amigavel_variavel) + 
    xlab("Time") + 
    theme(legend.position="bottom") +
    scale_y_continuous(labels = format_for_humans)
}


plot_partial_plot = function(dados, variavel, nome_amigavel_variavel) {
  
  call_grafico = substitute(
    expr = ggplot2::ggplot(dados, aes(x= Variavel, y= VariavelYHAT)),
    env = list(Variavel = as.name(variavel), VariavelYHAT = as.name(paste0("yhat.",variavel)))
  )
  
  p <- eval(call_grafico)
  
  p + 
    geom_line() + 
    ylab("yhat") + 
    xlab(variavel) + 
    theme(legend.position="bottom") +
    scale_y_continuous(labels = format_for_humans) +
    scale_x_continuous(labels = format_for_humans) + 
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=8))
    #theme(axis.text.x = element_text(size=20)) # + ggtitle(paste0("Part. Dep.: ",v))
}


plot_partial_plot_n_variaveis = function(dados) {
  
  p <- ggplot2::ggplot(dados, aes(x= Incerteza, y= PartialDependence))
  
  p = p + 
    geom_line() + 
    ylab("Partial Dependence") + 
    xlab("Uncertain Variable") + 
    theme(legend.position="bottom") +
    scale_y_continuous(labels = format_for_humans) +
    scale_x_continuous(labels = format_for_humans) + 
    theme(axis.text=element_text(size=8),
          axis.title=element_text(size=8)) 
  
  p = p + facet_wrap(~Variavel, scales = "free_x", ncol = 3, strip.position = "bottom")
  p 
  #theme(axis.text.x = element_text(size=20)) # + ggtitle(paste0("Part. Dep.: ",v))
}




plot_linha_duas_variaveis = function(dados, variavel1, nome_amigavel_variavel1, variavel2, nome_amigavel_variavel2) {
  
  p <- ggplot2::ggplot(dados, aes(x = time))
  
  call_variavel1 = substitute(
    expr = p + geom_line(aes(y = Variavel, colour = NomeVariavel)),
    env = list(Variavel = as.name(variavel1), NomeVariavel = nome_amigavel_variavel1)
  ) 
  
  razaovariavel = (max(dados[,variavel1]) - min(dados[,variavel1])) / (max(dados[,variavel2]) - min(dados[,variavel2]))
  
  p <- eval(call_variavel1)
  
  call_variavel2 = substitute(
    expr = p + geom_line(aes(y = Variavel * Razao, colour = NomeVariavel)),
    env = list(Variavel = as.name(variavel2), NomeVariavel = nome_amigavel_variavel2, Razao = razaovariavel)
  ) 
  
  p <- eval(call_variavel2)
  
  # now adding the secondary axis, following the example in the help file ?scale_y_continuous
  # and, very important, reverting the above transformation
  p <- p + scale_y_continuous(sec.axis = sec_axis(~./razaovariavel, name = nome_amigavel_variavel2, labels = format_for_humans), labels = format_for_humans)
  
  # modifying colours and theme options
  p <- p + scale_colour_manual(values = c("blue", "red"))
  p <- p + labs(y = nome_amigavel_variavel1,
                x = "Time",
                colour = "Variable")
  
  p <- p + theme(legend.position="bottom")

  p
  
}



plot_taxa_adocao_uma_estrategia = function(dados, estrategia) {
  gr2_dados = subset(dados, (Lever == estrategia))
  ggplot2::ggplot(gr2_dados,
                  aes(x=Tempo, y=Adoption_Rate, color=factor(Lever), group=Scenario)) + 
    geom_line() + 
    ylab("Adoption Rate") + 
    xlab("Time") +
    labs(color = "Strategy")
}


#' grafico_whisker_por_lever
#'
#' Este grafico 
#' 
#' @param dados_regret dados resultantes da análise de regret.
#' @param variavel nome da variável simulada a realizar o gráfico.
#' @param nome_amigavel_variavel nome amigável para a variável de resposta (plotada no eixo y)
#'
#' @return grafico whisker do ggplot2
#' @export
#'
grafico_whisker_por_lever = function(dados_regret, variavel, nome_amigavel_variavel) {
  dados_por_estrategia = dplyr::group_by(dados_regret, Lever)
  
  dados_por_estrategia$Lever = as.factor(dados_por_estrategia$Lever)
  
  # Gerando Grafico da Variável de Perda de Oportunidade
  call_grafico = substitute(
    expr = ggplot(dados_por_estrategia, aes(y = Variavel,x = Lever, group = Lever)),
    env = list(Variavel = as.name(variavel))
  )
  
  p <- eval(call_grafico)
  p + geom_boxplot() + 
    scale_y_continuous(labels = format_for_humans) + 
    ylab(nome_amigavel_variavel) + 
    theme(axis.text.x = element_text(size=7))
}

#' plot_fronteira_tradeoff_estrategia
#' 
#' Esta funcao ainda nao é completamente generalizada. estao dentro desta funcao a definicao do cenário a ser analisado.
#'
#' @param results list com os dados resultantes da funcao de simulacao e analise
#' @param opcoes list de opcoes do modelo (as opcoes sao padronizadas.)
#'
#' @return grafico plotly com a fronteira de tradeoffs conforme um determinado cenário.
#' @export
#'
plot_fronteira_tradeoff_estrategia = function(results, opcoes = opcoes) {
  
  ensemble = as.data.frame(results$Ensemble)
  
  # Na linha abaixo as variáveis devem ser definidas.
  # Resultados da Análise em 2/01
  # cenarios_escolhidos = subset(ensemble,
  #                              aReferenceIndustryDemandElasticity > 0.127 &
  #                                aReferenceIndustryDemandElasticity < 0.940 &
  #                                aFractionalDiscardRate > 0.143 &
  #                                aReferencePopulation > 5.3 * 10 ^ 4)
  
  # Resultados em 03/01:
  
  cenarios_escolhidos = subset(ensemble,
                                 aReferencePopulation > 5.8 * 10 ^ 4 &
                                 aNormalCapacityUtilization > 0.626 &
                                 aNormalCapacityUtilization < 0.864 &
                                 aDesiredMarketShare2 > 0.325 &
                                 aDesiredMarketShare2 < 0.528 &
                                 aSwitchForCapacityStrategy4 < 2.14 &
                                 aSwitchForCapacityStrategy4 > 0.611 &
                                 aSensOfAttractToPrice > -11.3
                                 )
  
  
  
  numero_cenarios_escolhidos = cenarios_escolhidos$Scenario
  
  dados_cenario = subset(results$DadosUltimoPeriodo, Scenario %in% numero_cenarios_escolhidos)
  
  #dados_cenario = results$DadosUltimoPeriodo %>% dplyr::filter(AdvertisingCost  < 5.727e+04 & AverageTicket  >  1.789e+00 & AdoptionFraction  <  2.895e-02)
  
  analise_regret_cenario = calcular_e_resumir_regret(dados = dados_cenario, var_resposta = opcoes$VarResposta, var_cenarios = opcoes$VarCenarios, var_estrategias = opcoes$VarEstrategias)
  
  variavel_comparacao = paste(opcoes$VarResposta,opcoes$VarCriterio, sep = "")
  
  variaveis_grafico_regret = c(opcoes$VarEstrategias, variavel_comparacao)
  
  regret_todos_os_futuros = results$AnaliseRegret$ResumoEstrategias[variaveis_grafico_regret]
  
  regret_todos_os_futuros = as.data.frame(regret_todos_os_futuros)
  
  names(regret_todos_os_futuros) = c(opcoes$VarEstrategias, "PerdaOportunidadeTodosOsCenarios")
  
  # names(regret_todos_os_futuros[variaveis_grafico_regret]) = c(opcoes$VarEstrategias, paste(variavel_comparacao, "TodosOsCenarios", sep = ""))
  
  regret_cenario = analise_regret_cenario$ResumoEstrategias[variaveis_grafico_regret]
  
  regret_cenario = as.data.frame(regret_cenario)
  
  names(regret_cenario) = c(opcoes$VarEstrategias, "PerdaOportunidadeNoCenario")
  
  dados_join = dplyr::left_join(regret_todos_os_futuros, regret_cenario)
  
  dados_join = dplyr::inner_join(dados_join, results$Inputs$Levers)
  
  # Esta descrição é customizada para os cenários definidos aqui.
  dados_join$Descricao = paste("CS",dados_join$aSwitchForCapacityStrategy1,"MS",dados_join$aDesiredMarketShare1,"OR",dados_join$aOrcamentoPeD1,"AB",dados_join$aPercPeDAberto1, sep = ".")
  
  # Gerando Plot com Tradeoff de Estratégias.
  plot_tradeoff_estrategias = ggplot2::ggplot(dados_join, aes(x=PerdaOportunidadeTodosOsCenarios, y=PerdaOportunidadeNoCenario, fill=aPercPeDAberto1)) +
    geom_label(label=as.character(dados_join$Lever), color="white", size=3) +
    scale_y_continuous(labels = format_for_humans) + 
    scale_x_continuous(labels = format_for_humans) +
    ylab("Regret (75% Percentile) - Vunerable Scenario") +
    xlab("Regret (75% Percentile) - All other Scenarios")
  
  `%notin%` = function(x,y) !(x %in% y)
  
  # definindo estratégias a usar neste gráfico: as 8 mais robustas em geral
  dados_join = dados_join %>% dplyr::arrange(PerdaOportunidadeTodosOsCenarios)
  
  # Estratégias a entrar no gráfico: As 6 mais robustas em geral:
  estrategias_grafico = dados_join$Lever[1:6]
  
  regret_medio_cenario_falha = results$AnaliseRegret$Dados %>% dplyr::filter(Scenario %in% numero_cenarios_escolhidos, Lever %in% estrategias_grafico) %>% group_by(Lever) %>% summarise(MediaRegretCenarioFalha = mean(sNPVProfit1Regret))
  
  regret_medio_cenario_sucesso = results$AnaliseRegret$Dados %>% dplyr::filter(Scenario %notin% numero_cenarios_escolhidos, Lever %in% estrategias_grafico) %>% group_by(Lever) %>% summarise(MediaRegretCenarioSucesso = mean(sNPVProfit1Regret))
  
  ProbCenario = seq(from = 1/101, to = 100/101, length.out = 100)
  
  OddsCenario = ProbCenario / (1-ProbCenario)
  
  #ProbCenario = c(1/1001, 1/101, 1/11, 1/2, 10/11, 100/101, 1000/1001)
  #OddsCenario = c(0.001, 0.01, 0.1, 1, 10, 100, 1000)
  
  OddsTextoBreaks = c("1:100", "1:10", "1:1", "10:1", "100:1")
  OddsCenarioBreaks = c(0.01, 0.1, 1, 10, 100)
  
  probs_df = data.frame(ProbCenario, OddsCenario)
  
  odds_e_levers = data.frame(expand.grid(Lever = results$Inputs$Levers$Lever, OddsCenario = OddsCenario))
  
  odds_e_levers = dplyr::inner_join(odds_e_levers, probs_df)
  
  tabela_analise_tradeoff = dplyr::inner_join(regret_medio_cenario_falha, 
                                              regret_medio_cenario_sucesso)
  
  tabela_analise_tradeoff = dplyr::inner_join(tabela_analise_tradeoff, 
                                              odds_e_levers)
  
  tabela_analise_tradeoff$RegretEsperado = tabela_analise_tradeoff$ProbCenario * tabela_analise_tradeoff$MediaRegretCenarioFalha + (1-tabela_analise_tradeoff$ProbCenario) * tabela_analise_tradeoff$MediaRegretCenarioSucesso
  
  # Tabela Gerada, Gerar Plot
  
  plot_curva_tradeoff = ggplot2::ggplot(tabela_analise_tradeoff,
                                        aes(x=OddsCenario, y=RegretEsperado, color=factor(Lever), group=factor(Lever))) + 
                          geom_line(size=1) + 
                          ylab("Expected Regret ($)") + 
                          xlab("Odds of Vulnerable Scenario") +
                          labs(color = "Strategy") +
                          scale_x_continuous(breaks = OddsCenarioBreaks, labels = OddsTextoBreaks, trans = "log10") + 
                          scale_y_continuous(labels = format_for_humans)
  
  list(
    DadosTradeoffPontos = dados_join,
    PlotTradeoffDispersao = plot_tradeoff_estrategias,
    DadosTradeoffChances = tabela_analise_tradeoff,
    PlotTradeoffOdds = plot_curva_tradeoff
  )
  
}


#' plot_fronteira_tradeoff_estrategia
#' 
#' Esta funcao ainda nao é completamente generalizada. estao dentro desta funcao a definicao do cenário a ser analisado.
#'
#' @param results list com os dados resultantes da funcao de simulacao e analise
#' @param opcoes list de opcoes do modelo (as opcoes sao padronizadas.)
#'
#' @return grafico plotly com a fronteira de tradeoffs conforme um determinado cenário.
#' @export
#'
plot_tradeoff_regret_vpl = function(results, opcoes = opcoes) {
  
  ensemble = as.data.frame(results$Ensemble)
  
  # Na linha abaixo as variáveis devem ser definidas.
  # Resultados da Análise em 2/01
  # cenarios_escolhidos = subset(ensemble,
  #                              aReferenceIndustryDemandElasticity > 0.127 &
  #                                aReferenceIndustryDemandElasticity < 0.940 &
  #                                aFractionalDiscardRate > 0.143 &
  #                                aReferencePopulation > 5.3 * 10 ^ 4)
  
  # Resultados em 03/01:
  
  cenarios_escolhidos = subset(ensemble,
                               aReferencePopulation > 5.8 * 10 ^ 4 &
                                 aNormalCapacityUtilization > 0.626 &
                                 aNormalCapacityUtilization < 0.864 &
                                 aDesiredMarketShare2 > 0.325 &
                                 aDesiredMarketShare2 < 0.528 &
                                 aSwitchForCapacityStrategy4 < 2.14 &
                                 aSwitchForCapacityStrategy4 > 0.611 &
                                 aSensOfAttractToPrice > -11.3
  )
  
  
  
  numero_cenarios_escolhidos = cenarios_escolhidos$Scenario
  
  dados_cenario = subset(results$DadosUltimoPeriodo, Scenario %in% numero_cenarios_escolhidos)
  
  #dados_cenario = results$DadosUltimoPeriodo %>% dplyr::filter(AdvertisingCost  < 5.727e+04 & AverageTicket  >  1.789e+00 & AdoptionFraction  <  2.895e-02)
  
  analise_regret_cenario = calcular_e_resumir_regret(dados = dados_cenario, var_resposta = opcoes$VarResposta, var_cenarios = opcoes$VarCenarios, var_estrategias = opcoes$VarEstrategias)
  
  variavel_comparacao = paste(opcoes$VarResposta,opcoes$VarCriterio, sep = "")
  
  variaveis_grafico_regret = c(opcoes$VarEstrategias, variavel_comparacao, "sNPVProfit1Percentil75")
  
  regret_todos_os_futuros = results$AnaliseRegret$ResumoEstrategias[variaveis_grafico_regret]
  
  regret_todos_os_futuros = as.data.frame(regret_todos_os_futuros)
  
  names(regret_todos_os_futuros) = c(opcoes$VarEstrategias, "PerdaOportunidade75", "VPL75")
  
  dados_join = dplyr::inner_join(regret_todos_os_futuros, results$Inputs$Levers)
  
  # Esta descrição é customizada para os cenários definidos aqui.
  dados_join$Descricao = paste("CS",dados_join$aSwitchForCapacityStrategy1,"MS",dados_join$aDesiredMarketShare1,"OR",dados_join$aOrcamentoPeD1,"AB",dados_join$aPercPeDAberto1, sep = ".")
  
  ggplot2::ggplot(dados_join, aes(x=PerdaOportunidade75, y=VPL75, fill=aPercPeDAberto1)) +
    geom_label(label=as.character(dados_join$Lever), color="white", size=3) +
    scale_y_continuous(labels = format_for_humans) + 
    scale_x_continuous(labels = format_for_humans) +
    ylab("Net Present Value (75% Percentile)") +
    xlab("Regret (75% Percentile)")
  
  
}

#' plot_estrategias_versus_incertezas
#'
#' @param ensemble_analisado data.frame resultante da funcao analisar_ensemble_com_melhor_estrategia
#' @param incertezas vetor com nomes das variáveis de incerteza a incluir no gráfico, devem corresponder à variáveis no ensemble.
#' @param binario default \code{TRUE}, pode apresentar a divisão entre as outras estratégias ou não.
#'
#' @return grafico que mostra a que condições a estratégia candidata é mais sucetível a falhar.
#' @export
plot_estrategias_versus_incertezas = function(df_vulnerabilidade, incertezas, binario = TRUE) {
  
  df_vulnerabilidade$CasoInteresse = as.factor(df_vulnerabilidade$CasoInteresse)
  
  p = if(binario) {
    GGally::ggpairs(df_vulnerabilidade, columns = incertezas, aes(colour = CasoInteresse, alpha = 0.7))
  } else {
    GGally::ggpairs(df_vulnerabilidade, columns = incertezas, aes(colour = sNPVProfit1Regret, alpha = 0.7))
  }
  
  p
}


plot_landscape_futuros_plausiveis = function(results, estrategia, variavelresp, nomeamigavel_variavelresp, variavel1, n_variavel1, variavel2, n_variavel2) {
  
  ensemble_e_resultados = dplyr::inner_join(as.data.frame(results$Ensemble), results$AnaliseRegret$Dados, by = "Scenario")
  
  ensemble_e_resultados = ensemble_e_resultados[which(ensemble_e_resultados$Lever == estrategia),]
  
  var_x = ensemble_e_resultados[,variavel1]
  var_y = ensemble_e_resultados[,variavel2] 
  var_z = ensemble_e_resultados[,variavelresp]
  
  my.df.interp <- interp(x = var_x, y = var_y, z = var_z, nx = 30, ny = 30)
  my.df.interp.xyz <- as.data.frame(interp2xyz(my.df.interp))
  names(my.df.interp.xyz) <- c(variavel1, variavel2, variavelresp)
  
  my.df.interp.xyz = my.df.interp.xyz[complete.cases(my.df.interp.xyz),] 
  
  
  call_plot = substitute(
    expr = ggplot(data = my.df.interp.xyz, aes(x = Variavelx, y = Variavely)) + geom_tile(aes(fill = Variavelz)),
    env = list(Variavelx = as.name(variavel1), Variavely = as.name(variavel2), Variavelz = as.name(variavelresp))
  ) 
  
  p <- eval(call_plot)
  p <- p + xlab(n_variavel1) + 
    ylab(n_variavel2) + 
    labs(fill = nomeamigavel_variavelresp) + 
    ggtitle(label = paste("Estratégia", estrategia)) + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "bottom") + 
    viridis::scale_fill_viridis()
    
    #scale_color_gradient()
    
    #scale_fill_gradient(low="green", high="red")
  
  p + scale_x_continuous(labels = format_for_humans) + scale_y_continuous(labels = format_for_humans)
}

plot_grid_estrategias_casos_vpl = function(results) {
  plot<-ggplot(results$DadosUltimoPeriodo, aes(Scenario, Lever, fill = sNPVProfit1)) + 
    geom_tile(colour="gray20", size=1.5, stat="identity") + 
    scale_fill_viridis(option="D") +
    scale_y_continuous(breaks=1:6)+
    xlab("Case") + 
    ylab("Strategy") +
    theme(
      #plot.title = element_text(color="white",hjust=0,vjust=1, size=rel(2)),
      plot.background = element_blank(),
      panel.background = element_blank(),
      #panel.border = element_rect(fill=NA,color="gray20", size=0.5, linetype="solid"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(), 
      #axis.text = element_text(color="white", size=rel(1.5)),
      axis.text.y  = element_text(hjust=1),
      #legend.text = element_text(color="white", size=rel(1.3)),
      #legend.background = element_rect(fill="gray20"),
      legend.position = "right"
      #legend.title=element_blank()
    )
  
  plot$labels$fill = "NPV"
  plot
}


sdrdm.pairs_plot = function(data, lever, variables) {
  dados_grafico = subset(data, Lever == lever)
  dados_grafico = dados_grafico[variables]
  
  pairs(dados_grafico)
}


#' salvar_plots_result
#'
#' Esta função gera uma série de gráficos a partir de um objeto de resultados
#' @param results  objeto de resultados retornado pela função simular_RDM
#' @param cenario_plot_players um cenário escolhido para exibir plots comparando os players.
#' @param estrategia_candidata o número de uma estratégia candidata a testar para filtrar os plots
#' @param opcoes variável global de opções.
#'
#' @return não gera nenhum retorno. Esta função salva os gráficos na pasta /imagem.
#' @export
#'
salvar_plots_result = function(results, cenario_plot_players, estrategia_candidata, opcoes = opcoes){
  # Nome Objeto
  nome_resultado = deparse(substitute(results))
  
  estrategia_plot_players = estrategia_candidata
  
  plots_linha_geral = list(
    plot_estrategia_candidata_vpl = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sNPVProfit1", nome_amigavel_variavel = "Net Present Value", estrategia = estrategia_candidata),
    plot_estrategia_candidata_preco = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "sPrice1", nome_amigavel_variavel = "Player 1 Avg. Price", estrategia = estrategia_candidata),
    plot_estrategia_candidata_share = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "aOrderShare1", nome_amigavel_variavel = "Player 1 Market Share", estrategia = estrategia_candidata),
    plot_estrategia_candidata_demanda_global = plot_linha_uma_variavel_ensemble(dados = results$DadosSimulados, variavel = "aIndustryShipments", nome_amigavel_variavel = "Prof. 3D Printer Sales", estrategia = estrategia_candidata)
  )
  
  plots_whisker = list(
    plot_whisker_lever_perc_regret = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1RegretPerc", nome_amigavel_variavel = "Percent Regret"),
    plot_whisker_lever_regret = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1Regret", nome_amigavel_variavel = "Regret"),
    plot_whisker_lever_profit = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sNPVProfit1", nome_amigavel_variavel = "Net Present Value"),
    plot_whisker_lever_share = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "aOrderShare1",  nome_amigavel_variavel = "Player 1 Market Share"),
    plot_whisker_lever_industry_order_rate = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "aIndustryShipments", nome_amigavel_variavel = "Prof. 3D Printer Sales"),
    plot_whisker_lever_price = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sPrice1", nome_amigavel_variavel = "Player 1 Avg. Price"),
    plot_whisker_lever_installed_base = grafico_whisker_por_lever(results$AnaliseRegret$Dados, variavel = "sInstalledBase1", nome_amigavel_variavel = "Player 1 Installed Base")
  )
  
  plots_players = list(
    plot_players_vpl = plot_linha_uma_variavel_players_um_cenario(dados = results$DadosSimulados, 
                                                                  estrategia = estrategia_plot_players, 
                                                                  cenario = cenario_plot_players, 
                                                                  variavel = "sNPVProfit", 
                                                                  nome_amigavel_variavel = "Net Present Value", 
                                                                  opcoes = opcoes)
    
    ,plot_players_vpl = plot_linha_uma_variavel_players_um_cenario(dados = results$DadosSimulados, 
                                                                   estrategia = estrategia_plot_players, 
                                                                   cenario = cenario_plot_players, 
                                                                   variavel = "aOrderShare", 
                                                                   nome_amigavel_variavel = "Market Share", 
                                                                   opcoes = opcoes)
    
    
    ,plot_players_net_income = plot_linha_uma_variavel_players_um_cenario(dados = results$DadosSimulados, 
                                                                          estrategia = estrategia_plot_players, 
                                                                          cenario = cenario_plot_players, 
                                                                          variavel = "fNetIncome", 
                                                                          nome_amigavel_variavel = "Net Profit", 
                                                                          opcoes = opcoes)
    
    
    ,plot_players_performance = plot_linha_uma_variavel_players_um_cenario(dados = results$DadosSimulados, 
                                                                           estrategia = estrategia_plot_players, 
                                                                           cenario = cenario_plot_players, 
                                                                           variavel = "aPerformance", 
                                                                           nome_amigavel_variavel = "Product Performance", 
                                                                           opcoes = opcoes)
    
    ,plot_players_pantes = plot_linha_uma_variavel_players_um_cenario(dados = results$DadosSimulados, 
                                                                      estrategia = estrategia_plot_players, 
                                                                      cenario = cenario_plot_players, 
                                                                      variavel = "aPatentesEmpresaTemAcesso", 
                                                                      nome_amigavel_variavel = "Patents Accessed by Player", 
                                                                      opcoes = opcoes)
  )
  
  
  # Salvando os Gráficos
  mapply(ggsave, file=paste0("./images/",nome_resultado,"-estrat",estrategia_candidata,"-",names(plots_linha_geral), ".png"), plot=plots_linha_geral, width = plots_width, height = plots_heigh)
  
  mapply(ggsave, file=paste0("./images/",nome_resultado,"-", names(plots_whisker), ".png"), plot=plots_whisker, width = plots_width, height = plots_heigh)
  
  mapply(ggsave, file=paste0("./images/",nome_resultado,"-cenario",cenario_plot_players,"-", names(plots_players), ".png"), plot=plots_players, width = plots_width, height = plots_heigh)
  
  # Retornar objeto com todos os plots:
  list(
    plots_players = plots_players,
    plots_whisker = plots_whisker,
    plots_linha_geral = plots_linha_geral
  )
}

#### DEMONSTRAÇÕES ####
#' gerar_grafico_curva_experiencia
#'
#' @return gráfico demonstrando como a curva de experiência do modelo funciona.
#' @export
gerar_grafico_curva_experiencia = function() {
  aLCStrength = c(0.5, 0.75, 0.85, 0.95, 1)
  
  aLCExponent = log(aLCStrength)/log(2)
  
  aInitialProductionExperience = 1000
  
  sCumulativeProduction = 1000:5000
  
  learning_df = data.frame(Learning0.5 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[1],
                           Learning0.75 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[2],
                           Learning0.85 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[3],
                           Learning0.95 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[4],
                           Learning1 = (sCumulativeProduction/aInitialProductionExperience)^aLCExponent[5])
  
  aInitialUnitFixedCost = 2000
  
  aInitialUnitVariableCost = 1000
  
  # aUnitFixedCost = aLearning * aInitialUnitFixedCost
  
  aUnitVariableCost = data.frame(learning_df * aInitialUnitVariableCost, Producao = sCumulativeProduction) 
  
  ggplot2::ggplot(aUnitVariableCost, aes(Producao)) + 
    geom_line(aes(y = Learning1, colour = "1")) + 
    geom_line(aes(y = Learning0.95, colour = "0.95")) + 
    geom_line(aes(y = Learning0.85, colour = "0.85")) + 
    geom_line(aes(y = Learning0.75, colour = "0.75")) + 
    geom_line(aes(y = Learning0.5, colour = "0.5")) + 
    ylab("Custo Variável de Produção") + 
    xlab("Produção Acumulada") +
    labs(color = expression(Gamma))
}

#### DADOS EXTERNOS ####

#' obter_dados_fundamentos_us_fundamentals
#' Esta rotina obtem dados do site http://www.usfundamentals.com/ usando a api deles. Esta rotina também trata os dados obtidos e os deixa prontos para serem usados em gráficos pelo ggplot2.
#' Todos os dados baixados são mantidos na pasta /fundamentals-data/ para consulta posterior.
#' 
#' @param api_us_fundamentals chave para a api
#' @param empresas nomes de empresas a buscar na base (pode ser definido arbitráriamente).
#' @param codigos códigos de empresas a buscar na base
#' @param indicadores vetor com o nome dos indicadores a manter
#'
#' @return
#' @export
#'
#' @examples
obter_dados_fundamentos_us_fundamentals = function(api_us_fundamentals = "AzfxuwuOWMDrCA28nAEUcw", empresas = c("Stratasys Inc", "3D Systems Corp", "Exone CO", "Proto Labs"), codigos = c(915735, 910638, 1561627, 1443669), indicadores = c("Revenues", "OperatingIncomeLoss", "SalesRevenueNet ", "GrossProfit", "NetCashProvidedByUsedInOperatingActivities", "ResearchAndDevelopmentExpense")) {
  codigos_api = paste(codigos, collapse = ",")
  
  indicadores_api = paste(indicadores, collapse = ",")
  
  call_api = paste(
    "https://api.usfundamentals.com/v1/indicators/xbrl?",
    "&companies=",
    codigos_api,
    "&period_type=yq",
    "&token=",
    api_us_fundamentals
    , sep = ""
  )
  
  anos = 2011:2016
  
  csv_dados_us_fundamentals <- RCurl::getURL(call_api)
  
  dados_us_fundamentals = read.csv(textConnection(csv_dados_us_fundamentals))
  
  # renomeando colunas
  colnames(dados_us_fundamentals) = c("company_id", "indicator_id", anos)
  
  # Gerando tabela de empresas
  empresas_e_codigos = data.frame(company_id = codigos, empresa = empresas)
  
  # Indicando nome da empresa
  dados_us_fundamentals = dplyr::inner_join(dados_us_fundamentals, empresas_e_codigos)
  
  
  # Escrevendo CSV para guardar os dados antes de filtrar:
  write.csv2(x = dados_us_fundamentals, file = "./fundamentals-data/dados_us_fundamentals.csv")
  
  # Filtrando só o indicador desejado, ou definindo um conjunto de indicadores
  if (length(indicadores)> 0){
    dados_us_fundamentals = subset(dados_us_fundamentals, indicator_id %in% indicadores)  
  } else {indicadores = unique(dados_us_fundamentals$indicator_id)}
  
  dados_finais = dados_us_fundamentals %>% tidyr::gather(Ano, Valor, 3:8) %>% tidyr::spread(indicator_id, Valor)
  
  dados_finais  
}


#' obter_fundamentos_financeiros_quandl
#' Função obtém dados de fundamentos financeiros usando a biblioteca quandl.
#' @param company_code código da empresa constante na base de dados https://www.quandl.com/data/SF0-Free-US-Fundamentals-Data
#'
#' @return list com dados financeiros
#' @export
obter_fundamentos_financeiros_quandl = function(company_code = "DDD") {
  
  # Base de Dados: - 
  # https://www.quandl.com/data/SF0-Free-US-Fundamentals-Data
  # Com algumas alterações é possível usar outras bases do quandl.
  
  # Definindo Chave de Acesso - Esta chave pertence a Pedro Lima, não utilizar:
  Quandl.api_key("RsCuvs4_WjRPP_zzSzfv")
  
  prefixo_base = "SF0/"
  
  variable_codes = c("REVENUE_MRY",
                     "INVENTORY_MRY",
                     "ASSETS_MRY",
                     "CAPEX_MRY",
                     "NETINC_MRY",
                     "GP_MRY",
                     "COR_MRY",
                     "TANGIBLES_MRY",
                     "EBT_MRY", 
                     "FCF_MRY",
                     "INTANGIBLES_MRY",
                     "NCFI_MRY",
                     "NCFF_MRY",
                     'NCFO_MRY',
                     "RND_MRY", 
                     "EBITDA_MRY")
  
  
  variable_names = c("Revenue",
                     "Inventory",
                     "Assets",
                     "Capex",
                     "NetIncome",
                     "GrossProfit",
                     "CostOfRevenue",
                     "TangibleAssets",
                     "EBT", 
                     "FreeCashFlow",
                     "IntangibleAssets",
                     "NetCashFlowFromInvestment",
                     "NetCashFlowFromFinancing",
                     'NetCashFlowFromOperations',
                     "ResearchAndDevelopmentExpenses", 
                     "EBITDA")
  
  
  variable_descriptions = c(
    "[Revenues]: Amount of Revenue recognized from goods sold, services rendered, insurance premiums, or other activities that constitute an earning process. Interest income for financial institutions is reported net of interest expense and provision for credit losses."
    ,"[Inventory]: A component of [ASSETS] representing the amount after valuation and reserves of inventory expected to be sold, or consumed within one year or operating cycle, if longer."
    ,"[Total Assets]: Sum of the carrying amounts as of the balance sheet date of all assets that are recognized. Major components are [CASHNEQ], [INVESTMENTS],[INTANGIBLES], [PPNENET],[TAXASSETS] and [RECEIVABLES]."
    ,"[Capital Expenditure]: A component of [NCFI] representing the net cash inflow (outflow) associated with the acquisition & disposal of long-lived, physical & intangible assets that are used in the normal conduct of business to produce goods and services and are not intended for resale. Includes cash inflows/outflows to pay for construction of self-constructed assets & software."
    ,"[Net Income]: The portion of profit or loss for the period, net of income taxes, which is attributable to the parent after the deduction of [NETINCNCI] from [CONSOLINC], and before the deduction of [PREFDIVIS]."
    ,"[Gross Profit]: Aggregate revenue [REVENUE] less cost of revenue [COR] directly attributable to the revenue generation activity."
    ,"[Cost of Revenue]: The aggregate cost of goods produced and sold and services rendered during the reporting period."
    ,"[Tangible Asset Value]: The value of tangibles assets calculated as the difference between [ASSETS] and [INTANGIBLES]."
    ,"[Earnings before Tax]: Earnings Before Tax is calculated by adding [TAXEXP] back to [NETINC]."
    ,"[Free Cash Flow]: Free Cash Flow is a measure of financial performance calculated as [NCFO] minus [CAPEX]."
    ,"[Goodwill and Intangible Assets]: A component of [ASSETS] representing the carrying amounts of all intangible assets and goodwill as of the balance sheet date, net of accumulated amortization and impairment charges."
    ,"[Net Cash Flow from Investing]: A component of [NCF] representing the amount of cash inflow (outflow) from investing activities, from continuing and discontinued operations. Principal components of investing cash flow are: capital (expenditure) disposal of equipment [CAPEX], business (acquisitions) disposition [NCFBUS] and investment (acquisition) disposal [NCFINV]."
    ,"[Net Cash Flow from Financing]: A component of [NCF] representing the amount of cash inflow (outflow) from financing activities, from continuing and discontinued operations. Principal components of financing cash flow are: issuance (purchase) of equity shares, issuance (repayment) of debt securities, and payment of dividends & other cash distributions."
    ,"[Net Cash Flow from Operations]: A component of [NCF] representing the amount of cash inflow (outflow) from operating activities, from continuing and discontinued operations."
    ,"[Research and Development Expense]: A component of [OPEX] representing the aggregate costs incurred in a planned search or critical investigation aimed at discovery of new knowledge with the hope that such knowledge will be useful in developing a new product or service."
    ,"[Earnings Before Interest, Taxes & Depreciation Amortization (EBITDA)]: EBITDA is a non-GAAP accounting metric that is widely used when assessing the performance of companies, calculated by adding [DEPAMOR] back to [EBIT]."
  )
  
  
  df_variaveis = data.frame(
    VariableCodes = variable_codes,
    VariableNames = variable_names,
    VariableDescriptions = variable_descriptions
  )
  
  sep = "_"
  
  queries = paste(prefixo_base, company_code,sep, variable_codes, sep = "")
  
  list_company = list()
  for (q in queries){
    message(paste("Queriying Quandl for variable", q))
    qnumber = which(queries == q)
    list_company[[variable_names[qnumber]]] = Quandl(q, collapse="annual", start_date="1900-01-01", type="ts") 
  }
  
  df_company = data.frame(time = as.vector(time(list_company[[1]])),
                          as.data.frame(list_company))
  
  ## Salvar Dados Coletados
  write.csv2(df_company, file = paste("./fundamentals-data/financial_data", company_code, ".csv", sep = ""))
  
  message(paste("Finalizada coleta de dados da empresa", company_code))
  
  ## Gerar List com descrição das variáveis e resultados
  list(Variaveis = df_variaveis, Dados = df_company)
}


#### Funções Auxiliares ####
# Funcao para formatar números para humanos.
# https://stackoverflow.com/questions/46657442/understanding-vectorisation
format_for_humans <- function(x, digits = 3){
  grouping <- pmax(floor(log(abs(x), 1000)), 0)
  paste(signif(x / (1000 ^ grouping), digits = digits), 
         c('', 'K', 'M', 'B', 'T')[grouping + 1],sep = " ")
}


transf_colunas_em_vetor = function(x) {as.vector(t(x))}


format_percentage_for_humans = function(x, digits = 1){
  paste(round(100 * x, digits), "%", sep = "")
}

format_currency_for_humans = function(x, currency = "$", digits = 4) {
  paste(currency, format_for_humans(x, digits = digits), sep = " ")
  }