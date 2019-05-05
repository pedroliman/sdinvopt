library(deSolve)

# Neste arquivo apenas ficará o modelo de dinâmica de sistemas.
# Definindo o Tempo da Simulação:
START<-1; FINISH<-150; STEP<-1; simtime <- seq(START, FINISH, by=STEP)

# Definindo variáveis iniciais:
auxs = c(a=1, b=2)

# Definindo Estoques Iniciais:
stocks = c(Stock1 = 1, Stock2 = 1)

# Modelo:
modelo = modelo <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    
    flow = a + b
    
    d_Stock1_dt = flow
    
    d_Stock2_dt = flow^2
    
    return (list(c(d_Stock1_dt, d_Stock2_dt),
                 flow = flow
                 ))   
  })
}

# Resultado da Simulação
result = deSolve::ode(y=stocks, simtime, func = modelo, 
                        parms=auxs, method="euler")
