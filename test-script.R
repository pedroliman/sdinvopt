library(deSolve)

# Neste arquivo apenas ficará o modelo de dinâmica de sistemas.
# Definindo o Tempo da Simulação:
START<-1; FINISH<-365; STEP<-1; simtime <- seq(START, FINISH, by=STEP)

skus = 10

z = 0.5

deliveries = matrix(rep(0, skus*length(simtime)*2),nrow = length(simtime)*2, ncol = skus)

# Main Decision Variable:

safety_stock_perc_change = rep(0, skus)

deliveries_per_period = rep(1/30, skus)

# Fake Data:
skus_cost = rep(10, skus)

avg_demand = rep(50, skus)

sd_demand = rep(50, skus)

avg_leadtime = rep(10, skus)

sd_leadtime = rep(1, skus)

sd_dtr = avg_leadtime*(sd_demand^2)+(avg_demand^2)*(sd_leadtime^2)

standard_safety_stock = z*sqrt(sd_dtr)

safety_stock = standard_safety_stock*(1+safety_stock_perc_change)

avg_dtr = avg_demand*avg_leadtime

reorder_level = avg_dtr+safety_stock

qty_reorder = round(avg_demand / deliveries_per_period) 

avg_inventory_level = qty_reorder/2 + safety_stock

holding_inventory_cost = avg_inventory_level * skus_cost

total_holding_cost = sum(holding_inventory_cost)

# Definindo variáveis iniciais:
auxs = list(reorder_level = reorder_level,
            qty_reorder = qty_reorder,
            avg_demand = avg_demand, 
            sd_demand = sd_demand,
            avg_leadtime = avg_leadtime,
            sd_leadtime = sd_leadtime)

# Definindo Estoques Iniciais:
stocks = c(OnHandInventory = unname(reorder_level - 200) ,
           OnOrder = rep(0,skus),
           TotalBackOrders = rep(0,skus),
           Shipped = rep(0,skus),
           TotalDemand = rep(0.1,skus),
           TotalLateSales = rep(0,skus))

# Modelo:
modelo = modelo <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    
    # Vetorizando os Estoques:
    OnHandInventory = stocks[grep("OnHandInventory", x = names(stocks))]
    OnOrder = stocks[grep("OnOrder", x = names(stocks))]
    TotalBackOrders = stocks[grep("TotalBackOrders", x = names(stocks))]
    Shipped = stocks[grep("Shipped", x = names(stocks))]
    TotalDemand = stocks[grep("TotalDemand", x = names(stocks))]
    TotalLateSales = stocks[grep("TotalLateSales", x = names(stocks))]

    
    ordering = ifelse(OnHandInventory+OnOrder-TotalBackOrders<=reorder_level, qty_reorder,0)
    
    lead_time = round(rnorm(n = avg_leadtime, mean = avg_leadtime, sd = sd_leadtime))
    
    # Setting Delivery for the Future:
    
    update_deliveries = function(sku, deliveries, ordering, lead_time, time) {
      deliveries[time+lead_time[sku],sku] <<- deliveries[time+lead_time[sku],sku] + ordering[sku]
    }
    
    for (sku in 1:skus) {
      update_deliveries(sku = sku, deliveries, ordering, lead_time, time)
    }
    
    receiving = deliveries[time,]
    
    daily_demand_qty = pmax(rnorm(n = avg_demand, mean = avg_demand, sd = sd_demand), 0)
    
    shipping = pmin(OnHandInventory,daily_demand_qty+TotalBackOrders)
    
    backorders = pmax(daily_demand_qty-shipping,0)
    
    latesales = pmax(pmin(TotalBackOrders,shipping-daily_demand_qty),0)
    
    fill_rate = (1-(TotalLateSales/TotalDemand))*100
    
    d_OnHandInventory = receiving - shipping
    d_OnOrder = ordering - receiving
    d_TotalBackOrders = backorders - latesales
    d_Shipped = shipping
    d_TotalDemand = daily_demand_qty
    d_TotalLateSales = latesales
    
    return (list(c(d_OnHandInventory, d_OnOrder, d_TotalBackOrders, d_Shipped, d_TotalDemand, d_TotalLateSales),
                 fill_rate = fill_rate
                 ))   
  })
}

# Resultado da Simulação
result = deSolve::ode(y=stocks, simtime, func = modelo, 
                        parms=auxs, method="euler")
