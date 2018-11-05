straddle_strategy_returns <- function( p_call0, p_put0, 
                                       p_call1, p_put1, 
                                       rf, trading_costs,
                                       strategy ){
  
  if( any( strategy != c() ) ) { stop("use only buy, sell or nothing entries") }
  
  ifelse( strategy == 'buy', (((p_call1+p_put1) - (p_call0+p_put0)) / (p_call0+p_put0)) - (2 *  trading_costs),
          ifelse( strategy == 'sell',  (((p_call0+p_put0) - (p_call1+p_put1)) / (p_call1+p_put1)) - (2 *  trading_costs),
                  rf ) )
  
}
