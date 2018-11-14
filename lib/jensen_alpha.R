jensen_alpha <- function( portfolio_return, market_return, rf ){
  
  beta = cov(portfolio_return, market_return) / var(market_return )
  
  alpha = mean(portfolio_return - rf) - beta * mean(market_return - rf )
  
  return( alpha )
  
}
