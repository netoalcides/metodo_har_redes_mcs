straddle_strategy_applied <- function( variation_expected, threshould ){
  
  ifelse( variation_expected > threshould, 'buy',
          ifelse( variation_expected < -threshould, 'sell', 'nothing' ) )
  
}
