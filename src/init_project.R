## load initial parameters from project

# package
library(ProjectTemplate)

# fixed window parameters
sample_percentage = 0.8

# VaR analysis
alpha_ = 0.05

# Options trading analysis
t_maturity = 22 - 1
tau = 0.15 # threshould
trading_costs = 0.0025
# tau_sequence = seq( from = 0, to = 0.5, by = 0.05 )
# trading_costs_sequence = seq( from = 0, to = 0.025, by = 0.0025 )

# run project
load.project()
