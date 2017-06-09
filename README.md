# Oil Monte Carlo Simulation Example

This is a small example project of using a Monte Carlo simulation approach to quantify risk of an oil investment.

This simulation goes through 4 different types of risk:
* Dry Hole Risk
* Drilling Risk
* Production Risk
* Price Risk


Different methods were used in each one of these areas such as using Cholesky Decompositions for simulating correlated distributions, forecasting oil price for distributions to be used in future years from historic data, and kernal estimations of historical distributions.

The culmination was determining the VaR (Value at Risk), the CVaR (Conditional Value at Risk), and the NPV_15 (Net Preset Value at a 15 year time period).
All assumptions can be seen within the code.

An RShiny app was then prototyped to visualize some of the data as well as act as a simple interface for inputs / outputs.
