# R_marginal_plot

## Description
Function to plot xy-plot with marginal density functions

Data may be grouped or ungrouped. For each group, a linear fit is plotted. The model can be modified using the 'lm_formula' argument. Setting 'lm_formula' to NULL prevents plotting model fits. The 'bw' argument specifies the bandwidth rule used for estimating probability density functions. See ?density for more information. For large datasets, opacity may be decreased by setting alpha to a value between 0 and 1.

Additional graphical parameters are passed to the main plot, so you can customize axis labels, titles etc.

## Examples
```R
marginal_plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
```
![](/example_plots/plot1.png)

```R
marginal_plot(x = Sepal.Width, y = Sepal.Length, group = Species, data = iris)
```
![](/example_plots/plot2.png)

```R
marginal_plot(x = Sepal.Width, y = Sepal.Length, group = Species, data = iris, bw = "nrd", lm_formula = NULL, xlab = "Sepal width", ylab = "Sepal length", pch = 15, cex = 0.5)
```
![](/example_plots/plot3.png)
