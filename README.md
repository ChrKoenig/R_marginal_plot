# R_marginal_plot

## Description
Plots a scatterplot with marginal probability density functions for x and y. 

Data may be grouped or ungrouped. For each group, a linear fit can be plotted. It is hidden by default, but can be shown by providing lm_show = TRUE. The model can be modified using the 'lm_formula' argument. The 'bw' and 'adjust' argument specify the granularity used for estimating probability density functions. See ?density for more information. For large datasets, opacity may be decreased by setting alpha to a value between 0 and 1. 

Additional graphical parameters are passed to the main plot, so you can customize axis labels, titles etc.

## Examples
```R
marginal_plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
```
![](/example_plots/plot1.png)

```R
marginal_plot(x = Sepal.Width, y = Sepal.Length, group = Species, data = iris, lm_show = T)
```
![](/example_plots/plot2.png)

```R
marginal_plot(x = Sepal.Width, y = Sepal.Length, group = Species, data = iris, bw = "nrd", xlab = "Sepal width", ylab = "Sepal length", pch = 15, cex = 0.5)
```
![](/example_plots/plot3.png)
