# R_marginal_plot
Function to plot xy-plot with marginal density functions

Data may be grouped or ungrouped. For each group, a linear fit is plotted. The model can be modified using the 'lm_formula' argument. Setting 'lm_formula' to NULL prevents plotting model fits. The 'bw' argument specifies the bandwidth rule used for estimating probability density functions. See ?density for more information. For large datasets, opacity may be decreased by setting alpha to a value between 0 and 1.

Additional graphical parameters are passed to the main plot, so you can customize axis labels, titles etc.
