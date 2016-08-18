marginal_plot = function(x, y, group = NULL, data = NULL, lm_formula = y ~ x, alpha = 1, ...){
  require(scales)
  ###############
  # Plots a scatterplot with marginal probability density functions for x and y. 
  # Data may be grouped or ungrouped
  # additional par arguments are passed to the main plot, so you can customize axis labels, 
  ###############
  moreargs = eval(substitute(list(...)))

  # prepare consistent df
  if(missing(group)){
    if(missing(data)){
      if(length(x) != length(y)){stop("Length of arguments not equal")}
      data = data.frame(x = as.numeric(x), y = as.numeric(y))
    } else {
      data = data.frame(x = as.numeric(data[,deparse(substitute(x))]), 
                        y = as.numeric(data[,deparse(substitute(y))]))
    }
    if(sum(!complete.cases(data)) > 0){
      warning(sprintf("Removed %i rows with missing data", sum(!complete.cases(data))))
      data = data[complete.cases(data),]
    }
    group_colors = "black"
  } else {
    if(missing(data)){
      if(length(x) != length(y) | length(x) != length(group)){stop("Length of arguments not equal")}
      data = data.frame(x = as.numeric(x), y = as.numeric(y), group = as.factor(group))
    } else {
      data = data.frame(x = as.numeric(data[,deparse(substitute(x))]), 
                        y = as.numeric(data[,deparse(substitute(y))]),
                        group = as.factor(data[,deparse(substitute(group))]))
    }
    if(sum(!complete.cases(data)) > 0){
      warning(sprintf("Removed %i rows with missing data", sum(!complete.cases(data))))
      data = data[complete.cases(data),]
      }
    data = subset(data, group %in% names(which(table(data$group) > 5)))
    data$group = droplevels(data$group)
    group_colors = rainbow(length(unique(data$group)))
  } 

  # log-transform data (this is need for correct plotting of density functions)
  if(!is.null(moreargs$log)){
    if(!moreargs$log %in% c("y", "x", "yx", "xy")){
      warning("Ignoring invalid 'log' argument. Use 'y', 'x', 'yx' or 'xy.")
    } else {
      data = data[apply(data[unlist(strsplit(moreargs$log, ""))], 1, function(x) !any(x <= 0)), ]
      data[,unlist(strsplit(moreargs$log, ""))] = log10(data[,unlist(strsplit(moreargs$log, ""))])
    }
    moreargs$log = NULL # remove to prevent double logarithm when plotting
  }
  
  # remove unwanted argument(s) from moreargs
  if(!is.null(moreargs$col)){moreargs$col = NULL}
  
  # create testplot and retrieve axis limits for exact alignment
  testplot <- do.call(plot, c(list(x = quote(data$x), y = quote(data$y)), moreargs))
  plot_dims = par("usr")
  
  ifelse(!is.null(data$group), data_split <- split(data, data$group), data_split <- list(data))
  
  par(mar = c(0,5,1,1))
  layout(matrix(1:4, nrow = 2, byrow = T), widths = c(10,3), heights = c(3,10))

  # upper density plot
  plot(NULL, type = "n", xlim = plot_dims[1:2], ylab = "density",
       ylim = c(0, max(sapply(data_split, function(group_set) max(density(group_set$x, bw = "SJ")$y)))), main = NA, axes = F)
  axis(2, las = 1)
  mapply(function(group_set, group_color){lines(density(group_set$x, bw = "SJ"), col = group_color, lwd = 2)}, data_split, group_colors)
  
  # legend
  par(mar = c(0,0,0,0))
  plot.new()
  if(!missing(group)){legend("center", levels(data$group), fill = group_colors, border = group_colors, bty = "n")}
  
  # main plot
  par(mar = c(4,5,0,0))
  do.call(plot, c(list(x = quote(data$x), y = quote(data$y), col = quote(scales::alpha(group_colors[data$group], alpha))), moreargs))
  axis(3, labels = F, tck=.015)
  axis(4, labels = F, tck=.015)
  box()
  abline(v = 0, lty = 2)
  
  mapply(function(group_set, group_color){
    lm_tmp = lm(lm_formula, data = group_set)
    x_coords = seq(min(group_set$x), max(group_set$x), length.out = 100)
    y_coords = predict(lm_tmp, newdata = data.frame(x = x_coords))
    lines(x = x_coords, y = y_coords, col = group_color, lwd = 2.5)
  }, data_split, rgb(t(ceiling(col2rgb(group_colors)*0.8)), maxColorValue = 255))
  
  # right density plot
  par(mar = c(4,0,0,1))
  plot(NULL, type = "n", ylim = plot_dims[3:4], xlim = c(0, max(sapply(data_split, function(group_set) max(density(group_set$y, bw = "SJ")$y)))), main = NA, axes = F, xlab = "density")
  mapply(function(group_set, group_color){lines(x = density(group_set$y, bw = "SJ")$y, y = density(group_set$y, bw = "SJ")$x, col = group_color, lwd = 2)}, data_split, group_colors)
  axis(1)
}
