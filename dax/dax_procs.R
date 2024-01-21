# function to compute (log or exact) return
# y is a vector of data
# type is either "log" or "exact", type of return computed
# h is the step size to compute cumulative returns
# e.g., h = 2 computes cumulative return over two periods

compute_return <- function(y, type = "log", 
                           h = 1){
  n <- length(y)
  y2 <- y[-(1:h)] # exclude first h observations
  y1 <- y[-(n:(n-h+1))] # exclude last h observations
  # compute h-step cumulative returns
  if (type == "log"){
    ret <- c(rep(NA, h), 100*(log(y2)-log(y1)))
  } else {
    ret <- c(rep(NA, h), 100*(y2-y1)/y1)
  }
  ret
}

# Function to make plot on page 21
quantile_comparison_plot <- function(pred_list,
                                     model_colors = NULL, 
                                     model_names = NULL, 
                                     font_size = 1.3, 
                                     lwd_line = 1.5, cex_pt = 1.3){
  
  n_models <- length(pred_list)
  x_pos <- seq(from = -.1, to = .1, length.out = n_models)
  if (is.null(model_colors)){
    model_colors <- c("#000000", "#E69F00", "#56B4E9", 
                      "#009E73", "#F0E442", "#0072B2", 
                      "#D55E00", "#CC79A7")
  }
  if (is.null(model_names)){
    model_names <- paste0("Model", 1:n_models)
  }
  
  rg_pl <- range(pred_list)
  plot(x = 1:5, y = rep(0, 5), bty = "n", xlab = "Horizon", 
       ylab = "Prediction", type = "n", ylim = rg_pl, 
       xlim = c(1+min(x_pos), 5+max(x_pos)), 
       cex.axis = font_size, cex.lab = font_size)
  
  for (jj in 1:n_models){
    col_tmp <- model_colors[jj]
    pred <- pred_list[[jj]]
    for (hh in 1:5){
      segments(x0 = hh + x_pos[jj], y0 = pred[1,hh], 
               y1 = pred[5,hh], col = col_tmp, lwd = lwd_line)
      points(x = rep(hh + x_pos[jj], 5), y = pred[,hh],
             col = col_tmp, pch = 20, cex = cex_pt)
    }
  }
  
  legend(1, max(rg_pl) + 2, model_names, bty = "n", col = model_colors, 
         pch = 20, horiz = TRUE, xpd = TRUE, cex = font_size)
  
}

qs <- function(x, y, tau){
  ((1-tau)*(x-y)*(x > y) + tau*(y-x)*(y >= x))
}

eval_helper <- function(pred_mat, y){
  sc <- qs(pred_mat[,1], y, .025) + 
    qs(pred_mat[,2], y, .25) + 
    qs(pred_mat[,3], y, .5) + 
    qs(pred_mat[,4], y, .75) + 
    qs(pred_mat[,5], y, .975)
  mean(sc)
}
