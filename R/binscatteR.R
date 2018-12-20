
#' Makes a bin scatter plot
#'
#'
#'
#' @param data data
#' @param x the x variable
#' @param y the y variable
#' @param weights optional weights
#' @param numbins number of bins. defaults to 20
#' @param xlab x label for plot. defaults to name of x
#' @param ylab y label for plot. defaults to name of y
#' @param title title for plot. defaults to binned scatter of y on x
#' @param se logical for whether to plot standard error of mean estimate. defaults to false
#' @param smooth logical for whether to plot smoother. defaults to false
#'
#' @return binned_df (the binned data) and plot_out (the plot)
#'
#' @export
binscatter <- function(data = NULL, x = NULL, y = NULL, weights = NULL,
                       numbins = 20, xlab = NULL, ylab = NULL, title = NULL,
                       se = FALSE, smooth = FALSE){

  #packages
  require(lazyeval)
  require(ggplot2)
  require(dplyr)
  require(Hmisc)
  require(tidyverse)

  #check inputs
  if (is.null(data)) {
    stop("Please provide a dataframe.")
  }
  if (is.null(x)) {
    stop("Please provide an x variable.")
  }
  if (is.null(y)) {
    stop("Please provide a y variable.")
  }
  if (!is_character(x)) {
    stop("X variable must be provided as a string.")
  }
  if (!is_character(y)) {
    stop("Y variable must be provided as a string.")
  }
  if (!is.null(weights) & !is_character(weights)) {
    stop("Weight variable must be provided as a string.")
  }

  #convert data to dataframe
  data <- as.data.frame(data)

  #check if x is continuous
  if(length(unique(data[,x]))/length(data[,x]) >= 0.8){
    is_continuous <- TRUE
  }else{
    is_continuous <- FALSE
  }



  #bin data if discrete
  if(!is_continuous){
    if(is.null(weights)) {
      binned_df <- data %>%
        group_by_(x) %>%
        dplyr::summarize_(meanoutcome = interp(~mean(var,na.rm = T),
                                               var = as.name(y)),
                          seoutcome = interp(~sd(var,na.rm = T),
                                             var = as.name(y)),
                          seoutcome = "seoutcome/n()",
                          lower_95 = "meanoutcome - 1.96*seoutcome",
                          upper_95 = "meanoutcome + 1.96*seoutcome",
                          sizebin = "n()") %>%
        dplyr::mutate_(x = interp(~var, var = as.name(x)))
    }
    else{
      binned_df <- data %>%
        group_by_(x) %>%
        dplyr::summarize_(meanoutcome = interp(~weighted.mean(var, w, na.rm = T),
                                               var = as.name(y), w = as.name(weights)),
                          seoutcome = interp(~sqrt(wtd.var(var, w, na.rm = T)),
                                             var = as.name(y), w = as.name(weights)),
                          seoutcome = "seoutcome/n()",
                          lower_95 = "meanoutcome - 1.96*seoutcome",
                          upper_95 = "meanoutcome + 1.96*seoutcome",
                          sizebin = "n()") %>%
        dplyr::mutate_(x = interp(~var, var = as.name(x)))
    }
  }else{ #if continuous
    if(is.null(weights)){
      binned_df <- data %>%
        mutate_(xbinned = interp(~cut(var, num), var = as.name(x), num = numbins)) %>%
        group_by(xbinned) %>%
        dplyr::summarize_(meanoutcome = interp(~mean(var, na.rm = T),
                                       var = as.name(y)),
                  seoutcome = interp(~sd(var, na.rm = T),
                                     var = as.name(y)),
                  seoutcome = "seoutcome/n()",
                  x = interp(~median(var,na.rm = T),
                                 var = as.name(x)),
                  lower_95 = "meanoutcome - 1.96*seoutcome",
                  upper_95 = "meanoutcome + 1.96*seoutcome",
                  sizebin = "n()")
    }
    else{
      binned_df <- data %>%
        mutate_(xbinned = interp(~cut(var, num), var = as.name(x), num = numbins)) %>%
        group_by(xbinned) %>%
        dplyr::summarize_(meanoutcome = interp(~weighted.mean(var, w, na.rm = T),
                                       var = as.name(y),w = as.name(weights)),
                  seoutcome = interp(~sqrt(weighted.var(var, w, na.rm = T)),
                                     var = as.name(y), w = as.name(weights)),
                  seoutcome = "seoutcome/n()",
                  x = interp(~median(var,na.rm = T),
                                 var = as.name(x)),
                  lower_95 = "meanoutcome - 1.96*seoutcome",
                  upper_95 = "meanoutcome + 1.96*seoutcome",
                  sizebin = "n()")
    }
  }

  #set axis labels
  if(is.null(xlab)){
    xlab <- x
  }

  if(is.null(ylab)){
    ylab <- y
  }

  #title of ggplot if none supplied
  if(is.null(title)){
    title <- paste("Binned scatter of ",ylab," on ",xlab,".",sep = "")
  }

  binned_df$outcomelabel <- ylab

  #add standard errors to the mean estimates
  if(se){
    p <- binned_df %>%
      ggplot(aes(x = x, y = meanoutcome,
                 ymin = lower_95, ymax = upper_95)) +
      geom_point(aes(size = sizebin),col = "IndianRed3", fill = "grey30", shape = 21) +
      geom_ribbon(alpha = 0.3) +
      theme_bw() +
      theme(legend.position = "none") +
      xlab(xlab) +
      ylab(ylab) +
      ggtitle(title)
  }else{
    p <- binned_df %>%
      ggplot(aes(x = x, y = meanoutcome)) +
      geom_point(aes(size = sizebin),col = "IndianRed3", fill = "grey30", shape = 21) +
      theme_bw() +
      theme(legend.position = "none") +
      xlab(xlab) +
      ylab(ylab) +
      ggtitle(title)
  }

  #add smoother
  if(smooth){
    p <- p + geom_smooth(aes(weight = sizebin),se = F,
                         color = "DodgerBlue")
  }

  return(list(plot_out = p, binned_df = binned_df))
}
