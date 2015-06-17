library(e1071)

#' The following function displays the skewness of column vectors of a data frame,
#' as bar- or density plot depending of the dimensions of th input data frame.
#' @author Jens Hooge jens.hooge@gmail.com

#' @title Skewness Plot.
#' 
#' @description \code{plotSkewness} plots the skewness of a distribution.
#' 
#' @details
#' \code{plotSkewness} expects a data frame X, and computes the skewness of its
#' column vectors. Skewness is a measure of asymmetry of a distribution. While a
#' skewness of 0 indicates a perfectly symmetrical distribution, larger skewness values
#' indicate that the density of values is higher for smaller values. In that case the
#' distribution would be called right skewed. Vice versa, for negative values of skewness
#' the distribution would be called left skewed. Skewness values between -2 and 2 indicate
#' a roughly symmetric distribution. If the number of columns in X is smaller or equal to 50,
#' skewness displayed in a barplot and a density plot otherwise.
#' 
#' @param X real valued data frame
#' 
#' @examples
#' plotSkewness(mtcars)
#' 
#' @return list{base}
plotSkewness <- function(X) {
  skewValues <- as.data.frame(sapply(X, skewness, na.rm=TRUE, type=1))
  skewValues$variable <- rownames(skewValues)
  rownames(skewValues) <- NULL
  colnames(skewValues) <- c("skewnessValue", "variable")
  skewValues$variable <- as.factor(skewValues$variable)
  
  if (ncol(X) <= 50){
    fig <- ggplot(skewValues, aes(x=variable, y=skewnessValue)) +
      geom_bar(stat="identity", position="dodge") +
      geom_hline(yintercept = 2, linetype = "dashed", color="red") +
      geom_hline(yintercept = -2, linetype = "dashed", color="red") +
      ylab("Skewness") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  } else {
    fig <- ggplot(skewValues, aes(x=skewnessValue)) +
      geom_density() +
      geom_point(aes(x=skewnessValue, y = 0.0005),
                 alpha = 0.25, size=4) +
      geom_vline(xintercept = 2, linetype = "dashed", color="red") +
      geom_vline(xintercept = -2, linetype = "dashed", color="red") +
      annotate("text", x = Inf, y = Inf, label = sprintf("n=%i", ncol(X)),
               vjust=1.8, hjust=1.2) +
      xlab("Skewness") +
      theme_bw()
  }
  return(fig)
}
