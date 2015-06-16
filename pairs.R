panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "#67000D", ...)
}


panel.smoothScatter <- function (x, y, bg = NA, 
                                 cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
  
  palette <- colorRampPalette(c("blue", "orange", "red"))
  s <- smoothScatter(x, y, colramp = palette, bg = bg, cex = cex, add=T)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)
}


data(iris)
myMatrix <- as.matrix(iris[,1:4])
pairs(myMatrix,
      lower.panel = panel.smoothScatter,
      upper.panel = panel.cor,
      diag.panel  = panel.hist)