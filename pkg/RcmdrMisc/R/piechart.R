piechart <- function(x, scale=c("percent", "frequency", "none"), 
                     col=rainbow_hcl(nlevels(x)), ...){
  scale <- match.arg(scale)
  if (!is.factor(x)) x <- as.factor(x)
  labels <- levels(x)
  tab <- table(x)
  labels <- if (scale == "percent") {
    tab <- 100*tab/sum(tab)
    paste0(labels, " (", round(tab), "%)")
  } else if (scale == "frequency") paste0(labels, " (", tab, ")")
    else labels
  pie(tab, labels=labels, col=col, ...)
}