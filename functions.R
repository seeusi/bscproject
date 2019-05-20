# Some functions to display messages in \code{shiny} reports

# Table message for exception
# Typically call \code{return(tab_exception(...))} where you would have called \code{stop(...)}
#' @examples
#' tab_exception("no data for current filter selection")
#' tab_exception("This doesn't work!")
tab_exception <-function(
  
  #' @param ... text to display, concatenated with sep
  ...){      
  txt = paste(...)
  df = data.frame("ISSUE" = txt)
  return(df)
  invisible(NULL)
}

# Plot message for exception
# Typically call \code{return(plot_exception(...))} where you would have called \code{stop(...)}
#' @examples
#' plot_exception("no data for current filter selection")
#' plot_exception("NO","WAY","!!!",color="blue",size=12,console=FALSE)
plot_exception <-function(
  
  #' @param ... text to display, concatenated with sep
  ...,
  
  #' @param sep separator used for concatenation
  sep=" ", 
  
  #' @param type function to use to print in console
  type=c("message","warning","cat","print"),
  
  #' @param color text color, by default red for message and warning else black
  color="auto",
  
  #' @param console if TRUE print in console, if FALSE just plot
  console=TRUE,
  
  #' @param size text size
  size = 6){      
  type=match.arg(type)
  txt = paste(...,collapse=sep)
  if(console){
    if(type == "message") message(txt)
    if(type == "warning") warning(txt)
    if(type == "cat") cat(txt)
    if(type == "print") print(txt)
  }
  if(color =="auto") color <- if(type == "cat") "black" else "red"
  if(txt == "warning") txt <- paste("warning:",txt)
  print(ggplot2::ggplot() +
          ggplot2::geom_text(ggplot2::aes(x=0,y=0,label=txt),color=color,size=size) + 
          ggplot2::theme_void())
  invisible(NULL)
}
