## Bottom detection =====
# works on Sv matrix or echogram

seabedDetector <- 
function(X, Y = NULL, det.lvl = -35, near.fld = 3, Sv.meanF = FALSE, 
         shapeK = c(3, 3), smooth = TRUE, offs = NULL, plot = TRUE){
  flip.matrix <- function(x)t(x[nrow(x):1, ])
  if (inherits(X, "echogram")){
    Sv <- X$Sv
    R <- X$depth #  depth range
    out <- 1
  } else {
    Sv <- X
    if (missing(Y))
      stop("need also a depth range vector")
    R <- Y
    out <- 2
  }
  if (!inherits(Sv, "matrix"))
    stop("need object of class 'echogram' or an Sv matrix")
    
  # number of depth samples and pings
  nd <- nrow(Sv)
  np <- ncol(Sv)
    
  # clear near field
  idx.nf <- R <= near.fld
  Sv[idx.nf, ] <- NA
    
  # clear Sv below detection level
  Sv[Sv < det.lvl] <- NA  
    
  # apply mean filter to bottom Sv
  if (Sv.meanF == TRUE){
    require(mmand)
    Sv <- meanFilter(Sv, shapeKernel(shapeK))
  }
    
  # find index of max Sv value
  Sv.max.idx <- apply(Sv, 2, which.max) 
  # and corresponding bottom
  bot <- R[Sv.max.idx]
  
  # Add an offset (m) above the detected bottom
  if (!missing(offs))
    bot <- bot - offs
  
  # smooth detected bottom (review!)
  if (smooth == TRUE)
    bot <- smooth(bot)
  
  # Sv matrix index for plot
  bl <- rep(NA, length(bot))
  for (k in 1:np)
    bl[k] <- which.min(abs(R - bot[k]))
  bl <- length(R) - bl

  if (out == 1){
    X$pings$detBottom <- as.vector(bot) # review. Should be stored negative?
    ans <- X
  }
  if (out == 2){
    ans <- as.vector(bot)
  }
  if (plot == TRUE){
    dev.new()
    image(x=1:np, y=1:nd, t(Sv[nd:1, ]))
    lines(bl, col="red")
  }
  ans
}