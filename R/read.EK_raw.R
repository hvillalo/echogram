#' Read EK* raw files from Simrad echosounders
#' 
#' This function will read the whole Simrad EK* raw file as raw bytes.
#' 
#' @param file EK* raw file name
#'
#' @details This function is not intended to be called directly by the user, as  
#' it returns hex digits that should be first interpreted to be useful. Instead, 
#' \code{read.EK_raw} is called by other echogram functions that also convert 
#' the data to the appropriate type. 
#'
#' @return A vector of class ``raw''.
#' 
#' @author Héctor Villalobos   
#'
#'
#'
read.EK_raw <- function(file){
  fid <- file(file, open = "rb")
    fsize <- file.info(file)$size
    ekraw <- readBin(file, what = 'raw', n = fsize, size = 1, endian = "little")
  close(fid)
  ekraw
}