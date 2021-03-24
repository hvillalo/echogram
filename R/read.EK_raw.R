#' Read EK* raw files from Simrad echosounders
#' 
#' This function imports Simrad EK60 or EK80 raw files as raw bytes.
#' 
#' @param file EK* raw file name.
#'
#' @details This function will return raw bytes as hex digits, therefore is not   
#' intended to be called directly by the user. Instead, \code{read.EK_raw} is 
#' called by other echogram's functions that also convert the various datagrams
#' in EK60 files to their appropriate type. 
#'
#' @return A vector of class ``raw''.
#' 
#' @seealso \code{read.EK60_raw}, \code{get_CON0}, \code{get_NME0}, and
#' \code{get_RAW0}.
#'
#' @author Héctor Villalobos. 
#'
read.EK_raw <- function(file){
  fid <- file(file, open = "rb")
    fsize <- file.info(file)$size
    ans <- readBin(file, what = 'raw', n = fsize, size = 1, endian = "little")
  close(fid)
  ans
}