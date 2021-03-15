#' Read raw files from EK* scientific echosounders 
#' 
#' This function will read the whole file in raw format. You must know what 
#' data is inside and how it is organised in order to use it.
#' 
#' @param file EK* raw file name
#'
#' @return raw data 
#'
#' @examples
#'  \dontrun{
#' ekraw <- read.EK_raw("D20140216-T203102.raw")
#' }
#'
#'
read.EK_raw <- function(file){
  fid <- file(file, open = "rb")
    fsize <- file.info(file)$size
    ekraw <- readBin(file, what = 'raw', n = fsize, size = 1, endian = "little")
  close(fid)
  ekraw
}