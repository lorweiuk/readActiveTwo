readActiveTwo <- function(con, channel_num, sample_num) {
  # read incoming data from active two system
  # con = socket connection to active-two port
  ### example: con <- socketConnection(host="localhost", port=778, server=FALSE, blocking=TRUE, open="rb")
  # channel_num = number of channels in each tcp package
  # sample_num = number of samples-per-channel in each package
  # returns one tcp package of data in matrix from active two, with rows = channels, cols = samples
  
  # read binary data: each 8-bit integer separately
  bin_in <- readBin(con    = con,
                    what   = integer(),
                    n      = 3*sample_num*channel_num,
                    size   = 1,
                    signed = FALSE)
  
  bin_in <- matrix(bin_in, ncol = 3, byrow = TRUE) # into matrix layout: 1st column = first integer, ...
  
  # combine three consecutive 8-bit integers
  int_in <- (bin_in[,3] * 65536) + (bin_in[,2] * 256) + bin_in[,1]
  
  # if msb is set: turn negative and reduce msb value
  msb_set <- int_in >= 8388608
  if (any(msb_set))
    int_in[msb_set] = -(int_in[msb_set] - 8388608)
  
  int_in = (int_in * 31.25) / 1000 # convert result to microVolt
  
  # store data in data_mat
  return( matrix(int_in, nrow = channel_num, ncol = sample_num) ) # return in matrix format
}
