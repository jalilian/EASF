
# Convert longitude and latitude coordinates in different formats 
#       to decimal degrees and adjusting for direction
# Abdollah Jalilian
# 02/12/2024

convert_coords <- function(lon, lat) 
{
  if (length(lon) != length(lat))
    stop("input vectors must have the same length")
  conv2dec <- function(x)
  {
    # numeric output vector
    out <- rep(NA_real_, length(x))
    # there is at least one numeric charcter 
    ok <- grepl("[0-9]", x)
    # return NAs if no valid entries exist
    if (!any(ok)) 
      return(out)
    
    # replace multiple spaces with a single space
    x1 <- gsub("\\s+", " ", x[ok])
    # replace the ring above character ˚ (unicode U+02DA) with
    #       the degree symbol character ° (unicode U+00B0)
    x1 <- gsub("\u02DA", "\u00B0", x1)
    # replacing the prime character (unicode U+2032) or 
    #       the combined horn or comma above character (unicode U+031B) or
    #       the combining comma above (unicode U+0315) with 
    #       the single straight quote or apostrophe character ' (unicode U+0027) 
    x1 <- gsub("\u031B|\u2032|\u0315", "'", x1)
    # replace the double prime character (unicode U+2033) with
    #       the  straight double quote character " (unicode U+0022)
    x1 <- gsub("\u2033", "\u0022", x1)
    # if there is two dots, replace the first with °
    x1 <- ifelse(sapply(gregexpr("\\.", x1), 
                        function(pos) sum(pos > 0) == 2), 
                 sub("\\.", "\u00B0", x1), x1)
    
    # only keep signs, numeric, degree/minute/second symbols and space
    x1 <- gsub("[^-+0-9.°˚'\" ]", "", x1)
    # split into components based on degree/minute/second/space characters
    x_sp <- strsplit(x1, "°|˚|'|\"| ")
    # remove empty components and convert remaining components to numeric
    x_sp <- lapply(x_sp, function(o) as.numeric(o[o != ""]))

    # components are treated as degree, minute and seconds
    #  thus no more than three components
    if (any(lapply(x_sp, length) > 3))
      stop(paste(x[which(lapply(x_sp, length) > 3)], "can not be converted"))
    
    # calculate the decimal degree value from existing components
    out[ok] <- sapply(x_sp, function(o) sum(o / 60^seq(0, length(o) - 1)))
    return(out)
  }
  
  # direction for longitude (W/w for negative, others are positive)
  lon_direction <- ifelse(grepl("W", lon, ignore.case=TRUE), -1, 1)
  # direction for latitude (S/s for negative, others are positive)
  lat_direction <- ifelse(grepl("S", lat, ignore.case=TRUE), -1, 1)
  
  # convert longitude to decimal degrees and adjusting for direction
  lon_dec <- conv2dec(lon) * lon_direction
  # convert latitude to decimal degrees and adjusting for direction
  lat_dec <- conv2dec(lat) * lat_direction
  
  # return converted longitude and latitude
  if (length(lon) == 1)
    return(c(lon_dec, lat_dec))
  else
    return(cbind(lon_dec, lat_dec))
}

if (FALSE)
{
  # Examples
  convert_coords("-3.1070570000000002",  "6.1937410000000002")
  convert_coords("-3.1070570000000002W",  "6.1937410000000002S")
  convert_coords("0.38577° W", "5.71441° N")
  convert_coords("002°54.343'",  "6°10.301'")
  convert_coords("002°5'4.343\"",  "6°1'0.301\"")
  convert_coords("002 5 4.343",  "6 1 0.301")
  convert_coords(c("12.1", "-5.3W", "01°54.343'", "1 2 3"),
                 c("-3S", "02°1.3", "0 1 0", "5"))
}

