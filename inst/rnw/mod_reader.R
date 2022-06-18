mod_reader <- function(x){
  # lines of lst files
  if( file.exists(x) ) {
    
    mod_lines <- readLines(x)
    
    base <- gsub(" ", "", sub(".*Based on:", "", mod_lines[grep("Based on:",mod_lines)]))
    des <- sub(".*Description:", "", mod_lines[grep("Description:",mod_lines)])
    lab <- sub(".*Label:", "", mod_lines[grep("Label:",mod_lines)])
    
    base <- ifelse(length(base) == 0, NA, base )
    des <- ifelse(length(des) == 0, NA, des )
    lab <- ifelse(length(lab) == 0, NA, lab )
    
    c(base, des, lab)
    
  } else {
    
    c(NA, NA, NA) 
    
  }
  
}

#"OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT" in lst file = ofv
#"PARAMETER ESTIMATE IS NEAR ITS BOUNDARY" in lst file = boundary. No boundary= successful covariance step
#"MATRIX ALGORITHMICALLY SINGULAR" in lst file = matrix singularity
#"DUE TO ROUNDING ERRORS" in lst file = rounding error
#"MINIMIZATION SUCCESSFUL" in lst file = minimization successful