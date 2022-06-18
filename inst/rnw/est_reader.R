est_reader <- function(x){
  # lines of lst files
  if( file.exists(x) ) {
    
    lst_lines <- readLines(x)
    
    # estimation result: ofv
    ofv <- signif(as.numeric(gsub(".*?([-+]?\\d*\\.?\\d+).*", "\\1", lst_lines[grep("OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT", lst_lines)]), 3)) # extract ofv
    ofv <- ifelse(is.null(ofv), NA, ofv)
    # estimation result: success, simulation, rounding error, termination
    s <- ifelse(
      any(grepl("MINIMIZATION SUCCESSFUL", lst_lines)), "s",
      ifelse(
        any(grepl("SIMULATION STEP PERFORMED", lst_lines)), "sim",
        ifelse(
          any(grepl("DUE TO ROUNDING ERRORS", lst_lines)), "r", "t"
        )
      )
    )
    # estimation result: boundary
    b <- ifelse(any(grepl("PARAMETER ESTIMATE IS NEAR ITS BOUNDARY", lst_lines)), "b", NA)
    # estimation result: singularity, covariance step success
    c <- ifelse(
      any(grepl("MATRIX ALGORITHMICALLY SINGULAR", lst_lines)), "m",
      ifelse(
        any(grepl("Elapsed covariance  time in seconds", lst_lines)), "c", NA
      )
    )
    
    c(ofv,s,b,c)
    
  } else {
    
    c(NA,NA,NA,NA) 
    
  }
   
}

#"OBJECTIVE FUNCTION VALUE WITHOUT CONSTANT" in lst file = ofv
#"PARAMETER ESTIMATE IS NEAR ITS BOUNDARY" in lst file = boundary. No boundary= successful covariance step
#"MATRIX ALGORITHMICALLY SINGULAR" in lst file = matrix singularity
#"DUE TO ROUNDING ERRORS" in lst file = rounding error
#"MINIMIZATION SUCCESSFUL" in lst file = minimization successful