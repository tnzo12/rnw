# code for summary table

# list of the values for model selection
sum_table_value <- c("Run description",
                     'Reference model',
                     'Input data',
                     'Run start time',
                     'Estimation runtime',
                     'Estimation method',
                     'Number of individuals',
                     'Number of observations',
                     'Objective function value',
                     'Termination message')


summary_tbl_server <- function(id, lst_df, mod_code, mod_selected) {
  
  shiny::moduleServer(id, function(input, output, session) {

    summary_tbl <- shiny::reactive({
      lst_df <- lst_df()
      mod_code <- mod_code()
      mod_selected <- mod_selected()
      
      sum <- summarise_nm_model(file=paste0(dir(),'/',mod_selected(),".lst"), model=mod_code, software="nonmem", rounding=3)
      
      sum_table <- data.frame(sum)[,c("descr",'value')]

      sum_table <- sum_table[sum_table$descr %in% sum_table_value, ]
      colnames(sum_table) <- c("description", "value")
      
      reactable::reactable(
        sum_table,
        compact = TRUE,
        style = list(
          fontSize = "12px",
          verticalAlign = "center",
          align = "left"
        ),
        theme = reactable_theme,
        columns = list(
          description = reactable::colDef(
            
            style = function(value) {
              color <- "#777777"
              list(color = color)
            }
            
          ),
          value  = reactable::colDef(
            
            style = function(value) {
              color <- if ( grepl(".csv", value) == TRUE) {
                "#66CCCC"
              } else if ( grepl("SUCCESSFUL", value) == TRUE ) {
                "#66CC99"
              } else if ( grepl("TERMINATED", value) == TRUE ) {
                "#FF6666"
              }
              list(fontWeight=300, color = color)
            }
            
          )
        )
      )
      
    })
    
    
  })
}