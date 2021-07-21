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


summary_tbl_ui <- function(id) {
  
  reactable::reactableOutput(NS(id,"summary"))
}

summary_tbl_server <- function(id, xpdb) {
  
  moduleServer(id, function(input, output, session) {
    
    output$summary <- reactable::renderReactable({
      sum_table <- data.frame(xpdb()$summary)[,c("descr",'value')]
      sum_table <- sum_table[sum_table$descr %in% sum_table_value, ]
      colnames(sum_table) <- c("description", "value")
      reactable::reactable(
        sum_table, compact = TRUE,
        style = list(
          fontSize = "12px",
          verticalAlign = "center",
          align = "left"
        ),
        theme = reactable_theme,
        columns = list(
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
  