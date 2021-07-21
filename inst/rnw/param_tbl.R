param_tbl_ui <- function(id){
  
  reactable::reactableOutput(NS(id, "params"))
  
}


param_tbl_server <- function(id, xpdb){
  
  moduleServer(id, function(input, output, session) {
    
    output$params <- reactable::renderReactable({
      xp_sum <- data.frame(xpdb()$summary)
      param_init <- data.frame(xpdb()$code)
      param_init <- param_init[param_init$subroutine=='the'|param_init$subroutine=='ome'|param_init$subroutine=='sig',"code"]
      param_init <- gsub(" ","",param_init) # remove space
      param_init <- gsub("[()]","",param_init) # remove brackets
      param_init <- strsplit(param_init, split=",")
      param_init <- lapply(
        
        param_init,
        function(x){
          if( length(x)==1 ){
            x <- c(NA,x,NA)
          }else{
            if( length(x)==2){
              x <- c(x,NA)
            }else{
              x
            }
          }
        }
        
      )
      param_init <- t(data.frame(param_init))
      param_init <- gsub("[^0-9.-]", "",param_init)
      rownames(param_init) <- NULL
      init <- param_init[,2]
      lower <- param_init[,1]
      upper <- param_init[,3]
      
      param_table <- data.frame(get_prm(xpdb(), transform=FALSE), init, lower,upper)
      param_table <- subset(param_table, select = -c(type,se,diagonal,m,n))
      
      param_table$fixed <- toupper(param_table$fixed)
      param_table$value <- as.numeric(as.character(param_table$value))
      param_table$init <- as.numeric(as.character(param_table$init))
      param_table$change <- (param_table$value - param_table$init)/param_table$init
      param_table$change[is.nan(param_table$change)] <- 0
      param_table$change[is.null(param_table$change)] <- 0
      param_table$change[is.na(param_table$change)] <- 0
      
      shrinkage <- unlist(c(strsplit(xp_sum[xp_sum$label=="etashk","value"], split = ","),
                            strsplit(xp_sum[xp_sum$label=="epsshk","value"], split = ",")))
      shrinkage <- gsub("\\[.*?\\]","", shrinkage)
      shrinkage <- gsub(" ","", shrinkage)
      
      param_table$shrinkage <- c(rep(NA,nrow(param_table)-length(shrinkage)), shrinkage)
      param_table$shrinkage[is.na(param_table$shrinkage)] <- "-"
      param_table$rse[is.na(param_table$rse)] <- " "
      param_table$rse <- as.numeric(as.character(param_table$rse))
      
      param_table$"value_( rse / shr )" <- paste0(param_table$value,
                                                  " ( ",round(param_table$rse*100,2),
                                                  " / ",param_table$shrinkage," )")
      param_table$'label_(init)' <- paste0(param_table$label," ( ",param_table$init," )")
      
      param_table_final <- param_table[,c("name","label_(init)","change","value_( rse / shr )")]
      #param_table_final <- param_table_final[,c(1,2,4,3)]
      reactable::reactable(
        data = param_table_final, compact = TRUE,
        rowStyle = function(index) {
          if ( param_table[index, 'fixed']=='TRUE' ){ list(background = "rgba(0, 0, 0, 0.15)") }
        },
        defaultColDef = reactable::colDef(
          align = "center",
          minWidth = 100
        ),
        theme = reactable_theme,
        columns = list(
          change = reactable::colDef(
            name = "change",
            defaultSortOrder = "desc",
            cell = function(value) {
              label <- paste0(round(value * 100), "%")
              bar_chart_pos_neg(label, value)
            },
            align = "center",
            minWidth = 100
          ),
          'label_(init)' = reactable::colDef(
            style = function(value) {
              list(color = "#B3B3B3")
            }
          ),
          name = reactable::colDef(
            
            style = function(value) {
              color <- if ( grepl("THETA", value) == TRUE) {
                "#66CCCC"
              } else if ( grepl("OMEGA", value) == TRUE ) {
                "#66CC99"
              } else {
                "#FF6666"
              }
              list(fontWeight=500, color = color)
            }
            
          )
        ),
        fullWidth = TRUE,
        style = list(
          fontSize = "12px",
          verticalAlign = "center",
          align = "left"
        ),
        pagination = FALSE
      )
      
    })
    
  })
  
}
