# code for terminal manger
terminal_ui <- function(id){
  
  reactable::reactableOutput(NS(id, "terminals"))
  
}

terminal_server <- function(id){
  shiny::moduleServer(id, function(input, output, session) {
    
    terminal_ref <- shiny::reactivePoll(3000, session, function () Sys.time(), function () {
      name = NULL
      command = NULL
      buffer = NULL
      state = NULL
      num_ter <- rstudioapi::terminalList()
      
      for(i in 1:length(num_ter)){
        
        ter_message <- tail( rstudioapi::terminalBuffer(num_ter[i], stripAnsi = TRUE),15 )
        
        name[i]<- rstudioapi::terminalContext(num_ter[i])$caption
        command[i] <- rstudioapi::terminalContext(num_ter[i])$title
        
        buffer[i] <- if(any(grepl("OBJECTIVE VALUE", ter_message))==TRUE) {
          ter_message[grepl("OBJECTIVE VALUE", ter_message)]
        } else if ( !is.null(ter_message[length(ter_message)-1]) ) {
          ter_message[length(ter_message)-1]
        } else { NA }
        
        state[i] <- if( (rstudioapi::terminalBusy(num_ter[i]))==1 ) {'running'} else { 'idle' }
        
      }
      df <- data.frame(name,command,buffer,state)
      df <- dplyr::arrange(df, name)
      df
    })
    
    
    
    output$terminals <- reactable::renderReactable({
      df <- terminal_ref()
      
      reactable::reactable(
        style = list(
          fontSize = "12px",
          verticalAlign = "center",
          align = "left"
        ),
        df,
        theme = reactable_theme,
        rowStyle = function(index) {
          if ( df[index, 'state']=='running' ){ list(background = "rgba(255,215,0, 0.1)") } else if (df[index, 'state']=='idle'){ list(background = "rgba(0,255,0, 0.1)") }
        },
        columns = list(
          name = reactable::colDef(
            align = "center",
            minWidth = 50,
            style = htmlwidgets::JS("function(rowInfo, colInfo, state) {
      var firstSorted = state.sorted[0]
      if (!firstSorted || firstSorted.id === 'name') {
        var prevRow = state.pageRows[rowInfo.viewIndex - 1]
        if (prevRow && rowInfo.row['name'] === prevRow['name']) {
          return { visibility: 'hidden' }
        }
      }
    }")
          ),
          command = reactable::colDef(
            align = "center",
            style = htmlwidgets::JS("function(rowInfo, colInfo, state) {
      var firstSorted = state.sorted[0]
      if (!firstSorted || firstSorted.id === 'command') {
        var prevRow = state.pageRows[rowInfo.viewIndex - 1]
        if (prevRow && rowInfo.row['command'] === prevRow['command']) {
          return { visibility: 'hidden' }
        }
      }
    }")
          ),
          state  = reactable::colDef(
            align = "center",
            style = function(value) {
              color <- if (value == 'running') {
                "rgba(250, 176, 32, 1)"
              } else if ( value == 'idle' ) {
                "rgba(94, 241, 2, 1)"
              }
              list(fontWeight=300, color = color)
            }
            
          )
        ),
        
      )
      
    })
    
    return(terminal_ref)
    
  })
  
}