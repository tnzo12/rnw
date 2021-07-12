options(reactable.theme = reactable::reactableTheme(
  backgroundColor = "transparent",
  inputStyle = list(backgroundColor = "transparent"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))

bar_chart_pos_neg <- function(label, value, max_value = 1, height = "16px",
                              pos_fill = "#FF6666", neg_fill = "#66CCCC") {
  neg_chart <- htmltools::div(style = list(flex = "1 1 0"))
  pos_chart <- htmltools::div(style = list(flex = "1 1 0"))
  width <- paste0(abs(value / max_value) * 100, "%")

  if (value < 0) {
    bar <- htmltools::div(style = list(marginLeft = "8px", background = neg_fill, width = width, height = height))
    chart <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"), label, bar)
    neg_chart <- tagAppendChild(neg_chart, chart)
  } else {
    bar <- htmltools::div(style = list(marginRight = "8px", background = pos_fill, width = width, height = height))
    chart <- htmltools::div(style = list(display = "flex", alignItems = "center"), bar, label)
    pos_chart <- tagAppendChild(pos_chart, chart)
  }

  htmltools::div(style = list(display = "flex"), neg_chart, pos_chart)
}

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

reactable_theme <- reactable::reactableTheme(
  backgroundColor = 'transparent',
  borderColor = 'rgba(102,102,102,0.15)',
  borderWidth = '1px'
)

# UI side =====================================================================
ui <- bs4Dash::dashboardPage(
  dark = TRUE,
  scrollToTop = TRUE,
  bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand("doge_nm_manager", image="https://i.imgflip.com/db5xf.jpg", opacity = 1),
    shiny::htmlOutput("dir_cur")
  ),
  controlbar = bs4Dash::dashboardControlbar(
    bs4Dash::sidebarUserPanel("doge_terminal_manager", image='http://i.imgur.com/RVXMPov.png'),
    id = NULL,
    width = '450px',
    collapsed = TRUE,
    overlay = TRUE,
    reactable::reactableOutput("terminals")
  ),
  bs4Dash::dashboardSidebar(
    bs4Dash::sidebarUserPanel("CNU_PM", image="https://res-3.cloudinary.com/crunchbase-production/image/upload/c_lpad,h_170,w_170,f_auto,b_white,q_auto:eco/oathengskxv2qsas7epw"),
    shinyFiles::shinyDirButton(
      id="dir",
      label="choose directory",
      title="",
      viewtype = "icon"
    ),
    reactable::reactableOutput("files_cur"),
    shiny::br(),
    shinyWidgets::pickerInput(
      inputId = "method",
      label = "run options",
      choices = list(

        execute = c("execute"),
        model_diagnostics = c("vpc","npc","bootstrap","cdd","llp","sir","ebe_npde"),
        design_evaluation = c("sse"),
        covariates = c("scm","xv_scm","boot_scm","lasso"),
        misc = c("nca","nonpb","mimp","gls","parallel_retries","precond","psn_clean","update_inits")

      )
    ),
    shiny::textAreaInput("cmd", label="command input", resize="vertical", rows=5),

    shiny::actionButton("run", "run selected model"),
    shiny::verbatimTextOutput('mod_selected')




  ),
  bs4Dash::dashboardBody(
    # Boxes need to be put in a row (or column)
    shiny::fluidRow(
      bs4Dash::box(
        title = "model summary",
        width=5,
        reactable::reactableOutput("summary")
      ),
      bs4Dash::tabBox(
        width=7,
        type="tabs",
        selected = 'est',
        shiny::tabPanel("est", reactable::reactableOutput("params")),
        shiny::tabPanel("prm_dist.", shiny::plotOutput('prm_dist')),
        shiny::tabPanel("q-q prm", shiny::plotOutput('prm_qq')),
        shiny::tabPanel("η_dist.", shiny::plotOutput('eta_dist')),
        shiny::tabPanel("q-q eta", shiny::plotOutput('eta_qq'))
      ),
      bs4Dash::tabBox(
        width=12,
        type="tabs",
        selected = "dv-ipred",
        shiny::tabPanel("dv-ipred", shiny::plotOutput('dv_ipred')),
        shiny::tabPanel("dv-idv", shiny::plotOutput('dv_idv')),
        shiny::tabPanel("cwres-idv", shiny::plotOutput('res_idv')),
        shiny::tabPanel("individual_plots", shiny::plotOutput('ind_plots', height = '1200px'), shiny::numericInput('ind_page', "go to page", value=1)),
        shiny::tabPanel("vpc", shiny::plotOutput('vpc'))
      ),
      bs4Dash::tabBox(
        width = 12,
        type="tabs",
        shiny::tabPanel("param_iter", shiny::plotOutput('prm_iter')),
        shiny::tabPanel("gradient_iter", shiny::plotOutput('grd_iter'))
      )
    )
  )
)

# Server side =================================================================
server <- function(input, output, session) {

  # Shiny files widget
  shinyFiles::shinyDirChoose(input=input,
                 id='dir',
                 roots = c(home='~'))

  # Directory management
  dir <- shiny::reactive(shinyFiles::parseDirPath(roots = c(home='~'), input$dir))
  mod_files <- shiny::reactive({ list.files(dir(), pattern='.mod', all.files=FALSE, full.names=FALSE, recursive=FALSE) })
  lst_files <- shiny::reactive({ list.files(dir(), pattern='.lst', all.files=FALSE, full.names=FALSE, recursive=FALSE) })

  mod_selected <- shiny::reactive({ gsub('.mod',"" , mod_files()[input$selected]) }) # getting rid of the selected model file's extension


  xpdb <- shiny::reactive({
    xpose::xpose_data(file = paste0(dir(),'/',mod_selected(),".lst"), quiet=TRUE ) %>%
      xpose::update_themes(
        gg_theme = ggplot2::theme(legend.position='bottom',
                         plot.background = ggplot2::element_blank(),
                         legend.background = ggplot2::element_blank(),
                         panel.background = ggplot2::element_blank(),
                         panel.grid.major = ggplot2::element_line(colour='grey60', size=0.1),
                         panel.grid.minor = ggplot2::element_line(colour='grey60', size=0.1),
                         text = ggplot2::element_text(colour='grey60'),
                         axis.text = ggplot2::element_text(colour='grey60'),
                         axis.ticks = ggplot2::element_line(colour='grey60', size=0.1),
                         strip.background = ggplot2::element_blank(),
                         strip.text = ggplot2::element_text(colour = 'grey60')
        ),
        xp_theme = list(point_color = '#999999', point_alpha = 0.7,
                        line_color  = '#999999', line_alpha = 0.7,
                        smooth_color = "#FF6666")
      )
  })
  xpdb_mod <- shiny::reactive({
    xpose::xpose_data(file = paste0(dir(),'/',mod_selected(),".mod"), quiet=TRUE ) %>%
      xpose::update_themes(
        gg_theme = ggplot2::theme(legend.position='bottom',
                         plot.background = ggplot2::element_blank(),
                         legend.background = ggplot2::element_blank(),
                         panel.background = ggplot2::element_blank(),
                         panel.grid.major = ggplot2::element_line(colour='grey60', size=0.1),
                         panel.grid.minor = ggplot2::element_line(colour='grey60', size=0.1),
                         text = ggplot2::element_text(colour='grey60'),
                         axis.text = ggplot2::element_text(colour='grey60'),
                         axis.ticks = ggplot2::element_line(colour='grey60', size=0.1),
                         strip.background = ggplot2::element_blank(),
                         strip.text = ggplot2::element_text(colour = 'grey60')
        ),
        xp_theme = list(point_color = '#999999', point_alpha = 0.4, area_alpha = 0.4,
                        line_color  = '#999999', line_alpha = 0.9,
                        smooth_color = "#FF6666")
      )
  })

  # Directory text output
  output$dir_cur <- shiny::renderText({
    mod_files()[input$selected]
  })
  # Directory prints only when there's any input
  shiny::observe({
    if(isTruthy(dir()))
      message(dir())
  })
  shiny::observe({
    updateTextInput(
      session, "cmd",
      label = NULL,
      value = switch(
        input$method ,
        "execute" = paste0("execute"," ",dir(),'/',mod_selected(),".mod"),
        "vpc" = paste0("vpc"," ","-samples=200"," ","-auto_bin=auto"," ","-dir=vpc_",mod_selected()," ",dir(),'/',mod_selected(),".mod"),
        "npc" = paste0("npc"," ","-samples=200"," ","-dir=npc_",mod_selected()," ",dir(),'/',mod_selected(),".mod"),
        "bootstrap" = paste0("bootstrap"," ","-samples=50"," ","-threads=4"," ","-dir=bs_",mod_selected()," ",dir(),'/',mod_selected(),".mod"),
        "cdd" = paste0("cdd"," ","-case_column=ID"," ","-bins=100"," ","-dir=cdd_",mod_selected()," ",dir(),'/',mod_selected(),".mod"),
        "llp" = paste0("llp"," ","-omegas=''"," ","--sigmas=''"," ","--thetas=''"," ","-dir=lasso_",mod_selected()," ",dir(),'/',mod_selected(),".mod"),
        "sir" = paste0("sir"," ","-samples=500"," ","-resamples=500"," ",dir(),'/',mod_selected(),".mod"),
        "ebe_npde" = paste0("ebe_npde"," ","-dir=ebe_",mod_selected()," ",dir(),'/',mod_selected(),".mod"),
        "sse" = paste0("sse"," ","-samples=500"," ","-dir=sse_",mod_selected()," ","-no-estimate-simulation"," ","-alt='run1.mod'"," ",dir(),'/',mod_selected(),".mod"),
        #"mcmp" = paste0(),# not yet implemented
        "scm" = paste0("scm"," ","-config_file=",mod_selected(),".scm"," ","-model="," ",dir(),'/',mod_selected(),".mod"),
        "xv_scm" = paste0("xv_scm"," ","-config_file="," ","-model="," ",dir(),'/',mod_selected(),".mod"),
        "boot_scm" = paste0("boot_scm"," ","-samples=100"," ","-threads=4"," ","-config_file="," ",dir(),'/',mod_selected(),".mod"),
        "lasso" = paste0("lasso"," ","-dir=lasso_",mod_selected()," ",dir(),'/',mod_selected(),".mod"),
        "nca" = paste0("nca"," ","-samples=500"," ","-columns=CL,V"," ",dir(),'/',mod_selected(),".mod"),
        "nonpb" = paste0("nonpb"," ",dir(),'/',mod_selected(),".mod"),
        "mimp" = paste0("mimp"," ","-dir=mimp_",mod_selected()," ",dir(),'/',mod_selected(),".mod"),
        "gls" = paste0("gls"," ","-dir=gls_",mod_selected()," ",dir(),'/',mod_selected(),".mod"),
        "parallel_retries" = paste0("parallel_retries"," ","-min_retries=4"," ","-threads=5"," ","-seed=12345"," ",dir(),'/',mod_selected(),".mod"),
        "precond" = paste0("precond"," ",dir(),'/',mod_selected(),".mod"),
        "psn_clean" = paste0("psn_clean"," ",dir(),'/',mod_selected(),".mod"),
        "update_inits" = paste0("update_inits"," ",dir(),'/',mod_selected(),".mod"," ","-out=",mod_selected(),".mod")
      )
    )
  })








  # show current files

  output$files_cur <- reactable::renderReactable({
    reactable::reactable(
      filterable = FALSE,
      highlight = TRUE,
      outlined = FALSE,
      compact = TRUE,
      selection = "single", onClick = 'select',
      selectionId = 'selected',
      data = data.frame(
        model_file = mod_files()
      ),
      columns = list(
        model_file = reactable::colDef(align = "left")
      ),
      theme = reactable_theme

    )

  })

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

  # Button action
  observeEvent(input$run,{
    rstudioapi::terminalExecute(input$cmd)
  })

  # first tabs
  output$prm_dist <- renderPlot({
    ( prm_distrib( xpdb() ) )
  }, bg="transparent")
  output$eta_dist <- renderPlot({
    ( eta_distrib( xpdb() ) )
  }, bg="transparent")
  output$prm_qq <- renderPlot({
    ( prm_qq( xpdb() ) )
  }, bg="transparent")
  output$eta_qq <- renderPlot({
    ( eta_qq( xpdb() ) )
  }, bg="transparent")

  # second tabs
  output$dv_ipred <- renderPlot({
    ( dv_vs_ipred( xpdb() ))
  }, bg="transparent")
  output$dv_idv <- renderPlot({
    ( dv_vs_idv( xpdb() ) )
  }, bg="transparent")
  output$res_idv <- renderPlot({
    ( res_vs_idv( xpdb() ) )
  }, bg="transparent")
  output$ind_plots <- renderPlot({
    ( ind_plots(
      xpdb(), page = input$ind_page,
      ncol=1, nrow=6,
      color = c("grey60", "#FF6666", "#66CCCC"),
      line_linetype = c("blank", "solid", "twodash")
    ) )
  }, bg="transparent")
  output$vpc <- renderPlot({
    xpdb_mod() %>%
      xpose::vpc_data(psn_folder = paste0( dir(),'/',"vpc_",mod_selected() ) ) %>%
      xpose::vpc(area_fill = c("#66CCCC", "#FF6666", "#66CCCC"),
          line_linetype = c("twodash", "solid", "twodash"))
  }, bg="transparent")

  # third tabs
  output$prm_iter <- renderPlot({
    ( prm_vs_iteration( xpdb() ) )
  }, bg="transparent")
  output$grd_iter <- renderPlot({
    ( grd_vs_iteration( xpdb() ) )
  }, bg="transparent")



  terminal_ref <- shiny::reactivePoll(2000, session, function () Sys.time(), function () {
    name = NULL
    command = NULL
    buffer = NULL
    state = NULL
    num_ter <- rstudioapi::terminalList()

    for(i in 1:length(num_ter)){

      ter_message <- tail( rstudioapi::terminalBuffer(num_ter[i], stripAnsi = TRUE),10 )

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

}

shiny::runGadget( app=shiny::shinyApp(ui=ui, server=server), viewer = shiny::paneViewer() )

