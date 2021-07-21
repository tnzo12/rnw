# module files
source("run_options.R")
source("summary_tbl.R")
source("param_tbl.R")
source("terminal_mg.R")
source("reactable_setting.R")


# UI side =====================================================================
ui <- bs4Dash::dashboardPage(
  dark = TRUE,
  scrollToTop = TRUE,
  bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand("doge_nm_manager", image="https://i.imgflip.com/db5xf.jpg", opacity = 1),
    shiny::htmlOutput("dir_cur")
  ),
  controlbar = bs4Dash::dashboardControlbar(
    bs4Dash::sidebarUserPanel("doge_terminal_manager", image='https://i.pinimg.com/originals/57/48/78/574878084f4314776358d2e515dba613.png'),
    id = NULL,
    width = '450px',
    collapsed = TRUE,
    overlay = TRUE,
    terminal_ui("terminals")
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
    run_options_ui("method"),
    shiny::verbatimTextOutput('mod_selected')
  ),
  bs4Dash::dashboardBody(
    shiny::fluidRow(
      bs4Dash::box(
        title = "model summary",
        width=5,
        summary_tbl_ui("summary")
      ),
      bs4Dash::tabBox(
        width=7,
        type="tabs",
        selected = 'est',
        shiny::tabPanel("est", param_tbl_ui("params")),
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
    db <- xpose::xpose_data(file = paste0(dir(),'/',mod_selected(),".lst"), quiet=TRUE )
      xpose::update_themes(
        db,
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
    db <- xpose::xpose_data(file = paste0(dir(),'/',mod_selected(),".mod"), quiet=TRUE )
      xpose::update_themes(
        db,
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


  # run options
  run_options_server("method", dir, mod_selected)
  # summary table
  summary_tbl_server("summary", xpdb)
  # terminal manager
  terminal_server("terminals")
  # parameter table
  param_tbl_server("params", xpdb)



  # first tabs
  output$prm_dist <- renderPlot({
    ( xpose::as.xpose.plot(xpose::prm_distrib( xpdb() )) )
  }, bg="transparent")
  output$eta_dist <- renderPlot({
    ( xpose::as.xpose.plot(xpose::eta_distrib( xpdb() )) )
  }, bg="transparent")
  output$prm_qq <- renderPlot({
    ( xpose::as.xpose.plot(xpose::prm_qq( xpdb() )) )
  }, bg="transparent")
  output$eta_qq <- renderPlot({
    ( xpose::as.xpose.plot(xpose::eta_qq( xpdb() )) )
  }, bg="transparent")

  # second tabs
  output$dv_ipred <- renderPlot({
    ( xpose::as.xpose.plot(xpose::dv_vs_ipred( xpdb() )) )
  }, bg="transparent")
  output$dv_idv <- renderPlot({
    ( xpose::as.xpose.plot(xpose::dv_vs_idv( xpdb() )) )
  }, bg="transparent")
  output$res_idv <- renderPlot({
    ( xpose::as.xpose.plot(xpose::res_vs_idv( xpdb() )) )
  }, bg="transparent")
  output$ind_plots <- renderPlot({
    ( xpose::ind_plots(
      xpdb(), page = input$ind_page,
      ncol=1, nrow=6,
      color = c("grey60", "#FF6666", "#66CCCC"),
      line_linetype = c("blank", "solid", "twodash")
    ) )
  }, bg="transparent")
  output$vpc <- renderPlot({
    vpc_dat <- xpose::vpc_data(xpdb_mod(), psn_folder = paste0( dir(),'/',"vpc_",mod_selected() ) )
    vpc <- xpose::vpc(vpc_dat, area_fill = c("#66CCCC", "#FF6666", "#66CCCC"),
                      line_linetype = c("twodash", "solid", "twodash"))
    vpc
  }, bg="transparent")

  # third tabs
  output$prm_iter <- renderPlot({
    ( xpose::as.xpose.plot(xpose::prm_vs_iteration( xpdb() )) )
  }, bg="transparent")
  output$grd_iter <- renderPlot({
    ( xpose::as.xpose.plot(xpose::grd_vs_iteration( xpdb() )) )
  }, bg="transparent")


}

shiny::runGadget( app=shiny::shinyApp(ui=ui, server=server), viewer = shiny::paneViewer() )

