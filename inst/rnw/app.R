# module files
source("run_options.R", local=TRUE)
source("summary_tbl.R", local=TRUE)
source("model_summary.R", local=TRUE)
source("param_tbl.R", local=TRUE)
source("terminal_mg.R", local=TRUE)
source("reactable_setting.R", local=TRUE)
source("est_reader.R", local=TRUE)
source("mod_reader.R", local=TRUE)
source("inspector.R", local=TRUE)
source("scatter_matrix.R", local=TRUE)

# UI side =====================================================================
ui <- bs4Dash::dashboardPage(
  fullscreen = TRUE,
  dark = ifelse(rstudioapi::getThemeInfo()$dark, # read theme of rstudio
         TRUE,
         FALSE),
  
  scrollToTop = TRUE,
  bs4Dash::dashboardHeader(
    title = bs4Dash::dashboardBrand(shiny::HTML("<i>LiLac - rnw</i>"), image="https://raw.githubusercontent.com/tnzo12/rnw/30778928d59837701f3a53ec431897751fa83d30/lilac.svg", opacity = 0.4,  href="https://lilac-co.kr/"),
    skin = ifelse(rstudioapi::getThemeInfo()$dark, # read theme of rstudio
                  'dark',
                  'light'),
    status = NULL,
    compact = TRUE,
    fixed = TRUE,
    shinyFiles::shinyDirButton(
      id="dir",
      label="directory",
      icon = shiny::icon("folder"),
      title="Choose directory",
      viewtype = "icon"
    ),
    htmltools::HTML("&nbsp;"),
    htmltools::HTML("&nbsp;"),
    shinyWidgets::dropdownButton( # color customizing
      splitLayout(cellWidths = c("33%","33%","33%"),
                  colourpicker::colourInput("col_point", "point", "#999999"),
                  colourpicker::colourInput("col_line", "line", "#999999"),
                  colourpicker::colourInput("col_smooth", "smooth", "#FF6666")
      ),
      shiny::br(),
      splitLayout(cellWidths = c("50%","50%"),
                  colourpicker::colourInput("col_area1", "vpc(50th),ipred", "#FF6666"),
                  colourpicker::colourInput("col_area2", "vpc(5,95th),pred", "#66CCCC"),
      ),
      shiny::br(),
      splitLayout(cellWidths = c("33%","33%","33%"),
                  colourpicker::colourInput("hist_fill", "histogram", "#66CCCC"),
                  colourpicker::colourInput("density_fill", "density", "#FF6666"),
                  colourpicker::colourInput("rug_color", "accent (rug)", "#FF6666")
      ),
      circle = FALSE, status = "danger",
      icon = shiny::icon("brush"), width = "300px",
      tooltip = shinyWidgets::tooltipOptions(title = "color options for plotting")
    )
  ),
  controlbar = bs4Dash::dashboardControlbar(
    skin = ifelse(rstudioapi::getThemeInfo()$dark, # read theme of rstudio
                  'dark',
                  'light'),
    bs4Dash::sidebarUserPanel("doge_terminal_manager", image='https://i.pinimg.com/originals/57/48/78/574878084f4314776358d2e515dba613.png'),
    id = NULL,
    width = '450px',
    collapsed = FALSE,
    overlay = TRUE,
    terminal_ui("terminals")
  ),
  bs4Dash::dashboardSidebar(
    skin = ifelse(rstudioapi::getThemeInfo()$dark, # read theme of rstudio
                  'dark',
                  'light'),
    bs4Dash::sidebarUserPanel("CNU_PM", image="https://res-3.cloudinary.com/crunchbase-production/image/upload/c_lpad,h_170,w_170,f_auto,b_white,q_auto:eco/oathengskxv2qsas7epw"),
    bs4Dash::sidebarMenu(
      bs4Dash::menuItem("non-comp_analysis",  tabName = "nca", icon = shiny::icon("chart-area")),
      bs4Dash::menuItem("comp_analysis", selected=TRUE, tabName = "ca", icon = shiny::icon("project-diagram")),
      bs4Dash::menuItem("edit_code (to be updated)", tabName = "coding", icon = shiny::icon("code")),
      run_options_ui("method")
    ),
    shiny::br(),
    shiny::verbatimTextOutput('mod_selected')
    
  ),
  bs4Dash::dashboardBody(
    bs4Dash::tabItems(
      
      
      bs4Dash::tabItem(
        tabName = "ca",
        shiny::fluidRow(
          bs4Dash::valueBoxOutput("tot_tm_running",width = 6),
          bs4Dash::valueBoxOutput("tot_tm_idle", width = 6),
        ),
        shiny::fluidRow(
          bs4Dash::sortable(
            width = 4,
            bs4Dash::box(
              title = "directory",
              width=12,
              elevation = 2,
              reactable::reactableOutput("files_cur_mod"), shiny::br(),
              shiny::HTML("<span style='color:grey'><i>
                              *designate folder with directory button in navbar, select a model to display
                              </i></span>")
            )  
          ),
          
          bs4Dash::sortable(
            width = 4,
            bs4Dash::box(
              title = "model summary",
              width=12,
              elevation = 2,
              reactable::reactableOutput("summary"), shiny::br(),
              shiny::HTML("<span style='color:grey'><i>
                              *summary of selected model in directory
                              </i></span>")
            )
          ),
          bs4Dash::sortable(
            width = 4,
            bs4Dash::tabBox(
              width=12,
              elevation = 2,
              type="pills",
              maximizable = TRUE,
              selected = 'est_theta',
              shiny::tabPanel("est_theta",
                              splitLayout(cellWidths = c("33%","33%","33%"),
                                          shiny::numericInput("prob", "problem", 1),
                                          shiny::numericInput("subprob", "sub-problem", 0),
                                          shiny::numericInput("digits", "digits", 3),
                              ),
                              param_theta_ui_view("param_theta_view"), shiny::br(),
                              shiny::HTML("<span style='color:grey'><i>
                              *estimates of thetas displayed. select problem/sub-problem (default is 1 and 0 each), adjust digits for significant figures. 
                              rse: relative standard error, iter: elapsed iterations
                              </i></span>")
                              ),
              shiny::tabPanel("prm_dist.",
                              shinyWidgets::checkboxGroupButtons(
                                inputId = "prm_dist_option",
                                justified = TRUE,
                                individual = TRUE,
                                width = '100%',
                                choices = c("hist" = "h", 
                                            "density" = "d",
                                            "rug" = "r"),
                                selected = c("h","r"),
                                checkIcon = list(
                                  yes = tags$i(class = "fa fa-circle", 
                                               style = "color: steelblue"),
                                  no = tags$i(class = "fa fa-circle-o", 
                                              style = "color: steelblue"))
                              ),
                              shiny::plotOutput('prm_dist'),
                              shiny::HTML("<span style='color:grey'><i>
                              *distribution plot for thetas. named parameter (V, CL, K12, ...) should be present in table output. 
                              check hist (histogram), density, rug to add graphical elements in the plot
                              </i></span>")
                              ),
              shiny::tabPanel("q-q prm", shiny::plotOutput('prm_qq'),
                              shiny::HTML("<span style='color:grey'><i>
                              *quantile-quantile plot for parameters. named parameters (V, CL, K12, ...) should be present in table output
                              </i></span>")
                              )
            )
          ),
          
          bs4Dash::sortable(
            width = 4,
            bs4Dash::tabBox(
              width=12,
              elevation = 2,
              type="pills",
              maximizable = TRUE,
              selected = 'est_etasig',
              shiny::tabPanel("est_etasig",
                              shinyWidgets::checkboxGroupButtons(
                                inputId = "add_option",
                                justified = TRUE,
                                individual = TRUE,
                                width = '100%',
                                choices = c("transform"="trans", 
                                            "off-diagonals"="off-diag"),
                                checkIcon = list(
                                  yes = tags$i(class = "fa fa-circle", 
                                               style = "color: steelblue"),
                                  no = tags$i(class = "fa fa-circle-o", 
                                              style = "color: steelblue"))
                              ),
                              param_etasig_ui_view("param_etasig_view"), shiny::br(),
                              shiny::HTML("<span style='color:grey'><i>
                              *estimates of etas displayed. check transform to get CV% of sqrt(exp(w^2)-1), check off-diagonals to show 'off-diagnoal' elements. 
                              rse: relative standard error, shr: shrinkage, iter: elapsed iterations
                              </i></span>")
                              ),
              shiny::tabPanel("eta_dist.",
                              shinyWidgets::checkboxGroupButtons(
                                inputId = "eta_dist_option",
                                justified = TRUE,
                                individual = TRUE,
                                width = '100%',
                                choices = c("hist" = "h", 
                                            "density" = "d",
                                            "rug" = "r"),
                                selected = c("h","r"),
                                checkIcon = list(
                                  yes = tags$i(class = "fa fa-circle", 
                                               style = "color: steelblue"),
                                  no = tags$i(class = "fa fa-circle-o", 
                                              style = "color: steelblue"))
                              ),
                              shiny::plotOutput('eta_dist'),
                              shiny::HTML("<span style='color:grey'><i>
                              *distribution plot for etas. ETA elements (ETA1, ETA2, ETA3, ...) should be present in table output. 
                              check hist (histogram), density, rug to add graphical elements in the plot
                              </i></span>")
                              ),
              shiny::tabPanel("q-q eta", shiny::plotOutput('eta_qq'),
                              shiny::HTML("<span style='color:grey'><i>
                              *quantile-quantile plot for etas. ETA elements (ETA1, ETA2, ETA3, ...) should be present in table output
                              </i></span>")
                              )
            )
          ),
          
          bs4Dash::sortable(
            width = 4,
            bs4Dash::tabBox(
              width=12,
              elevation = 2,
              type="pills",
              maximizable = TRUE,
              collapsed = TRUE,
              selected = "dv-ipred",
              shiny::tabPanel("dv-pred", plotly::plotlyOutput('dv_pred'),
                              shiny::HTML("<span style='color:grey'><i>
                              *observation vs population prediction. PRED should be present in the table output
                              </i></span>")
                              ),
              shiny::tabPanel("dv-ipred", plotly::plotlyOutput('dv_ipred'),
                              shiny::HTML("<span style='color:grey'><i>
                              *observation vs individual prediction. IPRED should be present in the table output
                              </i></span>")
                              ),
              shiny::tabPanel("iwres-pred", plotly::plotlyOutput('iwres_pred'),
                              shiny::HTML("<span style='color:grey'><i>
                              *individual weighted residuals vs population prediction. IWRES should be present in the table output
                              </i></span>")
                              
                              ),
              shiny::tabPanel("cwres-idv", plotly::plotlyOutput('cwres_idv'),
                              shiny::HTML("<span style='color:grey'><i>
                              *conditional weighted residuals vs independent varaible (time). CWRES should be present in the table output
                              </i></span>")
                              )
            )
          ),
          bs4Dash::sortable(
            width = 4,
            bs4Dash::tabBox(
              width=12,
              elevation = 2,
              type="pills",
              maximizable = TRUE,
              collapsed = TRUE,
              selected = "phi",
              shiny::tabPanel(title="phi",
                              shiny::uiOutput("phi_id_ex_ui"),
                              shiny::uiOutput("phi_var_ex_ui"),
                              plotly::plotlyOutput("phi"),
                              shiny::HTML("<span style='color:grey'><i>
                              *individual phi parameters phi(i) = mu(i) + eta(i), for (i)th parameter and their varaiances phc. 
                              objective function value is rescaled between -1 to 1 in this plot
                              </i></span>")
                              ),
              shiny::tabPanel(title="coi",
                              shiny::uiOutput("coi_x_ex_ui"),
                              shiny::uiOutput("coi_y_ex_ui"),
                              plotly::plotlyOutput("coi"),
                              shiny::HTML("<span style='color:grey'><i>
                              *full inverse covariance matrix (ficher information matrix) for thetats, sigmas, and omegas = inverse covariance matrix of estimate
                              </i></span>")
                              ),
              shiny::tabPanel(title="cor",
                              shiny::uiOutput("cor_x_ex_ui"),
                              shiny::uiOutput("cor_y_ex_ui"),
                              plotly::plotlyOutput("cor"),
                              shiny::HTML("<span style='color:grey'><i>
                              *full correlation matrix of thetas, sigmas, and omegas = correlation matrix of estimate
                              </i></span>")
                              ),
              shiny::tabPanel(title="cov",
                              shiny::uiOutput("cov_x_ex_ui"),
                              shiny::uiOutput("cov_y_ex_ui"),
                              plotly::plotlyOutput("cov"),
                              shiny::HTML("<span style='color:grey'><i>
                              *full variance-covariance error matrix of thetas, sigmas, and omegas = covariance matrix of estimates. 
                              elements that are fixed or not estimated are displayed as 0. only generated if the $COVARIANCE record is present
                              </i></span>")
                              )
            )
          ),
          
          bs4Dash::sortable(
            width = 4,
            bs4Dash::tabBox(
              width = 12,
              elevation =2,
              type = "pills",
              maximizable = TRUE,
              collapsed = TRUE,
              shiny::tabPanel("vpc",
                              shinyWidgets::checkboxGroupButtons(
                                inputId = "vpc_opt",
                                justified = TRUE,
                                individual = TRUE,
                                width = '100%',
                                choices = c("log x scale"="x", 
                                            "log y scale"="y"),
                                checkIcon = list(
                                  yes = tags$i(class = "fa fa-circle", 
                                               style = "color: steelblue"),
                                  no = tags$i(class = "fa fa-circle-o", 
                                              style = "color: steelblue"))
                              ),
                              plotly::plotlyOutput('vpc'),
                              shiny::HTML("<span style='color:grey'><i>
                              *vpc from the 'run options' on the left sidebar should be performed in prior. generated vpc folder is read
                              </i></span>")
                              ),
              shiny::tabPanel(
                title = "scatter_matrix",
                shiny::uiOutput("scat_mat_vars"),
                shiny::uiOutput("scat_mat_fact"),
                shiny::uiOutput("scat_mat_color"),
                shiny::actionButton("scat_button", label = "generate matrix"),
                shiny::selectInput("scat_method",
                                   label = "smooth method",
                                   choices = c("gam", "glm", "loess", "lm"),
                                   selected = "gam"),
                shiny::br(),shiny::br(),
                shiny::plotOutput("scat_mat_plot"),
                shiny::HTML("<span style='color:grey'><i>
                              *draw scatter matrix by selecting 'values to plot' after estimation.<br>
                              discontinuous value can be chosen after 'values to plot' is selected. colorizing should be done in the 'values as factor'
                              </i></span>")
                
              ) 
            )
          ),
          
          bs4Dash::sortable(
            width = 4,
            bs4Dash::tabBox(
              width = 12,
              elevation = 2,
              type = "pills",
              maximizable = TRUE,
              collapsed = TRUE,
              
              selected = "inspector",
              shiny::tabPanel(
                title="inspector",
                splitLayout(cellWidths = c("50%","50%"),
                            shiny::textInput("scatter_x", label = "x_var", value = "TIME"),
                            shiny::textInput("scatter_y", label = "y_var", value = "DV")
                ),
                plotly::plotlyOutput('scatter'),
                shiny::HTML("<span style='color:grey'><i>
                              *inspect data in a given model. includes imported xpose data of sdtab patab...
                              </i></span>")
              ),
              shiny::tabPanel("ind_plot",
                              splitLayout(cellWidths = c("33%","33%","33%"),
                                          shiny::numericInput('ind_cols', "columns", value=3, width="100%"),
                                          shiny::numericInput('ind_rows', "rows", value=8, width="100%"),
                                          shiny::numericInput('ind_page', "go to page", value=1)
                              ),
                              shiny::textInput('ind_group', "grouping", value=NA),
                              plotly::plotlyOutput('ind_plots', height = '1500px'),
                              shiny::HTML("<span style='color:grey'><i>
                              *individual plot, adjust each facet's size by columns/rows. grouping - compartment, covariates, or other grouping label is supported. currently pagination is not supported (in plotly)
                              </i></span>")
                              )
              
            )
          )
          ),
        shiny::actionButton("copy_theta", " copy est_theta"), 
        shiny::actionButton("copy_etasig", " copy est_etasig"), shiny::br(), shiny::br(), 
        shiny::HTML("<span style='color:grey'><i>
                              line distinction, window ratio of 1:1:1 (upper), window ratio of 1:1 (lower)
                              </i></span>"),
        shiny::HTML("<hr size='10px', style='color:#e0e0e0;border-style:solid'>"), # horizental line
        shiny::fluidRow(
          
          bs4Dash::sortable(
            width = 6,
            bs4Dash::box(
              width = 12,
              title = "export",
              shiny::fluidRow(
                bs4Dash::box(
                  title = 'choose data',
                  width = 6,
                  shinyWidgets::prettyCheckboxGroup(
                    inputId = "export_data",
                    label = NULL, 
                    selected = c("est_theta", "est_etasig", "dv-pred", "dv-ipred", "iwres-pred", "cwres-idv", "phi", "vpc", "ind_plot"),
                    choices = c("est_theta", "prm_dist", "q-q prm", "est_etasig", "eta_dist", "q-q eta", "dv-pred", "dv-ipred", "iwres-pred", "cwres-idv", "phi", "coi", "cor", "cov", "vpc", "scatter_matrix", "inspector", "ind_plot"),
                    icon = shiny::icon("check-square-o"), 
                    status = "warning",
                    outline = TRUE,
                    plain = TRUE,
                    animation = "smooth"
                  ),
                  shiny::HTML("<span style='color:grey'><i>
                              *check for the outputs to be exported
                              </i></span>")
                ),
                bs4Dash::box(
                  title = 'export as',
                  width = 6,
                  shinyWidgets::prettyRadioButtons(
                    inputId = "export_extension",
                    label = NULL,
                    shape = "square",
                    choices = c("html", "excel", "word"),
                    icon = shiny::icon("check-square-o"), 
                    status = "primary",
                    bigger = FALSE,
                    plain = TRUE,
                    outline = TRUE,
                    animation = "smooth"
                  ),
                  shiny::downloadButton("report", "report"), shiny::br(), shiny::br(),
                  shiny::HTML("<span style='color:grey'><i>
                              *currently, only html export is supported
                              </i></span>")
                )
              )
             
              
            )  
          ),
          bs4Dash::sortable(
            width = 6
          )
          
      
        )
        
      ),
      bs4Dash::tabItem(
        tabName = "nca",
        
        shiny::fluidRow(
          bs4Dash::box(
            title = "directory",
            width=12,
            elevation = 2,
            reactable::reactableOutput("files_cur_csv")
          ),
          bs4Dash::box(
            title = "processing options",
            width = 12,
            elevation  = 2,
            shiny::selectInput("adtype",
                               label = "admin_type",
                               choices = c("extravascular","iv-bolus","iv-infusion")),
            shiny::selectInput("ss_nca",
                               label = "steady-state",
                               choices = c("ns","ss")),
            splitLayout(cellWidths = c("33%","33%","33%"),
                        shiny::numericInput('dtime_nca', "dosing_time (ss only)", value=2, width="100%"),
                        shiny::numericInput('tau_nca', "dosing_interval (ss only)", value=2, width="100%"),
                        shiny::numericInput('tinf_nca', "time_infustion (iv-infusion only)", value=2, width="100%")
            )
          ),
          bs4Dash::box(
            title = "loaded table",
            width = 12,
            elevation  = 2,
            tags$head(tags$style(HTML(css))),
            rhandsontable::rHandsontableOutput("cur_csv")
          ),
          bs4Dash::box(
            title = "output (total)",
            width = 12,
            elevation  = 2,
            reactable::reactableOutput("nca_res")
          )
          
        )
        
      ),
      bs4Dash::tabItem(
        tabName = "coding",
        
        shiny::fluidRow(
          htmlOutput("frame")
        )
      )
      
      
    )
  )
)

# Server side =================================================================
server <- function(input, output, session) {
  
  # Reporting result
    output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Set up parameters to pass to Rmd document
      params <- list(
        mn = mod_selected(),
        s = summary_tbl(),
        p_t = if ("est_theta" %in% input$export_data) {param_tbl_theta()$theta} else {NA},
        p_e = if ("est_etasig" %in% input$export_data) {param_tbl_etasig()$etasig} else {NA},
        prm_dist = if ("prm_dist" %in% input$export_data) {prm_dist()} else {NA},
        prm_qq = if ("q-q prm" %in% input$export_data) {prm_qq()} else {NA},
        eta_dist = if ("eta_dist" %in% input$export_data) {eta_dist()} else {NA},
        eta_qq = if ("q-q eta" %in% input$export_data) {eta_qq()} else {NA},
        dv_pred = if ("dv-pred" %in% input$export_data) {dv_pred()} else {NA},
        dv_ipred = if ("dv-ipred" %in% input$export_data) {dv_ipred()} else {NA},
        iwres_pred = if ("iwres-pred" %in% input$export_data) {iwres_pred()} else {NA},
        cwres_idv = if ("cwres-idv" %in% input$export_data) {cwres_idv()} else {NA},
        phi_m = if ("phi" %in% input$export_data) {phi_mat()} else {NA},
        coi_m = if ("coi" %in% input$export_data) {coi_mat()} else {NA},
        cor_m = if ("cor" %in% input$export_data) {cor_mat()} else {NA},
        cov_m = if ("cov" %in% input$export_data) {cov_mat()} else {NA},
        vpc = if ("vpc" %in% input$export_data) {vpc_plot()} else {NA},
        scat = if ("scatter_matrix" %in% input$export_data) {scat_mat_p()} else {NA},
        ind = if ("ind_plot" %in% input$export_data) {iplot()} else {NA}
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render("report.Rmd", output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
    
  # Shiny files widget
  shinyFiles::shinyDirChoose(input=input,
                             id='dir',
                             roots = c(home='~'))
  
  # Directory management
  dir <- shiny::reactive(shinyFiles::parseDirPath(roots = c(home='~'), input$dir))
  mod_files <- shiny::reactive({ list.files(dir(), pattern='\\.mod$', all.files=FALSE, full.names=FALSE, recursive=FALSE) })
  lst_files <- shiny::reactive({ list.files(dir(), pattern='\\.lst$', all.files=FALSE, full.names=FALSE, recursive=FALSE) })
  csv_files <-  shiny::reactive({ list.files(dir(), pattern='\\.csv$', all.files=FALSE, full.names=FALSE, recursive=FALSE) })
 
  mod_selected <- shiny::reactive({ gsub('.mod',"" , mod_files()[input$selected]) }) # getting rid of the selected model file's extension
  csv_selected <- shiny::reactive({ gsub('.csv',"" , csv_files()[input$selected_csv]) }) # getting rid of the selected model file's extension
  
  mod_res <- shiny::reactive({
    mod_files <- mod_files()
    est_list <- paste0(
      tools::file_path_sans_ext(list.files(dir(), pattern='\\.mod$', all.files=FALSE, full.names=FALSE, recursive=FALSE)), # get rid of the extensions 
      ".lst"
    )
    df_mod <- as.data.frame(t(sapply( paste0(dir(),"/", mod_files), mod_reader)), row.names = FALSE)
    df_lst <- as.data.frame(t(sapply( paste0(dir(),"/", est_list), est_reader)), row.names = FALSE) # function from "est_reader.R"
    df <- cbind(mod_files, df_mod, df_lst)
    colnames(df) <- c("model","base","des","lab","ofv","s","b","c")
    df$ofv <- as.numeric(df$ofv)
    df$dofv <- df$ofv - df[match(df$base, gsub(".mod","",df$mod_files)), "ofv"]
    df
  })
  
  csv_res <- shiny::reactive({
    csv_files <- csv_files()
    csv_mod_time <- base::file.info(paste0(dir(),"/", csv_files))$mtime
    csv_mod_time <- as.character(csv_mod_time)
    df_csv <- cbind(csv_files, csv_mod_time) 
    df_csv
  })
  
  # load xpose data (xpdb)
  xpdb <- shiny::reactive({
    db <- xpose::xpose_data(file = paste0(dir(),'/',mod_selected(),".lst"), quiet=TRUE)
    xpose::update_themes(
      db,
      gg_theme = ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
                                legend.position='bottom',
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
      xp_theme = list(point_color = input$col_point, point_alpha = 0.7,
                      line_color  = input$col_line, line_alpha = 0.7,
                      smooth_color = input$col_smooth)
    )
  })
  
  csv_loaded <- shiny::reactive({
    read.csv(file = paste0(dir(),'/',csv_selected(),".csv"), check.names = FALSE )
  })
  
  
  # Directory prints only when there's any input
  shiny::observe({
    if(isTruthy(dir()))
      message(dir())
  })
  
  prm_df <- shiny::reactive({
    prm_df <- xpose::read_nm_files(runno = "", prefix = paste0(dir(),'/',mod_selected()) )
    prob_option <- prob_option()
    # options (parameter estimates)
    .problem <- prob_option[1] # selected problem
    .subprob <- prob_option[2] # selected sub-problem
    .method  <- prm_df$method[prm_df$extension == "ext" & prm_df$problem == .problem & prm_df$subprob == .subprob]
    # check '.ext' file
    if (!any(prm_df$extension == 'ext')) {
      stop('File extension `ext` not found in model output files.' , call. = FALSE) 
    }
    # filter '.ext', '.cov' file in selected problem/sub-problem
    prm_df <- prm_df %>%
      dplyr::filter(.$problem %in% .problem,                    
                    .$subprob %in% .subprob, 
                    .$method %in% .method)
    # check '.ext' file in selected problem/sub-problem
    if (!any(prm_df$extension == 'ext')) {
      stop('No parameter estimates found for $prob no.', 
           stringr::str_c(.problem, collapse = '/'), ', subprob no. ',
           stringr::str_c(.subprob, collapse = '/'), ', method ',
           stringr::str_c(.method, collapse = '/'), '.', call. = FALSE) 
    }
    # spread data - extension to columns
    prm_df <- prm_df %>% 
      dplyr::select(-dplyr::one_of('name', 'modified')) %>% 
      tidyr::spread(key = 'extension', value = 'data')
    
  }) # collect files derived from model
  
  lst_df <- shiny::reactive({ xpose4::read.lst(filename =  paste0(dir(),'/',mod_selected(),".lst")) })  # load model result
  mod_code <- shiny::reactive({ xpose::read_nm_model(file =  paste0(dir(),'/',mod_selected(),".lst")) })
  
  
  
  
  
  
  
  files_cur_mod <- reactive({
    reactable::reactable(
      style = list(
        verticalAlign = "center",
        align = "left"
      ),
      filterable = FALSE,
      highlight = TRUE,
      outlined = FALSE,
      compact = TRUE,
      rownames = FALSE,
      pagination = FALSE,
      resizable = TRUE,
      selection = "single", onClick = 'select',
      selectionId = 'selected',
      data = data.frame(
        mod_res()
      ),
      columns = list(
        model = reactable::colDef(
          
          minWidth = 60,
          align = "left",
          name = "model / description",
          cell = function(value,index) {
            des <- mod_res()$des[index]
            des <- if (!is.na(des)) des else "no description"
            div(
              div(style = list(fontSize = 13, fontWeight = 100), value),
              div(style = list(fontSize = 9, color = "#777777"), des)
            )
          }
        ),
        base = reactable::colDef(show = FALSE),
        des = reactable::colDef(show = FALSE),
        lab = reactable::colDef(show = FALSE),
        dofv = reactable::colDef(show = FALSE),
        ofv = reactable::colDef(
          minWidth = 30,
          align = "center",
          name = "ofv / change",
          cell = function(value,index) {
            dofv <- mod_res()$dofv[index]
            div(
              div(style = list(fontSize = 12), value),
              div(style = list(fontSize = 9, color = "#777777"), dofv)
            )
          }
        ),
        s = reactable::colDef(
          minWidth = 10,
          align = "center",
          style = function(value) {
            if (is.na(value)) {
              color <- "#CCCCCC"
            } else if (value == "r") {
              color <- "#CC99FF"
            } else if (value == "t") {
              color <- "#FF6666"
            } else if (value == "sim") {
              color <- "#66CCCC"
            } else if (value == "s") {
              color <- "#66CC99"
            }
            list(color = color)
          }
        ),
        b = reactable::colDef(
          minWidth = 10,
          align = "center",
          style = function(value) {
            list(color = "#66CCCC")
          }
        ),
        c = reactable::colDef(
          minWidth = 10,
          align = "center",
          style = function(value) {
            if (is.na(value)) {
              color <- "#CCCCCC"
            } else if (value == "c") {
              color <- "#FFCC00"
            } else if (value == "m") {
              color <- "#CC9999"
            } 
            list(color = color)
          }
        )
      ),
      theme = reactable_theme
      
    )
  })
  
  
  
  # show current files
  output$files_cur_mod <- reactable::renderReactable({ files_cur_mod() })
  
  
  # show current files (sheets)
  output$files_cur_csv <- reactable::renderReactable({
    reactable::reactable(
      
      style = list(
        fontSize = "13px",
        verticalAlign = "center",
        align = "left"
      ),
      filterable = FALSE,
      highlight = TRUE,
      outlined = FALSE,
      compact = TRUE,
      rownames = FALSE,
      pagination = FALSE,
      resizable = TRUE,
      selection = "single", onClick = 'select',
      selectionId = 'selected_csv',
      data = data.frame(
        csv_res()
      ),
      theme = reactable_theme,
      columns = list(
        csv_files = reactable::colDef(
          name = "file (csv)"
        ),
        csv_mod_time = reactable::colDef(
          name = "last mod.",
          format = reactable::colFormat(datetime = TRUE)
        )
      )
    )
  })
  
  output$cur_csv <- rhandsontable::renderRHandsontable({
    
    rhandsontable::rhandsontable(
      data = csv_loaded(),
      digits = 4,
      search = TRUE
    )
  })
  
  output$nca_res <- reactable::renderReactable({
    
    df_nca <- rhandsontable::hot_to_r(input$cur_csv)
    #removing all the special characters in column names
    colnames(df_nca) <- gsub("[^[:alnum:]]", "", colnames(df_nca))
    
    
    res <- data.frame(
      ncappc::ncappc(
        obsFile = df_nca,
        doseAmtNm = 'AMT',
        doseType = input$ss_nca,
        adminType = input$adtype,
        doseTime = input$dtime_nca,
        Tau = input$tau_nca,
        TI = input$tinf_nca,
        onlyNCA = TRUE,
        backExtrp = TRUE,
        extrapolate = TRUE,
        LambdaTimeRange = NULL,
        LambdaExclude = NULL,
        printOut = FALSE,
        evid = FALSE,
        noPlot = TRUE)
    )
    
    colnames(res) <- gsub("ncaOutput.", "",colnames(res))
    
    # change the order of the columns in res
    res <- res[,c(
      1,2,3,4,5,6,7,8,9,10,
      11,12,13,14,15,16,17,18,19,20,
      21,25,22,23,24,26,27,28,
      32,33,29,30,31,
      34,39,40,35,36,37,38,
      41,42,43,44,45,46,47,48,49,50,
      51,52,53,54,55,56,57
    )]
    
    #res_spark <- sapply(res, function(x){
    #  ifelse(
    #    is.na(x),
    #    NA,
    #    hist(x, plot=F)['counts']
    #  )
    #})
    #res_spark <- res_spark[1,]
    
    res <- res[,!grepl('sim',colnames(res))]
    
    res_mean <- sapply(res, function(x){mean(x, na.rm=TRUE)})
    res_median <- sapply(res, function(x){median(x, na.rm=TRUE)})
    res_sd <- sapply(res, function(x){sd(x, na.rm=TRUE)})
    
    res <- rbind(res, res_mean, res_median, res_sd)
    res$ID <- c(res$ID[1:(length(res$ID)-3)],"mean","median","sd")
    
    res <- cbind(ID = res$ID, round(res[,2:ncol(res)],3)) # rounding data (digits:3)
   
    #res <- rbind(res, t(res_spark)) # merge histogram data
    
    
    # preparing reactable for rendering
    reactable::reactable(
      style = list(
        fontSize = "13px",
        verticalAlign = "center",
        align = "left"
      ),
      filterable = FALSE,
      highlight = TRUE,
      outlined = FALSE,
      compact = TRUE,
      rownames = FALSE,
      pagination = FALSE,
      resizable = TRUE,
      data = res,
      rowStyle = function(index) {
        if ( res[index, 'ID'] %in% c("mean", "median", "sd") ){ list(background = "rgba(0, 0, 0, 0.15)") }
      },
      defaultColDef = reactable::colDef(footer = function(values) {
        if (!is.numeric(values)||is.na(values)) return()
        sparkline::sparkline(
          values = hist(values[1:(length(values)-3)], plot=FALSE)['counts'][[1]],
          type = "bar",
          width = 95, height = 30,
          barColor = '#99cc66',
          negBarColor = '#ff9966'
        )
      }),
      columns = list(
        ID = reactable::colDef(
          align = "center",
          minWidth = 70,
          style = function(value) {
            list(fontWeight=600, color = '#777777')
          }
        ),
        Cmax = reactable::colDef(
          
          style = function(value) {
            bar_style(width = value / max(res$Cmax), fill = "#999999", align = 'right')
          }
        ),
        Tmax = reactable::colDef(
          
          style = function(value) {
            bar_style(width = value / max(res$Tmax), fill = "#999999", align = 'right')
          }
        ),
        AUClast = reactable::colDef(
          
          style = function(value) {
            bar_style(width = value / max(res$AUClast), fill = "#999999", align = 'right')
          }
        ),
        Tmax = reactable::colDef(
          
          style = function(value) {
            bar_style(width = value / max(res$Tmax), fill = "#CC9999", align = 'right')
          }
        ),
        
        AUCINF_obs = reactable::colDef(
          
          style = function(value) {
            bar_style(width = value / max(res$AUCINF_obs), fill = "#FF9999", align = 'right')
          }
        ),
        
        HL_Lambda_z = reactable::colDef(
          
          style = function(value) {
            bar_style(width = value / max(res$HL_Lambda_z), fill = "#999999", align = 'right')
          }
        ),
        Vz_obs = reactable::colDef(
          
          style = function(value) {
            bar_style(width = value / max(res$Vz_obs), fill = "#99CCFF", align = 'right')
          }
        ),
        Cl_obs = reactable::colDef(
          
          style = function(value) {
            bar_style(width = value / max(res$Cl_obs), fill = "#FFCC66", align = 'right')
          }
        ),
        AUCINF_pred = reactable::colDef(
          
          style = function(value) {
            bar_style(width = value / max(res$AUCINF_obs), fill = "#FF99CC", align = 'right')
          }
        ),
        Vz_pred = reactable::colDef(
          
          style = function(value) {
            bar_style(width = value / max(res$Vz_obs), fill = "#66CCCC", align = 'right')
          }
        ),
        Cl_pred = reactable::colDef(
          
          style = function(value) {
            bar_style(width = value / max(res$Cl_obs), fill = "#CCCC33", align = 'right')
          }
        )
        
      ),
      theme = reactable_theme
    )
    
  })

  # run options
  run_options_server("method", dir, mod_selected)
  # summary table
  summary_tbl <- summary_tbl_server("summary", lst_df, mod_code, mod_selected)
  output$summary <- reactable::renderReactable({ summary_tbl() })
  # terminal manager
  terminal_ref <- terminal_server("terminals")
  # parameter table
  prob_option <- shiny::reactive({ c(input$prob, input$subprob, input$digits) })
  prm_option <- shiny::reactive({ c(input$add_option) })
  
  
  
  
  # theta result for view
  param_tbl_theta <- param_tbl_server_view("param_theta_view", prm_df, lst_df, mod_code, prob_option, prm_option)
  # eta/sig result for view
  param_tbl_etasig <- param_tbl_server_view("param_etasig_view", prm_df, lst_df, mod_code, prob_option, prm_option)
  
  
  
  # copy theta result of selected model
  observeEvent(input$copy_theta, {
    i <- sprintf('%04d', input$copy_theta)
    id <- sprintf('param_theta%s', i)
    insertUI(
      selector = '#copy_theta',
      where = "beforeBegin",
      ui = param_theta_ui_iso(id)
       
    )
    param_tbl_server_iso(id, prm_df, lst_df, mod_code, prob_option, prm_option, mod_selected)
    
    observeEvent(input[[paste0(id, '-deleteButton')]], {
      removeUI(selector = sprintf('#%s', id))
      remove_shiny_inputs(id, input)
    })
  })
  # copy eta/sig result of selected model
  observeEvent(input$copy_etasig, {
    i <- sprintf('%04d', input$etasig)
    id <- sprintf('param_theta%s', i)
    insertUI(
      selector = '#copy_theta',
      where = "beforeBegin",
      ui = param_etasig_ui_iso(id)
      
    )
    param_tbl_server_iso(id, prm_df, lst_df, mod_code, prob_option, prm_option, mod_selected)
    
    observeEvent(input[[paste0(id, '-deleteButton')]], {
      removeUI(selector = sprintf('#%s', id))
      remove_shiny_inputs(id, input)
    })
  })
  
  
  output$param_etasig <- reactable::renderReactable({ param_tbl()$etasig })
  
  
  # phi matrix
  phi_mat <- phi_mat_server("phi", prm_df, phi_id_ex, phi_var_ex)
  output$phi <- plotly::renderPlotly({ phi_mat() })
  
  output$phi_id_ex_ui <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "phi_id_ex", 
      label = "ID to exclude", 
      choices = data.frame(prm_df()$phi, check.names = FALSE)$ID, 
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `live-search` = TRUE)
    )
  })
  output$phi_var_ex_ui <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "phi_var_ex", 
      label = "var to exclude", 
      choices = colnames(data.frame(prm_df()$phi, check.names = FALSE))[-1][-1], 
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `live-search` = TRUE)
    )
  })
  phi_id_ex <- shiny::reactive({ input$phi_id_ex })
  phi_var_ex <- shiny::reactive({ input$phi_var_ex })
  
  # coi matrix
  coi_mat <- coi_mat_server("coi", dir, mod_selected, coi_x_ex, coi_y_ex)
  output$coi <- plotly::renderPlotly({ coi_mat() })

  output$coi_x_ex_ui <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "coi_x_ex", 
      label = "x to exclude", 
      choices = data.frame(prm_df()$cov, check.names = FALSE)$NAME, 
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `live-search` = TRUE)
    )
  })
  output$coi_y_ex_ui <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "coi_y_ex", 
      label = "y to exclude", 
      choices = data.frame(prm_df()$cov, check.names = FALSE)$NAME, 
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `live-search` = TRUE)
    )
  })
  coi_x_ex <- shiny::reactive({ input$coi_x_ex })
  coi_y_ex <- shiny::reactive({ input$coi_y_ex })
  
  # cov matrix
  cov_mat <- cov_mat_server("cov", prm_df, cov_x_ex, cov_y_ex)
  output$cov <- plotly::renderPlotly({ cov_mat() })
  
  output$cov_x_ex_ui <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "cov_x_ex", 
      label = "x to exclude", 
      choices = data.frame(prm_df()$cov, check.names = FALSE)$NAME, 
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `live-search` = TRUE)
    )
  })
  output$cov_y_ex_ui <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "cov_y_ex", 
      label = "y to exclude", 
      choices = data.frame(prm_df()$cov, check.names = FALSE)$NAME, 
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `live-search` = TRUE)
    )
  })
  cov_x_ex <- shiny::reactive({ input$cov_x_ex })
  cov_y_ex <- shiny::reactive({ input$cov_y_ex })
  
  # cor matrix
  cor_mat <- cor_mat_server("cor", prm_df, cor_x_ex, cor_y_ex)
  output$cor <- plotly::renderPlotly({ cor_mat() })
  
  output$cor_x_ex_ui <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "cor_x_ex", 
      label = "x to exclude", 
      choices = data.frame(prm_df()$cov, check.names = FALSE)$NAME, 
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `live-search` = TRUE)
    )
  })
  output$cor_y_ex_ui <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "cor_y_ex", 
      label = "y to exclude", 
      choices = data.frame(prm_df()$cov, check.names = FALSE)$NAME, 
      selected = NULL,
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `live-search` = TRUE)
    )
  })
  cor_x_ex <- shiny::reactive({ input$cor_x_ex })
  cor_y_ex <- shiny::reactive({ input$cor_y_ex })
  
  output$scat_mat_vars <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "scat_vars",
      label = "values to plot", 
      choices = colnames( xpdb()$data$data[[1]] ),
      multiple = TRUE,
      selected = NULL,
      options = list(`actions-box` = TRUE),
      choicesOpt = list(
        content = colnames( xpdb()$data$data[[1]] ) 
      )
    )
  })
  
  scat_vars <- shiny::reactive({ input$scat_vars }) # values selected from UI input
  
  output$scat_mat_fact <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "scat_fact",
      label = "values as factor (discontinuous)", 
      choices = scat_vars(),
      multiple = TRUE,
      selected = NULL,
      options = list(`actions-box` = TRUE),
      choicesOpt = list(
        content = scat_vars() 
      )
    )
  })
  
  scat_fact <- shiny::reactive({ input$scat_fact }) # values selected to be factor class
  
  output$scat_mat_color <- shiny::renderUI({
    
    shiny::textInput(
      inputId = "scat_color",
      label = "colorize (grouping)", 
      value = "",
      placeholder = "write a variable for color mapping"
    )
  })
  
  scat_color <- shiny::reactive({ input$scat_color })
  
  scat_button <- shiny::reactive({ input$scat_button })
  scat_method <-  shiny::reactive({ input$scat_method })
  
  scat_mat_p <- scat_mat_server("scat", xpdb, scat_vars, scat_fact, scat_color, scat_button, scat_method)
  
  output$scat_mat_plot <- shiny::renderPlot({ scat_mat_p() }, bg="transparent")
  
  
  # first tabs
  prm_dist <- reactive({ xpose::prm_distrib( xpdb(),
                                             type = paste0(input$prm_dist_option, collapse = ""),
                                             histogram_fill = input$hist_fill,
                                             density_fill = input$density_fill,
                                             rug_color = input$rug_color) })
  prm_qq <- reactive({ xpose::prm_qq( xpdb() ) })
  eta_dist <- reactive({ xpose::eta_distrib( xpdb(),
                                             type = paste0(input$eta_dist_option, collapse = ""),
                                             histogram_fill = input$hist_fill,
                                             density_fill = input$density_fill,
                                             rug_color = input$rug_color) })
  eta_qq <- reactive({ xpose::eta_qq( xpdb() ) })
  
  output$prm_dist <- renderPlot({ prm_dist() }, bg="transparent")
  output$eta_dist <- renderPlot({ eta_dist() }, bg="transparent")
  output$prm_qq <- renderPlot({ prm_qq() }, bg="transparent")
  output$eta_qq <- renderPlot({ eta_qq() }, bg="transparent")
  
  # second tabs
  # dependent variable - population prediction
  dv_pred <- reactive({
    plotly::layout(
      plotly::ggplotly(xpose::dv_vs_pred( xpdb(), title=NULL, subtitle=NULL )),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )
    
  }) 
  dv_ipred <- reactive({
    plotly::layout(
      plotly::ggplotly(xpose::dv_vs_ipred( xpdb(), title=NULL, subtitle=NULL )),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )
  })
  iwres_pred <- reactive({
    plotly::layout(
      plotly::ggplotly(xpose::absval_res_vs_pred( xpdb(), res="IWRES", title=NULL, subtitle=NULL )),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )
  })
  cwres_idv <- reactive({
    plotly::layout(
      plotly::ggplotly(xpose::res_vs_idv( xpdb(), res="CWRES", title=NULL, subtitle=NULL )),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )
  }) 
  scatter <- reactive({
    plotly::layout(
      plotly::ggplotly(xpose::xplot_scatter( xpdb(),
                                             aes(x = get(input$scatter_x), y = get(input$scatter_y)),
                                             title=NULL,
                                             subtitle=NULL,
                                             opt = xpose::data_opt(
                                               .problem = input$prob,
                                               .subprob = input$subprob,
                                               filter = function(x) x[x$DV != 0, ]
                                             )) + ggplot2::xlab(input$scatter_x) + ggplot2::ylab(input$scatter_y)
                       ),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)"
    )
  })
  iplot <- reactive({
    plotly::layout(
      plotly::ggplotly(
        xpose::ind_plots(
          xpdb(),
          group = if(input$ind_group==""){"variable"}else{input$ind_group},
          page = input$ind_page,
          ncol = input$ind_cols,
          nrow = input$ind_rows,
          title = NULL,
          subtitle = NULL,
          caption = NULL,
          facets = c("ID"),
          color = c("grey60", input$col_area1, input$col_area2),
          line_linetype = c("blank", "solid", "twodash")
        ) +
          ggplot2::theme(), tooltip = c("TIME","value")
      ),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      showlegend = FALSE
    )
    
  })
  
  output$dv_pred <- plotly::renderPlotly({ dv_pred() })  # observation - population prediction
  output$dv_ipred <- plotly::renderPlotly({ dv_ipred() }) # dependent variable - individual prediction
  output$iwres_pred <- plotly::renderPlotly({ iwres_pred() })  # individual weighted residuals - individual prediction 
  output$cwres_idv <- plotly::renderPlotly({ cwres_idv() }) # conditional weighted residuals - individual prediction
  output$ind_plots <- plotly::renderPlotly({ iplot() })
  vpc_plot <- shiny::reactive({
    
    if( base::file.exists( paste0( dir(),'/',"vpc_",mod_selected(),"/vpc_results.csv" ) ) ){
      vpc_dat <- xpose::vpc_data(xpdb(), psn_folder = paste0( dir(),'/',"vpc_",mod_selected() ) )
      vpc <- xpose::vpc(vpc_dat, area_fill = c(input$col_area2, input$col_area1, input$col_area2),
                        line_linetype = c("twodash", "solid", "twodash"),
                        log = paste(input$vpc_opt, collapse = " "),
                        title = NULL,
                        subtitle = NULL,
                        caption = NULL,
                        xp_theme = list(point_color = input$col_point, point_alpha = 0.55, area_alpha = 0.4,
                                        line_color = input$col_line, line_alpha = 0.7)
      )
      plotly::layout(
        plotly::ggplotly(vpc),
        plot_bgcolor = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        showlegend = FALSE
      )
    } else { empty_plot("visual predictive check data", "is not available")}
  
    
  })
  output$vpc <- plotly::renderPlotly({ vpc_plot() })
  output$scatter <- plotly::renderPlotly({ scatter() }) # TIME - DV by default
  
  output$tot_tm_running <- bs4Dash::renderbs4ValueBox(
    bs4Dash::valueBox(value = paste0( sum(terminal_ref()$state=="running"), " number of run. " ),
                      subtitle = paste0(
                        tail(terminal_ref()[terminal_ref()$state=="running","name"],1), " - ",
                        tail(terminal_ref()[terminal_ref()$state=="running","buffer"],1)
                      ),
                      icon = shiny::icon('stopwatch'),
                      color = "warning",
                      footer = "busy",
                      gradient = TRUE, elevation = 3)
  )
    
  output$tot_tm_idle <- bs4Dash::renderbs4ValueBox(
    bs4Dash::valueBox(value = paste0( sum(terminal_ref()$state=="idle"), " number of run. " ),
                      subtitle = paste0(
                        tail(terminal_ref()[terminal_ref()$state=="idle","name"],1), " - ",
                        tail(terminal_ref()[terminal_ref()$state=="idle","buffer"],1)
                      ),
                      icon = shiny::icon('check'), color = "success", footer = "finished",
                      gradient = TRUE, elevation = 3)
  )
    
  
}

shiny::runGadget( app=shiny::shinyApp(ui=ui, server=server), viewer = shiny::paneViewer() )