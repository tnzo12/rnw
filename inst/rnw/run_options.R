# code for run options
# namespace: 'method', 'cmd'

run_options_ui <- function(id){
  ns <- NS(id)
  tagList( # two widgets = picker, text input
    
    shinyWidgets::pickerInput(
      inputId = ns('method'),
      label = "run options",
      choices = list(
        
        execute = c("execute"),
        model_diagnostics = c("vpc","npc","bootstrap","cdd","llp","sir","ebe_npde"),
        design_evaluation = c("sse"),
        covariates = c("scm","xv_scm","boot_scm","lasso"),
        misc = c("nca","nonpb","mimp","gls","parallel_retries","precond","psn_clean","update_inits")
        
      )
    ),
    shiny::textAreaInput(ns("cmd"), label="command input", resize="vertical", rows=5),
    shiny::actionButton(ns("run"), label=HTML("&nbsp;run selected model"), icon=shiny::icon("desktop"))
    
    
  )
  
  
}

run_options_server <- function(id, dir, mod_selected){
  
  moduleServer(id, function(input, output, session) {
    
    # run options
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
    
    # Button action
    observeEvent(input$run,{
      rstudioapi::terminalExecute(input$cmd, workingDir = dir())
    })
    
    
  })
  
  
}