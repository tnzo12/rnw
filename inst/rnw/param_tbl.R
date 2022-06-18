# code for parameter estimation table
param_list <- function(prm_df, lst_df, mod_code, prob_option, prm_option){
  
  quiet <- FALSE
  transform <- FALSE # option for xpose get_prm
  transformation <- "trans" %in% prm_option # manual transformation
  show_all  <- "off-diag" %in% prm_option
  digits <- prob_option[3]
  
  # extract initial values / read iteration status for visualization: sparkline
  # initial values
  init <- prm_df$ext %>% 
    data.frame(check.names = FALSE) %>% 
    dplyr::filter(ITERATION  == 0) %>% 
    dplyr::select(-c(ITERATION, OBJ)) %>%
    unlist()
  
  # iterations
  iter <- prm_df$ext %>%
    data.frame(check.names = FALSE) %>% 
    dplyr::filter(ITERATION >= 0) %>% # sort out iteration status rows
    dplyr::select(-c(ITERATION, OBJ)) %>% # get rid of iteration, OFV column
    sapply(function(x){list(x)})
  # bind up iteration vectors with names
  iter <- cbind(name = names(iter), iteration = iter) %>% 
    data.frame()
  iter$name <- as.character(iter$name)
  
  
  # extract parameter estimates
  prm_df <- prm_df %>% 
    dplyr::mutate(prm_names = purrr::map(.x = as.list(.$problem), .f = function(x, code) {
      
      # Collect parameter names from the model code
      code <- code[code$problem == x & nchar(code$code) > 0,]
      list(theta = code$comment[code$subroutine == 'the'],
           omega = code[code$subroutine == 'ome', ] %>%
             dplyr::filter(!(stringr::str_detect(.$code, 'BLOCK\\(\\d+\\)(?!.*SAME)') & .$comment == '')) %>% 
             {purrr::flatten_chr(.[, 'comment'])},
           sigma = code$comment[code$subroutine == 'sig'])
    }, code = mod_code)) %>% 
    purrr::transpose() %>% 
    purrr::map(.f = function(data) {
      prm_mean <- grab_iter(ext = data$ext, iter = -1000000000) # mean value, estimated
      prm_se   <- grab_iter(ext = data$ext, iter = -1000000001) # standard error, estimated
      prm_fix  <- grab_iter(ext = data$ext, iter = -1000000006) # fixed, in estimation
      
      if (all(is.na(prm_fix))) {
        warning('Iteration `-1000000006` not found in the `.ext` file. Assuming no fixed parameters, check the output carefully.', call. = FALSE)
        prm_fix[is.na(prm_fix)] <- 0
      }
      
      if (transform) {
        if (!is.null(data$cov)) {
          # build covariance matrix from `.cov` file
          prm_cov <- as.data.frame(data$cov) %>% 
            tibble::column_to_rownames('NAME') %>% 
            as.matrix()
        } else {
          # build covariance matrix from `.ext` se
          prm_cov <- diag(prm_se)^2
          attr(prm_cov, 'dimnames') <- list(names(prm_se), names(prm_se))
          
          # No warning when .ext SE not available
          if (!all(is.na(prm_se))) {
            warning('Covariance matrix (`.cov`) not available, RSE for covariance parameters will be incorrect.', call. = FALSE)
          }
        }
        # obtain transformation formulas 
        prm_trans_formula <- get_prm_transformation_formulas(names(prm_mean))
        # transform parameters & calculate var, rse for transformation
        prms <- purrr::map_df(prm_trans_formula, ~transform_prm(.x, mu = prm_mean, sigma = prm_cov, method = 'delta')) %>% 
          dplyr::mutate(se = sqrt(.$variance))
      } else {
        prms <- dplyr::tibble(mean = purrr::flatten_dbl(prm_mean), 
                              se   = purrr::flatten_dbl(prm_se)) %>% 
          dplyr::mutate(rse = .$se/abs(.$mean))
      }
      
      prms <- prms %>% 
        dplyr::mutate(fixed = as.logical(as.numeric(prm_fix)),
                      name  = names(prm_mean)) %>%
        dplyr::mutate(type = dplyr::case_when(stringr::str_detect(.$name, 'THETA') ~ 'the',
                                              stringr::str_detect(.$name, 'OMEGA') ~ 'ome',
                                              stringr::str_detect(.$name, 'SIGMA') ~ 'sig'),
                      number = stringr::str_replace_all(.$name, '[^\\d,]+', ''),
                      se     = ifelse(.$fixed, NA_real_, as.numeric(.$se)),
                      rse    = ifelse(.$fixed, NA_real_, abs(as.numeric(.$rse)))) %>%
        tidyr::separate(col = 'number', into = c('m', 'n'), sep = ',', fill = 'right') %>% 
        dplyr::mutate(diagonal = dplyr::if_else(.$m == .$n, TRUE, FALSE)) %>% 
        dplyr::rename(!!rlang::sym('value') := !!rlang::sym('mean')) %>% 
        dplyr::mutate(label = '',
                      value = signif(.$value, digits = digits),
                      se    = signif(.$se, digits = digits),
                      rse   = signif(.$rse, digits = digits),
                      n     = as.numeric(.$n),
                      m     = as.numeric(.$m),
                      order = dplyr::case_when(type == 'the' ~ 1,
                                               type == 'ome' ~ 2,
                                               TRUE ~ 3)) %>% 
        dplyr::arrange_at(.vars = 'order') %>% 
        dplyr::select(dplyr::one_of('type', 'name', 'label', 'value', 'se', 'rse', 'fixed', 'diagonal', 'm', 'n'))
      
      # Assign THETA labels
      n_theta     <- sum(prms$type == 'the')
      theta_names <- data$prm_names$theta
      if (n_theta != length(theta_names)) {
        warning('[$prob no.', data$problem, ', subprob no.', data$subprob, ', ', data$method, 
                '] $THETA labels did not match the number of THETAs in the `.ext` file.', call. = FALSE)
      } else {
        prms$label[prms$type == 'the'] <- theta_names
      }
      
      # Assign OMEGA labels
      n_omega     <- sum(prms$type == 'ome' & prms$diagonal, na.rm = TRUE)
      omega_names <- data$prm_names$omega
      if (n_omega != length(omega_names)) {
        warning('[$prob no.', data$problem, ', subprob no.', data$subprob, ', ', data$method, 
                '] $OMEGA labels did not match the number of OMEGAs in the `.ext` file.', call. = FALSE)
      } else {
        prms$label[prms$type == 'ome' & prms$diagonal] <- omega_names
      }
      
      # Assign SIGMA labels
      n_sigma     <- sum(prms$type == 'sig' & prms$diagonal, na.rm = TRUE)
      sigma_names <- data$prm_names$sigma
      if (n_sigma != length(sigma_names)) {
        warning('[$prob no.', data$problem, ', subprob no.', data$subprob, ', ', data$method, 
                '] $SIGMA labels did not match the number of SIGMAs in the `.ext` file.', call. = FALSE)
      } else {
        prms$label[prms$type == 'sig' & prms$diagonal] <- sigma_names
      }
      
      # Filter_all
      if (!show_all) {
        prms <- dplyr::filter(.data = prms, !(prms$type %in% c('ome', 'sig') & 
                                                prms$value == 0 & !prms$diagonal))
      }
      
      # Add metadata to output
      structure(.Data = prms, file = 'ext', problem = data$problem, 
                subprob = data$subprob, method = data$method)
      
    }) %>% data.frame()
  
  
  # generate param table
  param_table <- data.frame(
    prm_df, init = init[match(prm_df$name, names(init))], # order matching
    row.names = NULL
  ) %>% 
    mutate(change = (value - init)/init)
  
  
  # process NaN, NULL, NA values (to 0)
  param_table$change[is.nan(param_table$change) |
                       is.null(param_table$change) |
                       is.na(param_table$change)] <- 0
  
  
  
  
  # extract shrinkage from model summary
  
  # ETA shrinkage
  etash <- lst_df$term[grepl(pattern = "ETASHRINKSD", lst_df$term)] %>% 
    strsplit(split = "\\s+") %>%
    unlist() %>%
    .[-1] %>% 
    as.numeric() %>% 
    data.frame(
      name = sapply(seq_along(.), function(x){ paste0("OMEGA(",x,",",x,")") }),
      shr = .
    )
  # ETA bar
  etabar <- lst_df$term[grepl(pattern = "P VAL", lst_df$term)] %>% 
    strsplit(split = "\\s+") %>% 
    unlist() %>% 
    .[3:length(.)] %>% 
    as.numeric() %>% 
    data.frame(
      name = sapply(seq_along(.), function(x){ paste0("OMEGA(",x,",",x,")") }),
      bar = .
    )
  
  
  # EPS shrinkage
  epssh <- lst_df$term[grepl(pattern = "EPSSHRINKSD", lst_df$term)] %>% 
    strsplit(split = "\\s+") %>%
    unlist() %>%
    .[-1] %>% 
    as.numeric() %>%
    data.frame(
      name = sapply(seq_along(.), function(x){ paste0("SIGMA(",x,",",x,")") }),
      shr = .
    )
  
  shk_table <- rbind(etash, epssh) # row bind two shrinkage
  
  
  param_table <- dplyr::left_join(param_table, shk_table, by='name') %>% 
    dplyr::left_join(etabar, by='name') %>% 
    dplyr::mutate(shr = ifelse(is.na(shr), "-", shr)) %>% # fill NA with hyphen 
    dplyr::mutate(rse = ifelse(is.na(rse), " ", round(rse*100,2) )) %>% # fill NA with white space
    dplyr::mutate(bar = ifelse(is.na(bar), 1, bar))  # fill non-ETA bar values with 1
  
  
  # *Table output order
  # Arrange the columns which should be used in parameter-table
  param_table_final <- param_table[,c("name","label","init","value","change","rse","shr", "bar", "fixed","m","n")]
  param_table_final <- dplyr::left_join(param_table_final, iter) %>% 
    mutate(value = ifelse(transformation == TRUE & grepl(pattern = "OMEGA|SIGMA", name),
                          paste0(signif( sqrt(exp(value)-1), digits = digits)*100,"%"),
                          value ))
  
  param_table_final_theta <- dplyr::filter(param_table_final, grepl("THETA", name))
  param_table_final_etasig <- dplyr::filter(param_table_final, !grepl("THETA", name))
  
  # table formatting (package: reactable)
  list( # list for thata & eta
    
    # reactable for theta -----------------------------
    theta =  reactable::reactable(
      data = param_table_final_theta,
      compact = TRUE,
      
      rowStyle = function(index) {
        if ( param_table_final_theta[index, 'fixed']=='TRUE' ){ list(background = "rgba(0, 0, 0, 0.15)") } else
          if ( param_table_final_theta[index, 'bar'] < 0.05 ){ list(background = "rgba(255, 0, 0, 0.10)") } else
            if ( param_table_final_theta[index, 'change'] > 0.9 ){ list(background = "rgba(0, 0, 255, 0.10)") } 
      },
      defaultColDef = reactable::colDef(
        align = "center",
        minWidth = 50
      ),
      theme = reactable_theme,
      columns = list(
        # designate the value to hide
        
        
        shr = reactable::colDef(show = FALSE),
        label = reactable::colDef(show = FALSE),
        init = reactable::colDef(show = FALSE),
        bar = reactable::colDef(show = FALSE),
        fixed = reactable::colDef(show = FALSE),
        m = reactable::colDef(show = FALSE),
        n = reactable::colDef(show = FALSE),
        
        
        # designate the value to show
        name = reactable::colDef(
          name = "param / label",
          minWidth = 40,
          align = "right",
          cell = function(value,index) {
            label <- param_table_final_theta$label[index]
            div(
              div(style = list(fontWeight = 600), value),
              div(style = list(fontSize = 9, color = "#777777"), label)
            )
          },
          style = function(value) {
            color <- if ( grepl("THETA", value) == TRUE) {
              "#66CCCC"
            } else if ( grepl("OMEGA", value) == TRUE ) {
              "#66CC99"
            } else {
              "#FF6666"
            }
            list(fontWeight = 500, color = color)
          }
          
        ),
        value = reactable::colDef(
          name = "value / init",
          align = "left",
          minWidth = 40,
          cell = function(value,index) {
            init <- param_table_final_theta$init[index]
            div(
              div(style = list(fontWeight = 500), value),
              div(style = list(fontSize = 9, color = "#777777"), init)
            )
          }
        ),
        change = reactable::colDef(
          name = "change",
          minWidth = 50,
          cell = function(value,index) {
            change <- param_table_final_theta$change[index]
            change <- if (!is.na(change)) change else "no change"
            change_abs <- param_table_final_theta$init[index] * change
            label <- paste0(round(change * 100), "%")
            div(
              div(style = list(fontWeight = 500), round(change_abs,4)),
              div(style = list(fontSize = 9, color = "#777777"), bar_chart_pos_neg(label, change))
            )
          }
        ),
        rse = reactable::colDef(
          name = "rse",
          align = "center",
          minWidth = 40,
          cell = function(value,index) {
            shr <- param_table_final_theta$shr[index]
            shr <- if (!is.na(shr)) shr else " "
            div(
              div(style = list(fontWeight = 500), value),
              div(style = list(fontSize = 9, color = "#B3B3B3"), shr),
            )
          }
        ),
        iteration = reactable::colDef(
          align = "center",
          minWidth = 50,
          name = "iter",
          cell = function(values) {
            sparkline::sparkline(
              values,
              fillColor = FALSE,
              lineColor = 'red'
            )
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
    ),
    # reactable for eta,, sig -------------------------
    etasig =  reactable::reactable(
      data = param_table_final_etasig,
      compact = TRUE,
      
      rowStyle = function(index) {
        if ( param_table_final_etasig[index, 'fixed']=='TRUE' ){ list(background = "rgba(0, 0, 0, 0.15)") } else
          if ( param_table_final_etasig[index, 'bar'] < 0.05 ){ list(background = "rgba(255, 0, 0, 0.10)") } else
            if ( param_table_final_etasig[index, 'change'] > 0.9 ){ list(background = "rgba(0, 0, 255, 0.10)") } 
      },
      defaultColDef = reactable::colDef(
        align = "center",
        minWidth = 50
      ),
      theme = reactable_theme,
      columns = list(
        # designate the value to hide
        
        
        shr = reactable::colDef(show = FALSE),
        #change = reactable::colDef(show = FALSE),
        label = reactable::colDef(show = FALSE),
        init = reactable::colDef(show = FALSE),
        bar = reactable::colDef(show = FALSE),
        fixed = reactable::colDef(show = FALSE),
        m = reactable::colDef(show = FALSE),
        n = reactable::colDef(show = FALSE),
        
        
        # designate the value to show
        name = reactable::colDef(
          name = "param / label",
          minWidth = 40,
          align = "right",
          cell = function(value,index) {
            label <- param_table_final_etasig$label[index]
            div(
              div(style = list(fontWeight = 600), value),
              div(style = list(fontSize = 9, color = "#777777"), label)
            )
          },
          style = function(value) {
            color <- if ( grepl("THETA", value) == TRUE) {
              "#66CCCC"
            } else if ( grepl("OMEGA", value) == TRUE ) {
              "#66CC99"
            } else {
              "#FF6666"
            }
            list(fontWeight = 500, color = color)
          }
          
        ),
        value = reactable::colDef(
          name = "value / init",
          align = "left",
          minWidth = 40,
          cell = function(value,index) {
            init <- param_table_final_etasig$init[index]
            div(
              div(style = list(fontWeight = 500), value),
              div(style = list(fontSize = 9, color = "#777777"), init)
            )
          }
        ),
        change = reactable::colDef(
          name = "change",
          minWidth = 50,
          cell = function(value,index) {
            change <- param_table_final_etasig$change[index]
            change <- if (!is.na(change)) change else "no change"
            change_abs <- param_table_final_etasig$init[index] * change
            label <- paste0(round(change * 100), "%")
            div(
              div(style = list(fontWeight = 500), round(change_abs,4)),
              div(style = list(fontSize = 9, color = "#777777"), bar_chart_pos_neg(label, change))
            )
          }
        ),
        rse = reactable::colDef(
          name = "rse / shr",
          align = "center",
          minWidth = 40,
          cell = function(value,index) {
            shr <- param_table_final_etasig$shr[index]
            shr <- if (!is.na(shr)) shr else " "
            div(
              div(style = list(fontWeight = 500), value),
              div(style = list(fontSize = 9, color = "#B3B3B3"), shr),
            )
          }
        ),
        iteration = reactable::colDef(
          align = "center",
          minWidth = 50,
          name = "iter",
          cell = function(values) {
            sparkline::sparkline(
              values,
              fillColor = FALSE,
              lineColor = 'red'
            )
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
    
    
    
  )
}

# function end ======================================================


# UI: ---------------------------------------------------------------
param_theta_ui_view <- function(id) { # ui for theta
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("param_theta_view"))
}

param_etasig_ui_view <- function(id) { # ui for eta/sig
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("param_etasig_view"))
}

param_theta_ui_iso <- function(id) { # ui for theta (copy)
    ns <- shiny::NS(id)
    shiny::uiOutput(ns("param_theta_iso"))
}
param_etasig_ui_iso <- function(id) { # ui for eta/sig (copy)
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("param_etasig_iso"))
}


# Server: parameter table for view ----------------------------------
param_tbl_server_view <- function(id, prm_df, lst_df, mod_code, prob_option, prm_option){
  
  moduleServer(id, function(input, output, session) {
    
    param_tbl <- shiny::reactive({
      prob_option <- prob_option()
      prm_option <- prm_option()
      prm_df <- prm_df() # load estimation data
      lst_df <- lst_df() # load result summaries
      mod_code <- mod_code() # load model codes
      
      param_list(prm_df, lst_df, mod_code, prob_option, prm_option)
     
    })
    
    
    
    output[["param_theta_view"]] <- shiny::renderUI({
      ns <- session$ns
      reactable::renderReactable({ param_tbl()$theta })
    })
    output[["param_etasig_view"]] <- shiny::renderUI({
      ns <- session$ns
      reactable::renderReactable({ param_tbl()$etasig })
    })
    
    param_tbl
    
  }) # moduleServer ends
}

# Server: parameter table for copy ----------------------------------
param_tbl_server_iso <- function(id, prm_df, lst_df, mod_code, prob_option, prm_option, mod_selected){
  
  moduleServer(id, function(input, output, session) {
    
    param_tbl <- shiny::reactive({
      
      isolate({ # value isolation
      
        prob_option <- prob_option()
        prm_option <- prm_option()
        prm_df <- prm_df() # load estimation data
        lst_df <- lst_df() # load result summaries
        mod_code <- mod_code() # load model codes
          
      })
      
      param_list(prm_df, lst_df, mod_code, prob_option, prm_option)
      
      
    })
    

    output[["param_theta_iso"]] <- shiny::renderUI({
      ns <- session$ns
      bs4Dash::box(
        width = 12,
        elevation = 2,
        closable = TRUE,
        title = paste0("copy: ",'"',isolate({mod_selected() }),'"'),
        reactable::renderReactable({ param_tbl()$theta })
      )
      
    })
    output[["param_etasig_iso"]] <- shiny::renderUI({
      ns <- session$ns
      bs4Dash::box(
        width = 12,
        elevation = 2,
        closable = TRUE,
        title = paste0("copy: ",'"',isolate({mod_selected() }),'"'),
        reactable::renderReactable({ param_tbl()$etasig })
      )
      
    })
    
  })
  
}
