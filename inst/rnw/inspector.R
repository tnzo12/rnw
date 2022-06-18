# data, output inspector with plotly

phi_mat_server <- function(id, prm_df, phi_id_ex, phi_var_ex){
  
  moduleServer(id, function(input, output, session){
    
    phi_mat <- shiny::reactive({
    
      phi <- data.frame(prm_df()$phi, check.names = FALSE)[,-1] # load phi file and remove subject number column
      phi$ID <- factor(phi$ID, levels = phi$ID)
      # reshaping into 3 columns
      phi <- reshape2::melt(
        phi, # data input
        id.vars=c("ID"),
        variable.name="param",
        value.name='value'
      )
      phi$value <- as.numeric(phi$value)
      phi$group <- ifelse(phi$param=="OBJ","ofv","eta")
      phi$value[phi$group=="ofv"] <- scales::rescale(phi$value[phi$group=="ofv"], to=c(-1,1)) # ofv is rescaled between -1 &  1
      # POSTV: posterior variance-covariance matrix
      # EBE - Empirical Bayes Estimate
      # OFV value is rescaled between -1 to 0
      
      phi <- dplyr::filter(phi, !(ID %in% phi_id_ex())) %>% 
        dplyr::filter(!(param %in% phi_var_ex()))
      
      plotly::layout(
        plotly::ggplotly({
          ggplot(data = phi,
                 aes(x=ID, y=param, fill = value,
                     text = paste("ID: ", ID,
                                  "<br>param: ", param,
                                  "<br>value: ", value))
                 
          ) +
            geom_tile() +
            #geom_text(aes(label=round(value,3)), size = 3, color="#FFCC99") +
            scale_fill_gradient2(low = "#66CCCC", high = "#FF6666", mid = "#FFCC99", 
                                 midpoint = 0, space = "Lab", 
                                 name="level", limits=c(-1,1)) +
            labs(x="", y="") +
            theme_light() + # minimal theme
            theme(
              plot.margin = margin(0, 0, 0, 0, "cm"),
              rect = ggplot2::element_rect(fill = "transparent"),
              text = ggplot2::element_text(colour='grey60'),
              axis.text = ggplot2::element_text(colour='grey60'),
              axis.ticks = ggplot2::element_line(colour='grey60', size=0.1),
              strip.background = ggplot2::element_blank(),
              strip.text = ggplot2::element_text(colour = 'grey60'),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              plot.background = ggplot2::element_blank(),
              legend.background = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              panel.grid.major = ggplot2::element_line(colour='grey60', size=0.1),
              panel.grid.minor = ggplot2::element_line(colour='grey60', size=0.1)
            )
        }, tooltip = "text"),
        plot_bgcolor  = "rgba(0, 0, 0, 0)",
        paper_bgcolor = "rgba(0, 0, 0, 0)",
        showlegend = FALSE
      )
      
      
      
     
        
    })
  
    
  })
}




cor_mat_server <- function(id, prm_df, cor_x_ex, cor_y_ex){
  
  moduleServer(
    id, function(input, output, session){
      
      cor_mat <- shiny::reactive({
        
        if (shiny::isTruthy( prm_df()$cor )){
          # correlation matrix
          cor <- data.frame(prm_df()$cor, check.names = FALSE) # load phi file and remove subject number column
          
          # ordering as in the order as it was generated
          cor$NAME <- factor(cor$NAME, levels = cor$NAME)
          
          # reshaping into 3 columns
          cor <- reshape2::melt(
            cor, # data input
            id.vars=c("NAME"),
            variable.name="param",
            value.name='value'
          )
          
          # need confirm <- bug fix?
          # from NONMEM 7.2, the diagonal elements are square root variance from covariance matrix
          cor[cor$NAME==cor$param, "value"] <- 1
          
          cor$value <- as.numeric(cor$value)
          
          cor <- dplyr::filter(cor, !(NAME %in% cor_x_ex())) %>% 
            dplyr::filter(!(param %in% cor_y_ex()))
          
          
          plotly::layout(
            plotly::ggplotly({
              ggplot(data=cor, aes(NAME, param, fill = value,
                                   text = paste("x: ", NAME,
                                                "<br>y: ", param,
                                                "<br>value: ", value)
              ))+
                geom_tile() +
                #geom_text(aes(label=round(value,3)), size = 3, color="#FFCC99") +
                scale_fill_gradient2(low = "#66CCCC", high = "#FF6666", mid = "#FFCC99", 
                                     midpoint = 0, space = "Lab", 
                                     name="level") +
                labs(x="", y="") +
                theme_light() + # minimal theme
                theme(
                  plot.margin = margin(0, 0, 0, 0, "cm"),
                  rect = ggplot2::element_rect(fill = "transparent"),
                  text = ggplot2::element_text(colour='grey60'),
                  axis.text = ggplot2::element_text(colour='grey60'),
                  axis.ticks = ggplot2::element_line(colour='grey60', size=0.1),
                  strip.background = ggplot2::element_blank(),
                  strip.text = ggplot2::element_text(colour = 'grey60'),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                  plot.background = ggplot2::element_blank(),
                  legend.background = ggplot2::element_blank(),
                  panel.background = ggplot2::element_blank(),
                  panel.grid.major = ggplot2::element_line(colour='grey60', size=0.1),
                  panel.grid.minor = ggplot2::element_line(colour='grey60', size=0.1)
                )
            }, tooltip = "text"),
            plot_bgcolor  = "rgba(0, 0, 0, 0)",
            paper_bgcolor = "rgba(0, 0, 0, 0)"
          )
        } else { empty_plot("correlation matrix (.cor file)","is not available") }
        
        
        
        
      })
      
      
    }
  )
}

cov_mat_server <- function(id, prm_df, cov_x_ex, cov_y_ex){
  
  moduleServer(
    id, function(input, output, session){
      
      cov_mat <- shiny::reactive({
        
        if (shiny::isTruthy( prm_df()$cov )){
          # covariance matrix
          cov <- data.frame(prm_df()$cov, check.names = FALSE) # load phi file and remove subject number column
          
          # ordering as in the order as it was generated
          cov$NAME <- factor(cov$NAME, levels = cov$NAME)
          cov <- reshape2::melt(
            cov, # data input
            id.vars=c("NAME"),
            variable.name="param",
            value.name='value'
          )
          
          cov$value <- as.numeric(cov$value)
          cov$rescale <- scales::rescale(cov$value, to=c(-1,1)) # ofv is rescaled between -1 &  1
          
          cov <- dplyr::filter(cov, !(NAME %in% cov_x_ex())) %>% 
            dplyr::filter(!(param %in% cov_y_ex()))
          
          
          plotly::layout(
            plotly::ggplotly({
              ggplot(data=cov, aes(NAME, param, fill = value,
                                   text = paste("x: ", NAME,
                                                "<br>y: ", param,
                                                "<br>value: ", value)
              ))+
                geom_tile() + # load correlation matrix from cor
                #geom_text(aes(label=round(value,3)), size = 3, color="#FFCC99") +
                
                scale_fill_gradient2(low = "#66CCCC", high = "#FF6666", mid = "#FFCC99", 
                                     midpoint = 0, space = "Lab", 
                                     name="level", limits=c(-1,1)) +
                labs(x="", y="") +
                theme_light() + # minimal theme
                theme(
                  plot.margin = margin(0, 0, 0, 0, "cm"),
                  rect = ggplot2::element_rect(fill = "transparent"),
                  text = ggplot2::element_text(colour='grey60'),
                  axis.text = ggplot2::element_text(colour='grey60'),
                  axis.ticks = ggplot2::element_line(colour='grey60', size=0.1),
                  strip.background = ggplot2::element_blank(),
                  strip.text = ggplot2::element_text(colour = 'grey60'),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                  plot.background = ggplot2::element_blank(),
                  legend.background = ggplot2::element_blank(),
                  panel.background = ggplot2::element_blank(),
                  panel.grid.major = ggplot2::element_line(colour='grey60', size=0.1),
                  panel.grid.minor = ggplot2::element_line(colour='grey60', size=0.1)
                )
            }, tooltip = "text"),
            plot_bgcolor  = "rgba(0, 0, 0, 0)",
            paper_bgcolor = "rgba(0, 0, 0, 0)"
          )
        } else { empty_plot("covariance matrix (.cov file)","is not available") }
        
        
       
        
      })
      
      
      
    }
  )
}





coi_mat_server <- function(id, dir, mod_selected, coi_x_ex, coi_y_ex){
  
  moduleServer(
    id, function(input, output, session){
      
      coi_mat <- shiny::reactive({
        
        
        if (base::file.exists( paste0(dir(),'/',mod_selected(),".coi") )){
          
          # covariance matrix
          coi <- readLines(paste0(dir(),'/',mod_selected(),".coi")) # read data skipping first line
          coi <- coi[-1]
          coi <- as.data.frame(coi)
          coi <- strsplit(coi[,1],"\\s+")
          coi <- matrix(unlist(coi), nrow = length(coi), byrow = TRUE)
          coi <- as.data.frame(coi[,-1])
          colnames(coi) <- c(coi[1,])
          coi <- data.frame(coi[-1,], row.names = NULL, check.names = FALSE)
          
          # ordering as in the order as it was generated
          coi$NAME <- factor(coi$NAME, levels = coi$NAME)
          coi <- reshape2::melt(
            coi, # data input
            id.vars=c("NAME"),
            variable.name="param",
            value.name='value'
          )
          
          coi$value <- as.numeric(coi$value)
          coi$rescale <- scales::rescale(coi$value, to=c(-1,1)) # rescaled between -1 &  1
          
          
          coi <- dplyr::filter(coi, !(NAME %in% coi_x_ex())) %>% 
            dplyr::filter(!(param %in% coi_y_ex()))
          
          plotly::layout(
            plotly::ggplotly({
              ggplot(data=coi, aes(NAME, param, fill = value,
                                   text = paste("x: ", NAME,
                                                "<br>y: ", param,
                                                "<br>value: ", value)
              ))+
                geom_tile() + # load correlation matrix from cor
                #geom_text(aes(label=scales::scientific(value,2)), size = 3, color="#FFCC99") +
                
                scale_fill_gradient2(low = "#66CCCC", high = "#FF6666", mid = "#FFCC99", 
                                     midpoint = 0, space = "Lab", 
                                     name="level", limits=c(-1,1)) +
                labs(x="", y="") +
                theme_light() + # minimal theme
                theme(
                  plot.margin = margin(0, 0, 0, 0, "cm"),
                  rect = ggplot2::element_rect(fill = "transparent"),
                  text = ggplot2::element_text(colour='grey60'),
                  axis.text = ggplot2::element_text(colour='grey60'),
                  axis.ticks = ggplot2::element_line(colour='grey60', size=0.1),
                  strip.background = ggplot2::element_blank(),
                  strip.text = ggplot2::element_text(colour = 'grey60'),
                  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                  plot.background = ggplot2::element_blank(),
                  legend.background = ggplot2::element_blank(),
                  panel.background = ggplot2::element_blank(),
                  panel.grid.major = ggplot2::element_line(colour='grey60', size=0.1),
                  panel.grid.minor = ggplot2::element_line(colour='grey60', size=0.1)
                )
            }, tooltip = "text"),
            plot_bgcolor  = "rgba(0, 0, 0, 0)",
            paper_bgcolor = "rgba(0, 0, 0, 0)"
          )  
        } else { empty_plot("inverse covariance matrix (.coi file)","is not available") }
        
        
        
        
        
      })
      
      
      
    }
  )
}



