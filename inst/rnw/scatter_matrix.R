scat_mat_server <- function(id, xpdb, scat_vars, scat_fact, scat_color, scat_button, scat_method){
  
  moduleServer(id, function(input, output, session) {

    scat_mat <- shiny::eventReactive(scat_button(), {
      if( scat_button()==0 ){
        ggplot2::ggplot() +
          ggplot2::geom_text(color = 'gray65', aes(x=0, y=0.1, label = "no selected values"), size=3.5) +
          ggplot2::ylim(-1,1) +
          ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
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
          )
      } else {
        xpdb <- xpdb()
        scat_vars <- scat_vars()
        scat_fact <- scat_fact()
        scat_color <- scat_color()
        scat_method <- scat_method()
        
        df <- data.frame(xpdb$data$data)
        
        # factor conversion to the choosen value
        df[scat_fact] <- lapply(df[scat_fact], function(x){ as.factor(x) })
        
        lowerFn <- function(data, mapping, method = "lm", ...) {
          p <- ggplot2::ggplot(data = data, mapping = mapping) +
            ggplot2::geom_point(aes(color = if(scat_color==""){""}else{.data[[scat_color]]} ), size = 0.3) +
            ggplot2::geom_smooth(method = method, color = "#FFCC99", ...)
          p
        }
        
        
        p_mat <- GGally::ggpairs(df, aes(color = if(scat_color==""){""}else{.data[[scat_color]]} ), # colorize
                                 columns = scat_vars,
                                 axisLabels = "show",
                                 lower = list(combo = GGally::wrap("facethist", color = "#999999"),
                                              continuous = GGally::wrap(lowerFn, method = scat_method)),
                                 upper = list(combo = GGally::wrap("box", color="#999999"),
                                              continuous = GGally::wrap("cor", color = "#999999")),
                                 diag = list(continuous = GGally::wrap("densityDiag", color = "#999999"),
                                             discrete = GGally::wrap("barDiag", color = "#999999"),
                                             na = "naDiag"),
                                 switch = "both") +
          ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
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
                         strip.text = ggplot2::element_text(colour = 'grey60'))
        p_mat
      }
        
      }, ignoreNULL = FALSE)
    
      
  
        
  })
  
}
