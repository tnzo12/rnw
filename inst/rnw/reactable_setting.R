# source for reactable theme

# basic reacatable options
options(reactable.theme = reactable::reactableTheme(
  backgroundColor = "transparent",
  cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
  inputStyle = list(backgroundColor = "transparent"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))

# reactable theme for designated table 
reactable_theme <- reactable::reactableTheme(
  cellStyle = list(display = "flex", flexDirection = "column", justifyContent = "center"),
  backgroundColor = 'transparent',
  borderColor = 'rgba(102,102,102,0.15)',
  borderWidth = '1px'
)


# bar charts for nca results
bar_style <- function(width = 1, fill = "#e6e6e6", height = "75%", align = c("left", "right"), color = NULL) {
  align <- match.arg(align)
  if (align == "left") {
    position <- paste0(width * 100, "%")
    image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
  } else { # in case when 'align' is right
    position <- paste0(100 - width * 100, "%")
    image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
  }
  list(
    backgroundImage = image,
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = color
  )
}


# bar charts for parameter estimation results
bar_chart_pos_neg <- function(label, value, max_value = 1, height = "10px",
                              pos_fill = "#FF6666", neg_fill = "#66CCCC", ade_fill = "#FFCC99") {
  neg_chart <- htmltools::div(style = list(flex = "1 1 0"))
  pos_chart <- htmltools::div(style = list(flex = "1 1 0"))
  width <- paste0(abs(value / max_value) * 100, "%")
  
  if (value < 0) {
    bar <- htmltools::div(style = list(marginLeft = "4px", background = ifelse(value>=-0.25, ade_fill, neg_fill), width = width, height = height))
    chart <- htmltools::div(style = list(display = "flex", alignItems = "center", justifyContent = "flex-end"), label, bar)
    neg_chart <- tagAppendChild(neg_chart, chart)
  } else {
    bar <- htmltools::div(style = list(marginRight = "4px", background = ifelse(value<=0.25, ade_fill, pos_fill), width = width, height = height))
    chart <- htmltools::div(style = list(display = "flex", alignItems = "center"), bar, label)
    pos_chart <- tagAppendChild(pos_chart, chart)
  }
  
  htmltools::div(style = list(display = "flex"), neg_chart, pos_chart)
}

# rhandsontable css settings
css <- "
.handsontable tbody th.ht__highlight,
.handsontable thead th.ht__highlight {
  background-color: #ffb347;
}

.handsontable.ht__selection--columns thead th.ht__highlight,
.handsontable.ht__selection--rows tbody th.ht__highlight {
  background-color: #ffb347;
  color: #FFF;
}

.wtBorder {
  background-color: #ffb347!important;
}

.handsontable td.area {
  background: -moz-linear-gradient(top,  rgba(181,209,255,0.34) 0%, rgba(181,209,255,0.34) 100%); /* FF3.6+ */
  background: -webkit-gradient(linear, left top, left bottom, color-stop(0%,rgba(181,209,255,0.34)), color-stop(100%,rgba(181,209,255,0.34))); /* Chrome,Safari4+ */
  background: -webkit-linear-gradient(top,  rgba(181,209,255,0.34) 0%,rgba(181,209,255,0.34) 100%); /* Chrome10+,Safari5.1+ */
  background: -o-linear-gradient(top,  rgba(181,209,255,0.34) 0%,rgba(181,209,255,0.34) 100%); /* Opera 11.10+ */
  background: -ms-linear-gradient(top,  rgba(181,209,255,0.34) 0%,rgba(181,209,255,0.34) 100%); /* IE10+ */
  background: linear-gradient(to bottom,  rgba(181,209,255,0.34) 0%,rgba(181,209,255,0.34) 100%); /* W3C */
  filter: progid:DXImageTransform.Microsoft.gradient( startColorstr='#57b5d1ff', endColorstr='#57b5d1ff',GradientType=0 ); /* IE6-9 */
  background-color: #673800;
}


.htContextMenu table tbody tr td {
  background: #7a8188;
  color: white;
  border-width: 0;
  border-radius: 0px;
  cursor: pointer;
  overflow: hidden;
  white-space: nowrap;
  text-overflow: ellipsis;
}

.htContextMenu table tbody tr td.current,
.htContextMenu table tbody tr td.zeroclipboard-is-hover {
  background: #59626A;
}

.handsontable th {
  background-color: #7a8188;
  color: white;
  font-size: 14px;
  border-radius: 3px;
}

.handsontable td {
  background-color: #59626A;
  color: white;
  font-size: 14px;
  border-radius: 3px;
}

.handsontableInput {
  border:none;
  outline-width: 0;
  margin: 0;
  font-family: inherit;
  line-height: 21px;
  font-size: inherit;
  resize: none;
  box-shadow: 0 0 0 2px #ffb347 inset;
  /*below are needed to overwrite stuff added by jQuery UI Bootstrap theme*/
  display: inline-block;
  color: white;
  border-radius: 4px;
  background-color: transparent;
}

"

# setting for empty plot
empty_plot <- function(title1 = NA, title2 = NA){
  plotly::layout(
    plotly::ggplotly(
      ggplot2::ggplot() +
        ggplot2::geom_text(color = 'gray65', aes(x=0, y=0.1, label = title1), size=3.5) +
        ggplot2::geom_text(color = 'gray65', aes(x=0, y=-0.1, label = title2), size=3.5) +
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
      
    ),
    plot_bgcolor  = "rgba(0, 0, 0, 0)",
    paper_bgcolor = "rgba(0, 0, 0, 0)"
  )
  
}

