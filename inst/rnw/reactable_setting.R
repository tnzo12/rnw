# source for reactable theme

# basic reacatable options
options(reactable.theme = reactable::reactableTheme(
  backgroundColor = "transparent",
  inputStyle = list(backgroundColor = "transparent"),
  pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
  pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
))

# reactable theme for designated table
reactable_theme <- reactable::reactableTheme(
  backgroundColor = 'transparent',
  borderColor = 'rgba(102,102,102,0.15)',
  borderWidth = '1px'
)

# mbar charts for parameter estimation results
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
