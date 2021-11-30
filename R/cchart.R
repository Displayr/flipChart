#' Creates charts
#'
#' @param chart.type The name of plot to create, e.g \code{\link[flipStandardCharts]{Area}}, \code{\link[flipStandardCharts]{Bar}}, \code{\link[flipStandardCharts]{BarPictograph}}, \code{\link[flipStandardCharts]{Bean}}, \code{\link[flipStandardCharts]{Box}}, \code{\link[flipStandardCharts]{Column}}, \code{\link[flipStandardCharts]{Density}}, \code{\link[flipStandardCharts]{Donut}}, \code{\link[flipStandardCharts]{GeographicMap}}, \code{\link[flipStandardCharts]{Heat}}, \code{\link[flipStandardCharts]{Histogram}}, \code{\link[flipStandardCharts]{Line}}, \code{\link[flipStandardCharts]{Palm}}, \code{\link[flipStandardCharts]{Pie}}, \code{\link[flipStandardCharts]{Pyramid}}, \code{\link[flipStandardCharts]{Radar}}, \code{\link[flipStandardCharts]{Scatter}}, \code{\link[flipStandardCharts]{Stream}}, \code{\link[flipStandardCharts]{TimeSeries}}, \code{\link[flipStandardCharts]{Venn}}, \code{\link[flipStandardCharts]{Violin}}. Spaces will automatically be stripped.
#' @param x The data to be plotted.
#' @param small.multiples Logical; Whether each series should be shown in its own panel. When this is true, parameters from \code{\link[flipStandardCharts]{SmallMultiples}} can be used (e.g. \code{nrows}, \code{x.order}, \code{share.axes}, \code{average.show}, \code{average.color}, \code{panel.title.show}).
#' @param multi.color.series Logical; Indicates whether multiple colors will be shown in a Bar or Column chart with a single series. By default this is off and different colors are used to distinguish between different series. However, when chart.type is "Pyramid", then \code{multi.color.series} is always \code{true}.
#' @param font.units One of "px" or "pt"
#' @param ... Arguments to the function \code{chart.type}. See documentation for specific chart types or see details below.
#' @param warn.if.no.match Logical; If TRUE, a warning is shown if any arugments are not matched.
#' @param append.data Logical; If TRUE, extra information is appended to the chart object which is used for exporting. These are appended as attributes
#' \describe{\item{ChartData}{This is the data passed to the charting functions after tidying by \code{PrepareData}. It is used for both Excel and Powerpoint exporting.}
#' \item{ChartSettings}{This a named list so that powerpoint chart can use the same visualization settings. Information about the structure of this list is \href{https://wiki.q-researchsoftware.com/wiki/PptChartSettings}{here}.}
#' \item{ChartWarning}{This attribute is set if the chart cannot be properly exported to Excel. It is set to a string which is shown as a warning message on export in Displayr.}
#' \item{ChartType}{This attribute is actually always set, even if \code{append.data} if not true. It is the name of the chart type as used in PowerPoint and can vary depending on the visualization settings used.}}
#' @details Parameters passed to the charting functions are listed below:
#' \describe{
#'     \itemize{\code{type} }{ For chart types \code{Area}, \code{Bar} and \code{Column}, this can be set to \code{"Stacked"} to show cumulative values.}
#'     \itemize{\code{grid.show} }{ Logical; whether to show grid lines.}
#'     \itemize{\code{opacity} }{ Opacity of bars as an alpha value (0 to 1).}
#'     \itemize{\code{colors} }{ Character; a vector containing one or more colors specified as hex codes.}
#'     \itemize{\code{fit.type} }{ Character; type of line of best fit. Can be one of "None", "Linear", "LOESS",
#'          "Friedman's super smoother" or "Cubic spline". This option is only valid for charts Area, Bar, Column, Line, Scatter, Pyramid.}
#'     \itemize{\code{fit.ignore.last} }{ Logical; whether to ignore the last data point in the fit.}
#'     \itemize{\code{fit.line.type} }{ Character; One of "solid", "dot", "dash, "dotdash", or length of dash "2px", "5px".}
#'     \itemize{\code{fit.line.colors} }{ Character; a vector containing one or more colors specified as hex codes.}
#'     \itemize{\code{fit.line.width} }{ Numeric; Line width of line of best fit.}
#'     \itemize{\code{fit.line.name} }{ Character; Name of the line of best fit, which will appear in the hovertext.}
#'     \itemize{\code{fit.line.opacity} }{ Opacity of trend line as an alpha value (0 to 1).}
#'     \itemize{\code{fit.CI.show} }{ Show 95\% confidence interval.}
#'     \itemize{\code{fit.CI.opacity} }{ Opacity of confidence interval ribbon as an alpha value (0 to 1).}
#'     \itemize{\code{fit.CI.colors} }{ Character; a vector containing one or more colors specified as hex codes.}
#'     \itemize{\code{title} }{ Character; chart title.}
#'     \itemize{\code{title.font.family} }{ Character; title font family. Can be "Arial Black",
#' "Arial", "Comic Sans MS", "Courier New", "Georgia", "Impact",
#' "Lucida Console", "Lucida Sans Unicode", "Marlett", "Symbol", "Tahoma",
#' "Times New Roman", "Trebuchet MS", "Verdana", "Webdings".}
#'     \itemize{\code{title.font.color} }{ Title font color as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{title.font.size} }{ Integer; Title font size; default = 10.}
#'     \itemize{\code{subtitle} }{ Character.}
#'     \itemize{\code{subtitle.font.color} }{ subtitle font color as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{subtitle.font.family} }{ Character; subtitle font family.}
#'     \itemize{\code{subtitle.font.size} }{ Integer; subtitle font size.}
#'     \itemize{\code{footer} }{ Character to show below the chart.}
#'     \itemize{\code{footer.font.color} }{ footer font color as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{footer.font.family} }{ Character; footer font family.}
#'     \itemize{\code{footer.font.size} }{ Integer; footer font size.}
#'     \itemize{\code{footer.wrap} }{ Logical; whether the footer text should be wrapped.}
#'     \itemize{\code{footer.wrap.nchar} }{ Number of characters (approximately) in each
#' line of the footer when \code{footer.wrap} \code{TRUE}.}
#'     \itemize{\code{background.fill.color} }{ Background color in character format (e.g. "black").}
#'     \itemize{\code{legend.show} }{ Logical; show the legend.}
#'     \itemize{\code{legend.wrap} }{ Logical; whether the legend text should be wrapped.}
#'     \itemize{\code{legend.wrap.nchar} }{ Number of characters (approximately) in each
#' line of the legend when \code{legend.wrap} \code{TRUE}.}
#'     \itemize{\code{legend.fill.color} }{ Legend fill color as a named color in character format
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).}
#'     \itemize{\code{legend.fill.opacity} }{ Legend fill opacity as an alpha value (0 to 1).}
#'     \itemize{\code{legend.ascending} }{ Logical; TRUE for ascending, FALSE for descending.
#' By default, we set it to to FALSE if the chart is stacked and TRUE otherwise.}
#'     \itemize{\code{legend.border.color} }{ Legend border color as a named color in character
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).}
#'     \itemize{\code{legend.border.line.width} }{ Width in pixels of the border
#' around the legend.  0 = no border.}
#'     \itemize{\code{legend.position.x} }{ A numeric controlling the position of the legend. Values range from -0.5 (left) to 1.5 (right).}
#'     \itemize{\code{legend.position.y} }{ A numeric controlling the position of the legend. Values range from 0 (bottom) to 1 (top).}
#'     \itemize{\code{legend.font.color} }{ Legend font color as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{legend.font.family} }{ Character; legend font family.}
#'     \itemize{\code{legend.font.size} }{ Integer; Legend font size.}
#'     \itemize{\code{legend.width} }{ Only used in a TimeSeries chart. Integer; Width (in pixels) of the textbox containing the legend.}
#'     \itemize{\code{values.title} }{ Character; title of the axis showing the independent values, i.e. y-axis (in a column or area chart) or x-axis (in a bar chart). This will override dim names read from the input table.}
#'     \itemize{\code{values.title.font.color} }{ values-axis title font color as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{values.title.font.family} }{ Character; values-axis title font family.}
#'     \itemize{\code{values.title.font.size} }{ Integer; values-axis title font size.}
#'     \itemize{\code{values.line.width} }{ values-axis line width in pixels (0 = no line).}
#'     \itemize{\code{values.line.color} }{ values-axis line color as a named color in character format.
#' (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).}
#'     \itemize{\code{values.tick.mark.length} }{ Length of tick marks in pixels. Ticks are only shown when \code{y.line.width > 0}.}
#'     \itemize{\code{values.bounds.minimum} }{ Minimum of range for plotting; NULL = no manual range set.}
#'     \itemize{\code{values.bounds.maximum} }{ Maximum of range for plotting; NULL = no manual range set.}
#'     \itemize{\code{values.tick.distance} }{ Distance between tick marks. Requires that \code{values.bounds.minimum} and \code{values.bounds.maximum} have been set.}
#'     \itemize{\code{values.number.ticks} }{ Only used in Stream. The total number of ticks on the y-axis.}

#'     \itemize{\code{values.zero} }{ Whether the values-axis should include zero. If false, the range will be determined from the input data.}
#'     \itemize{\code{values.zero.line.width} }{ Width in pixels of zero line;.}
#'     \itemize{\code{values.zero.line.color} }{ Color of horizontal zero line as a named.
#' color in character format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).}
#'     \itemize{\code{values.data.reversed} }{ Logical; whether to reverse values-axis or not.}
#'     \itemize{\code{values.grid.width} }{ Width of values-grid lines in pixels; 0 = no line.}
#'     \itemize{\code{values.grid.color} }{ Color of values-grid lines as a named color in character.
#' format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, maxColorValue = 255)).}
#'     \itemize{\code{values.tick.show} }{ Whether to display the values-axis tick labels.}
#'     \itemize{\code{values.tick.suffix} }{ values-axis tick label suffix.}
#'     \itemize{\code{values.tick.prefix} }{ values-axis tick label prefix.}
#'     \itemize{\code{values.tick.format} }{ A string representing a d3 formatting code. See \url{https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format}.}
#'     \itemize{\code{values.hovertext.format} }{ A string representing a d3 formatting code. See \url{https://github.com/d3/d3/blob/master/API.md#number-formats-d3-format}.}
#'     \itemize{\code{values.tick.angle} }{ values-axis tick label angle in degrees 90 = vertical; 0 = horizontal.}
#'     \itemize{\code{values.tick.font.color} }{ values-axis tick label font color as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{values.tick.font.family} }{ Character; values-axis tick label font family.}
#'     \itemize{\code{values.tick.font.size} }{ Integer; values-axis tick label font size.}
#'     \itemize{\code{categories.title} }{ Character; title of the axis showing the dependent values, i.e. x-axis (in a column or area chart) or y-axis (in a bar chart). This will override dim names read from the input table.}
#'     \itemize{\code{categories.title.font.color} }{ categories-axis title font color as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{categories.title.font.family} }{ Character; categories-axis title font family.}
#'     \itemize{\code{categories.title.font.size} }{ Integer; categories-axis title font size.}
#'     \itemize{\code{categories.line.width} }{ categories-axis line in pixels, 0 = no line.}
#'     \itemize{\code{categories.line.color} }{ categories-axis line color as a named color in character format (e.g. "black") or an rgb value (e.g. rgb(0, 0, 0, macategories.olorValue = 255)).}
#'     \itemize{\code{categories.tick.marks} }{ Character; whether and where to show tick marks on the categories.axis. Can be "outside", "inside", "none".}
#'     \itemize{\code{categories.tick.mark.length} }{ Length of tick marks in pixels.}
#'     \itemize{\code{categories.tick.units} }{ Only used in Stream. "Automatic", "Number", "Day", "Month" or "Year".}
#'     \itemize{\code{categories.tick.interval} }{ Only used in Stream. The frequency of ticks on the x-axis. Where the data crosses multiple years, re-starts at each year.}
#'     \itemize{\code{categories.bounds.minimum} }{ Minimum of range for plotting; NULL = no manual range set.  Must be less than categories.bounds.maximum.}
#'     \itemize{\code{categories.bounds.maximum} }{ Maximum of range for plotting; NULL = no manual range set.  Must be greater than categories.bounds.minimum.}
#'     \itemize{\code{categories.tick.distance} }{ Tick mark distance in categories.axis units between minimum and maximum for plotting; NULL = no manual range set.}
#'     \itemize{\code{categories.zero} }{ Whether the categories-axis should include zero. If false, the range will be determined from the input data.}
#'     \itemize{\code{categories.zero.line.width} }{ Width in pixels of zero line.}
#'     \itemize{\code{categories.zero.line.color} }{ Color of horizontal zero (origin) line as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{categories.data.reversed} }{ Logical; whether to reverse categories-axis or not.}
#'     \itemize{\code{categories.grid.width} }{ Width of y-grid lines in pixels; 0 = no line.}
#'     \itemize{\code{categories.grid.color} }{ Color of y-grid lines as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{categories.tick.show} }{ Whether to display the categories-axis tick labels.}
#'     \itemize{\code{categories.tick.suffix} }{ categories-axis tick label suffix.}
#'     \itemize{\code{categories.tick.prefix} }{ categories-axis tick label prefix.}
#'     \itemize{\code{categories.tick.format} }{ A string representing a d3 formatting code. See \url{https://github.com/mbostock/d3/wiki/Formatting#numbers}.}
#'     \itemize{\code{categories.hovertext.format} }{ A string representing a d3 formatting code. See \url{https://github.com/mbostock/d3/wiki/Formatting#numbers}.}
#'     \itemize{\code{categories.tick.angle} }{ categories-axis tick label angle in degrees. 90 = vertical; 0 = horizontal.}
#'     \itemize{\code{categories.tick.font.color} }{ categories-axis tick label font color as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{categories.tick.font.family} }{ Character; categories-axis tick label font family.}
#'     \itemize{\code{categories.tick.font.size} }{ Integer; categories-axis tick label font size.}
#'     \itemize{\code{categories.tick.label.wrap} }{ Logical; whether to wrap long labels on the categories-axis.}
#'     \itemize{\code{categories.tick.label.wrap.nchar} }{ Integer; number of characters in each line when \code{label.wrap} is \code{TRUE}.}
#'     \itemize{\code{tooltip.show} }{ Logical; whether to show a tooltip on hover.}
#'     \itemize{\code{modebar.show} }{ Logical; whether to show the zoom menu buttons or not.}
#'     \itemize{\code{global.font.family} }{ Character; font family for all occurrences of any
#' font attribute for the chart unless specified individually.}
#'     \itemize{\code{global.font.color} }{ Global font color as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{data.label.show} }{ Logical; whether to show data labels.}
#'     \itemize{\code{data.label.font.family} }{ Character; font family for data label.}
#'     \itemize{\code{data.label.font.size} }{ Integer; Font size for data label.px.}
#'     \itemize{\code{data.label.font.color} }{ Font color as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{data.label.format} }{ A string representing a d3 formatting code. See \url{https://github.com/mbostock/d3/wiki/Formatting#numbers}.}
#'     \itemize{\code{data.label.prefix} }{ Character; prefix for data values.}
#'     \itemize{\code{data.label.suffix} }{ Character; suffix for data values.}
#'     \itemize{\code{data.label.threshold} }{ The proportion of the total range below which data labels should not be displayed. Only applicable for pie, bar and column charts.}
#'     \itemize{\code{margin.top} }{ Margin between plot area and the top of the graphic in pixels.}
#'     \itemize{\code{margin.bottom} }{ Margin between plot area and the bottom of the graphic in pixels.}
#'     \itemize{\code{margin.left} }{ Margin between plot area and the left of the graphic in pixels.}
#'     \itemize{\code{margin.right} }{ Margin between plot area and the right of the graphic in pixels.}
#'     \itemize{\code{margin.inner.pad} }{ Padding in pixels between plot proper and axis lines.}
#'     \itemize{\code{background.fill.opacity} }{ Background opacity as an alpha value (0 to 1).}
#'     \itemize{\code{charting.area.fill.color} }{ Charting area background color as a named color in character format (e.g. "black") or hex code.}
#'     \itemize{\code{charting.area.fill.opacity} }{ Charting area background opacity as an alpha value (0 to 1).}
#'     \itemize{\code{line.thickness} }{ Only used in Scatter, Line Area or TimeSeries charts. Controls the line thickness in pixels.}
#'     \itemize{\code{line.colors} }{ Character; a vctor containing one or more colors specified as hex codes.}
#'     \itemize{\code{line.thickness} }{ Opacity of series lines as an alpha value (0 to 1).}
#'     \itemize{\code{marker.show} }{ Only used in Scatter, Line or Area charts. Can be "none", "automatic" or a vector referencing the plotly symbol dictionary using either numerics or strings.}
#'     \itemize{\code{marker.colors} }{ Only works if \code{marker.show} is true. Character; a vector containing one or more colors specified as hex codes.}
#'     \itemize{\code{marker.opacity} }{ Only works if \code{marker.show} is true. Controls ppacity for markers as an alpha value (0 to 1).}
#'     \itemize{\code{marker.size} }{ Only works if \code{marker.show} is true. Controls size in pixels of marker.}
#'     \itemize{\code{marker.border.width} }{ Width in pixels of border/line around series bars; 0 is no line.}
#'     \itemize{\code{marker.border.colors} }{ Character; a vector containing one or more colors specified as hex codes.}
#'     \itemize{\code{marker.border.opacity} }{ Opacity of border around bars as an alpha value (0 to 1).}

#'     \itemize{\code{bar.gap} }{ Only used in charts Bar, Column and Pyramid. Chart proportion between each bar or column if using bar or column charts, or between each cluster of bars or columns.}
#'     \itemize{\code{weights} }{ Only used in Bean, Box, Density, Histogram, Violin plots or Venn diagrams. An optional \code{\link{list}}, where each element is a vector containing weights corresponding to the values of \code{x}, or, a vector where the weights is assumed applicable for each element in \code{x}.}
#'     \itemize{\code{vertical} }{ Only used in Bean, Box, Density, Histogram or Violin plots Display the densities vertically.}
#'     \itemize{\code{show.mean} }{ Only used in Violin plots. Displays the mean of the data.}
#'     \itemize{\code{mean.color} }{ Defaults to "white".}
#'     \itemize{\code{show.median} }{ Only used in Violin plots. Displays the median of the data.}
#'     \itemize{\code{median.color} }{ Defaults to "black".}
#'     \itemize{\code{show.quartiles} }{ Only used in Violin plots. Displays the quartiles of the data.}
#'     \itemize{\code{quartile.color} }{ Defaults to "black".}
#'     \itemize{\code{show.range} }{ Only used in Violin plots. Displays the range of the data.}
#'     \itemize{\code{range.color} }{ Defaults to "black".}
#'     \itemize{\code{show.values} }{ Only used in Box, Density or Histogram (always true in Bean plots). Produces a rug plot of individual values.}
#'     \itemize{\code{values.color} }{ Defaults to "Green".}
#'     \itemize{\code{show.density} }{ Only used in Violin plots. Show the left or top (if \code{vertical} is FALSE) of the violin plot.}
#'     \itemize{\code{density.color} }{ Only used in Bean, Box, Density, Histogram or Violin plots. Defaults to "Green".}
#'     \itemize{\code{bw} }{ The smoothing bandwidth to be used when creating a Density, Bean, or Violin plot. This defaults to \code{"nrd0"}, whereas \code{"SJ"} may often be superior (see \code{\link{density}}). The default is to \code{"nrd0"} as \code{"SJ"} fails with trivial categorical cases.}
#'     \itemize{\code{adjust} }{ A scaling factor for the bandwidth when creating a Density, Bean, or Violin plot. E.g., a value of 0.5 sets the bandwidth to have of that computed using \code{bw}.}
#'     \itemize{\code{kernel} }{ The kernel used when when creating a Density, Bean, or Violin plot. One of "gaussian" (the default), "epanechnikov", "rectangular", "triangular", "biweight", "cosine", "optcosine".}
#'     \itemize{\code{n} }{ The number of equally-spaced points at which the density is to be estimated when creating a Density, Bean, or Violin plot. If greater than 512, it is rounded to a power of 2 (see \code{link{density}})}.
#'     \itemize{\code{from} }{ The left-most point of the grid used when creating a Density, Bean, or Violin plot.}
#'     \itemize{\code{to} }{ The right-most point of the grid used when creating a Density, Bean, or Violin plot.}
#'     \itemize{\code{cut} }{ By default, the values of \code{from} and \code{to} are \code{cut} bandwidths beyond the extremes of the data.}
#'     \itemize{\code{automatic.lower.density} }{ When \code{TRUE}, which is the default, \code{from} is set to the lowest value in the data.}
#'     \itemize{\code{histogram.cumulative} }{ Only used in Histogram plots. Plots the cumulative histogram, if set to TRUE.}
#'     \itemize{\code{histogram.counts} }{ Only used in Histogram plots. Displays the counts in tooltips of a histogram, rather than the proportions.}
#'     \itemize{\code{maximum.bins} }{ Only used in Histograph plots. The maximum number of bins of the histogram. If \code{NULL}, this is generated automatically.}
#'     \itemize{\code{box.points} }{ How outliers are displayed boxplots. \code{"All"} plots all the points. \code{"Suspected outliers"} plots points between 1.5 and 3 IQR from the 1st and 3rd quartile with un-filled circles. \code{"Outliers"} does not plot points between 1.5 and 3 IQR from the 1st and 3rd quartiles.}

#'     \itemize{\code{country} }{ Only used in GeographicMap. Character string optionally stating the country that the states are from, if \code{map.type} is \code{"states"}.}
#'     \itemize{\code{high.resolution} }{ Only used in GeographicMap. Specifically request a high resolution map. Otherwise the resolution of the map is chosen automatically based on the resolution required for the requested countries or regions.}
#'     \itemize{\code{show.missing.regions} }{ Only used in GeographicMap. Logical; Whether to plot regions not included in \code{x} with values of \code{NA}. Used only when \code{mapping.package} is \code{"leaflet"}.}
#'     \itemize{\code{treat.NA.as.0} }{ Only used in GeographicMap. Plots any \code{NA} values in the data and any geographical entities without data as having a zero value.}
#'     \itemize{\code{ocean.color} }{ Only used in GeographicMap. The color used for oceans (or background).}
#'     \itemize{\code{mapping.package} }{ Only used in GeographicMap. Either \code{"leaflet"} (better graphics, wider range of
#' maps) or \code{"plotly"} (faster).}
#'     \itemize{\code{background} }{ Only used in GeographicMap. If \code{"mapping.package"} is \code{"leaflet"}, add a background
#' tile from opensteetmaps.}
#'     \itemize{\code{zip.country} }{ Only used in GeographicMap. One of \code{"Automatic"}, \code{"USA"}, \code{"UK"} or \code{"Australia"}. If \code{"Automatic"} an attempt is made to infer the country from the data.}
#'     \itemize{\code{sort.rows} }{ Only used in Heat. Whether to sort rows by their averages or link as a dendrogram. Options are \code{"None"}, \code{"Sort by averages (ascending)"}, \code{"Sort by averages (descending)"} and \code{"Dendrogram"}.}
#'     \itemize{\code{sort.columns} }{ Only used in Heat. Whether to sort columns by their averages or link as a dendrogram. Options are \code{"None"}, \code{"Sort by averages (ascending)"}, \code{"Sort by averages (descending)"} and \code{"Dendrogram"}.}
#'     \itemize{\code{standardization} }{ Only used in Heat. Whether to standardize the shading of rows or columns. Options are \code{"None"}, \code{"Standardize rows"} and \code{"Standardize columns"}.}
#'     \itemize{\code{left.columns} }{ Only used in Heat. An optional list of vectors or matrices to be appended to the left of the heatmap.}
#'     \itemize{\code{left.column.headings} }{ Only used in Heat. An optional comma separated string containing headings for \code{left.columns}. If not supplied, colnames of the items in \code{left.columns} are used.}
#'     \itemize{\code{right.columns} }{ Only used in Heat. An optional list of vectors or matrices to be appended to the right of the heatmap.}
#'     \itemize{\code{right.column.headings} }{ Only used in Heat. An optional comma separated string containing headings for
#' \code{right.columns}. If not supplied, colnames of the items in \code{right.columns} are used.}
#'     \itemize{\code{pie.subslice.colors} }{ A vector containing hex value colors for the outer ring of the pie chart. If not supplied will default to the same colors as the inner ring.}
#'     \itemize{\code{pie.subslice.colors.repeat} }{  Logical; if, when a grouped pie chart is displayed, the colors of the subslices should repeat by group, or be different throughout; defaults to TRUE.}
#'     \itemize{\code{pie.groups.font.family} }{  Character; font family for group labels.}
#'     \itemize{\code{pie.groups.font.size} }{ Font size for group labels.}
#'     \itemize{\code{pie.groups.font.color} }{ Font color as a named color or hex code.}
#'     \itemize{\code{pie.data.threshold} }{ Labels with values smaller than the theshold are not shown.}
#'     \itemize{\code{pie.border.color} }{ A single color for space around pie and between segments.}
#'     \itemize{\code{pie.inner.radius} }{ The size of the inner radius of pie and donut charts as a proportion out of 100. Defaults to 70.}
#'     \itemize{\code{scatter.x.column} }{ When \code{x} is a dataframe or matrix, the index of the column (1-based) which contains the x-coordinate data in a scatterplot.}
#'     \itemize{\code{scatter.y.column} }{ When \code{x} is a dataframe or matrix, the index of the column (1-based) which contains the y-coordinate data in a scatterplot. This is ignored if \code{scatter.y} is provided instead.}
#'     \itemize{\code{scatter.sizes.column} }{ When \code{x} is a dataframe or matrix, the index of the column (1-based) which contains \code{scatter.sizes} data. This is ignored if \code{scatter.sizes} is provided instead.}
#'     \itemize{\code{scatter.colors.column} }{ When \code{x} is a dataframe or matrix, the index of the column (1-based) which contains \code{scatter.colors} data. This is ignored if \code{scatter.colors} is provided instead.}
#'     \itemize{\code{scatter.groups.column} }{ Only used for small multiples (otherwise use \code{scatter.colors.column} with \code{scatter.colors.as.categorical = TRUE}). The index of the column of \code{x} which is used to aggregate the data for small multiples. By default this is the last column in \code{x}.}
#'     \itemize{\code{scatter.labels} }{ Optional vector for labelling scatter points. This should be the same length as the number of observations in x and y. If not provided, the rownames of \code{x} will be used instead.}
#'     \itemize{\code{scatter.labels.name} }{ Character; Used for labelling subtitles and footers if \code{scatter.labels} is provided}.
#'     \itemize{\code{scatter.sizes} }{ Numeric vector determining of the size of each observation. These can alternatively be provided as a column in \code{x}.}
#'     \itemize{\code{scatter.sizes.name} }{ Character; Used for labelling footers and legends if \code{scatter.sizes} is provided.}
#'     \itemize{\code{scatter.colors} }{ Numeric, character, or categorical vector determining the color of each observation. These can alternatively be provided as a column in \code{x}.}
#'     \itemize{\code{scatter.colors.name} }{ Character; Used for labelling footers if \code{scatter.colors} is provided.}
#'     \itemize{\code{scatter.colors.as.categorical} }{ Logical; Whether to treat colors as a categorical groups, or a numeric scale..}
#'     \itemize{\code{scatter.labels.as.hovertext} }{ Logical; if TRUE, labels are shown has hovers; otherwise, as a labeled scatterplot.}
#'     \itemize{\code{scatter.sizes.as.diameter} }{ Whether to show the points with diameter (instead of area, which is the default) proportional to the sizes variable. Cannot be used if \code{scatter.labels.as.hovertext = FALSE}.}
#'     \itemize{\code{scatter.max.labels} }{ Integer; the maximum number of labels to show on a Labeled Scatterplot, i.e. when \code{scatter.labels.as.hovertext = FALSE}.}
#'     \itemize{\code{trend.lines} }{ Logical; indicating whether to plot trend lines for multiple tables when \code{scatter.labels.as.hovertext = FALSE}.}
#'     \itemize{\code{logos} }{ Optional list of images to be used to label scatterplot instead of the row names. This should be input as a comma-seperated list of URLs. Only used when \code{scatter.labels.as.hovertext = FALSE}.}
#'     \itemize{\code{logo.size} }{ Numeric controlling the size of the logos.}

#'     \itemize{\code{range.bars} }{ Only used in TimeSeries. Logical; whether the data consists of a single series with low, value, high in the columns, or multiple series.}
#'     \itemize{\code{window.start} }{ Only used in TimeSeries. The number of days before the end of the data series to start the range selector window.}
#'     \itemize{\code{nrows} }{ Only used for small multiples. Integer; Number of rows to arrange the charts in.}
#'     \itemize{\code{x.order} }{ Only used for small multiples. A vector containing the list index of the columns in the order which they are to be shown.}
#'     \itemize{\code{share.axes} }{ Only used for small multiples. Force range of the plot to be the same across all panels.}
#'     \itemize{\code{average.show} }{ Only used for small multiples. Logical; whether to show a second series in each panel containing the data averaged across all series.}
#'     \itemize{\code{average.color} }{ Only used for small multiples. The color in which the average series should be displayed.}
#'     \itemize{\code{panel.title.show} }{ Only used for small multiples. Logical; whether to show a title for each panel.}
#'     \itemize{\code{panel.title.font.family} }{ Only used for small multiples. Font family of panel titles.}
#'     \itemize{\code{panel.title.font.color} }{ Only used for small multiples. Font color of panel titles as hex code.}
#'     \itemize{\code{panel.title.font.size} }{ Only used for small multiples. Font size of panel titles.}
#'     \itemize{\code{panel.title.wrap} }{ Only used for small multiples. Logical; whether the panel title should be wrapped.}
#'     \itemize{\code{panel.title.wrap.nchar} }{ Only used for small multiples. Number of characters (approximately) in each line
#'     of the panel title when \code{panel.title.wordwrap} \code{TRUE}.}
#'     \itemize{\code{pad.top} }{ Only used for small multiples. Numeric in [0,1]; Spacing above chart (between panels).}
#'     \itemize{\code{pad.bottom} }{ Only used for small multiples. Numeric in [0,1]; Spacing below chart (between panels).}
#'     \itemize{\code{pad.left} }{ Only used for small multiples. Numeric in [0,1]; Spacing to the left of chart (between panels).}
#'     \itemize{\code{pad.right} }{ Only used for small multiples. Numeric in [0,1]; Spacing to the right chart (between panels).}
#' }
#' @importFrom methods formalArgs
#' @importFrom flipStandardCharts ErrorIfNotEnoughData
#' @return A chart object that can be printed. Most often, a plotly object.
#' @export
#' @examples
#' x <- matrix(1:21, 7, 3, dimnames = list(letters[1:7], LETTERS[1:3]))
#' CChart("Column", x, colors = c("red", "green", "blue"), categories.title = "Categories")
#' CChart("Bar", x, type = "Stacked", colors = grey(1:3/3), categories.title = "Categories")
#' CChart("Area", x, small.multiples = TRUE,  colors = rainbow(3), categories.title = "Categories")
CChart <- function(chart.type, x, small.multiples = FALSE,
                   multi.color.series = FALSE, font.units = "px",
                   ..., warn.if.no.match = TRUE, append.data = FALSE)
{
    if (chart.type %in% c("Venn"))
        ErrorIfNotEnoughData(x, require.tidy = FALSE)
    if (chart.type == "Funnel")
        chart.type <- "Pyramid"
    if (multi.color.series && chart.type %in% c("Bar", "Column"))
        chart.type <- paste0(chart.type, "MultiColor")
    user.args <- if (small.multiples) list(chart.type = chart.type, ...)
                 else list(...)

    chart.function <- gsub(" ", "", chart.type)             # spaces always removed
    fun.and.pars <- getFunctionAndParameters(chart.function, small.multiples)
    if (tolower(font.units) %in% c("pt", "points"))
        user.args <- scaleFontSizes(user.args)

    # This needs to be called before categories/values is converted
    # into x/y axis but after font sizes have been converted to pixels
    if (append.data)
    {
        chart.settings <- getPPTSettings(chart.type, user.args, x)
        categories.title <- user.args$categories.title
        values.title <- user.args$values.title
    }

    user.args <- substituteAxisNames(chart.function, user.args)
    arguments <- substituteArgumentNames(fun.and.pars$parameters.o, user.args, warn.if.no.match)
    args <- paste0("c(list(", fun.and.pars$parameter.1, " = x), arguments)")

    if (!append.data)
        return(do.call(fun.and.pars$chart.function, eval(parse(text = args))))
    result <- do.call(fun.and.pars$chart.function, eval(parse(text = args)))
    result <- addChartTypeWarning(result, chart.type, small.multiples)
    result <- addLabels(result, user.args$title, categories.title, values.title)

    # Convert data after the charting function has been applied
    if (chart.type %in% c("Scatter", "Bubble"))
    {
        #result <- addScatterAxisWarning(result, x) # set warning before data conversion
        x <- convertChartDataToNumeric(x)
        chart.settings <- setScatterAxesBounds(chart.settings, x)
    }
    if (is.null(attr(result, "ChartData")))
        attr(result,  "ChartData") <- x # Used by Displayr to permit exporting of the raw data.
    class(result) <- c(class(result), "visualization-selector")
    attr(result,  "ChartSettings") <- chart.settings
    result
}

addLabels <- function(x, chart.title, categories.title, values.title)
{
    chart.labels <- attr(x, "ChartLabels") # for plotly standard charts this will be a list
    if (is.null(chart.labels))
        chart.labels <- list()
    if (any(nzchar(chart.title)))
        chart.labels$ChartTitle <- chart.title
    if (any(nzchar(categories.title)))
        chart.labels$PrimaryAxisTitle <- categories.title
    if (any(nzchar(values.title)))
        chart.labels$ValueAxisTitle <- values.title
    if (length(chart.labels) == 0)
        chart.labels <- NULL
    attr(x, "ChartLabels") <- chart.labels
    return(x)
}

addChartTypeWarning <- function(x, chart.type, small.multiples)
{
    export.type <- attr(x, "ChartType")
    warnings <- attr(x, "ChartWarning")
    msg <- ""

    if (small.multiples)
        msg <- "This visualization is a small multiple which is not supported by PowerPoint."
    else if (chart.type %in% c("Palm", "Stream", "Venn", "Pyramid", "BarPictograph"))
        msg <- paste0("This visualization is of type '", chart.type,
                      "' which is not supported by PowerPoint.")
    else if (export.type %in% c("Sunburst", "Histogram", "Filled Map", "Box & Whisker"))
    {
        tmp.type <- chart.type
        if (tmp.type == "Pie")
            tmp.type <- "2-dimensional Pie"
        msg <- paste0("This visualization is a ", tmp.type,
                    " chart which cannot be exported to PowerPoint. ")
        # The charts in the last condition have chart types supported by powerpoint 2016,
        # however they cannot be handled by the API for exporting used by Displayr
    }

    if (nzchar(msg))
        attr(x, "ChartWarning") <- paste(warnings, msg,
            "It will be exported to PowerPoint as an image.",
            "Set 'PowerPoint Export > Format' to 'Microsoft Chart' and select a",
            "supported chart type or set the export format to 'Image' to",
            "suppress this warning.", collapse = "")
    return(x)
}

addScatterAxisWarning <- function(result, data)
{
    warnings <- attr(result, "ChartWarning")
    msg <- ""

    .isValidIndex <- function(i) {return (!is.null(i) && !is.na(i) && i > 0 &&
                        i <= NCOL(data))}
    v.ind <- attr(data, "scatter.variable.indices")
    ind.x <- v.ind["x"]
    ind.y <- v.ind["y"]
    if (.isValidIndex(ind.x) && !is.numeric(data[,ind.x]))
        msg <- "Powerpoint only supports numeric axes in scatterplots"
    else if (.isValidIndex(ind.y) && !is.numeric(data[,ind.y]))
        msg <- "Powerpoint only supports numeric axes in scatterplots"

    if (nzchar(msg))
        attr(result, "ChartWarning") <- paste(warnings, msg,
            "It will be exported to PowerPoint as an image.",
            "Set 'PowerPoint Export > Format' to 'Microsoft Chart' and select a",
            "supported chart type or set the export format to 'Image' to",
            "suppress this warning.", collapse = "")
    return(result)
}


getPPTSettings <- function(chart.type, args, data)
{
    # Opacity is by default set to NULL in the javascript code
    tmp.opacity <- args$opacity
    tmp.is.stacked <- isTRUE(args$type == "Stacked")
    if (is.null(tmp.opacity))
    {
        if (chart.type %in% c("Area", "Radar", "Palm") && !tmp.is.stacked)
            tmp.opacity <- 0.4
        else if (chart.type == "Scatter" && isTRUE(attr(data, "scatter.variable.indices")["sizes"] <= NCOL(data)))
            tmp.opacity <- 0.4
        else
            tmp.opacity <- 1.0
    }

    tmp.line.style <- "None"
    if (chart.type %in% c("Donut", "Pie", "Palm"))
        tmp.line.style <- "Solid"
    else if (chart.type %in% c("Line", "Radar", "Time Series"))
        tmp.line.style <- if (is.null(args$line.type)) "Solid" else args$line.type
    else if (!is.null(args$marker.border.opacity))
        tmp.line.style <- "Solid"

    tmp.line.thickness <- 1
    if (chart.type %in% c("Line", "Radar", "Time Series"))
        tmp.line.thickness <- as.numeric(ConvertCommaSeparatedStringToVector(args$line.thickness))
    else if (chart.type %in% c("Pie", "Donut"))
        tmp.line.thickness <- 1
    else if (!is.null(args$marker.border.opacity))
        tmp.line.thickness <- args$marker.border.width
    tmp.line.thickness <- rep(px2pt(tmp.line.thickness), length = length(args$colors))

    tmp.line.color <- args$colors
    if (chart.type %in% c("Pie", "Donut"))
        tmp.line.color <- args$pie.border.color
    else if (!is.null(args$marker.border.opacity))
        tmp.line.color <- args$marker.border.color
    tmp.line.color <- rep(tmp.line.color, length = length(args$colors))

    tmp.data.label.show <- isTRUE(args$data.label.show)
    tmp.data.label.show.category.labels <- FALSE
    if (chart.type == "Scatter" && !isTRUE(args$scatter.labels.as.hovertext))
        tmp.data.label.show <- TRUE
    if (chart.type %in% c("Donut", "Pie"))
    {
        tmp.data.label.show <- TRUE
        tmp.data.label.show.category.labels <- TRUE
    }
    if (chart.type == "Radar" && tmp.data.label.show)
        tmp.data.label.show.category.labels <- TRUE
    if (chart.type == "Bar Pictograph" && isTRUE(args$data.label.position != "No")) 
        tmp.data.label.show <- TRUE

    # DataLabelsPosition not supported for Area Chart
    tmp.data.label.position <- "BestFit"
    if (chart.type == "Column" && tmp.is.stacked && !isTRUE(args$data.label.centered))
        tmp.data.label.position <- "InsideEnd"
    else if (chart.type %in% c("Donut", "Pie"))
        tmp.data.label.position <- "OutsideEnd"
    else if (chart.type == "Scatter")
        tmp.data.label.position <- "Center"

    # Behaviour of 'Automatically' set data label font colors
    # change depending on the chart type
    tmp.data.label.font.color <- ConvertCommaSeparatedStringToVector(args$data.label.font.color)
    if (chart.type %in% c("Line", "Scatter") && isTRUE(args$data.label.font.autocolor))
        tmp.data.label.font.color <- args$colors
    else if (tmp.is.stacked && isTRUE(args$data.label.font.autocolor))
    {
        if (chart.type == "Area")
            tmp.data.label.font.color <- c(autoFontColor(args$colors[-1]), args$global.font.color)
        else
            tmp.data.label.font.color <- autoFontColor(args$colors)
    }
    if (length(tmp.data.label.font.color) < length(args$colors))
        tmp.data.label.font.color <- rep(tmp.data.label.font.color,
               length = length(args$colors))


    # Initialise series-specific parameters
    if (isDistribution(chart.type))
    {
        series.settings <- list(list(
            ShowDataLabels = FALSE,
            BackgroundColor = args$density.color))

    } else if (chart.type == "Scatter" && !isTRUE(args$scatter.colors.as.categorical))
    {
        # When scatterplots use colors as a numerical scale
        # we can assume a single template series
        series.settings <- list(list(
            CustomPoints = getColorsAsNumericScale(data, args$colors, tmp.opacity),
            Marker = list(Size = args$marker.size, OutlineStyle = "None"),
            ShowDataLabels = tmp.data.label.show,
            DataLabelsPosition = "Center",
            DataLabelsFont = list(family = args$data.label.font.family,
                size = px2pt(args$data.label.font.size),
                color = tmp.data.label.font.color[1]),
            OutlineStyle = "None"))

    } else if (chart.type %in% c("BarMultiColor", "ColumnMultiColor",
               "Pyramid", "Bar Pictograph"))
    {
        # Multi-color series is implemented as a single series
        # with many CustomPoints
        user.colors <- args$colors
        if (length(user.colors) == 0)
            user.color <- ChartColors(NROW(data))
        tmp.colors <- list()
        for (i in seq_along(user.colors))
            tmp.colors[[i]] <- list(BackgroundColor = sprintf("%s%X",
                user.colors[i], round(tmp.opacity*255)), Index = i - 1)
        series.settings <- list(list(
            CustomPoints = tmp.colors,
            ShowDataLabels = tmp.data.label.show,
            DataLabelsFont = list(family = args$data.label.font.family,
                size = px2pt(args$data.label.font.size),
                color = tmp.data.label.font.color[1]),
            DataLabelsPosition = tmp.data.label.position,
            OutlineColor = tmp.line.color[1], # style is none if no border color defined
            OutlineWidth = tmp.line.thickness[1],
            OutlineStyle = tmp.line.style))

    } else
        series.settings <- lapply(1:length(args$colors),
        function(i) {list(
            BackgroundColor = sprintf("%s%X", args$colors[i], round(tmp.opacity*255)),
            ShowDataLabels = tmp.data.label.show,
            ShowCategoryNames = tmp.data.label.show.category.labels,
            DataLabelsFont = list(family = args$data.label.font.family,
                size = px2pt(args$data.label.font.size),
                color = tmp.data.label.font.color[i]),
            DataLabelsPosition = tmp.data.label.position,
            OutlineColor = tmp.line.color[i],
            OutlineWidth = tmp.line.thickness[i],
            OutlineStyle = tmp.line.style)})
    tmp.n <- length(series.settings)


    if ((chart.type == "Scatter" && isTRUE(args$scatter.colors.as.categorical)) || chart.type == "Line")
        for (i in 1:tmp.n)
            series.settings[[i]]$Marker = list(Size = args$marker.size,
                OutlineStyle = "None",
                BackgroundColor = sprintf("%s%X", args$colors[i], round(tmp.opacity*255)))

    # Initialise return output
    res <- list()
    if (tmp.n > 0)
        res$TemplateSeries = series.settings
    if (isFALSE(args$legend.show) || isTRUE(args$legend.show == "Hide") || tmp.n == 1)
        res$ShowLegend <- FALSE
    legend.position <- "TopRight"
    if (isTRUE(args$legend.orientation == "Horizontal"))
        legend.position <- "Bottom"
    if (isTRUE(args$legend.x.positon < 0.1))
        legend.position <- "Left"
    if (isTRUE(args$legend.y.position < 0.1))
        legend.position <- "Bottom"
    res$Legend = list(Font = list(color = args$legend.font.color,
            family = args$legend.font.family, size = px2pt(args$legend.font.size)),
            Position = legend.position)
    if (isTRUE(nchar(args$background.fill.color) > 0) &&
        args$background.fill.color != "transparent")
        res$BackgroundColor <- sprintf("%s%X", args$background.fill.color,
                                       round(args$background.fill.opacity * 255))

    # Chart and Axis titles always seem to be ignored
    # Waiting on RS-7208
    res$ShowChartTitle = any(nzchar(args$title))
    res$ChartTitleFont = list(color = args$title.font.color, family = args$title.font.family,
            size = px2pt(args$title.font.size))

    if (!chart.type %in% c("Pie", "Donut"))
    {
        res$PrimaryAxis = list(LabelsFont = list(color = args$categories.tick.font.color,
            family = args$categories.tick.font.family,
            size = px2pt(args$categories.tick.font.size)),
            ShowTitle = any(nzchar(args$categories.title)),
            TitleFont = list(color = args$categories.title.font.color,
            family = args$categories.title.font.family,
            size = px2pt(args$categories.title.font.size)),
            AxisLine = list(Color = args$categories.line.color,
            Width = px2pt(args$categories.line.width),
            Style = if (isTRUE(args$categories.line.width == 0)) "None" else "Solid"),
            MajorGridLine = list(Color = args$categories.grid.color,
            Width = px2pt(args$categories.grid.width),
            Style = if (isTRUE(args$categories.grid.width == 0)) "None" else "Solid"),
            RotateLabels = isTRUE(args$categories.tick.angle == 90),
            LabelPosition = "Low")
        if (any(nzchar(args$categories.bounds.maximum)))
            res$PrimaryAxis$Maximum <- args$categories.bounds.maximum
        if (any(nzchar(args$categories.bounds.minimum)))
            res$PrimaryAxis$Minimum <- args$categories.bounds.minimum

        res$ValueAxis = list(LabelsFont = list(color = args$values.tick.font.color,
            family = args$values.tick.font.family, size = px2pt(args$values.tick.font.size)),
            ShowTitle = any(nzchar(args$values.title)),
            TitleFont = list(color = args$values.title.font.color,

            family = args$values.title.font.family, size = px2pt(args$values.title.font.size)),
            NumberFormat = if (isTRUE(grepl("%", attr(data, "statistic")))) "0%" else "General",
            AxisLine = list(Color = args$values.line.color,
            Width = px2pt(args$values.line.width),
            Style = if (isTRUE(args$values.line.width == 0)) "None" else "Solid"),
            MajorGridLine = list(Color = args$values.grid.color,
            Width = px2pt(args$values.grid.width),
            Style = if (isTRUE(args$values.grid.width == 0)) "None" else "Solid"))
        if (any(nzchar(args$values.bounds.maximum)))
            res$ValueAxis$Maximum <- args$values.bounds.maximum
        if (any(nzchar(args$values.bounds.minimum)))
            res$ValueAxis$Minimum <- args$values.bounds.minimum

        # We don't want to manually set axis label position
        # if they are not shown
        if (!is.null(args$values.axis.show) && args$values.axis.show == FALSE)
            res$ValueAxis$LabelPosition <- "None"
        if (!is.null(args$categories.axis.show) && args$categories.axis.show == FALSE)
            res$PrimaryAxis$LabelPosition <- "None"
    }

    # Chart-specfic parameters
    if (grepl("StackedColumn", chart.type) && isTRUE(args$values.zero))
    {
        # PPT doesn't have a concept of the zero line so use a workaround
        res$ValueAxis$Crosses <- "AutoZero"
        res$PrimaryAxis$AxisLine$Style <- "Solid"
        res$PrimaryAxis$AxisLine$Width <- px2pt(args$values.zero.line.width)
        res$PrimaryAxis$AxisLine$Color <- args$values.zero.line.color
    }

    if (chart.type %in% "Donut")
        res$HoleSize = args$pie.inner.radius
    if (chart.type %in% c("Donut", "Pie"))
        res$FirstSliceAngle <- 270
    if (chart.type %in% c("Bar", "Column", "Pyramid", "BarMultiColor", "ColumnMultiColor"))
        res$GapWidth = args$bar.gap * 100
    if (chart.type == "Line")
        res$Smooth = isTRUE(args$shape == "Curved")
    if (chart.type %in% c("BarMultiColor", "ColumnMultiColor", "Pyramid", "Bar Pictograph") ||
        (chart.type == "Scatter" && !isTRUE(args$scatter.colors.as.categorical)))
        res$ShowLegend <- FALSE
    if (chart.type == "Scatter" && tmp.data.label.show && isTRUE(args$grid.show))
    {
        # LabeledScatter does not have options to control color and width of grid
        res$PrimaryAxis$MajorGridLine <- list(Style = "Solid", Width = 1,
            Color = "#E1E1E1")
        res$ValueAxis$MajorGridLine <- list(Style = "Solid", Width = 1,
            Color = "#E1E1E1")
    }


    # There are some issues with Scatterplot exporting
    # See RS-7154 - try master.displayr.com
    if (chart.type %in% c("Scatter"))
    {
        res$ValueAxis$Crosses <- "Minimum"
        res$BubbleSizeType = if (isTRUE(args$scatter.sizes.as.diameter)) "Width" else "Area"
        res$BubbleScale = args$marker.size * 10
    }
    return(res)
}


# Fix minimum axes bounds if they are not already set
# This is performed only because PPT will set the minimum to 0 if not specified
# The default values for the maximum is usually quite reasonable
setScatterAxesBounds <- function(settings, data)
{
    # Skip setting if data is multiple tables
    if (is.list(data) && !is.data.frame(data))
        return(settings)

    .isValidIndex <- function(i) {return (!is.null(i) && !is.na(i) && i > 0 &&
                        i <= NCOL(data))}
    v.ind <- attr(data, "scatter.variable.indices")
    ind.x <- v.ind["x"]
    ind.y <- v.ind["y"]

    if (is.null(settings$ValueAxis$Minimum) && .isValidIndex(ind.y))
    {
        rg <- range(data[,ind.y], na.rm = TRUE)
        if (all(is.finite(rg)) && rg[1] != rg[2])
        {
            offset <- (rg[2] - rg[1]) * 0.1
            sc <- 10^(floor(log10(rg[2] - rg[1])))
            settings$ValueAxis$Minimum <- floor((rg[1] - offset)*sc) * sc
        }
    }
    if (is.null(settings$PrimaryAxis$Minimum) && .isValidIndex(ind.x))
    {
        rg <- range(data[,ind.x], na.rm = TRUE)
        if (all(is.finite(rg)) && rg[1] != rg[2])
        {
            offset <- (rg[2] - rg[1]) * 0.1
            sc <- 10^(floor(log10(rg[2] - rg[1])))
            settings$PrimaryAxis$Minimum <- floor((rg[1] - offset)*sc) * sc
        }
    }
    return(settings)
}

# converts sizes from pixels (which is used by plotly)
# into points (which is used in Displayr and PPT exporting)
px2pt <- function(x)
{
    return(x/1.3333)
}


# This function determines whether the font should be shown in black or white
# on the brightness of background. The coefficients are the same as in
# flipStandardCharts and rhtmlHeatmap. The values are originally from
# http://stackoverflow.com/questions/11867545/change-text-color-based-on-brightness-of-the-covered-background-area

#' @importFrom grDevices col2rgb rgb2hsv
autoFontColor <- function (colors)
{
    tmp.rgb <- col2rgb(colors)
    tmp.lum <- apply(tmp.rgb, 2, function(x) return(0.299*x[1] + 0.587*x[2] + 0.114*x[3]))
    return(ifelse(tmp.lum > 126, "#2C2C2C", "#FFFFFF"))
}


#' @importFrom grDevices colorRamp rgb
getColorsAsNumericScale <- function(data, colors, opacity)
{
    color.index <- attr(data, "scatter.variable.indices")["colors"]
    if (is.na(color.index) || NCOL(data) < color.index)
        return(NULL)
    if (length(colors) < 2)
        return(NULL)

    color.data <- data[,color.index]
    if (is.ordered(color.data))
        class(color.data) <- "factor"
    color.data <- suppressWarnings(AsNumeric(color.data, binary = FALSE))
    not.na <- which(!is.na(color.data))
    color.func <- colorRamp(unique(colors))
    dat.scaled <- (color.data - min(color.data, na.rm = TRUE))/
        diff(range(color.data, na.rm = TRUE))
    dat.scaled[is.na(color.data)] <- 0

    color.vec <- rgb(color.func(dat.scaled), alpha = 255 * opacity,
        maxColorValue = 255)
    data.points <- lapply(not.na, function(i) {list(Index = i - 1,
        BackgroundColor = color.vec[i],
        Marker = list(BackgroundColor = color.vec[i]))})
    return(data.points)
}


#' substituteAxisNames
#'
#' Substitutes 'categories' or 'values' for 'x' and 'y'.
#' @details Unlike \code{substituteArgumentNames}, the behaviour is specific to
#'   \code{chart.function}. It is also less agggressive in that it searches for
#'   replacements only at the beginning of the name.
#' @param chart.function Name of charting function
#' @param arguments List of arguments supplied by user
substituteAxisNames <- function(chart.function, arguments)
{
    a.names <- names(arguments)

    # constrain to only the first position to prevent excessive matching
    if (chart.function %in% c("Bar", "Pyramid", "BarMultiColor"))
    {
        a.names <- gsub("^categories", "y", a.names)
        a.names <- gsub("^values", "x", a.names)

    } else if (chart.function %in% c("Area", "Column", "ColumnMultiColor",
                    "Line", "Radar", "Scatter", "LabeledScatter"))
    {
        a.names <- gsub("^categories", "x", a.names)
        a.names <- gsub("^values", "y", a.names)
    }
    names(arguments) <- a.names
    return(arguments)
}

#' scaleFontSizes
#'
#' Convert font size from point to pixel.
#' @details All of the charts in flipStandardChart
#' take font sizes to be in units of pixels, however, textboxes in Displayr
#' assumes font sizes are in units of points. This function iterates through
#' \code{user.args} and scales all font sizes so they are in the equivalent points.
#' Note that arguments not specified by the user are not affected.
#' @param arguments List of arguments supplied by user
scaleFontSizes <- function(arguments)
{
    ind <- which(grepl("font.size$", names(arguments)))
    f.scale <- 1.3333
    for (i in ind)
    {
        arguments[[i]] <- round(f.scale * arguments[[i]], 0)
    }
    return(arguments)
}


#' getFunctionNameAndParameters
#'
#' Gets the function, loading parameters if necessary, and the parameters of the function.
#' @param chart.function.name The name of the function used for creating the chart (aka plot).
#' @param small.multiples Logical; whether a panel of charts should be created
#' @return A list with the following elements:
#' \item{\code{chart.function}}{The function}.
#' \item{\code{parameter.1}}{The first parameter in \code{chart.function}}.
#' \item{\code{parameter.o}}{The other parameters in \code{chart.function}}.
#'
getFunctionAndParameters <- function(chart.function.name, small.multiples)
{
    if (!is.character(chart.function.name))
        stop("'chart.function.name' must be of type 'character'.")

    chart.function <- gsub('"', "", chart.function.name, fixed = TRUE) # fixing mess created when 'type' is already a character

    # Getting the function
    loadPackage(chart.function)
    chart.function <- get0(chart.function, mode = "function")
    if (!is.function(chart.function))
        stop(paste0("Cannot find ", chart.function,"."))
    parameters <- formalArgs(chart.function)
    p.1 <- parameters[1]
    p.o <- parameters[-1]

    if (small.multiples)
    {
        chart.function <- get0("SmallMultiples", mode = "function")
        tmp.param <- formalArgs(chart.function)
        p.o <- unique(c(p.o, "chart.type", tmp.param[-1]))
    }
    list(chart.function = chart.function, parameter.1 = p.1, parameters.o = p.o)
}


# #' RGUIControls
# #'
# #' Writes the JavaScript to create the RGUI Controls for Displayr and Q. Note that the first parameter of the function
# #' is not automatically written (as it is assumed to be data, to be  addressed by the remaining arguments).
# #' @param chart.function.name The name of the function used for creating the chart (aka plot).
# #' @param vector \code{TRUE} if the function accepts a vector as the sole data input.
# #' @param matrix \code{TRUE} if the function accepts a \code{matrix} as the sole data input.
# #' @param raw.data.1 \code{TRUE} if the function accepts a single variable of 'raw' (non-aggregated) data as an input.
# #' @param raw.data.2 \code{TRUE} if the function accepts a pair of variables of 'raw' data.
# #' @param raw.data.multi \code{TRUE} if the function accepts multiple variables of 'raw' data as an input.
# #' @return A \code{character} object of JavaScript code.
# #' @export
#
# RGUIControls <- function(chart.function.name,
#                          vector = FALSE,
#                          matrix = FALSE,
#                          raw.data.1 = FALSE,
#                          raw.data.2 = FALSE,
#                          raw.data.multi = FALSE,
#                          r.object = FALSE,
#                          scalar = FALSE)
# {
#     parameters <- getFunctionAndParameters(chart.function.name)$parameters.o
#     parameters
# }
#


#' substituteArgumentNames
#'
#' @param parameter.names The names of parameters.
#' @param arguments The arguments to match to the parameters.
#' @param warn.if.no.match If TRUE, a warning is shown if any arugments are not matched.
#'
substituteArgumentNames <- function(parameter.names, arguments, warn.if.no.match = TRUE)
{
    a.names <- names(arguments)
    p.names <- parameter.names
    a.unmatched <- !a.names %in% p.names
    p.unmatched <- !p.names %in% a.names
    if (any(a.unmatched)) # Some argument names do not match parameter names
    {
        # Perform matches and update a.names
        .replaceMatches <- function(aa, pp)
        {
            for (a in aa[a.unmatched])
                for(p in pp[p.unmatched]){
                    if (parametersEqual(p, a))
                        a.names[match(a, aa)] <- p.names[match(p, pp)]
                }
            a.names <<- a.names
        }
        ## Permuting order and ignoring case
        a.regularized <- tolower(a.names)
        p.regularized <- tolower(p.names)
        .replaceMatches(a.regularized, p.regularized)

        # Substituting synonyms
        a.unmatched <- !a.names %in% p.names
        p.unmatched <- !p.names %in% a.names
        if (any(a.unmatched))
        {
            .replaceSynonyms <- function(names)
            {
                for (i in seq_along(synonyms))
                {
                    syns <- synonyms[[i]]
                    init.syn <- syns[1]
                    for (s in syns[-1])
                        names <- gsub(s, init.syn, names, fixed = TRUE)
                }
                names
            }
            a.regularized[a.unmatched] <- .replaceSynonyms(tolower(a.names[a.unmatched]))
            p.regularized[p.unmatched] <- .replaceSynonyms(tolower(p.names[p.unmatched]))
            .replaceMatches(a.regularized, p.regularized)
        }
    }
    a.unmatched <- !a.names %in% p.names
    if (any(a.unmatched) && warn.if.no.match)
        warning("The following arguments have been ignored: ", paste(a.names[a.unmatched], collapse = ", "))
    names(arguments) <- a.names
    arguments[!a.unmatched]
}



#' Make sure that the first synonym is the briefiest, and from there they are ordered from
#' most more verbose to any roots.
#' These are swapped out in order. E.g., after the first line, all "colours" will be changed to 'col'.
synonyms <- list(c("col", "colours", "colour", "colors", "color"),
                 c("size", "sizes"),
                 c("label", "labels"),
                 c("categories", "x", "categories", "x.axis", "xaxis"),
                 c("values", "y", "values", "y.axis", "yaxis"),
                 c("paneltitle", "panel.title"),
                 c("xtitle",  "x.title",   "xlab"),
                 c("ytitle", "y.title",  "ylab"),
                 c("title", "main"),
                 c("label.show", "data.label.show"),
                 c("fontsize", "font.size")
)

#' parametersEqual
#'
#' Checks if parameters are equal
#' @param recipient The name of the parameter in the function.
#' @param donor The provided name of the parameter.
parametersEqual <- function(recipient, donor)
{
    # Exact match
    if (recipient == donor)
        return(TRUE)
    # Matching after re-ordering full stops
    recipient.split <- sort(strsplit(recipient, ".", fixed = TRUE)[[1]])
    donor.split <- sort(strsplit(donor, ".", fixed = TRUE)[[1]])
    if(length(recipient.split) == length(donor.split))
        return((all(recipient.split == donor.split)))
    return(FALSE)
}

#' loadPackage
#'
#' Loads the package in which a chart is located. Only does this if the chart's function is registered
#' in this function as having a package requiring loading.
#'
#' @param chart.type The name of the function of the chart type to be loaded.
#' @importFrom flipU LookupName
loadPackage <- function(chart.type)
{
    # Make sure that the package is specified in suggest in the descriptions file
    #
    #              package           chart function
    package.type.lookup <- c("rhtmlMoonPlot" = "moonplot")
    package <- LookupName(chart.type, package.type.lookup)
    if (is.null(package))
        package <- "flipStandardCharts"
    if (!is.null(package))
        require(package, character.only = TRUE)
}

# Convert ChartData attribute so that it is readable by Powerpoint
# For X, Y and Size variables, this needs to numeric
# which means that any label information will be lost
# For Color variables, factor levels can be retained

#' @importFrom flipTransformations AsNumeric
#' @importFrom flipU CopyAttributes
convertChartDataToNumeric <- function(data)
{
    .isValidIndex <- function(i) {return (!is.null(i) && !is.na(i) && i > 0 &&
                        i <= NCOL(data))}

    v.ind <- attr(data, "scatter.variable.indices")
    new.data <- suppressWarnings(AsNumeric(data, binary = FALSE))

    # Color variable can be returned as a factor to retain
    # label names
    ind.color <- v.ind["colors"]
    if (.isValidIndex(ind.color) && is.factor(data[,ind.color]))
        new.data[,ind.color] <- data[,ind.color]
    else if (.isValidIndex(ind.color) && is.character(data[,ind.color]))
        new.data[,ind.color] <- factor(data[,ind.color], levels = unique(data[,ind.color]))
    return(CopyAttributes(new.data, data))
}
