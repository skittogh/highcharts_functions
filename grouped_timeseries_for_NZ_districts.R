#' time series plot for districts data
#'
#' @param df dataframe containing data to display
#' @param selected_district character vector; selected district(s) to be displayed as coloured lines (lines for other districts will be grey)
#' @param x_var character string; column header for x-axis data
#' @param y_var character string; column header for y-axis data
#' @param district_col character string; column header for district variable data
#' @param x_title character string; text for x-axis title. If none specified then x_var will be used
#' @param y_title character string; text for y-axis title. If none specified then y_var will be used
#' @param palette character vector; provide a palette with a minimum of 5 colours. Suggest using a palette from the pubr package such as 'palette_5var'
#' @param upper_CI character string; column header for error bar upper CI data (defaults to NULL)
#' @param lower_CI character string; column header for error bar lower CI data (defaults to NULL)
#' @param annotate_plot boolean; add annotated data labels within the chart bars (defaults to FALSE)
#' @param margin_right numeric; the is the margin to the right hand side of plot. Defaults to 40 (40px)
#' @param padding numeric; padding around chart. Defaults to 10, which represent 10px
#' @param label_font_size character string; font size in pixels for data label annotations. Defaults to "14px"
#' @param axis_label_font_size character string; font size in pixels for axis labels. Defaults to "16px"
#' @param ymax numeric; maximum limit of the y-axis to display. Defaults to NULL so that data range determines y-max
#' @param ymin numeric; minimum limit of the y-axis to display. Defaults to NULL so that data range determines y-min
#'
#' @return
#' @export
#'
#' @details
#' function to produce a time series line chart suitable to display districts data.
#' Districts to be display are specified using the 'district_col' parameter (a select input will likely be used here i.e.
#' 'district_col' = input$district. These chosen districts will be the focus of the chart and displayed as coloured
#' lines. Other districts will be displayed as grey lines to provide some comparison/context for the chosen districts
#'
#' @examples
#' years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020,2021,2022)
#' df <- data.frame()
#' for(i in years) {
#'     dhbs <- c("Auckland","Bay of Plenty","Canterbury","Capital and Coast","Counties Manukau","Hawke's Bay",
#'               "Hutt Valley","Lakes","MidCentral","Nelson Marlborough","Northland","South Canterbury","Southern",
#'               "Tairawhiti","Taranaki","Waikato","Wairarapa","Waitemata","West Coast","Whanganui")
#'     df1 <- data.frame("Year" = rep(i, 20), "dhb" = dhbs, "rate" = round(runif(20, 200, 300),2))
#'     df <- rbind(df, df1)
#' }
#'
#' pubs_district_ts_plot(
#'     df = df,
#'     selected_district = c("Auckland", "Whanganui", "Taranaki"),
#'     x_var = "Year",
#'     y_var = "rate",
#'     district_col = "dhb",
#'     x_title = "Year",
#'     y_title = "Rate",
#'     palette = c("#15284c", "#882255", "#88ccee", "#999933", "#117733"),
#'     annotate_plot = TRUE,
#'     ymin = 0
#' )
pubs_district_ts_plot <-
    function(df,
             selected_district,
             x_var,
             y_var,
             district_col,
             x_title,
             y_title,
             palette,
             upper_CI = NULL,
             lower_CI = NULL,
             annotate_plot = TRUE,
             margin_right = 130,
             padding = 10,
             label_font_size = '14px',
             axis_label_font_size = "16px",
             ymin = NULL,
             ymax = NULL
    ) {

        if (missing(y_title)) {
            y_title <- y_var
        }

        if (missing(x_title)) {
            x_title <- x_var
        }

        if (missing(district_col)) {
            stop("Please provide a district_col variable. Example: district_col = 'dhb'")
        }
        if (missing(df)) {
            stop("Please provide a source dataframe (df). Example: df = example_df")
        }
        if (missing(x_var)) {
            stop("Please provide an x-axis (x_var) variable. Example: x_var = 'example_var'")
        }
        if (missing(y_var)) {
            stop("Please provide an y-axis (y_var) variable. Example: y_var = 'example_var'")
        }
        if (missing(palette)) {
            stop("Please provide a palette with a minimum of 5 colours. Suggest using a palette
                 from the pubr package such as 'palette_5var' which should be available within 'app/r/pubr-source")
        }
        if (missing(selected_district)) {
            stop("Please provide a vector of districts to display. Usually from a select input. For example, input$...")
        }
        if (length(selected_district) > 5) {
            stop("The maximun number of districts that can be displayed using pubs_district_ts_plot() is five(5).
                 Please limit the number of districts selected. For example 'multiple = TRUE, options = list(maxItems = 5L)'
                 if using 'selectizeInput'")
        }

        this_data <- df

        this_data[x_var] <- lapply(this_data[x_var], as.character) # transform x variable to character string if not already

        var_num <- length(selected_district) # used to map error bars to particular series using 'linkedTo'

        data_selected <- this_data %>% filter(!!sym(district_col) %in% selected_district) # subset dataset to include selected districts

        data_not_selected <- this_data %>% filter(!(!!sym(district_col) %in% selected_district)) # subset dataset to exclude selected districts (i.e all others to be coloured grey in plot)

        data_selected_pal <- palette[1:length(selected_district)]

        if(is.null(upper_CI)){

            data_not_selected %>%
                hchart(.,
                       type = 'line',
                       hcaes(
                           x = !!sym(x_var),
                           group = !!sym(district_col),
                           y = as.numeric(!!sym(y_var)),

                       ),
                       color = "#f2f2f2",
                       marker = list(
                           enabled = FALSE
                       ),
                       showInLegend = FALSE
                ) %>%
                hc_yAxis(max = ymax, min = ymin, title = list(text = y_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_xAxis(title = list(text = x_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_chart(marginRight = margin_right) %>%

                hc_add_series(data = data_selected,
                              type = 'line',
                              hcaes(
                                  x = !!sym(x_var),
                                  group = !!sym(district_col),
                                  y = as.numeric(!!sym(y_var))
                              ),
                              color = data_selected_pal,
                              marker = list(
                                  enabled = TRUE
                              ),
                              showInLegend = TRUE
                ) %>%

                hc_plotOptions(
                    series = list(
                        # formatting data labels including a custom formatter
                        dataLabels = list(
                            enabled = annotate_plot,
                            allowOverlap = TRUE,
                            align = 'left',
                            crop = FALSE,
                            overflow = 'allow',
                            verticalAlign = 'middle',
                            padding = padding,
                            useHTML = TRUE,
                            style = list(fontSize = label_font_size,
                                         fontFamily = 'Arial, sans-serif'),
                            formatter = JS(
                                "function() {
                                if (this.point.index === this.series.data.length - 1) {
                                var color = this.series.color;
                                var label = this.series.name;
                                return '<span class=\"tesfv\" style=\"color:' + color + ';\">&nbsp;&nbsp;' + label + '</span><br/>'
                                } else {
                                return null;
                                }
                        }"
                            )
                        )
                    )
                ) %>%
                # shared and sorted tooltip
                hc_tooltip(shared=TRUE,
                           sort = TRUE)

        } else {

            data_not_selected %>%
                hchart(.,
                       type = 'line',
                       hcaes(
                           x = !!sym(x_var),
                           group = !!sym(district_col),
                           y = as.numeric(!!sym(y_var)),

                       ),
                       color = "#f2f2f2", #colour of the non-selected districts
                       marker = list(
                           enabled = FALSE
                       ),
                       showInLegend = FALSE
                ) %>%
                hc_yAxis(max = ymax, min = ymin, title = list(text = y_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_xAxis(title = list(text = x_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_chart(marginRight = margin_right) %>%

                hc_add_series(data = data_selected,
                              type = 'line',
                              hcaes(
                                  x = !!sym(x_var),
                                  group = !!sym(district_col),
                                  y = as.numeric(!!sym(y_var))
                              ),
                              id = letters[1:var_num],
                              color = data_selected_pal,
                              marker = list(
                                  enabled = TRUE
                              ),
                              showInLegend = TRUE
                ) %>%

                hc_add_series(
                    data = data_selected,
                    "errorbar",
                    hcaes(y = as.numeric(!!sym(y_var)), x = !!sym(x_var), low = as.numeric(!!sym(lower_CI)), high = as.numeric(!!sym(upper_CI)), group = !!sym(district_col)),
                    linkedTo = letters[1:var_num],
                    enableMouseTracking = FALSE,
                    showInLegend = FALSE,
                    color = data_selected_pal
                ) %>%

                hc_plotOptions(
                    errorbar = list(
                        stemWidth = 1,
                        animation = FALSE,
                        whiskerLength = '20%',
                        dataLabels = list(
                            enabled = FALSE)
                    ),
                    series = list(
                        # formatting data labels including a custom formatter
                        dataLabels = list(
                            enabled = annotate_plot,
                            allowOverlap = TRUE,
                            align = 'left',
                            crop = FALSE,
                            overflow = 'allow',
                            verticalAlign = 'middle',
                            padding = padding,
                            useHTML = TRUE,
                            style = list(fontSize = label_font_size,
                                         fontFamily = 'Arial, sans-serif'),
                            formatter = JS(
                                "function() {
                                if (this.point.index === this.series.data.length - 1) {
                                var color = this.series.color;
                                var label = this.series.name;
                                return '<span class=\"tesfv\" style=\"color:' + color + ';\">&nbsp;&nbsp;' + label + '</span><br/>'
                                } else {
                                    return null;
                                }
                        }"
                            )
                        )
                    )
                ) %>%

                # Add a custom tooltip which includes confidence intervals
                hc_tooltip(
                    shared = TRUE,
                    formatter = JS("function() {
                                var s = '<span style=\"font-size: 10px\">' + this.points[this.x].key + '</span>',
                                points = this.points;

                                // Sort the points array based on y value
                                points.sort(function(a, b) {
                                  return b.y - a.y;
                                });

                                points.forEach(function(point) {
                                  s += '<br/><span style=\"color:' + point.color + '\">\u25CF</span> ' + point.series.name + ': <b>' + point.y + '</b>' + ' (' +  point.point.lower_int + '-' + point.point.upper_int + ')';
                                });

                                return s;
                        }"
                    )
                )
        }
    }
