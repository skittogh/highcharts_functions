#' grouped time series chart
#'
#' @param df dataframe containing data to display
#' @param x_var character string; column header for x-axis data
#' @param y_var character string; column header for y-axis data
#' @param grouping character string; column header for grouping variable data
#' @param x_title character string; text for x-axis title. If none specified then x_var will be used
#' @param y_title character string; text for y-axis title. If none specified then y_var will be used
#' @param pal_col character string; column header that contains colour palette data as character strings
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
#' function to produce a grouped line chart (multiple line chart) using the highcharts library
#'
#' @examples
#' dat <- data.frame("year" = rep(c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),2),
#' "rate" = c(round(runif(13, 200, 300),2),round(runif(13, 200, 300),2)),
#' "sex" = c(rep("male", 13), rep("female", 13)),
#' "pal" = c(rep("#88ccee", 13), rep("#15284c", 13))
#' )
#' pubs_grouped_ts_plot(
#'     df = dat,
#'     x_var = "year",
#'     y_var = "rate",
#'     grouping = "sex",
#'     x_title = "Year",
#'     y_title = "Rate",
#'     pal_col = "pal",
#'     annotate_plot = TRUE
#' )
pubs_grouped_ts_plot <-
    function(df,
             x_var,
             y_var,
             grouping,
             x_title,
             y_title,
             pal_col,
             upper_CI = NULL,
             lower_CI = NULL,
             annotate_plot = FALSE,
             margin_right = 40,
             padding = 10,
             label_font_size = '14px',
             axis_label_font_size = "16px",
             ymin = NULL,
             ymax = NULL) {

        if (missing(y_title)) {
            y_title <- y_var
        }
        if (missing(x_title)) {
            x_title <- x_var
        }

        if (missing(grouping)) {
            stop("Please provide a grouping variable. Example: grouping = 'example_var'")
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
        if (missing(pal_col)) {
            stop("Please provide a column name that contains palette variable")
        }

        this_data <- df

        this_data[x_var] <- lapply(this_data[x_var], as.character) # transform x variable to character string

        if(!"pal" %in% colnames(this_data)) {
            stop("Data must contain a column called 'pal', from which plot colours are specified")
        }

        if(!is.factor(this_data[[grouping]])) {
            print("!!! Data in column specified by 'grouping' should be factorised! This is for explicit ordering of variables in the legend and for explicity matching colours to variables. Suggestion: use the order of the grouping variable defined in the pubs team standard palette used")
        }

        palette <- this_data %>% ungroup() %>% select(!!sym(grouping), !!sym(pal_col)) %>% unique() %>% arrange(xtfrm(!!sym(grouping))) %>% pull(!!sym(pal_col))
        # palette <- this_data %>% ungroup() %>% select(!!sym(grouping), pal) %>% unique() %>% arrange(levels(this_data[[grouping]])) %>% pull(pal)
        var_num <- length(unique(this_data[[grouping]]))

        if(is.null(upper_CI)){

            this_data %>%
                hchart(.,
                       type = 'line',
                       hcaes(
                           x = !!sym(x_var),
                           group = !!sym(grouping),
                           y = as.numeric(!!sym(y_var))
                       ),
                       color = palette) %>%
                hc_yAxis(max = ymax, min = ymin, title = list(text = y_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_xAxis(title = list(text = x_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_chart(marginRight = margin_right) %>%

                hc_plotOptions(series = list(
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
                )
        } else {

            this_data %>%
                hchart(
                    .,
                    type = 'line',
                    hcaes(
                        x = !!sym(x_var),
                        group = !!sym(grouping),
                        y = as.numeric(!!sym(y_var))
                    ),
                    id = letters[1:var_num],
                    color = palette
                ) %>%

                hc_yAxis(max = ymax, min = ymin, title = list(text = y_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_xAxis(title = list(text = x_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_chart(marginRight = margin_right) %>%

                hc_add_series(
                    this_data,
                    "errorbar",
                    hcaes(y = as.numeric(!!sym(y_var)), x = !!sym(x_var), low = as.numeric(!!sym(lower_CI)), high = as.numeric(!!sym(upper_CI)), group = !!sym(grouping)),
                    linkedTo = letters[1:var_num],
                    enableMouseTracking = FALSE,
                    showInLegend = FALSE,
                    color = palette
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
                                        return '<span class=\"series_lab\" style=\"color:' + color + ';\">&nbsp;&nbsp;' + label + '</span><br/>'
                                        } else {
                                                return null;
                                                }
                                        }"
                            )
                        )
                    )
                ) %>%

                hc_tooltip(
                    formatter = JS(
                        "function() {
                                        var s =  + this.point.name + '<br/>' +
                                                '<b>' + this.series.name + ': ' + '</b>' + this.point.y.toLocaleString() + '<br/>' +
                                                '<b>' + 'CI: ' + '</b>' + this.point.lower_int.toLocaleString() + ' - ' + this.point.upper_int.toLocaleString()
                                                ;
                                        return s;
                                        }"
                    )
                )
        }

    }
