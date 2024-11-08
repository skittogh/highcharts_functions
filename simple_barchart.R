#' simple bar plot function
#'
#' @param df dataframe containing data to display
#' @param x_var character string; column header for x-axis data
#' @param y_var character string; column header for y-axis data
#' @param x_title character string; text for x-axis title. If none specified then x_var will be used
#' @param y_title character string; text for y-axis title. If none specified then y_var will be used
#' @param horizontal boolean; if TRUE a horizontal bar chart will be rendered. FALSE (default) a vertical bar chart will be rendered
#' @param pal chartacter string; bar colour. Defaults to "#15284c"
#' @param upper_CI character string; column header for error bar upper CI data (defaults to NULL)
#' @param lower_CI character string; column header for error bar lower CI data (defaults to NULL)
#' @param annotate_plot boolean; add annotated data labels within the chart bars (defaults to FALSE)
#' @param padding numeric; padding around chart. Defaults to 10, which represent 10px
#' @param margin_right numeric; the is the margin to the right hand side of plot. Defaults to 40 (40px)
#' @param margin_top numeric; the is the margin to the top of plot. Defaults to 40 (40px)
#' @param label_font_size character string; font size in pixels for data label annotations. Defaults to "14px"
#' @param label_rotation numeric; rotation of the data labels if annotate_plot = TRUE. Defaults to 0 which represents 0 degrees rotation (horizontal)
#' @param axis_label_font_size character string; font size in pixels for axis labels. Defaults to "16px"
#' @param text_align The alignment of the data label compared to the point. If right, the right side of the label should be touching the point. Defaults to NULL which aligns annotation label to the centre
#' @param ymax numeric; maximum limit of the y-axis to display. Defaults to NULL so that data range determines y-max
#' @param ymin numeric; minimum limit of the y-axis to display. Defaults to NULL so that data range determines y-min
#' @param reverse_x boolean; option to reverse the x-axis order. This will sometimes be necessary for horizontal bar charts. Defaults to FALSE
#'
#' @return
#' @export
#'
#' @details
#' function to produce a simple bar plot
#'
#' @examples
#' df1 <- mtcars
#' df1$names <- rownames(df1)
#' df1$names <- factor(df1$names, levels = df1$names)
#' pubs_simple_bar_plot(
#'     df = df1,
#'     x_var = "names",
#'     y_var = "mpg",
#'     x_title = "Vehicle names",
#'     y_title = "Miles per gallon",
#'     horizontal = TRUE
#' )
pubs_simple_bar_plot <-
    function(df,
             x_var,
             y_var,
             x_title,
             y_title,
             horizontal = FALSE,
             pal = "#15284c",
             upper_CI = NULL,
             lower_CI = NULL,
             annotate_plot = FALSE,
             padding = 10,
             margin_right = 40,
             margin_top = 40,
             label_font_size = '14px',
             label_rotation = 0, # rotation degrees between 0 and 270 - default to 0 degrees but recommend set column charts to 270
             axis_label_font_size = "16px",
             text_align = NULL,
             ymin = NULL,
             ymax = NULL,
             reverse_x = FALSE) {

        if (missing(y_title)) {
            y_title <- y_var
        }

        if (missing(x_title)) {
            x_title <- x_var
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

        this_data <- df

        chart_type <- if(horizontal == FALSE) {"column"} else if(horizontal == TRUE) {"bar"}

        if(!is.factor(this_data[[x_var]])) {
            stop("!!! Data in column specified by 'x_var' should be factorised! This is for explicit ordering of variables")
        }

        cats <- levels(this_data[[x_var]])
        cats <- if(reverse_x == TRUE) {rev(cats)} else {cats}

        # for hover series name
        seriesName <-  sub("^(.)", "\\U\\1", y_var, perl = TRUE)

        if(is.null(upper_CI)){

            this_data %>%
                hchart(.,
                       type = chart_type,
                       id = "bars",
                       hcaes(
                           x = !!sym(x_var),
                           y = as.numeric(!!sym(y_var))
                       ),
                       color = pal
                ) %>%

                hc_yAxis(max = ymax, min = ymin, title = list(text = y_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_xAxis(title = list(text = x_title, style = list(fontSize = axis_label_font_size)), categories = cats) %>%
                hc_chart(marginRight = margin_right,
                         marginTop = margin_top) %>%

                hc_plotOptions(
                    series = list(
                        # animation = FALSE,
                        dataLabels = list(
                            enabled = annotate_plot,
                            inside = TRUE,
                            rotation = label_rotation,
                            allowOverlap = TRUE,
                            align = text_align,
                            crop = FALSE,
                            overflow = 'allow',
                            verticalAlign = 'middle',
                            horizontalAlign = 'middle',
                            padding = padding,
                            useHTML = TRUE,
                            style = list(fontSize = label_font_size,
                                         fontFamily = 'Arial, sans-serif')

                        ))) %>%
                #tooltip with series name defined by seriesName
                hc_tooltip(formatter = JS(paste0("function() {
                                          var s =  '<b>' + ",
                                                 "'",
                                                 seriesName,
                                                 "'",
                                                 " + ': ' + '</b>' + this.point.y.toLocaleString();
                                          return s;
                                          }")))

        } else {

            this_data %>%
                hchart(.,
                       type = chart_type,
                       id = "bars",
                       hcaes(
                           x = !!sym(x_var),
                           y = as.numeric(!!sym(y_var))
                       ),
                       color = pal
                ) %>%

                hc_yAxis(max = ymax, min = ymin, title = list(text = y_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_xAxis(title = list(text = x_title, style = list(fontSize = axis_label_font_size)), categories = cats) %>%
                hc_chart(marginRight = margin_right,
                         marginTop = margin_top) %>%

                hc_add_series(
                    this_data,
                    type = "errorbar",
                    id = "error",
                    hcaes(y = as.numeric(!!sym(y_var)), x = !!sym(x_var), low = as.numeric(!!sym(lower_CI)), high = as.numeric(!!sym(upper_CI))),
                    enableMouseTracking = FALSE,
                    showInLegend = FALSE
                ) %>%

                hc_plotOptions(
                    errorbar = list(
                        color = 'grey',
                        lineWidth = 1.5,
                        # animation = FALSE,
                        whiskerLength = '50%',
                        dataLabels = list(
                            enabled = FALSE)
                    ),
                    series = list(
                        dataLabels = list(
                            enabled = annotate_plot,
                            inside = TRUE,
                            rotation = label_rotation,
                            allowOverlap = TRUE,
                            align = text_align,
                            crop = FALSE,
                            overflow = 'allow',
                            verticalAlign = 'middle',
                            horizontalAlign = 'middle',
                            padding = padding,
                            useHTML = TRUE,
                            style = list(fontSize = label_font_size,
                                         fontFamily = 'Arial, sans-serif')
                        ))) %>%

                hc_tooltip(
                    formatter = JS(
                        "function() {
                                        var s =  '<b>' + this.key + ': ' + '</b>' + this.point.y.toLocaleString() + '<br/>' +
                                                '<b>' + 'CI: ' + '</b>' + this.point.lower_int.toLocaleString() + ' - ' + this.point.upper_int.toLocaleString()
                                                ;
                                        return s;
                                        }"
                    )
                )
        }

    }





#' proxy update function for pubs_simple_bar_plot(), for use in shiny applications where partial re-rendering is preferred
#'
#' @param shinyId Single-element character vector indicating the output ID of the chart to modify. For example 'plot', ns('plot') or session$ns('plot').
#' @param df dataframe of source data
#' @param x_var character string; column header for x-axis data
#' @param y_var character string; column header for y-axis data
#' @param ymax numeric; maximum limit of the y-axis to display. Defaults to NULL so that data range determines y-max
#' @param upper_CI character string; column header for error bar upper CI data (defaults to NULL)
#' @param lower_CI character string; column header for error bar lower CI data (defaults to NULL)
#'
#' @return
#' @export
#'
#' @details
#' function to update a pubs_simple_bar_plot() using highchartProxy.
#' This is used so that data in a given chart is updated with a smooth animated transition, without the full
#' chart reloading. Typically this would be used when a variable such as 'year' is updated. The
#' format of the source dataframe (df) remains the same, but the data within has been updated
#'
#' @examples
#' pubs_simple_bar_plot_PROXY(
#'     shinyId = session$ns('plot'),
#'     df = dat,
#'     x_var = "sex,
#'     y_var = "rate,
#'     upper_CI = "upper_int",
#'     lower_CI = "lower_int"
#' )
pubs_simple_bar_plot_PROXY <-
    function(
        shinyId,
        df,
        x_var,
        y_var,
        ymax = NULL,
        upper_CI = NULL,
        lower_CI = NULL) {

        if (missing(shinyId)) {
            stop("Please provide a shinyId. Example: 'plot', ns('plot') or session$ns('plot'). This indicates the output ID of the chart to modify")
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

        if(!is.factor(df[[x_var]])) {
            stop("!!! Data in column specified by 'x_var' should be factorised! This is for explicit ordering of variables")
        }

        if (is.null(upper_CI)) {
            highchartProxy(shinyId) %>%
                hcpxy_update_series(
                    id = "bars",
                    data = df[[y_var]]
                )
        }

        if (!is.null(upper_CI)) {
            highchartProxy(shinyId) %>%
                hcpxy_update_series(
                    id = "bars",
                    data = df[[y_var]]
                ) %>%
                hcpxy_remove_series(id = "error") %>%
                hcpxy_update(yAxis = list(max = max(ymax))) %>% #we want to adjust the max axis before the error bars are rendered so that the error bars don't trigger an axis adjustment
                {Sys.sleep(0.5) ; # add a small sleep so that the error bars are rendered after bars animate
                    hcpxy_add_series(highchartProxy(shinyId),
                                     data = df,
                                     id = "error",
                                     type = "errorbar",
                                     hcaes(x = !!sym(x_var),
                                           y = !!sym(y_var),
                                           low = as.numeric(!!sym(lower_CI)),
                                           high = as.numeric(!!sym(upper_CI))),
                                     animation = FALSE,
                                     redraw = FALSE,
                                     color = 'grey',
                                     lineWidth = 1.5,
                                     whiskerLength = '50%',
                                     dataLabels = list(
                                         enabled = FALSE)
                    )} %>%
                hcpxy_update(yAxis = list(max = max(ymax))) %>%
                hcpxy_redraw()
        }

    }



















