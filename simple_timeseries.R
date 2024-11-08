#' simple time series chart
#'
#' @param df dataframe containing data to display
#' @param x_var character string; column header for x-axis data
#' @param y_var character string; column header for y-axis data
#' @param x_title character string; text for x-axis title. If none specified then x_var will be used
#' @param y_title character string; text for y-axis title. If none specified then y_var will be used
#' @param pal character string; provide a colour for the line / markers displayed. Defaults to '#15284c', but consider using pubs standard colour 'palette_1var'
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
#' function to produce a single line chart using the highcharts library
#'
#' @examples
#' dat <- data.frame("year" = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
#'"rate" = round(runif(13, 200, 300),2)
#')
#'pubs_ts_plot(df = dat,
#'             x_var = "year",
#'             y_var = "rate",
#'             x_title = "Year",
#'             y_title = "Rate"
#')
pubs_ts_plot <-
    function(df,
             x_var,
             y_var,
             x_title,
             y_title,
             pal = '#15284c',
             upper_CI = NULL,
             lower_CI = NULL,
             annotate_plot = FALSE,
             margin_right = 40,
             padding = 0,
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

        if (missing(df)) {
            stop("Please provide a source dataframe (df). Example: df = example_df")
        }
        if (missing(x_var)) {
            stop("Please provide an x-axis (x_var) variable. Example: x_var = 'example_var'")
        }
        if (missing(y_var)) {
            stop("Please provide an y-axis (y_var) variable. Example: y_var = 'example_var'")
        }

        # if (missing(pal)) {
        #     stop("Please specify a colour for this plot. Example pal = '#15284c', or pal = palette_1var")
        # }

        this_data <- df

        this_data[x_var] <- lapply(this_data[x_var], as.character) # transform x variable to character string

        # palette <- this_data %>% ungroup() %>% select(pal) %>% unique() %>% pull(pal)
        palette <- pal

        # for hover series name
        seriesName <-  sub("^(.)", "\\U\\1", y_var, perl = TRUE)

        if(is.null(upper_CI)){

            this_data %>%
                hchart(
                    .,
                    type = 'line',
                    hcaes(
                        x = !!sym(x_var),
                        y = as.numeric(!!sym(y_var))
                    ),
                    color = palette
                ) %>%
                hc_yAxis(max = ymax, min = ymin, title = list(text = y_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_xAxis(title = list(text = x_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_chart(marginRight = margin_right) %>%

                hc_plotOptions(
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
                                            console.log(color)
                                            var label = this.series.name;
                                            var style = {
                                              color: color
                                            };
                                            return '<span class=\"series_lab\" style=\"color:' + color + ';\">&nbsp;&nbsp;' + label + '</span><br/>'
                                          } else {
                                            return null;
                                          }
                                        }"
                            )
                        )
                    )
                ) %>%
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
                hchart(
                    .,
                    type = 'line',
                    hcaes(
                        x = !!sym(x_var),
                        y = as.numeric(!!sym(y_var))
                    ),
                    color = palette
                ) %>%
                hc_yAxis(max = ymax, min = ymin, title = list(text = y_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_xAxis(title = list(text = x_title, style = list(fontSize = axis_label_font_size))) %>%
                hc_chart(marginRight = margin_right) %>%

                hc_add_series(
                    this_data,
                    "errorbar",
                    hcaes(y = as.numeric(!!sym(y_var)), x = !!sym(x_var), low = as.numeric(!!sym(lower_CI)), high = as.numeric(!!sym(upper_CI))),
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
                                        console.log(color)
                                        var label = this.series.name;
                                        var style = {
                                          color: color
                                        };
                                        return '<span class=\"tesfv\" style=\"color:' + color + ';\">&nbsp;&nbsp;' + label + '</span><br/>'
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
                                                '<b>' + 'Value' + ': ' + '</b>' + this.point.y.toLocaleString() + '<br/>' +
                                                '<b>' + 'CI: ' + '</b>' + this.point.lower_int.toLocaleString() + ' - ' + this.point.upper_int.toLocaleString()
                                                ;
                                        return s;
                                        }"
                    )
                )
        }
    }


