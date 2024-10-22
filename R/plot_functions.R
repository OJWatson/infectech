
#functions to plot

define_plot_df <- function(data = NULL,
                        date_cutoff_predictions,
                        vars_to_modify,
                        metrics_to_plot,
                        locations_to_include = NULL,
                        locations_to_exclude = NULL,
                        pred_date_filter = NULL,
                        plotting_window) {


  data <- data.table::copy(data)

  # Ensure date_cutoff is a Date object
  # Function to convert to Date, but only if it's a character
  convert_to_date <- function(x) {
    if (is.character(x)) {
      return(as.Date(x, format = "%Y-%m-%d"))
    } else {
      return(x)  # Keep it as is if it's already a Date
    }
  }

  # Apply the function to each element of the vector
  plotting_window <- sapply(plotting_window, convert_to_date)

  data <- data %>% filter(forecast_date > min(plotting_window) &
                            forecast_date < max(plotting_window))

  # Ensure date_cutoff is a Date object
  if (is.character(date_cutoff_predictions)) {
    date_cutoff_predictions <- as.Date(date_cutoff_predictions, format = "%Y-%m-%d")  # Adjust format if needed
  }

  # Ensure date_cutoff is a Date object
  if (!is.null(pred_date_filter)) {
    pred_date_filter <- as.Date(pred_date_filter)
    data <- data %>% filter(prediction_date == pred_date_filter)
  }

  # Apply date filter and set values to NA
  for (var in vars_to_modify) {
    data[forecast_date < date_cutoff_predictions, (var) := NA]
  }

  # Filter by countries (if specified)
  if (!is.null(locations_to_include)) {
    data <- data[loc %in% locations_to_include]
  }

  if (!is.null(locations_to_exclude)) {
    data <- data[!loc %in% locations_to_exclude]
  }

  # Filter metrics (if specified)
  if (!is.null(metrics_to_plot)) {
    data <- data[metric %in% metrics_to_plot]
  }

  return(data[])  # Still return the modified data if needed
}

# plot quantile forecasts
plot_forecasts <- function(df,
                           wrap_by = NULL,
                           forecast_type) {

  if (forecast_type == "quantile"){
    # Get all column names
    col_names <- names(df)

    # Identify columns with numeric parts in their names
    numeric_cols <- grepl("[0-9\\.]", col_names)

    # Extract numeric parts and replace only those column names
    new_col_names <- ifelse(numeric_cols,
                            gsub("[^0-9\\.]", "", col_names), # Extract numeric part
                            col_names) # Keep original name if no numeric part

    # Remove leading dot if present and length is greater than 1
    # (only for replaced names)
    new_col_names <- ifelse(numeric_cols & grepl("^\\.", new_col_names) & nchar(new_col_names) > 1,
                            substring(new_col_names, 2),
                            new_col_names)

    # Replace column names in the dataframe
    names(df) <- new_col_names

    # Convert numeric_parts to numeric for sorting
    numeric_parts_numeric <- suppressWarnings(as.numeric(new_col_names))

    # Sort the numeric values (handling potential NAs)
    sorted_values <- sort(numeric_parts_numeric, na.last = TRUE)
    sorted_values <- sorted_values[!is.na(sorted_values)]

    # Get the lowest, 2nd lowest, 2nd highest, and highest
    lowest <- sorted_values[1]
    second_lowest <- sorted_values[2]
    second_highest <- sorted_values[length(sorted_values) - 1]
    highest <- sorted_values[length(sorted_values)]

    # Get the corresponding column names
    lowest_col <- match(names(df)[numeric_parts_numeric == lowest], names(df))
    lowest_col <- lowest_col[!is.na(lowest_col)]

    second_lowest_col <- match(names(df)[numeric_parts_numeric == second_lowest], names(df))
    second_lowest_col <- second_lowest_col[!is.na(second_lowest_col)]

    second_highest_col <- match(names(df)[numeric_parts_numeric == second_highest], names(df))
    second_highest_col <- second_highest_col[!is.na(second_highest_col)]

    highest_col <- match(names(df)[numeric_parts_numeric == highest], names(df))
    highest_col <- highest_col[!is.na(highest_col)]

    # Check for a median column
    column_names <- names(df)
    quantile_columns <- grep("^0\\.\\d+", column_names)

    if (length(quantile_columns) %% 2 != 0) {
      # Calculate the middle position within the quantile columns
      median_position_quantiles <- ceiling(length(quantile_columns) / 2)

      # Get the actual position in the full column_names vector
      median_position_df <- quantile_columns[median_position_quantiles]

      # Get the median column name
      median_column <- column_names[median_position_df]
    } else {
      median_position_df <- NA
      median_column <- NA
    }

    # facet_wrap definition
    # Function to merge wrap_by values into a single string

    # Base plot
    p <- ggplot(df, aes(x = forecast_date)) +
      geom_point(aes(y = observed, x = forecast_date),
                 size = 0.2) +
      xlab("Time")  +
      ylab("")

    # Add facet_wrap if wrap_by is specified
    if (!is.null(wrap_by)) {
      p <- p + facet_wrap(as.formula(paste("metric ~", paste(wrap_by, collapse = " + "))), ncol = 4, scales = "free")
    }

    # Add second highest and second lowest ribbon to plot
    if (!is.na(second_lowest_col) && !is.na(second_highest_col)) {
      p <- p +
        geom_ribbon(aes(ymin = df[[second_lowest_col]],
                        ymax = df[[second_highest_col]]),
                    alpha = 0.6)
    }

    # Add median line if available
    if (!is.na(median_position_df)) {
      p <- p +
        geom_line(aes(y = df[[median_position_df]]))
    }

    # Add highest and lowest ribbon
    p <- p +
      geom_ribbon(aes(ymin = df[[lowest_col]],
                      ymax = df[[highest_col]]),
                  alpha = 0.5) +
      theme_scoringutils()
  }

  if (forecast_type == "point"){

    # Base plot
    p <- ggplot(df, aes(x = forecast_date)) +
      geom_point(aes(y = observed, x = forecast_date), size = 0.1) +
      geom_line(aes(y = predicted))  +
      xlab("Time")  +
      ylab("")

    # Add facet_wrap if wrap_by is specified
    if (!is.null(wrap_by)) {
      p <- p + facet_wrap(as.formula(paste("metric ~", paste(wrap_by, collapse = " + "))), ncol = 4, scales = "free")
    }
  }
  return(p)
}
