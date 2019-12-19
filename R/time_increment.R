#' This function increments a date to the first
#' or last date of the next month.
#'
#' @param date_to_increment The date to work on.
#' @param start_or_end Return the date of the start or the end of the next month.
#' @return The updated date.
#' @import lubridate
#' @export

increment_month <- function(date_to_increment, start_or_end){

        # Set begin date.
        if(month(date_to_increment) == 12){

                set_month = 1
                set_year = year(date_to_increment) + 1

        } else {

                set_month = month(date_to_increment) + 1
                set_year = year(date_to_increment)
        }

        # Add '0' to months 1 - 9.
        if(nchar(set_month) == 1){set_month = paste('0', set_month, sep = '')}

        date_to_increment <- as.Date(paste(set_year, set_month, '01', sep = '-'))

        if(start_or_end == 'end'){

                if(month(date_to_increment) == 12){

                        set_month = 1
                        set_year = year(date_to_increment) + 1
                } else {
                        set_month = month(date_to_increment) + 1
                        set_year = year(date_to_increment)
                }

                # Add '0' to months 1 - 9.
                if(nchar(set_month) == 1){set_month = paste('0', set_month, sep = '')}

                date_to_increment <- as.Date(paste(set_year, set_month, '01', sep = '-')) - 1
        }

        return(date_to_increment)
}
