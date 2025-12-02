#' Internal function: finding the position of the minimum value in a distance matrix.
#' Given a small matrix of distances, find the position of the minimum value.
#' @param distances A matrix of distances.
#' @return A vector of length 2 indicating the row and column of the minimum value.
#' @export
find_min_position <- function(distances) {
    position <- which.min(distances)[1]
    position_x <- position %% nrow(distances)
    position_x <- ifelse(position_x == 0, nrow(distances), position_x)
    position_y <- ceiling(position / nrow(distances))
    return(c(position_x, position_y))
}

#' Internal function: finding the best match between a set of detected features and a set of known features.
#'
#' Given a small matrix of distances, find the best column-row pairing that minimize the sum of distances of the matched pairs.
#'
#' @param distances A matrix of distances.
#' @param max_distance A distance larger than which cannot be accepted as pairs.
#' @return A binary matrix the same size as the input matrix, with matched position taking value 1, and all other positions taking value 0.
#' @export
find.match <- function(distances, max_distance) {
    matches <- matrix(0, nrow(distances), ncol(distances))

    if (ncol(distances) == 1) {
        sel <- which.min(distances[, 1])[1]
        matches[sel, 1] <- as.numeric(distances[sel, 1] <= max_distance)
    } else if (nrow(distances) == 1) {
        sel <- which(distances[1, ] == min(distances[1, ]))[1]
        matches[1, sel] <- as.numeric(distances[1, sel] <= max_distance)
    } else {
        while (TRUE) {
            min_position <- find_min_position(distances)
            if (distances[min_position[1], min_position[2]] > max_distance) {
                break
            }
            matches[min_position[1], min_position[2]] <- 1
            distances[min_position[1], ] <- 1e10
            distances[, min_position[2]] <- 1e10
        }
    }

    return(matches)
}
