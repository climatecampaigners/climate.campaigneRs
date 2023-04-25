
#' Access data API
#' @description If \code{challenges} is provided, it appends the variables "challenges.X.success"
#' that indicate if a challenge with ID X was finished successfully ("Yes") or not ("No").
#' @param challenges Optional data frame returned from \code{\link[climate.campaigneRs:get_challenges]{climate.campaigneRs::get_challenges()}}.
#'
#' @return Data frame from API endpoint.
#' @export
#' @importFrom utils read.csv
#' @return Data frame from API endpoint.
#'
#' @examples
#' \donttest{
#' data <- get_data()
#' }
get_data <- function(challenges) {
  url <- "https://climate-campaigners.app/api/data?pageSize=1000000"

  tryCatch({
    data <- read.csv(url, check.names = FALSE)
    # temp fix:
    data <- as.data.frame(apply(data, 2, function(x) replace(x, x == "Ingen", "No")))
    data <- as.data.frame(apply(data, 2, function(x) replace(x, x == "Ja", "Yes")))

    data$country[data$country == "Turkey"] <- "T\U00FCrkiye"
    data$city[data$city == ""] <- NA_character_
    data$dietType[data$dietType == ""] <- NA_character_

    data[data == ""] <- NA

    stopifnot(all(c("18 - 29", "30 - 39", "50 - 59", "40 - 49", "60 years or older",
                                      "Under 18") %in% unique(data$age)))

    data$age <- factor(data$age, levels = c("Under 18", "18 - 29", "30 - 39",
                                            "40 - 49", "50 - 59", "60 years or older"))

    stopifnot(all(c("Female", "Male", "Diverse", "I don’t want to say") %in% unique(data$gender)))

    data$gender <- factor(data$gender, levels = c("Female", "Male", "Diverse", "I don’t want to say"))

    stopifnot(all(c("No schooling", "Some secondary school (High School)",
                    "Completed primary education", "Completed undergraduate degree",
                    "Completed secondary school (High School)",
                    "A post-secondary school technical qualification",
                    "Completed postgraduate education") %in% unique(data$education)))

    data$education <- factor(data$education, levels = c("No schooling", "Completed primary education",
                                                        "Some secondary school (High School)",
                                                        "Completed secondary school (High School)",
                                                        "A post-secondary school technical qualification",
                                                         "Completed undergraduate degree",
                                                        "Completed postgraduate education"))


    stopifnot(all(c("Employed", "Student", "Home Parent", "Self-employed", "Unemployed", "Retired") %in%
                unique(data$occupation)))

    data$occupation <- factor(data$occupation, levels = c("Employed", "Student",
                                                          "Unemployed", "Home Parent",
                                                          "Self-employed", "Retired"))



    if (missing(challenges)) {
      return(data)
    } else {
      # Add variables "challenges.X.success" that indicate if a challenge was finished
      # successfully ("Yes") or not ("No")
      return(add_success(data, challenges))
    }


  }, error = function(e) {
    print(paste("Error:", e))
    return(NULL)
  })
}



#' Access challenges API
#'
#' @return Data frame from API endpoint.
#' @export
#' @importFrom utils read.csv
#'
#' @examples
#' \donttest{
#' challenges <- get_challenges()
#' }
get_challenges <- function() {
  tryCatch({
    challenges <- read.csv("https://climate-campaigners.app/api/challenges", check.names = FALSE)
    challenges$title <- gsub("^\"|\"$", "", challenges$title)
    challenges$category <- as.factor(challenges$category)
    challenges

  }, error = function(e) {
    print(paste("Error:", e))
    return(NULL)
  })
}


#' Access user_values API
#'
#' @return Data frame from API endpoint.
#' @export
#' @importFrom utils read.csv
#'
#' @examples
#' \donttest{
#' user_values = get_user_values()
#' }
get_user_values <- function() {
  tryCatch({
    user_values <- read.csv("https://climate-campaigners.app/api/user_values", check.names = FALSE)
    user_values
  }, error = function(e) {
    print(paste("Error:", e))
    return(NULL)
  })

}


#' Access weekly assignments API
#'
#' @return Data frame from API endpoint.
#' @export
#' @importFrom utils read.csv
#'
#' @examples
#' \donttest{
#' weekly_assignments = get_weekly_assignments()
#' }
get_weekly_assignments <- function() {
  tryCatch({
    weekly_assignments <- read.csv("https://climate-campaigners.app/api/weekly_assignment_matrix?pageSize=10000", check.names = FALSE)

    w <- weekly_assignments[,-1]

    l <- lapply(seq(ncol(w)/3) - 1, function(x) w[3 * x + 1:3])

    for (i in 1:length(l)) {
      cnt <- ifelse(l[[i]][,1] == 1 & l[[i]][,2] == 1, 1, 0)
      l[[i]] <- cbind(l[[i]], cnt)
      names(l[[i]])[4] <- gsub("selected", "prompt", names(l[[i]][1]))
    }

    do.call("cbind", l)

  }, error = function(e) {
    print(paste("Error:", e))
    return(NULL)
  })

}


#' Get subset
#'
#' @param data Data frame returned from \code{\link[climate.campaigneRs:get_data]{climate.campaigneRs::get_data()}}.
#' @param pattern Character vector giving the patterns that can be selected.
#'
#' @return Data frame only consisting of columns with column name according to \code{pattern}.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data <- get_data()
#' accepted <- get_subset(data, "\\.accepted")
#' rejected <- get_subset(data, "\\.rejected")
#' finished <- get_subset(data, "\\.finished")
#' interaction <- get_subset(data, "\\.interaction")
#' co2avoided <- get_subset(data, "\\.CO2avoided")
#' success <- get_subset(data, "\\.success")
#' }
get_subset <- function(data, pattern = "\\.accepted") {
  #stopifnot(pattern %in% c("\\.accepted", "\\.rejected", "\\.finished", "\\.interaction", "\\.CO2avoided", "\\.success"))
  df <- data[, grepl(pattern, colnames(data))]
  df <- cbind(data$uid, data$gender, data$age, data$country, data$income, data$education,
               data$occupation, df)
  #df <- cbind(data$uid,  df)
  names(df)[1:7] <- c("uid", "gender", "age", "country", "income", "education", "occupation")
  df
}



#' Create success variable from questions
#'
#' @param data Data frame returned from \code{\link[climate.campaigneRs:get_data]{climate.campaigneRs::get_data()}}.
#' @param challenges Data frame returned from \code{\link[climate.campaigneRs:get_challenges]{climate.campaigneRs::get_challenges()}}.
#'
#' @return List that contains the updated \code{data}, the list of challenges that
#' contain the question "Were you successful in completing this challenge?" and a list
#' of challenges that contain the question but do not occur in \code{data}.
#' @export
add_success <- function(data, challenges) {
  # 1. find variables that contain question for success
  idx <- c()
  for (i in 1:nrow(challenges)) {
    if ("Were you successful in completing this challenge?" %in% challenges[i,]) {
      idx <- rbind(idx, c(i, which(challenges[i,] %in% "Were you successful in completing this challenge?")))
    }
  }

  colnames(idx) <- c("row", "col")
  idx[ ,2] <- idx[ ,2] + 1
  success_list <- c()

  for (i in 1:nrow(idx)) {
    success_list <- rbind(success_list, c(challenges[idx[i], 1], challenges[idx[i,1], idx[i, 2]]))
  }

  # 2. find challenges that have a question for success but the variable does not occur in data
  missing <- c()
  for (j in 1:nrow(idx)) {
    if (!(success_list[j,2] %in% colnames(data))) {
      missing <- rbind(missing, c(success_list[j,1], success_list[j,2]))
    }
  }

  success_list <- as.data.frame(success_list)

  # 3. only keep success variables that are actually used (occur in user data)
  success_list <- success_list[success_list$V2 %in% colnames(data), ]

  rownames(success_list) <- NULL

  # 4. append "success" variable for each challenge
  for (i in 1:nrow(success_list)) {
    s <- data[,success_list[i,]$V2]
    data <- cbind(data, s)
    names(data)[ncol(data)] <- paste0("challenges.", success_list[i,]$V1, ".success")
  }

  l <- list(data, success_list, missing)
  names(l) <- c("data", "success_list", "missing")
  l
}

#' Helper function for aggregating challenges
#'
#' @param x A data frame.
#'
#' @return A data frame.
#' @noRd
#'
create_df <- function(x) {
  if (all(is.na(x))) {
    df <- rep(0, ncol(x))
    df <- as.data.frame(df)
    rows <- colnames(x)

  } else {
    x[!is.na(x)] <- 1
    x <- sapply(x, as.numeric)
    x <- as.data.frame(x)
    x[is.na(x)] <- 0

    df <- as.data.frame(colSums(x, na.rm = TRUE))
    rows <- rownames(df)
  }
  df <- cbind(rows, df)
  rownames(df) <- NULL
  names(df) <- c("challenge", "val")

  df$challenge <- gsub("challenges\\.", "", df$challenge)
  names(df)[2] <- unique(gsub("\\d*\\.", "", df$challenge))

  df$challenge <- gsub("\\..*", "", df$challenge)

  df

}

#' Aggregate challenges
#'
#' @param data Data frame returned from \code{\link[climate.campaigneRs:get_data]{climate.campaigneRs::get_data()}}.
#' @param challenges Data frame returned from \code{\link[climate.campaigneRs:get_challenges]{climate.campaigneRs::get_challenges()}}.
#'
#' @return A data frame that contains challenges and the counts of accepted, finished,
#' success, fail, open, diff (difference between finished, success and fail).
#' Challenges are considered as identical and are therefore aggregated when all variables
#' except id, start and end date are identical.

#' @export
#'
#' @examples
#' challenges <- get_challenges()
#' result <- get_data(challenges)
#' data <- result$data
#' aggregate_challenges(data, challenges)
aggregate_challenges <- function(data, challenges){
  challenge <- NULL
  df <- get_subset(data, "\\.accepted|\\.rejected|\\.finished|\\.success")
  # success:
  x <- df[, grepl("\\.success", colnames(df))]
  success <- colSums(x == "Yes", na.rm = TRUE)
  fail <- colSums(x == "No", na.rm = TRUE)
  x_df <- as.data.frame(cbind(success, fail))

  rows <- rownames(x_df)
  x_df <- cbind(rows, x_df)
  rownames(x_df) <- NULL
  names(x_df)[1] <- "challenge"
  x_df$challenge <- gsub("challenges\\.", "", x_df$challenge)
  x_df$challenge <- gsub("\\..*", "", x_df$challenge)

  # accepted finished rejected:
  accepted <- create_df(df[, grepl("\\.accepted", colnames(df))])
  finished <- create_df(df[, grepl("\\.finished", colnames(df))])
  rejected <- create_df(df[, grepl("\\.rejected", colnames(df))])


  # merge:
  m <- merge(accepted, finished, by = "challenge")
  m <- merge(m, rejected, by = "challenge")


  # merge with challenges
  challenges_subset <- challenges[, c("id", "category", "title", "duration")]
  challenges_df <- merge(m, challenges_subset, by.x = "challenge", by.y = "id")

  group_vars <- setdiff(names(challenges_df), c("challenge", "accepted", "finished", "rejected"))

  # group challenges
  challenges_df <- challenges_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(accepted = sum(accepted),
                     finished = sum(finished),
                     rejected = sum(rejected), .groups = "drop") |>
    dplyr::ungroup()

  success_df <- merge(x_df, challenges_subset, by.x = "challenge", by.y = "id")

  # remove duplicates
  success_df <- success_df[!duplicated(subset(success_df, select = -challenge)), ]


  final_df <- merge(challenges_df, subset(success_df, select = -challenge),
                    by = c("category", "title", "duration"), all.x = TRUE)
  final_df$success[is.na(final_df$success)] <- 0
  final_df$fail[is.na(final_df$fail)] <- 0

  final_df$open <- final_df$accepted - final_df$finished
  final_df$diff <- final_df$finished - final_df$success - final_df$fail

  final_df$unit <- sapply(strsplit(final_df$duration, split = "\\s+"), "[", 2)
  final_df$duration <- sapply(strsplit(final_df$duration, split = "\\s+"), "[", 1)
  final_df$duration <- as.numeric(final_df$duration)

  col_order <- c("title", "category", "duration", "unit", "accepted", "finished",
                 "success", "fail", "open", "diff")
  final_df <- final_df[, col_order]

  final_df
}

