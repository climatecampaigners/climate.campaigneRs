#' Access data API
#'
#' @importFrom httr modify_url GET content
#' @importFrom utils read.csv tail
#' @importFrom plyr rbind.fill
#' @return Modified Data frame from API endpoint.
get_data_api <- function() {
  base_url <- "https://climate-campaigners.app/api/data"
  page_size <- 1000
  cursor <- NULL
  data <- list()

  while (TRUE) {
    # Construct the API URL with the current cursor and page size
    url <- modify_url(
      base_url,
      query = list(
        cursor = cursor,
        pageSize = page_size
      )
    )

    response <- GET(url)

    if (response$status_code != 500) {

      response_data <- content(response, "text", encoding = "UTF-8")
      current_data <- read.csv(text = response_data, check.names = FALSE)

      data <- append(data, list(current_data))

      cursor <- tail(current_data$uid, n = 1)

      if (length(current_data$uid) < page_size) {
        break
      }
    } else {
      # If the status code is 500, there are no more records to return
      break
    }
  }

  # Combine all the data frames into one if needed
  final_data <- do.call(rbind.fill, data)
  final_data
}



#' Access data API and modify data
#' @description If \code{challenges} is provided, it appends the variables
#' \code{challenges.X.success}: indicate if a challenge with ID X was finished successfully ("Yes") or not ("No").
#' \code{success}: Number of successful completed challenges for each user.
#' \code{success_bin1}: Binary variable that indicates if a user completed one or more challenges.
#' \code{success_bin2}: Binary variable that indicates if a user completed two or more challenges.
#' \code{success_bin3}: Binary variable that indicates if a user completed three or more challenges.
#'
#'
#' @param challenges Optional data frame returned from \code{\link[climate.campaigneRs:get_challenges]{climate.campaigneRs::get_challenges()}}.
#'
#' @return Data frame from API endpoint.
#' @export
#' @importFrom utils read.csv
#' @return Modified Data frame from API endpoint.
#'
#' @examples
#' \donttest{
#' data <- get_data()
#' }
get_data <- function(challenges) {
  tryCatch({
    data <- get_data_api()
    # temp fix:
    swedish_col <- colnames(data)[sapply(data, function(x) any(grepl("^(Ingen|Ja)$", x)))]

    data[swedish_col] <- sapply(data[swedish_col], function(x) replace(x, x == "Ingen", "No"))
    data[swedish_col] <- sapply(data[swedish_col], function(x) replace(x, x == "Ja", "Yes"))

    # old income:
    # 5000: Less than 10 000 €
    # 15000: 10,000 - 20,000 €
    # 25000: 20,000 - 30,000 €
    # 35000: 30,000 - 40,000 €
    # 45000: 40,000 - 50,000 €
    # 55000: More than 50,000 €

    # new income (default):
    # 1: Less than 30 000 EUR
    # 2: 30 000 EUR - 40 000 EUR
    # 3: 40 000 EUR - 52 000 EUR
    # 4: More than 52 000 EUR
    # 5: I don’t want to say

    data[data$income %in% c(5000, 15000, 25000), ]$income <- 1
    data[data$income %in% c(35000), ]$income <- 2
    data[data$income %in% c(45000), ]$income <- 3
    data[data$income %in% c(55000), ]$income <- 4

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
    challenges$description <- gsub("^\"|\"$", "", challenges$description)

    challenges$len_description <- nchar(challenges$description)

    bins <- c(0, 200, 300, 400, 500, Inf)

    challenges$len_description_bin <- cut(challenges$len_description, breaks = bins,
                                          labels = c("0-200",
                                                     "201-300",
                                                     "301-400",
                                                     "401-500",
                                                     "more than 500"),
                                          include.lowest = TRUE)

    domains <- c()
    for (i in 1:nrow(challenges)) {
      ix <- which(challenges[i,] %in% "Were you successful in completing this challenge?")
      domains <- c(domains, ifelse(length(ix) > 0, challenges[i, (ix + 1)], "other"))
    }

    domains <- gsub("_.*", "", domains)
    domains[domains == "weFairChallengeGeneralSuccess"] <- "other"

    challenges$domains <- domains

    # special case for old challenges
    for (i in 1:nrow(challenges)) {
      if (challenges$category[i] != "Other" & challenges$domains[i] == "other") {
        if (challenges$category[i] == "Food") {
          challenges$domains[i] <- "diet"
        } else if (challenges$category[i] == "Mobility") {
          challenges$domains[i] <- "mobility"
        }

      }
    }

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
  # challenge.X.success: combination of _success and challenges.X.finished (otherwise duplicates)


  for (i in 1:nrow(challenges)) {
    if (any(grepl("_success$", challenges[i,]))) {
      idx <- rbind(idx, c(i, which(grepl("_success$", challenges[i,]))))
    }
  }

  colnames(idx) <- c("row", "col")
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
 # success_list <- success_list[success_list$V2 %in% colnames(data), ]

  rownames(success_list) <- NULL

  # 4. append "success" variable for each challenge
  for (i in 1:nrow(success_list)) {
    if (success_list$V2[i] %in% colnames(data)) {

      ch_id <- paste0("challenges.", success_list[i,]$V1, ".finished")

      if (ch_id %in% names(data)) {
        s <- ifelse(is.na(data[, ch_id]), NA, data[,success_list[i,]$V2])
      } else {
      s <- rep(NA, nrow(data))
    }

    data <- cbind(data, s)
    names(data)[ncol(data)] <- paste0("challenges.", success_list[i,]$V1, ".success")
    }
  }

  l <- list(data, success_list, missing)
  names(l) <- c("data", "success_list", "missing")
  success_vars <- grep("\\.success$", names(l$data), value = TRUE)
  success_df <- l$data[, success_vars]
  success_df[success_df == "Yes"] <- 1
  success_df[success_df == "No"] <- 0

  success_df <- apply(success_df, 2, as.numeric)

  success <- rowSums(success_df, na.rm = TRUE)

  l$data$success <- success
  l$data$success_bin1 <- ifelse(l$data$success >= 1, 1, 0)
  l$data$success_bin2 <- ifelse(l$data$success >= 2, 1, 0)
  l$data$success_bin3 <- ifelse(l$data$success >= 3, 1, 0)

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
  x_df <-  x[, !grepl("^city$|^country", names(x))]

  if (all(is.na(x_df))) {
    df <- rep(0, ncol(x_df))
    df <- as.data.frame(df)
    rows <- colnames(x_df)

  } else {
    x_df[!is.na(x_df)] <- 1
    x_df <- sapply(x_df, as.numeric)
    x_df <- as.data.frame(x_df)
    x_df[is.na(x_df)] <- 0

    x_df <- cbind(x$city, x$country, x_df)
    names(x_df)[1:2] <- c("city", "country")

    # df <- as.data.frame(colSums(x_df, na.rm = TRUE))


    df <- x_df |>
      group_by(country, city) |>
      summarise(across(everything(), sum)) |>
      ungroup()

    # drop any city / country with all zero
    df <- df[rowSums(df[,!(names(df) %in% c("country", "city"))])>0,]

    # Missing country or City name is "Unknown"
    df[is.na(df)] <- "Unknown"


    df_all_cities <- df |>
      group_by(country) |>
      summarise(across(-city, sum)) |>
      ungroup()

    df_all_cities$city <- "All cities"

    df_all <- df |>
      summarise(across(-c(country, city), sum)) |>
      ungroup()

    df_all$city <- "All"
    df_all$country <- "All"

    df_final <- rbind.fill(df, df_all_cities, df_all)

    df_final_long <- df_final |>
      pivot_longer(cols = starts_with("challenge"), names_to = "challenge", values_to = "val")



    #rows <- rownames(df)
  }
  #df <- cbind(rows, df)
  rownames(df_final_long) <- NULL
  #names(df) <- c("challenge", "val")

  df_final_long$challenge <- gsub("challenges\\.", "", df_final_long$challenge)
  names(df_final_long)[4] <- unique(gsub("\\d*\\.", "", df_final_long$challenge))

  df_final_long$challenge <- gsub("\\..*", "", df_final_long$challenge)

  df_final_long

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
#' @importFrom dplyr ungroup group_by across summarise
#' @examples
#' \donttest{
#' challenges <- get_challenges()
#' result <- get_data(challenges)
#' data <- result$data
#' aggregate_challenges(data, challenges)
#' }
aggregate_challenges <- function(data, challenges){
  challenge <- NULL
  df <- get_subset(data, "\\.accepted|\\.rejected|\\.finished|\\.success|^city$")

  # success:
  x <- df[, grepl("\\.success|^city$|^country$", colnames(df))]
  success <- colSums(x == "Yes", na.rm = TRUE)

  success <- x |>
    group_by(country, city) |>
    summarise(across(everything(), ~ sum(. == "Yes", na.rm = TRUE))) |>
    ungroup() |>
    pivot_longer(cols = starts_with("challenge"), names_to = "challenge", values_to = "success")

  success[is.na(success)] <- "Unknown"

  success_cities <- success |>
    group_by(country, challenge) |>
    summarise(across(-city, sum)) |>
    ungroup()

  success_cities$city <- "All cities"

  success_all <- success |>
    group_by(challenge) |>
    summarise(across(-c(country, city), sum)) |>
    ungroup()

  success_all$city <- "All"
  success_all$country <- "All"

  success_final <- rbind.fill(success, success_cities, success_all)


  fail <- x |>
    group_by(country, city) |>
    summarise(across(everything(), ~ sum(. == "No", na.rm = TRUE))) |>
    ungroup() |>
    pivot_longer(cols = starts_with("challenge"), names_to = "challenge", values_to = "fail")

  fail[is.na(fail)] <- "Unknown"

  fail_cities <- fail |>
    group_by(country, challenge) |>
    summarise(across(-city, sum)) |>
    ungroup()

  fail_cities$city <- "All cities"

  fail_all <- fail |>
    group_by(challenge) |>
    summarise(across(-c(country, city), sum)) |>
    ungroup()

  fail_all$city <- "All"
  fail_all$country <- "All"

  fail_final <- rbind.fill(fail, fail_cities, fail_all)


  #  fail <- colSums(x == "No", na.rm = TRUE)
  #  x_df <- as.data.frame(cbind(success, fail))
  x_df <- merge(success_final, fail_final, by = c("country", "city", "challenge"), all.x = TRUE, all.y = TRUE)

  #rows <- rownames(x_df)
  # x_df <- cbind(rows, x_df)
  rownames(x_df) <- NULL
  # names(x_df)[1] <- "challenge"
  x_df$challenge <- gsub("challenges\\.", "", x_df$challenge)
  x_df$challenge <- gsub("\\..*", "", x_df$challenge)

  # group challenges (by title)
  x_df <- merge(x_df, challenges[, c("id", "title")], by.x = "challenge", by.y = "id")


  # aggregate by same challenge name
  x_df <- x_df |>
    dplyr::group_by(title, country, city) |>
    dplyr::summarise(success = sum(success),
                     fail = sum(fail), .groups = "drop") |>
    dplyr::ungroup()



  # accepted finished rejected:
  accepted <- create_df(df[, grepl("\\.accepted|^city$|^country$", colnames(df))])
  finished <- create_df(df[, grepl("\\.finished|^city$|^country$", colnames(df))])
  rejected <- create_df(df[, grepl("\\.rejected|^city$|^country$", colnames(df))])



  # merge:
  m <- merge(accepted, finished, by = c("challenge", "country", "city"), all.x = TRUE, all.y = TRUE)
  m <- merge(m, rejected, by = c("challenge", "country", "city"), all.x = TRUE, all.y = TRUE)

  m[is.na(m)] <- 0

  # merge with challenges
  challenges_subset <- challenges[, c("id", "category", "title", "duration")]
  challenges_df <- merge(m, challenges_subset, by.x = "challenge", by.y = "id", all.x = TRUE)

  group_vars <- setdiff(names(challenges_df), c("challenge", "accepted", "finished", "rejected"))

  # group challenges by title and duration such that different challenges with the same name are summarised
  challenges_df <- challenges_df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(accepted = sum(accepted),
                     finished = sum(finished),
                     rejected = sum(rejected), .groups = "drop") |>
    dplyr::ungroup()



  final_df <- merge(x_df, challenges_df, by = c("title", "country", "city"))


  final_df$open <- final_df$accepted - final_df$finished
  final_df$diff <- final_df$finished - final_df$success - final_df$fail

  final_df$unit <- sapply(strsplit(final_df$duration, split = "\\s+"), "[", 2)
  final_df$duration <- sapply(strsplit(final_df$duration, split = "\\s+"), "[", 1)
  final_df$duration <- as.numeric(final_df$duration)

  col_order <- c("title", "category", "country", "city", "duration", "unit", "accepted", "finished",
                 "success", "fail", "open", "diff")
  final_df <- final_df[, col_order]

  final_df
}

