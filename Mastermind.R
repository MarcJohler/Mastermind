# class representing a game log for one game
GameLog <- R6::R6Class("GameLog",
  private = list(
    .max_turns = NA,
    .code_length = NA
  ),
  public = list(
    events = NA,
    initialize = function(time,
                          code_length,
                          max_turns) {
      # check input
      checkmate::assert_posixct(time, upper = Sys.time())
      checkmate::assert_integerish(code_length,
        len = 1,
        any.missing = FALSE,
        lower = 2
      )

      checkmate::assert_count(max_turns, positive = TRUE)

      # construct data with the right dimensions                                                        lower = 2)
      event_matrix <- matrix(NA, nrow = 0, ncol = code_length + 4)
      event_data <- data.frame(event_matrix)
      # add log entry for starting the game
      first_entry <- c(
        list(time, "start new game"),
        as.list(rep(NA, code_length)),
        list(0, 0, max_turns)
      )
      event_data <- rbind(event_data, first_entry)
      # add meaningful names
      names(event_data) <- c(
        "time", "event",
        paste("pos", seq_len(code_length), sep = ""),
        "positions_correct",
        "colors_correct",
        "remaining_turns"
      )
      # change format of time
      event_data$time <- as.POSIXct(event_data$time, origin = "1970/01/01")
      # assign it to the events attribute
      self$events <- event_data
      # assign max_turns and code_length to the corresponding attributes
      private$.max_turns <- max_turns
      private$.code_length <- code_length
    },
    new_event = function(time,
                         event_name,
                         guesses,
                         pos_correct,
                         col_correct,
                         turns) {
      # check input
      checkmate::assert_posixct(time, upper = Sys.time())
      checkmate::assert_choice(event_name, c(
        "guess",
        "game won",
        "game lost"
      ))
      checkmate::assert_character(guesses, min.chars = 1)
      checkmate::assert_count(pos_correct)
      checkmate::assert_count(col_correct)
      checkmate::assert_count(turns)

      # add to game log
      new_entry <- c(
        list(time, event_name),
        as.list(guesses),
        list(pos_correct, col_correct, turns)
      )
      self$events <- rbind(self$events, new_entry)
    },
    print = function() {
      print(self$events)
    },
    plot = function() {
      # check if there have already been guesses made
      if (nrow(self$events) == 1) {
        stop("No guesses have been made yet")
      }
      # save variables in calling environment for easier readability
      max_turns <- private$.max_turns
      code_length <- private$.code_length
      # get columns which contain color-guesses
      pos_columns <- stringr::str_detect(names(self$events), "^pos[:digit:]+")
      pos_columns <- self$events[-1, pos_columns]
      # create grid for plot of guesses
      guesses_grid <- data.frame(tidyr::pivot_longer(pos_columns,
        cols = names(pos_columns)
      ))
      # add turn variable
      guesses_grid$turn <- rep(seq_len(nrow(pos_columns)), each = code_length)
      # rename variables
      names(guesses_grid) <- c("position", "color", "turn")
      # compute a visually attractive number of breaks for turn axis
      turn_break_steps <- ceiling(max_turns / 12)
      turn_breaks <- seq(1, max_turns, by = turn_break_steps)
      # compute a nice size for the dots
      dot_size <- min(50 / code_length,
                      12.5 * 10 / max_turns)
      # plot colors in grid
      guess_plot <- ggplot2::ggplot(
        data = guesses_grid,
        mapping = ggplot2::aes(turn,
          position,
          color = color
        )
      ) +
        # change according to guess
        ggplot2::scale_color_manual(
          breaks = unique(guesses_grid$color),
          values = unique(guesses_grid$color)
        ) +
        ggplot2::geom_point(size = dot_size) +
        ggplot2::scale_y_discrete(name = NULL,
                                  breaks = NULL) +
        # extend axis to maximal allowed turns manually
        ggplot2::scale_x_continuous(
          name = "turn",
          breaks = turn_breaks,
          limits = c(0, max_turns + 0.5),
          # expand is necessary for axis being on the same height
          expand = ggplot2::expansion(add = 0)
        ) +
        ggplot2::theme_dark() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::coord_flip()

      # extract feedback information from events
      feedback_information <- self$events[-1, c("positions_correct", "colors_correct")]
      # transform into long format
      feedback_long <- data.frame(tidyr::pivot_longer(feedback_information,
        cols = names(feedback_information)
      ))
      # add turn variable
      feedback_long$turn <- rep(seq_len(nrow(feedback_information)), each = 2)
      # rename variables
      names(feedback_long) <- c("feedback_type", "frequency", "turn")
      # plot bar chart showing frequencys
      feedback_plot <- ggplot2::ggplot(feedback_long, mapping = ggplot2::aes(turn,
        frequency,
        fill = feedback_type
      )) +
        ggplot2::geom_bar(position = "stack", stat = "identity") +
        ggplot2::scale_y_continuous(
          name = NULL,
          breaks = seq(0, code_length),
          limits = c(0, code_length)
        ) +
        ggplot2::scale_x_continuous(
          name = NULL,
          breaks = turn_breaks,
          limits = c(0, max_turns + 0.5),
          # expand is necessary for axis being on the same height
          expand = ggplot2::expansion(add = 0)
        ) +
        # change the title, labels and colors of legend
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::scale_fill_manual(
          labels = c("Correct colors,\nwrong positions",
                     "Correct colors,\nright positions"),
          values = c("white", "black")
        ) +
        ggplot2::theme_dark() +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::coord_flip()

      # Combine the plots
      g <- gridExtra::gtable_cbind(ggplot2::ggplotGrob(guess_plot),
        ggplot2::ggplotGrob(feedback_plot),
        size = "max"
      )

      # Draw it
      grid::grid.newpage()
      grid::grid.draw(g)
    }
  )
)

# Class representing a score board for a specific configuration of games
ScoreBoard <- R6::R6Class("ScoreBoard",
  private = list(
    .n_colors = NA,
    .code_length = NA,
    .allow_duplicates = NA,
    .scores = NA
  ),
  public = list(
    initialize = function(colors,
                          code_length,
                          allow_duplicates) {
      # input  checking
      # currently are maximal 12 different colors available
      checkmate::assert_integerish(colors,
        len = 1,
        any.missing = FALSE,
        lower = 2,
        upper = 12
      )
      checkmate::assert_integerish(code_length,
        len = 1,
        any.missing = FALSE,
        lower = 2
      )
      checkmate::assert_flag(allow_duplicates)

      # assign values
      private$.n_colors <- colors
      private$.code_length <- code_length
      private$.allow_duplicates <- allow_duplicates

      # create score table
      private$.scores <- data.frame(matrix(NA, 0, 4))
    },
    print = function() {
      print(private$.scores)
    },
    new_entry = function(date,
                         player_name,
                         turns_played,
                         game_duration) {
      # check input
      checkmate::assert_character(player_name,
        len = 1,
        min.chars = 1, any.missing = FALSE
      )

      # if date has a tzone attribute, delete it, so that assertion will work
      attributes(date)$tzone <- NULL

      # proceed with input checks
      checkmate::assert_posixct(date, upper = Sys.time())
      checkmate::assert_count(turns_played, positive = TRUE)
      checkmate::assert_number(game_duration)
      checkmate::assert_class(game_duration, "Duration")

      # add values to score board
      score_board <- rbind(private$.scores, list(
        date,
        player_name,
        turns_played,
        game_duration
      ))
      # after first entries names must be set
      if (nrow(score_board) == 1) {
        names(score_board) <- c(
          "date",
          "player",
          "turns_played",
          "game_duration"
        )
        # change format of time
        score_board$date <- as.POSIXct(score_board$date,
          origin = "1970/01/01"
        )
      }
      score_board <- dplyr::arrange(
        score_board,
        turns_played,
        game_duration
      )
      private$.scores <- score_board
    }
  ),
  # access the attributes via active bindings from outside the class
  active = list(
    n_colors = function() {
      private$.n_colors
    },
    code_length = function() {
      private$.code_length
    },
    allow_duplicates = function() {
      private$.allow_duplicates
    }
  )
)


# class representing a game of Mastermind
Game <- R6::R6Class("Game",
  private = list(
    .player = NA,
    .n_colors = NA,
    .colors = NA,
    .code_length = NA,
    .allow_duplicates = NA,
    .max_turns = NA,
    .code = NA,
    .turns_played = NA,
    .game_log = NA,
    find_score_board = function() {
      # find all environment objects
      environments <- ls(pattern = "score_board", envir = globalenv())
      # loop over environments to find the score boards and check their configs
      for (object_name in environments) {
        object <- eval(parse(text = object_name))
        # if the object is a ScoreBoard, check its configuration
        if (class(object)[[1]] == "ScoreBoard") {
          # if the configuration fits return the object
          if (object$n_colors == private$.n_colors &&
            object$code_length == private$.code_length &&
            object$allow_duplicates == private$.allow_duplicates) {
            return(object)
          }
        }
      }
      # otherwise return an error
      stop("corresponding score board not found")
    }
  ),
  public = list(
    # used to open a new game
    initialize = function(player_name,
                          colors = 6,
                          code_length = 4,
                          allow_duplicates = TRUE,
                          max_turns = 12) {

      # check input
      checkmate::assert_character(player_name,
        len = 1,
        min.chars = 1, any.missing = FALSE
      )
      checkmate::assert_flag(allow_duplicates)
      # colors must be either a numeric value or a vector of colors
      # currently are maximal 12 different colors available
      checkmate::assert(
        checkmate::check_integerish(colors,
          len = 1,
          any.missing = FALSE,
          lower = 2,
          upper = 12
        ),
        checkmate::check_subset(colors, colors())
      )
      # if no duplicates are allowed in the code,
      # the length of the code is restricted to the number of colors
      checkmate::assert_integerish(code_length,
        len = 1,
        any.missing = FALSE,
        lower = 2,
        upper = ifelse(allow_duplicates, Inf,
                       ifelse(is.numeric(colors), colors, length(colors)))
      )
      checkmate::assert_count(max_turns,
        positive = TRUE
      )
      # set variables
      private$.player <- player_name

      # if colors is a number create a palette
      if (is.numeric(colors)) {
        # default list of colors
        potential_colors <-
          c(
            "red", "blue", "yellow", "green", "orange", "white",
            "purple", "brown", "pink", "cyan", "magenta", "turquoise"
          )

        private$.n_colors <- colors
        private$.colors <- potential_colors[seq_len(colors)]
      } else {
        # otherwise use the colors
        private$.n_colors <- length(colors)
        private$.colors <- colors
      }

      private$.code_length <- code_length
      private$.allow_duplicates <- allow_duplicates
      private$.max_turns <- max_turns
      private$.turns_played <- 0
      private$.game_log <- GameLog$new(
        Sys.time(),
        code_length,
        max_turns
      )

      # generate randomized code
      private$.code <- sample(private$.colors,
        size = code_length,
        replace = allow_duplicates
      )
      # print game setup
      cat("Setup game... \n")
      cat("Use colors {", private$.colors, "} for guesses \n")
    },
    # prints information on the current game, without showing the code
    print = function() {
      cat("Player Name: ", private$.player, "\n")
      cat("Allowed colors: ", private$.colors, "\n")
      cat("Code length: ", private$.code_length, "\n")
      cat("Duplicates allowed: ", private$.allow_duplicates, "\n")
      cat("Maximum of turns: ", private$.max_turns, "\n")
      cat("Turns played: ", private$.turns_played, "\n")
      cat("Turns remaining: ", private$.max_turns - private$.turns_played)
    },
    plot = function() {
      # simply uses the plot method on .game_log
      private$.game_log$plot()
    },
    # reset current game (new code with same configurations)
    # name can be adapted
    reset = function(player_name = private$.player) {
      # check input
      checkmate::assert_character(player_name,
                                  len = 1,
                                  min.chars = 1, any.missing = FALSE
      )
      # generate random code
      private$.player <- player_name
      private$.code <- sample(private$.colors,
        size = private$.code_length,
        replace = private$.allow_duplicates
      )
      private$.turns_played <- 0
      private$.game_log <- GameLog$new(
        Sys.time(),
        private$.code_length,
        private$.max_turns
      )
      cat("Reset game... \n")
      cat("Use colors {", private$.colors, "} for guesses \n")
    },
    # method to guess the code
    guess = function(..., show_logs = FALSE) {
      guess <- c(...)
      ## check input
      # the guess must have the same length as the code
      checkmate::assert_character(guess,
        len = private$.code_length
      )
      # all entries of the vector must be in the pre-defined colors
      checkmate::assert_subset(guess, private$.colors)
      # check show_legs to be a logical value
      checkmate::assert_flag(show_logs)
      # check duplicates are not allowed check if condition is fulfilled
      if (!(private$.allow_duplicates)) {
        if (!identical(guess, unique(guess))) {
          stop("'guess' invalid: duplicates are not allowed")
        }
      }
      # increase played turns
      private$.turns_played <- private$.turns_played + 1
      # turns left
      turns_left <- private$.max_turns - private$.turns_played
      # compare guess with the code
      code <- private$.code
      # number of right colors at right position
      correct_pos <- sum(guess == code)
      # number of right colors at wrong position
      remaining_code <- code[guess != code]
      remaining_guess <- guess[guess != code]
      correct_col <- 0
      for (color in remaining_guess) {
        i <- 1
        while (i <= length(remaining_code)) {
          if (color == remaining_code[i]) {
            correct_col <- correct_col + 1
            remaining_code <- remaining_code[-i]
            break
          }
          i <- i + 1
        }
      }
      if (correct_pos == private$.code_length) {
        # compute game duration
        events <- private$.game_log$events
        start_time <- events[1, 1]
        end_time <- Sys.time()
        time_score <- lubridate::as.duration(difftime(end_time, start_time))

        # add entry to game log
        private$.game_log$new_event(
          end_time,
          "game won",
          guess,
          correct_pos,
          correct_col,
          turns_left
        )

        # look if the score board for this configuration already exists
        score_board <- try(private$find_score_board(), silent = TRUE)
        # if score_board is not found create it
        if (class(score_board)[[1]] == "try-error") {
          score_board_name <- paste("score_board",
            private$.n_colors,
            private$.code_length,
            private$.allow_duplicates,
            sep = "_"
          )
          assign(score_board_name, ScoreBoard$new(
            private$.n_colors,
            private$.code_length,
            private$.allow_duplicates
          ),
          envir = globalenv()
          )
          score_board <- eval(parse(text = score_board_name))
        }

        # add new entry to score_board
        score_board$new_entry(
          start_time,
          private$.player,
          private$.turns_played,
          time_score
        )

        # plot guess
        private$.game_log$plot()

        # print congratulations and game stats
        cat("Congratulations! You have won the game! \n")
        cat("\n Game stats: \n")
        self$print()
        cat("\n")
        cat(paste("Finished in: ", time_score))
        cat("\n\n")
        self$reset()
      }
      else if (turns_left == 0) {
        # compute game duration
        events <- private$.game_log$events
        start_time <- events[1, 1]
        end_time <- Sys.time()
        time_score <- lubridate::as.duration(difftime(end_time, start_time))

        # add entry to game log
        private$.game_log$new_event(
          end_time,
          "game lost",
          guess,
          correct_pos,
          correct_col,
          turns_left
        )

        # plot guess
        private$.game_log$plot()

        # print GAME OVER message and game stats
        cat("GAME OVER \n")
        cat("The actual code was: ", private$.code, "\n")
        cat("\n Game stats: \n")
        self$print()
        cat("\n")
        cat(paste("Game duration: ", time_score))
      } else {
        # add entry to game log
        private$.game_log$new_event(
          Sys.time(),
          "guess",
          guess,
          correct_pos,
          correct_col,
          turns_left
        )
        # print game log if show_logs is set to TRUE
        if (show_logs) {
          private$.game_log$print()
        }
        # plot guess
        private$.game_log$plot()
      }
    }
  ),
  # active binding to print game log
  active = list(
    game_log = function() {
      private$.game_log
    }
  )
)

# play casual
game2 <- Game$new("marc")
game2$guess("red", "blue", "orange", "red")
