GameLog <- R6::R6Class("GameLog",
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
                                                          lower = 2)

                             checkmate::assert_count(max_turns, positive = TRUE)

                            # construct data with the right dimensions                                                        lower = 2)
                             event_matrix <- matrix(NA, nrow = 0, ncol = code_length + 4)
                             event_data <- data.frame(event_matrix)
                             # add log entry for starting the game
                             first_entry <- c(list(time, "start new game"),
                                              as.list(rep(NA, code_length)),
                                              list(0, 0, max_turns))
                             event_data <- rbind(event_data, first_entry)
                             # add meaningful names
                             names(event_data) <- c("time", "event",
                                                    paste("pos", seq_len(code_length), sep = ""),
                                                    "positions_correct",
                                                    "colors_correct",
                                                    "remaining_turns")
                             # change format of time
                             event_data$time <- as.POSIXct(event_data$time, origin = "1970/01/01")
                             # assign it to the events attribute
                             self$events <- event_data
                           },
                           new_event = function(time,
                                                event_name,
                                                guesses,
                                                pos_correct,
                                                col_correct,
                                                turns) {
                             # check input
                             checkmate::assert_posixct(time, upper = Sys.time())
                             checkmate::assert_choice(event_name, c("guess",
                                                                    "game won",
                                                                    "game lost"))
                             checkmate::assert_character(guesses, min.chars = 1)
                             checkmate::assert_count(pos_correct)
                             checkmate::assert_count(col_correct)
                             checkmate::assert_count(turns)

                             # add to game log
                             new_entry <- c(list(time, event_name),
                                            as.list(guesses),
                                            list(pos_correct, col_correct, turns))
                             self$events <- rbind(self$events, new_entry)
                           },
                           print = function() {
                             print(self$events)
                           }
                         ))

ScoreBoard <- R6::R6Class("ScoreBoard",
                          private = list(
                            .difficulty = NA,
                            .n_colors = NA,
                            .code_length = NA,
                            .allow_duplicates = NA,
                            .scores = NA
                            ),
                          public = list(
                            initialize = function(difficulty = "customized",
                                                  colors,
                                                  code_length,
                                                  allow_duplicates) {
                              # input  checking
                              checkmate::assert_character(difficulty, min.chars = 1, len = 1, any.missing = FALSE)
                              checkmate::assert_integerish(colors, len = 1,
                                                                            any.missing = FALSE,
                                                                            lower = 2)
                              checkmate::assert_integerish(code_length,
                                                           len = 1,
                                                           any.missing = FALSE,
                                                           lower = 2)
                              checkmate::assert_flag(allow_duplicates)

                              # assign values
                              private$.difficulty <- difficulty
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
                              checkmate::assert_character(player_name, len = 1,
                                                          min.chars = 1, any.missing = FALSE)
                              checkmate::assert_posixct(date, upper = Sys.time())
                              checkmate::assert_count(turns_played, positive = TRUE)
                              checkmate::assert_number(game_duration)
                              checkmate::assert_class(game_duration, "Duration")

                              # add values to score board
                              score_board <- rbind(private$.scores, list(date,
                                                                         player_name,
                                                                         turns_played,
                                                                         game_duration))
                              # after first entries names must be set
                              if (nrow(score_board) == 1) {
                                names(score_board) <- c("date",
                                                        "player",
                                                        "turns_played",
                                                        "game_duration")
                              }
                              score_board <- dplyr::arange(score_board,
                                                           turns_played,
                                                           game_duration)
                            }
                          )
)

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
                      .game_log = NA
                    ),
                    public = list(
                      # used to open a new game
                      initialize = function(player_name,
                                            colors = 6,
                                            code_length = 4,
                                            allow_duplicates = TRUE,
                                            max_turns = 12) {
                        # check input
                        checkmate::assert_character(player_name, len = 1,
                                                    min.chars = 1, any.missing = FALSE)
                        # colors must be either a numeric value or a vector of colors
                        checkmate::assert(checkmate::check_integerish(colors,
                                                                      len = 1,
                                                                      any.missing = FALSE,
                                                                      lower = 2),
                                          checkmate::check_subset(colors, colors()))

                        checkmate::assert_integerish(code_length,
                                                     len = 1,
                                                     any.missing = FALSE,
                                                     lower = 2)
                        checkmate::assert_flag(allow_duplicates)
                        checkmate::assert_count(max_turns,
                                                     positive = TRUE)
                        # set variables
                        private$.player = player_name

                        # if colors is a number create a palette
                        if (is.numeric(colors)) {
                          private$.n_colors = colors
                          private$.colors = names(palette.colors(colors,
                                                                 palette = "Alphabet"))
                        } else {
                          # otherwise use the colors
                          private$.n_colors = length(colors)
                          private$.colors = colors
                        }

                        private$.code_length = code_length
                        private$.allow_duplicates = allow_duplicates
                        private$.max_turns = max_turns
                        private$.turns_played = 0
                        private$.game_log = GameLog$new(Sys.time(),
                                                        code_length,
                                                        max_turns)

                        # generate randomized code
                        private$.code = sample(private$.colors,
                                               size = code_length,
                                               replace = allow_duplicates)
                        # print game setup
                        cat("Setup game... \n")
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
                      # reset current game (new code with same configurations)
                      reset = function() {
                        private$.code = sample(private$.colors,
                                               size = private$.code_length,
                                               replace = private$.allow_duplicates)
                        private$.turns_played = 0
                        private$.game_log = GameLog$new(Sys.time(),
                                                        private$.code_length,
                                                        private$.max_turns)
                        cat("Reset game... \n")
                      },
                      # method to guess the code
                      guess = function(guess) {
                        ## check input
                        # the guess must have the same length as the code
                        checkmate::assert_character(guess,
                                                    len = private$.code_length)
                        # all entries of the vector must be in the pre-defined colors
                        checkmate::assert_subset(guess, private$.colors)
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
                        correct_col <- min(sum(guess %in% code),
                                           sum(code %in% guess)) - correct_pos
                        if (correct_pos == private$.code_length) {
                          # compute game duration
                          events <- private$.game_log$events
                          start_time <- events[1, 1]
                          end_time <- Sys.time()
                          time_score <- lubridate::as.duration(difftime(end_time, start_time))

                          # add entry to game log
                          private$.game_log$new_event(end_time,
                                                      "game won",
                                                      guess,
                                                      correct_pos,
                                                      correct_col,
                                                      turns_left)

                          # print congratulations and game stats
                          cat("Congratulations! You have won the game! \n")
                          cat("\n Game stats: \n")
                          self$print()
                          cat("\n")
                          cat(paste("Finished in: ", time_score))
                        }
                        else if (turns_left == 0) {
                          # compute game duration
                          events <- private$.game_log$events
                          start_time <- events[1, 1]
                          end_time <- Sys.time()
                          time_score <- lubridate::as.duration(difftime(end_time, start_time))

                          # add entry to game log
                          private$.game_log$new_event(end_time,
                                                      "game lost",
                                                      guess,
                                                      correct_pos,
                                                      correct_col,
                                                      turns_left)

                          # print GAME OVER message and game stats
                          cat("GAME OVER \n")
                          cat("The actual code was: ", private$.code, "\n")
                          cat("\n Game stats: \n")
                          self$print()
                          cat("\n")
                          cat(paste("Game duration: ", time_score))
                        } else {
                          # add entry to game log
                          private$.game_log$new_event(Sys.time(),
                                                      "guess",
                                                      guess,
                                                      correct_pos,
                                                      correct_col,
                                                      turns_left)
                          # print game log
                          private$.game_log$print()
                        }
                      }
                    ))
