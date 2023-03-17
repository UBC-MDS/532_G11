library(shinytest2)






test_that("{shinytest2} recording: top3", {
  app <- AppDriver$new(variant = platform_variant(), name = "top3", height = 969, 
      width = 1619)
  app$set_inputs(genre_open = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(genre = c("Action", "Adventure", "Animation", "Biography", "Comedy", 
      "Crime", "Drama", "Family", "Fantasy", "Film-Noir", "History", "Horror", "Music", 
      "Musical", "Mystery", "Romance", "Sci-Fi", "Sport", "Thriller", "War", "Western"))
  app$set_inputs(genre_open = FALSE, allow_no_input_binding_ = TRUE)
  app$set_inputs(star_open = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(star = c("Chris Evans", "Morgan Freeman"))
  app$set_inputs(star_open = FALSE, allow_no_input_binding_ = TRUE)
  app$expect_screenshot()
})


test_that("{shinytest2} recording: willsmith", {
  app <- AppDriver$new(variant = platform_variant(), name = "willsmith", height = 969, 
      width = 1619)
  app$set_inputs(star_open = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(star = c("Morgan Freeman", "Will Smith"))
  app$set_inputs(star_open = FALSE, allow_no_input_binding_ = TRUE)
  app$set_inputs(genre_open = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(genre = c("Action", "Adventure", "Animation", "Biography", "Comedy", 
      "Crime", "Drama", "Family", "Fantasy", "Film-Noir", "History", "Horror", "Music", 
      "Musical", "Mystery", "Romance", "Sci-Fi", "Sport", "Thriller", "War", "Western"))
  app$set_inputs(genre_open = FALSE, allow_no_input_binding_ = TRUE)
  app$set_inputs(year = c(2004, 2020))
  app$expect_screenshot()
})


test_that("{shinytest2} recording: moviebygenre", {
  app <- AppDriver$new(variant = platform_variant(), name = "moviebygenre", height = 969, 
      width = 1619)
  app$set_inputs(genre_open = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(genre_open = FALSE, allow_no_input_binding_ = TRUE)
  app$set_inputs(genre_open = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(genre = c("Action", "Adventure", "Animation", "Biography", "Comedy", 
      "Crime", "Drama", "Family", "Fantasy", "Film-Noir", "History", "Horror", "Music", 
      "Musical", "Mystery", "Romance", "Sci-Fi", "Sport", "Thriller", "War", "Western"))
  app$set_inputs(genre_open = FALSE, allow_no_input_binding_ = TRUE)
  app$set_inputs(star_open = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(star_open = FALSE, allow_no_input_binding_ = TRUE)
  app$expect_screenshot()
})
