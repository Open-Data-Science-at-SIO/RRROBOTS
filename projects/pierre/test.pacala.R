library(testthat)


test_that("light vector is generated", {
  # setup
  source("pacala.R")
  light.cols = c('light')
  source_data = load_pacala_data("C:\\dev\\pacala", "pacala_data.txt")
  # execute
  r = generate_light_vector(source_data[["x"]], source_data[["y"]])
  # test
  expect_is(r, "data.frame")
  expect_equal(nrow(r), nrow(source_data))
  expect_equal(colnames(r), light.cols)
})

#================== SUBMODELS =========================

test_that("tree heights is computed", {
  # setup
  source("pacala.R")
  source_data = load_pacala_data("C:\\dev\\pacala", "pacala_data.txt")
  source_params = load_pacala_params("C:\\dev\\pacala", "pacala_parameters.txt")
  light = generate_light_vector(source_data[["x"]], source_data[["y"]])
  db = assemble_database(source_data, source_params, light)
  # execute
  r = figure_tree_height(db)
  # test
  expect_is(r, "numeric")
  expect_equal(length(r), nrow(db))
  })



test_that("growth submodels run", {
  # setup
  source("pacala.R")
  source_data = load_pacala_data("C:\\dev\\pacala", "pacala_data.txt")
  source_params = load_pacala_params("C:\\dev\\pacala", "pacala_parameters.txt")
  light = generate_light_vector(source_data[["x"]], source_data[["y"]])
  db = assemble_database(source_data, source_params, light)
  # execute
  r = growth_submodel(db)
  # test
  expect_is(r, "numeric")
  expect_equal(length(r), nrow(db))
})




#============ GRAPHICS =======================

#test_that("Trees drawn on landscape", {
#  source("pacala.R")
#  
#  db = load_pacala_data("C:\\dev\\pacala", "pacala_data.txt")
#  r = draw_landscape(db,1000,1000)
#})

