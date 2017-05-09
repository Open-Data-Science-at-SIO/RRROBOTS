library(testthat)
source(c_source_pac_fullpath)

# ============================== RESOURCES =====================================
context('resource submodels')

test_that("light vector is generated", {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  r = generate_light_vector(get_all_individuals(db_obj))
  # test
  expect_is(r, 'numeric')
  expect_equal(length(r), nrow(db_obj[['data']]))
})

test_that("resource submodel is run", {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  r = run_resources_submodel(db_obj)
  # test
  expect_equal(db_obj_is_valid(r), TRUE)
  # needs test for values in db
})


# ============================ GROWTH ==========================================
context('growth submodels')

test_that("tree ring widths are calculated", {
  # setup
  db_obj =  setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  db_obj = run_resources_submodel(db_obj)
  # execute
  r = calculate_ring_widths(db_obj[['data']])
  # test
  expect_is(r, 'numeric')
  expect_equal(length(r), nrow(db_obj[['data']]))
})

test_that("tree heights are calculated", {
  # setup
  db_obj =  setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  db_obj = run_resources_submodel(db_obj)
  # execute
  r = calculate_tree_height(db_obj[['data']])
  # test
  expect_is(r, 'numeric')
  expect_equal(length(r), nrow(db_obj[['data']]))
})


test_that("growth submodel is run", {
  # setup
  db_obj =  setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  db_obj = run_resources_submodel(db_obj)
  # execute
  r = run_growth_submodel(db_obj)
  # test
  expect_equal(db_obj_is_valid(r), TRUE)
  # needs test for values in db
})



# ======================== MORTALITY ==========================================
context('mortality submodels')

test_that("mortality vector is generated", {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  db_obj = run_resources_submodel(db_obj)
  db_obj = run_growth_submodel(db_obj)
  # execute
  r = calculate_mortality_vector(db_obj[['data']])
  # test - type & size
  expect_is(r, 'numeric')
  expect_equal(length(r), nrow(db_obj[['data']]))
})

test_that("growth rate distribution is generated", {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  db_obj = run_resources_submodel(db_obj)
  db_obj = run_growth_submodel(db_obj)
  # execute
  r = calculate_growth_rate_distribution(db_obj[['data']])
  # test - type & size
  expect_is(r, 'numeric')
  expect_equal(length(r), nrow(db_obj[['data']]))
})

test_that("mortality submodel is run", {
  # setup
  db_obj =  setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  db_obj = run_resources_submodel(db_obj)
  db_obj = run_growth_submodel(db_obj)
  # execute
  r = run_mortality_submodel(db_obj)
  # test
  expect_equal(db_obj_is_valid(r), TRUE)
  # needs test for values in db
})


# ======================== RECRUITMENT ==========================================
context('recruitment submodels')

test_that("tree contribution to a cell is calculated", {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  tree_individ = get_one_individual(db_obj, 1)
  # execute
  r = tree_contribution(tree_individ, 80, 80)
  # test
  expect_is(r, 'numeric')
  expect_equal(length(r), 1)
})

test_that("summed contribution to a cell is calculated", {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  r = summed_tree_contributions(db_obj[['data']],80, 80)
  # test 
  expect_is(r, 'numeric')
  expect_equal(length(r), 1)
})

test_that("recruitment matrix is made", {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  #r = make_seedling_matrix(db_obj[['data']],100,100, 'ACSA')
  # test 
  #expect_is(r, 'matrix')
})

test_that("species-specfic surafaces are made", {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  #r = make_all_matrices(db_obj[['data']],100,100, db_obj[['state']][['species_levels']])
  # test 
  #expect_is(r, 'list')
})

test_that("recruitment submodel is run", {
  # setup
  db_obj =  setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  #r = run_recruitment_submodel(db_obj, 100, 100)
  # test
  #expect_equal(db_obj_is_valid(r), TRUE)
})



