library(testthat)

# =================== PATHS =======================
c_file_path = 'C:\\dev\\pacala\\data'
c_data_file_name = 'pacala_data.txt'
c_params_file_name = 'pacala_parameters.txt'

# ======================= TEST SETUP UTILITIES ==================================
setup_test_db_with_schema_data_and_params = function(p_file_path, p_data_filename, p_params_filename){
  db_obj = setup_test_db_with_schema()
  db_obj = add_test_data(db_obj, p_file_path, p_data_filename)
  add_test_params(db_obj, p_file_path, p_params_filename)
}
setup_test_db_with_schema_and_data = function(p_file_path, p_data_filename){
  db_obj = setup_test_db_with_schema()
  add_test_data(db_obj, p_file_path, p_data_filename)
}
setup_test_db_with_schema = function(){
  schema_list = load_schema()
  setup_db(schema_list)
}
add_test_data = function(p_db, p_file_path, p_data_filename){
  data_to_insert = load_pacala_data(p_file_path, p_data_filename)
  insert_data_into_db(p_db, data_to_insert)
}
add_test_params = function(p_db, p_file_path, p_params_filename){
  params_to_insert = load_pacala_params(c_file_path, c_params_file_name)
  insert_db_params(p_db, params_to_insert)
}

# ================= DATA HANDLING ======================


#----- setup and utils ---
test_that('Db - object is set up', {
  # setup
  source('pacala_db.R')
  schema_list = load_schema()
  # execute
  r = setup_db(schema_list)
  # test
  expect_is(r, 'list')
  expect_equal(length(r), 2)
  expect_is(r[[1]], 'character')
  expect_equal(r[[1]],'null')
  expect_is(r[[2]], 'list')
  #expect_equal(length(r[[1]]), length(c(c_data_names, c_prm_names, c_derived_names)))
  expect_equal(length(r[[2]]), length(schema_list[[1]][['state_names']]))
})


#------- inserts ------------------

test_that('Db - data are inserted', {
  # setup
  source('pacala_db.R')
  db_obj = setup_test_db_with_schema()
  data_to_insert = load_pacala_data(c_file_path, c_data_file_name)
  # execute
  r = insert_data_into_db(db_obj, data_to_insert)
  # test
  expect_is(r, 'list')
  expect_equal(db_is_valid(r), TRUE)
  expect_equal(r[['state']][['db_length']], nrow(data_to_insert))
  # test for values in the data range
  # test for nulls outside the data range
})


test_that('Db - params are inserted', {
  # setup
  source('pacala_db.R')
  db_obj = setup_test_db_with_schema_and_data(c_file_path, c_data_file_name)
  params_to_insert = load_pacala_params(c_file_path, c_params_file_name)
  # execute
  r = insert_db_params(db_obj, params_to_insert)
  # test
  expect_is(r, 'list')
  expect_equal(db_is_valid(r), TRUE)
  expect_equal(r[['state']][['db_length']], 4)
  # test for values in the range
})



test_that('Db - derived values are populated', {
  # setup
  source('pacala_db.R')
  db_obj = setup_test_db_with_schema_data_and_params(c_file_path, c_data_file_name, 
                                                     c_params_file_name)
  derived_to_insert = seq(4)
  # execute
  r = insert_derived_values(db_obj, derived_to_insert)
  # test
  expect_is(r, 'list')
  expect_equal(db_is_valid(r), TRUE)
  expect_equal(r[['state']][['db_length']], 4)
  # test for values in the range
})



#----- retrieve ---
test_that('Db - get current data before inserts', {
  # setup
  source('pacala_db.R')
  db_obj = setup_test_db_with_schema()
  # execute
  r = get_current_data(db_obj)
  # test
  expect_is(r, 'character')
  expect_equal(r, 'null')  
})

test_that('Db - get current data after inserting the data', {
  # setup
  source('pacala_db.R')
  db_obj = setup_test_db_with_schema_and_data(c_file_path, c_data_file_name)
  # execute
  r = get_current_data(db_obj)
  # test
  expect_is(r, 'data.frame')
  expect_equal(nrow(r), 4)  
  expect_equal(ncol(r), 5)  
})

test_that('Db - get current data after inserting the data & params', {
  # setup
  source('pacala_db.R')
  db_obj = setup_test_db_with_schema_data_and_params(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  r = get_current_data(db_obj)
  # test
  expect_is(r, 'data.frame')
  expect_equal(nrow(r), 4)  
  expect_equal(ncol(r), 15)  
})


test_that('Db - species is retrieved for individual', {
  # setup
  source('pacala_db.R')
  db_obj = setup_test_db_with_schema_and_data(c_file_path, c_data_file_name)
  # execute
  r = get_species_of_individ(db_obj, 2)
  # test
  expect_is(r, 'character')
  expect_equal(r, 'BELU')
})


#------------- properties -------------
test_that('Db - db length is returned', {
  # setup
  source('pacala_db.R')
  db_obj = setup_test_db_with_schema_and_data(c_file_path, c_data_file_name)
  # execute
  r = get_db_length(db_obj)
  # test
  expect_equal(r, 4)
})

test_that('Db - db width is returned after initializing', {
  # setup
  source('pacala_db.R')
  # load schema and setup db
  db_obj = setup_test_db_with_schema()
  # execute
  r = get_db_current_width(db_obj)
  # test
  expect_equal(r, 0)
})

test_that('Db - db width is returned after inserting data', {
  # setup
  source('pacala_db.R')
  db_obj = setup_test_db_with_schema_and_data(c_file_path, c_data_file_name)
  # execute
  r = get_db_current_width(db_obj)
  # test
  expect_equal(r, 5)
})

test_that('Db - db width is returned after inserting parameters', {
  # setup
  source('pacala_db.R')
  db_obj = setup_test_db_with_schema_data_and_params(c_file_path, c_data_file_name, 
                                                     c_params_file_name)
  # execute
  r = get_db_current_width(db_obj)
  # test
  expect_equal(r, 15)
})

# ======================== FILES ==========================
test_that('Files - Data are loaded', {
  # setup
  source('pacala_db.R')
  schema_list = load_schema()
  # execute
  r = load_pacala_data(c_file_path, c_data_file_name)
  # test
  expect_is(r, 'data.frame')
  expect_equal(nrow(r), 4)
  expect_equal(colnames(r), schema_list[[1]][['data_names']])
})


test_that('Files - Parameters are loaded', {
  # setup
  source('pacala_db.R')
  schema_list = load_schema()
  # execute
  r = load_pacala_params(c_file_path, c_params_file_name)
  # test
  expect_is(r, 'data.frame')
  expect_equal(colnames(r), schema_list[[1]][['species_levels']])
  expect_equal(row.names(r), schema_list[[1]][['prm_names']])  
})

# ======================== GRAPHICS =============================

test_that('GRaphics - Landscape is drawn', {
  # setup
  source('pacala_db.R')
  db_obj = setup_test_db_with_schema_data_and_params(c_file_path, c_data_file_name, 
                                                     c_params_file_name)
  # execute
  
  #test
  
})

