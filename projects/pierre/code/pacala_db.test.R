library(testthat)
source(c_source_db_fullpath)

context('db')

# ================= DATA HANDLING ======================

#----- setup and utils ---
test_that('Db - object is set up', {
  # setup
  schema_list = load_schema()
  # execute
  r = setup_db_obj(schema_list)
  # test
  expect_is(r, 'list')
  expect_equal(length(r), 3)
  expect_is(r[[1]], 'character')
  expect_equal(r[[1]],'null')
  expect_is(r[[2]], 'list')
  expect_is(r[[3]], 'list')
  #expect_equal(length(r[[1]]), length(c(c_data_names, c_prm_names, c_derived_names)))
  expect_equal(length(r[[2]]), length(schema_list[[1]][['state_names']]))
})


#------- inserts ------------------

test_that('Db - data are inserted', {
  # setup
  db_obj = setup_test_db_with_schema()
  data_to_insert = load_pacala_data(c_file_path, c_data_file_name)
  # execute
  r = insert_individuals(db_obj, data_to_insert)
  # test
  expect_is(r, 'list')
  expect_equal(db_obj_is_valid(r), TRUE)
  expect_equal(r[['state']][['db_length']], nrow(data_to_insert))
  # test for values in the data range
  # test for nulls outside the data range
})


test_that('Db - params are inserted', {
  # setup
  db_obj = setup_test_db_with_schema_and_data(c_file_path, c_data_file_name)
  params_to_insert = load_pacala_params(c_file_path, c_params_file_name)
  # execute
  r = insert_species_params(db_obj, params_to_insert)
  # test
  expect_is(r, 'list')
  expect_equal(db_obj_is_valid(r), TRUE)
  expect_equal(r[['state']][['db_length']], 4)
  # test for values in the range
})



test_that('Db - derived values inserted, light', {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  derived_to_insert = seq(4)
  # 
  r = insert_derived_values(db_obj, derived_to_insert,'light')
  # test
  expect_is(r, 'list')
  expect_equal(db_obj_is_valid(r), TRUE)
  expect_equal(r[['state']][['db_length']], 4)
  # test for values in the range
})



#----- retrieve ---
test_that('Db - get current data before inserts', {
  # setup
  db_obj = setup_test_db_with_schema()
  # execute
  r = get_current_data(db_obj)
  # test
  expect_is(r, 'character')
  expect_equal(r, 'null')  
})

test_that('Db - get current data after inserting the data', {
  # setup
  db_obj = setup_test_db_with_schema_and_data(c_file_path, c_data_file_name)
  # execute
  r = get_current_data(db_obj)
  # test
  expect_is(r, 'data.frame')
  expect_equal(nrow(r), 4)  
  expect_equal(ncol(r), length(db_state_get_range(db_obj, 'data')))
})

test_that('Db - get current data after inserting the data & params', {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  r = get_current_data(db_obj)
  # test
  expect_is(r, 'data.frame')
  expect_equal(nrow(r), 4)  
  expect_equal(ncol(r), length(db_state_get_range(db_obj, 'data')) +
                        length(db_state_get_range(db_obj, 'params')))  
})


test_that('Db - species is retrieved for individual', {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  r = get_species_of_individ(db_obj, 2)
  # test
  expect_is(r, 'character')
  expect_equal(r, 'BELU')
})


test_that('Db - get all individuals', {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  r = get_all_individuals(db_obj)
  # test
  expect_is(r, 'data.frame')
  expect_equal(nrow(r), db_obj[['state']][['db_length']])  
  expect_equal(ncol(r), length(db_state_get_range(db_obj, 'data')) )  
})

test_that('Db - get one individual', {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  r = get_one_individual(db_obj, 2)
  # test
  expect_is(r, 'data.frame')
  expect_equal(nrow(r), 1)  
  expect_equal(ncol(r), db_obj[['state']][['db_width']])  
})


test_that('Db - get all individuals of a species', {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  r = get_individuals_by_species(db_obj, 'ACSA')
  # test
  expect_is(r, 'data.frame')
  expect_equal(nrow(r), 2)  
  expect_equal(ncol(r), db_obj[['state']][['db_width']])  
})

test_that('Db - get a single derived column', {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  r = get_derived_values(db_obj, 'light')
  # test
  expect_is(r, 'data.frame')
  expect_equal(nrow(r), db_obj[['state']][['db_length']])  
  expect_equal(ncol(r), 1)  
})

test_that('Db - get a multiple derived columns', {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, c_params_file_name)
  # execute
  r = get_derived_values(db_obj, c('ring_width', 'tree_height'))
  # test
  expect_is(r, 'data.frame')
  expect_equal(nrow(r), db_obj[['state']][['db_length']])  
  expect_equal(ncol(r), 2)  
})

#------------- properties -------------
test_that('Db - db length is returned', {
  # setup
  db_obj = setup_test_db_with_schema_and_data(c_file_path, c_data_file_name)
  # execute
  r = get_db_length(db_obj)
  # test
  expect_equal(r, db_obj[['state']][['db_length']])
})

test_that('Db - width is returned after initializing', {
  # setup
  # load schema and setup db
  db_obj = setup_test_db_with_schema()
  # execute
  r = get_db_current_width(db_obj)
  # test
  expect_equal(r, 0)
})

test_that('Db - width is returned after inserting data', {
  # setup
  db_obj = setup_test_db_with_schema_and_data(c_file_path, c_data_file_name)
  # execute
  r = get_db_current_width(db_obj)
  # test
  expect_equal(r, length(db_state_get_range(db_obj, 'data')))
})

test_that('Db - width is returned after inserting parameters', {
  # setup
  db_obj = setup_test_db(c_file_path, c_data_file_name, 
                        c_params_file_name)
  # execute
  r = get_db_current_width(db_obj)
  # test
  expect_equal(r, length(db_state_get_range(db_obj, 'data')) +
                  length(db_state_get_range(db_obj, 'params')) )
})

# ======================== FILES ==========================
context('files')
test_that('Files - Data are loaded', {
  # setup
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
  schema_list = load_schema()
  # execute
  r = load_pacala_params(c_file_path, c_params_file_name)
  # test
  expect_is(r, 'data.frame')
  expect_equal(colnames(r), schema_list[[1]][['species_levels']])
  expect_equal(row.names(r), schema_list[[1]][['prm_names']]) 
})

# ======================== GRAPHICS =============================
context('graphics')

test_that('Graphics - Landscape is drawn', {
  # setup
  db_obj = setup_test_db(c_file_path,  
                         c_data_file_name, c_params_file_name)
  # execute
  draw_landscape(db_obj, 10, 10)
  
  #test
  
})

