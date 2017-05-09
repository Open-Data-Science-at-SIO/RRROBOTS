library(testthat)

# =================== PATHS =======================
c_file_path = 'C:\\dev\\pacala\\data'
c_data_file_name = 'pacala_data.txt'
c_params_file_name = 'pacala_parameters.txt'

c_source_path = 'C:\\dev\\pacala\\code'
c_source_file_db = 'pacala_db.R'
c_source_file_pac = 'pacala.R'
c_test_file_db = 'pacala_db.test.R'
c_test_file_pac = 'pacala.test.R'
c_source_db_fullpath = paste(c_source_path, c_source_file_db, sep='\\')
c_source_pac_fullpath = paste(c_source_path, c_source_file_pac, sep='\\')


# ======================= TEST SETUP UTILITIES ==================================
setup_test_db = function(p_file_path, p_data_filename, p_params_filename){
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
  setup_db_obj(schema_list)
}
add_test_data = function(p_db, p_file_path, p_data_filename){
  data_to_insert = load_pacala_data(p_file_path, p_data_filename)
  insert_individuals(p_db, data_to_insert)
}
add_test_params = function(p_db, p_file_path, p_params_filename){
  params_to_insert = load_pacala_params(c_file_path, c_params_file_name)
  insert_species_params(p_db, params_to_insert)
}  

# ==================== DO TESTS ==========================
test_file(paste(c_source_path, c_test_file_db, sep='\\'))
test_file(paste(c_source_path, c_test_file_pac, sep='\\'))

