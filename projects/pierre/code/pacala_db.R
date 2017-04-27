
#
#       Object-as-List structure is a list with these parts
#           1) state as named list 
#           2) db as a datframe
#           3) methods as functions in the script that act on the object-as-list  
#
#


# ========================== DB SETUP ====================================

setup_db = function(p_schema) {
# DESC: builds a db object. Don't know how to do objects yet, so object is
#       implemented as a list that's passed around.
#---------------------------------------------------  

  # setup state as a list, holding column names, types and pointers
  db_state = db_state_setup(p_schema)
  
  # return as list with null db as can't init without data length
  list(data='null', state=db_state)
}



# =============================== DATA ENTRY ===================================

insert_data_into_db = function(p_db, p_data_to_insert) {
# DESC: initializes the db with data length & inserts the data
#if (!db_is_valid(p_db)) {stop("p_db is not valid")}
if (!is.data.frame(p_data_to_insert)) {stop('p_data_to_insert is not a dataframe')}  
#---------------------------------------------------
  # name pointers
  col_range = db_state_get_range(p_db, 'data')
  row_range = 1:nrow(p_data_to_insert)
  if (ncol(p_data_to_insert) != (max(col_range)-min(col_range)+1)) {stop('data to insert are too wide')}
  
  # init 
  p_db = db_matrix_init(p_db, nrow(p_data_to_insert))
  
  # insert the data into appropriate range and set width
  col_ptr = 0
  for (col_ptr in col_range){
      p_db[['data']][[col_ptr]][row_range] = as.vector(p_data_to_insert[, col_ptr])
  }
  return(set_db_current_width(p_db, max(col_range)))
}



insert_db_params = function(p_db, p_params) {
# DESC: inserts the params into the db
if (!db_is_valid(p_db)) {stop("p_db is not valid")}
if (!is.data.frame(p_params)) {stop('p_params is not a dataframe')}  
#---------------------------------------------------

  # get pointers
  col_range = db_state_get_range(p_db, 'params')
  
  # populate each individ already in db with looked up params
  i=0
  for (row_ptr in 1:get_db_length(p_db)){
    # lookup and insert species-specific parameters
    current_species = get_species_of_individ(p_db, row_ptr)
    p_db[['data']][row_ptr,][col_range] = as.vector(t(p_params[,current_species])) 
  } 
  return(set_db_current_width(p_db, max(col_range)))
}



insert_derived_values = function(p_db, p_light) {
# DESC: inserts the derived into the db
if (!is.numeric(p_light)) {stop('p_light is not numeric')}  
if (!db_is_valid(p_db))  {stop('p_db is not not valid')}  
#---------------------------------------------------

  # get pointers
  col_range = db_state_get_range(p_db, 'derived')
  
  # populate each individ already in db with looked up params
  i=0
  for (row_ptr in 1:get_db_length(p_db)){
    p_db[['data']][row_ptr,][col_range] = as.vector(p_light[row_ptr])
  } 
  return(set_db_current_width(p_db, max(col_range)))
}



# ======================================= QUERIES ===================================

get_current_data = function(p_db) {
# DESC: returns data frame with all populated columns
#------------------------------------------------------
  # only works for contigous regions
  # does nothing is data is null (ie not a dataframe)
  if (is.data.frame(p_db[['data']])) {
        p_db[['data']][1:get_db_current_width(p_db)]
  }
  else {
    'null'
  }
}



get_species_of_individ = function(p_db, p_individ_ptr) {
# DESC: get species of the specified individual
  
  p_db[['data']][['Species']][p_individ_ptr]
}



# ================================ DB_MATRIX ==================================
db_is_valid = function (p_db){

  r = TRUE
  r = r & (is.list(p_db))
  r = r & (length(p_db) = 2)
  r = r & (is.list(p_db[['data']]))
  r = r & (is.list(p_db[['state']]))
  return(r)
}



db_matrix_init = function(p_db, p_db_length){
# DESC: iterates across the width of the db, initializing each's length
  
  # record length in state, name pointers
  p_db = set_db_length(p_db, p_db_length)
  col_width = p_db[['state']][['db_width']]
  
  # name vectors
  all_names = p_db[['state']][['all_names']]
  all_types = p_db[['state']][['all_types']]
  all_species = p_db[['state']][['species_levels']]
    
  # setup list of lists with correct types
  make_typed_col = function(i) {do.call(all_types[i], list(p_db_length))}
  list_of_lists = lapply(1:col_width, function(i) {make_typed_col(i)})
  names(list_of_lists) = all_names

  # assemble as data frame
  df = data.frame(list_of_lists, stringsAsFactors = FALSE)
  p_db[['data']] = df
  
  p_db
}



# ================================== DB_STATE ==================================

#--- property getters & setters 

get_db_length = function(p_db) {
# DESC: returns number of rows in data structure
#---------------------------------------------------  
  p_db[['state']][['db_length']]
}

get_db_current_width = function(p_db) {
# DESC: returns number of populated columns in data structure
#---------------------------------------------------  
  p_db[['state']][['current_width']]
}

set_db_length = function (p_db, p_length){
# DESC: sets the length value in the state data structure
if (!is.integer(p_length)) {stop('p_length is not an integer')}
#---------------------------------------------------
  p_db[['state']][['db_length']] = p_length
  p_db
}

set_db_current_width = function (p_db, p_width){
# DESC: sets the length value in the state data structure
if (!is.integer(p_width)) {stop('p_length is not an integer')}
#---------------------------------------------------
  p_db[['state']][['current_width']] = p_width
  p_db
}

db_state_get_range = function (p_db, p_selector){
# DESC: returns start and end pointersfor the selected range
if (!is.character(p_selector)) {stop('p_selector is not a string')}
#---------------------------------------------------
  
  # pull range pointers out of data structure based on selector
  range_start=0
  range_end=0
  switch(p_selector,
         'data'={
           selector = 'data_range'
         },
         'params'={
           selector = 'params_range'
         },
         'derived'={
           selector = 'derived_range'
         },
         stop('no switch')
  )
  range_start = p_db[['state']][[selector]]['start']
  range_end = p_db[['state']][[selector]]['end']
  return(range_start:range_end)
}


#------- setup utils
db_state_setup = function(p_schema) {
# DESC: builds a db object's state as a named list
#---------------------------------------------------  
  # name vectos
  data_names = p_schema[[1]][['data_names']]
  data_types = p_schema[[1]][['data_types']]
  prm_names = p_schema[[1]][['prm_names']]
  prm_types = p_schema[[1]][['prm_types']]
  derived_names = p_schema[[1]][['derived_names']]
  derived_types = p_schema[[1]][['derived_types']]
  
  # collect all column names & types
  all_names = append(data_names, prm_names)
  all_names = append(all_names, derived_names)
  all_types = append(data_types, prm_types)
  all_types = append(all_types, derived_types)

  # keep track of range pointers for the columns
  data_range = c('start'=1, 'end'=length(data_names))
  data_end = data_range[['end']]
  params_range = c('start'=data_end + 1, 'end'=data_end + length(prm_names))
  params_end = params_range[['end']]
  derived_range = c('start'=params_end + 1, 'end'=params_end + length(derived_names))
  
  # data size
  db_width = length(all_names)
  db_length = 0
  current_width = 0
  
  # labels
  species_levels = p_schema[[1]][['species_levels']]

  
  # compile into list with names
  schema_names = p_schema[[1]][['state_names']]
  values = list(all_names, all_types, species_levels, db_width, db_length, 
                current_width, data_range, params_range, derived_range)
  if (length(values) != length(schema_names)) {stop('wrong number of state values')}
  names(values) = schema_names
  
  return(values)
}



# ===================================== FILE HANDLING =======================================

load_pacala_data = function(p.path, p.file, p.params) {
# DESC: loads the data from a csv file, arranged as matrix
if (!is.character(p.path)) {stop('path is not a string')}
if (!is.character(p.file)) {stop('file is not a string')}
#---------------------------------------------------
  
  # read file  
  read.csv(file.path(p.path, p.file))
}


load_pacala_params = function(p.path, p.file) {
# DESC: loads the parameters
if (!is.character(p.path)) {stop('path is not a string')}
if (!is.character(p.file)) {stop('file is not a string')}
#---------------------------------------------------
  
  # read file
  read.csv(file.path(p.path, p.file))
}

load_schema = function(){
# DESC: load in hard coded literals, return as named list
#-------------------------------------  
  
  # literals
  data_names = c('ID', 'Species', 'Diameter', 'x', 'y')
  data_types = c('numeric', 'character', 'numeric', 'numeric', 'numeric')
  prm_names = c('P1','P2','A','S','U','V','h', 'Ei', 'W', 'D') 
  prm_types = c('numeric','numeric','numeric','numeric','numeric','numeric','numeric', 'numeric', 'numeric', 'numeric') 
  derived_names = c('light') 
  derived_types = c('numeric') 
  species_levels = c('FAGR','TSCA','ACSA','BELU','ACRU','FRAM','PIST')
  state_names = c('all_names', 'all_types', 'species_levels', 'db_width', 'db_length',
                  'current_width', 'data_range', 'params_range', 'derived_range')
  schema_names = c('data_names', 'data_types', 'prm_names', 'prm_types', 
                   'derived_names', 'derived_types', 'species_levels', 'state_names')
  
  # define checker
  is_allowed_type = function(i){(i=='numeric')|(i=='character')}
  # run checks on the literals
  if (!is.character(data_names)) {stop('data_names is not strings')}
  if (length(data_names)!=length(data_types)) {stop('data_types is different length')}
  if (!all(unlist(lapply(data_types,is_allowed_type)))) {stop('data_types has unrecognized types ')}
  
  #  p_prm_names = rows of the parameter files, columns are species
  if (!is.character(prm_names)) {stop('prm_names is not strings')}
  if (length(prm_names)!=length(prm_types)) {stop('prm_types is different length')}
  if (!all(unlist(lapply(prm_types,is_allowed_type)))) {stop('prm_types has unrecognized types ')}
  
  #  p_derived_names = columns of the calculated derived
  if (!is.character(derived_names)) {stop('derived_names is not strings')}  
  if (length(derived_names)!=length(derived_types)) {stop('derived_types is different length')}
  if (!all(unlist(lapply(derived_types,is_allowed_type)))) {stop('derived_types has unrecognized types ')}
  
  #  p_state_names = labels of derived data metrics
  if (!is.character(state_names)) {stop('state_names is not strings')}  
  
  #  p_species_levels = labels of species
  if (!is.character(species_levels)) {stop('species_levels is not strings')} 
  
  # assemble into list of lists then into a dataframe
  schema_values = list(data_names, data_types, 
                       prm_names, prm_types, 
                       derived_names, derived_types, 
                       species_levels, state_names)
  schema_values = setNames(schema_values, schema_names)
  df = data.frame(I(schema_values))
  
  df
  }



# ====================================== GRAPHICS =======================================


draw_landscape = function(p.db, p.x.max, p.y.max) {
# DESC: draws the trees on the landscape
if (!is.data.frame(p.db)) {stop("p.db is not a data frame")}
#---------------------------------------------------

    symbols(p.db[["x"]], p.db[["y"]], 
          circles = p.db[["Diameter"]], bg=2:5,
          xlim = c(0,p.x.max), ylim =c(0,p.y.max),  
          xlab='', ylab = '', inches =FALSE)
}



