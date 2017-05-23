
#
#   Object-as-List structure is a list with these parts
#       1) db_indivds as a dataframe, individs as rows, variables as cols
#       2) surfaces as a list of matrices, indexed by species
#       3) methods as functions that act on the object-as-list, returning updated copies
#       4) state as named list 
#
#


# ========================== DB SETUP ====================================

# -----------------------------------------------------------------------------
setup_db_obj = function(p_schema) {
# DESC: builds a db object. Don't know how to do objects yet, so object is
#       implemented as a list that's passed around.
#---------------------------------------------------  
  # setup state as a list, holding column names, types and pointers
  db_state = db_state_setup(p_schema)
  db_surfaces = db_setup_surfaces(db_state[['species_levels']])
  
  # return as list with null db as can't init without data length
  list(data='null', state=db_state, surfaces=db_surfaces)
}



# =============================== DATA ENTRY ===================================

# -----------------------------------------------------------------------------
insert_individuals = function(p_db_obj, p_data_to_insert) {
# DESC: initializes the db with data length & inserts the data
if (!is.data.frame(p_data_to_insert)) {stop('p_data_to_insert is not a dataframe')}  
#---------------------------------------------------
  # name pointers
  col_range = db_state_get_range(p_db_obj, 'data')
  row_range = 1:nrow(p_data_to_insert)
  if (ncol(p_data_to_insert) != (max(col_range)-min(col_range)+1)) 
                                             {stop('data to insert are too wide')}
  # init 
  p_db_obj = db_matrix_init(p_db_obj, nrow(p_data_to_insert))
  
  # insert the data into appropriate range and set width
  col_ptr = 0
  for (col_ptr in col_range){
      p_db_obj[['data']][[col_ptr]][row_range] = as.vector(p_data_to_insert[, col_ptr])
  }
  return(set_db_current_width(p_db_obj, max(col_range)))
}



# -----------------------------------------------------------------------------
insert_species_params = function(p_db_obj, p_params) {
# DESC: inserts the params into the db
if (!db_obj_is_valid(p_db_obj)) {stop("p_db_obj is not valid")}
if (!is.data.frame(p_params)) {stop('p_params is not a dataframe')}  
#---------------------------------------------------
  # get pointers
  col_range = db_state_get_range(p_db_obj, 'params')
  
  # populate each individ already in db with looked up params
  i=0
  for (row_ptr in 1:get_db_length(p_db_obj)){
    current_species = get_species_of_individ(p_db_obj, row_ptr)
    p_db_obj[['data']][row_ptr,][col_range] = as.vector(t(p_params[,current_species])) 
  } 
  return(set_db_current_width(p_db_obj, max(col_range)))
}

# -----------------------------------------------------------------------------
insert_recruitment_surface = function(p_db_obj, p_species, p_matrix) {
# DESC: inserts the params into the db
if (!db_obj_is_valid(p_db_obj)) {stop("p_db_obj is not valid")}
if (!is.matrix(p_matrix)) {stop('p_matrix is not a matrix')}  
if (!is.character(p_species)) {stop('p_species is not characters')}  
#---------------------------------------------------
    p_db_obj[['data']][[p_species]] = p_matrix
}



# -----------------------------------------------------------------------------
insert_derived_values = function(p_db_obj, p_values, p_label) {
# DESC: inserts the derived into the db
if (!is.character(p_label)) {stop('p_label is not string')}  
if (!db_obj_is_valid(p_db_obj))  {stop('p_db is not not valid')}  
if (!p_label %in% colnames(p_db_obj[['data']])) {stop("can't find col name")}
if (length(p_values) != nrow(p_db_obj[['data']])) {stop("values wrong length")}
#---------------------------------------------------
  # add values
  p_db_obj[['data']][p_label] = p_values

  # return(set_db_current_width(p_db, max(col_range)))
  p_db_obj
}



# ======================================= QUERIES ===================================

# -----------------------------------------------------------------------------
get_current_data = function(p_db_obj) {
# DESC: returns data frame with all populated columns
#------------------------------------------------------
  # only works for contigous regions
  # does nothing is data is null (ie not a dataframe)
  if (is.data.frame(p_db_obj[['data']])) {
        p_db_obj[['data']][1:get_db_current_width(p_db_obj)]
  } else {'null'}
}



# -----------------------------------------------------------------------------
get_species_of_individ = function(p_db_obj, p_individ_ptr) {
# DESC: get species of the specified individual
if (!(p_individ_ptr <= nrow(p_db_obj[['data']]))) {stop('ptr out of range')}  
db_names = c('Species')  
if (!all(db_names %in% colnames(p_db_obj[['data']]))) {stop("unrecognized col names")}
#---------------------------------------------------    
  p_db_obj[['data']][[db_names[1]]][p_individ_ptr]
}



# -----------------------------------------------------------------------------
get_all_individuals = function(p_db_obj) {
# DESC: returns data portion for all individuals  
#---------------------------------------------------  
  col_range = db_state_get_range(p_db_obj, 'data')
  p_db_obj[['data']][col_range]
}


# -----------------------------------------------------------------------------
get_one_individual = function(p_db_obj, p_individ_id) {
# DESC: returns entire row for an individual
#---------------------------------------------------  
  p_db_obj[['data']][p_individ_id , ]
}

# -----------------------------------------------------------------------------
get_individuals_by_species = function(p_db_obj, p_species_label) {
# DESC: returns entire row for all individuals of a given species
#---------------------------------------------------  
  p_db_obj[['data']][ p_db_obj[['data']][['Species']] == p_species_label, ]
}

# -----------------------------------------------------------------------------
get_derived_values = function(p_db_obj, derived_values_names) {
# DESC: returns entire columns of derived values for all individuals
#---------------------------------------------------  
  r = as.data.frame(p_db_obj[['data']][ , derived_values_names])
  colnames(r) = derived_values_names
  r
}


# =========================== PROPERTIES & UTILS ==============================

#--- property getters & setters 
# -----------------------------------------------------------------------------
get_db_length = function(p_db_obj) {
# DESC: returns number of rows in data structure
#---------------------------------------------------  
  p_db_obj[['state']][['db_length']]
}


# -----------------------------------------------------------------------------
get_db_current_width = function(p_db_obj) {
# DESC: returns number of populated columns in data structure
#---------------------------------------------------  
  p_db_obj[['state']][['current_width']]
}


# -----------------------------------------------------------------------------
set_db_length = function (p_db_obj, p_length){
# DESC: sets the length value in the state data structure
if (!is.integer(p_length)) {stop('p_length is not an integer')}
#---------------------------------------------------
  p_db_obj[['state']][['db_length']] = p_length
  p_db_obj
}


# -----------------------------------------------------------------------------
set_db_current_width = function (p_db_obj, p_width){
# DESC: sets the length value in the state data structure
if (!is.integer(p_width)) {stop('p_length is not an integer')}
#---------------------------------------------------
  p_db_obj[['state']][['current_width']] = p_width
  p_db_obj
}


# -----------------------------------------------------------------------------
db_state_get_range = function (p_db_obj, p_selector){
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
  range_start = p_db_obj[['state']][[selector]]['start']
  range_end = p_db_obj[['state']][[selector]]['end']
  return(range_start:range_end)
}


#------- validity checks ----------------

# -----------------------------------------------------------------------------
db_is_valid = function (p_db){
#---------------------------------------------------  
  r = TRUE
  r = r & (is.data.frame(p_db))
  return(r)
}

# -----------------------------------------------------------------------------
db_obj_is_valid = function (p_db_obj){
#---------------------------------------------------    
  r = TRUE
  # tests data types and labels of top level 
  r = r & (is.list(p_db_obj))
  r = r & (length(p_db_obj) = 3)
  r = r & (is.list(p_db_obj[['data']]))
  r = r & (is.list(p_db_obj[['state']]))
  r = r & (is.list(p_db_obj[['surfaces']]))
  # test the db structure
  r = r & (db_is_valid(p_db_obj[['data']]))
  return(r)
}

# ================================ SETUP ==================================


# -----------------------------------------------------------------------------
db_matrix_init = function(p_db_obj, p_db_length){
# DESC: iterates across the width of the db, initializing each's length
#---------------------------------------------------    
  # record length in state, name pointers
  p_db_obj = set_db_length(p_db_obj, p_db_length)
  col_width = p_db_obj[['state']][['db_width']]
  
  # name vectors
  all_names = p_db_obj[['state']][['all_names']]
  all_types = p_db_obj[['state']][['all_types']]
  all_species = p_db_obj[['state']][['species_levels']]
  
  # setup list of lists with correct types
  make_typed_col = function(i) {do.call(all_types[i], list(p_db_length))}
  list_of_lists = lapply(1:col_width, function(i) {make_typed_col(i)})
  names(list_of_lists) = all_names
  
  # assemble as data frame
  df = data.frame(list_of_lists, stringsAsFactors = FALSE)
  p_db_obj[['data']] = df
  
  p_db_obj
}


# -----------------------------------------------------------------------------
db_state_setup = function(p_schema) {
# DESC: builds a db object's state as a named list
#---------------------------------------------------  
  # name vectors
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
  species_colors = p_schema[[1]][['species_colors']]
  
  # compile into list with names
  schema_names = p_schema[[1]][['state_names']]
  values = list(all_names, all_types, species_levels, species_colors, 
                db_width, db_length, current_width, data_range, 
                params_range, derived_range)
  if (length(values) != length(schema_names)) {stop('wrong number of state values')}
  names(values) = schema_names
  
  return(values)
}


# -----------------------------------------------------------------------------
db_setup_surfaces = function(p_species_list) {
# DESC: preps a list of small matrices indexed by species names
#---------------------------------------------------  
empty_matrices = replicate(length(p_species_list), diag(2), simplify=FALSE)
names(empty_matrices) = p_species_list
empty_matrices
}


# ===================================== FILE HANDLING =======================================

# -----------------------------------------------------------------------------
load_pacala_data = function(p.path, p.file, p.params) {
# DESC: loads the data from a csv file, arranged as matrix
if (!is.character(p.path)) {stop('path is not a string')}
if (!is.character(p.file)) {stop('file is not a string')}
#---------------------------------------------------
  
  # read file  
  read.csv(file.path(p.path, p.file))
}


# -----------------------------------------------------------------------------
load_pacala_params = function(p.path, p.file) {
# DESC: loads the parameters
if (!is.character(p.path)) {stop('path is not a string')}
if (!is.character(p.file)) {stop('file is not a string')}
#---------------------------------------------------
  
  # read file
  read.csv(file.path(p.path, p.file))
}



# -----------------------------------------------------------------------------
load_schema = function(){
# DESC: load in hard coded literals, return as named list
#-------------------------------------  
  
  # literals
  data_names = c('ID', 'Species', 'Diameter', 'x', 'y')
  data_types = c('numeric', 'character', 'numeric', 'numeric', 'numeric')
  
  prm_names = c('P1','P2','A','S','U','V','h', 'Ei', 'W', 'D', 'F') 
  prm_types = c('numeric','numeric','numeric','numeric','numeric','numeric','numeric', 'numeric', 'numeric', 'numeric', 'numeric') 
 
  derived_names = c('light', 'ring_width', 'tree_height', 'growth_dist', 'mortality_func', 'Y_L', 'Y_D') 
  derived_types = c('numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric') 
 
  species_levels = c('FAGR','TSCA','ACSA','BELU','ACRU','FRAM','PIST')
  species_colors = c('red','green','blue','black','cyan','magenta','yellow')
  
  state_names = c('all_names', 'all_types', 'species_levels', 'species_colors', 'db_width', 
                  'db_length', 'current_width', 'data_range', 'params_range', 'derived_range')
  schema_names = c('data_names', 'data_types', 'prm_names', 'prm_types', 'derived_names', 
                   'derived_types', 'species_levels', 'species_colors', 'state_names')
  
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
  if (!is.character(species_colors)) {stop('species_colors is not strings')} 
  
  # assemble into list of lists then into a dataframe
  schema_values = list(data_names, data_types, 
                       prm_names, prm_types, 
                       derived_names, derived_types, 
                       species_levels, species_colors, 
                       state_names)
  schema_values = setNames(schema_values, schema_names)
  data.frame(I(schema_values))
  }



# ====================================== GRAPHICS =======================================

# -----------------------------------------------------------------------------
draw_landscape = function(p_db_obj, p_x_max, p_y_max) {
# DESC: draws the trees on the landscape
if (!db_obj_is_valid(p_db_obj)) {stop("p_db_obj is not a data frame")}
db_names = c('x','y','Diameter', 'Species')  
if (!all(db_names %in% colnames(p_db_obj[['data']]))) {stop("unrecognized col names")}
#---------------------------------------------------
  # name levels
  species_levels = p_db_obj[['state']][['species_levels']]
  level_colors = p_db_obj[['state']][['species_colors']]

  # name values
  x_coords = p_db_obj[['data']][[db_names[1]]]
  y_coords = p_db_obj[['data']][[db_names[2]]]
  diameters = p_db_obj[['data']][[db_names[3]]]
  
  species = p_db_obj[['data']][[db_names[4]]]
  species_id = lapply(species, function(x){which(species_levels == x)} )
  species_colors = unlist(lapply(species_id, function(x){level_colors[x]} ))

  x_range = range(0:(1.1*p_x_max))
  y_range = range(0:(1.1*p_y_max))
  
  # draw plot & legend
  symbols(x_coords, y_coords, 
          circles = diameters, bg=species_colors,
          xlim = x_range, ylim = y_range, 
          xaxt='n', yaxt='n', xlab='', ylab = '', 
          inches =FALSE, bty='L')
  
  axis(side=1, at=1:p_x_max)
  axis(side=2, at=1:p_y_max, las=2)
  
  par(xpd=TRUE) # to allow legend to extend past the plot area
  legend(x = x_range[2], y = (1.2*y_range[2]),
        legend = species_levels, 
        col = level_colors,
        pch = 19, cex = .7)
}


