
# =============================== RESOURCES SUBMODEL ==========================

# -----------------------------------------------------------------------------
run_resources_submodel = function(p_db_obj){
# DESC: calculates resources and inserts them int othe db_obj
if (!db_obj_is_valid(p_db_obj)) {stop("p_db is not valid")}
#---------------------------------------------------
  light = generate_light_vector(p_db_obj[['data']])
  insert_derived_values(p_db_obj, light, 'light')
}


# -----------------------------------------------------------------------------
generate_light_vector = function(p_db) {
# DESC: generate the light intensity for each individual's location
#       uses a point source at the origin and has intesity index as 
#       the distance from the origin     
if (!db_is_valid(p_db)) {stop("p_db is not valid db")}   
db_names = c("x", "y")  
if (!all(db_names %in% colnames(p_db))) {stop("unrecognized col names")}
#---------------------------------------------------
#  
#    light index = Sqrt[x^2 + y^2]
#
#    x, y = coordinates of centroid of tree
        prm_X = p_db[[db_names[1]]]
        prm_Y = p_db[[db_names[2]]] 
#---------------------------------------------------

  # assigning it to give it a name in the dataframe
  light = sqrt(prm_X^2 +  prm_Y^2)
  as.vector(light)
}


# =============================== GROWTH SUBMODEL==============================

# -----------------------------------------------------------------------------
run_growth_submodel = function(p_db_obj){
# DESC: takes the calculated resources and calculates growth
if (!db_obj_is_valid(p_db_obj)) {stop("p_db_obj is not valid")}
#---------------------------------------------------

  ring_widths = calculate_ring_widths(p_db_obj[['data']])
  tree_heights = calculate_tree_height(p_db_obj[['data']])  
  
  p_db_obj = insert_derived_values(p_db_obj, ring_widths, 'ring_width')
  p_db_obj = insert_derived_values(p_db_obj, tree_heights, 'tree_height')
}


# -----------------------------------------------------------------------------
calculate_ring_widths = function(p_db){
# DESC: uses the radius and resource level, together with parameters, to figure
#       the growth in the radius of the treee
if (!db_is_valid(p_db)) {stop("p_db is not valid db")}
db_names = c("Diameter", "light", "P1", "P2", "D")  
if (!all(db_names %in% colnames(p_db))) {stop("unrecognized col names")}
#---------------------------------------------------
#
#   Ring Width =  Radius * |___(P1 * light)___\  + alpha 
#                          \ (P1/P2) + light  |
#  
#   radius = radius of tree
       radius = p_db[[db_names[1]]]/2
#   light = light index
       light_index = p_db[[db_names[2]]]
#   prm_P1 = asymptotic relative growth rate (ring width/radius)
       prm_P1 = p_db[[db_names[3]]]       
#   prm_P2 = slope of relative growth rate at zero light
       prm_P2 = p_db[[db_names[4]]]
#   prm_C = estimate of variance of alpha
       prm_C = p_db[[db_names[5]]] #using D as not sure what C is        
#   prm_D = estimate of variance of alpha
       prm_D = p_db[[db_names[5]]]
#   alpha  = normal random variable with zero mean and variance below
#   var(alpha) = C * [Predicted Ring Width]^D
#---------------------------------------------------  

  # compute derived values
  growth_coeff = ((prm_P1 * light_index) / ((prm_P1 / prm_P2) + light_index) )
  ring_widths_no_deviate  = radius * growth_coeff
  # generate random variable
  deviate_variance = (prm_C * ring_widths_no_deviate)^prm_D
  deviates = rnorm(n = nrow(p_db), mean = 0, sd = sqrt(deviate_variance))  
  # combine
  as.vector(ring_widths_no_deviate + deviates)
}


# -----------------------------------------------------------------------------
calculate_tree_height = function(p_db){
# DESC: figure tree heights from the diameter 
if (!db_is_valid(p_db)) {stop("p_db is not valid db")}
db_names = c("A", "S", "Diameter")  
if (!all(db_names %in% colnames(p_db))) {stop("unrecognized col names")}
#---------------------------------------------------
#
#   Height (m) = A [ 1 - exp(-S/A * Diameter(cm)) ] + alpha 
#  
#   prm_A = asymptotic height
  prm_A = p_db[[db_names[1]]]  
#   prm_S = slope of function at zero diameter
  prm_S = p_db[[db_names[2]]]
#   Dia  = diameter (cm)
  prm_Dia = p_db[[db_names[3]]]  
#   alpha = normal random variable with zero mean and
#           variance increasing as power law with predicted mean
#---------------------------------------------------      
  # get height from species parameters and diameter
  height_coeffs = (1 - exp(-prm_S/prm_A * prm_Dia))  
  heights_no_deviate = prm_A * height_coeffs
  
  # generate random variable
  deviate_variance = heights_no_deviate/30 #stub
  deviates = rnorm(n = nrow(p_db), mean = 0, sd = sqrt(deviate_variance))  
  # combine
  as.vector(heights_no_deviate + deviates)
}



# =============================== MORTALITY SUBMODEL ==========================

# -----------------------------------------------------------------------------
run_mortality_submodel = function(p_db_obj){
# DESC: 
#
if (!db_obj_is_valid(p_db_obj)) {stop("p_db_obj is not valid")}
#---------------------------------------------------
#
#  Conditional Distributions Y_D(g) and Y_L(g)
#  
#     Y_D(g) =  |_____X(g) M(g)_______\
#               \ integ{M(g) X(g) dg} |
# 
#     Y_L(g) =  |______[1- M(g)] X(g)_______\
#               \ integ{[1 - M(g)] X(g) dg} |
#
#     g   = growth rate
#     X(g) = distribution of growth rates in a site
#            we assume a flexible distribution, usually a gamma-distribution
#     M(g) = mortality function
#            we assume a functional form    
#       both interals are from -inf to +inf  
                
#---------------------------------------------------  
  growth_rates_dist = calculate_growth_rate_distribution(p_db_obj[['data']])
  mortality = calculate_mortality_vector(p_db_obj[['data']])
  Y_D = calculate_Y_D(p_db_obj[['data']], growth_rates_dist, mortality)
  Y_L = calculate_Y_L(p_db_obj[['data']], growth_rates_dist, mortality)
  
  p_db_obj = insert_derived_values(p_db_obj, growth_rates_dist, 'growth_dist')
  p_db_obj = insert_derived_values(p_db_obj, mortality, 'mortality_func')
  p_db_obj = insert_derived_values(p_db_obj, Y_D, 'Y_D')
  p_db_obj = insert_derived_values(p_db_obj, Y_L, 'Y_L')
}

# -----------------------------------------------------------------------------
calculate_Y_D = function(p_db, p_prmX, p_prmM){
  # DESC: 
  if (!db_is_valid(p_db)) {stop("p_db is not valid db")}
  #---------------------------------------------------      
  numerator = p_prmX * p_prmM
  denomator = p_prmX * p_prmM
  
  numerator/(denomator + 1)
}


# -----------------------------------------------------------------------------
calculate_Y_L = function(p_db, p_prmX, p_prmM){
  # DESC: 
  if (!db_is_valid(p_db)) {stop("p_db is not valid db")}
  #---------------------------------------------------      
  numerator = (1 - p_prmM) * p_prmX
  denomator = (1 - p_prmM) * p_prmX
  
  numerator/(denomator+1)
}



# -----------------------------------------------------------------------------
calculate_growth_rate_distribution = function(p_db){
  # DESC: create distribution of growth rates
  if (!db_is_valid(p_db)) {stop("p_db is not valid db")}
  #---------------------------------------------------      
  rgamma(nrow(p_db), shape= 1, rate = 1)
}


# -----------------------------------------------------------------------------
calculate_mortality_vector = function(p_db){
# DESC: 
if (!db_is_valid(p_db)) {stop("p_db is not valid db")}
db_names = c('U', 'V', 'ring_width')  
if (!all(db_names %in% colnames(p_db))) {stop("unrecognized col names")}
#---------------------------------------------------
#  
#         M(g) = exp( -U[Av Ring Width]^V )
#
#   M(g) =  mortality function
#    g   =  growth rate
#
#   U, V  are constant
      prm_U = p_db[[db_names[1]]]
      prm_V = p_db[[db_names[2]]]                       
#
#   Av Ring Width = 5yrs arithmetic mean  (using current ring width as stub)
      prm_Ring_Width = p_db[[db_names[3]]]    
#---------------------------------------------------      
  # compute parts
  ring_width_exponent = prm_Ring_Width^prm_V
  operand_of_exp = -prm_U * ring_width_exponent

  # combine
  exp(operand_of_exp )
}


# ========================= RECRUITMENT SUBMODEL =====================

# -----------------------------------------------------------------------------
run_recruitment_submodel = function(p_db_obj, p_max_x, p_max_y){
# DESC: 
if (!db_obj_is_valid(p_db_obj)) {stop("p_db_obj is not valid")}
# ------------------------------------ 
#
#    Modeling the seedling shadow of a single tree by that the mean density of 
#     seedling produced by a tree falls with distance as 
#
#          N = F*(DBH)^2 exp(-h distance^3)
#
#           for  N    =  Seedling Density
#               F, h  =  constants
# 
#   so total density of seedlings expected in the j^th quadrat, N_j
#
#         N_j = sum{from i=0 to q}[F*(D_i)^2 exp(-h (d_ij)^3)]
#
#          for  d_ij =  distance from i^th tree to the j^th seedling quadrat
#               D_i  =  DBH of i^th conspecific tree
#                q   =  number of conspecific trees in stand
# --------------------------------------------------------  

  species_names = p_db_obj[['state']][['species_levels']]  
  
  # iterate over each quadrat of plot
  surfaces = make_all_matrices(p_db_obj[['data']], p_max_x, p_max_y, species_names)
  p_db_obj[['surfaces']] = surfaces
  
  p_db_obj
}



# -----------------------------------------------------------------------------
make_all_matrices = function(p_db, p_max_x, p_max_y, p_species) {
# DESC: 
if (!is.character(p_species)) {stop("p_species is not string")}
# ------------------------------------     
  
  species_matrices = replicate(length(p_species), diag(1), simplify=FALSE)
  
  # iterate over species
  for (ptr in 1:length(p_species)) {
    species_matrices[[ptr]] = make_seedling_matrix(p_db, p_max_x, p_max_y, p_species[ptr])
  }
  species_matrices
}



# -----------------------------------------------------------------------------
make_seedling_matrix = function(p_db, p_max_x, p_max_y, p_species_name) {
# DESC: 
if (!(is.numeric(p_max_x) & is.numeric(p_max_y))) {stop("p_max coords not numeric")}
if (!is.character(p_species_name)) {stop("p_species not a string")}
db_names = c('Species')  
if (!all(db_names %in% colnames(p_db))) {stop("unrecognized col names")}
# ------------------------------------ 
  
  # prep matrix with zero's
  density_mat = matrix(rep(0, p_max_x*p_max_y), 
                       nrow = p_max_y, ncol = p_max_x)
  
  # filter conspecifics
  conspecifics = p_db[p_db[db_names[1]] == p_species_name,]
  
  # if there are conspecifics, iterate over each cell
  if (nrow(conspecifics) > 0) {
    for (quad_x in 1:p_max_x) {
      for (quad_y in 1:p_max_y) {
        density_mat[quad_y, quad_x] = summed_tree_contributions(conspecifics, quad_x, quad_y)
    } } } 
  density_mat
}



# -----------------------------------------------------------------------------
summed_tree_contributions = function(p_conspecifics, p_quad_x, p_quad_y) {
# DESC: sums contribution of all conspecific trees to a given quadrat
if (!is.data.frame(p_conspecifics)) {stop('p_conspecifics is not a dataframe')} 
# ------------------------------------ 
  # iterate over each conspecific tree
  sum_of_contribs = 0
  for (tree in 1:nrow(p_conspecifics)) {
    sum_of_contribs = sum_of_contribs + tree_contribution(p_conspecifics[tree,], 
                                                          p_quad_x, p_quad_y)
  } 
  sum_of_contribs
}



# -----------------------------------------------------------------------------
tree_contribution = function(p_individ, p_quad_x, p_quad_y) {
# DESC: calculates the contribution of a single tree to the seedling density at a given point
if (!is.data.frame(p_individ)) {stop('p_indiv not a dataframe')}
if (!(is.numeric(p_quad_x) & is.numeric(p_quad_y))) {stop("quad coords not numeric")}  
db_names = c('x','y','h','F','Diameter')  
#if (!all(db_names %in% colnames(p_individ))) {stop("unrecognized col names")}
# ------------------------------------ 
  # name values
  loc_x = p_individ[[db_names[1]]][1]
  loc_y = p_individ[[db_names[2]]][1]
  h_value = p_individ[[db_names[3]]][1]
  F_value = p_individ[[db_names[4]]][1]
  tree_diameter = p_individ[[db_names[5]]][1]

  # figure distance
  tree_d_ij = (sqrt((loc_x - p_quad_x)^2 + (loc_y - p_quad_y)^2)) / 100
  
  # expression parts
  operand_of_exp = -h_value * tree_d_ij^3
  coeff_part = F_value * tree_diameter^2
  
  # combine
  coeff_part  * exp(operand_of_exp)
}


