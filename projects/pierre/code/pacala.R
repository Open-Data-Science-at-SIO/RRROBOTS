
# ===================== SUBMODELS ========================

figure_tree_height = function(p_db){
# DESC: figure tree heights from the diameter 
if (!is.data.frame(p_db)) {stop("p_db is not a dataframe")}
db.names = c("A", "S", "Diameter")  
if (!all(db.names %in% colnames(p_db))) {stop("expecting colnames that aren't there")}
#---------------------------------------------------
#
#   Height (m) = A [ 1 - exp(-S/A * Diameter(cm)) ] + alpha 
#  
#   prm_A = asymptotic height
      prm_A = p_db[[db.names[1]]]  
#   prm_S = slope of function at zero diameter
      prm_S = p_db[[db.names[2]]]
#   Dia  = diameter (cm)
      prm_Dia = p_db[[db.names[3]]]  
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
  heights_no_deviate + deviates
}



generate_light_vector = function(p_db, p.x.coords, p.y.coords) {
# DESC: generate the light intensity for each individual's location
#       uses a point source at the origin and has intesity index as 
#       the distance from the origin     
if (!is.data.frame(p_db)) {stop("p_db is not a dataframe")}   
db.names = c("x", "y")  
if (!all(db.names %in% colnames(p_db))) {stop("expecting colnames that aren't there")}
#---------------------------------------------------
#  
#    light index = Sqrt[x^2 + y^2]
#
#    x, y = coordinates of centroid of tree
        prm_X = p_db[[db.names[1]]]
        prm_Y = p_db[[db.names[2]]] 
#---------------------------------------------------

  # assigning it to give it a name in the dataframe
  light = sqrt(prm_X^2 +  prm_Y^2)
  data.frame(light)
}



growth_submodel = function(p_db){
# DESC: uses the radius and resource level, together with parameters, to figure
#       the growth in the radius of the treee
if (!is.data.frame(p_db)) {stop("p.db is not a dataframe")}
db.names = c("Diameter", "light", "P1", "P2", "D")  
if (!all(db.names %in% colnames(p_db))) {stop("expecting colnames that aren't there")}
#---------------------------------------------------
#
#   Ring Width =  Radius * |_  (P1*light)  _\  + alpha 
#                          \ (P1/P2)+light  |
#  
#   radius = radius of tree
       radius = p_db[[db.names[1]]]/2
#   light = light index
       light_index = p_db[[db.names[2]]]
#   prm_P1 = asymptotic relative growth rate (ring width/radius)
       prm_P1 = p_db[[db.names[3]]]       
#   prm_P2 = slope of relative growth rate at zero light
       prm_P2 = p_db[[db.names[4]]]
#   prm_C = estimate of variance of alpha
       prm_C = p_db[[db.names[5]]] #using D as not sure what C is        
#   prm_D = estimate of variance of alpha
       prm_D = p_db[[db.names[5]]]
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
  ring_widths_no_deviate + deviates
}



