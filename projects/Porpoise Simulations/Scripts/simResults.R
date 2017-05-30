
library(dplyr)
library(tidyr)

# Uniform decline, unstratified
load("./Data/simSpatialb0Resultsx1000.RData")
spread(tab.res, P.Change, P.Dect)

# No.M  -0.5 -0.25  -0.1  0.25
# 1   10 0.826 0.232 0.082 0.150
# 2   25 0.997 0.512 0.128 0.354
# 3   50 1.000 0.761 0.228 0.555
# 4   75 1.000 0.866 0.317 0.714
# 5  100 1.000 0.924 0.337 0.790

# Range contraction, unstratified
load("./Data/simSpatialb1Resultsx1000.RData")
spread(tab.res, P.Change, P.Dect)
# No.M  -0.5 -0.25  -0.1  0.25
# 1   10 0.693 0.303 0.181 0.267
# 2   25 0.868 0.493 0.274 0.378
# 3   50 0.932 0.587 0.372 0.499
# 4   75 0.954 0.636 0.420 0.571
# 5  100 0.979 0.694 0.464 0.600

# Uniform decline, stratified
load("./Data/simSpatialStratb0Resultsx1000.RData")
spread(tab.res, P.Change, P.Dect)

# No.M  -0.5 -0.25  -0.1  0.25
# 1   10 0.832 0.225 0.071 0.185
# 2   25 0.998 0.507 0.140 0.346
# 3   50 1.000 0.746 0.217 0.538
# 4   75 1.000 0.871 0.269 0.710
# 5  100 1.000 0.935 0.337 0.782

# Range contraction, stratified
load("./Data/simSpatialStratb1Resultsx1000.RData")
spread(tab.res, P.Change, P.Dect)
# No.M  -0.5 -0.25  -0.1  0.25
# 1   10 0.724 0.284 0.150 0.226
# 2   25 0.910 0.485 0.242 0.379
# 3   50 0.959 0.597 0.350 0.507
# 4   75 0.985 0.670 0.389 0.595
# 5  100 0.989 0.729 0.400 0.615


