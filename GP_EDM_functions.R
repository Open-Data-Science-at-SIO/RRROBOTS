makeblock <- function(data,cols,lags,lib=NULL){
  # [block]=makeblock(data,cols,lags,varargin)
  # take matrix of time-series. construct data block for block_lnlp using
  # lags of columns of the matrix specified in "cols" and "lags".
  #
  # INPUT:
  # data -- matrix of various time series to construct block from.
  # cols -- identify the columns of the matrix to use for each lag. AGNOSTIC ABOUT TIME COL
  # lib -- specify break-points in the data.
  #
  
  ncol = length(cols)
  n_row = dim(data)[1]
  
  
  if(is.null(lib)){
    lib = matrix(c(1,n_row),1,2)
  }
  
  block = matrix(NA,n_row,ncol);
  
  # loop over each chunk of library and insert nans in column
  # corresponding to physical variable to make desired lag
  
  for (tchunk in 1:(dim(lib)[1])){
    for (datadex in 1:ncol){
      
      if (abs(lags[datadex])>=length(lib[tchunk,1]:lib[tchunk,2])){
      }else if (lags[datadex] >= 0){
        block[lib[tchunk,1]:lib[tchunk,2],datadex] <- 
          c( rep(NA,lags[datadex]), data[lib[tchunk,1]:(lib[tchunk,2]-lags[datadex]),cols[datadex]] );
      }
      else{
        block[lib[tchunk,1]:lib[tchunk,2],datadex] <- 
          c(data[(lib[tchunk,1]-lags[datadex]):lib[tchunk,2],cols[datadex]], rep(NA,-lags[datadex]))
      }
    }
  }
  
  return(block)
  
}

compute_stats <- function(obs, pred)
{
  # computes performance metrics for how well predictions match observations
  # obs = vector of observations
  # pred = vector of prediction
  
  N = sum(is.finite(obs) & is.finite(pred))
  rho = cor(obs, pred, use = "pairwise.complete.obs")
  mae = mean(abs(obs-pred), na.rm = TRUE)
  rmse = sqrt(mean((obs-pred)^2, na.rm = TRUE))
  return(data.frame(N = N, rho = rho, mae = mae, rmse = rmse))
}

fmingrad_Rprop <- function(fun,mu_0){
  
  # need fun to return function value and gradient value
  library('rootSolve')
  
  # optimization parameters for Rprop
  Delta_0 <- rep(0.1,length(mu_0))
  Delta_min <- 1e-6
  Delta_max <- 50
  eta_minus <- 0.5
  eta_minus <- eta_minus - 1
  eta_plus <- 1.2
  eta_plus <- eta_plus-1
  count_max <- 200
  
  mu <- mu_0
  df_out <- fun(mu_0)
  f <- df_out$f
  g <- df_out$g
  s <- sqrt(sum(g*g ))
  
  count <- 0
  Delta <- Delta_0
  del_f <- 10
  
  while( (s > 0.0001) & (count < 100) & (del_f > 0.0000001) ){
    
    # step 1: move
    mu_1 <- mu - sign(df_out$g)*Delta
    
    df_out <- fun(mu_1)
    f_1 <- df_out$f
    g_1 <- df_out$g
    
    s <- sqrt(sum (g_1*g_1))
    del_f <- abs(f_1/f - 1)
    
    # step 2: update step size
    gc <- g * g_1
    Delta <- min(Delta_max,
                 max(Delta_min,
                     Delta * (1 + eta_plus*(gc>0) + eta_minus*(gc<0))
                 )
    )
    
    # step 3: reset
    mu <- mu_1
    g <- g_1
    f <- f_1
    count <- count+1
    
  }
  
  
  return(mu)  
}

GP_predictor <- function(block,lib,pred=NULL,targ_col=1,use_col=(2:NCOL(block)),
                         pars = c(phi=0,ve=0,tau=0),do_gradient=FALSE,gp_model_out=FALSE){
  
  #basic model is x(t) = f[x(t-1),..., x(t-E)] + noise
  #with f~GP(0,C) and noise ~N(0,ve)
  #C is the covariance function - here restricted to C = tau*exp[-phi^2*||x-y||^2 ] 
  #Inputs: 
  #pars is a column vector or parameters: [phi; ve; tau], i.e. [length scale, process noise, pointwise variance in f] 
  #xd - the n_row x E matrix of lag coordinates
  #yd - the n_row x 1 vector of next states
  #Fig - set to 1 to produce some plots
  #lib - this is a n x 1 matrix of row indices of block which is used to construct and fit f
  #pred - this is a n x 1 matrix of row indices of block for which f should be estimated
  
  #to optimize the covariance function parameters, create an anonymous function
  #e.g. lpost=@(par) GP4Ethan_Embed_fit(par,xd,yd,0,[]);
  #then pass this off to a function minimizer. I have had most success with
  #the Rprop algorithm implemented in fmingrad_Rprop
  #the function call would look like
  #[pf2,nlogl1]=fmingrad_Rprop(lpost,init_par);
  #where init_par is an initial guess at the parameters - using init_par=[0 0 0]' usually works ok.
  #the estimated parameters are in pf2.
  
  #### remove rows with NAs from lib and pred
  lib <- lib[!apply(block[lib,c(use_col,targ_col)],MARGIN = 1,function(x) any(is.na(x)))]
  pred <- pred[!apply(block[pred,c(use_col,targ_col)],MARGIN = 1,function(x) any(is.na(x)))]
  
  #### name parameters
  if(length(pars) != 3){
    print("ERROR IN PARAMETER SPECIFICATION")
    break
  }else  if(is.null(names(pars))){
    names(pars) <- c("phi","ve","tau")
  }
  
  xd <- (block[lib,use_col,drop = FALSE])
  yd <- (block[lib,targ_col,drop = FALSE])
  
  n_row=dim(xd)[1]
  Y=yd
  Vy=as.numeric(var(Y))
  
  out <- vector(mode='list')
  
  #################################################################################55
  #transform parameters from real line to constrained space - do not change
  vemin=0.001
  vemax=.999
  taumin=.1
  taumax=.999
  gammin=.0001
  gammax=.999
  
  phi=exp(pars['phi'])/max(as.vector(xd))
  ve=((vemax-vemin)/(1+exp(-pars['ve']))+vemin)
  tau=((taumax-taumin)/(1+exp(-pars['tau']))+taumin)
  
  dpars <- c(phi=phi,
          ve=ve*(vemax-vemin-ve)/(vemax-vemin),
          tau=tau*(taumax-taumin-tau)/(taumax-taumin))
  #########################################################################55
  
  #########################################################################55
  #specify priors
  lam_phi=pi/2;#variance for gaussian - pi/2 means E(phi)=1
  lp_phi=-.5*sum(phi^2)/lam_phi
  dlp_phi=-sum(phi^1)/lam_phi
  a_tau=2
  b_tau=2 #beta
  lp_tau=(a_tau-1)*log(tau)+(b_tau-1)*log(1-tau)
  dlp_tau=(a_tau-1)/tau-(b_tau-1)/(1-tau);
  a_ve=2;b_ve=2;#beta
  lp_ve=(a_ve-1)*log(ve)+(b_ve-1)*log(1-ve);    dlp_ve=(a_ve-1)/ve-(b_ve-1)/(1-ve);
  lp=(lp_phi+lp_ve+lp_tau);
  dlp=c(dlp_phi,dlp_ve,dlp_tau)
  #########################################################################55
  
  #correlation function ###########################################
  D <- matrix(0,n_row,n_row)
  
  for (i_var in 1:dim(xd)[2]){
    D <- D + (matrix(xd[,i_var],n_row,n_row,byrow=FALSE) - matrix(xd[,i_var],n_row,n_row,byrow=TRUE) )^2
  }
  
  Rd <- exp(-phi^2*D)
  dRdp <- -2*phi*D %*% exp(-phi^2*D)
  ##########################################################
  
  #rescale by variance in y - so tau and ve are really fractions of total variance
  tau <- tau*Vy
  ve <- ve*Vy 
  
  Cd <- tau*Rd
  Md <- rep(0,n_row,1)
  mpt <- rep(0,n_row,1)
  
  Cdt <- Cd
  like <- 0
  dl <- 0*dlp
  
  Id <- diag(n_row)
  Sigma <- Cd+ve*Id
  
  dd <- det(Sigma)
  
  #chol algorithm from R&W
  L <- Matrix::chol(Sigma,pivot=TRUE)
  Linv <- chol2inv(L)
  a <- Linv %*% (chol2inv(t(L)) %*% (Y-Md))
  iKVs <- Linv %*% t(Linv)
  mpt <- Md + Cd %*% a
  Cdt <- Cd - Cd %*% iKVs %*% Cd
  like <- (-.5*t(Y-Md) %*% a) - sum(log(diag(L)))


  if(gp_model_out){
    out$gp_inputs <- c(points=xd,
                       a=a,
                       iKVs=iKVs,
                       pars=pars)
  }
  
if(do_gradient){ #calculate gradient
    vQ <- matrix( as.vector( a%*%t(a) - iKVs ), ncol = 1)
    W <- tau*dRdp
    
    dl <- c( phi=.5*sum(vQ * matrix(W,ncol=1)),
             ve=.5*sum(vQ * matrix(Id,ncol=1)),
             tau=.5*sum(vQ * matrix(Cd/tau,ncol=1)) )

    #J is gradient in parameter space - need gradient in transformed parameters
    J=dl+dlp;
    GradLpost=t(J)* dpars;
    out$neglgrad=-GradLpost;
}
  
if (!is.null(pred)){
    #produce mean and variance on grid specified by predgrid
    #uses loop over grid
  
  pred_block <- block[pred,use_col,drop=FALSE] 
  n_pred <- length(pred)
  
  ng <- dim(pred_block)[1]
    dd <- dim(pred_block)[2]
    xs=pred_block
    Ds=matrix(0,dim(xs)[1],n_row)
    for(i in 1:dim(xd)[2]){
      Ds=Ds+(xs[,i] %*% matrix(1,1,n_row)-matrix(1,dim(xs)[1],1) %*% t(xd[,i]))^2
    }
    Cs=tau*exp(-phi^2*Ds);
    mps=Cs %*% a;
    
    Cst <- matrix(0,ng,1)
    for(i in 1:ng){
      Cst[i,]=tau - t(Cs[i,]) %*% iKVs %*% Cs[i,];
    }

    out$predictions <- data.frame(index=pred,t_obsv=block[pred,targ_col],t_pred=numeric(n_pred),var=numeric(n_pred))
    out$predictions[,'t_pred'] <- mps
    out$predictions[,'var'] <- Cst+ve
    
    out$stats <- compute_stats(obs=out$prediction[,'t_obsv'],pred=out$predictions[,'t_pred'])
  }
  
lpost=like+lp
out$neglpost=-lpost

lnL_LOO=.5*sum(log(diag(iKVs)))-.5*sum( a^2 / diag(iKVs) );

out$mean=mpt;
out$cov=Cdt;
out$LOO=lnL_LOO;

return(out)
}

GP_EDM_fit <- function(block,lib,pred=NULL,targ_col=1,use_col=(2:NCOL(block)),pars0=c(phi=0,ve=0,theta=0)){
  
  out <- vector(mode='list')
  
  #### FIT IN-SAMPLE
  fg <- function(pars) {
    out_temp <- GP_predictor(block,lib=lib,pred=NULL,targ_col,use_col,pars,do_gradient=TRUE)
    return(list(f=out_temp$neglpost,g=out_temp$neglgrad))
    }

  pars_star <- fmingrad_Rprop(fg,pars0)
  
  out$pars <- pars_star
  
  #### FORECAST OUT-OF-SAMPLE
  
  if(!is.null(pred)){
   
    out_temp <- GP_predictor(block,lib,pred,targ_col,use_col,pars_star,do_gradient=FALSE)
    out <- c(out,out_temp)
    
  }
  
  return(c(out,out_temp))
  
}

GP_forecast_func <- function(gp_inputs,x0,add_error=FALSE){
  
  if(!(names(gp_inputs)==c(points,a,iKVs,pars))){
    print('ERROR: gp_inputs incorrectly specified')
    break
  }
  
  x_gp <- gp_inputs$points
  
  n_coord <- dim(gp_inputs$points)[2]
  
  Ds=matrix(0,1,NROW(gp_inputs$points))
  for(i in 1:n_coord){
    Ds=Ds+(x0[i] %*% matrix(1,1,NROW(x_gp))-t(x_gp[,i]))^2
  }
  Cs=tau*exp(-phi^2*Ds);
  
  y_pred=Cs %*% gp_inputs$a
  y_var = tau - t(Cs) %*% gp_inputs$iKVs %*% Cs + gp_inputs$par$ve
  
  if(add_error){
    y_pred = y_pred + rnorm(1,0,sqrt(y_var))
  }
  
  return(y_pred)
}

GP_EDM_sim <- function(block,lib,x0,N=1,pars0=c(phi=0,ve=0,theta=0)){
  
  n_var <- NCOL(block)
  big_block <- makeblock(block,cols=rep(1:n_var,2),c( rep(-1,n_var),rep(0,n_var) ) )
  
  
  #### CREATE NON-PARAMETRIC FUNCTION to map _x_(t) -> _x_(t+1)
  f <- vector(mode='list',n_var)
  
  for (i_var in 1:n_var){
    #### FIT GP
    pars_temp <- GP_EDM_fit(big_block,lib,pred=NULL,targ_col=i_var,use_col=(1:n_var)+n_var,pars0=c(phi=0,ve=0,theta=0))$pars
    out_temp <- GP_predictor(big_block,lib,pred=NULL,targ_col=i_var,use_col=(1:n_var)+n_var,
                 pars = pars_temp,do_gradient=FALSE,gp_model_out=TRUE)
    
    #### CREATE FORECAST FUNCTION
    # make sure to force inputs to resolve
    f[[i_var]] <- function(x) GP_forecast_func(out_temp$gp_inputs,x,add_error=TRUE)
  }
  
  
  #### ITERATE THROUGH TIME
  
  for (i_t in 1:N){
    
    xt <- numeric(n_var)
    
    for (i_var in 1:n_var){
      xt[i_var] <- f[[i_var]](x0)
    }
    
    block_sim[i_t,] <- xt
    x0 <- xt
    
  }
  
  return(block_sim)
}