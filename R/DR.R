#### GPS/mutli-scale/stratified space analysis tool
#### Author: Yen-Chi Chen <yenchic@uw.edu>
#### Reference: 
####    1. Chen, Yen-Chi, Adrian Dobra. "Measuring Human Activity Spaces With 
####       Density Ranking Based on GPS Data"
####    2. Chen, Yen-Chi. "Generalized Cluster Trees and Singular Measures." 
####       arXiv preprint arXiv:1611.02762 (2016).
#### Date: 08/09/2017
library(TDA)
library(RANN)

### --- Input ---
## data: input data
## h: smoothing bandwidth
## kernel: kernel function, choose from 'Gaussian', 'uniform', 'quartic'.
## xlim, ylim: range of the region of interst
## CL_lev: level of contour curves, values within [0,1]
## n_res: resoluation of the grids
## n_tg: grid resolution of topological/geometric curve 
## ... : other arguments for the 'gridDiag' function in the TDA package

### --- Output ---
## h: smoothing bandwidth
## x_grid, y_grid: grid points on the x-axis, y-axis; to get the grid points,
##                 use 'expand.grid(x_grid,y_grid)'
## gr_alpha: alpha value on the (x,y)-grid points
## data_alpha: alpha value on the data points
## CL_lev: level of contour curves
## CL: contour curves corresponding to each level in 'CL_lev'
## persistent: the topological features in the persistent diagram
## clevel: alpha levels that we evaluate the 'Mcurve', 'Bcurve', and 'Pcurve'.
## Mcurve: mass-volume curve.
## Bcurve: Betti number curve.
## Pcurve: persistence curve, a step function object.

### --- Function ---
DR = function(data, h, kernel="Gaussian",
              xlim=NULL, ylim=NULL, CL_lev = NULL, n_res=201, 
               n_tg=100, ...){
  if(is.null(h)){
    cat("Please select smoothing bandwidth.")
    break
  }
  if(ncol(data)>2){
    cat("The current version only support 2D.")
    break
  }
  if(is.null(xlim)){
    xlim = range(data[,1])
  }
  if(is.null(ylim)){
    ylim = range(data[,2])
  }
  if(is.null(CL_lev)){
    CL_lev = c(1:19)/20
  }
  n = nrow(data)
  
  ### derived parameters
  x_seq = seq(from=xlim[1], to=xlim[2], length.out=n_res)
  y_seq = seq(from=ylim[1], to=ylim[2], length.out=n_res)
  gr0 = expand.grid(x_seq,y_seq)
  al_seq = ((n_tg-1):0)/n_tg
  
  ### alpha value for each point
  if(kernel=="Gaussian"){
    D_kde = kde(data,Grid = data,h = h)
    gr0_kde = kde(X = data,Grid = gr0,h = h)
  }
  if(kernel=="uniform"){
    D_nn = nn2(data,data,k = n,searchtype = "radius",radius = h)
    D_kde = rowSums(D_nn$nn.idx!=0)/n
    gr0_nn = nn2(data,query = gr0,k = n,searchtype = "radius",radius = h)
    gr0_kde = rowSums(gr0_nn$nn.idx!=0)/n
  }
  if(kernel=="quartic"){
    D_nn = nn2(data,data,k = n,searchtype = "radius",radius = h)
    D_val = (1-D_nn$nn.dists^2/h^2)^2*(D_nn$nn.idx!=0)
    D_kde = rowSums(D_val,na.rm = T)
    gr0_nn = nn2(data,query = gr0,k = n,searchtype = "radius",radius = h)
    gr0_val = (1-gr0_nn$nn.dists^2/h^2)^2*(gr0_nn$nn.idx!=0)
    gr0_kde = rowSums(gr0_val,na.rm = T)
  }
  D_alpha = rank(D_kde)/n
  gr0_alpha = ecdf(D_kde)(gr0_kde)
  
  ### alpha contours
  CL = list()
  for(i_lv in 1:length(CL_lev)){
    CL[[i_lv]] = contourLines(x_seq,y_seq,matrix(gr0_alpha, nrow=n_res),
                   levels=c(CL_lev[i_lv]))
  }

  ### Persistent Diagram
  D_top = gridDiag(FUNvalue=matrix(gr0_alpha,n_res), 
                    maxdimension = ncol(data)-1,
                    sublevel = F)
  D_cc = D_top$diagram[which(D_top$diagram[,1]==0),2:3]
  
  ### persistence curve
  w0 = which(D_top$diagram[,1]==0)
  P0 = D_top$diagram[w0,3]-D_top$diagram[w0,2]
  P0_step = stepfun(x=sort(P0), y=(length(w0)):0, right=T)
  
  
  ### mass-volume and Betti number curves
  D_top_curve = rep(NA, n_tg)
  D_area_curve = rep(NA, n_tg)
  for(i in 1:n_tg){
    al = al_seq[i]
    if(sum(gr0_alpha>=al)==1){
      D_area_curve[i] = 1/nrow(gr0) 
    }
    if(sum(gr0_alpha>=al)>1){
      D_area_curve[i] = sum(gr0_alpha>=al)/nrow(gr0) 
    }
    if(length(D_cc)==2){
      D_top_curve[i] = sum(D_cc[2]>al) - sum(D_cc[1]>al)
    }else{
      D_top_curve[i] = sum(D_cc[,2]>al) - sum(D_cc[,1]>al)
    }
  }
  
  ### Output
  out_put = list()
  out_put$h = h
  out_put$x_grid = x_seq
  out_put$y_grid = y_seq
  out_put$gr_alpha = gr0_alpha
  out_put$data_alpha = D_alpha
  out_put$CL_lev = CL_lev
  out_put$CL = CL
  out_put$persistent = D_top$diagram
  out_put$clevel = al_seq
  out_put$Mcurve = D_area_curve
  out_put$Bcurve = D_top_curve
  out_put$Pcurve = P0_step
  return(out_put)
}
