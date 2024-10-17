# Spatially explitic model for regular pattern formation in mussel beds
# The model follows the paper :
#
# Van de Koppel, J., M. Rietkerk, N. Dankers and P. M. J. Herman (2005). 
# Scale-dependent feedback and regular spatial patterns in young mussel 
# beds. American Naturalist 165(3): E66-E77.

remove(list=ls())     # Remove all variables from memory
require("fields")     # Loading a package for the visualization of the model

# --- Parameters values of the model ------------------------------------------
 
# Algal Exchange parameters
Aup     =  1.1        # g/m3  Algal concentration in upper layer  Oosterschelde data
h       =  0.10       # m     Height of the lower layer  defined
f       =  100        # m3/m3/h  Phichange rate with upper layer  Guestimated

# Mussel update, growth & mortality parameters
c       =  0.1        # g/g/h Maximal consumption rate of the mussels  Riisgard 2001 
e	      =  0.2        # g/g   Trophic efficiency of mussels  Lamie
d_M     =  0.02       # g/g/h Density dependent mortality rate of the mussels  Calibrated
k_M     =  150        # g/m2  Effect of density on mortality  Guestimated

# Spatial movement parameters
D       =  0.0005     # m2/h  The diffusion constant describing the movement of mussels
V       =  0.1*60*60  # m/h   Tidal advection constant(0.1 m/s * 60 sec * 60 min)

# The speeding constant Phi 
Phi     =  1000       # Speeding constant, accelerates mussel growth

# --- Simulation parameters ---------------------------------------------------

Length  =  25         # m    The length of the simulated landscape, in meters
m       =  50         # #    gridcells

dx      =  Length/m   # The size of a grid cell in X direction
dy      =  Length/m   # The size of a grid cell in Y direction

dT      =  0.0002     # timestep (per hour)
EndTime =  360*24/Phi # end time (days x hours)
NoFrames=  360        # Number of frames displayed during the entire simulation

frac    =  0.05       # Initial settings: fraction of area filled with mussels

# --- Graphic settings --------------------------------------------------------

WinWidth=12           #      - Width of the simulation window 
WinHeight=5           #      - Height of the simulation window

# Color pallette for the algae and the mussels 
algae.palette = colorRampPalette(c("lightblue", "green"))
mussel.palette= colorRampPalette(c('#b2aa9e','#f4e8ed','#a3a3b5','#707a8e','#334447','#000000'))

# --- A "Open window" function that works both on Mac and on Windows ----------
OpenWindow <- function(width, height,...)
{
  if (Sys.info()["sysname"]=="Darwin"){
    quartz(width=width, height=height,...)
  } else {
    win.graph(width=width, height=height)
  }
}

# --- Initialization of matrices containing the state variables ---------------

A = M = dA = dM = matrix(nrow=m,ncol=m)    # The state and rate variables

# --- advective (Gradient) and diffusive (Laplacian) operators ----------------

# The advective (gradient) operator
d_dx = function (w) { # fluxes in x-direction (from left->right), backwards difference scheme
  # Flux = (middle cell - left side cell) / cell size in x dimension
  fx = (w[c(1:m),] - w[c(m,1:(m-1)),]) / dx
  return(fx)
}

# The diffusive (laplacian) operator
d2_dxy2 = function (w) { # Diffusion terms in x and y dimensions
  # Flux = Right + Left -2*middle cells +
  fxy = (w[1:m,c(2:m,1)] + w[1:m,c(m,1:m-1)] - 2*w[1:m,1:m])/dy/dy +
  #        Above + Below - 2*middle cells        
        (w[c(2:m,1),1:m] + w[c(m,1:m-1),1:m] - 2*w[1:m,1:m])/dx/dx;
  return(fxy)
}

#------ Initial setup and calculation -----------------------------------------

set.seed(20)  # Making sure the random number generator gives the same values

# Initial values for the algae and the musels
A[,]=0.5 
M=100+(matrix(ncol=m,nrow=m,data=runif(m*m))<=frac)*10 

# Some counters used in the loop below
Time =  0          # Begin time 
ii   =  1e6        # Setting the plot counter to max, so that drawing start immediately

# --- Setting up the figure ---------------------------------------------------

## Open a graphics window (Darwin stands for a Mac computer)
OpenWindow(width=WinWidth, height=WinHeight)
par(mfrow=c(1,2), mar=c(3, 4, 2, 6) + 0.1) # sets up the margins

# --- The simulation loop -----------------------------------------------------

while (Time<=EndTime){   # Here the time loop starts   
   
  # Calculating local input, uptake, growth, mortality, and fluxes
  drA = (Aup - A)*f - c/h*A*M - V*d_dx(A)
  drM = e*c*A*M - d_M*k_M/(M + k_M)*M + D*d2_dxy2(M)       
  
  # Summing up local processes and lateral flow to calculate new A and M
  A = A + drA*dT 
  M = M + drM*Phi*dT 
  
  # Graphic representation of the model every now and then
  if (ii>=EndTime/NoFrames/dT)
      {
       fields::image.plot(A, zlim=c(0,Aup), xaxt="n", yaxt="n",
             col = algae.palette(255),asp=1, bty="n",
             legend.shrink = 0.99, legend.width = 1.8)
       title("Algal concentration")      

       fields::image.plot(M, zlim=c(0,2000), xaxt="n", yaxt="n",
             col = mussel.palette(255),asp=1, bty="n",
             legend.shrink = 0.99, legend.width = 1.8)
       title("Mussel biomass")
       
       mtext(text=paste("Time : ",sprintf("%1.0f",Time*Phi/24),
                        "of" ,sprintf("%1.0f",EndTime*Phi/24), "days"), 
                        side=1, adj=-0.7, line=1.5)
       
       # The following two lines prevent flickering of the screen
       dev.flush() # Force the model to update the graphs
       dev.hold()  # Put all updating on hold  

       ii=0    # Resetting the plot counter

      } 

  Time=Time+dT  # Incrementing time with one
  ii=ii+1       # Incrementing the plot counter with one
 
} # Here the time loop ends


