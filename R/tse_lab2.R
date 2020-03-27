
# PREPARE ENVIRONMENT -----------------------------------------------------

    # load the libraries necessary
    library(tidyverse)
    library(ggplot2)
    library(here)


# 1. AR(4) SERIES ---------------------------------------------------------

    # Solve the characteristic equation and calculate the characteristic roots.
    z <- polyroot(c(1,-0.5,0.8,0.1,-0.2))
    round(z, digits=4)
    omega <- 1/z
    round(omega , digits=4)
    round(Mod(omega), digits=4)
    dfomega <- data.frame(Re(omega), Im(omega))
    colnames(dfomega)=c('Re','Im')
    
    # Function for generating a circle.
    circleFunc <- function(centre=c(0, 0), radius=1, npoints=100)
    {   theta <- seq(0, 2*pi, length.out=npoints)
    xvals <- centre[1] + radius*cos(theta)
    yvals <- centre[2] + radius*sin(theta)
    return(data.frame(x=xvals , y=yvals))
    }
    
    # Construct the unit circle.
    dfcirc <- data.frame(circleFunc(c(0 ,0), radius=1, npoints=100))
    colnames(dfcirc)=c('Re','Im')
    
    # Plot the unit circle and the solutions to the characteristic equation.
    ggplot(dfcirc , aes(x=Re, y=Im)) + xlim(-1, 1) + ylim(-1, 1) +
        labs(x='Re',y='Im') +
        ggtitle('Charateristic Roots for AR(4) Model') +
        geom_path(color='blue') + coord_fixed() +
        geom_point(data=dfomega, aes(x=Re, y=Im), colour='red') +
        geom_hline(yintercept=dfomega$Im, linetype='dashed', color='red') +
        geom_vline(xintercept=dfomega$Re, linetype='dashed', color='red') +
        theme(plot.title=element_text(hjust=0.5, size=15, face='bold'))


# 2. AR(3) SERIES ---------------------------------------------------------



# 3. SIMULATE SERIES FOR MODEL, ESTIMATE AUTOCORRELATIONS -----------------

    

# 4. CONSIDER ACF AND PACF ------------------------------------------------

    

    