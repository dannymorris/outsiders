biweight<-function(a,const1=9,const2=36,err=0.0001) {
    #
    #a is data matrix with two cols.
    #const1=common tuning constant
    #const2=bivariate tuning constant
    #err=convergence criterion.
    x<-a[,1]; y<-a[,2]; n<-length(x); mx<-median(x); my<-median(y); madx<-median(abs(x-mx)); mady<-median(abs(y-my))
    if(madx != 0) { ux<-(x-mx)/(const1*madx)
    ux1<-ux[abs(ux)<1]; tx<-mx+(sum((x[abs(ux)<1]-mx)*(1-ux1*ux1)^2)/sum((1-ux1^2)^2))
    sx<- sqrt(n)*sqrt(sum((x[abs(ux)<1]-mx)^2*(1-ux1*ux1)^4))/abs(sum((1-ux1*ux1)*(1-5*ux1*ux1)))
    }
    else { tx<-mx
    sx<-sum(abs(x-mx))/n
    }
    if(mady != 0) { uy<-(y-my)/(const1*mady)
    uy1<-uy[abs(uy)<1]; ty<-my+(sum((y[abs(uy)<1]-my)*(1-uy1*uy1)^2)/sum((1-uy1^2)^2))
    sy<- sqrt(n)*sqrt(sum((y[abs(uy)<1]-my)^2*(1-uy1*uy1)^4))/abs(sum((1-uy1*uy1)*(1-5*uy1*uy1)))
    }
    else { ty<-my
    sy<-sum(abs(y-my))/n
    }
    z1<-(y-ty)/sy+(x-tx)/sx; z2<-(y-ty)/sy-(x-tx)/sx; mz1<-median(z1); mz2<-median(z2);
    madz1<-median(abs(z1-mz1)); madz2<-median(abs(z2-mz2))
    if(madz1 != 0) { uz1<-(z1-mz1)/(const1*madz1)
    uz11<-uz1[abs(uz1)<1]; tz1<-mz1+(sum((z1[abs(uz1)<1]-mz1)*(1-uz11*uz11)^2)/sum((1-uz11^2)^2))
    sz1<- sqrt(n)*sqrt(sum((z1[abs(uz1)<1]-mz1)^2* (1-uz11*uz11)^4))/abs(sum((1-uz11*uz11)*(1-5*uz11*uz11)))
    }
    else { tz1<-mz1
    sz1<-sum(abs(z1-mz1))/n
    }
    if(mady != 0) { uz2<-(z2-mz2)/(const1*madz2)
    uz21<-uz2[abs(uz2)<1]; tz2<-mz2+(sum((z2[abs(uz2)<1]-mz2)*(1-uz21*uz21)^2)/sum((1-uz21^2)^2))
    sz2<- sqrt(n)*sqrt(sum((z2[abs(uz2)<1]-mz2)^2*(1-uz21*uz21)^4))/abs(sum((1-uz21*uz21)*(1-5*uz21*uz21)))
    }
    else { tz2<-mz2
    sz2<-sum(abs(z2-mz2))/n
    }
    esq<-((z1-tz1)/sz1)^2+((z2-tz2)/sz2)^2; w<-numeric(length=n); c2<-const2
    for(i in 1:10) {
        w[esq<const2]<-(1-esq[esq<const2]/const2)^2; w[esq>=const2]<-0; l<-length(w[w==0])
        if(l<0.5*n) break
        else const2<-2*const2
    }
    tx<-sum(w*x)/sum(w); sx<-sqrt(sum(w*(x-tx)^2)/sum(w)); ty<-sum(w*y)/sum(w);
    sy<-sqrt(sum(w*(y-ty)^2)/sum(w)); r<-sum(w*(x-tx)*(y-ty))/(sx*sy*sum(w)); const2<-c2; wold<-w
    for(i in 1:100) {
        z1<-((y-ty)/sy+(x-tx)/sx)/sqrt(2*(1+r)); z2<-((y-ty)/sy-(x-tx)/sx)/sqrt(2*(1+r)); esq<-z1*z1+z2*z2
        for(j in 1:10) {
            w[esq<const2]<-(1-esq[esq<const2]/const2)^2; w[esq>=const2]<-0; l<-length(w[w==0])
            if(l<0.5*n) break
            else const2<-2*const2
        }
        tx<-sum(w*x)/sum(w); sx<-sqrt(sum(w*(x-tx)^2)/sum(w)); ty<-sum(w*y)/sum(w); sy<-sqrt(sum(w*(y-ty)^2)/sum(w))
        r<-sum(w*(x-tx)*(y-ty))/(sx*sy*sum(w)); term<-sum((w-wold)^2)/(sum(w)/n)^2
        if(term<-err) break
        else {wold<-w
        const2<-c2
        }
    }
    param<-c(tx,ty,sx,sy,r)
    param
}
#####
######################


# Copy Everitt's bivbox function:
######################
#####
bivbox<-function(a, d = 7, alpha = 1)
{
    #a is data matrix
    #d is constant(usually 7)
    p <- length(a[1,  ])
    param <- biweight(a[, 1:2]); m1 <- param[1]; m2 <- param[2]
    s1 <- param[3]; s2 <- param[4]; r <- param[5]
    x <- (a[, 1] - m1)/s1; y <- (a[, 2] - m2)/s2
    e <- sqrt((x * x + y * y - 2 * r * x * y)/(1 - r * r))
    e2 <- e * e; em <- median(e); emax <- max(e[e2 < d * em * em])
    r1 <- em * sqrt((1 + r)/2); r2 <- em * sqrt((1 - r)/2); theta <- ((2 * pi)/360) * seq(0, 360, 3)
    xp <- m1 + (r1 * cos(theta) + r2 * sin(theta)) * s1; yp <- m2 + (r1 * cos(theta) - r2 * sin(theta)) * s2
    r1 <- emax * sqrt((1 + r)/2); r2 <- emax * sqrt((1 - r)/2); theta <- ((2 * pi)/360) * seq(0, 360, 3)
    xpp <- m1 + (r1 * cos(theta) + r2 * sin(theta)) * s1; ypp <- m2 + (r1 * cos(theta) - r2 * sin(theta)) * s2
    maxxl <- max(xpp); minxl <- min(xpp); maxyl <- max(ypp); minyl <- min(ypp)
    b1 <- (r * s2)/s1; a1 <- m2 - b1 * m1; y1 <- a1 + b1 * minxl; y2 <- a1 + b1 * maxxl
    b2 <- (r * s1)/s2; a2 <- m1 - b2 * m2; x1 <- a2 + b2 * minyl; x2 <- a2 + b2 * maxyl
    maxx <- max(c(a[, 1], xp, xpp, x1, x2)); minx <- min(c(a[, 1], xp, xpp, x1, x2))
    maxy <- max(c(a[, 2], yp, ypp, y1, y2)); miny <- min(c(a[, 2], yp, ypp, y1, y2))
    library(ggplot2)
    ggplot(as.data.frame(a), aes(x = a[,1], y = a[,2])) +
        geom_point(alpha = alpha) +
        geom_path(aes(x=xp, y=yp, group=1), data = data.frame(xp, yp), lty=2) +
        geom_path(aes(x=xpp, y=ypp, group=1), data = data.frame(xpp, ypp), lty=2) +
        geom_segment(aes(x=minxl, y=y1, xend = maxxl, yend=y2),
                     data = data.frame(minxl, y1, maxxl, y2), lty = 2) +
        geom_segment(aes(x=x1, y=minyl, xend = x2, yend=maxyl),
                     data = data.frame(x1, minyl, x2, maxyl), lty = 2)
    #   geom_line()
    # plot(a[, 1], a[, 2], xlim = c(minx, maxx), ylim = c(miny, maxy), xlab =xlab, ylab =ylab,
    #      lwd = 1, pch = 1, ...)
    # lines(xp, yp, lty = 2, lwd = 1); lines(xpp, ypp, lty = 2, lwd = 1)
    # segments(minxl, y1, maxxl, y2, lty = 3, lwd = 1); segments(x1, minyl, x2, maxyl, lty = 3, lwd = 1)
}
#####
######################

bivden<-function(x, y, ngridx = 30, ngridy = 30, constant.x = 1, constant.y = 1) {
    #x and y are vectors containing the bivariate data
    #ngridx and ngridy are the number of points in the grid
    mx <- mean(x); sdx <- sqrt(var(x)); my <- mean(y); sdy <- sqrt(var(y))
    #scale x and y before estimation
    x <- scale(x); y <- scale(y); den <- matrix(0, ngridx, ngridy)
    #find possible value for bandwidth
    n <- length(x); hx <- constant.x * n^(-0.2); hy <- constant.y * n^(-0.2)
    h <- hx * hy; hsqrt <- sqrt(h)
    seqx <- seq(range(x)[1], range(x)[2], length = ngridx); seqy <- seq(range(y)[1], range(y)[2], length = ngridy)
    for(i in 1:n) {
        X <- x[i]; Y <- y[i]; xx <- (seqx - X)/hsqrt; yy <- (seqy - Y)/hsqrt
        den <- den + outer(xx, yy, function(x, y)
            exp(-0.5 * (x^2 + y^2)))
    }
    den <- den/(n * 2 * pi * h); seqx <- sdx * seqx + mx; seqy <- sdy * seqy + my
    result <- list(seqx = seqx, seqy = seqy, den = den)
    result
}


#' Chi-Squared MVN Plot
#' 
#' @param data a matrix or data.frame.
#' @return 
#' @examples
#' @export
#' 
chisq_plot <- function(data, plot_line=FALSE, line_col='black', ...) {
    
    if (!is.matrix(data) & !is.data.frame(data)) 
        stop("Data must be a matrix or data frame")
    
    n <- nrow(data)
    p <- ncol(data)
    xbar <- apply(data, 2, mean)
    
    S <- var(data)
    S <- solve(S)
    
    index <- (1:n)/(n+1)
    xcent <- t(t(data) - xbar)
    
    di <- apply(xcent, 1, function(x,S) x %*% S %*% x,S)
    quant <- qchisq(index,p)
    
    plot(quant, sort(di), ylab = "Ordered distances",
         xlab = "Chi-square quantile", ...)
    
    if (plot_line == TRUE) {
        df <- data.frame(quant=quant, sort_di = sort(di))
        abline(lm(sort_di ~ quant, data=df), col=line_col, lty=2)        
    }
}
