# Calculating the circleness of a stone with R
# www.overfitting.net
# https://www.overfitting.net/2022/09/calculando-la-redondez-de-una-piedra.html

library(tiff)


NAME="circle"  # input RAW filenames
OUTNAME="circlenorm"  # output normalized stone
NSTONES=5


# READ RAW DATA AND NORMALIZE STONES TO BLACK/WHITE

# dcraw -v -r 1 1 1 1 -o 0 -4 -T *.arw (except stone 4)
for (i in 1:NSTONES) {
    print(paste0("Normalizing stone ", i, "..."))
    img=readTIFF(paste0(NAME, i, ".tiff"), native=FALSE, convert=FALSE)
    img=0.299*img[,,1]+0.587*img[,,2]+0.114*img[,,3]  # average channels
    img=img-min(img)  # normalize to 0..1
    img=img/max(img)
    img=img^(1/2.2)  # 2.2 gamma curve
    # hist(img, breaks=512)
    
    TH=0.35  # 0.2  # black/white thresholds
    img[img<TH]=0
    img[img>=TH]=1
    
    writeTIFF(img, paste0(OUTNAME, i, ".tif"), bits.per.sample=16,
              compression="none")    
}
# After cleaning in Photoshop... -> save as "_.tif"


# CALCULATE RADIUS PROFILE, CIRCLENESS AND EMPHASIZE RADIUS DIFFERENCES

circle=c()
for (i in 1:NSTONES) {
    print(paste0("Calculating circleness of stone ", i, "..."))
    img=readTIFF(paste0(OUTNAME, i, "_.tif"), native=FALSE, convert=FALSE)
    # hist(img, breaks=512)
    
    # Calculate centre of mass
    tmp=row(img)*img
    x0=mean(tmp[tmp>0])  # rows
    tmp=col(img)*img
    y0=mean(tmp[tmp>0])  # cols
    
    N=6000  # a good number of radial samples
    R=c()
    for (j in 0:(N-1)) {
        theta=j*2*pi/N
        r=500  # all radius are >500
        costheta=cos(theta)
        sintheta=sin(theta)
        x1=x0+r*costheta
        y1=y0+r*sintheta
        
        while (img[round(x1), round(y1)]==1) {
            r=r+0.5
            x1=x0+r*costheta
            y1=y0+r*sintheta          
        }
        R=c(R,r)
    }
    
    Rmean=mean(R)  # mean radius in pixels
    Rmin=min(R)  # min radius in pixels
    Rmax=max(R)  # max radius in pixels

    R=R/Rmean  # normalize R
    
    # Plot R/mean(R) for all the stones
    if (i==1) {
        plot(seq(from=0, to=360, length.out=N), R, type='l', col=i,
             ylim=c(0.85, 1.15), ylab='R/mean(R)', xlab='Angle')
        abline(h=1, col='lightgray')
    } else {
        lines(seq(from=0, to=360, length.out=N), R, type='l', col=i)  
    }
    
    VarR=mean((R-mean(R))^2)  # total variance of R
    k=1000  # circleness scaling factor
    circleness=exp(-k*VarR)  # scale circleness to 1(=perfect circle)..0
    circle=c(circle, circleness)
    print(paste0("  circleness=", circleness))    
    
    
    # Read bitmaps and implode stones from max radius
    print(paste0("Morphing stone ", i, "..."))
    img=readTIFF(paste0(NAME, i, ".tif"), native=FALSE, convert=FALSE)
    img2=img*0+1
    
    COMPRESS=4  # compression factor
    # Simple nearest neighbour interpolation
    for (x in round(x0-Rmax):round(x0+Rmax)) {
        for (y in round(y0-Rmax):round(y0+Rmax)) {
            r=((x-x0)^2+(y-y0)^2)^0.5
            rp=(r/Rmax)^(1/COMPRESS)*Rmax
            f=rp/r
            img2[x, y,]=img[round(x0+(x-x0)*f), round(y0+(y-y0)*f),]
        }
    }

    # Draw centre os mass axes and max radius circle
    # and save BEFORE and AFTER images
    img[round(x0),,]=0
    img[,round(y0),]=0
    for (j in 0:(N-1)) {
         theta=j*2*pi/N
         x1=x0+c(Rmax, Rmax, Rmax)*cos(theta)  #c(Rmean, Rmin, Rmax)*cos(theta)
         y1=y0+c(Rmax, Rmax, Rmax)*sin(theta)  #c(Rmean, Rmin, Rmax)*sin(theta)
         for (k in 1:3) img[round(x1[k]), round(y1[k]),]=0
    }
    img=img[round(x0-Rmax):round(x0+Rmax), round(y0-Rmax):round(y0+Rmax),]
    writeTIFF(img, paste0(OUTNAME, i, "_BEFORE.tif"), bits.per.sample=16,
              compression="none")
    
    img2[round(x0),,]=0
    img2[,round(y0),]=0
    for (j in 0:(N-1)) {
        theta=j*2*pi/N
        x1=x0+Rmax*cos(theta)
        y1=y0+Rmax*sin(theta)
        img2[round(x1), round(y1),]=0
    }
    img2=img2[round(x0-Rmax):round(x0+Rmax), round(y0-Rmax):round(y0+Rmax),]
    writeTIFF(img2, paste0(OUTNAME, i, "_AFTER.tif"), bits.per.sample=16,
              compression="none")
}  
