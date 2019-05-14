# Load/Install Packages ####

libs = c('xtable', 'RColorBrewer', 'boot', 'parallel','tiff','png')
void = sapply(libs,
              function(x)
                if(!require(x,
                            character.only = T,
                            warn.conflicts = F,
                            quietly = T)) 
                  install.packages(x)
)
rm(void,libs)
options(boot.ncpus = parallel::detectCores())

set.seed(1234)

# Setup ####

## Graphic params
height= 1000 # px
cex = 3
lwd = 4
mar = c(3,3.5,1.5,0.5)
mgp = c(2,.75,0)
tcl = -0.5

## Quick generation of graphs. 
### For high accuracy and resolution of graphs set to FALSE
quick = TRUE
### Nb of bootstrap replicates
nbst = ifelse(quick, 100, 1000)
### Nb of Monte Carlo iterations for Fog.13
nMC  = ifelse(quick,1000,10000)

## Nb of digits for uncertainty in parentheses notation 
## (cf. prettyUnc() numDig)
uDig = 1

## Define Colors
col2tr = function(col,alpha) 
  rgb(t(col2rgb(col)),alpha=alpha,maxColorValue=255)

### Palette
cols = c(brewer.pal(8,'Dark2'),brewer.pal(8,'Paired'))

### Transparent colors
col_tr=c()
for (i in 1:length(cols))
  col_tr[i] = col2tr(cols[i],40)
blue_tr   = col2tr("blue"  , 20)
blue_tr2  = col2tr("blue"  , 50)
orange_tr = col2tr("orange",120)

# Functions ####

prettyUnc = function(y, uy, numDig=2) {
  # Print result & uncertainty in parenthesis format 
  
  if (uy <= 0) return (y)
  
  # Get scales
  n0=1+floor(log10(abs(y)))
  n1=floor(log10(uy))
  
  # Format uncertainty
  switch(sign(n1) + 2, # Map (-1,0,1) to (1,2,3)
         {
           fmt = paste0("%",n0-n1+numDig-1,".",-n1+numDig-1,"f")
           short_uy = signif(uy/10^(n1-numDig+1),numDig)
         },
         {
           fmt = paste0("%",n0-n1+numDig-1,".",-n1+numDig-1,"f")
           short_uy = signif(uy/10^n1,numDig)
         },
         {    
           fmt = paste0("%",n0,".0f")
           short_uy = signif(uy/10^(n1-numDig+1),numDig)
         }
  )
  short_y  = sprintf(fmt, y)
  
  return( paste0(short_y,'(',short_uy,')') )
}

plotHist = function(X,cex=1,lwd=1,nclass=NULL,xlab='x',ylab='y',
                    col=1,plotGauss=FALSE,main=NULL) {
  # Plot histogram and fitted Normal
  
  h = hist(X, nclass=nclass, plot=FALSE)
  binWidth = h$breaks[2] - h$breaks[1]
  ymax = 1.1*max(h$counts)/length(X)/binWidth
  hist(X, nclass=nclass, freq = FALSE,
       xlab=xlab, ylab=ylab, main='', border=4, 
       yaxs='i', ylim = c(0,ymax))
  if (plotGauss) {
    xg  = seq(min(X),max(X), length.out = 1000)
    yg  = dnorm(xg,mean(X),sd(X))
    lines(xg,yg,col=2)
  }
  legend('topright',title=main,bty='n',legend='')
  grid();box()
}

plotDistHist = function(x,y,cex=1,lwd=1,nclass=NULL,xlab='x',ylab='y',
                        col=1,plotGauss=FALSE, outLiers=FALSE, p=0.9,
                        labels=1:length(x),select=NULL,main=NULL) {
  # Plot vertical histogram on the side of a scatterplot
  
  par(mfrow=c(1,1), cex=cex, lwd=lwd, mgp=c(2,.75,0))
  ylim=range(y)
  
  colp = col
  if(!is.null(select)) {
    y1 = y[select]
    y2 = y[!select]
    colp = rep(1,length(select))
    colp[!select] = 2
  }
  
  # Subfigure with scatterplot
  par(mar=c(3,3.5,1,0), fig=c(0,0.35,0,1))
  h = hist(y, nclass=nclass, plot=FALSE)
  binWidth = h$breaks[2] - h$breaks[1]
  n = length(h$counts)
  x.l = rep(0, n)
  x.r = x.l - h$counts 
  y.b = h$breaks[1:n]
  y.t = h$breaks[2:(n + 1)]
  plot(x.l,y.t,type='n',ylim=range(y),xlim=c(-1.1*max(h$counts),0),
       xaxt='n',xaxs='i',xlab='', ylab=ylab)
  grid();box()
  rect(xleft = x.l,  ybottom = y.b, 
       xright = x.r, ytop = y.t, border = col)
  if (plotGauss) {
    xg  = seq(min(y),max(y), length.out = 1000)
    yg  = dnorm(xg,mean(y),sd(y)) * length(y)
    lines(-yg,xg,col=2)
  }
  abline(h=0,lty=3)
  
  if(!is.null(select)) {
    y1 = y[select]
    h = hist(y1, breaks = h$breaks, plot=FALSE)
    n = length(h$counts)
    x.l = rep(0, n)
    x.r = x.l - h$counts 
    y.b = h$breaks[1:n]
    y.t = h$breaks[2:(n + 1)]
    rect(xleft = x.l, ybottom = y.b, 
         xright = x.r, ytop = y.t, 
         density=-1,border = NA,
         col = col2tr('black',120))
    
    y1 = y[!select]
    h = hist(y1, breaks = h$breaks, plot=FALSE)
    n = length(h$counts)
    x.l = rep(0, n)
    x.r = x.l - h$counts 
    y.b = h$breaks[1:n]
    y.t = h$breaks[2:(n + 1)]
    rect(xleft = x.l, ybottom = y.b, 
         xright = x.r, ytop = y.t, 
         density=-1,border = NA,
         col = col2tr('red',120))
  }
  
  
  par(mar=c(3,0,1,1),fig=c(0.35,1,0,1),new=TRUE)
  pch = 14 + colp
  # Transparent filled symbols
  if(!is.null(select)) {
    y1 = y[select]
    y2 = y[!select]
    colp = rep(col2tr('black',120),length(select))
    colp[!select] = col2tr('red',120)
  }
  plot(x, y, pch=pch, col=colp, ylim=range(y), 
       xlab=xlab, yaxt='n', main=NULL)
  grid();box()
  nClass=length(unique(colp))
  legend('topright',title=main,bty='n',legend='')
  abline(h=0,lty=3)
  if(outLiers) {
    plow = (1-p)/2
    pup  = p+plow
    lab = y > quantile(y,p=pup) | y < quantile(y,p=plow) 
    if(sum(lab)>0) {
      points(x = x[lab],
             y = y[lab],
             pch = 21,
             col = 2)
      text(x = x[lab],
           y = y[lab],
           labels = labels[lab],
           pos=4)
    }
  }
}

rescale = function(x) {
  # Rescale to [-1,1]
  2*((x - min(x, na.rm = TRUE)) / 
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))-0.5)
} 
  
paraPlot = function (x, col = 1, lty = 1, pch=19, 
                     las=las, var.label = NULL, ...) {
  # Parallel plot (adapted from MASS::parcoord)
  
  rx <- apply(x, 2, range, na.rm = TRUE)
  x <-  apply(x, 2, rescale)
  matplot(1:ncol(x), t(x), type = "l", col = col, lty = lty, 
          xlab = "", ylab = "Arb. units", mgp=c(1.5,0.75,0),
          xlim=c(1,ncol(x)+1),axes = FALSE, ...)
  matpoints(1:ncol(x), t(x), col = col, pch=pch,cex=0.8)
  axis(1, at = 1:ncol(x), labels = colnames(x), las=las)
  axis(2, at = seq(-1,1,by=0.2), labels = seq(-1,1,by=0.2), 
       pos=1, las=las)

  for (i in 1:ncol(x)) {
    lines(c(i, i), c(-1, 1), col = "grey70")
  }
  invisible()
}

genColors = function(sample) {
  ncols=length(sample)
  co=(    sample -min(sample))/
    (max(sample)-min(sample))
  indx=round(1+(ncols-1)*co)
  cols=fields::two.colors(ncols,start="blue",middle="yellow",end="red")[indx]
  return(cols)
}

myParPlot = function (X, maxPoints = 256, cex = 1, las=2,
                      labels=NULL, colors=NULL, lwd=1) {
  # Driver for paraPlot
  sdX = apply(X, 2, sd)
  nP = min(maxPoints, nrow(X))
  iSamp = seq.int(1, nrow(X), length.out = nP)
  X1 = X[iSamp, sdX != 0]
  par(cex = cex, mar=c(6,3,0.5,0.0))
  if(is.null(colors))
    colors = genColors(X1[, ncol(X1)])
  paraPlot(X1, col = colors, lwd = lwd, las=las, var.label=labels)
}

joyPlot <- function(X, xlab='x',xlim=range(X)) {
  # Joy/Ridge plot of distributions and 95% CI
  methList = colnames(X)
  for (i in rev(1:length(methList))) {
    meth=methList[i]
    dens = density(X[,meth])
    x = dens$x
    y = (i-1)+1.5*dens$y/max(dens$y)
    matplot(x,y,
            xlim=xlim,
            ylim=c(0,length(methList)+0.2),
            type='l', col=cols[i],
            add = i!=length(methList),
            yaxt = 'n',
            xlab= xlab, ylab='', bty='n')
    if(i==length(methList)) 
      grid(ny=0,col='gray80',lty=2,lwd=2)
    abline(h=(i-1),col=cols[i])
    
    # Density
    polygon(x,y,min(dens$x),(i-1),col=cols[i],border=NA)
    # Q95 box
    qq = quantile(X[,meth],probs = c(0.025,0.975))
    polygon(c(qq[1],qq[1],qq[2],qq[2]),
            c(i-1,i-0.5,i-0.5,i-1),col='gray80',border=NA)
    polygon(x,y,min(dens$x),(i-1),col=cols[i],border=NA)
    # Rug plot
    segments(X[,meth],i-1,X[,meth],i-0.9,col='gray20',lwd=2)
    # Mean
    segments(mean(X[,meth]),i-1,mean(X[,meth]),i-0.05,
             col='gray10',lwd=4,lty=2)
  }
  mtext(methList,side=2,col=1,
        at=(1:length(methList))-1, 
        las=1,line=0.25,padj=0,adj=1,cex=3)
  legend('topright',legend=c('Mean','95% CI'),
         lty=c(2,1),col=c('gray10','gray80'),lwd=c(4,40),
         bty='o', box.col='white' )
}


## Folded Normal Distribution functions
erf = function(x) 
  2 * pnorm(x * sqrt(2)) - 1

### Mean
muF = function(x) {
  mu=x[1]; sig=x[2]
  sig*sqrt(2/pi)*exp(-mu^2/(2*sig^2)) -mu*erf(-mu/(sqrt(2)*sig))
}

### CDF
cdfF = function(x) {
  u = x[1]; mu = x[2]; sig = x[3]
  (erf((u+mu)/(sqrt(2)*sig))+erf((u-mu)/(sqrt(2)*sig)))/2
}

### Q95
q95F = function(x,eps) {
  mu=x[1]; sig=x[2]
  G = apply(cbind(eps,mu,sig),1,cdfF)
  eps[(which(G>=0.95))[1]]
}

## Function for Bootstraping of FND quantiles quantiles
qF.fun = function(data,indices,probs=0.95,eps) {
  X = data[indices]
  M = cbind(mean(X),eps)
  G = apply(M,1,muF)
  sig = eps[(which(G>=mean(abs(X))))[1]] # Estimate sigma by inversion
  M = cbind(eps,mean(X),sig)
  G = apply(M,1,cdfF)
  eps[(which(G>=probs))[1]] # Estimate Q95 by inversion
} 

## Misc. Funcs for Bootstrap
m.fun = function(data,indices) {
  mean(data[indices])
}
q.fun = function(data,indices,probs) {
  quantile(data[indices],probs)
} 


# Get data ####

DF = read.csv(file='dataG399.csv', 
              stringsAsFactors = FALSE, 
              check.names = FALSE)

systems = DF$systems

nAtoms  = DF$nAtoms
names(nAtoms) = systems

compos  = DF$compos
names(compos) = systems

Eref    = DF$Eref
names(Eref) = systems

Errors  = DF[,-c(1:4)]
rownames(Errors) = systems

methList = colnames(Errors)
nMeth = length(methList)


# Data processing ####

## Variants of Errors
ErrorsPA    = Errors / nAtoms
AbsErrors   = abs(Errors)
AbsErrorsPA = abs(ErrorsPA)

## Process compositions for easier handling
cl = as.list(compos)
catoms =  lapply(cl,FUN = function(x) strsplit(x,split ='\\+'))
counts = latoms = matrix(NA,nrow=length(catoms),ncol=10)
for (i in 1:length(catoms)) {
  cp = unlist(catoms[[i]])
  for (j in 1:length(cp)) {
    elems = unlist(strsplit(cp[j],split=' '))
    sel = elems != ""
    counts[i,j] = as.numeric(elems[sel][1])
    latoms[i,j] = elems[sel][2]
  }
}
atomList = sort(unique(as.vector(latoms)))
compoTab = matrix(0,nrow=length(systems),ncol=length(atomList))
colnames(compoTab) = atomList
rownames(compoTab) = systems
for (i in 1:nrow(counts))
  for (j in 1:ncol(counts)) {
    if (!is.na(latoms[i,j]))
      compoTab[i,latoms[i,j]] = counts[i,j]
  }

hetero   = !(atomList %in% c("C","H","N","O"))
selHet   = rowSums(compoTab[,hetero]) != 0
selCHON  = !selHet
haloS    = atomList %in% c("S","F","Cl")
selHaloS = rowSums(compoTab[,haloS]) != 0

# Normality tests
nor = norPA = rep(NA,nMeth)
names(nor) = names(norPA) = methList
for (meth in methList) {
  nor[meth] = signif(shapiro.test(Errors[,meth])$p.value,2)
  norPA[meth] = signif(shapiro.test(ErrorsPA[,meth])$p.value,2)
}
write.csv(data.frame(AE=nor,IAE=norPA),file = 'NormalityTest.csv')

# Table 1 ####
## Usual stats 
### AE
MUE = RMSD = RMSE = MSE = LND = LPD = c()
for(meth in methList) {
  X = Errors[,meth]
  RMSD[meth] = sd(X)
  RMSE[meth] = sqrt(mean(X^2))
  MUE[meth]  = mean(abs(X))
  MSE[meth]  = mean(X)
  LND[meth]  = min(X)
  LPD[meth]  = max(X)
}
sink(file = 'Table_1_AE.tex')
xtable::xtable(data.frame(MUE=MUE, MSE=MSE, RMSE = RMSE, 
                          RMSD=RMSD, LND=LND, LPD=LPD), 
             type='latex', digits=2)
sink()

### IAE
for(meth in methList) {
  X = ErrorsPA[,meth]
  RMSD[meth] = sd(X)
  RMSE[meth] = sqrt(mean(X^2))
  MUE[meth]  = mean(abs(X))
  MSE[meth]  = mean(X)
  LND[meth]  = min(X)
  LPD[meth]  = max(X)
}
sink(file = 'Table_1_IAE.tex')
xtable::xtable(data.frame(MUE=MUE, MSE=MSE, RMSE = RMSE,
                          RMSD=RMSD, LND=LND, LPD=LPD), 
             type='latex', digits=2)
sink()


# Table 2 #### 
##Alternative stats for IAE

### Percentiles and thresholds to tabulate
probs  = c(0.5,0.75,0.90,0.95)
thresh = c(0.25,0.5,1.0,2.0)

pMUE = pCMUE = uCMUE = CMUE = MUE  = uMUE = MSE = uMSE = c()
pQ = quant = sigQ = matrix(NA,ncol=length(probs),nrow=nMeth)
ciQ   = array(NA,dim=c(nMeth,length(probs),2),
              dimnames=list(methList))
C     = matrix(NA,ncol=length(thresh),nrow=nMeth)
rownames(C)=rownames(pQ)=rownames(quant)=rownames(sigQ)=methList
colnames(C)=paste0('C(',thresh,')')

for(meth in methList) {
  X = AbsErrorsPA[,meth]
  MUE[meth]    = mean(X)
  uMUE[meth]   = sd(X)/length(X)^0.5
  pMUE[meth]   = prettyUnc(MUE[meth],uMUE[meth],uDig)
  CMUE[meth]   = sum(X<=MUE[meth])/length(X)
  uCMUE[meth]  = sqrt( CMUE[meth]*(1-CMUE[meth])/length(X) )
  pCMUE[meth]  = prettyUnc(CMUE[meth],uCMUE[meth],uDig)
  quant[meth,] = quantile(X,probs = probs)

  for(i in 1:length(probs)) { # Bootstrap quantiles
    SB           = boot::boot(X, q.fun, R=nbst, probs=probs[i], 
                              parallel="multicore")
    ciQ[meth,i,] = boot::boot.ci(SB,type="perc")$percent[4:5]
    sigQ[meth,i] = sd(SB$t[,1])
    pQ[meth,i]   = prettyUnc(quant[meth,i],sigQ[meth,i],uDig)
  }
  
  for(i in 1:length(thresh)) {
    p  = sum(X<=thresh[i])/length(X)
    up = sqrt(p*(1-p)/length(X))
    C[meth,i] = prettyUnc(p,up,uDig)
  }
}

DF = data.frame(pMUE,pCMUE,C)

sink(file = 'Table_2a.tex')
xtable::xtable(DF,type='latex')
sink()


DF = data.frame(
  Q50=pQ[,1],
  Q75=pQ[,2],
  Q90=pQ[,3],
  L_Q90=ciQ[,3,1],H_Q90=ciQ[,3,2],
  Q95=pQ[,4],
  L_Q95=ciQ[,4,1],H_Q95=ciQ[,4,2]
)

sink(file = 'Table_2b.tex')
xtable::xtable(DF,type='latex', digits=2)
sink()

# Table 3 ####
## Ranking inversion proba

io=order(MUE,decreasing = FALSE)

p12 = matrix(NA,nrow=nMeth,ncol=nMeth,
             dimnames=list(methList[io],methList[io]))
for (i1 in 2:nMeth) {
  for (i2 in 1:(i1-1)) {
    meth1 = methList[io][i1]
    meth2 = methList[io][i2]
    p12[i1,i2] = pnorm(0, mean=MUE[meth1]-MUE[meth2],
                          sd=sqrt(uMUE[meth1]^2+uMUE[meth2]^2)
    )
  }
}

sink(file = 'Table_3.tex')
xtable::xtable(p12, type='latex', digits=2)
sink()

# Fig 1 ####
## Error distribution example

x   = seq(0, 1, length.out = 1000)
m   = 2.75
Y1  = 1 + m*x
Y2  = (1+x)^2
err = Y1-Y2

tiff(file='Fig_01.tiff',width=2*height,height=height,compression="lzw")
par(mfrow=c(1,2), mar=mar, mgp=mgp, tcl=tcl, 
    cex=cex, lwd=lwd, xaxs='i', yaxs='i')

plot(x,Y2,type='l',xlab='x',ylab='y')
grid();box()
lines(x,Y1,lty=2,col=2)
polygon(c(x,rev(x)), c(Y1,rev(Y2)), col=blue_tr, border=NA)
legend('topleft', bty='o', box.lty=0,
       title ='  (a)', title.adj=0, 
       legend = c(expression(y=(1+x)^2),
                  expression(y=1+2.75*x),
                  'Error'
                  ),
       col=c(1,2,blue_tr),
       lty=c(1,2,1),
       lwd=c(lwd,lwd,24)
       )
box()

hist(err,nclass=33, border=4, col=blue_tr, freq=FALSE,
     main='',xlab='Error',ylim=c(0,16),xaxs='r',ylab='g(Error)')
grid()
abline(v=0,lty=2,col=1)
X = sort(err)
sel = X>=0
lines(X[sel] , 2/sqrt((m-2)^2-4*X[sel]),col=2)
lines(X[!sel], 1/sqrt((m-2)^2-4*X[!sel]),col=2)
legend('topleft',legend='',title='  (b)', title.adj=0, bty='o', box.lty=0)
box()

void=dev.off()


# Fig 2 ####
## Folded Normal Distribution

reso = ifelse(quick, 0.2, 0.02)

# Grid for inversion of quantiles
eps  = seq(0., 12, length.out=1000)

mu   = seq(0,5,by=reso)
sig  = seq(0,5,by=reso)
grid = expand.grid(x1=mu, x2=sig)
res  = apply(grid, 1, muF)
qmu  = apply(cbind(res,grid),1,cdfF)
resQ = apply(grid, 1, q95F, eps=eps)

tiff(file='Fig_02.tiff',width=2*height,height=2*height,compression="lzw")
par(mfrow=c(2,2),mar=mar,mgp=mgp,tcl=tcl, 
    pty='m',cex=4,lwd=6)

c1 = dnorm( eps,1,1)
c2 = dnorm(-eps,1,1)
c3 = c1+c2
plot(eps,c1, type='l', lty=1, col=4, 
     xlim=c(-4,4),ylim=c(0,0.5),yaxs='i', 
     # main='PDF', 
     xlab=expression(epsilon~(kcal/mol)),
     ylab='probability density (mol/kcal)')
lines(-eps,    c2, lty=1, col=4)
lines( eps,    c2, lty=2, col=4)
lines(-eps,-eps*0, lty=1, col=2)
lines( eps,    c3, lty=1, col=2)
abline(v=0,lty=3)
legend('topleft',legend=c('Normal','Folded'),
       col=c(4,2), lty=1, ncol = 1,
       title = '  (a)', title.adj=0, box.lty=1, seg.len=1.5)
box()

contour(mu, sig, matrix(res, length(mu), length(sig)), 
        20, col=4, labcex = cex, method="edge",
        vfont=c('sans serif','bold'),
        xlab=expression(mu~(kcal/mol)),
        ylab=expression(sigma~(kcal/mol)),
        # main=expression(mu[FN]),
        xaxs='i',yaxs='i'
)
grid()
contour(mu, sig, matrix(res, length(mu), length(sig)), 
        20, col=4, labcex = cex, method="edge",
        vfont=c('sans serif','bold'),
        add=TRUE # Replot over grid lines...
)
legend('topleft',
       legend=expression(mu[FN]),
       col=4, lty=1,
       title = '  (b)',title.adj=0, box.lty=1, seg.len=1.5)
box()

contour(mu, sig, matrix(qmu, length(mu), length(sig)), 
        15, col=4, labcex = cex, method="edge",
        vfont=c('sans serif','bold'),
        xlab=expression(mu~(kcal/mol)),
        ylab=expression(sigma~(kcal/mol)),
        # main=expression(C~(mu[FN])),
        xaxs='i',yaxs='i'
        )
grid()
legend('topleft',
       legend=expression(C~(mu[FN])),
       col=4, lty=1,
       title = '  (c)',title.adj=0, box.lty=1, seg.len=1.5)
box()

contour(mu, sig, matrix(res, length(mu), length(sig)), 
        20, col=4, labcex = cex, method="edge",
        vfont=c('sans serif','bold'),
        xlab=expression(mu~(kcal/mol)),
        ylab=expression(sigma~(kcal/mol)),
        # main=expression(Q[95]),
        xaxs='i',yaxs='i'
)
grid()
contour(mu, sig, matrix(res, length(mu), length(sig)), 
        20, col=4, labcex = cex, method="edge",
        vfont=c('sans serif','bold'),
        add=TRUE # Replot over grid lines...
)
contour(mu, sig, matrix(resQ, length(mu), length(sig)), 
        15, col=2, lty=2, lwd=8, labcex = cex, method="edge",
        vfont=c('sans serif','bold'),
        add=TRUE)
legend('topleft',
       legend=c(expression(mu[FN]),expression(Q[95])),
       col = c(4,2), lty= c(1,2), lwd=c(6,8),
       title = '  (d)',title.adj=0, box.lty=1, seg.len=1.5)
box()

void=dev.off()

# Fig 3 ####
##ECDF IAE/B3LYP

meth=1
X = abs(ErrorsPA[,meth])
tiff(file='Fig_03.tiff',width=1.2*height,height=height,compression="lzw")
par(mfrow=c(1,1),mar=mar, mgp=mgp, tcl=tcl, 
    cex=cex, lwd=lwd, lend=2)

x   = knots(ecdf(X))
prob= (1:length(x)) / length(x)

plot(x,prob,main='', type='l',     
     xlim=c(0,8), ylim=c(0,1), xaxs='i', yaxs='i',
     xlab=expression( epsilon ~(kcal/mol) ),
     ylab=expression( hat(G)[F](epsilon) ) 
     )
grid()

sigp = sqrt( prob*(1-prob)/length(X) )
polygon(c(x,rev(x)),c(prob-1.96*sigp,rev(prob+1.96*sigp)),
        col=blue_tr2,lty=0)

segments(2,0   ,2,0.85,col=2,lty=3,lwd=4)
arrows(2,0   ,2,0.1,col=2,lty=1,lwd=4,code=2,length=0.3)
segments(2,0.80,2,0.90,col=2,lty=1,lwd=4)
segments(0,0.80,2,0.80,col=2,lty=3,lwd=3)
segments(0,0.85,2,0.85,col=2,lty=2,lwd=3)
segments(0,0.90,2,0.90,col=2,lty=3,lwd=3)
segments(0,0.80,0,0.90,col=2,lty=1,lwd=20)
mtext(expression(eta),side=1,at=c(2),cex=2.5,col='red')
mtext(expression(C(eta)),side=2,at=c(0.85),cex=2.5,col='red',las=1)

segments(0  ,0.95,4.4,0.95,col='darkgreen',lty=3,lwd=4)
arrows(0  ,0.95,0.5,0.95,col='darkgreen',lty=1,lwd=4,code=2,length=0.3)
segments(3.4,0.95,5.5,0.95,col='darkgreen',lty=1,lwd=4)
segments(4.4,0.95,4.4,0.0 ,col='darkgreen',lty=2,lwd=3)
segments(3.4,0.95,3.4,0.0 ,col='darkgreen',lty=3,lwd=3)
segments(5.5,0.95,5.5,0.0 ,col='darkgreen',lty=3,lwd=3)
segments(3.4,0.0 ,5.5,0.0 ,col='darkgreen',lty=1,lwd=20)
mtext(expression(Q[95]),side=1,at=c(4.4),cex=2.5,col='darkgreen')
mtext('0.95',side=2,at=c(0.95),cex=2.5,col='darkgreen',las=1)

legend('bottomright', bty='n',
       legend=c('ECDF','95% CI'),
       col=c(1,blue_tr2), lty=c(1,1),lwd=c(4,30))
box()

void=dev.off()

# Fig 4 ####
## Histograms AE & IAE B3LYP

meth='B3LYP'
tiff(file=paste0('Fig_04.tiff'),width=2*height,height=height,compression="lzw")

par(mfrow=c(1,2),mar=mar, mgp=mgp, tcl=tcl, 
    pty='s', cex=cex, lwd=lwd)

X = Errors[,meth]
plotHist(X,nclass=33, plotGauss=TRUE,
         main='(a)',
         cex=cex,lwd=lwd,col=4,
         xlab='AE (kcal/mol)',
         ylab='Counts')

X = ErrorsPA[,meth]
plotHist(X,nclass=33, plotGauss=TRUE,
         main='(b)',
         cex=cex,lwd=lwd,col=4,
         xlab='IAE (kcal/mol)',
         ylab='Counts')

void=dev.off()

# Fig 5 ####
## Err dist AE & IAE B3LYP 

meth='B3LYP'
tiff(file=paste0('Fig_05a.tiff'),width=height,height=height,compression="lzw")
x = Eref
y = Errors[,meth]
plotDistHist(x,y,nclass=33, plotGauss=FALSE,
             main='(a)',
             cex=cex,lwd=lwd,col=4,
             xlab='AE (kcal/mol)',
             ylab='Error (kcal/mol)')
void=dev.off()

tiff(file=paste0('Fig_05b.tiff'),width=height,height=height,compression="lzw")
x = Eref/nAtoms
y = ErrorsPA[,meth]
plotDistHist(x,y,nclass=33, plotGauss=FALSE,
             main='(b)',
             cex=cex,lwd=lwd,col=4,
             xlab='IAE (kcal/mol)',
             ylab='Error (kcal/mol)')
void=dev.off()

# Figs 6 & 7  ####
## Error dististribution for all DFAs

col = rep(1,nrow(Errors))
col[selHet] = 2
 
x = nAtoms
for (meth in methList) {
    y = Errors[,meth]
  png(file=paste0('Fig_06_',meth,'.png'),width=1.2*height,height=1.2*height)
  plotDistHist(x,y,nclass=33, plotGauss=FALSE,
               main=meth,
               cex=4.5,lwd=3,col=4,
               xlab='Nb. of atoms in molecule',
               ylab='Error (kcal/mol)',
               outLiers=FALSE, p=0.70, labels=systems,
               select=selCHON)
  void=dev.off()
  
  y = ErrorsPA[,meth]
  png(file=paste0('Fig_07_',meth,'.png'),width=1.2*height,height=1.2*height)
  plotDistHist(x,y,nclass=33, plotGauss=FALSE,
               main=meth,
               cex=4.5,lwd=3,col=4,
               xlab='Nb. of atoms in molecule',
               ylab='IAE error (kcal/mol)',
               outLiers=FALSE, p=0.70, labels=systems,
               select=selCHON)
  void=dev.off()
}


### Assemble images
for(ifig in 6:7) {
  target = paste0('Fig_0',ifig)
  nImg = length(methList)
  meth = methList[1]
  img = png::readPNG(source=paste0(target,'_',meth,'.png'))
  dim1 = dim(img)[1]
  dim2 = dim(img)[2]
  bigImg = array(NA,c(3*dim1,3*dim2,3))
  i1 = 1; i2 = 1
  for (imeth in 1:nImg) {
    meth = methList[imeth]
    img = png::readPNG(source=paste0(target,'_',meth,'.png'))
    # cat(imeth,i1,i2,'\n')
    bigImg[i1:(i1+dim1-1),i2:(i2+dim2-1),1:3] = img
    i1 =  i1 + dim1
    if(imeth%%3 == 0) {
      i1 = 1
      i2 =  i2 + dim2
    }
    file.remove(paste0(target,'_',meth,'.png'))
  }
  tiff::writeTIFF(where = paste0(target,'.tiff'),what = bigImg, compression="LZW")
}

# Fig 8 ####
## Outliers B3LYP IAE

io = rev(c(1,5,8,3,6,2,4,9,7)) 
X1=ErrorsPA[,io]

tiff(file='Fig_08.tiff',width=1.5*height,height=1.2*height,compression="lzw")
y = X1[,'B3LYP']
lab1 = y > quantile(y,p=0.975)
lab2 = y < quantile(y,p=0.025) 
lab = lab1 | lab2
io = rev(order(y[lab]))
outl = data.frame(systems[lab][io],y[lab][io])

colPar=rep(orange_tr,nrow(X1))
colPar[lab1]='purple'
colPar[lab2]='darkgreen'

myParPlot(X1,labels=outl,cex=cex,lwd=lwd,colors = colPar)

labOut = c(expression(S~O[3]),
           expression(Cl[2]~O[2]~S),
           expression(S~O[2]),
           expression(Si~Cl[4]),
           expression(Si~F[4]),
           expression(S~F[6]),
           expression(N~O[2]),
           expression(N[2]~O),
           expression(P~H[2]),
           expression(N~H[2]),
           expression(N~H),
           expression(Be~H))
text(x=9.2, y=rescale(outl[,2]*c(1,1,1,1.02,1,0.95,0.8,1.01,1.13,1.05,1.1,1)),
     labels=labOut, 
     cex=0.75, pos=4, offset=-0.2)
void=dev.off()

# Figs 9 ####
## ECDFs IAE

tiff(file='Fig_09.tiff',width=3*height,height=3*height,compression="lzw")
par(mfrow=c(3,3),mar=c(3,3.5,0.5,0.5), mgp=mgp, tcl=tcl, 
    cex=2*cex,lwd=2*cex)
for(meth in methList) {
  X = abs(ErrorsPA[,meth])
  x   = knots(ecdf(X))
  prob= (1:length(x)) / length(x)
  
  plot(x,prob,main='', type='l',
       xlim=c(0,10), ylim=c(0,1), xaxs='i', yaxs='i',
       xlab=expression( epsilon ~(kcal/mol) ),
       ylab=expression( hat(G)[F](epsilon) )
  )
  grid()

  sigp = sqrt( prob*(1-prob)/length(X) )
  polygon(c(x,rev(x)),c(prob-1.96*sigp,rev(prob+1.96*sigp)),
          col=blue_tr,lty=0)
  polygon(c(x,rev(x)),c(prob-1.96*sigp,rev(prob+1.96*sigp)),
          col=blue_tr,lty=0)

  segments(0  ,0.95,ciQ[meth,4,1:2],0.95,col='darkgreen',lty=1)
  arrows(0  ,0.95,0.5,0.95,col='darkgreen',lty=1,code=2,length=0.3)
  mtext('0.95',side=2,at=c(0.95),cex=4,col='darkgreen',las=1)
  polygon(c(ciQ[meth,4,1:2],rev(ciQ[meth,4,1:2])),
          c(0.95,0.95,0,0),col=orange_tr,border = NA)
  segments(quant[meth,4],0,quant[meth,4],0.95,col='darkgreen',lty=1)

  legend('bottomright',bty='n',legend=meth,cex=0.8)
  box()
}
void=dev.off()

# Fig. 10 ####
## Q95 vs. Q95F

Q95 = Q95F = c()
ciQ95F = matrix(NA,nrow=nMeth,ncol=2)
rownames(ciQ95F) = methList
for(meth in methList) {
  X             = ErrorsPA[,meth]
  Q95[meth]     = quant[meth,4]
  Q95F[meth]    = qF.fun(X,1:length(X),eps=eps) # Estimate Q95 by inversion
  SB            = boot::boot(X, qF.fun, R=nbst, parallel="multicore", eps=eps)
  ciQ95F[meth,] = boot::boot.ci(SB,type="perc")$percent[4:5]
}

tiff(file='Fig_10.tiff',width=height,height=height,compression="lzw")
par(mfrow=c(1,1),mar=mar, mgp=mgp, tcl=tcl, 
    pty='s', cex=cex, lwd=lwd)

plot(Q95, Q95F, type='p', pch=19, col=cols,
     xlab=expression(Q[95]~(kcal/mol)), 
     ylab=expression(Q[95~F]~(kcal/mol)), 
     xlim=c(0,12),ylim=c(0,12),yaxs='i',xaxs='i')
abline(a=0,b=1)
grid()
segments(ciQ[,4,1],Q95F,ciQ[,4,2],Q95F,col=cols)
segments(Q95,ciQ95F[,1],Q95,ciQ95F[,2],col=cols)
legend('topleft',methList,ncol=1,
       bty='o', box.col='white',
       lwd=2,pch=19,col=cols,
       text.col=1,text.font=1)
box()

void = dev.off()

# Fig 11 ####
## Ranking

proba = uMUE = c()
sigq = matrix(NA,ncol=length(probs),nrow=nMeth)
rownames(sigq)=methList

for(meth in methList) {
  X            = AbsErrorsPA[,meth]
  uMUE[meth]   = sd(X)/length(X)^0.5
  proba[meth]  = sum(X<=MUE[meth])/length(X)
  D            = density(X, n=1024, adjust=1)
  for(i in 1:length(probs)) {
    d            = spline(D$x,D$y,xout=quant[meth,i])$y # Interpolate density
    sigq[meth,i] = sqrt( probs[i]*(1-probs[i])/length(X) ) / d
  }
}


tiff(file='Fig_11.tiff',width=2*height,height=1.1*height,compression="lzw")
par(mfrow=c(1,2),mar=c(5,3.5,0.5,0.5), mgp=mgp, tcl=tcl, 
    pty='s', cex=cex, lwd=lwd)


### MUE and percentiles for methods set
io = order(quant[,4],decreasing=FALSE)
matplot(1:nMeth,quant[io,],type='p',col=2:5,pch=19,
        xlab="",ylab='Absolute error (kcal/mol)',
        main='', xaxt='n',yaxs='i',ylim=c(0,15))
axis(1, at = 1:nMeth, labels = methList[io], las=2)
grid()
for(i in rev(1:length(probs))) {
   segments(1:nMeth,ciQ[io,i,1],
            1:nMeth,ciQ[io,i,2],col=i+1)
}
points(1:nMeth,MUE[io],col=1,pch=17)
lines( 1:nMeth,MUE[io],col=1)
legend('topleft',legend=c('MUE','Q50','Q75','Q90','Q95'),
       title ='  (a)', title.adj=0,text.font=1, lwd=lwd,
       col=1:5, bty='o',box.col='white',pch=c(17,19,19,19,19))
box()

### Correlation between MUE and Q95
plot(MUE, quant[,4], type='p', pch=19, 
     col=cols, log='',
     xlab='MUE (kcal/mol)', ylab=expression(Q[95]~(kcal/mol)),
     xaxs='i',yaxs='i',
     xlim= range(c(0.5,MUE-2*uMUE,MUE+2*uMUE)),
     ylim= c(0,15)
     )
grid()
segments(MUE,ciQ[,4,1],MUE,ciQ[,4,2],col=cols)
segments(MUE-2*uMUE,quant[,4],MUE+2*uMUE,quant[,4],col=cols)
legend('topleft',methList,ncol=2,
       bty='o',box.col='white',
       title ='  (b)', title.adj=0, lwd=lwd, pch=19, col=cols,
       text.col=1,text.font=1)
box()

void = dev.off()


# Fig 12 ####
## Bootstrapping

meth='B3LYP'
X = abs(ErrorsPA[,meth])
tiff(file='Fig_12.tiff',width=2*height,height=height,compression="lzw")
par(mfrow=c(1,2), mar=mar, mgp=mgp, tcl=tcl,
    cex=cex, lwd=lwd)

x   = knots(ecdf(X))
prob= (1:length(x)) / length(x)

plot(x,prob,main='', type='l',     
     xlim=c(0,8), ylim=c(0,1), xaxs='i', yaxs='i',
     xlab=expression( epsilon ~(kcal/mol) ),
     ylab=expression( hat(G)[F](epsilon) ) 
     )
grid()

set.seed(1234) # set seed for repeatability in closeup
for (i in 1:nbst) {
  Xb = sample(X,size=length(X),replace = TRUE)
  x   = knots(ecdf(Xb))
  prob= (1:length(x)) / length(x)
  lines(x,prob,col=blue_tr,lty=1)
}

x   = knots(ecdf(X))
prob= (1:length(x)) / length(x)
sigp = sqrt( prob*(1-prob)/length(X) )

lines(x,prob          ,col=1,lty=1)
lines(x,prob-1.96*sigp,col=1,lty=2)
lines(x,prob+1.96*sigp,col=1,lty=2)

D = density(X,n=1024,adjust=1)
d = spline(D$x,D$y,xout=x)$y
sigx = sqrt( prob*(1-prob)/length(X) ) / d

lines(x-1.96*sigx,prob,col=2,lty=2)
lines(x+1.96*sigx,prob,col=2,lty=2)

legend('bottomright', 
       bty='o', box.col='white', 
       title= '  (a)', title.adj=0,
       legend=c('CI for probabilities','CI for quantiles','Bootstraped ECDFs'),
       col=c(1,2,4), lty=c(2,2,1))
box()

### Close up 
set.seed(1234) # Ensure reproducibility for closeup

plot(x,prob,main='', type='l',    
     xlim=c(0,8), ylim=c(0.8,1), xaxs='i', yaxs='i',
     xlab=expression( epsilon ~(kcal/mol) ),
     ylab=expression( hat(G)[F](epsilon) ) 
     )
grid()

for (i in 1:nbst) {
  Xb = sample(X,size=length(X),replace = TRUE)
  x   = knots(ecdf(Xb))
  prob= (1:length(x)) / length(x)
  lines(x,prob,col=blue_tr,lty=1)
}

x   = knots(ecdf(X))
prob= (1:length(x)) / length(x)
sigp = sqrt( prob*(1-prob)/length(X) )

lines(x,prob          ,col=1,lty=1)
lines(x,prob-1.96*sigp,col=1,lty=2)
lines(x,prob+1.96*sigp,col=1,lty=2)

D = density(X,n=1024,adjust=1)
d = spline(D$x,D$y,xout=x)$y
sigx = sqrt( prob*(1-prob)/length(X) ) / d

lines(x-1.96*sigx,prob,col=2,lty=2)
lines(x+1.96*sigx,prob,col=2,lty=2)

legend('bottomright',        
       bty='o', box.col='white', 
       title= '  (b)', title.adj=0,
       legend=c('CI for probabilities','CI for quantiles','Bootstraped ECDFs'),
       col=c(1,2,4), lty=c(2,2,1))
box()

void=dev.off()

# Fig 13 #### 
## Size effects on CI

### Quantiles of the FND(0,1) vs. sample size

Ntry = c(10,20,30,40,50,75,100,200,300,400,500) # Sample sizes
q95 = q90 = matrix(NA,ncol=length(Ntry),nrow=nMC)
set.seed(1234)
for (j in 1:length(Ntry)) 
  for (i in 1:nMC) {
    X = abs(rnorm(Ntry[j],0,1))
    q90[i,j] = quantile(X,0.90)
    q95[i,j] = quantile(X,0.95)
  } 

Q90  = t(apply(q90,2,function(x) quantile(x,probs = c(0.025,0.975))))
Q90m = t(apply(q90,2,mean))
Q95  = t(apply(q95,2,function(x) quantile(x,probs = c(0.025,0.975))))
Q95m = t(apply(q95,2,mean))

tiff(file='Fig_13.tiff',width=2*height,height=height,compression="lzw")
par(mfrow=c(1,2), mar=mar, mgp=mgp, tcl=tcl, 
    pty='s', cex=cex, lwd=lwd)

plot(Ntry,Q95m,type='l',lty=1, lwd=6, col=cols[1], xaxs='i',
        xlab='Sample size N',ylab='Estimated Percentiles',
     ylim=range(c(Q95,Q90)))
grid()
polygon(c(Ntry,rev(Ntry)),c(Q95[,1],rev(Q95[,2])),col=col_tr[1],border = NA)
lines(Ntry,Q90m,lty=1, lwd=6,col=cols[2])
polygon(c(Ntry,rev(Ntry)),c(Q90[,1],rev(Q90[,2])),col=col_tr[2],border = NA)

legend('topright',
       title='(a) FND(0,1)',
       legend = c(expression(Q[95]),expression(Q[90])),
       bty='o', box.col='white', 
       col=cols, 
       lwd=6)
box()

### Quantiles of the B3LYP subsets
meth='B3LYP'
Ntry = c(10,20,30,40,50,75,100,200,nrow(AbsErrorsPA))
q95 = q90 = matrix(NA,ncol=length(Ntry),nrow=nMC)
for (j in 1:length(Ntry)) 
  for (i in 1:nMC) {
    X = sample(AbsErrorsPA[,meth],size=Ntry[j],replace=TRUE)
    q90[i,j] = quantile(X,0.90)
    q95[i,j] = quantile(X,0.95)
  } 
Q90  = t(apply(q90,2,function(x) quantile(x,probs = c(0.025,0.975))))
Q90m = t(apply(q90,2,mean))
Q95  = t(apply(q95,2,function(x) quantile(x,probs = c(0.025,0.975))))
Q95m = t(apply(q95,2,mean))

plot(Ntry,Q95m,type='l',lty=1, lwd=6, col=cols[1], xaxs='i',
        xlab='Sample size N',ylab='Estimated Percentiles',
     ylim=range(c(Q95,Q90)))
grid()
polygon(c(Ntry,rev(Ntry)),c(Q95[,1],rev(Q95[,2])),col=col_tr[1],border = NA)
lines(Ntry,Q90m,lty=1, lwd=6,col=cols[2])
polygon(c(Ntry,rev(Ntry)),c(Q90[,1],rev(Q90[,2])),col=col_tr[2],border = NA)

legend('topright',
       title = '(b) B3LYP',
       legend = c(expression(Q[95]),expression(Q[90])),
       bty='o', box.col='white', 
       col=cols, 
       lwd=6)
box()

void=dev.off()

# Alternative Figs ####

## Joy plots of IAE errors and unsigned errors
tiff(file='Fig_Alt1.tiff',width=1.2*height,height=height,compression="lzw")
par(mfrow=c(1,1), mar=c(3.0,5.0,0.5,0.5), mgp=mgp, tcl=tcl, 
    cex=cex, lwd=lwd, lend=2)
joyPlot(ErrorsPA,xlab='IAE Errors (kcal/mol)')
void=dev.off()

tiff(file='Fig_Alt2.tiff',width=1.2*height,height=height,compression="lzw")
par(mfrow=c(1,1), mar=c(3.0,5.0,0.5,0.5), mgp=mgp, tcl=tcl, 
    cex=cex, lwd=lwd, lend=2, xaxs='i')
joyPlot(AbsErrorsPA,xlab='Absolute IAE Errors (kcal/mol)',xlim=c(0,15))
void=dev.off()

## (median,iqr) plot

iqr = med = c()
for (meth in methList) {
  med[meth] = median(ErrorsPA[,meth])
  iqr[meth] = diff(quantile(ErrorsPA[,meth],probs = c(0.025,0.975)))
}

tiff(file='Fig_Alt3.tiff',width=height,height=height,compression="lzw")
par(mfrow=c(1,1), mar=c(3.0,3.5,0.5,0.5), mgp=mgp, tcl=tcl, 
    cex=cex, lwd=lwd, lend=2)
plot(med,iqr,col=cols,pch=19,
     xlab='Median Error', ylab='95% IQR of Errors')
grid()
abline(v=0,col='gray20',lty=2)
legend('bottomright',methList,ncol=1,
       bty='o', box.col='white',
       pch=19,col=cols,
       text.col=1,text.font=1)
box()
void=dev.off()

