library('RColorBrewer')
pal = brewer.pal(5,"Set1")
pdf("plot.pdf",height=9,width=12,bg="#EEEEEE")
par(mfrow=c(3,4))

races = read.table("races.results.tsv",as.is=T)
# Matrix for winners
all.win = matrix(NA,ncol=36,nrow=nrow(races))

for ( r in 1:nrow(races) ) {
cat( races[r,1] , '\n' )
lst = read.table(file=paste(races[r,1],".list",sep=''),as.is=T)[,1]

r.m = races[r,2]
r.y = races[r,1]
r.winner = races[r,3]

# load all files and get all unique names
# assign colors to names
cand = vector()
for ( l in lst ) {
cat(l,'\n')
cand = c(cand,read.table(l,as.is=T)[,1])
}
cand = unique(cand)

# load files again
mat = matrix(NA,nrow=length(lst),ncol=length(cand))
x = 1:nrow(mat)

colnames(mat) = cand
ctr = 1
for ( l in lst ) {
date = strsplit(l,".",fixed=T)[[1]]
x[ctr] = -1 * (12 * (r.y - as.numeric(date[1])) + r.m - as.numeric(date[2]))
cat(l,'\n')
poll = read.table(l,as.is=T)
m = match( poll[,1] , cand )
mat[ctr,m] = as.numeric(gsub("%","",poll[,2])) 
ctr = ctr + 1
}
# Keep if before the convention
keep = x <= 0 
mat = mat[keep,]
x = x[keep]

##write.table(mat,quote=F)

# Keep if in more than 50% of polls (unless this is 1976 w/ race tight)
if ( r.y != 1976 ) {
keep = apply(!is.na(mat),2,mean) > 0.5
} else {
keep = apply(!is.na(mat),2,mean) > 0.1
}
mat = mat[,keep]

# Keep top 5 performers
tmp = mat
tmp[ is.na(tmp) ] = 0
keep = head(order(apply(tmp,2,mean),decreasing=T),5)
#ever.win = unique(apply(tmp,1,which.max))
#keep = unique(keep,ever.win)
mat = mat[,keep]

tmp = mat
tmp[ is.na(tmp) ] = 0
keep = apply(tmp,1,sum) > 0
tmp = tmp[keep,]
mat = mat[keep,]
x = x[keep]

#cat( 36 + x , '\n' )
#cat( apply(tmp,1,which.max) , '\n' )
#write.table(tmp)

all.win[r,36+x] = apply(tmp,1,which.max) == which(colnames(mat)==tolower(r.winner))
all.win[r,(36+tail(x,1)):36] = all.win[r,36+tail(x,1)]
clr = pal
clr[ colnames(mat) == tolower(r.winner) ] = "#000000"

matplot( x , mat ,  main=paste(races[r,1],":",r.winner) , type="p" , col=paste(clr,"50",sep='') , lty=1 , pch=19 , cex=0.5 , ylim=c(0,100) , xlim=c(-30,0) , xlab="Months to convention", ylab="Gallup Polling %",bty="n",las=1)
for ( i in 1:ncol(mat) ) {
lo = loess( mat[,i] ~ x , na.action = na.omit)

lines( lo$x , lo$fitted , col=clr[i])
#lines( lowess(x , mat[,i]) , col=clr[i])
#lines( x , mat[,i] , col=clr[i])
}

legend("topleft",legend=colnames(mat),col=clr,pch=19,box.lty=0,bty="n",pt.cex=0.75)
}

tot = apply(!is.na(all.win),2,sum)
all.win[ is.na(all.win) ] = FALSE
y = 100 * apply( all.win , 2 , sum ) / tot
cat( y , '\n' )
x = 1:36 - 36
lo = loess( y ~ x , na.action = na.omit)
#plot( 1:36 - 36 , 100*y , type="o" , pch=19 , col="black" , ylim=c(0,100) , xlim=c(-30,0) , main="% where leader was the winner" , xlab="Months to convention" , cex=0.5)
plot( x , y , type="p" , pch=19 , col="#00000050" , ylim=c(0,100) , xlim=c(-30,0) , main="Across all races\n% where leader was the winner" , xlab="Months to convention" , cex=0.5,ylab="",bty="n",las=1)
lines( lo$x , lo$fitted , col="#000000" , lwd=2)

plx<-predict(lo , se=T)
#lines(lo$x,plx$fit - qt(0.975,plx$df)*plx$se, lty=3)
#lines(lo$x,plx$fit + qt(0.975,plx$df)*plx$se, lty=3)

y.polygon <- c(plx$fit + qt(0.975,plx$df)*plx$se , rev(plx$fit - qt(0.975,plx$df)*plx$se))
x.polygon <- c(lo$x, rev(lo$x))
polygon(x.polygon, y.polygon, col="#00000020",border=NA)

dev.off()
