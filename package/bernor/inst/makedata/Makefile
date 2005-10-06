
all : booth-1.RData booth-2.RData booth-3.RData salam.RData

booth-1.RData : booth-1.R
	R CMD BATCH --vanilla booth-1.R

booth-2.RData : booth-1.RData booth-2.R
	R CMD BATCH --vanilla booth-2.R

booth-3.RData : booth-2.RData booth-3.R
	R CMD BATCH --vanilla booth-3.R

salam.RData : salam.R
	R CMD BATCH --vanilla salam.R

clean :
	rm -f *.RData *.Rout
