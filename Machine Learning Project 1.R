install.packages("plyr")
library("plyr")


Program <- function(){
	
	
	n <- readline(prompt="Enter program name: ")
	

}


d1<-read.table("/Users/Dungeoun/Documents/sample1.txt", fill = TRUE)
p2<-read.table("/Users/Dungeoun/Documents/partition2.txt", fill = TRUE)


# Code to clean the data and extract info



partition<-function(dataset1, partition2){
	
# step 1: read the dataset and the partition2
	
	#d1: Dataset table
	#df: final dataset table
	
	d1<-read.table(dataset1, fill = TRUE)
	d1<-as.data.frame(d1)
	df<-d1[2:nrow(d1),]
	rownames(df)<-c(1:nrow(df))
	
	#p2: partitions
	#pf: Final partition table
	
	p2<-read.table(partition2, fill = TRUE) 
	p2<-as.data.frame(p2)
	rownames(p2)<-p2$V1
	pf<-p2[,2:ncol(p2)]
	pf<-t(pf)
	
	
	
# step 2: Determine which partition will get partitioned first

# Entropy of partition2

	# m1: matrix holding the number of total elements in a partition
	m1<-matrix(0,ncol(pf),1)
	
	for(i in 1:ncol(pf)){
		
		x<-pf[,i]
		
		m1[i,1]<- length(x[!is.na(x)])		# taking only non NA Values and
											# storing them in m1
			
			
	}
	
	# m2: matrices holding the contents of different partitions
	
	m2<-matrix(0,max(m1),length(m1))
	
	for(i in 1:nrow(m2)){
		
		for(j in 1:m1[i,1]){
			
			m2[j,i]<-df[pf[j,i],ncol(df)]   # taking values from df into m2 
											# with help of pf as index
											
											# MAY NEED TRY-CATCH STATEMENT
											
		}
	}
	

		
}

	
	
