install.packages("plyr")
library("plyr")


Program <- function(){
	
	
	n <- readline(prompt="Enter program name: ")
	

}


d1<-read.table("/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /sample1.txt", fill = TRUE)
p2<-read.table("/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /partition2.txt", fill = TRUE)


# Code to clean the data and extract info



partition<-function(dataset1, partition2){

	install.packages("plyr")
	library("plyr")
	
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
		
		tryCatch({
		m1[i,1]<- length(x[!is.na(x)])		# taking only non NA Values 														# and
														# storing them in m1
					
		}, error=function(e){cat("ERROR1 :",conditionMessage(e), "\n")})
	}
	print("clear 1")
	# m2: matrices holding the last column contents of different partitions
	
	m2<-matrix(NA,max(m1),length(m1))
	
	for(i in 1:nrow(m2)){
		tryCatch({
		for(j in 1:m1[i,1]){
			
			
			m2[j,i]<-df[pf[j,i],ncol(df)]   # taking values from df into m2 
											# with help of pf as index
											
											# MAY NEED TRY-CATCH STATEMENT
											
			
		}
		}, error=function(e){cat("ERROR1 :",conditionMessage(e),"\n")})
	}
	print("clear 2")
	# m3: matrix holding the frequencies of different elements of last column
	
	m3<-matrix(NA, max(m1), length(m1))
	
	# m4: matrix holding the corresponding elements of the frequency table
	
	m4<-matrix(NA, max(m1), length(m1))
	
	for( i in 1:ncol(m3)){
		
		for(j in 1:length(m2[,i][!is.na(m2[,i])])){ 		  #taking the length 															 not including the na 																	values 	
			
			m3[j,i] <- count(m2[,i][!is.na(m2[,i])])$freq[j]
			m4[j,i] <- count(m2[,i][!is.na(m2[,i])])$x[j]
			#print(j)
			
		}
		
					
			
	}
	
	
	# Calculating the Entropy of all the features
	# m5: matrix holding the entropies of each partition
	
	m5<-matrix(0,1,ncol(pf)) 
	
	for(i in 1:ncol(pf)){
		
		for( j in 1:length(m3[,i][!is.na(m3[,i])])){
			
			m5[1,i]<- m5[1,i] - (m3[j,i]/m1[i,1])*log2(m3[j,i]/m1[i,1])
			#print(j)
			
			
		}
	}
	print(m5)
} 

	
	
