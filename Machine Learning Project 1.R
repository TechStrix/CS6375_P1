install.packages("plyr")
library("plyr")


Program <- function(){
	
	
	n <- readline(prompt="Enter program name: ")
	

}


d1<-read.table("/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /sample1.txt", fill = TRUE)
p2<-read.table("/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /partition2.txt", fill = TRUE)


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
			print(j)								
			
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
	
	
	
	
	#m6: Matrix Holding Conditional entropies of all partitions (each in 1 column)
	#m7: Temporary Matrix for holding single array
	z <- ncol(df) - 1
	m6<-matrix(NA, ncol(df)-1,ncol(pf) )
	
		for(l in 1:ncol(pf)){			
			print(l)	
			for(k in 1:z){		
				m7<-matrix(NA, m1[l,1], 2)
				
				a = 0	
				for(n in 1:m1[l,1]){
				
					a=a+1
					b=pf[a,l]
					m7[n,1]<- df[b,k]
					m7[n,2]<- df[b,ncol(df)]
					
					#m7[l,1]<- df[pf[m1[l,1],l],k]		# taking value of df at [index a1, column k] into m7
					#m7[l,2]<- df[pf[m1[l,1],l],df[ncol(df)]]	# taking the target attribute value
					
				}
				print(m7)
				
				#Calculate entropy of this matrix
				
				#step1: Calculate the probabilities of labels of this matrix P(s1|attribute)
				
				# 1.1: m8: matrix of unique labels
				m8<-matrix(NA,nrow(m7),1)
				m8<-t(t(count(m7[,1][!is.na(m7[,1])])$x))
				
				# 1.2: m9: matrix of freq of each label
				m9<-matrix(NA,nrow(m7),1)	
				m9<-t(t(count(m7[,1][!is.na(m7[,1])])$freq))
				
				# 1.3: m10: Probability of each label from total no. of labels
				
				m10<-matrix(NA, nrow(m9),1)
				for( i in 1: nrow(m9)){
					m10[i,1]<-m9[i,1]/sum(m9)
				}	
				
				#Step 2: Calculate the probabilities of last attributes	
				
				# 2.1: m11:Matrix Containing the Values according to the uniqueness of the labels columnwise
				m11<-matrix(NA,nrow(pf),nrow(m8) )		# No. of columns = no. of unique labels
														# No. of rows < no. of total ele in parti  
				
				for(i in 1:nrow(m8)){					#traversing m8: having attri & target value
					t = 1								# k counter doesn't move if value doesn't match label
					for(j in 1:nrow(m7)){
					
						if(identical(m7[j,1],m8[i,1])){ #if element frm all label table = element from unique 														# label table
							
							m11[t,i]<-m7[j,2]		# put target elemwnt from all label table to matrix 														# (column wise)
							t<-t+1
						}
					}
								
				}
				
				#2.2: m12: Matrix holds the probabilities in different columns of each unique label
				
				m12<-matrix(NA, nrow(m7), ncol(m11))		
				
				for(i in 1:ncol(m11)){
					for(j in 1:nrow(m7)){
					
					m12[j,i]<-count(m11[,i][!is.na(m11[,i])])$freq[j]/length(m11[,i][!is.na(m11[,i])])				
					
					}
				}
				
				
				
				#Step 3: Calculate Entropy of the which is also the conditional probability
				
				# m13 : Matrix that will hold the conditional entropy
				m13<-matrix(0,nrow(m10),1)
				
				tryCatch({
				for( i in 1:nrow(m10)){
					for(j in 1:nrow(m12)){
						
						m13[i,1]<- -1*m10[i,1][!is.na(m10[i,1])]*(m12[j,i][!is.na(m12[j,i])])*log2(m12[j,i][!is.na(m12[j,i])])
						
					}
				}
				},error=function(e){})
				#print(m13)
				
			m6[l,k]<-sum(m13)
			}
					
		}

	
	
