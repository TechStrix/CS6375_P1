


Program <- function(){
	
	
	n <- readline(prompt="Enter program name: ")
	

}


dataset1<-read.table("/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /sam1.txt", fill = TRUE)
partition2<-read.table("/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /part2.txt", fill = TRUE)


d1<-read.table("/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /sam1.txt", fill = TRUE)
p2<-read.table("/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /part2.txt", fill = TRUE)



# Code to clean the data and extract info



partition<-function(dataset1, partition2){
	
	
	install.packages("plyr")
	library("plyr")
	
# step 1: read the dataset and the partition2
	
	#d1: Dataset table
	#df: final dataset table
	
	d1<-read.table(file=dataset1,fill = TRUE)

	d1<-as.data.frame(d1)
	df<-d1[2:nrow(d1),]
	rownames(df)<-c(1:nrow(df))
	
	
	
	#p2: partitions
	#pf: Final partition table
	
	d1<-read.table(file=partition2,fill =TRUE)
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
		
		#tryCatch({
		m1[i,1]<- length(x[!is.na(x)])		# taking only non NA Values 														# and
														# storing them in m1
					
		#}, error=function(e){cat("ERROR1 :",conditionMessage(e), "\n")})
	}
	
	# m2: matrices holding the last column contents of different partitions
	
	m2<-matrix(NA,max(m1),length(m1))
	
	for(i in 1:nrow(m2)){
		tryCatch({
		for(j in 1:m1[i,1]){
			
			
			m2[j,i]<-df[pf[j,i],ncol(df)]   # taking values from df into m2 
											# with help of pf as index
											
											# MAY NEED TRY-CATCH STATEMENT
											
			
		}
		}, error=function(e){})
	}
	
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
	
	
	
	
	
	#m6: Matrix Holding Conditional entropies of all partitions (each in a row)
	#m7: Temporary Matrix for holding single array
	z <- ncol(df) - 1
	m6<-matrix(NA, ncol(pf),ncol(df)-1 )
	
		for(l in 1:ncol(pf)){			
				
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
				print(m8)
				
				# 1.2: m9: matrix of freq of each label
				m9<-matrix(NA,nrow(m7),1)	
				m9<-t(t(count(m7[,1][!is.na(m7[,1])])$freq))
				print(m9)
				# 1.3: m10: Probability of each label from total no. of labels
				
				m10<-matrix(NA, nrow(m9),1)
				for( i in 1: nrow(m9)){
					m10[i,1]<-m9[i,1]/sum(m9)
				}	
				print(m10)
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
				print(m11)
				#2.2: m12: Matrix holds the probabilities in different columns of each unique label
				
				m12<-matrix(1, nrow(m7), ncol(m11))		
				
				for(i in 1:ncol(m11)){
					for(j in 1:nrow(m7)){
					
					m12[j,i]<-count(m11[,i][!is.na(m11[,i])])$freq[j]/length(m11[,i][!is.na(m11[,i])])				
					
					}
				}
				
				print(m12)
				
				#Step 3: Calculate Entropy of the which is also the conditional probability
				
				# m13 : Matrix that will hold the conditional entropy
				m13<-matrix(0,length(m12[!is.na(m12)]),nrow(m10))
				
				
				for( i in 1:nrow(m10)){
					#tryCatch({
					for(j in 1:length(m12[!is.na(m12)])){
						
						m13[j,i]<- m10[i,1]*(m12[j,i])*log2(1/m12[j,i])	 #remvd na!	#changed order of i,j in m13
						
					}
					#},error=function(e){})

				}
				#print(m13)
				
			m6[l,k]<-sum(m13[!is.na(m13)])
		}
		print(l)			
	}

	# m14 :  Gain Matrix
	
	m14<-matrix(NA,nrow(m6),ncol(m6))
	
	for(i in 1: ncol(m5)){
		for(j in 1:nrow(m6)){
			
			m14[i,j]<- m5[1,i] - m6[i,j] 
			
		}
	}
	
	
	#m15 : F-Matrix
	
	m15<-matrix(NA, 1, ncol(m14))
	
	for(i in 1:nrow(m1)){
		
			
		m15[i]<-m1[i,]/nrow(df)*max(m14[i,][!is.na(m14[i,])])	
			
		
	}
	
	# m16: Store Which Partition will get partitioned first
	
	m16<-which(m15[!is.na(m15)] == max(m15[!is.na(m15)]), arr.ind = TRUE)
	
	cat( "partition that will get partitioned first is ", colnames(pf)[m16])
		
	#m17: Store by which feature will the partition get partitioned
	
	m17<-which(m14[m16,][!is.na(m14[m16,])] == max(m14[m16,][!is.na(m14[m16,])]), arr.ind = TRUE)
	
	cat( " ||  ","feature that will be used to partition is: Feature", m17)
	
	#m18: partition to be divided and it's target values 
	#m19: final Partitions 
	
	m18<-matrix(NA, m1[m16,1], 2)
	
	a<-0
	for(n in 1:m1[m16,1]){
				
		a=a+1
		b=pf[a,m16]
					
		m18[n,1]<- pf[a,m16]			#m18: first col: index of the partition
		m18[n,2]<- df[b,m17]			#m18: Second col: the labels of the partition
				
	}

	

	
	m19<-matrix(NA,length(count(m18[,2])$x),nrow(m18))
	
	a<-0
	
	for( j in 1:length(count(m18[,2])$x)){
		
		a<-a+1
		
		for(i in 1:nrow(m18)){
		
			if(identical(m18[i,2], count(m18[,2])$x[a])){
			
			m19[j,i]<-m18[i,1]
			
			}
			
		}
	}
	
	
	
	m20<-m19
	
	m20[is.na(m20)]<-c("")
	
	rnames<-c("P1","P2","P3","P4","P5","P6")
	
	
	m20<-as.data.frame(m20)

	cat("the partition", colnames(pf)[m16], "was replaced with partitions: " )
	
	for( i in 1:nrow(m20)){
		cat(colnames(pf)[m16],i," ")
	}
	
	#rownames of new partition
	m21<-matrix(NA,1,nrow(m19))
	
	for( i in 1:nrow(m20)){
		m21[1,i]<-paste(colnames(pf)[m16],i)
	}
	
	rownames(m20)<- m21

	temp<-t(pf[,-m16])
	
	colnames(temp)<-NULL
	
	m22<-rbind(temp,m20)
	
	m22[is.na(m22)]<-c("")
	
	m23<-noquote(m22)
	
	write.table(m23,"/Users/Dungeoun/Documents/Amit UTD Course Material/Machine Learning CS 6375 /output200.txt",quote = FALSE, col.names = FALSE)
	
}
	
