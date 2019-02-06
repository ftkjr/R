cointossesheads= function(num_tosses,probability_heads)
{
	trials = rbinom( num_tosses , 1 , probability_heads )
	num_heads = sum(trials == 1)
	print("number of heads")	
	print(num_heads)
	tosses <- c(trials)
	tosses[tosses==0]<-"T"
	tosses[tosses==1]<-"H"
	
print(tosses)
}