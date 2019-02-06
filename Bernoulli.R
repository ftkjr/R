Bernoulli = function( num_trials, p_probability )
{
	trials = rbinom( num_trials, 1, p_probability )
	successes = sum( trials == 1 ) 
	print("number of successes")
	print(successes)
	print(trials)
}