BernoulliRandom = function( num_trials )
{
	rand_prob = (sample(1:10,1)/10)
	trials = rbinom( num_trials, 1, rand_prob )
	successes = sum( trials == 1 )
	print("Your random probability:")
	print(rand_prob)
	print("number of successes")
	print(successes)
	print(trials)
}