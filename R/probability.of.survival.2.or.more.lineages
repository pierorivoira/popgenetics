probability.of.survival.2.or.more.lineages = function()
{
p = NULL
q = NULL
gamma = NULL
G = as.numeric(readline(prompt='Please enter the number of generations: '))
n = as.numeric(readline(prompt='Please enter the number of founders: '))
lambda = as.numeric(readline(prompt='Please enter the average number of daughters per female: '))
for(i in 1:G)
{
if (i == 1) p[i] = exp(-lambda)
else p[i] = exp(lambda*(p[i-1]-1))
q[i] = 1 - p[i]
gamma[i] = 1 - p[i]^n - n*p[i]^(n-1)*q[i]
}
return(gamma[G])
}

