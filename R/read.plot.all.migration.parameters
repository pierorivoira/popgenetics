read.plot.all.migration.parameters = function()
{
library(openxlsx)

firstrawpar = NULL
lastrawpar = NULL
rows = NULL
migration.parameter = NULL
param_microsat = NULL
count_microsat = NULL
graph_color = NULL

file_name = readline(prompt='Please enter file name: ')
extension = readline(prompt='Please enter file extension: ')
parameter = readline(prompt='Please enter parameter (γ=ΘM or M=m/μ): ')
nloci = as.numeric(readline(prompt='Please enter the number of loci: '))
nbins = as.numeric(readline(prompt='Please enter the number of bins of the posterior distribution: '))
npopulations = as.numeric(readline(prompt='Please enter the number of populations: '))
background = readline(prompt='Please enter background color: ')
foreground = readline(prompt='Please enter foreground color: ')
axis_color = readline(prompt='Please enter axis color: ')
first_graph_color = readline(prompt='Please enter first graph color: ')
nparameters = npopulations^2

for(i in 1:(npopulations+2))
{
graph_color[i] = readline(prompt='Please enter graph color: ')
}

labes_color = readline(prompt='Please enter labels color: ')
max_y_limit = readline(prompt='Please enter the upper limit of the y axis: ')
line_width = readline(prompt='Please enter the line width of the graph: ')
max_y_limit = as.numeric(max_y_limit)
line_width = as.numeric(line_width)

skipraws = nparameters+14
skipraws=skipraws+(nloci*nparameters*(nbins+1))+3*(nbins+1)
firstrawpar4=skipraws+1
# la prima riga dell’area contenente il parametro 4 (locus 15) è 193653
# quante righe devo saltare per arrivare lì?

lastrawpar4=firstrawpar4+(nbins-1)

rows = as.numeric(c(firstrawpar4:lastrawpar4))
cols = c(5,6)

setwd('~/popgenetics/data')
working_dir = getwd()
path = paste(working_dir, file_name, sep='/')
path = paste(path, '.', extension, sep='')
first.migration.parameter=read.xlsx(xlsxFile=path,sheet = 1,rows=rows,cols=cols,colNames = FALSE)

paramfirst_microsat = first.migration.parameter$X1
countfirst_microsat = first.migration.parameter$X2

par(bg=background, fg = foreground, col.axis = axis_color, col.lab = labes_color)

if (parameter == 'M')

plot(paramfirst_microsat,countfirst_microsat,type='l',lwd=line_width,col=first_graph_color,xlab='M = m/μ',ylab='posterior density',cex.lab=2,cex.axis=2,ylim=c(0,max_y_limit))

else plot(paramfirst_microsat,countfirst_microsat,type='l',lwd=line_width,col=first_graph_color,xlab='γ=ΘM=xNeμ*m/μ=xNem',ylab='posterior density',cex.lab=2,cex.axis=2,ylim=c(0,max_y_limit))

for(i in 1:(npopulations+2))
{
firstrawpar[i] = firstrawpar4 + (nbins+1)
firstrawpar4 = firstrawpar4 + (nbins+1)
}

lastrawpar = firstrawpar + (nbins-1)

rows5 = as.numeric(c(firstrawpar[1]:lastrawpar[1]))
rows6 = rows5 + (nbins+1)
rows7 = rows6 + (nbins+1)
rows8 = rows7 + (nbins+1)
rows9 = rows8 + (nbins+1)

migration.parameter5 = read.xlsx(xlsxFile=path,sheet = 1,rows=rows5,cols=cols,colNames = FALSE)
migration.parameter6 = read.xlsx(xlsxFile=path,sheet = 1,rows=rows6,cols=cols,colNames = FALSE)
migration.parameter7 = read.xlsx(xlsxFile=path,sheet = 1,rows=rows7,cols=cols,colNames = FALSE)
migration.parameter8 = read.xlsx(xlsxFile=path,sheet = 1,rows=rows8,cols=cols,colNames = FALSE)
migration.parameter9 = read.xlsx(xlsxFile=path,sheet = 1,rows=rows9,cols=cols,colNames = FALSE)

param_microsat5 = migration.parameter5$X1
param_microsat6 = migration.parameter6$X1
param_microsat7 = migration.parameter7$X1
param_microsat8 = migration.parameter8$X1
param_microsat9 = migration.parameter9$X1

count_microsat5 = migration.parameter5$X2
count_microsat6 = migration.parameter6$X2
count_microsat7 = migration.parameter7$X2
count_microsat8 = migration.parameter8$X2
count_microsat9 = migration.parameter9$X2

lines(param_microsat5,count_microsat5,type='l',lwd=line_width,col=(graph_color[1]),xlab='M = m/μ',ylab='posterior density',cex.lab=2,cex.axis=2,ylim=c(0,max_y_limit))
lines(param_microsat6,count_microsat6,type='l',lwd=line_width,col=(graph_color[2]),xlab='M = m/μ',ylab='posterior density',cex.lab=2,cex.axis=2,ylim=c(0,max_y_limit))
lines(param_microsat7,count_microsat7,type='l',lwd=line_width,col=(graph_color[3]),xlab='M = m/μ',ylab='posterior density',cex.lab=2,cex.axis=2,ylim=c(0,max_y_limit))
lines(param_microsat8,count_microsat8,type='l',lwd=line_width,col=(graph_color[4]),xlab='M = m/μ',ylab='posterior density',cex.lab=2,cex.axis=2,ylim=c(0,max_y_limit))
lines(param_microsat9,count_microsat9,type='l',lwd=line_width,col=(graph_color[5]),xlab='M = m/μ',ylab='posterior density',cex.lab=2,cex.axis=2,ylim=c(0,max_y_limit))

legend(400, 0.02, legend=c('France-CH → Piedmont', 'Valais → Piedmont', 'Piedmont → France-CH', 'Valais → France-CH', 'Piedmont → Valais', 'France-CH → Valais'), col=c('cyan', 'red', 'pink', 'white', 'green', 'blue'), lwd=5, lty=2:3, cex=2)


mode4 = paramfirst_microsat[which(countfirst_microsat == max(countfirst_microsat))]
mode5 = param_microsat5[which(count_microsat5 == max(count_microsat5))]
mode6 = param_microsat6[which(count_microsat6 == max(count_microsat6))]
mode7 = param_microsat7[which(count_microsat7 == max(count_microsat7))]
mode8 = param_microsat8[which(count_microsat8 == max(count_microsat8))]
mode9 = param_microsat9[which(count_microsat9 == max(count_microsat9))]

modes = c(mode4, mode5, mode6, mode7, mode8, mode9)

#result = str(first.migration.parameter)

#return(parameter)
#return(result)
return(modes)

#print(paste('The kind of the first migration parameter is', parameter))
#print(paste('The structure of the first migration parameter is', result))
print(paste('The modes of the distribution are', modes))

}
