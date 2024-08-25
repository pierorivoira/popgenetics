#    Function plot.post.density 

#    draws the graph of the posterior density of the migration rates γ=ΘM or M=m/μ, estimated by Peter Beerli's program MIGRATE (https://peterbeerli.com/migrate/download_version4/)
#    returns the parameter's mode

#    the user must enter the following parameters:
#    EXAMPLE
#    file_name = ~/popgenetics/data/parameter_4_μsat_16_temperatures.xlsx
#    parameter = M
#    background = dark grey
#    foreground = white
#    axis_color = white
#    graph_color = cyan
#    labes_color = white
#    max_y_limit = 0.01
#    line_width = 5
#    > plot.post.density = function(lwd,max_y_limit)                                                             

{
file_name = readline(prompt='Enter file name: ')
parameter = readline(prompt='Enter parameter (γ=ΘM or M=m/μ): ')
background = readline(prompt='Enter background color: ')
foreground = readline(prompt='Enter foreground color: ')
axis_color = readline(prompt='Enter axis color: ')
graph_color = readline(prompt='Enter graph color: ')
labes_color = readline(prompt='Enter labels color: ')
max_y_limit = readline(prompt='Enter the upper limit of the y axis: ')
line_width = readline(prompt='Enter the line width of the graph: ')
max_y_limit = as.numeric(max_y_limit)
line_width = as.numeric(line_width)
setwd('~/popgenetics/data')
working_dir = getwd()
path = paste(working_dir, file_name, sep="/")

library(readxl)
parameter_x = read_excel(path)
countx_microsat = parameter_x$count
paramx_microsat = parameter_x$param

par(bg=background, fg = foreground, col.axis = axis_color, col.lab = labes_color)

if (parameter == 'M')

plot(paramx_microsat,countx_microsat,type='l',lwd=line_width,col=graph_color,xlab='M = m/μ',ylab='posterior density',cex.lab=2,cex.axis=2,ylim=c(0,max_y_limit))

else plot(paramx_microsat,countx_microsat,type='l',lwd=line_width,col=graph_color,xlab='γ=ΘM=xNeμ*m/μ=xNem',ylab='posterior density',cex.lab=2,cex.axis=2,ylim=c(0,max_y_limit))

#    index_mode = which(countx_microsat == max_count)
#    max_count = max(countx_microsat)
mode = paramx_microsat[which(countx_microsat == max(countx_microsat))]

return(mode)

}

print(paste("The mode of the distribution is", mode))






