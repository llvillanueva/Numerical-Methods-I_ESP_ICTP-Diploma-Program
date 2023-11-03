set term png
set output "rejection.png"
# Define the number of bins and the bin width
num_bins = 10000.0
bin_width = 1.0 / num_bins

# Define the range of the x-axis
xmin = 0
xmax = 1

# Create the histogram
bin(x) = floor((x - xmin) / bin_width)
plot "rejection.txt" using (bin($1)*bin_width+xmin):(1.0) smooth frequency with boxes notitle


# plot.plt
set term png
set output "pi.png"
set title "1000 couples (Fortran inbuilt Function)"
set xlabel "x"
set ylabel "y"
plot 'pi.txt' with line notitle
