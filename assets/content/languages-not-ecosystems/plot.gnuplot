set terminal svg size 400,300 enhanced fname 'arial'  fsize 10 butt solid
set output 'out.svg'
set term svg font "Times,14"
set xrange [1:10]
set yrange [4:35]
set xlabel 'Time (months)'
set ylabel 'Dependencies'
set title 'Dependencies over time'
unset ytics

set obj rect from 1, graph 0 to 3, graph 1 fc rgb "orange" fs solid 0.2
set label 8 at 2,32 "Early Phase" front center font ",10
set obj rect from 3, graph 0 to 10, graph 1 fc rgb "blue" fs solid 0.08
set label 9 at 6.5,32 "Late Phase" front center font ",10"

plot 33.57729 + (3.781509 - 33.57729)/(1 + (x/2.096745)**6.484434) notitle
