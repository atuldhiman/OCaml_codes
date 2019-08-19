javac QuickSorter_par.java

r=$((1000000))

for i in {1..10}
do
# y=$((i*500))
# r=$((r + 1000000)) 

(time java QuickSorter_par $r) 2>> output3_p.txt
r=$((r + 1000000))
done

echo "***************NON PARALLEL CODE***************"

javac QuickSorter_seq.java

r=$((1000000))

for i in {1..10}
do
# y=$((i*500))
# r=$((r + 1000000))

(time java QuickSorter_seq $r) 2>> output4.txt
r=$((r + 1000000))
done