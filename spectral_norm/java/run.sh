javac spectralnormthread.java

for i in {1..20}
do
y=$((i*500))
r=$((5500 + y)) 

(time java spectralnormthread $r) 2>> output3_p.txt
done

echo "***************NON PARALLEL CODE***************"

javac spectralnorm.java

for i in {1..20}
do
y=$((i*500))
r=$((5500 + y)) 

(time java spectralnorm $r) 2>> output4.txt
done