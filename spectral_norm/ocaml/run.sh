ocamlopt -o sn  sn.ml

for i in {1..20}
do
y=$((i*500))
r=$((5500 + y)) 

(time ./sn $r) 2>> output1_p.txt
done

echo "***************NON PARALLEL CODE***************"

ocamlopt -o sn_seq sn_seq.ml

for i in {1..20}
do
y=$((i*500))
r=$((5500 + y)) 

(time ./sn_seq $r) 2>> output2.txt
done