ocamlopt -o rec_qs rec_qs.ml

r=$((1000000))

for i in {1..10}
do
# y=$((i*500))
 


(time ./rec_qs $r) 2>> output2.txt
r=$((r + 1000000))
done

echo "*************** PARALLEL CODE***************"

ocamlopt -o  q_arr   q_arr_para_opt.ml

r=$((1000000))

for i in {1..10}
do
# y=$((i*500))
# r=$((r + 1000000))

(time ./q_arr $r) 2>> output1_p.txt
r=$((r + 1000000))
done

