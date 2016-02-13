rm $1\time.txt;
for s in 10000
do
	rm $1$s.txt;
	for i in `seq 100`; do /usr/bin/time  -ao $1$s.txt -f "%U"  yap -l $1 -g "e1_lw($s)"  ; done
	echo -n "q4,$s," >> $1\time.txt
	awk '{ total += $1; count++ } END { print total/count }' $1$s.txt >> $1\time.txt
done
for s in 10000
do
	rm $1$s.txt;
	for i in `seq 100`; do /usr/bin/time  -ao $1$s.txt -f "%U"  yap -l $1 -g "e2_lw($s)"  ; done
	echo -n "q3.9,$s," >> $1\time.txt
	awk '{ total += $1; count++ } END { print total/count }' $1$s.txt >> $1\time.txt
done
