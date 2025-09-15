year=$1
day=1;
pa=$PWD
while [ $day != 26 ]; do
	cd $pa
	cd $year/day$day/
	if [ -f ./${year}.day${day}.exe ] ; then
		./${year}.day$day $year.day$day.i1.txt
	fi 
	if [ -f ./${year}.day${day}.2.exe ] ; then
		./${year}.day${day}.2 $year.day$day.i1.txt
	fi 
	day=$(($day+1));
done
#-Xss1g
