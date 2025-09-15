year=2015
if [ '@'$1 = '@' ]; then
	echo "give a day"
	exit
fi
day=$1
pa=$PWD
while [ $year != 2025 ]; do
	cd $year/day$day
	./${year}.day${day}.exe *i1.txt
	if [ @$day != '@25' ]; then
		./${year}.day${day}.2.exe *i1.txt
	fi
	year=$((year+1))
	cd $pa
done
