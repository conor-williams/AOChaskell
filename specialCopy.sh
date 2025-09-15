set -xv
year=2015
day=1

while [ $year != 2025 ] ; do
	while [ $day != 26 ] ; do
		if [ -f ALL.$year/${year}.day${day}.hs ]; then
			cp ALL.$year/${year}.day${day}.hs $year/day$day/
		fi
		if [ -f ALL.$year/${year}.day${day}.2.hs ]; then
			cp ALL.$year/${year}.day${day}.2.hs $year/day$day/
		fi
		day=$(($day+1))
        done
	day=1
        year=$(($year + 1))
done
