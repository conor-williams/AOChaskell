
set -xv
day=1
year=2015
curdir=$PWD
echo $curdir
while [ $year != 2025 ];  do
	while [ $day != 26 ]; do
		if [ -d $curdir/$year/day$day ]; then
			cd $curdir/$year/day$day
			if [ ! -f ${year}.day$day.exe ] ; then
				if [ -f ${year}.day$day.hs ]; then
					$GHC -package mtl ${year}.day$day.hs 
				fi
			fi
			if [ $day != 25 ]; then
				if [ ! -f ${year}.day${day}.2.exe ]; then
					if [ -f ${year}.day${day}.2.hs ]; then
						$GHC -package mtl ${year}.day${day}.2.hs
					fi
				fi
			fi
		fi
		day=$(( day + 1))
	done
	day=1
	year=$((year + 1))
done
