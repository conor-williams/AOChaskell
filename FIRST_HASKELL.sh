export GHC=/cygdrive/f/ghc-9.10.2-x86_64-unknown-mingw32/bin/ghc-9.10.2.exe

if [ ! -f $GHC ]; then
	echo "set GHC in FIRST_HASKELL.sh";
	exit 0;
else 
	echo "GHC ok"
fi

### a guide to running...
echo "tar of input-2015-2014.tar.gz takes ~10seconds"
tar -xvzf input-2015-2024.tar.gz

echo "sh specialCopy.sh takes ~1minute"
sh specialCopy.sh

echo "sh specialCompile.sh takes ~11minutes"

sh specialCompile.sh
#mv 2019/day25/*.exe 2019/day25/2019.day25.INTERACTIVE.exe

sh runALL.sh

