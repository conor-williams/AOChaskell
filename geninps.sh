rm inp.tar 
rm inp.tar.gz
find 20* -name \*i1.txt -o -name i2.txt -o -name i3.txt -o -name \*ex\*.txt | xargs tar -rvf inp.tar
gzip inp.tar
cp inp.tar.gz input-2015-2024.tar.gz
echo "move to input-2015-2024.tar.gz"
