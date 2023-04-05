raco pict -o t.png ht-append img/*$1*overhead*png > /dev/null
raco pict -o b.png ht-append img/*$1*steps*png > /dev/null
raco pict -o img/$1-full.png vc-append 80 t.png b.png
rm t.png b.png
tar -czf $1.tar.gz img/$1-full.png img/$1*rktd
mv $1.tar.gz ~/Downloads/.
