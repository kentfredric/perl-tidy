# Entry by Logan Hanks in 5th Annual Obfuscated Perl Contest
# p=paper, r=rock, s=scissors, q=quit
$.=$|=1;%a=(p=>0,r=>1,s=>2);$r='Round ';$s=': [p/r/s/q]? ';@b=(Paper,Rock,
Scissors);@c=(2,0,1,1,2,0,0,1,2);@d=(You,Computer,Draw);@e=(0,0,0);$p=int(9*
rand);print"$r$.$s";while(<>){$y=$h[$p]?($h[$p]+1)%3:int(3*rand);if($_=~/
^[prs]/x){$x=$a{$&};$h[$p]=$x+1;$p=3*$x+$y;$e[$c[$p]]++;print"$r$.: ".
"$d[$[]: $b[$x], $d[1]: $b[$y]\n";print"$r$. Winner: $d[$c[$p]]\n";print"$r".
"$. Score:";foreach$i(0..$#d){print" $d[$i]: $e[$i] ";}print"\n\n";}elsif($_
=~/^q/i){exit}else{$.--;}$.++;print"$r$.$s";$.--;}
