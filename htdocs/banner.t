#!/usr/bin/perl -l

$A=25755;$E=       71317;$H=  55755;$J=74452;    $L=
11117  ;$N=25555;  $O=        25552  ;$P=35311;  $R=
35355  ;$T=72222;  $U=        55557 ;for(split   //,
uc<>){@b=split     //,$$_;    for(0..4){$c       =$b
[$_];              $t[        $_].=" "x4;while   ($c
){$d=              int        log(     $c)/log   2;$c
-=2**              $d;substr  ($t[     $_],$d+   $p,1)="@"}
                                                 }$p+=4}for
                                                 (@t){print}

