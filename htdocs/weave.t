# literate programming example from weave.pl (which was created from weave.web)
# see http://users.ox.ac.uk/~pemb0468/webperl/
# 31
elsif (/\|/) {            if ($nextchar eq "\|")  {		$_=$formatter{"or"} ;            } else {		    $_="\$\\|\$"; }        } elsif (/_/) {            $_="\\_{}";        } elsif (/\\\\/) {            $_="\\";        } elsif (/\$/) {           $_="\\\$";        } elsif (/\^/) {           $_="\\\^{}";        } elsif (/<|>/) {           $_="\$$_\$";        } elsif (/%/) {           $_="\\%";        } elsif ($_ eq $hash) {            $_="\\$hash";}	     

