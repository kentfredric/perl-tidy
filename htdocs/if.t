    # An if block
    if ($PREPROCESS_IMAGES) {
	local($pre_env,$which, $done, $indic);
	while ($contents =~ /$pre_processor_env_rx/) {
	    $done .= $`; $pre_env = $5; $which =$1; $contents = $';
	    if (($which =~ /begin/)&&($pre_env =~ /indica/)) {
		if ($contents =~ s/^\[(\w+)]//o) { $done .= '#'.$1 }
	    } elsif (($which =~ /end/)&&($pre_env =~ /indica/)) {
		$done .= '#NIL';
	    } elsif (($which =~ /begin/)&&($pre_env =~ /itrans/)) {
		if ($contents =~ s/^\[(\w+)]/$indic=$1;''/e)
	            { $done .= "\#$indic" }
	    } elsif (($which =~ /end/)&&($pre_env =~ /itrans/)) {
		$done .= "\#end$indic";
	    } elsif ($which =~ /begin/) {
		$done .= (($which =~ /end/)? $end_preprocessor{$pre_env}
		          : $begin_preprocessor{$pre_env} )
	    }
	}
	$contents = $done . $contents;
    }
