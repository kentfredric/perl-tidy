sub pid {
    if (-l pid_filename()) {
	return readlink(pid_filename());
    }
    undef;
}
sub pid_filename {
    $bbbike_configdir . "/serverpid-" . name();
}
