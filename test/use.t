# previously this caused an incorrect error message after '2.42'
use lib "$Common::global::gInstallRoot/lib";
use CGI 2.42 qw(fatalsToBrowser);
use RRDs 1.000101;

# Constant.pm
# the 0666 must expect an operator
use constant MODE           => do { 0666 & (0777 & ~umask) };


# From Safe.pm caused trouble with extrude
use Opcode 1.01, qw(
    opset opset_to_ops opmask_add
    empty_opset full_opset invert_opset verify_opset
    opdesc opcodes opmask define_optag opset_to_hex
);
