# This file is encoded in INFORMIX V6 ALS.
die "This file is not encoded in INFORMIX V6 ALS.\n" if q{��} ne "\x82\xa0";

use strict;
use Char::INFORMIXV6ALS;
print "1..1\n";

my $__FILE__ = __FILE__;

my $var1 = 'ABCDEFGH';
my $re = 'dEf';
if ($var1 =~ /C($re)g/i) {
    print qq{ok - 1 \$var1=~/C(\$re)g/i, \$1=($1) $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 \$var1=~/C(\$re)g/i, \$1=($1) $^X $__FILE__\n};
}

__END__
