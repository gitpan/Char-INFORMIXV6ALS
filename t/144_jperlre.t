# This file is encoded in INFORMIX V6 ALS.
die "This file is not encoded in INFORMIX V6 ALS.\n" if q{あ} ne "\x82\xa0";

use Char::INFORMIXV6ALS;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('あいえ' =~ /(あ[い-え]e)/) {
    print "not ok - 1 $^X $__FILE__ not ('あいえ' =~ /あ[い-え]e/).\n";
}
else {
    print "ok - 1 $^X $__FILE__ not ('あいえ' =~ /あ[い-え]e/).\n";
}

__END__
