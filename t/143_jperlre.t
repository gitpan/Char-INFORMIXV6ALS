# This file is encoded in INFORMIX V6 ALS.
die "This file is not encoded in INFORMIX V6 ALS.\n" if q{あ} ne "\x82\xa0";

use Char::INFORMIXV6ALS;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('あいえ' =~ /(あ[いう]え)/) {
    if ("$1" eq "あいえ") {
        print "ok - 1 $^X $__FILE__ ('あいえ' =~ /あ[いう]え/).\n";
    }
    else {
        print "not ok - 1 $^X $__FILE__ ('あいえ' =~ /あ[いう]え/).\n";
    }
}
else {
    print "not ok - 1 $^X $__FILE__ ('あいえ' =~ /あ[いう]え/).\n";
}

__END__
