# This file is encoded in INFORMIX V6 ALS.
die "This file is not encoded in INFORMIX V6 ALS.\n" if q{��} ne "\x82\xa0";

use Char::INFORMIXV6ALS;
print "1..1\n";

my $__FILE__ = __FILE__;

my @getc = ();
while (my $c = Char::INFORMIXV6ALS::getc(DATA)) {
    last if $c =~ /\A[\r\n]\z/;
    push @getc, $c;
}
my $result = join('', map {"($_)"} @getc);

if ($result eq '(1)(2)(�)(�)(��)(��)') {
    print "ok - 1 $^X $__FILE__ 12������ --> $result.\n";
}
else {
    print "not ok - 1 $^X $__FILE__ 12������ --> $result.\n";
}

__END__
12������
