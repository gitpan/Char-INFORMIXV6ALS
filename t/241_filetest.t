# This file is encoded in INFORMIX V6 ALS.
die "This file is not encoded in INFORMIX V6 ALS.\n" if q{‚ } ne "\x82\xa0";

# ˆø”‚ªÈ—ª‚³‚ê‚½ê‡‚ÌƒeƒXƒg

my $__FILE__ = __FILE__;

use Char::Einformixv6als;
print "1..25\n";

if ($^O !~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    for my $tno (1..25) {
        print "ok - $tno # SKIP $^X $0\n";
    }
    exit;
}

open(FILE,'>file');
close(FILE);

open(FILE,'file');

$_ = 'file';
if ((Char::Einformixv6als::r_ ne '') == (-r ne '')) {
    print "ok - 1 Char::Einformixv6als::r_ == -r  $^X $__FILE__\n";
}
else {
    print "not ok - 1 Char::Einformixv6als::r_ == -r  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::w_ ne '') == (-w ne '')) {
    print "ok - 2 Char::Einformixv6als::w_ == -w  $^X $__FILE__\n";
}
else {
    print "not ok - 2 Char::Einformixv6als::w_ == -w  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::x_ ne '') == (-x ne '')) {
    print "ok - 3 Char::Einformixv6als::x_ == -x  $^X $__FILE__\n";
}
else {
    print "not ok - 3 Char::Einformixv6als::x_ == -x  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::o_ ne '') == (-o ne '')) {
    print "ok - 4 Char::Einformixv6als::o_ == -o  $^X $__FILE__\n";
}
else {
    print "not ok - 4 Char::Einformixv6als::o_ == -o  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::R_ ne '') == (-R ne '')) {
    print "ok - 5 Char::Einformixv6als::R_ == -R  $^X $__FILE__\n";
}
else {
    print "not ok - 5 Char::Einformixv6als::R_ == -R  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::W_ ne '') == (-W ne '')) {
    print "ok - 6 Char::Einformixv6als::W_ == -W  $^X $__FILE__\n";
}
else {
    print "not ok - 6 Char::Einformixv6als::W_ == -W  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::X_ ne '') == (-X ne '')) {
    print "ok - 7 Char::Einformixv6als::X_ == -X  $^X $__FILE__\n";
}
else {
    print "not ok - 7 Char::Einformixv6als::X_ == -X  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::O_ ne '') == (-O ne '')) {
    print "ok - 8 Char::Einformixv6als::O_ == -O  $^X $__FILE__\n";
}
else {
    print "not ok - 8 Char::Einformixv6als::O_ == -O  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::e_ ne '') == (-e ne '')) {
    print "ok - 9 Char::Einformixv6als::e_ == -e  $^X $__FILE__\n";
}
else {
    print "not ok - 9 Char::Einformixv6als::e_ == -e  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::z_ ne '') == (-z ne '')) {
    print "ok - 10 Char::Einformixv6als::z_ == -z  $^X $__FILE__\n";
}
else {
    print "not ok - 10 Char::Einformixv6als::z_ == -z  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::s_ ne '') == (-s ne '')) {
    print "ok - 11 Char::Einformixv6als::s_ == -s  $^X $__FILE__\n";
}
else {
    print "not ok - 11 Char::Einformixv6als::s_ == -s  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::f_ ne '') == (-f ne '')) {
    print "ok - 12 Char::Einformixv6als::f_ == -f  $^X $__FILE__\n";
}
else {
    print "not ok - 12 Char::Einformixv6als::f_ == -f  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::d_ ne '') == (-d ne '')) {
    print "ok - 13 Char::Einformixv6als::d_ == -d  $^X $__FILE__\n";
}
else {
    print "not ok - 13 Char::Einformixv6als::d_ == -d  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::p_ ne '') == (-p ne '')) {
    print "ok - 14 Char::Einformixv6als::p_ == -p  $^X $__FILE__\n";
}
else {
    print "not ok - 14 Char::Einformixv6als::p_ == -p  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::S_ ne '') == (-S ne '')) {
    print "ok - 15 Char::Einformixv6als::S_ == -S  $^X $__FILE__\n";
}
else {
    print "not ok - 15 Char::Einformixv6als::S_ == -S  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::b_ ne '') == (-b ne '')) {
    print "ok - 16 Char::Einformixv6als::b_ == -b  $^X $__FILE__\n";
}
else {
    print "not ok - 16 Char::Einformixv6als::b_ == -b  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::c_ ne '') == (-c ne '')) {
    print "ok - 17 Char::Einformixv6als::c_ == -c  $^X $__FILE__\n";
}
else {
    print "not ok - 17 Char::Einformixv6als::c_ == -c  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::u_ ne '') == (-u ne '')) {
    print "ok - 18 Char::Einformixv6als::u_ == -u  $^X $__FILE__\n";
}
else {
    print "not ok - 18 Char::Einformixv6als::u_ == -u  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::g_ ne '') == (-g ne '')) {
    print "ok - 19 Char::Einformixv6als::g_ == -g  $^X $__FILE__\n";
}
else {
    print "not ok - 19 Char::Einformixv6als::g_ == -g  $^X $__FILE__\n";
}

local $^W = 0;
$_ = 'file';
if ((Char::Einformixv6als::k_ ne '') == (-k ne '')) {
    print "ok - 20 Char::Einformixv6als::k_ == -k  $^X $__FILE__\n";
}
else {
    print "not ok - 20 Char::Einformixv6als::k_ == -k  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::T_ ne '') == (-T ne '')) {
    print "ok - 21 Char::Einformixv6als::T_ == -T  $^X $__FILE__\n";
}
else {
    print "not ok - 21 Char::Einformixv6als::T_ == -T  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::B_ ne '') == (-B ne '')) {
    print "ok - 22 Char::Einformixv6als::B_ == -B  $^X $__FILE__\n";
}
else {
    print "not ok - 22 Char::Einformixv6als::B_ == -B  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::M_ ne '') == (-M ne '')) {
    print "ok - 23 Char::Einformixv6als::M_ == -M  $^X $__FILE__\n";
}
else {
    print "not ok - 23 Char::Einformixv6als::M_ == -M  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::A_ ne '') == (-A ne '')) {
    print "ok - 24 Char::Einformixv6als::A_ == -A  $^X $__FILE__\n";
}
else {
    print "not ok - 24 Char::Einformixv6als::A_ == -A  $^X $__FILE__\n";
}

$_ = 'file';
if ((Char::Einformixv6als::C_ ne '') == (-C ne '')) {
    print "ok - 25 Char::Einformixv6als::C_ == -C  $^X $__FILE__\n";
}
else {
    print "not ok - 25 Char::Einformixv6als::C_ == -C  $^X $__FILE__\n";
}

close(FILE);
unlink('file');

__END__
