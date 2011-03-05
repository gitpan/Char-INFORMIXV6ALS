# This file is encoded in INFORMIX V6 ALS.
die "This file is not encoded in INFORMIX V6 ALS.\n" if q{あ} ne "\x82\xa0";

# 引数に _ が指定された場合のテスト

my $__FILE__ = __FILE__;

use Char::Einformixv6als;
print "1..23\n";

if ($^O !~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    for my $tno (1..23) {
        print "ok - $tno # SKIP $^X $0\n";
    }
    exit;
}

open(FILE,'>file');
close(FILE);

open(FILE,'file');

if (-r ('file')) {
    if (Char::Einformixv6als::r(_)) {
        print "ok - 1 Char::Einformixv6als::r _ == -r _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 1 Char::Einformixv6als::r _ == -r _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::r(_)) {
        print "not ok - 1 Char::Einformixv6als::r _ == -r _ $^X $__FILE__\n";
    }
    else {
        print "ok - 1 Char::Einformixv6als::r _ == -r _ $^X $__FILE__\n";
    }
}

if (-w ('file')) {
    if (Char::Einformixv6als::w(_)) {
        print "ok - 2 Char::Einformixv6als::w _ == -w _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 2 Char::Einformixv6als::w _ == -w _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::w(_)) {
        print "not ok - 2 Char::Einformixv6als::w _ == -w _ $^X $__FILE__\n";
    }
    else {
        print "ok - 2 Char::Einformixv6als::w _ == -w _ $^X $__FILE__\n";
    }
}

if (-x ('file')) {
    if (Char::Einformixv6als::x(_)) {
        print "ok - 3 Char::Einformixv6als::x _ == -x _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 3 Char::Einformixv6als::x _ == -x _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::x(_)) {
        print "not ok - 3 Char::Einformixv6als::x _ == -x _ $^X $__FILE__\n";
    }
    else {
        print "ok - 3 Char::Einformixv6als::x _ == -x _ $^X $__FILE__\n";
    }
}

if (-o ('file')) {
    if (Char::Einformixv6als::o(_)) {
        print "ok - 4 Char::Einformixv6als::o _ == -o _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 4 Char::Einformixv6als::o _ == -o _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::o(_)) {
        print "not ok - 4 Char::Einformixv6als::o _ == -o _ $^X $__FILE__\n";
    }
    else {
        print "ok - 4 Char::Einformixv6als::o _ == -o _ $^X $__FILE__\n";
    }
}

if (-R ('file')) {
    if (Char::Einformixv6als::R(_)) {
        print "ok - 5 Char::Einformixv6als::R _ == -R _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 5 Char::Einformixv6als::R _ == -R _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::R(_)) {
        print "not ok - 5 Char::Einformixv6als::R _ == -R _ $^X $__FILE__\n";
    }
    else {
        print "ok - 5 Char::Einformixv6als::R _ == -R _ $^X $__FILE__\n";
    }
}

if (-W ('file')) {
    if (Char::Einformixv6als::W(_)) {
        print "ok - 6 Char::Einformixv6als::W _ == -W _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 6 Char::Einformixv6als::W _ == -W _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::W(_)) {
        print "not ok - 6 Char::Einformixv6als::W _ == -W _ $^X $__FILE__\n";
    }
    else {
        print "ok - 6 Char::Einformixv6als::W _ == -W _ $^X $__FILE__\n";
    }
}

if (-X ('file')) {
    if (Char::Einformixv6als::X(_)) {
        print "ok - 7 Char::Einformixv6als::X _ == -X _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 7 Char::Einformixv6als::X _ == -X _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::X(_)) {
        print "not ok - 7 Char::Einformixv6als::X _ == -X _ $^X $__FILE__\n";
    }
    else {
        print "ok - 7 Char::Einformixv6als::X _ == -X _ $^X $__FILE__\n";
    }
}

if (-O ('file')) {
    if (Char::Einformixv6als::O(_)) {
        print "ok - 8 Char::Einformixv6als::O _ == -O _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 8 Char::Einformixv6als::O _ == -O _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::O(_)) {
        print "not ok - 8 Char::Einformixv6als::O _ == -O _ $^X $__FILE__\n";
    }
    else {
        print "ok - 8 Char::Einformixv6als::O _ == -O _ $^X $__FILE__\n";
    }
}

if (-e ('file')) {
    if (Char::Einformixv6als::e(_)) {
        print "ok - 9 Char::Einformixv6als::e _ == -e _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 9 Char::Einformixv6als::e _ == -e _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::e(_)) {
        print "not ok - 9 Char::Einformixv6als::e _ == -e _ $^X $__FILE__\n";
    }
    else {
        print "ok - 9 Char::Einformixv6als::e _ == -e _ $^X $__FILE__\n";
    }
}

if (-z ('file')) {
    if (Char::Einformixv6als::z(_)) {
        print "ok - 10 Char::Einformixv6als::z _ == -z _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 10 Char::Einformixv6als::z _ == -z _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::z(_)) {
        print "not ok - 10 Char::Einformixv6als::z _ == -z _ $^X $__FILE__\n";
    }
    else {
        print "ok - 10 Char::Einformixv6als::z _ == -z _ $^X $__FILE__\n";
    }
}

$_ = -s 'file';
if (Char::Einformixv6als::s(_) == $_) {
    print "ok - 11 Char::Einformixv6als::s _ (@{[Char::Einformixv6als::s _]}) == -s 'file' ($_) $^X $__FILE__\n";
}
else {
    print "not ok - 11 Char::Einformixv6als::s _ (@{[Char::Einformixv6als::s _]}) == -s 'file' ($_) $^X $__FILE__\n";
}

if (-f ('file')) {
    if (Char::Einformixv6als::f(_)) {
        print "ok - 12 Char::Einformixv6als::f _ == -f _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 12 Char::Einformixv6als::f _ == -f _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::f(_)) {
        print "not ok - 12 Char::Einformixv6als::f _ == -f _ $^X $__FILE__\n";
    }
    else {
        print "ok - 12 Char::Einformixv6als::f _ == -f _ $^X $__FILE__\n";
    }
}

if (-d ('file')) {
    if (Char::Einformixv6als::d(_)) {
        print "ok - 13 Char::Einformixv6als::d _ == -d _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 13 Char::Einformixv6als::d _ == -d _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::d(_)) {
        print "not ok - 13 Char::Einformixv6als::d _ == -d _ $^X $__FILE__\n";
    }
    else {
        print "ok - 13 Char::Einformixv6als::d _ == -d _ $^X $__FILE__\n";
    }
}

if (-p ('file')) {
    if (Char::Einformixv6als::p(_)) {
        print "ok - 14 Char::Einformixv6als::p _ == -p _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 14 Char::Einformixv6als::p _ == -p _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::p(_)) {
        print "not ok - 14 Char::Einformixv6als::p _ == -p _ $^X $__FILE__\n";
    }
    else {
        print "ok - 14 Char::Einformixv6als::p _ == -p _ $^X $__FILE__\n";
    }
}

if (-S ('file')) {
    if (Char::Einformixv6als::S(_)) {
        print "ok - 15 Char::Einformixv6als::S _ == -S _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 15 Char::Einformixv6als::S _ == -S _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::S(_)) {
        print "not ok - 15 Char::Einformixv6als::S _ == -S _ $^X $__FILE__\n";
    }
    else {
        print "ok - 15 Char::Einformixv6als::S _ == -S _ $^X $__FILE__\n";
    }
}

if (-b ('file')) {
    if (Char::Einformixv6als::b(_)) {
        print "ok - 16 Char::Einformixv6als::b _ == -b _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 16 Char::Einformixv6als::b _ == -b _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::b(_)) {
        print "not ok - 16 Char::Einformixv6als::b _ == -b _ $^X $__FILE__\n";
    }
    else {
        print "ok - 16 Char::Einformixv6als::b _ == -b _ $^X $__FILE__\n";
    }
}

if (-c ('file')) {
    if (Char::Einformixv6als::c(_)) {
        print "ok - 17 Char::Einformixv6als::c _ == -c _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 17 Char::Einformixv6als::c _ == -c _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::c(_)) {
        print "not ok - 17 Char::Einformixv6als::c _ == -c _ $^X $__FILE__\n";
    }
    else {
        print "ok - 17 Char::Einformixv6als::c _ == -c _ $^X $__FILE__\n";
    }
}

if (-u ('file')) {
    if (Char::Einformixv6als::u(_)) {
        print "ok - 18 Char::Einformixv6als::u _ == -u _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 18 Char::Einformixv6als::u _ == -u _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::u(_)) {
        print "not ok - 18 Char::Einformixv6als::u _ == -u _ $^X $__FILE__\n";
    }
    else {
        print "ok - 18 Char::Einformixv6als::u _ == -u _ $^X $__FILE__\n";
    }
}

if (-g ('file')) {
    if (Char::Einformixv6als::g(_)) {
        print "ok - 19 Char::Einformixv6als::g _ == -g _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 19 Char::Einformixv6als::g _ == -g _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::g(_)) {
        print "not ok - 19 Char::Einformixv6als::g _ == -g _ $^X $__FILE__\n";
    }
    else {
        print "ok - 19 Char::Einformixv6als::g _ == -g _ $^X $__FILE__\n";
    }
}

if (-k ('file')) {
    if (Char::Einformixv6als::k(_)) {
        print "ok - 20 Char::Einformixv6als::k _ == -k _ $^X $__FILE__\n";
    }
    else {
        print "not ok - 20 Char::Einformixv6als::k _ == -k _ $^X $__FILE__\n";
    }
}
else {
    if (Char::Einformixv6als::k(_)) {
        print "not ok - 20 Char::Einformixv6als::k _ == -k _ $^X $__FILE__\n";
    }
    else {
        print "ok - 20 Char::Einformixv6als::k _ == -k _ $^X $__FILE__\n";
    }
}

$_ = -M 'file';
if (Char::Einformixv6als::M(_) == $_) {
    print "ok - 21 Char::Einformixv6als::M _ (@{[Char::Einformixv6als::M _]}) == -M 'file' ($_) $^X $__FILE__\n";
}
else {
    print "not ok - 21 Char::Einformixv6als::M _ (@{[Char::Einformixv6als::M _]}) == -M 'file' ($_) $^X $__FILE__\n";
}

$_ = -A 'file';
if (Char::Einformixv6als::A(_) == $_) {
    print "ok - 22 Char::Einformixv6als::A _ (@{[Char::Einformixv6als::A _]}) == -A 'file' ($_) $^X $__FILE__\n";
}
else {
    print "not ok - 22 Char::Einformixv6als::A _ (@{[Char::Einformixv6als::A _]}) == -A 'file' ($_) $^X $__FILE__\n";
}

$_ = -C 'file';
if (Char::Einformixv6als::C(_) == $_) {
    print "ok - 23 Char::Einformixv6als::C _ (@{[Char::Einformixv6als::C _]}) == -C 'file' ($_) $^X $__FILE__\n";
}
else {
    print "not ok - 23 Char::Einformixv6als::C _ (@{[Char::Einformixv6als::C _]}) == -C 'file' ($_) $^X $__FILE__\n";
}

close(FILE);
unlink('file');

__END__
