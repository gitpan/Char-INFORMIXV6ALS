# This file is encoded in INFORMIX V6 ALS.
die "This file is not encoded in INFORMIX V6 ALS.\n" if q{あ} ne "\x82\xa0";

use Char::INFORMIXV6ALS;

print "1..12\n";

# eval <<END has eval "..."
if (eval Char::INFORMIXV6ALS::escape <<END) {
eval Char::INFORMIXV6ALS::escape " if ('アソ' !~ /A/) { return 1 } else { return 0 } "
END
    print qq{ok - 1 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 1 $^X @{[__FILE__]}\n};
}

# eval <<END has eval qq{...}
if (eval Char::INFORMIXV6ALS::escape <<END) {
eval Char::INFORMIXV6ALS::escape qq{ if ('アソ' !~ /A/) { return 1 } else { return 0 } }
END
    print qq{ok - 2 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 2 $^X @{[__FILE__]}\n};
}

# eval <<END has eval '...'
if (eval Char::INFORMIXV6ALS::escape <<END) {
eval Char::INFORMIXV6ALS::escape ' if (qq{アソ} !~ /A/) { return 1 } else { return 0 } '
END
    print qq{ok - 3 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 3 $^X @{[__FILE__]}\n};
}

# eval <<END has eval q{...}
if (eval Char::INFORMIXV6ALS::escape <<END) {
eval Char::INFORMIXV6ALS::escape q{ if ('アソ' !~ /A/) { return 1 } else { return 0 } }
END
    print qq{ok - 4 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 4 $^X @{[__FILE__]}\n};
}

# eval <<END has eval $var
my $var = q{q{ if ('アソ' !~ /A/) { return 1 } else { return 0 } }};
if (eval Char::INFORMIXV6ALS::escape <<END) {
eval Char::INFORMIXV6ALS::escape $var
END
    print qq{ok - 5 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 5 $^X @{[__FILE__]}\n};
}

# eval <<END has eval (omit)
$_ = "if ('アソ' !~ /A/) { return 1 } else { return 0 }";
if (eval Char::INFORMIXV6ALS::escape <<END) {
eval Char::INFORMIXV6ALS::escape
END
    print qq{ok - 6 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 6 $^X @{[__FILE__]}\n};
}

# eval <<END has eval {...}
if (eval Char::INFORMIXV6ALS::escape <<END) {
eval { if ('アソ' !~ /A/) { return 1 } else { return 0 } }
END
    print qq{ok - 7 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 7 $^X @{[__FILE__]}\n};
}

# eval <<END has "..."
if (eval Char::INFORMIXV6ALS::escape <<END) {
if ('アソ' !~ /A/) { return \"1\" } else { return \"0\" }
END
    print qq{ok - 8 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 8 $^X @{[__FILE__]}\n};
}

# eval <<END has qq{...}
if (eval Char::INFORMIXV6ALS::escape <<END) {
if ('アソ' !~ /A/) { return qq{1} } else { return qq{0} }
END
    print qq{ok - 9 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 9 $^X @{[__FILE__]}\n};
}

# eval <<END has '...'
if (eval Char::INFORMIXV6ALS::escape <<END) {
if ('アソ' !~ /A/) { return '1' } else { return '0' }
END
    print qq{ok - 10 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 10 $^X @{[__FILE__]}\n};
}

# eval <<END has q{...}
if (eval Char::INFORMIXV6ALS::escape <<END) {
if ('アソ' !~ /A/) { return q{1} } else { return q{0} }
END
    print qq{ok - 11 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 11 $^X @{[__FILE__]}\n};
}

# eval <<END has $var
my $var1 = 1;
my $var0 = 0;
if (eval Char::INFORMIXV6ALS::escape <<END) {
if ('アソ' !~ /A/) { return $var1 } else { return $var0 }
END
    print qq{ok - 12 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 12 $^X @{[__FILE__]}\n};
}

__END__
