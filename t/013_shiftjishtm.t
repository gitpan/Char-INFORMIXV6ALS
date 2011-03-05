# This file is encoded in INFORMIX V6 ALS.
die "This file is not encoded in INFORMIX V6 ALS.\n" if q{あ} ne "\x82\xa0";

use Char::INFORMIXV6ALS;
print "1..1\n";

# Unrecognized character \x82
# 「認識されない文字 \x82」
if (join('',"対応表", "なんでもいいけど") eq join('',pack('C6',0x91,0xce,0x89,0x9e,0x95,0x5c),"なんでもいいけど")) {
    print qq<ok - 1 "TAIOUHYO","NANDEMOIIKEDO"\n>;
}
else {
    print qq<not ok - 1 "TAIOUHYO","NANDEMOIIKEDO"\n>;
}

__END__

Char::INFORMIXV6ALS.pm の処理結果が以下になることを期待している

if (join('',"対応表\", "なんでもいいけど") eq join('',pack('C6',0x91,0xce,0x89,0x9e,0x95,0x5c),"なんでもいいけど")) {

Shift-JISテキストを正しく扱う
http://homepage1.nifty.com/nomenclator/perl/shiftjis.htm
