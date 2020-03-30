read -p "Please specify your secret sentence for your account: (eg: fox smile) >> " ACCOUNT_SECRET
./wsclient.native -url http://127.0.0.1:9944 -f scripts/force_create.sut -args "$ACCOUNT_SECRET" "0x0" "0x10000000000000000000000000000000"  "0x12340000000000000000000000000000" "0xabc40000000000000000000000000000"
