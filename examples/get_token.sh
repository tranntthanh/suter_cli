read -p "Please specify your secret sentence for your account: (eg: fox smile) >> " ACCOUNT_SECRET
./wsclient.native -url http://127.0.0.1:9944 -f scripts/get_token.sut -args "$ACCOUNT_SECRET"
