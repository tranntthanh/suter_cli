read -p "Please specify your secret sentence for your account: (eg: fox smile) >> " ACCOUNT_SECRET
read -p "Please specify target account id: (256 bytes hex) >> " TARGET_ACCOUNT
read -p "Your nonce ?: (eg: 0x00) >> " NONCE
read -p "Transfer Amount ?: (eg: 0x10000000000000000000000000000000, must be u128 little-end encoded) >> " AMOUNT
./wsclient.native -url http://127.0.0.1:9944 -f scripts/transfer_token.sut -args "$ACCOUNT_SECRET" "$NONCE" "$AMOUNT" "$TARGET_ACCOUNT"
