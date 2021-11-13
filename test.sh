# ghcid -W -c "cabal new-repl pgrest" -T Main.main

curl --insecure --verbose --header "Content-Type: application/json" \
  --request POST \
  --data '{"login":"vampire.phan.01@gmail.com","company_id":1,"partner_id":1}' \
  "https://localhost:3333/res_users"
