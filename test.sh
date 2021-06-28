# ghcid -W -c "cabal new-repl pgrest" -T Main.main

curl --header "Content-Type: application/json" \
  --request POST \
  --data '{"login":"vampire@gmail.com","company_id":1}' \
  http://localhost:3333/res_users
