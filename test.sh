# ghcid -W -c "cabal new-repl pgrest" -T Main.main

curl --insecure --header "Content-Type: application/json" \
  --request PUT \
  --data '{"id":16,"login":"hoang.phan@gmail.com"}' \
  "https://localhost:3333/res_users?id=eq.16"
