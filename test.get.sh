# ghcid -W -c "cabal new-repl pgrest" -T Main.main
curl --insecure \
  --verbose \
  --header "Content-Type: application/json" \
  --request GET \
  "https://localhost:3333/res_users?id=eq.1" | jq
