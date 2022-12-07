new-user() {
curl -d "$(echo '{ "email": ' "\"$1@bob.bob\""', "password": "bobbobbob", "personal": { "firstName": "'$1'", "lastName": "borbur", "gender": "Male", "dateOfBirth": "2022-11-05T03:51:09.967925217Z", "height": 104, "description": "yuh", "race": "Polish" } }')" -H 'Content-Type: application/json' -iXPOST localhost:8080/register
}
