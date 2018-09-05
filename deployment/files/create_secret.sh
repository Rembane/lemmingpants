set -e
FP=/setup/secret_key.txt
if [ ! -f $FP ]; then
  CHARS=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789
  RESULT=""
  for _ in {1..50}; do
    RESULT=$RESULT${CHARS:RANDOM%${#CHARS}:1}
  done
  echo $RESULT > $FP
fi;
KEY=`cat $FP`
psql -d lemmingpants <<< 'ALTER DATABASE lemmingpants SET "app.jwt_secret" TO "'$KEY'"'
