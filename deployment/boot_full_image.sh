#!/usr/bin/env bash

set -eu

DOC=$(cat <<'EOF'
Initialize the image to point to the correct domain and so on...

Arguments:

$1 : Host port for REST API.
$2 : Host port for websockets and static file serving.
$3 : URL for where you have deployed the REST API.
$4 : URL for where you have deployed the Websockets.

Usage example:

./boot_full_image.sh 18800 13300 https://lemmingpants.example.com/api/ wss://lemmingpants.example.com/
EOF
)

# The script takes exactly four arguments.
if [[ $# -ne 4 ]]; then
    echo "$DOC"
    exit 1
fi

TAG='custom-lp-deployment-tag'
if docker image ls | awk '{print $2}' | grep -q "$TAG"; then
    echo "Found an image with tag $TAG, maybe it's already running like you want it to, and all is fine. Otherwise delete it and try again."
else
    docker run lemmingpants/backend-full:latest sed -i -e "s%http://localhost:3000%${3}%" -e "s%ws://localhost:8000%${4}%" /srv/static/main.js
    CONTAINER_ID=$(docker ps -lq)
    IMAGE_ID=$(docker commit $CONTAINER_ID | cut -c8-)
    docker tag $IMAGE_ID lp/deployed-full:"$TAG"
    docker run --restart always -d -p 127.0.0.1:"$1":3000 -p 127.0.0.1:"$2":8000 lp/deployed-full:"$TAG" /usr/local/bin/run_backend.sh
    echo "The container should now be online..."
fi
