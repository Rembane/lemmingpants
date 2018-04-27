#! /bin/bash
docker build ./ -f ./Dockerfile-frontend -t lemmingpants-frontend
docker build ./ -f ./Dockerfile-backend -t lemmingpants-backend
