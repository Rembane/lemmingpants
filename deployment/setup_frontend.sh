#!/usr/bin/env sh
#
# The image creation script for the frontend part of Lemmingpants.
# It should produce an index.html, main.js and style.css.
# Mount a volume before you run this image to get those files back.
#
# This script should be idempotent, if it isn't please file an issue.

set -eu

SETUP_PATH="/setup"
cd $SETUP_PATH

echo "Install Ubuntu packages."
apt-get update
apt-get install -y curl default-jre-headless git jq make nodejs sassc unzip

echo "Download and install psc-package."
if [ ! -e /usr/local/bin/psc-package ]; then
    mkdir psc-package
    cd psc-package
    curl --remote-name-all -sSL $(curl -sS https://api.github.com/repos/purescript/psc-package/releases/latest | jq -Mrc '.assets | map(select(.browser_download_url | contains("linux")) | .browser_download_url) | join ("\n")')
    mkdir bundle
    mv linux64.tar.gz bundle
    sha1sum -c linux64.sha
    gunzip -c bundle/linux64.tar.gz | tar x
    mv psc-package/psc-package /usr/local/bin
fi

cd $SETUP_PATH
echo "Download and install Purescript"
if [ ! -e /usr/local/bin/purs ]; then
    mkdir purescript
    cd purescript
    curl --remote-name-all -sSL $(curl -sS https://api.github.com/repos/purescript/purescript/releases/latest | jq -Mrc '.assets | map(select(.browser_download_url | contains("linux")) | .browser_download_url) | join ("\n")')
    mkdir bundle
    mv linux64.tar.gz bundle
    sha1sum -c linux64.sha
    gunzip -c bundle/linux64.tar.gz | tar x
    mv purescript/purs /usr/local/bin
fi

cd $SETUP_PATH
echo "Download and install Google Closure compiler"
if [ ! -e /usr/local/bin/closure-compiler.jar ]; then
    curl -sSL https://dl.google.com/closure-compiler/compiler-latest.zip | funzip > /usr/local/bin/closure-compiler.jar
    echo '#!/usr/bin/env sh' > /usr/local/bin/closure-compiler
    echo 'java -jar /usr/local/bin/closure-compiler.jar $@' >> /usr/local/bin/closure-compiler
    chmod +x /usr/local/bin/closure-compiler
fi

# We assume that /setup/frontend exists and contains the frontend code.
# Build the frontend.
cd /setup/frontend
make start-anew
make test
make package
