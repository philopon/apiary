#!/usr/bin/env bash -eu

if [ $# -eq 0 ]; then
  BASE=http://download.newrelic.com/agent_sdk
  FILE=`curl $BASE/ | sed -e '/<\/a>/ !d; s/.*<a *href="\(.*\)">.*/\1/g; /tar\.gz/ !d'`
  curl -O $BASE/$FILE
  tar xvf $FILE
  sudo cp ${FILE%.tar.gz}/include/* /usr/local/include
  sudo cp ${FILE%.tar.gz}/lib/*     /usr/local/lib
fi

[ "$1" == "develop" ] && shift

path=("." "./examples")
for p in `cat submodules`; do
  path=("${path[@]}" "./$p")
done

cabal --version
cabal sandbox init

if [ -e constraint ]; then
  cnst=()
  while read line; do
    cnst=("--constraint=$line" "$cnst")
  done < constraint

  echo cabal install --force-reinstalls --reorder-goals --only-dependencies --enable-tests "${path[@]}" "${cnst[@]}" "$@"
  cabal install --force-reinstalls --reorder-goals --only-dependencies --enable-tests "${path[@]}" "${cnst[@]}" "$@"
else
  echo cabal install --force-reinstalls --reorder-goals --only-dependencies --enable-tests "${path[@]}" "$@"
  cabal install --force-reinstalls --reorder-goals --only-dependencies --enable-tests "${path[@]}" "$@"
fi
