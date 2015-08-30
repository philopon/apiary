#!/usr/bin/env bash -eu

FILE=`curl -L https://gist.githubusercontent.com/philopon/b51788ae5c217dc7d269/raw/get-latest-newrelic-agent.sh | bash`
tar xvf $FILE
NEWRELIC_BASE=`pwd`/${FILE%.tar.gz}
export LD_LIBRARY_PATH=$NEWRELIC_BASE/lib

path=("." "./examples")
for p in `cat submodules`; do
  path=("${path[@]}" "./$p")
done

cabal update
cabal install\
  --force-reinstalls --reorder-goals --only-dependencies --enable-tests\
  --extra-include-dirs=$NEWRELIC_BASE/include\
  --extra-lib-dirs=$NEWRELIC_BASE/lib\
  "${path[@]}"
