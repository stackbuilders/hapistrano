export PATH=~/.cabal/bin:/usr/local/bin:$PATH
rm -rf .cabal-sandbox
cabal sandbox init
cabal clean
cabal update
cabal install --only-dependencies -j
cabal build -j
