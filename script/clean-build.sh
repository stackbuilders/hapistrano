# This is a comment
export PATH=~/.cabal/bin:/usr/local/bin:$PATH
 
cabal sandbox delete # kill it with fire!
cabal sandbox init
cabal clean
cabal update
cabal install --only-dependencies -j
cabal build -j
