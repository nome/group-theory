Build instructions
==================

Debian/Ubuntu
-------------
    apt-get install ghc libghc-parsec3-dev cabal-install libreadline-dev
    cabal update
    cabal install readline
    ghc --make rubik
    ghc --make d4

Examples
========
Superflip:
    ur2fbrb2ru2lb2ru'd'r2fr'lb2u2f2
    -> (Fl Lf)(Fr Rf)(Fu Uf)(Fd Df)(Bl Lb)(Br Rb)(Bu Ub)(Bd Db)(Lu Ul)(Ld Dl)(Ru Ur)(Rd Dr)

Two-corner-flip:
    lul'ulu2l'r'u'ru'r'u2r
    -> (Fru Ufr Rfu)(Bru Ubr Rbu)

Two-edge-flip:
    [rl'fflr'drl'f'r'l,u]
    -> (Fu Uf)(Lu Ul)

corner twist:
    [r'drd'r'dr,u]
    -> (Flu Ufl Lfu)(Fru Ufr Rfu)

edge swap:
    [rl'ffr'lurl'ffr'lu'rl'ffr'l,u]
    -> (Fu Lu Ru)(Uf Ul Ur)

