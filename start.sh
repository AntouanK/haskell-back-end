    #!/bin/sh

    echo ""
    echo "~~~~~ Building..."
    stack build \
        && echo "~~~~~ Building done." \
        && echo "" \
        && echo "~~~~~ Starting server..." \
        && ./.stack-work/dist/x86_64-linux-tinfo6-nopie/Cabal-2.0.1.0/build/public-client-back-end-exe/public-client-back-end-exe
    #    && stack exec public-client-back-end-exe
