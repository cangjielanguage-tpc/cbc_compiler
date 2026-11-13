#!/usr/bin/env bash

source "$TOOLCHAIN/envsetup.sh"
if [ -n "${CANGJIE_STDX_PATH:-}" ]; then
    if [ "$(uname -s)" = "Darwin" ]; then
        export DYLD_LIBRARY_PATH="$CANGJIE_STDX_PATH${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"
    else
        export LD_LIBRARY_PATH="$CANGJIE_STDX_PATH${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
    fi
fi
"$@"
exit
