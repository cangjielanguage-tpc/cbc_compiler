#!/usr/bin/env bash

source "$TOOLCHAIN/envsetup.sh"
if [ -n "${CANGJIE_STDX_PATH:-}" ]; then
    export LD_LIBRARY_PATH="$CANGJIE_STDX_PATH${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
fi
"$@"
exit
