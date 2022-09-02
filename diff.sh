#!/bin/sh
srec_cat -output - -Binary tbi68k.hex |xxd >good
srec_cat -output - -Binary tbi68k.s37 |xxd >bad
diff --unified good bad >diff
