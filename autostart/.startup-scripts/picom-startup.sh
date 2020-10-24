#!/usr/bin/env sh

# Terminate already running composior instances
killall -q picom

# Wait until the processes have been shut down
while pgrep -x picom >/dev/null; do sleep 1; done

picom --config $HOME/.config/picom/picom.conf --experimental-backends &
