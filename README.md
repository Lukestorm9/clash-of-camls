# Clash of Camls

An epic multiplayer competitive quest for camels, glory, and the golden icon.

## Authors:

Luis Hoderlein, Luke Murphy and Raj Rafalia

## Install instructions:

See the file INSTALL.md located in this repository.

## Running the game

To run the game, just do `make play`.

If you are on WSL, make sure that you have VcXsrv running (using `"C:\Program Files\VcXsrv\vcxsrv.exe" :0 -ac -terminate -lesspointer -multiwindow -clipboard -wgl -dpi auto` and then using the shortcut). Also make sure that you are running `make play` inside the window created by terminator (i.e. with `DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0 terminator &`).

To run the tests, just do `make test`. You do not need to install the X server or move to WSL 2 to do this, only install the distro and opam packages.