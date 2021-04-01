# Clash of Camls

An epic multiplayer competitive quest for camels, glory, and the golden icon.

## Authors:

Luis Hoderlein, Luke Murphy and Raj Rafalia

## Install instructions:

See the file INSTALL.md located in this repository.

## Running the game

To run a local version of the game with a server run, `make play CMD="local <port>"`. Make sure that `port` is a valid port, which currently has no other program listening. Choosing a number in the range 10000-50000 should almost guarantee this. However, previous instances of clash-of-camls running on the same port have been known to conflict, even if they have been killed. It has something to do with the Unix sockets not being cleaned up.

To connect to a server running somewhere else, use `make play CMD="remote <addr> <port>"`. Note that because of silly Unix restrictions, `addr` must be an IP address, and cannot be a standard web address.

Notice that the controls are the standard `WASD` to move, and `ESC` to quit the application.

HINT: You can use `ip addr` on linux to find your IP on the local network, and a tool like [ngrok](https://ngrok.io) to create a secure tunnel to your local machine.

If you are on WSL, make sure that you have VcXsrv running (using `"C:\Program Files\VcXsrv\vcxsrv.exe" :0 -ac -terminate -lesspointer -multiwindow -clipboard -wgl -dpi auto` and then using the shortcut). Also make sure that you are running `make play` inside the window created by terminator (i.e. with `DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0 terminator &`).

To run the tests, just do `make test`. You do not need to install the X server or move to WSL 2 to do this, only install the distro and opam packages.