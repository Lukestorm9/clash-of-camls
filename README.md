# Clash of Camls

An epic multiplayer competitive quest for camels, glory, and the golden icon.

## Authors:

Luis Hoderlein, Luke Murphy and Raj Rafalia

## Install instructions:

Requires sdl. Depending on your OS, this is a different process. Check your package manager. E.g. :

On Arch, it is something like: `yay -S sdl sdl_image sdl_ttf sdl_mixer`. Make sure you do **not** install sdl2. It will not work with `ocamlsdl`. At the end, `sdl-config --version` should be 1.2.xx.

On Ubuntu, it is something like: `sudo apt install libsdl1.2debian libsdl-gfx1.2-5 libsdl-image1.2 libsdl-image1.2-dev libsdl-mixer1.2 libsdl-mixer1.2-dev libsdl-sound1.2 libsdl-sound1.2-dev libsdl-ttf2.0-0 libsdl-ttf2.0-dev`.

Once you have sdl, you will need to install the relevant opam packages: `opam install ocamlsdl`. If, at this stage, there are syntax highlighting problems or the code does not compile, this probably means that the distro package list above is incomplete. In that case, `ocamlsdl` will build, but it will **not** notify you that it lacks the necessary depends, and will thus simply silently fail when building.

If you're running WSL, you will also have get WSL 2, or find some way of running an X Server on your machine other than what will follow here. See these instructions [here](https://docs.microsoft.com/en-gb/windows/wsl/install-win10) for how to move to WSL 2. Doing this has not induced any problems so far, but if you are worried, you can try to proceed without moving to WSL 2 (though we didn't test what happens if you don't).

You'll probably want to update an existing installation using something like `wsl --set-version Ubuntu-20.04 2` in powershell with admin to move to WSL 2. Use `wsl --list --verbose` to see what you have running currently if that distro version does not work.

You'll then need to install `https://sourceforge.net/projects/vcxsrv/`. After you have done so, run the following through CMD (not powershell) `"C:\Program Files\VcXsrv\vcxsrv.exe" :0 -ac -terminate -lesspointer -multiwindow -clipboard -wgl -dpi auto`. Run the shortcut it creates to configure VcXsrv.

Then run `sudo apt install terminator` on WSL. Run an X-Server compliant window with `DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0 terminator &`. If you try to run the game, it has to be from the window that command creates.

## Running the game

To run the game, just do `make play`.

If you are on WSL, make sure that you have VcXsrv running (using `"C:\Program Files\VcXsrv\vcxsrv.exe" :0 -ac -terminate -lesspointer -multiwindow -clipboard -wgl -dpi auto` and then using the shortcut). Also make sure that you are running `make play` inside the window created by terminator (i.e. with `DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0 terminator &`).

To run the tests, just do `make test`. You do not need to install the X server or move to WSL 2 to do this, only install the distro and opam packages.