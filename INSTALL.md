### Installation

Make sure that you have OCaml and Ocamlbuild running on your system. Additionally, it requires sdl. Depending on your OS, this is a different process. Check your package manager. E.g. :

On Arch, it is something like: `yay -S sdl sdl_image sdl_ttf sdl_mixer`. Make sure you do **not** install sdl2. It will not work with `ocamlsdl`. At the end, `sdl-config --version` should be 1.2.xx.

On Ubuntu, it is something like: `sudo apt update` and then `sudo apt install libsdl1.2debian libsdl-gfx1.2-5 libsdl-image1.2 libsdl-image1.2-dev libsdl-mixer1.2 libsdl-mixer1.2-dev libsdl-sound1.2 libsdl-sound1.2-dev libsdl-ttf2.0-0 libsdl-ttf2.0-dev`.

Once you have sdl, you will need to install the relevant opam packages: `opam install ounit2 ocamlsdl`. If, at this stage, there are syntax highlighting problems or the code does not compile, this probably means that the distro package list above is incomplete. In that case, `ocamlsdl` will build, but it will **not** notify you that it lacks the necessary depends, and will thus simply build "successfully" but without the necessary modules.

If you're running WSL, you will also have get WSL 2, or find some way of running an X Server on your machine other than what will follow here. See these instructions [here](https://docs.microsoft.com/en-gb/windows/wsl/install-win10) for how to move to WSL 2. Doing this has not induced any problems so far, but if you are worried, you can try to proceed without moving to WSL 2 (though we didn't test what happens if you don't).

You'll probably want to update an existing installation after following the Microsoft instructions. Use `wsl --list --verbose` to see what you have running. Then run something like `wsl --set-version Ubuntu-20.04 2` in powershell with admin to move to WSL 2 (assuming that Ubuntu-20.04) was your WSL distro/version.

You'll then need to install VcXsrv `https://sourceforge.net/projects/vcxsrv/` on your Windows machine itself. There are no setting you need to change. You will also need terminator: `sudo apt install terminator` on Ubuntu WSL. After you have done so, run the following through CMD (not powershell) `"C:\Program Files\VcXsrv\vcxsrv.exe" :0 -ac -terminate -lesspointer -multiwindow -clipboard -wgl -dpi auto`. Run the shortcut it creates to configure VcXsrv, just clicking next every time is fine.

Run an X-Server compliant window with `DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0 terminator &` on Ubuntu WSL. If you try to run the game, it has to be from the window that this command creates.

To then actually run the game, use `cd` into the directory, and then run `make play CMD="local <port>"`. Sometimes this will fail if there is a device listening at the port you specified, with the message that it is "not a valid server address". We recommend changing the port every time you run make. See the file README.md in this repository for more details.
