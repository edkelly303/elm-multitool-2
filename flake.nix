{
  description = "elm-multitool-2";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    inputs:
    let
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs {
        system = system;
        config.allowUnfree = true;
      };
    in
    {
      # SHELL
      devShells.${system}.default = pkgs.mkShell {
        name = "devShell";
        packages = with pkgs; [
          xdg-utils
          nodejs
          elmPackages.elm
          elmPackages.elm-json
          elmPackages.elm-format
          elmPackages.elm-test
          elmPackages.elm-doc-preview
          elmPackages.elm-review
        ];
        shellHook = ''
          DEVDIR="$PWD"
          echo -e "\n\033[1m*** Entering development shell for elm-multitool-2 ***\033[0m\n"

          echo -e -n "Updating repos... "
          (cd $DEVDIR && git pull --quiet &)
          echo -e "Update complete!\n"
          

          echo -e "\033[1;36mrun\033[0m: start the development environment"

          run () {
            cd "$DEVDIR"
            code .
            (sleep 2; xdg-open 'http://localhost:8008') &
            npx run-pty run-pty.json
          }
        '';
      };
    };
}
