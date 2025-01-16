# full "~/myproject/shell.nix" file
let
  myNixPkgs = import <nixpkgs> {
    overlays = [myNixPkgsOverlay];
  };

  myNixPkgsOverlay = (nixSelf: nixSuper: {
    myHaskellPackages = nixSelf.haskellPackages.override (oldHaskellPkgs: {
      overrides = nixSelf.lib.composeExtensions (oldHaskellPkgs.overrides or (_: _: {}))  myHaskellPkgsOverlay;
    });
  });

  myHaskellPkgsOverlay = (hSelf: hSuper: {
    # "myproject" is the first part of the "myproject.cabal" project definition file
    myProject = hSelf.callCabal2nix "anagrams" ./. {};
  });
  
  myDevTools = with myNixPkgs; [
    cabal-install 
    haskellPackages.ghcid
    haskell-language-server
    hlint
    ormolu 
  ];

  myShellHook = ''
    alias repl="cabal new-repl"
  '';
in
myNixPkgs.myHaskellPackages.myProject.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ myDevTools;
  shellHook = myShellHook;
})
