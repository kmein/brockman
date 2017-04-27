
let
  pname = "news";
  version = "1";

  pkgs = nixpkgs // extrapkgs;
  nixpkgs = import <nixpkgs> {};
  extrapkgs = {
  };
  hsPkgs = pkgs.haskellPackages.override {
    overrides = self: super: {
    };
  };
  hsEnv = hsPkgs.ghcWithPackages (_hsPkgs: with _hsPkgs;
    [
      bloomfilter
      irc
      irc-client
      feed
      split
      warp
      wai-util
      wreq
    ]);
in

pkgs.myEnvFun {
  name = "${pname}-${version}";

  buildInputs = with pkgs; [
    hsEnv
  ];

  extraCmds = with pkgs; ''
    $(grep export ${hsEnv.outPath}/bin/ghc)
  '';
}

# vim: set fdm=marker :
