{ system ? builtins.currentSystem
, iosSdkVersion ? "10.2"
, obelisk ? (import ./.obelisk/impl { inherit system iosSdkVersion; })
, withHoogle ? false
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }: with pkgs.haskell.lib; {
  inherit withHoogle;

  # android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  # android.displayName = "Obelisk Minimal Example";
  # ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  # ios.bundleName = "Obelisk Minimal Example";

  __closureCompilerOptimizationLevel = null;

})
