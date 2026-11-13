# Plasmon

Extension Plasmon

## Features

- Feature

## Requirements

- thing

## Extension Settings

This extension contributes the following settings:

* `plasmon.binary`: path to the Plasmon binary to use (optional).
* `plasmon.sourcepath`: have presentation compilers read the workspace's own modules from their sources (Scala 3 `-sourcepath`) rather than from the class directories the build tool reports, so nothing needs to be built for a module to see the ones it depends on. Starts the server with `--sourcepath`. The server is restarted when this changes (a prompt asks first).
* `plasmon.sourcepathMode`: advanced, `logical` (default) or `classic`, how sources are handed to the compiler with `plasmon.sourcepath`.

## Manual and automatic modes

In manual mode (the default), you pick the build tools and modules to load, with
"Plasmon: Load or unload build tool or module". In automatic mode, each Scala or Java file of the
workspace you open gets a build tool and a module loaded for it if it has none.

Switch with "Plasmon: Switch to automatic mode" / "Plasmon: Switch to manual mode", or from the
"Manual mode" / "Automatic mode" language status item. Switching back to manual mode doesn't unload
anything. The mode is kept in `.plasmon/mode.json`, so the server starts in the mode the workspace
was left in. `plasmon code --auto` starts it in automatic mode (`--auto=false`: manual).

## Release Notes

### 0.1.0

Initial release of Plasmon
