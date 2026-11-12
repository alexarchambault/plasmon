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

## Release Notes

### 0.1.0

Initial release of Plasmon
