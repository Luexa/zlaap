# zlaap

An advanced command line argument parsing framework for Zig.

Work in progress: for now only raw argument iterators are implemented.
WASI requires a single contiguous allocation of argv, while argv can be retrieved without allocation on Windows and POSIX.
Every platform supports argv iteration with no allocation required.

# Usage

To use this library, it is currently recommended to add it as a git-submodule or vendor the source directly.

Example `build.zig` (assumes repository is checked out in `deps/zlaap`):

```zig
// As a direct dependency (your executable imports it).
exe.addPackagePath("zlaap", "deps/zlaap/src/main.zig");

// As a transitive dependency (your dependency imports it).
exe.addPackage(Pkg{
    .name = "example",
    .path = "deps/example/src/main.zig",
    .dependencies = &[_]Pkg{
        .{ .name = "zlaap", .path = "deps/zlaap/src/main.zig" },
    },
});
```

## License

`zlaap` is made freely available under the terms of the [BSD 0-Clause License](LICENSE).
Third-party contributions shall be licensed under the same terms unless explicitly stated otherwise.
