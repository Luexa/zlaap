# zlaap

A minimal command line argument retrieval framework for Zig.

This library is a replacement for the argument iterators in `std.process`, which have several flaws:
 * The Windows argument iterator uses the non-Unicode API to access the process command line.
 * The WASI argument iterator performs several unnecessary allocations.

No allocation is required for any target except WASI.
However, as Windows does not have an argv array like POSIX and WASI do, allocation may be required for your use case.

# Usage

To use this library, it is currently recommended to add it as a git-submodule or to vendor the source directly.

Example `build.zig` (assumes repository is checked out in `deps/zlaap`):

```zig
// As a direct dependency (your executable imports it).
exe.addPackagePath("zlaap", "deps/zlaap/zlaap.zig");

// As a transitive dependency (your dependency imports it).
exe.addPackage(Pkg{
    .name = "example",
    .path = "deps/example/src/main.zig",
    .dependencies = &[_]Pkg{
        .{ .name = "zlaap", .path = "deps/zlaap/zlaap.zig" },
    },
});
```

## License

`zlaap` is made freely available under the terms of the [BSD 0-Clause License](LICENSE).
Third-party contributions shall be licensed under the same terms unless explicitly stated otherwise.
