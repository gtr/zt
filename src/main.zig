const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const Allocator = mem.Allocator;
const ArrayList = std.ArrayList;

const VERSION = "0.1.0";
const DEFAULT_DEPTH = std.math.maxInt(u32);
const NORMAL_BRANCH = "├── ";
const LAST_BRANCH = "└── ";

// ANSI colors
const Color = enum {
    reset,
    blue,
    green,

    pub fn to_ansi(self: Color) []const u8 {
        return switch (self) {
            .reset => "\x1b[0m",
            .blue => "\x1b[34m",
            .green => "\x1b[32m",
        };
    }
};

// File entry type
const EntryType = enum {
    file,
    directory,
    symlink,
    unknown,
};

// File system entry
const Entry = struct {
    name: []const u8,
    full_path: []const u8,
    type: EntryType,
    children: ?ArrayList(Entry),

    pub fn init(allocator: Allocator, name: []const u8, full_path: []const u8, entry_type: EntryType) !Entry {
        return Entry{
            .name = try allocator.dupe(u8, name),
            .full_path = try allocator.dupe(u8, full_path),
            .type = entry_type,
            .children = if (entry_type == .directory)
                try ArrayList(Entry).initCapacity(allocator, 8)
            else
                null,
        };
    }

    pub fn deinit(self: *Entry, allocator: Allocator) void {
        allocator.free(self.name);
        allocator.free(self.full_path);

        if (self.children) |*children| {
            for (children.items) |*child| {
                child.deinit(allocator);
            }
            children.deinit();
        }
    }

    pub fn add_child(self: *Entry, child: Entry) !void {
        if (self.children) |*children| {
            try children.append(child);
        }
    }
};

// Counts for files and directories
const Counts = struct {
    files: usize,
    dirs: usize,
};

// Options for tree display
const Options = struct {
    show_all: bool = false,
    max_depth: u32 = DEFAULT_DEPTH,
    show_full_path: bool = false,
    dirs_only: bool = false,
    use_color: bool = true,

    pub fn init() Options {
        return Options{};
    }
};

// Core scanning function
fn scan_directory(
    allocator: Allocator,
    path: []const u8,
    depth: u32,
    parent: *Entry,
    options: *const Options,
) !void {
    if (depth == 0) return;

    var dir = try fs.openDirAbsolute(path, .{ .iterate = true });
    defer dir.close();

    var iter = dir.iterate();
    while (try iter.next()) |entry| {
        if (!options.show_all and entry.name.len > 0 and entry.name[0] == '.') {
            continue;
        }

        const entry_path = try fs.path.join(allocator, &[_][]const u8{ path, entry.name });
        defer allocator.free(entry_path);

        const entry_type: EntryType = switch (entry.kind) {
            .file => .file,
            .directory => .directory,
            .sym_link => .symlink,
            else => .unknown,
        };

        if (options.dirs_only and entry_type != .directory) {
            continue;
        }

        const new_entry = try Entry.init(allocator, entry.name, entry_path, entry_type);
        try parent.add_child(new_entry);

        if (entry_type == .directory) {
            try scan_directory(
                allocator,
                entry_path,
                depth - 1,
                &parent.children.?.items[parent.children.?.items.len - 1],
                options,
            );
        }
    }
}

// Main tree printing function
fn print_tree(
    allocator: Allocator,
    writer: fs.File.Writer,
    entry: Entry,
    prefix: []const u8,
    is_last: bool,
    options: Options,
    counts: *Counts,
) !void {
    if (entry.type == .directory) {
        counts.dirs += 1;
    } else {
        counts.files += 1;
    }

    const branch = if (is_last) LAST_BRANCH else NORMAL_BRANCH;

    var color_code: []const u8 = "";
    var reset_code: []const u8 = "";

    if (options.use_color) {
        color_code = if (entry.type == .directory)
            Color.blue.to_ansi()
        else
            Color.green.to_ansi();
        reset_code = Color.reset.to_ansi();
    }

    try writer.print("{s}{s}{s}{s}{s}\n", .{
        prefix,
        branch,
        color_code,
        if (options.show_full_path) entry.full_path else entry.name,
        reset_code,
    });

    if (entry.type == .directory and entry.children != null) {
        const new_prefix = if (is_last)
            try std.fmt.allocPrint(allocator, "{s}    ", .{prefix})
        else
            try std.fmt.allocPrint(allocator, "{s}│   ", .{prefix});
        defer allocator.free(new_prefix);

        // Sort children (directories first, then alphabetically)
        std.sort.insertion(Entry, entry.children.?.items, {}, struct {
            fn less(_: void, a: Entry, b: Entry) bool {
                if (a.type == .directory and b.type != .directory) return true;
                if (a.type != .directory and b.type == .directory) return false;
                return std.mem.lessThan(u8, a.name, b.name);
            }
        }.less);

        const children = entry.children.?.items;

        for (0..children.len) |i| {
            try print_tree(allocator, writer, children[i], new_prefix, i == children.len - 1, options, counts);
        }
    }
}

fn parse_args(allocator: Allocator, args: []const []const u8) !struct {
    options: Options,
    paths: ArrayList([]const u8),
} {
    var options = Options.init();
    var paths = ArrayList([]const u8).init(allocator);

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];

        if (std.mem.eql(u8, arg, "-a") or std.mem.eql(u8, arg, "--all")) {
            options.show_all = true;
        } else if (std.mem.eql(u8, arg, "-d") or std.mem.eql(u8, arg, "--depth")) {
            i += 1;
            if (i >= args.len) {
                return error.MissingArgument;
            }
            options.max_depth = try std.fmt.parseInt(u32, args[i], 10);
        } else if (std.mem.eql(u8, arg, "-f") or std.mem.eql(u8, arg, "--full-path")) {
            options.show_full_path = true;
        } else if (std.mem.eql(u8, arg, "--dirs-only")) {
            options.dirs_only = true;
        } else if (std.mem.eql(u8, arg, "--no-color")) {
            options.use_color = false;
        } else if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            try print_help(std.io.getStdOut().writer());
            std.process.exit(0);
        } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) {
            try std.io.getStdOut().writer().print("zt version {s}\n", .{VERSION});
            std.process.exit(0);
        } else if (arg[0] == '-') {
            std.debug.print("Unknown option: {s}\n", .{arg});
            try print_help(std.io.getStdErr().writer());
            std.process.exit(1);
        } else {
            try paths.append(try allocator.dupe(u8, arg));
        }
    }

    // If no paths provided, use current directory
    if (paths.items.len == 0) {
        try paths.append(try allocator.dupe(u8, "."));
    }

    return .{ .options = options, .paths = paths };
}

fn print_help(writer: fs.File.Writer) !void {
    try writer.writeAll(
        \\USAGE:
        \\  zt [OPTIONS] [PATH...]
        \\
        \\ARGUMENTS:
        \\  PATH  Directory path(s) to display, defaults to current directory
        \\
        \\OPTIONS:
        \\  -a, --all             Show all files, including hidden
        \\  -d, --depth LEVEL     Limit directory display to LEVEL
        \\  -f, --full-path       Show full file paths
        \\  --dirs-only           Show directories only
        \\  --no-color            Disable colored output
        \\  -h, --help            Display this help message
        \\  -v, --version         Display version information
        \\
    );
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const parsed = try parse_args(allocator, args);
    defer {
        for (parsed.paths.items) |path| {
            allocator.free(path);
        }
        parsed.paths.deinit();
    }

    for (parsed.paths.items) |path| {
        var abs_path_buffer: [std.fs.max_path_bytes]u8 = undefined;
        const abs_path = try std.fs.realpath(path, &abs_path_buffer);
        const owned_abs_path = try allocator.dupe(u8, abs_path);
        defer allocator.free(owned_abs_path);

        var root = try Entry.init(allocator, path, owned_abs_path, .directory);
        defer root.deinit(allocator);

        try scan_directory(allocator, owned_abs_path, parsed.options.max_depth, &root, &parsed.options);
        const stdout = std.io.getStdOut().writer();
        try stdout.print("{s}\n", .{abs_path});
        var counts = Counts{ .files = 0, .dirs = 0 };

        if (root.children) |children| {
            std.sort.insertion(Entry, children.items, {}, struct {
                fn less(_: void, a: Entry, b: Entry) bool {
                    if (a.type == .directory and b.type != .directory) return true;
                    if (a.type != .directory and b.type == .directory) return false;
                    return std.mem.lessThan(u8, a.name, b.name);
                }
            }.less);

            for (0..children.items.len) |i| {
                try print_tree(allocator, stdout, children.items[i], "", i == children.items.len - 1, parsed.options, &counts);
            }
        }

        try stdout.print("\n{d} directories, {d} files\n", .{ counts.dirs, counts.files });
    }
}
