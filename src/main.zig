const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const StringHashMap = std.StringHashMap;
const ChildProcess = std.process.Child;
const Allocator = mem.Allocator;
const ArrayList = std.ArrayList;

const VERSION = "0.1.0";
const DEFAULT_DEPTH = std.math.maxInt(u32);
const NORMAL_BRANCH = "├── ";
const LAST_BRANCH = "└── ";

// ANSI colors
const Color = enum {
    reset,
    red,
    blue,
    green,
    yellow,
    magenta,

    pub fn to_ansi(self: Color) []const u8 {
        return switch (self) {
            .reset => "\x1b[0m",
            .red => "\x1b[31m",
            .blue => "\x1b[34m",
            .green => "\x1b[32m",
            .yellow => "\x1b[33m",
            .magenta => "\x1b[35m",
        };
    }
};

// Git status
const GitStatus = enum {
    none,
    modified,
    new,
    staged,
    deleted,
    conflict,

    pub fn to_color(self: GitStatus) Color {
        return switch (self) {
            .none => .reset,
            .modified => .yellow,
            .new => .green,
            .staged => .blue,
            .deleted => .red,
            .conflict => .magenta,
        };
    }

    pub fn to_indicator(self: GitStatus) []const u8 {
        return switch (self) {
            .none => "",
            .modified => "M",
            .new => "?",
            .staged => "+",
            .deleted => "-",
            .conflict => "!",
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

const Entry = struct {
    name: []const u8,
    git_status: GitStatus = .none,
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

fn get_git_status_from_code(code: []const u8) GitStatus {
    if (mem.eql(u8, code, "??")) return .new;
    if (mem.eql(u8, code, " M") or mem.eql(u8, code, "MM")) return .modified;
    if (mem.eql(u8, code, "M ") or mem.eql(u8, code, "A ")) return .staged;
    if (mem.eql(u8, code, " D") or mem.eql(u8, code, "D ")) return .deleted;
    if (mem.eql(u8, code, "UU")) return .conflict;
    return .none;
}

const GitRepository = struct {
    is_repo: bool,
    branch: []const u8,
    status_map: StringHashMap(GitStatus),
    gitignore_patterns: ArrayList([]const u8),

    pub fn init(allocator: Allocator) GitRepository {
        return GitRepository{
            .is_repo = false,
            .branch = "",
            .status_map = StringHashMap(GitStatus).init(allocator),
            .gitignore_patterns = ArrayList([]const u8).init(allocator),
        };
    }

    pub fn deinit(self: *GitRepository) void {
        if (self.is_repo) {
            self.status_map.allocator.free(self.branch);

            var it = self.status_map.iterator();
            while (it.next()) |entry| {
                self.status_map.allocator.free(entry.key_ptr.*);
            }

            for (self.gitignore_patterns.items) |pattern| {
                self.gitignore_patterns.allocator.free(pattern);
            }

            self.gitignore_patterns.deinit();
            self.status_map.deinit();
        }
    }

    pub fn load(self: *GitRepository, root_path: []const u8) !bool {
        var dir = fs.openDirAbsolute(root_path, .{}) catch |err| {
            if (err == error.FileNotFound) {
                return false;
            }
            return err;
        };
        defer dir.close();

        var git_dir = dir.openDir(".git", .{}) catch |err| {
            if (err == error.FileNotFound) {
                return false;
            }
            return err;
        };
        git_dir.close();

        const branch_result = try ChildProcess.run(.{
            .allocator = self.status_map.allocator,
            .argv = &[_][]const u8{ "git", "-C", root_path, "branch", "--show-current" },
        });

        if (self.is_repo) {
            self.status_map.allocator.free(self.branch);
        }

        defer self.status_map.allocator.free(branch_result.stderr);

        if (branch_result.term.Exited == 0) {
            const trimmed_branch = std.mem.trimRight(u8, branch_result.stdout, "\n\r");
            self.branch = try self.status_map.allocator.dupe(u8, trimmed_branch);
        } else {
            self.branch = try self.status_map.allocator.dupe(u8, "unknown");
        }
        self.status_map.allocator.free(branch_result.stdout);

        const status_result = try ChildProcess.run(.{
            .allocator = self.status_map.allocator,
            .argv = &[_][]const u8{ "git", "-C", root_path, "status", "--porcelain" },
        });

        defer {
            self.status_map.allocator.free(status_result.stdout);
            self.status_map.allocator.free(status_result.stderr);
        }

        if (status_result.term.Exited != 0) {
            self.is_repo = true;
            return true;
        }

        if (self.is_repo) {
            var it = self.status_map.iterator();
            while (it.next()) |entry| {
                self.status_map.allocator.free(entry.key_ptr.*);
            }
            self.status_map.clearRetainingCapacity();
        }

        var lines = std.mem.tokenizeAny(u8, status_result.stdout, "\n\r");
        while (lines.next()) |line| {
            if (line.len < 3) continue;

            const status_code = line[0..2];
            const path = line[3..];

            const status = get_git_status_from_code(status_code);

            const full_path = try fs.path.join(self.status_map.allocator, &[_][]const u8{ root_path, path });

            try self.status_map.put(full_path, status);
        }

        self.is_repo = true;
        return true;
    }

    pub fn get_status(self: GitRepository, path: []const u8) GitStatus {
        if (!self.is_repo) return .none;

        return self.status_map.get(path) orelse .none;
    }

    pub fn should_ignore(self: GitRepository, path: []const u8) bool {
        if (!self.is_repo) return false;

        for (self.gitignore_patterns.items) |pattern| {
            if (match_gitignore(path, pattern)) {
                return true;
            }
        }

        return false;
    }
};

fn match_gitignore(path: []const u8, pattern: []const u8) bool {
    return std.mem.endsWith(u8, path, pattern);
}

const Counts = struct {
    files: usize,
    dirs: usize,
};

const Options = struct {
    show_all: bool = false,
    max_depth: u32 = DEFAULT_DEPTH,
    show_full_path: bool = false,
    dirs_only: bool = false,
    use_color: bool = true,
    show_git_status: bool = true,
    respect_gitignore: bool = true,

    pub fn init() Options {
        return Options{};
    }
};

fn scan_directory(
    allocator: Allocator,
    path: []const u8,
    depth: u32,
    parent: *Entry,
    options: *const Options,
    git_repo: ?*const GitRepository,
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

        if (options.respect_gitignore and git_repo != null and git_repo.?.is_repo and git_repo.?.should_ignore(entry_path)) {
            continue;
        }

        const entry_type: EntryType = switch (entry.kind) {
            .file => .file,
            .directory => .directory,
            .sym_link => .symlink,
            else => .unknown,
        };

        if (options.dirs_only and entry_type != .directory) {
            continue;
        }

        var new_entry = try Entry.init(allocator, entry.name, entry_path, entry_type);

        if (options.show_git_status and git_repo != null and git_repo.?.is_repo) {
            new_entry.git_status = git_repo.?.get_status(entry_path);
        }

        try parent.add_child(new_entry);

        if (entry_type == .directory) {
            try scan_directory(
                allocator,
                entry_path,
                depth - 1,
                &parent.children.?.items[parent.children.?.items.len - 1],
                options,
                git_repo,
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
        if (options.show_git_status and entry.git_status != .none) {
            color_code = if (entry.type == .directory)
                Color.blue.to_ansi()
            else
                Color.green.to_ansi();
        }
        reset_code = Color.reset.to_ansi();
    }

    var git_indicator: []const u8 = "";
    if (options.show_git_status and entry.git_status != .none) {
        color_code = entry.git_status.to_color().to_ansi();
        git_indicator = entry.git_status.to_indicator();
    }

    try writer.print("{s}{s}{s}{s}{s} {s}\n", .{
        prefix,
        branch,
        color_code,
        if (options.show_full_path) entry.full_path else entry.name,
        reset_code,
        git_indicator,
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
    git_repo: ?*GitRepository,
} {
    var options = Options.init();
    var paths = ArrayList([]const u8).init(allocator);
    var git_repo: ?*GitRepository = null;

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
        } else if (std.mem.eql(u8, arg, "-g") or std.mem.eql(u8, arg, "--git")) {
            options.show_git_status = true;
            if (git_repo == null) {
                const repo_ptr = try allocator.create(GitRepository);
                repo_ptr.* = GitRepository.init(allocator);
                git_repo = repo_ptr;
            }
        } else if (std.mem.eql(u8, arg, "--no-git")) {
            options.show_git_status = false;
        } else if (std.mem.eql(u8, arg, "--no-gitignore")) {
            options.respect_gitignore = false;
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

    return .{ .options = options, .paths = paths, .git_repo = git_repo };
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
        \\
        \\  Git Options:
        \\  -g, --git             Enable Git status integration (default if in repo)
        \\  --no-git              Disable Git status integration
        \\  --no-gitignore        Don't respect .gitignore patterns
        \\
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

    var git_repo: ?*GitRepository = parsed.git_repo;
    defer if (git_repo) |repo_ptr| {
        repo_ptr.*.deinit();
        allocator.destroy(repo_ptr);
    };

    for (parsed.paths.items) |path| {
        var abs_path_buffer: [std.fs.max_path_bytes]u8 = undefined;
        const abs_path = try std.fs.realpath(path, &abs_path_buffer);

        // Check if this is a git repository (if not explicitly disabled)
        if (parsed.options.show_git_status and git_repo == null) {
            var temp_repo = GitRepository.init(allocator);
            const is_repo = try temp_repo.load(abs_path);
            if (is_repo) {
                const repo_ptr = try allocator.create(GitRepository);
                repo_ptr.* = temp_repo;
                git_repo = repo_ptr;
            } else {
                temp_repo.deinit();
            }
        } else if (git_repo != null) {
            // If git_repo already exists, load this specific path
            _ = try git_repo.?.load(abs_path);
        }

        const owned_abs_path = try allocator.dupe(u8, abs_path);
        defer allocator.free(owned_abs_path);

        var root = try Entry.init(allocator, path, owned_abs_path, .directory);
        defer root.deinit(allocator);

        try scan_directory(
            allocator,
            owned_abs_path,
            parsed.options.max_depth,
            &root,
            &parsed.options,
            if (git_repo != null) git_repo.? else null,
        );
        const stdout = std.io.getStdOut().writer();

        if (git_repo != null and git_repo.?.is_repo and parsed.options.show_git_status) {
            try stdout.print("{s} [git: {s}]\n", .{ abs_path, git_repo.?.branch });
        } else {
            try stdout.print("{s}\n", .{abs_path});
        }

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
