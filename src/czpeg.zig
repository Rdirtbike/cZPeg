const std = @import("std");
const meta = std.meta;
const Allocator = std.mem.Allocator;

pub const re = @import("czpeg/re.zig");
const util = @import("czpeg/util.zig");

/// Namespace for "pattern" factories.  Each pattern is a new (duck) type that
/// implements `.parse()`.  Pattern factories introspect their parameters *at
/// compile time* to generate appropriate types.  It is considered appropriate
/// to `usingnamespace czpeg.Pattern` from a local pattern/grammar namespace.
pub const Pattern = struct {
    /// match generic pattern arg, which can be one of
    ///   * pattern — returns existing pattern directly
    ///   * string — matches string literally (`str(pattern)`)
    ///   * int n >= 0 — matches exact number of characters (`any(n)`)
    ///   * int n < 0 — succeeds only if there are fewer than -n input
    ///       characters remaining (`not(any(-n))`)
    ///   * true — always succeeds without consuming any input (`any(0)`)
    ///   * false — always fails without consuming any input (`not(any(0))`)
    ///   * tuple — matches concatenated sequence of patterns (`seq(pattern)`)
    ///   * function — calls function directly to perform match
    pub fn pat(comptime pattern: anytype) type {
        return comptime switch (@typeInfo(@TypeOf(pattern))) {
            .Type => pattern,
            .Struct => seq(pattern),
            .Pointer => str(pattern),
            .ComptimeInt, .Int => if (pattern >= 0)
                any(pattern)
            else
                not(any(-pattern)),
            .Bool => if (pattern)
                any(0)
            else
                not(any(0)),
            .Fn => wrap(pattern),
            else => @compileError("Unsupported match on `" ++ @typeName(@TypeOf(pattern)) ++ "`"),
        };
    }

    fn wrap(comptime f: anytype) type {
        const R = comptime @typeInfo(@TypeOf(f)).Fn.return_type.?;
        if (comptime (R == type)) // NB parse() fns always return optional
            return f();

        const E = comptime ErrorOf(R);
        const T = comptime StripOption(StripError(R));
        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                return f(p);
            }
        };
    }

    // terminal patterns

    /// match string `pattern` literally
    pub fn str(comptime pattern: []const u8) type {
        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub usingnamespace if (pattern.len != 1) struct {} else struct {
                pub fn toCharset() type {
                    return charset(@as(u256, 1) << pattern[0]);
                }

                pub fn inv() type {
                    return @This().toCharset().inv();
                }
            };

            pub fn parse(p: *Parser) ?void {
                if (p.get(pattern.len)) |s| {
                    if (std.mem.eql(u8, s, pattern)) {
                        p.take(pattern.len);
                        return {};
                    }
                }
                return null;
            }
        };
    }

    /// match exactly `n` characters
    pub fn any(comptime n: comptime_int) type {
        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) ?void {
                if (p.get(n)) |_| {
                    p.take(n);
                    return {};
                }
                return null;
            }
        };
    }

    /// match any 1 character based on provided function
    /// (eg see std.ascii.is*)
    pub fn cls(comptime f: fn (u8) bool) type {
        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn toCharset() type {
                comptime var cs: u256 = 0;
                comptime var c = 0;

                @setEvalBranchQuota(1 << 16);
                inline while (c < 256) : (c += 1) {
                    if (comptime f(c))
                        cs |= 1 << c;
                }
                return charset(cs);
            }

            pub fn inv() type {
                return @This().toCharset().inv();
            }

            pub fn parse(p: *Parser) ?void {
                if (p.get(1)) |s| {
                    if (f(s[0])) {
                        p.take(1);
                        return {};
                    }
                }
                return null;
            }
        };
    }

    /// match any character from bit set `chars`
    pub fn charset(comptime chars_: u256) type {
        return struct {
            pub usingnamespace PatternBuilder(@This());
            pub const chars = chars_;

            pub fn toCharset() type {
                return @This();
            }

            pub fn inv() type {
                return charset(~chars);
            }

            pub fn parse(p: *Parser) ?void {
                if (p.get(1)) |s| {
                    if (chars_ & @as(u256, 1) << s[0] != 0) {
                        p.take(1);
                        return {};
                    }
                }
                return null;
            }
        };
    }

    /// match any 1 character found in string `chars`
    pub fn set(comptime chars: []const u8) type {
        if (comptime (chars.len == 0))
            return comptime pat(false);
        if (comptime (chars.len == 1))
            return comptime str(chars);

        comptime var cs: u256 = 0;
        @setEvalBranchQuota(1 << 16);
        inline for (chars) |c|
            cs |= 1 << c;

        return charset(cs);
    }

    /// match any 1 character in range `lo` to `hi` inclusive
    pub fn span(comptime lo: u8, comptime hi: u8) type {
        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn toCharset() type {
                comptime var cs: u256 = 0;
                comptime var c = lo;
                @setEvalBranchQuota(1 << 16);
                inline while (c <= hi) : (c += 1)
                    cs |= 1 << c;
                return charset(cs);
            }

            pub fn inv() type {
                return @This().toCharset().inv();
            }

            pub fn parse(p: *Parser) ?void {
                if (p.get(1)) |s| {
                    if (lo <= s[0] and s[0] <= hi) {
                        p.take(1);
                        return {};
                    }
                }
                return null;
            }
        };
    }

    // non-terminal pattern compositions

    /// indirect reference to another pattern.  used, eg, to break grammar
    /// rule reference cycles.  return type cannot be introspected (without
    /// reintroducing cycle), so must be provided.
    pub fn ref(
        comptime scope: anytype,
        comptime name: []const u8,
        comptime R: type,
    ) type {
        const E = comptime ErrorOf(R);
        const T = comptime StripError(R);

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                return @field(scope.*, name).parse(p);
            }
        };
    }

    /// matches only if pattern does not match (negative lookahead assertion)
    pub fn not(comptime pattern: anytype) type {
        const P = comptime pat(pattern);
        const E = comptime MatchError(P);
        if (comptime (MatchType(P) != void)) {
            @compileLog(MatchType(P));
            @compileError("Unsupported capture in not()");
        }

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, void) {
                const saved = p.save();
                defer p.restore(saved);

                const err = P.parse(p);
                const opt =
                    if (comptime canError(@TypeOf(err))) try err else err;
                return if (opt == null) {} else null;
            }
        };
    }

    /// matches pattern but consumes no input (positive lookahead assertion)
    pub fn if_(comptime pattern: anytype) type {
        const P = comptime pat(pattern);
        const E = comptime MatchError(P);
        const T = comptime MatchType(P);

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                const saved = p.save();
                defer p.restore(saved);

                return P.parse(p);
            }
        };
    }

    pub const Folder = struct {
        /// initial value for accumulator.  one of:
        ///   * comptime initial value of type T
        ///   * fn(Parser)T called to produce initial value (may error)
        init: anytype = {},

        /// optional function to cleanup after init() and fold().  only called
        /// on error or if partially matched pattern fails.  after calling
        /// init(), always either deinit will be called or accumulator will be
        /// returned as capture result (must not error).
        ///     fn(Parser, accumulator: *T) void
        deinit: anytype = {},

        /// function to merge new capture into current accumulator,
        /// signature like (may error):
        ///     fn(accumulator: *T, capture: anytype) void
        fold: anytype,
    };

    /// match `pattern` repeatedly, using supplied function(s) to accumulate
    /// captures.
    ///   * `nmin` to `nmax` — inclusive repetition specification.
    ///      use nmax < 0 for unbounded
    ///   * `pattern` — to fold captures of
    ///   * `folder` — functions to merge new captures
    /// returns new pattern that will produce final value of folded accumulator
    /// as capture result
    pub fn foldRep(
        comptime nmin: comptime_int,
        comptime nmax: comptime_int,
        comptime pattern: anytype,
        comptime init: anytype,
        comptime deinitFn: anytype,
        comptime foldFn: anytype,
    ) type {
        const P = comptime pat(pattern);
        const init_info = comptime @typeInfo(@TypeOf(init));
        const deinit_info = comptime @typeInfo(@TypeOf(deinitFn));
        const fold_info = comptime @typeInfo(@TypeOf(foldFn)).Fn;
        const acc_info = comptime @typeInfo(fold_info.args[0].arg_type.?);
        const A = comptime acc_info.Pointer.child;
        const T = comptime StripError(A);
        comptime var E = MatchError(P);
        // FIXME more unresolved return type hacks
        E = E || ErrorOf(fold_info.return_type orelse void);
        if (init_info == .Fn) {
            E = E || ErrorOf(init_info.Fn.return_type.?);
        } else if (init_info == .Type)
            E = E || MatchError(init);

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                const saved = p.save();
                var acc: T = switch (comptime init_info) {
                    .Fn => |f| blk: {
                        var aerr =
                            if (comptime (f.args.len == 0)) init() else init(p);
                        break :blk if (comptime canError(@TypeOf(aerr))) try aerr else aerr;
                    },
                    .Type => blk: {
                        var aerr = init.parse(p);
                        var aopt =
                            if (comptime canError(@TypeOf(aerr))) try aerr else aerr;
                        if (aopt) |a| {
                            break :blk a;
                        } else return null;
                    },
                    else => init,
                };
                errdefer if (comptime (deinit_info != .Fn)) {} else deinitFn(p, &acc);

                var m: usize = 0;
                while (comptime (nmax < 0) or m < nmax) {
                    const perr = P.parse(p);
                    const opt =
                        if (comptime canError(@TypeOf(perr))) try perr else perr;
                    if (opt) |c| {
                        const ferr = foldFn(&acc, c);
                        if (comptime canError(@TypeOf(ferr))) try ferr;
                    } else break;
                    m += 1;
                }

                if (m < nmin) {
                    if (comptime (deinit_info == .Fn))
                        deinitFn(p, &acc);
                    p.restore(saved);
                    return null;
                }
                return acc;
            }
        };
    }

    fn foldVoid(_: *void, _: void) void {}

    fn optionFolder(comptime T: type) Folder {
        const F = struct {
            fn fold(acc: *?T, c: T) void {
                std.debug.assert(acc.* == null);
                acc.* = c;
            }
        };

        return .{
            .init = @as(?T, null),
            .fold = F.fold,
        };
    }

    fn arrayFolder(comptime T: type) Folder {
        const List = std.ArrayList(T);

        const F = struct {
            fn init(p: *Parser) List {
                return List.init(p.alloc.*);
            }

            fn deinit(_: *Parser, acc: *List) void {
                acc.deinit();
            }

            fn fold(acc: *List, c: T) !void {
                try acc.append(c);
            }
        };

        return .{
            .init = F.init,
            .deinit = F.deinit,
            .fold = F.fold,
        };
    }

    /// match `pattern` repeatedly
    ///   * `nmin` to `nmax` — inclusive repitition specification.
    ///      use nmax < 0 for unbounded
    ///   * captures nothing if pattern does not capture, otherwise
    ///   * captures optional if nmin < nmax == 1, otherwise
    ///   * allocates and captures slice for variable capture count
    pub fn rep(
        comptime nmin: comptime_int,
        comptime nmax: comptime_int,
        comptime pattern: anytype,
    ) type {
        const P = comptime pat(pattern);
        const M = comptime MatchType(P);

        if (comptime (M == void))
            return foldRep(nmin, nmax, P, {}, {}, foldVoid);

        if (comptime (nmin < nmax and nmax == 1)) {
            return foldRep(nmin, nmax, P, @as(?M, null), {}, optionFolder(M).fold);
        } else {
            const fns = arrayFolder(M);
            return foldRep(nmin, nmax, P, fns.init, fns.deinit, fns.fold);
        }
    }

    /// match `pattern` at least `n` times with no (specified) upper bound.
    /// `more(n, p)` is preferred alternative to `rep(n, -1, p)`.
    /// `more(0, p)` is equivalent to PEG `*`,
    /// `more(1, p)` is equivalent to PEG `+`
    pub fn more(comptime n: comptime_int, comptime pattern: anytype) type {
        return rep(n, -1, pattern);
    }

    /// match concatenation of 2 patterns (special case of .seq() with 2 args).
    /// prefer .seq() for multiple concatenations.
    pub fn cat(comptime pat0: anytype, comptime pat1: anytype) type {
        return seq(.{ pat0, pat1 });
    }

    /// match concatenation of all patterns in `args` tuple
    pub fn seq(comptime args: anytype) type {
        if (@typeInfo(@TypeOf(args)) != .Struct)
            @compileError("Expected tuple of patterns, found '" ++ @typeName(@TypeOf(args)) ++ "'");
        switch (args.len) {
            0 => return pat(true),
            1 => return pat(args[0]),
            else => {},
        }

        // filter match results w/captures
        comptime var E: type = error{};
        comptime var Pats: [args.len]type = undefined;
        comptime var Caps: [args.len]type = undefined;
        comptime var ncaps = 0;
        inline for (args) |arg, i| {
            // within sequence, nested tuple alternates to alt
            Pats[i] = if (comptime @typeInfo(@TypeOf(arg)) == .Struct)
                alt(arg)
            else
                pat(arg);

            E = E || MatchError(Pats[i]);
            const M = comptime MatchType(Pats[i]);
            if (comptime (M != void)) {
                Caps[ncaps] = M;
                ncaps += 1;
            }
        }

        // reduce or gen tuple for mutiple results
        const T = comptime switch (ncaps) {
            0 => void,
            1 => Caps[0],
            else => meta.Tuple(Caps[0..ncaps]),
        };

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                var caps: T = undefined;
                const saved = p.save();
                comptime var j = 0;

                inline for (Pats) |P| {
                    const err = P.parse(p);
                    const opt =
                        if (comptime canError(@TypeOf(err))) try err else err;
                    if (opt == null) {
                        p.restore(saved);
                        return null;
                    }

                    if (comptime (MatchType(P) != void))
                        switch (ncaps) {
                            0 => unreachable,
                            1 => caps = opt.?,
                            else => {
                                comptime var buf: [128]u8 = undefined;
                                const nm = comptime std.fmt.bufPrint(&buf, "{d}", .{j}) catch unreachable;
                                @field(caps, nm) = opt.?;
                                j += 1;
                            },
                        };
                }
                return caps;
            }
        };
    }

    /// match *first* of ordered list of choices in `args` tuple
    pub fn alt(comptime args: anytype) type {
        if (@typeInfo(@TypeOf(args)) != .Struct)
            @compileError("Expected tuple of patterns, found '" ++ @typeName(@TypeOf(args)) ++ "'");
        switch (args.len) {
            0 => return pat(true),
            1 => return pat(args[0]),
            else => {},
        }

        comptime var Pats: [args.len]type = undefined;
        Pats[0] = pat(args[0]);
        comptime var is_charset = @hasDecl(Pats[0], "toCharset");
        comptime var E = MatchError(Pats[0]);
        const T = comptime MatchType(Pats[0]);

        inline for (args) |arg, i| {
            if (i > 0) {
                Pats[i] = pat(arg);
                is_charset = is_charset and @hasDecl(Pats[i], "toCharset");
                E = E || MatchError(Pats[i]);

                if (MatchType(Pats[i]) != T)
                    @compileError("Heterogeneous choice captures unsupported" ++ " (try capturing each choice to a tagged union) " ++ @typeName(Pats[i]) ++ " != " ++ @typeName(T));
            }
        }

        // optimize case of merged character sets (or patterns that can be)
        if (comptime is_charset) {
            comptime var cs: u256 = 0;
            inline for (Pats) |P|
                cs |= P.toCharset().chars;
            return charset(cs);
        }

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                const saved = p.save();
                inline for (Pats) |P| {
                    const err = P.parse(p);
                    const opt =
                        if (comptime canError(@TypeOf(err))) try err else err;
                    if (opt) |c|
                        return c;
                    p.restore(saved);
                }
                return null;
            }
        };
    }

    // captures

    /// capture offset from start of matched string to end of pattern.
    /// always matches without consuming input.
    pub fn pos() type {
        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) ?usize {
                return p.pos();
            }
        };
    }

    /// capture string matched by pattern as slice.  if pattern already
    /// captures, slice and capture will be wrapped in new tuple.
    /// returned slice is only valid until parse buffer is updated.
    pub fn cap(comptime pattern: anytype) type {
        const P = comptime pat(pattern);
        const E = comptime MatchError(P);
        const M = comptime MatchType(P);
        const T = comptime if (M == void) []const u8 else meta.Tuple(&.{ []const u8, M });

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                const start = p.save();
                const err = P.parse(p);
                const opt =
                    if (comptime canError(@TypeOf(err))) try err else err;
                if (opt) |sub| {
                    const c = p.buf[start.idx..p.pos()];
                    return if (comptime (M == void)) c else T{ .@"0" = c, .@"1" = sub };
                }
                return null;
            }
        };
    }

    // FIXME can't make TypeInfo.Fn.return_type resolve reliably.  so, for now,
    // need to specify *full* (including any error) return type of `f` as R

    /// transform result of matching pattern using another function.
    /// if pattern has no captures, matched string will be passed instead.
    /// function that returns optional null will fail to match.
    pub fn grok(comptime R: type, comptime f: anytype, comptime pattern: anytype) type {
        const P = comptime pat(pattern);
        const M = comptime MatchType(P);
        const E = comptime MatchError(P) || ErrorOf(R);
        const T = comptime StripOption(StripError(R));

        return struct {
            pub usingnamespace PatternBuilder(@This());

            pub fn parse(p: *Parser) MatchReturn(E, T) {
                const start =
                    if (comptime (M != void))
                {} else p.save();
                const perr = P.parse(p);
                const opt =
                    if (comptime canError(@TypeOf(perr))) try perr else perr;
                if (opt) |sub| {
                    const ferr =
                        if (comptime (M != void)) f(sub) else if (comptime (@typeInfo(@TypeOf(f)).Fn.args.len == 0)) f() else f(p.buf[start.idx..p.pos()]);
                    const v =
                        if (comptime canError(@TypeOf(ferr))) try ferr else ferr;
                    return v;
                }
                return null;
            }
        };
    }
};

/// generate adapters for using builder pattern with Patterns...
fn PatternBuilder(comptime Self_: type) type {
    const P = Pattern;

    return struct {
        const Self = Self_;

        /// concatenate another pattern after this.  see Pattern.cat().
        /// prefer Pattern.seq() for multiple concatenations.
        pub fn _(comptime pattern: anytype) type {
            return P.cat(Self, pattern);
        }

        /// concatenate character set after this.  see Pattern.set().
        /// prefer Pattern.seq() for multiple concatenations.
        pub fn set(comptime chars: []const u8) type {
            return P.cat(Self, P.set(chars));
        }

        /// concatenate character span after this.  see Pattern.span().
        /// prefer Pattern.seq() for multiple concatenations.
        pub fn span(comptime lo: u8, comptime hi: u8) type {
            return P.cat(Self, P.span(lo, hi));
        }

        /// match only if this pattern does not match.
        /// prefer Pattern.not() for clarity.
        pub fn not() type {
            return P.not(Self);
        }

        /// match this pattern without consuming input.
        /// prefer Pattern.if_() for clarity.
        pub fn if_() type {
            return P.if_(Self);
        }

        pub fn foldRep(
            comptime nmin: comptime_int,
            comptime nmax: comptime_int,
            comptime init: anytype,
            comptime deinitFn: anytype,
            comptime foldFn: anytype,
        ) type {
            return P.foldRep(nmin, nmax, Self, init, deinitFn, foldFn);
        }

        /// match this pattern repeatedly.
        /// prefer this to using Pattern.rep() directly.
        pub fn rep(comptime nmin: comptime_int, comptime nmax: comptime_int) type {
            return P.rep(nmin, nmax, Self);
        }

        /// match this pattern at least `n` times with no (specified) upper
        /// bound.  `more(n)` is preferred alternative to `rep(n, -1)`
        /// `more(0)` is equivalent to PEG `*`,
        /// `more(1)` is equivalent to PEG `+`.
        /// Prefer this to using Pattern.more() directly.
        pub fn more(comptime n: comptime_int) type {
            return P.more(n, Self);
        }

        /// try to match another choice if this does not match.
        /// prefer Pattern.alt() for > 2 choices.
        pub fn or_(comptime pattern: anytype) type {
            return P.alt(Self, pattern);
        }

        /// capture position after matching this pattern.  see Pattern.pos()
        pub fn pos() type {
            return P.cat(Self, P.pos());
        }

        /// capture this pattern as string slice.  see Pattern.cap()
        pub fn cap() type {
            return P.cap(Self);
        }

        /// transform string matched by this pattern.  see Pattern.grok()
        pub fn grok(comptime R: type, comptime f: anytype) type {
            return P.grok(R, f, Self);
        }

        /// match this pattern against provided string, returning any captures.
        pub fn match(str: []const u8, alloc: *Allocator) MatchReturn(MatchError(Self), MatchType(Self)) {
            var p = Parser.init(str, alloc);
            return p.match(Self);
        }

        /// match this pattern against provided string, returning any captures.
        /// Allocations are disallowed; any attempt to allocate will panic.
        pub fn matchLean(str: []const u8) MatchReturn(MatchError(Self), MatchType(Self)) {
            var p = Parser.init(str, &util.noalloc);
            return p.match(Self);
        }
    };
}

// patterns may optionally produce an error and can always fail to match.
// match result is superseded by any error and these need to be swizzled
// during result composition.

/// introspect capture result type from Pattern, *without* error or optional
pub fn MatchType(comptime P: type) type {
    const R = comptime StripError(@typeInfo(@TypeOf(P.parse)).Fn.return_type.?);
    return comptime switch (@typeInfo(R)) {
        .Optional => |opt| opt.child,
        else => @compileError("Expected Optional capture type, not " ++ @typeName(R)),
    };
}

/// introspect capture error set from Pattern or default to empty error set
fn MatchError(comptime P: type) type {
    return comptime ErrorOf(@typeInfo(@TypeOf(P.parse)).Fn.return_type.?);
}

/// rewrap new capture result in option and optional error
fn MatchReturn(comptime E: type, comptime T: type) type {
    return comptime if (isError(E)) E!?T else ?T;
}

/// extract payload from outer error union or return type directly
fn StripError(comptime T: type) type {
    return comptime switch (@typeInfo(T)) {
        .ErrorUnion => |eu| eu.payload,
        .ErrorSet => void,
        else => T,
    };
}

/// extract payload from optional or return type directly
fn StripOption(comptime T: type) type {
    return comptime switch (@typeInfo(T)) {
        .Optional => |opt| opt.child,
        else => T,
    };
}

/// extract error set from outer error union or return empty error set
fn ErrorOf(comptime T: type) type {
    return comptime switch (@typeInfo(T)) {
        .ErrorUnion => |eu| eu.error_set,
        .ErrorSet => T,
        else => error{},
    };
}

/// detect empty error sets
fn isError(comptime Error: type) bool {
    return comptime switch (@typeInfo(Error)) {
        .ErrorUnion => |eu| isError(eu.error_set),
        .ErrorSet => |errset| if (errset) |errs|
            errs.len > 0
        else
            true,
        else => false,
    };
}

fn canError(comptime T: type) bool {
    return comptime (@typeInfo(T) == .ErrorUnion);
}

pub const Parser = struct {
    buf: []const u8,
    alloc: *Allocator,
    state: State,

    const State = struct {
        idx: usize = 0,
    };

    pub fn init(str: []const u8, allocator: *Allocator) @This() {
        return .{
            .buf = str,
            .alloc = allocator,
            .state = .{},
        };
    }

    /// match current string against provided pattern
    pub fn match(self: *@This(), comptime pattern: anytype) blk: {
        const P = Pattern.pat(pattern);
        @setEvalBranchQuota(1 << 20);
        break :blk MatchReturn(MatchError(P), MatchType(P));
    } {
        return Pattern.pat(pattern).parse(self);
    }

    pub fn pos(self: *@This()) usize {
        return self.state.idx;
    }

    pub fn get(p: *@This(), n: usize) ?[]const u8 {
        if (p.state.idx + n <= p.buf.len)
            return p.buf[p.state.idx .. p.state.idx + n];
        return null;
    }

    fn take(p: *@This(), n: usize) void {
        p.state.idx += n;
        std.debug.assert(p.state.idx <= p.buf.len);
    }

    fn save(self: *@This()) State {
        return self.state;
    }

    fn restore(self: *@This(), prev: State) void {
        self.state = prev;
    }
};

//----------------------------------------------------------------------------
const testing = std.testing;
const expectStr = testing.expectEqualStrings;
const expectEqual = util.expectEqual;
const chkNoM = util.chkNoM;
const chkMatch = util.chkMatch;
const chkCap = util.chkCap;
const chkError = util.chkError;

fn injectError(_: *Parser) !?void {
    return error.Injected;
}

fn parseU32Dec(s: []const u8) !u32 {
    return std.fmt.parseInt(u32, s, 10);
}

test "Pattern.str" {
    const str = Pattern.str;
    try chkMatch(str(""), "", 0);
    try chkMatch(str(""), "abc", 0);

    try chkNoM(str("a"), "");
    try chkNoM(str("a"), "b");
    try chkNoM(str("abc"), "ab");

    try chkMatch(str("a"), "a", 1);
    try chkMatch(str("ab"), "abc", 2);

    try testing.expect(!@hasDecl(str(""), "toCharset"));
    try testing.expect(!@hasDecl(str("aa"), "toCharset"));

    const cs = str("a").toCharset();
    try chkNoM(cs, "");
    try chkNoM(cs, "b");
    try chkMatch(cs, "ab", 1);
}

test "Pattern.any" {
    const any = Pattern.any;
    try chkMatch(any(0), "", 0);

    try chkNoM(any(1), "");
    try chkMatch(any(1), "a", 1);
    try chkMatch(any(1), "b", 1);
    try chkMatch(any(1), "abc", 1);

    try chkMatch(any(2), "xyz", 2);
}

test "Pattern.set" {
    const set = Pattern.set;

    try chkNoM(set(""), "");
    try chkNoM(set(""), "a");

    try chkNoM(set(""), "b");
    try chkNoM(set("a"), "b");
    try chkMatch(set("a"), "ab", 1);

    const vowel = set("aeiou");
    try chkNoM(vowel, "");
    try chkNoM(vowel, "fail");
    try chkMatch(vowel, "abcde", 1);
    try chkMatch(vowel, "ignore", 1);
    try chkMatch(vowel, "oauie", 1);
    try chkMatch(vowel, "under", 1);

    try expectEqual(vowel, vowel.toCharset());
}

test "Pattern.cls" {
    const G = struct {
        fn even(c: u8) bool {
            return c & 1 == 0;
        }
    };
    const p = Pattern.cls(G.even);
    const cs = p.toCharset();

    try chkNoM(p, "");
    try chkNoM(cs, "");

    comptime var i = 1;
    inline while (i < 256) : (i += 2) {
        try chkNoM(p, &.{i});
        try chkNoM(cs, &.{i});
    }
    try chkMatch(p.more(1), "\x00\x02\x04\x08\x10\x20\x40\x80\xfe", 9);
    try chkMatch(cs.more(1), "\x00\x02\x04\x08\x10\x20\x40\x80\xfe", 9);
}

test "Pattern.span" {
    const dig = Pattern.span('0', '9');
    try chkNoM(dig, "");
    try chkNoM(dig, "!");
    try chkNoM(dig, "fail");
    try chkMatch(dig, "0abc", 1);
    try chkMatch(dig, "9876", 1);
    try chkMatch(dig, "42", 1);

    const cs = dig.toCharset();
    try chkNoM(cs, "");
    try chkNoM(cs, "()");
    try chkNoM(cs, "f");
    try chkMatch(cs, "77", 1);
}

test "Pattern.not" {
    const not = Pattern.not;

    try chkNoM(not(0), "");
    try chkNoM(not(0), "a");

    try chkNoM(not(1), "a");
    try chkMatch(not(1), "", 0);

    try chkNoM(not("a"), "a");
    try chkNoM(not("a"), "ab");
    try chkMatch(not("a"), "", 0);
    try chkMatch(not("a"), "b", 0);
    try chkMatch(not("a"), "ba", 0);

    try chkNoM(not("ab"), "ab");
    try chkNoM(not("ab"), "abc");
    try chkMatch(not("ab"), "", 0);
    try chkMatch(not("ab"), "a", 0);
    try chkMatch(not("ab"), "ba", 0);

    try chkError(not(injectError), "", error.Injected);
}

test "Pattern.if_" {
    const if_ = Pattern.if_;

    try chkMatch(if_(0), "", 0);
    try chkMatch(if_(0), "a", 0);

    try chkNoM(if_("a"), "b");
    try chkMatch(if_("a"), "a", 0);

    try chkError(if_(injectError), "", error.Injected);
}

test "Pattern.if_ capture" {
    const G = struct {
        const p = Pattern.seq(.{ "abc", Pattern.if_(Pattern.cap("123")) });
    };
    try chkNoM(G.p, "abc12xyz");
    try expectStr("123", try chkCap(G.p, "abc123xyz", 3));
}

test "Pattern.pat" {
    const pat = Pattern.pat;

    // pass thru
    const p = pat(3);
    try expectEqual(p, pat(p));
    try chkNoM(pat(p), "");
    try chkNoM(pat(p), "12");
    try chkMatch(pat(p), "123", 3);
    try chkMatch(pat(p), "12345", 3);

    // str
    try chkNoM(pat("a"), "b");
    try chkMatch(pat("a"), "a", 1);
    try chkMatch(pat("ab"), "abc", 2);
    try chkNoM(pat("abc"), "ab");

    // any(n) n >= 0
    try chkMatch(pat(0), "", 0);
    try chkNoM(pat(1), "");
    try chkMatch(pat(1), "a", 1);
    try chkMatch(pat(1), "b", 1);
    try chkMatch(pat(1), "abc", 1);
    try chkMatch(pat(2), "xyz", 2);

    // any(n) n < 0
    try chkMatch(pat(-1), "", 0);
    try chkNoM(pat(-1), "a");
    try chkMatch(pat(-2), "", 0);
    try chkMatch(pat(-2), "a", 0);
    try chkNoM(pat(-2), "ab");

    // bool
    try chkMatch(pat(true), "", 0);
    try chkMatch(pat(true), "abc", 0);
    try chkNoM(pat(false), "");
    try chkNoM(pat(false), "abc");

    // seq
    const s = pat(.{ "a", 1, "c" });
    try chkNoM(s, "");
    try chkNoM(s, "ab");
    try chkNoM(s, "abb");
    try chkMatch(s, "abcd", 3);
    try chkMatch(s, "aacc", 3);
    try chkMatch(s, "acca", 3);
}

test "except" {
    const p = Pattern.seq(.{ Pattern.if_("abc"), "ab" });
    try chkNoM(p, "ab");
    try chkNoM(p, "aba");
    try chkMatch(p, "abc", 2);
    try chkMatch(p, "abcde", 2);
}

test "Pattern.rep" {
    const rep = Pattern.rep;

    const a0 = rep(0, -1, "a");
    try chkMatch(a0, "", 0);
    try chkMatch(a0, "a", 1);
    try chkMatch(a0, "aa", 2);
    try chkMatch(a0, "aabaa", 2);
    try chkMatch(a0, "aaaaaaaaaa", 10);
    try chkMatch(a0, "aaaaaaaaaab", 10);

    const a1 = rep(1, -1, "a");
    try chkNoM(a1, "");
    try chkNoM(a1, "b");
    try chkMatch(a1, "a", 1);
    try chkMatch(a1, "aaa", 3);
    try chkMatch(a1, "aaaaaaiit", 6);

    const ab2 = rep(2, -1, "ab");
    try chkNoM(ab2, "");
    try chkNoM(ab2, "ab");
    try chkMatch(ab2, "abab", 4);
    try chkMatch(ab2, "ababab", 6);

    const ab_1 = rep(0, 1, Pattern.span('a', 'b'));
    try chkMatch(ab_1, "", 0);
    try chkMatch(ab_1, "A", 0);
    try chkMatch(ab_1, "z", 0);
    try chkMatch(ab_1, "a", 1);
    try chkMatch(ab_1, "b", 1);
    try chkMatch(ab_1, "ab", 1);

    const p2_2 = rep(0, 2, 2);
    try chkMatch(p2_2, "", 0);
    try chkMatch(p2_2, "a", 0);
    try chkMatch(p2_2, "aa", 2);
    try chkMatch(p2_2, "aaa", 2);
    try chkMatch(p2_2, "bbbbb", 4);
    try chkMatch(p2_2, "bbbbbbbbbbb", 4);

    try chkError(rep(3, -1, injectError), "", error.Injected);
}

test "Pattern.rep capture" {
    const rep = Pattern.rep;
    const cap = Pattern.cap;
    const span = Pattern.span;

    const a_1 = rep(0, 1, cap("a"));
    try expectEqual(@as(?[]const u8, null), try chkCap(a_1, "", 0));
    try expectEqual(@as(?[]const u8, null), try chkCap(a_1, "b", 0));
    try expectEqual(@as(?[]const u8, "a"), try chkCap(a_1, "a", 1));
    try expectEqual(@as(?[]const u8, "a"), try chkCap(a_1, "aaa", 1));

    const nums = rep(1, -1, .{ rep(1, -1, .{ rep(0, 1, "-"), span('0', '9') })
        .grok(error{ Overflow, InvalidCharacter }!u32, parseU32Dec), rep(0, 1, ",") });
    try chkNoM(nums, "");
    try chkNoM(nums, ",");
    try chkNoM(nums, "z");
    {
        var act = try chkCap(nums, "42", 2);
        try testing.expectEqualSlices(u32, &[_]u32{42}, act.items);
        act.deinit();
    }
    {
        var act = try chkCap(nums, "123,456,789 ", 11);
        try testing.expectEqualSlices(u32, &[_]u32{ 123, 456, 789 }, act.items);
        act.deinit();
    }

    try chkError(nums, "123,-123,456", error.Overflow);
    try chkError(nums, "123,1-23,456", error.InvalidCharacter);
}

test "Pattern.foldRep" {
    const P = Pattern;
    const Count = struct {
        fn initCount() usize {
            return 42;
        }

        fn foldCount(acc: *usize, _: void) void {
            acc.* += 1;
        }

        const a = P.foldRep(0, -1, "a", 0, {}, foldCount);
        const b = P.foldRep(1, -1, "b", initCount, {}, foldCount);
        const cd = P.foldRep(2, 4, "d", P.pat("c").grok(usize, initCount), {}, foldCount);

        const initerr = P.foldRep(0, -1, "xxx", P.pat(injectError), {}, P.foldVoid);
    };

    try expectEqual(@as(usize, 0), try chkCap(Count.a, "", 0));
    try expectEqual(@as(usize, 2), try chkCap(Count.a, "aab", 2));
    try expectEqual(@as(usize, 3), try chkCap(Count.a, "aaa", 3));

    try chkNoM(Count.b, "");
    try chkNoM(Count.b, "a");
    try expectEqual(@as(usize, 43), try chkCap(Count.b, "b", 1));
    try expectEqual(@as(usize, 44), try chkCap(Count.b, "bba", 2));

    try chkNoM(Count.cd, "d");
    try chkNoM(Count.cd, "cd");
    try expectEqual(@as(usize, 44), try chkCap(Count.cd, "cdd", 3));
    try expectEqual(@as(usize, 44), try chkCap(Count.cd, "cddc", 3));
    try expectEqual(@as(usize, 45), try chkCap(Count.cd, "cddd", 4));
    try expectEqual(@as(usize, 46), try chkCap(Count.cd, "cdddd", 5));
    try expectEqual(@as(usize, 46), try chkCap(Count.cd, "cddddd", 5));

    try chkError(Count.initerr, "", error.Injected);
}

test "Pattern.seq" {
    const seq = Pattern.seq;
    {
        const p = seq(.{ "abc", "xyz" });
        try chkNoM(p, "");
        try chkNoM(p, "abc");
        try chkNoM(p, "xyz");
        try chkNoM(p, "xyzabc");
        try chkMatch(p, "abcxyz123", 6);
    }
    {
        const p = seq(.{ "12", "34", "56" });
        try chkNoM(p, "");
        try chkNoM(p, "12 ");
        try chkNoM(p, "1234z");
        try chkNoM(p, "123467");
        try chkMatch(p, "123456", 6);
    }
    {
        const p = seq(.{ 1, "b", -1 }); // ".b$"
        try chkNoM(p, "abc");
        try chkMatch(p, "ab", 2);
        try chkNoM(p, "ba");
        try chkMatch(p, "zb", 2);
    }
    {
        const p = seq(.{ "abc", injectError, "xyz" });
        try chkNoM(p, "");
        try chkError(p, "abc123xyz", error.Injected);
    }
}

test "Pattern.seq capture" {
    const seq = Pattern.seq;
    const cap = Pattern.cap;
    {
        const p = seq(.{ "abc", cap("123"), "xyz" });
        try chkNoM(p, "xxx");
        try chkNoM(p, "abcxxx");
        try chkNoM(p, "abc123xxx");
        try expectStr("123", try chkCap(p, "abc123xyz ", 9));
    }
    {
        const p = seq(.{ cap("abc"), ",", cap("123"), ",", cap("xyz") });
        try chkNoM(p, "a");
        try chkNoM(p, "abc123xyz");
        try chkNoM(p, "abc,1");
        try chkNoM(p, "abc,123xyz");
        try chkNoM(p, "abc,123, ");
        const act = try chkCap(p, "abc,123,xyz ", 11);
        try expectStr("abc", act[0]);
        try expectStr("123", act[1]);
        try expectStr("xyz", act[2]);
    }
    {
        const p = seq(.{ cap("abc"), ",", cap("123"), ",", injectError });
        try chkNoM(p, "");
        try chkNoM(p, "abc.");
        try chkNoM(p, "abc,xxx");
        try chkNoM(p, "abc,123.");
        try chkError(p, "abc,123,xyz", error.Injected);
    }
}

test "Pattern.alt" {
    const alt = Pattern.alt;
    {
        const p = alt(.{ "abc", "xyz" });
        try chkNoM(p, "");
        try chkNoM(p, "azbycz");
        try chkMatch(p, "abc", 3);
        try chkMatch(p, "xyz", 3);
        try chkMatch(p, "abcxyz", 3);
        try chkMatch(p, "abcabc", 3);
    }
    {
        const p = alt(.{ "abc", injectError });
        try chkMatch(p, "abcd", 3);
        try chkError(p, "a", error.Injected);
    }
}

test "alt charsets" {
    const P = Pattern;
    const p = P.alt(.{ "a", P.set("bc"), P.span('1', '3') });
    try testing.expect(@hasDecl(p, "toCharset"));
    try chkNoM(p, "");
    try chkNoM(p, "d");
    try chkNoM(p, "0");
    try chkNoM(p, "4");
    try chkMatch(p.more(0), "abc123", 6);
}

test "Pattern.pos" {
    const pat = Pattern.pat;
    try expectEqual(@as(usize, 0), try chkCap(pat(0).pos(), "", 0));

    const p = pat(.{ pat("a").more(0).pos(), pat("b").more(0) });
    try expectEqual(@as(usize, 0), try chkCap(p, "", 0));
    try expectEqual(@as(usize, 1), try chkCap(p, "ac", 1));
    try expectEqual(@as(usize, 2), try chkCap(p, "aabb ", 4));
}

test "Pattern.cap" {
    const G = struct {
        // [a-c]*([x-z]+)[a-c]*$
        const p = Pattern.pat(.{
            Pattern.span('a', 'c').more(0),
            Pattern.span('x', 'z').more(1).cap(),
            Pattern.span('a', 'c').more(0),
        });

        const err = Pattern.cap(injectError);
    };

    try chkNoM(G.p, "");
    try chkNoM(G.p, "AXZ");
    try chkNoM(G.p, "{x}");
    try chkNoM(G.p, "bZ2");
    try chkNoM(G.p, "b|2");
    try chkNoM(G.p, "aabb");
    try expectStr("x", try chkCap(G.p, "xZ", 1));
    try expectStr("x", try chkCap(G.p, "x~", 1));
    try expectStr("xyz", try chkCap(G.p, "abcxyzabc", 9));
    try expectStr("yy", try chkCap(G.p, "aayyccxyz", 6));

    try chkError(G.err, "", error.Injected);
}

test "Pattern.cap nested" {
    const G = struct {
        const p = Pattern.seq(.{
            Pattern.set("a").inv().more(0),
            Pattern.set("a").more(1).cap(),
            Pattern.set("a").inv().more(0),
        }).cap();
    };

    try chkNoM(G.p, "");
    try chkNoM(G.p, "xyzbbb");
    {
        const act = try chkCap(G.p, "xaaax", 5);
        try expectEqual(@as(usize, 2), act.len);
        try expectStr("xaaax", act[0]);
        try expectStr("aaa", act[1]);
    }
    {
        const act = try chkCap(G.p, "aaxxaa", 4);
        try expectEqual(@as(usize, 2), act.len);
        try expectStr("aaxx", act[0]);
        try expectStr("aa", act[1]);
    }
}

test "Pattern.grok" {
    const G = struct {
        const p = Pattern.pat(.{
            Pattern.span('a', 'z').more(1),
            "=",
            Pattern.rep(0, 1, "-").span('0', '9').more(1)
                .grok(error{ Overflow, InvalidCharacter }!u32, parseU32Dec),
        });
    };

    try chkNoM(G.p, "");
    try chkNoM(G.p, "=");
    try chkNoM(G.p, "x");
    try chkNoM(G.p, "x=");
    try chkNoM(G.p, "x=a");
    try expectEqual(@as(u32, 1), try chkCap(G.p, "a=1 ", 3));
    try expectEqual(@as(u32, 123), try chkCap(G.p, "abc=123xyz", 7));

    try chkError(G.p, "xyz=-42", error.Overflow);
    try chkError(G.p, "xyz=0-0", error.InvalidCharacter);
}

test "Pattern.grok noargs" {
    const G = struct {
        fn noargs() bool {
            return true;
        }
        const p = Pattern.grok(bool, noargs, Pattern.any(1));
    };

    try chkNoM(G.p, "");
    try expectEqual(true, try chkCap(G.p, ".", 1));
}

test "Pattern.grok sub" {
    const P = Pattern;
    const Ass = struct {
        name: []const u8,
        val: u32,

        fn grok(spec: anytype) @This() {
            return .{
                .name = spec[0],
                .val = spec[1],
            };
        }

        const p = P.seq(.{
            P.span('a', 'z').more(1).cap(),
            "=",
            P.span('0', ':').more(1).grok(anyerror!u32, parseU32Dec),
        }).grok(@This(), grok);
    };

    try chkNoM(Ass.p, "");
    try chkNoM(Ass.p, "no");
    try chkNoM(Ass.p, "x=");
    try expectEqual(Ass{ .name = "abc", .val = 42 }, try chkCap(Ass.p, "abc=42", 6));

    try chkError(Ass.p, "a=:5", error.InvalidCharacter);
}

test "blind greedy" {
    const G = struct {
        const p = Pattern.pat(.{ Pattern.span('a', 'z').more(0), "1" });
    };
    try chkMatch(G.p, "count123", 6);
    try chkNoM(G.p, "count2");
}

test "non-blind greedy" {
    const G = struct {
        const lastdig = Pattern.alt(.{ .{ 1, lastdigref }, Pattern.span('0', '9') });
        const lastdigref = Pattern.ref(&@This(), "lastdig", void);
    };

    try chkNoM(G.lastdig, "abcxyz");
    try chkMatch(G.lastdig, "abc123xyz", 6);
}

test "non-blind non-greedy c comment grammar" {
    const G = struct {
        const comment = Pattern.cat("/*", close);
        const close = Pattern.alt(.{
            "*/",
            .{ 1, closeref },
        });
        const closeref = Pattern.ref(&@This(), "close", void);
    };

    try chkNoM(G.comment, "");
    try chkNoM(G.comment, "/* junk");
    try chkNoM(G.comment, "/*/");

    try chkMatch(G.close, "*/abc", 2);
    try chkMatch(G.close, "aa*/bb", 4);
    try chkMatch(G.comment, "/**/", 4);
    try chkMatch(G.comment, "/*aa*/bb", 6);
}

test "c comment, negative lookahead" {
    const seq = Pattern.seq;
    const not = Pattern.not;
    const com = seq(.{ "/*", (not("*/")._(1)).more(0), "*/" });

    try chkNoM(com, "");
    try chkNoM(com, "a");
    try chkNoM(com, "/*");
    try chkNoM(com, "/*/");
    try chkNoM(com, "*/");

    try chkMatch(com, "/**/", 4);
    try chkMatch(com, "/* */*/", 5);
}

test "predication" {
    const G = struct {
        // !((’int’ / ’float’) ![a-z]) [a-z]+
        const p = Pattern.pat(.{
            Pattern.not(.{ .{ "int", "float" }, Pattern.not(Pattern.span('a', 'z')) }),
            Pattern.span('a', 'z').more(1),
        });
    };

    try chkNoM(G.p, "int");
    try chkNoM(G.p, "float");
    try chkNoM(G.p, "int64");
    try chkNoM(G.p, "float32");
    try chkMatch(G.p, "in", 2);
    try chkMatch(G.p, "floof", 5);
    try chkMatch(G.p, "floaty", 6);
    try chkMatch(G.p, "intu ", 4);
    try chkMatch(G.p, "uint123", 4);
}

// FIXME how to detect grammars that are not well formed (left recursive)?
// (currently they just crash (probably stack overflow?))

test "even 0s even 1s" {
    const G = struct {
        // EE <- ’0’ OE / ’1’ EO / !.
        // OE <- ’0’ EE / ’1’ OO
        // EO <- ’0’ OO / ’1’ EE
        // OO <- ’0’ EO / ’1’ OE

        // add enough manual refs to break cycles
        const eeref = Pattern.ref(&@This(), "ee", void);
        const ooref = Pattern.ref(&@This(), "oo", void);

        const ee = Pattern.alt(.{ .{ "0", oe }, .{ "1", eo }, -1 });
        const oe = Pattern.alt(.{ .{ "0", eeref }, .{ "1", ooref } });
        const eo = Pattern.alt(.{ .{ "0", ooref }, .{ "1", eeref } });
        const oo = Pattern.alt(.{ .{ "0", eo }, .{ "1", oe } });
    };

    try chkMatch(G.ee, "", 0);
    try chkNoM(G.ee, "0");
    try chkNoM(G.ee, "1");
    try chkNoM(G.ee, "01");
    try chkNoM(G.ee, "10");
    try chkMatch(G.ee, "00", 2);
    try chkMatch(G.ee, "11", 2);
    try chkNoM(G.ee, "11z");
    try chkMatch(G.ee, "0101001110101100", 16);
    try chkNoM(G.ee, "0101100101");
}

test "re" {
    _ = re;
}
