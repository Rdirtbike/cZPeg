const std = @import("std");
const meta = std.meta;
const Allocator = std.mem.Allocator;
const panic = std.debug.panic;
const testing = std.testing;
const expectStr = testing.expectEqualStrings;
const czpeg = @import("../czpeg.zig");

const Empty = struct { v: u8 = 0 };
var empty = Empty{};
pub var noalloc = Allocator.init(&empty, noAlloc, Allocator.NoResize(Empty).noResize, Allocator.PanicFree(Empty).noOpFree);

fn noAlloc(_: *Empty, _: usize, _: u29, _: u29, _: usize) ![]u8 {
    panic("Unsanctioned allocation during pattern match", .{});
}

pub fn chkNoM(comptime pat: anytype, str: []const u8) !void {
    var failing_allocator = testing.failing_allocator;
    var p = czpeg.Parser.init(str, &failing_allocator);
    const m = expectOk(p.match(pat));
    try expectEqual(@as(@TypeOf(m), null), m);
    try expectEqual(@as(usize, 0), p.pos());
}

pub fn chkMatch(comptime pat: anytype, str: []const u8, n: usize) !void {
    var failing_allocator = testing.failing_allocator;
    var p = czpeg.Parser.init(str, &failing_allocator);
    const m = expectOk(p.match(pat));
    try expectEqual(@as(?void, {}), m);
    try expectEqual(n, p.pos());
}

pub fn chkCap(comptime pat: anytype, str: []const u8, n: usize) !czpeg.MatchType(czpeg.Pattern.pat(pat)) {
    var allocator = testing.allocator;
    var p = czpeg.Parser.init(str, &allocator);
    const m = expectOk(p.match(pat));
    try testing.expect(m != null);
    try expectEqual(n, p.pos());
    return m.?;
}

pub fn chkError(comptime pat: anytype, str: []const u8, err: anyerror) !void {
    var allocator = testing.allocator;
    var p = czpeg.Parser.init(str, &allocator);
    try testing.expectError(err, p.match(pat));
}

pub fn expectOk(ev: anytype) switch (@typeInfo(@TypeOf(ev))) {
    .ErrorUnion => |eu| eu.payload,
    else => @TypeOf(ev),
} {
    if (comptime @typeInfo(@TypeOf(ev)) != .ErrorUnion) {
        return ev;
    } else if (ev) |v| {
        return v;
    } else |e| panic("unexpected error.{s}", .{@errorName(e)});
}

// FIXME hacking around testing.expectEqual can't compare strings?!
// FIXME should fix annoying expected type things too...
pub fn expectEqual(exp: anytype, act: @TypeOf(exp)) !void {
    switch (@typeInfo(@TypeOf(act))) {
        .Undefined, .Void, .Null => return,

        .Bool, .Int, .Float, .ComptimeFloat, .ComptimeInt, .EnumLiteral, .Enum, .Fn, .ErrorSet, .Type => if (act != exp)
            panic("expected {}, found {}", .{ exp, act }),

        .Pointer => |ptr| {
            switch (ptr.size) {
                .One, .Many, .C => if (act != exp)
                    panic("expected {*}, found {*}", .{ exp, act }),
                .Slice => {
                    if (ptr.child == u8) {
                        return expectStr(exp, act);
                    } else return testing.expectEqualSlices(ptr.child, exp, act);
                },
            }
        },

        .Array => |ary| try testing.expectEqualSlices(ary.child, &exp, &act),

        .Struct => |s| {
            inline for (s.fields) |f|
                try expectEqual(@field(exp, f.name), @field(act, f.name));
        },

        .Union => |u| {
            if (u.tag_type == null)
                @compileError("Unable to compare untagged union values");

            const Tag = meta.Tag(@TypeOf(exp));
            try expectEqual(@as(Tag, exp), @as(Tag, act));

            inline for (std.meta.fields(@TypeOf(act))) |f| {
                if (std.mem.eql(u8, f.name, @tagName(@as(Tag, act)))) {
                    try expectEqual(@field(exp, f.name), @field(act, f.name));
                    return;
                }
            }
            unreachable;
        },

        .Optional => {
            if (exp) |expval| {
                if (act) |actval| {
                    try expectEqual(expval, actval);
                } else panic("expected {any}, found null", .{expval});
            } else if (act) |actval|
                panic("expected null, found {any}", .{actval});
        },

        else => panic("unsupported type: {s}", act),
    }
}
