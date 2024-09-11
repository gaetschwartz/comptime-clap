const std = @import("std");

pub fn expectSameTag(comptime expected: anytype, actual: @TypeOf(expected)) !void {
    const expectedTag = @intFromEnum(expected);
    const actualTag = @intFromEnum(actual);

    const expectedTagName = @tagName(expected);
    const actualTagName = @tagName(actual);

    if (expectedTag != actualTag) {
        std.debug.print("Expected tag {s}, got {s}\n", .{
            expectedTagName,
            actualTagName,
        });
        return error.TestExpectedEqual;
    } else {
        // std.debug.print("GOOD: {s} == {s}\n", .{
        //     expectedTagName,
        //     actualTagName,
        // });
    }
}
