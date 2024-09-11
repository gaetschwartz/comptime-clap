const std = @import("std");

pub const default_allocator = std.heap.page_allocator;

pub fn gpa() std.heap.GeneralPurposeAllocator(.{}) {
    return std.heap.GeneralPurposeAllocator(.{}){ .backing_allocator = default_allocator };
}

pub fn arena() std.heap.ArenaAllocator {
    return std.heap.ArenaAllocator.init(default_allocator);
}
