const std = @import("std");
const builtin = @import("builtin");

pub const FileFd = struct {
    const Inner = switch (builtin.os.tag) {
        .windows => struct {
            extern "c" fn _open_osfhandle(osfhandle: *c_int, flags: c_int) c_int;
            extern "c" fn _close(fd: c_int) c_int;

            pub fn getFd(file: std.fs.File) !c_int {
                const handle: std.os.windows.HANDLE = file.handle;
                const fd = _open_osfhandle(@alignCast(@ptrCast(handle)), 0);
                if (fd == -1) {
                    return error.failedToGetFd;
                }
                return fd;
            }

            pub fn close(fd: c_int) !void {
                if (_close(fd) == -1) {
                    return error.failedToCloseFd;
                }
            }
        },
        else => struct {
            pub fn getFd(file: std.fs.File) !c_int {
                return file.handle;
            }

            pub fn close(fd: c_int) !void {
                if (std.c.close(fd) == -1) {
                    return error.failedToCloseFd;
                }
            }
        },
    };
    file: std.fs.File,
    fd: c_int,

    const GetFdError = error{
        failedToGetFd,
    };
    const CloseError = error{
        failedToCloseFd,
    };

    pub fn forFile(file: std.fs.File) GetFdError!FileFd {
        const fd = try Inner.getFd(file);
        return FileFd{ .file = file, .fd = fd };
    }

    pub const CreateError = GetFdError || std.fs.File.OpenError;
    pub fn create(sub_path: []const u8, flags: std.fs.File.CreateFlags) CreateError!FileFd {
        const file = try std.fs.cwd().createFile(sub_path, flags);
        const fd = try Inner.getFd(file);
        return FileFd{ .file = file, .fd = fd };
    }

    pub fn close(self: *const FileFd) void {
        Inner.close(self.fd) catch unreachable;
    }
};
