const std = @import("std");
const sdl = @import("sdl2");
const vk = @import("vulkan");
const vkw = @import("vkw.zig");

const Allocator = std.mem.Allocator;

pub const std_options = .{

    // Set the log level to info
    .log_level = .info,

    // Define logFn to override the std implementation
    .logFn = myLogFn,
};

pub fn myLogFn(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {

    // Ignore all non-error logging from sources other than
    // .my_project, .nice_library and the default
    const scope_prefix = "(" ++ switch (scope) {
        std.log.default_log_scope => @tagName(scope),
        else => if (@intFromEnum(level) <= @intFromEnum(std.log.Level.err))
            @tagName(scope)
        else
            return,
    } ++ "): ";

    const prefix = "[" ++ comptime level.asText() ++ "] " ++ scope_prefix;

    // Print the message to stderr, silently ignoring any errors
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    const stderr = std.io.getStdErr().writer();
    nosuspend stderr.print(prefix ++ format ++ "\n", args) catch return;
}

pub fn main() !void {
    const app_name = "Shader Plate";

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    try sdl.init(.{
        .video = true,
        .events = true,
        .audio = true,
    });
    defer sdl.quit();

    const initial_extent = vk.Extent2D{ .width = 640, .height = 480 };
    var window = try sdl.createWindow(app_name, .{ .centered = {} }, .{ .centered = {} }, initial_extent.width, initial_extent.height, .{ .vis = .shown, .context = .vulkan, .resizable = true });
    defer window.destroy();

    const gc = vkw.GraphicsContext.init(allocator, app_name, window) catch |err| {
        switch (err) {
            error.NoSuitableDevice => {
                std.log.err("Failed to find a suitable physical device (gpu) that matched all requirements", .{});
            },
            else => {
                return err;
            },
        }
        return;
    };
    defer gc.deinit();

    var swapchain = try vkw.Swapchain.init(&gc, allocator, initial_extent);
    defer swapchain.deinit();

    const render_pass = try createRenderPass(&gc, swapchain);
    defer gc.dev.destroyRenderPass(render_pass, null);

    var framebuffers = try createFramebuffers(&gc, allocator, render_pass, swapchain);
    defer destroyFramebuffers(&gc, allocator, framebuffers);

    const pool = try gc.dev.createCommandPool(&.{
        .queue_family_index = gc.graphics_queue.family,
    }, null);
    defer gc.dev.destroyCommandPool(pool, null);

    // const buffer = try gc.dev.createBuffer(&.{
    //     .size = @sizeOf(@TypeOf(vertices)),
    //     .usage = .{ .transfer_dst_bit = true, .vertex_buffer_bit = true },
    //     .sharing_mode = .exclusive,
    // }, null);
    // defer gc.dev.destroyBuffer(buffer, null);
    var cmdbufs = try createCommandBuffers(
        &gc,
        pool,
        allocator,
        // buffer,
        swapchain.extent,
        render_pass,
        // pipeline,
        framebuffers,
    );
    defer destroyCommandBuffers(&gc, pool, allocator, cmdbufs);

    mainLoop: while (true) {
        var new_extent: ?vk.Extent2D = null;
        while (sdl.pollEvent()) |ev| {
            switch (ev) {
                .quit => break :mainLoop,
                .window => {
                    const window_event = ev.window;
                    switch (window_event.type) {
                        .resized => {
                            const new_size = window_event.type.resized;
                            new_extent = vk.Extent2D{ .width = @intCast(new_size.width), .height = @intCast(new_size.height) };
                        },
                        .size_changed => {
                            const new_size = window_event.type.size_changed;
                            new_extent = vk.Extent2D{ .width = @intCast(new_size.width), .height = @intCast(new_size.height) };
                        },
                        else => {},
                    }
                },
                .key_down => {
                    const key_event = ev.key_down;
                    if (key_event.scancode == sdl.Scancode.q) {
                        break :mainLoop;
                    }
                },
                else => {},
            }
        } // while poll sdl events

        const cmdbuf = cmdbufs[swapchain.image_index];
        const present_state = swapchain.present(cmdbuf) catch |err| switch (err) {
            error.OutOfDateKHR => vkw.Swapchain.PresentState.suboptimal,
            else => |e| return e,
        };
        if (present_state == .suboptimal or new_extent != null) {
            const extent = new_extent orelse swapchain.extent;
            try swapchain.recreate(extent);
            destroyFramebuffers(&gc, allocator, framebuffers);
            framebuffers = try createFramebuffers(&gc, allocator, render_pass, swapchain);

            destroyCommandBuffers(&gc, pool, allocator, cmdbufs);
            cmdbufs = try createCommandBuffers(&gc, pool, allocator, swapchain.extent, render_pass, framebuffers);
        }
    }
}

fn createCommandBuffers(
    gc: *const vkw.GraphicsContext,
    pool: vk.CommandPool,
    gpa: Allocator,
    // buffer: vk.Buffer,
    extent: vk.Extent2D,
    render_pass: vk.RenderPass,
    // pipeline: vk.Pipeline,
    framebuffers: []vk.Framebuffer,
) ![]vk.CommandBuffer {
    const cmdbufs = try gpa.alloc(vk.CommandBuffer, framebuffers.len);
    errdefer gpa.free(cmdbufs);

    try gc.dev.allocateCommandBuffers(&.{
        .command_pool = pool,
        .level = .primary,
        .command_buffer_count = @intCast(cmdbufs.len),
    }, cmdbufs.ptr);
    errdefer gc.dev.freeCommandBuffers(pool, @intCast(cmdbufs.len), cmdbufs.ptr);

    const clear: vk.ClearValue = .{
        .color = .{ .float_32 = .{ 0.5, 0.2, 0, 1 } },
    };

    const viewport: vk.Viewport = .{
        .x = 0,
        .y = 0,
        .width = @floatFromInt(extent.width),
        .height = @floatFromInt(extent.height),
        .min_depth = 0,
        .max_depth = 1,
    };

    const scissor: vk.Rect2D = .{
        .offset = .{ .x = 0, .y = 0 },
        .extent = extent,
    };

    for (cmdbufs, framebuffers) |cmdbuf, framebuffer| {
        try gc.dev.beginCommandBuffer(cmdbuf, &.{});

        gc.dev.cmdSetViewport(cmdbuf, 0, 1, @ptrCast(&viewport));
        gc.dev.cmdSetScissor(cmdbuf, 0, 1, @ptrCast(&scissor));

        const render_area: vk.Rect2D = .{
            .offset = .{ .x = 0, .y = 0 },
            .extent = extent,
        };

        gc.dev.cmdBeginRenderPass(cmdbuf, &.{
            .render_pass = render_pass,
            .framebuffer = framebuffer,
            .render_area = render_area,
            .clear_value_count = 1,
            .p_clear_values = @ptrCast(&clear),
        }, .@"inline");

        // gc.dev.cmdBindPipeline(cmdbuf, .graphics, pipeline);
        // const offset = [_]vk.DeviceSize{0};
        // gc.dev.cmdBindVertexBuffers(cmdbuf, 0, 1, @ptrCast(&buffer), &offset);
        // gc.dev.cmdDraw(cmdbuf, vertices.len, 1, 0, 0);

        gc.dev.cmdEndRenderPass(cmdbuf);
        try gc.dev.endCommandBuffer(cmdbuf);
    }

    return cmdbufs;
}

fn destroyCommandBuffers(gc: *const vkw.GraphicsContext, pool: vk.CommandPool, gpa: Allocator, cmdbufs: []vk.CommandBuffer) void {
    gc.dev.freeCommandBuffers(pool, @truncate(cmdbufs.len), cmdbufs.ptr);
    gpa.free(cmdbufs);
}

fn createFramebuffers(gc: *const vkw.GraphicsContext, gpa: Allocator, render_pass: vk.RenderPass, swapchain: vkw.Swapchain) ![]vk.Framebuffer {
    const framebuffers = try gpa.alloc(vk.Framebuffer, swapchain.swap_images.len);
    errdefer gpa.free(framebuffers);

    var i: usize = 0;
    errdefer for (framebuffers[0..i]) |fb| gc.dev.destroyFramebuffer(fb, null);

    for (framebuffers) |*fb| {
        fb.* = try gc.dev.createFramebuffer(&.{
            .render_pass = render_pass,
            .attachment_count = 1,
            .p_attachments = @ptrCast(&swapchain.swap_images[i].view),
            .width = swapchain.extent.width,
            .height = swapchain.extent.height,
            .layers = 1,
        }, null);
        i += 1;
    }

    return framebuffers;
}

fn destroyFramebuffers(gc: *const vkw.GraphicsContext, allocator: Allocator, framebuffers: []const vk.Framebuffer) void {
    for (framebuffers) |fb| gc.dev.destroyFramebuffer(fb, null);
    allocator.free(framebuffers);
}

fn createRenderPass(gc: *const vkw.GraphicsContext, swapchain: vkw.Swapchain) !vk.RenderPass {
    const color_attachment: vk.AttachmentDescription = .{
        .format = swapchain.surface_format.format,
        .samples = .{ .@"1_bit" = true },
        .load_op = .clear,
        .store_op = .store,
        .stencil_load_op = .dont_care,
        .stencil_store_op = .dont_care,
        .initial_layout = .undefined,
        .final_layout = .present_src_khr,
    };

    const color_attachment_ref: vk.AttachmentReference = .{
        .attachment = 0,
        .layout = .color_attachment_optimal,
    };

    const subpass: vk.SubpassDescription = .{
        .pipeline_bind_point = .graphics,
        .color_attachment_count = 1,
        .p_color_attachments = @ptrCast(&color_attachment_ref),
    };

    return try gc.dev.createRenderPass(&.{
        .attachment_count = 1,
        .p_attachments = @ptrCast(&color_attachment),
        .subpass_count = 1,
        .p_subpasses = @ptrCast(&subpass),
    }, null);
}
