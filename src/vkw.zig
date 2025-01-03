/// Copied mostly from:
/// https://github.com/andrewrk/zig-vulkan-triangle/blob/master/src/GraphicsContext.zig
const std = @import("std");
const sdl = @import("sdl2");
const vk = @import("vulkan");

// List the api's and extensions wanted, there HAS to be redundancy in the numbers
// because vk.features.version_1_2 does NOT automatically include vk.features.version_1_1,
// likewise 1_1 does NOT include features 1_0.
//
// The ability to render to a window requires both the KHR surface and KHR swapchain. These
// are both extensions because they are handled differently between platforms, thus they must
// be explicitly included in the API.
//
// This vkAPIs struct is used during comp-time to generate available functions for the dispatch
// tables
pub const vkAPIs: []const vk.ApiInfo = &.{
    .{
        .base_commands = .{
            .createInstance = true,
        },
        .instance_commands = .{
            .createDevice = true,
        },
    },
    vk.features.version_1_0,
    vk.features.version_1_1,
    vk.features.version_1_2,
    vk.extensions.khr_surface,
    vk.extensions.khr_swapchain,
};
const required_device_extensions = [_][*:0]const u8{vk.extensions.khr_swapchain.name};

pub const Allocator = std.mem.Allocator;

// "Dispatch Tables"
pub const BaseDispatch = vk.BaseWrapper(vkAPIs);
pub const InstanceDispatch = vk.InstanceWrapper(vkAPIs);
pub const DeviceDispatch = vk.DeviceWrapper(vkAPIs);

// "Proxying Wrappers" - I believe this means this struct represents
// the handle and the dispatch table becoming unified.
pub const Instance = vk.InstanceProxy(vkAPIs);
pub const Device = vk.DeviceProxy(vkAPIs);

// "Physical" device candidate
const DeviceCandidate = struct {
    physical_device: vk.PhysicalDevice,
    props: vk.PhysicalDeviceProperties,
    queues: QueueAllocation,
};

// This name is kinda dumb because it's only called
// "Allocation" due to the family indicies being acquired
// from an alloc vulkan function.
const QueueAllocation = struct {
    graphics_family: u32,
    present_family: u32,
};

pub const Queue = struct {
    handle: vk.Queue,
    family: u32,

    fn init(device: Device, family: u32) Queue {
        return .{
            .handle = device.getDeviceQueue(family, 0),
            .family = family,
        };
    }
};

pub const GraphicsContext = struct {
    allocator: Allocator,
    instance: Instance,
    dev: Device,
    pdev: vk.PhysicalDevice,
    surface: vk.SurfaceKHR,

    graphics_queue: Queue,
    present_queue: Queue,

    pub fn init(allocator: Allocator, app_name: [*:0]const u8, window: sdl.Window) !GraphicsContext {
        var self: GraphicsContext = undefined;
        self.allocator = allocator;

        // Use SDL to populate the vulkan dispatch table
        try sdl.vulkan.loadLibrary(null);
        const vkb = try BaseDispatch.load(try sdl.vulkan.getVkGetInstanceProcAddr());

        const extensions = try sdl.vulkan.getInstanceExtensionsAlloc(window, allocator);
        defer allocator.free(extensions);

        const app_info = vk.ApplicationInfo{
            .p_application_name = app_name,
            .application_version = vk.makeApiVersion(0, 0, 0, 0),
            .p_engine_name = app_name,
            .engine_version = vk.makeApiVersion(0, 0, 0, 0),
            .api_version = vk.API_VERSION_1_2,
        };

        // Basic zig-ified vulkan instance
        const instance_handle = try vkb.createInstance(&.{
            .p_application_info = &app_info,
            .enabled_extension_count = @intCast(extensions.len),
            .pp_enabled_extension_names = @ptrCast(extensions),
        }, null);

        // Basic zig-ified function calls related to the vulkan instance
        const vk_instance_wrapper = try allocator.create(InstanceDispatch);
        errdefer allocator.destroy(vk_instance_wrapper);
        vk_instance_wrapper.* = try InstanceDispatch.load(instance_handle, vkb.dispatch.vkGetInstanceProcAddr);

        // Zig-ified vulkan instance and function calls tied in a nice bow!
        const instance = Instance.init(instance_handle, vk_instance_wrapper);
        errdefer instance.destroyInstance(null);

        const surface = try sdl.vulkan.createSurface(window, instance.handle);
        errdefer instance.destroySurfaceKHR(surface, null);

        const physical_device = try pickPhysicalDevice(instance, allocator, surface);

        const priority = [_]f32{1};
        const qci = [_]vk.DeviceQueueCreateInfo{
            .{
                .queue_family_index = physical_device.queues.graphics_family,
                .queue_count = 1,
                .p_queue_priorities = &priority,
            },
            .{
                .queue_family_index = physical_device.queues.present_family,
                .queue_count = 1,
                .p_queue_priorities = &priority,
            },
        };
        const queue_count: u32 = if (physical_device.queues.graphics_family == physical_device.queues.present_family) 1 else 2;

        // Get the 'Logical' device handle from the 'Physical' one.
        const device_handle = try instance.createDevice(physical_device.physical_device, &.{
            .queue_create_info_count = queue_count,
            .p_queue_create_infos = &qci,
            .enabled_extension_count = required_device_extensions.len,
            .pp_enabled_extension_names = @ptrCast(&required_device_extensions),
        }, null);

        // Do the same thing as the VkInstance, wrap the handle and it's dispatch table into a a 'proxy'.
        const device_wrapper = try allocator.create(DeviceDispatch);
        errdefer allocator.destroy(device_wrapper);
        device_wrapper.* = try DeviceDispatch.load(device_handle, instance.wrapper.dispatch.vkGetDeviceProcAddr);

        const device = Device.init(device_handle, device_wrapper);
        errdefer device.destroyDevice(null);

        self.instance = instance;
        self.surface = surface;
        self.dev = device;
        self.pdev = physical_device.physical_device;
        self.graphics_queue = Queue.init(device, physical_device.queues.graphics_family);
        self.present_queue = Queue.init(device, physical_device.queues.present_family);
        return self;
    }

    pub fn deinit(self: GraphicsContext) void {
        self.dev.destroyDevice(null);
        self.instance.destroySurfaceKHR(self.surface, null);
        self.instance.destroyInstance(null);

        self.allocator.destroy(self.dev.wrapper);
        self.allocator.destroy(self.instance.wrapper);
    }
};

/// Returns the first physical device that supports extensions and a surface,
/// does not compare devices for optimal solution
fn pickPhysicalDevice(
    instance: Instance,
    allocator: std.mem.Allocator,
    surface: vk.SurfaceKHR,
) !DeviceCandidate {
    const pdevs = try instance.enumeratePhysicalDevicesAlloc(allocator);
    defer allocator.free(pdevs);

    for (pdevs) |pdev| {
        if (try checkDeviceSuitable(instance, pdev, surface, allocator)) |candidate| {
            return candidate;
        }
    }

    return error.NoSuitableDevice;
}

fn checkDeviceSuitable(instance: Instance, physical_device: vk.PhysicalDevice, surface: vk.SurfaceKHR, allocator: std.mem.Allocator) !?DeviceCandidate {
    if (!try checkExtensionSupport(instance, physical_device, allocator)) {
        return null;
    }

    if (!try checkSurfaceSupport(instance, physical_device, surface)) {
        return null;
    }

    if (try allocateQueues(instance, physical_device, allocator, surface)) |allocation| {
        const props = instance.getPhysicalDeviceProperties(physical_device);
        return DeviceCandidate{
            .physical_device = physical_device,
            .props = props,
            .queues = allocation,
        };
    }

    return null;
}

fn checkExtensionSupport(
    instance: Instance,
    pdev: vk.PhysicalDevice,
    allocator: std.mem.Allocator,
) !bool {
    const propsv = try instance.enumerateDeviceExtensionPropertiesAlloc(pdev, null, allocator);
    defer allocator.free(propsv);

    for (required_device_extensions) |ext| {
        for (propsv) |props| {
            if (std.mem.eql(u8, std.mem.span(ext), std.mem.sliceTo(&props.extension_name, 0))) {
                break;
            }
        } else {
            return false;
        }
    }

    return true;
}

fn checkSurfaceSupport(instance: Instance, pdev: vk.PhysicalDevice, surface: vk.SurfaceKHR) !bool {
    var format_count: u32 = undefined;
    _ = try instance.getPhysicalDeviceSurfaceFormatsKHR(pdev, surface, &format_count, null);

    var present_mode_count: u32 = undefined;
    _ = try instance.getPhysicalDeviceSurfacePresentModesKHR(pdev, surface, &present_mode_count, null);

    return format_count > 0 and present_mode_count > 0;
}

fn allocateQueues(instance: Instance, pdev: vk.PhysicalDevice, allocator: std.mem.Allocator, surface: vk.SurfaceKHR) !?QueueAllocation {
    const families = try instance.getPhysicalDeviceQueueFamilyPropertiesAlloc(pdev, allocator);
    defer allocator.free(families);

    var graphics_family: ?u32 = null;
    var present_family: ?u32 = null;

    for (families, 0..) |properties, i| {
        const family: u32 = @intCast(i);

        if (graphics_family == null and properties.queue_flags.graphics_bit) {
            graphics_family = family;
        }

        if (present_family == null and (try instance.getPhysicalDeviceSurfaceSupportKHR(pdev, family, surface)) == vk.TRUE) {
            present_family = family;
        }
    }

    if (graphics_family != null and present_family != null) {
        return QueueAllocation{
            .graphics_family = graphics_family.?,
            .present_family = present_family.?,
        };
    }

    return null;
}

pub const Swapchain = struct {
    pub const PresentState = enum {
        optimal,
        suboptimal,
    };

    gc: *const GraphicsContext,
    allocator: Allocator,

    surface_format: vk.SurfaceFormatKHR,
    present_mode: vk.PresentModeKHR,
    extent: vk.Extent2D,
    handle: vk.SwapchainKHR,

    swap_images: []SwapImage,
    image_index: u32,
    next_image_acquired: vk.Semaphore,

    pub fn init(gc: *const GraphicsContext, allocator: Allocator, extent: vk.Extent2D) !Swapchain {
        return try initRecycle(gc, allocator, extent, .null_handle);
    }

    pub fn initRecycle(gc: *const GraphicsContext, allocator: Allocator, extent: vk.Extent2D, old_handle: vk.SwapchainKHR) !Swapchain {
        const caps = try gc.instance.getPhysicalDeviceSurfaceCapabilitiesKHR(gc.pdev, gc.surface);
        const actual_extent = findActualExtent(caps, extent);
        if (actual_extent.width == 0 or actual_extent.height == 0) {
            return error.InvalidSurfaceDimensions;
        }

        const surface_format = try findSurfaceFormat(gc, allocator);
        const present_mode = try findPresentMode(gc, allocator);

        var image_count: u32 = caps.min_image_count + 1;
        const MAX_IMAGE_ANY: u32 = 0;
        if (caps.max_image_count != MAX_IMAGE_ANY) {
            image_count = @min(image_count, caps.max_image_count);
        }

        const qfi = [_]u32{ gc.graphics_queue.family, gc.present_queue.family };
        const sharing_mode: vk.SharingMode = if (gc.graphics_queue.family != gc.present_queue.family)
            .concurrent
        else
            .exclusive;

        const handle = try gc.dev.createSwapchainKHR(&.{
            .surface = gc.surface,
            .min_image_count = image_count,
            .image_format = surface_format.format,
            .image_color_space = surface_format.color_space,
            .image_extent = actual_extent,
            .image_array_layers = 1,
            .image_usage = .{ .color_attachment_bit = true, .transfer_dst_bit = true },
            .image_sharing_mode = sharing_mode,
            .queue_family_index_count = qfi.len,
            .p_queue_family_indices = &qfi,
            .pre_transform = caps.current_transform,
            .composite_alpha = .{ .opaque_bit_khr = true },
            .present_mode = present_mode,
            .clipped = vk.TRUE,
            .old_swapchain = old_handle,
        }, null);
        errdefer gc.dev.destroySwapchainKHR(handle, null);

        if (old_handle != .null_handle) {
            // Apparently, the old swapchain handle still needs to be destroyed after recreating.
            gc.dev.destroySwapchainKHR(old_handle, null);
        }

        const swap_images = try initSwapchainImages(gc, handle, surface_format.format, allocator);
        errdefer {
            for (swap_images) |si| si.deinit(gc);
            allocator.free(swap_images);
        }

        var next_image_acquired = try gc.dev.createSemaphore(&.{}, null);
        errdefer gc.dev.destroySemaphore(next_image_acquired, null);

        const result = try gc.dev.acquireNextImageKHR(handle, std.math.maxInt(u64), next_image_acquired, .null_handle);
        if (result.result != .success) {
            return error.ImageAcquireFailed;
        }

        std.mem.swap(vk.Semaphore, &swap_images[result.image_index].image_acquired, &next_image_acquired);
        return Swapchain{
            .gc = gc,
            .allocator = allocator,
            .surface_format = surface_format,
            .present_mode = present_mode,
            .extent = actual_extent,
            .handle = handle,
            .swap_images = swap_images,
            .image_index = result.image_index,
            .next_image_acquired = next_image_acquired,
        };
    }

    fn deinitExceptSwapchain(self: Swapchain) void {
        for (self.swap_images) |si| si.deinit(self.gc);
        self.allocator.free(self.swap_images);
        self.gc.dev.destroySemaphore(self.next_image_acquired, null);
    }

    pub fn waitForAllFences(self: Swapchain) !void {
        for (self.swap_images) |si| si.waitForFence(self.gc) catch {};
    }

    pub fn deinit(self: Swapchain) void {
        self.deinitExceptSwapchain();
        self.gc.dev.destroySwapchainKHR(self.handle, null);
    }

    pub fn recreate(self: *Swapchain, new_extent: vk.Extent2D) !void {
        const gc = self.gc;
        const allocator = self.allocator;
        const old_handle = self.handle;
        self.deinitExceptSwapchain();
        self.* = try initRecycle(gc, allocator, new_extent, old_handle);
    }

    pub fn currentImage(self: Swapchain) vk.Image {
        return self.swap_images[self.image_index].image;
    }

    pub fn currentSwapImage(self: Swapchain) *const SwapImage {
        return &self.swap_images[self.image_index];
    }

    pub fn present(self: *Swapchain, cmdbuf: vk.CommandBuffer) !PresentState {
        // Simple method:
        // 1) Acquire next image
        // 2) Wait for and reset fence of the acquired image
        // 3) Submit command buffer with fence of acquired image,
        //    dependendent on the semaphore signalled by the first step.
        // 4) Present current frame, dependent on semaphore signalled by previous step
        // Problem: This way we can't reference the current image while rendering.
        // Better method: Shuffle the steps around such that acquire next image is the last step,
        // leaving the swapchain in a state with the current image.
        // 1) Wait for and reset fence of current image
        // 2) Submit command buffer, signalling fence of current image and dependent on
        //    the semaphore signalled by step 4.
        // 3) Present current frame, dependent on semaphore signalled by the submit
        // 4) Acquire next image, signalling its semaphore
        // One problem that arises is that we can't know beforehand which semaphore to signal,
        // so we keep an extra auxilery semaphore that is swapped around

        // Step 1: Make sure the current frame has finished rendering
        const current = self.currentSwapImage();
        try current.waitForFence(self.gc);
        try self.gc.dev.resetFences(1, @ptrCast(&current.frame_fence));

        // Step 2: Submit the command buffer
        const wait_stage = [_]vk.PipelineStageFlags{.{ .top_of_pipe_bit = true }};
        try self.gc.dev.queueSubmit(self.gc.graphics_queue.handle, 1, &[_]vk.SubmitInfo{.{
            .wait_semaphore_count = 1,
            .p_wait_semaphores = @ptrCast(&current.image_acquired),
            .p_wait_dst_stage_mask = &wait_stage,
            .command_buffer_count = 1,
            .p_command_buffers = @ptrCast(&cmdbuf),
            .signal_semaphore_count = 1,
            .p_signal_semaphores = @ptrCast(&current.render_finished),
        }}, current.frame_fence);

        // Step 3: Present the current frame
        _ = try self.gc.dev.queuePresentKHR(self.gc.present_queue.handle, &.{
            .wait_semaphore_count = 1,
            .p_wait_semaphores = @ptrCast(&current.render_finished),
            .swapchain_count = 1,
            .p_swapchains = @ptrCast(&self.handle),
            .p_image_indices = @ptrCast(&self.image_index),
        });

        // Step 4: Acquire next frame
        const result = try self.gc.dev.acquireNextImageKHR(
            self.handle,
            std.math.maxInt(u64),
            self.next_image_acquired,
            .null_handle,
        );

        std.mem.swap(vk.Semaphore, &self.swap_images[result.image_index].image_acquired, &self.next_image_acquired);
        self.image_index = result.image_index;

        return switch (result.result) {
            .success => .optimal,
            .suboptimal_khr => .suboptimal,
            else => unreachable,
        };
    }

    const SwapImage = struct {
        image: vk.Image,
        view: vk.ImageView,
        image_acquired: vk.Semaphore,
        render_finished: vk.Semaphore,
        frame_fence: vk.Fence,

        fn init(gc: *const GraphicsContext, image: vk.Image, format: vk.Format) !SwapImage {
            const view = try gc.dev.createImageView(&.{
                .image = image,
                .view_type = .@"2d",
                .format = format,
                .components = .{ .r = .identity, .g = .identity, .b = .identity, .a = .identity },
                .subresource_range = .{
                    .aspect_mask = .{ .color_bit = true },
                    .base_mip_level = 0,
                    .level_count = 1,
                    .base_array_layer = 0,
                    .layer_count = 1,
                },
            }, null);
            errdefer gc.dev.destroyImageView(view, null);

            const image_acquired = try gc.dev.createSemaphore(&.{}, null);
            errdefer gc.dev.destroySemaphore(image_acquired, null);

            const render_finished = try gc.dev.createSemaphore(&.{}, null);
            errdefer gc.dev.destroySemaphore(render_finished, null);

            const frame_fence = try gc.dev.createFence(&.{ .flags = .{ .signaled_bit = true } }, null);
            errdefer gc.dev.destroyFence(frame_fence, null);

            return SwapImage{
                .image = image,
                .view = view,
                .image_acquired = image_acquired,
                .render_finished = render_finished,
                .frame_fence = frame_fence,
            };
        }

        fn deinit(self: SwapImage, gc: *const GraphicsContext) void {
            self.waitForFence(gc) catch return;
            gc.dev.destroyImageView(self.view, null);
            gc.dev.destroySemaphore(self.image_acquired, null);
            gc.dev.destroySemaphore(self.render_finished, null);
            gc.dev.destroyFence(self.frame_fence, null);
        }

        fn waitForFence(self: SwapImage, gc: *const GraphicsContext) !void {
            _ = try gc.dev.waitForFences(1, @ptrCast(&self.frame_fence), vk.TRUE, std.math.maxInt(u64));
        }
    };

    fn initSwapchainImages(gc: *const GraphicsContext, swapchain: vk.SwapchainKHR, format: vk.Format, allocator: Allocator) ![]SwapImage {
        const images = try gc.dev.getSwapchainImagesAllocKHR(swapchain, allocator);
        defer allocator.free(images);

        const swap_images = try allocator.alloc(SwapImage, images.len);
        errdefer allocator.free(swap_images);

        var i: usize = 0;
        errdefer for (swap_images[0..i]) |si| si.deinit(gc);

        for (images) |image| {
            swap_images[i] = try SwapImage.init(gc, image, format);
            i += 1;
        }

        return swap_images;
    }

    fn findSurfaceFormat(gc: *const GraphicsContext, allocator: Allocator) !vk.SurfaceFormatKHR {
        const preferred = vk.SurfaceFormatKHR{
            .format = .b8g8r8a8_srgb,
            .color_space = .srgb_nonlinear_khr,
        };

        const surface_formats = try gc.instance.getPhysicalDeviceSurfaceFormatsAllocKHR(gc.pdev, gc.surface, allocator);
        defer allocator.free(surface_formats);

        for (surface_formats) |sfmt| {
            if (std.meta.eql(sfmt, preferred)) {
                return preferred;
            }
        }

        return surface_formats[0]; // There must always be at least one supported surface format
    }

    fn findPresentMode(gc: *const GraphicsContext, allocator: Allocator) !vk.PresentModeKHR {
        const present_modes = try gc.instance.getPhysicalDeviceSurfacePresentModesAllocKHR(gc.pdev, gc.surface, allocator);
        defer allocator.free(present_modes);

        const preferred = [_]vk.PresentModeKHR{
            .mailbox_khr,
            .immediate_khr,
        };

        for (preferred) |mode| {
            if (std.mem.indexOfScalar(vk.PresentModeKHR, present_modes, mode) != null) {
                return mode;
            }
        }

        return .fifo_khr;
    }

    fn findActualExtent(caps: vk.SurfaceCapabilitiesKHR, extent: vk.Extent2D) vk.Extent2D {
        if (caps.current_extent.width != 0xFFFF_FFFF) {
            return caps.current_extent;
        } else {
            return .{
                .width = std.math.clamp(extent.width, caps.min_image_extent.width, caps.max_image_extent.width),
                .height = std.math.clamp(extent.height, caps.min_image_extent.height, caps.max_image_extent.height),
            };
        }
    }
};
