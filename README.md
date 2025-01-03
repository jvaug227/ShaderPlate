# Shader Plate
This is just a curiosity project to see what it would take to develop something akin to ShaderToy
for running modular shaders in Vulkan.  
This project will serve to help me learn the vulkan pipeline as well as making a project in zig.

## Design Goals
- [ ] Reload on Save
- [ ] User Inputs
- [ ] Texture Upload & Generation
- [ ] Buffer Upload & Generation
- [ ] *Potential* Scripting

I personally use neovim, and I neither feel like making neovim somehow display shaders in-terminal 
nor do I feel there is a need to make yet another text editor. This is why I would rather make a
"Run Once and Reload" design where the application can be told to run a shaders file/directory and it 
will watch for when the file's contents are updated, then compile the shaders again.  

My argument for adding scripting (currently leaning towards lua) is that a preset of configs is only so 
powerful and could lead to overlapping functionality, as well as custom input and variable handling 
would require modifications to the underlying application in each scenario. On the other hand, giving
a reasonable api with callbacks and letting the user define their own variables or generate data with 
custom functions; this would greatly simplify the codebase at the cost of maintaining a scripting language.
