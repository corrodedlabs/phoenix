#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 proj;
    vec3 camPos;
} ubo;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec2 inTexCoord;

layout(location = 0) out vec3 outWorldPos;
layout(location = 1) out vec3 outNormal;
layout(location = 2) out vec2 outTexCoord;



void main() {
    outWorldPos = vec3(ubo.model * vec4(inPosition, 1.0));
    outNormal = mat3(ubo.model) * inNormal;
    outTexCoord = inTexCoord;
    outTexCoord.t = 1.0 - inTexCoord.t;
    gl_Position = ubo.proj * ubo.view * vec4(outWorldPos, 1.0);
}
