#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(location = 0) in vec3 inWorldPos;
layout(location = 1) in vec3 inNormal;
layout(location = 2) in vec2 inTexCoord;

layout (binding = 0) uniform UBO {
    mat4 model;
    mat4 view;
    mat4 proj;
    vec3 camPos;
} ubo;

layout (binding = 1) uniform UBOParams {
    vec4 lights[4];
    float exposure;
    float gamma;
} uboParams;

// layout (binding = 2) uniform samplerCube samplerIrradiance;
layout (binding = 2) uniform sampler2D samplerBRDFLUT;
// layout (binding = 4) uniform samplerCube prefilteredMap;
layout (binding = 3) uniform sampler2D albedoMap;
layout (binding = 4) uniform sampler2D normalMap;
layout (binding = 5) uniform sampler2D aoMap;
layout (binding = 6) uniform sampler2D metallicMap;
layout (binding = 7) uniform sampler2D roughnessMap;

layout(location = 0) out vec4 outColor;

#define PI 3.1415926535897932384626433832795
#define ALBEDO pow(texture(albedoMap, inTexCoord).rgb, vec3(2.2))

vec3 Uncharted2Tonemap(vec3 x) {
    float A = 0.15;
    float B = 0.50;
    float C = 0.10;
    float D = 0.20;
    float E = 0.02;
    float F = 0.30;

    return ((x * (A * x + C * B) + D * E) / (x * (A * x + B) + D * F)) - E / F;
}

// Normal Distribution Function
float Distribution_GGX(float NdotH, float roughness) {
    float alpha = roughness * roughness;
    float alpha2 = alpha * alpha;

    float denom = NdotH * NdotH * (alpha2 - 1.0) + 1.0;

    return (alpha2) / (PI * denom * denom);
}

// Geometric Shadowing function
float GeometricSchlicksmithGGX(float NdotL, float NdotV, float roughness) {
    float r = (roughness + 1.0);
    float k = (r * r) / 8.0;
    
    float GL = NdotL / (NdotL * (1.0 - k) + k);
    float GV = NdotV / (NdotV * (1.0 - k) + k);
    return GL * GV;
}

// Fresnel functions

vec3 FresnelSchlick(float cos_theta, vec3 F0) {
    return F0 + (1.0 - F0) * pow(1.0 - cos_theta, 5.0);
}

vec3 FresnelSchlickRoughness(float cos_theta, vec3 F0, float roughness) {
    return F0 + (max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - cos_theta, 5.0);
}

// ibl

vec3 prefilteredReflection(vec3 R, float roughness) {
    const float MAX_REFLECTION_LOD = 9.0;
    
    float lod = roughness * MAX_REFLECTION_LOD;
    float lodFloor = floor(lod);
    float lodCeil = ceil(lod);

    vec3 a = textureLod(prefilteredMap, R, lodFloor).rgb;
    vec3 b = textureLod(prefilteredMap, R, lodCeil).rgb;

    return mix(a, b, lodFloor - lodCeil);
}

// Lighting calculations

vec3 specularContribution(vec3 L, vec3 V, vec3 N, vec3 F0, float metallic,
			  float roughness) {
    // precalculate vectors and dot products
    vec3 H = normalize(V + L);

    float NdotH = clamp(dot(N, H), 0.0, 1.0);
    float NdotV = clamp(dot(N, V), 0.0, 1.0);
    float NdotL = clamp(dot(N, L), 0.0, 1.0);

    // Light color
    vec3 lightColor = vec3(1.0);

    vec3 color = vec3(0.0);

    if (NdotL > 0.0) {
	// D = Normal distribution (Distribution of the microfacets)
	float D = Distribution_GGX(NdotH, roughness);

	// G = Geometric Shadowing term (Microfacets shadowing)
	float G = GeometricSchlicksmithGGX(NdotL, NdotV, roughness);

	// F = Fresnel factor (reflectance depending on angle of incidence)
	vec3 F = FresnelSchlick(NdotV, F0);

	vec3 spec = D * F * G / (4.0 * NdotL * NdotV + 0.001);
	vec3 KD = (vec3(1.0) - F) * (1.0 - metallic);
	color += (KD * ALBEDO / PI + spec) * NdotL;
    }

    return color;
}

// Normal mapping
// See  http://www.thetenthplanet.de/archives/1180
vec3 perturbNormal() {
    vec3 tangentNormal = texture(normalMap, inTexCoord).xyz * 2.0 - 1.0;
    vec3 q1 = dFdx(inWorldPos);
    vec3 q2 = dFdy(inWorldPos);
    vec2 st1 = dFdx(inTexCoord);
    vec2 st2 = dFdy(inTexCoord);

    vec3 N = normalize(inNormal);
    vec3 T = normalize(q1 * st2.t - q2 * st1.t);
    vec3 B = -normalize(cross(N, T));
    mat3 TBN = mat3(T, B, N);

    return normalize(TBN * tangentNormal);
}


void main () {

    vec3 N = perturbNormal();
    vec3 V = normalize(ubo.camPos - inWorldPos);
    vec3 R = reflect(- V, N);

    float metallic = texture(metallicMap, inTexCoord).r;
    float roughness = texture(roughnessMap, inTexCoord).r;

    vec3 F0 = vec3(0.04);
    F0 = mix(F0, ALBEDO, metallic);

    vec3 Lo = vec3(0.0);
    for(int i = 0; i < uboParams.lights[i].length(); i++) {
    	vec3 L = normalize(uboParams.lights[i].xyz - inWorldPos);
    	Lo += specularContribution(L, V, N, F0, metallic, roughness);
    }

    vec2 brdf = texture(samplerBRDFLUT, vec2(max(dot(N, V), 0.0), roughness)).rg;
    // vec3 reflection = prefilteredReflection(R, roughness).rgb;
    // vec3 irradiance = texture(samplerIrradiance, N).rgb;

    // Diffuse based on irradiance
    // vec3 diffuse = irradiance * ALBEDO;
    vec3 diffuse = ALBEDO;

    vec3 F = FresnelSchlickRoughness(max(dot(N, V), 0.0), F0, roughness);

    // Specular reflectance
    // vec3 specular = reflection * (F * brdf.x + brdf.y);

    vec3 specular = (F * brdf.x + brdf.y);

    // Ambient part
    vec3 kD = 1.0 - F;
    kD *= 1.0 - metallic;
    vec3 ambient = (kD * diffuse + specular) * texture(aoMap, inTexCoord).rrr;

    vec3 color = ambient + Lo;

    // Tone mapping
    color = Uncharted2Tonemap(color * uboParams.exposure);
    color = color * (1.0f / Uncharted2Tonemap(vec3(11.2f)));

    // gamma correction
    color = pow(color, vec3(1.0f / uboParams.gamma));

    outColor = vec4(color, 1.0);
}
