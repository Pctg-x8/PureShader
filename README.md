PureShader
---

[![Build Status](https://travis-ci.org/Pctg-x8/PureShader.svg?branch=master)](https://travis-ci.org/Pctg-x8/PureShader)

A purely programmable shader frontend for SPIR-V

## Example

```
@import shader.core
@shader vertex

@[in 0] pos :: Vec4
@[in 1] uv :: Vec4

data ProjectionMatrixes = ProjectionMatrixes { persp :: Vec4 }
@uniform(set = 0, binding = 0) pmatr :: ProjectionMatrixes

projection :: Vec4 -> Vec4
projection vin = pmatr.persp * vin

expandVec3 :: Vec3 -> Vec4
expandVec3 (Vec3 x y z) = Vec4 x y z 1
shrinkVec4 :: Vec4 -> Vec3
shrinkVec4 v = v.xyz

@[out 0] posv :: Vec4
posv = let p = projection pos in expandVec3 $ shrinkVec4 p
```

GLSL Equivalent is:

```:glsl
layout(location = 0) in vec4 pos;
layout(location = 1) in vec4 uv;
layout(location = 0) out vec4 posv;
layout(set = 0, binding = 0) uniform ProjectionMatrixes
{
	vec4 persp;
} pmatr;

vec4 projection(vec4 vin) { return pmatr.persp * vin; }
vec4 expandVec3(vec3 v) { return vec4(v.xyz, 1.0f); }
vec3 shrinkVec4(vec4 v) { return v.xyz; }

void main()
{
	vec4 p = projection(pos);
	posv = expandVec3(shrinkVec4(p));
}
```
