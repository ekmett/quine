#pragma version 410 core
#include "locations.h"

uniform vec2 iResolution = vec2(640.,480.);
uniform float iGlobalTime = 0.;

layout(location = FRAGMENT_COLOR) out vec4 color;

// https://www.shadertoy.com/view/Msl3Rn

// chains and gears - @P_Malin
    
#define ENABLE_AMBIENT_OCCLUSION
#define ENABLE_SPECULAR
#define ENABLE_REFLECTIONS
#define ENABLE_SHADOWS
#define ENABLE_FOG

//#define ENABLE_DIRECTIONAL_LIGHT
#define ENABLE_DIRECTIONAL_LIGHT_FLARE

#define ENABLE_POINT_LIGHT
#define ENABLE_POINT_LIGHT_FLARE

#define kRaymarchMaxIter 256

const float kPI = 3.141592654;
const float kTwoPI = kPI * 2.0;

struct C_Ray
{
    vec3 vOrigin;
    vec3 vDir;
    float fStartDistance;
    float fLength;
};

struct C_HitInfo
{
    vec3 vPos;
    float fDistance;
    vec3 vObjectId;
};
    
struct C_Surface
{
    vec3 vNormal;
    vec3 cReflection;
    vec3 cTransmission;    
};

struct C_Material
{
    vec3 cAlbedo;
    float fR0;
    float fSmoothness;
    vec2 vParam;
};

struct C_Shading
{
    vec3 cDiffuse;
    vec3 cSpecular;
};

struct C_PointLight
{
    vec3 vPos;
    vec3 cColour;
};

struct C_DirectionalLight
{
    vec3 vDir;
    vec3 cColour;
};

vec3 RotateX( const in vec3 vPos, const in float fAngle )
{
    float s = sin(fAngle);
    float c = cos(fAngle);
    
    vec3 vResult = vec3( vPos.x, c * vPos.y + s * vPos.z, -s * vPos.y + c * vPos.z);
    
    return vResult;
}

vec3 RotateY( const in vec3 vPos, const in float fAngle )
{
    float s = sin(fAngle);
    float c = cos(fAngle);
    
    vec3 vResult = vec3( c * vPos.x + s * vPos.z, vPos.y, -s * vPos.x + c * vPos.z);
    
    return vResult;
}

vec3 RotateZ( const in vec3 vPos, const in float fAngle )
{
    float s = sin(fAngle);
    float c = cos(fAngle);
    
    vec3 vResult = vec3( c * vPos.x + s * vPos.y, -s * vPos.x + c * vPos.y, vPos.z);
    
    return vResult;
}

/////////////////////////////////////
// Distance Field CSG
// These carry with them the material parameters in yzw

vec4 DistCombineUnion( const in vec4 v1, const in vec4 v2 )
{
    //if(v1.x < v2.x) return v1; else return v2;
    return mix(v1, v2, step(v2.x, v1.x));
}

vec4 DistCombineUnionConditional( const in vec4 v1, const in vec4 v2, const in float fCondition )
{    
    //if( fCondition < 0.0 )
    //            return v1;
                
    return mix(v1, v2, step(v2.x, v1.x) * step(0.0, fCondition));
}

vec4 DistCombineIntersect( const in vec4 v1, const in vec4 v2 )
{
    return mix(v2, v1, step(v2.x,v1.x));
}

vec4 DistCombineSubtract( const in vec4 v1, const in vec4 v2 )
{
    return DistCombineIntersect(v1, vec4(-v2.x, v2.yzw));
}

vec3 DomainRepeatXZGetTile( const in vec3 vPos, const in vec2 vRepeat, out vec2 vTile )
{
    vec3 vResult = vPos;
    vec2 vTilePos = (vPos.xz / vRepeat) + 0.5;
    vTile = floor(vTilePos + 1000.0);
    vResult.xz = (fract(vTilePos) - 0.5) * vRepeat;
    return vResult;
}

vec3 DomainRepeatXZ( const in vec3 vPos, const in vec2 vRepeat )
{
    vec3 vResult = vPos;
    vec2 vTilePos = (vPos.xz / vRepeat) + 0.5;
    vResult.xz = (fract(vTilePos) - 0.5) * vRepeat;
    return vResult;
}

vec3 DomainRepeatY( const in vec3 vPos, const in float fSize )
{
    vec3 vResult = vPos;
    vResult.y = (fract(vPos.y / fSize + 0.5) - 0.5) * fSize;
    return vResult;
}

vec3 DomainRotateSymmetry( const in vec3 vPos, const in float fSteps )
{
    float angle = atan( vPos.x, vPos.z );
    
    float fScale = fSteps / (kPI * 2.0);
    float steppedAngle = (floor(angle * fScale + 0.5)) / fScale;
    
    float s = sin(-steppedAngle);
    float c = cos(-steppedAngle);
    
    vec3 vResult = vec3( c * vPos.x + s * vPos.z, 
                 vPos.y,
                 -s * vPos.x + c * vPos.z);
    
    return vResult;
}

float GetDistanceXYTorus( const in vec3 p, const in float r1, const in float r2 )
{
   vec2 q = vec2(length(p.xy)-r1,p.z);
   return length(q)-r2;
}

float GetDistanceYZTorus( const in vec3 p, const in float r1, const in float r2 )
{
   vec2 q = vec2(length(p.yz)-r1,p.x);
   return length(q)-r2;
}

float GetDistanceCylinderY(const in vec3 vPos, const in float r)
{
    return length(vPos.xz) - r;
}

float GetDistanceChain( const in vec3 vPos )
{
    float fOuterCylinder = length(vPos.xz) - 1.05;
    if(fOuterCylinder > 0.5)
    {
        return fOuterCylinder;
    }
    
    vec3 vChainDomain = vPos;
    
    vChainDomain.y = fract(vChainDomain.y + 0.5) - 0.5;        
    float fDistTorus1 = GetDistanceXYTorus(vChainDomain, 0.35, 0.1);
    
    vChainDomain.y = fract(vChainDomain.y + 1.0) - 0.5;        
    float fDistTorus2 = GetDistanceYZTorus(vChainDomain, 0.35, 0.1);
    
    float fDist = min(fDistTorus1, fDistTorus2);

    return fDist;
}

float GetDistanceGear( const in vec3 vPos )
{
    float fOuterCylinder = length(vPos.xz) - 1.05;
    if(fOuterCylinder > 0.5)
    {
        return fOuterCylinder;
    }
    
    vec3 vToothDomain = DomainRotateSymmetry(vPos, 16.0);
    vToothDomain.xz = abs(vToothDomain.xz);
    float fGearDist = dot(vToothDomain.xz,normalize(vec2(1.0, 0.55))) - 0.55;
    float fSlabDist = abs(vPos.y + 0.1) - 0.15;
    
    vec3 vHoleDomain = abs(vPos);
    vHoleDomain -= 0.35;
    float fHoleDist = length(vHoleDomain.xz) - 0.2;
    
    float fBarDist =vToothDomain.z - 0.15;
    fBarDist = max(vPos.y - 0.1, fBarDist);
    
    float fResult = fGearDist;
    fResult = max(fResult, fSlabDist);
    fResult = max(fResult, fOuterCylinder);
    fResult = max(fResult, -fHoleDist);
    fResult = min(fResult, fBarDist);
    return fResult;
}

vec4 GetDistanceScene( const in vec3 vPos )
{                 
    vec2 vChainTile;
    vec2 vRepeat = vec2(4.0, 8.0);
    vec3 vRepeatDomain = DomainRepeatXZGetTile(vPos, vRepeat, vChainTile);
        
    vec4 vDistFloor = vec4(vPos.y + 0.5, 1.0, vec2(0.0));
    vec4 vResult = vDistFloor;
    {
        vec3 vGearDomain1 = DomainRepeatXZ(vPos+vec3(0.0, 0.0, 4.0), vRepeat);
        vGearDomain1 = RotateY( vGearDomain1, iGlobalTime);
        vec4 vDistGear = vec4(GetDistanceGear(vGearDomain1), 3.0, vec2(0.0));
        vResult = DistCombineUnion( vResult, vDistGear );
        
        vec3 vGearDomain2 = DomainRepeatXZ(vPos+vec3(2.0, 0.0, 4.0), vRepeat);
        vGearDomain2 = RotateY( vGearDomain2, -iGlobalTime + (2.0 * kPI / 32.0));
        vec4 vDistGear2 = vec4(GetDistanceGear(vGearDomain2), 3.0, vec2(0.0));        
        vResult = DistCombineUnion( vResult, vDistGear2 );
        
    }

    {
        vec4 vDistChainHole = vec4( GetDistanceCylinderY(vRepeatDomain, 0.7), 2.0, vec2(0.0));
        vResult = DistCombineSubtract( vResult, vDistChainHole );

        vec3 vChainDomain = vRepeatDomain;
        float fSpeed = (sin(vChainTile.y + vChainTile.x) + 1.1) * 0.5;
        vChainDomain.y += sin(iGlobalTime * fSpeed);
        vec4 vDistChain = vec4( GetDistanceChain(vChainDomain), 4.0, vec2(0.0));
        vResult = DistCombineUnion( vResult, vDistChain );
    }
    return vResult;
}

C_Material GetObjectMaterial( const in C_HitInfo hitInfo )
{
    C_Material mat;

    if(hitInfo.vObjectId.x < 1.5)
    {
        // floor
        mat.fR0 = 0.02;
        mat.fSmoothness = 0.8;
        mat.cAlbedo = vec3(0.7, 0.8, 0.3);
    }
    else
    if(hitInfo.vObjectId.x < 2.5)
    {
        // hole interior
        mat.fR0 = 0.0;
        mat.fSmoothness = 0.0;
        mat.cAlbedo = vec3(0.7, 0.8, 0.3);
    }
    else
    if(hitInfo.vObjectId.x < 3.5)
    {
        // gear
        mat.fR0 = 0.4;
        mat.fSmoothness = 0.7;
        mat.cAlbedo = vec3(0.5, 0.6, 0.6);
    }
    else
    {
        // chain
        mat.fR0 = 0.2;
        mat.fSmoothness = 0.1;
        mat.cAlbedo = vec3(0.15, 0.125, 0.1);
    }
    
    return mat;
}

float GetRayFirstStep( const in C_Ray ray )
{
    return ray.fStartDistance;  
}


vec3 GetSkyGradient( const in vec3 vDir )
{
    const vec3 cColourTop = vec3(0.7, 0.9, 1.0);
    const vec3 cColourHorizon = vec3(0.2, 0.3, 0.4);

    float fBlend = clamp(vDir.y, 0.0, 1.0);
    return mix(cColourHorizon, cColourTop, fBlend);
}

C_PointLight GetPointLight()
{
    C_PointLight result;

    result.vPos = vec3(sin(iGlobalTime), 2.0 + cos(iGlobalTime * 1.231), cos(iGlobalTime));
    result.cColour = vec3(32.0, 6.0, 1.0);

    return result;
}

C_DirectionalLight GetDirectionalLight()
{
    C_DirectionalLight result;

    result.vDir = normalize(vec3(-0.2, -0.3, 0.5));
    result.cColour = vec3(8.0, 7.5, 7.0);

    return result;
}

vec3 GetAmbientLight(const in vec3 vNormal)
{
    return GetSkyGradient(vNormal);
}

/////////////////////////////////////
// Raymarching 

vec3 GetSceneNormal( const in vec3 vPos )
{
    // tetrahedron normal
    const float fDelta = 0.01;

    vec3 vOffset1 = vec3( fDelta, -fDelta, -fDelta);
    vec3 vOffset2 = vec3(-fDelta, -fDelta,  fDelta);
    vec3 vOffset3 = vec3(-fDelta,  fDelta, -fDelta);
    vec3 vOffset4 = vec3( fDelta,  fDelta,  fDelta);

    float f1 = GetDistanceScene( vPos + vOffset1 ).x;
    float f2 = GetDistanceScene( vPos + vOffset2 ).x;
    float f3 = GetDistanceScene( vPos + vOffset3 ).x;
    float f4 = GetDistanceScene( vPos + vOffset4 ).x;

    vec3 vNormal = vOffset1 * f1 + vOffset2 * f2 + vOffset3 * f3 + vOffset4 * f4;

    return normalize( vNormal );
}

#define kRaymarchEpsilon 0.01
// This is an excellent resource on ray marching -> http://www.iquilezles.org/www/articles/distfunctions/distfunctions.htm
void Raymarch( const in C_Ray ray, out C_HitInfo result, const int maxIter )
{        
    result.fDistance = GetRayFirstStep( ray );
    result.vObjectId.x = 0.0;
        
    for(int i=0;i<=kRaymarchMaxIter;i++)              
    {
        result.vPos = ray.vOrigin + ray.vDir * result.fDistance;
        vec4 vSceneDist = GetDistanceScene( result.vPos );
        result.vObjectId = vSceneDist.yzw;
        
        // abs allows backward stepping - should only be necessary for non uniform distance functions
        if((abs(vSceneDist.x) <= kRaymarchEpsilon) || (result.fDistance >= ray.fLength) || (i > maxIter))
        {
            break;
        }                        

        result.fDistance = result.fDistance + vSceneDist.x; 
    }


    if(result.fDistance >= ray.fLength)
    {
        result.fDistance = 1000.0;
        result.vPos = ray.vOrigin + ray.vDir * result.fDistance;
        result.vObjectId.x = 0.0;
    }
}

float GetShadow( const in vec3 vPos, const in vec3 vNormal, const in vec3 vLightDir, const in float fLightDistance )
{
#ifdef ENABLE_SHADOWS
        C_Ray shadowRay;
        shadowRay.vDir = vLightDir;
        shadowRay.vOrigin = vPos;
        const float fShadowBias = 0.05;
        shadowRay.fStartDistance = fShadowBias / abs(dot(vLightDir, vNormal));
        shadowRay.fLength = fLightDistance - shadowRay.fStartDistance;
    
        C_HitInfo shadowIntersect;
        Raymarch(shadowRay, shadowIntersect, 32);
        
        float fShadow = step(0.0, shadowIntersect.fDistance) * step(fLightDistance, shadowIntersect.fDistance );
        
        return fShadow;          
#else
        return 1.0;
#endif
}

// use distance field to evaluate ambient occlusion
float GetAmbientOcclusion(const in C_HitInfo intersection, const in C_Surface surface)
{
#ifdef ENABLE_AMBIENT_OCCLUSION    
        vec3 vPos = intersection.vPos;
        vec3 vNormal = surface.vNormal;
    
        float fAmbientOcclusion = 1.0;
    
        float fDist = 0.0;
        for(int i=0; i<=5; i++)
        {
            fDist += 0.1;
    
            vec4 vSceneDist = GetDistanceScene(vPos + vNormal * fDist);
    
            fAmbientOcclusion *= 1.0 - max(0.0, (fDist - vSceneDist.x) * 0.2 / fDist );                                  
        }
    
        return fAmbientOcclusion;
#else
        return 1.0;
#endif    
}

/////////////////////////////////////
// Lighting and Shading

#define kFogDensity 0.1

void ApplyAtmosphere(inout vec3 col, const in C_Ray ray, const in C_HitInfo hitInfo)
{
#ifdef ENABLE_FOG
    // fog
    float fFogAmount = exp(hitInfo.fDistance * -kFogDensity);
    vec3 cFog = GetSkyGradient(ray.vDir);

#ifdef ENABLE_DIRECTIONAL_LIGHT_FLARE
    C_DirectionalLight directionalLight = GetDirectionalLight();
    float fDirDot = clamp(dot(-directionalLight.vDir, ray.vDir), 0.0, 1.0);
    cFog += directionalLight.cColour * pow(fDirDot, 10.0);
#endif 

    col = mix(cFog, col, fFogAmount);
#endif

    // glare from light (a bit hacky - use length of closest approach from ray to light)
#ifdef ENABLE_POINT_LIGHT_FLARE
    C_PointLight pointLight = GetPointLight();

    vec3 vToLight = pointLight.vPos - ray.vOrigin;
    float fPointDot = dot(vToLight, ray.vDir);
    fPointDot = clamp(fPointDot, 0.0, hitInfo.fDistance);

    vec3 vClosestPoint = ray.vOrigin + ray.vDir * fPointDot;
    float fDist = length(vClosestPoint - pointLight.vPos);
    col += pointLight.cColour * 0.01/ (fDist * fDist);
#endif    
}

// http://en.wikipedia.org/wiki/Schlick's_approximation
float Schlick( const in vec3 vNormal, const in vec3 vView, const in float fR0, const in float fSmoothFactor)
{
    float fDot = dot(vNormal, -vView);
    fDot = clamp((1.0 - fDot), 0.0, 1.0);
    float fDotPow = pow(fDot, 5.0);
    return fR0 + (1.0 - fR0) * fDotPow * fSmoothFactor;
}

vec3 ApplyFresnel(const in vec3 vDiffuse, const in vec3 vSpecular, const in vec3 vNormal, const in vec3 vView, const in C_Material material)
{
    float fFresnel = Schlick(vNormal, vView, material.fR0, material.fSmoothness * 0.9 + 0.1);
    return mix(vDiffuse, vSpecular, fFresnel);    
}

float GetBlinnPhongIntensity(const in vec3 vIncidentDir, const in vec3 vLightDir, const in vec3 vNormal, const in float fSmoothness)
{          
    vec3 vHalf = normalize(vLightDir - vIncidentDir);
    float fNdotH = max(0.0, dot(vHalf, vNormal));

    float fSpecPower = exp2(4.0 + 6.0 * fSmoothness);
    float fSpecIntensity = (fSpecPower + 2.0) * 0.125;

    return pow(fNdotH, fSpecPower) * fSpecIntensity;
}

C_Shading ApplyPointLight( const in C_PointLight light, const in vec3 vSurfacePos, const in vec3 vIncidentDir, const in vec3 vNormal, const in C_Material material )
{
    C_Shading shading;
    
    vec3 vToLight = light.vPos - vSurfacePos;
    vec3 vLightDir = normalize(vToLight);
    float fLightDistance = length(vToLight);
    
    float fAttenuation = 1.0 / (fLightDistance * fLightDistance);
    
    float fShadowFactor = GetShadow( vSurfacePos, vNormal, vLightDir, fLightDistance );
    vec3 vIncidentLight = light.cColour * fShadowFactor * fAttenuation * max(0.0, dot(vLightDir, vNormal));
    
    shading.cDiffuse = vIncidentLight;                                  
    shading.cSpecular = GetBlinnPhongIntensity( vIncidentDir, vLightDir, vNormal, material.fSmoothness ) * vIncidentLight;
    
    return shading;
}  

C_Shading ApplyDirectionalLight( const in C_DirectionalLight light, const in vec3 vSurfacePos, const in vec3 vIncidentDir, const in vec3 vNormal, const in C_Material material )
{
    C_Shading shading;

    const float kShadowRayLength = 10.0;      
    vec3 vLightDir = -light.vDir;
    float fShadowFactor = GetShadow( vSurfacePos, vNormal, vLightDir, kShadowRayLength );
    vec3 vIncidentLight = light.cColour * fShadowFactor * max(0.0, dot(vLightDir, vNormal));
    
    shading.cDiffuse = vIncidentLight;                                  
    shading.cSpecular = GetBlinnPhongIntensity( vIncidentDir, vLightDir, vNormal, material.fSmoothness ) * vIncidentLight;
    
    return shading;
}  


vec3 ShadeSurface(const in C_Ray ray, const in C_HitInfo hitInfo, const in C_Surface surface, const in C_Material material)
{
    vec3 cScene;
    
    C_Shading shading;

    shading.cDiffuse = vec3(0.0);
    shading.cSpecular = vec3(0.0);
    
    float fAmbientOcclusion = GetAmbientOcclusion(hitInfo, surface);
    vec3 vAmbientLight = GetAmbientLight(surface.vNormal) * fAmbientOcclusion;
    
    shading.cDiffuse += vAmbientLight;
    shading.cSpecular += surface.cReflection;
              
#ifdef ENABLE_POINT_LIGHT
    C_PointLight pointLight = GetPointLight(); 
    C_Shading pointLighting = ApplyPointLight(pointLight, hitInfo.vPos,ray.vDir, surface.vNormal, material);
    shading.cDiffuse += pointLighting.cDiffuse;
    shading.cSpecular += pointLighting.cSpecular;
#endif

#ifdef ENABLE_DIRECTIONAL_LIGHT
    C_DirectionalLight directionalLight = GetDirectionalLight();
    C_Shading directionLighting = ApplyDirectionalLight(directionalLight, hitInfo.vPos, ray.vDir, surface.vNormal, material);
    shading.cDiffuse += directionLighting.cDiffuse;
    shading.cSpecular += directionLighting.cSpecular;
#endif
    
    // fire in the hole
    shading.cDiffuse += clamp(-hitInfo.vPos.y - 0.5, 0.0, 1.0) * vec3(5.0, 0.25, 0.05);

    vec3 vDiffuseReflection = shading.cDiffuse * material.cAlbedo;              

#ifdef ENABLE_SPECULAR
    cScene = ApplyFresnel(vDiffuseReflection , shading.cSpecular, surface.vNormal, ray.vDir, material);
#else
    cScene = vDiffuseReflection;
#endif
    
    return cScene;
}

vec3 GetSceneColourSecondary( const in C_Ray ray );

vec3 GetReflection( const in C_Ray ray, const in C_HitInfo hitInfo, const in C_Surface surface )
{
#ifdef ENABLE_REFLECTIONS    
    {
        // get colour from reflected ray
        const float fSeparation    = 0.1;

        C_Ray reflectRay;
        reflectRay.vDir = reflect(ray.vDir, surface.vNormal);
        reflectRay.vOrigin = hitInfo.vPos;
        reflectRay.fLength = 16.0;
        reflectRay.fStartDistance = fSeparation / abs(dot(reflectRay.vDir, surface.vNormal));
        
        return GetSceneColourSecondary(reflectRay);      
    }
#else
        return GetSkyGradient(reflect(ray.vDir, surface.vNormal));                              
#endif
}

// no reflections, no transparency, used for secondary rays
vec3 GetSceneColourSecondary( const in C_Ray ray )
{
    C_HitInfo hitInfo;
    Raymarch(ray, hitInfo, 32);
                        
    vec3 cScene;

    if(hitInfo.vObjectId.x < 0.5)
    {
        cScene = GetSkyGradient(ray.vDir);
    }
    else
    {
        C_Surface surface;        
        surface.vNormal = GetSceneNormal(hitInfo.vPos);

        C_Material material = GetObjectMaterial(hitInfo);

        // use sky gradient instead of reflection
        surface.cReflection = GetSkyGradient(reflect(ray.vDir, surface.vNormal));

        // apply lighting
        cScene = ShadeSurface(ray, hitInfo, surface, material);
    }

    ApplyAtmosphere(cScene, ray, hitInfo);

    return cScene;
}

vec3 GetSceneColourPrimary( const in C_Ray ray )
{                                                          
    C_HitInfo intersection;
    Raymarch(ray, intersection, 256);
                
    vec3 cScene;

    if(intersection.vObjectId.x < 0.5)
    {
        cScene = GetSkyGradient(ray.vDir);
    }
    else
    {
        C_Surface surface;
        
        surface.vNormal = GetSceneNormal(intersection.vPos);

        C_Material material = GetObjectMaterial(intersection);

        surface.cReflection = GetReflection(ray, intersection, surface);

        // apply lighting
        cScene = ShadeSurface(ray, intersection, surface, material);
    }

    ApplyAtmosphere(cScene, ray, intersection);

    return cScene;
}

float kFarClip = 30.0;

void GetCameraRay( const in vec3 vPos, const in vec3 vForwards, const in vec3 vWorldUp, out C_Ray ray)
{
    vec2 vUV = ( gl_FragCoord.xy / iResolution.xy );
    vec2 vViewCoord = vUV * 2.0 - 1.0;

    float fRatio = iResolution.x / iResolution.y;
    vViewCoord.y /= fRatio;                          

    ray.vOrigin = vPos;

    vec3 vRight = normalize(cross(vForwards, vWorldUp));
    vec3 vUp = cross(vRight, vForwards);
        
    ray.vDir = normalize( vRight * vViewCoord.x + vUp * vViewCoord.y + vForwards); 
    ray.fStartDistance = 0.0;
    ray.fLength = kFarClip;      
}

void GetCameraRayLookat( const in vec3 vPos, const in vec3 vInterest, out C_Ray ray)
{
    vec3 vForwards = normalize(vInterest - vPos);
    vec3 vUp = vec3(0.0, 1.0, 0.0);

    GetCameraRay(vPos, vForwards, vUp, ray);
}

vec3 OrbitPoint( const in float fHeading, const in float fElevation )
{
    return vec3(sin(fHeading) * cos(fElevation), sin(fElevation), cos(fHeading) * cos(fElevation));
}

vec3 Gamma( const in vec3 cCol )
{
    return sqrt(cCol);
}

vec3 Tonemap( const in vec3 cCol )
{
    vec3 vResult = 1.0 - exp2(-cCol);

    return vResult;
}

void main( void )
{
    C_Ray ray;

    GetCameraRayLookat( OrbitPoint(iGlobalTime * 0.3, cos(iGlobalTime * 0.2) * 0.3 + 0.4) * 7.0, vec3(0.0, 0.0, 0.0), ray);

    vec3 cScene = GetSceneColourPrimary( ray );  

    const float fExposure = 1.5;    
    color = vec4( Tonemap(cScene * fExposure), 1.0 );
}

