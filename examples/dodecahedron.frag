#pragma version 410 core
#include "locations.h"

uniform vec2 iResolution = vec2(640.,480.);
uniform float iGlobalTime = 0.;

layout(location = FRAGMENT_COLOR) out vec4 color;

// based on https://www.shadertoy.com/view/MdS3Rw

// ray marching
const int max_iterations = 128;
const float stop_threshold = 0.001;
const float grad_step = 0.01;
const float clip_far = 1000.0;

// ao
const int   ao_iterations = 5;
const float ao_step = 0.2;
const float ao_scale = 1.46;

// math
const float PI = 3.14159265359;
const float DEG_TO_RAD = PI / 180.0;
const float GOLDEN = 1.6180339887499;

const float r = 1.0 * 2.0 * 1.414214 / 1.732051;
const vec2 h = vec2( r, 0.01 );
const vec2 h2 = h * vec2( 0.73, 4.0 );

const vec3 cc0 = vec3( 0.333333, -0.333333, -0.333333 );
const vec3 cx0 = vec3( 0.707107, 0.000000, 0.707107 );
const vec3 cy0 = vec3( 0.408248, 0.816496, -0.408248 );
const vec3 cz0 = vec3( -0.577350, 0.577350, 0.577350 );

const vec3 cc1 = vec3( -0.333333, -0.333333, 0.333333 );
const vec3 cx1 = vec3( 0.707107, 0.000000, 0.707107 );
const vec3 cy1 = vec3( -0.408248, 0.816496, 0.408248 );
const vec3 cz1 = vec3( -0.577350, -0.577350, 0.577350 );

const vec3 cc2 = vec3( -0.333333, 0.333333, -0.333333 );
const vec3 cx2 = vec3( 0.707107, 0.707107, 0.000000 );
const vec3 cy2 = vec3( -0.408248, 0.408248, 0.816496 );
const vec3 cz2 = vec3( 0.577350, -0.577350, 0.577350 );

const vec3 cc3 = vec3( 0.333333, 0.333333, 0.333333 );
const vec3 cx3 = vec3( 0.000000, 0.707107, -0.707107 );
const vec3 cy3 = vec3( -0.816496, 0.408248, 0.408248 );
const vec3 cz3 = vec3( 0.577350, 0.577350, 0.577350 );

const vec3 c0 = vec3( 0.333333, 0.333333, -0.333333 );
const vec3 x0 = vec3( 0.572061, 0.218508, 0.790569 );
const vec3 y0 = vec3( -0.582591, 0.786715, 0.204124 );
const vec3 z0 = vec3( -0.577350, -0.577350, 0.577350 );

const vec3 c1 = vec3( 0.206011, -0.539344, 0.000000 );
const vec3 x1 = vec3( 0.572061, 0.218508, 0.790569 );
const vec3 y1 = vec3( -0.738528, -0.282093, 0.612372 );
const vec3 z1 = vec3( 0.356822, -0.934172, 0.000000 );

const vec3 c2 = vec3( -0.539344, 0.000000, -0.206011 );
const vec3 x2 = vec3( -0.218508, 0.790569, 0.572061 );
const vec3 y2 = vec3( -0.282093, -0.612372, 0.738528 );
const vec3 z2 = vec3( 0.934172, 0.000000, 0.356822 );

const vec3 c3 = vec3( 0.000000, 0.206011, 0.539344 );
const vec3 x3 = vec3( -0.790569, 0.572061, -0.218508 );
const vec3 y3 = vec3( -0.612372, -0.738528, 0.282093 );
const vec3 z3 = vec3( -0.000000, 0.356822, 0.934172 );

// distance function

// iq's Signed Triangular Prism distance function
float dist_triXY( vec3 p, vec2 h ) {
    vec3 q = abs(p);
    return max(q.z-h.y,max(q.x*0.866025+p.y*0.5,-p.y)-h.x*0.5);
}

float dist_tri( vec3 v, vec3 c, vec3 x, vec3 y, vec3 z ) {
    v -= c;
    v = vec3( dot( v, x ), dot( v, y ), dot( v, z ) );
    return max( dist_triXY( v, h  ), -dist_triXY( v, h2 ) );
}

float dist_field( vec3 v ) {
    float b0, b1, b2, b3, b4;

    // cube
    {
        float d0 = dist_tri( v, cc0, cx0, cy0, cz0 );
        float d1 = dist_tri( v, cc1, cx1, cy1, cz1 );
        float d2 = dist_tri( v, cc2, cx2, cy2, cz2 );
        float d3 = dist_tri( v, cc3, cx3, cy3, cz3 );
        b0 = min( min( d0, d1 ), min( d2, d3 ) );
    }

    // xyz
    {
        float d0 = dist_tri( v, c0, x0, y0, z0 );
        float d1 = dist_tri( v, c1, x1, y1, z1 );
        float d2 = dist_tri( v, c2, x2, y2, z2 );
        float d3 = dist_tri( v, c3, x3, y3, z3 );
        b1 = min( min( d0, d1 ), min( d2, d3 ) );
    }

    // zx
    {
        v.zx = -v.zx;
        float d0 = dist_tri( v, c0, x0, y0, z0 );
        float d1 = dist_tri( v, c1, x1, y1, z1 );
        float d2 = dist_tri( v, c2, x2, y2, z2 );
        float d3 = dist_tri( v, c3, x3, y3, z3 );
        v.zx = -v.zx;
        b2 = min( min( d0, d1 ), min( d2, d3 ) );
    }

    // yz
    {
        v.yz = -v.yz;
        float d0 = dist_tri( v, c0, x0, y0, z0 );
        float d1 = dist_tri( v, c1, x1, y1, z1 );
        float d2 = dist_tri( v, c2, x2, y2, z2 );
        float d3 = dist_tri( v, c3, x3, y3, z3 );
        v.yz = -v.yz;
        b3 = min( min( d0, d1 ), min( d2, d3 ) );
    }

    // xy
    {
        v.xy = -v.xy;
        float d0 = dist_tri( v, c0, x0, y0, z0 );
        float d1 = dist_tri( v, c1, x1, y1, z1 );
        float d2 = dist_tri( v, c2, x2, y2, z2 );
        float d3 = dist_tri( v, c3, x3, y3, z3 );
        v.xy = -v.xy;
        b4 = min( min( d0, d1 ), min( d2, d3 ) );
    }

    return min( b0, min( min( b1, b2 ), min( b3, b4 ) ) );
}

// ao
float ao( vec3 v, vec3 n ) {
    float sum = 0.0;
    float att = 1.0;
    float len = ao_step;
    for ( int i = 0; i < ao_iterations; i++ ) {
        sum += ( len - dist_field( v + n * len ) ) * att;

        len += ao_step;

        att *= 0.5;
    }

    return max( 1.0 - sum * ao_scale, 0.0 );
}


// get gradient in the world
vec3 gradient( vec3 v ) {
    const vec3 dx = vec3( grad_step, 0.0, 0.0 );
    const vec3 dy = vec3( 0.0, grad_step, 0.0 );
    const vec3 dz = vec3( 0.0, 0.0, grad_step );
    return normalize (
        vec3(
            dist_field( v + dx ) - dist_field( v - dx ),
            dist_field( v + dy ) - dist_field( v - dy ),
            dist_field( v + dz ) - dist_field( v - dz )
        )
    );
}

// ray marching
float ray_marching( vec3 origin, vec3 dir, float start, float end ) {
    float depth = start;
    for ( int i = 0; i < max_iterations; i++ ) {
        float dist = dist_field( origin + dir * depth );
        if ( dist < stop_threshold ) {
            return depth;
        }
        depth += dist;
        if ( depth >= end) {
            return end;
        }
    }
    return end;
}

// shadow
float shadow( vec3 v, vec3 light ) {
    vec3 lv = v - light;
    float end = length( lv );
    lv /= end;

    float depth = ray_marching( light, lv, 0.0, end );

    return step( end - depth, 0.02 );
}

// phong shading
vec3 shading( vec3 v, vec3 n, vec3 eye ) {
    // ...add lights here...

    vec3 final = vec3( 0.0 );

    vec3 ev = normalize( v - eye );
    vec3 ref_ev = reflect( ev, n );

    // light 0
    {
        vec3 light_pos   = vec3( 5.0 );

        vec3 vl = normalize( light_pos - v );

        float diffuse  = max( 0.0, dot( vl, n ) );
        float specular = max( 0.0, dot( vl, ref_ev ) );
        specular = pow( specular, 12.0 );

        final += vec3( 0.9 ) * ( diffuse * 0.4 + specular * 0.9 ) * shadow( v, light_pos );
    }

    // light 1
    {
        vec3 light_pos   = vec3( -5.0 );

        vec3 vl = normalize( light_pos - v );

        float diffuse  = max( 0.0, dot( vl, n ) );
        float specular = max( 0.0, dot( vl, ref_ev ) );
        specular = pow( specular, 64.0 );

        final += vec3( 0.1 ) * ( diffuse * 0.4 + specular * 0.9 );
    }

    final += ao( v, n ) * vec3( 0.15 );

    return final;
}

// pitch, yaw
mat3 rot3xy( vec2 angle ) {
    vec2 c = cos( angle );
    vec2 s = sin( angle );

    return mat3(
        c.y      ,  0.0, -s.y,
        s.y * s.x,  c.x,  c.y * s.x,
        s.y * c.x, -s.x,  c.y * c.x
    );
}

// get ray direction
vec3 ray_dir( float fov, vec2 size, vec2 pos ) {
    vec2 xy = pos - size * 0.5;

    float cot_half_fov = tan( ( 90.0 - fov * 0.5 ) * DEG_TO_RAD );
    float z = size.y * 0.5 * cot_half_fov;

    return normalize( vec3( xy, -z ) );
}

void main(void)
{
    // default ray dir
    vec3 dir = ray_dir( 45.0, iResolution.xy, gl_FragCoord.xy );

    // default ray origin
    vec3 eye = vec3( 0.0, 0.0, 4.4 );

    // rotate camera
    mat3 rot = rot3xy( vec2( -DEG_TO_RAD * 30.0, iGlobalTime * 0.5 ) );
    dir = rot * dir;
    eye = rot * eye;

    // ray marching
    float depth = ray_marching( eye, dir, 0.0, clip_far );
    if ( depth >= clip_far ) {
        discard;
    }

    // shading
    vec3 pos = eye + dir * depth;
    vec3 n = gradient( pos );
    color = vec4( shading( pos, n, eye ) * 2.0, 1.0 );
}
