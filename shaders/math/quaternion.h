#ifndef INCLUDED_MATH_QUATERNION_H
#define INCLUDED_MATH_QUATERNION_H

struct Quaternion {
  vec4 data;
};

Quaternion quaternion(vec3 a, float b) {
  return Quaternion(vec4(a,b));
}

float scalar(Quaternion a) {
  return a.data.x;
}

vec3 ijk(Quaternion a) {
  return a.data.yzw;
}

Quaternion add(Quaternion a, Quaternion b) {
  return Quaternion(a.data+b.data);
}

Quaternion mul(Quaternion a, Quaternion b) {
  return Quaternion(vec4(scalar(a)*scalar(b) - dot(ijk(a),ijk(b)), cross(ijk(a),ijk(b)) + scalar(a)*ijk(b) + scalar(b)*ijk(a)));

}

mat3 rot(Quaternion a) {
  float w = scalar(a);
  vec3 v = ijk(a);
  vec3 sq = v*v;

  return mat3(
    1-2*(sq.y+sq.z),
    2*(v.x*v.y+v.z*w),
    2*(v.x*v.z-v.y*w),
    2*(v.x*v.y-v.z*w),
    1-2*(sq.x+sq.z),
    2*(v.y*v.z+v.x*w),
    2*(v.x*v.z+v.y*w),
    2*(v.y*v.z-v.x*w),
    1-2*(sq.x+sq.y)
  );
}

#endif // INCLUDED_MATH_QUATERNION_H
