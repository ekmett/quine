// SoC with Light by eiffie (added a light to the Sphere of Confusion script)
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

// https://www.shadertoy.com/view/4s23zW

#pragma version 410 core
#include "locations.h"

uniform vec2 iResolution = vec2(640.,480.);
uniform float iGlobalTime = 0.;

layout(location = FRAGMENT_COLOR) out vec4 color;

#define time iGlobalTime*0.5
#define size iResolution

float focalDistance=1.0,aperature=0.025,fudgeFactor=1.0,shadowCone=0.2;

bool bColoring=false;
vec3 mcol;
const int iters=3;

float scale=4.0*0.9000;vec3 offset=vec3(0.8700,0.6300+sin(time*0.1)*0.1,0.3100)*2.0;//menger
//float scale=2.0;vec3 offset=vec3(1.0,0.0,0.0);//sierpinski
float psni=pow(scale,-float(iters));
float DE(in vec3 z){//menger sponge by menger
	float flr=z.y+scale*0.4;
	for (int n = 0; n < iters; n++) {
		z = abs(z);
		if (z.x<z.y)z.xy = z.yx;
		if (z.x<z.z)z.xz = z.zx;
		if (z.y<z.z)z.yz = z.zy;
		z = z*scale - offset*(scale-1.0);
		if(z.z<-0.5*offset.z*(scale-1.0))z.z+=offset.z*(scale-1.0);
	}
	if(bColoring)mcol+=vec3(0.5)+sin(z)*((flr<0.1)?0.0:0.4);
	z=abs(z)-vec3(0.1,0.1,0.88)*scale;
	float d=min(flr,max(z.x,max(z.y,z.z))*psni);
	return d;
}

float Segment(vec3 p, vec3 p0, vec3 p1, float r){vec3 v=p1-p0;v*=clamp(dot(p-p0,v)/dot(v,v),0.0,1.0);return distance(p-p0,v)-r;}//from iq

float pixelSize;
float CircleOfConfusion(float t){//calculates the radius of the circle of confusion at length t
	return max(abs(focalDistance-t)*aperature,pixelSize*(1.0+t));
}
mat3 lookat(vec3 fw,vec3 up){
	fw=normalize(fw);vec3 rt=normalize(cross(fw,normalize(up)));return mat3(rt,cross(rt,fw),fw);
}
float linstep(float a, float b, float t){return clamp((t-a)/(b-a),0.,1.);}// i got this from knighty and/or darkbeam
float rand(vec2 co){// implementation found at: lumina.sourceforge.net/Tutorials/Noise.html
	return fract(sin(dot(co*0.123,vec2(12.9898,78.233))) * 43758.5453);
}
float FuzzyShadow(vec3 ro, vec3 rd, float lightDist, float coneGrad, float rCoC){
	float t=0.01,d=1.0,s=1.0;
	for(int i=0;i<16;i++){
		if(t>lightDist)continue;
		float r=rCoC+t*coneGrad;//radius of cone
		d=DE(ro+rd*t)+r*0.66;
		s*=linstep(-r,r,d);
		t+=abs(d)*(0.8+0.2*rand(gl_FragCoord.xy*vec2(i)));
	}
	return clamp(s,0.0,1.0);
}

void main() {
	pixelSize=1.0/size.y;
	float tim=time*0.5;
	vec3 ey=vec3(cos(tim),sin(tim*1.3)*0.35,sin(tim*0.87))*1.5;
	vec3 ro=mix(vec3(0.2,0.0,0.25),ey,length(ey));
	vec3 rd=lookat(-ro,vec3(0.0,1.0,0.0))*normalize(vec3((2.0*gl_FragCoord.xy-size.xy)/size.y,2.0));
	vec3 lightPos=vec3(cos(tim),0.0,sin(tim*0.5))*2.0;
	vec3 lightColor=vec3(1.0,0.5,0.25)*2.0,lp=ro;
	vec4 col=vec4(0.0);//color accumulator
	float t=0.0;//distance traveled
	for(int i=1;i<70;i++){//march loop
		if(col.w>0.9 || t>7.0)continue;//bail if we hit a surface or go out of bounds
		float rCoC=CircleOfConfusion(t);//calc the radius of CoC
		float d=DE(ro)+0.5*rCoC;
		if(d<rCoC){//if we are inside add its contribution
			vec3 p=ro-rd*abs(d-rCoC);//back up to border of CoC
			mcol=vec3(0.0);//clear the color trap, collecting color samples with normal deltas
			bColoring=true;
			vec2 v=vec2(rCoC*0.5,0.0);//use normal deltas based on CoC radius
			vec3 N=normalize(vec3(-DE(p-v.xyy)+DE(p+v.xyy),-DE(p-v.yxy)+DE(p+v.yxy),-DE(p-v.yyx)+DE(p+v.yyx)));
			bColoring=false;
			if(N!=N)N=-rd;//if we failed to find any direction assume facing camera
			vec3 L=lightPos-p;//the direction to the light
			float lightDist=length(L);//how far is the light
			L/=lightDist;//normalize the direction
			float lightStrength=1.0/(1.0+lightDist*lightDist*0.1);//how much light is there?
			vec3 scol=mcol*0.1666*(0.2+0.4*(1.0+dot(N,L)))*lightStrength;
			scol+=0.25*pow(max(0.0,dot(reflect(rd,N),L)),32.0)*lightColor*lightStrength;
			scol*=FuzzyShadow(p,L,lightDist,shadowCone,rCoC);
			lightDist=Segment(lightPos,lp,ro,0.01);//for the bloom find the nearest distance between ray and lightPos
			col.rgb+=lightColor/(1.0+lightDist*lightDist*100.0)*(1.0-clamp(col.w,0.0,1.0));//add bloom (should use rCoC)
			lp=ro;//save this point for next Segment calc
			float alpha=fudgeFactor*(1.0-col.w)*linstep(-rCoC,rCoC,-d);//calculate the mix like cloud density
			col+=vec4(scol*alpha,alpha);//blend in the new color
		}
		d=abs(fudgeFactor*d*(0.8+0.2*rand(gl_FragCoord.xy*vec2(i))));//add in noise to reduce banding and create fuzz
		ro+=d*rd;//march
		t+=d;
	}//mix in background color
	vec3 scol=vec3(0.4)+rd*0.1;
	float lightDist=Segment(lightPos,lp,ro+rd*7.0,0.01);//one last light bloom calc
	scol+=lightColor/(1.0+lightDist*lightDist*100.0);
	col.rgb+=scol*(1.0-clamp(col.w,0.0,1.0));

	color = vec4(clamp(col.rgb,0.0,1.0),1.0);
}

