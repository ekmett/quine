#version 410

uniform vec2 iResolution = vec2(640.,480.);
uniform float iGlobalTime = 0.;

layout(location = FRAGMENT_COLOR) out vec4 color;

// Prairie by eiffie
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.

// https://www.shadertoy.com/view/Ms23zw


#define time iGlobalTime*0.5
#define size iResolution
const float aperture=0.01,shadowCone=0.6;

// See http://www.iquilezles.org/www/articles/morenoise/morenoise.htm
float hash(float n) {return fract(sin(n) * 43758.5453123);}
float noyz(vec2 x) {
	vec2 p=floor(x),j=fract(x);
	const float tw=117.0;
	float n=p.x+p.y*tw;
	float a=hash(n),b=hash(n+1.0),c=hash(n+tw),d=hash(n+tw+1.0);
	vec2 u=j*j*(3.0-2.0*j);
	return a+(b-a)*u.x+(c-a)*u.y+(a-b-c+d)*u.x*u.y;
}

float fbm(vec2 p) {
	float h=noyz(p);
	h+=0.5*noyz(p*=2.3);
	return h+0.25*noyz(p*2.3);
}
//and 3d versions
float noyz(vec3 x) {
	vec3 p=floor(x),j=fract(x);
	const float tw=117.0,tx=11768.0;
	float n=p.x+p.y*tw+p.z*tx;
	float a=hash(n),b=hash(n+1.0),c=hash(n+tw),d=hash(n+tw+1.0);
	float e=hash(n+tx),f=hash(n+1.0+tx),g=hash(n+tw+tx),h=hash(n+1.0+tw+tx);
	vec3 u=j*j*(3.0-2.0*j);
	return mix(a+(b-a)*u.x+(c-a)*u.y+(a-b-c+d)*u.x*u.y,e+(f-e)*u.x+(g-e)*u.y+(e-f-g+h)*u.x*u.y,u.z);
}
float fbm(vec3 p) {
	float h=noyz(p);
	h+=0.5*noyz(p*=2.3);
	return h+0.25*noyz(p*2.3);
}

//from iq's 3d voronoi sample (converted back to 2d)
//find the original here - https://www.shadertoy.com/view/ldl3Dl
vec2 hash( vec2 p )
{
	p = vec2( dot(p,vec2(127.1,311.7)),
			  dot(p,vec2(269.5,183.3)));

	return fract(sin(p)*43758.5453123);
}
// returns closest, id
vec2 voronoi( in vec2 x )
{
	vec2 p = floor( x );
	vec2 f = fract( x )-vec2(0.125);
	float res=100.0,id;
	for( int j=-1; j<=1; j++ )
	for( int i=-1; i<=1; i++ )
	{
		vec2 b = vec2( float(i), float(j) );
		vec2 r = vec2( b ) - f  + 0.75*hash( p + b );
		float d = dot(r,r);
		if( d < res ) {res = d; id=dot(p+b,vec2(57.0,113.0));}			
    	}
	return vec2(sqrt(res),id);
}

float DE(in vec3 z0)
{//voronoi grass and fbm clouds
	float y=z0.y+fbm(z0.xz*0.5)*0.5;
	vec2 f=voronoi(z0.xz*10.0+sin(y*10.0+z0.zx)*0.1*y+vec2(abs(sin(time*5.0+0.5*z0.z)),sin(time*6.6+z0.x))*y*y*2.0);
	float d=f.x*0.1-0.02+(y+fract(sin(f.y)*43758.5453123))*0.02;
	d=min(max(d,y-1.0),y);
	float dC=fbm(z0+vec3(time*0.25))*0.5+sin(z0.z*0.4)*0.1+abs(z0.y-4.0)*0.58-0.4;
	return min(d,dC);
}

float pixelSize;
float CircleOfConfusion(float t){//calculates the radius of the circle of confusion at length t
	return max(t*aperture,pixelSize*(1.0+t));
}
mat3 lookat(vec3 fw,vec3 up){
	fw=normalize(fw);vec3 rt=normalize(cross(fw,normalize(up)));return mat3(rt,cross(rt,fw),fw);
}
float linstep(float a, float b, float t){return clamp((t-a)/(b-a),0.,1.);}// i got this from knighty and/or darkbeam
//random seed and generator
float randSeed;
float randStep(){//a simple jittered step generator based on iq's hash
	return  0.8+0.2*hash(++randSeed);
}

float FuzzyShadow(vec3 ro, vec3 rd, float coneGrad, float rCoC){
	float t=0.0,d=1.0,s=1.0;
	ro+=rd*rCoC*2.0;
	for(int i=0;i<2;i++){
		float r=rCoC+t*coneGrad;//radius of cone
		d=DE(ro+rd*t)+r*0.5;
		s*=linstep(-r,r,d);
		t+=abs(d)*randStep();
	}
	return clamp(s*0.5+0.5,0.0,1.0);
}

void main() {
	randSeed=fract(cos((gl_FragCoord.x+gl_FragCoord.y*117.0+time*10.0)*473.7192451));
	pixelSize=2.0/size.y;
	vec3 ro=vec3(4.3,2.82,4.3);
	vec3 rd=lookat(vec3(0.0,-0.05,-1.0),vec3(0.0,1.0,0.0))*normalize(vec3((2.0*gl_FragCoord.xy-size.xy)/size.y,2.0));
	vec3 L=normalize(vec3(0.5,0.4,-0.6));
	vec4 col=vec4(0.0);//color accumulator
	vec3 mcol;
	float t=0.0,alpha;//distance traveled
	for(int i=0;i<70;i++){//march loop
		if(col.w>0.9 || t>20.0)continue;//bail if we hit a surface or go out of bounds
		float rCoC=CircleOfConfusion(t);//calc the radius of CoC
		float d=DE(ro)+0.5*rCoC;
		if(d<rCoC){//if we are inside add its contribution
			if(ro.y>2.0){
				mcol=vec3(1.0);
				alpha=d-2.0*rCoC;
				alpha*=alpha*0.5;
			}else{
				mcol=vec3(0.6,0.6,0.2)*(0.33+0.67*ro.y);
				alpha=1.0;
			}
			vec3 scol=mcol.rgb*FuzzyShadow(ro,L,shadowCone,rCoC);
			alpha*=(1.0-col.w)*linstep(-rCoC,rCoC,-d);//calculate the mix like cloud density
			col+=vec4(scol*alpha,alpha);//blend in the new color			
		}
		d=max(d,pixelSize)*randStep();//add in noise to reduce banding and create fuzz
		ro+=d*rd;//march
		t+=d;
	}//mix in background color
	col.rgb+=mix(vec3(0.7,0.8,1.0),vec3(0.8,0.9,1.0)*(0.5+pow(max(0.0,dot(rd,L)),80.0)),smoothstep(-0.05,0.25,rd.y))*(1.0-clamp(col.w,0.0,1.0));
	color = vec4(clamp(col.rgb,0.0,1.0),1.0);
}

