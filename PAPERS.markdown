# References

## Signed Distance Fields

* [Rendering Worlds with Two Triangles](http://www.iquilezles.org/www/material/nvscene2008/rwwtt.pdf) by Iñigo Quilez
* [Rendering a Screen Covering Triangle in OpenGL (with no buffers)](http://rauwendaal.net/2014/06/14/rendering-a-screen-covering-triangle-in-opengl/) by Randall Roel Rauwendall
* [Fast Approximations of Lights for Dynamic Scenes](http://amd-dev.wpengine.netdna-cdn.com/wordpress/media/2012/10/Evans-Fast_Approximations_for_Lighting_of_Dynamic_Scenes-print.pdf) by Alex Evans is a pretty amazing tour de force and pretty much the first thing I ever saw that smashed together lots of SDFs into grids.

## Shadow Mapping

* [Shadow Mapping for Hemispherical and Omnidirectional Light Sources](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.11.3540&rep=rep1&type=pdf) by Brabec, Annen, and Seidel covers (dual) parabolic shadow mapping, which would let us get away with a single shadow map per hemisphere, especially if we didn't have any of those pesky triangles. Hrmm. =)

## Clustered Shading

* [Practical Clustered Shading](http://www.humus.name/Articles/PracticalClusteredShading.pdf) by Emil Persson, Avalanche

## Reflections

* [The Future of Screenspace Reflections](http://www.gamasutra.com/blogs/BartlomiejWronski/20140129/209609/The_future_of_screenspace_reflections.php) by Bartlomiej Wronski talks about some of the issues with using screen space approaches for reflections. However, it is interesting that at least in screenspace the reflections can reflect specular highlights.
