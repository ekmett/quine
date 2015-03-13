#ifndef INCLUDED_SIMTIME_H
#define INCLUDED_SIMTIME_H

layout (std140) uniform time {
  float now;   // number of seconds since the start of the simulation
  float alpha; // interpolation ratio between the last two physics frame
} time;

#endif
