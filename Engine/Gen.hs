module Engine.Gen where

import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw.Core31

data Gen = Gen
  { gen :: GLsizei -> Ptr GLuint -> IO ()
  , del :: GLsizei -> Ptr GLuint -> IO ()
  }

textureGen, bufferGen, queryGen, renderbufferGen, vertexArrayGen :: Gen
textureGen = Gen glGenTextures glDeleteTextures
bufferGen = Gen glGenBuffers glDeleteBuffers
queryGen = Gen glGenQueries glDeleteQueries
renderbufferGen = Gen glGenRenderbuffers glDeleteRenderbuffers
vertexArrayGen = Gen glGenVertexArrays glDeleteVertexArrays
