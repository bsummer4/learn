{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnicodeSyntax #-}

import Prelude.Unicode
import Control.Monad.Unicode
import Graphics.GL
import qualified Graphics.UI.GLFW as GLFW

import Linear (V2(..), V3(..), M44, Quaternion,
               perspective, lookAt, axisAngle,
               mkTransformation, (!*!), inv33,
               column, _xyz, negated, identity)

import Data.Distributive (distribute)
import Control.Lens      ((^.))

import Codec.Picture (readPng, Image(Image), DynamicImage(ImageRGBA8))

import Control.Monad       (void, when, unless, liftM2)
import Control.Applicative ((<$>), (<*>), pure)
import System.IO           (hSetBuffering, stdout, BufferMode(LineBuffering))
import Data.IORef          (IORef, newIORef, writeIORef, readIORef)
import Data.Bits           ((.|.))

import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import qualified Data.Text.Foreign as T
import qualified Data.Vector       as V

import qualified Data.Vector.Storable as SV

import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Storable      (peek, sizeOf)
import Foreign.Ptr           (Ptr, nullPtr, castPtr, wordPtrToPtr)

import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Cont  (ContT(..), evalContT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class    (liftIO)

getErrors ∷ IO [GLuint]
getErrors = do
  err ← glGetError
  if err == GL_NO_ERROR
     then return []
     else do
       errs ← getErrors
       return $ err:errs

showError ∷ GLuint → String
showError GL_INVALID_ENUM = "GL_INVALID_ENUM"
showError GL_INVALID_VALUE = "GL_INVALID_VALUE"
showError GL_INVALID_OPERATION = "GL_INVALID_OPERATION"
showError GL_INVALID_FRAMEBUFFER_OPERATION = "GL_INVALID_FRAMEBUFFER_OPERATION"
showError GL_OUT_OF_MEMORY = "GL_OUT_OF_MEMORY"
showError GL_STACK_UNDERFLOW = "GL_STACK_UNDERFLOW"
showError GL_STACK_OVERFLOW = "GL_STACK_OVERFLOW"
showError x = "GL Error " ++ show x

printErrors ∷ String → IO ()
printErrors prefix = do
  es ← map showError <$> getErrors
  when (not $ null es) $
    error (prefix ++ ": " ++ show es)

main ∷ IO ()
main = do
  hSetBuffering stdout LineBuffering

  success ← GLFW.init
  if not success
  then void $ putStrLn "Failed to initialise GLFW"
  else do

    mapM_ GLFW.windowHint
      [ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL
      , GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      , GLFW.WindowHint'OpenGLForwardCompat True
      , GLFW.WindowHint'ContextVersionMajor 3
      , GLFW.WindowHint'ContextVersionMinor 2
      ]

    w ← GLFW.createWindow 480 320 "Haskell GL"
        Nothing Nothing
    case w of
      Nothing  → putStrLn "Failed to create window"
      Just win → do
        GLFW.makeContextCurrent w

        closed ← newIORef False
        GLFW.setWindowCloseCallback win $
          Just (const $ writeIORef closed True)

        dims ← GLFW.getWindowSize win
        projectionMatrix ← newIORef $
          calculateProjectionMatrix dims

        GLFW.setWindowSizeCallback win $
          Just (const $ resize projectionMatrix)

        let swapper = GLFW.swapBuffers win ≫ GLFW.pollEvents

        initialise ≫= runDemo closed projectionMatrix swapper
        GLFW.terminate

getDeltaTime ∷ IO GLfloat
getDeltaTime = do
  t ← GLFW.getTime
  GLFW.setTime 0
  return $ maybe 0 (fromRational ∘ toRational) t

data MeshSpec = MeshSpec
  { specPositions ∷ V.Vector (V3 GLfloat)
  , specColours   ∷ V.Vector (V3 GLfloat)
  , specNormals   ∷ V.Vector (V3 GLfloat)
  , specUVs       ∷ V.Vector (V2 GLfloat)
  , specIndices   ∷ V.Vector (GLuint, GLuint, GLuint)
  }

v3 ∷ (a, a, a) → V3 a
v3 (x, y, z) = V3 x y z

cuboid ∷ GLfloat → GLfloat → GLfloat → MeshSpec
cuboid l' h' d' =
  MeshSpec positions colours normals uvs indices where
    l = l' * 0.5; d = d' * 0.5; h = h' * 0.5
    positions = V.fromList
      [v3 ( l, h, d), v3 ( l,-h, d), v3 ( l,-h,-d), v3 ( l, h,-d),
       v3 ( l, h,-d), v3 (-l, h,-d), v3 (-l, h, d), v3 ( l, h, d),
       v3 (-l, h, d), v3 (-l,-h, d), v3 ( l,-h, d), v3 ( l, h, d),
       v3 (-l, h,-d), v3 (-l,-h,-d), v3 (-l,-h, d), v3 (-l, h, d),
       v3 ( l,-h,-d), v3 ( l,-h, d), v3 (-l,-h, d), v3 (-l,-h,-d),
       v3 ( l, h,-d), v3 ( l,-h,-d), v3 (-l,-h,-d), v3 (-l, h,-d)]
    colours = V.map ((/ V3 l' h' d') ∘ (+ V3 l h d)) positions
    normals = V.concat ∘ map (V.replicate 4) $ ns ++ negated ns
      where ns = [V3 1 0 0, V3 0 1 0, V3 0 0 1]
    uvs = V.concat ∘ replicate 6 $
      V.fromList [V2 0 0, V2 0 1, V2 1 1, V2 1 0]

    indices =
      quads ∘ V.zipWith forFace (V.fromList [0..]) ∘ V.replicate 6 $ (0, 1, 2, 3)
      where
        forFace i (a, b, c, d)   = (a + i*4, b + i*4, c + i*4, d + i*4)
        quads                    ∷ V.Vector (a,a,a,a) → V.Vector (a,a,a)
        quads                    = V.concatMap triangulate
        triangulate ∷ (a,a,a,a) → V.Vector (a,a,a)
        triangulate (a, b, c, d) = V.fromList [(a, b, c), (c, d, a)]


data MeshData = MeshData
  { vertexData ∷ V.Vector GLfloat
  , indexData  ∷ V.Vector GLuint
  }


unpackIndices ∷ V.Vector (GLuint, GLuint, GLuint)
              → V.Vector GLuint
unpackIndices = V.concatMap unpack
  where unpack (a, b, c) = V.fromList [a, b, c]

interleave ∷ V.Vector (V3 GLfloat)
           → V.Vector (V3 GLfloat)
           → V.Vector (V3 GLfloat)
           → V.Vector (V2 GLfloat)
           → V.Vector GLfloat
interleave positions colours normals uvs =
  V.foldr (V.++) V.empty $
    V.zipWith4 combine positions colours normals uvs
  where combine (V3 x y z) (V3 r g b) (V3 nx ny nz) (V2 u v) =
          V.fromList [x, y, z, r, g, b, nx, ny, nz, u, v]

fromMeshSpec ∷ MeshSpec → MeshData
fromMeshSpec spec =
  MeshData (interleave      (specPositions spec)
           (specColours spec)
           (specNormals spec)
           (specUVs spec))
           (unpackIndices $ specIndices spec)

data Mesh = Mesh
  { meshVBO        ∷ GLuint
  , meshIBO        ∷ GLuint
  , meshVAO        ∷ GLuint
  , meshIndexCount ∷ GLsizei
  }

data Shader = Shader
  { shaderProgram   ∷ GLuint
  , positions       ∷ GLuint
  , colours         ∷ GLuint
  , normals         ∷ GLuint
  , uvs             ∷ GLuint
  , pvmMatrix       ∷ GLint
  , viewModelMatrix ∷ GLint
  , normalMatrix    ∷ GLint
  , diffuseColour   ∷ GLint
  , ambientColour   ∷ GLint
  , specularColour  ∷ GLint
  , shininess       ∷ GLint
  , lightDirection  ∷ GLint
  , diffuseMap      ∷ GLint
  }

type TextureID = GLuint

data Resources = Resources
  { mesh    ∷ Mesh
  , texture ∷ TextureID
  , shader  ∷ Shader
  }

initialise ∷ IO (Maybe Resources)
initialise = runMaybeT $ do
  png ← liftIO $ readPng "haskell.png"
  (Image texWidth texHeight texData) ← MaybeT $ case png of
    (Right (ImageRGBA8 i)) → return $ Just i
    (Left s)               → liftIO (print s) ≫ return Nothing
    _                      → return Nothing

  textureID ← liftIO ∘ alloca $ \texIDPtr → do
    glGenTextures 1 texIDPtr
    peek texIDPtr

  let fillWith f = liftIO ∘ alloca $ liftM2 (≫) f peek

  glBindTexture   GL_TEXTURE_2D textureID
  let (w, h) = (fromIntegral texWidth, fromIntegral texHeight)
  liftIO ∘ SV.unsafeWith texData $
    glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0
      GL_RGBA GL_UNSIGNED_BYTE ∘ castPtr

  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE

  let loadAndCompileShader ∷ GLenum → FilePath
                           → IO (Maybe GLuint)
      loadAndCompileShader shaderType filename = do
        shaderID ← glCreateShader shaderType
        shaderCode ← T.readFile filename
        T.withCStringLen shaderCode $
          \(str, len) → with str $
            \strPtr → with (fromIntegral len) $
              \lenPtr → glShaderSource shaderID 1 strPtr lenPtr

        glCompileShader shaderID
        compileStatus ← fillWith $
          glGetShaderiv shaderID GL_COMPILE_STATUS
        when (compileStatus == GL_FALSE) $ do
          infoLogLength ← fillWith $
            glGetShaderiv shaderID GL_INFO_LOG_LENGTH
          let infoLogLength' = fromIntegral infoLogLength
          allocaBytes infoLogLength' $ \infoBuffer → do
              glGetShaderInfoLog shaderID infoLogLength
                                 nullPtr infoBuffer
              T.putStr =≪ T.peekCStringLen ( infoBuffer,
                           infoLogLength')

        return $ if compileStatus == GL_TRUE
                 then Just shaderID else Nothing

  vs ← MaybeT $ loadAndCompileShader
     GL_VERTEX_SHADER "vertexShader.glsl"
  fs ← MaybeT $ loadAndCompileShader
     GL_FRAGMENT_SHADER "fragmentShader.glsl"

  programID ← glCreateProgram
  glAttachShader programID vs
  glAttachShader programID fs
  glLinkProgram  programID
  linkStatus ← fillWith $
    glGetProgramiv programID GL_LINK_STATUS

  when (linkStatus == GL_FALSE) ∘ MaybeT $ do
    infoLogLength ← fillWith $
      glGetProgramiv programID GL_INFO_LOG_LENGTH
    let infoLogLength' = fromIntegral infoLogLength
    allocaBytes infoLogLength' $ \infoBuffer → do
      glGetProgramInfoLog programID infoLogLength
                          nullPtr infoBuffer
      T.putStr =≪ T.peekCStringLen (infoBuffer, infoLogLength')
    return Nothing

  glDeleteShader vs
  glDeleteShader fs

  let unsign ∷ Integral a ⇒ GLint → Maybe a
      unsign x | x < 0     = Nothing
               | otherwise = Just $ fromIntegral x

  let forString ∷ Integral a
                ⇒ (GLuint → Ptr GLchar → IO GLint)
                → T.Text → MaybeT (ContT r IO) a
      f `forString` x = do
        (str, _) ← lift $
          ContT (T.withCStringLen $ T.concat [x, "\0"])
        loc ← liftIO $ f programID str
        MaybeT ∘ return $ unsign loc

  glShader ← MaybeT ∘ evalContT ∘ runMaybeT $
    Shader
      <$> pure programID
      <*> glGetAttribLocation  `forString` "position"
      <*> glGetAttribLocation  `forString` "colour"
      <*> glGetAttribLocation  `forString` "normal"
      <*> glGetAttribLocation  `forString` "uv"
      <*> glGetUniformLocation `forString` "pvmMatrix"
      <*> glGetUniformLocation `forString` "viewModelMatrix"
      <*> glGetUniformLocation `forString` "normalMatrix"
      <*> glGetUniformLocation `forString` "diffuseColour"
      <*> glGetUniformLocation `forString` "ambientColour"
      <*> glGetUniformLocation `forString` "specularColour"
      <*> glGetUniformLocation `forString` "shininess"
      <*> glGetUniformLocation `forString` "lightDirection"
      <*> glGetUniformLocation `forString` "diffuseMap"

  let cube = fromMeshSpec $ cuboid 1 1 1

  [vbo, ibo] ← liftIO ∘ allocaArray 2 $ \buffers → do
    glGenBuffers 2 buffers
    peekArray 2 buffers

  glBindBuffer GL_ARRAY_BUFFER vbo
  let vertices   = vertexData cube
      vertexBufSize = sizeOf (V.head vertices) * V.length vertices
  liftIO ∘ SV.unsafeWith (SV.convert vertices) $ \vsPtr →
    glBufferData GL_ARRAY_BUFFER
                 (fromIntegral vertexBufSize)
                 (castPtr vsPtr)
                 GL_STATIC_DRAW
  glBindBuffer GL_ARRAY_BUFFER 0

  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibo
  let indices   = indexData cube
      indexBufSize = sizeOf (V.head indices) * V.length indices
  liftIO ∘ SV.unsafeWith (SV.convert indices) $ \isPtr →
    glBufferData GL_ELEMENT_ARRAY_BUFFER
                 (fromIntegral indexBufSize)
                 (castPtr isPtr)
                 GL_STATIC_DRAW
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER 0

  vao ← liftIO ∘ alloca $ \vaoPtr → do
    glGenVertexArrays 1 vaoPtr
    peek vaoPtr
  glBindVertexArray vao

  glBindBuffer GL_ARRAY_BUFFER vbo
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibo

  glEnableVertexAttribArray (positions glShader)
  glEnableVertexAttribArray (colours glShader)
  glEnableVertexAttribArray (normals glShader)
  glEnableVertexAttribArray (uvs glShader)

  let offset x = wordPtrToPtr $ x * fromIntegral floatSize
      stride    = fromIntegral floatSize * 11
      floatSize = sizeOf (undefined∷GLfloat)

  glVertexAttribPointer (positions glShader)
    3 GL_FLOAT GL_FALSE stride (offset 0)
  glVertexAttribPointer (colours glShader)
    3 GL_FLOAT GL_FALSE stride (offset 3)
  glVertexAttribPointer (normals glShader)
    3 GL_FLOAT GL_FALSE stride (offset 6)
  glVertexAttribPointer (uvs glShader)
    2 GL_FLOAT GL_FALSE stride (offset 9)

  let glMesh = Mesh vbo ibo vao (fromIntegral $ V.length indices)

  liftIO initGL ≫ return (Resources glMesh textureID glShader)

initGL ∷ IO ()
initGL = do
  glClearColor 0.96 0.96 0.96 1
  glClearDepth 1
  glEnable     GL_DEPTH_TEST
  glDepthFunc  GL_LEQUAL
  glCullFace   GL_BACK

resize ∷ IORef (M44 GLfloat) → Int → Int → IO ()
resize projectionMatrix w h = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  writeIORef projectionMatrix $ calculateProjectionMatrix (w, h)

calculateProjectionMatrix ∷ Integral a ⇒ (a, a) → M44 GLfloat
calculateProjectionMatrix (w, h) =
  perspective (π/3) (fromIntegral w / fromIntegral h) 1 100

data DemoState = DemoState
  { cubeRotation   ∷ Quaternion GLfloat
  , cameraPosition ∷ V3 GLfloat
  }

defaultState ∷ DemoState
defaultState = DemoState
  { cubeRotation   = axisAngle (V3 0 1 0) 0
  , cameraPosition = V3 0 1 (-2)
  }

update ∷ DemoState → GLfloat → DemoState
update s dt =
  s { cubeRotation = cubeRotatedBy (rotationSpeed * dt) }
  where
    cubeRotatedBy θ = cubeRotation s * axisAngle (V3 0 1 0) θ
    rotationSpeed   = π / 2

runDemo ∷ IORef Bool
        → IORef (M44 GLfloat)
        → IO ()
        → Maybe Resources
        → IO ()
runDemo _ _ _ Nothing = return ()
runDemo closed projectionMatrix swapBuffers (Just res) =
  do loop defaultState
     cleanup res where
  loop s = readIORef closed ≫= \c → unless c (runFrame s)
  runFrame s = do
    draw res s =≪ readIORef projectionMatrix
    glFlush ≫ swapBuffers
    dt ← getDeltaTime
    loop $ update s dt

draw ∷ Resources → DemoState → M44 GLfloat → IO ()
draw res state projectionMatrix = do
  let viewMat = lookAt (cameraPosition state) (V3 0 0 0) (V3 0 1 0)
      modelMat       = mkTransformation (cubeRotation state) (V3 0 0 0)
      viewModelMat   = viewMat !*! modelMat
      pvmMat         = projectionMatrix !*! viewModelMat
      viewModelMat33 = viewModelMat ^. _xyz . column _xyz
      inverseMat     = inv33 viewModelMat33
      normalMat      = maybe identity distribute inverseMat

  glClear $  GL_COLOR_BUFFER_BIT
         .|. GL_DEPTH_BUFFER_BIT

  glUseProgram ∘ shaderProgram $ shader res
  glBindVertexArray ∘ meshVAO $ mesh res
  glActiveTexture GL_TEXTURE0
  glBindTexture GL_TEXTURE_2D $ texture res

  with pvmMat $
    glUniformMatrix4fv (pvmMatrix $ shader res)
                       1 GL_TRUE ∘ castPtr
  with viewModelMat $
    glUniformMatrix4fv (viewModelMatrix $ shader res)
                       1 GL_TRUE ∘ castPtr
  with normalMat $
    glUniformMatrix3fv (normalMatrix $ shader res)
                       1 GL_TRUE ∘ castPtr

  glUniform4f (diffuseColour $ shader res)  0.6 0.6 0.6 1
  glUniform4f (ambientColour $ shader res)  0.1 0.1 0.1 1
  glUniform4f (specularColour $ shader res) 0.7 0.7 0.7 1
  glUniform1f (shininess $ shader res)      0.4
  glUniform3f (lightDirection $ shader res) 0 0 1
  glUniform1i (diffuseMap $ shader res)     0

  glDrawElements GL_TRIANGLES
                 (meshIndexCount $ mesh res)
                 GL_UNSIGNED_INT
                 (wordPtrToPtr 0)

cleanup ∷ Resources → IO ()
cleanup (Resources m t s) = do
  with (meshVAO m) $ glDeleteVertexArrays 1
  with (meshVBO m) $ glDeleteBuffers 1
  with (meshIBO m) $ glDeleteBuffers 1
  with t           $ glDeleteTextures 1
  glDeleteProgram  $ shaderProgram s
