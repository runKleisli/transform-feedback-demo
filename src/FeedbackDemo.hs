module Main where

-- Graphics libraries
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

-- Setup for execution context
import qualified Control.Monad as Monad (when)
import Data.Maybe (isNothing)
import System.Directory (getCurrentDirectory, setCurrentDirectory)

-- Transform feedback examples
import FeedbackProg (feedProg)
import FeedbackProg2 (feedProg2)

run :: GLFW.Window -> IO ()
run win = do
  -- Binding here loads the shaders & compiles the shader program,
  -- w/c can be done per-frame or on scene change just as well.
  feed1 <- feedProg
  feed2 <- feedProg2

  -- For one frame, we run the transform feedback demos,
  -- printing their contents.
  perFrame $
	print "feedprog:"
	>> feed1
	>>= print
	>> print "feedprog:"
	>> feed2
	>>= print

  where
    perFrame :: IO () -> IO ()
    perFrame f = do
      -- Before rendering clear the framebuffer
      GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
      GL.clear [GL.ColorBuffer]

      -- Content
      f

      -- End of frame cleanup
      GL.flush
      GLFW.swapBuffers win

initGL :: String -> Int -> Int -> IO GLFW.Window
initGL windowTitle width height = do
	currDir <- getCurrentDirectory

	r <- GLFW.init
	Monad.when (not r) (error "Error initializing GLFW!")

	-- GLSL version determined by GL version
	GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'OpenGL
	GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
	GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
	GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
	GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 2

	m@(~(Just w)) <- GLFW.createWindow width height windowTitle Nothing Nothing
	Monad.when (isNothing m) (error "Couldn't create window!")

	GLFW.makeContextCurrent m

	-- Hack for retina displays
	(szx, szy) <- GLFW.getFramebufferSize w
	GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral szx) (fromIntegral szy))

	setCurrentDirectory currDir

	return w

main :: IO ()
main = initGL "Transform Feedback Demo" 400 400 >>= run
