{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}

module FeedbackProg2 where

import Data.Vinyl
import Graphics.GLUtil
import Graphics.GLUtil.BufferObjects.Feedback
import Graphics.Rendering.OpenGL hiding (normal, normalize, light, Normal, Color)
import Graphics.Rendering.OpenGL.GL.TransformFeedback (
	beginTransformFeedback
	, endTransformFeedback
	, TransformFeedbackBufferMode (..)
	, setTransformFeedbackVaryings
	)
-- import Foreign.Ptr (nullPtr)
import Linear (V2(..))
import Graphics.VinylGL
import System.FilePath ((</>))

-----
-- Feedback inputs
-----

type FeedbackInputs2 = '[ '("inNum", V2 GLfloat) ]

feedin2 :: SField '("inNum", V2 GLfloat)
feedin2 = SField

-----
-- Feedback outputs
-----

type FeedbackOutputs2 = '[ '("outNum", GLfloat) ]

feedout2 :: SField '("outNum", GLfloat)
feedout2 = SField

-----
-- Feedback data - models
-----

food :: [V2 GLfloat]
food = take 20 $ cycle [V2 2 8, V2 1 4]

foodqty :: GLsizei
foodqty = 20

-----
-- Feedback data - output description
-----

-- For certain shader programs, this is the number of
-- primitives generated.
foodoutqty :: GLsizei
foodoutqty = foodqty

-----
-- Asset programs - Model shaders
-----

feedProg2 :: IO (IO [GLfloat])
feedProg2 = do
		-- fw :: Program -> IO ()
		let fw = \progx -> setTransformFeedbackVaryings
			progx
			["outNum"]
			InterleavedAttribs
		sprog <- loadShaderProgramWith
			[(VertexShader, "etc"</>"feed2.vert")]
			fw
		vb <- bufferVertices . map (feedin2 =:) $ food
		{-
		Contrary to open.gl/feedback, the buffer binding
		target is TransformFeedbackBuffer, not ArrayBuffer,
		which is for buffering vertex attribute data:

		https://stackoverflow.com/questions/14802854/what-does-the-gl-array-buffer-target-mean-in-glbindbuffer#14803440

		Binding to ArrayBuffer will overwrite the vert data.

		Nonetheless, we bind to the ArrayBuffer after
		(bindVertices), under (makeVAO) below. It doesn't
		seem to harm, & I haven't experimented with it.
		-}
		fb <- makeFeedbackBufferForLists
			TransformFeedbackBuffer
			(const () :: GLfloat -> ())
			(fromIntegral foodoutqty)
		vao <- makeVAO $ do
			currentProgram $= Just (program sprog)
			enableVertices' sprog vb
			bindVertices vb
			bindBuffer ArrayBuffer $= Just fb
			{-
			open.gl/feedback writes

			//< GLint inputAttrib = glGetAttribLocation(program, "inValue");
			//< glEnableVertexAttribArray(inputAttrib);
			//< glVertexAttribPointer(inputAttrib, 1, GL_FLOAT, GL_FALSE, 0, 0);

			This could be missing or in (enableVertices').
			It doesn't have an effect, here.

			Src for implementation:
			https://github.com/acowley/GLUtil/blob/master/examples/example1.hs

			let vadvar = VertexArrayDescriptor 2 Float 0 nullPtr
			inAttribLoc <- get (attribLocation (program sprog) "inNum")
			vertexAttribPointer inAttribLoc $= (ToFloat, vadvar)
			vertexAttribArray inAttribLoc $= Enabled

			Other Haskell srcs:
			https://hackage.haskell.org/package/OpenGL-3.0.2.0/docs/src/Graphics-Rendering-OpenGL-GL-VertexArrays.html#arrayPointer

			Note we needed a VAD of our shader inputs.
			Generally, we'd want this in Vinyl-GL form.
			-}

		return $ withVAO vao $ discardingRasterizer $ do
			currentProgram $= Just (program sprog)
			bindBufferBase
				IndexedTransformFeedbackBuffer
				0	-- BufferIndex = GLuint
				$= Just fb
			{-
			This pops up in outside sources for drawing points
			< glVertexPointer ...
			as
			> vertexPointer $= ...
			or
			< arrayPointer VertexArray $= ...

			Refs:
			https://stackoverflow.com/questions/9213594/draw-array-of-points-with-opengl#9213692
			https://stackoverflow.com/questions/7960552/opengl-array-of-points
			https://stackoverflow.com/questions/15796162/opengl-vbos-in-haskell#19629936

			glVertexPointer(dim, GL_FLOAT, stride[note0], offset=pointerToFirstElem);
			[note0]:
			https://www.khronos.org/opengl/wiki/Vertex_Specification#Vertex_buffer_offset_and_stride
			* stride is size for HasSize
			* stride is 0 for non-interleaved array, 0 signalling to autocompute as tightly packed
			* stride 0 doesn't work for Vertex Attribute Pointers:
				https://www.khronos.org/opengl/wiki/Vertex_Specification#Separate_attribute_format
				Except apparently it does, by spec & as used in open.gl/feedback example:
				https://www.khronos.org/opengl/wiki/GLAPI/glVertexAttribPointer
			The offset is the element address minus the buffer address.
			So, this should be recursively computed as the sum of the init record sizes.

			Avoiding aquiring the VAD in Vinyl-GL, to draw our 2D verts, we can write

			arrayPointer VertexArray $= VertexArrayDescriptor 2 Float 0 nullPtr

			But testing revealed this call prevents
			feedback output from coming back, even
			for constant assignments of (outNum)'s value.

			It doesn't seem to be necessary.
			-}
			-- Transform feedback by drawing points
			beginTransformFeedback Points
			-- Graphics.Rendering.OpenGL.GL.VertexArrays.drawArrays
			drawArrays Points 0 foodqty
			endTransformFeedback
			flush
			getFeedbackBufferContentsFillvalLength 3.0 (fromIntegral foodoutqty)
