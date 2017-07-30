{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators,
	KindSignatures, FlexibleInstances, ScopedTypeVariables #-}
{-
Extensions for reflection:
KindSignatures (for (::Symbol)),
FlexibleInstances (for HasFieldDims instance creation),
ScopedTypeVariables (for (undefined :: FieldRec ts) in recursive inst.)
-}
module FeedbackProg where

import Data.Vinyl
-- BEGIN for reflection
import Data.Vinyl.Reflect (HasFieldDims (..))
import GHC.TypeLits
-- END for reflection
import Graphics.GLUtil
import Graphics.GLUtil.BufferObjects.Feedback
import Graphics.Rendering.OpenGL hiding (normal, normalize, light, Normal, Color)
import Graphics.Rendering.OpenGL.GL.TransformFeedback (
	beginTransformFeedback
	, endTransformFeedback
	, TransformFeedbackBufferMode (..)
	, setTransformFeedbackVaryings
	)
import Graphics.VinylGL
import System.FilePath ((</>))

-----
-- Utility
-----

-- Obligated by `FeedbackInputs = '[ '("inNum", GLfloat) ]`.
-- HasFieldDims required for ViableVertex, treat GLfloat as 1D.
instance (HasFieldDims (FieldRec ts))
	=> HasFieldDims (FieldRec ('((sy::Symbol), Float) ': ts)) where
	fieldDims _ = 1 : fieldDims (undefined :: FieldRec ts)

-----
-- Feedback inputs
-----

type FeedbackInputs = '[ '("inNum", GLfloat) ]

feedin :: SField '("inNum", GLfloat)
feedin = SField

-----
-- Feedback outputs
-----

type FeedbackOutputs = '[ '("outNum", GLfloat) ]

feedout :: SField '("outNum", GLfloat)
feedout = SField

-----
-- Feedback data - models
-----

food :: [GLfloat]
food = take 20 $ cycle [2, 1]

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

feedProg :: IO (IO [GLfloat])
feedProg = do
		-- fw :: Program -> IO ()
		let fw = \progx -> setTransformFeedbackVaryings
			progx
			["outNum"]
			InterleavedAttribs
		sprog <- loadShaderProgramWith
			[(VertexShader, "etc"</>"feed.vert")]
			fw
		vb <- bufferVertices . map (feedin =:) $ food
		fb <- makeFeedbackBufferForLists
			ArrayBuffer
			(const () :: GLfloat -> ())
			(fromIntegral foodoutqty)
		vao <- makeVAO $ do
			currentProgram $= Just (program sprog)
			enableVertices' sprog vb
			bindVertices vb
			bindBuffer ArrayBuffer $= Just fb
		return $ withVAO vao $ discardingRasterizer $ do
			currentProgram $= Just (program sprog)
			bindBufferBase
				IndexedTransformFeedbackBuffer
				0	-- BufferIndex = GLuint
				$= Just fb
			-- Transform feedback by drawing points
			beginTransformFeedback Points
			-- Graphics.Rendering.OpenGL.GL.VertexArrays.drawArrays
			drawArrays Points 0 foodqty
			endTransformFeedback
			flush
			getFeedbackBufferContentsFillvalLength 3.0 (fromIntegral foodoutqty)
