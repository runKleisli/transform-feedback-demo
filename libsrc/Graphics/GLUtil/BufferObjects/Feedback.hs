{-# LANGUAGE ScopedTypeVariables #-}
-- |Utilities for using 'BufferObject's in transform feedback.
module Graphics.GLUtil.BufferObjects.Feedback where

-- import Data.Word (Word32)
import Graphics.Rendering.OpenGL
-- import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Data.Array.Storable
-- import qualified Data.Vector.Storable as V
-- import Data.ByteString (ByteString, useAsCStringLen)

-----
-- Utility
-----

sizeFromTerminal :: Storable a => (a -> ()) -> Int
sizeFromTerminal = sizeOf . f
	where
		f :: (a -> ()) -> a
		f = const undefined

-----
-- Buffer creation/manipulation
-----

-- |Allocate a 'BufferObject' for use in transform feedback
-- to produce a list of a specified type and length.
makeFeedbackBufferForLists :: Storable a
	=> BufferTarget
	-> (a -> ()) -- |`const () :: A -> ()`. `Proxy` would be safer here.
	-> Int -- |Number of `a`s to capture
	-> IO BufferObject
makeFeedbackBufferForLists target proxy len =
	makeFeedbackBufferSize target $ fromIntegral $ len * (sizeFromTerminal proxy)

-- |Allocate a 'BufferObject' for use in transform feedback
-- whose bytesize is explicitly given.
-- ex: size = numOutputPts * sizeOf (undefined :: Vec3 GLfloat)
-- ```Foreign.Storable.sizeOf :: Storable a => a -> Int```
makeFeedbackBufferSize :: BufferTarget -> GLsizei -> IO BufferObject
makeFeedbackBufferSize target size = do
	[buffer] <- genObjectNames 1
	bindBuffer target $= Just buffer
	let n = fromIntegral size :: GLsizeiptr
	bufferData target $= (n, nullPtr, StaticRead)
	return buffer

-----
-- Buffer contents extraction
-----

-- |Read the transform feedback 'BufferObject' out as a list of
-- explicitly given length & initial value of entries.
getFeedbackBufferContentsFillvalLength :: Storable a
	=> a	-- |Fill value
	-> Int	-- |Length of output list
	-> IO [a]
getFeedbackBufferContentsFillvalLength fillval len = do
	let n = fromIntegral $ len * sizeOf fillval
	arr <- newArray (0, len-1) fillval
	withStorableArray arr $ \ptr ->
		-- This seems a little complex an interface
		-- likely extensible to cases of mixed-type contents
		bufferSubData
			TransformFeedbackBuffer
			ReadFromBuffer
			0 -- GLintptr
			n -- GLsizeiptr
			ptr
	getElems arr -- This still shows the incorrect values.
