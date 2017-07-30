# transform-feedback-demo

Demonstrates the OpenGL feature of transform feedback using GLUtil
& Vinyl-GL. Transform feedback lets you read things shader programs
have computed on the GPU, normally not sent back to the CPU.

This enables hacky GPGPU computing that's appropriate for programs
that are already using OpenGL for graphics, where a full-blown GPGPU
API may be a burden or the computation being done is easy enough to
write on a shader. Practitioners may or may not embrace black magic.

GLFW is used for the rendering context. It can be substituted for
whatever you normally use (SDL, GLUT).

## Usage

Once built, copy the binary & the `etc` folder into a fixed directory.

Run the executable from the shell, or the shaders may not be loaded.

## Progress

The examples run. It's pretty idiomatic with GLUtil, but not in a
completely general way. Integration with the Vinyl-GL idiom needs
to be done, which should generate the configuration of the buffers
during creation & extraction, & of the shader program for setting
feedback varyings. This entails interpreting records in ways that
will generate the strides & maybe offset to read from the buffers
at, which is part of the usual job of Vinyl-GL, and would use the
generalized GLUtil-idiomatic calculations. However, those parameters
would just cloud these examples, since the buffers are only holding
values from one output variable of the shader. Thus, they are not
exposed in the GLUtil-idiomatic calculations.

The examples are designed for use only with `InterleavedAttribs`
as the transform feedback buffer mode, and with a specified number
of outputs to obtain from feedback, rather than a calculation of
the number of outputs that will be received, which may depend on
the shader pipeline or on the shaders themselves. This calculation
could be addressed in a number of situations, if not completely
generally.

For ideas on how Haskell's transform feedback capabilities can
develop further, as well as an idea of what the raw OpenGL would
look like, please cross-reference with <open.gl/feedback>. An
expository cross-examination between that and a Haskell example
may come in the future.
