#version 150
in vec2 inNum;
out float outNum;

void main() {
  // For debugging reading the transform feedback buffer,
  // it is useful to output a constant.

  // outNum = 5.0;

  // For debugging binding the buffer & passing shader
  // inputs, it is useful to not disable the rasterizer
  // and to draw the position of the input.

  // gl_Position = vec4(inNum, 0, 1);

  outNum = inNum[0];
}
