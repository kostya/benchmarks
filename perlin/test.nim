# Imports
import math

# Constants
const RAND_MAX = 0x7fff

# Types
type
    TVec2 = object
        x, y: float

    TNoise2DContext = object
        rgradients: array[0..255, TVec2]
        permutations: array[0..255, int]
        gradients, origins: array[0..4, TVec2]


# Procedures
proc lerp(a, b, v: float): float =
    a * (1 - v) + b * v

proc smooth(v: float): float =
    v * v * (3 - 2 * v)

proc random_gradient: TVec2 =
    let v = random(2 * Pi)
    TVec2(x: cos(v), y: sin(v))

proc gradient(orig, grad, p: TVec2): float =
    let sp = TVec2(x: p.x - orig.x, y: p.y - orig.y)
    grad.x * sp.x + grad.y * sp.y

proc get_gradient(ctx: TNoise2DContext, x, y: int): TVec2 =
    let idx = ctx.permutations[x and 255] + ctx.permutations[y and 255];
    ctx.rgradients[idx and 255]

proc get_gradients(ctx: var TNoise2DContext, x, y: float) =
    let
        x0f = floor(x)
        y0f = floor(y)
        x0  = x0f.int
        y0  = y0f.int
        x1  = x0 + 1
        y1  = y0 + 1

    ctx.gradients[0] = get_gradient(ctx, x0, y0)
    ctx.gradients[1] = get_gradient(ctx, x1, y0)
    ctx.gradients[2] = get_gradient(ctx, x0, y1)
    ctx.gradients[3] = get_gradient(ctx, x1, y1)

    ctx.origins[0] = TVec2(x: x0f + 0.0, y: y0f + 0.0)
    ctx.origins[1] = TVec2(x: x0f + 1.0, y: y0f + 0.0)
    ctx.origins[2] = TVec2(x: x0f + 0.0, y: y0f + 1.0)
    ctx.origins[3] = TVec2(x: x0f + 1.0, y: y0f + 1.0)

proc noise2d_get(ctx: var TNoise2DContext, x, y: float): float =
    let p = TVec2(x: x, y: y)

    get_gradients(ctx, x, y)

    let
        v0 = gradient(ctx.origins[0], ctx.gradients[0], p)
        v1 = gradient(ctx.origins[1], ctx.gradients[1], p)
        v2 = gradient(ctx.origins[2], ctx.gradients[2], p)
        v3 = gradient(ctx.origins[3], ctx.gradients[3], p)

        fx  = smooth(x - ctx.origins[0].x)
        vx0 = lerp(v0, v1, fx)
        vx1 = lerp(v2, v3, fx)
        fy  = smooth(y - ctx.origins[0].y)

    lerp(vx0, vx1, fy)

proc init_noise2d(ctx: var TNoise2DContext) =
    for i in 0..255:
        ctx.rgradients[i] = random_gradient()

    for i in 0..255:
        let j = random(RAND_MAX) mod (i + 1)
        ctx.permutations[i] = ctx.permutations[j]
        ctx.permutations[j] = i


block main:
    randomize()

    const symbols = [ " ", "░", "▒", "▓", "█", "█" ]

    var pixels: array[256*256, float]

    var n2d = TNoise2DContext()
    init_noise2d(n2d)

    var n = 1
    if paramCount() > 0:
      n = parseInt(paramStr(1))

    for i in 0..<n:
        for y in 0..255:
            for x in 0..255:
                let v = noise2d_get(n2d, x.float * 0.1, y.float * 0.1) * 0.5 + 0.5
                pixels[y*256+x] = v

    for y in 0..255:
        for x in 0..255:
            stdout.write(symbols[int(pixels[y*256+x] / 0.2)])

        stdout.write("\L")
