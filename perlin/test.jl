#Perlin line noise test in Julia
const RAND_MAX = 0x7fff
const symbols = [ " ", "░", "▒", "▓", "█", "█" ]

type Vec2
  x::Float32
  y::Float32
end

function Vec2()
  Vec2(0,0)
end

type Noise2DContext
  rgradients::Vector{Vec2}
  permutations::Vector{Int64}
  gradients::Vector{Vec2}
  origins::Vector{Vec2}
end

function Noise2DContext()
  Noise2DContext( 
    Array(Vec2,256), #rgradients
    Array(Int64,256), #permutations
    Array(Vec2,4),
    Array(Vec2,4)
  )
end

lerp(a::Float32,b::Float32,v::Float32) = float32(a * (1.0 - v) + b * v)::Float32

smooth(v::Float32) = float32(v * v * (3.0 - 2.0*v))::Float32

function random_gradient()
  v = rand()*pi*2.0 
  Vec2(cos(v),sin(v))
end

function gradient(orig::Vec2,grad::Vec2,p::Vec2)
  sp = Vec2((p.x - orig.x),(p.y - orig.y))
  grad.x * sp.x + grad.y * sp.y
end

function get_gradient(ctx::Noise2DContext, x::Int64, y::Int64)
  idx = ctx.permutations[x&255+1] + ctx.permutations[y&255+1]
  ctx.rgradients[idx&255+1]
end

function get_gradient(ctx::Noise2DContext,x::Float32,y::Float32)
  x0f = floor(x)
  y0f = floor(y)
  x0  = int(x0f)
  y0  = int(y0f)
  y   = int(y0f)
  x1  = x0 + 1
  y1  = y0 + 1

  ctx.gradients[1] = get_gradient(ctx, x0, y0)
  ctx.gradients[2] = get_gradient(ctx, x1, y0)
  ctx.gradients[3] = get_gradient(ctx, x0, y1)
  ctx.gradients[4] = get_gradient(ctx, x1, y1)
  ctx.origins[1]   = Vec2( (x0f + 0.0), (y0f + 0.0))
  ctx.origins[2]   = Vec2( (x0f + 1.0), (y0f + 0.0))
  ctx.origins[3]   = Vec2( (x0f + 0.0), (y0f + 1.0))
  ctx.origins[4]   = Vec2( (x0f + 1.0), (y0f + 1.0))
  ctx.gradients
end

function noise2d_get(ctx:: Noise2DContext, x::Float32, y::Float32)
  p = Vec2(x,y)
  get_gradient(ctx,x,y)
  v0 = gradient(ctx.origins[1], ctx.gradients[1], p)
  v1 = gradient(ctx.origins[2], ctx.gradients[2], p)
  v2 = gradient(ctx.origins[3], ctx.gradients[3], p)
  v3 = gradient(ctx.origins[4], ctx.gradients[4], p)
  fx = smooth(x - ctx.origins[1].x)
  vx0 = lerp(v0, v1, fx)
  vx1 = lerp(v2, v3, fx)
  fy = smooth(y - ctx.origins[1].y)
  lerp(vx0,vx1,fy)
end

function init_noise2d(ctx::Noise2DContext) 
   for i in 1:256
     ctx.rgradients[i] = random_gradient()
   end
   for i in 1:256
     ctx.permutations[i] = i
   end
   shuffle!(ctx.permutations)
end

function main()
  n2d = Noise2DContext()
  init_noise2d(n2d)
  pixels = Array(Float32,256,256)
  n = 1
  if length(ARGS) >= 1
    n = int(ARGS[1])
  end
  for i in 1:n
    for y in 1:256
      for x in 1:256
        v = noise2d_get(n2d, float32(x*0.1), float32(y*0.1))*0.5 + 0.5
        pixels[y,x] = v
      end
    end
  end

   for y in 1:256
     for x in 1:256
       write(STDOUT,symbols[int(pixels[y,x]/0.2)])
     end
     write(STDOUT,"\n")    
   end
end

@time main()
