// Copyright 2014 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Multi-language Perlin noise benchmark.
// See https://github.com/nsf/pnoise for timings and alternative implementations.
// ignore-lexer-test FIXME #15679

extern crate rand;

use std::f32::consts::PI;
use rand::Rng;
use std::env;

#[derive(Copy, Clone)]
struct Vec2 {
    x: f32,
    y: f32,
}

fn lerp(a: f32, b: f32, v: f32) -> f32 { a * (1.0 - v) + b * v }

fn smooth(v: f32) -> f32 { v * v * (3.0 - 2.0 * v) }

fn random_gradient<R: Rng>(r: &mut R) -> Vec2 {
    let v = PI * 2.0 * r.gen::<f32>();
    Vec2 { x: v.cos(), y: v.sin() }
}

fn gradient(orig: Vec2, grad: Vec2, p: Vec2) -> f32 {
    (p.x - orig.x) * grad.x + (p.y - orig.y) * grad.y
}

struct Noise2DContext {
    rgradients: [Vec2; 256],
    permutations: [i32; 256],
}

impl Noise2DContext {
    fn new() -> Noise2DContext {
        let mut rng = rand::thread_rng();

        let mut rgradients = [Vec2 { x: 0.0, y: 0.0 }; 256];
        for x in &mut rgradients[..] {
            *x = random_gradient(&mut rng);
        }

        let mut permutations = [0; 256];
        for (i, x) in permutations.iter_mut().enumerate() {
            *x = i as i32;
        }
        rng.shuffle(&mut permutations);

        Noise2DContext { rgradients: rgradients, permutations: permutations }
    }

    fn get_gradient(&self, x: i32, y: i32) -> Vec2 {
        let idx = self.permutations[(x & 255) as usize] +
                    self.permutations[(y & 255) as usize];
        self.rgradients[(idx & 255) as usize]
    }

    fn get_gradients(&self, x: f32, y: f32) -> ([Vec2; 4], [Vec2; 4]) {
        let x0f = x.floor();
        let y0f = y.floor();
        let x1f = x0f + 1.0;
        let y1f = y0f + 1.0;

        let x0 = x0f as i32;
        let y0 = y0f as i32;
        let x1 = x0 + 1;
        let y1 = y0 + 1;

        ([self.get_gradient(x0, y0), self.get_gradient(x1, y0),
          self.get_gradient(x0, y1), self.get_gradient(x1, y1)],
         [Vec2 { x: x0f, y: y0f }, Vec2 { x: x1f, y: y0f },
          Vec2 { x: x0f, y: y1f }, Vec2 { x: x1f, y: y1f }])
    }

    fn get(&self, x: f32, y: f32) -> f32 {
        let p = Vec2 {x: x, y: y};
        let (gradients, origins) = self.get_gradients(x, y);

        let v0 = gradient(origins[0], gradients[0], p);
        let v1 = gradient(origins[1], gradients[1], p);
        let v2 = gradient(origins[2], gradients[2], p);
        let v3 = gradient(origins[3], gradients[3], p);

        let fx = smooth(x - origins[0].x);
        let vx0 = lerp(v0, v1, fx);
        let vx1 = lerp(v2, v3, fx);
        let fy = smooth(y - origins[0].y);

        lerp(vx0, vx1, fy)
    }
}

fn main() {
    let symbols = [' ', '░', '▒', '▓', '█', '█'];
    let mut pixels = [0f32; 256*256];
    let n2d = Noise2DContext::new();
    let mut n = 1;
    if env::args().len() > 1 { 
      let arg1 = env::args().nth(1).unwrap();
        n = ::std::str::FromStr::from_str(&arg1).unwrap(); 
    }

    for _ in 0..n {
        for y in 0..256 {
            for x in 0..256 {
                let v = n2d.get(x as f32 * 0.1, y as f32 * 0.1);
                pixels[y*256+x] = v * 0.5 + 0.5;
            }
        }
    }

    for y in 0..256 {
        for x in 0..256 {
            let idx = (pixels[y*256+x] / 0.2) as usize;
            print!("{}", symbols[idx]);
        }
        print!("\n");
    }
}
