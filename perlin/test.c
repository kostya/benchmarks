#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define M_PI 3.1415926535f

typedef struct {
        float x, y;
} Vec2;

static inline float lerp(float a, float b, float v)
{
	return a * (1 - v) + b * v;
}

static inline float smooth(float v) {
	return v * v * (3 - 2 * v);
}

static inline Vec2 random_gradient()
{
	const float v = (float)rand() / RAND_MAX * M_PI * 2.0f;
	return (Vec2){cosf(v), sinf(v)};
}

static inline float gradient(Vec2 orig, Vec2 grad, Vec2 p)
{
        Vec2 sp = {p.x - orig.x, p.y - orig.y};
	return grad.x * sp.x + grad.y * sp.y;
}

typedef struct {
        Vec2 rgradients[256];
        int  permutations[256];
        Vec2 gradients[4];
        Vec2 origins[4];
} Noise2DContext;

static inline Vec2 get_gradient(Noise2DContext *ctx, int x, int y) {
	int idx = ctx->permutations[x & 255] + ctx->permutations[y & 255];
	return ctx->rgradients[idx & 255];
}

static inline void get_gradients(Noise2DContext *ctx, float x, float y) {
	float x0f = floorf(x);
	float y0f = floorf(y);
	int x0 = x0f;
	int y0 = y0f;
	int x1 = x0 + 1;
	int y1 = y0 + 1;

	ctx->gradients[0] = get_gradient(ctx, x0, y0);
	ctx->gradients[1] = get_gradient(ctx, x1, y0);
	ctx->gradients[2] = get_gradient(ctx, x0, y1);
	ctx->gradients[3] = get_gradient(ctx, x1, y1);

	ctx->origins[0] = (Vec2){x0f + 0.0f, y0f + 0.0f};
	ctx->origins[1] = (Vec2){x0f + 1.0f, y0f + 0.0f};
	ctx->origins[2] = (Vec2){x0f + 0.0f, y0f + 1.0f};
	ctx->origins[3] = (Vec2){x0f + 1.0f, y0f + 1.0f};
}


static float noise2d_get(Noise2DContext *ctx, float x, float y)
{
	Vec2 p = {x, y};
	get_gradients(ctx, x, y);
	float v0 = gradient(ctx->origins[0], ctx->gradients[0], p);
	float v1 = gradient(ctx->origins[1], ctx->gradients[1], p);
	float v2 = gradient(ctx->origins[2], ctx->gradients[2], p);
	float v3 = gradient(ctx->origins[3], ctx->gradients[3], p);

	float fx = smooth(x - ctx->origins[0].x);
	float vx0 = lerp(v0, v1, fx);
	float vx1 = lerp(v2, v3, fx);
	float fy = smooth(y - ctx->origins[0].y);
        return lerp(vx0, vx1, fy);
}

static void init_noise2d(Noise2DContext *ctx)
{
        for (int i = 0; i < 256; i++)
                ctx->rgradients[i] = random_gradient();

        for (int i = 0; i < 256; i++) {
                int j = rand() % (i+1);
		ctx->permutations[i] = ctx->permutations[j];
                ctx->permutations[j] = i;
        }
}

int main(int argc, char **argv)
{
    srand(time(NULL));

    const char *symbols[] = {" ", "░", "▒", "▓", "█", "█"};
    float *pixels = malloc(sizeof(float) * 256 * 256);
    int n = 1;
    if (argc > 1) n = atoi(argv[1]);

    Noise2DContext n2d;
	init_noise2d(&n2d);

        for (int i = 0; i < n; i++) {
                for (int y = 0; y < 256; y++) {
                        for (int x = 0; x < 256; x++) {
                                float v = noise2d_get(&n2d, x * 0.1f, y * 0.1f)
					* 0.5f + 0.5f;
                                pixels[y*256+x] = v;
                        }
                }
        }

        for (int y = 0; y < 256; y++) {
                for (int x = 0; x < 256; x++) {
			int idx = pixels[y*256+x] / 0.2f;
                        printf("%s", symbols[idx]);
		}
                printf("\n");
        }

        return 0;
}
