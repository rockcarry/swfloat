#include <stdlib.h>
#include <stdio.h>
#include <conio.h>

typedef signed    int32_t;
typedef unsigned  uint32_t;
typedef long long int64_t;
typedef unsigned long long uint64_t;

enum {
    FLOAT_TYPE_ZERO,
    FLOAT_TYPE_NORMAL,
    FLOAT_TYPE_SUBNORMAL,
    FLOAT_TYPE_INFINITY,
    FLOAT_TYPE_NAN,
};

static void float2tsem(float f, int32_t *t, uint32_t *s, int32_t *e, uint32_t *m)
{
    uint32_t val = *(uint32_t*)&f;
    int      i;
    *s = val >> 31;
    *e =(val >> 23) & 0xff;
    *m = val & 0x7fffff;
    switch (*e) {
    case 0:
        if (*m == 0) {
            *t = FLOAT_TYPE_ZERO;
            *e = -128;
            return;
        } else {
            *t  = FLOAT_TYPE_SUBNORMAL;
            for (i=22; i>=0 && !(*m & (1<<i)); i--);
            *e  =-126 - (24 - i);
            *m<<= 24 - i;
        }
        break;
    case 0xff:
        *t = (*m == 0) ? FLOAT_TYPE_INFINITY : FLOAT_TYPE_NAN;
        return;
    default:
        *t  = FLOAT_TYPE_NORMAL;
        *e -= 128;
        *m  = (*m | (1 << 23)) << 1;
        break;
    }
}

static float tsem2float(int32_t t, uint32_t s, int32_t e, uint32_t m)
{
    uint32_t val;
    int      i;
    switch (t) {
    case FLOAT_TYPE_ZERO    : e = 0;    m = 0; break;
    case FLOAT_TYPE_INFINITY: e = 0xff; m = 0; break;
    case FLOAT_TYPE_NAN     : e = 0xff; m = m; break;
    case FLOAT_TYPE_NORMAL:
        for (i=25; i>=0 && !(m & (1<<i)); i--);
        if (i == 25) {
            m >>= 1;
            e  += 1;
        } else {
            m <<= 24 - i;
            e  -= 24 - i;
        }
        if ((m & 0x3) == 0x3) m++;
        if (m & (1 << 25)) { m >>= 1; e += 1; }
        if (1)             { m >>= 1; e += 1; }
        if (e >= 128) { // [128, +OO]
//          printf("overflow !\n");
            e = 0xff; m = 0;
        } else if (e >= -126) { // [-126, 127]
//          printf("normal number !\n");
            e += 127;
        } else if (e >= -150) { // [-149,-127]
//          printf("sub normal number !\n");
            m >>= -126 - e; e = 0;
        } else { // [-OO, -151]
//          printf("underflow !\n");
            m = 0; e = 0;
        }
        break;
    }
    val = (s << 31)|(e << 23)|(m & 0x7fffff);
    return *(float*)&val;
}

static float fadd(float a, float b)
{
    int32_t  ta, tb, ea, eb, ec;
    uint32_t sa, sb, sc, ma, mb, mc;

    float2tsem(a, &ta, &sa, &ea, &ma);
    float2tsem(b, &tb, &sb, &eb, &mb);

    if (ea >= eb) {
        mb >>= (ea - eb);
        ec   = ea;
    } else {
        ma >>= (eb - ea);
        ec   = eb;
    }
    if (sa == sb) {
        mc = ma + mb;
        sc = sa;
    } else {
        if (ma >= mb) {
            mc = ma - mb;
            sc = sa;
        } else {
            mc = mb - ma;
            sc = sb;
        }
    }
    return !mc ? tsem2float(FLOAT_TYPE_ZERO, sc, 0, 0) : tsem2float(FLOAT_TYPE_NORMAL, sc, ec, mc);
}

static float fsub(float a, float b)
{
    *(uint32_t*)&b ^= (1 << 31);
    return fadd(a, b);
}

static float fmul(float a, float b)
{
    int32_t  ta, tb, ea, eb, ec;
    uint32_t sa, sb, sc, ma, mb, mc;
    uint64_t t64;

    float2tsem(a, &ta, &sa, &ea, &ma);
    float2tsem(b, &tb, &sb, &eb, &mb);
    sc = sa ^ sb;
    ec = ea + eb + 2;

    t64 = (uint64_t)ma * mb;
    mc  = (uint32_t)(t64 >> 25);
    return !mc ? tsem2float(FLOAT_TYPE_ZERO, sc, 0, 0) : tsem2float(FLOAT_TYPE_NORMAL, sc, ec, mc);
}

static float fdiv(float a, float b)
{
    int32_t  ta, tb, ea, eb, ec;
    uint32_t sa, sb, sc, ma, mb, mc;
    uint64_t t64;

    float2tsem(a, &ta, &sa, &ea, &ma);
    float2tsem(b, &tb, &sb, &eb, &mb);
    sc = sa ^ sb;
    ec = ea - eb - 1;
    if (mb == 0) return tsem2float(FLOAT_TYPE_INFINITY, 0, 0, 0);
    t64 = ((uint64_t)ma << 24);
    mc  = (uint32_t)(t64 / mb);
    return !mc ? tsem2float(FLOAT_TYPE_ZERO, sc, 0, 0) : tsem2float(FLOAT_TYPE_NORMAL, sc, ec, mc);
}

static int32_t ftoi32(float f)
{
    int32_t  t, e;
    uint32_t s, m;
    float2tsem(f, &t, &s, &e, &m);
    switch (t) {
    case FLOAT_TYPE_ZERO       : return 0;
    case FLOAT_TYPE_INFINITY   : return s ? 0x80000000 : 0x7fffffff;
    case FLOAT_TYPE_SUBNORMAL  : return 0;
    case FLOAT_TYPE_NORMAL     : return m >> (23 - e);
    case FLOAT_TYPE_NAN:default: return *(int32_t*)&f;
    }
}

static float i32tof(int32_t n)
{
    int32_t  e, i;
    uint32_t s, m;

    if (n == 0) return 0;
    if (n < 0) {
        s = 1;
        m =-n;
    } else {
        s = 0;
        m = n;
    }

    for (i=31; i>=0 && !(m & (1<<i)); i--);
    if (i > 23) {
        m >>= (i - 23);
    } else {
        m <<= (23 - i);
    }
    e = i;
    return tsem2float(FLOAT_TYPE_NORMAL, s, e, m);
}

int main(void)
{
    float fa, fb, r1, r2;
    int   i;

    for (i=0; i<100000; i++) {
        int ra = rand(), rb = rand(), rc = rand(), rd = rand();
        fa = (float)(ra ? ra : ra) / (float)(rb ? rb : 1);
        fb = (float)(rc ? rc : rc) / (float)(rd ? rd : 1);
        switch (i % 5) {
        case 0:
            r1 = fa + fb;
            r2 = fadd(fa, fb);
            if (abs(*(uint32_t*)&r1 - *(uint32_t*)&r2) > 1) {
                printf("+%f + %f = %f, %x\n", fa, fb, r1, *(uint32_t*)&r1);
                printf("-%f + %f = %f, %x\n", fa, fb, r2, *(uint32_t*)&r2);
                r2 = fadd(fa, fb);
            }
            break;
        case 1:
            r1 = fa - fb;
            r2 = fsub(fa, fb);
            if (abs(*(uint32_t*)&r1 - *(uint32_t*)&r2) > 1) {
                printf("+%f - %f = %f, %x\n", fa, fb, r1, *(uint32_t*)&r1);
                printf("-%f - %f = %f, %x\n", fa, fb, r2, *(uint32_t*)&r2);
                r2 = fsub(fa, fb);
            }
            break;
        case 2:
            r1 = fa * fb;
            r2 = fmul(fa, fb);
            if (abs(*(uint32_t*)&r1 - *(uint32_t*)&r2) > 1) {
                printf("+%f * %f = %f, %x\n", fa, fb, r1, *(uint32_t*)&r1);
                printf("-%f * %f = %f, %x\n", fa, fb, r2, *(uint32_t*)&r2);
                r2 = fmul(fa, fb);
            }
            break;
        case 3:
            r1 = fa / fb;
            r2 = fdiv(fa, fb);
            if (abs(*(uint32_t*)&r1 - *(uint32_t*)&r2) > 1) {
                printf("+%f / %f = %f, %x\n", fa, fb, r1, *(uint32_t*)&r1);
                printf("-%f / %f = %f, %x\n", fa, fb, r2, *(uint32_t*)&r2);
                r2 = fdiv(fa, fb);
            }
            break;
        case 4:
            r1 = (float)i;
            r2 = i32tof(i);
            if (abs(*(uint32_t*)&r1 - *(uint32_t*)&r2) > 1) {
                printf("+%f, %x\n", r1, *(uint32_t*)&r1);
                printf("-%f, %x\n", r2, *(uint32_t*)&r2);
                r2 = i32tof(i);
            }
            break;
        }
    }
    printf("done.\n");
    getch();
}
