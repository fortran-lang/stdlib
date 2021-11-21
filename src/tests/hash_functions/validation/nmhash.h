/*
 * verification:
 * NMHASH32:
 *   rurban/smhasher: 0x12A30553
 *   demerphq/smhasher: 0x3D8F6C47
 * NMHASH32X:
 *   rurban/smhasher: 0xA8580227
 *   demerphq/smhasher: 0x40B451B3
 */

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _nmhash_h_
#define _nmhash_h_

#define NMH_VERSION 2

#ifdef _MSC_VER
#  pragma warning(push, 3)
#endif

#if defined(__cplusplus) && __cplusplus < 201103L
#  define __STDC_CONSTANT_MACROS 1
#endif

#include <stdint.h>
#include <string.h>

#if defined(__GNUC__)
#  if defined(__AVX2__)
#    include <immintrin.h>
#  elif defined(__SSE2__)
#    include <emmintrin.h>
#  endif
#elif defined(_MSC_VER)
#  include <intrin.h>
#endif

#ifdef _MSC_VER
#  pragma warning(pop)
#endif

#if (defined(__GNUC__) && (__GNUC__ >= 3))  \
  || (defined(__INTEL_COMPILER) && (__INTEL_COMPILER >= 800)) \
  || defined(__clang__)
#    define NMH_likely(x) __builtin_expect(x, 1)
#else
#    define NMH_likely(x) (x)
#endif

#if defined(__has_builtin)
#  if __has_builtin(__builtin_rotateleft32)
#    define NMH_rotl32 __builtin_rotateleft32 /* clang */
#  endif
#endif
#if !defined(NMH_rotl32)
#  if defined(_MSC_VER)
     /* Note: although _rotl exists for minGW (GCC under windows), performance seems poor */
#    define NMH_rotl32(x,r) _rotl(x,r)
#  else
#    define NMH_rotl32(x,r) (((x) << (r)) | ((x) >> (32 - (r))))
#  endif
#endif

#if ((defined(sun) || defined(__sun)) && __cplusplus) /* Solaris includes __STDC_VERSION__ with C++. Tested with GCC 5.5 */
#  define NMH_RESTRICT /* disable */
#elif defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L   /* >= C99 */
#  define NMH_RESTRICT   restrict
#elif defined(__cplusplus) && (defined(__GNUC__) || defined(__clang__) || defined(__INTEL_COMPILER))
#  define NMH_RESTRICT __restrict__
#elif defined(__cplusplus) && defined(_MSC_VER)
#  define NMH_RESTRICT __restrict
#else
#  define NMH_RESTRICT   /* disable */
#endif

/* endian macros */
#ifndef NMHASH_LITTLE_ENDIAN
#  if defined(_WIN32) || defined(__LITTLE_ENDIAN__) || defined(__x86_64__) || (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__) || defined(__SDCC)
#    define NMHASH_LITTLE_ENDIAN 1
#  elif defined(__BIG_ENDIAN__) || (defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
#    define NMHASH_LITTLE_ENDIAN 0
#  else
#    warning could not determine endianness! Falling back to little endian.
#    define NMHASH_LITTLE_ENDIAN 1
#  endif
#endif

/* vector macros */
#define NMH_SCALAR 0
#define NMH_SSE2   1
#define NMH_AVX2   2
#define NMH_AVX512 3

#ifndef NMH_VECTOR    /* can be defined on command line */
#  if defined(__AVX512BW__)
#    define NMH_VECTOR NMH_AVX512 /* _mm512_mullo_epi16 requires AVX512BW */
#  elif defined(__AVX2__)
#    define NMH_VECTOR NMH_AVX2  /* add '-mno-avx256-split-unaligned-load' and '-mn-oavx256-split-unaligned-store' for gcc */
#  elif defined(__SSE2__) || defined(_M_AMD64) || defined(_M_X64) || (defined(_M_IX86_FP) && (_M_IX86_FP == 2))
#    define NMH_VECTOR NMH_SSE2
#  else
#    define NMH_VECTOR NMH_SCALAR
#  endif
#endif

/* align macros */
#if defined (__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)   /* C11+ */
#  include <stdalign.h>
#  define NMH_ALIGN(n)      alignas(n)
#elif defined(__GNUC__)
#  define NMH_ALIGN(n)      __attribute__ ((aligned(n)))
#elif defined(_MSC_VER)
#  define NMH_ALIGN(n)      __declspec(align(n))
#else
#  define NMH_ALIGN(n)   /* disabled */
#endif

#if NMH_VECTOR > 0
#  define NMH_ACC_ALIGN 64
#elif defined(__BIGGEST_ALIGNMENT__)
#  define NMH_ACC_ALIGN __BIGGEST_ALIGNMENT__
#elif defined(__SDCC)
#  define NMH_ACC_ALIGN 1
#else
#  define NMH_ACC_ALIGN 16
#endif

/* constants */

/* primes from xxh */
#define NMH_PRIME32_1  UINT32_C(0x9E3779B1)
#define NMH_PRIME32_2  UINT32_C(0x85EBCA77)
#define NMH_PRIME32_3  UINT32_C(0xC2B2AE3D)
#define NMH_PRIME32_4  UINT32_C(0x27D4EB2F)

/*! Pseudorandom secret taken directly from FARSH. */
NMH_ALIGN(NMH_ACC_ALIGN) static const uint32_t NMH_ACC_INIT[32] = {
	UINT32_C(0xB8FE6C39), UINT32_C(0x23A44BBE), UINT32_C(0x7C01812C), UINT32_C(0xF721AD1C),
	UINT32_C(0xDED46DE9), UINT32_C(0x839097DB), UINT32_C(0x7240A4A4), UINT32_C(0xB7B3671F),
	UINT32_C(0xCB79E64E), UINT32_C(0xCCC0E578), UINT32_C(0x825AD07D), UINT32_C(0xCCFF7221),
	UINT32_C(0xB8084674), UINT32_C(0xF743248E), UINT32_C(0xE03590E6), UINT32_C(0x813A264C),

	UINT32_C(0x3C2852BB), UINT32_C(0x91C300CB), UINT32_C(0x88D0658B), UINT32_C(0x1B532EA3),
	UINT32_C(0x71644897), UINT32_C(0xA20DF94E), UINT32_C(0x3819EF46), UINT32_C(0xA9DEACD8),
	UINT32_C(0xA8FA763F), UINT32_C(0xE39C343F), UINT32_C(0xF9DCBBC7), UINT32_C(0xC70B4F1D),
	UINT32_C(0x8A51E04B), UINT32_C(0xCDB45931), UINT32_C(0xC89F7EC9), UINT32_C(0xD9787364),
};

#if defined(_MSC_VER) && _MSC_VER >= 1914
#  pragma warning(push)
#  pragma warning(disable: 5045)
#endif
#ifdef __SDCC
#  define const
#  pragma save
#  pragma disable_warning 110
#  pragma disable_warning 126
#endif

/* read functions */
static inline
uint32_t
NMH_readLE32(const void *const p)
{
	uint32_t v;
	memcpy(&v, p, 4);
#	if (NMHASH_LITTLE_ENDIAN)
	return v;
#	elif defined(__GNUC__) || defined(__INTEL_COMPILER) || defined(__clang__)
	return __builtin_bswap32(v);
#	elif defined(_MSC_VER)
	return _byteswap_ulong(v);
#	else
	return ((v >> 24) & 0xff) | ((v >> 8) & 0xff00) | ((v << 8) & 0xff0000) | ((v << 24) & 0xff000000);
#	endif
}

static inline
uint16_t
NMH_readLE16(const void *const p)
{
	uint16_t v;
	memcpy(&v, p, 2);
#	if (NMHASH_LITTLE_ENDIAN)
	return v;
#	else
	return (uint16_t)((v << 8) | (v >> 8));
#	endif
}

static inline
uint32_t
NMHASH32_0to8(uint32_t const x, uint32_t const seed2)
{
	/* base mixer: [-6 -12 776bf593 -19 11 3fb39c65 -15 -9 e9139917 -11 16] = 0.027071104091278835 */
	const uint32_t m1 = UINT32_C(0x776BF593);
	const uint32_t m2 = UINT32_C(0x3FB39C65);
	const uint32_t m3 = UINT32_C(0xE9139917);

#	if NMH_VECTOR == NMH_SCALAR
	{
		union { uint32_t u32; uint16_t u16[2]; } vx;
		vx.u32 = x;
		vx.u32 ^= (vx.u32 >> 12) ^ (vx.u32 >> 6);
		vx.u16[0] *= (uint16_t)m1;
		vx.u16[1] *= (uint16_t)(m1 >> 16);
		vx.u32 ^= (vx.u32 << 11) ^ ( vx.u32 >> 19);
		vx.u16[0] *= (uint16_t)m2;
		vx.u16[1] *= (uint16_t)(m2 >> 16);
		vx.u32 ^= seed2;
		vx.u32 ^= (vx.u32 >> 15) ^ ( vx.u32 >> 9);
		vx.u16[0] *= (uint16_t)m3;
		vx.u16[1] *= (uint16_t)(m3 >> 16);
		vx.u32 ^= (vx.u32 << 16) ^ ( vx.u32 >> 11);
		return vx.u32;
	}
#	else /* at least NMH_SSE2 */
	{
		__m128i hv = _mm_setr_epi32((int)x, 0, 0, 0);
		const __m128i sv = _mm_setr_epi32((int)seed2, 0, 0, 0);
		const uint32_t *const result = (const uint32_t*)&hv;

		hv = _mm_xor_si128(_mm_xor_si128(hv, _mm_srli_epi32(hv, 12)), _mm_srli_epi32(hv, 6));
		hv = _mm_mullo_epi16(hv, _mm_setr_epi32((int)m1, 0, 0, 0));
		hv = _mm_xor_si128(_mm_xor_si128(hv, _mm_slli_epi32(hv, 11)), _mm_srli_epi32(hv, 19));
		hv = _mm_mullo_epi16(hv, _mm_setr_epi32((int)m2, 0, 0, 0));

		hv = _mm_xor_si128(hv, sv);

		hv = _mm_xor_si128(_mm_xor_si128(hv, _mm_srli_epi32(hv, 15)), _mm_srli_epi32(hv, 9));
		hv = _mm_mullo_epi16(hv, _mm_setr_epi32((int)m3, 0, 0, 0));
		hv = _mm_xor_si128(_mm_xor_si128(hv, _mm_slli_epi32(hv, 16)), _mm_srli_epi32(hv, 11));

		return *result;
	}
#	endif
}

#define __NMH_M1 UINT32_C(0xF0D9649B)
#define __NMH_M2 UINT32_C(0x29A7935D)
#define __NMH_M3 UINT32_C(0x55D35831)

NMH_ALIGN(NMH_ACC_ALIGN) static const uint32_t __NMH_M1_V[32] = {
	__NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1,
	__NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1,
	__NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1,
	__NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1, __NMH_M1,
};
NMH_ALIGN(NMH_ACC_ALIGN) static const uint32_t __NMH_M2_V[32] = {
	__NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2,
	__NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2,
	__NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2,
	__NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2, __NMH_M2,
};
NMH_ALIGN(NMH_ACC_ALIGN) static const uint32_t __NMH_M3_V[32] = {
	__NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3,
	__NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3,
	__NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3,
	__NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3, __NMH_M3,
};

static inline
uint32_t
NMHASH32_9to255(const uint8_t* const NMH_RESTRICT p, size_t const len, uint32_t const seed, int const type)
{
	/* base mixer: [f0d9649b  5 -13 29a7935d -9 11 55d35831 -20 -10 ] = 0.93495901789135362 */
	uint32_t result = 0;
#	if NMH_VECTOR == NMH_SCALAR
	{
		union { uint32_t u32; uint16_t u16[2]; } x[4], y[4];
		uint32_t const sl = seed + (uint32_t)len;
		size_t j;
		x[0].u32 = NMH_PRIME32_1;
		x[1].u32 = NMH_PRIME32_2;
		x[2].u32 = NMH_PRIME32_3;
		x[3].u32 = NMH_PRIME32_4;
		for (j = 0; j < 4; ++j) y[j].u32 = sl;

		if (type) {
			/* 33 to 255 bytes */
			size_t const r = (len - 1) / 32;
			size_t i;
			for (i = 0; i < r; ++i) {
				for (j = 0; j < 4; ++j) x[j].u32 ^= NMH_readLE32(p + i * 32 + j * 4);
				for (j = 0; j < 4; ++j) y[j].u32 ^= NMH_readLE32(p + i * 32 + j * 4 + 16);
				for (j = 0; j < 4; ++j) x[j].u32 += y[j].u32;

				for (j = 0; j < 4; ++j) {
					x[j].u16[0] *= (uint16_t)(__NMH_M1 & 0xFFFF);
					x[j].u16[1] *= (uint16_t)(__NMH_M1 >> 16);
				}
				for (j = 0; j < 4; ++j) x[j].u32 ^= (x[j].u32 << 5) ^ (x[j].u32 >> 13);
				for (j = 0; j < 4; ++j) {
					x[j].u16[0] *= (uint16_t)(__NMH_M2 & 0xFFFF);
					x[j].u16[1] *= (uint16_t)(__NMH_M2 >> 16);
				}

				for (j = 0; j < 4; ++j) x[j].u32 ^= y[j].u32;

				for (j = 0; j < 4; ++j) x[j].u32 ^= (x[j].u32 << 11) ^ (x[j].u32 >> 9);
				for (j = 0; j < 4; ++j) {
					x[j].u16[0] *= (uint16_t)(__NMH_M3 & 0xFFFF);
					x[j].u16[1] *= (uint16_t)(__NMH_M3 >> 16);
				}
				for (j = 0; j < 4; ++j) x[j].u32 ^= (x[j].u32 >> 10) ^ (x[j].u32 >> 20);
			}
			for (j = 0; j < 4; ++j) x[j].u32 ^= NMH_readLE32(p + len - 32 + j * 4);
			for (j = 0; j < 4; ++j) y[j].u32 ^= NMH_readLE32(p + len - 16 + j * 4);
		} else {
			/* 9 to 32 bytes */
			x[0].u32 ^= NMH_readLE32(p);
			x[1].u32 ^= NMH_readLE32(p + ((len>>4)<<3));
			x[2].u32 ^= NMH_readLE32(p + len - 8);
			x[3].u32 ^= NMH_readLE32(p + len - 8 - ((len>>4)<<3));
			y[0].u32 ^= NMH_readLE32(p + 4);
			y[1].u32 ^= NMH_readLE32(p + ((len>>4)<<3) + 4);
			y[2].u32 ^= NMH_readLE32(p + len - 8 + 4);
			y[3].u32 ^= NMH_readLE32(p + len - 8 - ((len>>4)<<3) + 4);
		}

		for (j = 0; j < 4; ++j) x[j].u32 += y[j].u32;
		for (j = 0; j < 4; ++j) y[j].u32 ^= (y[j].u32 << 17) ^ (y[j].u32 >> 6);

		for (j = 0; j < 4; ++j) {
			x[j].u16[0] *= (uint16_t)(__NMH_M1 & 0xFFFF);
			x[j].u16[1] *= (uint16_t)(__NMH_M1 >> 16);
		}
		for (j = 0; j < 4; ++j) x[j].u32 ^= (x[j].u32 << 5) ^ (x[j].u32 >> 13);
		for (j = 0; j < 4; ++j) {
			x[j].u16[0] *= (uint16_t)(__NMH_M2 & 0xFFFF);
			x[j].u16[1] *= (uint16_t)(__NMH_M2 >> 16);
		}

		for (j = 0; j < 4; ++j) x[j].u32 ^= y[j].u32;

		for (j = 0; j < 4; ++j) x[j].u32 ^= (x[j].u32 << 11) ^ (x[j].u32 >> 9);
		for (j = 0; j < 4; ++j) {
			x[j].u16[0] *= (uint16_t)(__NMH_M3 & 0xFFFF);
			x[j].u16[1] *= (uint16_t)(__NMH_M3 >> 16);
		}
		for (j = 0; j < 4; ++j) x[j].u32 ^= (x[j].u32 >> 10) ^ (x[j].u32 >> 20);

		x[0].u32 ^= NMH_PRIME32_1;
		x[1].u32 ^= NMH_PRIME32_2;
		x[2].u32 ^= NMH_PRIME32_3;
		x[3].u32 ^= NMH_PRIME32_4;

		for (j = 1; j < 4; ++j) x[0].u32 += x[j].u32;

		x[0].u32 ^= sl + (sl >> 5);
		x[0].u16[0] *= (uint16_t)(__NMH_M3 & 0xFFFF);
		x[0].u16[1] *= (uint16_t)(__NMH_M3 >> 16);
		x[0].u32 ^= (x[0].u32 >> 10) ^ (x[0].u32 >> 20);

		result = x[0].u32;
	}
#	else /* at least NMH_SSE2 */
	{
		__m128i const h0 = _mm_setr_epi32((int)NMH_PRIME32_1, (int)NMH_PRIME32_2, (int)NMH_PRIME32_3, (int)NMH_PRIME32_4);
		__m128i const sl = _mm_set1_epi32((int)seed + (int)len);
		__m128i const m1 = _mm_set1_epi32((int)__NMH_M1);
		__m128i const m2 = _mm_set1_epi32((int)__NMH_M2);
		__m128i const m3 = _mm_set1_epi32((int)__NMH_M3);
		__m128i       x = h0;
		__m128i       y = sl;
		const uint32_t *const px = (const uint32_t*)&x;

		if (type) {
			/* 32 to 127 bytes */
			size_t const r = (len - 1) / 32;
			size_t i;
			for (i = 0; i < r; ++i) {
				x = _mm_xor_si128(x, _mm_loadu_si128((const __m128i *)(p + i * 32)));
				y = _mm_xor_si128(y, _mm_loadu_si128((const __m128i *)(p + i * 32 + 16)));
				x = _mm_add_epi32(x, y);
				x = _mm_mullo_epi16(x, m1);
				x = _mm_xor_si128(_mm_xor_si128(x, _mm_slli_epi32(x, 5)), _mm_srli_epi32(x, 13));
				x = _mm_mullo_epi16(x, m2);
				x = _mm_xor_si128(x, y);
				x = _mm_xor_si128(_mm_xor_si128(x, _mm_slli_epi32(x, 11)), _mm_srli_epi32(x, 9));
				x = _mm_mullo_epi16(x, m3);
				x = _mm_xor_si128(_mm_xor_si128(x, _mm_srli_epi32(x, 10)), _mm_srli_epi32(x, 20));
			}
			x = _mm_xor_si128(x, _mm_loadu_si128((const __m128i *)(p + len - 32)));
			y = _mm_xor_si128(y, _mm_loadu_si128((const __m128i *)(p + len - 16)));
		} else {
			/* 9 to 32 bytes */
			x = _mm_xor_si128(x, _mm_setr_epi32((int)NMH_readLE32(p), (int)NMH_readLE32(p + ((len>>4)<<3)), (int)NMH_readLE32(p + len - 8), (int)NMH_readLE32(p + len - 8 - ((len>>4)<<3))));
			y = _mm_xor_si128(y, _mm_setr_epi32((int)NMH_readLE32(p + 4), (int)NMH_readLE32(p + ((len>>4)<<3) + 4), (int)NMH_readLE32(p + len - 8 + 4), (int)NMH_readLE32(p + len - 8 - ((len>>4)<<3) + 4)));
		}

		x = _mm_add_epi32(x, y);

		y = _mm_xor_si128(_mm_xor_si128(y, _mm_slli_epi32(y, 17)), _mm_srli_epi32(y, 6));

		x = _mm_mullo_epi16(x, m1);
		x = _mm_xor_si128(_mm_xor_si128(x, _mm_slli_epi32(x, 5)), _mm_srli_epi32(x, 13));
		x = _mm_mullo_epi16(x, m2);
		x = _mm_xor_si128(x, y);
		x = _mm_xor_si128(_mm_xor_si128(x, _mm_slli_epi32(x, 11)), _mm_srli_epi32(x, 9));
		x = _mm_mullo_epi16(x, m3);
		x = _mm_xor_si128(_mm_xor_si128(x, _mm_srli_epi32(x, 10)), _mm_srli_epi32(x, 20));

		x = _mm_xor_si128(x, h0);
		x = _mm_add_epi32(x, _mm_srli_si128(x, 4));
		x = _mm_add_epi32(x, _mm_srli_si128(x, 8));

		x = _mm_xor_si128(x, _mm_add_epi32(sl, _mm_srli_epi32(sl, 5)));
		x = _mm_mullo_epi16(x, m3);
		x = _mm_xor_si128(_mm_xor_si128(x, _mm_srli_epi32(x, 10)), _mm_srli_epi32(x, 20));

		result = *px;
	}
#	endif
	return *&result;
}
#define NMHASH32_9to32(p, len, seed) NMHASH32_9to255(p, len, seed, 0)
#define NMHASH32_33to255(p, len, seed) NMHASH32_9to255(p, len, seed, 1)

#undef __NMH_M1
#undef __NMH_M2
#undef __NMH_M3

#if NMH_VECTOR == NMH_SCALAR
#define NMHASH32_long_round NMHASH32_long_round_scalar
static inline
void
NMHASH32_long_round_scalar(uint32_t *const NMH_RESTRICT accX, uint32_t *const NMH_RESTRICT accY, const uint8_t* const NMH_RESTRICT p)
{
	/* breadth first calculation will hint some compiler to auto vectorize the code
	 * on gcc, the performance becomes 10x than the depth first, and about 80% of the manually vectorized code
	 */
	const size_t nbGroups = sizeof(NMH_ACC_INIT) / sizeof(*NMH_ACC_INIT);
	size_t i;

	for (i = 0; i < nbGroups; ++i) {
		accX[i] ^= NMH_readLE32(p + i * 4);
	}
	for (i = 0; i < nbGroups; ++i) {
		accY[i] ^= NMH_readLE32(p + i * 4 + sizeof(NMH_ACC_INIT));
	}
	for (i = 0; i < nbGroups; ++i) {
		accX[i] += accY[i];
	}
	for (i = 0; i < nbGroups; ++i) {
		accY[i] ^= accX[i] >> 1;
	}
	for (i = 0; i < nbGroups * 2; ++i) {
		((uint16_t*)accX)[i] *= ((uint16_t*)__NMH_M1_V)[i];
	}
	for (i = 0; i < nbGroups; ++i) {
		accX[i] ^= accX[i] << 5 ^ accX[i] >> 13;
	}
	for (i = 0; i < nbGroups * 2; ++i) {
		((uint16_t*)accX)[i] *= ((uint16_t*)__NMH_M2_V)[i];
	}
	for (i = 0; i < nbGroups; ++i) {
		accX[i] ^= accY[i];
	}
	for (i = 0; i < nbGroups; ++i) {
		accX[i] ^= accX[i] << 11 ^ accX[i] >> 9;
	}
	for (i = 0; i < nbGroups * 2; ++i) {
		((uint16_t*)accX)[i] *= ((uint16_t*)__NMH_M3_V)[i];
	}
	for (i = 0; i < nbGroups; ++i) {
		accX[i] ^= accX[i] >> 10 ^ accX[i] >> 20;
	}
}
#endif

#if NMH_VECTOR == NMH_SSE2
#  define _NMH_MM_(F) _mm_ ## F
#  define _NMH_MMW_(F) _mm_ ## F ## 128
#  define _NMH_MM_T __m128i
#elif NMH_VECTOR == NMH_AVX2
#  define _NMH_MM_(F) _mm256_ ## F
#  define _NMH_MMW_(F) _mm256_ ## F ## 256
#  define _NMH_MM_T __m256i
#elif NMH_VECTOR == NMH_AVX512
#  define _NMH_MM_(F) _mm512_ ## F
#  define _NMH_MMW_(F) _mm512_ ## F ## 512
#  define _NMH_MM_T __m512i
#endif

#if NMH_VECTOR == NMH_SSE2 || NMH_VECTOR == NMH_AVX2 || NMH_VECTOR == NMH_AVX512
#  define NMHASH32_long_round NMHASH32_long_round_sse
#  define NMH_VECTOR_NB_GROUP (sizeof(NMH_ACC_INIT) / sizeof(*NMH_ACC_INIT) / (sizeof(_NMH_MM_T) / sizeof(*NMH_ACC_INIT)))
static inline
void
NMHASH32_long_round_sse(uint32_t *const NMH_RESTRICT accX, uint32_t *const NMH_RESTRICT accY, const uint8_t* const NMH_RESTRICT p)
{
	const _NMH_MM_T *const NMH_RESTRICT m1    = (const _NMH_MM_T * NMH_RESTRICT)__NMH_M1_V;
	const _NMH_MM_T *const NMH_RESTRICT m2    = (const _NMH_MM_T * NMH_RESTRICT)__NMH_M2_V;
	const _NMH_MM_T *const NMH_RESTRICT m3    = (const _NMH_MM_T * NMH_RESTRICT)__NMH_M3_V;
	      _NMH_MM_T *const              xaccX = (      _NMH_MM_T *             )accX;
	      _NMH_MM_T *const              xaccY = (      _NMH_MM_T *             )accY;
	      _NMH_MM_T *const              xp    = (      _NMH_MM_T *             )p;
	size_t i;

	for (i = 0; i < NMH_VECTOR_NB_GROUP; ++i) {
		xaccX[i] = _NMH_MMW_(xor_si)(xaccX[i], _NMH_MMW_(loadu_si)(xp + i));
	}
	for (i = 0; i < NMH_VECTOR_NB_GROUP; ++i) {
		xaccY[i] = _NMH_MMW_(xor_si)(xaccY[i], _NMH_MMW_(loadu_si)(xp + i + NMH_VECTOR_NB_GROUP));
	}
	for (i = 0; i < NMH_VECTOR_NB_GROUP; ++i) {
		xaccX[i] = _NMH_MM_(add_epi32)(xaccX[i], xaccY[i]);
	}
	for (i = 0; i < NMH_VECTOR_NB_GROUP; ++i) {
		xaccY[i] = _NMH_MMW_(xor_si)(xaccY[i], _NMH_MM_(srli_epi32)(xaccX[i], 1));
	}
	for (i = 0; i < NMH_VECTOR_NB_GROUP; ++i) {
		xaccX[i] = _NMH_MM_(mullo_epi16)(xaccX[i], *m1);
	}
	for (i = 0; i < NMH_VECTOR_NB_GROUP; ++i) {
		xaccX[i] = _NMH_MMW_(xor_si)(_NMH_MMW_(xor_si)(xaccX[i], _NMH_MM_(slli_epi32)(xaccX[i], 5)), _NMH_MM_(srli_epi32)(xaccX[i], 13));
	}
	for (i = 0; i < NMH_VECTOR_NB_GROUP; ++i) {
		xaccX[i] = _NMH_MM_(mullo_epi16)(xaccX[i], *m2);
	}
	for (i = 0; i < NMH_VECTOR_NB_GROUP; ++i) {
		xaccX[i] = _NMH_MMW_(xor_si)(xaccX[i], xaccY[i]);
	}
	for (i = 0; i < NMH_VECTOR_NB_GROUP; ++i) {
		xaccX[i] = _NMH_MMW_(xor_si)(_NMH_MMW_(xor_si)(xaccX[i], _NMH_MM_(slli_epi32)(xaccX[i], 11)), _NMH_MM_(srli_epi32)(xaccX[i], 9));
	}
	for (i = 0; i < NMH_VECTOR_NB_GROUP; ++i) {
		xaccX[i] = _NMH_MM_(mullo_epi16)(xaccX[i], *m3);
	}
	for (i = 0; i < NMH_VECTOR_NB_GROUP; ++i) {
		xaccX[i] = _NMH_MMW_(xor_si)(_NMH_MMW_(xor_si)(xaccX[i], _NMH_MM_(srli_epi32)(xaccX[i], 10)), _NMH_MM_(srli_epi32)(xaccX[i], 20));
	}
}
#  undef _NMH_MM_
#  undef _NMH_MMW_
#  undef _NMH_MM_T
#  undef NMH_VECTOR_NB_GROUP
#endif

static
uint32_t
NMHASH32_long(const uint8_t* const NMH_RESTRICT p, size_t const len, uint32_t const seed)
{
	NMH_ALIGN(NMH_ACC_ALIGN) uint32_t accX[sizeof(NMH_ACC_INIT)/sizeof(*NMH_ACC_INIT)];
	NMH_ALIGN(NMH_ACC_ALIGN) uint32_t accY[sizeof(accX)/sizeof(*accX)];
	size_t const nbRounds = (len - 1) / (sizeof(accX) + sizeof(accY));
	size_t i;
	uint32_t sum = 0;

	/* init */
	for (i = 0; i < sizeof(accX)/sizeof(*accX); ++i) accX[i] = NMH_ACC_INIT[i];
	for (i = 0; i < sizeof(accY)/sizeof(*accY); ++i) accY[i] = seed;

	for (i = 0; i < nbRounds; ++i) {
		NMHASH32_long_round(accX, accY, p + i * (sizeof(accX) + sizeof(accY)));
	}
	NMHASH32_long_round(accX, accY, p + len - (sizeof(accX) + sizeof(accY)));

	/* merge acc */
	for (i = 0; i < sizeof(accX)/sizeof(*accX); ++i) accX[i] ^= NMH_ACC_INIT[i];
	for (i = 0; i < sizeof(accX)/sizeof(*accX); ++i) sum += accX[i];

#	if SIZE_MAX > UINT32_C(-1)
	sum += (uint32_t)(len >> 32);
#	endif
	return sum ^ (uint32_t)len;
}

static inline
uint32_t
NMHASH32_avalanche32(uint32_t const x)
{
	/* [-21 -8 cce5196d 12 -7 464be229 -21 -8] = 3.2267098842182733 */
	const uint32_t m1 = UINT32_C(0xCCE5196D);
	const uint32_t m2 = UINT32_C(0x464BE229);
	union { uint32_t u32; uint16_t u16[2]; } vx;
	vx.u32    = x;
	vx.u32   ^= (vx.u32 >> 8) ^ (vx.u32 >> 21);
	vx.u16[0] = (uint16_t)(vx.u16[0] * (uint16_t)m1);
	vx.u16[1] = (uint16_t)(vx.u16[1] * (uint16_t)(m1 >> 16));
	vx.u32   ^= (vx.u32 << 12) ^ (vx.u32 >> 7);
	vx.u16[0] = (uint16_t)(vx.u16[0] * (uint16_t)m2);
	vx.u16[1] = (uint16_t)(vx.u16[1] * (uint16_t)(m2 >> 16));
	return vx.u32 ^ (vx.u32 >> 8) ^ (vx.u32 >> 21);
}

static inline
uint32_t
NMHASH32(const void* const NMH_RESTRICT input, size_t const len, uint32_t seed)
{
	const uint8_t *const p = (const uint8_t *)input;
	if (NMH_likely(len <= 32)) {
		if(NMH_likely(len > 8)) {
			return NMHASH32_9to32(p, len, seed);
		}
		if(NMH_likely(len > 4)) {
			uint32_t x = NMH_readLE32(p);
			uint32_t y = NMH_readLE32(p + len - 4) ^ (NMH_PRIME32_4 + 2 + seed);
			x += y;
			x ^= x << (len + 7);
			return NMHASH32_0to8(x, NMH_rotl32(y, 5));
		} else {
			union { uint32_t u32; uint16_t u16[2]; uint8_t u8[4]; } data;
			switch (len) {
				case 0: seed += NMH_PRIME32_2;
					data.u32 = 0;
					break;
				case 1: seed += NMH_PRIME32_2 + (UINT32_C(1) << 24) + (1 << 1);
					data.u32 = p[0];
					break;
				case 2: seed += NMH_PRIME32_2 + (UINT32_C(2) << 24) + (2 << 1);
					data.u32 = NMH_readLE16(p);
					break;
				case 3: seed += NMH_PRIME32_2 + (UINT32_C(3) << 24) + (3 << 1);
					data.u16[1] = p[2];
					data.u16[0] = NMH_readLE16(p);
					break;
				case 4: seed += NMH_PRIME32_3;
					data.u32 = NMH_readLE32(p);
					break;
				default: return 0;
			}
			return NMHASH32_0to8(data.u32 + seed, NMH_rotl32(seed, 5));
		}
	}
	if (NMH_likely(len < 256)) {
		return NMHASH32_33to255(p, len, seed);
	}
	return NMHASH32_avalanche32(NMHASH32_long(p, len, seed));
}

static inline
uint32_t
NMHASH32X_0to4(uint32_t x, uint32_t const seed)
{
	/* [bdab1ea9 18 a7896a1b 12 83796a2d 16] = 0.092922873297662509 */
	x ^= seed;
	x *= UINT32_C(0xBDAB1EA9);
	x += NMH_rotl32(seed, 31);
	x ^= x >> 18;
	x *= UINT32_C(0xA7896A1B);
	x ^= x >> 12;
	x *= UINT32_C(0x83796A2D);
	x ^= x >> 16;
	return x;
}

static inline
uint32_t
NMHASH32X_5to8(const uint8_t* const NMH_RESTRICT p, size_t const len, uint32_t const seed)
{
	/* - 5 to 9 bytes
	 * - mixer: [11049a7d 23 bcccdc7b 12 065e9dad 12] = 0.16577596555667246 */

	uint32_t       x = NMH_readLE32(p) ^ NMH_PRIME32_3;
	uint32_t const y = NMH_readLE32(p + len - 4) ^ seed;
	x += y;
	x ^= x >> len;
	x *= UINT32_C(0x11049A7D);
	x ^= x >> 23;
	x *= UINT32_C(0xBCCCDC7B);
	x ^= NMH_rotl32(y, 3);
	x ^= x >> 12;
	x *= UINT32_C(0x065E9DAD);
	x ^= x >> 12;
	return x;
}

static inline
uint32_t
NMHASH32X_9to255(const uint8_t* const NMH_RESTRICT p, size_t const len, uint32_t const seed)
{
	/* - at least 9 bytes
	 * - base mixer: [11049a7d 23 bcccdc7b 12 065e9dad 12] = 0.16577596555667246
	 * - tail mixer: [16 a52fb2cd 15 551e4d49 16] = 0.17162579707098322
	 */

	uint32_t x = NMH_PRIME32_3;
	uint32_t y = seed;
	uint32_t a = NMH_PRIME32_4;
	uint32_t b = seed;
	size_t i, r = (len - 1) / 16;

	for (i = 0; i < r; ++i) {
		x ^= NMH_readLE32(p + i * 16 + 0);
		y ^= NMH_readLE32(p + i * 16 + 4);
		x ^= y;
		x *= UINT32_C(0x11049A7D);
		x ^= x >> 23;
		x *= UINT32_C(0xBCCCDC7B);
		y  = NMH_rotl32(y, 4);
		x ^= y;
		x ^= x >> 12;
		x *= UINT32_C(0x065E9DAD);
		x ^= x >> 12;

		a ^= NMH_readLE32(p + i * 16 + 8);
		b ^= NMH_readLE32(p + i * 16 + 12);
		a ^= b;
		a *= UINT32_C(0x11049A7D);
		a ^= a >> 23;
		a *= UINT32_C(0xBCCCDC7B);
		b  = NMH_rotl32(b, 3);
		a ^= b;
		a ^= a >> 12;
		a *= UINT32_C(0x065E9DAD);
		a ^= a >> 12;
	}

	if (NMH_likely(((uint8_t)len-1) & 8)) {
		if (NMH_likely(((uint8_t)len-1) & 4)) {
			a ^= NMH_readLE32(p + r * 16 + 0);
			b ^= NMH_readLE32(p + r * 16 + 4);
			a ^= b;
			a *= UINT32_C(0x11049A7D);
			a ^= a >> 23;
			a *= UINT32_C(0xBCCCDC7B);
			a ^= NMH_rotl32(b, 4);
			a ^= a >> 12;
			a *= UINT32_C(0x065E9DAD);
		} else {
			a ^= NMH_readLE32(p + r * 16) + b;
			a ^= a >> 16;
			a *= UINT32_C(0xA52FB2CD);
			a ^= a >> 15;
			a *= UINT32_C(0x551E4D49);
		}

		x ^= NMH_readLE32(p + len - 8);
		y ^= NMH_readLE32(p + len - 4);
		x ^= y;
		x *= UINT32_C(0x11049A7D);
		x ^= x >> 23;
		x *= UINT32_C(0xBCCCDC7B);
		x ^= NMH_rotl32(y, 3);
		x ^= x >> 12;
		x *= UINT32_C(0x065E9DAD);
	} else {
		if (NMH_likely(((uint8_t)len-1) & 4)) {
			a ^= NMH_readLE32(p + r * 16) + b;
			a ^= a >> 16;
			a *= UINT32_C(0xA52FB2CD);
			a ^= a >> 15;
			a *= UINT32_C(0x551E4D49);
		}
		x ^= NMH_readLE32(p + len - 4) + y;
		x ^= x >> 16;
		x *= UINT32_C(0xA52FB2CD);
		x ^= x >> 15;
		x *= UINT32_C(0x551E4D49);
	}

	x ^= (uint32_t)len;
	x ^= NMH_rotl32(a, 27); /* rotate one lane to pass Diff test */
	x ^= x >> 14;
	x *= UINT32_C(0x141CC535);

	return x;
}

static inline
uint32_t
NMHASH32X_avalanche32(uint32_t x)
{
	/* mixer with 2 mul from skeeto/hash-prospector:
	 * [15 d168aaad 15 af723597 15] = 0.15983776156606694
	 */
	x ^= x >> 15;
	x *= UINT32_C(0xD168AAAD);
	x ^= x >> 15;
	x *= UINT32_C(0xAF723597);
	x ^= x >> 15;
	return x;
}

/* use 32*32->32 multiplication for short hash */
static inline
uint32_t
NMHASH32X(const void* const NMH_RESTRICT input, size_t const len, uint32_t seed)
{
	const uint8_t *const p = (const uint8_t *)input;
	if (NMH_likely(len <= 8)) {
		if (NMH_likely(len > 4)) {
			return NMHASH32X_5to8(p, len, seed);
		} else {
			/* 0-4 bytes */
			union { uint32_t u32; uint16_t u16[2]; uint8_t u8[4]; } data;
			switch (len) {
				case 0: seed += NMH_PRIME32_2;
					data.u32 = 0;
					break;
				case 1: seed += NMH_PRIME32_2 + (UINT32_C(1) << 24) + (1 << 1);
					data.u32 = p[0];
					break;
				case 2: seed += NMH_PRIME32_2 + (UINT32_C(2) << 24) + (2 << 1);
					data.u32 = NMH_readLE16(p);
					break;
				case 3: seed += NMH_PRIME32_2 + (UINT32_C(3) << 24) + (3 << 1);
					data.u16[1] = p[2];
					data.u16[0] = NMH_readLE16(p);
					break;
				case 4: seed += NMH_PRIME32_1;
					data.u32 = NMH_readLE32(p);
					break;
				default: return 0;
			}
			return NMHASH32X_0to4(data.u32, seed);
		}
	}
	if (NMH_likely(len < 256)) {
		return NMHASH32X_9to255(p, len, seed);
	}
	return NMHASH32X_avalanche32(NMHASH32_long(p, len, seed));
}

#if defined(_MSC_VER) && _MSC_VER >= 1914
#  pragma warning(pop)
#endif
#ifdef __SDCC
#  pragma restore
#  undef const
#endif

#endif /* _nmhash_h_ */

#ifdef __cplusplus
}
#endif
