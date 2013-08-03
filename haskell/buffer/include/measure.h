#ifdef __GLASGOW_HASKELL__
#define BOX I#
#define HASH #
#define ID(x) x
#define INT Int#
#define PACK(a,b) (let { BOX packed_a = a; BOX packed_b = b } in (ID(HASH) packed_a, packed_b ID(HASH)))
#define PLUS +ID(HASH)
#define UNBOX(a) = let I# unboxedResult = a in unboxedResult
#define UNPACK(a,b) (ID(HASH) BOX -> a, BOX -> b ID(HASH))
#define TUPLE(a,b) (ID(HASH) a, b ID(HASH))
#else
#define BOX
#define INT Int
#define PACK(a,b) (a,b)
#define UNPACK(a,b) (a,b)
#define TUPLE(a,b) (a,b)
#define PLUS +
#define UNBOX(a) a
#endif

#define MEASURED(a) UNPACK(s##a,s##a) = measureU a
#define MEASURED(a,b) MEASURED(a); MEASURED(b)
#define MEASURED(a,b,c) MEASURED(a,b); MEASURED(c)
#define MEASURED(a,b,c,d) MEASURED(a,b); MEASURED(c,d)
