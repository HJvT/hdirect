{-# OPTIONS -fglasgow-exts -cpp #-}
-- parser produced by Happy Version 1.13

{-
%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
% @(#) $Docid: Jun. 6th 2003  16:54  Sigbjorn Finne $
% @(#) $Contactid: sof@galois.com $
%

A grammar for IDL, DCE / MS (IDL/ODL) style.

Conflicts:
   - 1 reduce/reduce conflict due to `default'
     both being an attribute and a keyword.
   - 7 shift/reduce conflicts due to the overloading
     of `const' (t. qualifier and keyword.)

ToDo: 
  - fix above conflicts.

-}
module Parser ( parseIDL ) where
 
import LexM
import Lex
import IDLToken
import IDLSyn
import IDLUtils ( mkFunId, mkMethodId, toCConvAttrib,
                  mkGNUAttrib, toPackedAttrib, exprType )
import BasicTypes
import Literal
import System.IO ( hPutStrLn, stderr )
{-
BEGIN_GHC_ONLY
import GlaExts
END_GHC_ONLY
-}
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

data HappyAbsSyn 
        = HappyTerminal IDLToken
        | HappyErrorToken Int
        | HappyAbsSyn4 (Either [Defn] [(Name, Bool, [Attribute])])
        | HappyAbsSyn5 ([Defn])
        | HappyAbsSyn6 ([(String, Bool, [Attribute])])
        | HappyAbsSyn7 ((String, Bool, [Attribute] ))
        | HappyAbsSyn8 (String)
        | HappyAbsSyn9 (Defn)
        | HappyAbsSyn13 ([([Attribute], Type, Id)])
        | HappyAbsSyn14 ([CoClassMember])
        | HappyAbsSyn15 (CoClassMember)
        | HappyAbsSyn16 ([Attribute])
        | HappyAbsSyn18 (Id)
        | HappyAbsSyn25 (Inherit)
        | HappyAbsSyn29 ([String])
        | HappyAbsSyn30 (Type)
        | HappyAbsSyn34 (Expr)
        | HappyAbsSyn42 (BinaryOp)
        | HappyAbsSyn49 (UnaryOp)
        | HappyAbsSyn54 (Qualifier)
        | HappyAbsSyn62 (Either [Switch] [Member])
        | HappyAbsSyn63 ([ Id ])
        | HappyAbsSyn65 ((Id -> Id))
        | HappyAbsSyn71 ([[Qualifier]])
        | HappyAbsSyn72 ([ Qualifier ])
        | HappyAbsSyn73 ([Member])
        | HappyAbsSyn74 (Member)
        | HappyAbsSyn75 (Maybe Id)
        | HappyAbsSyn77 ([Switch])
        | HappyAbsSyn78 (Switch)
        | HappyAbsSyn79 (Maybe SwitchArm)
        | HappyAbsSyn81 ([CaseLabel])
        | HappyAbsSyn82 (CaseLabel)
        | HappyAbsSyn84 ([Expr])
        | HappyAbsSyn87 ([(Id, [Attribute], Maybe Expr)])
        | HappyAbsSyn88 ((Id, [Attribute], Maybe Expr))
        | HappyAbsSyn90 ((Maybe Expr -> Type))
        | HappyAbsSyn95 ([[Attribute]])
        | HappyAbsSyn98 (Attribute)
        | HappyAbsSyn101 ([AttrParam])
        | HappyAbsSyn103 (AttrParam)
        | HappyAbsSyn105 (Param)
        | HappyAbsSyn106 ([Param])
        | HappyAbsSyn109 ([GNUAttrib])
        | HappyAbsSyn111 (GNUAttrib)
        | HappyAbsSyn115 (())

type HappyReduction = 
           Int# 
        -> (IDLToken)
        -> HappyState (IDLToken) (HappyStk HappyAbsSyn -> LexM(HappyAbsSyn))
        -> [HappyState (IDLToken) (HappyStk HappyAbsSyn -> LexM(HappyAbsSyn))] 
        -> HappyStk HappyAbsSyn 
        -> LexM(HappyAbsSyn)

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277,
 action_278,
 action_279,
 action_280,
 action_281,
 action_282,
 action_283,
 action_284,
 action_285,
 action_286,
 action_287,
 action_288,
 action_289,
 action_290,
 action_291,
 action_292,
 action_293,
 action_294,
 action_295,
 action_296,
 action_297,
 action_298,
 action_299,
 action_300,
 action_301,
 action_302,
 action_303,
 action_304,
 action_305,
 action_306,
 action_307,
 action_308,
 action_309,
 action_310,
 action_311,
 action_312,
 action_313,
 action_314,
 action_315,
 action_316,
 action_317,
 action_318,
 action_319,
 action_320,
 action_321,
 action_322,
 action_323,
 action_324,
 action_325,
 action_326,
 action_327,
 action_328,
 action_329,
 action_330,
 action_331,
 action_332,
 action_333,
 action_334,
 action_335,
 action_336,
 action_337,
 action_338,
 action_339,
 action_340,
 action_341,
 action_342,
 action_343,
 action_344,
 action_345,
 action_346,
 action_347,
 action_348,
 action_349,
 action_350,
 action_351,
 action_352,
 action_353,
 action_354,
 action_355,
 action_356,
 action_357,
 action_358,
 action_359,
 action_360,
 action_361,
 action_362,
 action_363,
 action_364,
 action_365,
 action_366,
 action_367,
 action_368,
 action_369,
 action_370,
 action_371,
 action_372,
 action_373,
 action_374,
 action_375,
 action_376,
 action_377,
 action_378,
 action_379,
 action_380,
 action_381,
 action_382,
 action_383,
 action_384,
 action_385,
 action_386,
 action_387,
 action_388,
 action_389,
 action_390,
 action_391,
 action_392,
 action_393,
 action_394,
 action_395,
 action_396,
 action_397,
 action_398,
 action_399,
 action_400,
 action_401,
 action_402,
 action_403,
 action_404,
 action_405,
 action_406,
 action_407,
 action_408,
 action_409,
 action_410,
 action_411,
 action_412,
 action_413,
 action_414,
 action_415,
 action_416,
 action_417,
 action_418,
 action_419,
 action_420,
 action_421,
 action_422,
 action_423,
 action_424,
 action_425,
 action_426,
 action_427,
 action_428,
 action_429,
 action_430,
 action_431,
 action_432,
 action_433,
 action_434,
 action_435,
 action_436,
 action_437,
 action_438,
 action_439,
 action_440,
 action_441,
 action_442,
 action_443,
 action_444,
 action_445,
 action_446,
 action_447,
 action_448,
 action_449,
 action_450,
 action_451,
 action_452,
 action_453,
 action_454,
 action_455,
 action_456,
 action_457,
 action_458,
 action_459,
 action_460,
 action_461,
 action_462,
 action_463,
 action_464,
 action_465,
 action_466,
 action_467,
 action_468,
 action_469,
 action_470,
 action_471,
 action_472,
 action_473,
 action_474,
 action_475,
 action_476,
 action_477,
 action_478,
 action_479,
 action_480,
 action_481,
 action_482,
 action_483,
 action_484,
 action_485,
 action_486,
 action_487,
 action_488,
 action_489,
 action_490,
 action_491,
 action_492,
 action_493,
 action_494,
 action_495,
 action_496,
 action_497,
 action_498,
 action_499,
 action_500,
 action_501,
 action_502,
 action_503,
 action_504,
 action_505,
 action_506,
 action_507,
 action_508,
 action_509,
 action_510,
 action_511,
 action_512,
 action_513,
 action_514,
 action_515,
 action_516,
 action_517,
 action_518,
 action_519,
 action_520,
 action_521,
 action_522,
 action_523,
 action_524,
 action_525,
 action_526,
 action_527,
 action_528,
 action_529,
 action_530,
 action_531,
 action_532,
 action_533,
 action_534,
 action_535,
 action_536,
 action_537,
 action_538,
 action_539,
 action_540,
 action_541,
 action_542,
 action_543,
 action_544,
 action_545,
 action_546,
 action_547,
 action_548,
 action_549,
 action_550,
 action_551,
 action_552,
 action_553,
 action_554,
 action_555,
 action_556,
 action_557,
 action_558,
 action_559,
 action_560,
 action_561,
 action_562,
 action_563,
 action_564,
 action_565,
 action_566,
 action_567,
 action_568,
 action_569,
 action_570,
 action_571,
 action_572,
 action_573,
 action_574,
 action_575,
 action_576,
 action_577,
 action_578,
 action_579,
 action_580,
 action_581,
 action_582,
 action_583,
 action_584,
 action_585,
 action_586,
 action_587,
 action_588,
 action_589,
 action_590,
 action_591,
 action_592,
 action_593,
 action_594,
 action_595,
 action_596,
 action_597 :: Int# -> HappyReduction

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149,
 happyReduce_150,
 happyReduce_151,
 happyReduce_152,
 happyReduce_153,
 happyReduce_154,
 happyReduce_155,
 happyReduce_156,
 happyReduce_157,
 happyReduce_158,
 happyReduce_159,
 happyReduce_160,
 happyReduce_161,
 happyReduce_162,
 happyReduce_163,
 happyReduce_164,
 happyReduce_165,
 happyReduce_166,
 happyReduce_167,
 happyReduce_168,
 happyReduce_169,
 happyReduce_170,
 happyReduce_171,
 happyReduce_172,
 happyReduce_173,
 happyReduce_174,
 happyReduce_175,
 happyReduce_176,
 happyReduce_177,
 happyReduce_178,
 happyReduce_179,
 happyReduce_180,
 happyReduce_181,
 happyReduce_182,
 happyReduce_183,
 happyReduce_184,
 happyReduce_185,
 happyReduce_186,
 happyReduce_187,
 happyReduce_188,
 happyReduce_189,
 happyReduce_190,
 happyReduce_191,
 happyReduce_192,
 happyReduce_193,
 happyReduce_194,
 happyReduce_195,
 happyReduce_196,
 happyReduce_197,
 happyReduce_198,
 happyReduce_199,
 happyReduce_200,
 happyReduce_201,
 happyReduce_202,
 happyReduce_203,
 happyReduce_204,
 happyReduce_205,
 happyReduce_206,
 happyReduce_207,
 happyReduce_208,
 happyReduce_209,
 happyReduce_210,
 happyReduce_211,
 happyReduce_212,
 happyReduce_213,
 happyReduce_214,
 happyReduce_215,
 happyReduce_216,
 happyReduce_217,
 happyReduce_218,
 happyReduce_219,
 happyReduce_220,
 happyReduce_221,
 happyReduce_222,
 happyReduce_223,
 happyReduce_224,
 happyReduce_225,
 happyReduce_226,
 happyReduce_227,
 happyReduce_228,
 happyReduce_229,
 happyReduce_230,
 happyReduce_231,
 happyReduce_232,
 happyReduce_233,
 happyReduce_234,
 happyReduce_235,
 happyReduce_236,
 happyReduce_237,
 happyReduce_238,
 happyReduce_239,
 happyReduce_240,
 happyReduce_241,
 happyReduce_242,
 happyReduce_243,
 happyReduce_244,
 happyReduce_245,
 happyReduce_246,
 happyReduce_247,
 happyReduce_248,
 happyReduce_249,
 happyReduce_250,
 happyReduce_251,
 happyReduce_252,
 happyReduce_253,
 happyReduce_254,
 happyReduce_255,
 happyReduce_256,
 happyReduce_257,
 happyReduce_258,
 happyReduce_259,
 happyReduce_260,
 happyReduce_261,
 happyReduce_262,
 happyReduce_263,
 happyReduce_264,
 happyReduce_265,
 happyReduce_266,
 happyReduce_267,
 happyReduce_268,
 happyReduce_269,
 happyReduce_270,
 happyReduce_271,
 happyReduce_272,
 happyReduce_273,
 happyReduce_274,
 happyReduce_275,
 happyReduce_276,
 happyReduce_277,
 happyReduce_278,
 happyReduce_279,
 happyReduce_280,
 happyReduce_281,
 happyReduce_282,
 happyReduce_283,
 happyReduce_284,
 happyReduce_285,
 happyReduce_286,
 happyReduce_287,
 happyReduce_288,
 happyReduce_289,
 happyReduce_290,
 happyReduce_291,
 happyReduce_292,
 happyReduce_293,
 happyReduce_294,
 happyReduce_295,
 happyReduce_296,
 happyReduce_297,
 happyReduce_298,
 happyReduce_299,
 happyReduce_300,
 happyReduce_301,
 happyReduce_302,
 happyReduce_303,
 happyReduce_304,
 happyReduce_305,
 happyReduce_306,
 happyReduce_307,
 happyReduce_308,
 happyReduce_309,
 happyReduce_310,
 happyReduce_311,
 happyReduce_312,
 happyReduce_313,
 happyReduce_314,
 happyReduce_315,
 happyReduce_316,
 happyReduce_317,
 happyReduce_318,
 happyReduce_319,
 happyReduce_320,
 happyReduce_321,
 happyReduce_322,
 happyReduce_323,
 happyReduce_324,
 happyReduce_325,
 happyReduce_326,
 happyReduce_327,
 happyReduce_328,
 happyReduce_329,
 happyReduce_330,
 happyReduce_331,
 happyReduce_332,
 happyReduce_333,
 happyReduce_334,
 happyReduce_335,
 happyReduce_336,
 happyReduce_337,
 happyReduce_338,
 happyReduce_339,
 happyReduce_340,
 happyReduce_341,
 happyReduce_342,
 happyReduce_343,
 happyReduce_344,
 happyReduce_345,
 happyReduce_346,
 happyReduce_347,
 happyReduce_348,
 happyReduce_349,
 happyReduce_350,
 happyReduce_351,
 happyReduce_352,
 happyReduce_353,
 happyReduce_354,
 happyReduce_355,
 happyReduce_356,
 happyReduce_357,
 happyReduce_358,
 happyReduce_359 :: HappyReduction

action_0 (130#) = happyShift action_4
action_0 (4#) = happyGoto action_3
action_0 (5#) = happyGoto action_2
action_0 x = happyTcHack x happyReduce_3

action_1 (5#) = happyGoto action_2
action_1 x = happyTcHack x happyFail

action_2 (1#) = happyShift action_36
action_2 (118#) = happyShift action_37
action_2 (119#) = happyShift action_38
action_2 (128#) = happyShift action_39
action_2 (144#) = happyReduce_344
action_2 (145#) = happyReduce_344
action_2 (146#) = happyShift action_40
action_2 (147#) = happyShift action_41
action_2 (148#) = happyShift action_42
action_2 (149#) = happyShift action_43
action_2 (150#) = happyShift action_44
action_2 (151#) = happyShift action_45
action_2 (152#) = happyShift action_46
action_2 (153#) = happyShift action_47
action_2 (154#) = happyShift action_48
action_2 (155#) = happyShift action_49
action_2 (156#) = happyShift action_50
action_2 (157#) = happyShift action_51
action_2 (158#) = happyShift action_52
action_2 (159#) = happyShift action_53
action_2 (163#) = happyShift action_54
action_2 (168#) = happyShift action_55
action_2 (171#) = happyShift action_56
action_2 (176#) = happyShift action_57
action_2 (177#) = happyShift action_58
action_2 (178#) = happyShift action_59
action_2 (179#) = happyShift action_60
action_2 (183#) = happyShift action_61
action_2 (184#) = happyShift action_62
action_2 (187#) = happyShift action_63
action_2 (188#) = happyShift action_64
action_2 (189#) = happyShift action_65
action_2 (190#) = happyShift action_66
action_2 (191#) = happyShift action_67
action_2 (192#) = happyShift action_68
action_2 (193#) = happyShift action_69
action_2 (194#) = happyShift action_70
action_2 (195#) = happyShift action_71
action_2 (196#) = happyShift action_72
action_2 (197#) = happyShift action_73
action_2 (199#) = happyReduce_1
action_2 (9#) = happyGoto action_6
action_2 (10#) = happyGoto action_7
action_2 (11#) = happyGoto action_8
action_2 (12#) = happyGoto action_9
action_2 (17#) = happyGoto action_10
action_2 (18#) = happyGoto action_11
action_2 (19#) = happyGoto action_12
action_2 (20#) = happyGoto action_13
action_2 (21#) = happyGoto action_14
action_2 (26#) = happyGoto action_15
action_2 (27#) = happyGoto action_16
action_2 (28#) = happyGoto action_17
action_2 (31#) = happyGoto action_18
action_2 (32#) = happyGoto action_19
action_2 (51#) = happyGoto action_20
action_2 (53#) = happyGoto action_21
action_2 (55#) = happyGoto action_22
action_2 (58#) = happyGoto action_23
action_2 (59#) = happyGoto action_24
action_2 (60#) = happyGoto action_25
action_2 (61#) = happyGoto action_26
action_2 (86#) = happyGoto action_27
action_2 (89#) = happyGoto action_28
action_2 (90#) = happyGoto action_29
action_2 (93#) = happyGoto action_30
action_2 (96#) = happyGoto action_31
action_2 (104#) = happyGoto action_32
action_2 (109#) = happyGoto action_33
action_2 (110#) = happyGoto action_34
action_2 (111#) = happyGoto action_35
action_2 x = happyTcHack x happyFail

action_3 (199#) = happyAccept
action_3 x = happyTcHack x happyFail

action_4 (6#) = happyGoto action_5
action_4 x = happyTcHack x happyReduce_5

action_5 (146#) = happyShift action_175
action_5 (176#) = happyShift action_176
action_5 (7#) = happyGoto action_173
action_5 (8#) = happyGoto action_174
action_5 x = happyTcHack x happyReduce_2

action_6 x = happyTcHack x happyReduce_4

action_7 (1#) = happyShift action_166
action_7 (117#) = happyShift action_167
action_7 (115#) = happyGoto action_172
action_7 x = happyTcHack x happyFail

action_8 x = happyTcHack x happyReduce_20

action_9 x = happyTcHack x happyReduce_24

action_10 x = happyTcHack x happyReduce_21

action_11 (122#) = happyReduce_60
action_11 (124#) = happyShift action_171
action_11 (25#) = happyGoto action_170
action_11 x = happyTcHack x happyReduce_41

action_12 (122#) = happyShift action_169
action_12 x = happyTcHack x happyFail

action_13 (1#) = happyShift action_166
action_13 (117#) = happyShift action_167
action_13 (115#) = happyGoto action_168
action_13 x = happyTcHack x happyFail

action_14 (1#) = happyShift action_166
action_14 (117#) = happyShift action_167
action_14 (115#) = happyGoto action_165
action_14 x = happyTcHack x happyFail

action_15 (117#) = happyShift action_164
action_15 x = happyTcHack x happyFail

action_16 x = happyTcHack x happyReduce_18

action_17 x = happyTcHack x happyReduce_16

action_18 (153#) = happyShift action_161
action_18 (154#) = happyShift action_162
action_18 (155#) = happyShift action_163
action_18 x = happyTcHack x happyReduce_99

action_19 x = happyTcHack x happyReduce_184

action_20 (117#) = happyShift action_160
action_20 x = happyTcHack x happyFail

action_21 (168#) = happyShift action_159
action_21 x = happyTcHack x happyReduce_327

action_22 (128#) = happyShift action_157
action_22 (129#) = happyShift action_158
action_22 (54#) = happyGoto action_156
action_22 x = happyTcHack x happyReduce_176

action_23 x = happyTcHack x happyReduce_167

action_24 (120#) = happyReduce_198
action_24 (124#) = happyReduce_198
action_24 (128#) = happyReduce_198
action_24 (129#) = happyReduce_198
action_24 (146#) = happyReduce_198
action_24 (168#) = happyReduce_198
action_24 (172#) = happyReduce_198
action_24 (175#) = happyReduce_198
action_24 (176#) = happyReduce_198
action_24 (181#) = happyReduce_198
action_24 (193#) = happyReduce_198
action_24 x = happyTcHack x happyReduce_208

action_25 x = happyTcHack x happyReduce_210

action_26 x = happyTcHack x happyReduce_211

action_27 (120#) = happyReduce_199
action_27 (124#) = happyReduce_199
action_27 (128#) = happyReduce_199
action_27 (129#) = happyReduce_199
action_27 (146#) = happyReduce_199
action_27 (168#) = happyReduce_199
action_27 (172#) = happyReduce_199
action_27 (175#) = happyReduce_199
action_27 (176#) = happyReduce_199
action_27 (181#) = happyReduce_199
action_27 (193#) = happyReduce_199
action_27 x = happyTcHack x happyReduce_209

action_28 x = happyTcHack x happyReduce_328

action_29 (164#) = happyShift action_155
action_29 x = happyTcHack x happyReduce_295

action_30 x = happyTcHack x happyReduce_28

action_31 (1#) = happyShift action_36
action_31 (118#) = happyShift action_37
action_31 (119#) = happyShift action_38
action_31 (128#) = happyShift action_153
action_31 (146#) = happyShift action_40
action_31 (147#) = happyShift action_41
action_31 (148#) = happyShift action_42
action_31 (149#) = happyShift action_43
action_31 (150#) = happyShift action_44
action_31 (151#) = happyShift action_45
action_31 (152#) = happyShift action_46
action_31 (153#) = happyShift action_47
action_31 (154#) = happyShift action_48
action_31 (155#) = happyShift action_49
action_31 (156#) = happyShift action_50
action_31 (157#) = happyShift action_51
action_31 (158#) = happyShift action_52
action_31 (159#) = happyShift action_53
action_31 (163#) = happyShift action_54
action_31 (171#) = happyShift action_56
action_31 (176#) = happyShift action_57
action_31 (177#) = happyShift action_154
action_31 (178#) = happyShift action_59
action_31 (179#) = happyShift action_60
action_31 (183#) = happyShift action_61
action_31 (184#) = happyShift action_62
action_31 (197#) = happyShift action_73
action_31 (11#) = happyGoto action_151
action_31 (12#) = happyGoto action_9
action_31 (17#) = happyGoto action_10
action_31 (18#) = happyGoto action_11
action_31 (19#) = happyGoto action_12
action_31 (31#) = happyGoto action_18
action_31 (32#) = happyGoto action_19
action_31 (53#) = happyGoto action_21
action_31 (55#) = happyGoto action_22
action_31 (58#) = happyGoto action_152
action_31 (59#) = happyGoto action_24
action_31 (60#) = happyGoto action_25
action_31 (61#) = happyGoto action_26
action_31 (86#) = happyGoto action_27
action_31 (89#) = happyGoto action_28
action_31 (90#) = happyGoto action_29
action_31 (93#) = happyGoto action_30
action_31 (104#) = happyGoto action_32
action_31 x = happyTcHack x happyFail

action_32 (120#) = happyShift action_148
action_32 (124#) = happyShift action_104
action_32 (146#) = happyShift action_105
action_32 (172#) = happyShift action_106
action_32 (175#) = happyShift action_149
action_32 (176#) = happyShift action_86
action_32 (181#) = happyShift action_150
action_32 (193#) = happyShift action_69
action_32 (64#) = happyGoto action_139
action_32 (65#) = happyGoto action_140
action_32 (66#) = happyGoto action_141
action_32 (67#) = happyGoto action_142
action_32 (68#) = happyGoto action_143
action_32 (69#) = happyGoto action_144
action_32 (70#) = happyGoto action_145
action_32 (71#) = happyGoto action_146
action_32 (110#) = happyGoto action_147
action_32 (111#) = happyGoto action_35
action_32 (114#) = happyGoto action_102
action_32 x = happyTcHack x happyFail

action_33 (144#) = happyShift action_137
action_33 (145#) = happyShift action_138
action_33 x = happyTcHack x happyFail

action_34 (193#) = happyShift action_69
action_34 (111#) = happyGoto action_136
action_34 x = happyTcHack x happyReduce_345

action_35 x = happyTcHack x happyReduce_346

action_36 x = happyTcHack x happyReduce_200

action_37 (176#) = happyShift action_86
action_37 (114#) = happyGoto action_135
action_37 x = happyTcHack x happyFail

action_38 (146#) = happyShift action_133
action_38 (147#) = happyShift action_134
action_38 (176#) = happyShift action_86
action_38 (114#) = happyGoto action_132
action_38 x = happyTcHack x happyFail

action_39 (1#) = happyShift action_122
action_39 (146#) = happyShift action_123
action_39 (147#) = happyShift action_124
action_39 (148#) = happyShift action_125
action_39 (149#) = happyShift action_43
action_39 (150#) = happyShift action_44
action_39 (151#) = happyShift action_45
action_39 (152#) = happyShift action_46
action_39 (153#) = happyShift action_47
action_39 (154#) = happyShift action_126
action_39 (155#) = happyShift action_127
action_39 (156#) = happyShift action_128
action_39 (157#) = happyShift action_129
action_39 (171#) = happyShift action_130
action_39 (176#) = happyShift action_131
action_39 (183#) = happyShift action_61
action_39 (184#) = happyShift action_62
action_39 (30#) = happyGoto action_118
action_39 (31#) = happyGoto action_119
action_39 (32#) = happyGoto action_120
action_39 (89#) = happyGoto action_121
action_39 (90#) = happyGoto action_29
action_39 x = happyTcHack x happyFail

action_40 x = happyTcHack x happyReduce_195

action_41 x = happyTcHack x happyReduce_196

action_42 x = happyTcHack x happyReduce_181

action_43 x = happyTcHack x happyReduce_94

action_44 (150#) = happyShift action_117
action_44 x = happyTcHack x happyReduce_95

action_45 x = happyTcHack x happyReduce_97

action_46 x = happyTcHack x happyReduce_98

action_47 x = happyTcHack x happyReduce_101

action_48 (128#) = happyShift action_116
action_48 (146#) = happyShift action_111
action_48 (147#) = happyShift action_112
action_48 (148#) = happyShift action_113
action_48 (149#) = happyShift action_43
action_48 (150#) = happyShift action_44
action_48 (151#) = happyShift action_45
action_48 (152#) = happyShift action_46
action_48 (153#) = happyShift action_47
action_48 (156#) = happyShift action_114
action_48 (31#) = happyGoto action_107
action_48 (32#) = happyGoto action_108
action_48 (56#) = happyGoto action_115
action_48 x = happyTcHack x happyReduce_187

action_49 (128#) = happyShift action_110
action_49 (146#) = happyShift action_111
action_49 (147#) = happyShift action_112
action_49 (148#) = happyShift action_113
action_49 (149#) = happyShift action_43
action_49 (150#) = happyShift action_44
action_49 (151#) = happyShift action_45
action_49 (152#) = happyShift action_46
action_49 (153#) = happyShift action_47
action_49 (156#) = happyShift action_114
action_49 (31#) = happyGoto action_107
action_49 (32#) = happyGoto action_108
action_49 (56#) = happyGoto action_109
action_49 x = happyTcHack x happyReduce_186

action_50 x = happyTcHack x happyReduce_182

action_51 x = happyTcHack x happyReduce_183

action_52 (122#) = happyShift action_103
action_52 (124#) = happyShift action_104
action_52 (146#) = happyShift action_105
action_52 (172#) = happyShift action_106
action_52 (176#) = happyShift action_86
action_52 (67#) = happyGoto action_101
action_52 (114#) = happyGoto action_102
action_52 x = happyTcHack x happyFail

action_53 (176#) = happyShift action_86
action_53 (75#) = happyGoto action_99
action_53 (114#) = happyGoto action_100
action_53 x = happyTcHack x happyReduce_257

action_54 (122#) = happyShift action_98
action_54 (176#) = happyShift action_86
action_54 (114#) = happyGoto action_97
action_54 x = happyTcHack x happyFail

action_55 (169#) = happyShift action_94
action_55 (172#) = happyShift action_95
action_55 (176#) = happyShift action_86
action_55 (183#) = happyShift action_96
action_55 (97#) = happyGoto action_91
action_55 (98#) = happyGoto action_92
action_55 (114#) = happyGoto action_93
action_55 x = happyTcHack x happyFail

action_56 x = happyTcHack x happyReduce_185

action_57 x = happyTcHack x happyReduce_194

action_58 (146#) = happyShift action_90
action_58 (176#) = happyShift action_86
action_58 (114#) = happyGoto action_89
action_58 x = happyTcHack x happyFail

action_59 (146#) = happyShift action_88
action_59 (176#) = happyShift action_86
action_59 (114#) = happyGoto action_87
action_59 x = happyTcHack x happyFail

action_60 (176#) = happyShift action_86
action_60 (114#) = happyGoto action_85
action_60 x = happyTcHack x happyFail

action_61 x = happyTcHack x happyReduce_296

action_62 x = happyTcHack x happyReduce_297

action_63 (120#) = happyShift action_84
action_63 x = happyTcHack x happyFail

action_64 (120#) = happyShift action_83
action_64 x = happyTcHack x happyFail

action_65 x = happyTcHack x happyReduce_50

action_66 (120#) = happyShift action_82
action_66 x = happyTcHack x happyFail

action_67 x = happyTcHack x happyReduce_66

action_68 x = happyTcHack x happyReduce_67

action_69 (120#) = happyShift action_81
action_69 x = happyTcHack x happyFail

action_70 (174#) = happyShift action_80
action_70 (29#) = happyGoto action_79
action_70 x = happyTcHack x happyFail

action_71 x = happyTcHack x happyReduce_65

action_72 (176#) = happyShift action_78
action_72 x = happyTcHack x happyFail

action_73 (1#) = happyShift action_36
action_73 (146#) = happyShift action_40
action_73 (147#) = happyShift action_41
action_73 (148#) = happyShift action_42
action_73 (149#) = happyShift action_43
action_73 (150#) = happyShift action_44
action_73 (151#) = happyShift action_45
action_73 (152#) = happyShift action_46
action_73 (153#) = happyShift action_47
action_73 (154#) = happyShift action_48
action_73 (155#) = happyShift action_49
action_73 (156#) = happyShift action_50
action_73 (157#) = happyShift action_51
action_73 (158#) = happyShift action_52
action_73 (159#) = happyShift action_53
action_73 (163#) = happyShift action_54
action_73 (171#) = happyShift action_56
action_73 (176#) = happyShift action_57
action_73 (197#) = happyShift action_73
action_73 (31#) = happyGoto action_18
action_73 (32#) = happyGoto action_19
action_73 (55#) = happyGoto action_74
action_73 (57#) = happyGoto action_75
action_73 (59#) = happyGoto action_76
action_73 (60#) = happyGoto action_25
action_73 (61#) = happyGoto action_26
action_73 (86#) = happyGoto action_77
action_73 x = happyTcHack x happyFail

action_74 x = happyTcHack x happyReduce_206

action_75 (121#) = happyShift action_274
action_75 (181#) = happyShift action_275
action_75 x = happyTcHack x happyFail

action_76 x = happyTcHack x happyReduce_198

action_77 x = happyTcHack x happyReduce_199

action_78 (120#) = happyShift action_195
action_78 (141#) = happyShift action_196
action_78 (142#) = happyShift action_197
action_78 (170#) = happyShift action_198
action_78 (173#) = happyShift action_199
action_78 (174#) = happyShift action_200
action_78 (176#) = happyShift action_86
action_78 (180#) = happyShift action_201
action_78 (182#) = happyShift action_202
action_78 (34#) = happyGoto action_264
action_78 (35#) = happyGoto action_265
action_78 (36#) = happyGoto action_266
action_78 (37#) = happyGoto action_267
action_78 (38#) = happyGoto action_268
action_78 (39#) = happyGoto action_269
action_78 (40#) = happyGoto action_270
action_78 (41#) = happyGoto action_271
action_78 (43#) = happyGoto action_272
action_78 (44#) = happyGoto action_273
action_78 (45#) = happyGoto action_188
action_78 (46#) = happyGoto action_189
action_78 (47#) = happyGoto action_190
action_78 (48#) = happyGoto action_191
action_78 (49#) = happyGoto action_192
action_78 (50#) = happyGoto action_193
action_78 (114#) = happyGoto action_194
action_78 x = happyTcHack x happyFail

action_79 x = happyTcHack x happyReduce_63

action_80 (125#) = happyShift action_263
action_80 x = happyTcHack x happyReduce_69

action_81 (120#) = happyShift action_262
action_81 x = happyTcHack x happyFail

action_82 (174#) = happyShift action_261
action_82 x = happyTcHack x happyFail

action_83 (174#) = happyShift action_260
action_83 x = happyTcHack x happyFail

action_84 (174#) = happyShift action_259
action_84 x = happyTcHack x happyFail

action_85 (122#) = happyShift action_258
action_85 x = happyTcHack x happyFail

action_86 x = happyTcHack x happyReduce_355

action_87 (122#) = happyShift action_257
action_87 x = happyTcHack x happyFail

action_88 (122#) = happyShift action_256
action_88 x = happyTcHack x happyFail

action_89 (117#) = happyShift action_255
action_89 x = happyTcHack x happyReduce_46

action_90 x = happyTcHack x happyReduce_47

action_91 (125#) = happyShift action_254
action_91 (116#) = happyGoto action_253
action_91 x = happyTcHack x happyReduce_358

action_92 x = happyTcHack x happyReduce_307

action_93 (120#) = happyShift action_252
action_93 (101#) = happyGoto action_251
action_93 x = happyTcHack x happyReduce_316

action_94 x = happyTcHack x happyReduce_305

action_95 x = happyTcHack x happyReduce_311

action_96 x = happyTcHack x happyReduce_310

action_97 (122#) = happyShift action_250
action_97 x = happyTcHack x happyReduce_287

action_98 (168#) = happyShift action_55
action_98 (176#) = happyShift action_86
action_98 (87#) = happyGoto action_246
action_98 (88#) = happyGoto action_247
action_98 (96#) = happyGoto action_248
action_98 (114#) = happyGoto action_249
action_98 x = happyTcHack x happyFail

action_99 (122#) = happyShift action_244
action_99 (160#) = happyShift action_245
action_99 x = happyTcHack x happyFail

action_100 (122#) = happyReduce_258
action_100 (160#) = happyReduce_258
action_100 (193#) = happyShift action_69
action_100 (109#) = happyGoto action_243
action_100 (110#) = happyGoto action_34
action_100 (111#) = happyGoto action_35
action_100 x = happyTcHack x happyReduce_344

action_101 (122#) = happyShift action_242
action_101 x = happyTcHack x happyReduce_214

action_102 (124#) = happyShift action_241
action_102 x = happyTcHack x happyReduce_232

action_103 (168#) = happyShift action_55
action_103 (73#) = happyGoto action_238
action_103 (74#) = happyGoto action_239
action_103 (94#) = happyGoto action_240
action_103 (95#) = happyGoto action_219
action_103 (96#) = happyGoto action_220
action_103 x = happyTcHack x happyReduce_303

action_104 (173#) = happyShift action_237
action_104 x = happyTcHack x happyFail

action_105 (124#) = happyShift action_236
action_105 x = happyTcHack x happyReduce_236

action_106 x = happyTcHack x happyReduce_237

action_107 (153#) = happyShift action_161
action_107 x = happyTcHack x happyReduce_99

action_108 x = happyTcHack x happyReduce_201

action_109 x = happyTcHack x happyReduce_188

action_110 (146#) = happyShift action_111
action_110 (147#) = happyShift action_112
action_110 (148#) = happyShift action_113
action_110 (149#) = happyShift action_43
action_110 (150#) = happyShift action_44
action_110 (151#) = happyShift action_45
action_110 (152#) = happyShift action_46
action_110 (153#) = happyShift action_47
action_110 (156#) = happyShift action_114
action_110 (31#) = happyGoto action_107
action_110 (32#) = happyGoto action_108
action_110 (56#) = happyGoto action_235
action_110 x = happyTcHack x happyFail

action_111 x = happyTcHack x happyReduce_204

action_112 x = happyTcHack x happyReduce_205

action_113 x = happyTcHack x happyReduce_203

action_114 x = happyTcHack x happyReduce_202

action_115 x = happyTcHack x happyReduce_190

action_116 (146#) = happyShift action_111
action_116 (147#) = happyShift action_112
action_116 (148#) = happyShift action_113
action_116 (149#) = happyShift action_43
action_116 (150#) = happyShift action_44
action_116 (151#) = happyShift action_45
action_116 (152#) = happyShift action_46
action_116 (153#) = happyShift action_47
action_116 (156#) = happyShift action_114
action_116 (31#) = happyGoto action_107
action_116 (32#) = happyGoto action_108
action_116 (56#) = happyGoto action_234
action_116 x = happyTcHack x happyFail

action_117 x = happyTcHack x happyReduce_96

action_118 (168#) = happyShift action_233
action_118 (176#) = happyShift action_86
action_118 (114#) = happyGoto action_232
action_118 x = happyTcHack x happyFail

action_119 (153#) = happyShift action_161
action_119 (154#) = happyShift action_230
action_119 (155#) = happyShift action_231
action_119 x = happyTcHack x happyReduce_99

action_120 x = happyTcHack x happyReduce_71

action_121 x = happyTcHack x happyReduce_79

action_122 x = happyTcHack x happyReduce_93

action_123 (181#) = happyShift action_229
action_123 x = happyTcHack x happyReduce_90

action_124 x = happyTcHack x happyReduce_92

action_125 x = happyTcHack x happyReduce_74

action_126 (149#) = happyShift action_43
action_126 (150#) = happyShift action_44
action_126 (151#) = happyShift action_45
action_126 (152#) = happyShift action_46
action_126 (153#) = happyShift action_47
action_126 (156#) = happyShift action_228
action_126 (31#) = happyGoto action_107
action_126 (32#) = happyGoto action_227
action_126 x = happyTcHack x happyReduce_82

action_127 (149#) = happyShift action_43
action_127 (150#) = happyShift action_44
action_127 (151#) = happyShift action_45
action_127 (152#) = happyShift action_46
action_127 (153#) = happyShift action_47
action_127 (156#) = happyShift action_226
action_127 (31#) = happyGoto action_107
action_127 (32#) = happyGoto action_225
action_127 x = happyTcHack x happyReduce_81

action_128 (181#) = happyShift action_224
action_128 x = happyTcHack x happyReduce_72

action_129 (181#) = happyShift action_223
action_129 x = happyTcHack x happyReduce_73

action_130 (181#) = happyShift action_222
action_130 x = happyTcHack x happyReduce_75

action_131 x = happyTcHack x happyReduce_89

action_132 x = happyTcHack x happyReduce_43

action_133 x = happyTcHack x happyReduce_44

action_134 x = happyTcHack x happyReduce_45

action_135 (122#) = happyShift action_221
action_135 x = happyTcHack x happyReduce_22

action_136 x = happyTcHack x happyReduce_347

action_137 (168#) = happyShift action_55
action_137 (94#) = happyGoto action_218
action_137 (95#) = happyGoto action_219
action_137 (96#) = happyGoto action_220
action_137 x = happyTcHack x happyReduce_303

action_138 (193#) = happyShift action_69
action_138 (109#) = happyGoto action_217
action_138 (110#) = happyGoto action_34
action_138 (111#) = happyGoto action_35
action_138 x = happyTcHack x happyReduce_344

action_139 (193#) = happyShift action_69
action_139 (109#) = happyGoto action_216
action_139 (110#) = happyGoto action_34
action_139 (111#) = happyGoto action_35
action_139 x = happyTcHack x happyReduce_344

action_140 (120#) = happyShift action_148
action_140 (124#) = happyShift action_104
action_140 (146#) = happyShift action_105
action_140 (172#) = happyShift action_106
action_140 (176#) = happyShift action_86
action_140 (181#) = happyShift action_150
action_140 (66#) = happyGoto action_214
action_140 (67#) = happyGoto action_142
action_140 (68#) = happyGoto action_215
action_140 (69#) = happyGoto action_144
action_140 (70#) = happyGoto action_145
action_140 (71#) = happyGoto action_146
action_140 (114#) = happyGoto action_102
action_140 x = happyTcHack x happyFail

action_141 (120#) = happyShift action_212
action_141 (168#) = happyShift action_213
action_141 x = happyTcHack x happyReduce_225

action_142 x = happyTcHack x happyReduce_228

action_143 x = happyTcHack x happyReduce_223

action_144 x = happyTcHack x happyReduce_231

action_145 x = happyTcHack x happyReduce_230

action_146 (120#) = happyShift action_148
action_146 (124#) = happyShift action_104
action_146 (146#) = happyShift action_105
action_146 (172#) = happyShift action_106
action_146 (175#) = happyShift action_211
action_146 (176#) = happyShift action_86
action_146 (193#) = happyShift action_69
action_146 (66#) = happyGoto action_209
action_146 (67#) = happyGoto action_142
action_146 (69#) = happyGoto action_144
action_146 (70#) = happyGoto action_145
action_146 (110#) = happyGoto action_210
action_146 (111#) = happyGoto action_35
action_146 (114#) = happyGoto action_102
action_146 x = happyTcHack x happyFail

action_147 (193#) = happyShift action_69
action_147 (111#) = happyGoto action_136
action_147 x = happyTcHack x happyReduce_226

action_148 (120#) = happyShift action_148
action_148 (124#) = happyShift action_104
action_148 (146#) = happyShift action_105
action_148 (172#) = happyShift action_106
action_148 (175#) = happyShift action_149
action_148 (176#) = happyShift action_86
action_148 (181#) = happyShift action_150
action_148 (193#) = happyShift action_69
action_148 (64#) = happyGoto action_208
action_148 (65#) = happyGoto action_140
action_148 (66#) = happyGoto action_141
action_148 (67#) = happyGoto action_142
action_148 (68#) = happyGoto action_143
action_148 (69#) = happyGoto action_144
action_148 (70#) = happyGoto action_145
action_148 (71#) = happyGoto action_146
action_148 (110#) = happyGoto action_147
action_148 (111#) = happyGoto action_35
action_148 (114#) = happyGoto action_102
action_148 x = happyTcHack x happyFail

action_149 x = happyTcHack x happyReduce_227

action_150 (128#) = happyShift action_157
action_150 (129#) = happyShift action_158
action_150 (181#) = happyShift action_150
action_150 (54#) = happyGoto action_205
action_150 (71#) = happyGoto action_206
action_150 (72#) = happyGoto action_207
action_150 x = happyTcHack x happyReduce_247

action_151 x = happyTcHack x happyReduce_19

action_152 x = happyTcHack x happyReduce_166

action_153 (1#) = happyShift action_122
action_153 (146#) = happyShift action_123
action_153 (147#) = happyShift action_124
action_153 (148#) = happyShift action_125
action_153 (149#) = happyShift action_43
action_153 (150#) = happyShift action_44
action_153 (151#) = happyShift action_45
action_153 (152#) = happyShift action_46
action_153 (153#) = happyShift action_47
action_153 (154#) = happyShift action_126
action_153 (155#) = happyShift action_127
action_153 (156#) = happyShift action_128
action_153 (157#) = happyShift action_129
action_153 (171#) = happyShift action_130
action_153 (176#) = happyShift action_131
action_153 (183#) = happyShift action_61
action_153 (184#) = happyShift action_62
action_153 (30#) = happyGoto action_204
action_153 (31#) = happyGoto action_119
action_153 (32#) = happyGoto action_120
action_153 (89#) = happyGoto action_121
action_153 (90#) = happyGoto action_29
action_153 x = happyTcHack x happyFail

action_154 (146#) = happyShift action_90
action_154 (176#) = happyShift action_86
action_154 (114#) = happyGoto action_203
action_154 x = happyTcHack x happyFail

action_155 (120#) = happyShift action_195
action_155 (141#) = happyShift action_196
action_155 (142#) = happyShift action_197
action_155 (170#) = happyShift action_198
action_155 (173#) = happyShift action_199
action_155 (174#) = happyShift action_200
action_155 (176#) = happyShift action_86
action_155 (180#) = happyShift action_201
action_155 (182#) = happyShift action_202
action_155 (44#) = happyGoto action_187
action_155 (45#) = happyGoto action_188
action_155 (46#) = happyGoto action_189
action_155 (47#) = happyGoto action_190
action_155 (48#) = happyGoto action_191
action_155 (49#) = happyGoto action_192
action_155 (50#) = happyGoto action_193
action_155 (114#) = happyGoto action_194
action_155 x = happyTcHack x happyFail

action_156 x = happyTcHack x happyReduce_178

action_157 x = happyTcHack x happyReduce_179

action_158 x = happyTcHack x happyReduce_180

action_159 (169#) = happyShift action_186
action_159 x = happyTcHack x happyFail

action_160 x = happyTcHack x happyReduce_11

action_161 x = happyTcHack x happyReduce_100

action_162 (153#) = happyShift action_185
action_162 x = happyTcHack x happyFail

action_163 (153#) = happyShift action_184
action_163 x = happyTcHack x happyFail

action_164 x = happyTcHack x happyReduce_17

action_165 x = happyTcHack x happyReduce_15

action_166 x = happyTcHack x happyReduce_356

action_167 x = happyTcHack x happyReduce_357

action_168 x = happyTcHack x happyReduce_14

action_169 (119#) = happyShift action_182
action_169 (186#) = happyShift action_183
action_169 x = happyTcHack x happyFail

action_170 (122#) = happyShift action_181
action_170 x = happyTcHack x happyFail

action_171 (146#) = happyShift action_180
action_171 (176#) = happyShift action_86
action_171 (114#) = happyGoto action_179
action_171 x = happyTcHack x happyFail

action_172 x = happyTcHack x happyReduce_13

action_173 x = happyTcHack x happyReduce_6

action_174 (124#) = happyShift action_177
action_174 (130#) = happyShift action_178
action_174 x = happyTcHack x happyFail

action_175 x = happyTcHack x happyReduce_10

action_176 x = happyTcHack x happyReduce_9

action_177 (168#) = happyShift action_55
action_177 (94#) = happyGoto action_391
action_177 (95#) = happyGoto action_219
action_177 (96#) = happyGoto action_220
action_177 x = happyTcHack x happyReduce_303

action_178 (168#) = happyShift action_55
action_178 (94#) = happyGoto action_390
action_178 (95#) = happyGoto action_219
action_178 (96#) = happyGoto action_220
action_178 x = happyTcHack x happyReduce_303

action_179 x = happyTcHack x happyReduce_62

action_180 x = happyTcHack x happyReduce_61

action_181 (22#) = happyGoto action_389
action_181 x = happyTcHack x happyReduce_51

action_182 (124#) = happyShift action_104
action_182 (146#) = happyShift action_105
action_182 (172#) = happyShift action_106
action_182 (176#) = happyShift action_86
action_182 (67#) = happyGoto action_388
action_182 (114#) = happyGoto action_102
action_182 x = happyTcHack x happyFail

action_183 (124#) = happyShift action_387
action_183 x = happyTcHack x happyFail

action_184 x = happyTcHack x happyReduce_192

action_185 x = happyTcHack x happyReduce_193

action_186 x = happyTcHack x happyReduce_177

action_187 (138#) = happyShift action_276
action_187 (166#) = happyShift action_386
action_187 x = happyTcHack x happyFail

action_188 (180#) = happyShift action_384
action_188 (182#) = happyShift action_385
action_188 x = happyTcHack x happyReduce_143

action_189 (139#) = happyShift action_381
action_189 (140#) = happyShift action_382
action_189 (181#) = happyShift action_383
action_189 x = happyTcHack x happyReduce_145

action_190 x = happyTcHack x happyReduce_148

action_191 x = happyTcHack x happyReduce_152

action_192 (120#) = happyShift action_380
action_192 (173#) = happyShift action_199
action_192 (174#) = happyShift action_200
action_192 (176#) = happyShift action_86
action_192 (50#) = happyGoto action_379
action_192 (114#) = happyGoto action_194
action_192 x = happyTcHack x happyFail

action_193 x = happyTcHack x happyReduce_154

action_194 x = happyTcHack x happyReduce_161

action_195 (1#) = happyShift action_369
action_195 (120#) = happyShift action_195
action_195 (141#) = happyShift action_196
action_195 (142#) = happyShift action_197
action_195 (146#) = happyShift action_370
action_195 (147#) = happyShift action_371
action_195 (148#) = happyShift action_372
action_195 (149#) = happyShift action_43
action_195 (150#) = happyShift action_44
action_195 (151#) = happyShift action_45
action_195 (152#) = happyShift action_46
action_195 (153#) = happyShift action_47
action_195 (154#) = happyShift action_373
action_195 (155#) = happyShift action_374
action_195 (156#) = happyShift action_375
action_195 (157#) = happyShift action_376
action_195 (170#) = happyShift action_198
action_195 (171#) = happyShift action_377
action_195 (173#) = happyShift action_199
action_195 (174#) = happyShift action_200
action_195 (176#) = happyShift action_378
action_195 (180#) = happyShift action_201
action_195 (182#) = happyShift action_202
action_195 (183#) = happyShift action_61
action_195 (184#) = happyShift action_62
action_195 (30#) = happyGoto action_363
action_195 (31#) = happyGoto action_364
action_195 (32#) = happyGoto action_365
action_195 (33#) = happyGoto action_366
action_195 (34#) = happyGoto action_367
action_195 (35#) = happyGoto action_265
action_195 (36#) = happyGoto action_266
action_195 (37#) = happyGoto action_267
action_195 (38#) = happyGoto action_268
action_195 (39#) = happyGoto action_269
action_195 (40#) = happyGoto action_270
action_195 (41#) = happyGoto action_271
action_195 (43#) = happyGoto action_272
action_195 (44#) = happyGoto action_273
action_195 (45#) = happyGoto action_188
action_195 (46#) = happyGoto action_189
action_195 (47#) = happyGoto action_190
action_195 (48#) = happyGoto action_191
action_195 (49#) = happyGoto action_192
action_195 (50#) = happyGoto action_193
action_195 (89#) = happyGoto action_368
action_195 (90#) = happyGoto action_29
action_195 (114#) = happyGoto action_194
action_195 x = happyTcHack x happyFail

action_196 x = happyTcHack x happyReduce_159

action_197 x = happyTcHack x happyReduce_160

action_198 (120#) = happyShift action_362
action_198 x = happyTcHack x happyFail

action_199 x = happyTcHack x happyReduce_162

action_200 x = happyTcHack x happyReduce_163

action_201 x = happyTcHack x happyReduce_158

action_202 x = happyTcHack x happyReduce_157

action_203 x = happyTcHack x happyReduce_46

action_204 (168#) = happyShift action_233
action_204 (176#) = happyShift action_86
action_204 (114#) = happyGoto action_361
action_204 x = happyTcHack x happyFail

action_205 x = happyTcHack x happyReduce_251

action_206 x = happyTcHack x happyReduce_249

action_207 (128#) = happyShift action_157
action_207 (129#) = happyShift action_158
action_207 (181#) = happyShift action_150
action_207 (54#) = happyGoto action_359
action_207 (71#) = happyGoto action_360
action_207 x = happyTcHack x happyReduce_248

action_208 (121#) = happyShift action_358
action_208 x = happyTcHack x happyFail

action_209 (120#) = happyShift action_212
action_209 (168#) = happyShift action_213
action_209 x = happyTcHack x happyReduce_238

action_210 (120#) = happyShift action_148
action_210 (124#) = happyShift action_104
action_210 (146#) = happyShift action_105
action_210 (172#) = happyShift action_106
action_210 (176#) = happyShift action_86
action_210 (193#) = happyShift action_69
action_210 (66#) = happyGoto action_357
action_210 (67#) = happyGoto action_142
action_210 (69#) = happyGoto action_144
action_210 (70#) = happyGoto action_145
action_210 (111#) = happyGoto action_136
action_210 (114#) = happyGoto action_102
action_210 x = happyTcHack x happyFail

action_211 (120#) = happyShift action_148
action_211 (124#) = happyShift action_104
action_211 (146#) = happyShift action_105
action_211 (172#) = happyShift action_106
action_211 (176#) = happyShift action_86
action_211 (66#) = happyGoto action_356
action_211 (67#) = happyGoto action_142
action_211 (69#) = happyGoto action_144
action_211 (70#) = happyGoto action_145
action_211 (114#) = happyGoto action_102
action_211 x = happyTcHack x happyFail

action_212 (121#) = happyShift action_354
action_212 (127#) = happyShift action_355
action_212 (168#) = happyShift action_55
action_212 (94#) = happyGoto action_351
action_212 (95#) = happyGoto action_219
action_212 (96#) = happyGoto action_220
action_212 (105#) = happyGoto action_352
action_212 (106#) = happyGoto action_353
action_212 x = happyTcHack x happyReduce_303

action_213 (120#) = happyShift action_195
action_213 (141#) = happyShift action_196
action_213 (142#) = happyShift action_197
action_213 (169#) = happyShift action_349
action_213 (170#) = happyShift action_198
action_213 (173#) = happyShift action_199
action_213 (174#) = happyShift action_200
action_213 (176#) = happyShift action_86
action_213 (180#) = happyShift action_201
action_213 (181#) = happyShift action_350
action_213 (182#) = happyShift action_202
action_213 (34#) = happyGoto action_347
action_213 (35#) = happyGoto action_265
action_213 (36#) = happyGoto action_266
action_213 (37#) = happyGoto action_267
action_213 (38#) = happyGoto action_268
action_213 (39#) = happyGoto action_269
action_213 (40#) = happyGoto action_270
action_213 (41#) = happyGoto action_271
action_213 (43#) = happyGoto action_272
action_213 (44#) = happyGoto action_273
action_213 (45#) = happyGoto action_188
action_213 (46#) = happyGoto action_189
action_213 (47#) = happyGoto action_190
action_213 (48#) = happyGoto action_191
action_213 (49#) = happyGoto action_192
action_213 (50#) = happyGoto action_348
action_213 (114#) = happyGoto action_194
action_213 x = happyTcHack x happyFail

action_214 (120#) = happyShift action_212
action_214 (168#) = happyShift action_213
action_214 x = happyTcHack x happyReduce_224

action_215 x = happyTcHack x happyReduce_222

action_216 x = happyTcHack x happyReduce_301

action_217 (1#) = happyShift action_36
action_217 (128#) = happyShift action_157
action_217 (129#) = happyShift action_158
action_217 (146#) = happyShift action_40
action_217 (147#) = happyShift action_41
action_217 (148#) = happyShift action_42
action_217 (149#) = happyShift action_43
action_217 (150#) = happyShift action_44
action_217 (151#) = happyShift action_45
action_217 (152#) = happyShift action_46
action_217 (153#) = happyShift action_47
action_217 (154#) = happyShift action_48
action_217 (155#) = happyShift action_49
action_217 (156#) = happyShift action_50
action_217 (157#) = happyShift action_51
action_217 (158#) = happyShift action_52
action_217 (159#) = happyShift action_53
action_217 (163#) = happyShift action_54
action_217 (171#) = happyShift action_56
action_217 (176#) = happyShift action_57
action_217 (197#) = happyShift action_73
action_217 (31#) = happyGoto action_18
action_217 (32#) = happyGoto action_19
action_217 (52#) = happyGoto action_346
action_217 (54#) = happyGoto action_333
action_217 (55#) = happyGoto action_334
action_217 (59#) = happyGoto action_76
action_217 (60#) = happyGoto action_25
action_217 (61#) = happyGoto action_26
action_217 (86#) = happyGoto action_77
action_217 x = happyTcHack x happyFail

action_218 (1#) = happyShift action_36
action_218 (128#) = happyShift action_157
action_218 (129#) = happyShift action_158
action_218 (146#) = happyShift action_40
action_218 (147#) = happyShift action_41
action_218 (148#) = happyShift action_42
action_218 (149#) = happyShift action_43
action_218 (150#) = happyShift action_44
action_218 (151#) = happyShift action_45
action_218 (152#) = happyShift action_46
action_218 (153#) = happyShift action_47
action_218 (154#) = happyShift action_48
action_218 (155#) = happyShift action_49
action_218 (156#) = happyShift action_50
action_218 (157#) = happyShift action_51
action_218 (158#) = happyShift action_52
action_218 (159#) = happyShift action_53
action_218 (163#) = happyShift action_54
action_218 (171#) = happyShift action_56
action_218 (176#) = happyShift action_57
action_218 (197#) = happyShift action_73
action_218 (31#) = happyGoto action_18
action_218 (32#) = happyGoto action_19
action_218 (52#) = happyGoto action_345
action_218 (54#) = happyGoto action_333
action_218 (55#) = happyGoto action_334
action_218 (59#) = happyGoto action_76
action_218 (60#) = happyGoto action_25
action_218 (61#) = happyGoto action_26
action_218 (86#) = happyGoto action_77
action_218 x = happyTcHack x happyFail

action_219 x = happyTcHack x happyReduce_302

action_220 (168#) = happyShift action_55
action_220 (95#) = happyGoto action_344
action_220 (96#) = happyGoto action_220
action_220 x = happyTcHack x happyReduce_303

action_221 (5#) = happyGoto action_343
action_221 x = happyTcHack x happyReduce_3

action_222 x = happyTcHack x happyReduce_76

action_223 x = happyTcHack x happyReduce_78

action_224 x = happyTcHack x happyReduce_77

action_225 x = happyTcHack x happyReduce_84

action_226 x = happyTcHack x happyReduce_88

action_227 x = happyTcHack x happyReduce_83

action_228 x = happyTcHack x happyReduce_85

action_229 x = happyTcHack x happyReduce_91

action_230 (153#) = happyShift action_342
action_230 x = happyTcHack x happyFail

action_231 (153#) = happyShift action_341
action_231 x = happyTcHack x happyFail

action_232 (130#) = happyShift action_340
action_232 x = happyTcHack x happyFail

action_233 (169#) = happyShift action_339
action_233 x = happyTcHack x happyFail

action_234 x = happyTcHack x happyReduce_191

action_235 x = happyTcHack x happyReduce_189

action_236 (173#) = happyShift action_338
action_236 x = happyTcHack x happyFail

action_237 x = happyTcHack x happyReduce_235

action_238 (123#) = happyShift action_337
action_238 (168#) = happyShift action_55
action_238 (74#) = happyGoto action_336
action_238 (94#) = happyGoto action_240
action_238 (95#) = happyGoto action_219
action_238 (96#) = happyGoto action_220
action_238 x = happyTcHack x happyReduce_303

action_239 (193#) = happyShift action_69
action_239 (109#) = happyGoto action_335
action_239 (110#) = happyGoto action_34
action_239 (111#) = happyGoto action_35
action_239 x = happyTcHack x happyReduce_344

action_240 (1#) = happyShift action_36
action_240 (128#) = happyShift action_157
action_240 (129#) = happyShift action_158
action_240 (146#) = happyShift action_40
action_240 (147#) = happyShift action_41
action_240 (148#) = happyShift action_42
action_240 (149#) = happyShift action_43
action_240 (150#) = happyShift action_44
action_240 (151#) = happyShift action_45
action_240 (152#) = happyShift action_46
action_240 (153#) = happyShift action_47
action_240 (154#) = happyShift action_48
action_240 (155#) = happyShift action_49
action_240 (156#) = happyShift action_50
action_240 (157#) = happyShift action_51
action_240 (158#) = happyShift action_52
action_240 (159#) = happyShift action_53
action_240 (163#) = happyShift action_54
action_240 (171#) = happyShift action_56
action_240 (176#) = happyShift action_57
action_240 (197#) = happyShift action_73
action_240 (31#) = happyGoto action_18
action_240 (32#) = happyGoto action_19
action_240 (52#) = happyGoto action_332
action_240 (54#) = happyGoto action_333
action_240 (55#) = happyGoto action_334
action_240 (59#) = happyGoto action_76
action_240 (60#) = happyGoto action_25
action_240 (61#) = happyGoto action_26
action_240 (86#) = happyGoto action_77
action_240 x = happyTcHack x happyFail

action_241 (173#) = happyShift action_331
action_241 x = happyTcHack x happyFail

action_242 (168#) = happyShift action_55
action_242 (73#) = happyGoto action_330
action_242 (74#) = happyGoto action_239
action_242 (94#) = happyGoto action_240
action_242 (95#) = happyGoto action_219
action_242 (96#) = happyGoto action_220
action_242 x = happyTcHack x happyReduce_303

action_243 x = happyTcHack x happyReduce_217

action_244 (161#) = happyShift action_327
action_244 (162#) = happyShift action_328
action_244 (168#) = happyShift action_329
action_244 (62#) = happyGoto action_321
action_244 (73#) = happyGoto action_322
action_244 (74#) = happyGoto action_239
action_244 (77#) = happyGoto action_323
action_244 (78#) = happyGoto action_324
action_244 (81#) = happyGoto action_325
action_244 (82#) = happyGoto action_326
action_244 (94#) = happyGoto action_240
action_244 (95#) = happyGoto action_219
action_244 (96#) = happyGoto action_220
action_244 x = happyTcHack x happyReduce_303

action_245 (120#) = happyShift action_320
action_245 x = happyTcHack x happyFail

action_246 (125#) = happyShift action_319
action_246 (116#) = happyGoto action_318
action_246 x = happyTcHack x happyReduce_358

action_247 x = happyTcHack x happyReduce_288

action_248 (176#) = happyShift action_86
action_248 (114#) = happyGoto action_317
action_248 x = happyTcHack x happyFail

action_249 (130#) = happyShift action_316
action_249 x = happyTcHack x happyReduce_290

action_250 (168#) = happyShift action_55
action_250 (176#) = happyShift action_86
action_250 (87#) = happyGoto action_315
action_250 (88#) = happyGoto action_247
action_250 (96#) = happyGoto action_248
action_250 (114#) = happyGoto action_249
action_250 x = happyTcHack x happyFail

action_251 x = happyTcHack x happyReduce_309

action_252 (120#) = happyShift action_195
action_252 (122#) = happyShift action_310
action_252 (141#) = happyShift action_196
action_252 (142#) = happyShift action_197
action_252 (146#) = happyShift action_311
action_252 (154#) = happyShift action_312
action_252 (155#) = happyShift action_313
action_252 (170#) = happyShift action_198
action_252 (173#) = happyShift action_199
action_252 (174#) = happyShift action_200
action_252 (176#) = happyShift action_86
action_252 (180#) = happyShift action_201
action_252 (181#) = happyShift action_314
action_252 (182#) = happyShift action_202
action_252 (34#) = happyGoto action_307
action_252 (35#) = happyGoto action_265
action_252 (36#) = happyGoto action_266
action_252 (37#) = happyGoto action_267
action_252 (38#) = happyGoto action_268
action_252 (39#) = happyGoto action_269
action_252 (40#) = happyGoto action_270
action_252 (41#) = happyGoto action_271
action_252 (43#) = happyGoto action_272
action_252 (44#) = happyGoto action_273
action_252 (45#) = happyGoto action_188
action_252 (46#) = happyGoto action_189
action_252 (47#) = happyGoto action_190
action_252 (48#) = happyGoto action_191
action_252 (49#) = happyGoto action_192
action_252 (50#) = happyGoto action_193
action_252 (102#) = happyGoto action_308
action_252 (103#) = happyGoto action_309
action_252 (114#) = happyGoto action_194
action_252 x = happyTcHack x happyReduce_321

action_253 (169#) = happyShift action_306
action_253 x = happyTcHack x happyFail

action_254 (172#) = happyShift action_95
action_254 (176#) = happyShift action_86
action_254 (183#) = happyShift action_96
action_254 (98#) = happyGoto action_305
action_254 (114#) = happyGoto action_93
action_254 x = happyTcHack x happyReduce_359

action_255 x = happyTcHack x happyReduce_12

action_256 (168#) = happyShift action_303
action_256 (14#) = happyGoto action_304
action_256 (15#) = happyGoto action_301
action_256 (16#) = happyGoto action_302
action_256 x = happyTcHack x happyReduce_39

action_257 (168#) = happyShift action_303
action_257 (14#) = happyGoto action_300
action_257 (15#) = happyGoto action_301
action_257 (16#) = happyGoto action_302
action_257 x = happyTcHack x happyReduce_39

action_258 (5#) = happyGoto action_299
action_258 x = happyTcHack x happyReduce_3

action_259 (121#) = happyShift action_298
action_259 x = happyTcHack x happyFail

action_260 (121#) = happyShift action_297
action_260 x = happyTcHack x happyFail

action_261 (121#) = happyShift action_296
action_261 x = happyTcHack x happyFail

action_262 (146#) = happyShift action_293
action_262 (175#) = happyShift action_294
action_262 (176#) = happyShift action_295
action_262 (112#) = happyGoto action_291
action_262 (113#) = happyGoto action_292
action_262 x = happyTcHack x happyFail

action_263 (174#) = happyShift action_80
action_263 (29#) = happyGoto action_290
action_263 x = happyTcHack x happyFail

action_264 x = happyTcHack x happyReduce_68

action_265 (143#) = happyShift action_289
action_265 x = happyTcHack x happyReduce_121

action_266 (134#) = happyShift action_288
action_266 x = happyTcHack x happyReduce_122

action_267 (137#) = happyShift action_287
action_267 x = happyTcHack x happyReduce_124

action_268 (133#) = happyShift action_286
action_268 x = happyTcHack x happyReduce_126

action_269 (135#) = happyShift action_285
action_269 x = happyTcHack x happyReduce_128

action_270 (136#) = happyShift action_284
action_270 x = happyTcHack x happyReduce_130

action_271 (131#) = happyShift action_282
action_271 (132#) = happyShift action_283
action_271 (42#) = happyGoto action_281
action_271 x = happyTcHack x happyReduce_132

action_272 (164#) = happyShift action_277
action_272 (165#) = happyShift action_278
action_272 (166#) = happyShift action_279
action_272 (167#) = happyShift action_280
action_272 x = happyTcHack x happyReduce_134

action_273 (138#) = happyShift action_276
action_273 x = happyTcHack x happyReduce_138

action_274 x = happyTcHack x happyReduce_197

action_275 x = happyTcHack x happyReduce_207

action_276 (120#) = happyShift action_195
action_276 (141#) = happyShift action_196
action_276 (142#) = happyShift action_197
action_276 (170#) = happyShift action_198
action_276 (173#) = happyShift action_199
action_276 (174#) = happyShift action_200
action_276 (176#) = happyShift action_86
action_276 (180#) = happyShift action_201
action_276 (182#) = happyShift action_202
action_276 (45#) = happyGoto action_492
action_276 (46#) = happyGoto action_189
action_276 (47#) = happyGoto action_190
action_276 (48#) = happyGoto action_191
action_276 (49#) = happyGoto action_192
action_276 (50#) = happyGoto action_193
action_276 (114#) = happyGoto action_194
action_276 x = happyTcHack x happyFail

action_277 (120#) = happyShift action_195
action_277 (141#) = happyShift action_196
action_277 (142#) = happyShift action_197
action_277 (170#) = happyShift action_198
action_277 (173#) = happyShift action_199
action_277 (174#) = happyShift action_200
action_277 (176#) = happyShift action_86
action_277 (180#) = happyShift action_201
action_277 (182#) = happyShift action_202
action_277 (44#) = happyGoto action_491
action_277 (45#) = happyGoto action_188
action_277 (46#) = happyGoto action_189
action_277 (47#) = happyGoto action_190
action_277 (48#) = happyGoto action_191
action_277 (49#) = happyGoto action_192
action_277 (50#) = happyGoto action_193
action_277 (114#) = happyGoto action_194
action_277 x = happyTcHack x happyFail

action_278 (120#) = happyShift action_195
action_278 (141#) = happyShift action_196
action_278 (142#) = happyShift action_197
action_278 (170#) = happyShift action_198
action_278 (173#) = happyShift action_199
action_278 (174#) = happyShift action_200
action_278 (176#) = happyShift action_86
action_278 (180#) = happyShift action_201
action_278 (182#) = happyShift action_202
action_278 (44#) = happyGoto action_490
action_278 (45#) = happyGoto action_188
action_278 (46#) = happyGoto action_189
action_278 (47#) = happyGoto action_190
action_278 (48#) = happyGoto action_191
action_278 (49#) = happyGoto action_192
action_278 (50#) = happyGoto action_193
action_278 (114#) = happyGoto action_194
action_278 x = happyTcHack x happyFail

action_279 (120#) = happyShift action_195
action_279 (141#) = happyShift action_196
action_279 (142#) = happyShift action_197
action_279 (170#) = happyShift action_198
action_279 (173#) = happyShift action_199
action_279 (174#) = happyShift action_200
action_279 (176#) = happyShift action_86
action_279 (180#) = happyShift action_201
action_279 (182#) = happyShift action_202
action_279 (44#) = happyGoto action_489
action_279 (45#) = happyGoto action_188
action_279 (46#) = happyGoto action_189
action_279 (47#) = happyGoto action_190
action_279 (48#) = happyGoto action_191
action_279 (49#) = happyGoto action_192
action_279 (50#) = happyGoto action_193
action_279 (114#) = happyGoto action_194
action_279 x = happyTcHack x happyFail

action_280 (120#) = happyShift action_195
action_280 (141#) = happyShift action_196
action_280 (142#) = happyShift action_197
action_280 (170#) = happyShift action_198
action_280 (173#) = happyShift action_199
action_280 (174#) = happyShift action_200
action_280 (176#) = happyShift action_86
action_280 (180#) = happyShift action_201
action_280 (182#) = happyShift action_202
action_280 (44#) = happyGoto action_488
action_280 (45#) = happyGoto action_188
action_280 (46#) = happyGoto action_189
action_280 (47#) = happyGoto action_190
action_280 (48#) = happyGoto action_191
action_280 (49#) = happyGoto action_192
action_280 (50#) = happyGoto action_193
action_280 (114#) = happyGoto action_194
action_280 x = happyTcHack x happyFail

action_281 (120#) = happyShift action_195
action_281 (141#) = happyShift action_196
action_281 (142#) = happyShift action_197
action_281 (170#) = happyShift action_198
action_281 (173#) = happyShift action_199
action_281 (174#) = happyShift action_200
action_281 (176#) = happyShift action_86
action_281 (180#) = happyShift action_201
action_281 (182#) = happyShift action_202
action_281 (43#) = happyGoto action_487
action_281 (44#) = happyGoto action_273
action_281 (45#) = happyGoto action_188
action_281 (46#) = happyGoto action_189
action_281 (47#) = happyGoto action_190
action_281 (48#) = happyGoto action_191
action_281 (49#) = happyGoto action_192
action_281 (50#) = happyGoto action_193
action_281 (114#) = happyGoto action_194
action_281 x = happyTcHack x happyFail

action_282 x = happyTcHack x happyReduce_136

action_283 x = happyTcHack x happyReduce_137

action_284 (120#) = happyShift action_195
action_284 (141#) = happyShift action_196
action_284 (142#) = happyShift action_197
action_284 (170#) = happyShift action_198
action_284 (173#) = happyShift action_199
action_284 (174#) = happyShift action_200
action_284 (176#) = happyShift action_86
action_284 (180#) = happyShift action_201
action_284 (182#) = happyShift action_202
action_284 (41#) = happyGoto action_486
action_284 (43#) = happyGoto action_272
action_284 (44#) = happyGoto action_273
action_284 (45#) = happyGoto action_188
action_284 (46#) = happyGoto action_189
action_284 (47#) = happyGoto action_190
action_284 (48#) = happyGoto action_191
action_284 (49#) = happyGoto action_192
action_284 (50#) = happyGoto action_193
action_284 (114#) = happyGoto action_194
action_284 x = happyTcHack x happyFail

action_285 (120#) = happyShift action_195
action_285 (141#) = happyShift action_196
action_285 (142#) = happyShift action_197
action_285 (170#) = happyShift action_198
action_285 (173#) = happyShift action_199
action_285 (174#) = happyShift action_200
action_285 (176#) = happyShift action_86
action_285 (180#) = happyShift action_201
action_285 (182#) = happyShift action_202
action_285 (40#) = happyGoto action_485
action_285 (41#) = happyGoto action_271
action_285 (43#) = happyGoto action_272
action_285 (44#) = happyGoto action_273
action_285 (45#) = happyGoto action_188
action_285 (46#) = happyGoto action_189
action_285 (47#) = happyGoto action_190
action_285 (48#) = happyGoto action_191
action_285 (49#) = happyGoto action_192
action_285 (50#) = happyGoto action_193
action_285 (114#) = happyGoto action_194
action_285 x = happyTcHack x happyFail

action_286 (120#) = happyShift action_195
action_286 (141#) = happyShift action_196
action_286 (142#) = happyShift action_197
action_286 (170#) = happyShift action_198
action_286 (173#) = happyShift action_199
action_286 (174#) = happyShift action_200
action_286 (176#) = happyShift action_86
action_286 (180#) = happyShift action_201
action_286 (182#) = happyShift action_202
action_286 (39#) = happyGoto action_484
action_286 (40#) = happyGoto action_270
action_286 (41#) = happyGoto action_271
action_286 (43#) = happyGoto action_272
action_286 (44#) = happyGoto action_273
action_286 (45#) = happyGoto action_188
action_286 (46#) = happyGoto action_189
action_286 (47#) = happyGoto action_190
action_286 (48#) = happyGoto action_191
action_286 (49#) = happyGoto action_192
action_286 (50#) = happyGoto action_193
action_286 (114#) = happyGoto action_194
action_286 x = happyTcHack x happyFail

action_287 (120#) = happyShift action_195
action_287 (141#) = happyShift action_196
action_287 (142#) = happyShift action_197
action_287 (170#) = happyShift action_198
action_287 (173#) = happyShift action_199
action_287 (174#) = happyShift action_200
action_287 (176#) = happyShift action_86
action_287 (180#) = happyShift action_201
action_287 (182#) = happyShift action_202
action_287 (38#) = happyGoto action_483
action_287 (39#) = happyGoto action_269
action_287 (40#) = happyGoto action_270
action_287 (41#) = happyGoto action_271
action_287 (43#) = happyGoto action_272
action_287 (44#) = happyGoto action_273
action_287 (45#) = happyGoto action_188
action_287 (46#) = happyGoto action_189
action_287 (47#) = happyGoto action_190
action_287 (48#) = happyGoto action_191
action_287 (49#) = happyGoto action_192
action_287 (50#) = happyGoto action_193
action_287 (114#) = happyGoto action_194
action_287 x = happyTcHack x happyFail

action_288 (120#) = happyShift action_195
action_288 (141#) = happyShift action_196
action_288 (142#) = happyShift action_197
action_288 (170#) = happyShift action_198
action_288 (173#) = happyShift action_199
action_288 (174#) = happyShift action_200
action_288 (176#) = happyShift action_86
action_288 (180#) = happyShift action_201
action_288 (182#) = happyShift action_202
action_288 (37#) = happyGoto action_482
action_288 (38#) = happyGoto action_268
action_288 (39#) = happyGoto action_269
action_288 (40#) = happyGoto action_270
action_288 (41#) = happyGoto action_271
action_288 (43#) = happyGoto action_272
action_288 (44#) = happyGoto action_273
action_288 (45#) = happyGoto action_188
action_288 (46#) = happyGoto action_189
action_288 (47#) = happyGoto action_190
action_288 (48#) = happyGoto action_191
action_288 (49#) = happyGoto action_192
action_288 (50#) = happyGoto action_193
action_288 (114#) = happyGoto action_194
action_288 x = happyTcHack x happyFail

action_289 (120#) = happyShift action_195
action_289 (141#) = happyShift action_196
action_289 (142#) = happyShift action_197
action_289 (170#) = happyShift action_198
action_289 (173#) = happyShift action_199
action_289 (174#) = happyShift action_200
action_289 (176#) = happyShift action_86
action_289 (180#) = happyShift action_201
action_289 (182#) = happyShift action_202
action_289 (34#) = happyGoto action_481
action_289 (35#) = happyGoto action_265
action_289 (36#) = happyGoto action_266
action_289 (37#) = happyGoto action_267
action_289 (38#) = happyGoto action_268
action_289 (39#) = happyGoto action_269
action_289 (40#) = happyGoto action_270
action_289 (41#) = happyGoto action_271
action_289 (43#) = happyGoto action_272
action_289 (44#) = happyGoto action_273
action_289 (45#) = happyGoto action_188
action_289 (46#) = happyGoto action_189
action_289 (47#) = happyGoto action_190
action_289 (48#) = happyGoto action_191
action_289 (49#) = happyGoto action_192
action_289 (50#) = happyGoto action_193
action_289 (114#) = happyGoto action_194
action_289 x = happyTcHack x happyFail

action_290 x = happyTcHack x happyReduce_70

action_291 (121#) = happyShift action_480
action_291 x = happyTcHack x happyFail

action_292 (120#) = happyShift action_479
action_292 x = happyTcHack x happyReduce_349

action_293 x = happyTcHack x happyReduce_354

action_294 x = happyTcHack x happyReduce_350

action_295 x = happyTcHack x happyReduce_353

action_296 x = happyTcHack x happyReduce_64

action_297 x = happyTcHack x happyReduce_49

action_298 x = happyTcHack x happyReduce_48

action_299 (1#) = happyShift action_36
action_299 (118#) = happyShift action_37
action_299 (119#) = happyShift action_38
action_299 (123#) = happyShift action_478
action_299 (128#) = happyShift action_39
action_299 (144#) = happyReduce_344
action_299 (145#) = happyReduce_344
action_299 (146#) = happyShift action_40
action_299 (147#) = happyShift action_41
action_299 (148#) = happyShift action_42
action_299 (149#) = happyShift action_43
action_299 (150#) = happyShift action_44
action_299 (151#) = happyShift action_45
action_299 (152#) = happyShift action_46
action_299 (153#) = happyShift action_47
action_299 (154#) = happyShift action_48
action_299 (155#) = happyShift action_49
action_299 (156#) = happyShift action_50
action_299 (157#) = happyShift action_51
action_299 (158#) = happyShift action_52
action_299 (159#) = happyShift action_53
action_299 (163#) = happyShift action_54
action_299 (168#) = happyShift action_55
action_299 (171#) = happyShift action_56
action_299 (176#) = happyShift action_57
action_299 (177#) = happyShift action_58
action_299 (178#) = happyShift action_59
action_299 (179#) = happyShift action_60
action_299 (183#) = happyShift action_61
action_299 (184#) = happyShift action_62
action_299 (187#) = happyShift action_63
action_299 (188#) = happyShift action_64
action_299 (189#) = happyShift action_65
action_299 (190#) = happyShift action_66
action_299 (191#) = happyShift action_67
action_299 (192#) = happyShift action_68
action_299 (193#) = happyShift action_69
action_299 (194#) = happyShift action_70
action_299 (195#) = happyShift action_71
action_299 (196#) = happyShift action_72
action_299 (197#) = happyShift action_73
action_299 (9#) = happyGoto action_6
action_299 (10#) = happyGoto action_7
action_299 (11#) = happyGoto action_8
action_299 (12#) = happyGoto action_9
action_299 (17#) = happyGoto action_10
action_299 (18#) = happyGoto action_11
action_299 (19#) = happyGoto action_12
action_299 (20#) = happyGoto action_13
action_299 (21#) = happyGoto action_14
action_299 (26#) = happyGoto action_15
action_299 (27#) = happyGoto action_16
action_299 (28#) = happyGoto action_17
action_299 (31#) = happyGoto action_18
action_299 (32#) = happyGoto action_19
action_299 (51#) = happyGoto action_20
action_299 (53#) = happyGoto action_21
action_299 (55#) = happyGoto action_22
action_299 (58#) = happyGoto action_23
action_299 (59#) = happyGoto action_24
action_299 (60#) = happyGoto action_25
action_299 (61#) = happyGoto action_26
action_299 (86#) = happyGoto action_27
action_299 (89#) = happyGoto action_28
action_299 (90#) = happyGoto action_29
action_299 (93#) = happyGoto action_30
action_299 (96#) = happyGoto action_31
action_299 (104#) = happyGoto action_32
action_299 (109#) = happyGoto action_33
action_299 (110#) = happyGoto action_34
action_299 (111#) = happyGoto action_35
action_299 x = happyTcHack x happyFail

action_300 (117#) = happyShift action_477
action_300 x = happyTcHack x happyFail

action_301 x = happyTcHack x happyReduce_33

action_302 (119#) = happyShift action_475
action_302 (177#) = happyShift action_476
action_302 x = happyTcHack x happyFail

action_303 (162#) = happyShift action_474
action_303 (176#) = happyShift action_86
action_303 (99#) = happyGoto action_471
action_303 (100#) = happyGoto action_472
action_303 (114#) = happyGoto action_473
action_303 x = happyTcHack x happyFail

action_304 (117#) = happyShift action_470
action_304 x = happyTcHack x happyFail

action_305 x = happyTcHack x happyReduce_308

action_306 x = happyTcHack x happyReduce_306

action_307 x = happyTcHack x happyReduce_320

action_308 (121#) = happyShift action_468
action_308 (125#) = happyShift action_469
action_308 x = happyTcHack x happyFail

action_309 x = happyTcHack x happyReduce_318

action_310 (173#) = happyShift action_467
action_310 x = happyTcHack x happyFail

action_311 x = happyTcHack x happyReduce_322

action_312 (146#) = happyShift action_466
action_312 x = happyTcHack x happyFail

action_313 (146#) = happyShift action_465
action_313 x = happyTcHack x happyFail

action_314 (120#) = happyShift action_195
action_314 (122#) = happyShift action_310
action_314 (141#) = happyShift action_196
action_314 (142#) = happyShift action_197
action_314 (146#) = happyShift action_311
action_314 (154#) = happyShift action_312
action_314 (155#) = happyShift action_313
action_314 (170#) = happyShift action_198
action_314 (173#) = happyShift action_199
action_314 (174#) = happyShift action_200
action_314 (176#) = happyShift action_86
action_314 (180#) = happyShift action_201
action_314 (181#) = happyShift action_314
action_314 (182#) = happyShift action_202
action_314 (34#) = happyGoto action_307
action_314 (35#) = happyGoto action_265
action_314 (36#) = happyGoto action_266
action_314 (37#) = happyGoto action_267
action_314 (38#) = happyGoto action_268
action_314 (39#) = happyGoto action_269
action_314 (40#) = happyGoto action_270
action_314 (41#) = happyGoto action_271
action_314 (43#) = happyGoto action_272
action_314 (44#) = happyGoto action_273
action_314 (45#) = happyGoto action_188
action_314 (46#) = happyGoto action_189
action_314 (47#) = happyGoto action_190
action_314 (48#) = happyGoto action_191
action_314 (49#) = happyGoto action_192
action_314 (50#) = happyGoto action_193
action_314 (103#) = happyGoto action_464
action_314 (114#) = happyGoto action_194
action_314 x = happyTcHack x happyReduce_321

action_315 (125#) = happyShift action_319
action_315 (116#) = happyGoto action_463
action_315 x = happyTcHack x happyReduce_358

action_316 (120#) = happyShift action_195
action_316 (141#) = happyShift action_196
action_316 (142#) = happyShift action_197
action_316 (170#) = happyShift action_198
action_316 (173#) = happyShift action_199
action_316 (174#) = happyShift action_200
action_316 (176#) = happyShift action_86
action_316 (180#) = happyShift action_201
action_316 (182#) = happyShift action_202
action_316 (34#) = happyGoto action_462
action_316 (35#) = happyGoto action_265
action_316 (36#) = happyGoto action_266
action_316 (37#) = happyGoto action_267
action_316 (38#) = happyGoto action_268
action_316 (39#) = happyGoto action_269
action_316 (40#) = happyGoto action_270
action_316 (41#) = happyGoto action_271
action_316 (43#) = happyGoto action_272
action_316 (44#) = happyGoto action_273
action_316 (45#) = happyGoto action_188
action_316 (46#) = happyGoto action_189
action_316 (47#) = happyGoto action_190
action_316 (48#) = happyGoto action_191
action_316 (49#) = happyGoto action_192
action_316 (50#) = happyGoto action_193
action_316 (114#) = happyGoto action_194
action_316 x = happyTcHack x happyFail

action_317 (130#) = happyShift action_461
action_317 x = happyTcHack x happyReduce_291

action_318 (123#) = happyShift action_460
action_318 x = happyTcHack x happyFail

action_319 (168#) = happyShift action_55
action_319 (176#) = happyShift action_86
action_319 (88#) = happyGoto action_459
action_319 (96#) = happyGoto action_248
action_319 (114#) = happyGoto action_249
action_319 x = happyTcHack x happyReduce_359

action_320 (146#) = happyShift action_456
action_320 (149#) = happyShift action_43
action_320 (150#) = happyShift action_44
action_320 (151#) = happyShift action_45
action_320 (152#) = happyShift action_46
action_320 (153#) = happyShift action_47
action_320 (156#) = happyShift action_457
action_320 (158#) = happyShift action_52
action_320 (159#) = happyShift action_53
action_320 (163#) = happyShift action_54
action_320 (176#) = happyShift action_458
action_320 (31#) = happyGoto action_107
action_320 (32#) = happyGoto action_452
action_320 (59#) = happyGoto action_453
action_320 (60#) = happyGoto action_25
action_320 (61#) = happyGoto action_26
action_320 (76#) = happyGoto action_454
action_320 (86#) = happyGoto action_455
action_320 x = happyTcHack x happyFail

action_321 (123#) = happyShift action_451
action_321 x = happyTcHack x happyFail

action_322 (123#) = happyReduce_219
action_322 (168#) = happyShift action_55
action_322 (74#) = happyGoto action_336
action_322 (94#) = happyGoto action_240
action_322 (95#) = happyGoto action_219
action_322 (96#) = happyGoto action_220
action_322 x = happyTcHack x happyReduce_303

action_323 (161#) = happyShift action_327
action_323 (162#) = happyShift action_328
action_323 (168#) = happyShift action_450
action_323 (78#) = happyGoto action_449
action_323 (81#) = happyGoto action_325
action_323 (82#) = happyGoto action_326
action_323 x = happyTcHack x happyReduce_218

action_324 (117#) = happyShift action_448
action_324 x = happyTcHack x happyFail

action_325 (117#) = happyReduce_272
action_325 (161#) = happyShift action_327
action_325 (162#) = happyShift action_328
action_325 (168#) = happyShift action_55
action_325 (80#) = happyGoto action_445
action_325 (82#) = happyGoto action_446
action_325 (94#) = happyGoto action_447
action_325 (95#) = happyGoto action_219
action_325 (96#) = happyGoto action_220
action_325 x = happyTcHack x happyReduce_303

action_326 x = happyTcHack x happyReduce_275

action_327 (120#) = happyShift action_195
action_327 (141#) = happyShift action_196
action_327 (142#) = happyShift action_197
action_327 (170#) = happyShift action_198
action_327 (173#) = happyShift action_199
action_327 (174#) = happyShift action_200
action_327 (176#) = happyShift action_86
action_327 (180#) = happyShift action_201
action_327 (182#) = happyShift action_202
action_327 (34#) = happyGoto action_444
action_327 (35#) = happyGoto action_265
action_327 (36#) = happyGoto action_266
action_327 (37#) = happyGoto action_267
action_327 (38#) = happyGoto action_268
action_327 (39#) = happyGoto action_269
action_327 (40#) = happyGoto action_270
action_327 (41#) = happyGoto action_271
action_327 (43#) = happyGoto action_272
action_327 (44#) = happyGoto action_273
action_327 (45#) = happyGoto action_188
action_327 (46#) = happyGoto action_189
action_327 (47#) = happyGoto action_190
action_327 (48#) = happyGoto action_191
action_327 (49#) = happyGoto action_192
action_327 (50#) = happyGoto action_193
action_327 (114#) = happyGoto action_194
action_327 x = happyTcHack x happyFail

action_328 (124#) = happyShift action_443
action_328 x = happyTcHack x happyFail

action_329 (161#) = happyShift action_441
action_329 (162#) = happyShift action_442
action_329 (169#) = happyShift action_94
action_329 (172#) = happyShift action_95
action_329 (176#) = happyShift action_86
action_329 (183#) = happyShift action_96
action_329 (83#) = happyGoto action_440
action_329 (97#) = happyGoto action_91
action_329 (98#) = happyGoto action_92
action_329 (114#) = happyGoto action_93
action_329 x = happyTcHack x happyFail

action_330 (123#) = happyShift action_439
action_330 (168#) = happyShift action_55
action_330 (74#) = happyGoto action_336
action_330 (94#) = happyGoto action_240
action_330 (95#) = happyGoto action_219
action_330 (96#) = happyGoto action_220
action_330 x = happyTcHack x happyReduce_303

action_331 x = happyTcHack x happyReduce_233

action_332 (120#) = happyShift action_148
action_332 (124#) = happyShift action_104
action_332 (146#) = happyShift action_105
action_332 (168#) = happyShift action_428
action_332 (172#) = happyShift action_106
action_332 (175#) = happyShift action_149
action_332 (176#) = happyShift action_86
action_332 (181#) = happyShift action_150
action_332 (193#) = happyShift action_69
action_332 (63#) = happyGoto action_438
action_332 (64#) = happyGoto action_427
action_332 (65#) = happyGoto action_140
action_332 (66#) = happyGoto action_141
action_332 (67#) = happyGoto action_142
action_332 (68#) = happyGoto action_143
action_332 (69#) = happyGoto action_144
action_332 (70#) = happyGoto action_145
action_332 (71#) = happyGoto action_146
action_332 (110#) = happyGoto action_147
action_332 (111#) = happyGoto action_35
action_332 (114#) = happyGoto action_102
action_332 x = happyTcHack x happyReduce_256

action_333 (1#) = happyShift action_36
action_333 (128#) = happyShift action_157
action_333 (129#) = happyShift action_158
action_333 (146#) = happyShift action_40
action_333 (147#) = happyShift action_41
action_333 (148#) = happyShift action_42
action_333 (149#) = happyShift action_43
action_333 (150#) = happyShift action_44
action_333 (151#) = happyShift action_45
action_333 (152#) = happyShift action_46
action_333 (153#) = happyShift action_47
action_333 (154#) = happyShift action_48
action_333 (155#) = happyShift action_49
action_333 (156#) = happyShift action_50
action_333 (157#) = happyShift action_51
action_333 (158#) = happyShift action_52
action_333 (159#) = happyShift action_53
action_333 (163#) = happyShift action_54
action_333 (171#) = happyShift action_56
action_333 (176#) = happyShift action_57
action_333 (197#) = happyShift action_73
action_333 (31#) = happyGoto action_18
action_333 (32#) = happyGoto action_19
action_333 (52#) = happyGoto action_437
action_333 (54#) = happyGoto action_333
action_333 (55#) = happyGoto action_334
action_333 (59#) = happyGoto action_76
action_333 (60#) = happyGoto action_25
action_333 (61#) = happyGoto action_26
action_333 (86#) = happyGoto action_77
action_333 x = happyTcHack x happyFail

action_334 (128#) = happyShift action_157
action_334 (129#) = happyShift action_158
action_334 (54#) = happyGoto action_436
action_334 x = happyTcHack x happyReduce_171

action_335 (117#) = happyShift action_435
action_335 x = happyTcHack x happyFail

action_336 (193#) = happyShift action_69
action_336 (109#) = happyGoto action_434
action_336 (110#) = happyGoto action_34
action_336 (111#) = happyGoto action_35
action_336 x = happyTcHack x happyReduce_344

action_337 (193#) = happyShift action_69
action_337 (109#) = happyGoto action_433
action_337 (110#) = happyGoto action_34
action_337 (111#) = happyGoto action_35
action_337 x = happyTcHack x happyReduce_344

action_338 x = happyTcHack x happyReduce_234

action_339 x = happyTcHack x happyReduce_80

action_340 (120#) = happyShift action_195
action_340 (141#) = happyShift action_196
action_340 (142#) = happyShift action_197
action_340 (170#) = happyShift action_198
action_340 (173#) = happyShift action_199
action_340 (174#) = happyShift action_200
action_340 (176#) = happyShift action_86
action_340 (180#) = happyShift action_201
action_340 (182#) = happyShift action_202
action_340 (34#) = happyGoto action_432
action_340 (35#) = happyGoto action_265
action_340 (36#) = happyGoto action_266
action_340 (37#) = happyGoto action_267
action_340 (38#) = happyGoto action_268
action_340 (39#) = happyGoto action_269
action_340 (40#) = happyGoto action_270
action_340 (41#) = happyGoto action_271
action_340 (43#) = happyGoto action_272
action_340 (44#) = happyGoto action_273
action_340 (45#) = happyGoto action_188
action_340 (46#) = happyGoto action_189
action_340 (47#) = happyGoto action_190
action_340 (48#) = happyGoto action_191
action_340 (49#) = happyGoto action_192
action_340 (50#) = happyGoto action_193
action_340 (114#) = happyGoto action_194
action_340 x = happyTcHack x happyFail

action_341 x = happyTcHack x happyReduce_86

action_342 x = happyTcHack x happyReduce_87

action_343 (1#) = happyShift action_431
action_343 (117#) = happyShift action_167
action_343 (118#) = happyShift action_37
action_343 (119#) = happyShift action_38
action_343 (128#) = happyShift action_39
action_343 (144#) = happyReduce_344
action_343 (145#) = happyReduce_344
action_343 (146#) = happyShift action_40
action_343 (147#) = happyShift action_41
action_343 (148#) = happyShift action_42
action_343 (149#) = happyShift action_43
action_343 (150#) = happyShift action_44
action_343 (151#) = happyShift action_45
action_343 (152#) = happyShift action_46
action_343 (153#) = happyShift action_47
action_343 (154#) = happyShift action_48
action_343 (155#) = happyShift action_49
action_343 (156#) = happyShift action_50
action_343 (157#) = happyShift action_51
action_343 (158#) = happyShift action_52
action_343 (159#) = happyShift action_53
action_343 (163#) = happyShift action_54
action_343 (168#) = happyShift action_55
action_343 (171#) = happyShift action_56
action_343 (176#) = happyShift action_57
action_343 (177#) = happyShift action_58
action_343 (178#) = happyShift action_59
action_343 (179#) = happyShift action_60
action_343 (183#) = happyShift action_61
action_343 (184#) = happyShift action_62
action_343 (187#) = happyShift action_63
action_343 (188#) = happyShift action_64
action_343 (189#) = happyShift action_65
action_343 (190#) = happyShift action_66
action_343 (191#) = happyShift action_67
action_343 (192#) = happyShift action_68
action_343 (193#) = happyShift action_69
action_343 (194#) = happyShift action_70
action_343 (195#) = happyShift action_71
action_343 (196#) = happyShift action_72
action_343 (197#) = happyShift action_73
action_343 (9#) = happyGoto action_6
action_343 (10#) = happyGoto action_7
action_343 (11#) = happyGoto action_8
action_343 (12#) = happyGoto action_9
action_343 (17#) = happyGoto action_10
action_343 (18#) = happyGoto action_11
action_343 (19#) = happyGoto action_12
action_343 (20#) = happyGoto action_13
action_343 (21#) = happyGoto action_14
action_343 (26#) = happyGoto action_15
action_343 (27#) = happyGoto action_16
action_343 (28#) = happyGoto action_17
action_343 (31#) = happyGoto action_18
action_343 (32#) = happyGoto action_19
action_343 (51#) = happyGoto action_20
action_343 (53#) = happyGoto action_21
action_343 (55#) = happyGoto action_22
action_343 (58#) = happyGoto action_23
action_343 (59#) = happyGoto action_24
action_343 (60#) = happyGoto action_25
action_343 (61#) = happyGoto action_26
action_343 (86#) = happyGoto action_27
action_343 (89#) = happyGoto action_28
action_343 (90#) = happyGoto action_29
action_343 (93#) = happyGoto action_30
action_343 (96#) = happyGoto action_31
action_343 (104#) = happyGoto action_32
action_343 (109#) = happyGoto action_33
action_343 (110#) = happyGoto action_34
action_343 (111#) = happyGoto action_35
action_343 (115#) = happyGoto action_430
action_343 x = happyTcHack x happyFail

action_344 x = happyTcHack x happyReduce_304

action_345 (120#) = happyShift action_148
action_345 (124#) = happyShift action_104
action_345 (146#) = happyShift action_105
action_345 (168#) = happyShift action_428
action_345 (172#) = happyShift action_106
action_345 (175#) = happyShift action_149
action_345 (176#) = happyShift action_86
action_345 (181#) = happyShift action_150
action_345 (193#) = happyShift action_69
action_345 (63#) = happyGoto action_429
action_345 (64#) = happyGoto action_427
action_345 (65#) = happyGoto action_140
action_345 (66#) = happyGoto action_141
action_345 (67#) = happyGoto action_142
action_345 (68#) = happyGoto action_143
action_345 (69#) = happyGoto action_144
action_345 (70#) = happyGoto action_145
action_345 (71#) = happyGoto action_146
action_345 (110#) = happyGoto action_147
action_345 (111#) = happyGoto action_35
action_345 (114#) = happyGoto action_102
action_345 x = happyTcHack x happyFail

action_346 (120#) = happyShift action_148
action_346 (124#) = happyShift action_104
action_346 (146#) = happyShift action_105
action_346 (168#) = happyShift action_428
action_346 (172#) = happyShift action_106
action_346 (175#) = happyShift action_149
action_346 (176#) = happyShift action_86
action_346 (181#) = happyShift action_150
action_346 (193#) = happyShift action_69
action_346 (63#) = happyGoto action_426
action_346 (64#) = happyGoto action_427
action_346 (65#) = happyGoto action_140
action_346 (66#) = happyGoto action_141
action_346 (67#) = happyGoto action_142
action_346 (68#) = happyGoto action_143
action_346 (69#) = happyGoto action_144
action_346 (70#) = happyGoto action_145
action_346 (71#) = happyGoto action_146
action_346 (110#) = happyGoto action_147
action_346 (111#) = happyGoto action_35
action_346 (114#) = happyGoto action_102
action_346 x = happyTcHack x happyFail

action_347 (169#) = happyShift action_425
action_347 x = happyTcHack x happyFail

action_348 (126#) = happyShift action_424
action_348 x = happyTcHack x happyReduce_154

action_349 x = happyTcHack x happyReduce_241

action_350 (169#) = happyShift action_423
action_350 x = happyTcHack x happyFail

action_351 (1#) = happyShift action_36
action_351 (128#) = happyShift action_157
action_351 (129#) = happyShift action_158
action_351 (146#) = happyShift action_40
action_351 (147#) = happyShift action_41
action_351 (148#) = happyShift action_42
action_351 (149#) = happyShift action_43
action_351 (150#) = happyShift action_44
action_351 (151#) = happyShift action_45
action_351 (152#) = happyShift action_46
action_351 (153#) = happyShift action_47
action_351 (154#) = happyShift action_48
action_351 (155#) = happyShift action_49
action_351 (156#) = happyShift action_50
action_351 (157#) = happyShift action_51
action_351 (158#) = happyShift action_52
action_351 (159#) = happyShift action_53
action_351 (163#) = happyShift action_54
action_351 (171#) = happyShift action_56
action_351 (176#) = happyShift action_57
action_351 (197#) = happyShift action_73
action_351 (31#) = happyGoto action_18
action_351 (32#) = happyGoto action_19
action_351 (52#) = happyGoto action_422
action_351 (54#) = happyGoto action_333
action_351 (55#) = happyGoto action_334
action_351 (59#) = happyGoto action_76
action_351 (60#) = happyGoto action_25
action_351 (61#) = happyGoto action_26
action_351 (86#) = happyGoto action_77
action_351 x = happyTcHack x happyFail

action_352 x = happyTcHack x happyReduce_333

action_353 (121#) = happyShift action_420
action_353 (125#) = happyShift action_421
action_353 x = happyTcHack x happyFail

action_354 x = happyTcHack x happyReduce_246

action_355 x = happyTcHack x happyReduce_332

action_356 (120#) = happyShift action_212
action_356 (168#) = happyShift action_213
action_356 x = happyTcHack x happyReduce_239

action_357 (120#) = happyShift action_212
action_357 (168#) = happyShift action_213
action_357 x = happyTcHack x happyReduce_240

action_358 x = happyTcHack x happyReduce_229

action_359 x = happyTcHack x happyReduce_252

action_360 x = happyTcHack x happyReduce_250

action_361 (130#) = happyShift action_419
action_361 x = happyTcHack x happyFail

action_362 (1#) = happyShift action_122
action_362 (146#) = happyShift action_123
action_362 (147#) = happyShift action_124
action_362 (148#) = happyShift action_125
action_362 (149#) = happyShift action_43
action_362 (150#) = happyShift action_44
action_362 (151#) = happyShift action_45
action_362 (152#) = happyShift action_46
action_362 (153#) = happyShift action_47
action_362 (154#) = happyShift action_126
action_362 (155#) = happyShift action_127
action_362 (156#) = happyShift action_128
action_362 (157#) = happyShift action_129
action_362 (171#) = happyShift action_130
action_362 (176#) = happyShift action_131
action_362 (183#) = happyShift action_61
action_362 (184#) = happyShift action_62
action_362 (30#) = happyGoto action_418
action_362 (31#) = happyGoto action_119
action_362 (32#) = happyGoto action_120
action_362 (89#) = happyGoto action_121
action_362 (90#) = happyGoto action_29
action_362 x = happyTcHack x happyFail

action_363 (168#) = happyShift action_417
action_363 x = happyTcHack x happyFail

action_364 (153#) = happyShift action_161
action_364 (154#) = happyShift action_415
action_364 (155#) = happyShift action_416
action_364 x = happyTcHack x happyReduce_99

action_365 (168#) = happyReduce_71
action_365 x = happyTcHack x happyReduce_102

action_366 (121#) = happyShift action_414
action_366 x = happyTcHack x happyFail

action_367 (121#) = happyShift action_413
action_367 x = happyTcHack x happyFail

action_368 (168#) = happyReduce_79
action_368 x = happyTcHack x happyReduce_107

action_369 (168#) = happyReduce_93
action_369 x = happyTcHack x happyReduce_120

action_370 (168#) = happyReduce_90
action_370 (181#) = happyShift action_229
action_370 x = happyTcHack x happyReduce_117

action_371 (168#) = happyReduce_92
action_371 x = happyTcHack x happyReduce_119

action_372 (168#) = happyReduce_74
action_372 x = happyTcHack x happyReduce_105

action_373 (149#) = happyShift action_43
action_373 (150#) = happyShift action_44
action_373 (151#) = happyShift action_45
action_373 (152#) = happyShift action_46
action_373 (153#) = happyShift action_47
action_373 (156#) = happyShift action_412
action_373 (168#) = happyReduce_82
action_373 (31#) = happyGoto action_107
action_373 (32#) = happyGoto action_411
action_373 x = happyTcHack x happyReduce_110

action_374 (149#) = happyShift action_43
action_374 (150#) = happyShift action_44
action_374 (151#) = happyShift action_45
action_374 (152#) = happyShift action_46
action_374 (153#) = happyShift action_47
action_374 (156#) = happyShift action_410
action_374 (168#) = happyReduce_81
action_374 (31#) = happyGoto action_107
action_374 (32#) = happyGoto action_409
action_374 x = happyTcHack x happyReduce_109

action_375 (168#) = happyReduce_72
action_375 (181#) = happyShift action_224
action_375 x = happyTcHack x happyReduce_103

action_376 (168#) = happyReduce_73
action_376 (181#) = happyShift action_223
action_376 x = happyTcHack x happyReduce_104

action_377 (168#) = happyReduce_75
action_377 (181#) = happyShift action_408
action_377 x = happyTcHack x happyReduce_106

action_378 (168#) = happyReduce_89
action_378 x = happyTcHack x happyReduce_355

action_379 x = happyTcHack x happyReduce_156

action_380 (120#) = happyShift action_195
action_380 (141#) = happyShift action_196
action_380 (142#) = happyShift action_197
action_380 (170#) = happyShift action_198
action_380 (173#) = happyShift action_199
action_380 (174#) = happyShift action_200
action_380 (176#) = happyShift action_86
action_380 (180#) = happyShift action_201
action_380 (182#) = happyShift action_202
action_380 (34#) = happyGoto action_367
action_380 (35#) = happyGoto action_265
action_380 (36#) = happyGoto action_266
action_380 (37#) = happyGoto action_267
action_380 (38#) = happyGoto action_268
action_380 (39#) = happyGoto action_269
action_380 (40#) = happyGoto action_270
action_380 (41#) = happyGoto action_271
action_380 (43#) = happyGoto action_272
action_380 (44#) = happyGoto action_273
action_380 (45#) = happyGoto action_188
action_380 (46#) = happyGoto action_189
action_380 (47#) = happyGoto action_190
action_380 (48#) = happyGoto action_191
action_380 (49#) = happyGoto action_192
action_380 (50#) = happyGoto action_193
action_380 (114#) = happyGoto action_194
action_380 x = happyTcHack x happyFail

action_381 (120#) = happyShift action_195
action_381 (141#) = happyShift action_196
action_381 (142#) = happyShift action_197
action_381 (170#) = happyShift action_198
action_381 (173#) = happyShift action_199
action_381 (174#) = happyShift action_200
action_381 (176#) = happyShift action_86
action_381 (180#) = happyShift action_201
action_381 (182#) = happyShift action_202
action_381 (47#) = happyGoto action_407
action_381 (48#) = happyGoto action_191
action_381 (49#) = happyGoto action_192
action_381 (50#) = happyGoto action_193
action_381 (114#) = happyGoto action_194
action_381 x = happyTcHack x happyFail

action_382 (120#) = happyShift action_195
action_382 (141#) = happyShift action_196
action_382 (142#) = happyShift action_197
action_382 (170#) = happyShift action_198
action_382 (173#) = happyShift action_199
action_382 (174#) = happyShift action_200
action_382 (176#) = happyShift action_86
action_382 (180#) = happyShift action_201
action_382 (182#) = happyShift action_202
action_382 (47#) = happyGoto action_406
action_382 (48#) = happyGoto action_191
action_382 (49#) = happyGoto action_192
action_382 (50#) = happyGoto action_193
action_382 (114#) = happyGoto action_194
action_382 x = happyTcHack x happyFail

action_383 (120#) = happyShift action_195
action_383 (141#) = happyShift action_196
action_383 (142#) = happyShift action_197
action_383 (170#) = happyShift action_198
action_383 (173#) = happyShift action_199
action_383 (174#) = happyShift action_200
action_383 (176#) = happyShift action_86
action_383 (180#) = happyShift action_201
action_383 (182#) = happyShift action_202
action_383 (47#) = happyGoto action_405
action_383 (48#) = happyGoto action_191
action_383 (49#) = happyGoto action_192
action_383 (50#) = happyGoto action_193
action_383 (114#) = happyGoto action_194
action_383 x = happyTcHack x happyFail

action_384 (120#) = happyShift action_195
action_384 (141#) = happyShift action_196
action_384 (142#) = happyShift action_197
action_384 (170#) = happyShift action_198
action_384 (173#) = happyShift action_199
action_384 (174#) = happyShift action_200
action_384 (176#) = happyShift action_86
action_384 (180#) = happyShift action_201
action_384 (182#) = happyShift action_202
action_384 (46#) = happyGoto action_404
action_384 (47#) = happyGoto action_190
action_384 (48#) = happyGoto action_191
action_384 (49#) = happyGoto action_192
action_384 (50#) = happyGoto action_193
action_384 (114#) = happyGoto action_194
action_384 x = happyTcHack x happyFail

action_385 (120#) = happyShift action_195
action_385 (141#) = happyShift action_196
action_385 (142#) = happyShift action_197
action_385 (170#) = happyShift action_198
action_385 (173#) = happyShift action_199
action_385 (174#) = happyShift action_200
action_385 (176#) = happyShift action_86
action_385 (180#) = happyShift action_201
action_385 (182#) = happyShift action_202
action_385 (46#) = happyGoto action_403
action_385 (47#) = happyGoto action_190
action_385 (48#) = happyGoto action_191
action_385 (49#) = happyGoto action_192
action_385 (50#) = happyGoto action_193
action_385 (114#) = happyGoto action_194
action_385 x = happyTcHack x happyFail

action_386 x = happyTcHack x happyReduce_294

action_387 (13#) = happyGoto action_402
action_387 x = happyTcHack x happyReduce_31

action_388 (117#) = happyShift action_401
action_388 x = happyTcHack x happyFail

action_389 (1#) = happyShift action_36
action_389 (123#) = happyShift action_400
action_389 (128#) = happyShift action_39
action_389 (144#) = happyReduce_344
action_389 (145#) = happyReduce_344
action_389 (146#) = happyShift action_40
action_389 (147#) = happyShift action_41
action_389 (148#) = happyShift action_42
action_389 (149#) = happyShift action_43
action_389 (150#) = happyShift action_44
action_389 (151#) = happyShift action_45
action_389 (152#) = happyShift action_46
action_389 (153#) = happyShift action_47
action_389 (154#) = happyShift action_48
action_389 (155#) = happyShift action_49
action_389 (156#) = happyShift action_50
action_389 (157#) = happyShift action_51
action_389 (158#) = happyShift action_52
action_389 (159#) = happyShift action_53
action_389 (163#) = happyShift action_54
action_389 (168#) = happyShift action_55
action_389 (171#) = happyShift action_56
action_389 (176#) = happyShift action_57
action_389 (183#) = happyShift action_61
action_389 (184#) = happyShift action_62
action_389 (187#) = happyShift action_63
action_389 (188#) = happyShift action_64
action_389 (189#) = happyShift action_65
action_389 (190#) = happyShift action_66
action_389 (193#) = happyShift action_69
action_389 (194#) = happyShift action_70
action_389 (197#) = happyShift action_73
action_389 (20#) = happyGoto action_392
action_389 (21#) = happyGoto action_393
action_389 (23#) = happyGoto action_394
action_389 (24#) = happyGoto action_395
action_389 (26#) = happyGoto action_396
action_389 (31#) = happyGoto action_18
action_389 (32#) = happyGoto action_19
action_389 (51#) = happyGoto action_397
action_389 (53#) = happyGoto action_21
action_389 (55#) = happyGoto action_22
action_389 (58#) = happyGoto action_23
action_389 (59#) = happyGoto action_24
action_389 (60#) = happyGoto action_25
action_389 (61#) = happyGoto action_26
action_389 (86#) = happyGoto action_27
action_389 (89#) = happyGoto action_28
action_389 (90#) = happyGoto action_29
action_389 (93#) = happyGoto action_398
action_389 (96#) = happyGoto action_399
action_389 (104#) = happyGoto action_32
action_389 (109#) = happyGoto action_33
action_389 (110#) = happyGoto action_34
action_389 (111#) = happyGoto action_35
action_389 x = happyTcHack x happyFail

action_390 x = happyTcHack x happyReduce_8

action_391 x = happyTcHack x happyReduce_7

action_392 (1#) = happyShift action_166
action_392 (117#) = happyShift action_167
action_392 (115#) = happyGoto action_546
action_392 x = happyTcHack x happyFail

action_393 (1#) = happyShift action_166
action_393 (117#) = happyShift action_167
action_393 (115#) = happyGoto action_545
action_393 x = happyTcHack x happyFail

action_394 x = happyTcHack x happyReduce_52

action_395 (117#) = happyShift action_544
action_395 x = happyTcHack x happyFail

action_396 (117#) = happyShift action_543
action_396 x = happyTcHack x happyFail

action_397 (117#) = happyShift action_542
action_397 x = happyTcHack x happyFail

action_398 x = happyTcHack x happyReduce_59

action_399 (1#) = happyShift action_36
action_399 (128#) = happyShift action_153
action_399 (146#) = happyShift action_40
action_399 (147#) = happyShift action_41
action_399 (148#) = happyShift action_42
action_399 (149#) = happyShift action_43
action_399 (150#) = happyShift action_44
action_399 (151#) = happyShift action_45
action_399 (152#) = happyShift action_46
action_399 (153#) = happyShift action_47
action_399 (154#) = happyShift action_48
action_399 (155#) = happyShift action_49
action_399 (156#) = happyShift action_50
action_399 (157#) = happyShift action_51
action_399 (158#) = happyShift action_52
action_399 (159#) = happyShift action_53
action_399 (163#) = happyShift action_54
action_399 (171#) = happyShift action_56
action_399 (176#) = happyShift action_57
action_399 (183#) = happyShift action_61
action_399 (184#) = happyShift action_62
action_399 (197#) = happyShift action_73
action_399 (31#) = happyGoto action_18
action_399 (32#) = happyGoto action_19
action_399 (53#) = happyGoto action_21
action_399 (55#) = happyGoto action_22
action_399 (58#) = happyGoto action_152
action_399 (59#) = happyGoto action_24
action_399 (60#) = happyGoto action_25
action_399 (61#) = happyGoto action_26
action_399 (86#) = happyGoto action_27
action_399 (89#) = happyGoto action_28
action_399 (90#) = happyGoto action_29
action_399 (93#) = happyGoto action_541
action_399 (104#) = happyGoto action_32
action_399 x = happyTcHack x happyFail

action_400 x = happyTcHack x happyReduce_42

action_401 (123#) = happyShift action_540
action_401 x = happyTcHack x happyFail

action_402 (168#) = happyShift action_55
action_402 (185#) = happyShift action_539
action_402 (94#) = happyGoto action_538
action_402 (95#) = happyGoto action_219
action_402 (96#) = happyGoto action_220
action_402 x = happyTcHack x happyReduce_303

action_403 (139#) = happyShift action_381
action_403 (140#) = happyShift action_382
action_403 (181#) = happyShift action_383
action_403 x = happyTcHack x happyReduce_147

action_404 (139#) = happyShift action_381
action_404 (140#) = happyShift action_382
action_404 (181#) = happyShift action_383
action_404 x = happyTcHack x happyReduce_146

action_405 x = happyTcHack x happyReduce_149

action_406 x = happyTcHack x happyReduce_151

action_407 x = happyTcHack x happyReduce_150

action_408 (168#) = happyReduce_76
action_408 x = happyTcHack x happyReduce_118

action_409 (168#) = happyReduce_84
action_409 x = happyTcHack x happyReduce_112

action_410 (168#) = happyReduce_88
action_410 x = happyTcHack x happyReduce_116

action_411 (168#) = happyReduce_83
action_411 x = happyTcHack x happyReduce_111

action_412 (168#) = happyReduce_85
action_412 x = happyTcHack x happyReduce_115

action_413 x = happyTcHack x happyReduce_164

action_414 (120#) = happyShift action_195
action_414 (141#) = happyShift action_196
action_414 (142#) = happyShift action_197
action_414 (170#) = happyShift action_198
action_414 (173#) = happyShift action_199
action_414 (174#) = happyShift action_200
action_414 (176#) = happyShift action_86
action_414 (180#) = happyShift action_201
action_414 (182#) = happyShift action_202
action_414 (47#) = happyGoto action_537
action_414 (48#) = happyGoto action_191
action_414 (49#) = happyGoto action_192
action_414 (50#) = happyGoto action_193
action_414 (114#) = happyGoto action_194
action_414 x = happyTcHack x happyFail

action_415 (153#) = happyShift action_536
action_415 x = happyTcHack x happyFail

action_416 (153#) = happyShift action_535
action_416 x = happyTcHack x happyFail

action_417 (169#) = happyShift action_534
action_417 x = happyTcHack x happyFail

action_418 (121#) = happyShift action_533
action_418 (168#) = happyShift action_233
action_418 x = happyTcHack x happyFail

action_419 (120#) = happyShift action_195
action_419 (141#) = happyShift action_196
action_419 (142#) = happyShift action_197
action_419 (170#) = happyShift action_198
action_419 (173#) = happyShift action_199
action_419 (174#) = happyShift action_200
action_419 (176#) = happyShift action_86
action_419 (180#) = happyShift action_201
action_419 (182#) = happyShift action_202
action_419 (34#) = happyGoto action_532
action_419 (35#) = happyGoto action_265
action_419 (36#) = happyGoto action_266
action_419 (37#) = happyGoto action_267
action_419 (38#) = happyGoto action_268
action_419 (39#) = happyGoto action_269
action_419 (40#) = happyGoto action_270
action_419 (41#) = happyGoto action_271
action_419 (43#) = happyGoto action_272
action_419 (44#) = happyGoto action_273
action_419 (45#) = happyGoto action_188
action_419 (46#) = happyGoto action_189
action_419 (47#) = happyGoto action_190
action_419 (48#) = happyGoto action_191
action_419 (49#) = happyGoto action_192
action_419 (50#) = happyGoto action_193
action_419 (114#) = happyGoto action_194
action_419 x = happyTcHack x happyFail

action_420 x = happyTcHack x happyReduce_245

action_421 (127#) = happyShift action_355
action_421 (168#) = happyShift action_55
action_421 (94#) = happyGoto action_351
action_421 (95#) = happyGoto action_219
action_421 (96#) = happyGoto action_220
action_421 (105#) = happyGoto action_531
action_421 x = happyTcHack x happyReduce_303

action_422 (120#) = happyShift action_530
action_422 (124#) = happyShift action_104
action_422 (146#) = happyShift action_105
action_422 (168#) = happyShift action_428
action_422 (172#) = happyShift action_106
action_422 (175#) = happyShift action_149
action_422 (176#) = happyShift action_86
action_422 (181#) = happyShift action_150
action_422 (193#) = happyShift action_69
action_422 (64#) = happyGoto action_525
action_422 (65#) = happyGoto action_526
action_422 (66#) = happyGoto action_141
action_422 (67#) = happyGoto action_142
action_422 (68#) = happyGoto action_143
action_422 (69#) = happyGoto action_144
action_422 (70#) = happyGoto action_145
action_422 (71#) = happyGoto action_527
action_422 (107#) = happyGoto action_528
action_422 (108#) = happyGoto action_529
action_422 (110#) = happyGoto action_147
action_422 (111#) = happyGoto action_35
action_422 (114#) = happyGoto action_102
action_422 x = happyTcHack x happyReduce_329

action_423 x = happyTcHack x happyReduce_242

action_424 (126#) = happyShift action_524
action_424 x = happyTcHack x happyFail

action_425 x = happyTcHack x happyReduce_243

action_426 (125#) = happyShift action_517
action_426 (193#) = happyShift action_69
action_426 (109#) = happyGoto action_523
action_426 (110#) = happyGoto action_34
action_426 (111#) = happyGoto action_35
action_426 x = happyTcHack x happyReduce_344

action_427 x = happyTcHack x happyReduce_220

action_428 (120#) = happyShift action_195
action_428 (141#) = happyShift action_196
action_428 (142#) = happyShift action_197
action_428 (169#) = happyShift action_522
action_428 (170#) = happyShift action_198
action_428 (173#) = happyShift action_199
action_428 (174#) = happyShift action_200
action_428 (176#) = happyShift action_86
action_428 (180#) = happyShift action_201
action_428 (182#) = happyShift action_202
action_428 (34#) = happyGoto action_521
action_428 (35#) = happyGoto action_265
action_428 (36#) = happyGoto action_266
action_428 (37#) = happyGoto action_267
action_428 (38#) = happyGoto action_268
action_428 (39#) = happyGoto action_269
action_428 (40#) = happyGoto action_270
action_428 (41#) = happyGoto action_271
action_428 (43#) = happyGoto action_272
action_428 (44#) = happyGoto action_273
action_428 (45#) = happyGoto action_188
action_428 (46#) = happyGoto action_189
action_428 (47#) = happyGoto action_190
action_428 (48#) = happyGoto action_191
action_428 (49#) = happyGoto action_192
action_428 (50#) = happyGoto action_193
action_428 (114#) = happyGoto action_194
action_428 x = happyTcHack x happyFail

action_429 (125#) = happyShift action_517
action_429 (193#) = happyShift action_69
action_429 (109#) = happyGoto action_520
action_429 (110#) = happyGoto action_34
action_429 (111#) = happyGoto action_35
action_429 x = happyTcHack x happyReduce_344

action_430 (123#) = happyShift action_519
action_430 x = happyTcHack x happyFail

action_431 (123#) = happyReduce_356
action_431 x = happyTcHack x happyReduce_200

action_432 x = happyTcHack x happyReduce_169

action_433 x = happyTcHack x happyReduce_213

action_434 (117#) = happyShift action_518
action_434 x = happyTcHack x happyFail

action_435 x = happyTcHack x happyReduce_253

action_436 x = happyTcHack x happyReduce_174

action_437 (168#) = happyShift action_428
action_437 x = happyTcHack x happyReduce_175

action_438 (125#) = happyShift action_517
action_438 x = happyTcHack x happyReduce_255

action_439 (193#) = happyShift action_69
action_439 (109#) = happyGoto action_516
action_439 (110#) = happyGoto action_34
action_439 (111#) = happyGoto action_35
action_439 x = happyTcHack x happyReduce_344

action_440 (169#) = happyShift action_515
action_440 x = happyTcHack x happyFail

action_441 (120#) = happyShift action_514
action_441 x = happyTcHack x happyFail

action_442 x = happyTcHack x happyReduce_280

action_443 x = happyTcHack x happyReduce_278

action_444 (124#) = happyShift action_513
action_444 x = happyTcHack x happyFail

action_445 x = happyTcHack x happyReduce_267

action_446 x = happyTcHack x happyReduce_276

action_447 (1#) = happyShift action_36
action_447 (128#) = happyShift action_157
action_447 (129#) = happyShift action_158
action_447 (146#) = happyShift action_40
action_447 (147#) = happyShift action_41
action_447 (148#) = happyShift action_42
action_447 (149#) = happyShift action_43
action_447 (150#) = happyShift action_44
action_447 (151#) = happyShift action_45
action_447 (152#) = happyShift action_46
action_447 (153#) = happyShift action_47
action_447 (154#) = happyShift action_48
action_447 (155#) = happyShift action_49
action_447 (156#) = happyShift action_50
action_447 (157#) = happyShift action_51
action_447 (158#) = happyShift action_52
action_447 (159#) = happyShift action_53
action_447 (163#) = happyShift action_54
action_447 (171#) = happyShift action_56
action_447 (176#) = happyShift action_57
action_447 (197#) = happyShift action_73
action_447 (31#) = happyGoto action_18
action_447 (32#) = happyGoto action_19
action_447 (52#) = happyGoto action_512
action_447 (54#) = happyGoto action_333
action_447 (55#) = happyGoto action_334
action_447 (59#) = happyGoto action_76
action_447 (60#) = happyGoto action_25
action_447 (61#) = happyGoto action_26
action_447 (86#) = happyGoto action_77
action_447 x = happyTcHack x happyFail

action_448 x = happyTcHack x happyReduce_265

action_449 (117#) = happyShift action_511
action_449 x = happyTcHack x happyFail

action_450 (161#) = happyShift action_441
action_450 (162#) = happyShift action_442
action_450 (83#) = happyGoto action_440
action_450 x = happyTcHack x happyFail

action_451 (193#) = happyShift action_69
action_451 (109#) = happyGoto action_510
action_451 (110#) = happyGoto action_34
action_451 (111#) = happyGoto action_35
action_451 x = happyTcHack x happyReduce_344

action_452 x = happyTcHack x happyReduce_259

action_453 x = happyTcHack x happyReduce_262

action_454 (176#) = happyShift action_86
action_454 (114#) = happyGoto action_509
action_454 x = happyTcHack x happyFail

action_455 x = happyTcHack x happyReduce_261

action_456 x = happyTcHack x happyReduce_264

action_457 x = happyTcHack x happyReduce_260

action_458 x = happyTcHack x happyReduce_263

action_459 x = happyTcHack x happyReduce_289

action_460 x = happyTcHack x happyReduce_285

action_461 (120#) = happyShift action_195
action_461 (141#) = happyShift action_196
action_461 (142#) = happyShift action_197
action_461 (170#) = happyShift action_198
action_461 (173#) = happyShift action_199
action_461 (174#) = happyShift action_200
action_461 (176#) = happyShift action_86
action_461 (180#) = happyShift action_201
action_461 (182#) = happyShift action_202
action_461 (34#) = happyGoto action_508
action_461 (35#) = happyGoto action_265
action_461 (36#) = happyGoto action_266
action_461 (37#) = happyGoto action_267
action_461 (38#) = happyGoto action_268
action_461 (39#) = happyGoto action_269
action_461 (40#) = happyGoto action_270
action_461 (41#) = happyGoto action_271
action_461 (43#) = happyGoto action_272
action_461 (44#) = happyGoto action_273
action_461 (45#) = happyGoto action_188
action_461 (46#) = happyGoto action_189
action_461 (47#) = happyGoto action_190
action_461 (48#) = happyGoto action_191
action_461 (49#) = happyGoto action_192
action_461 (50#) = happyGoto action_193
action_461 (114#) = happyGoto action_194
action_461 x = happyTcHack x happyFail

action_462 x = happyTcHack x happyReduce_292

action_463 (123#) = happyShift action_507
action_463 x = happyTcHack x happyFail

action_464 x = happyTcHack x happyReduce_326

action_465 x = happyTcHack x happyReduce_324

action_466 x = happyTcHack x happyReduce_323

action_467 (123#) = happyShift action_506
action_467 x = happyTcHack x happyFail

action_468 x = happyTcHack x happyReduce_317

action_469 (120#) = happyShift action_195
action_469 (122#) = happyShift action_310
action_469 (141#) = happyShift action_196
action_469 (142#) = happyShift action_197
action_469 (146#) = happyShift action_311
action_469 (154#) = happyShift action_312
action_469 (155#) = happyShift action_313
action_469 (170#) = happyShift action_198
action_469 (173#) = happyShift action_199
action_469 (174#) = happyShift action_200
action_469 (176#) = happyShift action_86
action_469 (180#) = happyShift action_201
action_469 (181#) = happyShift action_314
action_469 (182#) = happyShift action_202
action_469 (34#) = happyGoto action_307
action_469 (35#) = happyGoto action_265
action_469 (36#) = happyGoto action_266
action_469 (37#) = happyGoto action_267
action_469 (38#) = happyGoto action_268
action_469 (39#) = happyGoto action_269
action_469 (40#) = happyGoto action_270
action_469 (41#) = happyGoto action_271
action_469 (43#) = happyGoto action_272
action_469 (44#) = happyGoto action_273
action_469 (45#) = happyGoto action_188
action_469 (46#) = happyGoto action_189
action_469 (47#) = happyGoto action_190
action_469 (48#) = happyGoto action_191
action_469 (49#) = happyGoto action_192
action_469 (50#) = happyGoto action_193
action_469 (103#) = happyGoto action_505
action_469 (114#) = happyGoto action_194
action_469 x = happyTcHack x happyReduce_321

action_470 (123#) = happyShift action_504
action_470 (168#) = happyShift action_303
action_470 (15#) = happyGoto action_496
action_470 (16#) = happyGoto action_302
action_470 x = happyTcHack x happyReduce_39

action_471 (125#) = happyShift action_502
action_471 (169#) = happyShift action_503
action_471 x = happyTcHack x happyFail

action_472 x = happyTcHack x happyReduce_312

action_473 x = happyTcHack x happyReduce_314

action_474 x = happyTcHack x happyReduce_315

action_475 (146#) = happyShift action_501
action_475 (176#) = happyShift action_86
action_475 (114#) = happyGoto action_500
action_475 x = happyTcHack x happyFail

action_476 (146#) = happyShift action_499
action_476 (176#) = happyShift action_86
action_476 (114#) = happyGoto action_498
action_476 x = happyTcHack x happyFail

action_477 (123#) = happyShift action_497
action_477 (168#) = happyShift action_303
action_477 (15#) = happyGoto action_496
action_477 (16#) = happyGoto action_302
action_477 x = happyTcHack x happyReduce_39

action_478 x = happyTcHack x happyReduce_27

action_479 (176#) = happyShift action_495
action_479 x = happyTcHack x happyFail

action_480 (121#) = happyShift action_494
action_480 x = happyTcHack x happyFail

action_481 (124#) = happyShift action_493
action_481 x = happyTcHack x happyFail

action_482 (137#) = happyShift action_287
action_482 x = happyTcHack x happyReduce_125

action_483 (133#) = happyShift action_286
action_483 x = happyTcHack x happyReduce_127

action_484 (135#) = happyShift action_285
action_484 x = happyTcHack x happyReduce_129

action_485 (136#) = happyShift action_284
action_485 x = happyTcHack x happyReduce_131

action_486 (131#) = happyShift action_282
action_486 (132#) = happyShift action_283
action_486 (42#) = happyGoto action_281
action_486 x = happyTcHack x happyReduce_133

action_487 (164#) = happyShift action_277
action_487 (165#) = happyShift action_278
action_487 (166#) = happyShift action_279
action_487 (167#) = happyShift action_280
action_487 x = happyTcHack x happyReduce_135

action_488 (138#) = happyShift action_276
action_488 x = happyTcHack x happyReduce_141

action_489 (138#) = happyShift action_276
action_489 x = happyTcHack x happyReduce_142

action_490 (138#) = happyShift action_276
action_490 x = happyTcHack x happyReduce_140

action_491 (138#) = happyShift action_276
action_491 x = happyTcHack x happyReduce_139

action_492 (180#) = happyShift action_384
action_492 (182#) = happyShift action_385
action_492 x = happyTcHack x happyReduce_144

action_493 (120#) = happyShift action_195
action_493 (141#) = happyShift action_196
action_493 (142#) = happyShift action_197
action_493 (170#) = happyShift action_198
action_493 (173#) = happyShift action_199
action_493 (174#) = happyShift action_200
action_493 (176#) = happyShift action_86
action_493 (180#) = happyShift action_201
action_493 (182#) = happyShift action_202
action_493 (36#) = happyGoto action_568
action_493 (37#) = happyGoto action_267
action_493 (38#) = happyGoto action_268
action_493 (39#) = happyGoto action_269
action_493 (40#) = happyGoto action_270
action_493 (41#) = happyGoto action_271
action_493 (43#) = happyGoto action_272
action_493 (44#) = happyGoto action_273
action_493 (45#) = happyGoto action_188
action_493 (46#) = happyGoto action_189
action_493 (47#) = happyGoto action_190
action_493 (48#) = happyGoto action_191
action_493 (49#) = happyGoto action_192
action_493 (50#) = happyGoto action_193
action_493 (114#) = happyGoto action_194
action_493 x = happyTcHack x happyFail

action_494 x = happyTcHack x happyReduce_348

action_495 (121#) = happyShift action_566
action_495 (125#) = happyShift action_567
action_495 x = happyTcHack x happyFail

action_496 x = happyTcHack x happyReduce_34

action_497 x = happyTcHack x happyReduce_25

action_498 x = happyTcHack x happyReduce_38

action_499 x = happyTcHack x happyReduce_36

action_500 x = happyTcHack x happyReduce_37

action_501 x = happyTcHack x happyReduce_35

action_502 (162#) = happyShift action_474
action_502 (176#) = happyShift action_86
action_502 (100#) = happyGoto action_565
action_502 (114#) = happyGoto action_473
action_502 x = happyTcHack x happyFail

action_503 x = happyTcHack x happyReduce_40

action_504 x = happyTcHack x happyReduce_26

action_505 x = happyTcHack x happyReduce_319

action_506 x = happyTcHack x happyReduce_325

action_507 x = happyTcHack x happyReduce_286

action_508 x = happyTcHack x happyReduce_293

action_509 (121#) = happyShift action_564
action_509 x = happyTcHack x happyFail

action_510 x = happyTcHack x happyReduce_216

action_511 x = happyTcHack x happyReduce_266

action_512 (120#) = happyShift action_148
action_512 (124#) = happyShift action_104
action_512 (146#) = happyShift action_105
action_512 (168#) = happyShift action_428
action_512 (172#) = happyShift action_106
action_512 (175#) = happyShift action_149
action_512 (176#) = happyShift action_86
action_512 (181#) = happyShift action_150
action_512 (193#) = happyShift action_69
action_512 (64#) = happyGoto action_563
action_512 (65#) = happyGoto action_140
action_512 (66#) = happyGoto action_141
action_512 (67#) = happyGoto action_142
action_512 (68#) = happyGoto action_143
action_512 (69#) = happyGoto action_144
action_512 (70#) = happyGoto action_145
action_512 (71#) = happyGoto action_146
action_512 (110#) = happyGoto action_147
action_512 (111#) = happyGoto action_35
action_512 (114#) = happyGoto action_102
action_512 x = happyTcHack x happyReduce_274

action_513 x = happyTcHack x happyReduce_277

action_514 (120#) = happyShift action_195
action_514 (141#) = happyShift action_196
action_514 (142#) = happyShift action_197
action_514 (170#) = happyShift action_198
action_514 (173#) = happyShift action_199
action_514 (174#) = happyShift action_200
action_514 (176#) = happyShift action_86
action_514 (180#) = happyShift action_201
action_514 (182#) = happyShift action_202
action_514 (34#) = happyGoto action_561
action_514 (35#) = happyGoto action_265
action_514 (36#) = happyGoto action_266
action_514 (37#) = happyGoto action_267
action_514 (38#) = happyGoto action_268
action_514 (39#) = happyGoto action_269
action_514 (40#) = happyGoto action_270
action_514 (41#) = happyGoto action_271
action_514 (43#) = happyGoto action_272
action_514 (44#) = happyGoto action_273
action_514 (45#) = happyGoto action_188
action_514 (46#) = happyGoto action_189
action_514 (47#) = happyGoto action_190
action_514 (48#) = happyGoto action_191
action_514 (49#) = happyGoto action_192
action_514 (50#) = happyGoto action_193
action_514 (84#) = happyGoto action_562
action_514 (114#) = happyGoto action_194
action_514 x = happyTcHack x happyFail

action_515 (1#) = happyShift action_36
action_515 (117#) = happyReduce_269
action_515 (128#) = happyShift action_157
action_515 (129#) = happyShift action_158
action_515 (146#) = happyShift action_40
action_515 (147#) = happyShift action_41
action_515 (148#) = happyShift action_42
action_515 (149#) = happyShift action_43
action_515 (150#) = happyShift action_44
action_515 (151#) = happyShift action_45
action_515 (152#) = happyShift action_46
action_515 (153#) = happyShift action_47
action_515 (154#) = happyShift action_48
action_515 (155#) = happyShift action_49
action_515 (156#) = happyShift action_50
action_515 (157#) = happyShift action_51
action_515 (158#) = happyShift action_52
action_515 (159#) = happyShift action_53
action_515 (163#) = happyShift action_54
action_515 (171#) = happyShift action_56
action_515 (176#) = happyShift action_57
action_515 (197#) = happyShift action_73
action_515 (31#) = happyGoto action_18
action_515 (32#) = happyGoto action_19
action_515 (52#) = happyGoto action_559
action_515 (54#) = happyGoto action_333
action_515 (55#) = happyGoto action_334
action_515 (59#) = happyGoto action_76
action_515 (60#) = happyGoto action_25
action_515 (61#) = happyGoto action_26
action_515 (79#) = happyGoto action_560
action_515 (86#) = happyGoto action_77
action_515 x = happyTcHack x happyFail

action_516 x = happyTcHack x happyReduce_212

action_517 (120#) = happyShift action_148
action_517 (124#) = happyShift action_104
action_517 (146#) = happyShift action_105
action_517 (172#) = happyShift action_106
action_517 (175#) = happyShift action_149
action_517 (176#) = happyShift action_86
action_517 (181#) = happyShift action_150
action_517 (193#) = happyShift action_69
action_517 (64#) = happyGoto action_558
action_517 (65#) = happyGoto action_140
action_517 (66#) = happyGoto action_141
action_517 (67#) = happyGoto action_142
action_517 (68#) = happyGoto action_143
action_517 (69#) = happyGoto action_144
action_517 (70#) = happyGoto action_145
action_517 (71#) = happyGoto action_146
action_517 (110#) = happyGoto action_147
action_517 (111#) = happyGoto action_35
action_517 (114#) = happyGoto action_102
action_517 x = happyTcHack x happyFail

action_518 x = happyTcHack x happyReduce_254

action_519 x = happyTcHack x happyReduce_23

action_520 x = happyTcHack x happyReduce_165

action_521 (169#) = happyShift action_557
action_521 x = happyTcHack x happyFail

action_522 x = happyTcHack x happyReduce_172

action_523 x = happyTcHack x happyReduce_170

action_524 (120#) = happyShift action_380
action_524 (173#) = happyShift action_199
action_524 (174#) = happyShift action_200
action_524 (176#) = happyShift action_86
action_524 (50#) = happyGoto action_556
action_524 (114#) = happyGoto action_194
action_524 x = happyTcHack x happyFail

action_525 x = happyTcHack x happyReduce_330

action_526 (120#) = happyShift action_530
action_526 (124#) = happyShift action_104
action_526 (146#) = happyShift action_105
action_526 (172#) = happyShift action_106
action_526 (175#) = happyShift action_149
action_526 (176#) = happyShift action_86
action_526 (181#) = happyShift action_150
action_526 (193#) = happyShift action_69
action_526 (65#) = happyGoto action_554
action_526 (66#) = happyGoto action_214
action_526 (67#) = happyGoto action_142
action_526 (68#) = happyGoto action_215
action_526 (69#) = happyGoto action_144
action_526 (70#) = happyGoto action_145
action_526 (71#) = happyGoto action_527
action_526 (107#) = happyGoto action_555
action_526 (108#) = happyGoto action_529
action_526 (110#) = happyGoto action_147
action_526 (111#) = happyGoto action_35
action_526 (114#) = happyGoto action_102
action_526 x = happyTcHack x happyFail

action_527 (120#) = happyShift action_530
action_527 (124#) = happyShift action_104
action_527 (146#) = happyShift action_105
action_527 (172#) = happyShift action_106
action_527 (175#) = happyShift action_211
action_527 (176#) = happyShift action_86
action_527 (193#) = happyShift action_69
action_527 (66#) = happyGoto action_209
action_527 (67#) = happyGoto action_142
action_527 (69#) = happyGoto action_144
action_527 (70#) = happyGoto action_145
action_527 (108#) = happyGoto action_553
action_527 (110#) = happyGoto action_210
action_527 (111#) = happyGoto action_35
action_527 (114#) = happyGoto action_102
action_527 x = happyTcHack x happyReduce_335

action_528 x = happyTcHack x happyReduce_331

action_529 (120#) = happyShift action_552
action_529 x = happyTcHack x happyReduce_337

action_530 (120#) = happyShift action_530
action_530 (121#) = happyShift action_551
action_530 (124#) = happyShift action_104
action_530 (127#) = happyShift action_355
action_530 (146#) = happyShift action_105
action_530 (168#) = happyShift action_55
action_530 (172#) = happyShift action_106
action_530 (175#) = happyShift action_149
action_530 (176#) = happyShift action_86
action_530 (181#) = happyShift action_150
action_530 (193#) = happyShift action_69
action_530 (64#) = happyGoto action_208
action_530 (65#) = happyGoto action_526
action_530 (66#) = happyGoto action_141
action_530 (67#) = happyGoto action_142
action_530 (68#) = happyGoto action_143
action_530 (69#) = happyGoto action_144
action_530 (70#) = happyGoto action_145
action_530 (71#) = happyGoto action_527
action_530 (94#) = happyGoto action_351
action_530 (95#) = happyGoto action_219
action_530 (96#) = happyGoto action_220
action_530 (105#) = happyGoto action_352
action_530 (106#) = happyGoto action_549
action_530 (107#) = happyGoto action_550
action_530 (108#) = happyGoto action_529
action_530 (110#) = happyGoto action_147
action_530 (111#) = happyGoto action_35
action_530 (114#) = happyGoto action_102
action_530 x = happyTcHack x happyReduce_303

action_531 x = happyTcHack x happyReduce_334

action_532 x = happyTcHack x happyReduce_168

action_533 x = happyTcHack x happyReduce_155

action_534 (168#) = happyReduce_80
action_534 x = happyTcHack x happyReduce_108

action_535 (168#) = happyReduce_86
action_535 x = happyTcHack x happyReduce_114

action_536 (168#) = happyReduce_87
action_536 x = happyTcHack x happyReduce_113

action_537 x = happyTcHack x happyReduce_153

action_538 (1#) = happyShift action_36
action_538 (128#) = happyShift action_157
action_538 (129#) = happyShift action_158
action_538 (146#) = happyShift action_40
action_538 (147#) = happyShift action_41
action_538 (148#) = happyShift action_42
action_538 (149#) = happyShift action_43
action_538 (150#) = happyShift action_44
action_538 (151#) = happyShift action_45
action_538 (152#) = happyShift action_46
action_538 (153#) = happyShift action_47
action_538 (154#) = happyShift action_48
action_538 (155#) = happyShift action_49
action_538 (156#) = happyShift action_50
action_538 (157#) = happyShift action_51
action_538 (158#) = happyShift action_52
action_538 (159#) = happyShift action_53
action_538 (163#) = happyShift action_54
action_538 (171#) = happyShift action_56
action_538 (176#) = happyShift action_57
action_538 (197#) = happyShift action_73
action_538 (31#) = happyGoto action_18
action_538 (32#) = happyGoto action_19
action_538 (52#) = happyGoto action_548
action_538 (54#) = happyGoto action_333
action_538 (55#) = happyGoto action_334
action_538 (59#) = happyGoto action_76
action_538 (60#) = happyGoto action_25
action_538 (61#) = happyGoto action_26
action_538 (86#) = happyGoto action_77
action_538 x = happyTcHack x happyFail

action_539 (124#) = happyShift action_547
action_539 x = happyTcHack x happyFail

action_540 x = happyTcHack x happyReduce_30

action_541 x = happyTcHack x happyReduce_58

action_542 x = happyTcHack x happyReduce_54

action_543 x = happyTcHack x happyReduce_57

action_544 x = happyTcHack x happyReduce_53

action_545 x = happyTcHack x happyReduce_56

action_546 x = happyTcHack x happyReduce_55

action_547 (91#) = happyGoto action_583
action_547 x = happyTcHack x happyReduce_298

action_548 (120#) = happyShift action_148
action_548 (124#) = happyShift action_104
action_548 (146#) = happyShift action_105
action_548 (168#) = happyShift action_428
action_548 (172#) = happyShift action_106
action_548 (175#) = happyShift action_149
action_548 (176#) = happyShift action_86
action_548 (181#) = happyShift action_150
action_548 (193#) = happyShift action_69
action_548 (64#) = happyGoto action_582
action_548 (65#) = happyGoto action_140
action_548 (66#) = happyGoto action_141
action_548 (67#) = happyGoto action_142
action_548 (68#) = happyGoto action_143
action_548 (69#) = happyGoto action_144
action_548 (70#) = happyGoto action_145
action_548 (71#) = happyGoto action_146
action_548 (110#) = happyGoto action_147
action_548 (111#) = happyGoto action_35
action_548 (114#) = happyGoto action_102
action_548 x = happyTcHack x happyFail

action_549 (121#) = happyShift action_581
action_549 (125#) = happyShift action_421
action_549 x = happyTcHack x happyFail

action_550 (121#) = happyShift action_580
action_550 x = happyTcHack x happyFail

action_551 x = happyTcHack x happyReduce_340

action_552 (121#) = happyShift action_579
action_552 (127#) = happyShift action_355
action_552 (168#) = happyShift action_55
action_552 (94#) = happyGoto action_351
action_552 (95#) = happyGoto action_219
action_552 (96#) = happyGoto action_220
action_552 (105#) = happyGoto action_352
action_552 (106#) = happyGoto action_578
action_552 x = happyTcHack x happyReduce_303

action_553 (120#) = happyShift action_552
action_553 x = happyTcHack x happyReduce_338

action_554 (120#) = happyShift action_577
action_554 (175#) = happyShift action_149
action_554 (181#) = happyShift action_150
action_554 (193#) = happyShift action_69
action_554 (65#) = happyGoto action_554
action_554 (71#) = happyGoto action_576
action_554 (107#) = happyGoto action_555
action_554 (108#) = happyGoto action_529
action_554 (110#) = happyGoto action_147
action_554 (111#) = happyGoto action_35
action_554 x = happyTcHack x happyFail

action_555 x = happyTcHack x happyReduce_336

action_556 (169#) = happyShift action_575
action_556 x = happyTcHack x happyFail

action_557 x = happyTcHack x happyReduce_173

action_558 x = happyTcHack x happyReduce_221

action_559 (120#) = happyShift action_148
action_559 (124#) = happyShift action_104
action_559 (146#) = happyShift action_105
action_559 (168#) = happyShift action_428
action_559 (172#) = happyShift action_106
action_559 (175#) = happyShift action_149
action_559 (176#) = happyShift action_86
action_559 (181#) = happyShift action_150
action_559 (193#) = happyShift action_69
action_559 (64#) = happyGoto action_574
action_559 (65#) = happyGoto action_140
action_559 (66#) = happyGoto action_141
action_559 (67#) = happyGoto action_142
action_559 (68#) = happyGoto action_143
action_559 (69#) = happyGoto action_144
action_559 (70#) = happyGoto action_145
action_559 (71#) = happyGoto action_146
action_559 (110#) = happyGoto action_147
action_559 (111#) = happyGoto action_35
action_559 (114#) = happyGoto action_102
action_559 x = happyTcHack x happyReduce_271

action_560 x = happyTcHack x happyReduce_268

action_561 x = happyTcHack x happyReduce_281

action_562 (121#) = happyShift action_572
action_562 (125#) = happyShift action_573
action_562 x = happyTcHack x happyFail

action_563 x = happyTcHack x happyReduce_273

action_564 (176#) = happyShift action_86
action_564 (75#) = happyGoto action_570
action_564 (114#) = happyGoto action_571
action_564 x = happyTcHack x happyReduce_257

action_565 x = happyTcHack x happyReduce_313

action_566 x = happyTcHack x happyReduce_351

action_567 (120#) = happyShift action_195
action_567 (141#) = happyShift action_196
action_567 (142#) = happyShift action_197
action_567 (170#) = happyShift action_198
action_567 (173#) = happyShift action_199
action_567 (174#) = happyShift action_200
action_567 (176#) = happyShift action_86
action_567 (180#) = happyShift action_201
action_567 (182#) = happyShift action_202
action_567 (34#) = happyGoto action_561
action_567 (35#) = happyGoto action_265
action_567 (36#) = happyGoto action_266
action_567 (37#) = happyGoto action_267
action_567 (38#) = happyGoto action_268
action_567 (39#) = happyGoto action_269
action_567 (40#) = happyGoto action_270
action_567 (41#) = happyGoto action_271
action_567 (43#) = happyGoto action_272
action_567 (44#) = happyGoto action_273
action_567 (45#) = happyGoto action_188
action_567 (46#) = happyGoto action_189
action_567 (47#) = happyGoto action_190
action_567 (48#) = happyGoto action_191
action_567 (49#) = happyGoto action_192
action_567 (50#) = happyGoto action_193
action_567 (84#) = happyGoto action_569
action_567 (114#) = happyGoto action_194
action_567 x = happyTcHack x happyFail

action_568 (134#) = happyShift action_288
action_568 x = happyTcHack x happyReduce_123

action_569 (121#) = happyShift action_591
action_569 (125#) = happyShift action_573
action_569 x = happyTcHack x happyFail

action_570 (122#) = happyShift action_590
action_570 x = happyTcHack x happyFail

action_571 x = happyTcHack x happyReduce_258

action_572 x = happyTcHack x happyReduce_279

action_573 (120#) = happyShift action_195
action_573 (141#) = happyShift action_196
action_573 (142#) = happyShift action_197
action_573 (170#) = happyShift action_198
action_573 (173#) = happyShift action_199
action_573 (174#) = happyShift action_200
action_573 (176#) = happyShift action_86
action_573 (180#) = happyShift action_201
action_573 (182#) = happyShift action_202
action_573 (34#) = happyGoto action_589
action_573 (35#) = happyGoto action_265
action_573 (36#) = happyGoto action_266
action_573 (37#) = happyGoto action_267
action_573 (38#) = happyGoto action_268
action_573 (39#) = happyGoto action_269
action_573 (40#) = happyGoto action_270
action_573 (41#) = happyGoto action_271
action_573 (43#) = happyGoto action_272
action_573 (44#) = happyGoto action_273
action_573 (45#) = happyGoto action_188
action_573 (46#) = happyGoto action_189
action_573 (47#) = happyGoto action_190
action_573 (48#) = happyGoto action_191
action_573 (49#) = happyGoto action_192
action_573 (50#) = happyGoto action_193
action_573 (114#) = happyGoto action_194
action_573 x = happyTcHack x happyFail

action_574 x = happyTcHack x happyReduce_270

action_575 x = happyTcHack x happyReduce_244

action_576 (120#) = happyShift action_577
action_576 (108#) = happyGoto action_553
action_576 x = happyTcHack x happyReduce_335

action_577 (120#) = happyShift action_577
action_577 (121#) = happyShift action_551
action_577 (127#) = happyShift action_355
action_577 (168#) = happyShift action_55
action_577 (175#) = happyShift action_149
action_577 (181#) = happyShift action_150
action_577 (193#) = happyShift action_69
action_577 (65#) = happyGoto action_554
action_577 (71#) = happyGoto action_576
action_577 (94#) = happyGoto action_351
action_577 (95#) = happyGoto action_219
action_577 (96#) = happyGoto action_220
action_577 (105#) = happyGoto action_352
action_577 (106#) = happyGoto action_549
action_577 (107#) = happyGoto action_550
action_577 (108#) = happyGoto action_529
action_577 (110#) = happyGoto action_147
action_577 (111#) = happyGoto action_35
action_577 x = happyTcHack x happyReduce_303

action_578 (121#) = happyShift action_588
action_578 (125#) = happyShift action_421
action_578 x = happyTcHack x happyFail

action_579 x = happyTcHack x happyReduce_342

action_580 x = happyTcHack x happyReduce_339

action_581 x = happyTcHack x happyReduce_341

action_582 (117#) = happyShift action_587
action_582 x = happyTcHack x happyFail

action_583 (123#) = happyShift action_586
action_583 (168#) = happyShift action_55
action_583 (92#) = happyGoto action_584
action_583 (94#) = happyGoto action_585
action_583 (95#) = happyGoto action_219
action_583 (96#) = happyGoto action_220
action_583 x = happyTcHack x happyReduce_303

action_584 (117#) = happyShift action_594
action_584 x = happyTcHack x happyFail

action_585 (1#) = happyShift action_36
action_585 (146#) = happyShift action_40
action_585 (147#) = happyShift action_41
action_585 (148#) = happyShift action_42
action_585 (149#) = happyShift action_43
action_585 (150#) = happyShift action_44
action_585 (151#) = happyShift action_45
action_585 (152#) = happyShift action_46
action_585 (153#) = happyShift action_47
action_585 (154#) = happyShift action_48
action_585 (155#) = happyShift action_49
action_585 (156#) = happyShift action_50
action_585 (157#) = happyShift action_51
action_585 (158#) = happyShift action_52
action_585 (159#) = happyShift action_53
action_585 (163#) = happyShift action_54
action_585 (171#) = happyShift action_56
action_585 (176#) = happyShift action_57
action_585 (183#) = happyShift action_61
action_585 (184#) = happyShift action_62
action_585 (197#) = happyShift action_73
action_585 (31#) = happyGoto action_18
action_585 (32#) = happyGoto action_19
action_585 (53#) = happyGoto action_21
action_585 (55#) = happyGoto action_22
action_585 (59#) = happyGoto action_76
action_585 (60#) = happyGoto action_25
action_585 (61#) = happyGoto action_26
action_585 (86#) = happyGoto action_77
action_585 (89#) = happyGoto action_28
action_585 (90#) = happyGoto action_29
action_585 (104#) = happyGoto action_593
action_585 x = happyTcHack x happyFail

action_586 x = happyTcHack x happyReduce_29

action_587 x = happyTcHack x happyReduce_32

action_588 x = happyTcHack x happyReduce_343

action_589 x = happyTcHack x happyReduce_282

action_590 (161#) = happyShift action_327
action_590 (162#) = happyShift action_328
action_590 (168#) = happyShift action_450
action_590 (77#) = happyGoto action_592
action_590 (78#) = happyGoto action_324
action_590 (81#) = happyGoto action_325
action_590 (82#) = happyGoto action_326
action_590 x = happyTcHack x happyFail

action_591 x = happyTcHack x happyReduce_352

action_592 (123#) = happyShift action_596
action_592 (161#) = happyShift action_327
action_592 (162#) = happyShift action_328
action_592 (168#) = happyShift action_450
action_592 (78#) = happyGoto action_449
action_592 (81#) = happyGoto action_325
action_592 (82#) = happyGoto action_326
action_592 x = happyTcHack x happyFail

action_593 (120#) = happyShift action_148
action_593 (124#) = happyShift action_104
action_593 (146#) = happyShift action_105
action_593 (172#) = happyShift action_106
action_593 (175#) = happyShift action_149
action_593 (176#) = happyShift action_86
action_593 (181#) = happyShift action_150
action_593 (193#) = happyShift action_69
action_593 (64#) = happyGoto action_595
action_593 (65#) = happyGoto action_140
action_593 (66#) = happyGoto action_141
action_593 (67#) = happyGoto action_142
action_593 (68#) = happyGoto action_143
action_593 (69#) = happyGoto action_144
action_593 (70#) = happyGoto action_145
action_593 (71#) = happyGoto action_146
action_593 (110#) = happyGoto action_147
action_593 (111#) = happyGoto action_35
action_593 (114#) = happyGoto action_102
action_593 x = happyTcHack x happyFail

action_594 x = happyTcHack x happyReduce_299

action_595 (193#) = happyShift action_69
action_595 (109#) = happyGoto action_597
action_595 (110#) = happyGoto action_34
action_595 (111#) = happyGoto action_35
action_595 x = happyTcHack x happyReduce_344

action_596 x = happyTcHack x happyReduce_215

action_597 x = happyTcHack x happyReduce_300

happyReduce_1 = happySpecReduce_1 4# happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn4
                 (Left  (reverse happy_var_1)
        )
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2 4# happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
        _
         =  HappyAbsSyn4
                 (Right (reverse happy_var_2)
        )
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0 5# happyReduction_3
happyReduction_3  =  HappyAbsSyn5
                 ([]
        )

happyReduce_4 = happySpecReduce_2 5# happyReduction_4
happyReduction_4 (HappyAbsSyn9  happy_var_2)
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_2 : happy_var_1
        )
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0 6# happyReduction_5
happyReduction_5  =  HappyAbsSyn6
                 ([]
        )

happyReduce_6 = happySpecReduce_2 6# happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_2)
        (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn6
                 (happy_var_2 : happy_var_1
        )
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3 7# happyReduction_7
happyReduction_7 (HappyAbsSyn16  happy_var_3)
        _
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 ((happy_var_1, True, happy_var_3)
        )
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3 7# happyReduction_8
happyReduction_8 (HappyAbsSyn16  happy_var_3)
        _
        (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 ((happy_var_1, False, happy_var_3)
        )
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1 8# happyReduction_9
happyReduction_9 (HappyTerminal (T_id happy_var_1))
         =  HappyAbsSyn8
                 (happy_var_1
        )
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1 8# happyReduction_10
happyReduction_10 (HappyTerminal (T_type happy_var_1))
         =  HappyAbsSyn8
                 (happy_var_1
        )
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2 9# happyReduction_11
happyReduction_11 _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyMonadReduce 3# 9# happyReduction_12
happyReduction_12 (_ `HappyStk`
        (HappyAbsSyn18  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = happyThen ( let (Id i) = happy_var_2 in addIfaceTypedef i >>= \ v -> return (Forward v)
        ) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_13 = happySpecReduce_2 9# happyReduction_13
happyReduction_13 _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2 9# happyReduction_14
happyReduction_14 _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2 9# happyReduction_15
happyReduction_15 _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1 9# happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2 9# happyReduction_17
happyReduction_17 _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1 9# happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2 10# happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn9
                 (Attributed happy_var_1 happy_var_2
        )
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1 10# happyReduction_20
happyReduction_20 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1 11# happyReduction_21
happyReduction_21 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2 11# happyReduction_22
happyReduction_22 (HappyAbsSyn18  happy_var_2)
        _
         =  HappyAbsSyn9
                 (Forward happy_var_2
        )
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 6# 11# happyReduction_23
happyReduction_23 (_ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn5  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (Module happy_var_2 (reverse happy_var_4)
        ) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1 11# happyReduction_24
happyReduction_24 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happyMonadReduce 6# 11# happyReduction_25
happyReduction_25 (_ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn14  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = happyThen ( let (Id i) = happy_var_2 in addIfaceTypedef i >>= \ v -> return (CoClass v (reverse happy_var_4))
        ) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_26 = happyReduce 6# 11# happyReduction_26
happyReduction_26 (_ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn14  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (T_type happy_var_2)) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (CoClass (Id happy_var_2) (reverse happy_var_4)
        ) `HappyStk` happyRest

happyReduce_27 = happyReduce 5# 11# happyReduction_27
happyReduction_27 (_ `HappyStk`
        (HappyAbsSyn5  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (Library happy_var_2 (reverse happy_var_4)
        ) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_1 11# happyReduction_28
happyReduction_28 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happyMonadReduce 9# 12# happyReduction_29
happyReduction_29 (_ `HappyStk`
        (HappyAbsSyn5  happy_var_8) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn13  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_1) `HappyStk`
        happyRest)
         = happyThen ( let (Id i) = happy_var_1 in addIfaceTypedef i >>= \ v -> return (DispInterface v happy_var_5 (reverse happy_var_8))
        ) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_30 = happyMonadReduce 6# 12# happyReduction_30
happyReduction_30 (_ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_4) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_1) `HappyStk`
        happyRest)
         = happyThen ( let (Id i) = happy_var_1 in addIfaceTypedef i >>= \ v -> return (DispInterfaceDecl v happy_var_4)
        ) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_31 = happySpecReduce_0 13# happyReduction_31
happyReduction_31  =  HappyAbsSyn13
                 ([]
        )

happyReduce_32 = happyReduce 5# 13# happyReduction_32
happyReduction_32 (_ `HappyStk`
        (HappyAbsSyn18  happy_var_4) `HappyStk`
        (HappyAbsSyn30  happy_var_3) `HappyStk`
        (HappyAbsSyn16  happy_var_2) `HappyStk`
        (HappyAbsSyn13  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn13
                 ((happy_var_2,happy_var_3,happy_var_4):happy_var_1
        ) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_1 14# happyReduction_33
happyReduction_33 (HappyAbsSyn15  happy_var_1)
         =  HappyAbsSyn14
                 ([happy_var_1]
        )
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3 14# happyReduction_34
happyReduction_34 (HappyAbsSyn15  happy_var_3)
        _
        (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn14
                 (happy_var_3 : happy_var_1
        )
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3 15# happyReduction_35
happyReduction_35 (HappyTerminal (T_type happy_var_3))
        _
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn15
                 ((True,  Id happy_var_3, happy_var_1)
        )
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3 15# happyReduction_36
happyReduction_36 (HappyTerminal (T_type happy_var_3))
        _
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn15
                 ((False, Id happy_var_3, happy_var_1)
        )
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3 15# happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_3)
        _
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn15
                 ((True,  happy_var_3, happy_var_1)
        )
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3 15# happyReduction_38
happyReduction_38 (HappyAbsSyn18  happy_var_3)
        _
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn15
                 ((False, happy_var_3, happy_var_1)
        )
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0 16# happyReduction_39
happyReduction_39  =  HappyAbsSyn16
                 ([]
        )

happyReduce_40 = happySpecReduce_3 16# happyReduction_40
happyReduction_40 _
        (HappyAbsSyn16  happy_var_2)
        _
         =  HappyAbsSyn16
                 (happy_var_2
        )
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1 17# happyReduction_41
happyReduction_41 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn9
                 (Forward happy_var_1
        )
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happyReduce 5# 17# happyReduction_42
happyReduction_42 (_ `HappyStk`
        (HappyAbsSyn5  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn25  happy_var_2) `HappyStk`
        (HappyAbsSyn18  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (Interface happy_var_1 happy_var_2 (reverse happy_var_4)
        ) `HappyStk` happyRest

happyReduce_43 = happyMonadReduce 2# 18# happyReduction_43
happyReduction_43 ((HappyAbsSyn18  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = happyThen ( let (Id i) = happy_var_2 in addIfaceTypedef i >>= return
        ) (\r -> happyReturn (HappyAbsSyn18 r))

happyReduce_44 = happySpecReduce_2 18# happyReduction_44
happyReduction_44 (HappyTerminal (T_type happy_var_2))
        _
         =  HappyAbsSyn18
                 ((Id happy_var_2)
        )
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2 18# happyReduction_45
happyReduction_45 _
        _
         =  HappyAbsSyn18
                 ((Id "Object")
        )

happyReduce_46 = happyMonadReduce 2# 19# happyReduction_46
happyReduction_46 ((HappyAbsSyn18  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = happyThen ( let (Id i) = happy_var_2 in addIfaceTypedef i >>= return
        ) (\r -> happyReturn (HappyAbsSyn18 r))

happyReduce_47 = happySpecReduce_2 19# happyReduction_47
happyReduction_47 (HappyTerminal (T_type happy_var_2))
        _
         =  HappyAbsSyn18
                 ((Id happy_var_2)
        )
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 4# 20# happyReduction_48
happyReduction_48 (_ `HappyStk`
        (HappyTerminal (T_string_lit happy_var_3)) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (CppQuote happy_var_3
        ) `HappyStk` happyRest

happyReduce_49 = happyReduce 4# 21# happyReduction_49
happyReduction_49 (_ `HappyStk`
        (HappyTerminal (T_string_lit happy_var_3)) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (HsQuote happy_var_3
        ) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_1 21# happyReduction_50
happyReduction_50 (HappyTerminal (T_include happy_var_1))
         =  HappyAbsSyn9
                 (CInclude happy_var_1
        )
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_0 22# happyReduction_51
happyReduction_51  =  HappyAbsSyn5
                 ([]
        )

happyReduce_52 = happySpecReduce_2 22# happyReduction_52
happyReduction_52 (HappyAbsSyn9  happy_var_2)
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_2 : happy_var_1
        )
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2 23# happyReduction_53
happyReduction_53 _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_2 23# happyReduction_54
happyReduction_54 _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_54 _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2 23# happyReduction_55
happyReduction_55 _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_2 23# happyReduction_56
happyReduction_56 _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2 23# happyReduction_57
happyReduction_57 _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2 24# happyReduction_58
happyReduction_58 (HappyAbsSyn9  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn9
                 (Attributed happy_var_1 happy_var_2
        )
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1 24# happyReduction_59
happyReduction_59 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_0 25# happyReduction_60
happyReduction_60  =  HappyAbsSyn25
                 ([]
        )

happyReduce_61 = happySpecReduce_2 25# happyReduction_61
happyReduction_61 (HappyTerminal (T_type happy_var_2))
        _
         =  HappyAbsSyn25
                 ([ happy_var_2 ]
        )
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2 25# happyReduction_62
happyReduction_62 (HappyAbsSyn18  happy_var_2)
        _
         =  HappyAbsSyn25
                 (let (Id i) = happy_var_2 in [ i ]
        )
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happyMonadReduce 2# 26# happyReduction_63
happyReduction_63 ((HappyAbsSyn29  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = happyThen ( slurpImports (parseIDL >>= \ (Left y) -> return y) happy_var_2
        ) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_64 = happyMonadReduce 4# 26# happyReduction_64
happyReduction_64 (_ `HappyStk`
        (HappyTerminal (T_string_lit happy_var_3)) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = happyThen ( handleImportLib (parseIDL >>= \ (Left y) -> return y) happy_var_3
        ) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_65 = happySpecReduce_1 27# happyReduction_65
happyReduction_65 (HappyTerminal (T_pragma happy_var_1))
         =  HappyAbsSyn9
                 (Pragma happy_var_1
        )
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1 27# happyReduction_66
happyReduction_66 (HappyTerminal (T_include_start happy_var_1))
         =  HappyAbsSyn9
                 (IncludeStart happy_var_1
        )
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1 27# happyReduction_67
happyReduction_67 _
         =  HappyAbsSyn9
                 (IncludeEnd
        )

happyReduce_68 = happySpecReduce_3 28# happyReduction_68
happyReduction_68 (HappyAbsSyn34  happy_var_3)
        (HappyTerminal (T_id happy_var_2))
        _
         =  HappyAbsSyn9
                 (Constant (Id happy_var_2) [] (exprType (TyInteger Natural) happy_var_3) happy_var_3
        )
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1 29# happyReduction_69
happyReduction_69 (HappyTerminal (T_string_lit happy_var_1))
         =  HappyAbsSyn29
                 ([ happy_var_1 ]
        )
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3 29# happyReduction_70
happyReduction_70 (HappyAbsSyn29  happy_var_3)
        _
        (HappyTerminal (T_string_lit happy_var_1))
         =  HappyAbsSyn29
                 ((happy_var_1 : happy_var_3)
        )
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1 30# happyReduction_71
happyReduction_71 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1 30# happyReduction_72
happyReduction_72 _
         =  HappyAbsSyn30
                 (TyChar
        )

happyReduce_73 = happySpecReduce_1 30# happyReduction_73
happyReduction_73 _
         =  HappyAbsSyn30
                 (TyWChar
        )

happyReduce_74 = happySpecReduce_1 30# happyReduction_74
happyReduction_74 (HappyTerminal (T_float happy_var_1))
         =  HappyAbsSyn30
                 (TyFloat happy_var_1
        )
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1 30# happyReduction_75
happyReduction_75 _
         =  HappyAbsSyn30
                 (TyVoid
        )

happyReduce_76 = happySpecReduce_2 30# happyReduction_76
happyReduction_76 _
        _
         =  HappyAbsSyn30
                 (TyPointer TyVoid
        )

happyReduce_77 = happySpecReduce_2 30# happyReduction_77
happyReduction_77 _
        _
         =  HappyAbsSyn30
                 (TyString Nothing
        )

happyReduce_78 = happySpecReduce_2 30# happyReduction_78
happyReduction_78 _
        _
         =  HappyAbsSyn30
                 (TyWString Nothing
        )

happyReduce_79 = happySpecReduce_1 30# happyReduction_79
happyReduction_79 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3 30# happyReduction_80
happyReduction_80 _
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyArray happy_var_1 []
        )
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1 30# happyReduction_81
happyReduction_81 _
         =  HappyAbsSyn30
                 (TySigned True
        )

happyReduce_82 = happySpecReduce_1 30# happyReduction_82
happyReduction_82 _
         =  HappyAbsSyn30
                 (TySigned False
        )

happyReduce_83 = happySpecReduce_2 30# happyReduction_83
happyReduction_83 (HappyAbsSyn30  happy_var_2)
        _
         =  HappyAbsSyn30
                 (TyApply (TySigned False) happy_var_2
        )
happyReduction_83 _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_2 30# happyReduction_84
happyReduction_84 (HappyAbsSyn30  happy_var_2)
        _
         =  HappyAbsSyn30
                 (TyApply (TySigned True)  happy_var_2
        )
happyReduction_84 _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_2 30# happyReduction_85
happyReduction_85 _
        _
         =  HappyAbsSyn30
                 (TyApply (TySigned False) TyChar
        )

happyReduce_86 = happySpecReduce_3 30# happyReduction_86
happyReduction_86 _
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyApply (TySigned True)  happy_var_1
        )
happyReduction_86 _ _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_3 30# happyReduction_87
happyReduction_87 _
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyApply (TySigned False) happy_var_1
        )
happyReduction_87 _ _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_2 30# happyReduction_88
happyReduction_88 _
        _
         =  HappyAbsSyn30
                 (TyApply (TySigned True)  TyChar
        )

happyReduce_89 = happySpecReduce_1 30# happyReduction_89
happyReduction_89 (HappyTerminal (T_id happy_var_1))
         =  HappyAbsSyn30
                 (TyName happy_var_1 Nothing
        )
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1 30# happyReduction_90
happyReduction_90 (HappyTerminal (T_type happy_var_1))
         =  HappyAbsSyn30
                 (TyName happy_var_1 Nothing
        )
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_2 30# happyReduction_91
happyReduction_91 _
        (HappyTerminal (T_type happy_var_1))
         =  HappyAbsSyn30
                 (TyPointer (TyName happy_var_1 Nothing)
        )
happyReduction_91 _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1 30# happyReduction_92
happyReduction_92 (HappyTerminal (T_idl_type happy_var_1))
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happyMonadReduce 1# 30# happyReduction_93
happyReduction_93 (_ `HappyStk`
        happyRest)
         = happyThen ( dumpErrMsg >> return TyVoid
        ) (\r -> happyReturn (HappyAbsSyn30 r))

happyReduce_94 = happySpecReduce_1 31# happyReduction_94
happyReduction_94 _
         =  HappyAbsSyn30
                 (TyInteger Short
        )

happyReduce_95 = happySpecReduce_1 31# happyReduction_95
happyReduction_95 _
         =  HappyAbsSyn30
                 (TyInteger Long
        )

happyReduce_96 = happySpecReduce_2 31# happyReduction_96
happyReduction_96 _
        _
         =  HappyAbsSyn30
                 (TyInteger LongLong
        )

happyReduce_97 = happySpecReduce_1 31# happyReduction_97
happyReduction_97 _
         =  HappyAbsSyn30
                 (TyInteger LongLong
        )

happyReduce_98 = happySpecReduce_1 31# happyReduction_98
happyReduction_98 _
         =  HappyAbsSyn30
                 (TyApply (TySigned False) (TyInteger LongLong)
        )

happyReduce_99 = happySpecReduce_1 32# happyReduction_99
happyReduction_99 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_2 32# happyReduction_100
happyReduction_100 _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_100 _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_1 32# happyReduction_101
happyReduction_101 _
         =  HappyAbsSyn30
                 (TyInteger Natural
        )

happyReduce_102 = happySpecReduce_1 33# happyReduction_102
happyReduction_102 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_1 33# happyReduction_103
happyReduction_103 _
         =  HappyAbsSyn30
                 (TyChar
        )

happyReduce_104 = happySpecReduce_1 33# happyReduction_104
happyReduction_104 _
         =  HappyAbsSyn30
                 (TyWChar
        )

happyReduce_105 = happySpecReduce_1 33# happyReduction_105
happyReduction_105 (HappyTerminal (T_float happy_var_1))
         =  HappyAbsSyn30
                 (TyFloat happy_var_1
        )
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_1 33# happyReduction_106
happyReduction_106 _
         =  HappyAbsSyn30
                 (TyVoid
        )

happyReduce_107 = happySpecReduce_1 33# happyReduction_107
happyReduction_107 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_107 _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_3 33# happyReduction_108
happyReduction_108 _
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyArray happy_var_1 []
        )
happyReduction_108 _ _ _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_1 33# happyReduction_109
happyReduction_109 _
         =  HappyAbsSyn30
                 (TySigned True
        )

happyReduce_110 = happySpecReduce_1 33# happyReduction_110
happyReduction_110 _
         =  HappyAbsSyn30
                 (TySigned False
        )

happyReduce_111 = happySpecReduce_2 33# happyReduction_111
happyReduction_111 (HappyAbsSyn30  happy_var_2)
        _
         =  HappyAbsSyn30
                 (TyApply (TySigned False) happy_var_2
        )
happyReduction_111 _ _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_2 33# happyReduction_112
happyReduction_112 (HappyAbsSyn30  happy_var_2)
        _
         =  HappyAbsSyn30
                 (TyApply (TySigned True)  happy_var_2
        )
happyReduction_112 _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_3 33# happyReduction_113
happyReduction_113 _
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyApply (TySigned False) happy_var_1
        )
happyReduction_113 _ _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_3 33# happyReduction_114
happyReduction_114 _
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyApply (TySigned True)  happy_var_1
        )
happyReduction_114 _ _ _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_2 33# happyReduction_115
happyReduction_115 _
        _
         =  HappyAbsSyn30
                 (TyApply (TySigned False) TyChar
        )

happyReduce_116 = happySpecReduce_2 33# happyReduction_116
happyReduction_116 _
        _
         =  HappyAbsSyn30
                 (TyApply (TySigned True)  TyChar
        )

happyReduce_117 = happySpecReduce_1 33# happyReduction_117
happyReduction_117 (HappyTerminal (T_type happy_var_1))
         =  HappyAbsSyn30
                 (TyName happy_var_1 Nothing
        )
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_2 33# happyReduction_118
happyReduction_118 _
        _
         =  HappyAbsSyn30
                 (TyPointer TyVoid
        )

happyReduce_119 = happySpecReduce_1 33# happyReduction_119
happyReduction_119 (HappyTerminal (T_idl_type happy_var_1))
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happyMonadReduce 1# 33# happyReduction_120
happyReduction_120 (_ `HappyStk`
        happyRest)
         = happyThen ( dumpErrMsg >> return TyVoid
        ) (\r -> happyReturn (HappyAbsSyn30 r))

happyReduce_121 = happySpecReduce_1 34# happyReduction_121
happyReduction_121 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_121 _  = notHappyAtAll 

happyReduce_122 = happySpecReduce_1 35# happyReduction_122
happyReduction_122 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_122 _  = notHappyAtAll 

happyReduce_123 = happyReduce 5# 35# happyReduction_123
happyReduction_123 ((HappyAbsSyn34  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn34  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn34  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn34
                 (Cond happy_var_1 happy_var_3 happy_var_5
        ) `HappyStk` happyRest

happyReduce_124 = happySpecReduce_1 36# happyReduction_124
happyReduction_124 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_124 _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_3 36# happyReduction_125
happyReduction_125 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary LogOr happy_var_1 happy_var_3
        )
happyReduction_125 _ _ _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_1 37# happyReduction_126
happyReduction_126 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_126 _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_3 37# happyReduction_127
happyReduction_127 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary LogAnd happy_var_1 happy_var_3
        )
happyReduction_127 _ _ _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_1 38# happyReduction_128
happyReduction_128 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_3 38# happyReduction_129
happyReduction_129 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary Or happy_var_1 happy_var_3
        )
happyReduction_129 _ _ _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_1 39# happyReduction_130
happyReduction_130 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_130 _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_3 39# happyReduction_131
happyReduction_131 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary Xor happy_var_1 happy_var_3
        )
happyReduction_131 _ _ _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_1 40# happyReduction_132
happyReduction_132 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_132 _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_3 40# happyReduction_133
happyReduction_133 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary And happy_var_1 happy_var_3
        )
happyReduction_133 _ _ _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_1 41# happyReduction_134
happyReduction_134 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_134 _  = notHappyAtAll 

happyReduce_135 = happySpecReduce_3 41# happyReduction_135
happyReduction_135 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary Eq happy_var_1 happy_var_3
        )
happyReduction_135 _ _ _  = notHappyAtAll 

happyReduce_136 = happySpecReduce_1 42# happyReduction_136
happyReduction_136 _
         =  HappyAbsSyn42
                 (Eq
        )

happyReduce_137 = happySpecReduce_1 42# happyReduction_137
happyReduction_137 _
         =  HappyAbsSyn42
                 (Ne
        )

happyReduce_138 = happySpecReduce_1 43# happyReduction_138
happyReduction_138 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_138 _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_3 43# happyReduction_139
happyReduction_139 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary Lt happy_var_1 happy_var_3
        )
happyReduction_139 _ _ _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_3 43# happyReduction_140
happyReduction_140 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary Le happy_var_1 happy_var_3
        )
happyReduction_140 _ _ _  = notHappyAtAll 

happyReduce_141 = happySpecReduce_3 43# happyReduction_141
happyReduction_141 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary Ge happy_var_1 happy_var_3
        )
happyReduction_141 _ _ _  = notHappyAtAll 

happyReduce_142 = happySpecReduce_3 43# happyReduction_142
happyReduction_142 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary Gt happy_var_1 happy_var_3
        )
happyReduction_142 _ _ _  = notHappyAtAll 

happyReduce_143 = happySpecReduce_1 44# happyReduction_143
happyReduction_143 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_143 _  = notHappyAtAll 

happyReduce_144 = happySpecReduce_3 44# happyReduction_144
happyReduction_144 (HappyAbsSyn34  happy_var_3)
        (HappyTerminal (T_shift happy_var_2))
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary (Shift happy_var_2) happy_var_1 happy_var_3
        )
happyReduction_144 _ _ _  = notHappyAtAll 

happyReduce_145 = happySpecReduce_1 45# happyReduction_145
happyReduction_145 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_145 _  = notHappyAtAll 

happyReduce_146 = happySpecReduce_3 45# happyReduction_146
happyReduction_146 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary Add happy_var_1 happy_var_3
        )
happyReduction_146 _ _ _  = notHappyAtAll 

happyReduce_147 = happySpecReduce_3 45# happyReduction_147
happyReduction_147 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary Sub happy_var_1 happy_var_3
        )
happyReduction_147 _ _ _  = notHappyAtAll 

happyReduce_148 = happySpecReduce_1 46# happyReduction_148
happyReduction_148 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_148 _  = notHappyAtAll 

happyReduce_149 = happySpecReduce_3 46# happyReduction_149
happyReduction_149 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary Mul happy_var_1 happy_var_3
        )
happyReduction_149 _ _ _  = notHappyAtAll 

happyReduce_150 = happySpecReduce_3 46# happyReduction_150
happyReduction_150 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary Div happy_var_1 happy_var_3
        )
happyReduction_150 _ _ _  = notHappyAtAll 

happyReduce_151 = happySpecReduce_3 46# happyReduction_151
happyReduction_151 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (Binary Mod happy_var_1 happy_var_3
        )
happyReduction_151 _ _ _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_1 47# happyReduction_152
happyReduction_152 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_152 _  = notHappyAtAll 

happyReduce_153 = happyReduce 4# 47# happyReduction_153
happyReduction_153 ((HappyAbsSyn34  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn30  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn34
                 (Cast happy_var_2 happy_var_4
        ) `HappyStk` happyRest

happyReduce_154 = happySpecReduce_1 48# happyReduction_154
happyReduction_154 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn34
                 (happy_var_1
        )
happyReduction_154 _  = notHappyAtAll 

happyReduce_155 = happyReduce 4# 48# happyReduction_155
happyReduction_155 (_ `HappyStk`
        (HappyAbsSyn30  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn34
                 (Sizeof happy_var_3
        ) `HappyStk` happyRest

happyReduce_156 = happySpecReduce_2 48# happyReduction_156
happyReduction_156 (HappyAbsSyn34  happy_var_2)
        (HappyAbsSyn49  happy_var_1)
         =  HappyAbsSyn34
                 (Unary happy_var_1 happy_var_2
        )
happyReduction_156 _ _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_1 49# happyReduction_157
happyReduction_157 _
         =  HappyAbsSyn49
                 (Minus
        )

happyReduce_158 = happySpecReduce_1 49# happyReduction_158
happyReduction_158 _
         =  HappyAbsSyn49
                 (Plus
        )

happyReduce_159 = happySpecReduce_1 49# happyReduction_159
happyReduction_159 _
         =  HappyAbsSyn49
                 (Not
        )

happyReduce_160 = happySpecReduce_1 49# happyReduction_160
happyReduction_160 _
         =  HappyAbsSyn49
                 (Negate
        )

happyReduce_161 = happySpecReduce_1 50# happyReduction_161
happyReduction_161 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn34
                 (let (Id i) = happy_var_1 in Var i
        )
happyReduction_161 _  = notHappyAtAll 

happyReduce_162 = happySpecReduce_1 50# happyReduction_162
happyReduction_162 (HappyTerminal (T_literal happy_var_1))
         =  HappyAbsSyn34
                 (Lit happy_var_1
        )
happyReduction_162 _  = notHappyAtAll 

happyReduce_163 = happySpecReduce_1 50# happyReduction_163
happyReduction_163 (HappyTerminal (T_string_lit happy_var_1))
         =  HappyAbsSyn34
                 (Lit (StringLit happy_var_1)
        )
happyReduction_163 _  = notHappyAtAll 

happyReduce_164 = happySpecReduce_3 50# happyReduction_164
happyReduction_164 _
        (HappyAbsSyn34  happy_var_2)
        _
         =  HappyAbsSyn34
                 (happy_var_2
        )
happyReduction_164 _ _ _  = notHappyAtAll 

happyReduce_165 = happyMonadReduce 6# 51# happyReduction_165
happyReduction_165 (_ `HappyStk`
        (HappyAbsSyn63  happy_var_5) `HappyStk`
        (HappyAbsSyn30  happy_var_4) `HappyStk`
        (HappyAbsSyn16  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = happyThen ( let decls = reverse happy_var_5 in addTypes decls >> return (Typedef happy_var_4 happy_var_3 decls)
        ) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_166 = happySpecReduce_2 51# happyReduction_166
happyReduction_166 (HappyAbsSyn30  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn9
                 (Attributed happy_var_1 (TypeDecl happy_var_2)
        )
happyReduction_166 _ _  = notHappyAtAll 

happyReduce_167 = happySpecReduce_1 51# happyReduction_167
happyReduction_167 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn9
                 (TypeDecl happy_var_1
        )
happyReduction_167 _  = notHappyAtAll 

happyReduce_168 = happyReduce 6# 51# happyReduction_168
happyReduction_168 ((HappyAbsSyn34  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_4) `HappyStk`
        (HappyAbsSyn30  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn16  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (Constant happy_var_4 happy_var_1 happy_var_3 happy_var_6
        ) `HappyStk` happyRest

happyReduce_169 = happyReduce 5# 51# happyReduction_169
happyReduction_169 ((HappyAbsSyn34  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_3) `HappyStk`
        (HappyAbsSyn30  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (Constant happy_var_3 [] happy_var_2 happy_var_5
        ) `HappyStk` happyRest

happyReduce_170 = happyMonadReduce 6# 51# happyReduction_170
happyReduction_170 (_ `HappyStk`
        (HappyAbsSyn63  happy_var_5) `HappyStk`
        (HappyAbsSyn30  happy_var_4) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = happyThen ( let decls = reverse happy_var_5 in addTypes decls >> return (ExternDecl happy_var_4 decls)
        ) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_171 = happySpecReduce_1 52# happyReduction_171
happyReduction_171 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_171 _  = notHappyAtAll 

happyReduce_172 = happySpecReduce_3 52# happyReduction_172
happyReduction_172 _
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyArray happy_var_1 []
        )
happyReduction_172 _ _ _  = notHappyAtAll 

happyReduce_173 = happyReduce 4# 52# happyReduction_173
happyReduction_173 (_ `HappyStk`
        (HappyAbsSyn34  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn30  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn30
                 (TyArray happy_var_1 [happy_var_3]
        ) `HappyStk` happyRest

happyReduce_174 = happySpecReduce_2 52# happyReduction_174
happyReduction_174 (HappyAbsSyn54  happy_var_2)
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyApply (TyQualifier happy_var_2) happy_var_1
        )
happyReduction_174 _ _  = notHappyAtAll 

happyReduce_175 = happySpecReduce_2 52# happyReduction_175
happyReduction_175 (HappyAbsSyn30  happy_var_2)
        (HappyAbsSyn54  happy_var_1)
         =  HappyAbsSyn30
                 (TyApply (TyQualifier happy_var_1) happy_var_2
        )
happyReduction_175 _ _  = notHappyAtAll 

happyReduce_176 = happySpecReduce_1 53# happyReduction_176
happyReduction_176 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_176 _  = notHappyAtAll 

happyReduce_177 = happySpecReduce_3 53# happyReduction_177
happyReduction_177 _
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyArray happy_var_1 []
        )
happyReduction_177 _ _ _  = notHappyAtAll 

happyReduce_178 = happySpecReduce_2 53# happyReduction_178
happyReduction_178 (HappyAbsSyn54  happy_var_2)
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyApply (TyQualifier happy_var_2) happy_var_1
        )
happyReduction_178 _ _  = notHappyAtAll 

happyReduce_179 = happySpecReduce_1 54# happyReduction_179
happyReduction_179 _
         =  HappyAbsSyn54
                 (Const
        )

happyReduce_180 = happySpecReduce_1 54# happyReduction_180
happyReduction_180 _
         =  HappyAbsSyn54
                 (Volatile
        )

happyReduce_181 = happySpecReduce_1 55# happyReduction_181
happyReduction_181 (HappyTerminal (T_float happy_var_1))
         =  HappyAbsSyn30
                 (TyFloat happy_var_1
        )
happyReduction_181 _  = notHappyAtAll 

happyReduce_182 = happySpecReduce_1 55# happyReduction_182
happyReduction_182 _
         =  HappyAbsSyn30
                 (TyChar
        )

happyReduce_183 = happySpecReduce_1 55# happyReduction_183
happyReduction_183 _
         =  HappyAbsSyn30
                 (TyWChar
        )

happyReduce_184 = happySpecReduce_1 55# happyReduction_184
happyReduction_184 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_184 _  = notHappyAtAll 

happyReduce_185 = happySpecReduce_1 55# happyReduction_185
happyReduction_185 _
         =  HappyAbsSyn30
                 (TyVoid
        )

happyReduce_186 = happySpecReduce_1 55# happyReduction_186
happyReduction_186 _
         =  HappyAbsSyn30
                 (TySigned True
        )

happyReduce_187 = happySpecReduce_1 55# happyReduction_187
happyReduction_187 _
         =  HappyAbsSyn30
                 (TySigned False
        )

happyReduce_188 = happySpecReduce_2 55# happyReduction_188
happyReduction_188 (HappyAbsSyn30  happy_var_2)
        _
         =  HappyAbsSyn30
                 (TyApply (TySigned True) happy_var_2
        )
happyReduction_188 _ _  = notHappyAtAll 

happyReduce_189 = happySpecReduce_3 55# happyReduction_189
happyReduction_189 (HappyAbsSyn30  happy_var_3)
        _
        _
         =  HappyAbsSyn30
                 (TyApply (TySigned True) happy_var_3
        )
happyReduction_189 _ _ _  = notHappyAtAll 

happyReduce_190 = happySpecReduce_2 55# happyReduction_190
happyReduction_190 (HappyAbsSyn30  happy_var_2)
        _
         =  HappyAbsSyn30
                 (TyApply (TySigned False) happy_var_2
        )
happyReduction_190 _ _  = notHappyAtAll 

happyReduce_191 = happySpecReduce_3 55# happyReduction_191
happyReduction_191 (HappyAbsSyn30  happy_var_3)
        _
        _
         =  HappyAbsSyn30
                 (TyApply (TySigned False) happy_var_3
        )
happyReduction_191 _ _ _  = notHappyAtAll 

happyReduce_192 = happySpecReduce_3 55# happyReduction_192
happyReduction_192 _
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyApply (TySigned True)  happy_var_1
        )
happyReduction_192 _ _ _  = notHappyAtAll 

happyReduce_193 = happySpecReduce_3 55# happyReduction_193
happyReduction_193 _
        _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyApply (TySigned False) happy_var_1
        )
happyReduction_193 _ _ _  = notHappyAtAll 

happyReduce_194 = happySpecReduce_1 55# happyReduction_194
happyReduction_194 (HappyTerminal (T_id happy_var_1))
         =  HappyAbsSyn30
                 (TyName happy_var_1 Nothing
        )
happyReduction_194 _  = notHappyAtAll 

happyReduce_195 = happySpecReduce_1 55# happyReduction_195
happyReduction_195 (HappyTerminal (T_type happy_var_1))
         =  HappyAbsSyn30
                 (TyName happy_var_1 Nothing
        )
happyReduction_195 _  = notHappyAtAll 

happyReduce_196 = happySpecReduce_1 55# happyReduction_196
happyReduction_196 (HappyTerminal (T_idl_type happy_var_1))
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_196 _  = notHappyAtAll 

happyReduce_197 = happySpecReduce_3 55# happyReduction_197
happyReduction_197 _
        (HappyAbsSyn30  happy_var_2)
        _
         =  HappyAbsSyn30
                 (TySafeArray happy_var_2
        )
happyReduction_197 _ _ _  = notHappyAtAll 

happyReduce_198 = happySpecReduce_1 55# happyReduction_198
happyReduction_198 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_198 _  = notHappyAtAll 

happyReduce_199 = happySpecReduce_1 55# happyReduction_199
happyReduction_199 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_199 _  = notHappyAtAll 

happyReduce_200 = happyMonadReduce 1# 55# happyReduction_200
happyReduction_200 (_ `HappyStk`
        happyRest)
         = happyThen ( dumpErrMsg >> return TyVoid
        ) (\r -> happyReturn (HappyAbsSyn30 r))

happyReduce_201 = happySpecReduce_1 56# happyReduction_201
happyReduction_201 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_201 _  = notHappyAtAll 

happyReduce_202 = happySpecReduce_1 56# happyReduction_202
happyReduction_202 _
         =  HappyAbsSyn30
                 (TyChar
        )

happyReduce_203 = happySpecReduce_1 56# happyReduction_203
happyReduction_203 (HappyTerminal (T_float happy_var_1))
         =  HappyAbsSyn30
                 (TyFloat happy_var_1
        )
happyReduction_203 _  = notHappyAtAll 

happyReduce_204 = happySpecReduce_1 56# happyReduction_204
happyReduction_204 (HappyTerminal (T_type happy_var_1))
         =  HappyAbsSyn30
                 (TyName happy_var_1 Nothing
        )
happyReduction_204 _  = notHappyAtAll 

happyReduce_205 = happySpecReduce_1 56# happyReduction_205
happyReduction_205 (HappyTerminal (T_idl_type happy_var_1))
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_205 _  = notHappyAtAll 

happyReduce_206 = happySpecReduce_1 57# happyReduction_206
happyReduction_206 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_206 _  = notHappyAtAll 

happyReduce_207 = happySpecReduce_2 57# happyReduction_207
happyReduction_207 _
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (TyPointer happy_var_1
        )
happyReduction_207 _ _  = notHappyAtAll 

happyReduce_208 = happySpecReduce_1 58# happyReduction_208
happyReduction_208 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_208 _  = notHappyAtAll 

happyReduce_209 = happySpecReduce_1 58# happyReduction_209
happyReduction_209 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_209 _  = notHappyAtAll 

happyReduce_210 = happySpecReduce_1 59# happyReduction_210
happyReduction_210 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_210 _  = notHappyAtAll 

happyReduce_211 = happySpecReduce_1 59# happyReduction_211
happyReduction_211 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_211 _  = notHappyAtAll 

happyReduce_212 = happyReduce 6# 60# happyReduction_212
happyReduction_212 ((HappyAbsSyn109  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn73  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn30
                 (TyStruct (Just happy_var_2) (reverse happy_var_4) (toPackedAttrib happy_var_6)
        ) `HappyStk` happyRest

happyReduce_213 = happyReduce 5# 60# happyReduction_213
happyReduction_213 ((HappyAbsSyn109  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn73  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn30
                 (TyStruct Nothing (reverse happy_var_3) (toPackedAttrib happy_var_5)
        ) `HappyStk` happyRest

happyReduce_214 = happySpecReduce_2 60# happyReduction_214
happyReduction_214 (HappyAbsSyn18  happy_var_2)
        _
         =  HappyAbsSyn30
                 (TyStruct (Just happy_var_2) [] Nothing
        )
happyReduction_214 _ _  = notHappyAtAll 

happyReduce_215 = happyReduce 11# 61# happyReduction_215
happyReduction_215 (_ `HappyStk`
        (HappyAbsSyn77  happy_var_10) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn75  happy_var_8) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_6) `HappyStk`
        (HappyAbsSyn30  happy_var_5) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn75  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn30
                 (TyUnion happy_var_2 happy_var_5 happy_var_6 happy_var_8 (reverse happy_var_10)
        ) `HappyStk` happyRest

happyReduce_216 = happyReduce 6# 61# happyReduction_216
happyReduction_216 ((HappyAbsSyn109  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn62  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn75  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn30
                 (case happy_var_4 of { Left sw -> TyUnionNon happy_var_2 (reverse sw) ; Right mem -> TyCUnion happy_var_2 (reverse mem) (toPackedAttrib happy_var_6) }
        ) `HappyStk` happyRest

happyReduce_217 = happySpecReduce_3 61# happyReduction_217
happyReduction_217 (HappyAbsSyn109  happy_var_3)
        (HappyAbsSyn18  happy_var_2)
        _
         =  HappyAbsSyn30
                 (TyCUnion (Just happy_var_2) [] (toPackedAttrib happy_var_3)
        )
happyReduction_217 _ _ _  = notHappyAtAll 

happyReduce_218 = happySpecReduce_1 62# happyReduction_218
happyReduction_218 (HappyAbsSyn77  happy_var_1)
         =  HappyAbsSyn62
                 (Left happy_var_1
        )
happyReduction_218 _  = notHappyAtAll 

happyReduce_219 = happySpecReduce_1 62# happyReduction_219
happyReduction_219 (HappyAbsSyn73  happy_var_1)
         =  HappyAbsSyn62
                 (Right happy_var_1
        )
happyReduction_219 _  = notHappyAtAll 

happyReduce_220 = happySpecReduce_1 63# happyReduction_220
happyReduction_220 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn63
                 ([ happy_var_1 ]
        )
happyReduction_220 _  = notHappyAtAll 

happyReduce_221 = happySpecReduce_3 63# happyReduction_221
happyReduction_221 (HappyAbsSyn18  happy_var_3)
        _
        (HappyAbsSyn63  happy_var_1)
         =  HappyAbsSyn63
                 (happy_var_3 : happy_var_1
        )
happyReduction_221 _ _ _  = notHappyAtAll 

happyReduce_222 = happySpecReduce_2 64# happyReduction_222
happyReduction_222 (HappyAbsSyn18  happy_var_2)
        (HappyAbsSyn65  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1 happy_var_2
        )
happyReduction_222 _ _  = notHappyAtAll 

happyReduce_223 = happySpecReduce_1 64# happyReduction_223
happyReduction_223 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1
        )
happyReduction_223 _  = notHappyAtAll 

happyReduce_224 = happySpecReduce_2 64# happyReduction_224
happyReduction_224 (HappyAbsSyn18  happy_var_2)
        (HappyAbsSyn65  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1 happy_var_2
        )
happyReduction_224 _ _  = notHappyAtAll 

happyReduce_225 = happySpecReduce_1 64# happyReduction_225
happyReduction_225 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1
        )
happyReduction_225 _  = notHappyAtAll 

happyReduce_226 = happySpecReduce_1 65# happyReduction_226
happyReduction_226 (HappyAbsSyn109  happy_var_1)
         =  HappyAbsSyn65
                 (toCConvAttrib happy_var_1
        )
happyReduction_226 _  = notHappyAtAll 

happyReduce_227 = happySpecReduce_1 65# happyReduction_227
happyReduction_227 (HappyTerminal (T_callconv happy_var_1))
         =  HappyAbsSyn65
                 (CConvId happy_var_1
        )
happyReduction_227 _  = notHappyAtAll 

happyReduce_228 = happySpecReduce_1 66# happyReduction_228
happyReduction_228 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1
        )
happyReduction_228 _  = notHappyAtAll 

happyReduce_229 = happySpecReduce_3 66# happyReduction_229
happyReduction_229 _
        (HappyAbsSyn18  happy_var_2)
        _
         =  HappyAbsSyn18
                 (happy_var_2
        )
happyReduction_229 _ _ _  = notHappyAtAll 

happyReduce_230 = happySpecReduce_1 66# happyReduction_230
happyReduction_230 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1
        )
happyReduction_230 _  = notHappyAtAll 

happyReduce_231 = happySpecReduce_1 66# happyReduction_231
happyReduction_231 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1
        )
happyReduction_231 _  = notHappyAtAll 

happyReduce_232 = happySpecReduce_1 67# happyReduction_232
happyReduction_232 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1
        )
happyReduction_232 _  = notHappyAtAll 

happyReduce_233 = happySpecReduce_3 67# happyReduction_233
happyReduction_233 (HappyTerminal (T_literal happy_var_3))
        _
        (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 ((let { (Id nm) = happy_var_1 ; x = mkBitField nm happy_var_3 } in BitFieldId x happy_var_1)
        )
happyReduction_233 _ _ _  = notHappyAtAll 

happyReduce_234 = happySpecReduce_3 67# happyReduction_234
happyReduction_234 (HappyTerminal (T_literal happy_var_3))
        _
        (HappyTerminal (T_type happy_var_1))
         =  HappyAbsSyn18
                 ((let x = mkBitField happy_var_1 happy_var_3 in BitFieldId x (Id happy_var_1))
        )
happyReduction_234 _ _ _  = notHappyAtAll 

happyReduce_235 = happySpecReduce_2 67# happyReduction_235
happyReduction_235 (HappyTerminal (T_literal happy_var_2))
        _
         =  HappyAbsSyn18
                 ((let x = mkBitField "" happy_var_2 in BitFieldId x (Id ""))
        )
happyReduction_235 _ _  = notHappyAtAll 

happyReduce_236 = happySpecReduce_1 67# happyReduction_236
happyReduction_236 (HappyTerminal (T_type happy_var_1))
         =  HappyAbsSyn18
                 ((Id happy_var_1)
        )
happyReduction_236 _  = notHappyAtAll 

happyReduce_237 = happySpecReduce_1 67# happyReduction_237
happyReduction_237 (HappyTerminal (T_mode happy_var_1))
         =  HappyAbsSyn18
                 ((if happy_var_1 == In then Id "in" else Id "out")
        )
happyReduction_237 _  = notHappyAtAll 

happyReduce_238 = happySpecReduce_2 68# happyReduction_238
happyReduction_238 (HappyAbsSyn18  happy_var_2)
        (HappyAbsSyn71  happy_var_1)
         =  HappyAbsSyn18
                 (Pointed happy_var_1 happy_var_2
        )
happyReduction_238 _ _  = notHappyAtAll 

happyReduce_239 = happySpecReduce_3 68# happyReduction_239
happyReduction_239 (HappyAbsSyn18  happy_var_3)
        (HappyTerminal (T_callconv happy_var_2))
        (HappyAbsSyn71  happy_var_1)
         =  HappyAbsSyn18
                 (Pointed happy_var_1 (CConvId happy_var_2 happy_var_3)
        )
happyReduction_239 _ _ _  = notHappyAtAll 

happyReduce_240 = happySpecReduce_3 68# happyReduction_240
happyReduction_240 (HappyAbsSyn18  happy_var_3)
        (HappyAbsSyn109  happy_var_2)
        (HappyAbsSyn71  happy_var_1)
         =  HappyAbsSyn18
                 (Pointed happy_var_1 (toCConvAttrib happy_var_2 happy_var_3)
        )
happyReduction_240 _ _ _  = notHappyAtAll 

happyReduce_241 = happySpecReduce_3 69# happyReduction_241
happyReduction_241 _
        _
        (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (ArrayId happy_var_1 []
        )
happyReduction_241 _ _ _  = notHappyAtAll 

happyReduce_242 = happyReduce 4# 69# happyReduction_242
happyReduction_242 (_ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn18
                 (ArrayId happy_var_1 []
        ) `HappyStk` happyRest

happyReduce_243 = happyReduce 4# 69# happyReduction_243
happyReduction_243 (_ `HappyStk`
        (HappyAbsSyn34  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn18
                 (ArrayId happy_var_1 [happy_var_3]
        ) `HappyStk` happyRest

happyReduce_244 = happyReduce 7# 69# happyReduction_244
happyReduction_244 (_ `HappyStk`
        (HappyAbsSyn34  happy_var_6) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn34  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn18
                 (ArrayId happy_var_1 [happy_var_3,happy_var_6]
        ) `HappyStk` happyRest

happyReduce_245 = happyReduce 4# 70# happyReduction_245
happyReduction_245 (_ `HappyStk`
        (HappyAbsSyn106  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn18
                 (mkFunId happy_var_1 (reverse happy_var_3)
        ) `HappyStk` happyRest

happyReduce_246 = happySpecReduce_3 70# happyReduction_246
happyReduction_246 _
        _
        (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (mkFunId happy_var_1 []
        )
happyReduction_246 _ _ _  = notHappyAtAll 

happyReduce_247 = happySpecReduce_1 71# happyReduction_247
happyReduction_247 _
         =  HappyAbsSyn71
                 ([[]]
        )

happyReduce_248 = happySpecReduce_2 71# happyReduction_248
happyReduction_248 (HappyAbsSyn72  happy_var_2)
        _
         =  HappyAbsSyn71
                 ([happy_var_2]
        )
happyReduction_248 _ _  = notHappyAtAll 

happyReduce_249 = happySpecReduce_2 71# happyReduction_249
happyReduction_249 (HappyAbsSyn71  happy_var_2)
        _
         =  HappyAbsSyn71
                 ([] : happy_var_2
        )
happyReduction_249 _ _  = notHappyAtAll 

happyReduce_250 = happySpecReduce_3 71# happyReduction_250
happyReduction_250 (HappyAbsSyn71  happy_var_3)
        (HappyAbsSyn72  happy_var_2)
        _
         =  HappyAbsSyn71
                 (happy_var_2 : happy_var_3
        )
happyReduction_250 _ _ _  = notHappyAtAll 

happyReduce_251 = happySpecReduce_1 72# happyReduction_251
happyReduction_251 (HappyAbsSyn54  happy_var_1)
         =  HappyAbsSyn72
                 ([happy_var_1]
        )
happyReduction_251 _  = notHappyAtAll 

happyReduce_252 = happySpecReduce_2 72# happyReduction_252
happyReduction_252 (HappyAbsSyn54  happy_var_2)
        (HappyAbsSyn72  happy_var_1)
         =  HappyAbsSyn72
                 (happy_var_2 : happy_var_1
        )
happyReduction_252 _ _  = notHappyAtAll 

happyReduce_253 = happySpecReduce_3 73# happyReduction_253
happyReduction_253 _
        _
        (HappyAbsSyn74  happy_var_1)
         =  HappyAbsSyn73
                 ([ happy_var_1 ]
        )
happyReduction_253 _ _ _  = notHappyAtAll 

happyReduce_254 = happyReduce 4# 73# happyReduction_254
happyReduction_254 (_ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn74  happy_var_2) `HappyStk`
        (HappyAbsSyn73  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn73
                 (happy_var_2 : happy_var_1
        ) `HappyStk` happyRest

happyReduce_255 = happySpecReduce_3 74# happyReduction_255
happyReduction_255 (HappyAbsSyn63  happy_var_3)
        (HappyAbsSyn30  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn74
                 ((happy_var_2, happy_var_1, reverse happy_var_3)
        )
happyReduction_255 _ _ _  = notHappyAtAll 

happyReduce_256 = happySpecReduce_2 74# happyReduction_256
happyReduction_256 (HappyAbsSyn30  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn74
                 ((happy_var_2, happy_var_1, [])
        )
happyReduction_256 _ _  = notHappyAtAll 

happyReduce_257 = happySpecReduce_0 75# happyReduction_257
happyReduction_257  =  HappyAbsSyn75
                 (Nothing
        )

happyReduce_258 = happySpecReduce_1 75# happyReduction_258
happyReduction_258 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn75
                 (Just happy_var_1
        )
happyReduction_258 _  = notHappyAtAll 

happyReduce_259 = happySpecReduce_1 76# happyReduction_259
happyReduction_259 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_259 _  = notHappyAtAll 

happyReduce_260 = happySpecReduce_1 76# happyReduction_260
happyReduction_260 _
         =  HappyAbsSyn30
                 (TyChar
        )

happyReduce_261 = happySpecReduce_1 76# happyReduction_261
happyReduction_261 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_261 _  = notHappyAtAll 

happyReduce_262 = happySpecReduce_1 76# happyReduction_262
happyReduction_262 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_262 _  = notHappyAtAll 

happyReduce_263 = happySpecReduce_1 76# happyReduction_263
happyReduction_263 (HappyTerminal (T_id happy_var_1))
         =  HappyAbsSyn30
                 (TyName happy_var_1 Nothing
        )
happyReduction_263 _  = notHappyAtAll 

happyReduce_264 = happySpecReduce_1 76# happyReduction_264
happyReduction_264 (HappyTerminal (T_type happy_var_1))
         =  HappyAbsSyn30
                 (TyName happy_var_1 Nothing
        )
happyReduction_264 _  = notHappyAtAll 

happyReduce_265 = happySpecReduce_2 77# happyReduction_265
happyReduction_265 _
        (HappyAbsSyn78  happy_var_1)
         =  HappyAbsSyn77
                 ([ happy_var_1 ]
        )
happyReduction_265 _ _  = notHappyAtAll 

happyReduce_266 = happySpecReduce_3 77# happyReduction_266
happyReduction_266 _
        (HappyAbsSyn78  happy_var_2)
        (HappyAbsSyn77  happy_var_1)
         =  HappyAbsSyn77
                 (happy_var_2 : happy_var_1
        )
happyReduction_266 _ _ _  = notHappyAtAll 

happyReduce_267 = happySpecReduce_2 78# happyReduction_267
happyReduction_267 (HappyAbsSyn79  happy_var_2)
        (HappyAbsSyn81  happy_var_1)
         =  HappyAbsSyn78
                 (Switch  happy_var_1  happy_var_2
        )
happyReduction_267 _ _  = notHappyAtAll 

happyReduce_268 = happyReduce 4# 78# happyReduction_268
happyReduction_268 ((HappyAbsSyn79  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn82  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn78
                 (Switch [happy_var_2] happy_var_4
        ) `HappyStk` happyRest

happyReduce_269 = happySpecReduce_0 79# happyReduction_269
happyReduction_269  =  HappyAbsSyn79
                 (Nothing
        )

happyReduce_270 = happySpecReduce_2 79# happyReduction_270
happyReduction_270 (HappyAbsSyn18  happy_var_2)
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn79
                 (Just (Param happy_var_2 happy_var_1 [])
        )
happyReduction_270 _ _  = notHappyAtAll 

happyReduce_271 = happySpecReduce_1 79# happyReduction_271
happyReduction_271 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn79
                 (Just (Param (Id "") happy_var_1 [])
        )
happyReduction_271 _  = notHappyAtAll 

happyReduce_272 = happySpecReduce_0 80# happyReduction_272
happyReduction_272  =  HappyAbsSyn79
                 (Nothing
        )

happyReduce_273 = happySpecReduce_3 80# happyReduction_273
happyReduction_273 (HappyAbsSyn18  happy_var_3)
        (HappyAbsSyn30  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn79
                 (Just (Param happy_var_3 happy_var_2 happy_var_1)
        )
happyReduction_273 _ _ _  = notHappyAtAll 

happyReduce_274 = happySpecReduce_2 80# happyReduction_274
happyReduction_274 (HappyAbsSyn30  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn79
                 (Just (Param (Id "") happy_var_2 happy_var_1)
        )
happyReduction_274 _ _  = notHappyAtAll 

happyReduce_275 = happySpecReduce_1 81# happyReduction_275
happyReduction_275 (HappyAbsSyn82  happy_var_1)
         =  HappyAbsSyn81
                 ([ happy_var_1 ]
        )
happyReduction_275 _  = notHappyAtAll 

happyReduce_276 = happySpecReduce_2 81# happyReduction_276
happyReduction_276 (HappyAbsSyn82  happy_var_2)
        (HappyAbsSyn81  happy_var_1)
         =  HappyAbsSyn81
                 (happy_var_2 : happy_var_1
        )
happyReduction_276 _ _  = notHappyAtAll 

happyReduce_277 = happySpecReduce_3 82# happyReduction_277
happyReduction_277 _
        (HappyAbsSyn34  happy_var_2)
        _
         =  HappyAbsSyn82
                 (Case [happy_var_2]
        )
happyReduction_277 _ _ _  = notHappyAtAll 

happyReduce_278 = happySpecReduce_2 82# happyReduction_278
happyReduction_278 _
        _
         =  HappyAbsSyn82
                 (Default
        )

happyReduce_279 = happyReduce 4# 83# happyReduction_279
happyReduction_279 (_ `HappyStk`
        (HappyAbsSyn84  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn82
                 (Case (reverse happy_var_3)
        ) `HappyStk` happyRest

happyReduce_280 = happySpecReduce_1 83# happyReduction_280
happyReduction_280 _
         =  HappyAbsSyn82
                 (Default
        )

happyReduce_281 = happySpecReduce_1 84# happyReduction_281
happyReduction_281 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn84
                 ([ happy_var_1 ]
        )
happyReduction_281 _  = notHappyAtAll 

happyReduce_282 = happySpecReduce_3 84# happyReduction_282
happyReduction_282 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn84  happy_var_1)
         =  HappyAbsSyn84
                 (happy_var_3 : happy_var_1
        )
happyReduction_282 _ _ _  = notHappyAtAll 

happyReduce_283 = happySpecReduce_0 85# happyReduction_283
happyReduction_283  =  HappyAbsSyn84
                 ([]
        )

happyReduce_284 = happySpecReduce_1 85# happyReduction_284
happyReduction_284 (HappyAbsSyn84  happy_var_1)
         =  HappyAbsSyn84
                 (happy_var_1
        )
happyReduction_284 _  = notHappyAtAll 

happyReduce_285 = happyReduce 5# 86# happyReduction_285
happyReduction_285 (_ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn87  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn30
                 (TyEnum Nothing (reverse happy_var_3)
        ) `HappyStk` happyRest

happyReduce_286 = happyReduce 6# 86# happyReduction_286
happyReduction_286 (_ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn87  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn30
                 (TyEnum (Just happy_var_2) (reverse happy_var_4)
        ) `HappyStk` happyRest

happyReduce_287 = happySpecReduce_2 86# happyReduction_287
happyReduction_287 (HappyAbsSyn18  happy_var_2)
        _
         =  HappyAbsSyn30
                 (TyEnum (Just happy_var_2) []
        )
happyReduction_287 _ _  = notHappyAtAll 

happyReduce_288 = happySpecReduce_1 87# happyReduction_288
happyReduction_288 (HappyAbsSyn88  happy_var_1)
         =  HappyAbsSyn87
                 ([ happy_var_1 ]
        )
happyReduction_288 _  = notHappyAtAll 

happyReduce_289 = happySpecReduce_3 87# happyReduction_289
happyReduction_289 (HappyAbsSyn88  happy_var_3)
        _
        (HappyAbsSyn87  happy_var_1)
         =  HappyAbsSyn87
                 (happy_var_3 : happy_var_1
        )
happyReduction_289 _ _ _  = notHappyAtAll 

happyReduce_290 = happySpecReduce_1 88# happyReduction_290
happyReduction_290 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn88
                 ((happy_var_1, [], Nothing)
        )
happyReduction_290 _  = notHappyAtAll 

happyReduce_291 = happySpecReduce_2 88# happyReduction_291
happyReduction_291 (HappyAbsSyn18  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn88
                 ((happy_var_2, happy_var_1, Nothing)
        )
happyReduction_291 _ _  = notHappyAtAll 

happyReduce_292 = happySpecReduce_3 88# happyReduction_292
happyReduction_292 (HappyAbsSyn34  happy_var_3)
        _
        (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn88
                 ((happy_var_1, [], Just happy_var_3)
        )
happyReduction_292 _ _ _  = notHappyAtAll 

happyReduce_293 = happyReduce 4# 88# happyReduction_293
happyReduction_293 ((HappyAbsSyn34  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_2) `HappyStk`
        (HappyAbsSyn16  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn88
                 ((happy_var_2, happy_var_1, Just happy_var_4)
        ) `HappyStk` happyRest

happyReduce_294 = happyReduce 4# 89# happyReduction_294
happyReduction_294 (_ `HappyStk`
        (HappyAbsSyn34  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn90  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn30
                 (happy_var_1 (Just happy_var_3)
        ) `HappyStk` happyRest

happyReduce_295 = happySpecReduce_1 89# happyReduction_295
happyReduction_295 (HappyAbsSyn90  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1 Nothing
        )
happyReduction_295 _  = notHappyAtAll 

happyReduce_296 = happySpecReduce_1 90# happyReduction_296
happyReduction_296 _
         =  HappyAbsSyn90
                 (TyString
        )

happyReduce_297 = happySpecReduce_1 90# happyReduction_297
happyReduction_297 _
         =  HappyAbsSyn90
                 (TyWString
        )

happyReduce_298 = happySpecReduce_0 91# happyReduction_298
happyReduction_298  =  HappyAbsSyn5
                 ([]
        )

happyReduce_299 = happySpecReduce_3 91# happyReduction_299
happyReduction_299 _
        (HappyAbsSyn9  happy_var_2)
        (HappyAbsSyn5  happy_var_1)
         =  HappyAbsSyn5
                 (happy_var_2 : happy_var_1
        )
happyReduction_299 _ _ _  = notHappyAtAll 

happyReduce_300 = happyReduce 4# 92# happyReduction_300
happyReduction_300 (_ `HappyStk`
        (HappyAbsSyn18  happy_var_3) `HappyStk`
        (HappyAbsSyn30  happy_var_2) `HappyStk`
        (HappyAbsSyn16  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (let m_id = mkMethodId happy_var_3 in (Attributed happy_var_1 (Operation m_id happy_var_2 Nothing Nothing))
        ) `HappyStk` happyRest

happyReduce_301 = happySpecReduce_3 93# happyReduction_301
happyReduction_301 _
        (HappyAbsSyn18  happy_var_2)
        (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn9
                 (let m_id = mkMethodId happy_var_2 in (Operation m_id happy_var_1 Nothing Nothing)
        )
happyReduction_301 _ _ _  = notHappyAtAll 

happyReduce_302 = happySpecReduce_1 94# happyReduction_302
happyReduction_302 (HappyAbsSyn95  happy_var_1)
         =  HappyAbsSyn16
                 (concat happy_var_1
        )
happyReduction_302 _  = notHappyAtAll 

happyReduce_303 = happySpecReduce_0 95# happyReduction_303
happyReduction_303  =  HappyAbsSyn95
                 ([]
        )

happyReduce_304 = happySpecReduce_2 95# happyReduction_304
happyReduction_304 (HappyAbsSyn95  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn95
                 (happy_var_1 : happy_var_2
        )
happyReduction_304 _ _  = notHappyAtAll 

happyReduce_305 = happySpecReduce_2 96# happyReduction_305
happyReduction_305 _
        _
         =  HappyAbsSyn16
                 ([]
        )

happyReduce_306 = happyReduce 4# 96# happyReduction_306
happyReduction_306 (_ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn16  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn16
                 ((reverse happy_var_2)
        ) `HappyStk` happyRest

happyReduce_307 = happySpecReduce_1 97# happyReduction_307
happyReduction_307 (HappyAbsSyn98  happy_var_1)
         =  HappyAbsSyn16
                 ([ happy_var_1 ]
        )
happyReduction_307 _  = notHappyAtAll 

happyReduce_308 = happySpecReduce_3 97# happyReduction_308
happyReduction_308 (HappyAbsSyn98  happy_var_3)
        _
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn16
                 (happy_var_3 : happy_var_1
        )
happyReduction_308 _ _ _  = notHappyAtAll 

happyReduce_309 = happySpecReduce_2 98# happyReduction_309
happyReduction_309 (HappyAbsSyn101  happy_var_2)
        (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn98
                 (Attrib happy_var_1 happy_var_2
        )
happyReduction_309 _ _  = notHappyAtAll 

happyReduce_310 = happySpecReduce_1 98# happyReduction_310
happyReduction_310 _
         =  HappyAbsSyn98
                 (Attrib (Id "string") []
        )

happyReduce_311 = happySpecReduce_1 98# happyReduction_311
happyReduction_311 (HappyTerminal (T_mode happy_var_1))
         =  HappyAbsSyn98
                 (Mode happy_var_1
        )
happyReduction_311 _  = notHappyAtAll 

happyReduce_312 = happySpecReduce_1 99# happyReduction_312
happyReduction_312 (HappyAbsSyn98  happy_var_1)
         =  HappyAbsSyn16
                 ([ happy_var_1 ]
        )
happyReduction_312 _  = notHappyAtAll 

happyReduce_313 = happySpecReduce_3 99# happyReduction_313
happyReduction_313 (HappyAbsSyn98  happy_var_3)
        _
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn16
                 (happy_var_3 : happy_var_1
        )
happyReduction_313 _ _ _  = notHappyAtAll 

happyReduce_314 = happySpecReduce_1 100# happyReduction_314
happyReduction_314 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn98
                 (Attrib happy_var_1 []
        )
happyReduction_314 _  = notHappyAtAll 

happyReduce_315 = happySpecReduce_1 100# happyReduction_315
happyReduction_315 _
         =  HappyAbsSyn98
                 (Attrib (Id "default") []
        )

happyReduce_316 = happySpecReduce_0 101# happyReduction_316
happyReduction_316  =  HappyAbsSyn101
                 ([]
        )

happyReduce_317 = happySpecReduce_3 101# happyReduction_317
happyReduction_317 _
        (HappyAbsSyn101  happy_var_2)
        _
         =  HappyAbsSyn101
                 ((reverse happy_var_2)
        )
happyReduction_317 _ _ _  = notHappyAtAll 

happyReduce_318 = happySpecReduce_1 102# happyReduction_318
happyReduction_318 (HappyAbsSyn103  happy_var_1)
         =  HappyAbsSyn101
                 ([happy_var_1]
        )
happyReduction_318 _  = notHappyAtAll 

happyReduce_319 = happySpecReduce_3 102# happyReduction_319
happyReduction_319 (HappyAbsSyn103  happy_var_3)
        _
        (HappyAbsSyn101  happy_var_1)
         =  HappyAbsSyn101
                 (happy_var_3:happy_var_1
        )
happyReduction_319 _ _ _  = notHappyAtAll 

happyReduce_320 = happySpecReduce_1 103# happyReduction_320
happyReduction_320 (HappyAbsSyn34  happy_var_1)
         =  HappyAbsSyn103
                 ((AttrExpr happy_var_1)
        )
happyReduction_320 _  = notHappyAtAll 

happyReduce_321 = happySpecReduce_0 103# happyReduction_321
happyReduction_321  =  HappyAbsSyn103
                 (EmptyAttr
        )

happyReduce_322 = happySpecReduce_1 103# happyReduction_322
happyReduction_322 (HappyTerminal (T_type happy_var_1))
         =  HappyAbsSyn103
                 ((AttrLit (TypeConst happy_var_1))
        )
happyReduction_322 _  = notHappyAtAll 

happyReduce_323 = happySpecReduce_2 103# happyReduction_323
happyReduction_323 (HappyTerminal (T_type happy_var_2))
        _
         =  HappyAbsSyn103
                 ((AttrLit (TypeConst ("unsigned " ++ happy_var_2)))
        )
happyReduction_323 _ _  = notHappyAtAll 

happyReduce_324 = happySpecReduce_2 103# happyReduction_324
happyReduction_324 (HappyTerminal (T_type happy_var_2))
        _
         =  HappyAbsSyn103
                 ((AttrLit (TypeConst ("signed " ++ happy_var_2)))
        )
happyReduction_324 _ _  = notHappyAtAll 

happyReduce_325 = happySpecReduce_3 103# happyReduction_325
happyReduction_325 _
        (HappyTerminal (T_literal happy_var_2))
        _
         =  HappyAbsSyn103
                 (AttrLit happy_var_2
        )
happyReduction_325 _ _ _  = notHappyAtAll 

happyReduce_326 = happySpecReduce_2 103# happyReduction_326
happyReduction_326 (HappyAbsSyn103  happy_var_2)
        _
         =  HappyAbsSyn103
                 ((AttrPtr happy_var_2)
        )
happyReduction_326 _ _  = notHappyAtAll 

happyReduce_327 = happySpecReduce_1 104# happyReduction_327
happyReduction_327 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_327 _  = notHappyAtAll 

happyReduce_328 = happySpecReduce_1 104# happyReduction_328
happyReduction_328 (HappyAbsSyn30  happy_var_1)
         =  HappyAbsSyn30
                 (happy_var_1
        )
happyReduction_328 _  = notHappyAtAll 

happyReduce_329 = happySpecReduce_2 105# happyReduction_329
happyReduction_329 (HappyAbsSyn30  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn105
                 (Param (Id "") happy_var_2 happy_var_1
        )
happyReduction_329 _ _  = notHappyAtAll 

happyReduce_330 = happySpecReduce_3 105# happyReduction_330
happyReduction_330 (HappyAbsSyn18  happy_var_3)
        (HappyAbsSyn30  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn105
                 (Param happy_var_3 happy_var_2 happy_var_1
        )
happyReduction_330 _ _ _  = notHappyAtAll 

happyReduce_331 = happySpecReduce_3 105# happyReduction_331
happyReduction_331 (HappyAbsSyn18  happy_var_3)
        (HappyAbsSyn30  happy_var_2)
        (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn105
                 (Param happy_var_3 happy_var_2 happy_var_1
        )
happyReduction_331 _ _ _  = notHappyAtAll 

happyReduce_332 = happySpecReduce_1 105# happyReduction_332
happyReduction_332 _
         =  HappyAbsSyn105
                 (Param (Id "vararg") TyVoid []
        )

happyReduce_333 = happySpecReduce_1 106# happyReduction_333
happyReduction_333 (HappyAbsSyn105  happy_var_1)
         =  HappyAbsSyn106
                 ([happy_var_1]
        )
happyReduction_333 _  = notHappyAtAll 

happyReduce_334 = happySpecReduce_3 106# happyReduction_334
happyReduction_334 (HappyAbsSyn105  happy_var_3)
        _
        (HappyAbsSyn106  happy_var_1)
         =  HappyAbsSyn106
                 (happy_var_3 : happy_var_1
        )
happyReduction_334 _ _ _  = notHappyAtAll 

happyReduce_335 = happySpecReduce_1 107# happyReduction_335
happyReduction_335 (HappyAbsSyn71  happy_var_1)
         =  HappyAbsSyn18
                 (Pointed happy_var_1 (Id "")
        )
happyReduction_335 _  = notHappyAtAll 

happyReduce_336 = happySpecReduce_2 107# happyReduction_336
happyReduction_336 (HappyAbsSyn18  happy_var_2)
        (HappyAbsSyn65  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1 happy_var_2
        )
happyReduction_336 _ _  = notHappyAtAll 

happyReduce_337 = happySpecReduce_1 107# happyReduction_337
happyReduction_337 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1
        )
happyReduction_337 _  = notHappyAtAll 

happyReduce_338 = happySpecReduce_2 107# happyReduction_338
happyReduction_338 (HappyAbsSyn18  happy_var_2)
        (HappyAbsSyn71  happy_var_1)
         =  HappyAbsSyn18
                 (Pointed happy_var_1 happy_var_2
        )
happyReduction_338 _ _  = notHappyAtAll 

happyReduce_339 = happySpecReduce_3 108# happyReduction_339
happyReduction_339 _
        (HappyAbsSyn18  happy_var_2)
        _
         =  HappyAbsSyn18
                 (happy_var_2
        )
happyReduction_339 _ _ _  = notHappyAtAll 

happyReduce_340 = happySpecReduce_2 108# happyReduction_340
happyReduction_340 _
        _
         =  HappyAbsSyn18
                 (FunId (Id "") Nothing []
        )

happyReduce_341 = happySpecReduce_3 108# happyReduction_341
happyReduction_341 _
        (HappyAbsSyn106  happy_var_2)
        _
         =  HappyAbsSyn18
                 (FunId (Id "") Nothing happy_var_2
        )
happyReduction_341 _ _ _  = notHappyAtAll 

happyReduce_342 = happySpecReduce_3 108# happyReduction_342
happyReduction_342 _
        _
        (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (FunId happy_var_1 Nothing []
        )
happyReduction_342 _ _ _  = notHappyAtAll 

happyReduce_343 = happyReduce 4# 108# happyReduction_343
happyReduction_343 (_ `HappyStk`
        (HappyAbsSyn106  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn18
                 (FunId happy_var_1 Nothing happy_var_3
        ) `HappyStk` happyRest

happyReduce_344 = happySpecReduce_0 109# happyReduction_344
happyReduction_344  =  HappyAbsSyn109
                 ([]
        )

happyReduce_345 = happySpecReduce_1 109# happyReduction_345
happyReduction_345 (HappyAbsSyn109  happy_var_1)
         =  HappyAbsSyn109
                 ((reverse happy_var_1)
        )
happyReduction_345 _  = notHappyAtAll 

happyReduce_346 = happySpecReduce_1 110# happyReduction_346
happyReduction_346 (HappyAbsSyn111  happy_var_1)
         =  HappyAbsSyn109
                 ([happy_var_1]
        )
happyReduction_346 _  = notHappyAtAll 

happyReduce_347 = happySpecReduce_2 110# happyReduction_347
happyReduction_347 (HappyAbsSyn111  happy_var_2)
        (HappyAbsSyn109  happy_var_1)
         =  HappyAbsSyn109
                 ((happy_var_2:happy_var_1)
        )
happyReduction_347 _ _  = notHappyAtAll 

happyReduce_348 = happyReduce 6# 111# happyReduction_348
happyReduction_348 (_ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn111  happy_var_4) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn111
                 (happy_var_4
        ) `HappyStk` happyRest

happyReduce_349 = happySpecReduce_1 112# happyReduction_349
happyReduction_349 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn111
                 (mkGNUAttrib happy_var_1 []
        )
happyReduction_349 _  = notHappyAtAll 

happyReduce_350 = happySpecReduce_1 112# happyReduction_350
happyReduction_350 (HappyTerminal (T_callconv happy_var_1))
         =  HappyAbsSyn111
                 (CConv happy_var_1
        )
happyReduction_350 _  = notHappyAtAll 

happyReduce_351 = happyReduce 4# 112# happyReduction_351
happyReduction_351 (_ `HappyStk`
        (HappyTerminal (T_id happy_var_3)) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn8  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn111
                 (mkGNUAttrib happy_var_1 [Var happy_var_3]
        ) `HappyStk` happyRest

happyReduce_352 = happyReduce 6# 112# happyReduction_352
happyReduction_352 (_ `HappyStk`
        (HappyAbsSyn84  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyTerminal (T_id happy_var_3)) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn8  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn111
                 (mkGNUAttrib happy_var_1 (Var happy_var_3:happy_var_5)
        ) `HappyStk` happyRest

happyReduce_353 = happySpecReduce_1 113# happyReduction_353
happyReduction_353 (HappyTerminal (T_id happy_var_1))
         =  HappyAbsSyn8
                 (happy_var_1
        )
happyReduction_353 _  = notHappyAtAll 

happyReduce_354 = happySpecReduce_1 113# happyReduction_354
happyReduction_354 (HappyTerminal (T_type happy_var_1))
         =  HappyAbsSyn8
                 (happy_var_1
        )
happyReduction_354 _  = notHappyAtAll 

happyReduce_355 = happySpecReduce_1 114# happyReduction_355
happyReduction_355 (HappyTerminal (T_id happy_var_1))
         =  HappyAbsSyn18
                 ((Id happy_var_1)
        )
happyReduction_355 _  = notHappyAtAll 

happyReduce_356 = happySpecReduce_1 115# happyReduction_356
happyReduction_356 _
         =  HappyAbsSyn115
                 (()
        )

happyReduce_357 = happySpecReduce_1 115# happyReduction_357
happyReduction_357 _
         =  HappyAbsSyn115
                 (()
        )

happyReduce_358 = happySpecReduce_0 116# happyReduction_358
happyReduction_358  =  HappyAbsSyn115
                 (()
        )

happyReduce_359 = happySpecReduce_1 116# happyReduction_359
happyReduction_359 _
         =  HappyAbsSyn115
                 (()
        )

happyNewToken action sts stk
        = lexIDL(\tk -> 
        let cont i = action i i tk (HappyState action) sts stk in
        case tk of {
        T_eof -> action 199# 199# (error "reading EOF!") (HappyState action) sts stk;
        T_semi -> cont 117#;
        T_module -> cont 118#;
        T_interface -> cont 119#;
        T_oparen -> cont 120#;
        T_cparen -> cont 121#;
        T_ocurly -> cont 122#;
        T_ccurly -> cont 123#;
        T_colon -> cont 124#;
        T_comma -> cont 125#;
        T_dot -> cont 126#;
        T_dotdotdot -> cont 127#;
        T_const -> cont 128#;
        T_volatile -> cont 129#;
        T_equal -> cont 130#;
        T_eqeq -> cont 131#;
        T_neq -> cont 132#;
        T_or -> cont 133#;
        T_rel_or -> cont 134#;
        T_xor -> cont 135#;
        T_and -> cont 136#;
        T_rel_and -> cont 137#;
        T_shift happy_dollar_dollar -> cont 138#;
        T_div -> cont 139#;
        T_mod -> cont 140#;
        T_not -> cont 141#;
        T_negate -> cont 142#;
        T_question -> cont 143#;
        T_typedef -> cont 144#;
        T_extern -> cont 145#;
        T_type happy_dollar_dollar -> cont 146#;
        T_idl_type happy_dollar_dollar -> cont 147#;
        T_float happy_dollar_dollar -> cont 148#;
        (T_int Short) -> cont 149#;
        (T_int Long) -> cont 150#;
        (T_int LongLong) -> cont 151#;
        (T_uint LongLong) -> cont 152#;
        (T_int Natural) -> cont 153#;
        T_unsigned -> cont 154#;
        T_signed -> cont 155#;
        T_char -> cont 156#;
        T_wchar -> cont 157#;
        T_struct -> cont 158#;
        T_union -> cont 159#;
        T_switch -> cont 160#;
        T_case -> cont 161#;
        T_default -> cont 162#;
        T_enum -> cont 163#;
        T_lt -> cont 164#;
        T_le -> cont 165#;
        T_gt -> cont 166#;
        T_ge -> cont 167#;
        T_osquare -> cont 168#;
        T_csquare -> cont 169#;
        T_sizeof -> cont 170#;
        T_void -> cont 171#;
        T_mode happy_dollar_dollar -> cont 172#;
        T_literal happy_dollar_dollar -> cont 173#;
        T_string_lit happy_dollar_dollar -> cont 174#;
        T_callconv happy_dollar_dollar -> cont 175#;
        T_id happy_dollar_dollar -> cont 176#;
        T_dispinterface -> cont 177#;
        T_coclass -> cont 178#;
        T_library -> cont 179#;
        T_plus -> cont 180#;
        T_times -> cont 181#;
        T_minus -> cont 182#;
        T_string -> cont 183#;
        T_wstring -> cont 184#;
        T_methods -> cont 185#;
        T_properties -> cont 186#;
        T_cpp_quote -> cont 187#;
        T_hs_quote -> cont 188#;
        T_include happy_dollar_dollar -> cont 189#;
        T_importlib -> cont 190#;
        T_include_start happy_dollar_dollar -> cont 191#;
        T_include_end -> cont 192#;
        T_gnu_attribute -> cont 193#;
        T_import -> cont 194#;
        T_pragma happy_dollar_dollar -> cont 195#;
        T_hdefine -> cont 196#;
        T_safearray -> cont 197#;
        T_unknown happy_dollar_dollar -> cont 198#;
        _ -> happyError
        })

happyThen :: LexM a -> (a -> LexM b) -> LexM b
happyThen = (thenLexM)
happyReturn :: a -> LexM a
happyReturn = (returnLexM)
happyThen1 = happyThen
happyReturn1 = happyReturn

parseIDL = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq

addTypes :: [Id] -> LexM ()
addTypes ids = do
  sequence (map addTypedef ls)
  return ()
 where
  ls = map getName ids

  getName (Id s) = s
  getName (ArrayId i _) = getName i
  getName (Pointed _ i) = getName i
  getName (CConvId _ i) = getName i
  getName (FunId i _ _) = getName i

addIfaceTypedef :: String -> LexM Id
addIfaceTypedef nm = addTypedef nm >> return (Id nm)

mkBitField :: String -> Literal -> Int
mkBitField nm l = 
  case l of
    IntegerLit (ILit _ i) -> fromInteger i
    _ -> error ("bitfield " ++ show nm ++ " not an int.")

warningMsg :: String -> LexM ()
warningMsg msg = do
  l <- getSrcLoc
  ioToLexM (hPutStrLn stderr (show l ++ ": warning: "++msg))

dumpErrMsg :: LexM ()
dumpErrMsg = do
 l   <- getSrcLoc
 str <- getStream
 ioToLexM (ioError (userError (show l ++ ": Parse error on input: " ++ takeWhile (/='\n') str)))

happyError :: LexM a
happyError = do
 l   <- getSrcLoc
 str <- getStream
 ioToLexM (ioError (userError (show l ++ ": Parse error: " ++ takeWhile (/='\n') str)))
{-# LINE 1 "GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.24 2003/06/03 09:41:51 ross Exp 













{-# LINE 27 "GenericTemplate.hs" #-}










































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts (HappyStk ans _) = (happyTcHack j 
                                                  )
                                           (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 150 "GenericTemplate.hs" #-}


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int# ->                    -- token number
         Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (1# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  1# tk old_st _ stk =
--      trace "failing" $ 
        happyError


{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  1# tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action 1# 1# tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action 1# 1# tk (HappyState (action)) sts ( (HappyErrorToken (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
