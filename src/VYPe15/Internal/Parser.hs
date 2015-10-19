{-# OPTIONS_GHC -w #-}
module VYPe15.Internal.Parser where

import Data.Char 

import VYPe15.Types.AST 
import VYPe15.Types.Tokens (Token(..))
import VYPe15.Types.Parser (Parser)
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16

action_0 (45) = happyShift action_7
action_0 (46) = happyShift action_8
action_0 (47) = happyShift action_9
action_0 (48) = happyShift action_10
action_0 (4) = happyGoto action_11
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 (13) = happyGoto action_5
action_0 (14) = happyGoto action_6
action_0 _ = happyFail

action_1 (45) = happyShift action_7
action_1 (46) = happyShift action_8
action_1 (47) = happyShift action_9
action_1 (48) = happyShift action_10
action_1 (4) = happyGoto action_2
action_1 (5) = happyGoto action_3
action_1 (6) = happyGoto action_4
action_1 (13) = happyGoto action_5
action_1 (14) = happyGoto action_6
action_1 _ = happyFail

action_2 (45) = happyShift action_7
action_2 (46) = happyShift action_8
action_2 (47) = happyShift action_9
action_2 (48) = happyShift action_10
action_2 (5) = happyGoto action_12
action_2 (6) = happyGoto action_13
action_2 (13) = happyGoto action_5
action_2 (14) = happyGoto action_6
action_2 _ = happyFail

action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 _ = happyReduce_48

action_6 (27) = happyShift action_15
action_6 (16) = happyGoto action_14
action_6 _ = happyFail

action_7 _ = happyReduce_46

action_8 _ = happyReduce_45

action_9 _ = happyReduce_44

action_10 _ = happyReduce_47

action_11 (45) = happyShift action_7
action_11 (46) = happyShift action_8
action_11 (47) = happyShift action_9
action_11 (48) = happyShift action_10
action_11 (50) = happyAccept
action_11 (5) = happyGoto action_12
action_11 (6) = happyGoto action_13
action_11 (13) = happyGoto action_5
action_11 (14) = happyGoto action_6
action_11 _ = happyFail

action_12 _ = happyReduce_1

action_13 _ = happyReduce_2

action_14 (43) = happyShift action_16
action_14 _ = happyFail

action_15 _ = happyReduce_51

action_16 (45) = happyShift action_7
action_16 (46) = happyShift action_8
action_16 (47) = happyShift action_9
action_16 (48) = happyShift action_20
action_16 (7) = happyGoto action_17
action_16 (8) = happyGoto action_18
action_16 (13) = happyGoto action_19
action_16 _ = happyFail

action_17 (44) = happyShift action_25
action_17 (49) = happyShift action_26
action_17 _ = happyFail

action_18 (44) = happyShift action_23
action_18 (49) = happyShift action_24
action_18 _ = happyFail

action_19 (27) = happyShift action_15
action_19 (16) = happyGoto action_22
action_19 _ = happyReduce_11

action_20 (44) = happyShift action_21
action_20 _ = happyFail

action_21 (21) = happyShift action_31
action_21 (22) = happyShift action_32
action_21 _ = happyFail

action_22 _ = happyReduce_9

action_23 (21) = happyShift action_30
action_23 _ = happyFail

action_24 (45) = happyShift action_7
action_24 (46) = happyShift action_8
action_24 (47) = happyShift action_9
action_24 (13) = happyGoto action_29
action_24 _ = happyFail

action_25 (22) = happyShift action_28
action_25 _ = happyFail

action_26 (45) = happyShift action_7
action_26 (46) = happyShift action_8
action_26 (47) = happyShift action_9
action_26 (13) = happyGoto action_27
action_26 _ = happyFail

action_27 (27) = happyShift action_15
action_27 (16) = happyGoto action_35
action_27 _ = happyFail

action_28 (9) = happyGoto action_34
action_28 _ = happyReduce_13

action_29 _ = happyReduce_12

action_30 _ = happyReduce_7

action_31 _ = happyReduce_8

action_32 (9) = happyGoto action_33
action_32 _ = happyReduce_13

action_33 (17) = happyShift action_39
action_33 (19) = happyShift action_40
action_33 (20) = happyShift action_41
action_33 (23) = happyShift action_43
action_33 (27) = happyShift action_15
action_33 (45) = happyShift action_7
action_33 (46) = happyShift action_8
action_33 (47) = happyShift action_9
action_33 (10) = happyGoto action_36
action_33 (13) = happyGoto action_37
action_33 (16) = happyGoto action_38
action_33 _ = happyFail

action_34 (17) = happyShift action_39
action_34 (19) = happyShift action_40
action_34 (20) = happyShift action_41
action_34 (23) = happyShift action_42
action_34 (27) = happyShift action_15
action_34 (45) = happyShift action_7
action_34 (46) = happyShift action_8
action_34 (47) = happyShift action_9
action_34 (10) = happyGoto action_36
action_34 (13) = happyGoto action_37
action_34 (16) = happyGoto action_38
action_34 _ = happyFail

action_35 _ = happyReduce_10

action_36 _ = happyReduce_14

action_37 (27) = happyShift action_15
action_37 (16) = happyGoto action_55
action_37 _ = happyFail

action_38 (28) = happyShift action_53
action_38 (43) = happyShift action_54
action_38 _ = happyFail

action_39 (43) = happyShift action_52
action_39 _ = happyFail

action_40 (21) = happyShift action_46
action_40 (24) = happyShift action_47
action_40 (25) = happyShift action_48
action_40 (26) = happyShift action_49
action_40 (42) = happyShift action_50
action_40 (43) = happyShift action_51
action_40 (12) = happyGoto action_45
action_40 _ = happyFail

action_41 (43) = happyShift action_44
action_41 _ = happyFail

action_42 _ = happyReduce_5

action_43 _ = happyReduce_6

action_44 (24) = happyShift action_47
action_44 (25) = happyShift action_48
action_44 (26) = happyShift action_49
action_44 (42) = happyShift action_50
action_44 (43) = happyShift action_51
action_44 (12) = happyGoto action_78
action_44 _ = happyFail

action_45 (21) = happyShift action_64
action_45 (29) = happyShift action_65
action_45 (30) = happyShift action_66
action_45 (31) = happyShift action_67
action_45 (32) = happyShift action_68
action_45 (33) = happyShift action_69
action_45 (34) = happyShift action_70
action_45 (35) = happyShift action_71
action_45 (36) = happyShift action_72
action_45 (37) = happyShift action_73
action_45 (38) = happyShift action_74
action_45 (39) = happyShift action_75
action_45 (40) = happyShift action_76
action_45 (41) = happyShift action_77
action_45 _ = happyFail

action_46 _ = happyReduce_18

action_47 _ = happyReduce_40

action_48 _ = happyReduce_42

action_49 _ = happyReduce_41

action_50 (24) = happyShift action_47
action_50 (25) = happyShift action_48
action_50 (26) = happyShift action_49
action_50 (42) = happyShift action_50
action_50 (43) = happyShift action_51
action_50 (12) = happyGoto action_63
action_50 _ = happyFail

action_51 (24) = happyShift action_47
action_51 (25) = happyShift action_48
action_51 (26) = happyShift action_49
action_51 (42) = happyShift action_50
action_51 (43) = happyShift action_51
action_51 (45) = happyShift action_7
action_51 (46) = happyShift action_8
action_51 (47) = happyShift action_9
action_51 (12) = happyGoto action_61
action_51 (13) = happyGoto action_62
action_51 _ = happyFail

action_52 (24) = happyShift action_47
action_52 (25) = happyShift action_48
action_52 (26) = happyShift action_49
action_52 (42) = happyShift action_50
action_52 (43) = happyShift action_51
action_52 (12) = happyGoto action_60
action_52 _ = happyFail

action_53 (24) = happyShift action_47
action_53 (25) = happyShift action_48
action_53 (26) = happyShift action_49
action_53 (42) = happyShift action_50
action_53 (43) = happyShift action_51
action_53 (12) = happyGoto action_59
action_53 _ = happyFail

action_54 (24) = happyShift action_47
action_54 (25) = happyShift action_48
action_54 (26) = happyShift action_49
action_54 (42) = happyShift action_50
action_54 (43) = happyShift action_51
action_54 (11) = happyGoto action_57
action_54 (12) = happyGoto action_58
action_54 _ = happyReduce_22

action_55 (15) = happyGoto action_56
action_55 _ = happyReduce_49

action_56 (21) = happyShift action_99
action_56 (49) = happyShift action_100
action_56 _ = happyFail

action_57 (44) = happyShift action_97
action_57 (49) = happyShift action_98
action_57 _ = happyFail

action_58 (29) = happyShift action_65
action_58 (30) = happyShift action_66
action_58 (31) = happyShift action_67
action_58 (32) = happyShift action_68
action_58 (33) = happyShift action_69
action_58 (34) = happyShift action_70
action_58 (35) = happyShift action_71
action_58 (36) = happyShift action_72
action_58 (37) = happyShift action_73
action_58 (38) = happyShift action_74
action_58 (39) = happyShift action_75
action_58 (40) = happyShift action_76
action_58 (41) = happyShift action_77
action_58 _ = happyReduce_24

action_59 (21) = happyShift action_96
action_59 (29) = happyShift action_65
action_59 (30) = happyShift action_66
action_59 (31) = happyShift action_67
action_59 (32) = happyShift action_68
action_59 (33) = happyShift action_69
action_59 (34) = happyShift action_70
action_59 (35) = happyShift action_71
action_59 (36) = happyShift action_72
action_59 (37) = happyShift action_73
action_59 (38) = happyShift action_74
action_59 (39) = happyShift action_75
action_59 (40) = happyShift action_76
action_59 (41) = happyShift action_77
action_59 _ = happyFail

action_60 (29) = happyShift action_65
action_60 (30) = happyShift action_66
action_60 (31) = happyShift action_67
action_60 (32) = happyShift action_68
action_60 (33) = happyShift action_69
action_60 (34) = happyShift action_70
action_60 (35) = happyShift action_71
action_60 (36) = happyShift action_72
action_60 (37) = happyShift action_73
action_60 (38) = happyShift action_74
action_60 (39) = happyShift action_75
action_60 (40) = happyShift action_76
action_60 (41) = happyShift action_77
action_60 (44) = happyShift action_95
action_60 _ = happyFail

action_61 (29) = happyShift action_65
action_61 (30) = happyShift action_66
action_61 (31) = happyShift action_67
action_61 (32) = happyShift action_68
action_61 (33) = happyShift action_69
action_61 (34) = happyShift action_70
action_61 (35) = happyShift action_71
action_61 (36) = happyShift action_72
action_61 (37) = happyShift action_73
action_61 (38) = happyShift action_74
action_61 (39) = happyShift action_75
action_61 (40) = happyShift action_76
action_61 (41) = happyShift action_77
action_61 (44) = happyShift action_94
action_61 _ = happyFail

action_62 (44) = happyShift action_93
action_62 _ = happyFail

action_63 _ = happyReduce_38

action_64 _ = happyReduce_17

action_65 (24) = happyShift action_47
action_65 (25) = happyShift action_48
action_65 (26) = happyShift action_49
action_65 (42) = happyShift action_50
action_65 (43) = happyShift action_51
action_65 (12) = happyGoto action_92
action_65 _ = happyFail

action_66 (24) = happyShift action_47
action_66 (25) = happyShift action_48
action_66 (26) = happyShift action_49
action_66 (42) = happyShift action_50
action_66 (43) = happyShift action_51
action_66 (12) = happyGoto action_91
action_66 _ = happyFail

action_67 (24) = happyShift action_47
action_67 (25) = happyShift action_48
action_67 (26) = happyShift action_49
action_67 (42) = happyShift action_50
action_67 (43) = happyShift action_51
action_67 (12) = happyGoto action_90
action_67 _ = happyFail

action_68 (24) = happyShift action_47
action_68 (25) = happyShift action_48
action_68 (26) = happyShift action_49
action_68 (42) = happyShift action_50
action_68 (43) = happyShift action_51
action_68 (12) = happyGoto action_89
action_68 _ = happyFail

action_69 (24) = happyShift action_47
action_69 (25) = happyShift action_48
action_69 (26) = happyShift action_49
action_69 (42) = happyShift action_50
action_69 (43) = happyShift action_51
action_69 (12) = happyGoto action_88
action_69 _ = happyFail

action_70 (24) = happyShift action_47
action_70 (25) = happyShift action_48
action_70 (26) = happyShift action_49
action_70 (42) = happyShift action_50
action_70 (43) = happyShift action_51
action_70 (12) = happyGoto action_87
action_70 _ = happyFail

action_71 (24) = happyShift action_47
action_71 (25) = happyShift action_48
action_71 (26) = happyShift action_49
action_71 (42) = happyShift action_50
action_71 (43) = happyShift action_51
action_71 (12) = happyGoto action_86
action_71 _ = happyFail

action_72 (24) = happyShift action_47
action_72 (25) = happyShift action_48
action_72 (26) = happyShift action_49
action_72 (42) = happyShift action_50
action_72 (43) = happyShift action_51
action_72 (12) = happyGoto action_85
action_72 _ = happyFail

action_73 (24) = happyShift action_47
action_73 (25) = happyShift action_48
action_73 (26) = happyShift action_49
action_73 (42) = happyShift action_50
action_73 (43) = happyShift action_51
action_73 (12) = happyGoto action_84
action_73 _ = happyFail

action_74 (24) = happyShift action_47
action_74 (25) = happyShift action_48
action_74 (26) = happyShift action_49
action_74 (42) = happyShift action_50
action_74 (43) = happyShift action_51
action_74 (12) = happyGoto action_83
action_74 _ = happyFail

action_75 (24) = happyShift action_47
action_75 (25) = happyShift action_48
action_75 (26) = happyShift action_49
action_75 (42) = happyShift action_50
action_75 (43) = happyShift action_51
action_75 (12) = happyGoto action_82
action_75 _ = happyFail

action_76 (24) = happyShift action_47
action_76 (25) = happyShift action_48
action_76 (26) = happyShift action_49
action_76 (42) = happyShift action_50
action_76 (43) = happyShift action_51
action_76 (12) = happyGoto action_81
action_76 _ = happyFail

action_77 (24) = happyShift action_47
action_77 (25) = happyShift action_48
action_77 (26) = happyShift action_49
action_77 (42) = happyShift action_50
action_77 (43) = happyShift action_51
action_77 (12) = happyGoto action_80
action_77 _ = happyFail

action_78 (29) = happyShift action_65
action_78 (30) = happyShift action_66
action_78 (31) = happyShift action_67
action_78 (32) = happyShift action_68
action_78 (33) = happyShift action_69
action_78 (34) = happyShift action_70
action_78 (35) = happyShift action_71
action_78 (36) = happyShift action_72
action_78 (37) = happyShift action_73
action_78 (38) = happyShift action_74
action_78 (39) = happyShift action_75
action_78 (40) = happyShift action_76
action_78 (41) = happyShift action_77
action_78 (44) = happyShift action_79
action_78 _ = happyFail

action_79 (22) = happyShift action_106
action_79 _ = happyFail

action_80 (29) = happyShift action_65
action_80 (30) = happyShift action_66
action_80 (31) = happyShift action_67
action_80 (32) = happyShift action_68
action_80 (33) = happyShift action_69
action_80 (34) = happyShift action_70
action_80 (35) = happyShift action_71
action_80 (36) = happyShift action_72
action_80 (37) = happyShift action_73
action_80 (38) = happyShift action_74
action_80 (39) = happyShift action_75
action_80 (40) = happyShift action_76
action_80 _ = happyReduce_25

action_81 (29) = happyShift action_65
action_81 (30) = happyShift action_66
action_81 (31) = happyShift action_67
action_81 (32) = happyShift action_68
action_81 (33) = happyShift action_69
action_81 (34) = happyShift action_70
action_81 (35) = happyShift action_71
action_81 (36) = happyShift action_72
action_81 (37) = happyShift action_73
action_81 (38) = happyShift action_74
action_81 (39) = happyShift action_75
action_81 _ = happyReduce_26

action_82 (29) = happyShift action_65
action_82 (30) = happyShift action_66
action_82 (31) = happyShift action_67
action_82 (32) = happyShift action_68
action_82 (33) = happyShift action_69
action_82 (34) = happyShift action_70
action_82 (35) = happyShift action_71
action_82 (36) = happyShift action_72
action_82 (37) = happyShift action_73
action_82 _ = happyReduce_28

action_83 (29) = happyShift action_65
action_83 (30) = happyShift action_66
action_83 (31) = happyShift action_67
action_83 (32) = happyShift action_68
action_83 (33) = happyShift action_69
action_83 (34) = happyShift action_70
action_83 (35) = happyShift action_71
action_83 (36) = happyShift action_72
action_83 (37) = happyShift action_73
action_83 _ = happyReduce_27

action_84 (29) = happyShift action_65
action_84 (30) = happyShift action_66
action_84 (31) = happyShift action_67
action_84 (32) = happyShift action_68
action_84 (33) = happyShift action_69
action_84 _ = happyReduce_32

action_85 (29) = happyShift action_65
action_85 (30) = happyShift action_66
action_85 (31) = happyShift action_67
action_85 (32) = happyShift action_68
action_85 (33) = happyShift action_69
action_85 _ = happyReduce_31

action_86 (29) = happyShift action_65
action_86 (30) = happyShift action_66
action_86 (31) = happyShift action_67
action_86 (32) = happyShift action_68
action_86 (33) = happyShift action_69
action_86 _ = happyReduce_30

action_87 (29) = happyShift action_65
action_87 (30) = happyShift action_66
action_87 (31) = happyShift action_67
action_87 (32) = happyShift action_68
action_87 (33) = happyShift action_69
action_87 _ = happyReduce_29

action_88 (29) = happyShift action_65
action_88 (30) = happyShift action_66
action_88 _ = happyReduce_37

action_89 (29) = happyShift action_65
action_89 (30) = happyShift action_66
action_89 _ = happyReduce_36

action_90 (29) = happyShift action_65
action_90 (30) = happyShift action_66
action_90 _ = happyReduce_35

action_91 _ = happyReduce_34

action_92 _ = happyReduce_33

action_93 (24) = happyShift action_47
action_93 (25) = happyShift action_48
action_93 (26) = happyShift action_49
action_93 (42) = happyShift action_50
action_93 (43) = happyShift action_51
action_93 (12) = happyGoto action_105
action_93 _ = happyFail

action_94 _ = happyReduce_43

action_95 (22) = happyShift action_104
action_95 _ = happyFail

action_96 _ = happyReduce_15

action_97 (21) = happyShift action_103
action_97 _ = happyFail

action_98 (24) = happyShift action_47
action_98 (25) = happyShift action_48
action_98 (26) = happyShift action_49
action_98 (42) = happyShift action_50
action_98 (43) = happyShift action_51
action_98 (12) = happyGoto action_102
action_98 _ = happyFail

action_99 _ = happyReduce_20

action_100 (27) = happyShift action_15
action_100 (16) = happyGoto action_101
action_100 _ = happyFail

action_101 _ = happyReduce_50

action_102 (29) = happyShift action_65
action_102 (30) = happyShift action_66
action_102 (31) = happyShift action_67
action_102 (32) = happyShift action_68
action_102 (33) = happyShift action_69
action_102 (34) = happyShift action_70
action_102 (35) = happyShift action_71
action_102 (36) = happyShift action_72
action_102 (37) = happyShift action_73
action_102 (38) = happyShift action_74
action_102 (39) = happyShift action_75
action_102 (40) = happyShift action_76
action_102 (41) = happyShift action_77
action_102 _ = happyReduce_23

action_103 _ = happyReduce_21

action_104 (9) = happyGoto action_108
action_104 _ = happyReduce_13

action_105 _ = happyReduce_39

action_106 (9) = happyGoto action_107
action_106 _ = happyReduce_13

action_107 (17) = happyShift action_39
action_107 (19) = happyShift action_40
action_107 (20) = happyShift action_41
action_107 (23) = happyShift action_110
action_107 (27) = happyShift action_15
action_107 (45) = happyShift action_7
action_107 (46) = happyShift action_8
action_107 (47) = happyShift action_9
action_107 (10) = happyGoto action_36
action_107 (13) = happyGoto action_37
action_107 (16) = happyGoto action_38
action_107 _ = happyFail

action_108 (17) = happyShift action_39
action_108 (19) = happyShift action_40
action_108 (20) = happyShift action_41
action_108 (23) = happyShift action_109
action_108 (27) = happyShift action_15
action_108 (45) = happyShift action_7
action_108 (46) = happyShift action_8
action_108 (47) = happyShift action_9
action_108 (10) = happyGoto action_36
action_108 (13) = happyGoto action_37
action_108 (16) = happyGoto action_38
action_108 _ = happyFail

action_109 (18) = happyShift action_111
action_109 _ = happyFail

action_110 _ = happyReduce_19

action_111 (22) = happyShift action_112
action_111 _ = happyFail

action_112 (9) = happyGoto action_113
action_112 _ = happyReduce_13

action_113 (17) = happyShift action_39
action_113 (19) = happyShift action_40
action_113 (20) = happyShift action_41
action_113 (23) = happyShift action_114
action_113 (27) = happyShift action_15
action_113 (45) = happyShift action_7
action_113 (46) = happyShift action_8
action_113 (47) = happyShift action_9
action_113 (10) = happyGoto action_36
action_113 (13) = happyGoto action_37
action_113 (16) = happyGoto action_38
action_113 _ = happyFail

action_114 _ = happyReduce_16

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (reverse $ happy_var_2:happy_var_1
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (reverse $ happy_var_2:happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 8 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (FunDef happy_var_1 happy_var_2 (Just $ reverse happy_var_4) happy_var_7
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 8 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (FunDef happy_var_1 happy_var_2 Nothing happy_var_7
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 6 6 happyReduction_7
happyReduction_7 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (FunDeclr happy_var_1 happy_var_2 (Just $ reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 6 6 happyReduction_8
happyReduction_8 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (FunDeclr happy_var_1 happy_var_2 Nothing
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  7 happyReduction_9
happyReduction_9 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn7
		 ([Param happy_var_1 happy_var_2]
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 7 happyReduction_10
happyReduction_10 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((Param happy_var_3 happy_var_4):happy_var_1
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_3:happy_var_1
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  9 happyReduction_13
happyReduction_13  =  HappyAbsSyn9
		 ([]
	)

happyReduce_14 = happySpecReduce_2  9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_2 : happy_var_1
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 10 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Assign happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 11 10 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (If happy_var_3 (reverse happy_var_6) (reverse happy_var_10)
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  10 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Return (Just happy_var_2)
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  10 happyReduction_18
happyReduction_18 _
	_
	 =  HappyAbsSyn10
		 (Return Nothing
	)

happyReduce_19 = happyReduce 7 10 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (While happy_var_3 (reverse happy_var_6)
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 10 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (VarDef (happy_var_2:happy_var_3)
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 5 10 happyReduction_21
happyReduction_21 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (FuncCall happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_0  11 happyReduction_22
happyReduction_22  =  HappyAbsSyn11
		 ([]
	)

happyReduce_23 = happySpecReduce_3  11 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3:happy_var_1
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  11 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  12 happyReduction_25
happyReduction_25 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (OR happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  12 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AND happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  12 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  12 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (NonEq happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  12 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Less happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  12 happyReduction_30
happyReduction_30 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Greater happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  12 happyReduction_31
happyReduction_31 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (LessEq happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  12 happyReduction_32
happyReduction_32 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (GreaterEq happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  12 happyReduction_33
happyReduction_33 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  12 happyReduction_34
happyReduction_34 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  12 happyReduction_35
happyReduction_35 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Times happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  12 happyReduction_36
happyReduction_36 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Div happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  12 happyReduction_37
happyReduction_37 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Mod happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  12 happyReduction_38
happyReduction_38 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (NOT happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 12 happyReduction_39
happyReduction_39 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Cast happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_1  12 happyReduction_40
happyReduction_40 (HappyTerminal (TokenNumConst happy_var_1))
	 =  HappyAbsSyn12
		 (ConsNum happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  12 happyReduction_41
happyReduction_41 (HappyTerminal (TokenStringConst happy_var_1))
	 =  HappyAbsSyn12
		 (ConsString happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  12 happyReduction_42
happyReduction_42 (HappyTerminal (TokenCharConst happy_var_1))
	 =  HappyAbsSyn12
		 (ConsChar happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  12 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Bracket happy_var_2
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  13 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn13
		 (Int
	)

happyReduce_45 = happySpecReduce_1  13 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn13
		 (Char
	)

happyReduce_46 = happySpecReduce_1  13 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn13
		 (String
	)

happyReduce_47 = happySpecReduce_1  14 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn14
		 (Nothing
	)

happyReduce_48 = happySpecReduce_1  14 happyReduction_48
happyReduction_48 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 (Just happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_0  15 happyReduction_49
happyReduction_49  =  HappyAbsSyn15
		 ([]
	)

happyReduce_50 = happySpecReduce_3  15 happyReduction_50
happyReduction_50 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_3 : happy_var_1
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  16 happyReduction_51
happyReduction_51 (HappyTerminal (TokenID happy_var_1))
	 =  HappyAbsSyn16
		 (Identifier happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 50 50 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenIf -> cont 17;
	TokenElse -> cont 18;
	TokenReturn -> cont 19;
	TokenWhile -> cont 20;
	TokenSemicolon -> cont 21;
	TokenOCB -> cont 22;
	TokenCCB -> cont 23;
	TokenNumConst happy_dollar_dollar -> cont 24;
	TokenCharConst happy_dollar_dollar -> cont 25;
	TokenStringConst happy_dollar_dollar -> cont 26;
	TokenID happy_dollar_dollar -> cont 27;
	TokenAssign -> cont 28;
	TokenPlus -> cont 29;
	TokenMinus -> cont 30;
	TokenTimes -> cont 31;
	TokenDiv -> cont 32;
	TokenMod -> cont 33;
	TokenLess -> cont 34;
	TokenGreater -> cont 35;
	TokenLEQ -> cont 36;
	TokenGEQ -> cont 37;
	TokenEQ -> cont 38;
	TokenNEQ -> cont 39;
	TokenAND -> cont 40;
	TokenOR -> cont 41;
	TokenNEG -> cont 42;
	TokenOB -> cont 43;
	TokenCB -> cont 44;
	TokenString -> cont 45;
	TokenChar -> cont 46;
	TokenInt -> cont 47;
	TokenVoid -> cont 48;
	TokenComma -> cont 49;
	_ -> happyError' (tk:tks)
	}

happyError_ 50 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (>>=)
happyReturn :: () => a -> Parser a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Parser a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> Parser a
happyError' = parseError

parseVYPe15 tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError a = error $ "Parse error near : " ++ show a
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/home/ghc/hp/build/ghc-bindist/local/lib/ghc-7.10.2/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







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

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
