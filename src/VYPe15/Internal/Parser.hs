{-# OPTIONS_GHC -w #-}
module VYPe15.Internal.Parser where

import Data.Char 

import VYPe15.Types.AST 
import VYPe15.Types.Tokens (Token(..))
import VYPe15.Types.Parser (Parser)
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
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

action_0 (43) = happyShift action_7
action_0 (44) = happyShift action_8
action_0 (45) = happyShift action_9
action_0 (46) = happyShift action_10
action_0 (4) = happyGoto action_11
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 (11) = happyGoto action_5
action_0 (12) = happyGoto action_6
action_0 _ = happyFail

action_1 (43) = happyShift action_7
action_1 (44) = happyShift action_8
action_1 (45) = happyShift action_9
action_1 (46) = happyShift action_10
action_1 (4) = happyGoto action_2
action_1 (5) = happyGoto action_3
action_1 (6) = happyGoto action_4
action_1 (11) = happyGoto action_5
action_1 (12) = happyGoto action_6
action_1 _ = happyFail

action_2 (43) = happyShift action_7
action_2 (44) = happyShift action_8
action_2 (45) = happyShift action_9
action_2 (46) = happyShift action_10
action_2 (5) = happyGoto action_12
action_2 (6) = happyGoto action_13
action_2 (11) = happyGoto action_5
action_2 (12) = happyGoto action_6
action_2 _ = happyFail

action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 _ = happyReduce_42

action_6 (25) = happyShift action_15
action_6 (14) = happyGoto action_14
action_6 _ = happyFail

action_7 _ = happyReduce_40

action_8 _ = happyReduce_39

action_9 _ = happyReduce_38

action_10 _ = happyReduce_41

action_11 (43) = happyShift action_7
action_11 (44) = happyShift action_8
action_11 (45) = happyShift action_9
action_11 (46) = happyShift action_10
action_11 (48) = happyAccept
action_11 (5) = happyGoto action_12
action_11 (6) = happyGoto action_13
action_11 (11) = happyGoto action_5
action_11 (12) = happyGoto action_6
action_11 _ = happyFail

action_12 _ = happyReduce_1

action_13 _ = happyReduce_2

action_14 (41) = happyShift action_16
action_14 _ = happyFail

action_15 _ = happyReduce_45

action_16 (25) = happyShift action_15
action_16 (14) = happyGoto action_17
action_16 _ = happyFail

action_17 (42) = happyShift action_18
action_17 _ = happyFail

action_18 (19) = happyShift action_19
action_18 (20) = happyShift action_20
action_18 _ = happyFail

action_19 _ = happyReduce_6

action_20 (7) = happyGoto action_21
action_20 _ = happyReduce_7

action_21 (15) = happyShift action_25
action_21 (17) = happyShift action_26
action_21 (18) = happyShift action_27
action_21 (21) = happyShift action_28
action_21 (25) = happyShift action_15
action_21 (43) = happyShift action_7
action_21 (44) = happyShift action_8
action_21 (45) = happyShift action_9
action_21 (8) = happyGoto action_22
action_21 (11) = happyGoto action_23
action_21 (14) = happyGoto action_24
action_21 _ = happyFail

action_22 _ = happyReduce_8

action_23 (25) = happyShift action_15
action_23 (14) = happyGoto action_40
action_23 _ = happyFail

action_24 (26) = happyShift action_38
action_24 (41) = happyShift action_39
action_24 _ = happyFail

action_25 (41) = happyShift action_37
action_25 _ = happyFail

action_26 (19) = happyShift action_31
action_26 (22) = happyShift action_32
action_26 (23) = happyShift action_33
action_26 (24) = happyShift action_34
action_26 (40) = happyShift action_35
action_26 (41) = happyShift action_36
action_26 (10) = happyGoto action_30
action_26 _ = happyFail

action_27 (41) = happyShift action_29
action_27 _ = happyFail

action_28 _ = happyReduce_5

action_29 (22) = happyShift action_32
action_29 (23) = happyShift action_33
action_29 (24) = happyShift action_34
action_29 (40) = happyShift action_35
action_29 (41) = happyShift action_36
action_29 (10) = happyGoto action_63
action_29 _ = happyFail

action_30 (19) = happyShift action_49
action_30 (27) = happyShift action_50
action_30 (28) = happyShift action_51
action_30 (29) = happyShift action_52
action_30 (30) = happyShift action_53
action_30 (31) = happyShift action_54
action_30 (32) = happyShift action_55
action_30 (33) = happyShift action_56
action_30 (34) = happyShift action_57
action_30 (35) = happyShift action_58
action_30 (36) = happyShift action_59
action_30 (37) = happyShift action_60
action_30 (38) = happyShift action_61
action_30 (39) = happyShift action_62
action_30 _ = happyFail

action_31 _ = happyReduce_12

action_32 _ = happyReduce_34

action_33 _ = happyReduce_36

action_34 _ = happyReduce_35

action_35 (22) = happyShift action_32
action_35 (23) = happyShift action_33
action_35 (24) = happyShift action_34
action_35 (40) = happyShift action_35
action_35 (41) = happyShift action_36
action_35 (10) = happyGoto action_48
action_35 _ = happyFail

action_36 (22) = happyShift action_32
action_36 (23) = happyShift action_33
action_36 (24) = happyShift action_34
action_36 (40) = happyShift action_35
action_36 (41) = happyShift action_36
action_36 (43) = happyShift action_7
action_36 (44) = happyShift action_8
action_36 (45) = happyShift action_9
action_36 (10) = happyGoto action_46
action_36 (11) = happyGoto action_47
action_36 _ = happyFail

action_37 (22) = happyShift action_32
action_37 (23) = happyShift action_33
action_37 (24) = happyShift action_34
action_37 (40) = happyShift action_35
action_37 (41) = happyShift action_36
action_37 (10) = happyGoto action_45
action_37 _ = happyFail

action_38 (22) = happyShift action_32
action_38 (23) = happyShift action_33
action_38 (24) = happyShift action_34
action_38 (40) = happyShift action_35
action_38 (41) = happyShift action_36
action_38 (10) = happyGoto action_44
action_38 _ = happyFail

action_39 (22) = happyShift action_32
action_39 (23) = happyShift action_33
action_39 (24) = happyShift action_34
action_39 (40) = happyShift action_35
action_39 (41) = happyShift action_36
action_39 (9) = happyGoto action_42
action_39 (10) = happyGoto action_43
action_39 _ = happyReduce_16

action_40 (13) = happyGoto action_41
action_40 _ = happyReduce_43

action_41 (19) = happyShift action_84
action_41 (47) = happyShift action_85
action_41 _ = happyFail

action_42 (42) = happyShift action_82
action_42 (47) = happyShift action_83
action_42 _ = happyFail

action_43 (27) = happyShift action_50
action_43 (28) = happyShift action_51
action_43 (29) = happyShift action_52
action_43 (30) = happyShift action_53
action_43 (31) = happyShift action_54
action_43 (32) = happyShift action_55
action_43 (33) = happyShift action_56
action_43 (34) = happyShift action_57
action_43 (35) = happyShift action_58
action_43 (36) = happyShift action_59
action_43 (37) = happyShift action_60
action_43 (38) = happyShift action_61
action_43 (39) = happyShift action_62
action_43 _ = happyReduce_18

action_44 (19) = happyShift action_81
action_44 (27) = happyShift action_50
action_44 (28) = happyShift action_51
action_44 (29) = happyShift action_52
action_44 (30) = happyShift action_53
action_44 (31) = happyShift action_54
action_44 (32) = happyShift action_55
action_44 (33) = happyShift action_56
action_44 (34) = happyShift action_57
action_44 (35) = happyShift action_58
action_44 (36) = happyShift action_59
action_44 (37) = happyShift action_60
action_44 (38) = happyShift action_61
action_44 (39) = happyShift action_62
action_44 _ = happyFail

action_45 (27) = happyShift action_50
action_45 (28) = happyShift action_51
action_45 (29) = happyShift action_52
action_45 (30) = happyShift action_53
action_45 (31) = happyShift action_54
action_45 (32) = happyShift action_55
action_45 (33) = happyShift action_56
action_45 (34) = happyShift action_57
action_45 (35) = happyShift action_58
action_45 (36) = happyShift action_59
action_45 (37) = happyShift action_60
action_45 (38) = happyShift action_61
action_45 (39) = happyShift action_62
action_45 (42) = happyShift action_80
action_45 _ = happyFail

action_46 (27) = happyShift action_50
action_46 (28) = happyShift action_51
action_46 (29) = happyShift action_52
action_46 (30) = happyShift action_53
action_46 (31) = happyShift action_54
action_46 (32) = happyShift action_55
action_46 (33) = happyShift action_56
action_46 (34) = happyShift action_57
action_46 (35) = happyShift action_58
action_46 (36) = happyShift action_59
action_46 (37) = happyShift action_60
action_46 (38) = happyShift action_61
action_46 (39) = happyShift action_62
action_46 (42) = happyShift action_79
action_46 _ = happyFail

action_47 (42) = happyShift action_78
action_47 _ = happyFail

action_48 _ = happyReduce_32

action_49 _ = happyReduce_11

action_50 (22) = happyShift action_32
action_50 (23) = happyShift action_33
action_50 (24) = happyShift action_34
action_50 (40) = happyShift action_35
action_50 (41) = happyShift action_36
action_50 (10) = happyGoto action_77
action_50 _ = happyFail

action_51 (22) = happyShift action_32
action_51 (23) = happyShift action_33
action_51 (24) = happyShift action_34
action_51 (40) = happyShift action_35
action_51 (41) = happyShift action_36
action_51 (10) = happyGoto action_76
action_51 _ = happyFail

action_52 (22) = happyShift action_32
action_52 (23) = happyShift action_33
action_52 (24) = happyShift action_34
action_52 (40) = happyShift action_35
action_52 (41) = happyShift action_36
action_52 (10) = happyGoto action_75
action_52 _ = happyFail

action_53 (22) = happyShift action_32
action_53 (23) = happyShift action_33
action_53 (24) = happyShift action_34
action_53 (40) = happyShift action_35
action_53 (41) = happyShift action_36
action_53 (10) = happyGoto action_74
action_53 _ = happyFail

action_54 (22) = happyShift action_32
action_54 (23) = happyShift action_33
action_54 (24) = happyShift action_34
action_54 (40) = happyShift action_35
action_54 (41) = happyShift action_36
action_54 (10) = happyGoto action_73
action_54 _ = happyFail

action_55 (22) = happyShift action_32
action_55 (23) = happyShift action_33
action_55 (24) = happyShift action_34
action_55 (40) = happyShift action_35
action_55 (41) = happyShift action_36
action_55 (10) = happyGoto action_72
action_55 _ = happyFail

action_56 (22) = happyShift action_32
action_56 (23) = happyShift action_33
action_56 (24) = happyShift action_34
action_56 (40) = happyShift action_35
action_56 (41) = happyShift action_36
action_56 (10) = happyGoto action_71
action_56 _ = happyFail

action_57 (22) = happyShift action_32
action_57 (23) = happyShift action_33
action_57 (24) = happyShift action_34
action_57 (40) = happyShift action_35
action_57 (41) = happyShift action_36
action_57 (10) = happyGoto action_70
action_57 _ = happyFail

action_58 (22) = happyShift action_32
action_58 (23) = happyShift action_33
action_58 (24) = happyShift action_34
action_58 (40) = happyShift action_35
action_58 (41) = happyShift action_36
action_58 (10) = happyGoto action_69
action_58 _ = happyFail

action_59 (22) = happyShift action_32
action_59 (23) = happyShift action_33
action_59 (24) = happyShift action_34
action_59 (40) = happyShift action_35
action_59 (41) = happyShift action_36
action_59 (10) = happyGoto action_68
action_59 _ = happyFail

action_60 (22) = happyShift action_32
action_60 (23) = happyShift action_33
action_60 (24) = happyShift action_34
action_60 (40) = happyShift action_35
action_60 (41) = happyShift action_36
action_60 (10) = happyGoto action_67
action_60 _ = happyFail

action_61 (22) = happyShift action_32
action_61 (23) = happyShift action_33
action_61 (24) = happyShift action_34
action_61 (40) = happyShift action_35
action_61 (41) = happyShift action_36
action_61 (10) = happyGoto action_66
action_61 _ = happyFail

action_62 (22) = happyShift action_32
action_62 (23) = happyShift action_33
action_62 (24) = happyShift action_34
action_62 (40) = happyShift action_35
action_62 (41) = happyShift action_36
action_62 (10) = happyGoto action_65
action_62 _ = happyFail

action_63 (27) = happyShift action_50
action_63 (28) = happyShift action_51
action_63 (29) = happyShift action_52
action_63 (30) = happyShift action_53
action_63 (31) = happyShift action_54
action_63 (32) = happyShift action_55
action_63 (33) = happyShift action_56
action_63 (34) = happyShift action_57
action_63 (35) = happyShift action_58
action_63 (36) = happyShift action_59
action_63 (37) = happyShift action_60
action_63 (38) = happyShift action_61
action_63 (39) = happyShift action_62
action_63 (42) = happyShift action_64
action_63 _ = happyFail

action_64 (20) = happyShift action_91
action_64 _ = happyFail

action_65 (27) = happyShift action_50
action_65 (28) = happyShift action_51
action_65 (29) = happyShift action_52
action_65 (30) = happyShift action_53
action_65 (31) = happyShift action_54
action_65 (32) = happyShift action_55
action_65 (33) = happyShift action_56
action_65 (34) = happyShift action_57
action_65 (35) = happyShift action_58
action_65 (36) = happyShift action_59
action_65 (37) = happyShift action_60
action_65 (38) = happyShift action_61
action_65 _ = happyReduce_19

action_66 (27) = happyShift action_50
action_66 (28) = happyShift action_51
action_66 (29) = happyShift action_52
action_66 (30) = happyShift action_53
action_66 (31) = happyShift action_54
action_66 (32) = happyShift action_55
action_66 (33) = happyShift action_56
action_66 (34) = happyShift action_57
action_66 (35) = happyShift action_58
action_66 (36) = happyShift action_59
action_66 (37) = happyShift action_60
action_66 _ = happyReduce_20

action_67 (27) = happyShift action_50
action_67 (28) = happyShift action_51
action_67 (29) = happyShift action_52
action_67 (30) = happyShift action_53
action_67 (31) = happyShift action_54
action_67 (32) = happyShift action_55
action_67 (33) = happyShift action_56
action_67 (34) = happyShift action_57
action_67 (35) = happyShift action_58
action_67 _ = happyReduce_22

action_68 (27) = happyShift action_50
action_68 (28) = happyShift action_51
action_68 (29) = happyShift action_52
action_68 (30) = happyShift action_53
action_68 (31) = happyShift action_54
action_68 (32) = happyShift action_55
action_68 (33) = happyShift action_56
action_68 (34) = happyShift action_57
action_68 (35) = happyShift action_58
action_68 _ = happyReduce_21

action_69 (27) = happyShift action_50
action_69 (28) = happyShift action_51
action_69 (29) = happyShift action_52
action_69 (30) = happyShift action_53
action_69 (31) = happyShift action_54
action_69 _ = happyReduce_26

action_70 (27) = happyShift action_50
action_70 (28) = happyShift action_51
action_70 (29) = happyShift action_52
action_70 (30) = happyShift action_53
action_70 (31) = happyShift action_54
action_70 _ = happyReduce_25

action_71 (27) = happyShift action_50
action_71 (28) = happyShift action_51
action_71 (29) = happyShift action_52
action_71 (30) = happyShift action_53
action_71 (31) = happyShift action_54
action_71 _ = happyReduce_24

action_72 (27) = happyShift action_50
action_72 (28) = happyShift action_51
action_72 (29) = happyShift action_52
action_72 (30) = happyShift action_53
action_72 (31) = happyShift action_54
action_72 _ = happyReduce_23

action_73 (27) = happyShift action_50
action_73 (28) = happyShift action_51
action_73 _ = happyReduce_31

action_74 (27) = happyShift action_50
action_74 (28) = happyShift action_51
action_74 _ = happyReduce_30

action_75 (27) = happyShift action_50
action_75 (28) = happyShift action_51
action_75 _ = happyReduce_29

action_76 _ = happyReduce_28

action_77 _ = happyReduce_27

action_78 (22) = happyShift action_32
action_78 (23) = happyShift action_33
action_78 (24) = happyShift action_34
action_78 (40) = happyShift action_35
action_78 (41) = happyShift action_36
action_78 (10) = happyGoto action_90
action_78 _ = happyFail

action_79 _ = happyReduce_37

action_80 (20) = happyShift action_89
action_80 _ = happyFail

action_81 _ = happyReduce_9

action_82 (19) = happyShift action_88
action_82 _ = happyFail

action_83 (22) = happyShift action_32
action_83 (23) = happyShift action_33
action_83 (24) = happyShift action_34
action_83 (40) = happyShift action_35
action_83 (41) = happyShift action_36
action_83 (10) = happyGoto action_87
action_83 _ = happyFail

action_84 _ = happyReduce_14

action_85 (25) = happyShift action_15
action_85 (14) = happyGoto action_86
action_85 _ = happyFail

action_86 _ = happyReduce_44

action_87 (27) = happyShift action_50
action_87 (28) = happyShift action_51
action_87 (29) = happyShift action_52
action_87 (30) = happyShift action_53
action_87 (31) = happyShift action_54
action_87 (32) = happyShift action_55
action_87 (33) = happyShift action_56
action_87 (34) = happyShift action_57
action_87 (35) = happyShift action_58
action_87 (36) = happyShift action_59
action_87 (37) = happyShift action_60
action_87 (38) = happyShift action_61
action_87 (39) = happyShift action_62
action_87 _ = happyReduce_17

action_88 _ = happyReduce_15

action_89 (7) = happyGoto action_93
action_89 _ = happyReduce_7

action_90 _ = happyReduce_33

action_91 (7) = happyGoto action_92
action_91 _ = happyReduce_7

action_92 (15) = happyShift action_25
action_92 (17) = happyShift action_26
action_92 (18) = happyShift action_27
action_92 (21) = happyShift action_95
action_92 (25) = happyShift action_15
action_92 (43) = happyShift action_7
action_92 (44) = happyShift action_8
action_92 (45) = happyShift action_9
action_92 (8) = happyGoto action_22
action_92 (11) = happyGoto action_23
action_92 (14) = happyGoto action_24
action_92 _ = happyFail

action_93 (15) = happyShift action_25
action_93 (17) = happyShift action_26
action_93 (18) = happyShift action_27
action_93 (21) = happyShift action_94
action_93 (25) = happyShift action_15
action_93 (43) = happyShift action_7
action_93 (44) = happyShift action_8
action_93 (45) = happyShift action_9
action_93 (8) = happyGoto action_22
action_93 (11) = happyGoto action_23
action_93 (14) = happyGoto action_24
action_93 _ = happyFail

action_94 (16) = happyShift action_96
action_94 _ = happyFail

action_95 _ = happyReduce_13

action_96 (20) = happyShift action_97
action_96 _ = happyFail

action_97 (7) = happyGoto action_98
action_97 _ = happyReduce_7

action_98 (15) = happyShift action_25
action_98 (17) = happyShift action_26
action_98 (18) = happyShift action_27
action_98 (21) = happyShift action_99
action_98 (25) = happyShift action_15
action_98 (43) = happyShift action_7
action_98 (44) = happyShift action_8
action_98 (45) = happyShift action_9
action_98 (8) = happyGoto action_22
action_98 (11) = happyGoto action_23
action_98 (14) = happyGoto action_24
action_98 _ = happyFail

action_99 _ = happyReduce_10

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
	(HappyAbsSyn7  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (FunDef happy_var_1 happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 6 6 happyReduction_6
happyReduction_6 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (FunDeclr happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_0  7 happyReduction_7
happyReduction_7  =  HappyAbsSyn7
		 ([]
	)

happyReduce_8 = happySpecReduce_2  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_2 : happy_var_1
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 8 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Assign happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 11 8 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (If happy_var_3 (reverse happy_var_6) (reverse happy_var_10)
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  8 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Return (Just happy_var_2)
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  8 happyReduction_12
happyReduction_12 _
	_
	 =  HappyAbsSyn8
		 (Return Nothing
	)

happyReduce_13 = happyReduce 7 8 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (While happy_var_3 (reverse happy_var_6)
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 8 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (VarDef (happy_var_2:happy_var_3)
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 5 8 happyReduction_15
happyReduction_15 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (FuncCall happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_0  9 happyReduction_16
happyReduction_16  =  HappyAbsSyn9
		 ([]
	)

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3:happy_var_1
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  9 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (OR happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  10 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (AND happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  10 happyReduction_21
happyReduction_21 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  10 happyReduction_22
happyReduction_22 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (NonEq happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  10 happyReduction_23
happyReduction_23 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Less happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  10 happyReduction_24
happyReduction_24 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Greater happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  10 happyReduction_25
happyReduction_25 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (LessEq happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  10 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (GreaterEq happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  10 happyReduction_27
happyReduction_27 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  10 happyReduction_28
happyReduction_28 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  10 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Times happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  10 happyReduction_30
happyReduction_30 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Div happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  10 happyReduction_31
happyReduction_31 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Mod happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  10 happyReduction_32
happyReduction_32 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (NOT happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happyReduce 4 10 happyReduction_33
happyReduction_33 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Cast happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_1  10 happyReduction_34
happyReduction_34 (HappyTerminal (TokenNumConst happy_var_1))
	 =  HappyAbsSyn10
		 (ConsNum happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  10 happyReduction_35
happyReduction_35 (HappyTerminal (TokenStringConst happy_var_1))
	 =  HappyAbsSyn10
		 (ConsString happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  10 happyReduction_36
happyReduction_36 (HappyTerminal (TokenCharConst happy_var_1))
	 =  HappyAbsSyn10
		 (ConsChar happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  10 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Bracket happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  11 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn11
		 (Int
	)

happyReduce_39 = happySpecReduce_1  11 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn11
		 (Char
	)

happyReduce_40 = happySpecReduce_1  11 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn11
		 (String
	)

happyReduce_41 = happySpecReduce_1  12 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn12
		 (Void
	)

happyReduce_42 = happySpecReduce_1  12 happyReduction_42
happyReduction_42 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (Type happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_0  13 happyReduction_43
happyReduction_43  =  HappyAbsSyn13
		 ([]
	)

happyReduce_44 = happySpecReduce_3  13 happyReduction_44
happyReduction_44 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_3 : happy_var_1
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  14 happyReduction_45
happyReduction_45 (HappyTerminal (TokenID happy_var_1))
	 =  HappyAbsSyn14
		 (Identifier happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 48 48 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenIf -> cont 15;
	TokenElse -> cont 16;
	TokenReturn -> cont 17;
	TokenWhile -> cont 18;
	TokenSemicolon -> cont 19;
	TokenOCB -> cont 20;
	TokenCCB -> cont 21;
	TokenNumConst happy_dollar_dollar -> cont 22;
	TokenCharConst happy_dollar_dollar -> cont 23;
	TokenStringConst happy_dollar_dollar -> cont 24;
	TokenID happy_dollar_dollar -> cont 25;
	TokenAssign -> cont 26;
	TokenPlus -> cont 27;
	TokenMinus -> cont 28;
	TokenTimes -> cont 29;
	TokenDiv -> cont 30;
	TokenMod -> cont 31;
	TokenLess -> cont 32;
	TokenGreater -> cont 33;
	TokenLEQ -> cont 34;
	TokenGEQ -> cont 35;
	TokenEQ -> cont 36;
	TokenNEQ -> cont 37;
	TokenAND -> cont 38;
	TokenOR -> cont 39;
	TokenNEG -> cont 40;
	TokenOB -> cont 41;
	TokenCB -> cont 42;
	TokenString -> cont 43;
	TokenChar -> cont 44;
	TokenInt -> cont 45;
	TokenVoid -> cont 46;
	TokenComma -> cont 47;
	_ -> happyError' (tk:tks)
	}

happyError_ 48 tk tks = happyError' tks
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
