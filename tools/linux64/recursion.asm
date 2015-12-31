.text
.org 0

LI $sp, 0x4000
MOVE $fp, $sp
JAL main
BREAK

main:
  // v Function call block
  SW $ra, ($sp)
  SW $fp, -4($sp)
  ADDIU $sp, $sp, -8
  MOVE $fp, $sp
  // ^ Function call block

  // Stack allocation
  ADDIU $sp, $sp, -4

  LI $8, 65
  print_char $8
  JAL fn
  LI $8, 66
  print_char $8
  LI $8, 10
  print_char $8

  LI $9, 15
  SW $9, 4($sp)
  JAL recursive

  // Stack deallocation
  //ADDIU $sp, $sp, 4

  // v Function return block
  MOVE $sp, $fp
  LW $fp, 4($sp)
  LW $ra, 8($sp)

  JR $ra
  // ^ Function return block

fn:
  SW $ra, ($sp)
  SW $fp, -4($sp)
  ADDIU $sp, $sp, -8
  MOVE $fp, $sp

  LI $8, 10
  print_char $8

  ADDIU $sp, $fp, 8
  LW $ra, 0($sp)
  LW $fp, -4($sp)

  JR $ra

recursive:
  SW $ra, ($sp)
  SW $fp, -4($sp)
  ADDIU $sp, $sp, -8
  MOVE $fp, $sp

  // Stack allocation
  ADDIU $sp, $sp, -4

  LW $8, 12($fp)

  beq $8, $0, recursive_end

  print_int $8
  ADDIU $8, $8, -1

  SW $8, 4($sp)

  LI $8, 10
  print_char $8

  JAL recursive

recursive_end:

  // Stack deallocation
  ADDIU $sp, $sp, 4

  ADDIU $sp, $fp, 8
  LW $ra, 0($sp)
  LW $fp, -4($sp)

  JR $ra
