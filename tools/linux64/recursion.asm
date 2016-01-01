.data
__null_string_:  .byte  0
.text
.org 0
  li $sp, 32768
  move $fp, $sp
  addiu $16, $sp, 4
  jal main
  break
recurse:
  sw $ra, 0($sp)
  sw $fp, -4($sp)
  sw $16, -8($sp)
  addiu $sp, $sp, -12
  move $fp, $sp
  addiu $sp, $sp, -32
  li $8, 10
  sb $8, 0($fp)
  lw $8, 16($fp)
  print_int $8
  lb $8, 0($fp)
  print_char $8
  li $8, 0
  sw $8, -4($fp)
  lw $8, 16($fp)
  lw $9, -4($fp)
  li $10, 1
  sub $11, $8, $9
  bgtz $11, label__GT_0
  li $10, 0
label__GT_0:
  sw $10, -8($fp)
  lw $8, -8($fp)
  beq $8, $0, IfElse_0
  li $8, 1
  sw $8, -12($fp)
  lw $8, 16($fp)
  lw $9, -12($fp)
  sub $10, $8, $9
  sw $10, -16($fp)
  lw $4, -16($fp)
  sw $4, 0($sp)
  addiu $sp, $sp, -4
  jal recurse
  addiu $sp, $sp, 4
  b IfEnd_0
IfElse_0:
  li $8, 79
  sb $8, -20($fp)
  li $8, 75
  sb $8, -24($fp)
  li $8, 10
  sb $8, -28($fp)
  lb $8, -20($fp)
  print_char $8
  lb $8, -24($fp)
  print_char $8
  lb $8, -28($fp)
  print_char $8
IfEnd_0:
  li $2, 0
  b __recurse_return_
__recurse_return_:
  addiu $sp, $fp, 12
  lw $ra, 0($sp)
  lw $fp, -4($sp)
  lw $16, -8($sp)
  jr $ra
main:
  sw $ra, 0($sp)
  sw $fp, -4($sp)
  sw $16, -8($sp)
  addiu $sp, $sp, -12
  move $fp, $sp
  addiu $sp, $sp, -16
  li $8, 0
  sw $8, 0($fp)
  li $8, 5
  sw $8, -4($fp)
  lw $8, -4($fp)
  sw $8, 0($fp)
  li $8, 10
  sw $8, -8($fp)
  lw $4, -8($fp)
  sw $4, 0($sp)
  addiu $sp, $sp, -4
  jal recurse
  addiu $sp, $sp, 4
  li $8, 5
  sw $8, -12($fp)
  lw $4, -12($fp)
  sw $4, 0($sp)
  addiu $sp, $sp, -4
  jal recurse
  addiu $sp, $sp, 4
  li $2, 0
  b __main_return_
__main_return_:
  addiu $sp, $fp, 12
  lw $ra, 0($sp)
  lw $fp, -4($sp)
  lw $16, -8($sp)
  jr $ra
