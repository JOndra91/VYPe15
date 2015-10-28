	.text
	.org 0
	nop
	read_char $2
	nop
	read_short $3
	nop
	read_int $4
	nop
	read_string $4, $5
	nop
	print_char $2
	nop
	print_char $3
	nop
	print_short $3
	nop
	print_char $3
	nop
	print_int $4
	nop
	print_char $3
	nop
	print_string $4
	nop
	print_char $3
	nop
	break
