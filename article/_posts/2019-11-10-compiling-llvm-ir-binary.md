---
title: Compiling LLVM IR to Binary
summary: A note to my future self.
tags: [llvm]
---

The code:

```llvm
; hello-world.ll

@string = private constant [15 x i8] c"Hello, world!\0A\00"

declare i32 @puts(i8*)

define i32 @main() {
  %address = getelementptr [15 x i8], [15 x i8]* @string, i64 0, i64 0
  call i32 @puts(i8* %address)
  ret i32 0
}
```

Directly to binary:

```bash
$ llc -filetype=obj hello-world.ll -o hello-world.o
$ clang hello-world.o -o hello-world
$ ./hello-world
Hello, world!
```

Through LLVM bitcode:

```bash
$ llvm-as hello-world.ll -o hello-world.bc
$ llc -filetype=obj hello-world.bc -o hello-world.o
$ clang hello-world.o -o hello-world
$ ./hello-world
Hello, world!
```

To assembly:

```nasm
$ llc -filetype=asm hello-world.ll -o hello-world.s
$ cat hello-world.s
	.text
	.file	"hello-world.ll"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$.Lstring, %edi
	callq	puts
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.Lstring,@object        # @string
	.section	.rodata,"a",@progbits
.Lstring:
	.asciz	"Hello, world!"
	.size	.Lstring, 14


	.section	".note.GNU-stack","",@progbits
```
