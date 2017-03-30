# Milan SINGH 

# Change the value N and filenames to test different given matrix problems
.data
N: .word 64
Afname: .asciiz "A64.bin"
Bfname: .asciiz "B64.bin"
Cfname: .asciiz "C64.bin"
Dfname: .asciiz "D64.bin"       

#################################################################

.text
main:	la   $t0, N
	lw   $s7, ($t0)		# Let $s7 be the matrix size n

	move $a0 $s7
	jal mallocMatrix	# allocate heap memory and load matrix A
	move $s0 $v0		# $s0 is a pointer to matrix A
	la $a0 Afname
	move $a1 $s7
	move $a2 $s7
	move $a3 $s0
	jal loadMatrix
	
	move $a0 $s7
	jal mallocMatrix	# allocate heap memory and load matrix B
	move $s1 $v0		# $s1 is a pointer to matrix B
	la $a0 Bfname
	move $a1 $s7
	move $a2 $s7
	move $a3 $s1
	jal loadMatrix
	
	move $a0 $s7
	jal mallocMatrix	# allocate heap memory and load matrix C
	move $s2 $v0		# $s2 is a pointer to matrix C
	la $a0 Cfname
	move $a1 $s7
	move $a2 $s7
	move $a3 $s2
	jal loadMatrix
	
	move $a0 $s7
	jal mallocMatrix	# allocate heap memory and load matrix A
	move $s3 $v0		# $s3 is a pointer to matrix D
	la $a0 Dfname
	move $a1 $s7
	move $a2 $s7
	move $a3 $s3
	jal loadMatrix		# D is the answer, i.e., D = AB+C 

	la $t0 N		#t0 - > N
	lw $t0 0($t0)		#t0 = N
	move $a3 $t0
	move $a2 $s2
	move $a1 $s1
	move $a0 $s0
	jal multiplyAndAddV2
	
	li $v0, 10      # load exit call code 10 into $v0
        syscall         # call operating system to exit	

		
###############################################################
# mallocMatrix( int N )
# Allocates memory for an N by N matrix of floats
# The pointer to the memory is returned in $v0	
mallocMatrix: 	mul  $a0, $a0, $a0	# Let $s5 be n squared
		sll  $a0, $a0, 2	# Let $s4 be 4 n^2 bytes
		li   $v0, 9		
		syscall			# malloc A
		jr $ra
	
###############################################################
# loadMatrix( char* filename, int width, int height, float* buffer )
.data
errorMessage: .asciiz "FILE NOT FOUND" 
.text
loadMatrix:	mul $t0, $a1, $a2 # words to read (width x height) in a2
		sll $t0, $t0, 2	  # multiply by 4 to get bytes to read
		li $a1, 0     # flags (0: read, 1: write)
		li $a2, 0     # mode (unused)
		li $v0, 13    # open file, $a0 is null-terminated string of file name
		syscall
		slti $t1 $v0 0
		beq $t1 $0 fileFound
		la $a0 errorMessage
		li $v0 4
		syscall		  # print error message
		li $v0 10         # and then exit
		syscall		
fileFound:	move $a0, $v0     # file descriptor (negative if error) as argument for read
  		move $a1, $a3     # address of buffer in which to write
		move $a2, $t0	  # number of bytes to read
		li  $v0, 14       # system call for read from file
		syscall           # read from file
		# $v0 contains number of characters read (0 if end-of-file, negative if error).
                # We'll assume that we do not need to be checking for errors!
		
		# Note, the bitmap display doesn't update properly on load, 
		# so let's go touch each memory address to refresh it!
		move $t0, $a3	   # start address
		add $t1, $a3, $a2  # end address
loadloop:	lw $t2, ($t0)
		sw $t2, ($t0)
		addi $t0, $t0, 4
		bne $t0, $t1, loadloop
		jr $ra	

###############################################################
# void subtract( float* A, float* B, float* C, int n )
# C = A-B
# Registers to conserve:  None
subtract:	mult $a3 $a3		#hilo  = N^2
		mflo $a3		#a3 = lo(N^2)
		sll $a3 $a3 2		#a3 = 4*N^2
		add $a3 $a0 $a3		#a3 = a0 + 4*N^2
subLoop:	l.s $f4 ($a0)		#f4 = A_ti
		l.s $f6 ($a1)		#f6 = B_ti
		sub.s $f4 $f4 $f6	#f4 = A_ti - B_ti
		s.s $f4 ($a2)		#C_ti = A_ti - B_ti
		addi $a0 $a0 4		#t1+=4
		addi $a1 $a1 4		#t1+=4
		addi $a2 $a2 4		#t1+=4
		bne $a0 $a3 subLoop	#if a0 =/= t1 loop else end loop
		jr $ra
		
###############################################################
# float frobeneousNorm( float* A, int n )
# frob = sqrt(sum 0->i sum 0->j (Aij)^2)
# Registers to conserve: None
frobeneousNorm:	mult $a1 $a1		#hilo  = N^2
		mflo $a1		#a1 = lo(N^2)
		sll $a1 $a1 2		#a1 = 4*N^2
		add $a1 $a0 $a1		#t1 = a0 + 4*N^2
		mtc1 $0 $f0		#f0 = 0
frobLoop:	l.s $f4 ($a0)		#f4 = A_ti
		mul.s $f4 $f4 $f4	#f4 = A_ti^2
		add.s $f0 $f0 $f4	#f0+= A_ti^2
		addi $a0 $a0 4		#a0+=4
		bne $a0 $a1 frobLoop	#if a0 =/= t1 loop else end loop
		sqrt.s $f0 $f0		#f0 = sqrt(f0)
		jr $ra
		
###############################################################
# void check( float* A, float* B, int n )
# print frob(A-B)
# registers to preserve: s0, s1, s2, ra
check:		addi $sp $sp -16	#adjust stack pointer
		sw $s0 ($sp)		#save regs
		sw $s1 4($sp)		#save regs
		sw $s2 8($sp)		#save regs
		sw $ra 12($sp)		#save regs
		move $s0 $a0		#stores *A
		move $s1 $a1		#stores *B
		move $s2 $a2		#stores n
		move $a2 $s0		#set up args for sub(A B A N)
		move $a3 $s2		#set up args for sub(A B A N)
		jal subtract		#subtract
		move $a0 $s0		#restore address of A -> a0 for frob
		move $a1 $s2		#a1 = N
		jal frobeneousNorm	#frob it
		mov.s $f12 $f0		#load arg for print
		li $v0 2		#load print
		syscall			#print
		lw $s0 ($sp)		#load regs
		lw $s1 4($sp)		#load regs
		lw $s2 8($sp)		#load regs
		lw $ra 12($sp)		#load regs
		addi $sp $sp 16		#adjust stack pointer
		jr $ra			#return
		
###############################################################		
#void multiplyAndAddV1( float* A, float* B, float* C, int n ) THIS IS CACHE UNFRIENDLY 
#c[i][j] += a[i][k] * b[k][j];
multiplyAndAddV1: 
		move $t5 $a3		#t5 = N
		sll $a3 $a3 2		#a3=N*4
		mult $t5 $a3		#hilo= (N*4)*N 
		mflo $t5 		#i_row_max = (N*4)*N
		move $t1 $0		#j = 0
		move $t2 $0 		#k = 0
		move $t3 $0		#i_row = 0
		move $t4 $0		#k_row = 0
		move $t6 $a2		#t6 = *C
		#load c[i][j]
		l.s $f6 ($t6)		#f6 = c[0][0]
v1Loop:			
		#load a[i][k]
		add $t7 $t3 $t2		#t7 = [i,k]
		add $t7 $t7 $a0		#t7 -> A[i,k]
		l.s $f4 ($t7)		#f4 = A[i,k]
		#load b[k][j]
		add $t7 $t4 $t1		#t7 = [k,j]
		add $t7 $t7 $a1		#t7 -> B[k,j]
		l.s $f8 ($t7)		#f8 = B[k,j]
		#Compute a[i][k]*b[k][j]
		mul.s $f4 $f4 $f8	#f4 = a[i][k]*b[k][j]	
		#add to c[i,j]
		l.s $f6, ($t6)
		add.s $f6 $f6 $f4
		s.s $f6 ($t6)		#commit c[i,j]
		#inc k
		addi $t2 $t2 4		#k+=4
		add $t4 $t4 $a3		#k_row+=N*4
		beq $t2 $a3 incj	#if k==n*4 ->inc j
		j v1Loop		#else loop
incj:		li $t2 0		#k=0
		li $t4 0		#k_row = 0
		addi $t1 $t1 4		#j+=4
		addi $t6 $t6 4		#[i,j]+=4
		beq $t1 $a3 inci	#if j==n*4 ->inc i
		l.s $f6 ($t6)		#load new c[i,j]
		j v1Loop		#else loop
inci:		li $t1 0		#j=0
		add $t3 $t3 $a3		#i_row+=N*4
		beq $t3 $t5 endV1	#if i==n*4 ->endV1	
		j v1Loop		#else loop
endV1:		jr $ra			#leave V1

###############################################################		
#void multiplyAndAddV2( float* A, float* B, float* C, int n ) THIS IS CACHE-FRIENDLY
#for 0=jj -> n; jj+=bsize
#for 0=kk -> n; kk+=bsize
#for 0=i -> n; i++
#for jj=j -> min(jj+bsize,n); j++
#sum = 0
#for kk=k -> min(kk+bsize,n); k++
 #sum += a[i][k]*b[k][j]
#c[i][j] += sum
#REGISTERS TO CONSERVE: s0
multiplyAndAddV2:
		addi $sp $sp -4
		sw $s0 ($sp)
		### Basic Var Ini
		li $t0 0		#t0 -> k = 0
		li $s0 0		#s0 -> k_row = 0
		li $t1 0		#t0 -> j = 0
		li $t2 0		#t0 -> i = 0
		li $t3 0		#t0 -> kk = 0
		li $t4 0		#t0 -> jj = 0
		### Useful saved values
		move $t5 $a2		#t5 = *C[0,0]
		sll $t6 $a3 2		#t6 = 4N
		mult $t6 $a3		#hilo=4N^2
		mflo $t7		#t7 = 4N^2
		
		
		l.s $f4 ($t5)		#$f4 = C[0,0]

Loop:		#
		 #Load up A[i][k]
		add $t9 $t2 $t0 	#t9 = 4(iN + k) = *_[i,k]
		add $t9 $a0 $t9		#t9 = *A[i][k]
		l.s $f8 ($t9)		#f6 = A[i][k]
	  	 ###
	  	 
	  	 #Load up B[k][j]
		add $t9 $s0 $t1 	#t9 = 4(kN + j) = *_[k,j]
		add $t9 $a1 $t9		#t9 = *B[k][j]
		l.s $f6 ($t9)		#f6 = B[k][j]
	  	 ###
	  	 
	  	mul.s $f6 $f6 $f8	#f6 = A[i][k]*B[k][j]
	  	add.s $f4 $f4 $f6	#save+=A[i][k]*B[k][j]
incK:		#Inc K
		addi $t0 $t0 4		#inc k
		add $s0 $s0 $t6		#inc k_row
		 #Calc min(kk+bsize, N)
		addi $t9 $t3 16		#t9 = kk+bsize
		slt $t8 $t9 $t6 	#t9 = (kk+bsize)<N ? 1:0 
		beq $t8 $0 minK		#if !((kk+bsize)<N) minK else continue
		 #
retK:		bne $t0 $t9 Loop	#if k=1..min(KK+bsize-1,N) else continue
incJ:		#Inc J
		 #Save C[i][j] & reset save
		s.s $f4 ($t5)		#C[i][j] = save
		mtc1 $0 $f4		#$f4 = 0
		 ###
		addi $t1 $t1 4		#Inc J
		addi $t5 $t5 4		#*C[i,j]->*C[i,j+1]
		l.s $f4 ($t5)		#f6 = A[i][j]
		move $t0 $t3		#reset k = kk
		mult $t3 $a3		#hilo = kk*4N
		mflo $s0		#k_row =  kk*4N
		 #Calc min(kk+bsize, N)
		addi $t9 $t4 16		#t9 = jj+bsize
		slt $t8 $t9 $t6 	#t9 = (jj+bsize)<N ? 1:0 
		beq $t8 $0 minJ		#if !((jj+bsize)<N) minJ else continue
		 #
retJ:		bne $t1 $t9 Loop	#if k=1..min(KK+bsize-1,N) else continue
incI:		#Inc I
		move $t1 $t4		#j = jj
		add $t2 $t2 $t6		#inc i
		 #Update C pointer & save
		add $t9 $t2 $t1 	#t9 = 4(iN + k) = *_[i,j]
		add $t5 $a2 $t9		#t5 = *C[i][j]
		l.s $f4 ($t5)		#f4 = C[i][j]
		 ###
		bne $t2 $t7 Loop	#if i=4N..4N^2-4N Loop else continue
incKK:		#Inc KK
		move $t2 $0		#i = 0
		 #Update C pointer & save
		add $t9 $t2 $t1 	#t9 = 4(iN + k) = *_[i,j]
		add $t5 $a2 $t9		#t5 = *C[i][j]
		l.s $f4 ($t5)		#f6 = A[i][j]
		 ###
		addi $t3 $t3 16		#inc kk
		move $t0 $t3		#update k = kk
		mult $t3 $a3		#hilo = kk*4N
		mflo $s0		#k_row =  kk*4N
		slt $t9 $t3 $t6		# t9 ? (kk<N) 1: 0
		bne $t9 $0 Loop		#if kk=4..N-4 Loop else Continue
incJJ:		#Inc JJ
		move $t3 $0		#kk = 0
		move $t0 $t3		#update k = kk
		mult $t3 $a3		#hilo = kk*4N
		mflo $s0		#k_row =  kk*4N
		addi $t4 $t4 16		#inc jj
		move $t1 $t4		#update j = jj
		 #Update C pointer & save
		add $t9 $t2 $t1 	#t9 = 4(iN + k) = *_[i,j]
		add $t5 $a2 $t9		#t5 = *C[i][j]
		l.s $f4 ($t5)		#f6 = A[i][j]
		 ###
		slt $t9 $t4 $t6		# t9 ? (jj<N) 1: 0
		bne $t9 $0 Loop		#if jj=4..N-4 Loop else Continue
		lw $s0 ($sp)
		addi $sp $sp 4
		jr $ra
		
		###NOTE minK and minJ have a duplicate version of the code following retK and retJ labels respectively, 
		#this is to reduce the number of instructions by removing the neccesity to jump back to that code (Note the only change is that N is used in the conditional loop)
minK:		bne $t0 $t6 Loop	#if k=1..min(KK+bsize-1,N) else continue
		#Inc J
		 #Save C[i][j] & reset save
		s.s $f4 ($t5)		#C[i][j] = save
		mtc1 $0 $f4		#$f4 = 0
		 ###
		addi $t1 $t1 4		#Inc J
		addi $t5 $t5 4		#*C[i,j]->*C[i,j+1]
		l.s $f4 ($t5)		#f6 = A[i][j]
		move $t0 $t3		#reset k = kk
		mult $t3 $a3		#hilo = kk*4N
		mflo $s0		#k_row =  kk*4N
		 #Calc min(kk+bsize, N)
		addi $t9 $t4 16		#t9 = jj+bsize
		slt $t8 $t9 $t6 	#t9 = (jj+bsize)<N ? 1:0 
		beq $t8 $0 minJ		#if !((jj+bsize)<N) minJ else continue
		 #
		bne $t1 $t9 Loop	#if k=1..min(KK+bsize-1,N) else continue
		#Inc I
		move $t1 $t4		#j = jj
		add $t2 $t2 $t6		#inc i
		 #Update C pointer & save
		add $t9 $t2 $t1 	#t9 = 4(iN + k) = *_[i,j]
		add $t5 $a2 $t9		#t5 = *C[i][j]
		l.s $f4 ($t5)		#f4 = C[i][j]
		 ###
		bne $t2 $t7 Loop	#if i=4N..4N^2-4N Loop else continue
		#Inc KK
		move $t2 $0		#i = 0
		 #Update C pointer & save
		add $t9 $t2 $t1 	#t9 = 4(iN + k) = *_[i,j]
		add $t5 $a2 $t9		#t5 = *C[i][j]
		l.s $f4 ($t5)		#f6 = A[i][j]
		 ###
		addi $t3 $t3 16		#inc kk
		move $t0 $t3		#update k = kk
		mult $t3 $a3		#hilo = kk*4N
		mflo $s0		#k_row =  kk*4N
		slt $t9 $t3 $t6		# t9 ? (kk<N) 1: 0
		bne $t9 $0 Loop		#if kk=4..N-4 Loop else Continue
		#Inc JJ
		move $t3 $0		#kk = 0
		move $t0 $t3		#update k = kk
		mult $t3 $a3		#hilo = kk*4N
		mflo $s0		#k_row =  kk*4N
		addi $t4 $t4 16		#inc jj
		move $t1 $t4		#update j = jj
		 #Update C pointer & save
		add $t9 $t2 $t1 	#t9 = 4(iN + k) = *_[i,j]
		add $t5 $a2 $t9		#t5 = *C[i][j]
		l.s $f4 ($t5)		#f6 = A[i][j]
		 ###
		slt $t9 $t4 $t6		# t9 ? (jj<N) 1: 0
		bne $t9 $0 Loop		#if jj=4..N-4 Loop else Continue
		lw $s0 ($sp)
		addi $sp $sp 4
		jr $ra
		
		###NOTE minK and minJ have a duplicate version of the code following retK and retJ labels respectively (in the original code)
		#, this is to reduce the number of instructions by removing the neccesity to jump back to that code (Note the only change is that N is used in the conditional loop)
minJ:		bne $t1 $t6 Loop	#if k=1..min(KK+bsize-1,N) else continue
		#Inc I
		move $t1 $t4		#j = jj
		add $t2 $t2 $t6		#inc i
		 #Update C pointer & save
		add $t9 $t2 $t1 	#t9 = 4(iN + k) = *_[i,j]
		add $t5 $a2 $t9		#t5 = *C[i][j]
		l.s $f4 ($t5)		#f4 = C[i][j]
		 ###
		bne $t2 $t7 Loop	#if i=4N..4N^2-4N Loop else continue
		#Inc KK
		move $t2 $0		#i = 0
		 #Update C pointer & save
		add $t9 $t2 $t1 	#t9 = 4(iN + k) = *_[i,j]
		add $t5 $a2 $t9		#t5 = *C[i][j]
		l.s $f4 ($t5)		#f6 = A[i][j]
		 ###
		addi $t3 $t3 16		#inc kk
		move $t0 $t3		#update k = kk
		mult $t3 $a3		#hilo = kk*4N
		mflo $s0		#k_row =  kk*4N
		slt $t9 $t3 $t6		# t9 ? (kk<N) 1: 0
		bne $t9 $0 Loop		#if kk=4..N-4 Loop else Continue
		#Inc JJ
		move $t3 $0		#kk = 0
		move $t0 $t3		#update k = kk
		mult $t3 $a3		#hilo = kk*4N
		mflo $s0		#k_row =  kk*4N
		addi $t4 $t4 16		#inc jj
		move $t1 $t4		#update j = jj
		 #Update C pointer & save
		add $t9 $t2 $t1 	#t9 = 4(iN + k) = *_[i,j]
		add $t5 $a2 $t9		#t5 = *C[i][j]
		l.s $f4 ($t5)		#f6 = A[i][j]
		 ###
		slt $t9 $t4 $t6		# t9 ? (jj<N) 1: 0
		bne $t9 $0 Loop		#if jj=4..N-4 Loop else Continue
		lw $s0 ($sp)
		addi $sp $sp 4
		jr $ra
