#Matrix -----
#Two scalar arguments to specify rows and columns
a <- matrix(1:6, ncol = 3, nrow = 2)
length(a)
nrow(a)
ncol(a)
rownames(a) <- c("A", "B")
colnames(a) <- c("a", "b", "c")
a

str(1:3)                   # 1d vector
str(matrix(1:3, ncol = 1)) # column vector
str(matrix(1:3, nrow = 1)) # row vector
str(array(1:3, 3))         # "array" vector


# A * B	Element-wise multiplication
# A %*% B	Matrix multiplication
# A %o% B	Outer product. AB'
# crossprod(A,B)
# crossprod(A)	A'B and A'A respectively.
# t(A)	Transpose
# diag(x)	Creates diagonal matrix with elements of x in the principal diagonal
# diag(A)	Returns a vector containing the elements of the principal diagonal
# diag(k)	If k is a scalar, this creates a k x k identity matrix. Go figure.
# solve(A, b)	Returns vector x in the equation b = Ax (i.e., A-1b)
# solve(A)	Inverse of A where A is a square matrix.
# ginv(A)	Moore-Penrose Generalized Inverse of A. 
# ginv(A) requires loading the MASS package.
# y<-eigen(A)	y$val are the eigenvalues of A
# y$vec are the eigenvectors of A
# y<-svd(A)	Single value decomposition of A.
# y$d = vector containing the singular values of A
# y$u = matrix with columns contain the left singular vectors of A 
# y$v = matrix with columns contain the right singular vectors of A
# R <- chol(A)	Choleski factorization of A. Returns the upper triangular factor, such that R'R = A.
# y <- qr(A)	QR decomposition of A. 
# y$qr has an upper triangle that contains the decomposition and a lower triangle that contains information on the Q decomposition.
# y$rank is the rank of A. 
# y$qraux a vector which contains additional information on Q. 
# y$pivot contains information on the pivoting strategy used.
# cbind(A,B,...)	Combine matrices(vectors) horizontally. Returns a matrix.
# rbind(A,B,...)	Combine matrices(vectors) vertically. Returns a matrix.
# rowMeans(A)	Returns vector of row means.
# rowSums(A)	Returns vector of row sums.
# colMeans(A)	Returns vector of column means.
# colSums(A)	Returns vector of column sums.


