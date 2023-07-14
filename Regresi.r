# Deklarasi Input Soal
x_cos <- c(10, 20, 30, 40, 50, 60, 70, 80)
y_cos <- c(0.9848, 0.9397, 0.866, 0.766, 0.6428, 0.5, 0.342, 0.1737)
n <- 8

# Input Metode Pilihan
cat("
Metode Regresi
Note :
1) Linear
2) Pangkat
3) Eksponensial\n")
print("Masukkan Metode Yang Dipilih : ")
m <- as.integer(readLines(n = 1))
if ((m > 3)) {
    print("Maaf, command yang anda masukan salah!")
    break
} else if ((m < 1)) {
    print("Maaf, command yang anda masukan salah!")
    break
}
if (m == 1) t <- "linear" else if (m == 2) t <- "pangkat" else t <- "eksponensial"

# Inisialisasi
x <- 0
x_pangkat <- 0
cosx <- 0
x_cosx <- 0
a <- 0
b <- 0

# Memulai kalkulasi
for (i in 1:n) {
    # Menjumlahkan nilai X
    x <- x + as.double(x_cos[i])

    # Menjumlahkan nilai X^2
    x_pangkat <- x_pangkat + as.double(x_cos[i]^2)

    # Menjumlahkan nilai Cos(X)
    cosx <- cosx + as.double(y_cos[i])

    # Menjumlahkan nilai X * Cos(X)
    x_cosx <- x_cosx + as.double(x_cos[i] * y_cos[i])
}

# Rumus a dan b
b <- (((n * cosx) - (x * cosx)) / ((n * x_pangkat) - (x^2)))
a <- (cosx / n) - (b * (x / n))

print(paste("-------------------------------------------------------------------------------------------------"))

# Menampilkan rumus untuk regresi tiap metode sesuai dengan spesifikasi soal
if (m == 1) {
    print(paste("Persamaan Rumus Regresi Linear f(x) =", a, "+", b, "x"))
} else if (m == 2) {
    print(paste("Persamaan Rumus Regresi Pangkat f(x) =", a, "x^", b))
} else if (m == 3) {
    print(paste("Persamaan Rumus Eksponensial f(x) =", a, "e^(", b, "x)"))
}

# Kalkulasi error
fx <- 0
y <- 0
sum <- as.double(0)

print(paste("-------------------------------------------------------------------------------------------------"))

for (i in 1:n) {
    if (m == 1) {
        fx <- a + (b * x_cos[i])
    } else if (m == 2) {
        fx <- a * x_cos[i]^b
    } else if (m == 3) {
        fx <- a * (exp(b * x_cos[i]))
    }

    error <- abs(y_cos[i] - fx)
    hasil <- sprintf("i: %d  |X: %d |Cos(X): %f |X * Cos(X): %f |X^2 : %d|F(X): %f |e: %f", i, x_cos[i], y_cos[i], x_cos[i] * y_cos[i], x_cos[i]^2, fx, error)
    print(hasil)

    # Jumlah error
    sum <- sum + error

    # Increment
    i < i + 1
}

print(paste("-------------------------------------------------------------------------------------------------"))
# Rata-rata error
sum <- sum / n
hasil_error <- sprintf("Rata-rata error regresi %s adalah %f", t, sum)
print(hasil_error)
