# fungsi metode lagrange
lagrange <- function(n, x, y, x_baru) {
    cari <- 0
    for (i in 1:n) {
        # inisialisasi kalkulasi (atas/bawah)
        pmblg_atas <- 1
        pnybt_bwh <- 1

        # loop kalkulasi
        for (j in 1:n) {
            if (i != j) {
                pmblg_atas <- pmblg_atas * (x_baru - x[j])
                pnybt_bwh <- pnybt_bwh * (x[i] - x[j])
            }
        }
        cari <- cari + (pmblg_atas / pnybt_bwh) * y[i]
    }

    # return output
    return(cari)
}

# fungsi metode newton
newton <- function(n, x, y, x_baru) {
    b <- rep(0, n)

    # kalkulasi nilai B dalam satu orde
    for (i in 1:n) {
        # untuk b0
        if (i == 1) {
            b[i] <- y[i]
        } else if (i == 2) {
            # untuk b1# untuk b1
            b[i] <- (y[i] - y[i - 1]) / (x[i] - x[i - 1])
        } else {
            # untuk > b1 atau > derajat 2
            b[i] <- (((y[i] - y[i - 1]) / (x[i] - x[i - 1])) - b[i - 1]) / (x[i] - x[1])
        }
    }

    # kalkulasi nilai fx atau output
    cari <- 0
    for (i in 1:n) {
        if (i == 1) {
            # untuk b0
            cari <- b[i]
        } else if (i == 2) {
            # untuk b1
            cari <- cari + (b[i] * (x_baru - x[i - 1]))
        } else {
            # untuk > b1 atau > derajat 2
            tempj <- 1
            for (j in 1:(i - 1)) {
                tempj <- tempj * (x_baru - x[j])
            }
            cari <- cari + (b[i] * tempj)
        }
    }

    # return output
    return(cari)
}

# input pilihan metode
cat("SOAL UAS METNUM NO.3\n")
cat("1) Lagrange dan 2) Newton\n")
print("Pilih metode yang diinginkan : ")
intrpls <- as.integer(readLines(n = 1))
if (intrpls == 1) {
    cat("Metode Interpolasi Numerik Lagrange")
    mtd <- "Lagrange"
} else if (intrpls == 2) {
    cat("Metode Interpolasi Numerik Newton")
    mtd <- "Newton"
} else {
    print("Maaf, command yang anda masukan salah!")
    break
}

# input orde dan titik
cat("
1) Berderajat 1 = 2 titik;
2) Berderajat 2 = 3 titik;
3) Berderajat 3 = 4 titik;
")
print("Masukkan jumlah titik data : ")
n <- as.integer(readLines(n = 1))
if ((n > 4) || (n < 2)) {
    print("Maaf, command yang anda masukan salah!")
    break
# } else if ((n < 2)) {
#     print("Maaf, command yang anda masukan salah!")
#     break
}
if (n == 2) derajat <- 1 else if (n == 3) derajat <- 2 else derajat <- 3

# input x (cos derajat) dan y (cos y)
print("Masukkan nilai x dan y :")
x <- y <- rep(0, n)
for (i in 1:n) {
    x[i] <- as.numeric(readline(paste("Nilai x titik ", i, ": ", sep = "")))
    y[i] <- as.double(readline(paste("Nilai y titik ", i, ": ", sep = "")))
}

# input nilai x yang diinginkan
print("Masukkan nilai x baru :")
x_baru <- as.numeric(readLines(n = 1))

# memanggi fungsi metode
if (intrpls == 1) {
    output <- as.double(lagrange(n, x, y, x_baru))
} else {
    output <- as.double(newton(n, x, y, x_baru))
}

# Print nilai y dan eksak untuk perbandingan
# print(paste("Nilai y yang di dapatkan dari Interpolasi adalah :", output))

printhasil <- sprintf("Nilai y yang di dapatkan dari Interpolasi adalah : %0.12f", output)
print(printhasil)

eksak <- as.double(cos(x_baru * pi / 180))
x_eksak <- sprintf("Nilai y eksak adalah : %f", eksak)
print(x_eksak)

# perbandingan error
error <- as.double(0)
error <- abs(eksak - output)
banding <- sprintf("Error yang didapatkan dengan Metode Interpolasi %s Berderajat %d dari Cos%0.f' adalah %f", mtd, derajat, x_baru, error)
print(banding)
