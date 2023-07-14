# define fungsi soal
fx <- function(x) {
    return((exp((-2) * x)) * sin(x) + 2)
}
f_eks <- function(x) {
    return(((-2 * (exp((-2) * x))) * sin(x) + (((exp((-2) * x)) * cos(x)))))
}

# input
print("Metode yang diinginkan : 1) Maju, 2) Tengah, 3) Mundur")
metode <- as.integer(readline())
if ((metode > 3)) {
    print("Maaf, command yang anda masukan salah!")
    break
} else if ((metode < 1)) {
    print("Maaf, command yang anda masukan salah!")
    break
}

print("Nasukkan nilai batas bawah: ")
xbawah <- as.double(readline())
print("Nasukkan nilai batas atas: ")
xatas <- as.double(readline())
print("Nasukkan nilai h: ")
nilai_h <- as.double(readline())

# inisialisasi
sum <- as.double(0)
if (metode == 1) tandametode <- "maju" else if (metode == 2) tandametode <- "tengah" else tandametode <- "mundur"

# float acceptable
float_accept <- 0.00000001

# loop kalkulasi
i <- 0
while (xbawah <= (xatas + float_accept)) {
    fx_bawah <- as.double(fx(xbawah))

    if (metode == 1) {
        fx_aksen <- as.double((fx(xbawah + nilai_h) - fx_bawah) / nilai_h)
    } else if (metode == 2) {
        fx_temp1 <- as.double(fx(xbawah + nilai_h))
        fx_temp2 <- as.double(fx(xbawah - nilai_h))
        fx_aksen <- as.double(((fx_temp1 - fx_temp2) / (2 * nilai_h)))
    } else if (metode == 3) {
        fx_aksen <- as.double((fx_bawah - fx(xbawah - nilai_h)) / nilai_h)
    }

    fx_eksak <- as.double(f_eks(xbawah))
    error <- as.double(abs(fx_eksak - fx_aksen))

    # print
    hasil <- sprintf("i: %d  |x: %f |fx: %f |fx_aksen: %f |fx_eksak: %f |error : %f |", i, xbawah, fx_bawah, fx_aksen, fx_eksak, error)
    print(hasil)

    # sum error
    sum <- sum + error

    # iterasi
    i <- i + 1
    xbawah <- xbawah + nilai_h
}

# rata - rata error
sum <- sum / i
rata_error <- sprintf("Rata-rata error metode %s dengan nilai h %0.2f adalah %f", tandametode, nilai_h, sum)
print(rata_error)
