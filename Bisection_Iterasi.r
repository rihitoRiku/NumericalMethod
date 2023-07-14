# define fungsi soal
fx <- function(x) {
    return((97.8 - (19.55 * x) + (16.3 * (x^2)) - (10.8 * (x^3))))
    # return(x * 2.71828182846^-x + 1)
}

# Input
print("Nasukkan selang bawah: ")
inputxbawah <- readline()
print("Nasukkan selang atas: ")
inputxatas <- readline()
print("Masukkan banyaknya iterasi: ")
inputn <- readline()

# start eksekusi
start_time <- Sys.time()

# Mengubah nilai input menjadi double dan inisialisasi variable double
xbawah <- as.double(inputxbawah)
xatas <- as.double(inputxatas)
n <- as.integer(inputn)
fbawah <- as.double(fx(xbawah))
fatas <- as.double(fx(xatas))

# inisialisasi
fxhasil <- 0
found <- TRUE
i <- 0

print("i | a        | b        | x        | fx         | fa        |  fb")
while (i < n) {
    # loop hingga maksimal iterasi

    # tentukan nilai x
    x <- ((xbawah + xatas) / 2)

    # mencari nilai fx
    fbawah <- fx(xbawah)
    fatas <- fx(xatas)
    fxhasil <- fx(x)

    error <- xatas - xbawah
    hasil <- sprintf("%d | %f | %f | %f | %f | %f | %f |%f", i, xbawah, xatas, x, fxhasil, fbawah, fatas, error)
    print(hasil)
    if (fatas * fbawah > 0) {
        # jika fa * fb > 0, maka akar tidak dapat ditemukan
        found <- FALSE
        break
    }
    if (fatas * fbawah < 0) {
        # jika fa * fb < 0, maka akar dapat ditemukan

        if ((fxhasil * fbawah) < 0) {
            # jika fxhasil * fbawah < 0, maka a menjadi x
            xatas <- x
            fatas <- fxhasil
            xbawah <- xbawah
            fbawah <- fbawah
            akar <- sprintf("Akarnya adalah : %f", x)
            found <- TRUE
        } else {
            # jika tidak, b menjadi x
            xatas <- xatas
            fatas <- fxhasil
            xbawah <- x
            fbawah <- fbawah
            akar <- sprintf("Akarnya adalah : %f", x)
            found <- TRUE
        }
    }
    i <- i + 1
}

if (found == FALSE) {
    # print akar tidak ditemukan
    print("Akar tidak ditemukan!")
} else {
    # print akar persamaanya
    print(akar)
}

# end eksekusi
end_time <- Sys.time()

# hitung hasil eksekusi
print(end_time - start_time)
