# definisi fungsi soal
fx <- function(x) {
    return((97.8 - (19.55 * x) + (16.3 * (x^2)) - (10.8 * (x^3))))
    # return(x + 2.71828182846^x)
}

# Input
print("Masukkan selang bawah : ")
inputxbawah <- readline()
print("Masukkan selang atas : ")
inputxatas <- readline()

# start eksekusi
start_time <- Sys.time()

# Mengubah nilai input menjadi double dan inisialisasi variable double
xbawah <- as.double(inputxbawah)
xatas <- as.double(inputxatas)
fbawah <- as.double(fx(xbawah))
fatas <- as.double(fx(xatas))

# Inisialisasi
fxhasil <- 0
fxbefore <- 0
xbefore <- 0
found <- FALSE

i <- 0
h <- 0.0000000001
x <- xbawah + (h * i)

print("i ---------- x ------------------ fx")

condition <- TRUE
while (condition) {
    if(i==2){
        break
    }
    # loop hingga maksimal iterasi

    # inisialisasi xprev dan x
    xbefore <- x
    x <- xbawah + (h * i)

    # inisialisasi fxprev dan fx
    fxbefore <- fxhasil
    fxhasil <- fx(x)

    # print hasil
    hasil <- sprintf("%d ---------- %0.10f ---------- %0.10f", i, x, fxhasil)
    print(hasil)

    if (fxbefore * fxhasil < 0) {
        # jika fxi-1 * fxi < 0

        if (fxhasil == 0) {
            # jika fxi == 0, maka xi itu adalah akar persamaannya
            ket <- sprintf("Akarnya: %f", x)
            found <- TRUE
        } else if (abs(fxbefore) < abs(fxhasil)) {
            # jika |fxi-1| < |fxi|, maka xi-1 itu adalah akar persamaannya
            ket <- sprintf("Akarnya: %f", xbefore)
            found <- TRUE
        } else {
            # jika |fxi-1| > |fxi|, maka xi itu adalah akar persamaannya
            # jika iterasi sudah maksimal, xi pada iterasi terakhir adalah akar persamaannya
            ket <- sprintf("Akarnya: %f", x)
            found <- TRUE
        }
    }

    if (fxhasil * fxbefore < 0) {
        condition <- FALSE
    }

    i <- i + 1
}

if (found == FALSE) {
    # print akar tidak ditemukan
    print("Akar tidak ditemukan!")
} else {
    # print akar persamaanya
    print(ket)
}

# eksekusi berakhir
end_time <- Sys.time()

# print waktu eksekusi
print(end_time - start_time)
