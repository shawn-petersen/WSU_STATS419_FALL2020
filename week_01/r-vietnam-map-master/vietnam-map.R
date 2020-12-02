### Lấy dữ liệu
# con <- url("http://biogeo.ucdavis.edu/data/gadm2/R/VNM_adm2.RData")
# print(load(con))
# close(con)

# Sáp nhập tỉnh
# Tải dữ liệu
load("VNM_adm2.RData")

# Gọi thư viện cần thiết
libs <- c("rgeos", "maptools", "sp")
lapply(libs, require, character.only = TRUE)

# Nhóm và sáp nhập tỉnh
# Sáp nhập SpatialPolygons
gadm63.fac <- as.factor(c(1:18,19,18,21:64))
gadm63.sp <- unionSpatialPolygons(gadm, gadm63.fac)


# Sáp nhập DataFrame
# Lấy data frame từ gadm
gadm63.df <- gadm@data

# Xóa tỉnh "Hà Tây"
gadm63.df <- gadm63.df[-20, ]

# Trộn SP và DF
gadm63 <- SpatialPolygonsDataFrame(gadm63.sp, gadm63.df)

# Kiểm tra
plot(gadm[c(18,20),])
plot(gadm63[10, ], add = TRUE, border = "red", lwd = 2)


# Lưu lại dùng sau:
saveRDS(gadm63, file="geo63.rds")

# Bản đồ Việt Nam
geo63 <- readRDS("geo63.rds")
spplot(geo63, "ID_1", main="63 tỉnh")
