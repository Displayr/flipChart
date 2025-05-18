context("PrepareData: table manipulation")

# This is QTable with the Base n statistic
# Note that 'Statistics - Right' and 'Statistics - Below' are never passed
# to the R output

tabWithN <- structure(c(12.2448979591837, 6.12244897959184, 4.08163265306122,
6.12244897959184, 4.08163265306122, 8.16326530612245, 22.4489795918367,
19.3877551020408, 17.3469387755102, 100, 32.2033898305085, 13.5593220338983,
5.08474576271187, 10.1694915254237, 5.08474576271187, 0, 5.08474576271187,
13.5593220338983, 15.2542372881356, 100, 11.8811881188119, 12.8712871287129,
12.8712871287129, 13.3663366336634, 8.41584158415842, 13.3663366336634,
13.3663366336634, 8.91089108910891, 4.95049504950495, 100, 10.8843537414966,
17.687074829932, 11.5646258503401, 7.48299319727891, 16.3265306122449,
3.40136054421769, 6.12244897959184, 22.4489795918367, 4.08163265306122,
100, 12.5, 6.25, 18.75, 6.25, 9.375, 12.5, 18.75, 15.625, 0,
100, 3.57142857142857, 19.6428571428571, 8.92857142857143, 16.0714285714286,
26.7857142857143, 5.35714285714286, 10.7142857142857, 8.92857142857143,
0, 100, 13.0769230769231, 2.30769230769231, 12.3076923076923,
20, 13.8461538461538, 6.92307692307692, 11.5384615384615, 12.3076923076923,
7.69230769230769, 100, 6.57894736842105, 15.7894736842105, 7.89473684210526,
5.26315789473684, 11.8421052631579, 9.21052631578947, 9.21052631578947,
28.9473684210526, 5.26315789473684, 100, 98, 98, 98, 98, 98,
98, 98, 98, 98, 98, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 202,
202, 202, 202, 202, 202, 202, 202, 202, 202, 147, 147, 147, 147,
147, 147, 147, 147, 147, 147, 32, 32, 32, 32, 32, 32, 32, 32,
32, 32, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 130, 130, 130,
130, 130, 130, 130, 130, 130, 130, 76, 76, 76, 76, 76, 76, 76,
76, 76, 76, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
800, 800, 800, 800, 800), .Dim = c(10L, 8L, 3L), .Dimnames = list(
    c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
    "45 to 49", "50 to 54", "55 to 64", "65 or more", "NET"),
    c("Every or nearly every day", "4 to 5 days a week", "2 to 3 days a week",
    "Once a week", "Once every 2 weeks", "Once a month", "Less than once a month",
    "Never"), c("Column %", "Column n", "Base n")), name = "Age by Exercise frequency",
questions = c("Age", "Exercise frequency"))

tab1d <- structure(c(12.375, 11.75, 10.375, 11.375, 11.625, 7.875, 11.875,
     15.75, 7, 100, 800, 800, 800, 800, 800, 800, 800, 800, 800, 800,
     1.1375, 0.575, -0.6625, 0.237500000000001, 0.462500000000001,
     -2.9125, 0.6875, 4.175, -3.7, 80, 0.255329324592568, 0.565291296994401,
     0.507650834828445, 0.812268919201514, 0.643722802327307, 0.00358548200450471,
     0.491767700760523, 2.97986053738875e-05, 0.000215599466954778,
     0), .Dim = c(10L, 4L), .Dimnames = list(c("18 to 24", "25 to 29",
       "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
       "65 or more", "NET"), c("%", "Base n", "z-Statistic", "p")),
     name = "Age", questions = c("Age", "SUMMARY"))

tabAsChar <- structure(c("25.2327048028994", "31.2881504763389", "30.9835063713764",
                         "17.5546469946982", "33.2850525773139", "21.3918060868462", "14.8891326905278",
                         "99.999999992966", "32.1856718881156", "39.0384865558457", "38.7008828633533",
                         "23.043980297302", "41.2332045095876", "27.6781998955081", "19.7445739902877",
                         "99.9999999949984", "19.3901751154243", "24.5030129463578", "24.24112651755",
                         "13.1766253337998", "26.232192530017", "16.2453846830681", "11.0864828737829",
                         "99.9999999901313", "22.3980226347181", "28.0282076203562", "27.7424891486241",
                         "15.4048099182854", "29.9076106591684", "18.8796593933267", "13.0142006255211",
                         "99.9999999917754", "25.6383779654847", "31.7498370032718", "31.4427546535753",
                         "17.866375634708", "33.7617270166789", "21.753694692716", "15.1622330335653",
                         "99.9999999931149", "25.3986416353672", "31.4771442945425", "31.1714948165763",
                         "17.682032200925", "33.4802311923131", "21.5397609802415", "15.0006948800344",
                         "99.9999999930275", "18.6950249396378", "23.6784065423265", "23.422599150159",
                         "12.6692243563911", "25.3689477903583", "15.6411012525024", "10.6496959686249",
                         "99.9999999896761", "25.1773988570791", "31.2251150086532", "30.9208084668817",
                         "17.5122283403448", "33.2199390423663", "21.342515643933", "14.8519946407419",
                         "99.9999999929453", "38.3662387414414", "45.6491679510049", "45.2968741467223",
                         "28.1990522244833", "47.9233721374194", "33.4197641302466", "24.3955306686821",
                         "99.9999999961865", "87.274341983319", "90.2471601302471", "90.1213865181602",
                         "81.22746990624", "91.0222544777148", "84.6863241611387", "78.0460628231802",
                         "99.9999999996539", "B I", "C D E F G H", "C D G H", "B I", "C D E F G H",
                         "c d g H", "H i", "-", "i", "C D E F G H", "A C D G H", "I",
                         "C D E F G H", "A C D G H", "A D e H I", "-", "A B I", "E F H",
                         "D H", "B I", "D E F G H", "h", "A B D E F H I", "-", "A B C E F G I",
                         "E F H", "h", "A B C E F G I", "F H", "h", "H", "-", "A B C G I",
                         "F", "A C D G H", "A B C I", "D F H", "A B C D f G H I", "a D H I",
                         "-", "A B C G I", NA, "A B C D E G H", "A B C g I", NA, "A B C D G H",
                         "A D H I", "-", "a B I", "E F H", "D H", "a B c I", "D E F H",
                         NA, "A B C D E F H I", "-", "A B C D E F G I", "E F", NA, "A B C D E F G I",
                         NA, NA, NA, "-", NA, "A B C D E F G H", "A B C D E G H", NA,
                         "A B C D E F G H", "A B C D G H", "H", "-", "-", "-", "-", "-",
                         "-", "-", "-", "-"), .Dim = c(8L, 10L, 2L), .Dimnames = list(
                         c("Coke", "Diet Coke", "Coke Zero", "Pepsi", "Diet Pepsi",
                         "Pepsi Max", "None of these", "NET"), c("Feminine", "Health-conscious",
                         "Innocent", "Older", "Open to new experiences", "Rebellious",
                         "Sleepy", "Traditional", "Weight-conscious", "NET"),
                         c("Expected %", "Column Comparisons")), name = "q5 - column comparisons", questions = c("q5", "SUMMARY"))

x2d <- tabWithN[,,1]

# vector (with no dimensions)
x1d <- structure(1:10, .Names = c("a", "b", "c", "d", "e", "f", "g",
"h", "i", "j"))

# array (with dimension of 1-d)
array1d <- table(rpois(20, 4))
test_that("Select Rows",
{
    expect_warning(res <- PrepareData("Column", input.data.table = tabWithN, tidy = TRUE),
                   "Multiple statistics detected")

    expect_silent(res <- PrepareData("Table", input.data.table = tabWithN, tidy = FALSE))
    expect_equal(dim(res$data), c(9,8,3))

    expect_warning(res <- PrepareData("Column", input.data.table = tabWithN, select.rows = "30 to 34, 35 to 39"), "Multiple statistics detected")
    expect_equal(rownames(res$data), c("30 to 34", "35 to 39"))

    expect_warning(res <- PrepareData("Table", input.data.table = LifeCycleSavings, first.k.rows = 10,
                       sort.rows = TRUE, reverse.rows = TRUE, reverse.columns = TRUE))
    expect_equal(colnames(res$data), rev(colnames(LifeCycleSavings)))
    expect_equal(rownames(res$data), c("Libya", "Jamaica", "Japan", "Malta", "Netherlands",
                                       "Portugal", "China", "Greece", "Korea", "Zambia"))
    # hide rows before selecting first/last rows
    expect_warning(res <- PrepareData("Table", input.data.table = tabWithN, last.k.rows = 4,
                        row.names.to.remove = "65 or more, NET"), "Multiple statistics detected")
    expect_equal(rownames(res$data),c("40 to 44", "45 to 49", "50 to 54", "55 to 64"))
})

test_that("Sorting rows",
{
    expect_warning(res0 <- PrepareData("Table", input.data.table = tabWithN, tidy = FALSE,
            sort.rows = TRUE), "Table has been sorted on column 8")
    expect_equal(dim(res0$data), c(9,8,3))

    expect_warning(res1 <- PrepareData("Column", input.data.table = tabWithN, tidy = TRUE, sort.rows = TRUE),
                   "Multiple statistics detected")
    expect_equal(rownames(res0$data), rownames(res1$data))

    expect_warning(res2 <- PrepareData("Column", input.data.table = tabWithN, sort.rows = TRUE,
            column.names.to.remove = "Never"), "Multiple statistics detected")
    expect_equal(rownames(res2$data), c("35 to 39", "65 or more", "18 to 24",
        "30 to 34", "45 to 49", "50 to 54", "40 to 44", "25 to 29", "55 to 64"))
})

test_that("Automatic ordering",
{
    dat <- structure(c(17.6551724137931, 13.6551724137931, 20.2758620689655,
        34.2068965517241, 16.551724137931, 78.3448275862069, 13.9310344827586,
        20.6896551724138, 94.7586206896552, 23.3103448275862, 22.2068965517241,
        37.9310344827586, 37.5172413793103, 25.9310344827586, 45.2413793103448,
        19.7241379310345, 23.8620689655172, 95.448275862069, 16.9655172413793,
        10.2068965517241, 12.2758620689655, 41.7931034482759, 27.7241379310345,
        27.0344827586207, 32.2758620689655, 43.1724137931034, 95.1724137931034,
        17.7931034482759, 11.5862068965517, 20.8275862068966, 37.6551724137931,
        38.2068965517241, 13.9310344827586, 36, 34.7586206896552, 98.7586206896552,
        11.3103448275862, 9.79310344827586, 11.1724137931034, 32.9655172413793,
        36, 20.9655172413793, 36.8275862068965, 51.8620689655172, 96.4137931034483,
        34.2068965517241, 29.1034482758621, 39.448275862069, 26.3448275862069,
        25.5172413793103, 51.1724137931034, 20.8275862068966, 19.1724137931034,
        97.7931034482759, 9.51724137931034, 6.20689655172414, 5.93103448275862,
        47.448275862069, 11.8620689655172, 63.8620689655172, 9.24137931034483,
        45.1034482758621, 98.2068965517241, 15.7241379310345, 35.7241379310345,
        88.2758620689655, 3.58620689655172, 13.5172413793103, 1.93103448275862,
        14.2068965517241, 3.17241379310345, 99.0344827586207, 3.72413793103448,
        2.06896551724138, 2.48275862068966, 35.448275862069, 6.89655172413793,
        71.8620689655172, 4.13793103448276, 43.3103448275862, 98.3448275862069,
        32.9655172413793, 33.2413793103448, 46.4827586206897, 52.1379310344828,
        33.9310344827586, 34.2068965517241, 30.4827586206897, 33.9310344827586,
        97.6551724137931, 10.4827586206897, 9.24137931034483, 9.51724137931034,
        44, 21.5172413793103, 39.1724137931035, 15.1724137931034, 44.2758620689655,
        85.3793103448276, 4.55172413793103, 3.58620689655172, 3.72413793103448,
        62.8965517241379, 16, 60.8275862068966, 10.0689655172414, 53.6551724137931,
        95.7241379310345, 11.3103448275862, 7.72413793103448, 8.27586206896552,
        23.448275862069, 11.1724137931034, 69.5172413793103, 9.51724137931034,
        21.1034482758621, 91.0344827586207, 22.6206896551724, 32.1379310344828,
        65.3793103448276, 24.551724137931, 22.0689655172414, 19.3103448275862,
        18.2068965517241, 15.5862068965517, 95.8620689655172, 11.1724137931034,
        9.24137931034483, 10.2068965517241, 48.551724137931, 16.2758620689655,
        46.0689655172414, 12.551724137931, 42.6206896551724, 94.8965517241379,
        10.7586206896552, 8.27586206896552, 8.27586206896552, 44.6896551724138,
        20.4137931034483, 57.6551724137931, 12.1379310344828, 43.8620689655172,
        95.448275862069, 14.3448275862069, 12.2758620689655, 13.3793103448276,
        49.2413793103448, 25.6551724137931, 37.2413793103448, 23.1724137931034,
        50.0689655172414, 98.2068965517241, 37.6551724137931, 37.3793103448276,
        57.3793103448276, 40.2758620689655, 37.5172413793103, 49.7931034482759,
        33.3793103448276, 35.0344827586207, 94.7586206896552, 63.3103448275862,
        72, 24.2758620689655, 6.48275862068965, 28, 4.96551724137931,
        47.1724137931034, 11.3103448275862, 92.6896551724138, 93.6551724137931,
        93.3793103448276, 97.9310344827586, 98.2068965517241, 95.0344827586207,
        99.0344827586207, 94.4827586206897, 98.0689655172414, 99.0344827586207,
        18.8812865367469, 18.6873914152161, 24.9532380078143, 35.9189432991956,
        22.2926160553237, 41.227595340562, 20.426609058679, 32.9147514767701,
        95.5499844178, 20.9626508570219, 20.7528499995758, 27.4772158266968,
        38.9760327502179, 24.635820424588, 44.4232880404067, 22.6307966771533,
        35.8592333390535, 96.0732774941342, 18.4939506910547, 18.3031357012777,
        24.4789290648211, 35.3343369184194, 21.8541527994267, 40.6113441590399,
        20.0153940559839, 32.354353320749, 95.4403289227576, 18.428494518865,
        18.2382037647973, 24.3986311061917, 35.2350437306111, 21.7799817677866,
        40.5065108642247, 19.9458707662173, 32.2592566937499, 95.4213684108198,
        18.4399523924604, 18.2495697950757, 24.4126899967151, 35.2524351469724,
        21.7929667100187, 40.5248761161163, 19.9580412378151, 32.2759112944404,
        95.4246965558361, 22.0107359479287, 21.7933378601471, 28.7326071324124,
        40.463663874615, 25.8075894975009, 45.9622348159307, 23.7372367584339,
        37.300617624981, 96.3010970808892, 17.2768218242276, 17.0959307626027,
        22.9789630080342, 33.4639404271919, 20.4714245537934, 38.6284755795346,
        18.7211343377201, 30.5671356000059, 95.0656665510427, 15.0386149580832,
        14.8769438358378, 20.182271419026, 29.8863168344862, 17.9089554814062,
        34.7873904305477, 16.3327600458921, 27.172731440838, 94.2291415951822,
        14.4458364347862, 14.2894661241965, 19.4331192122233, 28.9074126446099,
        17.2259757782178, 33.7251717539369, 15.6983955141234, 26.2494329381992,
        93.9672358327422, 27.5704196274036, 27.3173432699466, 35.2233318532865,
        47.8260482395969, 31.9336263565784, 53.4272777683398, 29.5676816573771,
        44.5178230378969, 97.2310258539699, 16.6949728865843, 16.5189573564523,
        22.2567449762626, 32.5514573260804, 19.8077492622335, 37.6548649176822,
        18.1012874634252, 29.6983847788433, 94.8684480711984, 18.8822582531637,
        18.6883554518383, 24.9544260834714, 35.9204035768005, 22.2937150846664,
        41.2291325824524, 20.4276402769986, 32.9161523558492, 95.5502541656122,
        13.6957360468555, 13.5461996639202, 18.4799913264904, 27.6490733355298,
        16.3591199574243, 32.3525526879967, 14.8945789800919, 25.0660010161444,
        93.6057330374344, 19.3417942613796, 19.1442861754127, 25.515257026322,
        36.6074662822481, 22.812926318196, 41.9512603582732, 20.9150889466107,
        33.5758608199651, 95.6749466765067, 17.0290747123312, 16.8502492287871,
        22.6718580601105, 33.076886006566, 20.1890475201878, 38.2159960198072,
        18.4572971098443, 30.1983796295263, 94.983240249471, 17.9572503355103,
        17.7707655138411, 23.8193006594019, 34.5158817221267, 21.2453563282933,
        39.7457849578561, 19.4450745505214, 31.5712240156895, 95.2810192621828,
        19.9045161285035, 19.7026654754044, 26.1992470126767, 37.4393449009092,
        23.4472787865852, 42.8225425857857, 21.5113760742494, 34.3762062923946,
        95.820205383602, 31.0768339392893, 30.8052605258886, 39.1765956383204,
        52.0570560864126, 35.721278647289, 57.6066882398447, 33.2116343083796,
        48.7295094405976, 97.6522484322996, 23.2390834015737, 23.0131330372317,
        30.1908632807925, 42.1650469406046, 27.1739975958874, 47.7095452488098,
        25.0310105006889, 38.9560266964988, 96.5431228947029, 94.5578315198983,
        94.492055979158, 96.1271033068156, 97.6658232667802, 95.538659183812,
        98.1260636301102, 95.0402024622649, 97.3421980910691, 99.9376487669884
        ), .Dim = c(9L, 20L, 2L), .Dimnames = list(c("AAPT/Cellular One",
        "New Tel", "One-tel", "Optus", "Orange (Hutchison)", "Telstra (Mobile Net)",
        "Virgin Mobile", "Vodafone", "NET"), c("Bureaucratic", "Slow service",
        "Friendly", "Low prices", "Fashionable", "Unfashionable", "Reliable",
        "Here today, gone tomorrow", "Good coverage", "Network often down",
        "The best phones", "Conveniently located stores", "High prices",
        "Unreliable", "Meet all my communication needs", "Leaders in mobile phone technology",
        "I like them", "I hate them", "Don't know much about them", "NET"
        ), c("%", "Expected %")), name = "q20", questions = c("q20",
        "SUMMARY"))

    res <- PrepareData("Column", input.data.table = dat, auto.order.rows = TRUE,
                       reverse.rows = TRUE, tidy = FALSE)
    expect_equal(colnames(res$data), colnames(dat)[-20])
    expect_equal(rownames(res$data), rev(c("One-tel", "New Tel", "AAPT/Cellular One", "Virgin Mobile",
        "Orange (Hutchison)", "Optus", "Vodafone", "Telstra (Mobile Net)")))
    expect_equal(dim(res$data), c(8, 19, 2))
})

test_that("Preseve column name if SelectColumns is used",
{
    expect_warning(res <- PrepareData("Column", input.data.table = tabWithN, select.columns = "Never", tidy = TRUE),
        "Multiple statistics detected")
    expect_equal(dim(res$data), c(9, 1))
})

test_that("Statistics are preserved when percentages are computed",
{
    expect_warning(dat0 <- PrepareData("Column", input.data.table = tabWithN, tidy = TRUE, as.percentages = TRUE)$data)
    dat1 <- PrepareData("Column", input.data.table = tabWithN, tidy = FALSE, as.percentages = FALSE)$data
    dat2 <- PrepareData("Column", input.data.table = tabWithN, tidy = FALSE, as.percentages = TRUE)$data
    expect_equal(dat2[,,2], dat1[,,2])
    expect_equal(dat2[,,1], dat0, check.attributes = FALSE)

    expect_warning(dat0 <- PrepareData("Column", input.data.table = tab1d, tidy = TRUE, as.percentages = TRUE)$data)
    dat1 <- PrepareData("Column", input.data.table = tab1d, tidy = FALSE, as.percentages = FALSE)$data
    expect_error(dat2 <- PrepareData("Column", input.data.table = tab1d, tidy = FALSE, as.percentages = TRUE)$data, NA)
    expect_equal(dat2[,,2], dat1[,,2])
    expect_equal(dat2[,1,1], dat0, check.attributes = FALSE)
    #expect_equal(attr(dat1, "statistic"), "%")
    #expect_equal(attr(dat1, "multi-stat"), TRUE)

    expect_warning(dat0 <- PrepareData("Column", input.data.table = tabAsChar, tidy = TRUE, as.percentages = TRUE)$data)
    dat1 <- PrepareData("Column", input.data.table = tabAsChar, tidy = FALSE, as.percentages = FALSE)$data
    dat2 <- PrepareData("Column", input.data.table = tabAsChar, tidy = FALSE, as.percentages = TRUE)$data
    expect_equal(dat2[,,2], dat1[,,2])
    expect_equal(as.numeric(dat2[,,1]), dat0, check.attributes = FALSE)

})


data("EuStockMarkets")
# print(dim(EuStockMarkets))
# print(str(EuStockMarkets))
# print(dput(EuStockMarkets))
test_that("Time series object",
{
    res <- PrepareData("Column", input.data.table = EuStockMarkets, sort.columns = TRUE,
                sort.columns.row = 1860, first.k.columns = 1, tidy = FALSE)
    expect_equal(dimnames(res$data)[[2]], "CAC")
})

data("LifeCycleSavings")
test_that("Sort occurs after select",
{
    # Select top 5 coutries based on DPI
    res <- PrepareData("Column", input.data.table = LifeCycleSavings,
                sort.rows = TRUE, sort.rows.column = "dpi", last.k.rows = 5)
    expect_equal(rownames(res$data),
                c("Denmark", "Switzerland", "Canada", "Sweden", "United States"))
    res <- PrepareData("Column", input.data.table = LifeCycleSavings,
                sort.rows = TRUE, sort.rows.column = "dpi", last.k.rows = 1, first.k.rows = 1)
    expect_equal(rownames(res$data), c("India", "United States"))
})

tb.relpct <- structure(c(5.00326180684867, 7.285650634158, 8.40518598530936,
        1.65355506560255, 1.18760515702743, 0.521327814156639, 4.39786697496822,
        1.75140820408684, 1.11568225085694, 1.37727686766775, 7.61129131532569,
        34.9813460703661, 24.7085418536258, 14.268685096228, 20.6234084126415,
        22.0247934074517, 2.26928383507658, 0.444469618293972, 0.0491519931163263,
        11.1697915924708, 3.16938691746763, 3.1519169031485, 3.44376161925286,
        5.53629513579208, 6.1663839420065, 7.68267152705353, 6.42796457126485,
        8.53756247160265, 35.2207794777853, 3.89846718166861, 2.74433259262905,
        1.32146599595611, 6.65436732836921, 16.6711143529724, 5.56397740293424,
        1.84346974001887, 4.16942689473746, 3.25091211393703, 3.69615987612428,
        10.0841978739082, 9.71314106380638, 3.58472505077769, 10.6629950601532,
        6.63252870968269, 3.04727943867946, 13.4976897092017, 3.11989094055043,
        5.29730785599083, 5.04575174914577, 15.0743192309562, 6.89120634167006,
        7.3489669754774, 10.9982674808771, 13.48944192091, 11.835990363407,
        5.05938144178828, 2.89838529011283, 1.27445257728199, 10.8957430649813,
        5.15282897117355, 4.54116694290573, 4.03026496965101, 9.90802883293892,
        9.51897180615974, 10.3970763378126), statistic = "Relative importance (%)", .Dim = c(13L,
        5L), .Dimnames = list(c("Overall, I feel confident and optimistic that I could prepare for, cope with and recover from an epidemic event in the future",
        "I feel able to protect my home and family from a potential epidemic",
        "Even outside times of risk of illness, I regularly do things like clean and disinfect my house and its contents",
        "In reality, the risk of illness from an epidemic is pretty low in my area",
        "To be honest, preparing or planning for a potential future epidemic is not a top priority for me",
        "I dont really know what I should do to prepare or plan for a future epidemic",
        "I feel I can deal with whatever comes", "During a future epidemic I'd do whatever I could to protect myself and my family",
        "Rather than relying on authorities, I would make my own decisions about how to respond during a future epidemic",
        "I feel confident I have the financial resources to recover if my workplace and therefore my income was impacted by a future epidemic",
        "I feel confident we would receive timely and accurate information from authorities about what to do",
        "I could rely on my neighbours or friends to help me if I was affected by a future epidemic",
        "There is good community spirit in my area, people get involved and help out others"
        ), c("Segment 1", "Segment 2", "Segment 3", "Segment 4", "NET"
        )), basedescriptiontext = "base n = 999", basedescription = list(
            Minimum = 999L, Maximum = 999L, Range = FALSE, Total = 1000L,
            Missing = 0L, EffectiveSampleSize = 999L, EffectiveSampleSizeProportion = 100,
            FilteredProportion = 0), questiontypes = c("Ranking", "PickOne"
        ), span = list(rows = structure(list(c("Overall, I feel confident and optimistic that I could prepare for, cope with and recover from an epidemic event in the future",
        "I feel able to protect my home and family from a potential epidemic",
        "Even outside times of risk of illness, I regularly do things like clean and disinfect my house and its contents",
        "In reality, the risk of illness from an epidemic is pretty low in my area",
        "To be honest, preparing or planning for a potential future epidemic is not a top priority for me",
        "I dont really know what I should do to prepare or plan for a future epidemic",
        "I feel I can deal with whatever comes", "During a future epidemic I'd do whatever I could to protect myself and my family",
        "Rather than relying on authorities, I would make my own decisions about how to respond during a future epidemic",
        "I feel confident I have the financial resources to recover if my workplace and therefore my income was impacted by a future epidemic",
        "I feel confident we would receive timely and accurate information from authorities about what to do",
        "I could rely on my neighbours or friends to help me if I was affected by a future epidemic",
        "There is good community spirit in my area, people get involved and help out others"
        )), class = "data.frame", .Names = "", row.names = c(NA, 13L)),
            columns = structure(list(c("Segment 1", "Segment 2", "Segment 3",
            "Segment 4", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
            5L))), name = "table.AS.RANKING.Q17.How.strongly.do.you.agree.or.disagree.with.by.Segments.using.AS.RANKING.Q17", questions = c("[AS RANKING] Q17. How strongly do you agree or disagree with ... ?",
        "Segments using [AS RANKING] Q17"))

    test_that("Relative percent treated as percentage",
    {
        res <- PrepareData("Table", input.data.table = tb.relpct)
        expect_equal(res$data,
            structure(100*c(0.0500326180684867, 0.07285650634158, 0.0840518598530936,
            0.0165355506560255, 0.0118760515702743, 0.00521327814156639,
            0.0439786697496822, 0.0175140820408684, 0.0111568225085694, 0.0137727686766775,
            0.0761129131532569, 0.349813460703661, 0.247085418536258, 0.14268685096228,
            0.206234084126415, 0.220247934074517, 0.0226928383507658, 0.00444469618293972,
            0.000491519931163263, 0.111697915924708, 0.0316938691746763,
            0.031519169031485, 0.0344376161925286, 0.0553629513579208, 0.061663839420065,
            0.0768267152705353, 0.0642796457126485, 0.0853756247160265, 0.352207794777853,
            0.0389846718166861, 0.0274433259262905, 0.0132146599595611, 0.0665436732836921,
            0.166711143529724, 0.0556397740293424, 0.0184346974001887, 0.0416942689473746,
            0.0325091211393703, 0.0369615987612428, 0.100841978739082, 0.0971314106380638,
            0.0358472505077769, 0.106629950601532, 0.0663252870968269, 0.0304727943867946,
            0.134976897092017, 0.0311989094055043, 0.0529730785599083, 0.0504575174914577,
            0.150743192309562, 0.0689120634167006, 0.073489669754774), statistic = "Relative importance (%)", basedescriptiontext = "base n = 999", basedescription = list(
                Minimum = 999L, Maximum = 999L, Range = FALSE, Total = 1000L,
                Missing = 0L, EffectiveSampleSize = 999L, EffectiveSampleSizeProportion = 100,
                FilteredProportion = 0), questiontypes = c("Ranking", "PickOne"
            ), span = list(rows = structure(list(c("Overall, I feel confident and optimistic that I could prepare for, cope with and recover from an epidemic event in the future",
            "I feel able to protect my home and family from a potential epidemic",
            "Even outside times of risk of illness, I regularly do things like clean and disinfect my house and its contents",
            "In reality, the risk of illness from an epidemic is pretty low in my area",
            "To be honest, preparing or planning for a potential future epidemic is not a top priority for me",
            "I dont really know what I should do to prepare or plan for a future epidemic",
            "I feel I can deal with whatever comes", "During a future epidemic I'd do whatever I could to protect myself and my family",
            "Rather than relying on authorities, I would make my own decisions about how to respond during a future epidemic",
            "I feel confident I have the financial resources to recover if my workplace and therefore my income was impacted by a future epidemic",
            "I feel confident we would receive timely and accurate information from authorities about what to do",
            "I could rely on my neighbours or friends to help me if I was affected by a future epidemic",
            "There is good community spirit in my area, people get involved and help out others"
            )), class = "data.frame", .Names = "", row.names = c(NA, 13L)),
                columns = structure(list(c("Segment 1", "Segment 2", "Segment 3",
                "Segment 4", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
                5L))), name = "table.AS.RANKING.Q17.How.strongly.do.you.agree.or.disagree.with.by.Segments.using.AS.RANKING.Q17", questions = c("[AS RANKING] Q17. How strongly do you agree or disagree with ... ?",
            "Segments using [AS RANKING] Q17"), assigned.rownames = TRUE, .Dim = c(13L,
            4L), .Dimnames = list(c("Overall, I feel confident and optimistic that I could prepare for, cope with and recover from an epidemic event in the future",
            "I feel able to protect my home and family from a potential epidemic",
            "Even outside times of risk of illness, I regularly do things like clean and disinfect my house and its contents",
            "In reality, the risk of illness from an epidemic is pretty low in my area",
            "To be honest, preparing or planning for a potential future epidemic is not a top priority for me",
            "I dont really know what I should do to prepare or plan for a future epidemic",
            "I feel I can deal with whatever comes", "During a future epidemic I'd do whatever I could to protect myself and my family",
            "Rather than relying on authorities, I would make my own decisions about how to respond during a future epidemic",
            "I feel confident I have the financial resources to recover if my workplace and therefore my income was impacted by a future epidemic",
            "I feel confident we would receive timely and accurate information from authorities about what to do",
            "I could rely on my neighbours or friends to help me if I was affected by a future epidemic",
            "There is good community spirit in my area, people get involved and help out others"
            ), c("Segment 1", "Segment 2", "Segment 3", "Segment 4"))))

})

tb1d.spans <- structure(c(Male = 49.375, Female = 50.625, NET = 100, `18 to 24` = 12.375,
`25 to 29` = 11.75, `30 to 34` = 10.375, `35 to 39` = 11.375,
`40 to 44` = 11.625, `45 to 49` = 7.875, `50 to 54` = 11.875,
`55 to 64` = 15.75, `65 or more` = 7, NET = 100), statistic = "%", dim = 13L, dimnames = list(
    c("Male", "Female", "NET", "18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET")), class = c("array", "QTable"), dimnets = list(
    integer(0)), dimduplicates = list(integer(0)), span = list(
    rows = structure(list(c("Gender", "Gender", "Gender", "Age",
    "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age"
    ), c("Male", "Female", "NET", "18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET")), class = "data.frame", names = c("",
    ""), row.names = c(NA, 13L))), basedescriptiontext = "sample size = 800", basedescription = list(
    Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), QStatisticsTestingInfo = structure(list(
    significancearrowratio = structure(c(1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1), dim = 13L), significancedirection = structure(c("Up",
    "Up", "Up", "Down", "Down", "Down", "Down", "Down", "Down",
    "Down", "Down", "Down", "Up"), dim = 13L), significancefontsizemultiplier = structure(c(4.89,
    4.89, 4.89, 0.204498977505112, 0.204498977505112, 0.204498977505112,
    0.204498977505112, 0.204498977505112, 0.204498977505112,
    0.204498977505112, 0.204498977505112, 0.204498977505112,
    4.89), dim = 13L), significanceissignificant = structure(c(TRUE,
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, TRUE), dim = 13L), significanceargbcolor = structure(c(-16776961L,
    -16776961L, -16776961L, -65536L, -65536L, -65536L, -65536L,
    -65536L, -65536L, -65536L, -65536L, -65536L, -16776961L), dim = 13L),
    backgroundargbcolor = structure(c(0L, 0L, 0L, 0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L), dim = 13L), zstatistic = structure(c(11.4020968466331,
    12.1681291929185, 42.4264068711928, -11.2724606034155, -11.6554767765583,
    -12.4981123574722, -11.8852864804439, -11.7320800111868,
    -14.0301770500431, -11.5788735419297, -9.20417326844489,
    -14.5663996924429, 42.4264068711928), dim = 13L), pcorrected = structure(c(0,
    0, 0, 1.79482771878254e-29, 2.15172670928116e-31, 7.64449328139232e-36,
    1.41150291299093e-32, 8.72869765128773e-32, 1.01896870191481e-44,
    5.27344335539372e-31, 3.44311401819752e-20, 4.59476185309941e-48,
    0), dim = 13L)), class = "data.frame", row.names = c(NA,
13L)), questiontypes = "PickAny", footerhtml = "BANNER SUMMARY&lt;br /&gt;sample size = 800; 95% confidence level", name = "BANNER", questions = c("BANNER",
"SUMMARY"))
tb2d.with.rowspan <- structure(c(34.8484848484849, 65.1515151515152, 100, 10.6060606060606,
10.6060606060606, 10.6060606060606, 4.54545454545455, 13.6363636363636,
15.1515151515152, 10.6060606060606, 15.1515151515152, 9.09090909090909,
100, 100, 49.5121951219512, 50.4878048780488, 100, 11.7073170731707,
11.219512195122, 8.78048780487805, 9.02439024390244, 10, 9.26829268292683,
12.6829268292683, 17.0731707317073, 10.2439024390244, 100, 100,
52.1604938271605, 47.8395061728395, 100, 13.5802469135802, 12.6543209876543,
12.3456790123457, 15.7407407407407, 13.2716049382716, 4.62962962962963,
11.1111111111111, 14.1975308641975, 2.46913580246914, 100, 100,
49.375, 50.625, 100, 12.375, 11.75, 10.375, 11.375, 11.625, 7.875,
11.875, 15.75, 7, 100, 100), statistic = "Column %", dim = c(14L,
4L), dimnames = list(c("Male", "Female", "NET", "18 to 24", "25 to 29",
"30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
"65 or more", "NET", "NET"), c("I am on a diet, so I tend to watch what I eat and drink",
"I tend watch what I eat and drink, but don’t consider myself",
"I typically eat and drink whatever I feel like", "NET")), class = c("matrix",
"array", "QTable"), dimnets = list(14L, 4L), dimduplicates = list(
    14L, 4L), span = list(rows = structure(list(c("Gender", "Gender",
"Gender", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age",
"Age", "Age", NA), c("Male", "Female", "NET", "18 to 24", "25 to 29",
"30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
"65 or more", "NET", "NET")), class = "data.frame", names = c("",
""), row.names = c(NA, 14L)), columns = structure(list(c("I am on a diet, so I tend to watch what I eat and drink",
"I tend watch what I eat and drink, but don’t consider myself",
"I typically eat and drink whatever I feel like", "NET")), class = "data.frame", names = "", row.names = c(NA,
4L))), basedescriptiontext = "sample size = 800", basedescription = list(
    Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), QStatisticsTestingInfo = structure(list(
    significancearrowratio = structure(c(0.246786632390746, 0,
    0, 0, 0.246786632390746, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0.246786632390746, 0.465295629820051,
    0, 0, 0, 0, 0, 0.246786632390746, 0, 0.465295629820051, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0.588688946015424, 0.74293059125964,
    0, 0, 0, 0, 0, 0, 0, 0, 0), dim = 56L), significancedirection = structure(c("Down",
    "None", "None", "None", "Up", "None", "None", "None", "None",
    "None", "None", "None", "None", "None", "None", "None", "None",
    "None", "None", "None", "None", "None", "None", "None", "None",
    "Down", "Up", "None", "None", "None", "None", "None", "Up",
    "None", "Down", "None", "None", "None", "None", "None", "None",
    "None", "None", "None", "None", "Up", "Down", "None", "None",
    "None", "None", "None", "None", "None", "None", "None"), dim = 56L),
    significancefontsizemultiplier = structure(c(0.510204081632653,
    1, 1, 1, 1.96, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 0.510204081632653, 2.81, 1, 1, 1, 1, 1,
    1.96, 1, 0.355871886120996, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    3.29, 0.25706940874036, 1, 1, 1, 1, 1, 1, 1, 1, 1), dim = 56L),
    significanceissignificant = structure(c(TRUE, FALSE, FALSE,
    FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE,
    FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), dim = 56L),
    significanceargbcolor = structure(c(-65536L, -8355712L, -8355712L,
    -8355712L, -16776961L, -8355712L, -8355712L, -8355712L, -8355712L,
    -8355712L, -8355712L, -8355712L, -8355712L, -8355712L, -8355712L,
    -8355712L, -8355712L, -8355712L, -8355712L, -8355712L, -8355712L,
    -8355712L, -8355712L, -8355712L, -8355712L, -65536L, -16776961L,
    -8355712L, -8355712L, -8355712L, -8355712L, -8355712L, -16776961L,
    -8355712L, -65536L, -8355712L, -8355712L, -8355712L, -8355712L,
    -8355712L, -8355712L, -8355712L, -8355712L, -8355712L, -8355712L,
    -16776961L, -65536L, -8355712L, -8355712L, -8355712L, -8355712L,
    -8355712L, -8355712L, -8355712L, -8355712L, -8355712L), dim = 56L),
    backgroundargbcolor = structure(c(0L, 0L, 0L, 0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L), dim = 56L), zstatistic = structure(c(-2.46430410396057,
    0.0795806012424474, 1.30011015397555, NaN, 2.46430410396057,
    -0.0795806012424474, -1.30011015397555, NaN, NaN, NaN, NaN,
    NaN, -0.455612117101389, -0.588014817629355, 0.854089594757057,
    NaN, -0.301297589432649, -0.477753340731895, 0.655330441078807,
    NaN, 0.0642667299386742, -1.51643509388075, 1.5080692220922,
    NaN, -1.82434472132931, -2.14699644791851, 3.20861889801731,
    NaN, 0.532228750874652, -1.4702689629657, 1.19878759878797,
    NaN, 2.29128091462373, 1.50014397827836, -2.81166941935835,
    NaN, -0.332692826948818, 0.724286306497465, -0.551033295254478,
    NaN, -0.139346754328631, 1.05340474507031, -0.994515974143204,
    NaN, 0.695046063335142, 3.68707446075828, -4.14385521918865,
    NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN), dim = 56L),
    pcorrected = structure(c(0.0137279582372565, 0.936570824241361,
    0.19356321801095, NaN, 0.0137279582372565, 0.936570824241361,
    0.19356321801095, NaN, NaN, NaN, NaN, NaN, 0.648668928213484,
    0.556522347028733, 0.39305534877777, NaN, 0.763187578923791,
    0.632825774278165, 0.512255025650413, NaN, 0.948757844333437,
    0.129409370852577, 0.131536811435796, NaN, 0.0680999734029903,
    0.0317935644032437, 0.00133374150303678, NaN, 0.594567572401358,
    0.141488923495995, 0.230610545835739, NaN, 0.0219471717154441,
    0.133577111123972, 0.00492851278988693, NaN, 0.739366164674592,
    0.46888998201993, 0.581610850537185, NaN, 0.889176145632785,
    0.292155530369392, 0.319971732404236, NaN, 0.487026434606843,
    0.000226846995657892, 0.0000341515402061399, NaN, NaN, NaN,
    NaN, NaN, NaN, NaN, NaN, NaN), dim = 56L)), class = "data.frame", row.names = c(NA,
56L)), questiontypes = c("PickAny", "PickOne"), footerhtml = "BANNER by Weight-consciousness&lt;br /&gt;sample size = 800; 95% confidence level", name = "BANNER by Weight-consciousness", questions = c("BANNER",
"Weight-consciousness"))
    test_that("Table with Spans", {
        assign("ALLOW.QTABLE.CLASS", TRUE, envir = .GlobalEnv)
        res <- PrepareData("Table", input.data.table = tb1d.spans, tidy = FALSE,
                    row.names.to.remove = "", column.names.to.remove = "")
        expect_equal(attr(res$data, "span"),
            list(rows = structure(list(c("Gender", "Gender", "Gender", "Age",
                "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age"
                ), c("Male", "Female", "NET", "18 to 24", "25 to 29", "30 to 34",
                "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
                "NET")), names = c("", ""), row.names = c(NA, 13L), class = "data.frame")))

        res <- PrepareData("Table", input.data.table = tb1d.spans, tidy = FALSE,
                    sort.rows = TRUE)
        expect_equal(attr(res$data, "span")$rows,
            structure(list(c("Age", "Age", "Age", "Age", "Age", "Age", "Age",
                "Age", "Age", "Gender", "Gender"), c("65 or more", "45 to 49",
                "30 to 34", "35 to 39", "40 to 44", "25 to 29", "50 to 54", "18 to 24",
                "55 to 64", "Male", "Female")), names = c("", ""), row.names = c(12L,
                9L, 6L, 7L, 8L, 5L, 10L, 4L, 11L, 1L, 2L), class = "data.frame"))

        res <- PrepareData("Table", input.data.table = tb1d.spans, tidy = FALSE,
                    transpose = TRUE, row.names.to.remove = NULL,
                    column.names.to.remove = NULL)
        expect_equal(attr(res$data, "span"),
            list(rows = structure(list(), names = character(0), row.names = integer(0), class = "data.frame"),
                columns = structure(list(c("Gender", "Gender",
                "Gender", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age",
                "Age", "Age"), c("Male", "Female", "NET", "18 to 24", "25 to 29",
                "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
                "65 or more", "NET")), names = c("", ""), row.names = c(NA, 13L
                ), class = "data.frame")))

        res <- PrepareData("Table", input.data.table = tb1d.spans, tidy = FALSE,
                    transpose = TRUE)
        expect_equal(colnames(res$data), c("Male", "Female", "18 to 24",
                "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                "50 to 54", "55 to 64", "65 or more"))
        expect_equal(attr(res$data, "span"), list(
                columns = structure(list(c("Gender", "Gender",
                "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age"),
                c("Male", "Female", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
                "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more")),
                names = c("", ""), row.names = c(1L, 2L, 4L, 5L, 6L, 7L, 8L, 9L,
                10L, 11L, 12L), class = "data.frame")))

        res <- PrepareData("Table", input.data.table = tb2d.with.rowspan, tidy = FALSE)
        expect_equal(attr(res$data, "span"),
            list(rows = structure(list(c("Gender", "Gender", "Age", "Age",
                "Age", "Age", "Age", "Age", "Age", "Age", "Age"), c("Male", "Female",
                "18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49",
                "50 to 54", "55 to 64", "65 or more")), names = c("", ""), row.names = c(1L,
                2L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L), class = "data.frame"),
                columns = structure(list(c("I am on a diet, so I tend to watch what I eat and drink",
                "I tend watch what I eat and drink, but don’t consider myself",
                "I typically eat and drink whatever I feel like")), names = "",
                row.names = c(NA, 3L), class = "data.frame")))

        res <- PrepareData("Table", input.data.table = tb2d.with.rowspan, tidy = FALSE,
                transpose = TRUE, column.names.to.remove = "NET, 65 or more")
        expect_equal(attr(res$data, "span"),
             list(rows = structure(list(c("I am on a diet, so I tend to watch what I eat and drink",
                "I tend watch what I eat and drink, but don’t consider myself",
                "I typically eat and drink whatever I feel like")), names = "", row.names = c(NA,
                3L), class = "data.frame"), columns = structure(list(c("Gender",
                "Gender", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age"
                ), c("Male", "Female", "18 to 24", "25 to 29", "30 to 34", "35 to 39",
            "40 to 44", "45 to 49", "50 to 54", "55 to 64")), names = c("",
            ""), row.names = c(1L, 2L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L), class = "data.frame")))


        res <- PrepareData("Table", input.data.table = tb2d.with.rowspan, tidy = FALSE,
                           first.k.rows = 5)
        expect_equal(attr(res$data, "span")$rows, structure(list(c("Gender",
                "Gender", "Age", "Age", "Age"), c("Male", "Female",
                "18 to 24", "25 to 29", "30 to 34")), names = c("", ""),
                row.names = c(1L, 2L, 4L, 5L, 6L), class = "data.frame"))

        expect_warning(res <- PrepareData("Table", input.data.table = tb2d.with.rowspan,
                tidy = FALSE, sort.rows = TRUE, sort.rows.column = 1))
        expect_equal(attr(res$data, "span")$rows, structure(list(c("Age",
                "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Age", "Gender", "Gender"),
                c("35 to 39", "65 or more", "18 to 24", "25 to 29", "30 to 34",
                 "50 to 54", "40 to 44", "45 to 49", "55 to 64", "Male", "Female")),
                names = c("", ""), row.names = c(7L, 12L, 4L, 5L, 6L, 10L, 8L, 9L, 11L, 1L, 2L),
                class = "data.frame"))
    remove(ALLOW.QTABLE.CLASS, envir = .GlobalEnv)
})
