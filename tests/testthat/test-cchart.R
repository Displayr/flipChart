context("CChart: miscellaneous tests")

test_that("flipStandardCharts::Chart chart functions",{
    library(flipStandardCharts)
    # Data
    pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
    names(pie.sales) <- c("Blueberry", "Cherry", "Apple", "Boston Cream", "Other", "Vanilla Cream")
    # Chart
    #print(Chart(pie.sales, title = "My pie sales", title.font.size = 20, title.font.color = "red", x.title = "Pie type", x.title.font.color = "brown"))
    # CChart
    print(CChart("Pie", pie.sales, title = "My pie sales", title.font.size = 20, title.font.color = "red", x.title = "Pie type", x.title.font.color = "brown", warn.if.no.match = F))
    # Capitalization
    print(CChart("Pie", pie.sales, title = "My pie sales", title.font.Size = 20, title.Font.color = "red", x.title = "Pie type", x.title.font.color = "brown", warn.if.no.match = F))
    # Re-ordering
    print(CChart("Pie", pie.sales, title = "My pie sales", title.font.Size = 20, color.title.Font = "red", x.title = "Pie type", x.title.font.color = "brown", warn.if.no.match = F))
    # Substitution of parameter name
    print(CChart("Pie", pie.sales, main = "My pie sales", title.font.Size = 20, color.title.Font = "red", x.title = "Pie type", x.title.font.color = "brown", warn.if.no.match = F))
    # Substitution of part of a parameter name (within . as a delimiter)
    print(CChart("Pie", pie.sales, main = "My pie sales", font.Size.main = 20, main.font.color = "red", xlab = "Pie type", x.title.font.color = "brown", warn.if.no.match = F))
    # Non-supported arugment
    expect_warning(print(CChart("Pie", pie.sales, sfdsmain = "My pie sales", font.Size.main = 20, main.font.color = "red", xlab = "Pie type", x.title.font.color = "brown")))
    expect_warning(print(CChart("Pie", pie.sales, sfdsmain = "My pie sales")))
    expect_warning(print(CChart("Pie", pie.sales, sfdsmain = "My pie sales", warn.if.no.match = FALSE)), NA)
    expect_warning(print(CChart("Column", pie.sales, sfdsmain = "My pie sales", warn.if.no.match = FALSE)), NA)
    expect_warning(print(CChart("Scatter", x=1:10, y=1:10, sfdsmain = "My pie sales", warn.if.no.match = FALSE)), NA)
# expect_warning(print(CChart("Scatter", x=matrix(1:12, 6, 2, dimnames=list(letters[1:6], c("X", "Y"))),
#                             scatter.labels.as.hovertext = FALSE,
#                             main = "My pie sales", warn.if.no.match = FALSE)), NA)
    expect_warning(print(CChart("Scatter", x=1:10, y=2:11, scatter.sizes=factor(letters[1:10]))))

    # Axis names
    CChart("Distribution", list(rnorm(100)), values.tick.format=".2f")
    CChart("Bar", c(A=1, B=2, C=3), values.tick.format=".2f")
    #CChart("Stacked Bar", cbind(X=1:10, Y=1:10), values.tick.format=".2f")
    CChart("Bar", cbind(X=1:10, Y=1:10), values.tick.format=".2f", series.stack = T )
    CChart("Column", c(A=1, B=2, C=3), values.tick.format=".2f")
})

test_that("Comparing parameters",{
    # Exactly equal (after re-arranging)
    expect_true(flipChart:::parametersEqual("x.axis.title", "x.axis.title"))
    expect_true(flipChart:::parametersEqual("x.axis.title", "axis.x.title"))
    expect_false(flipChart:::parametersEqual("x.axis.title", "xlab"))
    expect_false(flipChart:::parametersEqual("xlab", "x.axis.title"))
    expect_false(flipChart:::parametersEqual("x.axis.title", "x.title"))
    expect_false(flipChart:::parametersEqual("x.axis.title", "axis.title"))
    expect_false(flipChart:::parametersEqual("colors", "main"))
 })

test_that("selecting chart functions",{

    ####    Reproducing pie charts, with focus on classic pie charts
    requireNamespace("grDevices")
    # Original
    pie(rep(1, 24), col = rainbow(24), radius = 0.9)
    # Called via CChart
    expect_error(print(CChart("pie", rep(1, 24))), NA)
    # Data pased in as a variable
    x = rep(1, 24)
    CChart("pie", x)

    # Original
    pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
    names(pie.sales) <- c("Blueberry", "Cherry", "Apple", "Boston Cream", "Other", "Vanilla Cream")
    pie(pie.sales, col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))
    # CChart
    CChart("pie", pie.sales, col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))

    # Various parameters of pie
    CChart("pie", pie.sales, col = rainbow(24), radius = 0.4)
    colr = gray(seq(0.4, 1.0, length = 6))
    CChart("pie", pie.sales, col = colr)

    CChart("pie", pie.sales, clockwise = TRUE, main = "pie(*, clockwise = TRUE)")
    segments(0, 0, 0, 1, col = "red", lwd = 2)
    text(0, 1, "init.angle = 90", col = "red")

    n <- 200
    CChart("pie", rep(1, n), labels = "", col = rainbow(n), border = NA,
        main = "pie(*, labels=\"\", col=rainbow(n), border=NA,..")

    ## Another case showing pie() is rather fun than science:
    ## (original by FinalBackwardsGlance on http://imgur.com/gallery/wWrpU4X)
    dt = c(Sky = 78, "Sunny side of pyramid" = 17, "Shady side of pyramid" = 5)
    cols = c("deepskyblue", "yellow", "yellow3")
    CChart("pie", dt, init.angle = 315, col = cols, border = FALSE)

})

test_that("Appending data",{

    ####    Reproducing pie charts, with focus on classic pie charts
    requireNamespace("grDevices")
    z = CChart("Pie", rep(1, 24), append.data = TRUE, warn.if.no.match = FALSE)
    expect_true(!is.null(attr(z, "ChartData")))
    z = CChart("Pie", rep(1, 24), append.data = FALSE, warn.if.no.match = FALSE)
    expect_true(is.null(attr(z, "ChartData")))
    expect_error(CChart("pie", rep(1, 24), append.data = TRUE, warn.if.no.match = FALSE))
    z = CChart("pie", rep(1, 24), append.data = FALSE, warn.if.no.match = FALSE)
    expect_true(is.null(attr(z, "ChartData")))
})


test_that("Scatterplots", {

    # DS-1657
    tab3 <- structure(c(1, 2, 3, 4), .Dim = c(4L, 1L), .Dimnames = list(c("Apple", "Microsoft", "Google", "Yahoo"), "Price"))
    tab4 <- structure(c(1, 2, 3, 4), .Dim = c(4L, 1L), .Dimnames = list(c("Apple","Microsoft", "Google", "Yahoo"), "Price"))
    pd <- PrepareData("Scatter", input.data.tables = list(tab3, tab4))
    expect_warning(CChart("Scatter",
                          pd$data,
                          scatter.x.column = pd$scatter.variable.indices["x"],
                          scatter.y.column = pd$scatter.variable.indices["y"],
                          scatter.sizes.column = pd$scatter.variable.indices["sizes"],
                          scatter.colors.column = pd$scatter.variable.indices["colors"],
                          trend.lines = TRUE), "Chart contains overlapping points")
})



test_that("Tables as an input to histograms", {

    tab3 <- structure(c(853, 854, 855, 851, 852, 883, 884, 885, 881, 882,
713, 714, 715, 711, 712, 1351, 1352, 1353, 1354, 1355, 871, 872,
873, 874, 875, 761, 762, 763, 764, 765, 411, 412, 413, 414, 415,
791, 792, 793, 794, 795, 941, 942, 943, 944, 945, 1023, 1024,
1025, 1021, 1022, 911, 912, 913, 914, 915, 841, 842, 843, 844,
845, 893, 894, 895, 891, 892, 1034, 1033, 1035, 1031, 1032, 1171,
1172, 1173, 1174, 1175, 691, 692, 693, 694, 695, 631, 632, 633,
634, 635, 1311, 1312, 1313, 1314, 1315, 231, 232, 233, 234, 235,
551, 552, 553, 554, 555, 131, 132, 133, 134, 135, 1141, 1142,
1143, 1144, 1145, 483, 484, 485, 481, 482, 543, 544, 545, 541,
542, 1671, 1672, 1673, 1674, 1675, 1691, 1692, 1693, 1694, 1695,
1603, 1604, 1605, 1601, 1602, NaN, NaN, NaN, NaN, NaN, 1541,
1542, 1543, 1544, 1545, 1621, 1622, 1623, 1624, 1625, 571, 572,
573, 574, 575, 1631, 1632, 1633, 1634, 1635, 1684, 1685, 1682,
1683, 1681, 1511, 1512, 1513, 1514, 1515, 1163, 1164, 1165, 1161,
1162, 823, 824, 825, 821, 822, 1013, 1014, 1015, 1012, 1011,
863, 864, 865, 861, 862, 383, 384, 385, 381, 382, 813, 814, 815,
811, 812, 200029169, 200029169, 200029169, 200029169, 200029169,
43, 44, 45, 41, 42, 1193, 1194, 1195, 1191, 1192, 991, 992, 993,
994, 995, 371, 372, 373, 374, 375, 5417, 5417, 5417, 5417, 5417,
122, 122, 122, 122, 122, 1101, 1102, 1103, 1104, 1105, 1291,
1292, 1293, 1294, 1295, 573, 574, 575, 572, 571, 533, 534, 535,
531, 532, 593, 594, 595, 591, 592, 1211, 1212, 1213, 1214, 1215,
472, 473, 475, 471, 474, 1203, 1204, 1205, 1201, 1202, 963, 964,
965, 961, 962, 571, 572, 573, 574, 575, 1023, 1024, 1025, 1021,
1022, 621, 622, 623, 624, 625, 1423, 1424, 1425, 1421, 1422,
1371, 1372, 1373, 1374, 1375, 641, 642, 643, 644, 645, 573, 574,
575, 571, 572, 1261, 1262, 1263, 1264, 1265, 783, 784, 785, 781,
782, 31, 32, 33, 34, 35, 701, 702, 703, 704, 705, 901, 902, 903,
904, 905, 201, 202, 203, 204, 205, 511, 512, 513, 514, 515, 141,
142, 143, 144, 145, 161, 162, 163, 164, 165, 773, 774, 775, 771,
772, 664, 663, 661, 662, 665, 311, 312, 313, 314, 315, 261, 262,
263, 264, 265, 741, 742, 743, 744, 745, 281, 282, 283, 284, 285,
571, 572, 573, 574, 575, 833, 834, 835, 831, 832, 671, 672, 673,
674, 675, 21, 22, 23, 24, 25, 1321, 1322, 1703, 1704, 1705, 1331,
1332, 1333, 1334, 1335, 1493, 1494, 1495, 1491, 1492, 1481, 1482,
1483, 1484, 1485, 291, 292, 293, 294, 295, 251, 252, 253, 254,
255, 681, 682, 683, 684, 685, 1271, 1272, 1273, 1274, 1275, 71,
72, 73, 74, 75, 391, 392, 393, 394, 395, 451, 452, 453, 454,
455, 1441, 1442, 1443, 1444, 1445, 343, 344, 345, 341, 342, 61,
61, 61, 61, 61, 331, 332, 333, 334, 335, 124, 124, 124, 124,
124, 931, 932, 933, 934, 935, 1361, 1362, 1363, 1364, 1365, 491,
492, 493, 494, 495, 301, 302, 303, 304, 305, 1611, 1612, 1613,
1614, 1615, 463, 464, 465, 461, 462, 423, 424, 425, 422, 421,
401, 402, 403, 404, 405, 601, 602, 603, 604, 605, 271, 272, 273,
274, 275, 1471, 1472, 1473, 1474, 1475, 723, 724, 725, 721, 722,
1231, 1232, 1233, 1234, 1235, 181, 182, 183, 184, 185, 321, 322,
323, 324, 325, 653, 654, 655, 652, 651, 1581, 1582, 1583, 1584,
1585, NaN, NaN, NaN, NaN, NaN, 563, 564, 565, 561, 562, 171,
172, 173, 174, 175, 61, 62, 63, 64, 65, 981, 982, 983, 984, 985,
1671, 1672, 1673, 1674, 1675, 106, 106, 106, 106, 106, 1091,
1092, 1093, 1094, 1095, 1301, 1302, 1303, 1304, 1305, 1651, 1652,
1653, 1654, 1655, 921, 922, 923, 924, 925, 573, 574, 575, 571,
572, 1571, 1572, 1573, 1574, 1575, 501, 502, 503, 504, 505, 361,
362, 363, 364, 365, 1133, 1134, 1135, 1131, 1132, 1051, 1052,
1053, 1054, 1055, NaN, NaN, NaN, NaN, NaN, 1701, 1702, 1703,
1704, 1705, 1521, 1522, 1523, 1524, 1525, 213, 214, 215, 211,
212, 111, 112, 113, 114, 115, 351, 352, 353, 354, 355, 951, 952,
953, 954, 955, 801, 802, 803, 804, 805, 1531, 1532, 1533, 1534,
1535, 1561, 1562, 1563, 1564, 1565, 1381, 1382, 1383, 1384, 1385,
1041, 1042, 1043, 1044, 1045, 53, 54, 55, 51, 52), .Dim = 725L, .Dimnames = structure(list(
    `IID - Interviewer Identification` = c("1", "2", "3", "4",
    "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
    "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
    "26", "27", "28", "29", "30", "31", "32", "33", "34", "35",
    "36", "37", "38", "39", "40", "41", "42", "43", "44", "45",
    "46", "47", "48", "49", "50", "51", "52", "53", "54", "55",
    "56", "57", "58", "59", "60", "61", "62", "63", "64", "65",
    "66", "67", "68", "69", "70", "71", "72", "73", "74", "75",
    "76", "77", "78", "79", "80", "81", "82", "83", "84", "85",
    "86", "87", "88", "89", "90", "91", "92", "93", "94", "95",
    "96", "97", "98", "99", "100", "101", "102", "103", "104",
    "105", "106", "107", "108", "109", "110", "111", "112", "113",
    "114", "115", "116", "117", "118", "119", "120", "121", "122",
    "123", "124", "125", "126", "127", "128", "129", "130", "131",
    "132", "133", "134", "135", "136", "137", "138", "139", "140",
    "141", "142", "143", "144", "145", "146", "147", "148", "149",
    "150", "151", "152", "153", "154", "155", "156", "157", "158",
    "159", "160", "161", "162", "163", "164", "165", "166", "167",
    "168", "169", "170", "171", "172", "173", "174", "175", "176",
    "177", "178", "179", "180", "181", "182", "183", "184", "185",
    "186", "187", "188", "189", "190", "191", "192", "193", "194",
    "195", "196", "197", "198", "199", "200", "201", "202", "203",
    "204", "205", "206", "207", "208", "209", "210", "211", "212",
    "213", "214", "215", "216", "217", "218", "219", "220", "221",
    "222", "223", "224", "225", "226", "227", "228", "229", "230",
    "231", "232", "233", "234", "235", "236", "237", "238", "239",
    "240", "241", "242", "243", "244", "245", "246", "247", "248",
    "249", "250", "251", "252", "253", "254", "255", "256", "257",
    "258", "259", "260", "261", "262", "263", "264", "265", "266",
    "267", "268", "269", "270", "271", "272", "273", "274", "275",
    "276", "277", "278", "279", "280", "281", "282", "283", "284",
    "285", "286", "287", "288", "289", "290", "291", "292", "293",
    "294", "295", "296", "297", "298", "299", "300", "301", "302",
    "303", "304", "305", "306", "307", "308", "309", "310", "311",
    "312", "313", "314", "315", "316", "317", "318", "319", "320",
    "321", "322", "323", "324", "325", "326", "327", "328", "329",
    "330", "331", "332", "333", "334", "335", "336", "337", "338",
    "339", "340", "341", "342", "343", "344", "345", "346", "347",
    "348", "349", "350", "351", "352", "353", "354", "355", "356",
    "357", "358", "359", "360", "361", "362", "363", "364", "365",
    "366", "367", "368", "369", "370", "371", "372", "373", "374",
    "375", "376", "377", "378", "379", "380", "381", "382", "383",
    "384", "385", "386", "387", "388", "389", "390", "391", "392",
    "393", "394", "395", "396", "397", "398", "399", "400", "401",
    "402", "403", "404", "405", "406", "407", "408", "409", "410",
    "411", "412", "413", "414", "415", "416", "417", "418", "419",
    "420", "421", "422", "423", "424", "425", "426", "427", "428",
    "429", "430", "431", "432", "433", "434", "435", "436", "437",
    "438", "439", "440", "441", "442", "443", "444", "445", "446",
    "447", "448", "449", "450", "451", "452", "453", "454", "455",
    "456", "457", "458", "459", "460", "461", "462", "463", "464",
    "465", "466", "467", "468", "469", "470", "471", "472", "473",
    "474", "475", "476", "477", "478", "479", "480", "481", "482",
    "483", "484", "485", "486", "487", "488", "489", "490", "491",
    "492", "493", "494", "495", "496", "497", "498", "499", "500",
    "501", "502", "503", "504", "505", "506", "507", "508", "509",
    "510", "511", "512", "513", "514", "515", "516", "517", "518",
    "519", "520", "521", "522", "523", "524", "525", "526", "527",
    "528", "529", "530", "531", "532", "533", "534", "535", "536",
    "537", "538", "539", "540", "541", "542", "543", "544", "545",
    "546", "547", "548", "549", "550", "551", "552", "553", "554",
    "555", "556", "557", "558", "559", "560", "561", "562", "563",
    "564", "565", "566", "567", "568", "569", "570", "571", "572",
    "573", "574", "575", "576", "577", "578", "579", "580", "581",
    "582", "583", "584", "585", "586", "587", "588", "589", "590",
    "591", "592", "593", "594", "595", "596", "597", "598", "599",
    "600", "601", "602", "603", "604", "605", "606", "607", "608",
    "609", "610", "611", "612", "613", "614", "615", "616", "617",
    "618", "619", "620", "621", "622", "623", "624", "625", "626",
    "627", "628", "629", "630", "631", "632", "633", "634", "635",
    "636", "637", "638", "639", "640", "641", "642", "643", "644",
    "645", "646", "647", "648", "649", "650", "651", "652", "653",
    "654", "655", "656", "657", "658", "659", "660", "661", "662",
    "663", "664", "665", "666", "667", "668", "669", "670", "671",
    "672", "673", "674", "675", "676", "677", "678", "679", "680",
    "681", "682", "683", "684", "685", "686", "687", "688", "689",
    "690", "691", "692", "693", "694", "695", "696", "697", "698",
    "699", "700", "701", "702", "703", "704", "705", "706", "707",
    "708", "709", "710", "711", "712", "713", "714", "715", "716",
    "717", "718", "719", "720", "721", "722", "723", "724", "725"
    )), .Names = "IID - Interviewer Identification"), statistic = "Values", name = "IID - Interviewer Identification", questions = c("IID - Interviewer Identification",
"RAW DATA"))

    pd <- PrepareData("Histogram", input.data.table = tab3, data.source = "Link to a table")
    CChart("Histogram", pd$data)
    expect_error(CChart("Histogram", pd$data), NA)


    t1 = structure(c(0, 8.28402366863905, 34.3195266272189, 18.0473372781065,
2.9585798816568, 4.14201183431953, 25.4437869822485, 4.73372781065089,
2.07100591715976, 0, 100, 0.265957446808511, 11.968085106383,
30.0531914893617, 11.7021276595745, 3.98936170212766, 5.58510638297872,
29.5212765957447, 5.31914893617021, 1.59574468085106, 0, 100,
0.140056022408964, 10.2240896358543, 32.0728291316527, 14.7058823529412,
3.50140056022409, 4.90196078431373, 27.5910364145658, 5.04201680672269,
1.82072829131653, 0, 100), .Dim = c(11L, 3L), statistic = "Column %", .Dimnames = list(
    c("15 and under", "16-19 yrs", "20-24 yrs", "25-29 yrs",
    "30-34 yrs", "35-44 yrs", "45-54 yrs", "55-64 yrs", "65 and over",
    "Don't know", "NET"), c("male", "female", "NET")), name = "Age by Gender", questions = c("Age",
"Gender"))
    pd <- PrepareData("Column", input.data.table = t1, data.source = "Link to a table")



})







