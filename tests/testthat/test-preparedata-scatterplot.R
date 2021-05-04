context("PrepareData: Scatterplots")

tb.no.stats <- list(`Age by Income` = structure(c(34.7826086956522, 0, 0, 8.69565217391304,
       0, 0, 0, 47.8260869565217, 8.69565217391304, 100, 19.4805194805195,
       6.49350649350649, 0, 7.79220779220779, 16.8831168831169, 3.8961038961039,
       9.09090909090909, 20.7792207792208, 15.5844155844156, 100, 16.4383561643836,
       5.47945205479452, 8.21917808219178, 9.58904109589041, 4.10958904109589,
       8.21917808219178, 12.3287671232877, 26.027397260274, 9.58904109589041,
       100, 13.953488372093, 6.97674418604651, 16.2790697674419, 10.077519379845,
       7.75193798449612, 12.4031007751938, 10.8527131782946, 4.65116279069767,
       17.0542635658915, 100, 8.22784810126582, 11.3924050632911, 8.86075949367089,
       8.22784810126582, 20.253164556962, 8.86075949367089, 16.4556962025316,
       16.4556962025316, 1.26582278481013, 100, 7.76699029126214, 27.1844660194175,
       11.6504854368932, 18.4466019417476, 6.79611650485437, 2.9126213592233,
       15.5339805825243, 9.70873786407767, 0, 100, 6.77966101694915,
       10.1694915254237, 16.9491525423729, 6.77966101694915, 10.1694915254237,
       13.5593220338983, 10.1694915254237, 25.4237288135593, 0, 100,
       4.54545454545455, 11.3636363636364, 11.3636363636364, 29.5454545454545,
       6.81818181818182, 9.09090909090909, 4.54545454545455, 22.7272727272727,
       0, 100, 17.0731707317073, 17.0731707317073, 14.6341463414634,
       7.31707317073171, 14.6341463414634, 0, 14.6341463414634, 14.6341463414634,
       0, 100, 12.3055162659123, 11.5983026874116, 10.4667609618105,
       11.3154172560113, 11.3154172560113, 7.63790664780764, 12.1640735502122,
       16.8316831683168, 6.36492220650637, 100), statistic = "Column %", .Dim = c(10L,
       10L), .Dimnames = list(c("18 to 24", "25 to 29", "30 to 34",
       "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
       "NET"), c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
       "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
       "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
       "NET")), name = "Age by Income", questions = c("Age", "Income")))

tb.with.stats <- list(`Age by Income` = structure(c(34.7826086956522, 0, 0, 8.69565217391304,
       0, 0, 0, 47.8260869565217, 8.69565217391304, 100, 19.4805194805195,
       6.49350649350649, 0, 7.79220779220779, 16.8831168831169, 3.8961038961039,
       9.09090909090909, 20.7792207792208, 15.5844155844156, 100, 16.4383561643836,
       5.47945205479452, 8.21917808219178, 9.58904109589041, 4.10958904109589,
       8.21917808219178, 12.3287671232877, 26.027397260274, 9.58904109589041,
       100, 13.953488372093, 6.97674418604651, 16.2790697674419, 10.077519379845,
       7.75193798449612, 12.4031007751938, 10.8527131782946, 4.65116279069767,
       17.0542635658915, 100, 8.22784810126582, 11.3924050632911, 8.86075949367089,
       8.22784810126582, 20.253164556962, 8.86075949367089, 16.4556962025316,
       16.4556962025316, 1.26582278481013, 100, 7.76699029126214, 27.1844660194175,
       11.6504854368932, 18.4466019417476, 6.79611650485437, 2.9126213592233,
       15.5339805825243, 9.70873786407767, 0, 100, 6.77966101694915,
       10.1694915254237, 16.9491525423729, 6.77966101694915, 10.1694915254237,
       13.5593220338983, 10.1694915254237, 25.4237288135593, 0, 100,
       4.54545454545455, 11.3636363636364, 11.3636363636364, 29.5454545454545,
       6.81818181818182, 9.09090909090909, 4.54545454545455, 22.7272727272727,
       0, 100, 17.0731707317073, 17.0731707317073, 14.6341463414634,
       7.31707317073171, 14.6341463414634, 0, 14.6341463414634, 14.6341463414634,
       0, 100, 12.3055162659123, 11.5983026874116, 10.4667609618105,
       11.3154172560113, 11.3154172560113, 7.63790664780764, 12.1640735502122,
       16.8316831683168, 6.36492220650637, 100, 0.0039807217070846,
       0, 0, 0.00199888492189559, 0, 0, 0, 0.00465778242730789, 0.00199888492189559,
       0.00667682824465732, 0.00542346588195757, 0.00315378289881675,
       0, 0.00345233451241458, 0.00505626290103049, 0.00244638718226599,
       0.00372628677967813, 0.00559728280918975, 0.00486139892923941,
       0.0117244876360376, 0.00486139892923941, 0.00282283760180859,
       0.00345233451241458, 0.00372628677967813, 0.00244638718226599,
       0.00345233451241458, 0.00421917172205166, 0.0060862425405387,
       0.00372628677967813, 0.0114520779299328, 0.00592821709207783,
       0.00421917172205166, 0.00638925087184309, 0.00505626290103049,
       0.00444421053699322, 0.00559728280918975, 0.00524335027110719,
       0.00345233451241458, 0.00653483854222681, 0.0145357354437494,
       0.00505626290103049, 0.00592821709207783, 0.00524335027110719,
       0.00505626290103049, 0.0078235724961344, 0.00524335027110719,
       0.00708334615220998, 0.00708334615220998, 0.00199888492189559,
       0.0156780827148481, 0.0039807217070846, 0.00733993372435252,
       0.00486139892923941, 0.0060862425405387, 0.00372628677967813,
       0.00244638718226599, 0.00559728280918975, 0.00444421053699322,
       0, 0.0132774696490871, 0.00282283760180859, 0.00345233451241458,
       0.00444421053699322, 0.00282283760180859, 0.00345233451241458,
       0.0039807217070846, 0.00345233451241458, 0.00542346588195757,
       0, 0.0104085862289755, 0.00199888492189559, 0.00315378289881675,
       0.00315378289881675, 0.00505626290103049, 0.00244638718226599,
       0.00282283760180859, 0.00199888492189559, 0.00444421053699322,
       0, 0.00909204000853127, 0.00372628677967813, 0.00372628677967813,
       0.00345233451241458, 0.00244638718226599, 0.00345233451241458,
       0, 0.00345233451241458, 0.00345233451241458, 0, 0.00879644676254966,
       0.0123632800456569, 0.0120510578304054, 0.0115211531722201, 0.0119222163766617,
       0.0119222163766617, 0.00999612564337838, 0.0123019302804119,
       0.0140812230998479, 0.00918784134634635, 0, 0.000849377419153474,
       0.0773809827461264, 0.0954961038909552, 0.686781943204848, 0.0815725762954532,
       0.160878761024438, 0.0696059066711372, 0.0000536534428476898,
       0.641571893614577, NA, 0.0423210095424223, 0.138351665068541,
       0.00148126755068367, 0.301199195429941, 0.102298995198857, 0.190338372185203,
       0.382136279730124, 0.326704874125211, 0.000447169134715319, NA,
       0.256328046748208, 0.0846852156976564, 0.507691842184744, 0.62292953328991,
       0.0401358267826595, 0.843470320141708, 0.963740814436351, 0.0265869880109406,
       0.233427571493747, NA, 0.528586480556088, 0.0698293923241086,
       0.0170784486631649, 0.623518700409737, 0.157642936167409, 0.0242179617346248,
       0.614296809388373, 0.0000432417394942153, 3.79435104447268e-08,
       NA, 0.0766221933689186, 0.926918518829267, 0.454254318229909,
       0.16443692441598, 0.0000570765610781176, 0.51134932415577, 0.0610920797125845,
       0.886017825067399, 0.00288807619190024, NA, 0.129264005505931,
       9.05778875237573e-08, 0.671140990940982, 0.0134437150729616,
       0.117238912014026, 0.0507654829575195, 0.257628154136641, 0.0365830352009795,
       0.00419949566936639, NA, 0.177137754498547, 0.720335604552778,
       0.0893252341570526, 0.250644853277108, 0.771638738277393, 0.0736617002828646,
       0.624430468982927, 0.0654054200653444, 0.0364541201647898, NA,
       0.105638606959378, 0.959963120931475, 0.840945546998515, 0.0000808329136292585,
       0.330828649229419, 0.707865703158119, 0.110368183500707, 0.280430074299729,
       0.0741158466703972, NA, 0.338321115867427, 0.259318212136639,
       0.369126520991046, 0.405017436714764, 0.489466621817869, 0.0578063340496739,
       0.618104574183931, 0.698395168435894, 0.0854228412526985, NA,
       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), .Dim = c(10L, 10L, 3L
       ), .Dimnames = list(c("18 to 24", "25 to 29", "30 to 34", "35 to 39",
       "40 to 44", "45 to 49", "50 to 54", "55 to 64", "65 or more",
       "NET"), c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
       "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
       "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
       "NET"), c("Column %", "Standard Error", "p")), name = "Age by Income",
       questions = c("Age","Income")))

banner.1d.with.multstats <- list(X = structure(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income", "Gender",
    "Gender", "Gender"), .Dim = c(13L, 1L), .Dimnames = list(c("Less than $15,000",
    "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
    "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
    "$150,001 to $200,000", "$200,001 or more", "NET", "Male", "Female",
    "NET"), "table.BANNER"), statistic = "%",
    basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing",
    basedescription = list(Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L))), name = "table.BANNER", questions = c("BANNER",
    "SUMMARY")), Y = list(table.BANNER.2 = structure(c(3.25318246110325,
    10.8910891089109, 10.3253182461103, 18.2461103253182, 22.3479490806223,
    14.5685997171146, 8.34512022630834, 6.22347949080622, 5.7991513437058,
    100, 49.375, 50.625, 100, -15.85213900942, -11.4519011349191,
    -11.7778446811784, -7.2146350335478, -4.85154432316768, -9.33326808423343,
    -12.918647093086, -14.1409353915585, -14.385393051253, 39.884207400925,
    11.4020968466331, 12.1681291929185, 42.4264068711928, 1.35876092092925e-56,
    2.30044126687814e-30, 5.07748161228579e-32, 5.4078820119266e-13,
    0.00000122503824673038, 1.02655768301792e-20, 3.53298929345856e-38,
    2.12441682682102e-45, 6.39139645248431e-47, 0, 0, 0, 0), .Dim = c(13L,
    3L), .Dimnames = list(c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET"), c("%", "z-Statistic",
    "p")), basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing",
    basedescription = list(Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L))), name = "table.BANNER.2", questions = c("BANNER",
    "SUMMARY"))), Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL,
    structure(c(-15.85213900942, -11.4519011349191, -11.7778446811784,
    -7.2146350335478, -4.85154432316768, -9.33326808423343, -12.918647093086,
    -14.1409353915585, -14.385393051253, 39.884207400925, 11.4020968466331,
    12.1681291929185, 42.4264068711928), .Dim = c(13L, 1L), .Dimnames = list(
    c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
    "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
    "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
    "NET", "Male", "Female", "NET"), "table.BANNER.3"), statistic = "z-Statistic",
    basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing",
    basedescription = list(Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income",
    "Income", "Gender", "Gender", "Gender"), c("Less than $15,000",
    "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
    "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
    "$150,001 to $200,000", "$200,001 or more", "NET", "Male",
    "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L))), name = "table.BANNER.3", questions = c("BANNER",
    "SUMMARY")))

banner.1d.with.stats.and.Z1.Z2 <- list(X = structure(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income", "Gender",
    "Gender", "Gender"), .Dim = c(13L, 1L), .Dimnames = list(c("Less than $15,000",
    "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
    "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
    "$150,001 to $200,000", "$200,001 or more", "NET", "Male", "Female",
    "NET"), " ")), Y = list(table.BANNER.2 = structure(c(3.25318246110325,
    10.8910891089109, 10.3253182461103, 18.2461103253182, 22.3479490806223,
    14.5685997171146, 8.34512022630834, 6.22347949080622, 5.7991513437058,
    100, 49.375, 50.625, 100, -15.85213900942, -11.4519011349191,
    -11.7778446811784, -7.2146350335478, -4.85154432316768, -9.33326808423343,
    -12.918647093086, -14.1409353915585, -14.385393051253, 39.884207400925,
    11.4020968466331, 12.1681291929185, 42.4264068711928, 1.35876092092925e-56,
    2.30044126687814e-30, 5.07748161228579e-32, 5.4078820119266e-13,
    0.00000122503824673038, 1.02655768301792e-20, 3.53298929345856e-38,
    2.12441682682102e-45, 6.39139645248431e-47, 0, 0, 0, 0), .Dim = c(13L,
    3L), .Dimnames = list(c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET"),
    c("%", "z-Statistic", "p")),
    basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing",
    basedescription = list(
    Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L))), name = "table.BANNER.2", questions = c("BANNER",
    "SUMMARY"))), Z1 = structure(c(3.25318246110325, 10.8910891089109,
    10.3253182461103, 18.2461103253182, 22.3479490806223, 14.5685997171146,
    8.34512022630834, 6.22347949080622, 5.7991513437058, 100, 49.375,
    50.625, 100), .Dim = c(13L, 1L), .Dimnames = list(c("Less than $15,000",
    "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
    "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
    "$150,001 to $200,000", "$200,001 or more", "NET", "Male", "Female",
    "NET"), "table.BANNER"), statistic = "%", basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing", basedescription = list(
    Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L))), name = "table.BANNER", questions = c("BANNER",
    "SUMMARY")), Z2 = structure(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income", "Gender",
    "Gender", "Gender"), .Dim = c(13L, 1L), .Dimnames = list(c("Less than $15,000",
    "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
    "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
    "$150,001 to $200,000", "$200,001 or more", "NET", "Male", "Female",
    "NET"), " ")), groups = NULL, labels = NULL)


banner.2d.with.multstats <- list(X = structure(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income", "Gender",
    "Gender", "Gender"), .Dim = c(13L, 1L), .Dimnames = list(c("Less than $15,000",
    "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
    "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
    "$150,001 to $200,000", "$200,001 or more", "NET", "Male", "Female",
    "NET"), "table.BANNER"), statistic = "%",
    basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing",
    basedescription = list(Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L))), name = "table.BANNER", questions = c("BANNER",
    "SUMMARY")), Y = list(table.BANNER.by.Age.2 = structure(c(3.29218106995885,
    8.23045267489712, 9.05349794238683, 19.7530864197531, 18.5185185185185,
    19.7530864197531, 8.23045267489712, 4.93827160493827, 8.23045267489712,
    100, 48.9130434782609, 51.0869565217391, 100, 0.934579439252336,
    10.2803738317757, 7.47663551401869, 18.2242990654206, 27.5700934579439,
    13.5514018691589, 8.41121495327103, 9.34579439252336, 4.20560747663551,
    100, 48.582995951417, 51.417004048583, 100, 5.2, 14, 14, 16.8,
    21.6, 10.4, 8.4, 4.8, 4.8, 100, 50.5415162454874, 49.4584837545126,
    100, 3.25318246110325, 10.8910891089109, 10.3253182461103, 18.2461103253182,
    22.3479490806223, 14.5685997171146, 8.34512022630834, 6.22347949080622,
    5.7991513437058, 100, 49.375, 50.625, 100, 0.0422990305251389,
    -1.64339955030345, -0.804253504739953, 0.750794108247138, -1.76885710560662,
    2.82775611667072, -0.0797811509368206, -1.02367841609268, 2.0016263618645,
    NA, -0.189670121023773, 0.189670121023773, NA, -2.28953336287652,
    -0.343427995094239, -1.64002539048777, -0.00989314845553605,
    2.19607266733246, -0.50510410581336, 0.041866422496426, 2.26415496102534,
    -1.19439376612256, NA, -0.299448563242615, 0.299448563242615,
    NA, 2.15811362332206, 1.96260843919401, 2.37495124743934, -0.736348959803586,
    -0.353100507673859, -2.32377533684291, 0.0390247229049508, -1.1588017352177,
    -0.840704280815369, NA, 0.480273641839049, -0.480273641839049,
    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA), .Dim = c(13L,
    4L, 2L), .Dimnames = list(c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET"), c("Young",
    "Middle-aged", "Old", "NET"), c("Column %", "z-Statistic")),
    basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing",
    basedescription = list(Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = c("PickAny", "PickOne"
    ), span = list(rows = structure(list(c("Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L)), columns = structure(list(c("Young",
    "Middle-aged", "Old", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    4L))), name = "table.BANNER.by.Age.2", questions = c("BANNER",
    "Age 2"))), Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)

tables.with.banners <- list(X = structure(c(3.25318246110325, 10.8910891089109, 10.3253182461103,
    18.2461103253182, 22.3479490806223, 14.5685997171146, 8.34512022630834,
    6.22347949080622, 5.7991513437058, 100, 49.375, 50.625, 100,
    -15.85213900942, -11.4519011349191, -11.7778446811784, -7.2146350335478,
    -4.85154432316768, -9.33326808423343, -12.918647093086, -14.1409353915585,
    -14.385393051253, 39.884207400925, 11.4020968466331, 12.1681291929185,
    42.4264068711928, 1.35876092092925e-56, 2.30044126687814e-30,
    5.07748161228579e-32, 5.4078820119266e-13, 1.22503824673038e-06,
    1.02655768301792e-20, 3.53298929345856e-38, 2.12441682682102e-45,
    6.39139645248431e-47, 0, 0, 0, 0), .Dim = c(13L, 3L), .Dimnames = list(
    c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
    "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
    "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
    "NET", "Male", "Female", "NET"), c("%", "z-Statistic", "p"
    )), basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing", basedescription = list(
    Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L))), name = "table.BANNER.2", questions = c("BANNER",
    "SUMMARY")), Y = list(table.BANNER.by.Unique.Identifier = structure(c(1623.91304347826,
    1540.68831168831, 1600.98630136986, 1582.82170542636, 1656.58860759494,
    1527.40776699029, 1604.22033898305, 1512.13636363636, 1732.58536585366,
    1595.92927864215, 1567.88101265823, 1632.31358024691, 1600.5), .Dim = c(13L,
    1L), statistic = "Average", .Dimnames = list(c("Less than $15,000",
    "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
    "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
    "$150,001 to $200,000", "$200,001 or more", "NET", "Male", "Female",
    "NET"), "Unique Identifier"), basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing", basedescription = list(
    Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = c("PickAny", "Number"
    ), span = list(rows = structure(list(c("Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L)), columns = structure(list("Unique Identifier"), class = "data.frame", .Names = "", row.names = 1L)), name = "table.BANNER.by.Unique.Identifier", questions = c("BANNER",
    "Unique Identifier"))), Z1 = structure(c(`Less than $15,000` = 3.25318246110325,
    `$15,001 to $30,000` = 10.8910891089109, `$30,001 to $45,000` = 10.3253182461103,
    `$45,001 to $60,000` = 18.2461103253182, `$60,001 to $90,000` = 22.3479490806223,
    `$90,001 to $120,000` = 14.5685997171146, `$120,001 to $150,000` = 8.34512022630834,
    `$150,001 to $200,000` = 6.22347949080622, `$200,001 or more` = 5.7991513437058,
    NET = 100, Male = 49.375, Female = 50.625, NET = 100), .Dim = 13L, .Dimnames = list(
    c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
    "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
    "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
    "NET", "Male", "Female", "NET")), statistic = "%", basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing", basedescription = list(
    Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L))), name = "table.BANNER", questions = c("BANNER",
    "SUMMARY")), Z2 = NULL, groups = NULL, labels = NULL, structure(c(`Less than $15,000` = -15.85213900942,
    `$15,001 to $30,000` = -11.4519011349191, `$30,001 to $45,000` = -11.7778446811784,
    `$45,001 to $60,000` = -7.2146350335478, `$60,001 to $90,000` = -4.85154432316768,
    `$90,001 to $120,000` = -9.33326808423343, `$120,001 to $150,000` = -12.918647093086,
    `$150,001 to $200,000` = -14.1409353915585, `$200,001 or more` = -14.385393051253,
    NET = 39.884207400925, Male = 11.4020968466331, Female = 12.1681291929185,
    NET = 42.4264068711928), .Dim = 13L, .Dimnames = list(c("Less than $15,000",
    "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
    "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
    "$150,001 to $200,000", "$200,001 or more", "NET", "Male", "Female",
    "NET")), statistic = "z-Statistic", basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing", basedescription = list(
    Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L))), name = "table.BANNER.3", questions = c("BANNER",
"SUMMARY")))

xy.same.inputs <- list(X = structure(c(`Less than $15,000` = 3.25318246110325,
`$15,001 to $30,000` = 10.8910891089109, `$30,001 to $45,000` = 10.3253182461103,
`$45,001 to $60,000` = 18.2461103253182, `$60,001 to $90,000` = 22.3479490806223,
`$90,001 to $120,000` = 14.5685997171146, `$120,001 to $150,000` = 8.34512022630834,
`$150,001 to $200,000` = 6.22347949080622, `$200,001 or more` = 5.7991513437058,
NET = 100, Male = 49.375, Female = 50.625, NET = 100), statistic = "%", .Dim = 13L, .Dimnames = list(
    c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
    "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
    "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
    "NET", "Male", "Female", "NET")), basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing", basedescription = list(
    Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L))), name = "table.BANNER", questions = c("BANNER",
"SUMMARY")), Y = list(table.BANNER = structure(c(`Less than $15,000` = 3.25318246110325,
`$15,001 to $30,000` = 10.8910891089109, `$30,001 to $45,000` = 10.3253182461103,
`$45,001 to $60,000` = 18.2461103253182, `$60,001 to $90,000` = 22.3479490806223,
`$90,001 to $120,000` = 14.5685997171146, `$120,001 to $150,000` = 8.34512022630834,
`$150,001 to $200,000` = 6.22347949080622, `$200,001 or more` = 5.7991513437058,
NET = 100, Male = 49.375, Female = 50.625, NET = 100), statistic = "%", .Dim = 13L, .Dimnames = list(
    c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
    "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
    "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
    "NET", "Male", "Female", "NET")), basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing", basedescription = list(
    Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L))), name = "table.BANNER", questions = c("BANNER",
"SUMMARY"))), Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL)

banner.tb.no.stats <- list(table.BANNER = structure(c(`Less than $15,000` = 3.25318246110325,
    `$15,001 to $30,000` = 10.8910891089109, `$30,001 to $45,000` = 10.3253182461103,
    `$45,001 to $60,000` = 18.2461103253182, `$60,001 to $90,000` = 22.3479490806223,
    `$90,001 to $120,000` = 14.5685997171146, `$120,001 to $150,000` = 8.34512022630834,
    `$150,001 to $200,000` = 6.22347949080622, `$200,001 or more` = 5.7991513437058,
    NET = 100, Male = 49.375, Female = 50.625, NET = 100), .Dim = 13L, .Dimnames = list(
    c("Less than $15,000", "$15,001 to $30,000", "$30,001 to $45,000",
    "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
    "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
    "NET", "Male", "Female", "NET")), statistic = "%",
    basedescriptiontext = "sample size = from 707 to 800; total sample size = 800; 93 missing",
    basedescription = list(
    Minimum = 707L, Maximum = 800L, Range = TRUE, Total = 800L,
    Missing = 93L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickAny", span = list(
    rows = structure(list(c("Income", "Income", "Income", "Income",
    "Income", "Income", "Income", "Income", "Income", "Income",
    "Gender", "Gender", "Gender"), c("Less than $15,000", "$15,001 to $30,000",
    "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000",
    "$90,001 to $120,000", "$120,001 to $150,000", "$150,001 to $200,000",
    "$200,001 or more", "NET", "Male", "Female", "NET")), class = "data.frame", .Names = c("",
    ""), row.names = c(NA, 13L))), name = "table.BANNER", questions = c("BANNER",
    "SUMMARY")))

## Tests start here
test_that("Handle y-values in multiple columns + multiple statistics",
{
    res0 <- PrepareData("Scatter", input.data.table = tb.no.stats, scatter.mult.yvals = TRUE)
    expect_equal(colnames(res0$data), c("Age", "Column %", "Income"))
    res1 <- PrepareData("Scatter", input.data.table = tb.with.stats, scatter.mult.yvals = TRUE)
    expect_equal(colnames(res1$data), c("Age", "Column %", "Income", "Standard Error", "p"))
    res1 <- PrepareData("Scatter", input.data.table = tb.with.stats, scatter.mult.yvals = TRUE)


    res2 <- PrepareData("Scatter", input.data.table = tb.with.stats,
                        row.names.to.remove = "65 or more", column.labels = "Small",
                        scatter.mult.yvals = TRUE)
    expect_true(!"65 or more" %in% res2$data[,1])
    expect_equal(levels(res2$data[,3]), c("Small", "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
                       "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
                       "$150,001 to $200,000", "$200,001 or more"))


    res3 <- PrepareData("Scatter", input.data.table = tb.with.stats,
                        first.k.columns = 3,
                        scatter.mult.yvals = TRUE)
    expect_equal(nlevels(res3$data[,3]), 3)
    expect_equal(dim(res3$data), c(27,5))

    res4 <- PrepareData("Scatter", input.data.table = tb.with.stats,
                        sort.columns = TRUE, sort.columns.row = "18 to 24",
                        scatter.mult.yvals = TRUE)
    expect_equal(levels(res4$data[,3]), c("$150,001 to $200,000", "$120,001 to $150,000", "$90,001 to $120,000",
                                          "$60,001 to $90,000", "$45,001 to $60,000", "$30,001 to $45,000",
                                          "$200,001 or more", "$15,001 to $30,000", "Less than $15,000"))

    res5 <- PrepareData("Scatter", input.data.table = tb.with.stats,
                        select.columns = "Less than $15",
                        scatter.mult.yvals = TRUE)
    expect_equal(res5$scatter.variable.indices, c(x = 1, y = 2, sizes = 0, colors = 3, groups = 3))
    expect_equal(dim(res5$data), c(9, 5))

    res6 <- PrepareData("Scatter", input.data.table = banner.tb.no.stats)
    expect_equal(dimnames(res6$data), list(c("Less than $15,000", "$15,001 to $30,000",
        "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
        "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
        "Male", "Female"), NULL))
})



test_that("Using tables with banners",
{
    tables.with.banners[[7]] <- PrepareForCbind(tables.with.banners[[7]])
    expect_warning(pd <- PrepareData("Scatter", input.data.raw = tables.with.banners),
        "Only the first column")

    expect_error(PrepareForCbind(tables.with.banners[[2]]), NA)

    tb2 <- tables.with.banners
    tb2$X <- PrepareForCbind(tb2$X)
    expect_warning(pd2 <- PrepareData("Scatter", input.data.raw = tb2), "Only the first column")
    expect_equal(pd$data, pd2$data)

    tb3 <- tables.with.banners
    tb3$X <- PrepareForCbind(tb3$X, use.span = TRUE)
    pd3 <- PrepareData("Scatter", input.data.raw = tb3)
    expect_equal(dim(pd3$data), dim(pd$data))
    expect_equal(rownames(pd3$data), rownames(pd$data))
    expect_equal(pd3$data[,1], c("Income", "Income", "Income", "Income",
        "Income", "Income", "Income", "Income", "Income", "Gender", "Gender"))

    pd4 <- PrepareData("Scatter", input.data.raw = banner.1d.with.multstats)
    expect_equal(dimnames(pd4$data), list(c("Less than $15,000", "$15,001 to $30,000",
        "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
        "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
        "Male", "Female"), c("table.BANNER", "%", "z-Statistic", "p",
        "table.BANNER.3")))
    expect_equal(pd4$scatter.variable.indices, c(x = 1, y = 2, sizes = NA, colors = NA, groups = NA))

    pd5 <- PrepareData("Scatter", input.data.raw = banner.2d.with.multstats,
        hide.empty.columns = FALSE)
    expect_equal(dimnames(pd5$data), list(c("Less than $15,000",
        "$15,001 to $30,000", "$30,001 to $45,000",
        "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
        "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
        "Male", "Female", "Less than $15,000 ", "$15,001 to $30,000 ",
        "$30,001 to $45,000 ", "$45,001 to $60,000 ", "$60,001 to $90,000 ",
        "$90,001 to $120,000 ", "$120,001 to $150,000 ", "$150,001 to $200,000 ",
        "$200,001 or more ", "Male ", "Female ", "Less than $15,000  ",
        "$15,001 to $30,000  ", "$30,001 to $45,000  ", "$45,001 to $60,000  ",
        "$60,001 to $90,000  ", "$90,001 to $120,000  ", "$120,001 to $150,000  ",
        "$150,001 to $200,000  ", "$200,001 or more  ", "Male  ", "Female  "
        ), c("table.BANNER", "Y", "Groups", "z-Statistic")))
    expect_equal(levels(pd5$data$Groups), c("Young", "Middle-aged", "Old"))
})

vv <- structure(c(0.287577520124614, 0.788305135443807, 0.4089769218117,
    0.883017404004931, 0.940467284293845, 0.0455564993899316, 0.528105488047004,
    0.892419044394046, 0.551435014465824, 0.456614735303447, 0.956833345349878,
    0.835255319951102, 0.143817043630406, 0.192815946880728, 0.896738682640716,
    0.308119554305449, 0.363300543511286, 0.783946478739381), questiontype = "Number",
    dataset = "timedata", name = "v1", label = "Variable A", question = "Variable A")
v.unnamed <- structure(1:10, .Names = c("a", "b", "c", "d", "e", "f", "g",
    "h", "i", "j"))

test_that("PrepareForCbind shows names for a single variable",
{
    res0 <- PrepareData("Scatter", input.data.raw = list(X = vv, Y = NULL,
            Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL), show.labels = FALSE)
    res <- PrepareData("Scatter", input.data.raw = list(X = PrepareForCbind(vv, show.labels = FALSE),
            Y = NULL, Z1 = NULL, Z2 = NULL, groups = NULL, labels = NULL), show.labels = FALSE)
    expect_equal(colnames(res0$data), colnames(res$data))

    res.unnamed <- PrepareForCbind(v.unnamed)
    expect_equal(res.unnamed, structure(1:10, .Dim = c(10L, 1L),
        .Dimnames = list(c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), " ")))
})


v2 <- structure(c(9L, 9L, 9L, 7L, 6L, 6L, 7L, 7L, 9L, 4L, 7L, 6L, 5L, 8L, 4L, 9L,
    6L, 9L, 7L, 7L, 4L, 6L, 6L, 9L, 8L, 9L, 7L, 4L, 5L, 7L, 7L, 9L,
    8L, 9L, 9L, 6L, 7L, 9L, 9L, 9L, 8L, 9L, 6L, 6L, 5L, 9L, 8L, 4L,
    6L, 4L, 9L, 8L, 7L, 7L, 9L, 7L, 6L, 4L), class = "factor", .Label = c("Never",
    "Once or twice a year", "Once every 3 months", "Once a month",
    "Once every 2 weeks", "Once a week", "2 to 3 days a week", "4 to 5 days a week",
    "Every or nearly every day"), questiontype = "PickOneMulti",
    dataset = "Visualization - Standard R Charts.sav", values = c(Never = 1,
    `Once or twice a year` = 2, `Once every 3 months` = 3, `Once a month` = 4,
    `Once every 2 weeks` = 5, `Once a week` = 6, `2 to 3 days a week` = 7,
    `4 to 5 days a week` = 8, `Every or nearly every day` = 9), sourcevalues = c(Never = 1,
    `Once or twice a year` = 2, `Once every 3 months` = 3, `Once a month` = 4,
    `Once every 2 weeks` = 5, `Once a week` = 6, `2 to 3 days a week` = 7,
    `4 to 5 days a week` = 8, `Every or nearly every day` = 9), name = "Q4_A",
    label = "Colas (e.g., Coca Cola, Pepsi Max)?", question = "Q4. Frequency of drinking cola")

test_that("PrepareForCbind with factors",
{
    v2b <- PrepareForCbind(v2)
    expect_equal(levels(v2), levels(unlist(v2b)))
    expect_equal(colnames(v2b), "Q4. Frequency of drinking cola: Colas (e.g., Coca Cola, Pepsi Max)?")
})

test_that("Check that a table can be used twice for the span and values",
{
    tmp.input <- xy.same.inputs
    tmp.input[[1]] <- PrepareForCbind(tmp.input[[1]], use.span = TRUE)
    tmp.input[[2]] <- PrepareForCbind(tmp.input[[2]])

    res <- PrepareData("Scatter", input.data.raw = tmp.input)
    expect_equal(res$data, structure(list(` ` = c("Income", "Income", "Income",
        "Income", "Income", "Income", "Income", "Income", "Income", "Gender", "Gender"),
        table.BANNER = c(3.25318246110325, 10.8910891089109, 10.3253182461103,
        18.2461103253182, 22.3479490806223, 14.5685997171146, 8.34512022630834,
        6.22347949080622, 5.7991513437058, 49.375, 50.625)), row.names = c("Less than $15,000",
        "$15,001 to $30,000", "$30,001 to $45,000", "$45,001 to $60,000",
        "$60,001 to $90,000", "$90,001 to $120,000", "$120,001 to $150,000",
        "$150,001 to $200,000", "$200,001 or more", "Male", "Female"),
        scatter.variable.indices = c(x = 1,
        y = 2, sizes = NA, colors = NA, groups = NA), class = "data.frame"))
    expect_equal(res$scatter.variable.indices,
        c(x = 1, y = 2, sizes = NA, colors = NA, groups = NA))
})

test_that("Keeping extra data input.data.raw$Y for annotations",
{
    expect_warning(res <- PrepareData("Scatter", input.data.raw = banner.1d.with.stats.and.Z1.Z2),
        "Only the first column")
    expect_equal(dimnames(res$data), list(c("Less than $15,000", "$15,001 to $30,000",
        "$30,001 to $45,000", "$45,001 to $60,000", "$60,001 to $90,000", "$90,001 to $120,000",
        "$120,001 to $150,000", "$150,001 to $200,000", "$200,001 or more",
        "Male", "Female"), c(" ", "%", "table.BANNER", "  ", "z-Statistic",
        "p")))
    expect_equal(res$scatter.variable.indices, c(x = 1, y = 2, sizes = 3, colors = 4, groups = NA))
})


tables.related.raw <- list(X = structure(c(4.56316652994258, 4.32444626743232, 4.42104183757178,
4.49384741591468, 4.04532403609516, 4.4954881050041, 3.79860541427399,
3.16529942575882, 4.18232157506153, 4.40853158326497, 4.09741591468417,
4.20262510254307, 4.04470877768663, 4.11853978671042, 4.21821164889253,
4.28260869565217, 4.19708777686628, 3.96821164889253, 4.0278917145201,
4.0674733388023, 3.93949958982773, 3.82629204265792, 3.60951599671862,
3.90730106644791, 3.67309269893355, 4.26968826907301, 3.9343724364233,
3.53424938474159, 3.26681706316653, 3.26763740771124, 3.44954881050041,
3.70570139458573, 3.88617719442166, 3.18785890073831), .Dim = c(34L,
1L), .Dimnames = list(c("Packed w/nutr so pet lives best life",
"Nutr you can see with real pieces", "Healthier, happy pet in 30 days",
"So nutritious alleviate issues", "Easy to switch flavors/recipes",
"Made w/qual ingred, formulated for them", "Cooked way food should be",
"Ingreds based on ancestral diet", "Human grade ingreds", "First five ingr whole foods",
"Gently cooked to retain nutrition", "Limited set of ingreds",
"Suppl w/add'l vitamins/nutrients", "Handmade by small U.S. team",
"Recipes by vet nutritionist", "Based on real pet science by PhD scientists",
"Ridiculously delicious", "Pre-portioned meals", "Customized for dog’s lifestyle/needs",
"Customized stage of pet’s life", "Nutr. based on health assessment",
"Age specific recipe", "Breed specific recipe", "Recipe for spec dietary requ",
"Approp any age/breed", "Really easy way to feed", "Easy-to-carry",
"Easily stored in pack/cabinet", "Kept in fridge", "Sold where shop for everyday prods",
"Sold where shop for pet", "Shipped fresh/cold to door", "Affordable",
"Expensive but worth it"), "table.Needs.Numeric.Abbr"), statistic = "Average", basedescriptiontext = "sample size = 4876", basedescription = list(
    Minimum = 4876L, Maximum = 4876L, Range = FALSE, Total = 4876L,
    Missing = 0L, EffectiveSampleSize = 4876L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "NumberMulti", span = list(
    rows = structure(list(c("Packed w/nutr so pet lives best life",
    "Nutr you can see with real pieces", "Healthier, happy pet in 30 days",
    "So nutritious alleviate issues", "Easy to switch flavors/recipes",
    "Made w/qual ingred, formulated for them", "Cooked way food should be",
    "Ingreds based on ancestral diet", "Human grade ingreds",
    "First five ingr whole foods", "Gently cooked to retain nutrition",
    "Limited set of ingreds", "Suppl w/add'l vitamins/nutrients",
    "Handmade by small U.S. team", "Recipes by vet nutritionist",
    "Based on real pet science by PhD scientists", "Ridiculously delicious",
    "Pre-portioned meals", "Customized for dog’s lifestyle/needs",
    "Customized stage of pet’s life", "Nutr. based on health assessment",
    "Age specific recipe", "Breed specific recipe", "Recipe for spec dietary requ",
    "Approp any age/breed", "Really easy way to feed", "Easy-to-carry",
    "Easily stored in pack/cabinet", "Kept in fridge", "Sold where shop for everyday prods",
    "Sold where shop for pet", "Shipped fresh/cold to door",
    "Affordable", "Expensive but worth it")), class = "data.frame", .Names = "", row.names = c(NA,
    34L))), name = "table.Needs.Numeric.Abbr", questions = c("Needs (Numeric) Abbr",
"SUMMARY")), Y = list(table.q23.Brand.Ratings.0.100.Abbr.by.Brand.Rated.Nom.Nom = structure(c(79.6084417269885,
87.771326167364, 68.3793970444807, 70.1070173167587, 80.2871114130367,
82.205387355652, 81.3008926828239, 51.0926862113278, 83.6605992844583,
85.8698605476664, 79.3798646076355, 78.9385034114656, 70.4655369023058,
80.8756716931017, 80.6834542637314, 80.5274057891423, 70.8457435807492,
86.0137949711641, 68.2868886024737, 62.3706359245212, 73.9384289577716,
56.2267463286951, 44.4246715204357, 61.6741705294043, 77.3096977454645,
83.2381252564137, 81.2812321163229, 34.5326469820457, 87.6206338061132,
14.975060411063, 14.7074768555916, 84.4304058894253, 37.2443774707076,
63.8486457182518), statistic = "Average", .Dim = c(34L, 1L), .Dimnames = list(
    c("Packed w/nutr so pet lives best life", "Nutr you can see with real pieces",
    "Healthier, happy pet in 30 days", "So nutritious alleviate issues",
    "Easy to switch flavors/recipes", "Made w/qual ingred, formulated for them",
    "Cooked way food should be", "Ingreds based on ancestral diet",
    "Human grade ingreds", "First five ingr whole foods", "Gently cooked to retain nutrition",
    "Limited set of ingreds", "Suppl w/add'l vitamins/nutrients",
    "Handmade by small U.S. team", "Recipes by vet nutritionist",
    "Based on real pet science by PhD scientists", "Ridiculously delicious",
    "Pre-portioned meals", "Customized for dog’s lifestyle/needs",
    "Customized stage of pet’s life", "Nutr. based on health assessment",
    "Age specific recipe", "Breed specific recipe", "Recipe for spec dietary requ",
    "Approp any age/breed", "Really easy way to feed", "Easy-to-carry",
    "Easily stored in pack/cabinet", "Kept in fridge", "Sold where shop for everyday prods",
    "Sold where shop for pet", "Shipped fresh/cold to door",
    "Affordable", "Expensive but worth it"), "Nom Nom"), basedescriptiontext = "sample size = 4876; effective sample size = 1446 (30%)", basedescription = list(
    Minimum = 4876L, Maximum = 4876L, Range = FALSE, Total = 4876L,
    Missing = 0L, EffectiveSampleSize = 1446L, EffectiveSampleSizeProportion = 30,
    FilteredProportion = 0), questiontypes = c("NumberMulti",
"PickOne"), span = list(rows = structure(list(c("Packed w/nutr so pet lives best life",
"Nutr you can see with real pieces", "Healthier, happy pet in 30 days",
"So nutritious alleviate issues", "Easy to switch flavors/recipes",
"Made w/qual ingred, formulated for them", "Cooked way food should be",
"Ingreds based on ancestral diet", "Human grade ingreds", "First five ingr whole foods",
"Gently cooked to retain nutrition", "Limited set of ingreds",
"Suppl w/add'l vitamins/nutrients", "Handmade by small U.S. team",
"Recipes by vet nutritionist", "Based on real pet science by PhD scientists",
"Ridiculously delicious", "Pre-portioned meals", "Customized for dog’s lifestyle/needs",
"Customized stage of pet’s life", "Nutr. based on health assessment",
"Age specific recipe", "Breed specific recipe", "Recipe for spec dietary requ",
"Approp any age/breed", "Really easy way to feed", "Easy-to-carry",
"Easily stored in pack/cabinet", "Kept in fridge", "Sold where shop for everyday prods",
"Sold where shop for pet", "Shipped fresh/cold to door", "Affordable",
"Expensive but worth it")), class = "data.frame", .Names = "", row.names = c(NA,
34L)), columns = structure(list("Nom Nom"), class = "data.frame", .Names = "", row.names = 1L)), name = "table.q23.Brand.Ratings.0.100.Abbr.by.Brand.Rated.Nom.Nom", questions = c("q23 Brand Ratings (0-100) Abbr",
"Brand Rated Nom Nom [NOM21101 Nom Nom Segmentation SPSS Data CLEAN 04-13-2021.sav]"
), weight.name = "totweight", weight.label = "totweight")), Z1 = structure(c(4.09862178002913,
38.5194885411235, 10.2959656084732, 7.96437012687751, 15.7720967528089,
7.55880577290877, 49.6463444187019, -6.40608008959823, 32.6966063388257,
13.4970166219969, 32.5565415745722, 11.4306766747824, 4.37870631698961,
37.1271969510048, 22.6244340073409, 21.6746145469792, 9.23190078757678,
56.3527262561522, 13.5545999594125, -4.12605822054729, 36.3963680413646,
-20.4137231922625, -1.44204730196014, 0.28559678270728, 6.82494591098468,
1.4158334335138, 12.5888274017534, -46.5355621660909, 69.8451489617674,
-54.8829645032358, -69.1605457486509, 65.8016547870764, -12.6259788946724,
1.55845825298484), statistic = "Average", basedescriptiontext = "sample size = 4876; effective sample size = 1446 (30%)", basedescription = list(
    Minimum = 4876L, Maximum = 4876L, Range = FALSE, Total = 4876L,
    Missing = 0L, EffectiveSampleSize = 1446L, EffectiveSampleSizeProportion = 30,
    FilteredProportion = 0), questiontypes = c("NumberMulti",
"PickOne"), span = list(rows = structure(list(c("Packed w/nutr so pet lives best life",
"Nutr you can see with real pieces", "Healthier, happy pet in 30 days",
"So nutritious alleviate issues", "Easy to switch flavors/recipes",
"Made w/qual ingred, formulated for them", "Cooked way food should be",
"Ingreds based on ancestral diet", "Human grade ingreds", "First five ingr whole foods",
"Gently cooked to retain nutrition", "Limited set of ingreds",
"Suppl w/add'l vitamins/nutrients", "Handmade by small U.S. team",
"Recipes by vet nutritionist", "Based on real pet science by PhD scientists",
"Ridiculously delicious", "Pre-portioned meals", "Customized for dog’s lifestyle/needs",
"Customized stage of pet’s life", "Nutr. based on health assessment",
"Age specific recipe", "Breed specific recipe", "Recipe for spec dietary requ",
"Approp any age/breed", "Really easy way to feed", "Easy-to-carry",
"Easily stored in pack/cabinet", "Kept in fridge", "Sold where shop for everyday prods",
"Sold where shop for pet", "Shipped fresh/cold to door", "Affordable",
"Expensive but worth it")), class = "data.frame", .Names = "", row.names = c(NA,
34L)), columns = structure(list("Nom Nom"), class = "data.frame", .Names = "", row.names = 1L)), name = "table.q23.Brand.Ratings.0.100.Abbr.by.Brand.Rated.Nom.Nom", questions = c("q23 Brand Ratings (0-100) Abbr",
"Brand Rated Nom Nom [NOM21101 Nom Nom Segmentation SPSS Data CLEAN 04-13-2021.sav]"
), weight.name = "totweight", weight.label = "totweight", .Dim = c(34L,
1L), .Dimnames = list(c("Packed w/nutr so pet lives best life",
"Nutr you can see with real pieces", "Healthier, happy pet in 30 days",
"So nutritious alleviate issues", "Easy to switch flavors/recipes",
"Made w/qual ingred, formulated for them", "Cooked way food should be",
"Ingreds based on ancestral diet", "Human grade ingreds", "First five ingr whole foods",
"Gently cooked to retain nutrition", "Limited set of ingreds",
"Suppl w/add'l vitamins/nutrients", "Handmade by small U.S. team",
"Recipes by vet nutritionist", "Based on real pet science by PhD scientists",
"Ridiculously delicious", "Pre-portioned meals", "Customized for dog’s lifestyle/needs",
"Customized stage of pet’s life", "Nutr. based on health assessment",
"Age specific recipe", "Breed specific recipe", "Recipe for spec dietary requ",
"Approp any age/breed", "Really easy way to feed", "Easy-to-carry",
"Easily stored in pack/cabinet", "Kept in fridge", "Sold where shop for everyday prods",
"Sold where shop for pet", "Shipped fresh/cold to door", "Affordable",
"Expensive but worth it"), "table.q23.Brand.Ratings.0.100.Abbr.by.Brand.Rated.Nom.Nom")),
    Z2 = NULL, groups = NULL, labels = NULL)
test_that("Tables supplied as raw input data",
{
    res <- PrepareData("Scatter", input.data.raw = tables.related.raw)
    expect_equal(res$scatter.variable.indices, c(x = 1, y = 2, sizes = 3, colors = NA, groups = NA))
})

dat.qtable.yvar <- list(X = structure(c(99, 94, 83, 91, 93, 63, 95, 126, 56, 800
), .Dim = c(10L, 1L), .Dimnames = list(c("18 to 24", "25 to 29",
"30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
"65 or more", "NET"), "table.Age.2"), statistic = "Count", basedescriptiontext = "sample size = 800", basedescription = list(
    Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickOne", span = list(
    rows = structure(list(c("18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    10L))), name = "table.Age.2", questions = c("Age", "SUMMARY"
)), Y = list(table.Age = structure(c(`18 to 24` = 12.375, `25 to 29` = 11.75,
`30 to 34` = 10.375, `35 to 39` = 11.375, `40 to 44` = 11.625,
`45 to 49` = 7.875, `50 to 54` = 11.875, `55 to 64` = 15.75,
`65 or more` = 7, NET = 100), statistic = "%", .Dim = 10L, .Dimnames = list(
    c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
    "45 to 49", "50 to 54", "55 to 64", "65 or more", "NET")), basedescriptiontext = "sample size = 800", basedescription = list(
    Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickOne", span = list(
    rows = structure(list(c("18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    10L))), name = "table.Age", questions = c("Age", "SUMMARY"
))), Z1 = structure(c(12.375, 11.75, 10.375, 11.375, 11.625,
7.875, 11.875, 15.75, 7, 100), .Dim = c(10L, 1L), .Dimnames = list(
    c("18 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44",
    "45 to 49", "50 to 54", "55 to 64", "65 or more", "NET"),
    "table.Age"), statistic = "%", basedescriptiontext = "sample size = 800", basedescription = list(
    Minimum = 800L, Maximum = 800L, Range = FALSE, Total = 800L,
    Missing = 0L, EffectiveSampleSize = 800L, EffectiveSampleSizeProportion = 100,
    FilteredProportion = 0), questiontypes = "PickOne", span = list(
    rows = structure(list(c("18 to 24", "25 to 29", "30 to 34",
    "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 64",
    "65 or more", "NET")), class = "data.frame", .Names = "", row.names = c(NA,
    10L))), name = "table.Age", questions = c("Age", "SUMMARY"
)), Z2 = NULL, groups = NULL, labels = NULL)

test_that("Scatter with Qtables keeps table labels",
{
    res <- PrepareData("Scatter", input.data.raw = dat.qtable.yvar)
    expect_equal(colnames(res$data), c("table.Age.2", "table.Age", "table.Age "))

})

