# Minimum Diet Related Indicators

## Introduction

This article will mainly demonstrate how the `riycf` package’s function
can apply to calculating minimum diet-related indicators. According to
the new WHO 2021 guideline, there are four types of complementary
feeding practice indicators related to minimum diet requirements;

- Minimum Dietary Diversity 6-23 Months (MDD),
- Minimum Meal Frequency 6-23 Months (MMF),
- Minimum Milk Feeding Frequency For Non-Breastfed Children 6-23 Months
  (MMFF) and
- Minimum Acceptable Diet.

## Dataset Set-up

Firstly, load the required libraries for data analysis work. We will use
`riycf` package for the IYCF indicator generation work.

``` r

library(riycf)
```

For this guideline demonstration, we will use the sample dataset
provided by the CARE Myanmar team on IYCF modules. A detailed
description of the variable’s name and variable labels are mentioned in
the documentation. Use the following syntax to check for the dataset
description.

``` r

iycfData
#>     csex calc_age_months child_bf child_eibf child_eibf_hrs child_eibf_days
#> 1      1               5        1          0             NA              NA
#> 2      1               4        1          0             NA              NA
#> 3      1               2        1          0             NA              NA
#> 4      1               0        1          0             NA              NA
#> 5      1               2        1          0             NA              NA
#> 6      1               2        1          0             NA              NA
#> 7      1               2        0         NA             NA              NA
#> 8      1               1        1          0             NA              NA
#> 9      1               5        1          1              2              NA
#> 10     1               5        1          0             NA              NA
#> 11     1               0        1          0             NA              NA
#> 12     1               3        1          1              1              NA
#> 13     1               5        1          0             NA              NA
#> 14     0               5        1          0             NA              NA
#> 15     1               1        1          1              1              NA
#> 16     1               1        1          2             NA               3
#> 17     1               1        1          0             NA              NA
#> 18     1               4        1          1              1              NA
#> 19     1               2        1          1              1              NA
#> 20     1               0        1          1              1              NA
#> 21     1               3        1          0             NA              NA
#> 22     1               5        1          0             NA              NA
#> 23     1               3        1          0             NA              NA
#> 24     1               1        1          0             NA              NA
#> 25     1               4        1          0             NA              NA
#> 26     1               4        1          2             NA               2
#> 27     1               4        1          1              1              NA
#> 28     1               5        1          0             NA              NA
#> 29     1               5        0         NA             NA              NA
#> 30     1               4        1          1              3              NA
#> 31     1               5        1          0             NA              NA
#> 32     1               5        1          0             NA              NA
#> 33     1               3        1          2             NA               1
#> 34     1               3        1          1              1              NA
#> 35     1               2        1          2             NA               1
#> 36     1               5        1          0             NA              NA
#> 37     0              -4        1          0             NA              NA
#> 38     1               2        1          0             NA              NA
#> 39     1               1        1          0             NA              NA
#> 40     1               1        1          0             NA              NA
#> 41     1              29       NA         NA             NA              NA
#> 42     1              29       NA         NA             NA              NA
#> 43     1              38       NA         NA             NA              NA
#> 44     1              29       NA         NA             NA              NA
#> 45     1              15        1          1              9              NA
#> 46     1               8        1          0             NA              NA
#> 47     1               7        1          0             NA              NA
#> 48     1              26       NA         NA             NA              NA
#> 49     1              15        1          1              1              NA
#> 50     1              40       NA         NA             NA              NA
#> 51     1              26       NA         NA             NA              NA
#> 52     1              43       NA         NA             NA              NA
#> 53     1              46       NA         NA             NA              NA
#> 54     1              34       NA         NA             NA              NA
#> 55     1              31       NA         NA             NA              NA
#> 56     1              42       NA         NA             NA              NA
#> 57     1              47       NA         NA             NA              NA
#> 58     1              58       NA         NA             NA              NA
#> 59     1              35       NA         NA             NA              NA
#> 60     1              35       NA         NA             NA              NA
#> 61     1              30       NA         NA             NA              NA
#> 62     1              44       NA         NA             NA              NA
#> 63     1              56       NA         NA             NA              NA
#> 64     1               8        1          0             NA              NA
#> 65     1               8        1          1              1              NA
#> 66     1              47       NA         NA             NA              NA
#> 67     1              42       NA         NA             NA              NA
#> 68     1              35       NA         NA             NA              NA
#> 69     1               6        1          0             NA              NA
#> 70     1              56       NA         NA             NA              NA
#> 71     1              22        1          1              2              NA
#> 72     1              52       NA         NA             NA              NA
#> 73     1              11        1          0             NA              NA
#> 74     1              52       NA         NA             NA              NA
#> 75     1              52       NA         NA             NA              NA
#> 76     1              18        1          0             NA              NA
#> 77     1              48       NA         NA             NA              NA
#> 78     1              44       NA         NA             NA              NA
#> 79     1               6        1          0             NA              NA
#> 80     1              58       NA         NA             NA              NA
#> 81     1              32       NA         NA             NA              NA
#> 82     1              41       NA         NA             NA              NA
#> 83     0              42       NA         NA             NA              NA
#> 84     1              24       NA         NA             NA              NA
#> 85     1              42       NA         NA             NA              NA
#> 86     1              43       NA         NA             NA              NA
#> 87     1              46       NA         NA             NA              NA
#> 88     1              55       NA         NA             NA              NA
#> 89     1              19        1          0             NA              NA
#> 90     1              31       NA         NA             NA              NA
#> 91     1               7        1          0             NA              NA
#> 92     1              31       NA         NA             NA              NA
#> 93     1              29       NA         NA             NA              NA
#> 94     1              38       NA         NA             NA              NA
#> 95     1              30       NA         NA             NA              NA
#> 96     1              43       NA         NA             NA              NA
#> 97     1              29       NA         NA             NA              NA
#> 98     1              30       NA         NA             NA              NA
#> 99     1              44       NA         NA             NA              NA
#> 100    1              52       NA         NA             NA              NA
#> 101    1               6        1          0             NA              NA
#> 102    1              57       NA         NA             NA              NA
#> 103    1              29       NA         NA             NA              NA
#> 104    1              42       NA         NA             NA              NA
#> 105    1              34       NA         NA             NA              NA
#> 106    1              53       NA         NA             NA              NA
#> 107    1              24       NA         NA             NA              NA
#> 108    1              49       NA         NA             NA              NA
#> 109    1              39       NA         NA             NA              NA
#> 110    1              38       NA         NA             NA              NA
#> 111    1               6        1          0             NA              NA
#> 112    1              26       NA         NA             NA              NA
#> 113    1              52       NA         NA             NA              NA
#> 114    1              53       NA         NA             NA              NA
#> 115    1              20        1          0             NA              NA
#> 116    1              44       NA         NA             NA              NA
#> 117    1              20        1          1              2              NA
#> 118    1               7        1          0             NA              NA
#> 119    1              10        1          1              4              NA
#> 120    1              48       NA         NA             NA              NA
#> 121    1              54       NA         NA             NA              NA
#> 122    1              32       NA         NA             NA              NA
#> 123    1              26       NA         NA             NA              NA
#> 124    1              34       NA         NA             NA              NA
#> 125    1              41       NA         NA             NA              NA
#> 126    1              21        1          0             NA              NA
#> 127    1              24       NA         NA             NA              NA
#> 128    1              32       NA         NA             NA              NA
#> 129    1              23        1          1              1              NA
#> 130    1              54       NA         NA             NA              NA
#> 131    1              44       NA         NA             NA              NA
#> 132    1              18        1          0             NA              NA
#> 133    0               9        1          0             NA              NA
#> 134    1              13        1          2             NA               3
#> 135    1              29       NA         NA             NA              NA
#> 136    1              48       NA         NA             NA              NA
#> 137    1              23        1          0             NA              NA
#> 138    1              58       NA         NA             NA              NA
#> 139    1              30       NA         NA             NA              NA
#> 140    1              14        1          1              1              NA
#> 141    0              20        1          0             NA              NA
#> 142    1              11        1          0             NA              NA
#> 143    1              41       NA         NA             NA              NA
#> 144    1              21        1          0             NA              NA
#> 145    1              42       NA         NA             NA              NA
#> 146    1               9        1          0             NA              NA
#> 147    1              54       NA         NA             NA              NA
#> 148    1              31       NA         NA             NA              NA
#> 149    1              17        1          2             NA               1
#> 150    1               9        1          0             NA              NA
#> 151    1              35       NA         NA             NA              NA
#> 152    1              44       NA         NA             NA              NA
#> 153    1              12        1          0             NA              NA
#> 154    1              39       NA         NA             NA              NA
#> 155    1               7        1          0             NA              NA
#> 156    1              28       NA         NA             NA              NA
#> 157    1              34       NA         NA             NA              NA
#> 158    1              23        1          1             12              NA
#> 159    1              47       NA         NA             NA              NA
#> 160    1              11        1          0             NA              NA
#> 161    1              47       NA         NA             NA              NA
#> 162    1              19        1          0             NA              NA
#> 163    1              35       NA         NA             NA              NA
#> 164    1              13        1          0             NA              NA
#> 165    1               8        1          0             NA              NA
#> 166    0              56       NA         NA             NA              NA
#> 167    0              58       NA         NA             NA              NA
#> 168    0              40       NA         NA             NA              NA
#> 169    1              44       NA         NA             NA              NA
#> 170    1              20        1          0             NA              NA
#> 171    0              54       NA         NA             NA              NA
#> 172    0              19        1          0             NA              NA
#> 173    1              11        1          0             NA              NA
#> 174    1              22        1          0             NA              NA
#> 175    1              58       NA         NA             NA              NA
#> 176    1              46       NA         NA             NA              NA
#> 177    1              45       NA         NA             NA              NA
#> 178    1              22        1          0             NA              NA
#> 179    1              24       NA         NA             NA              NA
#> 180    1              43       NA         NA             NA              NA
#> 181    1              34       NA         NA             NA              NA
#> 182    0              42       NA         NA             NA              NA
#> 183    0              NA       NA         NA             NA              NA
#> 184    0               9        1          2             NA               4
#> 185    1              12        1          0             NA              NA
#> 186    1              51       NA         NA             NA              NA
#> 187    1              33       NA         NA             NA              NA
#> 188    1               6        1          0             NA              NA
#> 189    1              30       NA         NA             NA              NA
#> 190    1              18        1          0             NA              NA
#> 191    1              24       NA         NA             NA              NA
#> 192    1              47       NA         NA             NA              NA
#> 193    1              27       NA         NA             NA              NA
#> 194    1              46       NA         NA             NA              NA
#> 195    1              37       NA         NA             NA              NA
#> 196    1              12        1          0             NA              NA
#> 197    1              23        1          0             NA              NA
#> 198    1              50       NA         NA             NA              NA
#> 199    1              28       NA         NA             NA              NA
#> 200    1              37       NA         NA             NA              NA
#> 201    1              39       NA         NA             NA              NA
#> 202    1              58       NA         NA             NA              NA
#> 203    1              24       NA         NA             NA              NA
#> 204    1              39       NA         NA             NA              NA
#> 205    1              50       NA         NA             NA              NA
#> 206    1              49       NA         NA             NA              NA
#> 207    1              28       NA         NA             NA              NA
#> 208    1              38       NA         NA             NA              NA
#> 209    1              36       NA         NA             NA              NA
#> 210    1              NA       NA         NA             NA              NA
#> 211    1              46       NA         NA             NA              NA
#> 212    1              11        1          0             NA              NA
#> 213    0              22        1          1              1              NA
#> 214    1              12        1          1              2              NA
#> 215    1              42       NA         NA             NA              NA
#> 216    1              NA        1          0             NA              NA
#> 217    1              35       NA         NA             NA              NA
#> 218    1              30       NA         NA             NA              NA
#> 219    1              18        1          0             NA              NA
#> 220    1              41       NA         NA             NA              NA
#> 221    1              58       NA         NA             NA              NA
#> 222    1              36       NA         NA             NA              NA
#> 223    1              10        1          1             10              NA
#> 224    1              39       NA         NA             NA              NA
#> 225    1              47       NA         NA             NA              NA
#> 226    1              52       NA         NA             NA              NA
#> 227    1              23        1          0             NA              NA
#> 228    1              38       NA         NA             NA              NA
#> 229    1               8        1          0             NA              NA
#> 230    1              56       NA         NA             NA              NA
#> 231    1              10        1          0             NA              NA
#> 232    1              49       NA         NA             NA              NA
#> 233    1              26       NA         NA             NA              NA
#> 234    1              21        1          0             NA              NA
#> 235    1              46       NA         NA             NA              NA
#> 236    1              26       NA         NA             NA              NA
#> 237    1              28       NA         NA             NA              NA
#> 238    1              14        1          0             NA              NA
#> 239    1              51       NA         NA             NA              NA
#> 240    1              NA       NA         NA             NA              NA
#> 241    1              22        1          0             NA              NA
#> 242    1               6        1          0             NA              NA
#> 243    0              19        1          0             NA              NA
#> 244    1              17        1          0             NA              NA
#> 245    1              NA       NA         NA             NA              NA
#> 246    1              NA        1          0             NA              NA
#> 247    1              NA       NA         NA             NA              NA
#> 248    1              NA        1          0             NA              NA
#> 249    1              24       NA         NA             NA              NA
#> 250    0              56       NA         NA             NA              NA
#> 251    1              19        1          0             NA              NA
#> 252    1              NA       NA         NA             NA              NA
#> 253    1              NA        1          0             NA              NA
#> 254    1              25       NA         NA             NA              NA
#> 255    1               6        1          1              2              NA
#> 256    1              NA       NA         NA             NA              NA
#> 257    1              32       NA         NA             NA              NA
#> 258    1              38       NA         NA             NA              NA
#> 259    1              15        1          1              1              NA
#> 260    1              19        1          0             NA              NA
#> 261    1              47       NA         NA             NA              NA
#> 262    1              57       NA         NA             NA              NA
#> 263    1              23        1          0             NA              NA
#> 264    1              NA        1          1              1              NA
#> 265    1              28       NA         NA             NA              NA
#> 266    0              22        1          1              1              NA
#> 267    0              10        1        999             NA              NA
#> 268    1              32       NA         NA             NA              NA
#> 269    1              NA       NA         NA             NA              NA
#> 270    1              45       NA         NA             NA              NA
#> 271    1              NA       NA         NA             NA              NA
#> 272    1              19        1          0             NA              NA
#> 273    1              NA        1          0             NA              NA
#> 274    1              NA       NA         NA             NA              NA
#> 275    1              40       NA         NA             NA              NA
#> 276    1              NA       NA         NA             NA              NA
#> 277    0               8        1          0             NA              NA
#> 278    0              NA       NA         NA             NA              NA
#> 279    1              20        1          0             NA              NA
#> 280    1               9        1          0             NA              NA
#> 281    1              39       NA         NA             NA              NA
#> 282    1              19        1          3             NA              NA
#> 283    1              NA        1          0             NA              NA
#> 284    1              17        1          1              2              NA
#> 285    1              36       NA         NA             NA              NA
#> 286    1              14        1          0             NA              NA
#> 287    1              38       NA         NA             NA              NA
#> 288    1              35       NA         NA             NA              NA
#> 289    1              46       NA         NA             NA              NA
#> 290    1              51       NA         NA             NA              NA
#> 291    1              14        0         NA             NA              NA
#> 292    1              27       NA         NA             NA              NA
#> 293    1               8        1          1              1              NA
#> 294    1              58       NA         NA             NA              NA
#> 295    1              43       NA         NA             NA              NA
#> 296    1              48       NA         NA             NA              NA
#> 297    1              19        0         NA             NA              NA
#> 298    1              45       NA         NA             NA              NA
#> 299    1               7        0         NA             NA              NA
#> 300    1              18        1          0             NA              NA
#> 301    1               8        1          1              1              NA
#> 302    1              39       NA         NA             NA              NA
#> 303    1              12        1          0             NA              NA
#> 304    1              10        1          1              1              NA
#> 305    1              48       NA         NA             NA              NA
#> 306    1              10        1          1              3              NA
#> 307    1              NA        1          0             NA              NA
#> 308    1              NA        1          0             NA              NA
#> 309    1              49       NA         NA             NA              NA
#> 310    1              18        1          1              1              NA
#> 311    1               7        1          1              3              NA
#> 312    1              NA       NA         NA             NA              NA
#> 313    1              18        1          0             NA              NA
#> 314    1              44       NA         NA             NA              NA
#> 315    1              20        0         NA             NA              NA
#> 316    0              21        1          0             NA              NA
#> 317    0               7        1          0             NA              NA
#> 318    0               9        1          0             NA              NA
#> 319    1               6        1          0             NA              NA
#> 320    1              46       NA         NA             NA              NA
#> 321    1              25       NA         NA             NA              NA
#> 322    1              42       NA         NA             NA              NA
#> 323    1              34       NA         NA             NA              NA
#> 324    1              11        1          1              1              NA
#> 325    1              13        1          0             NA              NA
#> 326    1               6        1          0             NA              NA
#> 327    1              59       NA         NA             NA              NA
#> 328    1              34       NA         NA             NA              NA
#> 329    1              12        1          0             NA              NA
#> 330    0              18        1          0             NA              NA
#> 331    0              30       NA         NA             NA              NA
#> 332    0               7        1          0             NA              NA
#> 333    0              45       NA         NA             NA              NA
#> 334    1              46       NA         NA             NA              NA
#> 335    1              41       NA         NA             NA              NA
#> 336    1              50       NA         NA             NA              NA
#> 337    1              11        1          0             NA              NA
#> 338    1              33       NA         NA             NA              NA
#> 339    1               9        1          0             NA              NA
#> 340    1              14        1          0             NA              NA
#> 341    1              40       NA         NA             NA              NA
#> 342    1              23        1          0             NA              NA
#> 343    1              55       NA         NA             NA              NA
#> 344    1              28       NA         NA             NA              NA
#> 345    1              11        1          0             NA              NA
#> 346    1              41       NA         NA             NA              NA
#> 347    1               8        1          0             NA              NA
#> 348    1              NA       NA         NA             NA              NA
#> 349    1              20      999         NA             NA              NA
#> 350    1              46       NA         NA             NA              NA
#> 351    1              NA       NA         NA             NA              NA
#> 352    1              39       NA         NA             NA              NA
#> 353    1              38       NA         NA             NA              NA
#> 354    1              11        1          1              3              NA
#> 355    1              44       NA         NA             NA              NA
#> 356    1              13        0         NA             NA              NA
#> 357    0              14        1          2             NA               1
#> 358    1               8        1          0             NA              NA
#> 359    1              23        1          0             NA              NA
#>     bf_2days child_bfyest bf_bottle child_water child_bms child_bms_freq
#> 1          1            1         0           0         0             NA
#> 2          1            1         0           1         0             NA
#> 3          0            1         0           0         0             NA
#> 4          0            1         0           1         0             NA
#> 5          0            0         1           0         0             NA
#> 6          0            1         0           0         0             NA
#> 7          0            0         1           0         1              3
#> 8          1            0         1           0         0             NA
#> 9          0            1         0           1         0             NA
#> 10         0            1         0           1         0             NA
#> 11         0            1         0           0         0             NA
#> 12         0            1         0           0         0             NA
#> 13         0            1         0           1         0             NA
#> 14         0            1         0           0         0             NA
#> 15         1            1         0           1         0             NA
#> 16         1            1         0           1         0             NA
#> 17         0            1         0           0         0             NA
#> 18         1            1         0           1         1              2
#> 19         1            1         0           0         0             NA
#> 20         0            1         1           0         0             NA
#> 21         1            1         1           1         0             NA
#> 22         0            0         0           0         0             NA
#> 23         1            1         0           0         0             NA
#> 24         0            1         1           0         0             NA
#> 25         0            1         0           1         0             NA
#> 26         1            1         0           1         0             NA
#> 27         0            1         0           0         0             NA
#> 28         0            1         1           1         0             NA
#> 29         0            0         0           1         0             NA
#> 30         1            1         0           1         1              5
#> 31         1            1         0           1         0             NA
#> 32         1            1         0           1         0             NA
#> 33         0            1         0           1         0             NA
#> 34         0            1         0           0         0             NA
#> 35         1            1         0           0         0             NA
#> 36         1            1         0           0         0             NA
#> 37         0            1         0           1         0             NA
#> 38         0            1         0           1         0             NA
#> 39         1            1         0           0         0             NA
#> 40         0            1         1           0         0             NA
#> 41         0           NA         0          NA        NA             NA
#> 42         1           NA         0          NA        NA             NA
#> 43         1           NA         0          NA        NA             NA
#> 44         0           NA         0          NA        NA             NA
#> 45         0            1         1           1         0             NA
#> 46         0            1         0           1         0             NA
#> 47         0            1         0           1         0             NA
#> 48         1           NA         0          NA        NA             NA
#> 49         0            0         0           1         0             NA
#> 50         0           NA         0          NA        NA             NA
#> 51         1           NA         0          NA        NA             NA
#> 52         1           NA         0          NA        NA             NA
#> 53         0           NA         0          NA        NA             NA
#> 54         0           NA         0          NA        NA             NA
#> 55         1           NA         0          NA        NA             NA
#> 56         0           NA         0          NA        NA             NA
#> 57         1           NA         1          NA        NA             NA
#> 58         0           NA         0          NA        NA             NA
#> 59         1           NA         0          NA        NA             NA
#> 60         1           NA         0          NA        NA             NA
#> 61         1           NA         0          NA        NA             NA
#> 62         0           NA         0          NA        NA             NA
#> 63         0           NA         0          NA        NA             NA
#> 64         0            1         0           1         0             NA
#> 65         0            1         0           1         0             NA
#> 66         1           NA         0          NA        NA             NA
#> 67         0           NA         0          NA        NA             NA
#> 68         1           NA         0          NA        NA             NA
#> 69         0            1         0           1         1              3
#> 70         1           NA         0          NA        NA             NA
#> 71         0            0         0           1         0             NA
#> 72         0           NA         0          NA        NA             NA
#> 73         1            0         0           1         0             NA
#> 74         1           NA         0          NA        NA             NA
#> 75         1           NA         0          NA        NA             NA
#> 76         0            1         0           1         0             NA
#> 77         0           NA         0          NA        NA             NA
#> 78         1           NA         0          NA        NA             NA
#> 79         1            0         0           0         0             NA
#> 80         1           NA         1          NA        NA             NA
#> 81         0           NA         0          NA        NA             NA
#> 82         0           NA         1          NA        NA             NA
#> 83         0           NA         0          NA        NA             NA
#> 84         0           NA         0          NA        NA             NA
#> 85         0           NA         0          NA        NA             NA
#> 86         1           NA         0          NA        NA             NA
#> 87         0           NA         0          NA        NA             NA
#> 88         1           NA         0          NA        NA             NA
#> 89         0            0         1           1         0             NA
#> 90         1           NA         1          NA        NA             NA
#> 91         0            0         0           1         0             NA
#> 92         1           NA         0          NA        NA             NA
#> 93         1           NA         0          NA        NA             NA
#> 94         0           NA         0          NA        NA             NA
#> 95         0           NA         0          NA        NA             NA
#> 96         1           NA         0          NA        NA             NA
#> 97         1           NA         0          NA        NA             NA
#> 98         1           NA         0          NA        NA             NA
#> 99         0           NA         1          NA        NA             NA
#> 100        0           NA         0          NA        NA             NA
#> 101        0            1         0           1         0             NA
#> 102        1           NA         0          NA        NA             NA
#> 103        1           NA         0          NA        NA             NA
#> 104        0           NA         0          NA        NA             NA
#> 105        0           NA         0          NA        NA             NA
#> 106        0           NA         0          NA        NA             NA
#> 107        0           NA         0          NA        NA             NA
#> 108        1           NA         0          NA        NA             NA
#> 109        0           NA         0          NA        NA             NA
#> 110        0           NA         1          NA        NA             NA
#> 111        1            1         0           1         0             NA
#> 112        0           NA         0          NA        NA             NA
#> 113        0           NA         0          NA        NA             NA
#> 114        1           NA         1          NA        NA             NA
#> 115        0            1         0           1         0             NA
#> 116        0           NA         0          NA        NA             NA
#> 117        0            0         0           1         0             NA
#> 118        0            0         0           1         0             NA
#> 119        0            1         0           1         0             NA
#> 120        0           NA         0          NA        NA             NA
#> 121        1           NA         1          NA        NA             NA
#> 122        1           NA         0          NA        NA             NA
#> 123        0           NA         0          NA        NA             NA
#> 124        0           NA         1          NA        NA             NA
#> 125        1           NA         0          NA        NA             NA
#> 126        0            0         0           1         0             NA
#> 127        0           NA         0          NA        NA             NA
#> 128        0           NA         0          NA        NA             NA
#> 129        0            0         1           1         0             NA
#> 130        0           NA         0          NA        NA             NA
#> 131        0           NA         0          NA        NA             NA
#> 132        1            1         0           1         0             NA
#> 133        0            1         1           1         0             NA
#> 134        1            1         1           1         0             NA
#> 135        1           NA         0          NA        NA             NA
#> 136        0           NA         0          NA        NA             NA
#> 137        1            1         0           1         0             NA
#> 138        0           NA         0          NA        NA             NA
#> 139        0           NA         0          NA        NA             NA
#> 140        0            1         0           1         1              1
#> 141        0            1         0           1         0             NA
#> 142        0            1         0           1         0             NA
#> 143        0           NA         0          NA        NA             NA
#> 144        0            0         0           1         0             NA
#> 145        0           NA         0          NA        NA             NA
#> 146        1            1         0           1         0             NA
#> 147        1           NA         1          NA        NA             NA
#> 148        0           NA         1          NA        NA             NA
#> 149        0            0         0           1         0             NA
#> 150        1            1         0           1         0             NA
#> 151        0           NA         0          NA        NA             NA
#> 152        1           NA         0          NA        NA             NA
#> 153        0            1         0           1         0             NA
#> 154        0           NA         0          NA        NA             NA
#> 155        0            1         0           1         1              3
#> 156        0           NA         0          NA        NA             NA
#> 157        0           NA         0          NA        NA             NA
#> 158        0            0         0           1         1              2
#> 159        0           NA         0          NA        NA             NA
#> 160        1            1         0           1         0             NA
#> 161        0           NA         0          NA        NA             NA
#> 162        0            0         0           1         0             NA
#> 163        1           NA         0          NA        NA             NA
#> 164        1            1         0           1         1              4
#> 165        0            1         0           1         0             NA
#> 166        0           NA         0          NA        NA             NA
#> 167        1           NA         0          NA        NA             NA
#> 168        0           NA         0          NA        NA             NA
#> 169        0           NA         0          NA        NA             NA
#> 170        0            1         0           1         0             NA
#> 171        0           NA         0          NA        NA             NA
#> 172        1            0         1           0         0             NA
#> 173        1            1         0           1         0             NA
#> 174        0            0         0           1         0             NA
#> 175        1           NA         0          NA        NA             NA
#> 176        0           NA         0          NA        NA             NA
#> 177        0           NA         0          NA        NA             NA
#> 178        0            0         0           1         0             NA
#> 179        1           NA         1          NA        NA             NA
#> 180        0           NA         0          NA        NA             NA
#> 181        0           NA         0          NA        NA             NA
#> 182        1           NA         0          NA        NA             NA
#> 183        0           NA         0          NA        NA             NA
#> 184        0            1         0           1         0             NA
#> 185        1            1         0           1         0             NA
#> 186        0           NA         0          NA        NA             NA
#> 187        0           NA         0          NA        NA             NA
#> 188        0            1         0           1         0             NA
#> 189        0           NA         1          NA        NA             NA
#> 190        0            1         0           1         0             NA
#> 191        1           NA         0          NA        NA             NA
#> 192        0           NA         1          NA        NA             NA
#> 193        0           NA         0          NA        NA             NA
#> 194        0           NA         0          NA        NA             NA
#> 195        0           NA         0          NA        NA             NA
#> 196        0            1         0           1         0             NA
#> 197        0            0         0           1         0             NA
#> 198        0           NA         0          NA        NA             NA
#> 199        1           NA         0          NA        NA             NA
#> 200        1           NA         0          NA        NA             NA
#> 201        0           NA         0          NA        NA             NA
#> 202        1           NA         0          NA        NA             NA
#> 203        0           NA         0          NA        NA             NA
#> 204        1           NA         0          NA        NA             NA
#> 205        0           NA         0          NA        NA             NA
#> 206        0           NA         1          NA        NA             NA
#> 207        0           NA         0          NA        NA             NA
#> 208        0           NA         1          NA        NA             NA
#> 209        1           NA         0          NA        NA             NA
#> 210        0           NA         0          NA        NA             NA
#> 211        0           NA         1          NA        NA             NA
#> 212        0            1         0           1         0             NA
#> 213        0            1         0           1         0             NA
#> 214        0            1         0           1         0             NA
#> 215        0           NA         1          NA        NA             NA
#> 216        1            1         0           1         0             NA
#> 217        0           NA         0          NA        NA             NA
#> 218        0           NA         0          NA        NA             NA
#> 219        0            0         0           1         0             NA
#> 220        0           NA         0          NA        NA             NA
#> 221        1           NA         0          NA        NA             NA
#> 222        0           NA         0          NA        NA             NA
#> 223        0            1         0           1         0             NA
#> 224        1           NA         0          NA        NA             NA
#> 225        0           NA         0          NA        NA             NA
#> 226        1           NA         0          NA        NA             NA
#> 227        1            0         0           1         0             NA
#> 228        0           NA         0          NA        NA             NA
#> 229        1            1         1           1         0             NA
#> 230        0           NA         0          NA        NA             NA
#> 231        0            1         0           0         0             NA
#> 232        0           NA         1          NA        NA             NA
#> 233        1           NA         1          NA        NA             NA
#> 234        1            0         0           1         0             NA
#> 235        1           NA         0          NA        NA             NA
#> 236        0           NA         0          NA        NA             NA
#> 237        1           NA         0          NA        NA             NA
#> 238        0            1         0           1         0             NA
#> 239        1           NA         0          NA        NA             NA
#> 240        1           NA         0          NA        NA             NA
#> 241        0            0         0           1         0             NA
#> 242        0            1         0           1         0             NA
#> 243        0            0         0           0         0             NA
#> 244        0            1         0           1         0             NA
#> 245        0           NA         1          NA        NA             NA
#> 246        0            0         0           1         0             NA
#> 247        0           NA         0          NA        NA             NA
#> 248        0            0         0           1         0             NA
#> 249        0           NA         0          NA        NA             NA
#> 250        0           NA         0          NA        NA             NA
#> 251        0            1         0           1         0             NA
#> 252        0           NA         1          NA        NA             NA
#> 253        0            1         0           1         0             NA
#> 254        0           NA         1          NA        NA             NA
#> 255        1            1         0           1         0             NA
#> 256        1           NA         0          NA        NA             NA
#> 257        1           NA         0          NA        NA             NA
#> 258        0           NA         0          NA        NA             NA
#> 259        0            1         1           1         0             NA
#> 260        0            1         0           1         0             NA
#> 261        0           NA         0          NA        NA             NA
#> 262        0           NA         0          NA        NA             NA
#> 263        0            0         0           1         0             NA
#> 264        0            1         1           1         0             NA
#> 265        0           NA         0          NA        NA             NA
#> 266        0            1         0           1         0             NA
#> 267        0            1         1           1         0             NA
#> 268        0           NA         0          NA        NA             NA
#> 269        1           NA         0          NA        NA             NA
#> 270        0           NA         0          NA        NA             NA
#> 271        1           NA         0          NA        NA             NA
#> 272        1            1         0           1         0             NA
#> 273        0            1         0           1         0             NA
#> 274        0           NA         0          NA        NA             NA
#> 275        0           NA         0          NA        NA             NA
#> 276        0           NA         0          NA        NA             NA
#> 277        0            1         0           1         0             NA
#> 278        0           NA         0          NA        NA             NA
#> 279        1            1         0           1         0             NA
#> 280        0            1         0           1         0             NA
#> 281        0           NA         1          NA        NA             NA
#> 282        1            0         1           1         1              3
#> 283        1            1         1           1         0             NA
#> 284        0            0         0           1         0             NA
#> 285        0           NA         1          NA        NA             NA
#> 286        0            1         0           1         0             NA
#> 287        1           NA         0          NA        NA             NA
#> 288        0           NA         0          NA        NA             NA
#> 289        0           NA         0          NA        NA             NA
#> 290        0           NA         0          NA        NA             NA
#> 291        1            0         0           1         0             NA
#> 292        0           NA         0          NA        NA             NA
#> 293        0            1         0           1         0             NA
#> 294        1           NA         0          NA        NA             NA
#> 295        0           NA         1          NA        NA             NA
#> 296        0           NA         0          NA        NA             NA
#> 297        1            0         0           1         1              3
#> 298        0           NA         0          NA        NA             NA
#> 299        0            0         0           1         0             NA
#> 300        0            0         0           1         0             NA
#> 301        0            1         0           1         0             NA
#> 302        0           NA         0          NA        NA             NA
#> 303        1            1         0           1         0             NA
#> 304        1            1         0           1         0             NA
#> 305        0           NA         0          NA        NA             NA
#> 306        1            1         0           1         0             NA
#> 307        0            1         1           1         0             NA
#> 308        1            1         0           1         0             NA
#> 309        0           NA         1          NA        NA             NA
#> 310        1            1         0           1         0             NA
#> 311        0            1         0           0         0             NA
#> 312        0           NA         0          NA        NA             NA
#> 313        0            1         1           1         0             NA
#> 314        0           NA         0          NA        NA             NA
#> 315        1            1         0           1         0             NA
#> 316        1            0         1           1         0             NA
#> 317        1            1         0           1         0             NA
#> 318        0            1         0           1         1              1
#> 319        0            0         1           1         1              5
#> 320        1           NA         0          NA        NA             NA
#> 321        0           NA         0          NA        NA             NA
#> 322        1           NA         0          NA        NA             NA
#> 323        0           NA         0          NA        NA             NA
#> 324        0            1         0           1         0             NA
#> 325        0            1         0           1         0             NA
#> 326        0            1         1           1         1              1
#> 327        1           NA         0          NA        NA             NA
#> 328        0           NA         0          NA        NA             NA
#> 329        0            1         1           1         0             NA
#> 330        0            0         0           1         0             NA
#> 331        0           NA         1          NA        NA             NA
#> 332        1            0         0           1         0             NA
#> 333        0           NA         1          NA        NA             NA
#> 334        0           NA         0          NA        NA             NA
#> 335        0           NA         0          NA        NA             NA
#> 336        1           NA         0          NA        NA             NA
#> 337        1            0         0           1         0             NA
#> 338        0           NA         0          NA        NA             NA
#> 339        0            0         1           1         0             NA
#> 340        0            0         0           1         0             NA
#> 341        1           NA         0          NA        NA             NA
#> 342        0            0         0           1         0             NA
#> 343        1           NA         0          NA        NA             NA
#> 344        1           NA         0          NA        NA             NA
#> 345        0            1         1           1         0             NA
#> 346        0           NA         1          NA        NA             NA
#> 347        0            1         1           1         0             NA
#> 348        1           NA         1          NA        NA             NA
#> 349        0            1         0           1         0             NA
#> 350        1           NA         1          NA        NA             NA
#> 351        0           NA         0          NA        NA             NA
#> 352        1           NA         0          NA        NA             NA
#> 353        0           NA         1          NA        NA             NA
#> 354        1            1         0           0         0             NA
#> 355        0           NA         1          NA        NA             NA
#> 356        1            0         0           1         0             NA
#> 357        0            1         0           1         0             NA
#> 358        1            1         0           1         0             NA
#> 359        0            0         0           1         0             NA
#>     child_milk child_milk_freq child_milk_sweet child_mproduct
#> 1            0              NA               NA              0
#> 2            0              NA               NA              0
#> 3            0              NA               NA              0
#> 4            0              NA               NA              0
#> 5            0              NA               NA              0
#> 6            0              NA               NA              0
#> 7            0              NA               NA              0
#> 8            0              NA               NA              0
#> 9            0              NA               NA              0
#> 10           0              NA               NA              0
#> 11           0              NA               NA              0
#> 12           0              NA               NA              0
#> 13           0              NA               NA              0
#> 14           0              NA               NA              0
#> 15           0              NA               NA              0
#> 16           0              NA               NA              0
#> 17           1               2                1              0
#> 18           1               2                1              0
#> 19           0              NA               NA              0
#> 20           0              NA               NA              0
#> 21           0              NA               NA              0
#> 22           0              NA               NA              0
#> 23           0              NA               NA              0
#> 24           0              NA               NA              0
#> 25           0              NA               NA              0
#> 26           0              NA               NA              0
#> 27           0              NA               NA              0
#> 28           0              NA               NA              0
#> 29           1               7                1              0
#> 30           0              NA               NA              0
#> 31           0              NA               NA              0
#> 32           0              NA               NA              0
#> 33           0              NA               NA              0
#> 34           0              NA               NA              0
#> 35           0              NA               NA              0
#> 36           0              NA               NA              0
#> 37           0              NA               NA              0
#> 38           0              NA               NA              0
#> 39           0              NA               NA              0
#> 40           0              NA               NA              0
#> 41          NA              NA               NA             NA
#> 42          NA              NA               NA             NA
#> 43          NA              NA               NA             NA
#> 44          NA              NA               NA             NA
#> 45           0              NA               NA              0
#> 46           0              NA               NA              0
#> 47           0              NA               NA              0
#> 48          NA              NA               NA             NA
#> 49           1               3                1              0
#> 50          NA              NA               NA             NA
#> 51          NA              NA               NA             NA
#> 52          NA              NA               NA             NA
#> 53          NA              NA               NA             NA
#> 54          NA              NA               NA             NA
#> 55          NA              NA               NA             NA
#> 56          NA              NA               NA             NA
#> 57          NA              NA               NA             NA
#> 58          NA              NA               NA             NA
#> 59          NA              NA               NA             NA
#> 60          NA              NA               NA             NA
#> 61          NA              NA               NA             NA
#> 62          NA              NA               NA             NA
#> 63          NA              NA               NA             NA
#> 64           0              NA               NA              0
#> 65           0              NA               NA              0
#> 66          NA              NA               NA             NA
#> 67          NA              NA               NA             NA
#> 68          NA              NA               NA             NA
#> 69           0              NA               NA              0
#> 70          NA              NA               NA             NA
#> 71           0              NA               NA              0
#> 72          NA              NA               NA             NA
#> 73           0              NA               NA              0
#> 74          NA              NA               NA             NA
#> 75          NA              NA               NA             NA
#> 76           0              NA               NA              0
#> 77          NA              NA               NA             NA
#> 78          NA              NA               NA             NA
#> 79           0              NA               NA              0
#> 80          NA              NA               NA             NA
#> 81          NA              NA               NA             NA
#> 82          NA              NA               NA             NA
#> 83          NA              NA               NA             NA
#> 84          NA              NA               NA             NA
#> 85          NA              NA               NA             NA
#> 86          NA              NA               NA             NA
#> 87          NA              NA               NA             NA
#> 88          NA              NA               NA             NA
#> 89           0              NA               NA              0
#> 90          NA              NA               NA             NA
#> 91           0              NA               NA              0
#> 92          NA              NA               NA             NA
#> 93          NA              NA               NA             NA
#> 94          NA              NA               NA             NA
#> 95          NA              NA               NA             NA
#> 96          NA              NA               NA             NA
#> 97          NA              NA               NA             NA
#> 98          NA              NA               NA             NA
#> 99          NA              NA               NA             NA
#> 100         NA              NA               NA             NA
#> 101          0              NA               NA              0
#> 102         NA              NA               NA             NA
#> 103         NA              NA               NA             NA
#> 104         NA              NA               NA             NA
#> 105         NA              NA               NA             NA
#> 106         NA              NA               NA             NA
#> 107         NA              NA               NA             NA
#> 108         NA              NA               NA             NA
#> 109         NA              NA               NA             NA
#> 110         NA              NA               NA             NA
#> 111          0              NA               NA              0
#> 112         NA              NA               NA             NA
#> 113         NA              NA               NA             NA
#> 114         NA              NA               NA             NA
#> 115          0              NA               NA              0
#> 116         NA              NA               NA             NA
#> 117          0              NA               NA              0
#> 118          0              NA               NA              0
#> 119          0              NA               NA              0
#> 120         NA              NA               NA             NA
#> 121         NA              NA               NA             NA
#> 122         NA              NA               NA             NA
#> 123         NA              NA               NA             NA
#> 124         NA              NA               NA             NA
#> 125         NA              NA               NA             NA
#> 126          0              NA               NA              0
#> 127         NA              NA               NA             NA
#> 128         NA              NA               NA             NA
#> 129          0              NA               NA              0
#> 130         NA              NA               NA             NA
#> 131         NA              NA               NA             NA
#> 132          0              NA               NA              0
#> 133          0              NA               NA              0
#> 134          0              NA               NA              0
#> 135         NA              NA               NA             NA
#> 136         NA              NA               NA             NA
#> 137          0              NA               NA              0
#> 138         NA              NA               NA             NA
#> 139         NA              NA               NA             NA
#> 140          1               2                1              0
#> 141          0              NA               NA              0
#> 142          0              NA               NA              0
#> 143         NA              NA               NA             NA
#> 144          0              NA               NA              0
#> 145         NA              NA               NA             NA
#> 146          0              NA               NA              0
#> 147         NA              NA               NA             NA
#> 148         NA              NA               NA             NA
#> 149          0              NA               NA              0
#> 150          0              NA               NA              0
#> 151         NA              NA               NA             NA
#> 152         NA              NA               NA             NA
#> 153          0              NA               NA              0
#> 154         NA              NA               NA             NA
#> 155          1               3                1              0
#> 156         NA              NA               NA             NA
#> 157         NA              NA               NA             NA
#> 158          0              NA               NA              0
#> 159         NA              NA               NA             NA
#> 160          0              NA               NA              0
#> 161         NA              NA               NA             NA
#> 162          0              NA               NA              0
#> 163         NA              NA               NA             NA
#> 164          0              NA               NA              0
#> 165          0              NA               NA              0
#> 166         NA              NA               NA             NA
#> 167         NA              NA               NA             NA
#> 168         NA              NA               NA             NA
#> 169         NA              NA               NA             NA
#> 170          0              NA               NA              0
#> 171         NA              NA               NA             NA
#> 172          0              NA               NA              0
#> 173          0              NA               NA              0
#> 174          0              NA               NA              0
#> 175         NA              NA               NA             NA
#> 176         NA              NA               NA             NA
#> 177         NA              NA               NA             NA
#> 178          0              NA               NA              0
#> 179         NA              NA               NA             NA
#> 180         NA              NA               NA             NA
#> 181         NA              NA               NA             NA
#> 182         NA              NA               NA             NA
#> 183         NA              NA               NA             NA
#> 184          0              NA               NA              0
#> 185          1               1                1              0
#> 186         NA              NA               NA             NA
#> 187         NA              NA               NA             NA
#> 188          0              NA               NA              0
#> 189         NA              NA               NA             NA
#> 190          0              NA               NA              0
#> 191         NA              NA               NA             NA
#> 192         NA              NA               NA             NA
#> 193         NA              NA               NA             NA
#> 194         NA              NA               NA             NA
#> 195         NA              NA               NA             NA
#> 196          0              NA               NA              0
#> 197          1               2                1              0
#> 198         NA              NA               NA             NA
#> 199         NA              NA               NA             NA
#> 200         NA              NA               NA             NA
#> 201         NA              NA               NA             NA
#> 202         NA              NA               NA             NA
#> 203         NA              NA               NA             NA
#> 204         NA              NA               NA             NA
#> 205         NA              NA               NA             NA
#> 206         NA              NA               NA             NA
#> 207         NA              NA               NA             NA
#> 208         NA              NA               NA             NA
#> 209         NA              NA               NA             NA
#> 210         NA              NA               NA             NA
#> 211         NA              NA               NA             NA
#> 212          0              NA               NA              0
#> 213          0              NA               NA              0
#> 214          0              NA               NA              0
#> 215         NA              NA               NA             NA
#> 216          0              NA               NA              0
#> 217         NA              NA               NA             NA
#> 218         NA              NA               NA             NA
#> 219          0              NA               NA              0
#> 220         NA              NA               NA             NA
#> 221         NA              NA               NA             NA
#> 222         NA              NA               NA             NA
#> 223          0              NA               NA              0
#> 224         NA              NA               NA             NA
#> 225         NA              NA               NA             NA
#> 226         NA              NA               NA             NA
#> 227          0              NA               NA              0
#> 228         NA              NA               NA             NA
#> 229          0              NA               NA              0
#> 230         NA              NA               NA             NA
#> 231          0              NA               NA              0
#> 232         NA              NA               NA             NA
#> 233         NA              NA               NA             NA
#> 234          0              NA               NA              0
#> 235         NA              NA               NA             NA
#> 236         NA              NA               NA             NA
#> 237         NA              NA               NA             NA
#> 238          0              NA               NA              0
#> 239         NA              NA               NA             NA
#> 240         NA              NA               NA             NA
#> 241          0              NA               NA              0
#> 242          0              NA               NA              0
#> 243          0              NA               NA              0
#> 244          0              NA               NA              0
#> 245         NA              NA               NA             NA
#> 246          0              NA               NA              0
#> 247         NA              NA               NA             NA
#> 248          0              NA               NA              0
#> 249         NA              NA               NA             NA
#> 250         NA              NA               NA             NA
#> 251          0              NA               NA              0
#> 252         NA              NA               NA             NA
#> 253          0              NA               NA              0
#> 254         NA              NA               NA             NA
#> 255          0              NA               NA              0
#> 256         NA              NA               NA             NA
#> 257         NA              NA               NA             NA
#> 258         NA              NA               NA             NA
#> 259          0              NA               NA              0
#> 260          0              NA               NA              0
#> 261         NA              NA               NA             NA
#> 262         NA              NA               NA             NA
#> 263          1               4                1              0
#> 264          0              NA               NA              0
#> 265         NA              NA               NA             NA
#> 266          0              NA               NA              0
#> 267          0              NA               NA              0
#> 268         NA              NA               NA             NA
#> 269         NA              NA               NA             NA
#> 270         NA              NA               NA             NA
#> 271         NA              NA               NA             NA
#> 272          0              NA               NA              0
#> 273          0              NA               NA              0
#> 274         NA              NA               NA             NA
#> 275         NA              NA               NA             NA
#> 276         NA              NA               NA             NA
#> 277          0              NA               NA              0
#> 278         NA              NA               NA             NA
#> 279          0              NA               NA              0
#> 280          0              NA               NA              0
#> 281         NA              NA               NA             NA
#> 282          0              NA               NA              0
#> 283          0              NA               NA              0
#> 284          0              NA               NA              0
#> 285         NA              NA               NA             NA
#> 286          0              NA               NA              0
#> 287         NA              NA               NA             NA
#> 288         NA              NA               NA             NA
#> 289         NA              NA               NA             NA
#> 290         NA              NA               NA             NA
#> 291          0              NA               NA              0
#> 292         NA              NA               NA             NA
#> 293          0              NA               NA              0
#> 294         NA              NA               NA             NA
#> 295         NA              NA               NA             NA
#> 296         NA              NA               NA             NA
#> 297          0              NA               NA              0
#> 298         NA              NA               NA             NA
#> 299          0              NA               NA              0
#> 300          0              NA               NA              0
#> 301          0              NA               NA              0
#> 302         NA              NA               NA             NA
#> 303          0              NA               NA              0
#> 304          0              NA               NA              0
#> 305         NA              NA               NA             NA
#> 306          0              NA               NA              0
#> 307          0              NA               NA              0
#> 308          0              NA               NA              0
#> 309         NA              NA               NA             NA
#> 310          0              NA               NA              0
#> 311          0              NA               NA              0
#> 312         NA              NA               NA             NA
#> 313          1               1                1              0
#> 314         NA              NA               NA             NA
#> 315          0              NA               NA              0
#> 316          0              NA               NA              0
#> 317          0              NA               NA              0
#> 318          0              NA               NA              0
#> 319          0              NA               NA              0
#> 320         NA              NA               NA             NA
#> 321         NA              NA               NA             NA
#> 322         NA              NA               NA             NA
#> 323         NA              NA               NA             NA
#> 324          0              NA               NA              0
#> 325          0              NA               NA              0
#> 326          0              NA               NA              0
#> 327         NA              NA               NA             NA
#> 328         NA              NA               NA             NA
#> 329          0              NA               NA              0
#> 330          0              NA               NA              0
#> 331         NA              NA               NA             NA
#> 332          0              NA               NA              0
#> 333         NA              NA               NA             NA
#> 334         NA              NA               NA             NA
#> 335         NA              NA               NA             NA
#> 336         NA              NA               NA             NA
#> 337          0              NA               NA              0
#> 338         NA              NA               NA             NA
#> 339          0              NA               NA              0
#> 340          1               1                1              0
#> 341         NA              NA               NA             NA
#> 342          0              NA               NA              0
#> 343         NA              NA               NA             NA
#> 344         NA              NA               NA             NA
#> 345          0              NA               NA              0
#> 346         NA              NA               NA             NA
#> 347          0              NA               NA              0
#> 348         NA              NA               NA             NA
#> 349          0              NA               NA              0
#> 350         NA              NA               NA             NA
#> 351         NA              NA               NA             NA
#> 352         NA              NA               NA             NA
#> 353         NA              NA               NA             NA
#> 354          0              NA               NA              0
#> 355         NA              NA               NA             NA
#> 356          1               7                1              0
#> 357          0              NA               NA              0
#> 358          0              NA               NA              0
#> 359          0              NA               NA              0
#>     child_mproduct_freq child_mproduct_sweet child_chocolate child_juice
#> 1                    NA                   NA               0           0
#> 2                    NA                   NA               0           0
#> 3                    NA                   NA               0           0
#> 4                    NA                   NA               1           0
#> 5                    NA                   NA               0           0
#> 6                    NA                   NA               1           0
#> 7                    NA                   NA               0           0
#> 8                    NA                   NA               1           0
#> 9                    NA                   NA               0           0
#> 10                   NA                   NA               0           0
#> 11                   NA                   NA               1           0
#> 12                   NA                   NA               1           0
#> 13                   NA                   NA               0           0
#> 14                   NA                   NA               1           0
#> 15                   NA                   NA               1           0
#> 16                   NA                   NA               0           0
#> 17                   NA                   NA               1           0
#> 18                   NA                   NA               0           0
#> 19                   NA                   NA               0           0
#> 20                   NA                   NA               0           0
#> 21                   NA                   NA               0           0
#> 22                   NA                   NA               0           0
#> 23                   NA                   NA               0           0
#> 24                   NA                   NA               0           0
#> 25                   NA                   NA               0           0
#> 26                   NA                   NA               0           0
#> 27                   NA                   NA               0           0
#> 28                   NA                   NA               0           0
#> 29                   NA                   NA               0           0
#> 30                   NA                   NA               0           0
#> 31                   NA                   NA               0           0
#> 32                   NA                   NA               1           0
#> 33                   NA                   NA               1           0
#> 34                   NA                   NA               0           0
#> 35                   NA                   NA               1           0
#> 36                   NA                   NA               1           0
#> 37                   NA                   NA               0           0
#> 38                   NA                   NA               0           0
#> 39                   NA                   NA               1           0
#> 40                   NA                   NA               0           0
#> 41                   NA                   NA               0          NA
#> 42                   NA                   NA               1          NA
#> 43                   NA                   NA               1          NA
#> 44                   NA                   NA               0          NA
#> 45                   NA                   NA               0           0
#> 46                   NA                   NA               0           0
#> 47                   NA                   NA               0           0
#> 48                   NA                   NA               1          NA
#> 49                   NA                   NA               1           1
#> 50                   NA                   NA               1          NA
#> 51                   NA                   NA               0          NA
#> 52                   NA                   NA               1          NA
#> 53                   NA                   NA               0          NA
#> 54                   NA                   NA               1          NA
#> 55                   NA                   NA               1          NA
#> 56                   NA                   NA               0          NA
#> 57                   NA                   NA               0          NA
#> 58                   NA                   NA               0          NA
#> 59                   NA                   NA               1          NA
#> 60                   NA                   NA               0          NA
#> 61                   NA                   NA               0          NA
#> 62                   NA                   NA               1          NA
#> 63                   NA                   NA               1          NA
#> 64                   NA                   NA               0           0
#> 65                   NA                   NA               1           0
#> 66                   NA                   NA               0          NA
#> 67                   NA                   NA               0          NA
#> 68                   NA                   NA               0          NA
#> 69                   NA                   NA               0           0
#> 70                   NA                   NA               1          NA
#> 71                   NA                   NA               0           1
#> 72                   NA                   NA               0          NA
#> 73                   NA                   NA               0           0
#> 74                   NA                   NA               0          NA
#> 75                   NA                   NA               1          NA
#> 76                   NA                   NA               0           0
#> 77                   NA                   NA               0          NA
#> 78                   NA                   NA               1          NA
#> 79                   NA                   NA               1           0
#> 80                   NA                   NA               0          NA
#> 81                   NA                   NA               1          NA
#> 82                   NA                   NA               0          NA
#> 83                   NA                   NA               0          NA
#> 84                   NA                   NA               0          NA
#> 85                   NA                   NA               0          NA
#> 86                   NA                   NA               0          NA
#> 87                   NA                   NA               0          NA
#> 88                   NA                   NA               0          NA
#> 89                   NA                   NA               1           0
#> 90                   NA                   NA               0          NA
#> 91                   NA                   NA               0           0
#> 92                   NA                   NA               0          NA
#> 93                   NA                   NA               0          NA
#> 94                   NA                   NA               1          NA
#> 95                   NA                   NA               0          NA
#> 96                   NA                   NA               1          NA
#> 97                   NA                   NA               1          NA
#> 98                   NA                   NA               1          NA
#> 99                   NA                   NA               0          NA
#> 100                  NA                   NA               0          NA
#> 101                  NA                   NA               0           0
#> 102                  NA                   NA               0          NA
#> 103                  NA                   NA               0          NA
#> 104                  NA                   NA               0          NA
#> 105                  NA                   NA               0          NA
#> 106                  NA                   NA               1          NA
#> 107                  NA                   NA               0          NA
#> 108                  NA                   NA               0          NA
#> 109                  NA                   NA               0          NA
#> 110                  NA                   NA               0          NA
#> 111                  NA                   NA               0           0
#> 112                  NA                   NA               0          NA
#> 113                  NA                   NA               0          NA
#> 114                  NA                   NA               0          NA
#> 115                  NA                   NA               0           0
#> 116                  NA                   NA               0          NA
#> 117                  NA                   NA               0           0
#> 118                  NA                   NA               0           0
#> 119                  NA                   NA               0           0
#> 120                  NA                   NA               0          NA
#> 121                  NA                   NA               0          NA
#> 122                  NA                   NA               1          NA
#> 123                  NA                   NA               1          NA
#> 124                  NA                   NA               0          NA
#> 125                  NA                   NA               1          NA
#> 126                  NA                   NA               0           0
#> 127                  NA                   NA               0          NA
#> 128                  NA                   NA               1          NA
#> 129                  NA                   NA               0           1
#> 130                  NA                   NA               1          NA
#> 131                  NA                   NA               1          NA
#> 132                  NA                   NA               1           1
#> 133                  NA                   NA               1           0
#> 134                  NA                   NA               1           0
#> 135                  NA                   NA               0          NA
#> 136                  NA                   NA               0          NA
#> 137                  NA                   NA               1           0
#> 138                  NA                   NA               1          NA
#> 139                  NA                   NA               0          NA
#> 140                  NA                   NA               1           1
#> 141                  NA                   NA               0           0
#> 142                  NA                   NA               0           0
#> 143                  NA                   NA               0          NA
#> 144                  NA                   NA               0           0
#> 145                  NA                   NA               0          NA
#> 146                  NA                   NA               0           0
#> 147                  NA                   NA               1          NA
#> 148                  NA                   NA               0          NA
#> 149                  NA                   NA               0           0
#> 150                  NA                   NA               0           0
#> 151                  NA                   NA               0          NA
#> 152                  NA                   NA               0          NA
#> 153                  NA                   NA               0           0
#> 154                  NA                   NA               0          NA
#> 155                  NA                   NA               1           1
#> 156                  NA                   NA               1          NA
#> 157                  NA                   NA               0          NA
#> 158                  NA                   NA               0           0
#> 159                  NA                   NA               0          NA
#> 160                  NA                   NA               1           0
#> 161                  NA                   NA               0          NA
#> 162                  NA                   NA               1           0
#> 163                  NA                   NA               0          NA
#> 164                  NA                   NA               0           0
#> 165                  NA                   NA               0           0
#> 166                  NA                   NA               0          NA
#> 167                  NA                   NA               1          NA
#> 168                  NA                   NA               0          NA
#> 169                  NA                   NA               0          NA
#> 170                  NA                   NA               0           0
#> 171                  NA                   NA               0          NA
#> 172                  NA                   NA               1           0
#> 173                  NA                   NA               0           0
#> 174                  NA                   NA               0           1
#> 175                  NA                   NA               0          NA
#> 176                  NA                   NA               0          NA
#> 177                  NA                   NA               0          NA
#> 178                  NA                   NA               1           0
#> 179                  NA                   NA               0          NA
#> 180                  NA                   NA               1          NA
#> 181                  NA                   NA               0          NA
#> 182                  NA                   NA               1          NA
#> 183                  NA                   NA               0          NA
#> 184                  NA                   NA               0           0
#> 185                  NA                   NA               0           0
#> 186                  NA                   NA               1          NA
#> 187                  NA                   NA               0          NA
#> 188                  NA                   NA               1           0
#> 189                  NA                   NA               0          NA
#> 190                  NA                   NA               0           0
#> 191                  NA                   NA               0          NA
#> 192                  NA                   NA               0          NA
#> 193                  NA                   NA               0          NA
#> 194                  NA                   NA               0          NA
#> 195                  NA                   NA               1          NA
#> 196                  NA                   NA               0           0
#> 197                  NA                   NA               1           0
#> 198                  NA                   NA               0          NA
#> 199                  NA                   NA               0          NA
#> 200                  NA                   NA               0          NA
#> 201                  NA                   NA               0          NA
#> 202                  NA                   NA               0          NA
#> 203                  NA                   NA               0          NA
#> 204                  NA                   NA               0          NA
#> 205                  NA                   NA               1          NA
#> 206                  NA                   NA               0          NA
#> 207                  NA                   NA               0          NA
#> 208                  NA                   NA               0          NA
#> 209                  NA                   NA               1          NA
#> 210                  NA                   NA               0          NA
#> 211                  NA                   NA               0          NA
#> 212                  NA                   NA               0           0
#> 213                  NA                   NA               0           1
#> 214                  NA                   NA               0           1
#> 215                  NA                   NA               0          NA
#> 216                  NA                   NA               1           0
#> 217                  NA                   NA               0          NA
#> 218                  NA                   NA               1          NA
#> 219                  NA                   NA               0           0
#> 220                  NA                   NA               0          NA
#> 221                  NA                   NA               0          NA
#> 222                  NA                   NA               0          NA
#> 223                  NA                   NA               1           0
#> 224                  NA                   NA               0          NA
#> 225                  NA                   NA               0          NA
#> 226                  NA                   NA               1          NA
#> 227                  NA                   NA               1           0
#> 228                  NA                   NA               0          NA
#> 229                  NA                   NA               1           0
#> 230                  NA                   NA               0          NA
#> 231                  NA                   NA               0           0
#> 232                  NA                   NA               0          NA
#> 233                  NA                   NA               0          NA
#> 234                  NA                   NA               0           0
#> 235                  NA                   NA               1          NA
#> 236                  NA                   NA               1          NA
#> 237                  NA                   NA               0          NA
#> 238                  NA                   NA               0           0
#> 239                  NA                   NA               0          NA
#> 240                  NA                   NA               1          NA
#> 241                  NA                   NA               0           0
#> 242                  NA                   NA               0           0
#> 243                  NA                   NA               0           0
#> 244                  NA                   NA               0           1
#> 245                  NA                   NA               1          NA
#> 246                  NA                   NA               1           0
#> 247                  NA                   NA               1          NA
#> 248                  NA                   NA               1           0
#> 249                  NA                   NA               0          NA
#> 250                  NA                   NA               1          NA
#> 251                  NA                   NA               1           0
#> 252                  NA                   NA               0          NA
#> 253                  NA                   NA               0           0
#> 254                  NA                   NA               0          NA
#> 255                  NA                   NA               1           0
#> 256                  NA                   NA               0          NA
#> 257                  NA                   NA               1          NA
#> 258                  NA                   NA               0          NA
#> 259                  NA                   NA               0           0
#> 260                  NA                   NA               1           0
#> 261                  NA                   NA               1          NA
#> 262                  NA                   NA               0          NA
#> 263                  NA                   NA               0           1
#> 264                  NA                   NA               0           0
#> 265                  NA                   NA               0          NA
#> 266                  NA                   NA               0           0
#> 267                  NA                   NA               0           0
#> 268                  NA                   NA               0          NA
#> 269                  NA                   NA               1          NA
#> 270                  NA                   NA               0          NA
#> 271                  NA                   NA               1          NA
#> 272                  NA                   NA               0           1
#> 273                  NA                   NA               0           0
#> 274                  NA                   NA               0          NA
#> 275                  NA                   NA               0          NA
#> 276                  NA                   NA               0          NA
#> 277                  NA                   NA               0           0
#> 278                  NA                   NA               0          NA
#> 279                  NA                   NA               0           0
#> 280                  NA                   NA               1           0
#> 281                  NA                   NA               0          NA
#> 282                  NA                   NA               0           0
#> 283                  NA                   NA               0           0
#> 284                  NA                   NA               0           0
#> 285                  NA                   NA               0          NA
#> 286                  NA                   NA               0           0
#> 287                  NA                   NA               0          NA
#> 288                  NA                   NA               0          NA
#> 289                  NA                   NA               0          NA
#> 290                  NA                   NA               0          NA
#> 291                  NA                   NA               1           0
#> 292                  NA                   NA               0          NA
#> 293                  NA                   NA               0           0
#> 294                  NA                   NA               1          NA
#> 295                  NA                   NA               0          NA
#> 296                  NA                   NA               0          NA
#> 297                  NA                   NA               0           0
#> 298                  NA                   NA               0          NA
#> 299                  NA                   NA               0           0
#> 300                  NA                   NA               0           0
#> 301                  NA                   NA               1           0
#> 302                  NA                   NA               0          NA
#> 303                  NA                   NA               0           0
#> 304                  NA                   NA               0           0
#> 305                  NA                   NA               0          NA
#> 306                  NA                   NA               0           0
#> 307                  NA                   NA               0           1
#> 308                  NA                   NA               1           0
#> 309                  NA                   NA               0          NA
#> 310                  NA                   NA               0           0
#> 311                  NA                   NA               0           0
#> 312                  NA                   NA               0          NA
#> 313                  NA                   NA               0           1
#> 314                  NA                   NA               1          NA
#> 315                  NA                   NA               1           0
#> 316                  NA                   NA               0           0
#> 317                  NA                   NA               0           0
#> 318                  NA                   NA               1           0
#> 319                  NA                   NA               1           0
#> 320                  NA                   NA               0          NA
#> 321                  NA                   NA               1          NA
#> 322                  NA                   NA               0          NA
#> 323                  NA                   NA               0          NA
#> 324                  NA                   NA               0           0
#> 325                  NA                   NA               0           1
#> 326                  NA                   NA               0           0
#> 327                  NA                   NA               1          NA
#> 328                  NA                   NA               0          NA
#> 329                  NA                   NA               1           0
#> 330                  NA                   NA               0           0
#> 331                  NA                   NA               0          NA
#> 332                  NA                   NA               1           1
#> 333                  NA                   NA               0          NA
#> 334                  NA                   NA               1          NA
#> 335                  NA                   NA               0          NA
#> 336                  NA                   NA               0          NA
#> 337                  NA                   NA               0           0
#> 338                  NA                   NA               0          NA
#> 339                  NA                   NA               0           0
#> 340                  NA                   NA               1           1
#> 341                  NA                   NA               0          NA
#> 342                  NA                   NA               0           0
#> 343                  NA                   NA               1          NA
#> 344                  NA                   NA               0          NA
#> 345                  NA                   NA               1           0
#> 346                  NA                   NA               0          NA
#> 347                  NA                   NA               0           0
#> 348                  NA                   NA               1          NA
#> 349                  NA                   NA               1           0
#> 350                  NA                   NA               0          NA
#> 351                  NA                   NA               0          NA
#> 352                  NA                   NA               0          NA
#> 353                  NA                   NA               0          NA
#> 354                  NA                   NA               0           0
#> 355                  NA                   NA               1          NA
#> 356                  NA                   NA               1           0
#> 357                  NA                   NA               0           0
#> 358                  NA                   NA               0           0
#> 359                  NA                   NA               0           1
#>     child_soda child_tea child_tea_sweet child_broth child_oth_drink
#> 1            0         0              NA           0               0
#> 2            0         0              NA           0               0
#> 3            0         0              NA           0               0
#> 4            0         0              NA           0               0
#> 5            0         0              NA           0               0
#> 6            0         0              NA           0               1
#> 7            0         0              NA           0               0
#> 8            0         0              NA           0               0
#> 9            1         0              NA           0               1
#> 10           0         0              NA           0               0
#> 11           0         0              NA           0               0
#> 12           0         0              NA           0               0
#> 13           0         0              NA           0               0
#> 14           0         0              NA           0               0
#> 15           1         0              NA           0               1
#> 16           0         0              NA           0               0
#> 17           0         0              NA           0               0
#> 18           0         0              NA           0               1
#> 19           0         0              NA           0               1
#> 20           1         0              NA           0               1
#> 21           1         0              NA           0               1
#> 22           0         0              NA           0               0
#> 23           1         0              NA           0               0
#> 24           1         0              NA           0               1
#> 25           1         0              NA           0               1
#> 26           0         0              NA           0               0
#> 27           0         0              NA           0               0
#> 28           0         0              NA           0               0
#> 29           1         0              NA           0               0
#> 30           0         0              NA           0               0
#> 31           0         0              NA           0               1
#> 32           0         0              NA           0               1
#> 33           0         0              NA           0               0
#> 34           1         0              NA           0               1
#> 35           0         0              NA           0               1
#> 36           0         0              NA           0               0
#> 37           0         0              NA           0               0
#> 38           0         0              NA           0               0
#> 39           1         0              NA           0               1
#> 40           1         0              NA           0               0
#> 41           0        NA              NA          NA               1
#> 42           0        NA              NA          NA               1
#> 43           1        NA              NA          NA               1
#> 44           0        NA              NA          NA               0
#> 45           1         0              NA           0               0
#> 46           0         0              NA           0               0
#> 47           0         0              NA           0               0
#> 48           0        NA              NA          NA               0
#> 49           1         0              NA           0               0
#> 50           1        NA              NA          NA               0
#> 51           0        NA              NA          NA               0
#> 52           1        NA              NA          NA               1
#> 53           1        NA              NA          NA               0
#> 54           1        NA              NA          NA               0
#> 55           1        NA              NA          NA               0
#> 56           0        NA              NA          NA               0
#> 57           0        NA              NA          NA               1
#> 58           1        NA              NA          NA               1
#> 59           0        NA              NA          NA               0
#> 60           0        NA              NA          NA               0
#> 61           0        NA              NA          NA               0
#> 62           0        NA              NA          NA               0
#> 63           0        NA              NA          NA               0
#> 64           0         0              NA           0               0
#> 65           0         1               1           1               0
#> 66           0        NA              NA          NA               0
#> 67           0        NA              NA          NA               1
#> 68           0        NA              NA          NA               1
#> 69           0         0              NA           0               0
#> 70           1        NA              NA          NA               0
#> 71           0         0              NA           1               0
#> 72           0        NA              NA          NA               1
#> 73           0         0              NA           0               0
#> 74           0        NA              NA          NA               1
#> 75           0        NA              NA          NA               1
#> 76           1         0              NA           0               0
#> 77           1        NA              NA          NA               1
#> 78           1        NA              NA          NA               0
#> 79           0         0              NA           0               0
#> 80           1        NA              NA          NA               1
#> 81           1        NA              NA          NA               0
#> 82           0        NA              NA          NA               1
#> 83           0        NA              NA          NA               0
#> 84           0        NA              NA          NA               0
#> 85           0        NA              NA          NA               0
#> 86           0        NA              NA          NA               0
#> 87           1        NA              NA          NA               0
#> 88           0        NA              NA          NA               1
#> 89           0         1               1           0               1
#> 90           1        NA              NA          NA               0
#> 91           1         0              NA           0               1
#> 92           1        NA              NA          NA               0
#> 93           0        NA              NA          NA               1
#> 94           0        NA              NA          NA               1
#> 95           0        NA              NA          NA               0
#> 96           0        NA              NA          NA               0
#> 97           0        NA              NA          NA               0
#> 98           0        NA              NA          NA               1
#> 99           0        NA              NA          NA               1
#> 100          0        NA              NA          NA               1
#> 101          0         0              NA           0               1
#> 102          0        NA              NA          NA               1
#> 103          0        NA              NA          NA               0
#> 104          0        NA              NA          NA               0
#> 105          0        NA              NA          NA               0
#> 106          0        NA              NA          NA               0
#> 107          0        NA              NA          NA               1
#> 108          0        NA              NA          NA               0
#> 109          1        NA              NA          NA               1
#> 110          0        NA              NA          NA               1
#> 111          0         0              NA           0               0
#> 112          1        NA              NA          NA               0
#> 113          0        NA              NA          NA               0
#> 114          0        NA              NA          NA               0
#> 115          0         0              NA           0               0
#> 116          0        NA              NA          NA               0
#> 117          0         0              NA           0               0
#> 118          0         0              NA           0               1
#> 119          0         0              NA           0               0
#> 120          0        NA              NA          NA               0
#> 121          1        NA              NA          NA               0
#> 122          0        NA              NA          NA               0
#> 123          1        NA              NA          NA               1
#> 124          0        NA              NA          NA               0
#> 125          0        NA              NA          NA               0
#> 126          1         0              NA           0               0
#> 127          0        NA              NA          NA               0
#> 128          1        NA              NA          NA               0
#> 129          0         1               1           0               0
#> 130          0        NA              NA          NA               1
#> 131          1        NA              NA          NA               1
#> 132          0         0              NA           0               0
#> 133          0         0              NA           0               0
#> 134          0         0              NA           1               0
#> 135          0        NA              NA          NA               0
#> 136          0        NA              NA          NA               1
#> 137          0         0              NA           0               0
#> 138          0        NA              NA          NA               0
#> 139          0        NA              NA          NA               0
#> 140          1         0              NA           0               0
#> 141          0         0              NA           1               0
#> 142          1         0              NA           0               0
#> 143          1        NA              NA          NA               0
#> 144          0         0              NA           0               0
#> 145          0        NA              NA          NA               0
#> 146          0         1               1           0               0
#> 147          0        NA              NA          NA               1
#> 148          0        NA              NA          NA               0
#> 149          0         1               1           0               0
#> 150          0         0              NA           0               0
#> 151          0        NA              NA          NA               0
#> 152          0        NA              NA          NA               1
#> 153          0         0              NA           0               0
#> 154          1        NA              NA          NA               1
#> 155          0         0              NA           0               0
#> 156          0        NA              NA          NA               1
#> 157          0        NA              NA          NA               0
#> 158          0         0              NA           0               1
#> 159          0        NA              NA          NA               0
#> 160          0         0              NA           0               0
#> 161          1        NA              NA          NA               1
#> 162          0         0              NA           0               0
#> 163          0        NA              NA          NA               0
#> 164          0         0              NA           0               0
#> 165          0         0              NA           0               0
#> 166          0        NA              NA          NA               0
#> 167          0        NA              NA          NA               0
#> 168          0        NA              NA          NA               0
#> 169          1        NA              NA          NA               0
#> 170          0         0              NA           0               0
#> 171          1        NA              NA          NA               0
#> 172          0         0              NA           0               1
#> 173          1         0              NA           0               0
#> 174          0         0              NA           0               1
#> 175          0        NA              NA          NA               0
#> 176          0        NA              NA          NA               0
#> 177          0        NA              NA          NA               0
#> 178          1         0              NA           0               0
#> 179          0        NA              NA          NA               0
#> 180          1        NA              NA          NA               0
#> 181          0        NA              NA          NA               0
#> 182          0        NA              NA          NA               0
#> 183          0        NA              NA          NA               0
#> 184          0         0              NA           0               0
#> 185          1         0              NA           0               0
#> 186          0        NA              NA          NA               0
#> 187          0        NA              NA          NA               0
#> 188          0         0              NA           0               0
#> 189          0        NA              NA          NA               1
#> 190          0         0              NA           0               1
#> 191          0        NA              NA          NA               0
#> 192          0        NA              NA          NA               1
#> 193          0        NA              NA          NA               1
#> 194          0        NA              NA          NA               0
#> 195          0        NA              NA          NA               0
#> 196          1         0              NA           0               0
#> 197          0         0              NA           0               0
#> 198          0        NA              NA          NA               0
#> 199          0        NA              NA          NA               1
#> 200          1        NA              NA          NA               0
#> 201          1        NA              NA          NA               1
#> 202          0        NA              NA          NA               0
#> 203          0        NA              NA          NA               1
#> 204          0        NA              NA          NA               0
#> 205          0        NA              NA          NA               1
#> 206          1        NA              NA          NA               1
#> 207          1        NA              NA          NA               1
#> 208          0        NA              NA          NA               1
#> 209          0        NA              NA          NA               0
#> 210          0        NA              NA          NA               0
#> 211          1        NA              NA          NA               0
#> 212          0         0              NA           0               0
#> 213          0         1               1           0               0
#> 214          0         1               1           1               1
#> 215          0        NA              NA          NA               1
#> 216          1         0              NA           0               1
#> 217          1        NA              NA          NA               0
#> 218          0        NA              NA          NA               0
#> 219          1         0              NA           0               0
#> 220          0        NA              NA          NA               1
#> 221          0        NA              NA          NA               1
#> 222          1        NA              NA          NA               1
#> 223          1         1               1           0               0
#> 224          0        NA              NA          NA               1
#> 225          1        NA              NA          NA               0
#> 226          0        NA              NA          NA               0
#> 227          0         0              NA           0               0
#> 228          0        NA              NA          NA               0
#> 229          0         0              NA           0               0
#> 230          1        NA              NA          NA               0
#> 231          1         0              NA           1               0
#> 232          0        NA              NA          NA               0
#> 233          1        NA              NA          NA               0
#> 234          0         0              NA           0               1
#> 235          1        NA              NA          NA               0
#> 236          0        NA              NA          NA               0
#> 237          0        NA              NA          NA               0
#> 238          0         0              NA           0               1
#> 239          1        NA              NA          NA               1
#> 240          0        NA              NA          NA               0
#> 241          1         0              NA           0               0
#> 242          1         0              NA           0               1
#> 243          0         0              NA           0               0
#> 244          0         0              NA           1               0
#> 245          0        NA              NA          NA               0
#> 246          0         0              NA           0               0
#> 247          0        NA              NA          NA               0
#> 248          0         0              NA           0               0
#> 249          1        NA              NA          NA               0
#> 250          0        NA              NA          NA               0
#> 251          1         0              NA           0               0
#> 252          0        NA              NA          NA               0
#> 253          0         0              NA           0               0
#> 254          1        NA              NA          NA               0
#> 255          0         0              NA           0               1
#> 256          0        NA              NA          NA               1
#> 257          0        NA              NA          NA               0
#> 258          1        NA              NA          NA               0
#> 259          0         0              NA           0               0
#> 260          0         0              NA           0               1
#> 261          0        NA              NA          NA               0
#> 262          0        NA              NA          NA               0
#> 263          0         0              NA           0               0
#> 264          1         0              NA           0               1
#> 265          0        NA              NA          NA               0
#> 266          0         0              NA           0               0
#> 267          0         0              NA           0               0
#> 268          1        NA              NA          NA               0
#> 269          0        NA              NA          NA               0
#> 270          0        NA              NA          NA               1
#> 271          1        NA              NA          NA               1
#> 272          1         0              NA           0               0
#> 273          0         0              NA           0               1
#> 274          1        NA              NA          NA               0
#> 275          1        NA              NA          NA               0
#> 276          1        NA              NA          NA               0
#> 277          0         0              NA           0               0
#> 278          0        NA              NA          NA               0
#> 279          0         0              NA           0               0
#> 280          0         0              NA           0               0
#> 281          1        NA              NA          NA               0
#> 282          0         0              NA           0               0
#> 283          0         0              NA           0               0
#> 284          1         0              NA           0               0
#> 285          1        NA              NA          NA               0
#> 286          1         0              NA           0               1
#> 287          1        NA              NA          NA               0
#> 288          0        NA              NA          NA               0
#> 289          1        NA              NA          NA               0
#> 290          1        NA              NA          NA               0
#> 291          1         0              NA           0               1
#> 292          1        NA              NA          NA               0
#> 293          0         1               1           0               1
#> 294          0        NA              NA          NA               0
#> 295          0        NA              NA          NA               0
#> 296          1        NA              NA          NA               1
#> 297          1         0              NA           0               0
#> 298          0        NA              NA          NA               0
#> 299          1         0              NA           0               1
#> 300          1         0              NA           0               0
#> 301          0         0              NA           0               1
#> 302          1        NA              NA          NA               0
#> 303          0         0              NA           0               0
#> 304          0         0              NA           1               0
#> 305          1        NA              NA          NA               0
#> 306          0         0              NA           0               1
#> 307          1         0              NA           0               0
#> 308          0         0              NA           0               1
#> 309          1        NA              NA          NA               0
#> 310          0         0              NA           1               0
#> 311          0         0              NA           0               0
#> 312          0        NA              NA          NA               0
#> 313          0         0              NA           0               0
#> 314          1        NA              NA          NA               0
#> 315          0         0              NA           0               1
#> 316          0         0              NA           0               0
#> 317          0         0              NA           0               1
#> 318          0         0              NA           0               0
#> 319          0         0              NA           0               0
#> 320          1        NA              NA          NA               0
#> 321          1        NA              NA          NA               0
#> 322          1        NA              NA          NA               0
#> 323          0        NA              NA          NA               0
#> 324          0         0              NA           0               0
#> 325          0         0              NA           0               1
#> 326          0         0              NA           0               0
#> 327          0        NA              NA          NA               1
#> 328          0        NA              NA          NA               0
#> 329          0         0              NA           0               0
#> 330          0         0              NA           0               0
#> 331          0        NA              NA          NA               0
#> 332          0         0              NA           0               0
#> 333          0        NA              NA          NA               0
#> 334          0        NA              NA          NA               1
#> 335          0        NA              NA          NA               0
#> 336          0        NA              NA          NA               0
#> 337          1         0              NA           0               1
#> 338          0        NA              NA          NA               0
#> 339          0         0              NA           0               0
#> 340          0         0              NA           0               0
#> 341          0        NA              NA          NA               1
#> 342          0         0              NA           0               1
#> 343          1        NA              NA          NA               1
#> 344          0        NA              NA          NA               1
#> 345          0         0              NA           0               1
#> 346          0        NA              NA          NA               1
#> 347          0         0              NA           0               0
#> 348          1        NA              NA          NA               0
#> 349          0         0              NA           0               0
#> 350          0        NA              NA          NA               0
#> 351          0        NA              NA          NA               0
#> 352          0        NA              NA          NA               0
#> 353          1        NA              NA          NA               1
#> 354          0         0              NA           0               0
#> 355          0        NA              NA          NA               0
#> 356          0         0              NA           0               0
#> 357          0         0              NA           0               1
#> 358          1         0              NA           1               0
#> 359          0         0              NA           0               0
#>     child_oth_drink_sweet child_yogurt child_yogurt_frq child_rice
#> 1                      NA            0               NA          0
#> 2                      NA            0               NA          0
#> 3                      NA            0               NA          0
#> 4                      NA            0               NA          1
#> 5                      NA            0               NA          0
#> 6                       1            0               NA          0
#> 7                      NA            0               NA          0
#> 8                      NA            0               NA          0
#> 9                       1            0               NA          1
#> 10                     NA            0               NA          1
#> 11                     NA            0               NA          0
#> 12                     NA            0               NA          0
#> 13                     NA            0               NA          0
#> 14                     NA            0               NA          0
#> 15                      1            0               NA          0
#> 16                     NA            0               NA          0
#> 17                     NA            0               NA          0
#> 18                      1            0               NA          1
#> 19                      1            0               NA          0
#> 20                      1            0               NA          0
#> 21                      1            0               NA          0
#> 22                     NA            0               NA          0
#> 23                     NA            0               NA          0
#> 24                      1            0               NA          0
#> 25                      1            0               NA          0
#> 26                     NA            0               NA          0
#> 27                     NA            0               NA          0
#> 28                     NA            0               NA          1
#> 29                     NA            1                1          1
#> 30                     NA            0               NA          1
#> 31                      1            0               NA          1
#> 32                      1            0               NA          1
#> 33                     NA            0               NA          0
#> 34                      1            0               NA          0
#> 35                      1            0               NA          0
#> 36                     NA            0               NA          0
#> 37                     NA            0               NA          1
#> 38                     NA            0               NA          0
#> 39                      1            0               NA          0
#> 40                     NA            0               NA          0
#> 41                      1           NA               NA         NA
#> 42                      1           NA               NA         NA
#> 43                      1           NA               NA         NA
#> 44                     NA           NA               NA         NA
#> 45                     NA            0               NA          1
#> 46                     NA            0               NA          1
#> 47                     NA            0               NA          0
#> 48                     NA           NA               NA         NA
#> 49                     NA            1                1          1
#> 50                     NA           NA               NA         NA
#> 51                     NA           NA               NA         NA
#> 52                      1           NA               NA         NA
#> 53                     NA           NA               NA         NA
#> 54                     NA           NA               NA         NA
#> 55                     NA           NA               NA         NA
#> 56                     NA           NA               NA         NA
#> 57                      1           NA               NA         NA
#> 58                      1           NA               NA         NA
#> 59                     NA           NA               NA         NA
#> 60                     NA           NA               NA         NA
#> 61                     NA           NA               NA         NA
#> 62                     NA           NA               NA         NA
#> 63                     NA           NA               NA         NA
#> 64                     NA            0               NA          1
#> 65                     NA            0               NA          1
#> 66                     NA           NA               NA         NA
#> 67                      1           NA               NA         NA
#> 68                      1           NA               NA         NA
#> 69                     NA            0               NA          1
#> 70                     NA           NA               NA         NA
#> 71                     NA            0               NA          1
#> 72                      1           NA               NA         NA
#> 73                     NA            0               NA          1
#> 74                      1           NA               NA         NA
#> 75                      1           NA               NA         NA
#> 76                     NA            0               NA          1
#> 77                      1           NA               NA         NA
#> 78                     NA           NA               NA         NA
#> 79                     NA            0               NA          1
#> 80                      1           NA               NA         NA
#> 81                     NA           NA               NA         NA
#> 82                      1           NA               NA         NA
#> 83                     NA           NA               NA         NA
#> 84                     NA           NA               NA         NA
#> 85                     NA           NA               NA         NA
#> 86                     NA           NA               NA         NA
#> 87                     NA           NA               NA         NA
#> 88                      1           NA               NA         NA
#> 89                      1            0               NA          1
#> 90                     NA           NA               NA         NA
#> 91                      1            0               NA          1
#> 92                     NA           NA               NA         NA
#> 93                      1           NA               NA         NA
#> 94                      1           NA               NA         NA
#> 95                     NA           NA               NA         NA
#> 96                     NA           NA               NA         NA
#> 97                     NA           NA               NA         NA
#> 98                      1           NA               NA         NA
#> 99                      1           NA               NA         NA
#> 100                     1           NA               NA         NA
#> 101                     1            0               NA          1
#> 102                     1           NA               NA         NA
#> 103                    NA           NA               NA         NA
#> 104                    NA           NA               NA         NA
#> 105                    NA           NA               NA         NA
#> 106                    NA           NA               NA         NA
#> 107                     1           NA               NA         NA
#> 108                    NA           NA               NA         NA
#> 109                     1           NA               NA         NA
#> 110                     1           NA               NA         NA
#> 111                    NA            0               NA          0
#> 112                    NA           NA               NA         NA
#> 113                    NA           NA               NA         NA
#> 114                    NA           NA               NA         NA
#> 115                    NA            0               NA          1
#> 116                    NA           NA               NA         NA
#> 117                    NA            0               NA          1
#> 118                     1            0               NA        999
#> 119                    NA            0               NA          1
#> 120                    NA           NA               NA         NA
#> 121                    NA           NA               NA         NA
#> 122                    NA           NA               NA         NA
#> 123                     1           NA               NA         NA
#> 124                    NA           NA               NA         NA
#> 125                    NA           NA               NA         NA
#> 126                    NA            0               NA          1
#> 127                    NA           NA               NA         NA
#> 128                    NA           NA               NA         NA
#> 129                    NA            0               NA          1
#> 130                     1           NA               NA         NA
#> 131                     1           NA               NA         NA
#> 132                    NA            0               NA          1
#> 133                    NA            0               NA          1
#> 134                    NA            0               NA          1
#> 135                    NA           NA               NA         NA
#> 136                     1           NA               NA         NA
#> 137                    NA            0               NA          1
#> 138                    NA           NA               NA         NA
#> 139                    NA           NA               NA         NA
#> 140                    NA            0               NA          1
#> 141                    NA            0               NA          1
#> 142                    NA            0               NA          1
#> 143                    NA           NA               NA         NA
#> 144                    NA            0               NA          1
#> 145                    NA           NA               NA         NA
#> 146                    NA            0               NA          1
#> 147                     1           NA               NA         NA
#> 148                    NA           NA               NA         NA
#> 149                    NA            1                1          1
#> 150                    NA            0               NA          1
#> 151                    NA           NA               NA         NA
#> 152                     1           NA               NA         NA
#> 153                    NA            0               NA          0
#> 154                     1           NA               NA         NA
#> 155                    NA            0               NA          1
#> 156                     1           NA               NA         NA
#> 157                    NA           NA               NA         NA
#> 158                     1            0               NA          1
#> 159                    NA           NA               NA         NA
#> 160                    NA            0               NA          1
#> 161                     1           NA               NA         NA
#> 162                    NA            0               NA          1
#> 163                    NA           NA               NA         NA
#> 164                    NA            0               NA          1
#> 165                    NA            0               NA          1
#> 166                    NA           NA               NA         NA
#> 167                    NA           NA               NA         NA
#> 168                    NA           NA               NA         NA
#> 169                    NA           NA               NA         NA
#> 170                    NA            0               NA          1
#> 171                    NA           NA               NA         NA
#> 172                     1            0               NA          1
#> 173                    NA            0               NA          1
#> 174                     1            0               NA          1
#> 175                    NA           NA               NA         NA
#> 176                    NA           NA               NA         NA
#> 177                    NA           NA               NA         NA
#> 178                    NA            0               NA          1
#> 179                    NA           NA               NA         NA
#> 180                    NA           NA               NA         NA
#> 181                    NA           NA               NA         NA
#> 182                    NA           NA               NA         NA
#> 183                    NA           NA               NA         NA
#> 184                    NA            0               NA          1
#> 185                    NA            0               NA          1
#> 186                    NA           NA               NA         NA
#> 187                    NA           NA               NA         NA
#> 188                    NA            0               NA          1
#> 189                     1           NA               NA         NA
#> 190                     1            0               NA          1
#> 191                    NA           NA               NA         NA
#> 192                     1           NA               NA         NA
#> 193                     1           NA               NA         NA
#> 194                    NA           NA               NA         NA
#> 195                    NA           NA               NA         NA
#> 196                    NA            0               NA          1
#> 197                    NA            1                1          1
#> 198                    NA           NA               NA         NA
#> 199                     1           NA               NA         NA
#> 200                    NA           NA               NA         NA
#> 201                     1           NA               NA         NA
#> 202                    NA           NA               NA         NA
#> 203                     1           NA               NA         NA
#> 204                    NA           NA               NA         NA
#> 205                     1           NA               NA         NA
#> 206                     1           NA               NA         NA
#> 207                     1           NA               NA         NA
#> 208                     1           NA               NA         NA
#> 209                    NA           NA               NA         NA
#> 210                    NA           NA               NA         NA
#> 211                    NA           NA               NA         NA
#> 212                    NA            0               NA          1
#> 213                    NA            0               NA          1
#> 214                     1            0               NA          1
#> 215                     1           NA               NA         NA
#> 216                     1            0               NA          1
#> 217                    NA           NA               NA         NA
#> 218                    NA           NA               NA         NA
#> 219                    NA            0               NA          1
#> 220                     1           NA               NA         NA
#> 221                     1           NA               NA         NA
#> 222                     1           NA               NA         NA
#> 223                    NA            0               NA          1
#> 224                     1           NA               NA         NA
#> 225                    NA           NA               NA         NA
#> 226                    NA           NA               NA         NA
#> 227                    NA            0               NA          1
#> 228                    NA           NA               NA         NA
#> 229                    NA            0               NA          1
#> 230                    NA           NA               NA         NA
#> 231                    NA            0               NA          1
#> 232                    NA           NA               NA         NA
#> 233                    NA           NA               NA         NA
#> 234                     1            0               NA          1
#> 235                    NA           NA               NA         NA
#> 236                    NA           NA               NA         NA
#> 237                    NA           NA               NA         NA
#> 238                     1            0               NA          1
#> 239                     1           NA               NA         NA
#> 240                    NA           NA               NA         NA
#> 241                    NA            0               NA          1
#> 242                     1            0               NA          1
#> 243                    NA            0               NA          1
#> 244                    NA            0               NA          1
#> 245                    NA           NA               NA         NA
#> 246                    NA            0               NA          1
#> 247                    NA           NA               NA         NA
#> 248                    NA            0               NA          1
#> 249                    NA           NA               NA         NA
#> 250                    NA           NA               NA         NA
#> 251                    NA            0               NA          1
#> 252                    NA           NA               NA         NA
#> 253                    NA            0               NA          1
#> 254                    NA           NA               NA         NA
#> 255                     1            0               NA          1
#> 256                     1           NA               NA         NA
#> 257                    NA           NA               NA         NA
#> 258                    NA           NA               NA         NA
#> 259                    NA            0               NA          0
#> 260                     1            1                1          1
#> 261                    NA           NA               NA         NA
#> 262                    NA           NA               NA         NA
#> 263                    NA            0               NA          1
#> 264                     1            0               NA          1
#> 265                    NA           NA               NA         NA
#> 266                    NA            0               NA          1
#> 267                    NA            0               NA          1
#> 268                    NA           NA               NA         NA
#> 269                    NA           NA               NA         NA
#> 270                     1           NA               NA         NA
#> 271                     1           NA               NA         NA
#> 272                    NA            0               NA          1
#> 273                     1            0               NA          1
#> 274                    NA           NA               NA         NA
#> 275                    NA           NA               NA         NA
#> 276                    NA           NA               NA         NA
#> 277                    NA            0               NA          1
#> 278                    NA           NA               NA         NA
#> 279                    NA            0               NA          1
#> 280                    NA            0               NA          1
#> 281                    NA           NA               NA         NA
#> 282                    NA            0               NA          1
#> 283                    NA            0               NA          1
#> 284                    NA            0               NA          1
#> 285                    NA           NA               NA         NA
#> 286                     1            0               NA          1
#> 287                    NA           NA               NA         NA
#> 288                    NA           NA               NA         NA
#> 289                    NA           NA               NA         NA
#> 290                    NA           NA               NA         NA
#> 291                     1            0               NA          1
#> 292                    NA           NA               NA         NA
#> 293                     1            0               NA          1
#> 294                    NA           NA               NA         NA
#> 295                    NA           NA               NA         NA
#> 296                     1           NA               NA         NA
#> 297                    NA            0               NA          1
#> 298                    NA           NA               NA         NA
#> 299                     1            0               NA          1
#> 300                    NA          999                1          1
#> 301                     1          999                1          1
#> 302                    NA           NA               NA         NA
#> 303                    NA            0               NA          1
#> 304                    NA            0               NA          1
#> 305                    NA           NA               NA         NA
#> 306                     1            0               NA          1
#> 307                    NA            0               NA          1
#> 308                     1            0               NA          1
#> 309                    NA           NA               NA         NA
#> 310                    NA            0               NA          1
#> 311                    NA            0               NA          0
#> 312                    NA           NA               NA         NA
#> 313                    NA            0               NA          1
#> 314                    NA           NA               NA         NA
#> 315                     1            0               NA          1
#> 316                    NA            0               NA          1
#> 317                     1            0               NA          1
#> 318                    NA            0               NA          1
#> 319                    NA            0               NA          1
#> 320                    NA           NA               NA         NA
#> 321                    NA           NA               NA         NA
#> 322                    NA           NA               NA         NA
#> 323                    NA           NA               NA         NA
#> 324                    NA            0               NA          1
#> 325                     1            0               NA          1
#> 326                    NA            0               NA          0
#> 327                     1           NA               NA         NA
#> 328                    NA           NA               NA         NA
#> 329                    NA            0               NA          1
#> 330                    NA            0               NA          1
#> 331                    NA           NA               NA         NA
#> 332                    NA            0               NA          1
#> 333                    NA           NA               NA         NA
#> 334                     1           NA               NA         NA
#> 335                    NA           NA               NA         NA
#> 336                    NA           NA               NA         NA
#> 337                     1            0               NA          1
#> 338                    NA           NA               NA         NA
#> 339                    NA            0               NA          1
#> 340                    NA            0               NA          1
#> 341                     1           NA               NA         NA
#> 342                     1            0               NA          1
#> 343                     1           NA               NA         NA
#> 344                     1           NA               NA         NA
#> 345                     1            0               NA          1
#> 346                     1           NA               NA         NA
#> 347                    NA            0               NA          1
#> 348                    NA           NA               NA         NA
#> 349                    NA            0               NA          1
#> 350                    NA           NA               NA         NA
#> 351                    NA           NA               NA         NA
#> 352                    NA           NA               NA         NA
#> 353                     1           NA               NA         NA
#> 354                    NA            0               NA          1
#> 355                    NA           NA               NA         NA
#> 356                    NA            0               NA          1
#> 357                     1            0               NA          1
#> 358                    NA            0               NA          1
#> 359                    NA            0               NA          1
#>     child_pumpkin child_potatoes child_leafyveg child_oth_veg child_mango
#> 1               0              0              0             1           0
#> 2               0              0              0             0           0
#> 3               0              0              0             0           0
#> 4               0              0              0             0           0
#> 5               0              0              0             0           0
#> 6               0              0              0             1           0
#> 7               0              0              0             0           0
#> 8               0              0              0             0           0
#> 9               0              0              0             0           0
#> 10              0              0              0             0           0
#> 11              0              0              0             0           0
#> 12              0              0              0             0           0
#> 13              0              0              0             1           0
#> 14              0              0              0             0           0
#> 15              0              0              0             0           0
#> 16              0              0              0             1           0
#> 17              0              0              0             0           0
#> 18              0              0              0             0           0
#> 19              0              0              0             0           0
#> 20              0              0              0             0           0
#> 21              0              0              0             0           0
#> 22              0              0              0             0           0
#> 23              0              0              0             1           0
#> 24              0              0              0             0           0
#> 25              0              0              0             0           0
#> 26              0              0              0             0           0
#> 27              0              0              0             0           0
#> 28              0              0              0             1           0
#> 29              0              0              0             1           0
#> 30              0              0              0             1           0
#> 31              0              0              0             0           0
#> 32              0              0              0             0           0
#> 33              0              0              0             0           0
#> 34              0              0              0             1           0
#> 35              0              0              0             0           0
#> 36              0              0              0             0           0
#> 37              0              1              0             0           0
#> 38              0              0              0             0           0
#> 39              0              0              0             0           0
#> 40              0              0              0             0           0
#> 41             NA             NA             NA             0          NA
#> 42             NA             NA             NA             1          NA
#> 43             NA             NA             NA             0          NA
#> 44             NA             NA             NA             0          NA
#> 45              0              0              1             0           0
#> 46              0              0              0             0           0
#> 47              0              0              0             0           0
#> 48             NA             NA             NA             1          NA
#> 49              0              1              0             0           0
#> 50             NA             NA             NA             1          NA
#> 51             NA             NA             NA             1          NA
#> 52             NA             NA             NA             0          NA
#> 53             NA             NA             NA             0          NA
#> 54             NA             NA             NA             0          NA
#> 55             NA             NA             NA             0          NA
#> 56             NA             NA             NA             0          NA
#> 57             NA             NA             NA             0          NA
#> 58             NA             NA             NA             0          NA
#> 59             NA             NA             NA             1          NA
#> 60             NA             NA             NA             0          NA
#> 61             NA             NA             NA             1          NA
#> 62             NA             NA             NA             0          NA
#> 63             NA             NA             NA             1          NA
#> 64              0              0              0             0           0
#> 65              0              0              0             1           0
#> 66             NA             NA             NA             0          NA
#> 67             NA             NA             NA             0          NA
#> 68             NA             NA             NA             0          NA
#> 69              0              1              0             0           1
#> 70             NA             NA             NA             0          NA
#> 71              0              0              0             0           1
#> 72             NA             NA             NA             0          NA
#> 73              0              0              1             1           0
#> 74             NA             NA             NA             1          NA
#> 75             NA             NA             NA             0          NA
#> 76              0              0              0             1           0
#> 77             NA             NA             NA             1          NA
#> 78             NA             NA             NA             0          NA
#> 79              0              0              0             0           0
#> 80             NA             NA             NA             0          NA
#> 81             NA             NA             NA             0          NA
#> 82             NA             NA             NA             0          NA
#> 83             NA             NA             NA             1          NA
#> 84             NA             NA             NA             0          NA
#> 85             NA             NA             NA             0          NA
#> 86             NA             NA             NA             0          NA
#> 87             NA             NA             NA             0          NA
#> 88             NA             NA             NA             0          NA
#> 89              0              0              0             1           0
#> 90             NA             NA             NA             0          NA
#> 91              0              0              0             1           0
#> 92             NA             NA             NA             0          NA
#> 93             NA             NA             NA             0          NA
#> 94             NA             NA             NA             0          NA
#> 95             NA             NA             NA             0          NA
#> 96             NA             NA             NA             0          NA
#> 97             NA             NA             NA             0          NA
#> 98             NA             NA             NA             1          NA
#> 99             NA             NA             NA             0          NA
#> 100            NA             NA             NA             0          NA
#> 101             0              0              0             1           0
#> 102            NA             NA             NA             0          NA
#> 103            NA             NA             NA             0          NA
#> 104            NA             NA             NA             0          NA
#> 105            NA             NA             NA             1          NA
#> 106            NA             NA             NA             1          NA
#> 107            NA             NA             NA             0          NA
#> 108            NA             NA             NA             0          NA
#> 109            NA             NA             NA             0          NA
#> 110            NA             NA             NA             0          NA
#> 111             0              0              0             0           0
#> 112            NA             NA             NA             1          NA
#> 113            NA             NA             NA             0          NA
#> 114            NA             NA             NA             1          NA
#> 115             0              0              0             0           0
#> 116            NA             NA             NA             0          NA
#> 117             0              0              0             0           0
#> 118             0              1              0             0           0
#> 119             0              0              0             0           0
#> 120            NA             NA             NA             0          NA
#> 121            NA             NA             NA             1          NA
#> 122            NA             NA             NA             1          NA
#> 123            NA             NA             NA             0          NA
#> 124            NA             NA             NA             1          NA
#> 125            NA             NA             NA             0          NA
#> 126             0              0              1             1           0
#> 127            NA             NA             NA             1          NA
#> 128            NA             NA             NA             1          NA
#> 129             0              0              1             1           0
#> 130            NA             NA             NA             1          NA
#> 131            NA             NA             NA             0          NA
#> 132             0              0              1             0           1
#> 133             0              1              1             0           1
#> 134             0              0              1             0           1
#> 135            NA             NA             NA             0          NA
#> 136            NA             NA             NA             1          NA
#> 137             0              0              1             1           1
#> 138            NA             NA             NA             0          NA
#> 139            NA             NA             NA             0          NA
#> 140             0              0              1             0           0
#> 141             0              0              1             0           1
#> 142             0              0              1             0           0
#> 143            NA             NA             NA             0          NA
#> 144             0              1              1             0           1
#> 145            NA             NA             NA             1          NA
#> 146             0              1              0             0           0
#> 147            NA             NA             NA             0          NA
#> 148            NA             NA             NA             0          NA
#> 149             0              0              0             0           0
#> 150             0              0              0             0           0
#> 151            NA             NA             NA             0          NA
#> 152            NA             NA             NA             1          NA
#> 153             0              0              0             0           0
#> 154            NA             NA             NA             1          NA
#> 155             0              0              0             0           0
#> 156            NA             NA             NA             0          NA
#> 157            NA             NA             NA             0          NA
#> 158             0              0              1             0           0
#> 159            NA             NA             NA             1          NA
#> 160             1              1              0             0           0
#> 161            NA             NA             NA             0          NA
#> 162             0              0              1             1           0
#> 163            NA             NA             NA             0          NA
#> 164             0              0              0             0           0
#> 165             0              1              1             1           0
#> 166            NA             NA             NA             0          NA
#> 167            NA             NA             NA             0          NA
#> 168            NA             NA             NA             0          NA
#> 169            NA             NA             NA             0          NA
#> 170             0              0              0             0           1
#> 171            NA             NA             NA             0          NA
#> 172             0              0              1             0           0
#> 173             0              1              0             0           0
#> 174             0              0              1             1           0
#> 175            NA             NA             NA             1          NA
#> 176            NA             NA             NA             1          NA
#> 177            NA             NA             NA             0          NA
#> 178             0              1              1             0           1
#> 179            NA             NA             NA             0          NA
#> 180            NA             NA             NA             0          NA
#> 181            NA             NA             NA             1          NA
#> 182            NA             NA             NA             0          NA
#> 183            NA             NA             NA             0          NA
#> 184             0              0              0             0           0
#> 185             0              0              1             0           0
#> 186            NA             NA             NA             0          NA
#> 187            NA             NA             NA             0          NA
#> 188             0              0              0             0           0
#> 189            NA             NA             NA             1          NA
#> 190             0              0              0             1           0
#> 191            NA             NA             NA             0          NA
#> 192            NA             NA             NA             0          NA
#> 193            NA             NA             NA             0          NA
#> 194            NA             NA             NA             0          NA
#> 195            NA             NA             NA             0          NA
#> 196             0              1              1             0           0
#> 197             0              0              1             0           0
#> 198            NA             NA             NA             0          NA
#> 199            NA             NA             NA             0          NA
#> 200            NA             NA             NA             0          NA
#> 201            NA             NA             NA             0          NA
#> 202            NA             NA             NA             0          NA
#> 203            NA             NA             NA             0          NA
#> 204            NA             NA             NA             0          NA
#> 205            NA             NA             NA             1          NA
#> 206            NA             NA             NA             1          NA
#> 207            NA             NA             NA             1          NA
#> 208            NA             NA             NA             0          NA
#> 209            NA             NA             NA             0          NA
#> 210            NA             NA             NA             1          NA
#> 211            NA             NA             NA             0          NA
#> 212             0              0              0             0           0
#> 213             0              0              0             0           0
#> 214             0              0              0             0           0
#> 215            NA             NA             NA             1          NA
#> 216             0              0              0             0           0
#> 217            NA             NA             NA             1          NA
#> 218            NA             NA             NA             1          NA
#> 219             0              0              0             0           0
#> 220            NA             NA             NA             0          NA
#> 221            NA             NA             NA             0          NA
#> 222            NA             NA             NA             0          NA
#> 223             0              0              0             0           0
#> 224            NA             NA             NA             0          NA
#> 225            NA             NA             NA             0          NA
#> 226            NA             NA             NA             0          NA
#> 227             0              0              1             1           0
#> 228            NA             NA             NA             0          NA
#> 229             0              0              0             1           0
#> 230            NA             NA             NA             0          NA
#> 231             0              0              0             1           0
#> 232            NA             NA             NA             0          NA
#> 233            NA             NA             NA             0          NA
#> 234             0              1              1             0           0
#> 235            NA             NA             NA             1          NA
#> 236            NA             NA             NA             1          NA
#> 237            NA             NA             NA             1          NA
#> 238             0              0              0             0           1
#> 239            NA             NA             NA             1          NA
#> 240            NA             NA             NA             0          NA
#> 241             0              0              1             1           0
#> 242             0              0            999             0           0
#> 243             0              0              0             1           0
#> 244             0              1              0             1           0
#> 245            NA             NA             NA             0          NA
#> 246             0              0              1             0           0
#> 247            NA             NA             NA             0          NA
#> 248             1              0              1             0           0
#> 249            NA             NA             NA             0          NA
#> 250            NA             NA             NA             1          NA
#> 251             0              0              0             0           0
#> 252            NA             NA             NA             1          NA
#> 253             0              0              0             0           0
#> 254            NA             NA             NA             0          NA
#> 255             0              0              0             0           0
#> 256            NA             NA             NA             0          NA
#> 257            NA             NA             NA             0          NA
#> 258            NA             NA             NA             0          NA
#> 259             0              0              1             0           0
#> 260             0              0              0             0           0
#> 261            NA             NA             NA             0          NA
#> 262            NA             NA             NA             0          NA
#> 263             0              0              1             0           0
#> 264             0              0              0             0           0
#> 265            NA             NA             NA             0          NA
#> 266             0              0              0             0           0
#> 267             0              0              1             1           0
#> 268            NA             NA             NA             0          NA
#> 269            NA             NA             NA             1          NA
#> 270            NA             NA             NA             0          NA
#> 271            NA             NA             NA             0          NA
#> 272             0              0              0             1           0
#> 273             0              0              1             0           0
#> 274            NA             NA             NA             0          NA
#> 275            NA             NA             NA             0          NA
#> 276            NA             NA             NA             0          NA
#> 277             0              0              0             1           0
#> 278            NA             NA             NA             1          NA
#> 279             0              0              0             0           0
#> 280             0              0              0             0           0
#> 281            NA             NA             NA             1          NA
#> 282             0              0              0             0           0
#> 283             0              0              0             1           0
#> 284             0              0              1             0           0
#> 285            NA             NA             NA             0          NA
#> 286             0              1              1             1           0
#> 287            NA             NA             NA             0          NA
#> 288            NA             NA             NA             0          NA
#> 289            NA             NA             NA             0          NA
#> 290            NA             NA             NA             1          NA
#> 291             0              1              0             0           0
#> 292            NA             NA             NA             1          NA
#> 293             0              0              1             0           0
#> 294            NA             NA             NA             1          NA
#> 295            NA             NA             NA             0          NA
#> 296            NA             NA             NA             0          NA
#> 297             0              1              0             0           0
#> 298            NA             NA             NA             0          NA
#> 299             0              0              0             0           0
#> 300           999            999            999             1         999
#> 301           999            999            999             0         999
#> 302            NA             NA             NA             0          NA
#> 303             0              0              1             0           0
#> 304             0              1              1             0           0
#> 305            NA             NA             NA             0          NA
#> 306             0              0              0             0           0
#> 307             0              0              0             0           0
#> 308             0              0              1             0           0
#> 309            NA             NA             NA             0          NA
#> 310             0              0              0             0           0
#> 311             0              0              0             0           0
#> 312            NA             NA             NA             0          NA
#> 313             0              0              1             0           0
#> 314            NA             NA             NA             0          NA
#> 315             0              0              0             1           0
#> 316             0              0              0             0           0
#> 317             0              0              0             0           0
#> 318             0              0              0             0           0
#> 319             0              0              0             1           0
#> 320            NA             NA             NA             0          NA
#> 321            NA             NA             NA             0          NA
#> 322            NA             NA             NA             1          NA
#> 323            NA             NA             NA             0          NA
#> 324             0              0              0             0           0
#> 325             0              1              1             0           0
#> 326             0              0              0             0           0
#> 327            NA             NA             NA             0          NA
#> 328            NA             NA             NA             1          NA
#> 329             0              0              1             0           0
#> 330             1              1              1             1           0
#> 331            NA             NA             NA             0          NA
#> 332             0              0              0             0           0
#> 333            NA             NA             NA             1          NA
#> 334            NA             NA             NA             0          NA
#> 335            NA             NA             NA             0          NA
#> 336            NA             NA             NA             1          NA
#> 337             0              1              1             1           1
#> 338            NA             NA             NA             0          NA
#> 339             0              1              1             0           0
#> 340             0              0              1             0           0
#> 341            NA             NA             NA             1          NA
#> 342             0              1              1             1           0
#> 343            NA             NA             NA             0          NA
#> 344            NA             NA             NA             0          NA
#> 345             0              0              0             1           0
#> 346            NA             NA             NA             0          NA
#> 347             1              0              0             1           0
#> 348            NA             NA             NA             0          NA
#> 349             0              0              1             0           0
#> 350            NA             NA             NA             0          NA
#> 351            NA             NA             NA             0          NA
#> 352            NA             NA             NA             0          NA
#> 353            NA             NA             NA             1          NA
#> 354             0              0              0             0           0
#> 355            NA             NA             NA             0          NA
#> 356             0              0              0             1           0
#> 357             0              0              1             1           1
#> 358             0              0              0             1           0
#> 359             0              1              1             0           0
#>     child_fruit child_organ child_processmeat child_beef child_eggs child_fish
#> 1             0           0                 0          0          0          0
#> 2             0           0                 0          0          0          0
#> 3             0           0                 0          0          0          0
#> 4             0           1                 0          1          0          0
#> 5             0           0                 0          0          0          0
#> 6             0           0                 0          0          0          0
#> 7             0           0                 0          0          0          0
#> 8             0           0                 0          0          0          0
#> 9             0           0                 0          0          0          0
#> 10            0           0                 0          0          0          0
#> 11            0           0                 0          0          0          0
#> 12            0           0                 0          0          0          0
#> 13            0           0                 0          0          0          0
#> 14            0           0                 0          0          0          0
#> 15            0           0                 0          0          0          0
#> 16            0           0                 0          0          0          0
#> 17            0           0                 0          0          0          0
#> 18            0           0                 0          0          0          0
#> 19            0           0                 0          0          0          0
#> 20            0           0                 0          0          0          0
#> 21            0           0                 0          0          0          0
#> 22            0           0                 0          0          0          0
#> 23            0           0                 0          0          0          0
#> 24            0           0                 0          0          0          0
#> 25            0           0                 0          0          0          0
#> 26            0           0                 0          0          0          0
#> 27            0           0                 0          0          0          0
#> 28            0           0                 0          0          0          0
#> 29            0           0                 0          0          0          0
#> 30            0           0                 0          0          0          0
#> 31            0           0                 0          0          1          0
#> 32            0           0                 0          0          0          0
#> 33            0           0                 0          0          0          0
#> 34            0           0                 0          0          0          0
#> 35            0           0                 0          0          0          0
#> 36            0           0                 0          0          0          0
#> 37            0           0                 0          0          0          0
#> 38            0           0                 0          0          0          0
#> 39            0           0                 0          0          0          0
#> 40            0           0                 0        999        999          0
#> 41           NA          NA                NA         NA         NA         NA
#> 42           NA          NA                NA         NA         NA         NA
#> 43           NA          NA                NA         NA         NA         NA
#> 44           NA          NA                NA         NA         NA         NA
#> 45            0           0                 0          0          0          0
#> 46            0           0                 0          0          0          0
#> 47            0           0                 0          0          0          0
#> 48           NA          NA                NA         NA         NA         NA
#> 49            0           0                 0          0          0          0
#> 50           NA          NA                NA         NA         NA         NA
#> 51           NA          NA                NA         NA         NA         NA
#> 52           NA          NA                NA         NA         NA         NA
#> 53           NA          NA                NA         NA         NA         NA
#> 54           NA          NA                NA         NA         NA         NA
#> 55           NA          NA                NA         NA         NA         NA
#> 56           NA          NA                NA         NA         NA         NA
#> 57           NA          NA                NA         NA         NA         NA
#> 58           NA          NA                NA         NA         NA         NA
#> 59           NA          NA                NA         NA         NA         NA
#> 60           NA          NA                NA         NA         NA         NA
#> 61           NA          NA                NA         NA         NA         NA
#> 62           NA          NA                NA         NA         NA         NA
#> 63           NA          NA                NA         NA         NA         NA
#> 64            0           0                 0          0          0          0
#> 65            0           0                 0          1          0          0
#> 66           NA          NA                NA         NA         NA         NA
#> 67           NA          NA                NA         NA         NA         NA
#> 68           NA          NA                NA         NA         NA         NA
#> 69            0           0                 0          0          0          0
#> 70           NA          NA                NA         NA         NA         NA
#> 71            0           0                 0          1          1          0
#> 72           NA          NA                NA         NA         NA         NA
#> 73            1           0                 0          0          0          0
#> 74           NA          NA                NA         NA         NA         NA
#> 75           NA          NA                NA         NA         NA         NA
#> 76            1           0                 0          0          0          0
#> 77           NA          NA                NA         NA         NA         NA
#> 78           NA          NA                NA         NA         NA         NA
#> 79            0           0                 0          0          0          0
#> 80           NA          NA                NA         NA         NA         NA
#> 81           NA          NA                NA         NA         NA         NA
#> 82           NA          NA                NA         NA         NA         NA
#> 83           NA          NA                NA         NA         NA         NA
#> 84           NA          NA                NA         NA         NA         NA
#> 85           NA          NA                NA         NA         NA         NA
#> 86           NA          NA                NA         NA         NA         NA
#> 87           NA          NA                NA         NA         NA         NA
#> 88           NA          NA                NA         NA         NA         NA
#> 89            1           0                 0          0          1          0
#> 90           NA          NA                NA         NA         NA         NA
#> 91            1           0                 0          1          0          0
#> 92           NA          NA                NA         NA         NA         NA
#> 93           NA          NA                NA         NA         NA         NA
#> 94           NA          NA                NA         NA         NA         NA
#> 95           NA          NA                NA         NA         NA         NA
#> 96           NA          NA                NA         NA         NA         NA
#> 97           NA          NA                NA         NA         NA         NA
#> 98           NA          NA                NA         NA         NA         NA
#> 99           NA          NA                NA         NA         NA         NA
#> 100          NA          NA                NA         NA         NA         NA
#> 101           0           0                 0          0          0        999
#> 102          NA          NA                NA         NA         NA         NA
#> 103          NA          NA                NA         NA         NA         NA
#> 104          NA          NA                NA         NA         NA         NA
#> 105          NA          NA                NA         NA         NA         NA
#> 106          NA          NA                NA         NA         NA         NA
#> 107          NA          NA                NA         NA         NA         NA
#> 108          NA          NA                NA         NA         NA         NA
#> 109          NA          NA                NA         NA         NA         NA
#> 110          NA          NA                NA         NA         NA         NA
#> 111           0           0                 0          0          0          0
#> 112          NA          NA                NA         NA         NA         NA
#> 113          NA          NA                NA         NA         NA         NA
#> 114          NA          NA                NA         NA         NA         NA
#> 115           0           0                 0          0          0          0
#> 116          NA          NA                NA         NA         NA         NA
#> 117           0           0                 0          0          1          0
#> 118           0           0                 0          0          0          0
#> 119           0           0                 0          0          0          0
#> 120          NA          NA                NA         NA         NA         NA
#> 121          NA          NA                NA         NA         NA         NA
#> 122          NA          NA                NA         NA         NA         NA
#> 123          NA          NA                NA         NA         NA         NA
#> 124          NA          NA                NA         NA         NA         NA
#> 125          NA          NA                NA         NA         NA         NA
#> 126           1           0                 0          0          1          1
#> 127          NA          NA                NA         NA         NA         NA
#> 128          NA          NA                NA         NA         NA         NA
#> 129           0           0                 0          0          0          0
#> 130          NA          NA                NA         NA         NA         NA
#> 131          NA          NA                NA         NA         NA         NA
#> 132           1           0                 0          0          0          1
#> 133           0           0                 0          0          0          0
#> 134           0           0                 0          1          1          1
#> 135          NA          NA                NA         NA         NA         NA
#> 136          NA          NA                NA         NA         NA         NA
#> 137           0           0                 0          1          0          0
#> 138          NA          NA                NA         NA         NA         NA
#> 139          NA          NA                NA         NA         NA         NA
#> 140           1           0                 0          1          1          0
#> 141           1           0                 0          1          0          0
#> 142           1           0                 0          0          0          0
#> 143          NA          NA                NA         NA         NA         NA
#> 144           1           0                 0          1          0          1
#> 145          NA          NA                NA         NA         NA         NA
#> 146           0           0                 0          0          0          0
#> 147          NA          NA                NA         NA         NA         NA
#> 148          NA          NA                NA         NA         NA         NA
#> 149           0           0                 0          0          1          0
#> 150           0           0                 0          1          0          0
#> 151          NA          NA                NA         NA         NA         NA
#> 152          NA          NA                NA         NA         NA         NA
#> 153           0           0                 0          0          1          0
#> 154          NA          NA                NA         NA         NA         NA
#> 155           0           0                 0          0          0          0
#> 156          NA          NA                NA         NA         NA         NA
#> 157          NA          NA                NA         NA         NA         NA
#> 158           0           0                 0          0          1          0
#> 159          NA          NA                NA         NA         NA         NA
#> 160           1           0                 0          1          0          1
#> 161          NA          NA                NA         NA         NA         NA
#> 162           1           0                 0          1          1          0
#> 163          NA          NA                NA         NA         NA         NA
#> 164           0           0                 0          0          1          0
#> 165           0           0                 0          0          1          0
#> 166          NA          NA                NA         NA         NA         NA
#> 167          NA          NA                NA         NA         NA         NA
#> 168          NA          NA                NA         NA         NA         NA
#> 169          NA          NA                NA         NA         NA         NA
#> 170           0           0                 0          0          1          1
#> 171          NA          NA                NA         NA         NA         NA
#> 172           0           0                 0          0          1          0
#> 173           1           0                 0          0          0          0
#> 174           1           0                 0          0          1          0
#> 175          NA          NA                NA         NA         NA         NA
#> 176          NA          NA                NA         NA         NA         NA
#> 177          NA          NA                NA         NA         NA         NA
#> 178           1           0                 0          0          0          1
#> 179          NA          NA                NA         NA         NA         NA
#> 180          NA          NA                NA         NA         NA         NA
#> 181          NA          NA                NA         NA         NA         NA
#> 182          NA          NA                NA         NA         NA         NA
#> 183          NA          NA                NA         NA         NA         NA
#> 184           0           0                 0          0          0          0
#> 185           1           0                 0          1          1          0
#> 186          NA          NA                NA         NA         NA         NA
#> 187          NA          NA                NA         NA         NA         NA
#> 188           0           0                 0          0          0          0
#> 189          NA          NA                NA         NA         NA         NA
#> 190           0           0                 0          1          0          0
#> 191          NA          NA                NA         NA         NA         NA
#> 192          NA          NA                NA         NA         NA         NA
#> 193          NA          NA                NA         NA         NA         NA
#> 194          NA          NA                NA         NA         NA         NA
#> 195          NA          NA                NA         NA         NA         NA
#> 196           1           1                 0          0          0          0
#> 197           0           0                 0          1          1          0
#> 198          NA          NA                NA         NA         NA         NA
#> 199          NA          NA                NA         NA         NA         NA
#> 200          NA          NA                NA         NA         NA         NA
#> 201          NA          NA                NA         NA         NA         NA
#> 202          NA          NA                NA         NA         NA         NA
#> 203          NA          NA                NA         NA         NA         NA
#> 204          NA          NA                NA         NA         NA         NA
#> 205          NA          NA                NA         NA         NA         NA
#> 206          NA          NA                NA         NA         NA         NA
#> 207          NA          NA                NA         NA         NA         NA
#> 208          NA          NA                NA         NA         NA         NA
#> 209          NA          NA                NA         NA         NA         NA
#> 210          NA          NA                NA         NA         NA         NA
#> 211          NA          NA                NA         NA         NA         NA
#> 212           1           0                 0          0          0          0
#> 213           0           0                 0          1          1          1
#> 214           0           0                 0          1          0          0
#> 215          NA          NA                NA         NA         NA         NA
#> 216           0           0                 0          0          0          1
#> 217          NA          NA                NA         NA         NA         NA
#> 218          NA          NA                NA         NA         NA         NA
#> 219           0           0                 0          0          0          1
#> 220          NA          NA                NA         NA         NA         NA
#> 221          NA          NA                NA         NA         NA         NA
#> 222          NA          NA                NA         NA         NA         NA
#> 223           0           0                 0          0          0          0
#> 224          NA          NA                NA         NA         NA         NA
#> 225          NA          NA                NA         NA         NA         NA
#> 226          NA          NA                NA         NA         NA         NA
#> 227           0           0                 0          0          1          0
#> 228          NA          NA                NA         NA         NA         NA
#> 229           0           0                 0          0          1          0
#> 230          NA          NA                NA         NA         NA         NA
#> 231           0           0                 0          1          0          0
#> 232          NA          NA                NA         NA         NA         NA
#> 233          NA          NA                NA         NA         NA         NA
#> 234           0           0                 0          0          0          1
#> 235          NA          NA                NA         NA         NA         NA
#> 236          NA          NA                NA         NA         NA         NA
#> 237          NA          NA                NA         NA         NA         NA
#> 238           1           0                 0          0          0          1
#> 239          NA          NA                NA         NA         NA         NA
#> 240          NA          NA                NA         NA         NA         NA
#> 241           0           0                 0          0          1          1
#> 242           0           0                 0          0          0          0
#> 243           0           0                 0          0          0          0
#> 244           0           0                 0          1          0          1
#> 245          NA          NA                NA         NA         NA         NA
#> 246           0           0                 0          1          0          1
#> 247          NA          NA                NA         NA         NA         NA
#> 248           0           0                 0          0          0          1
#> 249          NA          NA                NA         NA         NA         NA
#> 250          NA          NA                NA         NA         NA         NA
#> 251           0           0                 0          0          1          0
#> 252          NA          NA                NA         NA         NA         NA
#> 253           0           0                 0          1          0          0
#> 254          NA          NA                NA         NA         NA         NA
#> 255           0           0                 0          0          0          0
#> 256          NA          NA                NA         NA         NA         NA
#> 257          NA          NA                NA         NA         NA         NA
#> 258          NA          NA                NA         NA         NA         NA
#> 259           0           0                 0          1          0          0
#> 260           0           0                 0          0          1          0
#> 261          NA          NA                NA         NA         NA         NA
#> 262          NA          NA                NA         NA         NA         NA
#> 263           1           0                 0          0          1          1
#> 264           0           0                 0          0          0          0
#> 265          NA          NA                NA         NA         NA         NA
#> 266           0           0                 0          1          0          0
#> 267           0           0                 0          0          0          0
#> 268          NA          NA                NA         NA         NA         NA
#> 269          NA          NA                NA         NA         NA         NA
#> 270          NA          NA                NA         NA         NA         NA
#> 271          NA          NA                NA         NA         NA         NA
#> 272           0           0                 0          0          1          0
#> 273           0           0                 0          0          1          0
#> 274          NA          NA                NA         NA         NA         NA
#> 275          NA          NA                NA         NA         NA         NA
#> 276          NA          NA                NA         NA         NA         NA
#> 277           0           0                 0          0          0          0
#> 278          NA          NA                NA         NA         NA         NA
#> 279           0           0                 0          0          0          0
#> 280           0           0                 0          1          1          0
#> 281          NA          NA                NA         NA         NA         NA
#> 282           0           0                 0          0          1          0
#> 283           0           0                 0          0          0          0
#> 284           1           0                 0          1          0          0
#> 285          NA          NA                NA         NA         NA         NA
#> 286           1           0                 0          0          0          0
#> 287          NA          NA                NA         NA         NA         NA
#> 288          NA          NA                NA         NA         NA         NA
#> 289          NA          NA                NA         NA         NA         NA
#> 290          NA          NA                NA         NA         NA         NA
#> 291           0           0                 0          0          1          0
#> 292          NA          NA                NA         NA         NA         NA
#> 293           1           0                 0          1          1          0
#> 294          NA          NA                NA         NA         NA         NA
#> 295          NA          NA                NA         NA         NA         NA
#> 296          NA          NA                NA         NA         NA         NA
#> 297           0           0                 0          0          0          1
#> 298          NA          NA                NA         NA         NA         NA
#> 299           0           0                 0          0          1          0
#> 300         999         999               999        999        999        999
#> 301         999         999               999        999        999        999
#> 302          NA          NA                NA         NA         NA         NA
#> 303           0           0                 0          1          0          0
#> 304           0           0                 0          1          0          0
#> 305          NA          NA                NA         NA         NA         NA
#> 306           0           0                 0          0          0          0
#> 307           0           0                 0          0          1          0
#> 308           0           0                 0          0          1          0
#> 309          NA          NA                NA         NA         NA         NA
#> 310           0           0                 0          1          0          0
#> 311           0           0                 0          0          0          0
#> 312          NA          NA                NA         NA         NA         NA
#> 313           0           0                 0          0          1          0
#> 314          NA          NA                NA         NA         NA         NA
#> 315           0           0                 0          0          1          1
#> 316           1           0                 0          0          1          0
#> 317           0           0                 0          0          0          0
#> 318           0           0                 0          1          1          0
#> 319           0           0               999          1          1        999
#> 320          NA          NA                NA         NA         NA         NA
#> 321          NA          NA                NA         NA         NA         NA
#> 322          NA          NA                NA         NA         NA         NA
#> 323          NA          NA                NA         NA         NA         NA
#> 324           0           0                 0          0          1          0
#> 325           0           0                 0          0          1          0
#> 326           0           0                 0          0          0          0
#> 327          NA          NA                NA         NA         NA         NA
#> 328          NA          NA                NA         NA         NA         NA
#> 329           0           0                 0          0          1          0
#> 330           1           0                 0          0          0          0
#> 331          NA          NA                NA         NA         NA         NA
#> 332           0           0                 0          0          1          0
#> 333          NA          NA                NA         NA         NA         NA
#> 334          NA          NA                NA         NA         NA         NA
#> 335          NA          NA                NA         NA         NA         NA
#> 336          NA          NA                NA         NA         NA         NA
#> 337           1           0                 0          0          1          0
#> 338          NA          NA                NA         NA         NA         NA
#> 339           1           0                 0          0          1          0
#> 340           0           0                 0          0          1          0
#> 341          NA          NA                NA         NA         NA         NA
#> 342           0           0                 0          0          1          0
#> 343          NA          NA                NA         NA         NA         NA
#> 344          NA          NA                NA         NA         NA         NA
#> 345           1           0                 0          0          0          0
#> 346          NA          NA                NA         NA         NA         NA
#> 347           0           0                 0          1          0          0
#> 348          NA          NA                NA         NA         NA         NA
#> 349           0           0                 0          0          1          0
#> 350          NA          NA                NA         NA         NA         NA
#> 351          NA          NA                NA         NA         NA         NA
#> 352          NA          NA                NA         NA         NA         NA
#> 353          NA          NA                NA         NA         NA         NA
#> 354           0           0                 0          0          0          0
#> 355          NA          NA                NA         NA         NA         NA
#> 356           0           0                 0          0          0          0
#> 357           0           0                 0          1          0          0
#> 358           0           0                 0          1          0          0
#> 359           1           0                 0          1          1          0
#>     child_beans child_cheese child_sweets child_snack child_oth_food
#> 1             0            0            0           0              0
#> 2             0            0            0           0              0
#> 3             0            0            0           0              1
#> 4             0            0            0           1              0
#> 5             0            0            1           0              0
#> 6             0            0            0           0              0
#> 7             0            0            0           0              1
#> 8             0            0            0           0              0
#> 9             0            0            1           0              0
#> 10            0            0            0           1              0
#> 11            0            0            0           0              1
#> 12            0            0            0           0              1
#> 13            0            0            0           0              0
#> 14            0            0            0           0              0
#> 15            0            0            0           0              1
#> 16            0            0            0           0              0
#> 17            0            0            0           0              0
#> 18            0            0            0           0              1
#> 19            0            0            0           0              1
#> 20            0            0            0           0              0
#> 21            0            0            0           0              0
#> 22            0            0            0           0              0
#> 23            0            0            0           0              0
#> 24            0            0            0           0              1
#> 25            0            0            0           0              0
#> 26            0            0            0           0              1
#> 27            0            0            0           0              0
#> 28            0            0            0           1              0
#> 29            0            0            0           0              1
#> 30            0            0            0           0              0
#> 31            0            0            1           1              0
#> 32            0            0            1           0              1
#> 33            0            0            0           0              0
#> 34            0            0            0           0              0
#> 35            0            0            0           0              0
#> 36            0            0            0           0              1
#> 37            0            0            1           1              0
#> 38            0            0            0           0              0
#> 39            0            0            0           0              0
#> 40          999            0            0           0              1
#> 41           NA           NA           NA          NA              1
#> 42           NA           NA           NA          NA              0
#> 43           NA           NA           NA          NA              1
#> 44           NA           NA           NA          NA              0
#> 45            1            1            0           1              1
#> 46            0            0            0           1              0
#> 47            0            0            0           0              0
#> 48           NA           NA           NA          NA              1
#> 49            0            0            1           0              0
#> 50           NA           NA           NA          NA              0
#> 51           NA           NA           NA          NA              0
#> 52           NA           NA           NA          NA              0
#> 53           NA           NA           NA          NA              0
#> 54           NA           NA           NA          NA              0
#> 55           NA           NA           NA          NA              0
#> 56           NA           NA           NA          NA              0
#> 57           NA           NA           NA          NA              0
#> 58           NA           NA           NA          NA              1
#> 59           NA           NA           NA          NA              0
#> 60           NA           NA           NA          NA              0
#> 61           NA           NA           NA          NA              1
#> 62           NA           NA           NA          NA              0
#> 63           NA           NA           NA          NA              0
#> 64            1            1            1           1              0
#> 65            0            0            0           1              0
#> 66           NA           NA           NA          NA              0
#> 67           NA           NA           NA          NA              0
#> 68           NA           NA           NA          NA              0
#> 69            1            0            1           1              0
#> 70           NA           NA           NA          NA              0
#> 71            0            1            0           1              0
#> 72           NA           NA           NA          NA              0
#> 73            0            0            0           1              1
#> 74           NA           NA           NA          NA              0
#> 75           NA           NA           NA          NA              0
#> 76            1            1            1           1              0
#> 77           NA           NA           NA          NA              0
#> 78           NA           NA           NA          NA              0
#> 79            0            0            0           0              0
#> 80           NA           NA           NA          NA              1
#> 81           NA           NA           NA          NA              0
#> 82           NA           NA           NA          NA              0
#> 83           NA           NA           NA          NA              0
#> 84           NA           NA           NA          NA              0
#> 85           NA           NA           NA          NA              0
#> 86           NA           NA           NA          NA              0
#> 87           NA           NA           NA          NA              0
#> 88           NA           NA           NA          NA              0
#> 89            0            1            1           1              0
#> 90           NA           NA           NA          NA              1
#> 91            0            0            0           1              1
#> 92           NA           NA           NA          NA              0
#> 93           NA           NA           NA          NA              0
#> 94           NA           NA           NA          NA              0
#> 95           NA           NA           NA          NA              0
#> 96           NA           NA           NA          NA              0
#> 97           NA           NA           NA          NA              1
#> 98           NA           NA           NA          NA              0
#> 99           NA           NA           NA          NA              0
#> 100          NA           NA           NA          NA              0
#> 101           0            0            1           1              1
#> 102          NA           NA           NA          NA              0
#> 103          NA           NA           NA          NA              0
#> 104          NA           NA           NA          NA              1
#> 105          NA           NA           NA          NA              1
#> 106          NA           NA           NA          NA              0
#> 107          NA           NA           NA          NA              1
#> 108          NA           NA           NA          NA              0
#> 109          NA           NA           NA          NA              0
#> 110          NA           NA           NA          NA              0
#> 111           0            0            0           0              0
#> 112          NA           NA           NA          NA              1
#> 113          NA           NA           NA          NA              0
#> 114          NA           NA           NA          NA              1
#> 115           0            0            1           1              0
#> 116          NA           NA           NA          NA              0
#> 117           0            0            1           0              1
#> 118           1            1            1           0              0
#> 119           0            0            0           0              0
#> 120          NA           NA           NA          NA              0
#> 121          NA           NA           NA          NA              0
#> 122          NA           NA           NA          NA              0
#> 123          NA           NA           NA          NA              0
#> 124          NA           NA           NA          NA              1
#> 125          NA           NA           NA          NA              0
#> 126           0            1            1           1              0
#> 127          NA           NA           NA          NA              0
#> 128          NA           NA           NA          NA              0
#> 129           0            1            1           1              0
#> 130          NA           NA           NA          NA              1
#> 131          NA           NA           NA          NA              0
#> 132           0            0            1           1              1
#> 133           0            0            1           1              0
#> 134           0            0            0           1              0
#> 135          NA           NA           NA          NA              0
#> 136          NA           NA           NA          NA              0
#> 137           0            0            1           1              0
#> 138          NA           NA           NA          NA              1
#> 139          NA           NA           NA          NA              1
#> 140           0            1            1           1              0
#> 141           0            1            0           1              0
#> 142           0            0            0           1              0
#> 143          NA           NA           NA          NA              1
#> 144           0            0            1           1              0
#> 145          NA           NA           NA          NA              0
#> 146           0            0            1           1              0
#> 147          NA           NA           NA          NA              0
#> 148          NA           NA           NA          NA              0
#> 149           0            1            1           1              0
#> 150           0            0            1           1              0
#> 151          NA           NA           NA          NA              0
#> 152          NA           NA           NA          NA              0
#> 153           0            0            1           0              0
#> 154          NA           NA           NA          NA              0
#> 155           0            0            1           0              0
#> 156          NA           NA           NA          NA              0
#> 157          NA           NA           NA          NA              1
#> 158           0            1            1           0              0
#> 159          NA           NA           NA          NA              0
#> 160           0            0            0           1              0
#> 161          NA           NA           NA          NA              0
#> 162           0            0            1           1              0
#> 163          NA           NA           NA          NA              0
#> 164           1            0            1           1              0
#> 165           0            0            1           1              0
#> 166          NA           NA           NA          NA              0
#> 167          NA           NA           NA          NA              1
#> 168          NA           NA           NA          NA              1
#> 169          NA           NA           NA          NA              0
#> 170           0            0            0           1              0
#> 171          NA           NA           NA          NA              1
#> 172           0            0            0           1              1
#> 173           0            0            0           1              0
#> 174           0            0            1           1              0
#> 175          NA           NA           NA          NA              0
#> 176          NA           NA           NA          NA              0
#> 177          NA           NA           NA          NA              0
#> 178           0            0            1           1              0
#> 179          NA           NA           NA          NA              1
#> 180          NA           NA           NA          NA              0
#> 181          NA           NA           NA          NA              1
#> 182          NA           NA           NA          NA              0
#> 183          NA           NA           NA          NA              0
#> 184           0            0            0           0              0
#> 185           0            1            1           1              0
#> 186          NA           NA           NA          NA              0
#> 187          NA           NA           NA          NA              0
#> 188           0            0            0           0              0
#> 189          NA           NA           NA          NA              1
#> 190           0            0            0           0              0
#> 191          NA           NA           NA          NA              0
#> 192          NA           NA           NA          NA              1
#> 193          NA           NA           NA          NA              1
#> 194          NA           NA           NA          NA              0
#> 195          NA           NA           NA          NA              0
#> 196           0            0            0           1              0
#> 197           0            0            1           1              1
#> 198          NA           NA           NA          NA              0
#> 199          NA           NA           NA          NA              0
#> 200          NA           NA           NA          NA              1
#> 201          NA           NA           NA          NA              1
#> 202          NA           NA           NA          NA              0
#> 203          NA           NA           NA          NA              0
#> 204          NA           NA           NA          NA              0
#> 205          NA           NA           NA          NA              0
#> 206          NA           NA           NA          NA              0
#> 207          NA           NA           NA          NA              1
#> 208          NA           NA           NA          NA              0
#> 209          NA           NA           NA          NA              0
#> 210          NA           NA           NA          NA              0
#> 211          NA           NA           NA          NA              1
#> 212           0            0            1           0              0
#> 213           0            1            1           1              0
#> 214           0            0            1           0              0
#> 215          NA           NA           NA          NA              1
#> 216           0            0            0           1              0
#> 217          NA           NA           NA          NA              0
#> 218          NA           NA           NA          NA              1
#> 219           0            1            1           1              1
#> 220          NA           NA           NA          NA              1
#> 221          NA           NA           NA          NA              0
#> 222          NA           NA           NA          NA              0
#> 223           0            0            0           0              0
#> 224          NA           NA           NA          NA              0
#> 225          NA           NA           NA          NA              0
#> 226          NA           NA           NA          NA              0
#> 227           1            0            1           1              0
#> 228          NA           NA           NA          NA              0
#> 229           0            0            1           1              0
#> 230          NA           NA           NA          NA              0
#> 231           0            0            0           1              0
#> 232          NA           NA           NA          NA              1
#> 233          NA           NA           NA          NA              1
#> 234           0            1            1           1              0
#> 235          NA           NA           NA          NA              1
#> 236          NA           NA           NA          NA              0
#> 237          NA           NA           NA          NA              0
#> 238           0            0            1           1              1
#> 239          NA           NA           NA          NA              0
#> 240          NA           NA           NA          NA              0
#> 241           0            0            0           1              1
#> 242           0            0            0           1              1
#> 243           0            1            1           1              0
#> 244           0            0            1           1              0
#> 245          NA           NA           NA          NA              0
#> 246           0            0            1           1              0
#> 247          NA           NA           NA          NA              0
#> 248           0            0            0           1              0
#> 249          NA           NA           NA          NA              0
#> 250          NA           NA           NA          NA              0
#> 251           0            0            1           1              1
#> 252          NA           NA           NA          NA              0
#> 253           0            1            0           1              0
#> 254          NA           NA           NA          NA              0
#> 255           0            0            0           1              0
#> 256          NA           NA           NA          NA              1
#> 257          NA           NA           NA          NA              0
#> 258          NA           NA           NA          NA              1
#> 259           0            1            1           0              0
#> 260           0            1            1           1              1
#> 261          NA           NA           NA          NA              0
#> 262          NA           NA           NA          NA              0
#> 263           0            1            1           1              1
#> 264           0            0            0           0              0
#> 265          NA           NA           NA          NA              1
#> 266           0            1            0           1              0
#> 267           0            1            1           1              1
#> 268          NA           NA           NA          NA              0
#> 269          NA           NA           NA          NA              0
#> 270          NA           NA           NA          NA              0
#> 271          NA           NA           NA          NA              1
#> 272           0            0            1           1              1
#> 273           0            0            1           1              0
#> 274          NA           NA           NA          NA              0
#> 275          NA           NA           NA          NA              0
#> 276          NA           NA           NA          NA              0
#> 277           0            0            0           0              0
#> 278          NA           NA           NA          NA              0
#> 279           0            1            1           1              0
#> 280           1            0            1           1              0
#> 281          NA           NA           NA          NA              0
#> 282           0            0            1           1              1
#> 283           0            0            0           1              1
#> 284           0            0            1           1              1
#> 285          NA           NA           NA          NA              0
#> 286           1            0            1           1              0
#> 287          NA           NA           NA          NA              0
#> 288          NA           NA           NA          NA              1
#> 289          NA           NA           NA          NA              0
#> 290          NA           NA           NA          NA              0
#> 291           0            1            1           1              1
#> 292          NA           NA           NA          NA              0
#> 293           1            0            0           1              0
#> 294          NA           NA           NA          NA              0
#> 295          NA           NA           NA          NA              0
#> 296          NA           NA           NA          NA              0
#> 297           1            1            1           1              1
#> 298          NA           NA           NA          NA              0
#> 299           0            0            1           0              0
#> 300         999          999          999         999              1
#> 301         999          999          999         999              0
#> 302          NA           NA           NA          NA              0
#> 303           0            1            1           1              0
#> 304           0            0            0           1              1
#> 305          NA           NA           NA          NA              0
#> 306           0            0            0           1              1
#> 307           0            0            0           1              0
#> 308           0            0            0           1              0
#> 309          NA           NA           NA          NA              1
#> 310           0            0            0           1              0
#> 311           0            0            0           0              0
#> 312          NA           NA           NA          NA              1
#> 313           0            0            0           1              1
#> 314          NA           NA           NA          NA              0
#> 315           0            1            0           1              0
#> 316           0            1            1           1              0
#> 317           0            0            0           1              0
#> 318           1            0            1           1              0
#> 319           0            1            0           1              0
#> 320          NA           NA           NA          NA              1
#> 321          NA           NA           NA          NA              1
#> 322          NA           NA           NA          NA              0
#> 323          NA           NA           NA          NA              0
#> 324           0            1            1           1              0
#> 325           0            1            1           1              0
#> 326           1            0            0           0              0
#> 327          NA           NA           NA          NA              0
#> 328          NA           NA           NA          NA              0
#> 329           0            0            0           1              1
#> 330           0            0            1           1              0
#> 331          NA           NA           NA          NA              0
#> 332           0            0            1           1              0
#> 333          NA           NA           NA          NA              1
#> 334          NA           NA           NA          NA              0
#> 335          NA           NA           NA          NA              0
#> 336          NA           NA           NA          NA              0
#> 337           1            0            0           1              0
#> 338          NA           NA           NA          NA              1
#> 339           1            0            1           1              0
#> 340           0            0            1           1              0
#> 341          NA           NA           NA          NA              0
#> 342           0            0            1           1              0
#> 343          NA           NA           NA          NA              0
#> 344          NA           NA           NA          NA              0
#> 345           0            1            0           1              0
#> 346          NA           NA           NA          NA              1
#> 347           0            0            1           1              1
#> 348          NA           NA           NA          NA              0
#> 349           1            0            1           1              0
#> 350          NA           NA           NA          NA              0
#> 351          NA           NA           NA          NA              0
#> 352          NA           NA           NA          NA              0
#> 353          NA           NA           NA          NA              0
#> 354           0            0            0           0              1
#> 355          NA           NA           NA          NA              0
#> 356           0            0            0           1              1
#> 357           0            0            1           1              1
#> 358           0            0            0           1              0
#> 359           1            0            1           1              0
#>     child_food_freq
#> 1                 0
#> 2                 3
#> 3                 0
#> 4                 3
#> 5                 5
#> 6                 0
#> 7                 3
#> 8                10
#> 9                 3
#> 10                5
#> 11                0
#> 12                0
#> 13                0
#> 14                0
#> 15              999
#> 16                0
#> 17                0
#> 18                0
#> 19                0
#> 20                0
#> 21                0
#> 22                0
#> 23                0
#> 24                6
#> 25                0
#> 26                5
#> 27                0
#> 28                4
#> 29                3
#> 30                5
#> 31                1
#> 32                5
#> 33                0
#> 34                0
#> 35                2
#> 36                0
#> 37                4
#> 38                0
#> 39                0
#> 40                6
#> 41               NA
#> 42               NA
#> 43               NA
#> 44               NA
#> 45                0
#> 46                0
#> 47                0
#> 48               NA
#> 49                3
#> 50               NA
#> 51               NA
#> 52               NA
#> 53               NA
#> 54               NA
#> 55               NA
#> 56               NA
#> 57               NA
#> 58               NA
#> 59               NA
#> 60               NA
#> 61               NA
#> 62               NA
#> 63               NA
#> 64                0
#> 65                3
#> 66               NA
#> 67               NA
#> 68               NA
#> 69                0
#> 70               NA
#> 71                5
#> 72               NA
#> 73                0
#> 74               NA
#> 75               NA
#> 76                4
#> 77               NA
#> 78               NA
#> 79                2
#> 80               NA
#> 81               NA
#> 82               NA
#> 83               NA
#> 84               NA
#> 85               NA
#> 86               NA
#> 87               NA
#> 88               NA
#> 89                3
#> 90               NA
#> 91                4
#> 92               NA
#> 93               NA
#> 94               NA
#> 95               NA
#> 96               NA
#> 97               NA
#> 98               NA
#> 99               NA
#> 100              NA
#> 101               5
#> 102              NA
#> 103              NA
#> 104              NA
#> 105              NA
#> 106              NA
#> 107              NA
#> 108              NA
#> 109              NA
#> 110              NA
#> 111               0
#> 112              NA
#> 113              NA
#> 114              NA
#> 115               3
#> 116              NA
#> 117               1
#> 118               0
#> 119               0
#> 120              NA
#> 121              NA
#> 122              NA
#> 123              NA
#> 124              NA
#> 125              NA
#> 126               3
#> 127              NA
#> 128              NA
#> 129               0
#> 130              NA
#> 131              NA
#> 132               5
#> 133               3
#> 134               3
#> 135              NA
#> 136              NA
#> 137               3
#> 138              NA
#> 139              NA
#> 140               0
#> 141               0
#> 142               0
#> 143              NA
#> 144               5
#> 145              NA
#> 146               0
#> 147              NA
#> 148              NA
#> 149               0
#> 150               0
#> 151              NA
#> 152              NA
#> 153               0
#> 154              NA
#> 155               3
#> 156              NA
#> 157              NA
#> 158               3
#> 159              NA
#> 160               3
#> 161              NA
#> 162               3
#> 163              NA
#> 164               4
#> 165               3
#> 166              NA
#> 167              NA
#> 168              NA
#> 169              NA
#> 170               3
#> 171              NA
#> 172               3
#> 173               3
#> 174               0
#> 175              NA
#> 176              NA
#> 177              NA
#> 178               5
#> 179              NA
#> 180              NA
#> 181              NA
#> 182              NA
#> 183              NA
#> 184               3
#> 185               0
#> 186              NA
#> 187              NA
#> 188               0
#> 189              NA
#> 190               4
#> 191              NA
#> 192              NA
#> 193              NA
#> 194              NA
#> 195              NA
#> 196               3
#> 197               4
#> 198              NA
#> 199              NA
#> 200              NA
#> 201              NA
#> 202              NA
#> 203              NA
#> 204              NA
#> 205              NA
#> 206              NA
#> 207              NA
#> 208              NA
#> 209              NA
#> 210              NA
#> 211              NA
#> 212               2
#> 213               2
#> 214               2
#> 215              NA
#> 216               4
#> 217              NA
#> 218              NA
#> 219               3
#> 220              NA
#> 221              NA
#> 222              NA
#> 223               0
#> 224              NA
#> 225              NA
#> 226              NA
#> 227               3
#> 228              NA
#> 229               2
#> 230              NA
#> 231               2
#> 232              NA
#> 233              NA
#> 234               3
#> 235              NA
#> 236              NA
#> 237              NA
#> 238               4
#> 239              NA
#> 240              NA
#> 241               4
#> 242               3
#> 243               3
#> 244               0
#> 245              NA
#> 246               3
#> 247              NA
#> 248               3
#> 249              NA
#> 250              NA
#> 251               4
#> 252              NA
#> 253               0
#> 254              NA
#> 255               2
#> 256              NA
#> 257              NA
#> 258              NA
#> 259               0
#> 260               5
#> 261              NA
#> 262              NA
#> 263               0
#> 264               0
#> 265              NA
#> 266               4
#> 267               3
#> 268              NA
#> 269              NA
#> 270              NA
#> 271              NA
#> 272               5
#> 273               5
#> 274              NA
#> 275              NA
#> 276              NA
#> 277               2
#> 278              NA
#> 279               3
#> 280               3
#> 281              NA
#> 282               0
#> 283               3
#> 284               3
#> 285              NA
#> 286               2
#> 287              NA
#> 288              NA
#> 289              NA
#> 290              NA
#> 291               3
#> 292              NA
#> 293               3
#> 294              NA
#> 295              NA
#> 296              NA
#> 297               3
#> 298              NA
#> 299               3
#> 300               3
#> 301               3
#> 302              NA
#> 303               0
#> 304               3
#> 305              NA
#> 306               3
#> 307               1
#> 308               4
#> 309              NA
#> 310               3
#> 311               3
#> 312              NA
#> 313               3
#> 314              NA
#> 315               0
#> 316               4
#> 317               2
#> 318               3
#> 319               0
#> 320              NA
#> 321              NA
#> 322              NA
#> 323              NA
#> 324               0
#> 325               0
#> 326               0
#> 327              NA
#> 328              NA
#> 329               3
#> 330               4
#> 331              NA
#> 332               3
#> 333              NA
#> 334              NA
#> 335              NA
#> 336              NA
#> 337               4
#> 338              NA
#> 339               3
#> 340               4
#> 341              NA
#> 342               5
#> 343              NA
#> 344              NA
#> 345               0
#> 346              NA
#> 347               4
#> 348              NA
#> 349               2
#> 350              NA
#> 351              NA
#> 352              NA
#> 353              NA
#> 354               2
#> 355              NA
#> 356               3
#> 357               3
#> 358               3
#> 359               3
```

This dataset contains 359 observations and 46 variables, and all
variables were organized based on the WHO IYCF sample questionnaire. We
tried to mention explicitly matching function inputs parameters and
question numbers from the WHO IYCF sample question. If your dataset did
not have the same variables as the WHO sample one, please apply the most
relevant ones in the respective input parameter.

## Minimum Diet Related Indicators

### Minimum Dietary Diversity 6-23 Months (MDD)

Calculation of MDD included three main steps: (1) identification of food
groups that were consumed by the children 6-23 months of age in the
previous days, (2) calculation of the total number of food groups (out
of total 8 food groups) consumed in the previous day and (3)
identification of children who meet the minimum requirement of food
groups (at least 5 food groups) in their previous day’s consumption.

#### Individual food group consumption: dummy variables

As the first step of MDD, we will now identify the food group the
children consumed the previous day. We will use the `get_dummy` function
to generate the dummy variable for each 8 food groups:

- breast milk;
- grains, white/pale starchy roots, tubers and plantains;
- beans, peas, lentils, nuts and seeds;
- dairy products (milk, infant formula, yogurt, cheese);
- flesh foods (meat, fish, poultry, organ meats);
- eggs;
- vitamin A-rich fruits and vegetables; and
- other fruits and vegetables.

Please note that the input parameter of the `get_dummy` function is the
`list`, not `vector`. We need to prepare the list of variables for the
food items we would like to include in each food group dummy variable
calculation. In this article, I used the same variables applied in 2021
WHO IYCF indicators guideline. The detailed description of the input
variables used for each food group calculation can be checked using the
following syntax in the sample dataset description file.

``` r

?iycfData
```

For the breastfeeding dummy variable, the guideline-recommended to using
the `Question 4`: breastfed yesterday during the day or at night.

``` r

df <- iycfData

df$breastmilk <- get_dummy(var_list = list(df$child_bfyest))
```

For grains-related food groups, including grains, white/pale starchy
roots, tubers and plantains, we used `Question 7B` and `Question 7D`
from the WHO IYCF sample questionnaires.

``` r

df$grains <- get_dummy(var_list = list(df$child_rice, df$child_potatoes))
```

For pulses food groups which include beans, peas, lentils, nuts, and
seeds, we only need to use one variable, `Question 7N`.

``` r

df$pulses <- get_dummy(var_list = list(df$child_beans))
```

In the dairy products food group, the following foods items are
included; milk, infant formula, yogurt, and cheese) and the following
questions from WHO guidelines provide that information; `Question 6B`,
`Question 6C`, `Question 6D`, `Question 7A`, and `Question 7O`.

``` r

dairy_list <- list(df$child_bms, df$child_milk, df$child_mproduct, df$child_yogurt, df$child_yogurt)

df$dairy <- get_dummy(var_list = dairy_list)
```

The information about flesh foods, meat, fish, poultry, and organ meats,
is available from the following four questions from the guideline;
`Question 7I`, `Question 7J`, `Question 7K`, and `Question 7M`.

``` r

meat_list <- list(df$child_organ, df$child_insects,  df$child_beef, df$child_fish)

df$meat <- get_dummy(var_list = meat_list)
```

Only one variable requires for the Eggs food group, which is
`Question 7L`.

``` r

df$eggs <- get_dummy(var_list = list(df$child_eggs))
```

We will use the following questions from the guideline for calculating
vitamin A-rich fruits and vegetables; `Question 7C`, `Question 7E`, and
`Question 7G`.

``` r

vita_fruveg_list <- list(df$child_pumpkin, df$child_leafyveg, df$child_mango)

df$vita_fruveg <- get_dummy(var_list = vita_fruveg_list)
```

We used the following two variables for other fruits and vegetable food
groups; `Question 7F` and `Question 7H`.

``` r

df$oth_fruveg <- get_dummy(var_list = list(df$child_fruit, df$child_fruit))
```

#### Food groups score calculation

Then, we can now calculate the total food group score the children
consumed on the previous day. We will use all the individual food group
variables in this calculation.

``` r

df$food_score <- get_foodscore(breastmilk = df$breastmilk, 
                               grains = df$grains, 
                               pulses = df$pulses, 
                               dairy = df$dairy, 
                               meat = df$meat, 
                               eggs = df$eggs, 
                               vita_fruveg = df$vita_fruveg, 
                               oth_fruveg = df$oth_fruveg)
table(df$food_score)
#> 
#>  0  1  2  3  4  5  6  7  8 
#>  3 30 32 43 28 18  3  4  1
summary(df$food_score)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
#>   0.000   2.000   3.000   2.957   4.000   8.000     197
```

#### Calculation of MDD

Finally, we can calculate indicators for minimum dietary diversity,
which is identified by the children 6-23 months who get at least 5 food
groups in their previous day’s diet.

``` r

df$mdd <- get_mdd(food_score = df$food_score, 
                  age = df$calc_age_months)
table(df$mdd)
#> 
#>  0  1 
#> 87 26
```

### Minimum Meal Frequency 6-23 Months (MMF)

Unlike MDD, the MMF requires more complex criteria, and those depend on
the children breastfeeding status and the age range. We need four
essential information to calculate this indicator; child age,
breastfeeding status, meal frequency, and milk (beside breastmilk)
feeding frequency.

#### Breastfed child

The minimum meal frequency for breastfeeding varies by the child’s age.
It only requires at least two meals for 6-8 months old children, but at
least three times for children of 9-23 months old. Below are the sample
syntax using three functions to calculate the minimum meal frequency for
breastfeeding children.

- `get_mmf_bf_6to8` - to calculate the minimum meal frequency for 6-8
  months old breastfed children
- `get_mmf_bf_9to23` - to calculate the minimum meal frequency for 6-8
  months old breastfed children
- `get_mmf_bf` - to calculate the minimum meal frequency for 6-8 months
  old breastfed children

``` r

# BF 6-8 months 
df$mmfbf_6to8 <- get_mmf_bf_6to8(q4 = df$child_bfyest, 
                                 q8 = df$child_food_freq, 
                                 age = df$calc_age_months)
table(df$mmfbf_6to8)
#> 
#>  0  1 
#>  7 14

# BF 9-23 months 
df$mmfbf_9to23 <- get_mmf_bf_9to23(q4 = df$child_bfyest, 
                                   q8 = df$child_food_freq, 
                                   age = df$calc_age_months)

table(df$mmfbf_9to23)
#> 
#>  0  1 
#> 24 28

# Overall BF
df$mmf_bf <- get_mmf_bf(mmfbf_6to8 = df$mmfbf_6to8, 
                        mmfbf_9to23 = df$mmfbf_9to23)
table(df$mmf_bf)
#> 
#>  0  1 
#> 31 42
```

#### Non-Breastfed child

In MMF calculation for non-breastfeeding children, we need to consider
the number of milk feeds and the meal frequency. To get that, use the
`get_nonbf_frq` function to calculate the milk & meal feeding frequency
using the following questions;

- `Question 6Bnum`: infant formula feeding frequency,
- `Question 6Cnum`: animal milk feeding frequency,
- `Question 6Dnum`: milk-product liquid feeding frequency and
- `Question 8`: meal frequency.

``` r


df$nonbf_frq <- get_nonbf_frq(q6bnum = df$child_bms_freq, 
                              q6cnum = df$child_milk_freq, 
                              q6dnum = df$child_mproduct_freq, 
                              q8 = df$child_food_freq)

table(df$nonbf_frq)
#> < table of extent 0 >
sum(df$nonbf_frq)
#> [1] NA
```

Then, calculate the minimum meal frequency for the 6-23 months of
non-breastfeeding children using that milk & meal frequency information
calculate above. Whoever gets at least 4-times milk and meal frequency
in the previous are identified as the children who get minimum meal
frequency for non-breastfed children. Use the `get_mmf_nonbf` function
to calculate this indicator as the sample syntax provided below.

``` r

df$mmf_nonbf <- get_mmf_nonbf(q4 = df$child_bfyest,  
                              q8 = df$child_food_freq, 
                              nonbf_frq = df$nonbf_frq, 
                              age = df$calc_age_months)
table(df$mmf_nonbf)
#> < table of extent 0 >
```

#### Calculation of MMF

After calculating minimum meal frequency for breastfed and non-breastfed
children separately, use the `get_mmf` function to consolidate as one
overall minimum meal frequency indicator for reporting. We also
frequently report this indicator in the aggregated figure by
breastfeeding status and age range (for breastfed children). Therefore,
all the immediate variables we created along the path to calculate this
overall MMF indicator are also helpful for reporting work.

``` r

df$mmf <- get_mmf(mmf_bf = df$mmf_bf, 
                  mmf_nonbf = df$mmf_nonbf)
table(df$mmf)
#> 
#>  0  1 
#> 31 42
```

### Minimum Milk Feeding Frequency For Non-Breastfed Children 6-23 Months (MMFF)

This is the minimum milk feeding frequency indicator for
non-breastfeeding children. It requires two-stage of calculations; (1)
calculating the milk feeding frequency score and (2) calculating the
MMFF indicator. From our package, we can use the `get_milk_frq` function
to get this score, and the following input information is required for
this score;

- `Question 6Bnum`: infant formula feeding frequency,
- `Question 6Cnum`: animal milk feeding frequency,
- `Question 6Dnum`: milk product liquid feeding frequency, and
- `Question 7Anum`: the milk-product solid food feeding frequency.

``` r

df$milk_frq <- get_milk_frq(q6bnum = df$child_bms_freq, 
                            q6cnum = df$child_milk_freq, 
                            q6dnum = df$child_mproduct_freq, 
                            q7anum = df$child_yogurt)
table(df$milk_frq)
#> < table of extent 0 >
sum(df$milk_frq)
#> [1] NA
```

Whoever 6-23 months old non-breastfed children who get at least two milk
feeds from the previous day are considered minimally milk-fed children.
Use the `get_mmff` function to calculate this indicator, and it only
requires two input parameters; age and milk feeding frequency score.

``` r

df$mmff <- get_mmff(q4 = df$child_bfyest, 
                    age = df$calc_age_months, 
                    milk_frq = df$milk_frq)
table(df$mmff)
#> < table of extent 0 >
```

### Minimum Acceptable Diet

Using all the information calculated before in this article; minimum
dietary diversity, minimum meal frequency, and minimum milk frequency
for non-breastfeed children. Whoever 6-23 months old children who meet
the following condition are identified as children who get the minimum
acceptable diet; + get a minimum dietary diversity, + get minimum meal
frequency (for both breastfed or non-breastfed), and + get breastfed or
get minimum milk feeding frequency.

We will use the `get_mad` function to calculate this MAD indicator, and
sample syntax is provided below.

``` r

df$mad <- get_mad(age = df$calc_age_months, 
                  q4 = df$child_bfyest, 
                  mdd = df$mdd, 
                  mmf = df$mmf, 
                  mmff = df$mmff)
table(df$mad)
#> 
#>  0  1 
#> 61 12
```
