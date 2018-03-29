April in Japan is a time of new beginnings. Unlike most countries, the school year starts in April and is also the beginning of the fiscal year for the government and most corporations. This past April, The Economist presented an interesting article with a beautiful ggplot2 graphic depicting the steadily early blossoming of sakura trees in Kyoto:

![Imgur](https://i.imgur.com/1lGPG0Y.png)

For this article I will try to recreate this graphic but with a few tricks of my own. Thankfully the raw data is made available by the original author of the research paper documenting this phenomenon, Dr. Yasuyuki Aono from Osaka Prefecture University. The data set can be downloaded from [here](http://atmenv.envi.osakafu-u.ac.jp/aono/kyophenotemp4/).

Let's get started by loading up our trusty packages and data:

``` r
library(tidyverse)
library(stringr)
library(scales)

# Load data ---------------------------------------------------------------

sakura <- read.csv("~/R_materials/sakura_bloom/Kyoto_Flowers.csv")

glimpse(sakura)
```

    ## Observations: 1,235
    ## Variables: 7
    ## $ AD                        <int> 801, 802, 803, 804, 805, 806, 807, 8...
    ## $ Full.flowering.date..DOY. <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ Full.flowering.date       <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ Source.code               <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ Data.type.code            <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
    ## $ Reference.Name            <fct> -, -, -, -, -, -, -, -, -, -, -, NIH...
    ## $ X                         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, ...

From looking at the raw data, it appears that data is missing from a number of rows until **812 AD**, along with many other rows throughout the years. We might as well cut those rows out especially since we can manually specify the limits of our `year` variable when we start plotting.

``` r
# remove rows without flowering date
sakura <- sakura %>% filter(!is.na(Full.flowering.date)) %>% glimpse()
```

    ## Observations: 827
    ## Variables: 7
    ## $ AD                        <int> 812, 815, 831, 851, 853, 864, 866, 8...
    ## $ Full.flowering.date..DOY. <int> 92, 105, 96, 108, 104, 100, 106, 95,...
    ## $ Full.flowering.date       <int> 401, 415, 406, 418, 414, 409, 416, 4...
    ## $ Source.code               <int> 1, 1, 1, 1, 1, 1, 1, 5, 5, 5, 5, 5, ...
    ## $ Data.type.code            <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 3, 2, ...
    ## $ Reference.Name            <fct> NIHON-KOKI, NIHON-KOKI, NIHON-KOKI, ...
    ## $ X                         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, ...

``` r
# 12/1/2017: another way, use drop_na()

sakura %>% drop_na(Full.flowering.date) %>% glimpse()
```

    ## Observations: 827
    ## Variables: 7
    ## $ AD                        <int> 812, 815, 831, 851, 853, 864, 866, 8...
    ## $ Full.flowering.date..DOY. <int> 92, 105, 96, 108, 104, 100, 106, 95,...
    ## $ Full.flowering.date       <int> 401, 415, 406, 418, 414, 409, 416, 4...
    ## $ Source.code               <int> 1, 1, 1, 1, 1, 1, 1, 5, 5, 5, 5, 5, ...
    ## $ Data.type.code            <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 4, 3, 2, ...
    ## $ Reference.Name            <fct> NIHON-KOKI, NIHON-KOKI, NIHON-KOKI, ...
    ## $ X                         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, ...

``` r
colnames(sakura)   # look at the column names
```

    ## [1] "AD"                        "Full.flowering.date..DOY."
    ## [3] "Full.flowering.date"       "Source.code"              
    ## [5] "Data.type.code"            "Reference.Name"           
    ## [7] "X"

From looking at the column names... they don't look very neat or tidy.

``` r
colnames(sakura) <- sakura %>% 
  colnames() %>% 
  str_to_lower() %>%                  # to lower case letters
  str_replace_all("\\.", "_")         # replace . with _

colnames(sakura)
```

    ## [1] "ad"                        "full_flowering_date__doy_"
    ## [3] "full_flowering_date"       "source_code"              
    ## [5] "data_type_code"            "reference_name"           
    ## [7] "x"

Much better! However there are two columns with names that need some manual fixing using the `rename()` function:

``` r
sakura <- sakura %>% rename(year = ad, 
                            full_flowering_day_of_year = full_flowering_date__doy_)

colnames(sakura)
```

    ## [1] "year"                       "full_flowering_day_of_year"
    ## [3] "full_flowering_date"        "source_code"               
    ## [5] "data_type_code"             "reference_name"            
    ## [7] "x"

OK! Now let's separate the column containing our three-digit date data into individual `month` and `day` columns. After converting the `full_flowering_date` column into **character** format, we can use regular expressions (**regex**) to parse the character strings of the date and then separate the `month` digits and `day` digits with a `.` period symbol.

``` r
# split into two backreferences on the first digit, then place a '.' in between
date_sep <- as.character(sakura$full_flowering_date) %>% 
  str_replace_all("(.{1})(.*)", "\\1.\\2") %>% 
  as_data_frame()

colnames(date_sep)[1] <- "date_fl"                        # properly name column
colnames(date_sep)
```

    ## [1] "date_fl"

``` r
# separate into 'month' and 'day' columns by '.'
date_sep <- date_sep %>% separate(date_fl, c("month", "day"), "\\.")  
```

Now we can combine `date_sep` into `sakura` using `bind_cols()` and then remove all the variables we don't need:

``` r
sakura <- bind_cols(date_sep, sakura)   
sakura <- sakura %>% 
  select(-full_flowering_date, -full_flowering_day_of_year, 
         -x, -data_type_code, -reference_name, -source_code)  
```

Using the `make_date()` function in the `lubridate` package we can create a separate variable in YYYY/MM/DD format as well as format the individual date components into their own variables.

``` r
library(lubridate)

sakura <- sakura %>% 
  mutate(bloom = make_date(year, month, day))

# Reformat date variables into specific date formats:
sakura$Day_Of_Year <- as.numeric(format(sakura$bloom, "%j"))   #  %j: decimal day of the year
sakura$Year <- format(sakura$bloom, "%Y")                      #  %Y: 4 digit year
sakura$Month <- format(sakura$bloom, "%b")                     #  %b: abbreviated month
sakura$Day <- format(sakura$bloom, "%d")                       #  %d: decimal date

glimpse(sakura)
```

    ## Observations: 827
    ## Variables: 8
    ## $ month       <chr> "4", "4", "4", "4", "4", "4", "4", "4", "4", "4", ...
    ## $ day         <chr> "01", "15", "06", "18", "14", "09", "16", "05", "1...
    ## $ year        <int> 812, 815, 831, 851, 853, 864, 866, 869, 889, 891, ...
    ## $ bloom       <date> 0812-04-01, 0815-04-15, 0831-04-06, 0851-04-18, 0...
    ## $ Day_Of_Year <dbl> 92, 105, 96, 108, 104, 100, 106, 95, 104, 109, 108...
    ## $ Year        <chr> "0812", "0815", "0831", "0851", "0853", "0864", "0...
    ## $ Month       <chr> "Apr", "Apr", "Apr", "Apr", "Apr", "Apr", "Apr", "...
    ## $ Day         <chr> "01", "15", "06", "18", "14", "09", "16", "05", "1...

Now we are ready to plot!

``` r
ggplot(sakura, aes(x = Year, y = Day_Of_Year)) +
  geom_point() +
  scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
  scale_x_discrete(breaks = pretty_breaks(5))
```

<img src="sakura_bloom.spin_files/figure-markdown_github/basic plot-1.png" style="display: block; margin: auto;" />

Does not look very clear...

Let's add a smoother to see the overall trend over time!

``` r
ggplot(sakura, aes(x = year, y = Day_Of_Year)) + 
  geom_point() +
  geom_smooth(span = 0.2, size = 2.5) +
  scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%b-%d"),
                     limits = c(84, 125))
```

<img src="sakura_bloom.spin_files/figure-markdown_github/smoother-1.png" style="display: block; margin: auto;" />

Looks better! Now we can clearly see the trend of the sakura blooming date over time! But could we make it more... sakura-y?

``` r
ggplot(sakura, aes(x = year, y = Day_Of_Year)) +  # or just use original 'year' variable...
  geom_point(shape = 8, size = 5, color = "pink") +
  geom_smooth(span = 0.2, color = "#dd1c77", fill = "red", size = 2.5) +
  scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%b-%d"),
                     limits = c(84, 125))
```

<img src="sakura_bloom.spin_files/figure-markdown_github/sakura colors-1.png" style="display: block; margin: auto;" />

Better! But, the colors don't look good on a drab grey background... So let's add a background image! We can do this by utilizing a variety of packages such as `jpeg`, `grid`, and `gridExtra` to download an image from the internet, turn it into a raster, and then place our ggplot on top of it in a customized function!

In this custom function we will:

1.  Download an image and store it in a temporary file
2.  Save that image with the `readJPEG()` function
3.  Use `rasterGrob()` to render a raster object with the downloaded image
4.  Build up our ggplot visualization step-by-step

``` r
library(jpeg)
library(grid)
library(gridExtra)

sakura_r <- function(df = sakura, xvar = "as.numeric(Year)", yvar = "Day_Of_Year") {
  
  img_url <- "https://i.imgur.com/CgwU1zb.jpg"
  temp <- tempfile()
  download.file(img_url, temp, mode = "wb")
  img <- readJPEG(temp)
  file.remove(temp)
  
  rstr <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE)
  
  g <- ggplot(data = df) + 
    annotation_custom(rstr, -Inf, Inf, -Inf, Inf) +
    geom_point(aes_string(x = xvar, y = yvar), alpha = 0.8, color = "pink", shape = 8) +
    geom_smooth(aes_string(x = xvar, y = yvar), 
                alpha = 0.7, color = "#dd1c77", span = 0.2, size = 2.5, fill = "#f768a1") +
    scale_y_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) +
    scale_x_continuous(limits = c(800, 2020), breaks = seq(800, 2000, 200)) +
    labs(x = "Year", y = "Date of peak sakura bloom") +
    ggtitle("Sakura blooming", subtitle = "Date of sakura blossoming in Kyoto (800-2015 CE)") +
    theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 11))
  
  return (g)
  
}

sakura_r()
```

<img src="sakura_bloom.spin_files/figure-markdown_github/background image-1.png" style="display: block; margin: auto;" />

Normally, you would **never** add in a background image to a plot as it would obscure the individual data points and other details.

However, for this graph the main point of interest is the trend line going across and decreasing from the 20th century, pointing out how sakura have been steadily blossoming earlier and earlier in the year which can be clearly seen despite the background image.

I mainly wanted to show a cool new trick I learned and to also do something *slightly* different from what *The Economist* did. Some other cool uses of Dr. Aono's sakura data set include [this](http://opiateforthemass.es/articles/sakura/) visualization done by Christoph Safferling from *Opiate for the masses* where they incorporated sakura emoji as the data point shapes; definitely much better than the ersatz `geom_point` shape I used as "sakura" for my own plot!

To cap off, I'll leave you with some photos I took of sakura in Tokyo when I went back home for Spring Break back in April! Can't wait for the blossoming next year!

![sakura1](https://i.imgur.com/mGb9bms.jpg%20=50X10) ![sakura2](https://i.imgur.com/i2z7d4y.jpg%20=30x20) ![sakura3](https://i.imgur.com/fXEmtzx.jpg%20=100x30)
