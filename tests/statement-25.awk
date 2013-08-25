#!/usr/bin/gawk -f
BEGIN {
    x = 4
    x /= 2    # comment
    y = 3
    print x, y
}

$1 ~ "string \\
x = \
4  \

/regexp/ foo = "string \
"                               # 

/regexp \\
x = 4                           #

 \
/regexp   