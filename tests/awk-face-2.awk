BEGIN { a = 1 }
/a/ { print /abc/ }
/a/ { print (/abc/) /a/ a }
/a/ { print /abc/ /a/ a }
/a/ {print (/abc/) /a/}
/a/ { p /a/ a}
function foo() {
    switch (a) {
    case 1:
    case /a/:
        return /xyz/
    default:
        return bar /a/ a
    }
    if (/xyz/)
        a = 1
    if ("xyz" ~ /xyz/)
        a = 1
}
NF {
    /xyz/
}
