<?php

function gcd ($a, $b)
{
    if ($b == 0) {
        return $a;
    } else {
        return $a; //gcd ($b, $a % $b);
    }
}

$a = gcd (24, 8);

$x = true;
?>