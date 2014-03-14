<?php

error_reporting(E_ALL ^ E_NOTICE);
    
if (true) {
    $x = 1;
} else {
    $y = true;
}

$z = $x;
$w = $y;

//-------

if (false) {
    $a = 1;
} else {
    $b = true;
}

$c = $a;
$d = $b;

?>