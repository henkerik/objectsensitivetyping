<?php

$x = 1;
$y = false;

if (false) {
    $r =& $x;
} else {
    $r =& $y;
}

$r = "foo";
$s =& $r;

$s = 0;

$a = $r; // Should be "foo"
$b = $s; // Should be 0
$c = $x; // Could be both 1 and "foo"
$d = $y; // Could be both false and "foo"

?>