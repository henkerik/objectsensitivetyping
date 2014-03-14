<?php

class X {
    function x () {
        return 1;
    }
}

$cond = true;

if ($cond) {
    $a = new X ();
} else {
    $a = array ("foo");
}

if ($cond) {
    $b = $a->x();
} else {
    $b = $a[0];
}

$c = $b;

?>