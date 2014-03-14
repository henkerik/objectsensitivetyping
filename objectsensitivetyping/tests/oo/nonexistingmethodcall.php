<?php

class X {
    function x () {
        return 1;
    }
}

class Y { 
    function y () {
        return true;
    }
}

$cond = true;

if ($cond) {
    $a = new X ();
} else {
    $a = new Y ();
}

if ($cond) {
    $b = $a->x();
} else {
    $b = $a->y();
}

$c = $b;

?>