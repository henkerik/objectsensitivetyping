<?php

class X {
    function foo ($x)
    {
        return $x;
    }
}

$a = true;
$x = new X ();
$y = $x->foo (false);
$b = $a;

?>