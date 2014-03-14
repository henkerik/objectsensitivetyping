<?php

class X {
    function foo ()
    {
        return true;
    }
}

$x = new X();
$a = $x->foo();
$b = $a;

?>