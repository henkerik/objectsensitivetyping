<?php

class Point {
    public $x;
    public $y;
}

class X { 
    function foo ($x, $y) 
    {
        return $x + $y;
    }
}

$p = new Point();
$p->x = 1;
$p->y = 0;

$x = new X();
$a = $x->foo ($p->x, $p->y);

//var_dump ($a);

?>