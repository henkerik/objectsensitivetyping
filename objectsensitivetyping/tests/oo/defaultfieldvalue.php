<?php

class X {
    public $foo = "test";
    
    public $bar;
    
    public $var;
}

$x = new X();
$a = $x->foo;
$b = $x->bar;

if (false) {
    $x->var = true;
}

$c = $x->var;

?>