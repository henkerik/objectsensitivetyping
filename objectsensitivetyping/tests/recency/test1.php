<?php

class X {
    var $foo;
}

$i = 0;
$x = null;
while ($i < 3) {
    $z = &$x;
    $x = new X();
    $x->foo = $i++;
}

$y = $x;

$y->foo = true;

$a = $z->foo;

?>