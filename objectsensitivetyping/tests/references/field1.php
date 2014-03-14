<?php

$x = 0;
$y->foo = $x;
$z =& $y->foo;
$z = true;

$a = $y->foo;
$b = $z;

?>