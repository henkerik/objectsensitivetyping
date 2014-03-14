<?php

error_reporting(E_ALL ^ E_NOTICE);

$y->foo = $x;
$z =& $y->foo;
$z = true;

$a = $y->foo;
$b = $z;
$c = $x;

?>