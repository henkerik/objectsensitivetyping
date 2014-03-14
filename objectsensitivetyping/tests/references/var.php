<?php

$x = 1;
$y =& $x;

$x = true;
$a = $x;
$b = $y;

$a = "foo";

$c = $a;
$d = $b;


?>