<?php

error_reporting(E_ALL ^ E_NOTICE);

$x = true;
$y->foo = $x;

$a = $y->foo;
?>