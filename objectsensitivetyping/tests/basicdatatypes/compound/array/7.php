<?php

error_reporting(E_ALL ^ E_NOTICE);

$a = 1;
$b[1][1] = $a;
$c = $b[1][1];

?>