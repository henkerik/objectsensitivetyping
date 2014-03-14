<?php

error_reporting(E_ALL ^ E_NOTICE);

$a = array();
$i = 0;
while ($i < 10) {
    $a[1] = $a;
    $i++;
}



?>