<?php

error_reporting(E_ALL ^ E_NOTICE);

// Default Index

$idx1 = "a" . "b";
$idx2 = "a" . "c";

$a[$idx1] = true;
$x = $a[$idx2];
?>