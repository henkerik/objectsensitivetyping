<?php

$a1 = true;
$a2 = true;
if ($a1) {
    $b = 2;
} elseif ($a2) {
    $b = "hi";
} else {
    $b = false;
}

$a1 = false;
$a2 = true;
if ($a1) {
    $b = 2;
} elseif ($a2) {
    $b = "hi";
} else {
    $b = false;
}

$a1 = false;
$a2 = false;
if ($a1) {
    $b = 2;
} elseif ($a2) {
    $b = "hi";
} else {
    $b = false;
}

$c = $b;

?>
