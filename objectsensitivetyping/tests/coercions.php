<?php

$string = "foo";
$int    = 42;
$double = 1.3;
$bool   = true;
$null   = null;

$a = (int) $string;
$b = (int) $int;
$c = (int) $double;
$d = (int) $bool;
$e = (int) $null;

$f = (string) $string;
$g = (string) $int;
$h = (string) $double;
$i = (string) $bool;
$j = (string) $null;

$k = (double) $string;
$l = (double) $int;
$m = (double) $double;
$n = (double) $bool;
$o = (double) $null;

$p = (bool) $string;
$q = (bool) $int;
$r = (bool) $double;
$s = (bool) $bool;
$t = (bool) $null;


?>