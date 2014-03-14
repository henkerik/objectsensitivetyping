<?php

class ChildException extends Exception { }

try {
    throw new ChildException ();
} catch (Exception $e) {
    $m = $e->getMessage ();
}

?>