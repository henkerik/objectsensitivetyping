<?php

class MyException extends Exception { }

class Test {
    public function testing() 
    {
        try {
            try {
                throw new MyException();
            } catch (MyException $e) {
                // rethrow it 
                throw $e;
            }
        } catch (MyException $e) {
            $m = $e->getMessage();
        }
    }
}

$foo = new Test();
$foo->testing();

?>