<?php

class Log {

    protected static $instance = null;
    
    protected $debugMode;
    
    protected function __construct () 
    {
        $this->debugMode = true;
    }
    
    /**
     * Singleton
     *
     * @return Log
     */
    public static function getInstance ()
    {
        if (self::$instance == null) {
            self::$instance = new Log ();
        }
        return self::$instance;
    }
    
    
    public function debug ($message)
    {
        if ($this->debugMode)   
            echo $message . '<br>';
    }
    
    public function write ($message)
    {
        echo $message;
    }
}    
?>