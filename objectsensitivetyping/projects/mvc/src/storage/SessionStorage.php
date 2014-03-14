<?php

require_once 'mvc/storage/Storage.php';

class SessionStorage extends Storage
{
    const GLOBAL_NAMESPACE = 'global';
    
    public function __construct ()
    {
        // Start a session if not present
        if(!session_id ()) {
            session_start ();
        }
    }

    public function getAttribute ($name, $ns = null)
    {
        if ($this->hasAttribute ($name, $ns)) {
            if ($ns == null)
                $ns = SessionStorage::GLOBAL_NAMESPACE;
            
            return $_SESSION[$ns][$name];
        } else {
            return null;
        }
    }

    public function setAttribute ($name, $value, $ns = null)
    {
        if ($ns == null)
            $ns = SessionStorage::GLOBAL_NAMESPACE;
            
        $_SESSION[$ns][$name] = $value;
    }

    public function hasAttribute ($name, $ns = null)
    {
        if ($ns == null)
            $ns = SessionStorage::GLOBAL_NAMESPACE;
        
        return isset ($_SESSION[$ns][$name]);
    }
    
    public function removeAttribute($name, $ns = null)
    {
        if ($ns == null)
            $ns = SessionStorage::GLOBAL_NAMESPACE;
        
        unset($_SESSION[$ns][$name]);   
    }
}

?>