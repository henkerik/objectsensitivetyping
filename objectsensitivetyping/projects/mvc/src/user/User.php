<?php

abstract class User {
    protected $context = null;

    
    public function initialize (Context $context)
    {
        $this->context = $context;
    }

    /**
     * Returns a reference to the context
     * 
     * @return Context
     */
    protected function getContext ()
    {
        return $this->context;
    }

    /**
     * Attributes in the User object are kept in a session. They will 
     * be available in a next http request.
     *
     * @param string $name
     * @param object $value
     */
    public function setAttribute($name, $value, $ns = 'application')
    {
        $storage = $this->getContext()->getStorage ();
        $storage->setAttribute ($name, $value, $ns);
    }

    public function getAttribute($name, $ns = 'application')
    {
        $storage = $this->getContext()->getStorage ();
        
        return $storage->getAttribute ($name, $ns);
    }
    
    public function removeAttribute($name, $ns = 'application')
    {
        $storage = $this->getContext()->getStorage ();
        
        return $storage->removeAttribute ($name, $ns);
    }

    public function hasAttribute ($name, $ns = 'application')
    {
        $storage = $this->getContext()->getStorage ();
        
        return $storage->hasAttribute ($name, $ns);
    }

}

?>