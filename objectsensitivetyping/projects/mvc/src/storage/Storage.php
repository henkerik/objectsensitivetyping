<?php

abstract class Storage {
    
    protected $context 	= null;
    
    protected $data = array();
    

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

    public abstract function getAttribute ($name, $ns = null);

    public abstract function setAttribute ($name, $value, $ns = null);

    public abstract function hasAttribute ($name, $ns = null);
    
    public abstract function removeAttribute ($name, $ns = null);
}

?>