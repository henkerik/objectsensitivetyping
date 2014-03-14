<?php

abstract class Request {
	
    /**
	 * @var Map
	 */
	protected $attributes;
	
	/**
	 * @var Map
	 */
	protected $errorMessages;
	
	protected $moduleName;
	
	protected $actionName;
    
    protected $requestMethod;

	public function __construct ()
	{
		$this->attributes = new ArrayMap();
		$this->errorMessages = new ArrayMap();
	}
	
	public function getModuleName ()
    {
		return $this->moduleName;
    }
    
    public function getActionName ()
    {
		return $this->actionName;
    }

	public function setAttribute($name, $value) 
	{
    	$this->attributes->put ($name, $value);
  	}

	public function getAttribute($name) 
	{
        $parts = explode(".", $name);
        
        if (sizeOf($parts) == 1) {
            return $this->attributes->get ($name);
        } else {
            // it is an array
            $attribute = $this->attributes->get ($parts[0]);
            return isset($attribute[$parts[1]]) ? $attribute[$parts[1]] : null;  
        }

	}
	
	public function hasAttribute ($name)
	{
		return $this->attributes->containsKey ($name);
	}
	
	public function getAttributes ()
	{
		return $this->attributes;
	}
	

	/*
		ErrorMessages
	*/

	/**
	 * @return Map
	 */
	public function getErrorMessages ()
	{
		return $this->errorMessages;
	}

	
	public function addErrorMessage ($fieldName, $message)
	{
	    $this->errorMessages->put($fieldName, $message);
	}

	public function hasErrorMessages ()
	{
	    return !$this->errorMessages->isEmpty();
	}

    function getRequestMethod ()
    {
        return $this->requestMethod;
    }

    function setRequestMethod ($requestMethod)
    {
        $this->requestMethod = $requestMethod;
    }
    
    // TODO: iewl willen we dit hier?
    public function addJavascriptFile($file)
    {   
        if (!$this->hasAttribute('javascriptFiles')) {
            $javascriptFiles = new ArrayList();   
        } else {
            $javascriptFiles = $this->getAttribute('javascriptFiles');   
        }

        $javascriptFiles->add($file);

        $this->setAttribute('javascriptFiles', $javascriptFiles);
    }
}

?>