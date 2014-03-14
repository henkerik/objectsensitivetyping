<?php

require_once 'collection/ArrayMap.php';

class Template
{
    protected $template;
    
    protected $attributes;
    
    public function Template($template) {
        $this->setTemplate($template);
        $this->setAttributes(new ArrayMap());  
    }
    
    public function setTemplate($template) {
        if (is_readable ($template)) {
            $this->template = $template;
        } else {
            $message = 'The given template doesn\'t exist: ' . $template;

            throw new FileNotFoundException ($message);
        }   
    }
    
    public function getTemplate() {
        return $this->template;   
    }
    
    public function setAttribute($key, $value) {
        $this->attributes->put($key, $value);   
    }
    
    public function getAttribute($key) {
        return $this->attributes->get($key);        
    }

    public function getAttributes() {
        return $this->attributes;   
    }
    
    public function setAttributes(Map $attributes) {
        $this->attributes = $attributes;   
    }
    
    public function toString() {
        // Make attributes accessible in the template
        foreach ($this->getAttributes() as $key => $value)
            $$key = $value;
        
        // render to variable
        ob_start();

        require($this->getTemplate());

        $content = ob_get_contents();

        ob_end_clean();
        
        return $content;
    }
    
}

?>