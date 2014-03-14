<?php

abstract class XslView extends View {
    
    protected $template = null;
    
    /**
     *  Deze code staat ook in PHP View, moet naar View.
     *  Enigste verschil is de bestands exstensie
     */
    public function setTemplate ($template)
    {
        // Look if this template exists.
        if (is_readable ($file = (NXT_MODULES_DIR . $this->moduleName . '/'. $this->actionName . '/templates/' . $template))) {
            $this->template = $file;
        } elseif (is_readable ($template)) {
            $this->template = $template;
        } else {
            $message = 'The given template doesn\'t exist: ' . $template;
            
            throw new FileNotFoundException ($message);
        }
    }

    protected function getTemplate ()
    {
        if (!$this->template) {

            $actionName     = $this->getActionName ();
            $viewName       = $this->getViewName ();

            $template = $actionName . $viewName . 'Template.xsl';
            $this->setTemplate ($template);
        }

        return $this->template;
    }
    
    public function render (Request $request)
    {

        $document = $request->getAttribute ('document');
        
        // Look if there exists a javascript file for this actions
        if (is_readable ($file = (NXT_MODULES_DIR . $this->getModuleName() . '/' . $this->getActionName() . '/static/js/' . $this->getActionName() . 'Action.js'))) {

            $javascriptFiles = $request->getAttribute ('javascriptFiles');
            if ($javascriptFiles == null) {
                $javascriptFiles = new ArrayList ();
                $request->setAttribute('javascriptFiles', $javascriptFiles);            
            }
                
            if (!$javascriptFiles->contains('/' . $file))
               $javascriptFiles->add('/' . $file);
        }
        
        $xsl = new DOMDocument ();
        $xsl->load($this->getTemplate ());
        $xsl->xinclude();
        
        $proc = new XSLTProcessor ();
        $proc->importStyleSheet($xsl);
        
        foreach ($request->getAttributes () as $key => $value)
        {
            if (is_string ($value)) 
                $proc->setParameter ('', $key, $value);
        }

        
        $time_start = microtime(true);
        
        $content = $proc->transformToXml($document);
        
        $time_end = microtime(true);
        $time = $time_end - $time_start;
        
        
        $request->setAttribute ('xsltExecutionTime', $time);
        
        $request->setAttribute ('content', $content);
        
        // Look if we need to decorate the output
        if ($this->isDecorated ()) {
            $controller = $this->getContext()->getController();
            $decorator = $controller->getDecorator ($this->decorator);
            $decorator->execute ($request);
        
            // Return the decorated output          
            return $decorator->render ($request);
        } else {
            // Return the undecorated output
            return $content;
        }
    }    
}

// Private class 
/*
class VariableStream {
    var $position;
    var $varname;

    function stream_open($path, $mode, $options, $opened_path) 
    {
        $url = parse_url($path);
        $this->varname = $url["host"];
        $this->position = 0;
        return true;
    }
    
    function stream_read($count) 
    {
        $ret = substr($GLOBALS[$this->varname], $this->position, $count);
        $this->position += strlen($ret);
        return $ret;
    }
    
    function stream_eof() 
    {
        return $this->position >= strlen($GLOBALS[$this->varname]);
    }
    
    function url_stat() 
    {
        return array();
    }
}
*/
?>