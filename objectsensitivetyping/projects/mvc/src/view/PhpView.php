<?php

/**
 *
 * The PhpView class is a usable subclass of View. The PhpView class uses PHP
 * it self as an template engine.
 *
 *
 * @author Henk Erik van der Hoek (mail@henkerikvanderhoek.nl)
 * @author Jorgen Horstink (mail@jorgenhorstink.nl)
 *
 * @version 1.0
 * @package mvc
 * @subpackage view
 */

require_once 'collection/ArrayList.php';

abstract class PhpView extends View {

    // +-----------------------------------------------------------------------+
    // | PRIVATE ATTRIBUTES                                                     |
    // +-----------------------------------------------------------------------+

    protected $template = null;



    // +-----------------------------------------------------------------------+
    // | METHODS                                                               |
    // +-----------------------------------------------------------------------+

 
    public function setTemplate ($template)
    {
		// Look if this template exists.
        if (is_readable ($file = (NXT_MODULES_DIR . $this->moduleName . '/'. $this->actionName . '/templates/' . $template))) {
            $this->template = $file;
        } elseif (is_readable ($file = (NXT_MODULES_DIR . $this->moduleName . '/templates/' . $template))) {
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

            $actionName 	= $this->getActionName ();
            $viewName 		= $this->getViewName ();

            $template = $actionName . $viewName . 'Template.php';
			$this->setTemplate ($template);
        }

        return $this->template;
    }


    // -------------------------------------------------------------------------


    public function render (Request $request)
    {
        if (!defined(strtoupper($this->getModuleName()) . '_TEMPLATES_DIR')) {
            define (strtoupper($this->getModuleName()) . '_TEMPLATES_DIR', NXT_MODULES_DIR . $this->getModuleName() . '/templates/');
        }

        $localTemplateDir = strtoupper ($this->getModuleName ()) . '_' . $this->getActionName() . '_TEMPLATES_DIR'; 
        set_include_path(get_include_path() . PATH_SEPARATOR . $localTemplateDir);
        
		// Make attributes accessible in the template
        foreach ($request->getAttributes () as $key => $value)
        	$$key = $value;


        // Make the error messages accesible in the template
        $errorMessages = $request->getErrorMessages ();

        // Make the isAuthenticated status accessible in the template
        $user = $this->getContext()->getUser ();

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
        
        // render to variable
        ob_start();

        require($this->getTemplate());

        $content = ob_get_contents();

        ob_end_clean();
        
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

    function addJavascriptFile ($javascriptFile)
    {
        if ($this->hasAttribute ('javascriptFiles'))
            $javascriptFiles = $this->getAttribute ('javascriptFiles');

        $javascriptFiles[] = $javascriptFile;
        $this->setAttribute ('javascriptFiles', $javascriptFiles);
    }
}

?>