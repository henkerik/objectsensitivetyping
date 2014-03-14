<?php

class FilterChain {
    protected $chain = array ();
    protected $index = -1;
    protected $context = null;

    public function initialize ($context)
    {
        $this->context = $context;
    }

    public function register (Filter $filter)
    {
        $filter->initialize ($this->context);
        $this->chain[] = $filter;
    }


    public function execute (Action $action, Request $request)
    {
        $this->index++;

        if ($this->index < count($this->chain)) {

            $this->chain[$this->index]->execute($this, $action, $request);

        }
    }

    public function reset ()
    {
        $this->index = -1;
    }
}

?>