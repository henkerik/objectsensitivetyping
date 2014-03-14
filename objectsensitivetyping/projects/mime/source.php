<?php

/******************************************************************************
 * Collection Classes                                                         *
 *****************************************************************************/


interface AbstractList {
	
	public function add ($item);
	
	public function addList ($list);
	
	public function contains ($item);
		
	public function isEmpty ();
	
	public function remove ($remove);
	
	public function length ();
	
	public function index ($item);
}

class ArrayList implements AbstractList {
	
	protected $list;
	
	public function __construct ()
	{
	    $this->list = array ();
	}
	
	public function add ($item)
	{
		$this->list[$this->length()] = $item;
	}
	
	public function get ($index)
	{
	    return $this->list[$index];
	}
	
	public function addList ($list)
	{
	    for ($i = 0; $i < $list->length(); $i++) 
	        $this->add($list->get($i));
	}
	
	public function contains ($item)
	{    
	    for ($i = 0; $i < $list->length(); $i++) 
	        if ($item === $list->get ($i))
	            return true;
	            
		return false;
	}
	
	public function remove ($remove)
	{
	    $new = array();
	    
	    for ($i = 0; $i < $this->length(); $i++) 
	        if ($remove != $this->get($i))
	            $new[sizeof($new)] = $this->get($i);
	           
        $this->list = $new;
	}
	
    
    public function isEmpty ()
    {
        return sizeof ($this->list) == null;   
    }
    
    public function join ($separator)
    {
        return join($separator, $this->list);
    }
    
    public function index ($item) 
    {
        for ($i = 0; $i < $this->length (); $i++)
            if ($this->list[$i] === $item)
                return $i;
    }
    
    public function length ()
    {
        return sizeof ($this->list);
    }
}

/******************************************************************************
 * Mailable and exceptions                                                    *
 *****************************************************************************/


interface Mailable {

    public function getToAddresses ();
    
    public function getSubject ();
    
    public function build ();
    
}

class MailException extends Exception { }

/******************************************************************************
 * Mime messages                                                              *
 *****************************************************************************/
 
interface Encoding {

    public function getType ();
    
    public function encode ($content);

}

class Base64Encoding implements Encoding {

    protected function __construct () { }
    
    protected static $instance;
    
    public static function getInstance ()
    {
        if (self::$instance == null) {
            self::$instance = new Base64Encoding ();
        }
        
        return self::$instance;
    }

    public function getType ()
    {
        return 'base64';
    }

    public function encode ($content)
    {
        return rtrim (chunk_split (base64_encode ($content), 76, MimeMail::LINE_BREAK_SEQUENCE));
    }
}

class SevenBitEncoding implements Encoding {

    protected function __construct () { }
    
    protected static $instance;
    
    public static function getInstance ()
    {
        if (self::$instance == null) {
            self::$instance = new SevenBitEncoding ();
        }
        
        return self::$instance;
    }

    public function getType ()
    {
        return '7bit';
    }

    public function encode ($content)
    {
        return $content;
    }

}
 
class MimeEntity {

    protected $contentType;
    protected $contentTransferEncoding;
    protected $contentDisposition;
    protected $contentID;
    protected $content;

    public function __construct ($content, $contentType, $contentTransferEncoding, $contentDisposition, $contentID)
    {
        $this->content = $content;
        $this->contentType = $contentType;
        $this->contentTransferEncoding = $contentTransferEncoding;
        $this->contentDisposition = $contentDisposition;
        $this->contentID = $contentID;
    }

    public function build ()
    {
        $headers  = isset ($this->contentType) ? 'Content-Type: ' . $this->contentType . MimeMail::LINE_BREAK_SEQUENCE : ''; 
        $headers .= isset ($this->contentTransferEncoding) ? 'Content-Transfer-Encoding: ' . $this->contentTransferEncoding->getType () . MimeMail::LINE_BREAK_SEQUENCE : ''; 
        $headers .= isset ($this->contentDisposition) ? 'Content-Disposition: ' . $this->contentDisposition . MimeMail::LINE_BREAK_SEQUENCE : '';
        $headers .= isset ($this->contentID) ? 'Content-ID: ' . $this->contentID . MimeMail::LINE_BREAK_SEQUENCE : '';

        $headers .= MimeMail::LINE_BREAK_SEQUENCE;
        $headers .= $this->contentTransferEncoding->encode ($this->content); 
        $headers .= MimeMail::LINE_BREAK_SEQUENCE  . MimeMail::LINE_BREAK_SEQUENCE;
        
        return $headers;
    }

}

class CompositeMimeEntity extends MimeEntity {

    protected $children;
    
    protected $boundary;

    public function __construct ()
    {
        $this->content = 'This part of the E-mail should never be seen. If you are reading this, consider upgrading your e-mail client to a MIME-compatible client.';
        $this->children = new ArrayList ();
        $this->boundary = 'XX' . chr (rand (65, 90)) . '------' . md5 (uniqid (rand (0, getrandmax()), false), false);
    }

    
    public function add ($mimeEntity)
    {
        $this->children->add ($mimeEntity);
    }

    public function getBoundary ()
    {
        return $this->boundary;
    }


    public function build ()
    {
        $headers = parent::build ();
        
        for ($i = 0; $i < $this->children->length(); $i++) {
            $headers .= '--' . $this->getBoundary () . MimeMail::LINE_BREAK_SEQUENCE;
            $headers .= $this->children->get($i)->build ();
        }

        $headers .= '--' . $this->getBoundary () . '--' . MimeMail::LINE_BREAK_SEQUENCE;

        return $headers;
    }
}

class AlternativeMimeEntity extends CompositeMimeEntity {

    public function __construct ()
    {
        parent::__construct ();

        $this->contentType = sprintf ('multipart/alternative; boundary=%s', $this->getBoundary ());
        $this->contentTransferEncoding = SevenBitEncoding::getInstance ();
    }

}

class MixedMimeEntity extends CompositeMimeEntity {

    public function __construct ()
    {
        parent::__construct ();

        $this->contentType = sprintf ('multipart/mixed; boundary=%s', $this->getBoundary ());
        $this->contentTransferEncoding = SevenBitEncoding::getInstance ();
    }

}

class RelatedMimeEntity extends CompositeMimeEntity {

    public function __construct ()
    {
        parent::__construct();
        
        $this->contentType = sprintf ('multipart/related; boundary=%s', $this->getBoundary ());
        $this->contentTransferEncoding = SevenBitEncoding::getInstance ();
    }

}

class MimeMail implements Mailable {

    const LINE_BREAK_SEQUENCE = '\n';
    
    protected $rootMimeEntity = null;

    protected $subject;

    protected $fromAddress;

    protected $version = '1.0';

    protected $toAddresses;
    
    protected $ccAddresses;
    
    protected $bccAddresses;

    public function __construct ($fromAddress, $subject)
    {
        $this->fromAddress = $fromAddress;
        $this->subject = $subject;
        
        $this->toAddresses = new ArrayList();
        $this->ccAddresses = new ArrayList();
        $this->bccAddresses = new ArrayList();
    }

    public function getVersion ()
    {
        return $this->version;
    }

    protected function getDate ()
    {
        return date ('r', time());
    }

    public function hasFromAddress ()
    {
        return (($this->fromAddress) ? true : false);
    } 
    public function setFromAddress ($fromAddress)
    {
        $this->fromAddress = $fromAddress;
    }

    public function setSubject ($subject)
    {
        $this->subject = $subject;
    }
    
    public function hasSubject ()
    {
        return (($this->subject) ? true : false);
    } 

    public function getSubject ()
    {
        return $this->subject;
    }
    
    public function addToAddress ($address)
    {
        $this->toAddresses->add($address);
    }

    
    public function removeToAddresses ()
    {
        $this->toAddresses = new ArrayList ();
    }
    
    public function getToAddresses ()
    {
        return $this->toAddresses;
    }

    public function addCcAddress ($address)
    {
        $this->ccAddresses->add($address);
    }

    public function addBccAddress ($address) {
        $this->bccAddresses->add($address);
    }


    public function setRootMimeEntity ($rootMimeEntity)
    {
        $this->rootMimeEntity = $rootMimeEntity;
    }
    
    public function hasRootMimeEntity ()
    {
        return (($this->rootMimeEntity) ? true : false);
    } 

    public function build ()
    {        
        if (!$this->hasFromAddress ())
            throw new MailException ('The from address doesn\'t exists.');

        if (!$this->hasSubject ()) 
            throw new MailException ('The subject doesn\'t exists.');
        
        if ($this->toAddresses->isEmpty () && $this->ccAddresses->isEmpty () && $this->bccAddresses->isEmpty ()) 
            throw new MailException ('There is no valid to, cc or bcc address');
        
        if (!$this->hasRootMimeEntity ()) 
            throw new MailException ('The root mime entity doesn\'t exists.');
        
        $headers  = 'MIME-Version: ' . 	$this->getVersion () . MimeMail::LINE_BREAK_SEQUENCE;
        $headers .= 'From: ' . 			$this->fromAddress . MimeMail::LINE_BREAK_SEQUENCE;

        if (!$this->toAddresses->isEmpty ()) {
            $toAddresses = array ();
            for ($i = 0; $i < $this->toAddresses->length(); $i++)
                $toAddresses[sizeof($toAddresses)] = $this->toAddresses->get($i);
            
            $headers .= 'To: ' . implode (', ', $toAddresses) . MimeMail::LINE_BREAK_SEQUENCE;
        }
        
        if (!$this->ccAddresses->isEmpty ()) {
            $ccAddresses = array ();
            for ($i = 0; $i < $this->ccAddresses->length(); $i++)
                $ccAddresses[sizeof($ccAddresses)] = $this->ccAddresses->get($i);
        
            $headers .= 'Cc: ' . implode (', ', $ccAddresses) . MimeMail::LINE_BREAK_SEQUENCE;
        }
        
        if (!$this->bccAddresses->isEmpty ()) {
            $bccAddresses = array ();
            for ($i = 0; $i < $this->bccAddresses->length(); $i++)
                $bccAddresses[sizeof($bccAddresses)] = $this->bccAddresses->get($i);
        
            $headers .= 'Bcc:  ' . implode (', ', $bccAddresses) . MimeMail::LINE_BREAK_SEQUENCE;
        }
            
        $headers .= 'Date: ' . $this->getDate () . MimeMail::LINE_BREAK_SEQUENCE;
        $headers .= 'Subject: ' . $this->subject . MimeMail::LINE_BREAK_SEQUENCE;
        $headers .= $this->rootMimeEntity->build();
        

        return $headers;
    }
}

/******************************************************************************
 * Mail Transport                                                             *
 *****************************************************************************/
 
interface MailTransport {
	public function open ();
	
	public function send ($mailable);
	
	public function close ();
}

class NativeMailTransport implements MailTransport {

	public function open () { }
	
	public function send ($mailable)
	{
		$headers = $mailable->build ();
		
		// Implode to addresses
		$toAddresses = array ();
		for ($i = 0; $i < $mailable->getToAddresses()->length(); $i++)
		  $toAddresses[sizeof($toAddresses)] = $mailable->getToAddresses()->get($i);
		
		// Send email
		if (!mail (implode (', ', $toAddresses), $mailable->getSubject (), '', $headers, ''))
		    throw new MailException ('Failed to send email');
	}

	public function close () { }

}

$root = new MixedMimeEntity ();
$root->add (new MimeEntity (file_get_contents ('test.jpg'), 'image/jpg', Base64Encoding::getInstance (), 'attachment', NULL));
$root->add (new MimeEntity ('Content', 'text/plain; charset=utf-8', Base64Encoding::getInstance (), NULL, NULL));

$mail = new MimeMail ('mail@henkerikvanderhoek.nl', 'Subject');
$mail->addToAddress ('mail@henkerikvanderhoek.nl');
$mail->setRootMimeEntity ($root);

try { 
    $transport = new NativeMailTransport ();
    $transport->open ();
    $transport->send ($mail);
    $transport->close ();
} catch (MailException $e) {
    
}

?>