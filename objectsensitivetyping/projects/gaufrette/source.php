<?php

/**
 * Interface for the filesystem adapters
 *
 * @author Antoine Hérault <antoine.herault@gmail.com>
 * @author Leszek Prabucki <leszek.prabucki@gmail.com>
 */
interface Adapter
{
    /**
     * Reads the content of the file
     *
     * @param string $key
     *
     * @return string|boolean if cannot read content
     */
    public function read($key);

    /**
     * Writes the given content into the file
     *
     * @param string $key
     * @param string $content
     *
     * @return integer|boolean The number of bytes that were written into the file
     */
    public function write($key, $content);

    /**
     * Indicates whether the file exists
     *
     * @param string $key
     *
     * @return boolean
     */
    public function exists($key);

    /**
     * Returns an array of all keys (files and directories)
     *
     * @return array
     */
    public function keys();

    /**
     * Returns the last modified time
     *
     * @param string $key
     *
     * @return integer|boolean An UNIX like timestamp or false
     */
    public function mtime($key);

    /**
     * Deletes the file
     *
     * @param string $key
     *
     * @return boolean
     */
    public function delete($key);

    /**
     * Renames a file
     *
     * @param string $sourceKey
     * @param string $targetKey
     *
     * @return boolean
     */
    public function rename($sourceKey, $targetKey);

    /**
     * Check if key is directory
     *
     * @param string $key
     *
     * @return boolean
     */
    public function isDirectory($key);
}

/**
 * Points to a file in a filesystem
 *
 * @author Antoine Hérault <antoine.herault@gmail.com>
 */
class File
{
    protected $key;
    protected $filesystem;

    /**
     * Content variable is lazy. It will not be read from filesystem until it's requested first time
     * @var content
     */
    protected $content = null;

    /**
     * @var array metadata in associative array. Only for adapters that support metadata
     */
    protected $metadata = null;

    /**
     * Human readable filename (usually the end of the key)
     * @var string name
     */
    protected $name = null;

    /**
     * File size in bytes
     * @var int size
     */
    protected $size = 0;

    /**
     * File date modified
     * @var int mtime
     */
    protected $mtime = null;

    /**
     * Constructor
     *
     * @param string     $key
     * @param Filesystem $filesystem
     */
    public function __construct($key, Filesystem $filesystem)
    {
        $this->key = $key;
        $this->name = $key;
        $this->filesystem = $filesystem;
    }

    /**
     * Returns the key
     *
     * @return string
     */
    public function getKey()
    {
        return $this->key;
    }

    /**
     * Returns the content
     *
     * @throws Gaufrette\Exception\FileNotFound
     *
     * @param  array  $metadata optional metadata which should be send when read
     * @return string
     */
    public function getContent($metadata = array())
    {
        if (isset($this->content)) {
            return $this->content;
        }
        $this->setMetadata($metadata);

        return $this->content = $this->filesystem->read($this->key);
    }

    /**
     * @return string name of the file
     */
    public function getName()
    {
        return $this->name;
    }

    /**
     * @return int size of the file
     */
    public function getSize()
    {
        if ($this->size) {
            return $this->size;
        }

        try {
            return $this->size = Size::fromContent($this->getContent());
        } catch (FileNotFound $exception) {
        }

        return 0;
    }

    /**
     * Returns the file modified time
     *
     * @return int
     */    
    public function getMtime()
    {
        return $this->mtime = $this->filesystem->mtime($this->key);
    }

    /**
     * @param int size of the file
     */
    public function setSize($size)
    {
        $this->size = $size;
    }

    /**
     * Sets the content
     *
     * @param string $content
     * @param array  $metadata optional metadata which should be send when write
     *
     * @return integer The number of bytes that were written into the file, or
     *                 FALSE on failure
     */
    public function setContent($content, $metadata = array())
    {
        $this->content = $content;
        $this->setMetadata($metadata);

        return $this->size = $this->filesystem->write($this->key, $this->content, true);
    }

    /**
     * @param string name of the file
     */
    public function setName($name)
    {
        $this->name = $name;
    }

    /**
     * Indicates whether the file exists in the filesystem
     *
     * @return boolean
     */
    public function exists()
    {
        return $this->filesystem->has($this->key);
    }

    /**
     * Deletes the file from the filesystem
     *
     * @throws Gaufrette\Exception\FileNotFound
     * @throws \RuntimeException                when cannot delete file
     * @param  array                            $metadata optional metadata which should be send when write
     * @return boolean                          TRUE on success
     */
    public function delete($metadata = array())
    {
        $this->setMetadata($metadata);

        return $this->filesystem->delete($this->key);
    }

    /**
     * Creates a new file stream instance of the file
     *
     * @return FileStream
     */
    public function createStream()
    {
        return $this->filesystem->createStream($this->key);
    }

    /**
     * Sets the metadata array to be stored in adapters that can support it
     *
     * @param  array   $metadata
     * @return boolean
     */
    protected function setMetadata(array $metadata)
    {
        if ($metadata && $this->supportsMetadata()) {
            $this->filesystem->getAdapter()->setMetadata($this->key, $metadata);

            return true;
        }

        return false;
    }

    /**
     * @return boolean
     */
    private function supportsMetadata()
    {
        return $this->filesystem->getAdapter() instanceof MetadataSupporter;
    }
}

/**
 * A filesystem is used to store and retrieve files
 *
 * @author Antoine Hérault <antoine.herault@gmail.com>
 * @author Leszek Prabucki <leszek.prabucki@gmail.com>
 */
class Filesystem
{
    protected $adapter;

    /**
     * Constructor
     *
     * @param Adapter $adapter A configured Adapter instance
     */
    public function __construct(Adapter $adapter)
    {
        $this->adapter = $adapter;
    }

    /**
     * Returns the adapter
     *
     * @return Adapter
     */
    public function getAdapter()
    {
        return $this->adapter;
    }

    /**
     * Indicates whether the file matching the specified key exists
     *
     * @param string $key
     *
     * @return boolean TRUE if the file exists, FALSE otherwise
     */
    public function has($key)
    {
        return $this->adapter->exists($key);
    }

    /**
     * Renames a file
     *
     * @param string $sourceKey
     * @param string $targetKey
     *
     * @return boolean                  TRUE if the rename was successful
     * @throws Exception\FileNotFound   when sourceKey does not exist
     * @throws Exception\UnexpectedFile when targetKey exists
     * @throws \RuntimeException        when cannot rename
     */
    public function rename($sourceKey, $targetKey)
    {
        $this->assertHasFile($sourceKey);

        if ($this->has($targetKey)) {
            throw new UnexpectedFile($targetKey);
        }

        if (!$this->adapter->rename($sourceKey, $targetKey)) {
            throw new RuntimeException(sprintf('Could not rename the "%s" key to "%s".', $sourceKey, $targetKey));
        }

        return true;
    }

    /**
     * Returns the file matching the specified key
     *
     * @param string  $key    Key of the file
     * @param boolean $create Whether to create the file if it does not exist
     *
     * @throws Gaufrette\Exception\FileNotFound
     * @return File
     */
    public function get($key, $create = false)
    {
        if (!$create) {
            $this->assertHasFile($key);
        }

        return $this->createFile($key);
    }

    /**
     * Writes the given content into the file
     *
     * @param string  $key                 Key of the file
     * @param string  $content             Content to write in the file
     * @param boolean $overwrite           Whether to overwrite the file if exists
     * @throws Exception\FileAlreadyExists When file already exists and overwrite is false
     * @throws \RuntimeException           When for any reason content could not be written
     *
     * @return integer The number of bytes that were written into the file
     */
    public function write($key, $content, $overwrite = false)
    {
        if (!$overwrite && $this->has($key)) {
            throw new FileAlreadyExists($key);
        }

        $numBytes = $this->adapter->write($key, $content);

        if (false === $numBytes) {
            throw new RuntimeException(sprintf('Could not write the "%s" key content.', $key));
        }

        return $numBytes;
    }

    /**
     * Reads the content from the file
     *
     * @param  string                 $key Key of the file
     * @throws Exception\FileNotFound when file does not exist
     * @throws \RuntimeException      when cannot read file
     *
     * @return string
     */
    public function read($key)
    {
        $this->assertHasFile($key);

        $content = $this->adapter->read($key);

        if (false === $content) {
            throw new RuntimeException(sprintf('Could not read the "%s" key content.', $key));
        }

        return $content;
    }

    /**
     * Deletes the file matching the specified key
     *
     * @param string $key
     * @throws \RuntimeException when cannot read file
     *
     * @return boolean
     */
    public function delete($key)
    {
        $this->assertHasFile($key);

        if ($this->adapter->delete($key)) {
            return true;
        }

        throw new RuntimeException(sprintf('Could not remove the "%s" key.', $key));
    }

    /**
     * Returns an array of all keys
     *
     * @return array
     */
    public function keys()
    {
        return $this->adapter->keys();
    }

    /**
     * Lists keys beginning with given prefix
     * (no wildcard / regex matching)
     *
     * if adapter implements ListKeysAware interface, adapter's implementation will be used,
     * in not, ALL keys will be requested and iterated through.
     *
     * @param  string $prefix
     * @return array
     */
    public function listKeys($prefix = '')
    {
        if ($this->adapter instanceof ListKeysAware) {
            return $this->adapter->listKeys($prefix);
        }

        $dirs = array();
        $keys = array();

        foreach ($this->keys() as $key) {
            if (empty($prefix) || 0 === strpos($key, $prefix)) {
                if ($this->adapter->isDirectory($key)) {
                    $dirs[] = $key;
                } else {
                    $keys[] = $key;
                }
            }
        }

        return array(
            'keys' => $keys,
            'dirs' => $dirs
        );
    }

    /**
     * Returns the last modified time of the specified file
     *
     * @param string $key
     *
     * @return integer An UNIX like timestamp
     */
    public function mtime($key)
    {
        $this->assertHasFile($key);

        return $this->adapter->mtime($key);
    }

    /**
     * Returns the checksum of the specified file's content
     *
     * @param string $key
     *
     * @return string A MD5 hash
     */
    public function checksum($key)
    {
        $this->assertHasFile($key);

        if ($this->adapter instanceof ChecksumCalculator) {
            return $this->adapter->checksum($key);
        }

        return Checksum::fromContent($this->read($key));
    }

    /**
     * {@inheritDoc}
     */
    public function createStream($key)
    {
        if ($this->adapter instanceof StreamFactory) {
            return $this->adapter->createStream($key);
        }

        return new InMemoryBuffer($this, $key);
    }

    /**
     * {@inheritDoc}
     */
    public function createFile($key)
    {
        if ($this->adapter instanceof FileFactory) {
            return $this->adapter->createFile($key, $this);
        }

        return new File($key, $this);
    }

    /**
     * Checks if matching file by given key exists in the filesystem
     *
     * Key must be non empty string, otherwise it will throw Exception\FileNotFound
     * {@see http://php.net/manual/en/function.empty.php}
     *
     * @param string $key
     *
     * @throws Exception\FileNotFound   when sourceKey does not exist
     */
    private function assertHasFile($key)
    {
        if (!empty($key) && !$this->has($key)) {
            throw new FileNotFound($key);
        }
    }
}

/**
 * Associates filesystem instances to domains
 *
 * @author Antoine Hérault <antoine.herault@gmail.com>
 */
class FilesystemMap
{
    private $filesystems = array();

    /**
     * Returns an array of all the registered filesystems where the key is the
     * domain and the value the filesystem
     *
     * @return array
     */
    public function all()
    {
        return $this->filesystems;
    }

    /**
     * Register the given filesystem for the specified domain
     *
     * @param string     $domain
     * @param Filesystem $filesystem
     *
     * @throws InvalidArgumentException when the specified domain contains
     *                                  forbidden characters
     */
    public function set($domain, Filesystem $filesystem)
    {
        if (!preg_match('/^[-_a-zA-Z0-9]+$/', $domain)) {
            throw new InvalidArgumentException(sprintf(
                'The specified domain "%s" is not a valid domain.',
                $domain
            ));
        }

        $this->filesystems[$domain] = $filesystem;
    }

    /**
     * Indicates whether there is a filesystem registered for the specified
     * domain
     *
     * @param string $domain
     *
     * @return Boolean
     */
    public function has($domain)
    {
        return isset($this->filesystems[$domain]);
    }

    /**
     * Returns the filesystem registered for the specified domain
     *
     * @param string $domain
     *
     * @return Filesystem
     *
     * @throw  InvalidArgumentException when there is no filesystem registered
     *                                  for the specified domain
     */
    public function get($domain)
    {
        if (!$this->has($domain)) {
            throw new InvalidArgumentException(sprintf(
                'There is no filesystem defined for the "%s" domain.',
                $domain
            ));
        }

        return $this->filesystems[$domain];
    }

    /**
     * Removes the filesystem registered for the specified domain
     *
     * @param string $domain
     *
     * @return void
     */
    public function remove($domain)
    {
        if (!$this->has($domain)) {
            throw new InvalidArgumentException(sprintf(
                'Cannot remove the "%s" filesystem as it is not defined.',
                $domain
            ));
        }

        unset($this->filesystems[$domain]);
    }

    /**
     * Clears all the registered filesystems
     *
     * @return void
     */
    public function clear()
    {
        $this->filesystems = array();
    }
}

/**
 * Interface for the file streams
 *
 * @author Antoine Hérault <antoine.herault@gmail.com>
 */
interface Stream
{
    /**
     * Opens the stream in the specified mode
     *
     * @param StreamMode $mode
     *
     * @return Boolean TRUE on success or FALSE on failure
     */
    public function open(StreamMode $mode);

    /**
     * Reads the specified number of bytes from the current position
     *
     * If the current position is the end-of-file, you must return an empty
     * string.
     *
     * @param integer $count The number of bytes
     *
     * @return string
     */
    public function read($count);

    /**
     * Writes the specified data
     *
     * Don't forget to update the current position of the stream by number of
     * bytes that were successfully written.
     *
     * @param string $data
     *
     * @return integer The number of bytes that were successfully written
     */
    public function write($data);

    /**
     * Closes the stream
     *
     * It must free all the resources. If there is any data to flush, you
     * should do so
     *
     * @return void
     */
    public function close();

    /**
     * Flushes the output
     *
     * If you have cached data that is not yet stored into the underlying
     * storage, you should do so now
     *
     * @return Boolean TRUE on success or FALSE on failure
     */
    public function flush();

    /**
     * Seeks to the specified offset
     *
     * @param integer $offset
     * @param integer $whence
     *
     * @return Boolean
     */
    public function seek($offset, $whence = SEEK_SET);

    /**
     * Returns the current position
     *
     * @return integer
     */
    public function tell();

    /**
     * Indicates whether the current position is the end-of-file
     *
     * @return Boolean
     */
    public function eof();

    /**
     * Gathers statistics of the stream
     *
     * @return array
     */
    public function stat();

    /**
     * Retrieve the underlying resource
     *
     * @param  integer $castAs
     * @return mixed   using resource or false
     */
    public function cast($castAs);

    /**
     * Delete a file
     *
     * @return Boolean TRUE on success FALSE otherwise
     */
    public function unlink();
}

/**
 * Represents a stream mode
 *
 * @author Antoine Hérault <antoine.herault@gmail.com>
 */
class StreamMode
{
    private $mode;
    private $base;
    private $plus;
    private $flag;

    /**
     * Constructor
     *
     * @param string $mode A stream mode as for the use of fopen()
     */
    public function __construct($mode)
    {
        $this->mode = $mode;

        $mode = substr($mode, 0, 3);
        $rest = substr($mode, 1);

        $this->base = substr($mode, 0, 1);
        $this->plus = false !== strpos($rest, '+');
        $this->flag = trim($rest, '+');
    }

    /**
     * Returns the underlying mode
     *
     * @return string
     */
    public function getMode()
    {
        return $this->mode;
    }

    /**
     * Indicates whether the mode allows to read
     *
     * @return Boolean
     */
    public function allowsRead()
    {
        if ($this->plus) {
            return true;
        }

        return 'r' === $this->base;
    }

    /**
     * Indicates whether the mode allows to write
     *
     * @return Boolean
     */
    public function allowsWrite()
    {
        if ($this->plus) {
            return true;
        }

        return 'r' !== $this->base;
    }

    /**
     * Indicates whether the mode allows to open an existing file
     *
     * @return Boolean
     */
    public function allowsExistingFileOpening()
    {
        return 'x' !== $this->base;
    }

    /**
     * Indicates whether the mode allows to create a new file
     *
     * @return Boolean
     */
    public function allowsNewFileOpening()
    {
        return 'r' !== $this->base;
    }

    /**
     * Indicates whether the mode implies to delete the existing content of the
     * file when it already exists
     *
     * @return Boolean
     */
    public function impliesExistingContentDeletion()
    {
        return 'w' === $this->base;
    }

    /**
     * Indicates whether the mode implies positioning the cursor at the
     * beginning of the file
     *
     * @return Boolean
     */
    public function impliesPositioningCursorAtTheBeginning()
    {
        return 'a' !== $this->base;
    }

    /**
     * Indicates whether the mode implies positioning the cursor at the end of
     * the file
     *
     * @return Boolean
     */
    public function impliesPositioningCursorAtTheEnd()
    {
        return 'a' === $this->base;
    }

    /**
     * Indicates whether the stream is in binary mode
     *
     * @return Boolean
     */
    public function isBinary()
    {
        return 'b' === $this->flag;
    }

    /**
     * Indicates whether the stream is in text mode
     *
     * @return Boolean
     */
    public function isText()
    {
        return false === $this->isBinary();
    }
}

/**
 * Stream wrapper class for the Gaufrette filesystems
 *
 * @author Antoine Hérault <antoine.herault@gmail.com>
 * @author Leszek Prabucki <leszek.prabucki@gmail.com>
 */
// class StreamWrapper
// {
//     private static $filesystemMap;
// 
//     private $stream;
// 
//     /**
//      * Defines the filesystem map
//      *
//      * @param FilesystemMap $map
//      */
//     public static function setFilesystemMap(FilesystemMap $map)
//     {
//         StreamWrapper::$filesystemMap = $map;
//     }
// 
//     /**
//      * Returns the filesystem map
//      *
//      * @return FilesystemMap $map
//      */
//     public static function getFilesystemMap()
//     {
//         if (null === StreamWrapper::$filesystemMap) {
//             StreamWrapper::$filesystemMap = StreamWrapper::createFilesystemMap();
//         }
// 
//         return StreamWrapper::$filesystemMap;
//     }
// 
//     /**
//      * Registers the stream wrapper to handle the specified scheme
//      *
//      * @param string $scheme Default is gaufrette
//      */
//     public static function register($scheme = 'gaufrette')
//     {
//         echo "register\n";
//         
//         StreamWrapper::streamWrapperUnregister($scheme);
// 
//         if (!StreamWrapper::streamWrapperRegister($scheme, __CLASS__)) {
//             throw new RuntimeException(sprintf(
//                 'Could not register stream wrapper class %s for scheme %s.',
//                 __CLASS__,
//                 $scheme
//             ));
//         }
//     }
// 
//     /**
//      * @return FilesystemMap
//      */
//     protected static function createFilesystemMap()
//     {
//         return new FilesystemMap();
//     }
// 
//     /**
//      * @param string $scheme - protocol scheme
//      */
//     protected static function streamWrapperUnregister($scheme)
//     {
//         return @stream_wrapper_unregister($scheme);
//     }
// 
//     /**
//      * @param string $scheme    - protocol scheme
//      * @param string $className
//      */
//     protected static function streamWrapperRegister($scheme, $className)
//     {
//         return stream_wrapper_register($scheme, $className);
//     }
// 
//     public function stream_open($path, $mode)
//     {
//         $this->stream = $this->createStream($path);
// 
//         return $this->stream->open($this->createStreamMode($mode));
//     }
// 
//     /**
//      * @param  int   $bytes
//      * @return mixed
//      */
//     public function stream_read($bytes)
//     {
//         if ($this->stream) {
//             return $this->stream->read($bytes);
//         }
// 
//         return false;
//     }
// 
//     /**
//      * @param  string $data
//      * @return int
//      */
//     public function stream_write($data)
//     {
//         if ($this->stream) {
//             return $this->stream->write($data);
//         }
// 
//         return 0;
//     }
// 
//     public function stream_close()
//     {
//         if ($this->stream) {
//             $this->stream->close();
//         }
//     }
// 
//     /**
//      * @return boolean
//      */
//     public function stream_flush()
//     {
//         if ($this->stream) {
//             return $this->stream->flush();
//         }
// 
//         return false;
//     }
// 
//     /**
//      * @param  int     $offset
//      * @param  int     $whence - one of values [SEEK_SET, SEEK_CUR, SEEK_END]
//      * @return boolean
//      */
//     public function stream_seek($offset, $whence = SEEK_SET)
//     {
//         if ($this->stream) {
//             return $this->stream->seek($offset, $whence);
//         }
// 
//         return false;
//     }
// 
//     /**
//      * @return mixed
//      */
//     public function stream_tell()
//     {
//         if ($this->stream) {
//             return $this->stream->tell();
//         }
// 
//         return false;
//     }
// 
//     /**
//      * @return boolean
//      */
//     public function stream_eof()
//     {
//         echo "stream_eof\n";
//         var_dump ($this);
//         
//         if ($this->stream) {
//             return $this->stream->eof();
//         }
// 
//         return true;
//     }
// 
//     /**
//      * @return mixed
//      */
//     public function stream_stat()
//     {
//         if ($this->stream) {
//             return $this->stream->stat();
//         }
// 
//         return false;
//     }
// 
//     /**
//      * @param  string $path
//      * @param  int    $flags
//      * @return mixed
//      * @todo handle $flags parameter
//      */
//     public function url_stat($path, $flags)
//     {
//         $stream = $this->createStream($path);
// 
//         try {
//             $stream->open($this->createStreamMode('r+'));
//         } catch (RuntimeException $e) {
//             return false;
//         }
// 
//         return $stream->stat();
//     }
// 
//     /**
//      * @param  string $path
//      * @return mixed
//      */
//     public function unlink($path)
//     {
//         $stream = $this->createStream($path);
// 
//         try {
//             $stream->open($this->createStreamMode('w+'));
//         } catch (RuntimeException $e) {
//             return false;
//         }
// 
//         return $stream->unlink();
//     }
// 
//     /**
//      * @return mixed
//      */
//     public function stream_cast($castAs)
//     {
//         if ($this->stream) {
//             return $this->stream->cast($castAs);
//         }
// 
//         return false;
//     }
// 
//     protected function createStream($path)
//     {
//         $parts = array_merge(
//             array(
//                 'scheme'    => null,
//                 'host'      => null,
//                 'path'      => null,
//                 'query'     => null,
//                 'fragment'  => null,
//             ),
//             (parse_url($path)) ? parse_url($path) : array()
//         );
// 
//         $domain = $parts['host'];
//         $key    = substr($parts['path'], 1);
// 
//         if (null !== $parts['query']) {
//             $key.= '?' . $parts['query'];
//         }
// 
//         if (null !== $parts['fragment']) {
//             $key.= '#' . $parts['fragment'];
//         }
// 
//         if (empty($domain) || empty($key)) {
//             throw new InvalidArgumentException(sprintf(
//                 'The specified path (%s) is invalid.',
//                 $path
//             ));
//         }
// 
//         return StreamWrapper::getFilesystemMap()->get($domain)->createStream($key);
//     }
// 
//     protected function createStreamMode($mode)
//     {
//         return new StreamMode($mode);
//     }
// }

/**
 * Checksum utils
 *
 * @author  Antoine Hérault <antoine.herault@gmail.com>
 */
class Checksum
{
    /**
     * Returns the checksum of the given content
     *
     * @param string $content
     *
     * @return string
     */
    public static function fromContent($content)
    {
        return md5($content);
    }

    /**
     * Returns the checksum of the specified file
     *
     * @param string $filename
     *
     * @return string
     */
    public static function fromFile($filename)
    {
        return md5_file($filename);
    }
}

/**
 * Path utils
 *
 * @package Gaufrette
 * @author  Antoine Hérault <antoine.herault@gmail.com>
 */
class Path
{
    /**
     * Normalizes the given path
     *
     * @param string $path
     *
     * @return string
     */
    public static function normalize($path)
    {
        $path   = str_replace('\\', '/', $path);
        $prefix = Path::getAbsolutePrefix($path);
        $path   = substr($path, strlen($prefix));
        $parts  = array_filter(explode('/', $path), 'strlen');
        $tokens = array();

        foreach ($parts as $part) {
            switch ($part) {
                case '.':
                    continue;
                case '..':
                    if (0 !== count($tokens)) {
                        array_pop($tokens);
                        continue;
                    } elseif (!empty($prefix)) {
                        continue;
                    }
                default:
                    $tokens[] = $part;
            }
        }

        return $prefix . implode('/', $tokens);
    }

    /**
     * Indicates whether the given path is absolute or not
     *
     * @param string $path A normalized path
     *
     * @return boolean
     */
    public static function isAbsolute($path)
    {
        return '' !== Path::getAbsolutePrefix($path);
    }

    /**
     * Returns the absolute prefix of the given path
     *
     * @param string $path A normalized path
     *
     * @return string
     */
    public static function getAbsolutePrefix($path)
    {
        preg_match('|^(?P<prefix>([a-zA-Z]:)?/)|', $path, $matches);

        if (empty($matches['prefix'])) {
            return '';
        }

        return strtolower($matches['prefix']);
    }
}

/**
 * Utility class for file sizes
 *
 * @author Antoine Hérault <antoine.herault@gmail.com>
 */
class Size
{
    /**
     * Returns the size in bytes from the given content
     *
     * @param string $content
     *
     * @return integer
     *
     * @todo handle the case the mbstring is not loaded
     */
    public static function fromContent($content)
    {
        // Make sure to get the real length in byte and not
        // accidentally mistake some bytes as a UTF BOM.
        return mb_strlen($content, '8bit');
    }
}

/**
 * Interface for the Gaufrette related exceptions
 *
 * @author Antoine Hérault <antoine.herault@gmail.com>
 */
interface GaufretteException
{
}


/**
 * Exception to be thrown when a file already exists
 *
 * @author Benjamin Dulau <benjamin.dulau@gmail.com>
 */
class FileAlreadyExists extends RuntimeException implements GaufretteException
{
    private $key;

    public function __construct($key, $code = 0, Exception $previous = null)
    {
        $this->key = $key;

        parent::__construct(
            sprintf('The file %s already exists and can not be overwritten.', $key),
            $code,
            $previous
        );
    }

    public function getKey()
    {
        return $this->key;
    }
}


/**
 * Exception to be thrown when a file was not found
 *
 * @author Antoine Hérault <antoine.herault@gmail.com>
 */
class FileNotFound extends RuntimeException implements GaufretteException
{
    private $key;

    public function __construct($key, $code = 0, Exception $previous = null)
    {
        $this->key = $key;

        parent::__construct(
            sprintf('The file "%s" was not found.', $key),
            $code,
            $previous
        );
    }

    public function getKey()
    {
        return $this->key;
    }
}

/**
 * Exception to be thrown when an unexpected file exists
 *
 * @author  Antoine Hérault <antoine.herault@gmail.com>
 */
class UnexpectedFile extends RuntimeException implements GaufretteException
{
    private $key;

    public function __construct($key, $code = 0, Exception $previous = null)
    {
        $this->key = $key;

        parent::__construct(
            sprintf('The file "%s" was not supposed to exist.', $key),
            $code,
            $previous
        );
    }

    public function getKey()
    {
        return $this->key;
    }
}

/**
 * Interface which add checksum calculation support to adapter
 *
 * @author Leszek Prabucki <leszek.prabucki@gmail.com>
 */
interface ChecksumCalculator
{
    /**
     * Returns the checksum of the specified key
     *
     * @param string $key
     *
     * @return string
     */
    public function checksum($key);
}

/**
 * Interface for the stream creation class
 *
 * @author Leszek Prabucki <leszek.prabucki@gmail.com>
 */
interface StreamFactory
{
    /**
     * Creates a new stream instance of the specified file
     *
     * @param string $key
     *
     * @return Gaufrette\Stream
     */
    public function createStream($key);
}

/**
 * Interface for the file creation class
 *
 * @author Leszek Prabucki <leszek.prabucki@gmail.com>
 */
interface FileFactory
{
    /**
     * Creates a new File instance and returns it
     *
     * @param string     $key
     * @param Filesystem $filesystem
     *
     * @return File
     */
    public function createFile($key, Filesystem $filesystem);
}

class InMemoryBuffer implements Stream
{
    private $filesystem;
    private $key;
    private $mode;
    private $content;
    private $numBytes;
    private $position;
    private $synchronized;

    /**
     * Constructor
     *
     * @param Filesystem $filesystem The filesystem managing the file to stream
     * @param string     $key        The file key
     */
    public function __construct(Filesystem $filesystem, $key)
    {
        $this->filesystem = $filesystem;
        $this->key     = $key;
    }

    /**
     * {@inheritDoc}
     */
    public function open(StreamMode $mode)
    {
        $this->mode = $mode;

        $exists = $this->filesystem->has($this->key);

        if (($exists && !$mode->allowsExistingFileOpening())
            || (!$exists && !$mode->allowsNewFileOpening())) {
            return false;
        }

        if ($mode->impliesExistingContentDeletion()) {
            $this->content = $this->writeContent('');
        } elseif (!$exists && $mode->allowsNewFileOpening()) {
            $this->content = $this->writeContent('');
        } else {
            $this->content = $this->filesystem->read($this->key);
        }

        $this->numBytes = Size::fromContent($this->content);
        $this->position = $mode->impliesPositioningCursorAtTheEnd() ? $this->numBytes : 0;

        $this->synchronized = true;

        return true;
    }

    public function read($count)
    {
        if (false === $this->mode->allowsRead()) {
            throw new LogicException('The stream does not allow read.');
        }

        $chunk = substr($this->content, $this->position, $count);
        $this->position+= Size::fromContent($chunk);

        return $chunk;
    }

    public function write($data)
    {
        if (false === $this->mode->allowsWrite()) {
            throw new LogicException('The stream does not allow write.');
        }

        $numWrittenBytes = Size::fromContent($data);

        $newPosition     = $this->position + $numWrittenBytes;
        $newNumBytes     = $newPosition > $this->numBytes ? $newPosition : $this->numBytes;

        if ($this->eof()) {
            $this->numBytes += $numWrittenBytes;
            if ($this->hasNewContentAtFurtherPosition()) {
                $data = str_pad($data, $this->position + strlen($data), " ", STR_PAD_LEFT);
            }
            $this->content .= $data;
        } else {
            $before = substr($this->content, 0, $this->position);
            $after  = $newNumBytes > $newPosition ? substr($this->content, $newPosition) : '';
            $this->content  = $before . $data . $after;
        }

        $this->position     = $newPosition;
        $this->numBytes     = $newNumBytes;
        $this->synchronized = false;

        return $numWrittenBytes;
    }

    public function close()
    {
        if (! $this->synchronized) {
            $this->flush();
        }
    }

    public function seek($offset, $whence = SEEK_SET)
    {
        switch ($whence) {
            case SEEK_SET:
                $this->position = $offset;
                break;
            case SEEK_CUR:
                $this->position += $offset;
                break;
            case SEEK_END:
                $this->position = $this->numBytes + $offset;
                break;
            default:
                return false;
        }

        return true;
    }

    public function tell()
    {
        return $this->position;
    }

    public function flush()
    {
        if ($this->synchronized) {
            return true;
        }

        try {
            $this->writeContent($this->content);
        } catch (Exception $e) {
            return false;
        }

        return true;
    }

    public function eof()
    {
        return $this->position >= $this->numBytes;
    }

    /**
     * {@inheritDoc}
     */
    public function stat()
    {
        if ($this->filesystem->has($this->key)) {
            $time = $this->filesystem->mtime($this->key);

            $stats = array(
                'dev'   => 1,
                'ino'   => 0,
                'mode'  => 33204,
                'nlink' => 1,
                'uid'   => 0,
                'gid'   => 0,
                'rdev'  => 0,
                'size'  => Size::fromContent($this->content),
                'atime' => $time,
                'mtime' => $time,
                'ctime' => $time,
                'blksize' => -1,
                'blocks'  => -1,
            );

            return array_merge(array_values($stats), $stats);
        }

        return false;
    }

    /**
     * {@inheritDoc}
     */
    public function cast($castAst)
    {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    public function unlink()
    {
        if ($this->mode && $this->mode->impliesExistingContentDeletion()) {
            return $this->filesystem->delete($this->key);
        }

        return false;
    }

    /**
     * @return Boolean
     */
    protected function hasNewContentAtFurtherPosition()
    {
        return $this->position > 0 && !$this->content;
    }

    /**
     * @param string $content Empty string by default
     * @param bool $overwrite Overwrite by default
     * @return string
     */
    protected function writeContent($content = '', $overwrite = true)
    {
        $this->filesystem->write($this->key, $content, $overwrite);

        return $content;
    }
}

class Local implements Stream
{
    private $path;
    private $mode;
    private $fileHandle;

    /**
     * Constructor
     *
     * @param string $path
     */
    public function __construct($path)
    {
        $this->path = $path;
    }

    /**
     * {@inheritDoc}
     */
    public function open(StreamMode $mode)
    {
        $fileHandle = @fopen($this->path, $mode->getMode());

        if (false === $fileHandle) {
            throw new RuntimeException(sprintf('File "%s" cannot be opened', $this->path));
        }

        $this->mode = $mode;
        $this->fileHandle = $fileHandle;

        return true;
    }

    /**
     * {@inheritDoc}
     */
    public function read($count)
    {
        if (! $this->fileHandle) {
            return false;
        }

        if (false === $this->mode->allowsRead()) {
            throw new LogicException('The stream does not allow read.');
        }

        return fread($this->fileHandle, $count);
    }

    /**
     * {@inheritDoc}
     */
    public function write($data)
    {
        if (! $this->fileHandle) {
            return false;
        }

        if (false === $this->mode->allowsWrite()) {
            throw new LogicException('The stream does not allow write.');
        }

        return fwrite($this->fileHandle, $data);
    }

    /**
     * {@inheritDoc}
     */
    public function close()
    {
        if (! $this->fileHandle) {
            return false;
        }

        $closed = fclose($this->fileHandle);

        if ($closed) {
            $this->mode = null;
            $this->fileHandle = null;
        }

        return $closed;
    }

    /**
     * {@inheritDoc}
     */
    public function flush()
    {
        if ($this->fileHandle) {
            return fflush($this->fileHandle);
        }

        return false;
    }

    /**
     * {@inheritDoc}
     */
    public function seek($offset, $whence = SEEK_SET)
    {
        if ($this->fileHandle) {
            return 0 === fseek($this->fileHandle, $offset, $whence);
        }

        return false;
    }

    /**
     * {@inheritDoc}
     */
    public function tell()
    {
        if ($this->fileHandle) {
            return ftell($this->fileHandle);
        }

        return false;
    }

    /**
     * {@inheritDoc}
     */
    public function eof()
    {
        if ($this->fileHandle) {
            return feof($this->fileHandle);
        }

        return true;
    }

    /**
     * {@inheritDoc}
     */
    public function stat()
    {
        if ($this->fileHandle) {
            return fstat($this->fileHandle);
        }

        return false;
    }

    /**
     * {@inheritDoc}
     */
    public function cast($castAs)
    {
        if ($this->fileHandle) {
            return $this->fileHandle;
        }

        return false;
    }

    /**
     * {@inheritDoc}
     */
    public function unlink()
    {
        if ($this->mode && $this->mode->impliesExistingContentDeletion()) {
            return @unlink($this->path);
        }

        return false;
    }
}

/**
 * In memory adapter
 *
 * Stores some files in memory for test purposes
 *
 * @package Gaufrette
 * @author Antoine Hérault <antoine.herault@gmail.com>
 */
class InMemoryAdapter implements Adapter
{
    protected $files = array();

    /**
     * Constructor
     *
     * @param array $files An array of files
     */
    public function __construct(array $files = array())
    {
        $this->setFiles($files);
    }

    /**
     * Defines the files
     *
     * @param array $files An array of files
     */
    public function setFiles(array $files)
    {
        $this->files = array();
        foreach ($files as $key => $file) {
            if (!is_array($file)) {
                $file = array('content' => $file);
            }

            $file = array_merge(array(
                'content'   => null,
                'mtime'     => null,
            ), $file);

            $this->setFile($key, $file['content'], $file['mtime']);
        }
    }

    /**
     * Defines a file
     *
     * @param string  $key     The key
     * @param string  $content The content
     * @param integer $mtime   The last modified time (automatically set to now if NULL)
     */
    public function setFile($key, $content = null, $mtime = null)
    {
        if (null === $mtime) {
            $mtime = time();
        }

        $this->files[$key] = array(
            'content'   => (string) $content,
            'mtime'     => (integer) $mtime
        );
    }

    /**
     * {@inheritDoc}
     */
    public function read($key)
    {
        return $this->files[$key]['content'];
    }

    /**
     * {@inheritDoc}
     */
    public function rename($sourceKey, $targetKey)
    {
        $content = $this->read($sourceKey);
        $this->delete($sourceKey);

        return (boolean) $this->write($targetKey, $content);
    }

    /**
     * {@inheritDoc}
     */
    public function write($key, $content, array $metadata = null)
    {
        $this->files[$key]['content']  = $content;
        $this->files[$key]['mtime']    = time();

        return Size::fromContent($content);
    }

    /**
     * {@inheritDoc}
     */
    public function exists($key)
    {
        return array_key_exists($key, $this->files);
    }

    /**
     * {@inheritDoc}
     */
    public function keys()
    {
        return array_keys($this->files);
    }

    /**
     * {@inheritDoc}
     */
    public function mtime($key)
    {
        return isset($this->files[$key]['mtime']) ? $this->files[$key]['mtime'] : false;
    }

    /**
     * {@inheritDoc}
     */
    public function delete($key)
    {
        unset($this->files[$key]);
        clearstatcache();

        return true;
    }

    /**
     * {@inheritDoc}
     */
    public function isDirectory($path)
    {
        return false;
    }
}

/**
 * Adapter for the local filesystem
 *
 * @author Antoine Hérault <antoine.herault@gmail.com>
 * @author Leszek Prabucki <leszek.prabucki@gmail.com>
 */
class LocalAdapter implements Adapter,
                              StreamFactory,
                              ChecksumCalculator
{
    protected $directory;
    private $create;

    /**
     * Constructor
     *
     * @param string  $directory Directory where the filesystem is located
     * @param boolean $create    Whether to create the directory if it does not
     *                            exist (default FALSE)
     *
     * @throws RuntimeException if the specified directory does not exist and
     *                          could not be created
     */
    public function __construct($directory, $create = false)
    {
        $this->directory = Path::normalize($directory);

        if (is_link($this->directory)) {
            $this->directory = realpath($this->directory);
        }

        $this->create = $create;
    }

    /**
     * {@inheritDoc}
     */
    public function read($key)
    {
        return file_get_contents($this->computePath($key));
    }

    /**
     * {@inheritDoc}
     */
    public function write($key, $content)
    {
        $path = $this->computePath($key);
        $this->ensureDirectoryExists(dirname($path), true);

        return file_put_contents($path, $content);
    }

    /**
     * {@inheritDoc}
     */
    public function rename($sourceKey, $targetKey)
    {
        $targetPath = $this->computePath($targetKey);
        $this->ensureDirectoryExists(dirname($targetPath), true);

        return rename($this->computePath($sourceKey), $targetPath);
    }

    /**
     * {@inheritDoc}
     */
    public function exists($key)
    {
        return file_exists($this->computePath($key));
    }

    /**
     * {@inheritDoc}
     */
    public function keys()
    {
        $this->ensureDirectoryExists($this->directory, $this->create);

        try {
            $iterator = new RecursiveIteratorIterator(
                new RecursiveDirectoryIterator(
                    $this->directory,
                    FilesystemIterator::SKIP_DOTS | FilesystemIterator::UNIX_PATHS
                )
            );
        } catch (GaufretteException $e) {
            $iterator = new EmptyIterator;
        }
        $files = iterator_to_array($iterator);

        $keys = array();
        foreach ($files as $file) {
            $keys[] = $key = $this->computeKey($file);
            if ('.' !== dirname($key)) {
                $keys[] = dirname($key);
            }
        }
        sort($keys);

        return $keys;
    }

    /**
     * {@inheritDoc}
     */
    public function mtime($key)
    {
        return filemtime($this->computePath($key));
    }

    /**
     * {@inheritDoc}
     */
    public function delete($key)
    {
        if ($this->isDirectory($key)) {
            return rmdir($this->computePath($key));
        }

        return unlink($this->computePath($key));
    }

    /**
     * @param  string  $key
     * @return boolean
     */
    public function isDirectory($key)
    {
        return is_dir($this->computePath($key));
    }

    /**
     * {@inheritDoc}
     */
    public function createStream($key)
    {
        return new Local($this->computePath($key));
    }

    public function checksum($key)
    {
        return Checksum::fromFile($this->computePath($key));
    }

    /**
     * Computes the key from the specified path
     *
     * @param string $path
     *
     * return string
     */
    public function computeKey($path)
    {
        $path = $this->normalizePath($path);

        return ltrim(substr($path, strlen($this->directory)), '/');
    }

    /**
     * Computes the path from the specified key
     *
     * @param string $key The key which for to compute the path
     *
     * @return string A path
     *
     * @throws OutOfBoundsException If the computed path is out of the
     *                              directory
     * @throws RuntimeException If directory does not exists and cannot be created
     */
    protected function computePath($key)
    {
        $this->ensureDirectoryExists($this->directory, $this->create);

        return $this->normalizePath($this->directory . '/' . $key);
    }

    /**
     * Normalizes the given path
     *
     * @param string $path
     *
     * @return string
     */
    protected function normalizePath($path)
    {
        $path = Path::normalize($path);

        if (0 !== strpos($path, $this->directory)) {
            throw new OutOfBoundsException(sprintf('The path "%s" is out of the filesystem.', $path));
        }

        return $path;
    }

    /**
     * Ensures the specified directory exists, creates it if it does not
     *
     * @param string  $directory Path of the directory to test
     * @param boolean $create    Whether to create the directory if it does
     *                            not exist
     *
     * @throws RuntimeException if the directory does not exists and could not
     *                          be created
     */
    protected function ensureDirectoryExists($directory, $create = false)
    {
        if (!is_dir($directory)) {
            if (!$create) {
                throw new RuntimeException(sprintf('The directory "%s" does not exist.', $directory));
            }

            $this->createDirectory($directory);
        }
    }

    /**
     * Creates the specified directory and its parents
     *
     * @param string $directory Path of the directory to create
     *
     * @throws InvalidArgumentException if the directory already exists
     * @throws RuntimeException         if the directory could not be created
     */
    protected function createDirectory($directory)
    {
        $umask = umask(0);
        $created = mkdir($directory, 0777, true);
        umask($umask);

        if (!$created) {
            throw new RuntimeException(sprintf('The directory \'%s\' could not be created.', $directory));
        }
    }
}

/**
 * ZIP Archive adapter
 *
 * @author Boris Guéry <guery.b@gmail.com>
 * @author Antoine Hérault <antoine.herault@gmail.com>
 */
class ZipAdapter implements Adapter
{
    /**
     * @var string The zip archive full path
     */
    protected $zipFile;

    /**
     * @var ZipArchive
     */
    protected $zipArchive;

    public function __construct($zipFile)
    {
        if (!extension_loaded('zip')) {
            throw new RuntimeException(sprintf(
                'Unable to use %s as the ZIP extension is not available.',
                __CLASS__
            ));
        }

        $this->zipFile = $zipFile;
        $this->reinitZipArchive();
    }

    /**
     * {@inheritDoc}
     */
    public function read($key)
    {
        if (false === ($content = $this->zipArchive->getFromName($key, 0))) {
            return false;
        }

        return $content;
    }

    /**
     * {@inheritDoc}
     */
    public function write($key, $content)
    {
        if (!$this->zipArchive->addFromString($key, $content)) {
            return false;
        }

        if (!$this->save()) {
            return false;
        }

        return Size::fromContent($content);
    }

    /**
     * {@inheritDoc}
     */
    public function exists($key)
    {
        return (boolean) $this->getStat($key);
    }

    /**
     * {@inheritDoc}
     */
    public function keys()
    {
        $keys = array();

        for ($i = 0; $i < $this->zipArchive->numFiles; ++$i) {
            $keys[$i] = $this->zipArchive->getNameIndex($i);
        }

        return $keys;
    }

    /**
     * @todo implement
     *
     * {@inheritDoc}
     */
    public function isDirectory($key)
    {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    public function mtime($key)
    {
        $stat = $this->getStat($key);

        return isset($stat['mtime']) ? $stat['mtime'] : false;
    }

    /**
     * {@inheritDoc}
     */
    public function delete($key)
    {
        if (!$this->zipArchive->deleteName($key)) {
            return false;
        }

        return $this->save();
    }

    /**
     * {@inheritDoc}
     */
    public function rename($sourceKey, $targetKey)
    {
        if (!$this->zipArchive->renameName($sourceKey, $targetKey)) {
            return false;
        }

        return $this->save();
    }

    /**
     * Returns the stat of a file in the zip archive
     *  (name, index, crc, mtime, compression size, compression method, filesize)
     *
     * @param $key
     * @return array|bool
     */
    public function getStat($key)
    {
        $stat = $this->zipArchive->statName($key);
        if (false === $stat) {
            return array();
        }

        return $stat;
    }

/*
    public function __destruct()
    {
        if ($this->zipArchive) {
            try {
                $this->zipArchive->close();
            } catch (Exception $e) {

            }
            unset($this->zipArchive);
        }
    }
*/

    protected function reinitZipArchive()
    {
        $this->zipArchive = new ZipArchive();

        if (true !== ($resultCode = $this->zipArchive->open($this->zipFile, ZipArchive::CREATE))) {
            switch ($resultCode) {
            case ZipArchive::ER_EXISTS:
                $errMsg = 'File already exists.';
                break;
            case ZipArchive::ER_INCONS:
                $errMsg = 'Zip archive inconsistent.';
                break;
            case ZipArchive::ER_INVAL:
                $errMsg = 'Invalid argument.';
                break;
            case ZipArchive::ER_MEMORY:
                $errMsg = 'Malloc failure.';
                break;
            case ZipArchive::ER_NOENT:
                $errMsg = 'Invalid argument.';
                break;
            case ZipArchive::ER_NOZIP:
                $errMsg = 'Not a zip archive.';
                break;
            case ZipArchive::ER_OPEN:
                $errMsg = 'Can\'t open file.';
                break;
            case ZipArchive::ER_READ:
                $errMsg = 'Read error.';
                break;
            case ZipArchive::ER_SEEK;
                $errMsg = 'Seek error.';
                break;
            default:
                $errMsg = 'Unknown error.';
                break;
            }

            throw new RuntimeException(sprintf('%s', $errMsg));
        }

        return $this;
    }

    /**
     * Saves archive modifications and updates current ZipArchive instance
     *
     * @throws \RuntimeException If file could not be saved
     */
    protected function save()
    {
        // Close to save modification
        if (!$this->zipArchive->close()) {
            return false;
        }

        // Re-initialize to get updated version
        $this->reinitZipArchive();

        return true;
    }
}


/******************************************************************************
 * Unit Testing                                                               *
 *****************************************************************************/
 
abstract class TestCase {
    public function setup () {
        
    }
    
    public function tearDown () {
        
    }
    
    public function assertTrue ($mFound)
    {
        return $this->assertEquals (true, $mFound);
    }

    public function assertFalse ($mFound)
    {
        return $this->assertEquals (false, $mFound);
    }
    
    public function assertJsonStringEqualsJsonString ($mExpected, $mFound) 
    {
        return $this->assertEquals (json_decode ($mExpected), json_decode ($mFound));
    }
    
    public function assertEquals ($mExpected, $mFound) 
    {
        if ($mExpected != $mFound) {
            $this->displayFailure ($mExpected, $mFound);
        } else {
            $this->displaySuccess ();
        }
    }
    
    public function assertSame ($mExpected, $mFound)
    {
        if ($mExpected !== $mFound) {
            $this->displayFailure ($mExpected, $mFound);
        } else {
            $this->displaySuccess ();
        }
    }
    
    public function assertGreaterThan ($mExpected, $mFound)
    {
        if ($mExpected >= $mFound) {
            $this->displayFailure ($mExpected, $mFound);
        } else {
            $this->displaySuccess ();
        }
    }
    
    public function assertEmpty ($mMixed)
    {
        if (!empty ($mMixed)) {
            $this->displayFailure ("[EMPTY]", $mMixed);
        } else {
            $this->displaySuccess ();
        }
    }
    
    public function assertContains ($mNeedle, $mHaystack)
    {
        if (!in_array ($mNeedle, $mHaystack)) {
            $this->displayFailure ($mNeedle, $mHaystack);
        } else {
            $this->displaySuccess ();
        }
    }
    
    public function assertCount ($expectedCount, $other)
    {
        if ($this->getCountOf($other) !== $expectedCount) {
            $this->displayFailure ($this->getCountOf($other), $expectedCount);
        } else {
            $this->displaySuccess ();
        }
    }
    
    public function assertFileExists ($file)
    {
        $this->assertTrue (is_file($file));
    }
    
    protected function displayFailure ($mExpected, $mFound)
    {
        echo "FAIL\n\tExpected: ";
        var_dump ($mExpected);
        echo "\tFound: ";
        var_dump ($mFound);
    }
    
    protected function displaySuccess ()
    {
        echo "PASS\n";
    }
    
    protected function getCountOf($other) 
    {
        if ($other instanceof Countable || is_array($other)) {
            return count($other);
        } else if ($other instanceof Iterator) {
          return iterator_count($other);
        }
    }
}

abstract class FunctionalTestCase extends TestCase
{
    /**
     * @var Filesystem
     */
    protected $filesystem;

    public function getAdapterName()
    {
        if (!preg_match('/\\\\(\w+)Test$/', get_class($this), $matches)) {
            throw new RuntimeException(sprintf(
                'Unable to guess filesystem name from class "%s", '.
                'please override the ->getAdapterName() method.',
                get_class($this)
            ));
        }

        return $matches[1];
    }

    public function setUp()
    {
        $basename = $this->getAdapterName();
        $filename = sprintf(
            '%s/adapters/%s.php',
            dirname(__DIR__),
            $basename
        );

        if (!file_exists($filename)) {
            return $this->markTestSkipped(<<<EOF
To run the {$basename} filesystem tests, you must:

 1. Copy the file "{$filename}.dist" as "{$filename}"
 2. Modify the copied file to fit your environment
EOF
            );
        }

        $adapter = include $filename;
        $this->filesystem = new Filesystem($adapter);
    }

    public function tearDown()
    {
        if (null === $this->filesystem) {
            return;
        }

        $this->filesystem = null;
    }

    /**
     * @test
     * @group functional
     */
    public function shouldWriteAndRead()
    {
        $this->assertEquals(12, $this->filesystem->write('foo', 'Some content'));
        $this->assertEquals(13, $this->filesystem->write('test/subdir/foo', 'Some content1', true));

        $this->assertEquals('Some content', $this->filesystem->read('foo'));
        $this->assertEquals('Some content1', $this->filesystem->read('test/subdir/foo'));
        $this->filesystem->delete('foo');
        $this->filesystem->delete('test/subdir/foo');
    }

    /**
     * @test
     * @group functional
     */
    public function shouldUpdateFileContent()
    {
        $this->filesystem->write('foo', 'Some content');
        $this->filesystem->write('foo', 'Some content updated', true);

        $this->assertEquals('Some content updated', $this->filesystem->read('foo'));
        $this->filesystem->delete('foo');
    }

    /**
     * @test
     * @group functional
     */
    public function shouldCheckIfFileExists()
    {
        $this->assertFalse($this->filesystem->has('foo'));

        $this->filesystem->write('foo', 'Some content');

        $this->assertTrue($this->filesystem->has('foo'));
        $this->assertFalse($this->filesystem->has('test/somefile'));
        $this->assertFalse($this->filesystem->has('test/somefile'));

        $this->filesystem->delete('foo');
    }

    /**
     * @test
     * @group functional
     */
    public function shouldGetMtime()
    {
        $this->filesystem->write('foo', 'Some content');

        $this->assertGreaterThan(0, $this->filesystem->mtime('foo'));

        $this->filesystem->delete('foo');
    }

    /**
     * @test
     * @group functional
     * @expectedException \RuntimeException
     * @expectedMessage Could not get mtime for the "foo" key
     */
    public function shouldFailWhenTryMtimeForKeyWhichDoesNotExist()
    {
        $this->assertFalse($this->filesystem->mtime('foo'));
    }

    /**
     * @test
     * @group functional
     */
    public function shouldRenameFile()
    {
        $this->filesystem->write('foo', 'Some content');
        $this->filesystem->rename('foo', 'boo');

        $this->assertFalse($this->filesystem->has('foo'));
        $this->assertEquals('Some content', $this->filesystem->read('boo'));
        $this->filesystem->delete('boo');

        $this->filesystem->write('foo', 'Some content');
        $this->filesystem->rename('foo', 'somedir/sub/boo');

        $this->assertFalse($this->filesystem->has('somedir/sub/foo'));
        $this->assertEquals('Some content', $this->filesystem->read('somedir/sub/boo'));
        $this->filesystem->delete('somedir/sub/boo');
    }

    /**
     * @test
     * @group functional
     */
    public function shouldDeleteFile()
    {
        $this->filesystem->write('foo', 'Some content');

        $this->assertTrue($this->filesystem->has('foo'));

        $this->filesystem->delete('foo');

        $this->assertFalse($this->filesystem->has('foo'));
    }

    /**
     * @test
     * @group functional
     */
    public function shouldFetchKeys()
    {
        $this->assertEquals(array(), $this->filesystem->keys());

        $this->filesystem->write('foo', 'Some content');
        $this->filesystem->write('bar', 'Some content');
        $this->filesystem->write('baz', 'Some content');

        $actualKeys = $this->filesystem->keys();

        $this->assertEquals(3, count($actualKeys));
        foreach (array('foo', 'bar', 'baz') as $key) {
            $this->assertContains($key, $actualKeys);
        }

        $this->filesystem->delete('foo');
        $this->filesystem->delete('bar');
        $this->filesystem->delete('baz');
    }

    /**
     * @test
     * @group functional
     */
    public function shouldWorkWithHiddenFiles()
    {
        $this->filesystem->write('.foo', 'hidden');
        $this->assertTrue($this->filesystem->has('.foo'));
        $this->assertContains('.foo', $this->filesystem->keys());
        $this->filesystem->delete('.foo');
        $this->assertFalse($this->filesystem->has('.foo'));
    }
}

class LocalTest extends FunctionalTestCase
{
    private $directory;
    
    public function __construct () 
    {
        $this->directory = sprintf('%s/filesystem', str_replace('\\', '/', __DIR__));
    }

    public function setUp()
    {
        if (!file_exists($this->directory)) {
            mkdir($this->directory);
        }

        $this->filesystem = new Filesystem(new LocalAdapter($this->directory));
    }

    public function tearDown()
    {
        $this->filesystem = null;

        if (file_exists($this->directory)) {
            $iterator = new RecursiveIteratorIterator(
                new RecursiveDirectoryIterator(
                    $this->directory,
                    FilesystemIterator::SKIP_DOTS | FilesystemIterator::UNIX_PATHS
                )
            );

            foreach ($iterator as $item) {
                if ($item->isDir()) {
                    rmdir(strval($item));
                } else {
                    unlink(strval($item));
                }
            }
        }
    }

    /**
     * @test
     * @group functional
     */
    public function shouldWorkWithSyslink()
    {
        $dirname = sprintf(
            '%s/adapters',
            dirname(__DIR__)
        );
        $linkname = sprintf(
            '%s/link',
            dirname(__DIR__)
        );

        @mkdir($dirname);
        @unlink($linkname);
        symlink($dirname, $linkname);

        $this->filesystem = new Filesystem(new LocalAdapter($linkname));
        $this->filesystem->write('test.txt', 'abc 123');

        $this->assertSame('abc 123', $this->filesystem->read('test.txt'));
        $this->filesystem->delete('test.txt');
        @unlink($linkname);
        @rmdir($dirname);
    }

    /**
     * @test
     * @covers Gaufrette\Adapter\Local
     */
    public function shouldListingOnlyGivenDirectory()
    {
        $dirname = sprintf(
            '%s/localDir',
            $this->directory
        );
        @mkdir($dirname);

        $this->filesystem = new Filesystem(new LocalAdapter($this->directory));
        $this->filesystem->write('aaa.txt', 'some content');
        $this->filesystem->write('localDir/test.txt', 'some content');

        $dirs = $this->filesystem->listKeys('localDir/test');
        $this->assertEmpty($dirs['dirs']);
        $this->assertCount(1, $dirs['keys']);
        $this->assertEquals('localDir/test.txt', $dirs['keys'][0]);

        $dirs = $this->filesystem->listKeys();

        $this->assertCount(1, $dirs['dirs']);
        $this->assertEquals('localDir', $dirs['dirs'][0]);
        $this->assertCount(2, $dirs['keys']);
        $this->assertEquals('aaa.txt', $dirs['keys'][0]);
        $this->assertEquals('localDir/test.txt', $dirs['keys'][1]);

        @unlink($dirname.DIRECTORY_SEPARATOR.'test.txt');
        @unlink($this->directory.DIRECTORY_SEPARATOR.'aaa.txt');
        @rmdir($dirname);
    }
}

$test = new LocalTest ();
$test->tearDown();
$test->setUp();
$test->shouldWriteAndRead();
$test->tearDown();
$test->setUp();
$test->shouldUpdateFileContent();
$test->tearDown();
$test->setUp();
$test->shouldCheckIfFileExists();
$test->tearDown();
$test->setUp();
$test->shouldGetMtime();
$test->tearDown();
$test->setUp();
try {
    $test->shouldFailWhenTryMtimeForKeyWhichDoesNotExist();
} catch (Exception $e) {}
$test->tearDown();
$test->setUp();
$test->shouldRenameFile();
$test->tearDown();
$test->setUp();
$test->shouldDeleteFile();
$test->tearDown();
$test->setUp();
$test->shouldFetchKeys();
$test->tearDown();
$test->setUp();
$test->shouldWorkWithHiddenFiles();
$test->tearDown();
$test->setUp();
$test->shouldWorkWithSyslink();
$test->tearDown();
$test->setUp();
$test->shouldListingOnlyGivenDirectory();
$test->tearDown();



class ZipTest extends FunctionalTestCase
{
    public function setUp()
    {
        if (!extension_loaded('zip')) {
            return $this->markTestSkipped('The zip extension is not available.');
        }

        @touch(__DIR__ . '/test.zip');
        
        $this->filesystem = new Filesystem(new ZipAdapter(__DIR__ . '/test.zip'));
    }

    public function tearDown()
    {
        parent::tearDown();

        @unlink(__DIR__ . '/test.zip');
    }

    /**
     * @test
     * @expectedException \RuntimeException
     * @group functional
     */
    public function shouldNotAcceptInvalidZipArchive()
    {
        new ZipAdapter(__FILE__);
    }

    /**
     * @test
     * @group functional
     */
    public function shouldCreateNewZipArchive()
    {
        $tmp = tempnam(sys_get_temp_dir(), uniqid());
        $za = new ZipAdapter($tmp);

        $this->assertFileExists($tmp);

        return $za;
    }
}

$test = new ZipTest ();
$test->tearDown();
$test->setUp();
$test->shouldWriteAndRead();
$test->tearDown();
$test->setUp();
$test->shouldUpdateFileContent();
$test->tearDown();
$test->setUp();
$test->shouldCheckIfFileExists();
$test->tearDown();
$test->setUp();
$test->shouldGetMtime();
$test->tearDown();
$test->setUp();
try {
    $test->shouldFailWhenTryMtimeForKeyWhichDoesNotExist();
} catch (Exception $e) {}
$test->tearDown();
$test->setUp();
$test->shouldRenameFile();
$test->tearDown();
$test->setUp();
$test->shouldDeleteFile();
$test->tearDown();
$test->setUp();
$test->shouldFetchKeys();
$test->tearDown();
$test->setUp();
$test->shouldWorkWithHiddenFiles();
$test->tearDown();
$test->setUp();
try {
    $test->shouldNotAcceptInvalidZipArchive();
} catch (Exception $e) { }
$test->tearDown();
$test->setUp();
$test->shouldCreateNewZipArchive();
$test->tearDown();

?>