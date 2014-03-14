<?php
	interface Adapter
	{
		public function read($key);
		public function write($key, $content);
		public function exists($key);
		public function keys();
		public function mtime($key);
		public function delete($key);
		public function rename($sourceKey, $targetKey);
		public function isDirectory($key);
	}
	class File
	{
		protected $key;
		protected $filesystem;
		protected $content = NULL;
		protected $metadata = NULL;
		protected $name = NULL;
		protected $size = 0;
		protected $mtime = NULL;
		public function __construct($key, Filesystem $filesystem)
		{
			$TSNNt805 = $key;
			$this->key = $TSNNt805;
			$TSNNt806 = $key;
			$this->name = $TSNNt806;
			$TSNNt807 = $filesystem;
			$this->filesystem = $TSNNt807;
		}
		public function getKey()
		{
			$TSNNt808 = $this->key;
			return $TSNNt808;
		}
		public function getContent($metadata = array())
		{
			$TSNNt809 = $this->content;
			$TLE57 = isset($TSNNt809);
			if ($TLE57) {
				$TSNNt810 = $this->content;
				return $TSNNt810;
			}
			$this->setMetadata($metadata);
			$TSNNt811 = $this->filesystem;
			$TSNNt812 = $this->key;
			$TLE58 = $TSNNt811->read($TSNNt812);
			$TSNNt813 = $TLE58;
			$this->content = $TSNNt813;
			$TSNNt814 = $this->content;
			$TLE59 = $TSNNt814;
			return $TLE59;
		}
		public function getName()
		{
			$TSNNt815 = $this->name;
			return $TSNNt815;
		}
		public function getSize()
		{
			$TSNNt816 = $this->size;
			if ($TSNNt816) {
				$TSNNt817 = $this->size;
				return $TSNNt817;
			}
			try {
				$TLE60 = $this->getContent();
				$TLE61 = Size::fromContent($TLE60);
				$TSNNt818 = $TLE61;
				$this->size = $TSNNt818;
				$TSNNt819 = $this->size;
				$TLE62 = $TSNNt819;
				return $TLE62;
			} catch (FileNotFound $exception) {
			}
			$TLE63 = 0;
			return $TLE63;
		}
		public function getMtime()
		{
			$TSNNt820 = $this->filesystem;
			$TSNNt821 = $this->key;
			$TLE64 = $TSNNt820->mtime($TSNNt821);
			$TSNNt822 = $TLE64;
			$this->mtime = $TSNNt822;
			$TSNNt823 = $this->mtime;
			$TLE65 = $TSNNt823;
			return $TLE65;
		}
		public function setSize($size)
		{
			$TSNNt824 = $size;
			$this->size = $TSNNt824;
		}
		public function setContent($content, $metadata = array())
		{
			$TSNNt825 = $content;
			$this->content = $TSNNt825;
			$this->setMetadata($metadata);
			$TLE66 = True;
			$TSNNt826 = $this->filesystem;
			$TSNNt827 = $this->key;
			$TSNNt828 = $this->content;
			$TLE67 = $TSNNt826->write($TSNNt827, $TSNNt828, $TLE66);
			$TSNNt829 = $TLE67;
			$this->size = $TSNNt829;
			$TSNNt830 = $this->size;
			$TLE68 = $TSNNt830;
			return $TLE68;
		}
		public function setName($name)
		{
			$TSNNt831 = $name;
			$this->name = $TSNNt831;
		}
		public function exists()
		{
			$TSNNt832 = $this->filesystem;
			$TSNNt833 = $this->key;
			$TLE69 = $TSNNt832->has($TSNNt833);
			return $TLE69;
		}
		public function delete($metadata = array())
		{
			$this->setMetadata($metadata);
			$TSNNt834 = $this->filesystem;
			$TSNNt835 = $this->key;
			$TLE70 = $TSNNt834->delete($TSNNt835);
			return $TLE70;
		}
		public function createStream()
		{
			$TSNNt836 = $this->filesystem;
			$TSNNt837 = $this->key;
			$TLE71 = $TSNNt836->createStream($TSNNt837);
			return $TLE71;
		}
		protected function setMetadata(array $metadata)
		{
			$TLE24 = $metadata;
			if ($TLE24) {
				$TEF25 = $this->supportsMetadata();
			} else {
				$TEF25 = $TLE24;
			}
			$TLE72 = (bool) $TEF25;
			if ($TLE72) {
				$TSNNt838 = $this->filesystem;
				$TLE73 = $TSNNt838->getAdapter();
				$TSNNt839 = $this->key;
				$TLE73->setMetadata($TSNNt839, $metadata);
				$TLE74 = True;
				return $TLE74;
			}
			$TLE75 = False;
			return $TLE75;
		}
		private function supportsMetadata()
		{
			$TSNNt840 = $this->filesystem;
			$TLE76 = $TSNNt840->getAdapter();
			$TLE77 = $TLE76 instanceof MetadataSupporter;
			return $TLE77;
		}
	}
	class Filesystem
	{
		protected $adapter;
		public function __construct(Adapter $adapter)
		{
			$TSNNt841 = $adapter;
			$this->adapter = $TSNNt841;
		}
		public function getAdapter()
		{
			$TSNNt842 = $this->adapter;
			return $TSNNt842;
		}
		public function has($key)
		{
			$TSNNt843 = $this->adapter;
			$TLE78 = $TSNNt843->exists($key);
			return $TLE78;
		}
		public function rename($sourceKey, $targetKey)
		{
			$this->assertHasFile($sourceKey);
			$TLE79 = $this->has($targetKey);
			if ($TLE79) {
				$TLE80 = new UnexpectedFile($targetKey);
				throw $TLE80;
			}
			$TSNNt844 = $this->adapter;
			$TLE81 = $TSNNt844->rename($sourceKey, $targetKey);
			$TLE82 = !$TLE81;
			if ($TLE82) {
				$TLE83 = 'Could not rename the "%s" key to "%s".';
				$TLE84 = sprintf($TLE83, $sourceKey, $targetKey);
				$TLE85 = new RuntimeException($TLE84);
				throw $TLE85;
			}
			$TLE86 = True;
			return $TLE86;
		}
		public function get($key, $create = False)
		{
			$TLE87 = !$create;
			if ($TLE87) {
				$this->assertHasFile($key);
			}
			$TLE88 = $this->createFile($key);
			return $TLE88;
		}
		public function write($key, $content, $overwrite = False)
		{
			$TLE26 = !$overwrite;
			if ($TLE26) {
				$TEF27 = $this->has($key);
			} else {
				$TEF27 = $TLE26;
			}
			$TLE89 = (bool) $TEF27;
			if ($TLE89) {
				$TLE90 = new FileAlreadyExists($key);
				throw $TLE90;
			}
			$TSNNt845 = $this->adapter;
			$numBytes = $TSNNt845->write($key, $content);
			$TLE91 = False;
			$TLE92 = ($TLE91 === $numBytes);
			if ($TLE92) {
				$TLE93 = 'Could not write the "%s" key content.';
				$TLE94 = sprintf($TLE93, $key);
				$TLE95 = new RuntimeException($TLE94);
				throw $TLE95;
			}
			return $numBytes;
		}
		public function read($key)
		{
			$this->assertHasFile($key);
			$TSNNt846 = $this->adapter;
			$content = $TSNNt846->read($key);
			$TLE96 = False;
			$TLE97 = ($TLE96 === $content);
			if ($TLE97) {
				$TLE98 = 'Could not read the "%s" key content.';
				$TLE99 = sprintf($TLE98, $key);
				$TLE100 = new RuntimeException($TLE99);
				throw $TLE100;
			}
			return $content;
		}
		public function delete($key)
		{
			$this->assertHasFile($key);
			$TSNNt847 = $this->adapter;
			$TLE101 = $TSNNt847->delete($key);
			if ($TLE101) {
				$TLE102 = True;
				return $TLE102;
			}
			$TLE103 = 'Could not remove the "%s" key.';
			$TLE104 = sprintf($TLE103, $key);
			$TLE105 = new RuntimeException($TLE104);
			throw $TLE105;
		}
		public function keys()
		{
			$TSNNt848 = $this->adapter;
			$TLE106 = $TSNNt848->keys();
			return $TLE106;
		}
		public function listKeys($prefix = '')
		{
			$TSNNt849 = $this->adapter;
			$TLE107 = $TSNNt849 instanceof ListKeysAware;
			if ($TLE107) {
				$TSNNt850 = $this->adapter;
				$TLE108 = $TSNNt850->listKeys($prefix);
				return $TLE108;
			}
			unset($TSa109);
			$TSa109 = (array) $TSa109;
			$dirs = $TSa109;
			unset($TSa110);
			$TSa110 = (array) $TSa110;
			$keys = $TSa110;
			$TLE111 = $this->keys();
			foreach ($TLE111 as $key) {
				$TLE28 = empty($prefix);
				if ($TLE28) {
					$TEF29 = $TLE28;
				} else {
					$TLE112 = 0;
					$TLE113 = strpos($key, $prefix);
					$TEF29 = ($TLE112 === $TLE113);
				}
				$TLE114 = (bool) $TEF29;
				if ($TLE114) {
					$TSNNt851 = $this->adapter;
					$TLE115 = $TSNNt851->isDirectory($key);
					if ($TLE115) {
						$dirs[] = $key;
					} else {
						$keys[] = $key;
					}
				}
			}
			$TLE116 = 'keys';
			$TLE117 = 'dirs';
			unset($TSa118);
			$TSa118 = (array) $TSa118;
			$TSNNi852 = $keys;
			$TSa118[$TLE116] = $TSNNi852;
			$TSNNi853 = $dirs;
			$TSa118[$TLE117] = $TSNNi853;
			return $TSa118;
		}
		public function mtime($key)
		{
			$this->assertHasFile($key);
			$TSNNt854 = $this->adapter;
			$TLE119 = $TSNNt854->mtime($key);
			return $TLE119;
		}
		public function checksum($key)
		{
			$this->assertHasFile($key);
			$TSNNt855 = $this->adapter;
			$TLE120 = $TSNNt855 instanceof ChecksumCalculator;
			if ($TLE120) {
				$TSNNt856 = $this->adapter;
				$TLE121 = $TSNNt856->checksum($key);
				return $TLE121;
			}
			$TLE122 = $this->read($key);
			$TLE123 = Checksum::fromContent($TLE122);
			return $TLE123;
		}
		public function createStream($key)
		{
			$TSNNt857 = $this->adapter;
			$TLE124 = $TSNNt857 instanceof StreamFactory;
			if ($TLE124) {
				$TSNNt858 = $this->adapter;
				$TLE125 = $TSNNt858->createStream($key);
				return $TLE125;
			}
			$TLE126 = new InMemoryBuffer($this, $key);
			return $TLE126;
		}
		public function createFile($key)
		{
			$TSNNt859 = $this->adapter;
			$TLE127 = $TSNNt859 instanceof FileFactory;
			if ($TLE127) {
				$TSNNt860 = $this->adapter;
				$TLE128 = $TSNNt860->createFile($key, $this);
				return $TLE128;
			}
			$TLE129 = new File($key, $this);
			return $TLE129;
		}
		private function assertHasFile($key)
		{
			$TLE130 = empty($key);
			$TLE30 = !$TLE130;
			if ($TLE30) {
				$TLE131 = $this->has($key);
				$TEF31 = !$TLE131;
			} else {
				$TEF31 = $TLE30;
			}
			$TLE132 = (bool) $TEF31;
			if ($TLE132) {
				$TLE133 = new FileNotFound($key);
				throw $TLE133;
			}
		}
	}
	class FilesystemMap
	{
		private $filesystems = array();
		public function all()
		{
			$TSNNt861 = $this->filesystems;
			return $TSNNt861;
		}
		public function set($domain, Filesystem $filesystem)
		{
			$TLE134 = '/^[-_a-zA-Z0-9]+$/';
			$TLE135 = preg_match($TLE134, $domain);
			$TLE136 = !$TLE135;
			if ($TLE136) {
				$TLE137 = 'The specified domain "%s" is not a valid domain.';
				$TLE138 = sprintf($TLE137, $domain);
				$TLE139 = new InvalidArgumentException($TLE138);
				throw $TLE139;
			}
			$TSNNt862 = $this->filesystems;
			$TSNNt862[$domain] = $filesystem;
			$this->filesystems = $TSNNt862;
		}
		public function has($domain)
		{
			$TSNNt863 = $this->filesystems;
			$TSNNi864 = $TSNNt863[$domain];
			$TLE140 = isset($TSNNi864);
			$TSNNt863[$domain] = $TSNNi864;
			$this->filesystems = $TSNNt863;
			return $TLE140;
		}
		public function get($domain)
		{
			$TLE141 = $this->has($domain);
			$TLE142 = !$TLE141;
			if ($TLE142) {
				$TLE143 = 'There is no filesystem defined for the "%s" domain.';
				$TLE144 = sprintf($TLE143, $domain);
				$TLE145 = new InvalidArgumentException($TLE144);
				throw $TLE145;
			}
			$TSNNt865 = $this->filesystems;
			$TSNNi866 = $TSNNt865[$domain];
			return $TSNNi866;
		}
		public function remove($domain)
		{
			$TLE146 = $this->has($domain);
			$TLE147 = !$TLE146;
			if ($TLE147) {
				$TLE148 = 'Cannot remove the "%s" filesystem as it is not defined.';
				$TLE149 = sprintf($TLE148, $domain);
				$TLE150 = new InvalidArgumentException($TLE149);
				throw $TLE150;
			}
			$TSNNt867 = $this->filesystems;
			$TSNNi868 = $TSNNt867[$domain];
			unset($TSNNi868);
			$TSNNt867[$domain] = $TSNNi868;
			$this->filesystems = $TSNNt867;
		}
		public function clear()
		{
			unset($TSa151);
			$TSa151 = (array) $TSa151;
			$TSNNt869 = $TSa151;
			$this->filesystems = $TSNNt869;
		}
	}
	interface Stream
	{
		public function open(StreamMode $mode);
		public function read($count);
		public function write($data);
		public function close();
		public function flush();
		public function seek($offset, $whence = SEEK_SET);
		public function tell();
		public function eof();
		public function stat();
		public function cast($castAs);
		public function unlink();
	}
	class StreamMode
	{
		private $mode;
		private $base;
		private $plus;
		private $flag;
		public function __construct($mode)
		{
			$TSNNt870 = $mode;
			$this->mode = $TSNNt870;
			$TLE152 = 0;
			$TLE153 = 3;
			$mode = substr($mode, $TLE152, $TLE153);
			$TLE154 = 1;
			$rest = substr($mode, $TLE154);
			$TLE155 = 0;
			$TLE156 = 1;
			$TLE157 = substr($mode, $TLE155, $TLE156);
			$TSNNt871 = $TLE157;
			$this->base = $TSNNt871;
			$TLE158 = False;
			$TLE159 = '+';
			$TLE160 = strpos($rest, $TLE159);
			$TLE161 = ($TLE158 !== $TLE160);
			$TSNNt872 = $TLE161;
			$this->plus = $TSNNt872;
			$TLE162 = '+';
			$TLE163 = trim($rest, $TLE162);
			$TSNNt873 = $TLE163;
			$this->flag = $TSNNt873;
		}
		public function getMode()
		{
			$TSNNt874 = $this->mode;
			return $TSNNt874;
		}
		public function allowsRead()
		{
			$TSNNt875 = $this->plus;
			if ($TSNNt875) {
				$TLE164 = True;
				return $TLE164;
			}
			$TLE165 = 'r';
			$TSNNt876 = $this->base;
			$TLE166 = ($TLE165 === $TSNNt876);
			return $TLE166;
		}
		public function allowsWrite()
		{
			$TSNNt877 = $this->plus;
			if ($TSNNt877) {
				$TLE167 = True;
				return $TLE167;
			}
			$TLE168 = 'r';
			$TSNNt878 = $this->base;
			$TLE169 = ($TLE168 !== $TSNNt878);
			return $TLE169;
		}
		public function allowsExistingFileOpening()
		{
			$TLE170 = 'x';
			$TSNNt879 = $this->base;
			$TLE171 = ($TLE170 !== $TSNNt879);
			return $TLE171;
		}
		public function allowsNewFileOpening()
		{
			$TLE172 = 'r';
			$TSNNt880 = $this->base;
			$TLE173 = ($TLE172 !== $TSNNt880);
			return $TLE173;
		}
		public function impliesExistingContentDeletion()
		{
			$TLE174 = 'w';
			$TSNNt881 = $this->base;
			$TLE175 = ($TLE174 === $TSNNt881);
			return $TLE175;
		}
		public function impliesPositioningCursorAtTheBeginning()
		{
			$TLE176 = 'a';
			$TSNNt882 = $this->base;
			$TLE177 = ($TLE176 !== $TSNNt882);
			return $TLE177;
		}
		public function impliesPositioningCursorAtTheEnd()
		{
			$TLE178 = 'a';
			$TSNNt883 = $this->base;
			$TLE179 = ($TLE178 === $TSNNt883);
			return $TLE179;
		}
		public function isBinary()
		{
			$TLE180 = 'b';
			$TSNNt884 = $this->flag;
			$TLE181 = ($TLE180 === $TSNNt884);
			return $TLE181;
		}
		public function isText()
		{
			$TLE182 = False;
			$TLE183 = $this->isBinary();
			$TLE184 = ($TLE182 === $TLE183);
			return $TLE184;
		}
	}
	class Checksum
	{
		public static function fromContent($content)
		{
			$TLE185 = md5($content);
			return $TLE185;
		}
		public static function fromFile($filename)
		{
			$TLE186 = md5_file($filename);
			return $TLE186;
		}
	}
	class Path
	{
		public static function normalize($path)
		{
			$TLE187 = '\\';
			$TLE188 = '/';
			$path = str_replace($TLE187, $TLE188, $path);
			$prefix = Path::getAbsolutePrefix($path);
			$TLE189 = strlen($prefix);
			$path = substr($path, $TLE189);
			$TLE190 = '/';
			$TLE191 = explode($TLE190, $path);
			$TLE192 = 'strlen';
			$parts = array_filter($TLE191, $TLE192);
			unset($TSa193);
			$TSa193 = (array) $TSa193;
			$tokens = $TSa193;
			foreach ($parts as $part) {
				$TEL0 = $part;
				$TSM1 = False;
				$ElcfPD4 = True;
				while (True) {
					if ($ElcfPD4) {
						$ElcfPD4 = False;
					} else {
						$TLE194 = False;
						$TLE195 = !$TLE194;
						if ($TLE195) {
							break;
						}
					}
					if ($TSM1) {
					} else {
						$TL2 = '.';
						$TLE196 = ($TEL0 == $TL2);
						if ($TLE196) {
							$TSM1 = True;
						}
					}
					if ($TSM1) {
						continue;
					}
					if ($TSM1) {
					} else {
						$TL3 = '..';
						$TLE197 = ($TEL0 == $TL3);
						if ($TLE197) {
							$TSM1 = True;
						}
					}
					if ($TSM1) {
						$TLE198 = 0;
						$TLE199 = count($tokens);
						$TLE200 = ($TLE198 !== $TLE199);
						if ($TLE200) {
							array_pop($tokens);
							continue;
						} else {
							$TLE201 = empty($prefix);
							$TLE202 = !$TLE201;
							if ($TLE202) {
								continue;
							}
						}
					}
					if ($TSM1) {
						$tokens[] = $part;
					}
					if ($TSM1) {
					} else {
						$tokens[] = $part;
					}
				}
			}
			$TLE203 = '/';
			$TLE204 = implode($TLE203, $tokens);
			$TLE205 = ($prefix . $TLE204);
			return $TLE205;
		}
		public static function isAbsolute($path)
		{
			$TLE206 = '';
			$TLE207 = Path::getAbsolutePrefix($path);
			$TLE208 = ($TLE206 !== $TLE207);
			return $TLE208;
		}
		public static function getAbsolutePrefix($path)
		{
			$TLE209 = '|^(?P<prefix>([a-zA-Z]:)?/)|';
			preg_match($TLE209, $path, $matches);
			$TLE210 = 'prefix';
			$TSNNi885 = $matches[$TLE210];
			$TLE211 = empty($TSNNi885);
			if ($TLE211) {
				$TLE212 = '';
				return $TLE212;
			}
			$TLE213 = 'prefix';
			$TSNNi886 = $matches[$TLE213];
			$TLE214 = strtolower($TSNNi886);
			return $TLE214;
		}
	}
	class Size
	{
		public static function fromContent($content)
		{
			$TLE215 = '8bit';
			$TLE216 = mb_strlen($content, $TLE215);
			return $TLE216;
		}
	}
	interface GaufretteException
	{
	}
	class FileAlreadyExists extends RuntimeException implements GaufretteException
	{
		private $key;
		public function __construct($key, $code = 0, Exception $previous = NULL)
		{
			$TSNNt887 = $key;
			$this->key = $TSNNt887;
			$TLE217 = 'The file %s already exists and can not be overwritten.';
			$TLE218 = sprintf($TLE217, $key);
			RuntimeException::__construct($TLE218, $code, $previous);
		}
		public function getKey()
		{
			$TSNNt888 = $this->key;
			return $TSNNt888;
		}
	}
	class FileNotFound extends RuntimeException implements GaufretteException
	{
		private $key;
		public function __construct($key, $code = 0, Exception $previous = NULL)
		{
			$TSNNt889 = $key;
			$this->key = $TSNNt889;
			$TLE219 = 'The file "%s" was not found.';
			$TLE220 = sprintf($TLE219, $key);
			RuntimeException::__construct($TLE220, $code, $previous);
		}
		public function getKey()
		{
			$TSNNt890 = $this->key;
			return $TSNNt890;
		}
	}
	class UnexpectedFile extends RuntimeException implements GaufretteException
	{
		private $key;
		public function __construct($key, $code = 0, Exception $previous = NULL)
		{
			$TSNNt891 = $key;
			$this->key = $TSNNt891;
			$TLE221 = 'The file "%s" was not supposed to exist.';
			$TLE222 = sprintf($TLE221, $key);
			RuntimeException::__construct($TLE222, $code, $previous);
		}
		public function getKey()
		{
			$TSNNt892 = $this->key;
			return $TSNNt892;
		}
	}
	interface ChecksumCalculator
	{
		public function checksum($key);
	}
	interface StreamFactory
	{
		public function createStream($key);
	}
	interface FileFactory
	{
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
		public function __construct(Filesystem $filesystem, $key)
		{
			$TSNNt893 = $filesystem;
			$this->filesystem = $TSNNt893;
			$TSNNt894 = $key;
			$this->key = $TSNNt894;
		}
		public function open(StreamMode $mode)
		{
			$TSNNt895 = $mode;
			$this->mode = $TSNNt895;
			$TSNNt896 = $this->filesystem;
			$TSNNt897 = $this->key;
			$exists = $TSNNt896->has($TSNNt897);
			$TLE32 = $exists;
			if ($TLE32) {
				$TLE223 = $mode->allowsExistingFileOpening();
				$TEF33 = !$TLE223;
			} else {
				$TEF33 = $TLE32;
			}
			$TLE34 = !$exists;
			if ($TLE34) {
				$TLE224 = $mode->allowsNewFileOpening();
				$TEF35 = !$TLE224;
			} else {
				$TEF35 = $TLE34;
			}
			$TLE36 = (bool) $TEF33;
			if ($TLE36) {
				$TEF37 = $TLE36;
			} else {
				$TEF37 = (bool) $TEF35;
			}
			$TLE225 = (bool) $TEF37;
			if ($TLE225) {
				$TLE226 = False;
				return $TLE226;
			}
			$TLE227 = $mode->impliesExistingContentDeletion();
			if ($TLE227) {
				$TLE228 = '';
				$TLE229 = $this->writeContent($TLE228);
				$TSNNt898 = $TLE229;
				$this->content = $TSNNt898;
			} else {
				$TLE38 = !$exists;
				if ($TLE38) {
					$TEF39 = $mode->allowsNewFileOpening();
				} else {
					$TEF39 = $TLE38;
				}
				$TLE230 = (bool) $TEF39;
				if ($TLE230) {
					$TLE231 = '';
					$TLE232 = $this->writeContent($TLE231);
					$TSNNt899 = $TLE232;
					$this->content = $TSNNt899;
				} else {
					$TSNNt900 = $this->filesystem;
					$TSNNt901 = $this->key;
					$TLE233 = $TSNNt900->read($TSNNt901);
					$TSNNt902 = $TLE233;
					$this->content = $TSNNt902;
				}
			}
			$TSNNt903 = $this->content;
			$TLE234 = Size::fromContent($TSNNt903);
			$TSNNt904 = $TLE234;
			$this->numBytes = $TSNNt904;
			$TLE235 = $mode->impliesPositioningCursorAtTheEnd();
			if ($TLE235) {
				$TSNNt905 = $this->numBytes;
				$TEF40 = $TSNNt905;
			} else {
				$TEF40 = 0;
			}
			$TSNNt906 = $TEF40;
			$this->position = $TSNNt906;
			$TLE236 = True;
			$TSNNt907 = $TLE236;
			$this->synchronized = $TSNNt907;
			$TLE237 = True;
			return $TLE237;
		}
		public function read($count)
		{
			$TLE238 = False;
			$TSNNt908 = $this->mode;
			$TLE239 = $TSNNt908->allowsRead();
			$TLE240 = ($TLE238 === $TLE239);
			if ($TLE240) {
				$TLE241 = 'The stream does not allow read.';
				$TLE242 = new LogicException($TLE241);
				throw $TLE242;
			}
			$TSNNt909 = $this->content;
			$TSNNt910 = $this->position;
			$chunk = substr($TSNNt909, $TSNNt910, $count);
			$TSNNt911 = $this->position;
			$Toa53 = $TSNNt911;
			$TLE243 = Size::fromContent($chunk);
			$Toa53 = ($Toa53 + $TLE243);
			$TSNNt912 = $Toa53;
			$this->position = $TSNNt912;
			return $chunk;
		}
		public function write($data)
		{
			$TLE244 = False;
			$TSNNt913 = $this->mode;
			$TLE245 = $TSNNt913->allowsWrite();
			$TLE246 = ($TLE244 === $TLE245);
			if ($TLE246) {
				$TLE247 = 'The stream does not allow write.';
				$TLE248 = new LogicException($TLE247);
				throw $TLE248;
			}
			$numWrittenBytes = Size::fromContent($data);
			$TSNNt914 = $this->position;
			$newPosition = ($TSNNt914 + $numWrittenBytes);
			$TSNNt915 = $this->numBytes;
			$TLE249 = ($TSNNt915 < $newPosition);
			if ($TLE249) {
				$TEF41 = $newPosition;
			} else {
				$TSNNt916 = $this->numBytes;
				$TEF41 = $TSNNt916;
			}
			$newNumBytes = $TEF41;
			$TLE250 = $this->eof();
			if ($TLE250) {
				$TSNNt917 = $this->numBytes;
				$Toa54 = $TSNNt917;
				$Toa54 = ($Toa54 + $numWrittenBytes);
				$TSNNt918 = $Toa54;
				$this->numBytes = $TSNNt918;
				$TLE251 = $this->hasNewContentAtFurtherPosition();
				if ($TLE251) {
					$TLE252 = strlen($data);
					$TSNNt919 = $this->position;
					$TLE253 = ($TSNNt919 + $TLE252);
					$TLE254 = " ";
					$TLE255 = STR_PAD_LEFT;
					$data = str_pad($data, $TLE253, $TLE254, $TLE255);
				}
				$TSNNt920 = $this->content;
				$Toa55 = $TSNNt920;
				$Toa55 = ($Toa55 . $data);
				$TSNNt921 = $Toa55;
				$this->content = $TSNNt921;
			} else {
				$TLE256 = 0;
				$TSNNt922 = $this->content;
				$TSNNt923 = $this->position;
				$before = substr($TSNNt922, $TLE256, $TSNNt923);
				$TLE257 = ($newPosition < $newNumBytes);
				if ($TLE257) {
					$TSNNt924 = $this->content;
					$TEF42 = substr($TSNNt924, $newPosition);
				} else {
					$TEF42 = '';
				}
				$after = $TEF42;
				$TLE258 = ($before . $data);
				$TLE259 = ($TLE258 . $after);
				$TSNNt925 = $TLE259;
				$this->content = $TSNNt925;
			}
			$TSNNt926 = $newPosition;
			$this->position = $TSNNt926;
			$TSNNt927 = $newNumBytes;
			$this->numBytes = $TSNNt927;
			$TLE260 = False;
			$TSNNt928 = $TLE260;
			$this->synchronized = $TSNNt928;
			return $numWrittenBytes;
		}
		public function close()
		{
			$TSNNt929 = $this->synchronized;
			$TLE261 = !$TSNNt929;
			if ($TLE261) {
				$this->flush();
			}
		}
		public function seek($offset, $whence = SEEK_SET)
		{
			$TEL5 = $whence;
			$TSM6 = False;
			$ElcfPD10 = True;
			while (True) {
				if ($ElcfPD10) {
					$ElcfPD10 = False;
				} else {
					$TLE262 = False;
					$TLE263 = !$TLE262;
					if ($TLE263) {
						break;
					}
				}
				if ($TSM6) {
				} else {
					$TL7 = SEEK_SET;
					$TLE264 = ($TEL5 == $TL7);
					if ($TLE264) {
						$TSM6 = True;
					}
				}
				if ($TSM6) {
					$TSNNt930 = $offset;
					$this->position = $TSNNt930;
					break;
				}
				if ($TSM6) {
				} else {
					$TL8 = SEEK_CUR;
					$TLE265 = ($TEL5 == $TL8);
					if ($TLE265) {
						$TSM6 = True;
					}
				}
				if ($TSM6) {
					$TSNNt931 = $this->position;
					$Toa56 = $TSNNt931;
					$Toa56 = ($Toa56 + $offset);
					$TSNNt932 = $Toa56;
					$this->position = $TSNNt932;
					break;
				}
				if ($TSM6) {
				} else {
					$TL9 = SEEK_END;
					$TLE266 = ($TEL5 == $TL9);
					if ($TLE266) {
						$TSM6 = True;
					}
				}
				if ($TSM6) {
					$TSNNt933 = $this->numBytes;
					$TLE267 = ($TSNNt933 + $offset);
					$TSNNt934 = $TLE267;
					$this->position = $TSNNt934;
					break;
				}
				if ($TSM6) {
					$TLE268 = False;
					return $TLE268;
				}
				if ($TSM6) {
				} else {
					$TLE269 = False;
					return $TLE269;
				}
			}
			$TLE270 = True;
			return $TLE270;
		}
		public function tell()
		{
			$TSNNt935 = $this->position;
			return $TSNNt935;
		}
		public function flush()
		{
			$TSNNt936 = $this->synchronized;
			if ($TSNNt936) {
				$TLE271 = True;
				return $TLE271;
			}
			try {
				$TSNNt937 = $this->content;
				$this->writeContent($TSNNt937);
			} catch (Exception $e) {
				$TLE272 = False;
				return $TLE272;
			}
			$TLE273 = True;
			return $TLE273;
		}
		public function eof()
		{
			$TSNNt938 = $this->position;
			$TSNNt939 = $this->numBytes;
			$TLE274 = ($TSNNt939 <= $TSNNt938);
			return $TLE274;
		}
		public function stat()
		{
			$TSNNt940 = $this->filesystem;
			$TSNNt941 = $this->key;
			$TLE275 = $TSNNt940->has($TSNNt941);
			if ($TLE275) {
				$TSNNt942 = $this->filesystem;
				$TSNNt943 = $this->key;
				$time = $TSNNt942->mtime($TSNNt943);
				$TLE276 = 'dev';
				$TLE277 = 1;
				$TLE278 = 'ino';
				$TLE279 = 0;
				$TLE280 = 'mode';
				$TLE281 = 33204;
				$TLE282 = 'nlink';
				$TLE283 = 1;
				$TLE284 = 'uid';
				$TLE285 = 0;
				$TLE286 = 'gid';
				$TLE287 = 0;
				$TLE288 = 'rdev';
				$TLE289 = 0;
				$TLE290 = 'size';
				$TSNNt944 = $this->content;
				$TLE291 = Size::fromContent($TSNNt944);
				$TLE292 = 'atime';
				$TLE293 = 'mtime';
				$TLE294 = 'ctime';
				$TLE295 = 'blksize';
				$TLE296 = -1;
				$TLE297 = 'blocks';
				$TLE298 = -1;
				unset($TSa299);
				$TSa299 = (array) $TSa299;
				$TSNNi945 = $TLE277;
				$TSa299[$TLE276] = $TSNNi945;
				$TSNNi946 = $TLE279;
				$TSa299[$TLE278] = $TSNNi946;
				$TSNNi947 = $TLE281;
				$TSa299[$TLE280] = $TSNNi947;
				$TSNNi948 = $TLE283;
				$TSa299[$TLE282] = $TSNNi948;
				$TSNNi949 = $TLE285;
				$TSa299[$TLE284] = $TSNNi949;
				$TSNNi950 = $TLE287;
				$TSa299[$TLE286] = $TSNNi950;
				$TSNNi951 = $TLE289;
				$TSa299[$TLE288] = $TSNNi951;
				$TSNNi952 = $TLE291;
				$TSa299[$TLE290] = $TSNNi952;
				$TSNNi953 = $time;
				$TSa299[$TLE292] = $TSNNi953;
				$TSNNi954 = $time;
				$TSa299[$TLE293] = $TSNNi954;
				$TSNNi955 = $time;
				$TSa299[$TLE294] = $TSNNi955;
				$TSNNi956 = $TLE296;
				$TSa299[$TLE295] = $TSNNi956;
				$TSNNi957 = $TLE298;
				$TSa299[$TLE297] = $TSNNi957;
				$stats = $TSa299;
				$TLE300 = array_values($stats);
				$TLE301 = array_merge($TLE300, $stats);
				return $TLE301;
			}
			$TLE302 = False;
			return $TLE302;
		}
		public function cast($castAst)
		{
			$TLE303 = False;
			return $TLE303;
		}
		public function unlink()
		{
			$TSNNt958 = $this->mode;
			$TLE43 = $TSNNt958;
			if ($TLE43) {
				$TSNNt959 = $this->mode;
				$TEF44 = $TSNNt959->impliesExistingContentDeletion();
			} else {
				$TEF44 = $TLE43;
			}
			$TLE304 = (bool) $TEF44;
			if ($TLE304) {
				$TSNNt960 = $this->filesystem;
				$TSNNt961 = $this->key;
				$TLE305 = $TSNNt960->delete($TSNNt961);
				return $TLE305;
			}
			$TLE306 = False;
			return $TLE306;
		}
		protected function hasNewContentAtFurtherPosition()
		{
			$TLE307 = 0;
			$TSNNt962 = $this->position;
			$TLE45 = ($TLE307 < $TSNNt962);
			if ($TLE45) {
				$TSNNt963 = $this->content;
				$TEF46 = !$TSNNt963;
			} else {
				$TEF46 = $TLE45;
			}
			$TLE308 = (bool) $TEF46;
			return $TLE308;
		}
		protected function writeContent($content = '', $overwrite = True)
		{
			$TSNNt964 = $this->filesystem;
			$TSNNt965 = $this->key;
			$TSNNt964->write($TSNNt965, $content, $overwrite);
			return $content;
		}
	}
	class Local implements Stream
	{
		private $path;
		private $mode;
		private $fileHandle;
		public function __construct($path)
		{
			$TSNNt966 = $path;
			$this->path = $TSNNt966;
		}
		public function open(StreamMode $mode)
		{
			$TSie309 = 0;
			$TSie310 = error_reporting($TSie309);
			$TLE311 = $mode->getMode();
			$TSNNt967 = $this->path;
			$TLE312 = fopen($TSNNt967, $TLE311);
			$TSie313 = error_reporting($TSie310);
			$fileHandle = $TLE312;
			$TLE314 = False;
			$TLE315 = ($TLE314 === $fileHandle);
			if ($TLE315) {
				$TLE316 = 'File "%s" cannot be opened';
				$TSNNt968 = $this->path;
				$TLE317 = sprintf($TLE316, $TSNNt968);
				$TLE318 = new RuntimeException($TLE317);
				throw $TLE318;
			}
			$TSNNt969 = $mode;
			$this->mode = $TSNNt969;
			$TSNNt970 = $fileHandle;
			$this->fileHandle = $TSNNt970;
			$TLE319 = True;
			return $TLE319;
		}
		public function read($count)
		{
			$TSNNt971 = $this->fileHandle;
			$TLE320 = !$TSNNt971;
			if ($TLE320) {
				$TLE321 = False;
				return $TLE321;
			}
			$TLE322 = False;
			$TSNNt972 = $this->mode;
			$TLE323 = $TSNNt972->allowsRead();
			$TLE324 = ($TLE322 === $TLE323);
			if ($TLE324) {
				$TLE325 = 'The stream does not allow read.';
				$TLE326 = new LogicException($TLE325);
				throw $TLE326;
			}
			$TSNNt973 = $this->fileHandle;
			$TLE327 = fread($TSNNt973, $count);
			return $TLE327;
		}
		public function write($data)
		{
			$TSNNt974 = $this->fileHandle;
			$TLE328 = !$TSNNt974;
			if ($TLE328) {
				$TLE329 = False;
				return $TLE329;
			}
			$TLE330 = False;
			$TSNNt975 = $this->mode;
			$TLE331 = $TSNNt975->allowsWrite();
			$TLE332 = ($TLE330 === $TLE331);
			if ($TLE332) {
				$TLE333 = 'The stream does not allow write.';
				$TLE334 = new LogicException($TLE333);
				throw $TLE334;
			}
			$TSNNt976 = $this->fileHandle;
			$TLE335 = fwrite($TSNNt976, $data);
			return $TLE335;
		}
		public function close()
		{
			$TSNNt977 = $this->fileHandle;
			$TLE336 = !$TSNNt977;
			if ($TLE336) {
				$TLE337 = False;
				return $TLE337;
			}
			$TSNNt978 = $this->fileHandle;
			$closed = fclose($TSNNt978);
			if ($closed) {
				$TLE338 = NULL;
				$TSNNt979 = $TLE338;
				$this->mode = $TSNNt979;
				$TLE339 = NULL;
				$TSNNt980 = $TLE339;
				$this->fileHandle = $TSNNt980;
			}
			return $closed;
		}
		public function flush()
		{
			$TSNNt981 = $this->fileHandle;
			if ($TSNNt981) {
				$TSNNt982 = $this->fileHandle;
				$TLE340 = fflush($TSNNt982);
				return $TLE340;
			}
			$TLE341 = False;
			return $TLE341;
		}
		public function seek($offset, $whence = SEEK_SET)
		{
			$TSNNt983 = $this->fileHandle;
			if ($TSNNt983) {
				$TLE342 = 0;
				$TSNNt984 = $this->fileHandle;
				$TLE343 = fseek($TSNNt984, $offset, $whence);
				$TLE344 = ($TLE342 === $TLE343);
				return $TLE344;
			}
			$TLE345 = False;
			return $TLE345;
		}
		public function tell()
		{
			$TSNNt985 = $this->fileHandle;
			if ($TSNNt985) {
				$TSNNt986 = $this->fileHandle;
				$TLE346 = ftell($TSNNt986);
				return $TLE346;
			}
			$TLE347 = False;
			return $TLE347;
		}
		public function eof()
		{
			$TSNNt987 = $this->fileHandle;
			if ($TSNNt987) {
				$TSNNt988 = $this->fileHandle;
				$TLE348 = feof($TSNNt988);
				return $TLE348;
			}
			$TLE349 = True;
			return $TLE349;
		}
		public function stat()
		{
			$TSNNt989 = $this->fileHandle;
			if ($TSNNt989) {
				$TSNNt990 = $this->fileHandle;
				$TLE350 = fstat($TSNNt990);
				return $TLE350;
			}
			$TLE351 = False;
			return $TLE351;
		}
		public function cast($castAs)
		{
			$TSNNt991 = $this->fileHandle;
			if ($TSNNt991) {
				$TSNNt992 = $this->fileHandle;
				return $TSNNt992;
			}
			$TLE352 = False;
			return $TLE352;
		}
		public function unlink()
		{
			$TSNNt993 = $this->mode;
			$TLE47 = $TSNNt993;
			if ($TLE47) {
				$TSNNt994 = $this->mode;
				$TEF48 = $TSNNt994->impliesExistingContentDeletion();
			} else {
				$TEF48 = $TLE47;
			}
			$TLE353 = (bool) $TEF48;
			if ($TLE353) {
				$TSie354 = 0;
				$TSie355 = error_reporting($TSie354);
				$TSNNt995 = $this->path;
				$TLE356 = unlink($TSNNt995);
				$TSie357 = error_reporting($TSie355);
				return $TLE356;
			}
			$TLE358 = False;
			return $TLE358;
		}
	}
	class InMemoryAdapter implements Adapter
	{
		protected $files = array();
		public function __construct(array $files = array())
		{
			$this->setFiles($files);
		}
		public function setFiles(array $files)
		{
			unset($TSa359);
			$TSa359 = (array) $TSa359;
			$TSNNt996 = $TSa359;
			$this->files = $TSNNt996;
			foreach ($files as $key => $file) {
				$TLE360 = is_array($file);
				$TLE361 = !$TLE360;
				if ($TLE361) {
					$TLE362 = 'content';
					unset($TSa363);
					$TSa363 = (array) $TSa363;
					$TSNNi997 = $file;
					$TSa363[$TLE362] = $TSNNi997;
					$file = $TSa363;
				}
				$TLE364 = 'content';
				$TLE365 = NULL;
				$TLE366 = 'mtime';
				$TLE367 = NULL;
				unset($TSa368);
				$TSa368 = (array) $TSa368;
				$TSNNi998 = $TLE365;
				$TSa368[$TLE364] = $TSNNi998;
				$TSNNi999 = $TLE367;
				$TSa368[$TLE366] = $TSNNi999;
				$file = array_merge($TSa368, $file);
				$TLE369 = 'content';
				$TLE370 = 'mtime';
				$TSNNi1000 = $file[$TLE369];
				$TSNNi1001 = $file[$TLE370];
				$this->setFile($key, $TSNNi1000, $TSNNi1001);
			}
		}
		public function setFile($key, $content = NULL, $mtime = NULL)
		{
			$TLE371 = NULL;
			$TLE372 = ($TLE371 === $mtime);
			if ($TLE372) {
				$mtime = time();
			}
			$TLE373 = 'content';
			$TLE374 = (string) $content;
			$TLE375 = 'mtime';
			$TLE376 = (int) $mtime;
			unset($TSa377);
			$TSa377 = (array) $TSa377;
			$TSNNi1002 = $TLE374;
			$TSa377[$TLE373] = $TSNNi1002;
			$TSNNi1003 = $TLE376;
			$TSa377[$TLE375] = $TSNNi1003;
			$TSNNt1004 = $this->files;
			$TSNNt1004[$key] = $TSa377;
			$this->files = $TSNNt1004;
		}
		public function read($key)
		{
			$TLE378 = 'content';
			$TSNNt1005 = $this->files;
			$TSNNi1006 = $TSNNt1005[$key];
			$TSNNi1007 = $TSNNi1006[$TLE378];
			return $TSNNi1007;
		}
		public function rename($sourceKey, $targetKey)
		{
			$content = $this->read($sourceKey);
			$this->delete($sourceKey);
			$TLE379 = $this->write($targetKey, $content);
			$TLE380 = (bool) $TLE379;
			return $TLE380;
		}
		public function write($key, $content, array $metadata = NULL)
		{
			$TLE381 = 'content';
			$TSNNt1008 = $this->files;
			$TSNNi1009 = $TSNNt1008[$key];
			$TSNNi1009[$TLE381] = $content;
			$TSNNt1008[$key] = $TSNNi1009;
			$this->files = $TSNNt1008;
			$TLE382 = 'mtime';
			$TLE383 = time();
			$TSNNt1010 = $this->files;
			$TSNNi1011 = $TSNNt1010[$key];
			$TSNNi1011[$TLE382] = $TLE383;
			$TSNNt1010[$key] = $TSNNi1011;
			$this->files = $TSNNt1010;
			$TLE384 = Size::fromContent($content);
			return $TLE384;
		}
		public function exists($key)
		{
			$TSNNt1012 = $this->files;
			$TLE385 = array_key_exists($key, $TSNNt1012);
			return $TLE385;
		}
		public function keys()
		{
			$TSNNt1013 = $this->files;
			$TLE386 = array_keys($TSNNt1013);
			return $TLE386;
		}
		public function mtime($key)
		{
			$TLE387 = 'mtime';
			$TSNNt1014 = $this->files;
			$TSNNi1015 = $TSNNt1014[$key];
			$TSNNi1016 = $TSNNi1015[$TLE387];
			$TLE388 = isset($TSNNi1016);
			$TSNNi1015[$TLE387] = $TSNNi1016;
			$TSNNt1014[$key] = $TSNNi1015;
			$this->files = $TSNNt1014;
			if ($TLE388) {
				$TLE389 = 'mtime';
				$TSNNt1017 = $this->files;
				$TSNNi1018 = $TSNNt1017[$key];
				$TSNNi1019 = $TSNNi1018[$TLE389];
				$TEF49 = $TSNNi1019;
				$TSNNi1018[$TLE389] = $TSNNi1019;
				$TSNNt1017[$key] = $TSNNi1018;
				$this->files = $TSNNt1017;
			} else {
				$TEF49 = False;
			}
			return $TEF49;
		}
		public function delete($key)
		{
			$TSNNt1020 = $this->files;
			$TSNNi1021 = $TSNNt1020[$key];
			unset($TSNNi1021);
			$TSNNt1020[$key] = $TSNNi1021;
			$this->files = $TSNNt1020;
			clearstatcache();
			$TLE390 = True;
			return $TLE390;
		}
		public function isDirectory($path)
		{
			$TLE391 = False;
			return $TLE391;
		}
	}
	class LocalAdapter implements Adapter, StreamFactory, ChecksumCalculator
	{
		protected $directory;
		private $create;
		public function __construct($directory, $create = False)
		{
			$TLE392 = Path::normalize($directory);
			$TSNNt1022 = $TLE392;
			$this->directory = $TSNNt1022;
			$TSNNt1023 = $this->directory;
			$TLE393 = is_link($TSNNt1023);
			if ($TLE393) {
				$TSNNt1024 = $this->directory;
				$TLE394 = realpath($TSNNt1024);
				$TSNNt1025 = $TLE394;
				$this->directory = $TSNNt1025;
			}
			$TSNNt1026 = $create;
			$this->create = $TSNNt1026;
		}
		public function read($key)
		{
			$TLE395 = $this->computePath($key);
			$TLE396 = file_get_contents($TLE395);
			return $TLE396;
		}
		public function write($key, $content)
		{
			$path = $this->computePath($key);
			$TLE397 = dirname($path);
			$TLE398 = True;
			$this->ensureDirectoryExists($TLE397, $TLE398);
			$TLE399 = file_put_contents($path, $content);
			return $TLE399;
		}
		public function rename($sourceKey, $targetKey)
		{
			$targetPath = $this->computePath($targetKey);
			$TLE400 = dirname($targetPath);
			$TLE401 = True;
			$this->ensureDirectoryExists($TLE400, $TLE401);
			$TLE402 = $this->computePath($sourceKey);
			$TLE403 = rename($TLE402, $targetPath);
			return $TLE403;
		}
		public function exists($key)
		{
			$TLE404 = $this->computePath($key);
			$TLE405 = file_exists($TLE404);
			return $TLE405;
		}
		public function keys()
		{
			$TSNNt1027 = $this->directory;
			$TSNNt1028 = $this->create;
			$this->ensureDirectoryExists($TSNNt1027, $TSNNt1028);
			try {
				$TLE406 = FilesystemIterator::SKIP_DOTS;
				$TLE407 = FilesystemIterator::UNIX_PATHS;
				$TLE408 = ($TLE406 | $TLE407);
				$TSNNt1029 = $this->directory;
				$TLE409 = new RecursiveDirectoryIterator($TSNNt1029, $TLE408);
				$iterator = new RecursiveIteratorIterator($TLE409);
			} catch (GaufretteException $e) {
				$iterator = new EmptyIterator();
			}
			$files = iterator_to_array($iterator);
			unset($TSa410);
			$TSa410 = (array) $TSa410;
			$keys = $TSa410;
			foreach ($files as $file) {
				$key = $this->computeKey($file);
				$TLE411 = $key;
				$keys[] = $TLE411;
				$TLE412 = '.';
				$TLE413 = dirname($key);
				$TLE414 = ($TLE412 !== $TLE413);
				if ($TLE414) {
					$TLE415 = dirname($key);
					$keys[] = $TLE415;
				}
			}
			sort($keys);
			return $keys;
		}
		public function mtime($key)
		{
			$TLE416 = $this->computePath($key);
			$TLE417 = filemtime($TLE416);
			return $TLE417;
		}
		public function delete($key)
		{
			$TLE418 = $this->isDirectory($key);
			if ($TLE418) {
				$TLE419 = $this->computePath($key);
				$TLE420 = rmdir($TLE419);
				return $TLE420;
			}
			$TLE421 = $this->computePath($key);
			$TLE422 = unlink($TLE421);
			return $TLE422;
		}
		public function isDirectory($key)
		{
			$TLE423 = $this->computePath($key);
			$TLE424 = is_dir($TLE423);
			return $TLE424;
		}
		public function createStream($key)
		{
			$TLE425 = $this->computePath($key);
			$TLE426 = new Local($TLE425);
			return $TLE426;
		}
		public function checksum($key)
		{
			$TLE427 = $this->computePath($key);
			$TLE428 = Checksum::fromFile($TLE427);
			return $TLE428;
		}
		public function computeKey($path)
		{
			$path = $this->normalizePath($path);
			$TSNNt1030 = $this->directory;
			$TLE429 = strlen($TSNNt1030);
			$TLE430 = substr($path, $TLE429);
			$TLE431 = '/';
			$TLE432 = ltrim($TLE430, $TLE431);
			return $TLE432;
		}
		protected function computePath($key)
		{
			$TSNNt1031 = $this->directory;
			$TSNNt1032 = $this->create;
			$this->ensureDirectoryExists($TSNNt1031, $TSNNt1032);
			$TLE433 = '/';
			$TSNNt1033 = $this->directory;
			$TLE434 = ($TSNNt1033 . $TLE433);
			$TLE435 = ($TLE434 . $key);
			$TLE436 = $this->normalizePath($TLE435);
			return $TLE436;
		}
		protected function normalizePath($path)
		{
			$path = Path::normalize($path);
			$TLE437 = 0;
			$TSNNt1034 = $this->directory;
			$TLE438 = strpos($path, $TSNNt1034);
			$TLE439 = ($TLE437 !== $TLE438);
			if ($TLE439) {
				$TLE440 = 'The path "%s" is out of the filesystem.';
				$TLE441 = sprintf($TLE440, $path);
				$TLE442 = new OutOfBoundsException($TLE441);
				throw $TLE442;
			}
			return $path;
		}
		protected function ensureDirectoryExists($directory, $create = False)
		{
			$TLE443 = is_dir($directory);
			$TLE444 = !$TLE443;
			if ($TLE444) {
				$TLE445 = !$create;
				if ($TLE445) {
					$TLE446 = 'The directory "%s" does not exist.';
					$TLE447 = sprintf($TLE446, $directory);
					$TLE448 = new RuntimeException($TLE447);
					throw $TLE448;
				}
				$this->createDirectory($directory);
			}
		}
		protected function createDirectory($directory)
		{
			$TLE449 = 0;
			$umask = umask($TLE449);
			$TLE450 = 511;
			$TLE451 = True;
			$created = mkdir($directory, $TLE450, $TLE451);
			umask($umask);
			$TLE452 = !$created;
			if ($TLE452) {
				$TLE453 = 'The directory \'%s\' could not be created.';
				$TLE454 = sprintf($TLE453, $directory);
				$TLE455 = new RuntimeException($TLE454);
				throw $TLE455;
			}
		}
	}
	class ZipAdapter implements Adapter
	{
		protected $zipFile;
		protected $zipArchive;
		public function __construct($zipFile)
		{
			$TLE456 = 'zip';
			$TLE457 = extension_loaded($TLE456);
			$TLE458 = !$TLE457;
			if ($TLE458) {
				$TLE459 = 'Unable to use %s as the ZIP extension is not available.';
				$TLE460 = __CLASS__;
				$TLE461 = sprintf($TLE459, $TLE460);
				$TLE462 = new RuntimeException($TLE461);
				throw $TLE462;
			}
			$TSNNt1035 = $zipFile;
			$this->zipFile = $TSNNt1035;
			$this->reinitZipArchive();
		}
		public function read($key)
		{
			$TLE463 = False;
			$TLE464 = 0;
			$TSNNt1036 = $this->zipArchive;
			$content = $TSNNt1036->getFromName($key, $TLE464);
			$TLE465 = $content;
			$TLE466 = ($TLE463 === $TLE465);
			if ($TLE466) {
				$TLE467 = False;
				return $TLE467;
			}
			return $content;
		}
		public function write($key, $content)
		{
			$TSNNt1037 = $this->zipArchive;
			$TLE468 = $TSNNt1037->addFromString($key, $content);
			$TLE469 = !$TLE468;
			if ($TLE469) {
				$TLE470 = False;
				return $TLE470;
			}
			$TLE471 = $this->save();
			$TLE472 = !$TLE471;
			if ($TLE472) {
				$TLE473 = False;
				return $TLE473;
			}
			$TLE474 = Size::fromContent($content);
			return $TLE474;
		}
		public function exists($key)
		{
			$TLE475 = $this->getStat($key);
			$TLE476 = (bool) $TLE475;
			return $TLE476;
		}
		public function keys()
		{
			unset($TSa477);
			$TSa477 = (array) $TSa477;
			$keys = $TSa477;
			$i = 0;
			$ElcfPF11 = True;
			while (True) {
				if ($ElcfPF11) {
					$ElcfPF11 = False;
				} else {
					++$i;
				}
				$TSNNt1038 = $this->zipArchive;
				$TSNNt1039 = $TSNNt1038->numFiles;
				$TLE478 = ($i < $TSNNt1039);
				$this->zipArchive = $TSNNt1038;
				if ($TLE478) {
				} else {
					break;
				}
				$TSNNt1040 = $this->zipArchive;
				$TLE479 = $TSNNt1040->getNameIndex($i);
				$keys[$i] = $TLE479;
			}
			return $keys;
		}
		public function isDirectory($key)
		{
			$TLE480 = False;
			return $TLE480;
		}
		public function mtime($key)
		{
			$stat = $this->getStat($key);
			$TLE481 = 'mtime';
			$TSNNi1041 = $stat[$TLE481];
			$TLE482 = isset($TSNNi1041);
			if ($TLE482) {
				$TLE483 = 'mtime';
				$TSNNi1042 = $stat[$TLE483];
				$TEF50 = $TSNNi1042;
			} else {
				$TEF50 = False;
			}
			return $TEF50;
		}
		public function delete($key)
		{
			$TSNNt1043 = $this->zipArchive;
			$TLE484 = $TSNNt1043->deleteName($key);
			$TLE485 = !$TLE484;
			if ($TLE485) {
				$TLE486 = False;
				return $TLE486;
			}
			$TLE487 = $this->save();
			return $TLE487;
		}
		public function rename($sourceKey, $targetKey)
		{
			$TSNNt1044 = $this->zipArchive;
			$TLE488 = $TSNNt1044->renameName($sourceKey, $targetKey);
			$TLE489 = !$TLE488;
			if ($TLE489) {
				$TLE490 = False;
				return $TLE490;
			}
			$TLE491 = $this->save();
			return $TLE491;
		}
		public function getStat($key)
		{
			$TSNNt1045 = $this->zipArchive;
			$stat = $TSNNt1045->statName($key);
			$TLE492 = False;
			$TLE493 = ($TLE492 === $stat);
			if ($TLE493) {
				unset($TSa494);
				$TSa494 = (array) $TSa494;
				return $TSa494;
			}
			return $stat;
		}
		protected function reinitZipArchive()
		{
			$TLE495 = new ZipArchive();
			$TSNNt1046 = $TLE495;
			$this->zipArchive = $TSNNt1046;
			$TLE496 = True;
			$TLE497 = ZipArchive::CREATE;
			$TSNNt1047 = $this->zipArchive;
			$TSNNt1048 = $this->zipFile;
			$resultCode = $TSNNt1047->open($TSNNt1048, $TLE497);
			$TLE498 = $resultCode;
			$TLE499 = ($TLE496 !== $TLE498);
			if ($TLE499) {
				$TEL12 = $resultCode;
				$TSM13 = False;
				$ElcfPD23 = True;
				while (True) {
					if ($ElcfPD23) {
						$ElcfPD23 = False;
					} else {
						$TLE500 = False;
						$TLE501 = !$TLE500;
						if ($TLE501) {
							break;
						}
					}
					if ($TSM13) {
					} else {
						$TL14 = ZipArchive::ER_EXISTS;
						$TLE502 = ($TEL12 == $TL14);
						if ($TLE502) {
							$TSM13 = True;
						}
					}
					if ($TSM13) {
						$errMsg = 'File already exists.';
						break;
					}
					if ($TSM13) {
					} else {
						$TL15 = ZipArchive::ER_INCONS;
						$TLE503 = ($TEL12 == $TL15);
						if ($TLE503) {
							$TSM13 = True;
						}
					}
					if ($TSM13) {
						$errMsg = 'Zip archive inconsistent.';
						break;
					}
					if ($TSM13) {
					} else {
						$TL16 = ZipArchive::ER_INVAL;
						$TLE504 = ($TEL12 == $TL16);
						if ($TLE504) {
							$TSM13 = True;
						}
					}
					if ($TSM13) {
						$errMsg = 'Invalid argument.';
						break;
					}
					if ($TSM13) {
					} else {
						$TL17 = ZipArchive::ER_MEMORY;
						$TLE505 = ($TEL12 == $TL17);
						if ($TLE505) {
							$TSM13 = True;
						}
					}
					if ($TSM13) {
						$errMsg = 'Malloc failure.';
						break;
					}
					if ($TSM13) {
					} else {
						$TL18 = ZipArchive::ER_NOENT;
						$TLE506 = ($TEL12 == $TL18);
						if ($TLE506) {
							$TSM13 = True;
						}
					}
					if ($TSM13) {
						$errMsg = 'Invalid argument.';
						break;
					}
					if ($TSM13) {
					} else {
						$TL19 = ZipArchive::ER_NOZIP;
						$TLE507 = ($TEL12 == $TL19);
						if ($TLE507) {
							$TSM13 = True;
						}
					}
					if ($TSM13) {
						$errMsg = 'Not a zip archive.';
						break;
					}
					if ($TSM13) {
					} else {
						$TL20 = ZipArchive::ER_OPEN;
						$TLE508 = ($TEL12 == $TL20);
						if ($TLE508) {
							$TSM13 = True;
						}
					}
					if ($TSM13) {
						$errMsg = 'Can\'t open file.';
						break;
					}
					if ($TSM13) {
					} else {
						$TL21 = ZipArchive::ER_READ;
						$TLE509 = ($TEL12 == $TL21);
						if ($TLE509) {
							$TSM13 = True;
						}
					}
					if ($TSM13) {
						$errMsg = 'Read error.';
						break;
					}
					if ($TSM13) {
					} else {
						$TL22 = ZipArchive::ER_SEEK;
						$TLE510 = ($TEL12 == $TL22);
						if ($TLE510) {
							$TSM13 = True;
						}
					}
					if ($TSM13) {
						$errMsg = 'Seek error.';
						break;
					}
					if ($TSM13) {
						$errMsg = 'Unknown error.';
						break;
					}
					if ($TSM13) {
					} else {
						$errMsg = 'Unknown error.';
						break;
					}
				}
				$TLE511 = '%s';
				$TLE512 = sprintf($TLE511, $errMsg);
				$TLE513 = new RuntimeException($TLE512);
				throw $TLE513;
			}
			return $this;
		}
		protected function save()
		{
			$TSNNt1049 = $this->zipArchive;
			$TLE514 = $TSNNt1049->close();
			$TLE515 = !$TLE514;
			if ($TLE515) {
				$TLE516 = False;
				return $TLE516;
			}
			$this->reinitZipArchive();
			$TLE517 = True;
			return $TLE517;
		}
	}
	abstract class TestCase
	{
		public function setup()
		{
		}
		public function tearDown()
		{
		}
		public function assertTrue($mFound)
		{
			$TLE518 = True;
			$TLE519 = $this->assertEquals($TLE518, $mFound);
			return $TLE519;
		}
		public function assertFalse($mFound)
		{
			$TLE520 = False;
			$TLE521 = $this->assertEquals($TLE520, $mFound);
			return $TLE521;
		}
		public function assertJsonStringEqualsJsonString($mExpected, $mFound)
		{
			$TLE522 = json_decode($mExpected);
			$TLE523 = json_decode($mFound);
			$TLE524 = $this->assertEquals($TLE522, $TLE523);
			return $TLE524;
		}
		public function assertEquals($mExpected, $mFound)
		{
			$TLE525 = ($mExpected != $mFound);
			if ($TLE525) {
				$this->displayFailure($mExpected, $mFound);
			} else {
				$this->displaySuccess();
			}
		}
		public function assertSame($mExpected, $mFound)
		{
			$TLE526 = ($mExpected !== $mFound);
			if ($TLE526) {
				$this->displayFailure($mExpected, $mFound);
			} else {
				$this->displaySuccess();
			}
		}
		public function assertGreaterThan($mExpected, $mFound)
		{
			$TLE527 = ($mFound <= $mExpected);
			if ($TLE527) {
				$this->displayFailure($mExpected, $mFound);
			} else {
				$this->displaySuccess();
			}
		}
		public function assertEmpty($mMixed)
		{
			$TLE528 = empty($mMixed);
			$TLE529 = !$TLE528;
			if ($TLE529) {
				$TLE530 = "[EMPTY]";
				$this->displayFailure($TLE530, $mMixed);
			} else {
				$this->displaySuccess();
			}
		}
		public function assertContains($mNeedle, $mHaystack)
		{
			$TLE531 = in_array($mNeedle, $mHaystack);
			$TLE532 = !$TLE531;
			if ($TLE532) {
				$this->displayFailure($mNeedle, $mHaystack);
			} else {
				$this->displaySuccess();
			}
		}
		public function assertCount($expectedCount, $other)
		{
			$TLE533 = $this->getCountOf($other);
			$TLE534 = ($TLE533 !== $expectedCount);
			if ($TLE534) {
				$TLE535 = $this->getCountOf($other);
				$this->displayFailure($TLE535, $expectedCount);
			} else {
				$this->displaySuccess();
			}
		}
		public function assertFileExists($file)
		{
			$TLE536 = is_file($file);
			$this->assertTrue($TLE536);
		}
		protected function displayFailure($mExpected, $mFound)
		{
			$TLE537 = "FAIL\n\tExpected: ";
			print($TLE537);
			var_dump($mExpected);
			$TLE538 = "\tFound: ";
			print($TLE538);
			var_dump($mFound);
		}
		protected function displaySuccess()
		{
			$TLE539 = "PASS\n";
			print($TLE539);
		}
		protected function getCountOf($other)
		{
			$TLE51 = $other instanceof Countable;
			if ($TLE51) {
				$TEF52 = $TLE51;
			} else {
				$TEF52 = is_array($other);
			}
			$TLE540 = (bool) $TEF52;
			if ($TLE540) {
				$TLE541 = count($other);
				return $TLE541;
			} else {
				$TLE542 = $other instanceof Iterator;
				if ($TLE542) {
					$TLE543 = iterator_count($other);
					return $TLE543;
				}
			}
		}
	}
	abstract class FunctionalTestCase extends TestCase
	{
		protected $filesystem;
		public function getAdapterName()
		{
			$TLE544 = '/\\\\(\w+)Test$/';
			$TLE545 = get_class($this);
			$TLE546 = preg_match($TLE544, $TLE545, $matches);
			$TLE547 = !$TLE546;
			if ($TLE547) {
				$TLE548 = 'Unable to guess filesystem name from class "%s", ';
				$TLE549 = 'please override the ->getAdapterName() method.';
				$TLE550 = ($TLE548 . $TLE549);
				$TLE551 = get_class($this);
				$TLE552 = sprintf($TLE550, $TLE551);
				$TLE553 = new RuntimeException($TLE552);
				throw $TLE553;
			}
			$TLE554 = 1;
			$TSNNi1050 = $matches[$TLE554];
			return $TSNNi1050;
		}
		public function setUp()
		{
			$basename = $this->getAdapterName();
			$TLE555 = '%s/adapters/%s.php';
			$TLE556 = __DIR__;
			$TLE557 = dirname($TLE556);
			$filename = sprintf($TLE555, $TLE557, $basename);
			$TLE558 = file_exists($filename);
			$TLE559 = !$TLE558;
			if ($TLE559) {
				$TLE560 = 'To run the ';
				$TLE561 = ($TLE560 . $basename);
				$TLE562 = ' filesystem tests, you must:

 1. Copy the file "';
				$TLE563 = ($TLE561 . $TLE562);
				$TLE564 = ($TLE563 . $filename);
				$TLE565 = '.dist" as "';
				$TLE566 = ($TLE564 . $TLE565);
				$TLE567 = ($TLE566 . $filename);
				$TLE568 = '"
 2. Modify the copied file to fit your environment';
				$TLE569 = ($TLE567 . $TLE568);
				$TLE570 = $this->markTestSkipped($TLE569);
				return $TLE570;
			}
			$adapter = include($filename);
			$TLE571 = new Filesystem($adapter);
			$TSNNt1051 = $TLE571;
			$this->filesystem = $TSNNt1051;
		}
		public function tearDown()
		{
			$TLE572 = NULL;
			$TSNNt1052 = $this->filesystem;
			$TLE573 = ($TLE572 === $TSNNt1052);
			if ($TLE573) {
				$TLE574 = NULL;
				return $TLE574;
			}
			$TLE575 = NULL;
			$TSNNt1053 = $TLE575;
			$this->filesystem = $TSNNt1053;
		}
		public function shouldWriteAndRead()
		{
			$TLE576 = 12;
			$TLE577 = 'foo';
			$TLE578 = 'Some content';
			$TSNNt1054 = $this->filesystem;
			$TLE579 = $TSNNt1054->write($TLE577, $TLE578);
			$this->assertEquals($TLE576, $TLE579);
			$TLE580 = 13;
			$TLE581 = 'test/subdir/foo';
			$TLE582 = 'Some content1';
			$TLE583 = True;
			$TSNNt1055 = $this->filesystem;
			$TLE584 = $TSNNt1055->write($TLE581, $TLE582, $TLE583);
			$this->assertEquals($TLE580, $TLE584);
			$TLE585 = 'Some content';
			$TLE586 = 'foo';
			$TSNNt1056 = $this->filesystem;
			$TLE587 = $TSNNt1056->read($TLE586);
			$this->assertEquals($TLE585, $TLE587);
			$TLE588 = 'Some content1';
			$TLE589 = 'test/subdir/foo';
			$TSNNt1057 = $this->filesystem;
			$TLE590 = $TSNNt1057->read($TLE589);
			$this->assertEquals($TLE588, $TLE590);
			$TLE591 = 'foo';
			$TSNNt1058 = $this->filesystem;
			$TSNNt1058->delete($TLE591);
			$TLE592 = 'test/subdir/foo';
			$TSNNt1059 = $this->filesystem;
			$TSNNt1059->delete($TLE592);
		}
		public function shouldUpdateFileContent()
		{
			$TLE593 = 'foo';
			$TLE594 = 'Some content';
			$TSNNt1060 = $this->filesystem;
			$TSNNt1060->write($TLE593, $TLE594);
			$TLE595 = 'foo';
			$TLE596 = 'Some content updated';
			$TLE597 = True;
			$TSNNt1061 = $this->filesystem;
			$TSNNt1061->write($TLE595, $TLE596, $TLE597);
			$TLE598 = 'Some content updated';
			$TLE599 = 'foo';
			$TSNNt1062 = $this->filesystem;
			$TLE600 = $TSNNt1062->read($TLE599);
			$this->assertEquals($TLE598, $TLE600);
			$TLE601 = 'foo';
			$TSNNt1063 = $this->filesystem;
			$TSNNt1063->delete($TLE601);
		}
		public function shouldCheckIfFileExists()
		{
			$TLE602 = 'foo';
			$TSNNt1064 = $this->filesystem;
			$TLE603 = $TSNNt1064->has($TLE602);
			$this->assertFalse($TLE603);
			$TLE604 = 'foo';
			$TLE605 = 'Some content';
			$TSNNt1065 = $this->filesystem;
			$TSNNt1065->write($TLE604, $TLE605);
			$TLE606 = 'foo';
			$TSNNt1066 = $this->filesystem;
			$TLE607 = $TSNNt1066->has($TLE606);
			$this->assertTrue($TLE607);
			$TLE608 = 'test/somefile';
			$TSNNt1067 = $this->filesystem;
			$TLE609 = $TSNNt1067->has($TLE608);
			$this->assertFalse($TLE609);
			$TLE610 = 'test/somefile';
			$TSNNt1068 = $this->filesystem;
			$TLE611 = $TSNNt1068->has($TLE610);
			$this->assertFalse($TLE611);
			$TLE612 = 'foo';
			$TSNNt1069 = $this->filesystem;
			$TSNNt1069->delete($TLE612);
		}
		public function shouldGetMtime()
		{
			$TLE613 = 'foo';
			$TLE614 = 'Some content';
			$TSNNt1070 = $this->filesystem;
			$TSNNt1070->write($TLE613, $TLE614);
			$TLE615 = 0;
			$TLE616 = 'foo';
			$TSNNt1071 = $this->filesystem;
			$TLE617 = $TSNNt1071->mtime($TLE616);
			$this->assertGreaterThan($TLE615, $TLE617);
			$TLE618 = 'foo';
			$TSNNt1072 = $this->filesystem;
			$TSNNt1072->delete($TLE618);
		}
		public function shouldFailWhenTryMtimeForKeyWhichDoesNotExist()
		{
			$TLE619 = 'foo';
			$TSNNt1073 = $this->filesystem;
			$TLE620 = $TSNNt1073->mtime($TLE619);
			$this->assertFalse($TLE620);
		}
		public function shouldRenameFile()
		{
			$TLE621 = 'foo';
			$TLE622 = 'Some content';
			$TSNNt1074 = $this->filesystem;
			$TSNNt1074->write($TLE621, $TLE622);
			$TLE623 = 'foo';
			$TLE624 = 'boo';
			$TSNNt1075 = $this->filesystem;
			$TSNNt1075->rename($TLE623, $TLE624);
			$TLE625 = 'foo';
			$TSNNt1076 = $this->filesystem;
			$TLE626 = $TSNNt1076->has($TLE625);
			$this->assertFalse($TLE626);
			$TLE627 = 'Some content';
			$TLE628 = 'boo';
			$TSNNt1077 = $this->filesystem;
			$TLE629 = $TSNNt1077->read($TLE628);
			$this->assertEquals($TLE627, $TLE629);
			$TLE630 = 'boo';
			$TSNNt1078 = $this->filesystem;
			$TSNNt1078->delete($TLE630);
			$TLE631 = 'foo';
			$TLE632 = 'Some content';
			$TSNNt1079 = $this->filesystem;
			$TSNNt1079->write($TLE631, $TLE632);
			$TLE633 = 'foo';
			$TLE634 = 'somedir/sub/boo';
			$TSNNt1080 = $this->filesystem;
			$TSNNt1080->rename($TLE633, $TLE634);
			$TLE635 = 'somedir/sub/foo';
			$TSNNt1081 = $this->filesystem;
			$TLE636 = $TSNNt1081->has($TLE635);
			$this->assertFalse($TLE636);
			$TLE637 = 'Some content';
			$TLE638 = 'somedir/sub/boo';
			$TSNNt1082 = $this->filesystem;
			$TLE639 = $TSNNt1082->read($TLE638);
			$this->assertEquals($TLE637, $TLE639);
			$TLE640 = 'somedir/sub/boo';
			$TSNNt1083 = $this->filesystem;
			$TSNNt1083->delete($TLE640);
		}
		public function shouldDeleteFile()
		{
			$TLE641 = 'foo';
			$TLE642 = 'Some content';
			$TSNNt1084 = $this->filesystem;
			$TSNNt1084->write($TLE641, $TLE642);
			$TLE643 = 'foo';
			$TSNNt1085 = $this->filesystem;
			$TLE644 = $TSNNt1085->has($TLE643);
			$this->assertTrue($TLE644);
			$TLE645 = 'foo';
			$TSNNt1086 = $this->filesystem;
			$TSNNt1086->delete($TLE645);
			$TLE646 = 'foo';
			$TSNNt1087 = $this->filesystem;
			$TLE647 = $TSNNt1087->has($TLE646);
			$this->assertFalse($TLE647);
		}
		public function shouldFetchKeys()
		{
			unset($TSa648);
			$TSa648 = (array) $TSa648;
			$TSNNt1088 = $this->filesystem;
			$TLE649 = $TSNNt1088->keys();
			$this->assertEquals($TSa648, $TLE649);
			$TLE650 = 'foo';
			$TLE651 = 'Some content';
			$TSNNt1089 = $this->filesystem;
			$TSNNt1089->write($TLE650, $TLE651);
			$TLE652 = 'bar';
			$TLE653 = 'Some content';
			$TSNNt1090 = $this->filesystem;
			$TSNNt1090->write($TLE652, $TLE653);
			$TLE654 = 'baz';
			$TLE655 = 'Some content';
			$TSNNt1091 = $this->filesystem;
			$TSNNt1091->write($TLE654, $TLE655);
			$TSNNt1092 = $this->filesystem;
			$actualKeys = $TSNNt1092->keys();
			$TLE656 = 3;
			$TLE657 = count($actualKeys);
			$this->assertEquals($TLE656, $TLE657);
			$TLE658 = 'foo';
			$TLE659 = 'bar';
			$TLE660 = 'baz';
			unset($TSa661);
			$TSa661 = (array) $TSa661;
			$TLE802 = 0;
			$TSNNi1093 = $TLE658;
			$TSa661[$TLE802] = $TSNNi1093;
			$TLE803 = 1;
			$TSNNi1094 = $TLE659;
			$TSa661[$TLE803] = $TSNNi1094;
			$TLE804 = 2;
			$TSNNi1095 = $TLE660;
			$TSa661[$TLE804] = $TSNNi1095;
			foreach ($TSa661 as $key) {
				$this->assertContains($key, $actualKeys);
			}
			$TLE662 = 'foo';
			$TSNNt1096 = $this->filesystem;
			$TSNNt1096->delete($TLE662);
			$TLE663 = 'bar';
			$TSNNt1097 = $this->filesystem;
			$TSNNt1097->delete($TLE663);
			$TLE664 = 'baz';
			$TSNNt1098 = $this->filesystem;
			$TSNNt1098->delete($TLE664);
		}
		public function shouldWorkWithHiddenFiles()
		{
			$TLE665 = '.foo';
			$TLE666 = 'hidden';
			$TSNNt1099 = $this->filesystem;
			$TSNNt1099->write($TLE665, $TLE666);
			$TLE667 = '.foo';
			$TSNNt1100 = $this->filesystem;
			$TLE668 = $TSNNt1100->has($TLE667);
			$this->assertTrue($TLE668);
			$TLE669 = '.foo';
			$TSNNt1101 = $this->filesystem;
			$TLE670 = $TSNNt1101->keys();
			$this->assertContains($TLE669, $TLE670);
			$TLE671 = '.foo';
			$TSNNt1102 = $this->filesystem;
			$TSNNt1102->delete($TLE671);
			$TLE672 = '.foo';
			$TSNNt1103 = $this->filesystem;
			$TLE673 = $TSNNt1103->has($TLE672);
			$this->assertFalse($TLE673);
		}
	}
	class LocalTest extends FunctionalTestCase
	{
		private $directory;
		public function __construct()
		{
			$TLE674 = '%s/filesystem';
			$TLE675 = '\\';
			$TLE676 = '/';
			$TLE677 = __DIR__;
			$TLE678 = str_replace($TLE675, $TLE676, $TLE677);
			$TLE679 = sprintf($TLE674, $TLE678);
			$TSNNt1104 = $TLE679;
			$this->directory = $TSNNt1104;
		}
		public function setUp()
		{
			$TSNNt1105 = $this->directory;
			$TLE680 = file_exists($TSNNt1105);
			$TLE681 = !$TLE680;
			if ($TLE681) {
				$TSNNt1106 = $this->directory;
				mkdir($TSNNt1106);
			}
			$TSNNt1107 = $this->directory;
			$TLE682 = new LocalAdapter($TSNNt1107);
			$TLE683 = new Filesystem($TLE682);
			$TSNNt1108 = $TLE683;
			$this->filesystem = $TSNNt1108;
		}
		public function tearDown()
		{
			$TLE684 = NULL;
			$TSNNt1109 = $TLE684;
			$this->filesystem = $TSNNt1109;
			$TSNNt1110 = $this->directory;
			$TLE685 = file_exists($TSNNt1110);
			if ($TLE685) {
				$TLE686 = FilesystemIterator::SKIP_DOTS;
				$TLE687 = FilesystemIterator::UNIX_PATHS;
				$TLE688 = ($TLE686 | $TLE687);
				$TSNNt1111 = $this->directory;
				$TLE689 = new RecursiveDirectoryIterator($TSNNt1111, $TLE688);
				$iterator = new RecursiveIteratorIterator($TLE689);
				foreach ($iterator as $item) {
					$TLE690 = $item->isDir();
					if ($TLE690) {
						$TLE691 = strval($item);
						rmdir($TLE691);
					} else {
						$TLE692 = strval($item);
						unlink($TLE692);
					}
				}
			}
		}
		public function shouldWorkWithSyslink()
		{
			$TLE693 = '%s/adapters';
			$TLE694 = __DIR__;
			$TLE695 = dirname($TLE694);
			$dirname = sprintf($TLE693, $TLE695);
			$TLE696 = '%s/link';
			$TLE697 = __DIR__;
			$TLE698 = dirname($TLE697);
			$linkname = sprintf($TLE696, $TLE698);
			$TSie699 = 0;
			$TSie700 = error_reporting($TSie699);
			$TLE701 = mkdir($dirname);
			$TSie702 = error_reporting($TSie700);
			$TSie703 = 0;
			$TSie704 = error_reporting($TSie703);
			$TLE705 = unlink($linkname);
			$TSie706 = error_reporting($TSie704);
			symlink($dirname, $linkname);
			$TLE707 = new LocalAdapter($linkname);
			$TLE708 = new Filesystem($TLE707);
			$TSNNt1112 = $TLE708;
			$this->filesystem = $TSNNt1112;
			$TLE709 = 'test.txt';
			$TLE710 = 'abc 123';
			$TSNNt1113 = $this->filesystem;
			$TSNNt1113->write($TLE709, $TLE710);
			$TLE711 = 'abc 123';
			$TLE712 = 'test.txt';
			$TSNNt1114 = $this->filesystem;
			$TLE713 = $TSNNt1114->read($TLE712);
			$this->assertSame($TLE711, $TLE713);
			$TLE714 = 'test.txt';
			$TSNNt1115 = $this->filesystem;
			$TSNNt1115->delete($TLE714);
			$TSie715 = 0;
			$TSie716 = error_reporting($TSie715);
			$TLE717 = unlink($linkname);
			$TSie718 = error_reporting($TSie716);
			$TSie719 = 0;
			$TSie720 = error_reporting($TSie719);
			$TLE721 = rmdir($dirname);
			$TSie722 = error_reporting($TSie720);
		}
		public function shouldListingOnlyGivenDirectory()
		{
			$TLE723 = '%s/localDir';
			$TSNNt1116 = $this->directory;
			$dirname = sprintf($TLE723, $TSNNt1116);
			$TSie724 = 0;
			$TSie725 = error_reporting($TSie724);
			$TLE726 = mkdir($dirname);
			$TSie727 = error_reporting($TSie725);
			$TSNNt1117 = $this->directory;
			$TLE728 = new LocalAdapter($TSNNt1117);
			$TLE729 = new Filesystem($TLE728);
			$TSNNt1118 = $TLE729;
			$this->filesystem = $TSNNt1118;
			$TLE730 = 'aaa.txt';
			$TLE731 = 'some content';
			$TSNNt1119 = $this->filesystem;
			$TSNNt1119->write($TLE730, $TLE731);
			$TLE732 = 'localDir/test.txt';
			$TLE733 = 'some content';
			$TSNNt1120 = $this->filesystem;
			$TSNNt1120->write($TLE732, $TLE733);
			$TLE734 = 'localDir/test';
			$TSNNt1121 = $this->filesystem;
			$dirs = $TSNNt1121->listKeys($TLE734);
			$TLE735 = 'dirs';
			$TSNNi1122 = $dirs[$TLE735];
			$this->assertEmpty($TSNNi1122);
			$TLE736 = 1;
			$TLE737 = 'keys';
			$TSNNi1123 = $dirs[$TLE737];
			$this->assertCount($TLE736, $TSNNi1123);
			$TLE738 = 'localDir/test.txt';
			$TLE739 = 'keys';
			$TLE740 = 0;
			$TSNNi1124 = $dirs[$TLE739];
			$TSNNi1125 = $TSNNi1124[$TLE740];
			$this->assertEquals($TLE738, $TSNNi1125);
			$TSNNt1126 = $this->filesystem;
			$dirs = $TSNNt1126->listKeys();
			$TLE741 = 1;
			$TLE742 = 'dirs';
			$TSNNi1127 = $dirs[$TLE742];
			$this->assertCount($TLE741, $TSNNi1127);
			$TLE743 = 'localDir';
			$TLE744 = 'dirs';
			$TLE745 = 0;
			$TSNNi1128 = $dirs[$TLE744];
			$TSNNi1129 = $TSNNi1128[$TLE745];
			$this->assertEquals($TLE743, $TSNNi1129);
			$TLE746 = 2;
			$TLE747 = 'keys';
			$TSNNi1130 = $dirs[$TLE747];
			$this->assertCount($TLE746, $TSNNi1130);
			$TLE748 = 'aaa.txt';
			$TLE749 = 'keys';
			$TLE750 = 0;
			$TSNNi1131 = $dirs[$TLE749];
			$TSNNi1132 = $TSNNi1131[$TLE750];
			$this->assertEquals($TLE748, $TSNNi1132);
			$TLE751 = 'localDir/test.txt';
			$TLE752 = 'keys';
			$TLE753 = 1;
			$TSNNi1133 = $dirs[$TLE752];
			$TSNNi1134 = $TSNNi1133[$TLE753];
			$this->assertEquals($TLE751, $TSNNi1134);
			$TSie754 = 0;
			$TSie755 = error_reporting($TSie754);
			$TLE756 = DIRECTORY_SEPARATOR;
			$TLE757 = ($dirname . $TLE756);
			$TLE758 = 'test.txt';
			$TLE759 = ($TLE757 . $TLE758);
			$TLE760 = unlink($TLE759);
			$TSie761 = error_reporting($TSie755);
			$TSie762 = 0;
			$TSie763 = error_reporting($TSie762);
			$TLE764 = DIRECTORY_SEPARATOR;
			$TSNNt1135 = $this->directory;
			$TLE765 = ($TSNNt1135 . $TLE764);
			$TLE766 = 'aaa.txt';
			$TLE767 = ($TLE765 . $TLE766);
			$TLE768 = unlink($TLE767);
			$TSie769 = error_reporting($TSie763);
			$TSie770 = 0;
			$TSie771 = error_reporting($TSie770);
			$TLE772 = rmdir($dirname);
			$TSie773 = error_reporting($TSie771);
		}
	}
	$test = new LocalTest();
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
	} catch (Exception $e) {
	}
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
			$TLE774 = 'zip';
			$TLE775 = extension_loaded($TLE774);
			$TLE776 = !$TLE775;
			if ($TLE776) {
				$TLE777 = 'The zip extension is not available.';
				$TLE778 = $this->markTestSkipped($TLE777);
				return $TLE778;
			}
			$TSie779 = 0;
			$TSie780 = error_reporting($TSie779);
			$TLE781 = __DIR__;
			$TLE782 = '/test.zip';
			$TLE783 = ($TLE781 . $TLE782);
			$TLE784 = touch($TLE783);
			$TSie785 = error_reporting($TSie780);
			$TLE786 = __DIR__;
			$TLE787 = '/test.zip';
			$TLE788 = ($TLE786 . $TLE787);
			$TLE789 = new ZipAdapter($TLE788);
			$TLE790 = new Filesystem($TLE789);
			$TSNNt1136 = $TLE790;
			$this->filesystem = $TSNNt1136;
		}
		public function tearDown()
		{
			FunctionalTestCase::tearDown();
			$TSie791 = 0;
			$TSie792 = error_reporting($TSie791);
			$TLE793 = __DIR__;
			$TLE794 = '/test.zip';
			$TLE795 = ($TLE793 . $TLE794);
			$TLE796 = unlink($TLE795);
			$TSie797 = error_reporting($TSie792);
		}
		public function shouldNotAcceptInvalidZipArchive()
		{
			$TLE798 = __FILE__;
			$TLE799 = new ZipAdapter($TLE798);
		}
		public function shouldCreateNewZipArchive()
		{
			$TLE800 = sys_get_temp_dir();
			$TLE801 = uniqid();
			$tmp = tempnam($TLE800, $TLE801);
			$za = new ZipAdapter($tmp);
			$this->assertFileExists($tmp);
			return $za;
		}
	}
	$test = new ZipTest();
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
	} catch (Exception $e) {
	}
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
	} catch (Exception $e) {
	}
	$test->tearDown();
	$test->setUp();
	$test->shouldCreateNewZipArchive();
	$test->tearDown();
?>
/home/henkerik/Development/haskell/objectsensitivetyping/tests
